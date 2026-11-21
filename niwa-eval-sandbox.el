;;; niwa-eval-sandbox.el --- Persistent confined Eval services -*- lexical-binding: t -*-

;;; Commentary:

;; Session/owner-scoped child Emacsen, with framed protocol calls and
;; one-shot idle expiry.  Process authority and lifetime stay in the execution
;; facade.  This module does not register a model-facing tool.

;;; Code:

(require 'cl-lib)
(require 'niwa-eval-capture)
(require 'niwa-execution)
(require 'niwa-transport)
(require 'subr-x)

;; `niwa-utilities'
(declare-function niwa--emacs-executable "niwa-utilities" ())
(autoload 'niwa--emacs-executable "niwa-utilities")

(defcustom niwa-eval-sandbox-idle-timeout 600
  "Seconds before an idle Eval sandbox stops; nil disables idle expiry."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'niwa)

(cl-defstruct (niwa-eval-sandbox--entry
               (:constructor niwa-eval-sandbox--entry-create))
  registry service timer workdir roots callback started-at features lost request-id failure sent
  (serial 0) (partial ""))

(defvar niwa-eval-sandbox--sessions (make-hash-table :test #'eq :weakness 'key)
  "Session to (live count . owner table); dead entries remember lost definitions.")

(defun niwa-eval-sandbox--entry (session owner)
  "Return the lazily allocated sandbox entry for SESSION and OWNER."
  (let ((registry (or (gethash session niwa-eval-sandbox--sessions)
                      (puthash session (cons 0 (make-hash-table :test #'equal))
                               niwa-eval-sandbox--sessions))))
    (or (gethash owner (cdr registry))
        (puthash owner (niwa-eval-sandbox--entry-create :registry registry)
                 (cdr registry)))))

(defun niwa-eval-sandbox-any-live-p (session)
  "Whether SESSION has a service, without enumeration or allocation."
  (> (or (car (gethash session niwa-eval-sandbox--sessions)) 0) 0))

(defun niwa-eval-sandbox--set-service (entry service)
  "Attach SERVICE to ENTRY, counting it once across preparation and readiness."
  (unless (niwa-eval-sandbox--entry-service entry)
    (cl-incf (car (niwa-eval-sandbox--entry-registry entry))))
  (setf (niwa-eval-sandbox--entry-service entry) service))

(defun niwa-eval-sandbox--cancel-timer (entry)
  "Cancel ENTRY's idle deadline, if any."
  (when (timerp (niwa-eval-sandbox--entry-timer entry))
    (cancel-timer (niwa-eval-sandbox--entry-timer entry)))
  (setf (niwa-eval-sandbox--entry-timer entry) nil))

(defun niwa-eval-sandbox-status (session owner)
  "Return live sandbox age and feature count for SESSION and OWNER.
Do not allocate a registry entry."
  (let* ((owners (cdr (gethash session niwa-eval-sandbox--sessions)))
         (entry (and owners (gethash owner owners)))
         (live (and entry (niwa-eval-sandbox--entry-service entry))))
    (list :age-seconds (and live (- (float-time) (niwa-eval-sandbox--entry-started-at entry)))
          :features (and live (niwa-eval-sandbox--entry-features entry)))))

(defun niwa-eval-sandbox-list-idle (session calls)
  "Return detached idle sandbox rows for SESSION's user interfaces.
CALLS are current execution snapshots; suppress their owners even during
call admission/settlement.  Do not allocate entries or expose service handles."
  (let (rows)
    (when-let* ((owners (cdr (gethash session niwa-eval-sandbox--sessions))))
      (maphash
       (lambda (owner entry)
         (when (and (niwa-execution-service-live-p
                     (niwa-eval-sandbox--entry-service entry))
                    (not (niwa-eval-sandbox--entry-callback entry))
                    (not (cl-find-if (lambda (call)
                                      (and (eq (plist-get call :eval-mode) 'sandbox)
                                           (equal owner (plist-get call :owner)))) calls)))
           (push (list :kind 'sandbox :id (list 'sandbox (copy-sequence owner))
                       :owner (copy-sequence owner) :state 'idle
                       :started-at (niwa-eval-sandbox--entry-started-at entry)
                       :features (niwa-eval-sandbox--entry-features entry)) rows)))
       owners))
    rows))

(defun niwa-eval-sandbox-stop-user (session snapshot)
  "Stop SESSION's idle sandbox identified by SNAPSHOT, unless replaced."
  (let* ((owners (cdr (gethash session niwa-eval-sandbox--sessions)))
         (entry (and owners (gethash (plist-get snapshot :owner) owners))))
    (when (and entry (not (niwa-eval-sandbox--entry-callback entry))
               (equal (plist-get snapshot :started-at)
                      (niwa-eval-sandbox--entry-started-at entry)))
      (niwa-execution-service-stop (niwa-eval-sandbox--entry-service entry)))))

(defun niwa-eval-sandbox--closed (entry facts)
  "Clear ENTRY after service death, settling its call with terminal FACTS."
  (niwa-eval-sandbox--cancel-timer entry)
  (when (niwa-eval-sandbox--entry-service entry)
    (cl-decf (car (niwa-eval-sandbox--entry-registry entry))))
  (let ((reason (or (niwa-eval-sandbox--entry-failure entry)
                    (plist-get facts :error)
                    (plist-get facts :termination))))
    (when (or (niwa-eval-sandbox--entry-features entry)
              (niwa-eval-sandbox--entry-sent entry))
      (setf (niwa-eval-sandbox--entry-lost entry) t))
    (setf (niwa-eval-sandbox--entry-service entry) nil
          (niwa-eval-sandbox--entry-partial entry) ""
          (niwa-eval-sandbox--entry-request-id entry) nil
          (niwa-eval-sandbox--entry-failure entry) nil
          (niwa-eval-sandbox--entry-sent entry) nil
          (niwa-eval-sandbox--entry-features entry) nil)
    (when-let* ((callback (niwa-eval-sandbox--entry-callback entry)))
      (setf (niwa-eval-sandbox--entry-callback entry) nil)
      (funcall callback
               (list :error
                     (concat "Sandbox ended: "
                             (if (consp reason) (error-message-string reason)
                               (format "%s" reason))
                             (when (niwa-eval-sandbox--entry-lost entry)
                               ". Earlier definitions are gone.")))))))

(defun niwa-eval-sandbox--arm-timer (entry service)
  "Arm ENTRY's idle deadline for SERVICE, guarding cancelled held timers."
  (when niwa-eval-sandbox-idle-timeout
    (let (timer)
      (setq timer
            (niwa-transport-run-at-time
             niwa-eval-sandbox-idle-timeout
             (lambda ()
               (when (and (eq timer (niwa-eval-sandbox--entry-timer entry))
                          (eq service (niwa-eval-sandbox--entry-service entry))
                          (not (niwa-eval-sandbox--entry-callback entry)))
                 (niwa-execution-service-stop service)))))
      (setf (niwa-eval-sandbox--entry-timer entry) timer))))

(defun niwa-eval-sandbox--reply (entry line)
  "Decode LINE for ENTRY into property-free typed fields, ignoring other IDs."
  (let* ((parsed (read-from-string line)) (reply (car parsed)))
    (unless (and (= (cdr parsed) (length line))
                 (proper-list-p reply) (= (length reply) 13)
                 (niwa-eval-sandbox--entry-callback entry)
                 (natnump (plist-get (cdr reply) :features)))
      (error "Unexpected sandbox reply"))
    (let ((clean
           (append
            (cl-loop for key in '(:value :output :error :messages :backtrace)
                     for value = (plist-get (cdr reply) key)
                     unless (and (plist-member (cdr reply) key)
                                 (or (null value) (stringp value)))
                     do (error "Invalid sandbox reply field")
                     append (list key (and value (substring-no-properties value))))
            (list :features (plist-get (cdr reply) :features)))))
      (when (equal (car reply) (niwa-eval-sandbox--entry-request-id entry)) clean))))

(defun niwa-eval-sandbox--receive (entry service chunk)
  "Consume a bounded reply CHUNK from SERVICE into ENTRY."
  (condition-case err
      (niwa-eval-sandbox--consume entry service chunk)
    (error
     (setf (niwa-eval-sandbox--entry-failure entry) (error-message-string err))
     (niwa-execution-service-stop service))))

(defun niwa-eval-sandbox--consume (entry service chunk)
  "Parse CHUNK from SERVICE for ENTRY, signaling on protocol errors."
  (let ((text (concat (niwa-eval-sandbox--entry-partial entry) chunk)) end)
    (when (> (string-bytes text) (* 1024 1024))
      (error "Sandbox reply exceeds 1 MiB"))
    (while (setq end (string-search "\n" text))
      (let* ((line (substring text 0 end))
             (start (string-search "\036" line)))
        (setq text (substring text (1+ end)))
        (when-let* ((reply (and start (niwa-eval-sandbox--reply
                                      entry (substring line (1+ start)))))
                    (callback (niwa-eval-sandbox--entry-callback entry)))
          (setf (niwa-eval-sandbox--entry-callback entry) nil
                (niwa-eval-sandbox--entry-lost entry) nil
                (niwa-eval-sandbox--entry-features entry)
                (plist-get reply :features))
          (niwa-execution-service-set-active service nil)
          (funcall callback reply)
          (when (and (eq service (niwa-eval-sandbox--entry-service entry))
                     (not (niwa-eval-sandbox--entry-callback entry)))
            (niwa-eval-sandbox--arm-timer entry service)))))
    (setf (niwa-eval-sandbox--entry-partial entry) text)))

(defun niwa-eval-sandbox--server (workdir)
  "Return a read-safe line-protocol child script for WORKDIR."
  (let ((print-length nil) (print-level nil) (print-escape-newlines t)
        (print-circle t) (print-gensym t)
        (input (make-symbol "input")) (parse (make-symbol "parse"))
        (head (make-symbol "head")) (body (make-symbol "body"))
        (emit (make-symbol "emit")) (request (make-symbol "request")))
    (concat
     ";;; -*- lexical-binding: t -*-\n"
     (prin1-to-string
      `(progn
         (require 'backtrace)
         (setq load-path ',load-path default-directory ,workdir)
         ;; Capture protocol primitives before evaluated code can advise them.
         ;; These lexical bindings are not passed to the evaluated form.
         (let ((,input (symbol-function 'read-from-minibuffer))
               (,parse (symbol-function 'read)) (,head (symbol-function 'car))
               (,body (symbol-function 'cadr)) (,emit (symbol-function 'prin1)))
           (while t
             (let* ((,request (funcall ,parse (funcall ,input "")))
                    (payload ,(macroexpand-all
                               `(niwa-eval-capture
                                 (eval (funcall ,parse (funcall ,body ,request)) t) t))))
               (let ((print-circle t) (print-length nil) (print-level nil)
                     (print-escape-newlines t) (print-escape-control-characters t))
                 (princ "\036")
                 (funcall ,emit (cons (funcall ,head ,request)
                                     (append payload (list :features (length features)))))
                 (princ "\n"))))))))))

(defun niwa-eval-sandbox--request-id (serial)
  "Return a fresh unpredictable request ID combined with SERIAL."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "/dev/urandom" nil nil 16)
    (unless (= (buffer-size) 16) (error "Cannot generate sandbox request nonce"))
    (format "%d:%s" serial (base64-encode-string (buffer-string) t))))

(cl-defun niwa-eval-sandbox-call
    (session owner expression callback &key workdir writable-roots temporary-root reset call)
  "Evaluate EXPRESSION in SESSION and OWNER's confined child.
CALLBACK receives a read-safe result plist, including :started and :state-lost.
WORKDIR and WRITABLE-ROOTS are compared with the live profile on every call;
a change or RESET replaces the child.  TEMPORARY-ROOT follows the facade.
CALL, when supplied, binds the facade's managed call to the service.
Return a cancellation function scoped to this protocol call."
  (let* ((entry (niwa-eval-sandbox--entry session owner))
         (roots (sort (delete-dups (mapcar #'expand-file-name writable-roots)) #'string<))
         (workdir (file-name-as-directory (expand-file-name workdir))))
    (when (niwa-eval-sandbox--entry-callback entry)
      (error "Sandbox is busy; wait for its current call to settle"))
    (when (and (niwa-eval-sandbox--entry-service entry)
               (or reset (not (equal workdir (niwa-eval-sandbox--entry-workdir entry)))
                   (not (equal roots (niwa-eval-sandbox--entry-roots entry)))))
      (niwa-execution-service-stop (niwa-eval-sandbox--entry-service entry)))
    (niwa-eval-sandbox--cancel-timer entry)
    (let* ((started (null (niwa-eval-sandbox--entry-service entry)))
           (lost (niwa-eval-sandbox--entry-lost entry))
           (id (niwa-eval-sandbox--request-id
                (cl-incf (niwa-eval-sandbox--entry-serial entry))))
           script
           (send (lambda (service)
                   (niwa-eval-sandbox--set-service entry service)
                   (setf (niwa-eval-sandbox--entry-sent entry) t)
                   (when call (niwa-execution-service-bind-call service call))
                   (niwa-execution-service-set-active service t)
                   (let ((print-length nil) (print-level nil) (print-escape-newlines t))
                     (niwa-execution-service-write
                      service (concat (prin1-to-string (list id expression)) "\n"))))))
      (setf (niwa-eval-sandbox--entry-request-id entry) id
            (niwa-eval-sandbox--entry-callback entry)
            (lambda (reply)
              (funcall callback (append reply (list :started started :state-lost lost)))))
      (condition-case err
          (if (not started)
              (funcall send (niwa-eval-sandbox--entry-service entry))
            (progn
              (setq script (make-temp-file "niwa-eval-sandbox-" nil ".el"))
              (setf (niwa-eval-sandbox--entry-workdir entry) workdir
                    (niwa-eval-sandbox--entry-roots entry) roots
                    (niwa-eval-sandbox--entry-started-at entry) (float-time))
              (with-temp-file script (insert (niwa-eval-sandbox--server workdir)))
              (let ((service
                     (niwa-execution-start-service
                      send (lambda (service chunk)
                             (niwa-eval-sandbox--receive entry service chunk))
                      (lambda (facts)
                        (delete-file script)
                        (niwa-eval-sandbox--closed entry facts))
                      :session session :owner owner
                      :command (list (niwa--emacs-executable)
                                     "-Q" "--batch" "-l" script)
                      :workdir workdir :writable-roots roots :temporary-root temporary-root)))
                (when (niwa-execution-service-live-p service)
                  (when (and call (niwa-eval-sandbox--entry-callback entry))
                    (niwa-execution-service-bind-call service call))
                  (unless (niwa-eval-sandbox--entry-service entry)
                    (niwa-eval-sandbox--set-service entry service)
                    ;; Admission notified before we could count the service.
                    ;; Publish the preparing call now, without waiting for READY.
                    (niwa-execution-service-set-active service t))))))
        (error
         (when script (ignore-errors (delete-file script)))
         (if (niwa-eval-sandbox--entry-service entry)
             (niwa-execution-service-stop (niwa-eval-sandbox--entry-service entry))
           (niwa-eval-sandbox--closed entry (list :error err)))))
      (lambda ()
        (when (and (equal id (niwa-eval-sandbox--entry-request-id entry))
                   (niwa-eval-sandbox--entry-callback entry))
          (niwa-execution-service-stop (niwa-eval-sandbox--entry-service entry)))))))

(provide 'niwa-eval-sandbox)
;;; niwa-eval-sandbox.el ends here
