;;; mevedel-eval-sandbox.el --- Persistent confined Eval services -*- lexical-binding: t -*-

;;; Commentary:

;; Session/owner-scoped child Emacsen, with foreground protocol calls and
;; one-shot idle expiry.  Process authority and lifetime stay in the execution
;; facade.  This module does not register a model-facing tool.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-transport)
(require 'subr-x)

(defcustom mevedel-eval-sandbox-idle-timeout 600
  "Seconds before an idle Eval sandbox stops; nil disables idle expiry."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'mevedel)

(cl-defstruct (mevedel-eval-sandbox--entry
               (:constructor mevedel-eval-sandbox--entry-create))
  service timer workdir roots callback started-at features lost request-id failure
  (serial 0) (partial ""))

(defvar mevedel-eval-sandbox--sessions (make-hash-table :test #'eq :weakness 'key)
  "Session to owner registry; dead entries remember lost definitions.")

(defun mevedel-eval-sandbox--entry (session owner)
  "Return the lazily allocated sandbox entry for SESSION and OWNER."
  (let ((owners (or (gethash session mevedel-eval-sandbox--sessions)
                    (puthash session (make-hash-table :test #'equal)
                             mevedel-eval-sandbox--sessions))))
    (or (gethash owner owners)
        (puthash owner (mevedel-eval-sandbox--entry-create) owners))))

(defun mevedel-eval-sandbox--cancel-timer (entry)
  "Cancel ENTRY's idle deadline, if any."
  (when (timerp (mevedel-eval-sandbox--entry-timer entry))
    (cancel-timer (mevedel-eval-sandbox--entry-timer entry)))
  (setf (mevedel-eval-sandbox--entry-timer entry) nil))

(defun mevedel-eval-sandbox--closed (entry facts)
  "Clear ENTRY after service death, settling its call with terminal FACTS."
  (mevedel-eval-sandbox--cancel-timer entry)
  (let ((reason (or (mevedel-eval-sandbox--entry-failure entry)
                    (plist-get facts :error)
                    (plist-get facts :termination))))
    (when (mevedel-eval-sandbox--entry-features entry)
      (setf (mevedel-eval-sandbox--entry-lost entry) t))
    (setf (mevedel-eval-sandbox--entry-service entry) nil
          (mevedel-eval-sandbox--entry-partial entry) ""
          (mevedel-eval-sandbox--entry-request-id entry) nil
          (mevedel-eval-sandbox--entry-failure entry) nil
          (mevedel-eval-sandbox--entry-features entry) nil)
    (when-let* ((callback (mevedel-eval-sandbox--entry-callback entry)))
      (setf (mevedel-eval-sandbox--entry-callback entry) nil)
      (funcall callback
               (list :error
                     (concat "Sandbox ended: "
                             (if (consp reason) (error-message-string reason)
                               (format "%s" reason))
                             (when (mevedel-eval-sandbox--entry-lost entry)
                               ". Earlier definitions are gone.")))))))

(defun mevedel-eval-sandbox--arm-timer (entry service)
  "Arm ENTRY's idle deadline for SERVICE, guarding cancelled held timers."
  (when mevedel-eval-sandbox-idle-timeout
    (let (timer)
      (setq timer
            (mevedel-transport-run-at-time
             mevedel-eval-sandbox-idle-timeout
             (lambda ()
               (when (and (eq timer (mevedel-eval-sandbox--entry-timer entry))
                          (eq service (mevedel-eval-sandbox--entry-service entry))
                          (not (mevedel-eval-sandbox--entry-callback entry)))
                 (mevedel-execution-service-stop service)))))
      (setf (mevedel-eval-sandbox--entry-timer entry) timer))))

(defun mevedel-eval-sandbox--reply (entry line)
  "Decode LINE for ENTRY into property-free typed fields, ignoring other IDs."
  (let* ((parsed (read-from-string line)) (reply (car parsed)))
    (unless (and (= (cdr parsed) (length line))
                 (proper-list-p reply) (= (length reply) 9)
                 (mevedel-eval-sandbox--entry-callback entry)
                 (natnump (plist-get (cdr reply) :features)))
      (error "Unexpected sandbox reply"))
    (let ((clean
           (append
            (cl-loop for key in '(:value :output :error)
                     for value = (plist-get (cdr reply) key)
                     unless (and (plist-member (cdr reply) key)
                                 (or (null value) (stringp value)))
                     do (error "Invalid sandbox reply field")
                     append (list key (and value (substring-no-properties value))))
            (list :features (plist-get (cdr reply) :features)))))
      (when (equal (car reply) (mevedel-eval-sandbox--entry-request-id entry)) clean))))

(defun mevedel-eval-sandbox--receive (entry service chunk)
  "Consume a bounded reply CHUNK from SERVICE into ENTRY."
  (condition-case err
      (mevedel-eval-sandbox--consume entry service chunk)
    (error
     (setf (mevedel-eval-sandbox--entry-failure entry) (error-message-string err))
     (mevedel-execution-service-stop service))))

(defun mevedel-eval-sandbox--consume (entry service chunk)
  "Parse CHUNK from SERVICE for ENTRY, signaling on protocol errors."
  (let ((text (concat (mevedel-eval-sandbox--entry-partial entry) chunk)) end)
    (when (> (string-bytes text) (* 1024 1024))
      (error "Sandbox reply exceeds 1 MiB"))
    (while (setq end (string-search "\n" text))
      (let* ((line (substring text 0 end))
             (start (string-search "\036" line)))
        (setq text (substring text (1+ end)))
        (when-let* ((reply (and start (mevedel-eval-sandbox--reply
                                      entry (substring line (1+ start)))))
                    (callback (mevedel-eval-sandbox--entry-callback entry)))
          (setf (mevedel-eval-sandbox--entry-callback entry) nil
                (mevedel-eval-sandbox--entry-lost entry) nil
                (mevedel-eval-sandbox--entry-features entry)
                (plist-get reply :features))
          (mevedel-execution-service-set-active service nil)
          (mevedel-eval-sandbox--arm-timer entry service)
          (funcall callback reply))))
    (setf (mevedel-eval-sandbox--entry-partial entry) text)))

(defun mevedel-eval-sandbox--server (workdir)
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
         (setq load-path ',load-path default-directory ,workdir)
         ;; Capture protocol primitives before evaluated code can advise them.
         ;; These lexical bindings are not passed to the evaluated form.
         (let ((,input (symbol-function 'read-from-minibuffer))
               (,parse (symbol-function 'read)) (,head (symbol-function 'car))
               (,body (symbol-function 'cadr)) (,emit (symbol-function 'prin1)))
           (while t
             (let* ((,request (funcall ,parse (funcall ,input "")))
                    (buffer (generate-new-buffer " *sandbox-output*"))
                    value failure output)
               (unwind-protect
                   (let ((standard-output buffer) (inhibit-message t))
                     (condition-case err
                         (setq value (eval (funcall ,parse (funcall ,body ,request)) t))
                       (error (setq failure (error-message-string err))))
                     (setq output (with-current-buffer buffer (buffer-string))))
                 (kill-buffer buffer))
               (let ((print-circle t) (print-length nil) (print-level nil)
                     (print-escape-newlines t) (print-escape-control-characters t))
                 (princ "\036")
                 (funcall ,emit (list (funcall ,head ,request) :value (format "%S" value)
                                     :output output :error failure :features (length features)))
                 (princ "\n"))))))))))

(defun mevedel-eval-sandbox--request-id (serial)
  "Return a fresh unpredictable request ID combined with SERIAL."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "/dev/urandom" nil nil 16)
    (unless (= (buffer-size) 16) (error "Cannot generate sandbox request nonce"))
    (format "%d:%s" serial (base64-encode-string (buffer-string) t))))

(cl-defun mevedel-eval-sandbox-call
    (session owner expression callback &key workdir writable-roots temporary-root reset)
  "Evaluate EXPRESSION in SESSION and OWNER's confined child.
CALLBACK receives a read-safe result plist, including :started and :state-lost.
WORKDIR and WRITABLE-ROOTS are compared with the live profile on every call;
a change or RESET replaces the child.  TEMPORARY-ROOT follows the facade.
Return a cancellation function scoped to this foreground call."
  (let* ((entry (mevedel-eval-sandbox--entry session owner))
         (roots (sort (delete-dups (mapcar #'expand-file-name writable-roots)) #'string<))
         (workdir (file-name-as-directory (expand-file-name workdir))))
    (when (mevedel-eval-sandbox--entry-callback entry)
      (error "Sandbox is busy; wait for its current call to settle"))
    (when (and (mevedel-eval-sandbox--entry-service entry)
               (or reset (not (equal workdir (mevedel-eval-sandbox--entry-workdir entry)))
                   (not (equal roots (mevedel-eval-sandbox--entry-roots entry)))))
      (mevedel-execution-service-stop (mevedel-eval-sandbox--entry-service entry)))
    (mevedel-eval-sandbox--cancel-timer entry)
    (let* ((started (null (mevedel-eval-sandbox--entry-service entry)))
           (lost (mevedel-eval-sandbox--entry-lost entry))
           (id (mevedel-eval-sandbox--request-id
                (cl-incf (mevedel-eval-sandbox--entry-serial entry))))
           script
           (send (lambda (service)
                   (setf (mevedel-eval-sandbox--entry-service entry) service)
                   (mevedel-execution-service-set-active service t)
                   (let ((print-length nil) (print-level nil) (print-escape-newlines t))
                     (mevedel-execution-service-write
                      service (concat (prin1-to-string (list id expression)) "\n"))))))
      (setf (mevedel-eval-sandbox--entry-request-id entry) id
            (mevedel-eval-sandbox--entry-callback entry)
            (lambda (reply)
              (funcall callback (append reply (list :started started :state-lost lost)))))
      (condition-case err
          (if (not started)
              (funcall send (mevedel-eval-sandbox--entry-service entry))
            (progn
              (setq script (make-temp-file "mevedel-eval-sandbox-" nil ".el"))
              (setf (mevedel-eval-sandbox--entry-workdir entry) workdir
                    (mevedel-eval-sandbox--entry-roots entry) roots
                    (mevedel-eval-sandbox--entry-started-at entry) (float-time))
              (with-temp-file script (insert (mevedel-eval-sandbox--server workdir)))
              (let ((service
                     (mevedel-execution-start-service
                      send (lambda (service chunk)
                             (mevedel-eval-sandbox--receive entry service chunk))
                      (lambda (facts)
                        (delete-file script)
                        (mevedel-eval-sandbox--closed entry facts))
                      :session session :owner owner
                      :command (list (expand-file-name invocation-name invocation-directory)
                                     "-Q" "--batch" "-l" script)
                      :workdir workdir :writable-roots roots :temporary-root temporary-root)))
                (when (mevedel-execution-service-live-p service)
                  (setf (mevedel-eval-sandbox--entry-service entry) service)))))
        (error
         (when script (ignore-errors (delete-file script)))
         (if (mevedel-eval-sandbox--entry-service entry)
             (mevedel-execution-service-stop (mevedel-eval-sandbox--entry-service entry))
           (mevedel-eval-sandbox--closed entry (list :error err)))))
      (lambda ()
        (when (and (equal id (mevedel-eval-sandbox--entry-request-id entry))
                   (mevedel-eval-sandbox--entry-callback entry))
          (mevedel-execution-service-stop (mevedel-eval-sandbox--entry-service entry)))))))

(provide 'mevedel-eval-sandbox)
;;; mevedel-eval-sandbox.el ends here
