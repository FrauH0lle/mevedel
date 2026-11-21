;;; test-niwa-session-publication-async.el --- Async delivery adversaries -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the producer and persisted consumer, not just publication phase
;; flags.  Fault injection intercepts callbacks around real filesystem work.
;; Each fixture owns its staging files, timers, processes, and conversation.

;;; Code:

(require 'niwa-agent-control)
(require 'niwa-session-checkpoint)
(require 'niwa-session-collection)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-transport)
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))

(defun test-niwa-publication-async--failure (phase)
  "Fail PHASE once, assert rollback, then retry the same producer identity."
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let* ((symbol (pcase phase
                      ('admission 'niwa-session-durability-admit-async)
                      ('reservation 'niwa-session-durability-reserve-publication-lease-async)
                      ('generation 'niwa-session-publication--prepare-generation)
                      ('head 'niwa-session-durability-commit-publication-head-async)))
            (original (symbol-function symbol))
            (head (plist-get (niwa-session-publication session) :head))
            (injections 0) acknowledgements)
       (cl-letf (((symbol-function symbol)
                  (lambda (&rest arguments)
                    (cl-incf injections)
                    (if (eq phase 'generation)
                        (error "Injected immutable generation failure")
                      (if (eq phase 'head)
                          (funcall (car (last arguments)) nil '(error "Injected head refusal"))
                        ;; Admission/reservation really ran.  Refuse their result
                        ;; at the continuation boundary, including fixed writes.
                        (let ((ack (car (last arguments))))
                          (apply original
                                 (append (butlast arguments)
                                         (list (lambda (_success _err)
                                                 (funcall ack nil '(error "Injected phase failure"))))))))))))
         (test-niwa-publication-async--send
          session "retry-id" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= injections 1))
       (should (= (length acknowledgements) 1))
       (should-not (caar acknowledgements))
       (should (cadar acknowledgements))
       (should-not (test-niwa-publication-async--mail session))
       (should-not (test-niwa-publication-async--restored-mail session))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (test-niwa-publication-async--send
        session "retry-id" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (caar acknowledgements))
       (should-not (cadar acknowledgements))
       (should (equal (list (secure-hash 'sha256 "/root:retry-id"))
                      (test-niwa-publication-async--ids
                       (test-niwa-publication-async--restored-mail session))))))))

;; Timers fired while a publisher waits inside its rider binding inherit it.
(defun test-niwa-publication-async--foreign-riders (refuse-reservation)
  "Publish while waiting inside a foreign commit-rider binding.
With REFUSE-RESERVATION the committed reservation is refused, so the rollback
phase commits the lease back to `active' without riders of its own."
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let* ((foreign (list :before nil
                           :after (list (list :op 'path-exists-p
                                              :path (niwa-session-save-path session)
                                              :optional t))
                           :consumed nil :before-results nil :after-results nil))
            (submit (symbol-function 'niwa-session-publication-submit))
            (advance (symbol-function 'niwa-session-publication--advance))
            (reserve (symbol-function
                      'niwa-session-durability-reserve-publication-lease-async))
            job phases acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication-submit)
                  (lambda (&rest arguments)
                    (prog1 (apply submit arguments)
                      (setq job (niwa-session-publication-job session)))))
                 ;; Observe that each phase really starts inside the binding.
                 ((symbol-function 'niwa-session-publication--advance)
                  (lambda (current)
                    (when (eq foreign niwa-session-durability--commit-riders)
                      (push (niwa-session-publication--job-phase current) phases))
                    (funcall advance current)))
                 ((symbol-function
                   'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (if refuse-reservation
                                 (lambda (_success _error)
                                   (funcall callback nil '(error "Injected refusal")))
                               callback)))))
         (let ((niwa-session-durability--commit-riders foreign))
           (test-niwa-publication-async--send
            session "foreign-rider" (lambda (ok err) (push (list ok err) acknowledgements)))
           (test-niwa-publication-async--wait (lambda () acknowledgements))))
       (should job)
       (should (memq (if refuse-reservation 'rollback 'generation) phases))
       (should-not (plist-get foreign :consumed))
       (should-not (plist-get foreign :after-results))
       ;; The job's own reserve riders ran inside the same binding.
       (should (plist-get (niwa-session-publication--job-riders job) :consumed))
       (should (plist-get (niwa-session-publication--job-riders job) :after-results))
       (if refuse-reservation
           (progn
             (should (equal '((nil (error "Injected refusal"))) acknowledgements))
             (should (eq 'active (plist-get (niwa-session-lease session) :status)))
             (should-not (test-niwa-publication-async--restored-mail session)))
         ;; The head commit's generation riders published the mail.
         (should (equal '((t nil)) acknowledgements))
         (should (equal (list (secure-hash 'sha256 "/root:foreign-rider"))
                        (test-niwa-publication-async--ids
                         (test-niwa-publication-async--restored-mail session)))))))))

(defun test-niwa-publication-async--transcript-artifacts
    (session buffer logical content &optional checkpoint)
  "Return the artifacts of one transcript checkpoint of SESSION from BUFFER.
A non-fixed transcript LOGICAL holding CONTENT, an ancillary fixed file below
a missing directory chain, and the sidecar commit marker carrying CHECKPOINT."
  (let ((save-path (niwa-session-save-path session)))
    (list (list :path (file-name-concat save-path logical) :content content)
          (list :path (file-name-concat save-path "notes/deep/extra.txt")
                :content "extra")
          (niwa-session-artifacts--sidecar-artifact session buffer checkpoint))))

(niwa-deftest niwa-session-publication--fixed-write-error ()
  ,test
  (test)
  :doc "optional conflicts are benign and a skipped write names its blocking proof"
  (should
   (equal '(file-error "Portable session publication reserve failed: operation=verify-latest status=mismatch code=72")
          (niwa-session-publication--fixed-write-error
           '((:op make-directory :optional t) (:op verify-latest) (:op write))
           '((:op make-directory :status conflict :code 73)
             (:op verify-latest :status mismatch :code 72 :diagnostic "private payload")
             (:op write :status skipped))))))

(niwa-deftest niwa-session-publication--job-result ()
  ,test
  (test)
  :doc "an unexplained refusal names its actual phase without inventing a cause"
  (dolist (case '((admission freeze rollback) (reserve generation rollback) (head adopt reconcile)))
    (let* ((session (niwa-session--create))
           (job (niwa-session-publication--job-create :session session :phase (car case))))
      (setf (niwa-session-publication-job session) job)
      (cl-letf (((symbol-function 'niwa-session-publication--advance-soon) #'ignore))
        (niwa-session-publication--job-result job (nth 1 case) nil nil))
      (should (eq (nth 2 case) (niwa-session-publication--job-phase job)))
      (should (equal (format "Portable session publication %s failed (cause unavailable)" (car case))
                     (error-message-string (niwa-session-publication--job-error job)))))))

(niwa-deftest niwa-agent-control-enqueue-execution-result/async (:quiet t)
  ,test
  (test)
  :doc "overlapping completions return before commit and acknowledge FIFO exactly once"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           acknowledgements)
       (dolist (id '("first" "second"))
         (should (eq 'pending
                     (test-niwa-publication-async--send
                      session id (lambda (ok err) (push (list id ok err) acknowledgements))))))
       ;; No event-loop opportunity has occurred.  Launch is not commitment.
       (should-not acknowledgements)
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should (niwa-session-publication-busy-p session))
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '(("first" t nil) ("second" t nil)) (reverse acknowledgements)))
       (let ((ids (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                          '("first" "second"))))
         (should (equal ids (test-niwa-publication-async--ids
                             (test-niwa-publication-async--mail session))))
         (should (equal ids (test-niwa-publication-async--ids
                             (test-niwa-publication-async--restored-mail session)))))
       (accept-process-output nil 0.05)
       (should (= 2 (length acknowledgements))))))

  :doc "admission failure rolls back and retry has one stable durable identity"
  (test-niwa-publication-async--failure 'admission)
  :doc "reservation failure rolls back the tentative mailbox despite fixed sidecar writes"
  (test-niwa-publication-async--failure 'reservation)
  :doc "generation failure leaves no ghost mail and permits same-ID retry"
  (test-niwa-publication-async--failure 'generation)
  :doc "head refusal reconciles old authority before rollback and same-ID retry"
  (test-niwa-publication-async--failure 'head)

  :doc "a committed head with a lost process result reconciles and retry cannot duplicate mail"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((commit (symbol-function 'niwa-session-durability-commit-publication-head-async))
           (lost 0) acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head-async)
                  (lambda (owner head callback)
                    (funcall commit owner head
                             (lambda (ok err)
                               (should ok) (should-not err)
                               (cl-incf lost)
                               (funcall callback nil '(file-error "Program died after head commit")))))))
         (test-niwa-publication-async--send
          session "lost-result" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= lost 1))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session))))
       (test-niwa-publication-async--send
        session "lost-result" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should (equal (list (secure-hash 'sha256 "/root:lost-result"))
                      (test-niwa-publication-async--ids
                       (test-niwa-publication-async--restored-mail session)))))))

  :doc "retry of consumed but uncheckpointed mail does not redeliver its stable identity"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (test-niwa-publication-async--send
        session "consumed" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (niwa-session-checkpoint-consume
        session "/root" (test-niwa-publication-async--mail session))
       (niwa-agent-control--set-mailbox-queue session "/root" nil)
       (test-niwa-publication-async--send
        session "consumed" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should-not (test-niwa-publication-async--mail session))
       ;; Until the transcript checkpoints consumption, restore must retain it.
       (should (= 1 (length (test-niwa-publication-async--restored-mail session))))
       (niwa-session-artifacts-save session buffer nil t)
       (should-not (test-niwa-publication-async--restored-mail session))))))

(niwa-deftest niwa-session-publication-submit/async (:quiet t)
  ,test
  (test)
  :doc "quit while staging removes partial files and fails the checkpoint once"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((write (symbol-function 'write-region))
           (writes 0) directories acknowledgements)
       (cl-letf (((symbol-function 'write-region)
                  (lambda (start end filename &rest arguments)
                    (when (and (stringp filename)
                               (string-match-p "niwa-publication-" filename))
                      (push (file-name-directory filename) directories)
                      (when (= (cl-incf writes) 2) (signal 'quit nil)))
                    (apply write start end filename arguments))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "Partial staging"))
          (lambda (ok err) (push (list ok err) acknowledgements)) nil t)
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= writes 2))
       (should (equal '((nil (quit))) acknowledgements))
       (should-not (niwa-session-publication-busy-p session))
       (should-not (niwa-session-pending-publication session))
       (dolist (directory directories) (should-not (file-exists-p directory))))))

  :doc "quit in an acknowledged producer callback cannot strand its successor"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements diagnostics)
       (niwa-test--with-captured-diagnostics diagnostics
         (test-niwa-publication-async--send
          session "callback-quit"
          (lambda (ok err)
            (push (list ok err) acknowledgements)
            (signal 'quit nil)))
         (test-niwa-publication-async--send
          session "after-callback-quit"
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait
          (lambda () (= 2 (length acknowledgements)))))
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should (string-match-p "Publication acknowledgement failed: Quit" diagnostics))
       (should-not (niwa-session-publication-busy-p session))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "disabled transport still drains accepted overlapping publications exactly once"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((niwa-transport--enabled-p nil) acknowledgements)
       (dolist (id '("disabled-one" "disabled-two"))
         (test-niwa-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       (should-not acknowledgements)
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (accept-process-output nil 0.02)
       (should (equal '(("disabled-one" t nil) ("disabled-two" t nil))
                      (reverse acknowledgements)))
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "global transport cancellation cannot strand a job or its queued successor"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((niwa-transport--enabled-p t) acknowledgements)
       (dolist (id '("cancelled-one" "cancelled-two"))
         (test-niwa-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       ;; Cancel both the job-advance and queued-preparation opportunities.
       (should (>= (hash-table-count niwa-transport--pending) 2))
       (niwa-transport-cancel-pending)
       (should-not acknowledgements)
       (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
       (accept-process-output nil 0.02)
       (should (equal '(("cancelled-one" t nil) ("cancelled-two" t nil))
                      (reverse acknowledgements)))
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "an ACK can stage a new artifact without its predecessor cleanup losing the batch"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((logical "agents/ack-staged.chat.org") acknowledgements inherited-cells)
       (test-niwa-publication-async--send
        session "nested-stage"
        (lambda (ok err)
          (push (list ok err) acknowledgements)
          (setq inherited-cells
                (list niwa-session-durability--transaction-clock
                      niwa-session-durability--asserted-directories
                      niwa-session-recovery--mutation-cache
                      niwa-session-durability--commit-riders
                      niwa-session-durability--final-commit
                      niwa-session-durability-async-process-function))
          (niwa-session-publication-publish
           session (list (list :path (file-name-concat (niwa-session-save-path session) logical)
                               :content "ACK staged transcript")))))
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should (equal '(nil nil nil nil nil nil) inherited-cells))
       (should (niwa-session-publication-uncommitted-artifact session logical))
       (niwa-session-artifacts-save session buffer nil t)
       (should-not (niwa-session-publication-uncommitted-artifact session logical))
       (should (equal "ACK staged transcript"
                      (niwa-session-artifacts-read-artifact session logical t)))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "a phase advanced inside another publisher's rider binding runs only its own riders"
  (test-niwa-publication-async--foreign-riders nil)
  :doc "a rollback commit advanced inside another publisher's rider binding leaves them alone"
  (test-niwa-publication-async--foreign-riders t)

  :doc "cleanup failure after authoritative adoption cannot change a success acknowledgement"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((delete (symbol-function 'niwa-session-publication--delete-batches))
           (failed 0) acknowledgements diagnostics)
       (niwa-test--with-captured-diagnostics diagnostics
         (cl-letf (((symbol-function 'niwa-session-publication--delete-batches)
                    (lambda (batches)
                      (funcall delete batches)
                      (cl-incf failed)
                      (error "Injected cleanup failure"))))
           (test-niwa-publication-async--send
            session "cleanup" (lambda (ok err) (push (list ok err) acknowledgements)))
           (test-niwa-publication-async--wait (lambda () acknowledgements))))
       (should (= failed 1))
       (should (string-match-p "Injected cleanup failure" diagnostics))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "collection scheduling failure after adoption warns but still succeeds exactly once"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((failed 0) acknowledgements diagnostics)
       (niwa-test--with-captured-diagnostics diagnostics
         (cl-letf (((symbol-function 'niwa-session-collection-schedule)
                    (lambda (_session)
                      (cl-incf failed)
                      (error "Injected collection scheduling failure"))))
           (test-niwa-publication-async--send
            session "collection" (lambda (ok err) (push (list ok err) acknowledgements)))
           (test-niwa-publication-async--wait (lambda () acknowledgements))))
       (should (= failed 1))
       (should (string-match-p "Injected collection scheduling failure" diagnostics))
       (should (equal '((t nil)) acknowledgements))
       (should-not (niwa-session-publication-busy-p session))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "disk restore on either side of head adoption excludes then includes tentative mail"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((reserve (symbol-function 'niwa-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-niwa-publication-async--send
          session "restore" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () continuation))
         (should-not acknowledgements)
         (should (plist-get (car (test-niwa-publication-async--mail session)) :uncommitted))
         (should-not (test-niwa-publication-async--restored-mail session))
         (funcall continuation)
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should-not (plist-get (car (test-niwa-publication-async--mail session)) :uncommitted))
       (should (equal (list (secure-hash 'sha256 "/root:restore"))
                      (test-niwa-publication-async--ids
                       (test-niwa-publication-async--restored-mail session)))))))

  :doc "a multi-artifact checkpoint publishes transcripts and ancillary fixed files asynchronously"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           (save-path (niwa-session-save-path session))
           acknowledgements)
       (niwa-session-publication-submit
        session
        (lambda ()
          (test-niwa-publication-async--transcript-artifacts
           session buffer "segment-9.chat.org" "async transcript"))
        (lambda (ok err) (push (list ok err) acknowledgements)))
       (should-not acknowledgements)
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should-not (equal head (plist-get (niwa-session-publication session) :head)))
       (should (equal "extra"
                      (with-temp-buffer
                        (insert-file-contents
                         (file-name-concat save-path "notes/deep/extra.txt"))
                        (buffer-string))))
       (should (equal "async transcript"
                      (niwa-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (should-not (niwa-session-pending-publication session)))))

  :doc "a real fixed write failure reports its cause while rolling back a healthy reservation"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((head (plist-get (niwa-session-publication session) :head))
            (directory (niwa-session-save-path session))
            (reserve (symbol-function 'niwa-session-durability-reserve-publication-lease-async))
            reservation acknowledgements)
       (make-directory (file-name-concat directory "notes/deep/extra.txt") t)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok error)
                               (setq reservation (list ok error))
                               (funcall callback ok error))))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "refused transcript"))
          (lambda (ok error) (push (list ok error) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (equal '(t nil) reservation))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should (equal "Portable session publication reserve failed: operation=write status=failed code=67"
                      (error-message-string (cadar acknowledgements))))
       (should (equal head (plist-get (niwa-session-publication-read directory) :head)))
       (should (eq 'active (plist-get (niwa-session-lease session) :status)))
       (should (niwa-session-durability-lease-owned-p session))
       (should-not (niwa-session-publication-busy-p session)))))

  :doc "a nil preparation is a clean no-op success"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           acknowledgements)
       (niwa-session-publication-submit
        session #'ignore (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (should (equal '((t nil)) acknowledgements))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (should-not (niwa-session-publication-active-p session)))))

  :doc "a replacing marker is refused before any target write"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           acknowledgements)
       (niwa-session-publication-submit
        session
        (lambda ()
          (mapcar (lambda (artifact)
                    (if (plist-get artifact :commit-marker)
                        (plist-put (copy-sequence artifact) :replace t)
                      artifact))
                  (test-niwa-publication-async--transcript-artifacts
                   session buffer "segment-9.chat.org" "refused")))
        (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (should-not (caar acknowledgements))
       (should (cadar acknowledgements))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should-not (niwa-session-pending-publication session))
       (should-not (file-exists-p
                    (file-name-concat (niwa-session-save-path session)
                                      "notes/deep/extra.txt"))))))

  :doc "a checkpoint candidate is installed only after its head commits"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((install (symbol-function 'niwa-session-checkpoint-install))
           (head (plist-get (niwa-session-publication session) :head))
           installed acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-checkpoint-install)
                  (lambda (owner candidate)
                    (push (plist-get (niwa-session-publication owner) :head) installed)
                    (funcall install owner candidate))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "checkpointed"
             (niwa-session-checkpoint-capture session "/root")))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length installed)))
       (should-not (equal head (car installed)))
       (should (equal (plist-get (niwa-session-publication session) :head)
                      (car installed))))))

  :doc "a candidate invalidated before the head fails without installing or writing"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           (installs 0)
           acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-checkpoint-install)
                  (lambda (&rest _) (cl-incf installs))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (let ((candidate (niwa-session-checkpoint-capture session "/root")))
              ;; Adopting another history invalidates every earlier candidate.
              (niwa-session-checkpoint-seed
               session (niwa-session-artifacts--live-sidecar session buffer))
              (test-niwa-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "stale" candidate)))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should-not (caar acknowledgements))
       (should (= 0 installs))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should-not (niwa-session-pending-publication session)))))

  :doc "retain-failure keeps the frozen batch as recovery and a retry commits it"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((prepare (symbol-function 'niwa-session-publication--prepare-generation))
           (head (plist-get (niwa-session-publication session) :head))
           (failures 0)
           acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--prepare-generation)
                  (lambda (&rest arguments)
                    (if (= 0 failures)
                        (progn (cl-incf failures)
                               (error "Injected immutable generation failure"))
                      (apply prepare arguments)))))
         (niwa-test--with-captured-diagnostics nil
           (niwa-session-publication-submit
            session
            (lambda ()
              (test-niwa-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "retained"))
            (lambda (ok err) (push (list ok err) acknowledgements))
            nil t)
           (test-niwa-publication-async--wait (lambda () acknowledgements))
           (should-not (caar acknowledgements))
           (should (equal head (plist-get (niwa-session-publication session) :head)))
           (should (niwa-session-pending-publication session))
           (should (plist-get (niwa-session-pending-publication session) :batches))
           (niwa-session-publication-retry session)))
       (should-not (niwa-session-pending-publication session))
       (should-not (equal head (plist-get (niwa-session-publication session) :head)))
       (should (equal "retained"
                      (niwa-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))
       (should (= 1 (length acknowledgements))))))

  :doc "without retain-failure the frozen batch is discarded and later mutation proceeds"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--prepare-generation)
                  (lambda (&rest _) (error "Injected immutable generation failure"))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "discarded"))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (niwa-session-pending-publication session))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should-not (niwa-session-publication-uncommitted-artifact
                    session "segment-9.chat.org")))))

  :doc "a committed head whose result was lost reconciles, adopts, and installs once"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((original (symbol-function 'niwa-session-durability-commit-publication-head-async))
            (install (symbol-function 'niwa-session-checkpoint-install))
            (head (plist-get (niwa-session-publication session) :head))
            (installs 0)
            acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head-async)
                  (lambda (owner candidate callback)
                    ;; The head really commits; only its result is lost.
                    (funcall original owner candidate
                             (lambda (_success _error)
                               (funcall callback nil '(error "Injected lost result"))))))
                 ((symbol-function 'niwa-session-checkpoint-install)
                  (lambda (owner candidate)
                    (cl-incf installs)
                    (funcall install owner candidate))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "late"
             (niwa-session-checkpoint-capture session "/root")))
          (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 installs))
       (should-not (equal head (plist-get (niwa-session-publication session) :head)))
       (should-not (niwa-session-pending-publication session))
       (should (equal "late"
                      (niwa-session-artifacts-read-artifact
                       session "segment-9.chat.org" t)))))))

(niwa-deftest niwa-session-publication--async-fixed-operations (:quiet t)
  ,test
  (test)
  :doc "one program proves the lease then ensures each parent level and writes each fixed artifact"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (niwa-session-publication--stage-artifacts
                    session
                    (test-niwa-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (plan (niwa-session-publication--async-fixed-operations session batch))
            (operations (car plan)))
       (unwind-protect
           (progn
             ;; The immutable-only transcript contributes nothing; the deep
             ;; ancillary file brings its two parents, the sidecar none.
             (should (equal '(verify-latest make-directory make-directory write write)
                            (mapcar (lambda (operation) (plist-get operation :op))
                                    operations)))
             (should (equal '(3 4) (cdr plan)))
             (should (cl-every (lambda (index)
                                 (eq 'write (plist-get (nth index operations) :op)))
                               (cdr plan)))
             (should (equal ".el" (plist-get (car operations) :content)))
             (should (cl-every (lambda (operation) (plist-get operation :optional))
                               (seq-filter (lambda (operation)
                                             (eq 'make-directory (plist-get operation :op)))
                                           operations)))
             (should (equal (file-name-concat (niwa-session-save-path session)
                                              "notes/deep/extra.txt")
                            (file-truename (plist-get (nth 3 operations) :path)))))
         (niwa-session-publication--delete-batch batch)))))
  :doc "a session without a bound lease generation plans nothing"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((batch (niwa-session-publication--stage-artifacts
                   session
                   (test-niwa-publication-async--transcript-artifacts
                    session buffer "segment-9.chat.org" "transcript")))
           (lease (niwa-session-lease session)))
       (unwind-protect
           (progn
             (setf (niwa-session-lease session) nil)
             (should (equal '(nil) (niwa-session-publication--async-fixed-operations
                                    session batch))))
         (setf (niwa-session-lease session) lease)
         (niwa-session-publication--delete-batch batch)))))
  :doc "a fixed artifact outside the session directory is refused"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (should-error
      (niwa-session-publication--async-fixed-operations
       session (list :artifacts (list (list :path (make-temp-name "/tmp/niwa-outside-")
                                            :source "/tmp/niwa-source"))))))))

(niwa-deftest niwa-session-publication--project-staged-sidecar (:quiet t)
  ,test
  (test)
  :doc "an identity the checkpoint candidate consumed since freezing leaves the staged sidecar"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (test-niwa-publication-async--send
        session "stale-id" (lambda (ok err) (push (list ok err) acknowledgements)))
       (test-niwa-publication-async--wait (lambda () acknowledgements))
       (let* ((mail (test-niwa-publication-async--mail session))
              (id (plist-get (car mail) :id))
              (batch (niwa-session-publication--stage-artifacts
                      session
                      (test-niwa-publication-async--transcript-artifacts
                       session buffer "segment-9.chat.org" "transcript")))
              (marker (niwa-session-publication--batch-marker batch))
              (source (plist-get marker :source)))
         (unwind-protect
             (progn
               (should (member id (test-niwa-publication-async--ids
                                   (plist-get (niwa-session-publication--read-staged-sidecar
                                               source)
                                              :messages))))
               ;; The mail is consumed after the sidecar was frozen, and the
               ;; candidate captured with that consumption retires it.
               (niwa-session-checkpoint-consume session "/root" mail)
               (niwa-agent-control--set-mailbox-queue session "/root" nil)
               (niwa-session-publication--project-staged-sidecar
                session marker (niwa-session-checkpoint-capture session "/root"))
               (should-not (member id (test-niwa-publication-async--ids
                                       (plist-get
                                        (niwa-session-publication--read-staged-sidecar
                                         source)
                                        :messages))))
               (should-not (plist-get marker :source-sha256)))
           (niwa-session-publication--delete-batch batch))))))
  :doc "an already projected sidecar keeps its staged bytes"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (niwa-session-publication--stage-artifacts
                    session
                    (test-niwa-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (marker (niwa-session-publication--batch-marker batch))
            (source (plist-get marker :source))
            (before (niwa-session-publication--file-sha256 source)))
       (unwind-protect
           (progn
             (niwa-session-publication--project-staged-sidecar session marker nil)
             (should (equal before (niwa-session-publication--file-sha256 source))))
         (niwa-session-publication--delete-batch batch)))))
  :doc "a session without a checkpoint owner is left untouched"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((batch (niwa-session-publication--stage-artifacts
                    session
                    (test-niwa-publication-async--transcript-artifacts
                     session buffer "segment-9.chat.org" "transcript")))
            (marker (niwa-session-publication--batch-marker batch))
            (source (plist-get marker :source))
            (before (niwa-session-publication--file-sha256 source)))
       (let ((owner (niwa-session-checkpoint-state session)))
         (unwind-protect
             (cl-letf (((symbol-function 'niwa-session-checkpoint-project)
                        (lambda (&rest _) (ert-fail "Projected without an owner"))))
               (setf (niwa-session-checkpoint-state session) nil)
               (niwa-session-publication--project-staged-sidecar session marker nil)
               (should (equal before (niwa-session-publication--file-sha256 source))))
           (setf (niwa-session-checkpoint-state session) owner)
           (niwa-session-publication--delete-batch batch)))))))

(defun test-niwa-publication-async--interleaving (fail &optional latch-first)
  "Interleave checkpoint, observational registry save and latch with delivery.
FAIL refuses the reserved job, otherwise it commits before the other writers.
LATCH-FIRST starts the latch writer, rather than the root checkpoint, mid-job."
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((reserve (symbol-function 'niwa-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation
                                     (lambda ()
                                       (funcall callback (and (not fail) ok)
                                                (if fail '(error "Injected rollback") err)))))))))
         (test-niwa-publication-async--send
          session "interleaved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () continuation))
         (should-not (test-niwa-publication-async--restored-mail session))
         ;; An observational writer must queue PREPARATION, not frozen mail.
         (niwa-session-artifacts-save-agent-registry session buffer)
         (with-current-buffer buffer (goto-char (point-max)) (insert "More streamed text\n"))
         (run-at-time 0.01 nil continuation)
         ;; Both are synchronous callers: they must drain before capturing or
         ;; mutating the generation the active job is proving.
         (when latch-first
           (should (niwa-session-durability-set-unsettled-mutation session t)))
         (niwa-session-artifacts-save session buffer nil t)
         (unless latch-first
           (should (niwa-session-durability-set-unsettled-mutation session t)))
         (test-niwa-publication-async--wait
          (lambda () (not (niwa-session-publication-busy-p session)))))
       (should (= 1 (length acknowledgements)))
       (should (eq (not fail) (and (caar acknowledgements) t)))
       (should (niwa-session-durability-unsettled-mutation-p session))
       (should (= (if fail 0 1) (length (test-niwa-publication-async--restored-mail session))))
       (unless fail
         ;; Consumption after commit must not be resurrected by a deferred save.
         (let ((mail (test-niwa-publication-async--mail session)))
           (niwa-session-checkpoint-consume session "/root" mail)
           (niwa-agent-control--set-mailbox-queue session "/root" nil)
           (niwa-session-artifacts-save-agent-registry session buffer)
           (niwa-session-artifacts-save session buffer nil t)
           (should-not (test-niwa-publication-async--restored-mail session))))))))

(niwa-deftest niwa-session-publication-defer/async (:quiet t)
  ,test
  (test)
  :doc "quit in a queued preparation and its failure handler releases the next entry"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledged failures successor diagnostics)
       (niwa-test--with-captured-diagnostics diagnostics
         (test-niwa-publication-async--send
          session "preparation-quit" (lambda (&rest _) (setq acknowledged t)))
         (should (niwa-session-publication-defer
                  session (lambda (_token) (signal 'quit nil))
                  (lambda (err) (push err failures) (signal 'quit nil))))
         (should (niwa-session-publication-defer
                  session (lambda (_token) (setq successor t))))
         (test-niwa-publication-async--wait (lambda () successor)))
       (should acknowledged)
       (should (equal '((quit)) failures))
       (should (string-match-p "Quit" diagnostics))
       (should-not (niwa-session-publication-busy-p session))
       (should-not (niwa-session-publication-preparation-token session)))))

  :doc "a timer cannot borrow a queued synchronous publication's held lease window"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((drain (symbol-function 'niwa-session-publication--drain))
           injected timer-fired entered blocked prepared acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--drain)
                  (lambda (&rest arguments)
                    (when (and (not injected)
                               (niwa-session-publication-preparation-token session))
                      (setq injected t)
                      (should (niwa-session-publication-active-p session))
                      (should-not (niwa-session-publication-job session))
                      (run-at-time
                       0 nil
                       (lambda ()
                         (unwind-protect
                             (condition-case err
                                 (niwa-session-durability-call-with-held-lease
                                  session (lambda ()
                                            (setq entered t)
                                            (niwa-session-durability-set-unsettled-mutation session t)))
                               (user-error (setq blocked err)))
                           (setq timer-fired t))))
                      (test-niwa-publication-async--wait (lambda () timer-fired)))
                    (apply drain arguments))))
         (test-niwa-publication-async--send
          session "lease-window" (lambda (ok err) (push (list ok err) acknowledgements)))
         (should (niwa-session-publication-defer
                  session (lambda (token)
                            (niwa-session-artifacts-save session buffer nil t token)
                            (setq prepared t))))
         (test-niwa-publication-async--wait (lambda () prepared))
         (niwa-session-publication-barrier session))
       (should injected)
       (should timer-fired)
       (should blocked)
       (should-not entered)
       (should-not (niwa-session-durability-unsettled-mutation-p session))
       (should (equal '((t nil)) acknowledgements))
       (should (niwa-session-durability-set-unsettled-mutation session t))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "a timer during preparation cannot inherit the owner's mutation-gate bypass"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((artifact (symbol-function 'niwa-session-artifacts--sidecar-artifact))
           first-entered preparing timer-fired (nested-freezes 0) acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-artifacts--sidecar-artifact)
                  (lambda (&rest arguments)
                    (when preparing (cl-incf nested-freezes))
                    (unless first-entered
                      (setq first-entered t preparing t)
                      (unwind-protect
                          (progn
                            (run-at-time
                             0 nil
                             (lambda ()
                               (test-niwa-publication-async--send
                                session "timer-second"
                                (lambda (ok err) (push (list "second" ok err) acknowledgements)))
                               (niwa-session-artifacts-save-agent-registry session buffer)
                               (setq timer-fired t)))
                            (test-niwa-publication-async--wait (lambda () timer-fired)))
                        (setq preparing nil)))
                    (apply artifact arguments))))
         (test-niwa-publication-async--send
          session "timer-first"
          (lambda (ok err) (push (list "first" ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements))))
         (niwa-session-publication-barrier session))
       (should timer-fired)
       (should (= 0 nested-freezes))
       (should (equal '(("first" t nil) ("second" t nil)) (reverse acknowledgements)))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "checkpoint, registry and latch writers preserve newly committed mail and consumption"
  (test-niwa-publication-async--interleaving nil)
  :doc "a registry save requested during tentative delivery cannot resurrect rolled-back mail"
  (test-niwa-publication-async--interleaving t)
  :doc "a latch change requested during reservation waits without erasing newly committed mail"
  (test-niwa-publication-async--interleaving nil t)
  :doc "a latch change requested during reservation cannot resurrect rolled-back mail"
  (test-niwa-publication-async--interleaving t t))

(niwa-deftest niwa-session-publication--schedule/async (:quiet t)
  ,test
  (test)
  :doc "the gate-open hook runs before the next queued preparation and a failing hook does not stop it"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (log diagnostics)
       (let ((niwa-session-publication-gate-open-functions
              (list (lambda (owner)
                      (push (list 'hook (eq owner session)
                                  (niwa-session-publication-preparation-token owner)
                                  (niwa-session-publication-job owner)
                                  (length (niwa-session-publication-preparations owner)))
                            log))
                    (lambda (_owner) (error "Injected gate hook failure")))))
         ;; A running preparation keeps the gate closed: no hook, no start.
         (setf (niwa-session-publication-preparation-token session) '(in-flight))
         (should (niwa-session-publication-defer
                  session (lambda (_token) (push 'queued log))))
         (accept-process-output nil 0.05)
         (should-not log)
         (setf (niwa-session-publication-preparation-token session) nil)
         (niwa-test--with-captured-diagnostics diagnostics
           (niwa-session-publication--schedule session)
           (test-niwa-publication-async--wait (lambda () (memq 'queued log)))))
       ;; The hook saw the open gate with the successor still queued.
       (should (equal '((hook t nil nil 1) queued) (reverse log)))
       (should (string-match-p "Injected gate hook failure" diagnostics))
       (should-not (niwa-session-publication-busy-p session))))))

(niwa-deftest niwa-session-publication-barrier/async (:quiet t)
  ,test
  (test)
  :doc "a busy transport rejects a synchronous barrier before entering its wait loop"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements)
       (test-niwa-publication-async--send
        session "busy-transport" (lambda (ok err) (push (list ok err) acknowledgements)))
       (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&rest _) t))
                 ((symbol-function 'accept-process-output)
                  (lambda (&rest _) (ert-fail "Barrier waited behind its active transport"))))
         (should-error (niwa-session-publication-barrier session) :type 'user-error))
       (should-not acknowledgements)
       (niwa-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements)))))

  :doc "a synchronous publication owner cannot recursively drain its own queued preparation"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledgements)
       (setf (niwa-session-publication-active-p session) t)
       (test-niwa-publication-async--send
        session "active-owner" (lambda (ok err) (push (list ok err) acknowledgements)))
       (should-not (niwa-session-publication-job session))
       (should (niwa-session-publication-preparations session))
       (unwind-protect
           (cl-letf (((symbol-function 'accept-process-output)
                      (lambda (&rest _) (ert-fail "Barrier waited behind its synchronous owner"))))
             (should-error (niwa-session-publication-barrier session) :type 'user-error))
         (setf (niwa-session-publication-active-p session) nil))
       (niwa-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements)))))

  :doc "a barrier re-kicks an existing job stranded between admission and freeze"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((advance (symbol-function 'niwa-session-publication--advance-soon))
           paused acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--advance-soon)
                  (lambda (job)
                    (if (eq 'freeze (niwa-session-publication--job-phase job))
                        (setq paused t)
                      (funcall advance job)))))
         (test-niwa-publication-async--send
          session "rekick" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () paused)))
       (should-not acknowledgements)
       (niwa-session-publication-barrier session)
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "a bounded barrier times out without discarding or falsely acknowledging accepted work"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'niwa-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (funcall admit owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-niwa-publication-async--send
          session "timeout" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () continuation))
         (let ((niwa-session-publication--barrier-seconds 0))
           (should-error (niwa-session-publication-barrier session) :type 'user-error))
         (should-not acknowledgements)
         (should (niwa-session-publication-busy-p session))
         (funcall continuation)
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (equal '((t nil)) acknowledgements))
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "owner epoch replacement during admission fails closed with exactly one acknowledgement"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'niwa-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (funcall admit owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-niwa-publication-async--send
          session "epoch" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () continuation))
         (niwa-session-checkpoint-seed
          session (niwa-session-artifacts-build-sidecar session (niwa-session-root-buffer session)))
         (funcall continuation)
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (test-niwa-publication-async--restored-mail session))
       (should-not (niwa-session-publication-busy-p session)))))

  :doc "expired target lease between reservation and head cannot publish tentative mail"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((reserve (symbol-function 'niwa-session-durability-reserve-publication-lease-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (funcall reserve owner
                             (lambda (ok err)
                               (setq continuation (lambda () (funcall callback ok err))))))))
         (test-niwa-publication-async--send
          session "expired" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () continuation))
         (let* ((directory (niwa-session-durability--lease-path (niwa-session-save-path session)))
                (lease (niwa-session-durability--lease-head directory)))
           (setf (plist-get lease :renewed-at) 1 (plist-get lease :expires-at) 2)
           (niwa-session-durability--write-generation directory lease))
         (funcall continuation)
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should-not (test-niwa-publication-async--mail session))
       (should-not (test-niwa-publication-async--restored-mail session))))))

(defun test-niwa-publication-async--shutdown (kind)
  "Drain a running and queued delivery through the lifecycle boundary KIND."
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((admit (symbol-function 'niwa-session-durability-admit-async))
           continuation acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (if continuation (funcall admit owner callback)
                      (funcall admit owner
                               (lambda (ok err)
                                 (setq continuation (lambda () (funcall callback ok err)))))))))
         (dolist (id '("closing-one" "closing-two"))
           (test-niwa-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (test-niwa-publication-async--wait (lambda () continuation))
         (should-not acknowledgements)
         (run-at-time 0.01 nil continuation)
         (pcase kind
           ('buffer
            (with-current-buffer buffer
              (add-hook 'kill-buffer-query-functions #'niwa-session-persistence-allow-buffer-close-p nil t)
              (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t)
              (should (kill-buffer buffer))))
           ('exit
            (should (niwa-session-persistence--allow-emacs-exit-p))
            (should-not (niwa-session-persistence--stop-publications)))))
       (should (equal '(("closing-one" t nil) ("closing-two" t nil))
                      (reverse acknowledgements)))
       (should-not (niwa-session-publication-busy-p session))
       (should (niwa-session-publication-stopping-p session))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session))))
       (should-error (test-niwa-publication-async--send session "too-late" #'ignore))))))

(niwa-deftest niwa-session-publication-stopping/async (:quiet t)
  ,test
  (test)
  :doc "buffer kill drains running and queued completions before releasing the lease"
  (test-niwa-publication-async--shutdown 'buffer)
  :doc "Emacs exit query and stop phase drain running and queued completions"
  (test-niwa-publication-async--shutdown 'exit))

(defun test-niwa-publication-async--unresolved (committed action &optional queued)
  "Make a head outcome unobservable, then apply recovery ACTION.
COMMITTED selects whether the real head write ran before its result was lost.
QUEUED adds a second completion while the first job is unresolved."
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((commit (symbol-function 'niwa-session-durability-commit-publication-head-async))
           (observe (symbol-function 'niwa-session-durability-observe-publication-head-async))
           (observations 0) acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head-async)
                  (lambda (owner head callback)
                    (if committed
                        (funcall commit owner head
                                 (lambda (ok err)
                                   (should ok) (should-not err)
                                   (funcall callback nil '(file-error "Lost head result"))))
                      (funcall callback nil '(file-error "Head write did not run")))))
                 ((symbol-function 'niwa-session-durability-observe-publication-head-async)
                  (lambda (owner callback)
                    (funcall observe owner
                             (lambda (_head _err)
                               (cl-incf observations)
                               (funcall callback nil '(file-error "Head temporarily unreadable")))))))
         (test-niwa-publication-async--send
          session "unresolved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (test-niwa-publication-async--wait
          (lambda ()
            (when-let* ((job (niwa-session-publication-job session)))
              (eq 'unresolved (niwa-session-publication--job-phase job))))))
       (should (= observations 1))
       (should-not acknowledgements)
       (should (niwa-session-publication-busy-p session))
       (should-error (niwa-session-publication-barrier session) :type 'user-error)
       (should (= (if committed 1 0)
                  (length (test-niwa-publication-async--restored-mail session))))
       (when queued
         (test-niwa-publication-async--send
          session "after-unresolved" (lambda (ok err) (push (list ok err) acknowledgements)))
         (should (niwa-session-publication-preparations session)))
       (pcase action
         ('retry
          (should (equal (niwa-session-publication-retry session)
                         (niwa-session-publication session))))
         ('abandon
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (niwa-session-publication-abandon session))))
       (test-niwa-publication-async--wait
        (lambda () (= (if queued 2 1) (length acknowledgements))))
       (when queued (should (equal '((t nil) (t nil)) acknowledgements)))
       (should (eq committed (and (caar acknowledgements) t)))
       (should-not (niwa-session-publication-busy-p session))
       (should-not (niwa-session-pending-publication session))
       (should (= (+ (if committed 1 0) (if queued 1 0))
                  (length (test-niwa-publication-async--mail session))))
       (should (= (+ (if committed 1 0) (if queued 1 0))
                  (length (test-niwa-publication-async--restored-mail session))))))))

(niwa-deftest niwa-session-publication-retry/async (:quiet t)
  ,test
  (test)
  :doc "explicit retry reconciles an unresolved committed head and completes its original ACK"
  (test-niwa-publication-async--unresolved t 'retry)
  :doc "retry succeeds when draining its queued successor advances the head again"
  (test-niwa-publication-async--unresolved t 'retry t))

(niwa-deftest niwa-session-publication-abandon/async (:quiet t)
  ,test
  (test)
  :doc "explicit abandonment releases an unresolved uncommitted job and fails its original ACK"
  (test-niwa-publication-async--unresolved nil 'abandon)

  :doc "abandoning retained checkpoint recovery deletes staging and permits close and exit"
  (dolist (kind '(buffer exit))
    (test-niwa-publication-async--fixture
     (lambda (session buffer)
       (let ((head (plist-get (niwa-session-publication session) :head))
             (commits 0) (observations 0) acknowledgements directories)
         (cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head-async)
                    (lambda (_owner _head callback)
                      (cl-incf commits)
                      (funcall callback nil '(file-error "Head write did not run"))))
                   ((symbol-function 'niwa-session-durability-observe-publication-head-async)
                    (lambda (_owner callback)
                      (cl-incf observations)
                      (funcall callback nil '(file-error "Head temporarily unreadable")))))
           (niwa-session-publication-submit
            session
            (lambda ()
              (test-niwa-publication-async--transcript-artifacts
               session buffer "segment-9.chat.org" "Uncommitted checkpoint"))
            (lambda (ok err) (push (list ok err) acknowledgements)) nil t)
           (test-niwa-publication-async--wait
            (lambda ()
              (when-let* ((job (niwa-session-publication-job session)))
                (eq 'unresolved (niwa-session-publication--job-phase job))))))
         (should (= commits 1))
         (should (= observations 1))
         (should-not acknowledgements)
         (should (niwa-session-pending-publication session))
         (setq directories
               (mapcar (lambda (batch) (plist-get batch :directory))
                       (niwa-session-publication--job-batches
                        (niwa-session-publication-job session))))
         (should directories)
         (dolist (directory directories) (should (file-directory-p directory)))
         (with-current-buffer buffer
           (should-not (niwa-session-persistence-allow-buffer-close-p)))
         (should-not (niwa-session-persistence--allow-emacs-exit-p))
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should (niwa-session-publication-abandon session)))
         (should (equal '((nil (error "Session publication was explicitly abandoned")))
                        acknowledgements))
         (should-not (niwa-session-pending-publication session))
         (should-not (niwa-session-publication-job session))
         (should-not (niwa-session-publication-busy-p session))
         (should-not (niwa-session-publication-active-p session))
         (should-not (niwa-session-publication-uncommitted-batches session))
         (should-not (niwa-session-publication-queue session))
         (dolist (directory directories) (should-not (file-exists-p directory)))
         (should (equal head (plist-get (niwa-session-publication session) :head)))
         (should (niwa-session-persistence--allow-emacs-exit-p))
         (with-current-buffer buffer
           (should (niwa-session-persistence-allow-buffer-close-p)))
         (pcase kind
           ('buffer
            (with-current-buffer buffer
              (add-hook 'kill-buffer-query-functions
                        #'niwa-session-persistence-allow-buffer-close-p nil t)
              (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t)
              (should (kill-buffer buffer))))
           ('exit (should-not (niwa-session-persistence--stop-publications))))
         (accept-process-output nil 0.05)
         (should (= 1 (length acknowledgements)))
         (should-not (niwa-session-pending-publication session)))))))

(niwa-deftest niwa-agent-control-reoffer-mail/async (:quiet t)
  ,test
  (test)
  :doc "a close veto after nested drains reoffers two suppressed wakes once in FIFO order"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((notify (symbol-function 'niwa-agent-control-notify))
           (stop (symbol-function 'niwa-session-publication-stopping))
           reasons wake-tokens acknowledgements stopping-states offers vetoed nested)
       (niwa-agent-control-wait
        session (lambda (reason)
                  (push (niwa-session-publication-preparation-token session) wake-tokens)
                  (push reason reasons)) 10000)
       (should (niwa-agent-control-root-waiting-p session))
       (cl-letf (((symbol-function 'niwa-agent-control-notify)
                  (lambda (owner key function)
                    (funcall notify owner key
                             (lambda () (push (cadr key) offers) (funcall function)))))
                 ((symbol-function 'niwa-session-publication-stopping)
                  (lambda (owner)
                    (prog1 (funcall stop owner)
                      (unless nested
                        (setq nested t)
                        (unwind-protect
                            (niwa-session-persistence--drain-temporarily owner)
                          (setq nested nil)))))))
         (dolist (id '("veto-first" "veto-second"))
           (test-niwa-publication-async--send
            session id
            (lambda (ok err)
              (push (niwa-session-publication-stopping-p session) stopping-states)
              (push (list id ok err) acknowledgements))))
         (with-current-buffer buffer
           (setq-local kill-buffer-query-functions
                       (list #'niwa-session-persistence-allow-buffer-close-p
                             (lambda () (setq vetoed t) nil)))
           (should-not (kill-buffer buffer))))
       (should vetoed)
       (should (buffer-live-p buffer))
       (should (equal '(t t) stopping-states))
       (should (equal '(("veto-first" t nil) ("veto-second" t nil))
                      (reverse acknowledgements)))
       (should-not (niwa-session-publication-stopping-p session))
       (should-not reasons)
       (should-not offers)
       (should (= 2 (length (niwa-session-mail-notifications session))))
       (let ((timer (niwa-session-mail-notification-timer session)))
         (should (timerp timer))
         (niwa-session-persistence--drain-temporarily session)
         (niwa-agent-control-reoffer-mail session)
         (should (eq timer (niwa-session-mail-notification-timer session))))
       (test-niwa-publication-async--wait (lambda () (= 2 (length offers))))
       (should (equal (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                             '("veto-first" "veto-second"))
                      (reverse offers)))
       (should (equal '(mailbox) reasons))
       (should (equal '(nil) wake-tokens))
       (should-not (niwa-agent-control-root-waiting-p session))
       (niwa-agent-control-reoffer-mail session)
       (accept-process-output nil 0.02)
       (should (= 2 (length offers)))
       (should (equal '(mailbox) reasons))
       (should-not (niwa-session-mail-notification-timer session))
       (should-not (niwa-session-mail-notifications session))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "a completed close suppresses the query's deferred wake reoffer"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (reasons acknowledgements)
       (niwa-agent-control-wait session (lambda (reason) (push reason reasons)) 10000)
       (test-niwa-publication-async--send
        session "closed-with-mail" (lambda (ok err) (push (list ok err) acknowledgements)))
       (with-current-buffer buffer
         (setq-local kill-buffer-query-functions
                     (list #'niwa-session-persistence-allow-buffer-close-p)
                     kill-buffer-hook
                     (list #'niwa-session-persistence-release-on-kill
                           #'niwa-view--on-data-killed))
         (should (kill-buffer buffer)))
       (should-not (buffer-live-p buffer))
       (should (equal '((t nil)) acknowledgements))
       (should (niwa-session-publication-stopping-p session))
       (should-not (niwa-session-mail-notifications session))
       (should-not (niwa-session-mail-notification-timer session))
       (accept-process-output nil 0.05)
       (should-not reasons)
       (should (= 1 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "an idle close query with no suppressed offers allocates no notification timer"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((timers (append timer-list timer-idle-list)))
       (with-current-buffer buffer
         (should (niwa-session-persistence-allow-buffer-close-p)))
       (should-not (niwa-session-mail-notifications session))
       (should-not (niwa-session-mail-notification-timer session))
       (should-not (seq-difference (append timer-list timer-idle-list) timers))))))

(defun test-niwa-publication-async--notification-cancel (uninstall)
  "Cancel transport with pending notifications, using full UNINSTALL if non-nil."
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((niwa-transport--enabled-p t)
           (admit (symbol-function 'niwa-session-durability-admit-async))
           (wake (symbol-function 'niwa-agent-control--wake))
           (admissions 0) (offers 0) continuation reasons acknowledgements)
       (niwa-agent-control-wait session (lambda (reason) (push reason reasons)) 10000)
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (cl-incf admissions)
                    (funcall admit owner
                             (lambda (ok err)
                               (if (= admissions 1) (funcall callback ok err)
                                 (setq continuation (lambda () (funcall callback ok err))))))))
                 ((symbol-function 'niwa-agent-control--wake)
                  (lambda (&rest arguments) (cl-incf offers) (apply wake arguments))))
         (dolist (id '("cancel-notify-one" "cancel-notify-two"))
           (test-niwa-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (test-niwa-publication-async--wait (lambda () continuation))
         ;; The first wake ran when the gate opened before the second job.
         (should (equal '(mailbox) reasons))
         (should (= offers 1))
         (should-not (niwa-session-mail-notifications session))
         ;; Reoffered while that job is in flight, it waits in the FIFO.
         (niwa-agent-control--deliver-mail
          session "/root" (car (test-niwa-publication-async--mail session)))
         (should (= 1 (length (niwa-session-mail-notifications session))))
         (should (niwa-session-mail-notification-timer session))
         (if uninstall
             (progn (niwa-uninstall) (should-not niwa-transport--enabled-p))
           (niwa-transport-cancel-pending))
         (funcall continuation)
         (test-niwa-publication-async--wait
          (lambda () (and (= 2 (length acknowledgements)) (= offers 3))))
         (accept-process-output nil 0.1)
         (should (= offers 3))
         (should (equal '(mailbox) reasons))
         (should (equal '(("cancel-notify-one" t nil) ("cancel-notify-two" t nil))
                        (reverse acknowledgements)))
         (should-not (niwa-session-publication-busy-p session))
         (should-not (niwa-session-mail-notifications session))
         (should-not (niwa-session-mail-notification-timer session))
         (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))))

(niwa-deftest niwa-agent-control--deliver-mail/async (:quiet t)
  ,test
  (test)
  :doc "a queued delivery wakes its waiter once outside the preparation token"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (reasons wake-tokens acknowledgements)
       (niwa-agent-control-wait
        session (lambda (reason)
                  (push (niwa-session-publication-preparation-token session) wake-tokens)
                  (push reason reasons)) 10000)
       (dolist (id '("wake-first" "wake-second"))
         (test-niwa-publication-async--send
          session id (lambda (ok err) (push (list id ok err) acknowledgements))))
       (test-niwa-publication-async--wait
        (lambda () (and (= 2 (length acknowledgements)) reasons)))
       (niwa-session-publication-barrier session)
       (accept-process-output nil 0.05)
       (should (equal '(mailbox) reasons))
       (should (equal '(nil) wake-tokens))
       (should (equal '(("wake-first" t nil) ("wake-second" t nil))
                      (reverse acknowledgements)))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session)))))))

  :doc "global transport cancellation cannot strand an accepted coalesced notification"
  (test-niwa-publication-async--notification-cancel nil)

  :doc "full uninstall disables transport without stranding accepted coalesced notifications"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path)
                (when (native-comp-available-p)
                  (startup-redirect-eln-cache (file-name-concat (getenv "HOME") ".cache" "eln")))
                (require 'niwa)
                (load ,(symbol-file 'test-niwa-publication-async--notification-cancel 'defun) nil t)
                (let (diagnostics)
                  (niwa-test--with-captured-diagnostics diagnostics
                    (test-niwa-publication-async--notification-cancel t))
                  (should (equal "niwa uninstalled successfully\n" diagnostics))))))))
      (ert-info ((buffer-string))
        (should (equal 0 status))
        (should (equal "" (buffer-string))))))

  :doc "successive busy jobs offer each notification at the next gate opening in FIFO order"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((admit (symbol-function 'niwa-session-durability-admit-async))
           (notify (symbol-function 'niwa-agent-control-notify))
           (wake (symbol-function 'niwa-agent-control--wake))
           (admissions 0) continuation delivering offers acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (cl-incf admissions)
                    (funcall admit owner
                             (lambda (ok err)
                               (if (= admissions 1) (funcall callback ok err)
                                 (setq continuation (lambda () (funcall callback ok err))))))))
                 ((symbol-function 'niwa-agent-control-notify)
                  (lambda (owner key function)
                    (funcall notify owner key
                             (lambda ()
                               (let ((previous delivering))
                                 (setq delivering (cadr key))
                                 (unwind-protect (funcall function)
                                   (setq delivering previous)))))))
                 ((symbol-function 'niwa-agent-control--wake)
                  (lambda (&rest arguments)
                    ;; Queued successors may wait, but nothing runs in flight.
                    (should-not (niwa-session-publication-preparation-token session))
                    (should-not (niwa-session-publication-job session))
                    (should-not (niwa-session-publication-active-p session))
                    (push delivering offers)
                    (apply wake arguments))))
         (dolist (id '("fifo-one" "fifo-two" "fifo-three"))
           (test-niwa-publication-async--send
            session id (lambda (ok err) (push (list id ok err) acknowledgements))))
         (dotimes (stage 2)
           ;; The next job is held in admission, so the queue stayed busy
           ;; throughout; the earlier notification still ran when the gate
           ;; opened, before that job started.
           (test-niwa-publication-async--wait (lambda () continuation))
           (should (= (1+ stage) (length acknowledgements)))
           (should (= (1+ stage) (length offers)))
           (should-not (niwa-session-mail-notifications session))
           (should-not (niwa-session-mail-notification-timer session))
           (let ((resume continuation))
             (setq continuation nil)
             (funcall resume)))
         (test-niwa-publication-async--wait (lambda () (= 3 (length offers))))
         (accept-process-output nil 0.05)
         (should (equal (mapcar (lambda (id) (secure-hash 'sha256 (concat "/root:" id)))
                               '("fifo-one" "fifo-two" "fifo-three"))
                        (reverse offers)))
         (should (equal '(("fifo-one" t nil) ("fifo-two" t nil) ("fifo-three" t nil))
                        (reverse acknowledgements)))
         (should-not (niwa-session-mail-notifications session))
         (should-not (niwa-session-mail-notification-timer session))
         (should-not (niwa-session-publication-busy-p session))
         (should (= 3 (length (test-niwa-publication-async--restored-mail session)))))))))

(niwa-deftest niwa-session-publication--job-complete/finishing (:quiet t)
  ,test
  (test)
  :doc "yielding acknowledgement and cleanup keep wakes and successors outside finalization"
  (dolist (phase '(ack cleanup))
    (dolist (cancel '(nil t))
      (test-niwa-publication-async--fixture
       (lambda (session _buffer)
         (let ((complete (symbol-function 'niwa-session-publication--job-complete))
               (deliver (symbol-function 'niwa-agent-control--deliver-mail))
               (delete (symbol-function 'niwa-session-publication--delete-batches))
               finishing injected yielded observations acknowledgements premature-gates diagnostics)
           (let ((niwa-session-publication-gate-open-functions
                  (cons (lambda (_session) (when finishing (push t premature-gates)))
                        niwa-session-publication-gate-open-functions)))
             (cl-labels
                 ((yield-once ()
                    (when (and (not injected)
                               (niwa-session-publication-preparations session))
                      (setq injected t)
                      ;; Force both dispatch paths due now, then wait until a
                      ;; later timer ran.  No wall-clock delay is the oracle.
                      (cancel-timer (niwa-session-mail-notification-timer session))
                      (setf (niwa-session-mail-notification-timer session)
                            (run-at-time 0 nil #'niwa-agent-control--flush-notifications session))
                      (niwa-session-publication--schedule session)
                      (run-at-time 0 nil (lambda () (setq yielded t)))
                      (test-niwa-publication-async--wait (lambda () yielded))
                      (when cancel (signal 'quit nil)))))
               (niwa-test--with-captured-diagnostics diagnostics
                 (cl-letf (((symbol-function 'niwa-session-publication--job-complete)
                            (lambda (&rest args)
                              (let ((previous finishing))
                                (setq finishing t)
                                (unwind-protect (apply complete args)
                                  (setq finishing previous)))))
                           ((symbol-function 'niwa-agent-control--deliver-mail)
                            (lambda (&rest args)
                              (prog1 (apply deliver args)
                                (when (eq phase 'ack) (yield-once)))))
                           ((symbol-function 'niwa-session-publication--delete-batches)
                            (lambda (&rest args)
                              (when (eq phase 'cleanup) (yield-once))
                              (apply delete args))))
                   (niwa-agent-control-wait
                    session
                    (lambda (reason)
                      (push (list reason finishing
                                  (length (niwa-session-publication-preparations session)))
                            observations)) 10000)
                   (dolist (id '("finish-first" "finish-second"))
                     (test-niwa-publication-async--send
                      session id (lambda (ok err) (push (list ok err) acknowledgements))))
                   (test-niwa-publication-async--wait
                    (lambda () (and (= 2 (length acknowledgements)) observations)))))))
           (should injected)
           (should yielded)
           (should-not premature-gates)
           (should (equal '((mailbox nil 1)) observations))
           (should (equal '((t nil) (t nil)) acknowledgements))
           (should (= 2 (length (test-niwa-publication-async--restored-mail session))))
           (should-not (niwa-session-publication-finishing-p session))
           (should-not (niwa-session-publication-busy-p session))
           (if cancel
               (should (string-match-p
                        (if (eq phase 'ack)
                            "Publication acknowledgement failed: Quit"
                          "Session publication cleanup failed: Quit") diagnostics))
             (should (equal "" diagnostics))))))))

  :doc "an acknowledgement's first successor queues until finalization returns"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((complete (symbol-function 'niwa-session-publication--job-complete))
           (admit (symbol-function 'niwa-session-durability-admit-async))
           finishing premature-admissions queued yielded acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--job-complete)
                  (lambda (&rest args)
                    (let ((previous finishing))
                      (setq finishing t)
                      (unwind-protect (apply complete args)
                        (setq finishing previous)))))
                 ((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (&rest args)
                    (when finishing (push t premature-admissions))
                    (apply admit args))))
         (test-niwa-publication-async--send
          session "ack-parent"
          (lambda (ok err)
            (push (list ok err) acknowledgements)
            (test-niwa-publication-async--send
             session "ack-child" (lambda (success error) (push (list success error) acknowledgements)))
            (setq queued (and (not (niwa-session-publication-job session))
                              (= 1 (length (niwa-session-publication-preparations session)))))
            (run-at-time 0 nil (lambda () (setq yielded t)))
            (test-niwa-publication-async--wait (lambda () yielded))))
         (test-niwa-publication-async--wait (lambda () (= 2 (length acknowledgements)))))
       (should queued)
       (should yielded)
       (should-not premature-admissions)
       (should (equal '((t nil) (t nil)) acknowledgements))
       (should-not (niwa-session-publication-finishing-p session))
       (should-not (niwa-session-publication-busy-p session))
       (should (= 2 (length (test-niwa-publication-async--restored-mail session))))))))

(niwa-deftest niwa-agent-control--notification-blocked-p/async (:quiet t)
  ,test
  (test)
  :doc "notification admission respects both synchronous ownership and transport activity"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (should-not (niwa-agent-control--notification-blocked-p session))
     (setf (niwa-session-publication-active-p session) t)
     (should (niwa-agent-control--notification-blocked-p session))
     (setf (niwa-session-publication-active-p session) nil)
     (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (_) t)))
       (should (niwa-agent-control--notification-blocked-p session)))
     (should-not (niwa-agent-control--notification-blocked-p session))))
  :doc "work in flight blocks notifications but queued preparations alone do not"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (unwind-protect
         (progn
           (setf (niwa-session-publication-preparations session)
                 (list (cons #'ignore nil)))
           (should (niwa-session-publication-busy-p session))
           (should-not (niwa-agent-control--notification-blocked-p session))
           (setf (niwa-session-publication-preparation-token session) '(running))
           (should (niwa-agent-control--notification-blocked-p session)))
       (setf (niwa-session-publication-preparation-token session) nil
             (niwa-session-publication-preparations session) nil)))))

(niwa-deftest niwa-agent-control--queue-notification/async (:quiet t)
  ,test
  (test)
  :doc "duplicate keys coalesce without replacing the original FIFO callback or timer"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (offers)
       (niwa-agent-control--queue-notification session 'first (lambda () (push 'first offers)))
       (let ((timer (niwa-session-mail-notification-timer session)))
         (niwa-agent-control--queue-notification session 'first (lambda () (push 'duplicate offers)))
         (niwa-agent-control--queue-notification session 'second (lambda () (push 'second offers)))
         (should (eq timer (niwa-session-mail-notification-timer session))))
       (should (= 2 (length (niwa-session-mail-notifications session))))
       (test-niwa-publication-async--wait (lambda () (= 2 (length offers))))
       (should (equal '(first second) (reverse offers)))
       (should-not (niwa-session-mail-notification-timer session))
       (should-not (niwa-session-mail-notification-delay session))))))

(niwa-deftest niwa-agent-control--flush-notifications/async (:quiet t)
  ,test
  (test)
  :doc "a temporary stop retains deferred wake offers without a timer until reoffered"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (offered)
       (niwa-agent-control--queue-notification session 'stopped (lambda () (setq offered t)))
       (setf (niwa-session-publication-stopping-p session) t)
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-mail-notification-timer session))))
       (should-not offered)
       (should (= 1 (length (niwa-session-mail-notifications session))))
       (setf (niwa-session-publication-stopping-p session) nil)
       (niwa-agent-control-reoffer-mail session)
       (should-not offered)
       (test-niwa-publication-async--wait (lambda () offered))
       (should-not (niwa-session-mail-notifications session)))))

  :doc "a notification timer cannot inherit another transaction's dynamically bound authority"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (inherited)
       (niwa-agent-control--queue-notification
        session 'authority
        (lambda ()
          (setq inherited
                (list niwa-session-durability--transaction-clock
                      niwa-session-durability--asserted-directories
                      niwa-session-durability--commit-riders
                      niwa-session-durability--final-commit
                      niwa-session-durability-async-process-function
                      niwa-session-recovery--mutation-cache))))
       (let ((niwa-session-durability--transaction-clock '(sentinel))
             (niwa-session-durability--asserted-directories '(sentinel))
             (niwa-session-durability--commit-riders '(sentinel))
             (niwa-session-durability--final-commit 'sentinel)
             (niwa-session-durability-async-process-function #'ignore)
             (niwa-session-recovery--mutation-cache '(sentinel)))
         (test-niwa-publication-async--wait (lambda () inherited)))
       (should (equal '(nil nil nil nil nil nil) inherited))))))

(niwa-deftest niwa-agent-control--cancel-notifications/async (:quiet t)
  ,test
  (test)
  :doc "teardown cancels the owned timer and clears every notification slot"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (offered)
       (niwa-agent-control--queue-notification session 'cancelled (lambda () (setq offered t)))
       (let ((timer (niwa-session-mail-notification-timer session)))
         (niwa-agent-control-teardown-session session)
         (should-not (memq timer timer-list)))
       (accept-process-output nil 0.1)
       (should-not offered)
       (should-not (niwa-session-mail-notifications session))
       (should-not (niwa-session-mail-notification-timer session))
       (should-not (niwa-session-mail-notification-delay session))))))

(niwa-deftest niwa-session-publication-busy-p/async (:quiet t)
  ,test
  (test)
  :doc "accepted jobs stay busy until their acknowledgements settle"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acknowledged)
       (should-not (niwa-session-publication-busy-p session))
       (test-niwa-publication-async--send
        session "busy" (lambda (ok err) (should ok) (should-not err) (setq acknowledged t)))
       (should (niwa-session-publication-busy-p session))
       (test-niwa-publication-async--wait (lambda () acknowledged))
       (should-not (niwa-session-publication-busy-p session))))))

(niwa-deftest niwa-session-publication-async-eligible-p/async (:quiet t)
  ,test
  (test)
  :doc "only warm local portable sessions with a live root are eligible"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (should (niwa-session-publication-async-eligible-p session))
     (let ((path (niwa-session-save-path session))
           (root (niwa-session-root-buffer session))
           (publication (niwa-session-publication session)))
       (unwind-protect
           (progn
             (setf (niwa-session-save-path session) "/ssh:uncontacted.invalid:/session/")
             (should-not (niwa-session-publication-async-eligible-p session))
             (setf (niwa-session-save-path session) path
                   (niwa-session-publication session) nil)
             (should-not (niwa-session-publication-async-eligible-p session))
             (setf (niwa-session-publication session) publication
                   (niwa-session-root-buffer session) nil)
             (should-not (niwa-session-publication-async-eligible-p session))
             (setf (niwa-session-root-buffer session) root
                   (niwa-session-publication-stopping-p session) t)
             (should-not (niwa-session-publication-async-eligible-p session)))
         (setf (niwa-session-save-path session) path
               (niwa-session-root-buffer session) root
               (niwa-session-publication session) publication
               (niwa-session-publication-stopping-p session) nil))))))

(niwa-deftest niwa-session-publication--write-diagnostics/async (:quiet t)
  ,test
  (test)
  :doc "exit drain persists an append queued behind an in-flight append and empties the backlog"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let ((path (file-name-concat (niwa-session-save-path session) "exit-diagnostics.log"))
           (niwa-session-publication--diagnostic-backlog (make-hash-table :test #'eq))
           (niwa-session-control-fs--async-jobs nil)
           (niwa-session-control-fs--async-wait nil))
       (niwa-session-publication--write-diagnostics
        session (list (cons path "first\n")))
       ;; No event-loop opportunity has occurred: the first program has not
       ;; delivered, so its successor must join the backlog, not start I/O.
       (should niwa-session-control-fs--async-jobs)
       (should (car (gethash session niwa-session-publication--diagnostic-backlog)))
       (niwa-session-publication--write-diagnostics
        session (list (cons path "second\n")))
       (should (equal (list (cons path "second\n"))
                      (cdr (gethash session niwa-session-publication--diagnostic-backlog))))
       (should (niwa-session-publication-diagnostic-backlog-p session))
       (let ((niwa-session-control-fs--async-wait t))
         (niwa-session-publication--write-diagnostics session nil))
       ;; Assert immediately after the exit flush, without a later save or
       ;; explicit wait that could conceal a stranded append.
       (should-not niwa-session-control-fs--async-jobs)
       (should-not (car (gethash session niwa-session-publication--diagnostic-backlog)))
       (should-not (niwa-session-publication-diagnostic-backlog-p session))
       (should (equal "first\nsecond\n"
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))))))

(defun test-niwa-publication-async--staged-submit
    (session buffer select &optional logical content retain)
  "Submit LOGICAL's CONTENT through SESSION with SELECT; return the ack list."
  (let (acknowledgements)
    (niwa-session-publication-submit
     session
     (lambda ()
       (test-niwa-publication-async--transcript-artifacts
        session buffer (or logical "segment-8.chat.org") (or content "Staged text")))
     (lambda (ok err) (push (list ok err) acknowledgements))
     nil retain select)
    (test-niwa-publication-async--wait (lambda () acknowledgements))
    acknowledgements))

(niwa-deftest niwa-session-publication--write-staged-content ()
  ,test
  (test)
  :doc "writes exactly the encoded bytes for Unicode, codings, raw and unibyte content"
  (let ((file (make-temp-file "niwa-staged-write-"))
        (annotated nil))
    (unwind-protect
        (cl-letf (((default-value 'write-region-annotate-functions)
                   (list (lambda (&rest _) (setq annotated t) nil))))
          (dolist (spec `(("λ ünï \U0001F600\nline" utf-8-unix)
                          ("one\ntwo\n" utf-8-dos)
                          (,(string-to-multibyte "raw\200\377") utf-8-unix)
                          (,(unibyte-string 0 1 128 255 10) nil)))
            (let ((content (car spec)) (coding (cadr spec)))
              (with-temp-buffer
                (setq-local buffer-file-coding-system 'iso-8859-1)
                (insert "live buffer text")
                (let ((before (buffer-string)) (point (point)))
                  (niwa-session-publication--write-staged-content
                   file content coding)
                  (should (equal before (buffer-string)))
                  (should (= point (point)))))
              (should (equal (if (multibyte-string-p content)
                                 (encode-coding-string content (or coding 'utf-8-unix))
                               content)
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (buffer-string))))))
          (should-not annotated))
      (delete-file file))))

(niwa-deftest niwa-session-publication--valid-digest-p ()
  ,test
  (test)
  :doc "accepts only lowercase 64-character hexadecimal digests"
  (should (niwa-session-publication--valid-digest-p (secure-hash 'sha256 "x")))
  (dolist (bad (list nil 1 "" (make-string 64 ?A) (make-string 63 ?a)
                     (make-string 65 ?a) (concat (make-string 63 ?a) "g")
                     (concat (make-string 64 ?a) "\n")))
    (should-not (niwa-session-publication--valid-digest-p bad))))

(niwa-deftest niwa-session-publication--require-digests ()
  ,test
  (test)
  :doc "demands a valid digest on every written artifact and ignores deletions"
  (let ((digest (secure-hash 'sha256 "x")))
    (should-not (niwa-session-publication--require-digests
                 (list (list :logical "a" :source-sha256 digest)
                       (list :logical "b" :delete t))))
    (dolist (bad (list (list :logical "a")
                       (list :logical "a" :source-sha256 "short")))
      (should-error (niwa-session-publication--require-digests (list bad))))))

(niwa-deftest niwa-session-publication--job-digest (:quiet t)
  ,test
  (test)
  :doc "hashes staged files with the real control program and never on the main thread"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (acknowledgements)
       (cl-letf (((symbol-function 'niwa-session-publication--files-sha256)
                  (lambda (&rest _) (error "Main-thread hash")))
                 ((symbol-function 'niwa-session-publication--file-sha256)
                  (lambda (&rest _) (error "Main-thread hash")))
                 ((symbol-function 'niwa-session-publication--generation-digests)
                  (lambda (&rest _) (error "Main-thread hash"))))
         (setq acknowledgements
               (test-niwa-publication-async--staged-submit
                session buffer nil "segment-9.chat.org" "λ digest me")))
       (should (equal '((t nil)) acknowledgements))
       (should (equal (encode-coding-string "λ digest me" 'utf-8-unix)
                      (niwa-session-artifacts-read-artifact
                       session "segment-9.chat.org" t))))))

  :doc "fails the job and rolls back on a failed, missing or malformed result"
  (dolist (outcome (list (lambda (_ops callback &optional _scope)
                           (funcall callback nil '(file-error "Injected hash failure")) nil)
                         (lambda (_ops callback &optional _scope) (funcall callback nil nil) nil)
                         (lambda (ops callback &optional _scope)
                           (funcall callback
                                    (mapcar (lambda (_) (list :status 'ok :value (list :sha256 "bad")))
                                            ops)
                                    nil)
                           nil)
                         (lambda (ops callback &optional _scope)
                           (funcall callback
                                    (mapcar (lambda (_) (list :status 'failed)) ops) nil)
                           nil)))
    (test-niwa-publication-async--fixture
     (lambda (session buffer)
       (let ((head (plist-get (niwa-session-publication session) :head)))
         (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                    outcome))
           (let ((acknowledgements
                  (test-niwa-publication-async--staged-submit
                   session buffer nil "segment-9.chat.org" "never published")))
             (should-not (caar acknowledgements))
             (should (cadar acknowledgements))))
         (should (equal head (plist-get (niwa-session-publication session) :head)))
         (should-not (niwa-session-publication-busy-p session))))))
  :doc "hashes undigested sources of retained earlier batches with the same program"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((save-path (niwa-session-save-path session))
            (retained (niwa-session-publication--stage-artifacts
                       session
                       (list (list :path (file-name-concat save-path "segment-6.chat.org")
                                   :content "λ retained"))))
            (hashed nil))
       (should-not (plist-get (car (plist-get retained :artifacts)) :source-sha256))
       (niwa-session-publication--retain-uncommitted-batch session retained)
       (cl-letf* ((orig (symbol-function 'niwa-session-control-fs-run-program-async))
                  ((symbol-function 'niwa-session-control-fs-run-program-async)
                   (lambda (operations callback &optional scope)
                     (setq hashed (append hashed
                                          (mapcar (lambda (op) (plist-get op :path))
                                                  operations)))
                     (funcall orig operations callback scope))))
         (should (equal '((t nil))
                        (test-niwa-publication-async--staged-submit
                         session buffer nil "segment-5.chat.org" "new"))))
       (should (cl-some (lambda (path) (string-match-p "niwa-publication-" path)) hashed))
       ;; Retained and new sources both went through the one program.
       (should (>= (length hashed) 2))
       (should (equal (encode-coding-string "λ retained" 'utf-8-unix)
                      (niwa-session-artifacts-read-artifact
                       session "segment-6.chat.org" t)))
       (should (equal "new" (niwa-session-artifacts-read-artifact
                             session "segment-5.chat.org" t))))))

  :doc "a live root edit during the digest cannot change the frozen published bytes"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((orig (symbol-function 'niwa-session-control-fs-run-program-async))
           edited)
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                  (lambda (operations callback &optional scope)
                    (unless edited
                      (setq edited t)
                      (with-current-buffer buffer
                        (goto-char (point-max))
                        (insert "LIVE EDIT DURING DIGEST")))
                    (funcall orig operations callback scope))))
         (should (equal '((t nil))
                        (test-niwa-publication-async--staged-submit
                         session buffer nil "segment-4.chat.org" "frozen text"))))
       (should edited)
       (should (equal "frozen text"
                      (niwa-session-artifacts-read-artifact
                       session "segment-4.chat.org" t)))
       (with-current-buffer buffer
         (should (string-match-p "LIVE EDIT DURING DIGEST" (buffer-string))))))))

(niwa-deftest niwa-session-publication--job-select (:quiet t)
  ,test
  (test)
  :doc "a nil selector is a clean no-op that commits nothing and leaves no staging"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           directories)
       (cl-letf* ((orig (symbol-function 'niwa-session-publication--stage-artifacts))
                  ((symbol-function 'niwa-session-publication--stage-artifacts)
                   (lambda (&rest args)
                     (let ((batch (apply orig args)))
                       (push (plist-get batch :directory) directories)
                       batch))))
         (should (equal '((t nil))
                        (test-niwa-publication-async--staged-submit
                         session buffer (lambda (_batch) nil)))))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should directories)
       (dolist (directory directories) (should-not (file-exists-p directory)))
       (should-not (niwa-session-publication-busy-p session)))))

  :doc "the selector sees digests, its batch alone is published and the original is cleaned"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let (seen original-directory)
       (should (equal
                '((t nil))
                (test-niwa-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (setq seen (mapcar (lambda (artifact) (plist-get artifact :source-sha256))
                                      (plist-get batch :artifacts))
                         original-directory (plist-get batch :directory))
                   (let ((copy (copy-sequence batch)))
                     (setf (plist-get copy :artifacts)
                           (cl-remove-if
                            (lambda (artifact)
                              (equal (plist-get artifact :logical) "notes/deep/extra.txt"))
                            (plist-get batch :artifacts)))
                     copy))
                 "segment-7.chat.org" "kept")))
       (should (cl-every #'niwa-session-publication--valid-digest-p seen))
       (should-not (file-exists-p original-directory))
       (should (equal "kept" (niwa-session-artifacts-read-artifact
                              session "segment-7.chat.org" t)))
       (should-not (niwa-session-artifacts-artifact-present-p
                    session "notes/deep/extra.txt" t)))))

  :doc "a selected batch without a valid digest fails before anything is reserved"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           reserved)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (&rest _) (setq reserved t))))
         ;; The same batch comes back, but its commit marker was pruned.
         (let ((acknowledgements
                (test-niwa-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (setf (plist-get batch :artifacts)
                         (cl-remove-if (lambda (artifact) (plist-get artifact :commit-marker))
                                       (plist-get batch :artifacts)))
                   batch))))
           (should-not (caar acknowledgements))
           (should (cadar acknowledgements))))
       (should-not reserved)
       (should (equal head (plist-get (niwa-session-publication session) :head))))))

  :doc "a selected batch without a valid digest fails before anything is reserved"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (niwa-session-publication session) :head))
           reserved)
       (cl-letf (((symbol-function 'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (&rest _) (setq reserved t))))
         (let ((acknowledgements
                (test-niwa-publication-async--staged-submit
                 session buffer
                 (lambda (batch)
                   (let ((copy (copy-sequence batch)))
                     (setf (plist-get copy :artifacts)
                           (mapcar (lambda (artifact)
                                     (let ((a (copy-sequence artifact)))
                                       (setf (plist-get a :source-sha256) "forged")
                                       a))
                                   (plist-get batch :artifacts)))
                     copy)))))
           (should-not (caar acknowledgements))
           (should (cadar acknowledgements))))
       (should-not reserved)
       (should (equal head (plist-get (niwa-session-publication session) :head))))))
  :doc "a nil selector is refused while retained recovery is pending and keeps it"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((save-path (niwa-session-save-path session))
            (retained (niwa-session-publication--stage-artifacts
                       session
                       (list (list :path (file-name-concat save-path "segment-3.chat.org")
                                   :content "retained recovery"))))
            (head (plist-get (niwa-session-publication session) :head)))
       (niwa-session-publication--retain-uncommitted-batch session retained)
       (let ((acknowledgements
              (test-niwa-publication-async--staged-submit
               session buffer (lambda (_batch) nil))))
         (should-not (caar acknowledgements))
         (should (cadar acknowledgements)))
       (should (equal head (plist-get (niwa-session-publication session) :head)))
       (should (= 1 (length (niwa-session-publication-uncommitted-batches session))))
       (should (file-exists-p (plist-get retained :directory)))
       (should-not (niwa-session-publication-busy-p session)))))
)

(niwa-deftest niwa-session-publication-submit/warm-admission (:quiet t)
  ,test
  (test)
  :doc "a real warm admission decline reaches the acknowledgement with its cause"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((directory (niwa-session-save-path session))
            (head (plist-get (niwa-session-publication-read directory) :head))
            (lease (copy-sequence (niwa-session-lease session)))
            (admit (symbol-function 'niwa-session-durability-admit-async))
            admissions acknowledgements)
       ;; A recovery marker root on the target: the warm program observes it.
       (make-directory (file-name-concat directory ".recovery") t)
       ;; Record the real admission's answer; its behaviour is unchanged.
       (cl-letf (((symbol-function 'niwa-session-durability-admit-async)
                  (lambda (owner callback)
                    (funcall admit owner
                             (lambda (ok error)
                               (push (list ok error) admissions)
                               (funcall callback ok error))))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "declined transcript"))
          (lambda (ok error) (push (list ok error) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should (= 1 (length admissions)))
       (should-not (caar admissions))
       (should (= 1 (length acknowledgements)))
       (should-not (caar acknowledgements))
       (should (eq 'user-error (car (cadar acknowledgements))))
       (should (equal "Portable session has a pending recovery marker"
                      (error-message-string (cadar acknowledgements))))
       (should (equal head (plist-get (niwa-session-publication-read directory) :head)))
       (should (equal lease (niwa-session-lease session)))
       (should-not (niwa-session-artifacts-artifact-present-p
                    session "segment-9.chat.org" t))
       (should-not (niwa-session-publication-busy-p session))))))

(niwa-deftest niwa-session-publication-submit/lease-update-decline (:quiet t)
  ,test
  (test)
  :doc "a real lease-update decline reaches the acknowledgement with its cause"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let* ((directory (niwa-session-save-path session))
            (reserve (symbol-function
                      'niwa-session-durability-reserve-publication-lease-async))
            acknowledgements)
       ;; Another client takes the bound generation after admission, before
       ;; the reservation reads it.
       (cl-letf (((symbol-function
                   'niwa-session-durability-reserve-publication-lease-async)
                  (lambda (owner callback)
                    (let* ((lease (niwa-session-lease owner))
                           (niwa-session-durability--client-id
                            (make-string 64 ?b)))
                      (niwa-session-durability--write-plist
                       (niwa-session-durability--generation-path
                        (niwa-session-durability--lease-path directory)
                        (plist-get lease :generation))
                       (niwa-session-durability--lease-record
                        "*thief*" (plist-get lease :generation) 'active nil nil
                        (plist-get lease :renewed-at))))
                    (funcall reserve owner callback))))
         (niwa-session-publication-submit
          session
          (lambda ()
            (test-niwa-publication-async--transcript-artifacts
             session buffer "segment-9.chat.org" "declined transcript"))
          (lambda (ok error) (push (list ok error) acknowledgements)))
         (test-niwa-publication-async--wait (lambda () acknowledgements)))
       (should-not (caar acknowledgements))
       (should (equal "Portable session lease belongs to another client"
                      (error-message-string (cadar acknowledgements))))
       (should (eq 'user-error (car (cadar acknowledgements))))
       (should-not (niwa-session-publication-busy-p session))))))

(provide 'test-niwa-session-publication-async)
;;; test-niwa-session-publication-async.el ends here
