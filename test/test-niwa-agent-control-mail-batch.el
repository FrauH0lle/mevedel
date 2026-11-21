;;; test-niwa-agent-control-mail-batch.el --- Coalesced durable mail -*- lexical-binding: t -*-

;;; Commentary:
;; Production Monitor delivery, real sidecars, and cold consumer snapshots.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-agent-control)
(require 'niwa-tool-exec)
(require 'niwa-session-publication)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-workspace-identity)
(require 'tramp-sh)
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-execution-test-helpers"))

(defun test-niwa-mail-batch--fixture (body)
  "Call BODY with a materialized session on a mock TRAMP target."
  (let* ((local-root (file-name-as-directory (make-temp-file "mail-batch-" t)))
         ;; Warm local file sessions now use async jobs, not the batch writer.
         (root (concat "/niwamock:mail-batch:" local-root))
         (timers (append timer-list timer-idle-list))
         (buffers (buffer-list))
         (niwa-transport--pending (make-hash-table :test #'equal))
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("mail-batch")
          (niwa-workspace-identity-ensure root)
          (setq session (test-niwa-execution--session root))
          (let ((buffer (generate-new-buffer " *mail-batch*")))
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "*** Original prompt\nOriginal response\n")
              (niwa-session-set-root-buffer session buffer)
              (niwa-session-artifacts-save session buffer nil t))
            (funcall body session)))
      (when session
        (niwa-session-persistence-cancel-deferred-agent-save session)
        (niwa-session-durability--cancel-renewal session))
      (niwa-transport-cancel-pending)
      (dolist (timer (seq-difference (append timer-list timer-idle-list) timers))
        (cancel-timer timer))
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (niwa-workspace-clear-registry)
      (delete-directory local-root t))))

(defun test-niwa-mail-batch--restored (session)
  "Read SESSION's authoritative sidecar into a new consumer, returning mail."
  (let* ((niwa-session-publication--generation-cache (make-hash-table :test #'equal))
         (niwa-session-publication--facts-cache (make-hash-table :test #'equal))
         (path (if (niwa-session-codec-portable-authority-p session)
                   (plist-get (niwa-session-publication-read
                               (niwa-session-save-path session)) :sidecar)
                 (file-name-concat (niwa-session-save-path session) "session.meta.el")))
         (raw (niwa-session-persistence-load-sidecar path))
         (restored (niwa-session-codec-deserialize
                    raw (niwa-session-workspace session))))
    (niwa-agent-control-context-mailbox (plist-get restored :session))))

(defun test-niwa-mail-batch--send (session id ack)
  "Deliver one Monitor batch for ID through SESSION's production sink with ACK."
  (niwa-tool-exec-handle-execution-event
   (list :type 'monitor-batch :session session :owner "/root"
         :facts (list :kind 'monitor :execution-id id :event-count 1
                      :description id)
         :monitor-lines (list id) :monitor-omitted 0)
   session ack))

(niwa-deftest niwa-agent-control-mail-batch (:quiet t)
  ,test
  (test)
  :doc "TRAMP lease batches share one commit; a crash before the next retains prior mail"
  (test-niwa-mail-batch--fixture
     (lambda (session)
       (let ((commit (symbol-function 'niwa-agent-control-commit-session))
             (commits 0) acks)
         (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                    (lambda (&rest args)
                      (cl-incf commits)
                      (apply commit args))))
           ;; Hold only scheduling, not storage.  Remote reads pump the event
           ;; loop; they must not accidentally commit the batch being inspected.
           (cl-letf (((symbol-function 'niwa-session-publication--schedule) #'ignore))
             (dolist (id '("first" "second" "third"))
               (should (eq 'pending
                           (test-niwa-mail-batch--send
                            session id (lambda (ok err) (push (list id ok err) acks))))))
             (should-not acks)
             (should-not (niwa-agent-control-context-mailbox session))
             (should-not (test-niwa-mail-batch--restored session)))
           (niwa-session-publication-barrier session)
           (should (= commits 1))
           (should (equal '(("first" t nil) ("second" t nil) ("third" t nil))
                          (reverse acks)))
           (let ((saved (test-niwa-mail-batch--restored session)))
             (should (= 3 (length saved)))
             ;; A new batch is accepted but not yet committed.  A cold reader
             ;; sees the prior commit, not tentative mail or an empty queue.
             (cl-letf (((symbol-function 'niwa-session-publication--schedule) #'ignore))
               (test-niwa-mail-batch--send session "fourth" #'ignore)
               (should (equal saved (test-niwa-mail-batch--restored session))))
             (niwa-session-publication-stopping session)
             (should (= commits 2))
             (should (= 4 (length (test-niwa-mail-batch--restored session)))))))))

  :doc "failed shared commit acknowledges every producer once and rolls back only its mail"
  (test-niwa-mail-batch--fixture
   (lambda (session)
     (test-niwa-mail-batch--send session "prior" #'ignore)
     (niwa-session-publication-barrier session)
     (let ((saved (test-niwa-mail-batch--restored session))
           (injections 0) acks)
       (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                  (lambda (&rest _)
                    (cl-incf injections)
                    (error "Injected batch commit failure"))))
         (dolist (id '("first" "second"))
           (test-niwa-mail-batch--send
            session id (lambda (ok err) (push (list id ok err) acks))))
         (niwa-session-publication-barrier session))
       (should (= injections 1))
       (should (equal '(("first" nil (error "Injected batch commit failure"))
                        ("second" nil (error "Injected batch commit failure")))
                      (reverse acks)))
       (should (equal saved (test-niwa-mail-batch--restored session)))
       (should (= 1 (length (niwa-agent-control-context-mailbox session)))))))

  :doc "synchronous mail drains accepted batches before capturing its own sidecar"
  (test-niwa-mail-batch--fixture
   (lambda (session)
     (let (ack)
       (test-niwa-mail-batch--send session "monitor" (lambda (ok _) (setq ack ok)))
       (niwa-agent-control-enqueue-execution-result
        session "/root" "ordinary" "ordinary" nil nil)
       (should ack)
       (should (= 2 (length (test-niwa-mail-batch--restored session)))))))

  :doc "nested arrivals commit in the next batch even if a producer acknowledgement quits"
  (test-niwa-mail-batch--fixture
   (lambda (session)
     (let ((commit (symbol-function 'niwa-agent-control-commit-session))
           (commits 0) snapshots acks diagnostics)
       (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                  (lambda (&rest args)
                    (cl-incf commits)
                    (when (= commits 1)
                      (test-niwa-mail-batch--send
                       session "nested" (lambda (ok _) (push (list 'nested ok) acks))))
                    (prog1 (apply commit args)
                      (push (length (test-niwa-mail-batch--restored session)) snapshots)))))
         (test-niwa-mail-batch--send
          session "first" (lambda (ok _) (push (list 'first ok) acks) (signal 'quit nil)))
         (test-niwa-mail-batch--send
          session "second" (lambda (ok _) (push (list 'second ok) acks)))
         (niwa-test--with-captured-diagnostics diagnostics
           (niwa-session-publication-barrier session)))
       (should (= commits 2))
       (should (equal '(2 3) (reverse snapshots)))
       (should (equal '((first t) (second t) (nested t)) (reverse acks)))
       (should (string-match-p "Mailbox acknowledgement failed: Quit" diagnostics))
       (should-not (niwa-session-mail-batch session))
       (should-not (niwa-session-publication-busy-p session))))))

(niwa-deftest niwa-agent-control-mail-batch/producer (:quiet t)
  ,test
  (test)
  :doc "real Monitor producers retain lines until the shared commit and retry failed output"
  (test-niwa-mail-batch--fixture
   (lambda (session)
     (let* ((root (niwa-session-save-path session))
            (niwa-execution-mailbox-delivery-function
             #'niwa-tool-exec-handle-execution-event)
            (niwa-sandbox-mode 'off)
            (gate (file-name-concat (file-local-name root) "producer-go"))
            records)
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'niwa-session-publication--schedule) #'ignore))
               (dotimes (i 2)
                 (niwa-execution-start-bash
                  (lambda (observation)
                    (push (gethash (plist-get (plist-get observation :facts) :execution-id)
                                   (niwa-execution--state-records
                                    (niwa-session-execution-state session))) records))
                  :kind 'monitor :description (format "producer%d" i)
                  :session session :owner "/root" :owner-context session
                  :command (list "sh" "-c"
                                 "while [ ! -f \"$1\" ]; do sleep .02; done; printf 'first\n'; sleep 60"
                                 "sh" gate)
                  :workdir root :writable-roots (list root)
                  :artifact-directory
                  (niwa-tool-exec--execution-artifact-directory session nil)))
               (test-niwa-execution--wait (lambda () (= 2 (length records))))
               (let* ((wait (symbol-function 'tramp-wait-for-output))
                      (default-directory root)
                      (record (car records))
                      (process (niwa-execution-process--child-process
                                (niwa-execution--record-child record)))
                      released)
                 ;; Force real Monitor output while TRAMP is waiting on a
                 ;; different command. A remote live spool would reenter it.
                 (cl-letf (((symbol-function 'tramp-wait-for-output)
                            (lambda (&rest args)
                              (unless released
                                (setq released t)
                                (should (niwa-transport-nested-p))
                                (write-region "" nil gate nil 'silent)
                                (let ((end (+ (float-time) 5)))
                                  (while (and (string-empty-p (niwa-execution--monitor-body record))
                                              (< (float-time) end))
                                    (accept-process-output process 0.05 nil 1)))
                                (should (equal "first\n" (niwa-execution--monitor-body record))))
                              (apply wait args))))
                   (should (= 0 (process-file "true" nil nil nil))))
                 (should released))
               (test-niwa-execution--wait
                (lambda ()
                  (cl-every #'niwa-execution--record-monitor-inflight records)))
               (dolist (record records)
                 (should (equal "first\n" (niwa-execution--monitor-body record))))
               (should-not (test-niwa-mail-batch--restored session)))
             (niwa-session-publication-barrier session)
             (should (= 2 (length (test-niwa-mail-batch--restored session))))
             (dolist (record records)
               (should-not (niwa-execution--record-monitor-inflight record))
               (should (equal "" (niwa-execution--monitor-body record))))
             (let ((injections 0))
               (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                          (lambda (&rest _) (cl-incf injections)
                            (error "Injected producer commit failure"))))
                 (dolist (record records)
                   (niwa-execution--monitor-frame record "retry\n")
                   (cancel-timer (niwa-execution--record-monitor-timer record))
                   (niwa-execution--monitor-flush record))
                 (niwa-session-publication-barrier session))
               (should (= injections 1)))
             (dolist (record records)
               (should (equal "retry\n" (niwa-execution--monitor-body record)))
               (should-not (niwa-execution--record-monitor-inflight record))
               (niwa-execution--monitor-flush record))
             (niwa-session-publication-barrier session)
             (should (= 4 (length (test-niwa-mail-batch--restored session))))
             (dolist (record records)
               (should (equal "" (niwa-execution--monitor-body record)))))
         (niwa-execution-teardown-session session))))))

(provide 'test-niwa-agent-control-mail-batch)
;;; test-niwa-agent-control-mail-batch.el ends here
