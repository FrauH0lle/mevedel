;;; test-mevedel-agent-control-mail-batch.el --- Coalesced durable mail -*- lexical-binding: t -*-

;;; Commentary:
;; Production Monitor delivery, real sidecars, and cold consumer snapshots.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-tool-exec)
(require 'mevedel-session-publication)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-workspace-identity)
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-execution-test-helpers"))

(defun test-mevedel-mail-batch--fixture (remote body)
  "Call BODY with a materialized session, optionally on a REMOTE mock target."
  (let* ((local-root (file-name-as-directory (make-temp-file "mail-batch-" t)))
         (root (if remote (concat "/mevedelmock:mail-batch:" local-root) local-root))
         (timers (append timer-list timer-idle-list))
         (buffers (buffer-list))
         (mevedel-transport--pending (make-hash-table :test #'equal))
         session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("mail-batch")
          (mevedel-workspace-identity-ensure root)
          (setq session (test-mevedel-execution--session root))
          (let ((buffer (generate-new-buffer " *mail-batch*")))
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "*** Original prompt\nOriginal response\n")
              (mevedel-session-set-root-buffer session buffer)
              (mevedel-session-artifacts-save session buffer nil t))
            (funcall body session)))
      (when session
        (mevedel-session-persistence-cancel-deferred-agent-save session)
        (mevedel-session-durability--cancel-renewal session))
      (mevedel-transport-cancel-pending)
      (dolist (timer (seq-difference (append timer-list timer-idle-list) timers))
        (cancel-timer timer))
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (mevedel-workspace-clear-registry)
      (delete-directory local-root t))))

(defun test-mevedel-mail-batch--restored (session)
  "Read SESSION's authoritative sidecar into a new consumer, returning mail."
  (let* ((mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
         (mevedel-session-publication--facts-cache (make-hash-table :test #'equal))
         (path (if (mevedel-session-codec-portable-authority-p session)
                   (plist-get (mevedel-session-publication-read
                               (mevedel-session-save-path session)) :sidecar)
                 (file-name-concat (mevedel-session-save-path session) "session.meta.el")))
         (raw (mevedel-session-persistence-load-sidecar path))
         (restored (mevedel-session-codec-deserialize
                    raw (mevedel-session-workspace session))))
    (mevedel-agent-control-context-mailbox (plist-get restored :session))))

(defun test-mevedel-mail-batch--send (session id ack)
  "Deliver one Monitor batch for ID through SESSION's production sink with ACK."
  (mevedel-tool-exec-handle-execution-event
   (list :type 'monitor-batch :session session :owner "/root"
         :facts (list :kind 'monitor :execution-id id :event-count 1
                      :description id)
         :monitor-lines (list id) :monitor-omitted 0)
   session ack))

(mevedel-deftest mevedel-agent-control-mail-batch (:quiet t)
  ,test
  (test)
  :doc "file and TRAMP batches share one commit; a crash before the next retains prior mail"
  (dolist (remote '(nil t))
    (test-mevedel-mail-batch--fixture
     remote
     (lambda (session)
       (let ((commit (symbol-function 'mevedel-agent-control-commit-session))
             (commits 0) acks)
         (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                    (lambda (&rest args)
                      (cl-incf commits)
                      (apply commit args))))
           ;; Hold only scheduling, not storage.  Remote reads pump the event
           ;; loop; they must not accidentally commit the batch being inspected.
           (cl-letf (((symbol-function 'mevedel-session-publication--schedule) #'ignore))
             (dolist (id '("first" "second" "third"))
               (should (eq 'pending
                           (test-mevedel-mail-batch--send
                            session id (lambda (ok err) (push (list id ok err) acks))))))
             (should-not acks)
             (should-not (mevedel-agent-control-context-mailbox session))
             (should-not (test-mevedel-mail-batch--restored session)))
           (mevedel-session-publication-barrier session)
           (should (= commits 1))
           (should (equal '(("first" t nil) ("second" t nil) ("third" t nil))
                          (reverse acks)))
           (let ((saved (test-mevedel-mail-batch--restored session)))
             (should (= 3 (length saved)))
             ;; A new batch is accepted but not yet committed.  A cold reader
             ;; sees the prior commit, not tentative mail or an empty queue.
             (cl-letf (((symbol-function 'mevedel-session-publication--schedule) #'ignore))
               (test-mevedel-mail-batch--send session "fourth" #'ignore)
               (should (equal saved (test-mevedel-mail-batch--restored session))))
             (mevedel-session-publication-stopping session)
             (should (= commits 2))
             (should (= 4 (length (test-mevedel-mail-batch--restored session))))))))))

  :doc "failed shared commit acknowledges every producer once and rolls back only its mail"
  (test-mevedel-mail-batch--fixture
   nil
   (lambda (session)
     (test-mevedel-mail-batch--send session "prior" #'ignore)
     (mevedel-session-publication-barrier session)
     (let ((saved (test-mevedel-mail-batch--restored session))
           (injections 0) acks)
       (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                  (lambda (&rest _)
                    (cl-incf injections)
                    (error "Injected batch commit failure"))))
         (dolist (id '("first" "second"))
           (test-mevedel-mail-batch--send
            session id (lambda (ok err) (push (list id ok err) acks))))
         (mevedel-session-publication-barrier session))
       (should (= injections 1))
       (should (equal '(("first" nil (error "Injected batch commit failure"))
                        ("second" nil (error "Injected batch commit failure")))
                      (reverse acks)))
       (should (equal saved (test-mevedel-mail-batch--restored session)))
       (should (= 1 (length (mevedel-agent-control-context-mailbox session)))))))

  :doc "synchronous mail drains accepted batches before capturing its own sidecar"
  (test-mevedel-mail-batch--fixture
   nil
   (lambda (session)
     (let (ack)
       (test-mevedel-mail-batch--send session "monitor" (lambda (ok _) (setq ack ok)))
       (mevedel-agent-control-enqueue-execution-result
        session "/root" "ordinary" "ordinary" nil nil)
       (should ack)
       (should (= 2 (length (test-mevedel-mail-batch--restored session)))))))

  :doc "nested arrivals commit in the next batch even if a producer acknowledgement quits"
  (test-mevedel-mail-batch--fixture
   nil
   (lambda (session)
     (let ((commit (symbol-function 'mevedel-agent-control-commit-session))
           (commits 0) snapshots acks diagnostics)
       (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                  (lambda (&rest args)
                    (cl-incf commits)
                    (when (= commits 1)
                      (test-mevedel-mail-batch--send
                       session "nested" (lambda (ok _) (push (list 'nested ok) acks))))
                    (prog1 (apply commit args)
                      (push (length (test-mevedel-mail-batch--restored session)) snapshots)))))
         (test-mevedel-mail-batch--send
          session "first" (lambda (ok _) (push (list 'first ok) acks) (signal 'quit nil)))
         (test-mevedel-mail-batch--send
          session "second" (lambda (ok _) (push (list 'second ok) acks)))
         (mevedel-test--with-captured-diagnostics diagnostics
           (mevedel-session-publication-barrier session)))
       (should (= commits 2))
       (should (equal '(2 3) (reverse snapshots)))
       (should (equal '((first t) (second t) (nested t)) (reverse acks)))
       (should (string-match-p "Mailbox acknowledgement failed: Quit" diagnostics))
       (should-not (mevedel-session-mail-batch session))
       (should-not (mevedel-session-publication-busy-p session))))))

(mevedel-deftest mevedel-agent-control-mail-batch/producer (:quiet t)
  ,test
  (test)
  :doc "real Monitor producers retain lines until the shared commit and retry failed output"
  (test-mevedel-mail-batch--fixture
   nil
   (lambda (session)
     (let* ((root (mevedel-session-save-path session))
            (mevedel-execution-mailbox-delivery-function
             #'mevedel-tool-exec-handle-execution-event)
            (mevedel-sandbox-mode 'off)
            records)
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'mevedel-session-publication--schedule) #'ignore))
               (dotimes (i 2)
                 (mevedel-execution-start-bash
                  (lambda (observation)
                    (push (gethash (plist-get (plist-get observation :facts) :execution-id)
                                   (mevedel-execution--state-records
                                    (mevedel-session-execution-state session))) records))
                  :kind 'monitor :description (format "producer%d" i)
                  :session session :owner "/root" :owner-context session
                  :command '("sh" "-c" "printf 'first\n'; sleep 60")
                  :workdir root :writable-roots (list root)
                  :artifact-directory (file-name-concat root "artifacts")))
               (test-mevedel-execution--wait
                (lambda () (and (= 2 (length records))
                                (cl-every #'mevedel-execution--record-monitor-inflight records))))
               (dolist (record records)
                 (should (equal "first\n" (mevedel-execution--monitor-body record))))
               (should-not (test-mevedel-mail-batch--restored session)))
             (mevedel-session-publication-barrier session)
             (should (= 2 (length (test-mevedel-mail-batch--restored session))))
             (dolist (record records)
               (should-not (mevedel-execution--record-monitor-inflight record))
               (should (equal "" (mevedel-execution--monitor-body record))))
             (let ((injections 0))
               (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                          (lambda (&rest _) (cl-incf injections)
                            (error "Injected producer commit failure"))))
                 (dolist (record records)
                   (mevedel-execution--monitor-frame record "retry\n")
                   (cancel-timer (mevedel-execution--record-monitor-timer record))
                   (mevedel-execution--monitor-flush record))
                 (mevedel-session-publication-barrier session))
               (should (= injections 1)))
             (dolist (record records)
               (should (equal "retry\n" (mevedel-execution--monitor-body record)))
               (should-not (mevedel-execution--record-monitor-inflight record))
               (mevedel-execution--monitor-flush record))
             (mevedel-session-publication-barrier session)
             (should (= 4 (length (test-mevedel-mail-batch--restored session))))
             (dolist (record records)
               (should (equal "" (mevedel-execution--monitor-body record)))))
         (mevedel-execution-teardown-session session))))))

(provide 'test-mevedel-agent-control-mail-batch)
;;; test-mevedel-agent-control-mail-batch.el ends here
