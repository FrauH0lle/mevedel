;;; test-niwa-execution-spool-lifecycle.el -- Retired spool coverage -*- lexical-binding: t -*-

;;; Commentary:

;; Real managed Bash output stays raw while running and while WriteStdin may
;; still poll its terminal result.  The production retirement timer, rather
;; than a direct compressor call, makes retained output eligible for gzip.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))
(require 'niwa-execution-spool)
(require 'niwa-resource)
(require 'niwa-tool-exec)
(require 'niwa-tool-fs-read)

(defun test-niwa-execution-spool-lifecycle--command ()
  "Return a command emitting over 64 KiB before waiting for a release file."
  (concat
   "python3 -c "
   (shell-quote-argument
    (concat "import pathlib,sys,time\n"
            "sys.stdout.write('spool lifecycle line\\n' * 4096)\n"
            "sys.stdout.flush()\n"
            "pathlib.Path('ready').write_text('ready')\n"
            "while not pathlib.Path('release').exists(): time.sleep(0.02)\n"
            "print('terminal-poll-tail', flush=True)\n"))))

(niwa-deftest niwa-execution-spool-lifecycle/terminal-polls
  (:doc "WriteStdin retains raw output until the natural terminal retirement callback")
  (let* ((root (make-temp-file "niwa-spool-lifecycle-" t))
         (session (test-niwa-execution--session root))
         (default-directory (file-name-as-directory root))
         (niwa--session session)
         (niwa--current-request nil)
         (niwa--agent-invocation nil)
         (niwa-execution-spool--compressions (make-hash-table :test #'equal))
         (compress (symbol-function 'niwa-execution-spool-compress))
         (buffer (generate-new-buffer " *spool-lifecycle*"))
         compression-paths compression-cancels initial terminal repeated raw record)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-execution-spool-compress)
                     (lambda (path)
                       (push path compression-paths)
                       (let ((cancel (funcall compress path)))
                         (push cancel compression-cancels)
                         cancel))))
            (niwa-tool-exec--bash
             (lambda (value) (setq initial value))
             (list :command (test-niwa-execution-spool-lifecycle--command)
                   :yield_time_ms 250 :description "Retirement regression"))
            (test-niwa-execution--wait (lambda () initial))
            (let* ((facts (plist-get initial :render-data))
                   (id (plist-get facts :execution-id))
                   (address (plist-get facts :output-path)))
              (should (eq 'running (plist-get facts :state)))
              (should (stringp id))
              (should (string-prefix-p "artifact://executions/" address))
              (setq record (gethash id (niwa-execution--state-records
                                       (niwa-session-execution-state session)))
                    raw (niwa-execution--spool-path record))
              (test-niwa-execution--wait
               (lambda ()
                 (and (file-exists-p (file-name-concat root "ready"))
                      (> (file-attribute-size (file-attributes raw)) (* 64 1024)))))
              (should (niwa-execution-session-live-p session))
              (should-not compression-paths)
              (should-not (file-exists-p (concat raw ".gz")))
              ;; Install the public observer before allowing terminal settlement;
              ;; this exercises model delivery without manufacturing mailbox state.
              (niwa-tool-exec--write-stdin
               (lambda (value) (setq terminal value))
               (list :execution_id id :yield_time_ms 5000))
              (write-region "release" nil (file-name-concat root "release") nil 'silent)
              (test-niwa-execution--wait (lambda () terminal))
              (should (eq 'completed (plist-get (plist-get terminal :render-data) :state)))
              (should (string-search "terminal-poll-tail" (plist-get terminal :result)))
              (should (file-regular-p raw))
              (should-not compression-paths)
              (should-not (file-exists-p (concat raw ".gz")))
              ;; A poll of the already-finished record must remain idempotent
              ;; throughout the retained terminal window, with its raw spool intact.
              (niwa-tool-exec--write-stdin
               (lambda (value) (setq repeated value))
               (list :execution_id id :yield_time_ms 5000))
              (test-niwa-execution--wait (lambda () repeated))
              (should (equal (plist-get terminal :result) (plist-get repeated :result)))
              (should (file-regular-p raw))
              (should-not compression-paths)
              (let ((timer (niwa-execution--record-retire-timer record)))
                (should (timerp timer))
                (should (eq record
                            (gethash id (niwa-execution--state-records
                                         (niwa-session-execution-state session)))))
                ;; Run the existing production timer promptly, without changing
                ;; retention policy, output caps, or calling cleanup directly.
                (cancel-timer timer)
                (timer-set-time timer (time-add nil 0.01))
                (timer-activate timer))
              (test-niwa-execution--wait
               (lambda ()
                 (and (file-regular-p (concat raw ".gz"))
                      (not (file-exists-p raw))))
               10)
              (should (equal (list raw) compression-paths))
              (should-not (gethash id (niwa-execution--state-records
                                       (niwa-session-execution-state session))))
              (should-not (niwa-execution--record-retire-timer record))
              (let* ((attempt (niwa-resource-prepare 'read address (list :session session)))
                     (niwa-resource-current-attempts (list (cons address attempt)))
                     (read (niwa-test--read (list :file_path address :offset 1 :limit 2))))
                (should-not (eq 'error (plist-get read :status)))
                (should (string-search "spool lifecycle line" (plist-get read :result)))
                (should-not (string-search ".log.gz" (plist-get read :result)))))))
      (niwa-execution-teardown-session session)
      (dolist (cancel compression-cancels)
        (when (functionp cancel) (funcall cancel)))
      (test-niwa-execution--wait
       (lambda () (zerop (hash-table-count niwa-execution-spool--compressions))) 5)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-lifecycle/live-foreground
  (:doc "large live foreground output is not compressed or exposed before yielding")
  (let* ((root (make-temp-file "niwa-spool-foreground-" t))
         (session (test-niwa-execution--session root))
         (default-directory (file-name-as-directory root))
         (niwa--session session)
         (niwa--current-request nil)
         (niwa--agent-invocation nil)
         (niwa-execution-spool--compressions (make-hash-table :test #'equal))
         (compress (symbol-function 'niwa-execution-spool-compress))
         (buffer (generate-new-buffer " *spool-foreground*"))
         compression-cancels compression-paths result)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-execution-spool-compress)
                     (lambda (path)
                       (push path compression-paths)
                       (let ((cancel (funcall compress path)))
                         (push cancel compression-cancels)
                         cancel))))
            (niwa-tool-exec--bash
             (lambda (value) (setq result value))
             (list :command (test-niwa-execution-spool-lifecycle--command)
                   :wait-for-completion-p t :description "Foreground regression"))
            (test-niwa-execution--wait
             (lambda () (file-exists-p (file-name-concat root "ready"))))
            (let* ((snapshot (car (niwa-execution-list-user session)))
                   (raw (plist-get snapshot :artifact-path)))
              (should (eq 'running (plist-get snapshot :state)))
              (should-not (plist-get snapshot :yielded))
              (test-niwa-execution--wait
               (lambda () (> (file-attribute-size (file-attributes raw)) (* 64 1024))))
              (should-not result)
              (should-not compression-paths)
              (should-not (file-exists-p (concat raw ".gz")))
              (should-not (niwa-execution-list session "/root"))
              (let* ((attempt (niwa-resource-prepare 'read "artifact://" (list :session session)))
                     (listing (plist-get (niwa-resource-execute attempt) :result)))
                (should-not (string-search ".niwa-pending-executions" listing)))
              (write-region "release" nil (file-name-concat root "release") nil 'silent)
              (test-niwa-execution--wait (lambda () result))
              (should (eq 'completed (plist-get (plist-get result :render-data) :state)))
              (should (string-search "terminal-poll-tail" (plist-get result :result))))))
      (niwa-execution-teardown-session session)
      (dolist (cancel compression-cancels)
        (when (functionp cancel) (funcall cancel)))
      (test-niwa-execution--wait
       (lambda () (zerop (hash-table-count niwa-execution-spool--compressions))) 5)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(provide 'test-niwa-execution-spool-lifecycle)
;;; test-niwa-execution-spool-lifecycle.el ends here
