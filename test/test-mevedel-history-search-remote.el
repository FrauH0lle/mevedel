;;; test-mevedel-history-search-remote.el --- Real TRAMP history -*- lexical-binding: t -*-

;;; Commentary:
;; Saved-history caller checks on disposable, opt-in SSH and container targets.

;;; Code:

(require 'mevedel-history-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-history-test-helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-execution-test-helpers"))
(require 'json)
(require 'tramp)
(require 'tramp-container)
(require 'tramp-sh)

(defun test-mevedel-history-search-remote--exercise (variable method)
  "Exercise saved history on the real target from VARIABLE using METHOD."
  (let* ((tramp-use-connection-share tramp-use-connection-share)
         (tramp-ssh-controlmaster-options tramp-ssh-controlmaster-options)
         (base (test-mevedel-execution-remote--real-root variable method))
         (root (test-mevedel-execution-remote--real-temp-directory
                base "mevedel-history-"))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (mevedel-session-durability--disclosed-targets
          (copy-hash-table mevedel-session-durability--disclosed-targets))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (setup-start (float-time))
         sources samples setup)
    (unwind-protect
        (progn
          (test-mevedel-execution-remote--accept-storage
           (mevedel-session-create "disclosure" workspace root))
          (dotimes (index 6)
            (let ((text (with-temp-buffer
                          (dotimes (turn 20)
                            (insert (format "User investigation %d/%d.\n" index turn))
                            (dotimes (line 49)
                              (insert (propertize
                                       (format "Evidence %d/%d: REMOTE-VISIBLE.\n" turn line)
                                       'gptel 'response))))
                          (insert (propertize
                                   "\n<system-reminder>\nREMOTE-PRIVATE\n</system-reminder>\n"
                                   'gptel 'ignore))
                          (buffer-string))))
              (push (test-mevedel-history-search--source
                     workspace (format "remote-%d" index) text)
                    sources)))
          (dolist (source sources)
            (test-mevedel-session-persistence--release-and-kill
             (cdr source) (car source)))
          (setq setup (- (float-time) setup-start))
          (cl-labels
           ((query
              (operation args &optional cancel-after)
              (let* ((address (plist-get args (if (eq operation 'read)
                                                  :file_path :path)))
                     (attempt (mevedel-resource-prepare
                               operation address (list :workspace workspace)))
                     (mevedel-resource-current-attempts
                      (list (cons address attempt)))
                     (start (float-time)) (previous start) (maximum 0)
                     (heartbeat
                      (run-at-time
                       0 0.01
                       (lambda ()
                         (let ((now (float-time)))
                           (setq maximum (max maximum (- now previous))
                                 previous now)))))
                     cancel cancel-timer cancel-requested returned done result
                     (callbacks 0))
                (unwind-protect
                    (progn
                      (setq cancel
                            (funcall
                             (pcase operation
                               ('read #'mevedel-tool-fs-read)
                               ('glob #'mevedel-tool-fs-search-glob)
                               ('grep #'mevedel-tool-fs-search-grep))
                             (lambda (value)
                               (cl-incf callbacks)
                               (setq result value done (float-time)))
                             args)
                            returned (float-time))
                      (should (functionp cancel))
                      (should-not done)
                      (when cancel-after
                        (setq cancel-timer
                              (run-at-time
                               cancel-after nil
                               (lambda ()
                                 (setq cancel-requested (float-time))
                                 (funcall cancel)))))
                      (with-timeout (60 (ert-fail "Remote history timed out"))
                        (while (not done) (accept-process-output nil 0.01)))
                      (accept-process-output nil 0.05)
                      (should (= 1 callbacks))
                      (if cancel-after
                          (progn
                            (should (eq 'cancelled (plist-get result :status)))
                            (should (< (- done cancel-requested) 1)))
                        (should-not (eq 'error (plist-get result :status))))
                      (push (list :operation (symbol-name operation)
                                  :cancelled (if cancel-after t :false)
                                  :seconds (- done start)
                                  :synchronous-seconds (- returned start)
                                  :heartbeat-max maximum
                                  :cancellation-feedback-seconds
                                  (if cancel-requested (- done cancel-requested) 0))
                            samples)
                      result)
                  (cancel-timer heartbeat)
                  (when cancel-timer (cancel-timer cancel-timer))
                  (when cancel (funcall cancel))
                  (mevedel-resource-discard-attempts (list attempt))))))
           (let* ((listing (plist-get (query 'glob '(:path "history://saved"
                                                           :pattern "**/*.org")) :result))
                  (addresses (split-string listing "\n" t))
                  (address (car addresses)))
             (should (= 6 (length addresses)))
             (dotimes (_ 3)
               (let ((text (plist-get
                            (query 'grep '(:path "history://saved"
                                                 :pattern "REMOTE-" :head_limit 10
                                                 :output_mode "content")) :result)))
                 (should (string-search "REMOTE-VISIBLE" text))
                 (should-not (string-search "REMOTE-PRIVATE" text))
                 (should (string-search "history://saved/" text))))
             (let ((text (plist-get (query 'read (list :file_path address
                                                       :limit 30)) :result)))
               (should (string-search "REMOTE-VISIBLE" text))
               (should-not (string-search "REMOTE-PRIVATE" text)))
             (query 'grep '(:path "history://saved" :pattern "REMOTE-"
                                  :output_mode "content") 0.001)))
          (when-let* ((directory (getenv "HISTORY_REMOTE_REPORT_DIR")))
            (let ((path (file-name-concat directory
                                          (format "remote-%s.json" method))))
              (should-not (file-exists-p path))
              (with-temp-file path
                (insert (json-serialize
                         (list :method (symbol-name method) :sessions 6
                               :lines-per-session 1000 :fixture-seconds setup
                               :samples (vconcat (nreverse samples)))))))))
      (dolist (source sources)
        (test-mevedel-session-persistence--release-and-kill
         (cdr source) (car source)))
      (mevedel-execution-teardown-all)
      (unwind-protect
          (when (file-exists-p root) (delete-directory root t))
        (tramp-cleanup-connection (tramp-dissect-file-name base) nil t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/remote
  (:quiet t :tags (external remote history))
  ,test
  (test)
  :doc "searches filtered saved sources and cancels through real SSH"
  (test-mevedel-history-search-remote--exercise "MEVEDEL_TEST_SSH_ROOT" 'ssh)
  :doc "searches filtered saved sources and cancels through real Podman"
  (test-mevedel-history-search-remote--exercise "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(provide 'test-mevedel-history-search-remote)
;;; test-mevedel-history-search-remote.el ends here
