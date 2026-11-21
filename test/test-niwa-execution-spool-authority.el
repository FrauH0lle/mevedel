;;; test-niwa-execution-spool-authority.el --- Remote spool authority tests -*- lexical-binding: t -*-

;;; Commentary:
;; Compressed execution storage must not grant access to unpublished portable
;; artifacts.  Exercise authored resource addresses through the public tools.

;;; Code:

(require 'niwa-execution-spool-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-spool-test-support"))

(niwa-deftest niwa-execution-spool-remote-authority
  (:quiet t :doc "remote Read denies unpublished raw/gzip artifacts but reads and searches execution gzip")
  (let* ((root (make-temp-file "niwa-spool-authority-" t))
         (ordinary (file-name-concat root "tool-results" "ordinary.log"))
         (execution (file-name-concat root "tool-results" "executions" "execution.log"))
         (compressed-secret "UNPUBLISHED-COMPRESSED-CACHE-BODY")
         (raw-secret "UNPUBLISHED-RAW-CACHE-BODY")
         (execution-address "artifact://executions/execution.log"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory execution) t)
          (with-temp-file ordinary
            (insert compressed-secret "\n" (make-string 70000 ?x) "\n"))
          (with-temp-file execution
            (insert "first\nretained execution needle\n" (make-string 70000 ?x) "\n"))
          (dolist (path (list ordinary execution))
            (niwa-execution-spool-compress path))
          (test-niwa-spool--wait
           (lambda ()
             (and (not (file-exists-p ordinary))
                  (not (file-exists-p execution))
                  (zerop (hash-table-count niwa-execution-spool--compressions)))))
          (should (file-regular-p (concat ordinary ".gz")))
          (should (file-regular-p (concat execution ".gz")))
          (with-temp-file ordinary (insert raw-secret "\n"))
          (niwa-test--with-local-shell-tramp '("spool-authority")
            (let* ((remote-root (format "/niwamock:spool-authority:%s/" root))
                   (workspace (niwa-workspace--create
                               :type 'project :id remote-root :root remote-root
                               :name "spool-authority"))
                   (session (niwa-session-create "main" workspace))
                   (default-directory remote-root))
              (setf (niwa-session-save-path session) remote-root
                    (niwa-session-authority-mode session) 'portable
                    (niwa-session-sandbox-mode session) 'off
                    (niwa-session-publication session)
                    (list :head ".publications/generation/manifest.el"
                          :sidecar nil :artifacts nil))
              (dolist (storage '(raw-and-gzip gzip-only))
                (ert-info ((format "Unpublished ordinary artifact with %s storage" storage))
                  (when (eq storage 'gzip-only) (delete-file ordinary))
                  (let (result failure)
                    (condition-case err
                        (setq result
                              (test-niwa-spool--tool
                               session 'read "artifact://ordinary.log" :offset 1 :limit 1))
                      (error (setq failure (error-message-string err))))
                    ;; Check the entire delivered value independently of how
                    ;; the handler represents denial.  An error envelope must
                    ;; not conceal a successful read of either fixed cache.
                    (let ((delivered (prin1-to-string (list result failure))))
                      (should-not (string-search raw-secret delivered))
                      (should-not (string-search compressed-secret delivered)))
                    (should (or failure
                                (eq 'error (plist-get result :status))
                                (plist-get result :error))))))
              (let ((publication (niwa-session-publication session)))
                (unwind-protect
                    (progn
                      (setf (niwa-session-publication session) nil)
                      (dolist (target '("artifact://executions" "artifact://"))
                        (let* ((result (test-niwa-spool--tool
                                        session 'read target :limit 1))
                               (text (plist-get result :result)))
                          (should-not (eq 'error (plist-get result :status)))
                          (should (string-search execution-address text))
                          (should-not (string-search (concat execution-address ".gz") text))
                          (should-not (string-search root text)))))
                  (setf (niwa-session-publication session) publication)))
              (let* ((result (test-niwa-spool--tool
                              session 'read execution-address :offset 2 :limit 1))
                     (text (plist-get result :result)))
                (should-not (eq 'error (plist-get result :status)))
                (should (string-search "2\tretained execution needle" text))
                (should-not (string-search root text))
                (should-not (string-search ".gz" text)))
              (dolist (target (list execution-address "artifact://executions" "artifact://"))
                (dolist (mode '("content" "count" "files_with_matches"))
                  (ert-info ((format "Remote Grep %s in %s" mode target))
                    (let* ((result (test-niwa-spool--tool
                                    session 'grep target :pattern "needle" :output_mode mode))
                           (text (plist-get result :result)))
                      (should-not (eq 'error (plist-get result :status)))
                      ;; Ripgrep may omit the filename for a single-file
                      ;; count.  Directory results must always retain it.
                      (unless (and (equal target execution-address)
                                   (equal mode "count"))
                        (should (string-search execution-address text)))
                      (pcase mode
                        ("content" (should (string-search "retained execution needle" text)))
                        ("count"
                         (should (string-match-p
                                  "\\(?:\\`\\|[:\n]\\)1\\(?:\n\\|\\'\\)" text))))
                      (should-not (string-search ".gz" text))
                      (should-not (string-search root text))
                      (should-not (string-search "/niwamock:" text))
                      (should-not (string-search ".niwa-spool-" text))))))
              (should-not niwa-execution-spool--readers))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-remote-publication-authority
  (:quiet t :doc "remote execution Read honors verified publication over conflicting raw/gzip caches")
  (let* ((root (make-temp-file "niwa-spool-published-" t))
         (logical "tool-results/executions/execution.log")
         (fixed (file-name-concat root logical))
         (local-published (file-name-concat root ".publications" "generation" "000001.data"))
         (address "artifact://executions/execution.log")
         (authorized "authorized\n")
         (stale-raw "stale raw fixed cache")
         (stale-gzip "stale gzip fixed cache")
         (corrupt "corrupt published bytes\n"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory fixed) t)
          (make-directory (file-name-directory local-published) t)
          (with-temp-file fixed
            (insert stale-gzip "\n" (make-string 70000 ?x) "\n"))
          (niwa-execution-spool-compress fixed)
          (test-niwa-spool--wait
           (lambda ()
             (and (not (file-exists-p fixed))
                  (zerop (hash-table-count niwa-execution-spool--compressions)))))
          (should (file-regular-p (concat fixed ".gz")))
          (with-temp-file fixed (insert stale-raw "\n"))
          (with-temp-file local-published (insert authorized))
          (niwa-test--with-local-shell-tramp '("spool-published")
            (let* ((remote-root (format "/niwamock:spool-published:%s/" root))
                   (workspace (niwa-workspace--create
                               :type 'project :id remote-root :root remote-root
                               :name "spool-published"))
                   (session (niwa-session-create "main" workspace))
                   (published (file-name-concat
                               remote-root ".publications" "generation" "000001.data"))
                   (default-directory remote-root))
              (setf (niwa-session-save-path session) remote-root
                    (niwa-session-authority-mode session) 'portable
                    (niwa-session-sandbox-mode session) 'off
                    (niwa-session-publication session)
                    (list :head ".publications/generation/manifest.el"
                          :sidecar published
                          :artifacts
                          (list (list logical :published published
                                      :sha256 (secure-hash 'sha256 authorized)))))
              (dolist (storage '(raw-and-gzip gzip-only))
                (ert-info ((format "Published execution with conflicting %s cache" storage))
                  (when (eq storage 'gzip-only) (delete-file fixed))
                  (let* ((result (test-niwa-spool--tool
                                  session 'read address :offset 1 :limit 1))
                         (text (plist-get result :result))
                         (delivered (prin1-to-string result)))
                    (should-not (eq 'error (plist-get result :status)))
                    (should (string-search "1\tauthorized" text))
                    (should-not (string-search stale-raw delivered))
                    (should-not (string-search stale-gzip delivered))
                    (should-not (string-search root text))
                    (should-not (string-search ".publications" text)))
                  ;; Keep the captured hash unchanged while corrupting its
                  ;; publication.  Neither fixed representation may rescue it.
                  (write-region corrupt nil published nil 'silent)
                  (let (result failure)
                    (condition-case err
                        (setq result (test-niwa-spool--tool
                                      session 'read address :offset 1 :limit 1))
                      (error (setq failure (error-message-string err))))
                    (let ((delivered (prin1-to-string (list result failure))))
                      (should-not (string-search stale-raw delivered))
                      (should-not (string-search stale-gzip delivered))
                      (should-not (string-search (string-trim-right corrupt) delivered))
                      (should-not (string-search "authorized" delivered))
                      (should (string-search "failed verification" delivered)))
                    (should (or failure
                                (eq 'error (plist-get result :status))
                                (plist-get result :error))))
                  ;; Physical storage suffixes are never authored handles,
                  ;; regardless of the publication's integrity or raw cache.
                  (dolist (publication-content (list authorized corrupt))
                    (write-region publication-content nil published nil 'silent)
                    (dolist (operation '(read grep glob))
                      (ert-info ((format "Reject physical gzip %s with %s publication and %s cache"
                                         operation
                                         (if (equal publication-content authorized) "valid" "corrupt")
                                         storage))
                        (let ((options (pcase operation
                                         ('read '(:offset 1 :limit 1))
                                         ('grep '(:pattern "stale|authorized|corrupt"
                                                  :output_mode "content"))
                                         ('glob '(:pattern "*"))))
                              result failure)
                          (condition-case err
                              (setq result
                                    (apply #'test-niwa-spool--tool
                                           session operation (concat address ".gz") options))
                            (error (setq failure (error-message-string err))))
                          (let ((delivered (prin1-to-string (list result failure))))
                            (should (string-search "not published" delivered))
                            (should-not (string-search stale-raw delivered))
                            (should-not (string-search stale-gzip delivered))
                            (should-not (string-search "authorized" delivered))
                            (should-not (string-search (string-trim-right corrupt) delivered)))
                          (should (or failure
                                      (eq 'error (plist-get result :status))
                                      (plist-get result :error)))))))
                  (write-region authorized nil published nil 'silent)))
              (should-not niwa-execution-spool--readers))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-fs-resource-child-address-remote
  (:quiet t :doc "native remote process paths retain address encoding and cannot escape the selected root")
  (let* ((base (make-temp-file "niwa-spool-native-path-" t))
         (root (file-name-concat base "selected"))
         (file (file-name-concat root "report:7.log"))
         (outside (file-name-concat base "selected-outside" "report:7.log"))
         (address "work://reports")
         (logical "work://reports/report%3A7.log"))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory (file-name-directory outside) t)
          (with-temp-file file (insert "needle\n"))
          (with-temp-file outside (insert "outside\n"))
          (niwa-test--with-local-shell-tramp '("spool-native-path")
            (let* ((prefix "/niwamock:spool-native-path:")
                   (remote-root (concat prefix root))
                   (remote-file (concat prefix file)))
              (dolist (path (list file remote-file))
                (should (equal logical
                               (niwa-tool-fs-resource-child-address
                                address remote-root path))))
              (dolist (path (list outside
                                  (file-name-concat root ".." "selected-outside" "report:7.log")))
                (should-not (niwa-tool-fs-resource-child-address address remote-root path)))
              (should-not
               (niwa-tool-fs-search--resource-search-candidate-end
                (concat outside "\0" "1:outside") remote-root))
              ;; A colon in the filename is not a line/count delimiter, both
              ;; with native NUL separation and the captured-output fallback.
              (dolist (suffix '("\0" ":"))
                (let ((line (concat file suffix "1:needle")))
                  (should (= (length file)
                             (niwa-tool-fs-search--resource-search-candidate-end
                              line remote-root)))
                  (should (equal (concat logical ":1:needle")
                                 (niwa-tool-fs-search--rewrite-resource-search-output
                                  line remote-root address nil ":")))))
              (should (= (length file)
                         (niwa-tool-fs-search--resource-search-candidate-end
                          (concat file "\0" "1") remote-file)))
              (should-not
               (string-search
                logical
                (niwa-tool-fs-search--rewrite-resource-search-output
                 (concat outside "\0" "1:outside") remote-root address nil ":"))))))
      (delete-directory base t))))

(provide 'test-niwa-execution-spool-authority)
;;; test-niwa-execution-spool-authority.el ends here
