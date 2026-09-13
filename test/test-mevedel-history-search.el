;;; test-mevedel-history-search.el --- Saved conversation search -*- lexical-binding: t -*-

;;; Commentary:
;; Native saved sources through the authorized resource and tool interfaces.

;;; Code:

(require 'mevedel-history-test-helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-history-test-helpers"))

(mevedel-deftest mevedel-history-search-start (:quiet t)
  ,test
  (test)
  :doc "searches saved workspace conversations asynchronously with canonical filtering"
  (let* ((root (make-temp-file "mevedel-history-search-" t))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "source" workspace))
         (buffer (generate-new-buffer " *history-source*"))
         (address "history://saved")
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         done result cancel)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel--transcript-org-mode)
            (setq-local mevedel--session session)
            (mevedel-transcript-enable-gptel-mode)
            (insert "User needle and literal <system-reminder>example</system-reminder>.\n")
            (insert (propertize "Assistant needle.\n" 'gptel 'response))
            (insert (propertize "\n<system-reminder>\nPrivate needle.\n</system-reminder>\n"
                                'gptel 'ignore))
            (mevedel-session-artifacts-save session buffer))
          (let* ((attempt (mevedel-resource-prepare
                           'grep address (list :workspace workspace)))
                 (mevedel-resource-current-attempts (list (cons address attempt))))
            (setq cancel
                  (mevedel-tool-fs-search-grep
                   (lambda (value) (setq result value done t))
                   (list :path address :pattern "needle" :output_mode "content"))))
          (should (functionp cancel))
          (should-not done)
          (with-timeout (10 (ert-fail "History callback timed out"))
            (while (not done) (accept-process-output nil 0.01)))
          (let ((text (plist-get result :result)))
            (should (string-search "User needle" text))
            (should (string-search "Assistant needle" text))
            (should (string-search "<system-reminder>example</system-reminder>" text))
            (should-not (string-search "Private needle" text))
            (should (string-match-p "history://saved/[^/\n]+/segment-0001.chat.org:[0-9]+:" text))
            (should-not (string-search root text)))
          (let* ((source (concat "history://saved/"
                                 (mevedel-resource-encode-component
                                  (file-name-nondirectory
                                   (directory-file-name (mevedel-session-save-path session))))
                                 "/segment-0001.chat.org"))
                 (attempt (mevedel-resource-prepare 'read source (list :workspace workspace)))
                 (mevedel-resource-current-attempts (list (cons source attempt))))
            (setq done nil
                  cancel (mevedel-tool-fs-read
                          (lambda (value) (setq result value done t))
                          (list :file_path source :offset 1 :limit 30)))
            (should-not done)
            (with-timeout (10 (ert-fail "History Read timed out"))
              (while (not done) (accept-process-output nil 0.01)))
            (should (string-search "provenance: assistant" (plist-get result :result)))
            (should-not (string-search "Private needle" (plist-get result :result)))))
      (when cancel (funcall cancel))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search--prepare (:quiet t)
  ,test
  (test)
  :doc "reads current and archived native PID and portable sources without live views"
  (dolist (portable '(nil t))
    (let* ((root (make-temp-file "mevedel-history-profiles-" t))
           (workspace (if portable
                          (test-mevedel-session-persistence--make-workspace root)
                        (test-mevedel-session-persistence--make-file-workspace root)))
           (mevedel-history-search--cache (make-hash-table :test #'equal))
           (mevedel-history-search--cache-bytes 0)
           (source (test-mevedel-history-search--source
                    workspace "first"
                    (concat "Original user fact.\n"
                            (propertize "Original assistant fact.\n" 'gptel 'response)
                            (propertize "\n<system-reminder>\nPRIVATE\n</system-reminder>\n" 'gptel 'ignore))))
           (other (test-mevedel-history-search--source workspace "second" "Other session fact.\n"))
           (session (car source))
           (buffer (cdr source)))
      (unwind-protect
          (progn
            (mevedel-session-artifacts-rotate-segment session buffer "Inherited summary")
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "Current user fact.\n" (propertize "Current assistant fact.\n" 'gptel 'response))
              (mevedel-session-artifacts-save session buffer)
              (set-buffer-modified-p nil))
            (kill-buffer buffer)
            (let* ((listing (plist-get (test-mevedel-history-search--query
                                        workspace 'glob '(:path "history://saved" :pattern "**/*.org")) :result))
                   (addresses (split-string listing "\n" t))
                   (selected (seq-find (lambda (address)
                                         (string-search "/first-" address)) addresses)))
              (should (= 3 (length addresses)))
              (dolist (address addresses)
                (let ((text (plist-get (test-mevedel-history-search--query
                                        workspace 'read (list :file_path address :limit 40)) :result)))
                  (should-not (string-search "PRIVATE" text))
                  (when (and (string-search "/first-" address)
                             (string-suffix-p "segment-0001.chat.org" address))
                    (should (string-search "Original assistant fact." text))
                    (should (string-search "provenance: assistant" text)))))
              (let ((text (plist-get (test-mevedel-history-search--query
                                      workspace 'grep (list :path selected :pattern "fact" :output_mode "content")) :result)))
                (should (string-search selected text))
                (should-not (string-search "Other session" text)))
              (let ((scope (substring selected 0 (string-match "/segment-" selected))))
                (should (= 2 (length (split-string
                                      (plist-get (test-mevedel-history-search--query
                                                  workspace 'glob (list :path scope :pattern "**/*.org")) :result)
                                      "\n" t)))))))
        (test-mevedel-session-persistence--release-and-kill (cdr source) (car source))
        (test-mevedel-session-persistence--release-and-kill (cdr other) (car other))
        (mevedel-execution-teardown-all)
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-history-search--remember ()
  ,test
  (test)
  :doc "bounds disposable payload, replaces keys and skips oversized entries"
  (let ((mevedel-history-search--cache (make-hash-table :test #'equal))
        (mevedel-history-search--cache-bytes 0)
        (mevedel-history-search--cache-limit 5))
    (mevedel-history-search--remember "one" "a" "123")
    (mevedel-history-search--remember "one" "b" "12")
    (should (= 2 mevedel-history-search--cache-bytes))
    (should (equal '("b" . "12") (gethash "one" mevedel-history-search--cache)))
    (mevedel-history-search--remember "two" "c" "4567")
    (should (= 4 mevedel-history-search--cache-bytes))
    (should-not (gethash "one" mevedel-history-search--cache))
    (mevedel-history-search--remember "large" "d" "123456")
    (should (= 0 mevedel-history-search--cache-bytes))
    (should (= 0 (hash-table-count mevedel-history-search--cache)))))

(mevedel-deftest mevedel-history-search-start/freshness (:quiet t)
  ,test
  (test)
  :doc "warm projections cannot hide same-size edits, redirected sources or changed authority"
  (let* ((root (make-temp-file "mevedel-history-fresh-" t))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (source (test-mevedel-history-search--source workspace "fresh" "Observed version ONE.\n"))
         (session (car source))
         (directory (mevedel-session-save-path session))
         (file (file-name-concat directory "segment-0001.chat.org"))
         (stamp (file-attribute-modification-time (file-attributes file)))
         (args '(:path "history://saved" :pattern "version" :output_mode "content")))
    (unwind-protect
        (progn
          (should (string-search "version ONE" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          (should (= 1 (hash-table-count mevedel-history-search--cache)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "ONE")
            (replace-match "TWO" t t)
            (write-region (point-min) (point-max) file nil 'silent))
          (set-file-times file stamp)
          (should (string-search "version TWO" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          (make-directory (file-name-concat directory ".lease"))
          (should-not (string-search "version" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          (delete-directory (file-name-concat directory ".lease"))
          (let ((moved (concat file ".moved")))
            (rename-file file moved)
            (make-symbolic-link moved file)
            (should-not (string-search "version TWO" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
            (delete-file file)
            (rename-file moved file))
          (let ((moved (file-name-concat root "moved-session")))
            (rename-file (directory-file-name directory) moved)
            (make-symbolic-link moved (directory-file-name directory))
            (unwind-protect
                (should-not (string-search "version TWO" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
              (delete-file (directory-file-name directory))
              (rename-file moved (directory-file-name directory)))))
      (test-mevedel-session-persistence--release-and-kill (cdr source) session)
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/portable-freshness (:quiet t)
  ,test
  (test)
  :doc "revalidates new portable publications and rejects corrupt committed bytes"
  (let* ((root (make-temp-file "mevedel-history-publication-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (source (test-mevedel-history-search--source workspace "portable" "Published fact ONE.\n"))
         (session (car source))
         (args '(:path "history://saved" :pattern "fact" :output_mode "content")))
    (unwind-protect
        (progn
          (should (string-search "fact ONE" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          (with-current-buffer (cdr source)
            (goto-char (point-min))
            (search-forward "ONE")
            (replace-match "TWO" t t)
            (mevedel-session-artifacts-save session (current-buffer)))
          (should (string-search "fact TWO" (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          (let* ((publication (mevedel-session-publication-read (mevedel-session-save-path session)))
                 (path (plist-get (cdr (assoc "segment-0001.chat.org" (plist-get publication :artifacts))) :published)))
            (write-region "Corrupt fact THREE" nil path nil 'silent)
            (let ((result (test-mevedel-history-search--query workspace 'grep args)))
              (should (eq 'error (plist-get result :status)))
              (should (string-search "verification" (plist-get result :result)))
              (should-not (string-search "fact TWO" (plist-get result :result)))
              (should-not (string-search root (plist-get result :result))))))
      (test-mevedel-session-persistence--release-and-kill (cdr source) session)
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/cancellation (:quiet t)
  ,test
  (test)
  :doc "cancels every preparation phase and helper launch without late results or files"
  (let* ((root (make-temp-file "mevedel-history-cancel-" t))
         (temporary-file-directory (file-name-as-directory root))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (source (test-mevedel-history-search--source workspace "cancel" "Cancellation needle.\n"))
         (native-next (symbol-function 'iter-next))
         (native-helper (symbol-function 'mevedel-execution-start-helper))
         (native-process (symbol-function 'make-process)))
    (unwind-protect
        (dolist (phase '(discovery reads restoration projection temporary-preparation search))
          (let* ((mevedel-history-search--cache (make-hash-table :test #'equal))
                 (mevedel-history-search--cache-bytes 0)
                 (before (directory-files temporary-file-directory nil "^mevedel-history-search-"))
                 (attempt (mevedel-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                 (mevedel-resource-current-attempts (list (cons "history://saved" attempt)))
                 (deadline (+ (float-time) 5))
                 cancel reached results child)
            (unwind-protect
                (cl-letf (((symbol-function 'iter-next)
                           (lambda (&rest args)
                             (let ((value (apply native-next args)))
                               (when (and (eq value phase) (not reached))
                                 (setq reached (float-time))
                                 (funcall cancel))
                               value)))
                          ((symbol-function 'make-process)
                           (lambda (&rest args)
                             (let ((process (apply native-process args)))
                               (when (string-search "mevedel-grep" (or (plist-get args :name) ""))
                                 (setq child process))
                               process)))
                          ((symbol-function 'mevedel-execution-start-helper)
                           (lambda (callback name command paths roots &rest keys)
                             (let ((handle (apply native-helper callback name
                                                  (if (eq phase 'search)
                                                      (append '("sh" "-c" "sleep 2; exec \"$@\"" "history-test") command)
                                                    command)
                                                  paths roots keys)))
                               (when (eq phase 'search)
                                 (setq reached (float-time))
                                 (funcall cancel))
                               handle))))
                         (setq cancel (mevedel-tool-fs-search-grep
                                       (lambda (value) (push (cons (float-time) value) results))
                                       '(:path "history://saved" :pattern "needle" :output_mode "content")))
                         (while (and (not results) (< (float-time) deadline))
                           (accept-process-output nil 0.01))
                         (should reached)
                         (should (= 1 (length results)))
                         (should (eq 'cancelled (plist-get (cdar results) :status)))
                         (should (< (- (caar results) reached) 1))
                         (accept-process-output nil 0.15)
                         (should (= 1 (length results)))
                         (should-not (and child (process-live-p child)))
                         (should (equal before (directory-files temporary-file-directory nil "^mevedel-history-search-"))))
              (when cancel (funcall cancel))
              (mevedel-execution-teardown-all))))
      (test-mevedel-session-persistence--release-and-kill (cdr source) (car source))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/owner-close (:quiet t)
  ,test
  (test)
  :doc "owner buffer closure, request teardown and terminal agents suppress queued delivery"
  (dolist (kind '(buffer request agent))
    (let* ((root (make-temp-file "mevedel-history-owner-" t))
           (temporary-file-directory (file-name-as-directory root))
           (workspace (test-mevedel-session-persistence--make-file-workspace root))
           (owner (generate-new-buffer " *history-owner*"))
           (request (mevedel-request--create :origin "/root"))
           (invocation (mevedel-agent-invocation--create))
           (before (directory-files temporary-file-directory nil "^mevedel-history-search-"))
           result cancel)
      (unwind-protect
          (progn
            (with-current-buffer owner
              (setq-local mevedel--current-request request)
              (setq-local mevedel--agent-invocation invocation)
              (let* ((attempt (mevedel-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                     (mevedel-resource-current-attempts (list (cons "history://saved" attempt))))
                (setq cancel (mevedel-tool-fs-search-grep
                              (lambda (value) (setq result value))
                              '(:path "history://saved" :pattern "needle")))))
            (pcase kind
              ('buffer (kill-buffer owner))
              ('request (mevedel-request-drain-cancellers request))
              ('agent (setf (mevedel-agent-invocation-runtime-settled-p invocation) t)))
            (accept-process-output nil 0.05)
            (should-not result)
            (should (equal before (directory-files temporary-file-directory nil "^mevedel-history-search-"))))
        (when cancel (funcall cancel))
        (mevedel-request-drain-cancellers request)
        (when (buffer-live-p owner) (kill-buffer owner))
        (mevedel-execution-teardown-all)
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-resource-prepare/saved-history ()
  ,test
  (test)
  :doc "validates scope before access and leaves discovery to the authorized handler"
  (let* ((root (make-temp-file "mevedel-history-prepare-" t))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (native (symbol-function 'mevedel-session-control-fs-run-program))
         touched)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (&rest args) (setq touched t) (apply native args))))
                 (should (plist-get (mevedel-resource-parse-address "history://saved") :dynamic-p))
                 (should (eq 'workspace-relative
                             (plist-get (mevedel-resource-parse-address "history://saved/session") :locator-class)))
                 (dolist (address '("history://saved" "history://saved/session"
                                    "history://saved/session/segment-0001.chat.org"))
                   (dolist (operation '(read glob grep))
                     (let ((attempt (mevedel-resource-prepare operation address (list :workspace workspace))))
                       (should attempt)
                       (mevedel-resource-discard-attempts (list attempt))))
                   (should-error (mevedel-resource-prepare 'apply-patch address (list :workspace workspace))))
                 (dolist (address '("history://saved/a/../b" "history://saved/a/.lock"
                                    "history://saved/a/segment-1.chat.org" "history://saved/a/b/c"
                                    "history://saved/a%2Fb" "history://saved/a#fragment"))
                   (should-error (mevedel-resource-prepare 'read address (list :workspace workspace))))
                 (should-not touched))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-publication-read-batch (:quiet t)
  ,test
  (test)
  :doc "batches portable publication observations without losing source evidence"
  (let* ((root (make-temp-file "mevedel-history-batch-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (native (symbol-function 'process-file))
         (programs 0)
         pairs)
    (unwind-protect
        (progn
          (dotimes (index 6)
            (push (test-mevedel-history-search--source
                   workspace (format "source-%d" index)
                   (format "Batch evidence canary-%d\n" index)) pairs))
          (let ((result
                 (cl-letf (((symbol-function 'process-file)
                            (lambda (&rest args)
                              (cl-incf programs)
                              (apply native args))))
                          (test-mevedel-history-search--query
                           workspace 'grep
                           '(:path "history://saved" :pattern "Batch evidence" :output_mode "content")))))
            (dotimes (index 6)
              (should (string-search (format "canary-%d" index) (plist-get result :result))))
            ;; Two discovery programs, three publication stages, one pinned
            ;; transcript batch and at most one search-binary availability probe.
            (should (<= programs 7))))
      (dolist (pair pairs)
        (when (buffer-live-p (cdr pair))
          (with-current-buffer (cdr pair) (set-buffer-modified-p nil))
          (kill-buffer (cdr pair)))
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path (car pair)) (car pair)))
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/publication-boundaries (:quiet t)
  ,test
  (test)
  :doc "keeps a captured publication coherent and rejects unpublished or invalid evidence"
  (let* ((root (make-temp-file "mevedel-history-boundaries-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (source (test-mevedel-history-search--source workspace "changing" "Evidence OLD\n"))
         (other (test-mevedel-history-search--source workspace "stable" "Evidence STABLE\n"))
         (session (car source))
         (directory (mevedel-session-save-path session))
         (native-next (symbol-function 'iter-next))
         (args '(:path "history://saved" :pattern "Evidence" :output_mode "content"))
         published)
    (unwind-protect
        (progn
          (let ((result
                 (cl-letf (((symbol-function 'iter-next)
                            (lambda (&rest arguments)
                              (let ((value (apply native-next arguments)))
                                (when (and (eq value 'reads) (not published))
                                  (setq published t)
                                  (with-current-buffer (cdr source)
                                    (goto-char (point-min))
                                    (search-forward "OLD")
                                    (replace-match "NEW" t t)
                                    (mevedel-session-artifacts-save session (current-buffer))))
                                value))))
                          (test-mevedel-history-search--query workspace 'grep args))))
            (should published)
            (should (string-search "Evidence OLD" (plist-get result :result)))
            (should-not (string-search "Evidence NEW" (plist-get result :result))))
          (should (string-search "Evidence NEW"
                                 (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          ;; A loose transcript has never been committed into the publication.
          (write-region "Evidence UNPUBLISHED" nil
                        (file-name-concat directory "segment-9999.chat.org") nil 'silent)
          (should-not (string-search "UNPUBLISHED"
                                     (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
          ;; Corrupt control data cannot recover the warm projection. A broken
          ;; candidate also must not prevent other valid sessions being searched.
          (let* ((head (plist-get (mevedel-session-publication session) :head))
                 (manifest (file-name-concat directory head)))
            (write-region "(:broken t)" nil manifest nil 'silent)
            (let ((text (plist-get (test-mevedel-history-search--query workspace 'grep args) :result)))
              (should (string-search "Evidence STABLE" text))
              (should (string-search "Skipped 1 unavailable or incompatible" text))
              (should-not (string-search "Evidence NEW" text))
              (should-not (string-search root text)))))
      (dolist (pair (list source other))
        (test-mevedel-session-persistence--release-and-kill (cdr pair) (car pair)))
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/helper-owner-close (:quiet t)
  ,test
  (test)
  :doc "owner close at helper launch releases the real child without normal delivery"
  (dolist (kind '(buffer request))
    (let* ((root (make-temp-file "mevedel-history-helper-owner-" t))
           (temporary-file-directory (file-name-as-directory root))
           (workspace (test-mevedel-session-persistence--make-file-workspace root))
           (owner (generate-new-buffer " *history-helper-owner*"))
           (request (mevedel-request--create :origin "/root"))
           (native-helper (symbol-function 'mevedel-execution-start-helper))
           (native-process (symbol-function 'make-process))
           (before (directory-files temporary-file-directory nil "^mevedel-history-search-"))
           child reached result cancel)
      (unwind-protect
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (let ((process (apply native-process args)))
                         (when (string-search "mevedel-grep" (or (plist-get args :name) ""))
                           (setq child process))
                         process)))
                    ((symbol-function 'mevedel-execution-start-helper)
                     (lambda (callback name command paths roots &rest keys)
                       (let ((handle (apply native-helper callback name
                                            (append '("sh" "-c" "sleep 2; exec \"$@\"" "history-test") command)
                                            paths roots keys)))
                         (setq reached t)
                         (if (eq kind 'buffer) (kill-buffer owner)
                           (mevedel-request-drain-cancellers request))
                         handle))))
                   (with-current-buffer owner
                     (setq-local mevedel--current-request request)
                     (let* ((attempt (mevedel-resource-prepare 'grep "history://saved" (list :workspace workspace)))
                            (mevedel-resource-current-attempts (list (cons "history://saved" attempt))))
                       (setq cancel (mevedel-tool-fs-search-grep
                                     (lambda (value) (setq result value))
                                     '(:path "history://saved" :pattern "needle")))))
                   (with-timeout (5 (ert-fail "History helper did not launch"))
                     (while (not reached) (accept-process-output nil 0.01)))
                   (accept-process-output nil 0.15)
                   (should child)
                   (should-not (process-live-p child))
                   (should-not result)
                   (should (equal before (directory-files temporary-file-directory nil "^mevedel-history-search-"))))
        (when cancel (funcall cancel))
        (mevedel-request-drain-cancellers request)
        (when (buffer-live-p owner) (kill-buffer owner))
        (mevedel-execution-teardown-all)
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-history-search-start/selected-pagination (:quiet t)
  ,test
  (test)
  :doc "selected source pagination shares Read line references and workspace caches stay isolated"
  (let* ((root (make-temp-file "mevedel-history-pages-" t))
         (one-root (file-name-concat root "one"))
         (two-root (file-name-concat root "two"))
         (_ (make-directory one-root))
         (_ (make-directory two-root))
         (one (test-mevedel-session-persistence--make-file-workspace one-root))
         (two (test-mevedel-session-persistence--make-file-workspace two-root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (first (test-mevedel-history-search--source
                 one "source" (mapconcat (lambda (index) (format "Pagination A-%d\n" index))
                                         (number-sequence 0 9) "")))
         (second (test-mevedel-history-search--source two "source" "Pagination B-private\n"))
         (address (concat "history://saved/"
                          (mevedel-resource-encode-component
                           (file-name-nondirectory (directory-file-name (mevedel-session-save-path (car first)))))
                          "/segment-0001.chat.org"))
         (sessions-root (directory-file-name (mevedel-session-artifacts-sessions-dir one)))
         (native-list (symbol-function 'directory-files)))
    (unwind-protect
        (progn
          (dolist (glob '("segment-*.chat.org" "segment-0001.chat.org"))
            (dolist (scope (list "history://saved" address))
              (should
               (string-search
                "Pagination A-0"
                (plist-get
                 (test-mevedel-history-search--query
                  one 'grep (list :path scope :pattern "Pagination" :glob glob
                                  :output_mode "content" :head_limit 1))
                 :result)))))
          (cl-letf (((symbol-function 'directory-files)
                     (lambda (directory &rest args)
                       (when (equal (directory-file-name directory) sessions-root)
                         (ert-fail "Selected source enumerated every session"))
                       (apply native-list directory args))))
                   (let* ((text (plist-get
                                 (test-mevedel-history-search--query
                                  one 'grep (list :path address :pattern "Pagination" :glob "*.org"
                                                  :output_mode "content" :head_limit 2 :offset 3)) :result))
                          (line (and (string-match (concat (regexp-quote address) ":\\([0-9]+\\):Pagination A-3") text)
                                     (string-to-number (match-string 1 text)))))
                     (should line)
                     (should (string-search "Pagination A-4" text))
                     (should-not (string-search "Pagination A-2" text))
                     (should-not (string-search "Pagination A-5" text))
                     (should (string-search address text))
                     (should (string-search
                              "Pagination A-3"
                              (plist-get (test-mevedel-history-search--query
                                          one 'read (list :file_path address :offset line :limit 1)) :result)))))
          (let ((text (plist-get (test-mevedel-history-search--query
                                  two 'grep '(:path "history://saved" :pattern "Pagination" :output_mode "content")) :result)))
            (should (string-search "Pagination B-private" text))
            (should-not (string-search "Pagination A-" text)))
          (let ((text (plist-get (test-mevedel-history-search--query
                                  one 'grep '(:path "history://saved" :pattern "Pagination" :output_mode "content")) :result)))
            (should (string-search "Pagination A-0" text))
            (should-not (string-search "Pagination B-private" text))))
      (dolist (pair (list first second))
        (test-mevedel-session-persistence--release-and-kill (cdr pair) (car pair)))
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-history-search-start/global-format-bound (:quiet t)
  ,test
  (test)
  :doc "formats only the requested global content page across dense sources"
  (let* ((root (make-temp-file "mevedel-history-global-page-" t))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (native-rewrite (symbol-function 'mevedel-tool-fs-resource-child-address))
         (rewrites 0)
         sources)
    (unwind-protect
        (progn
          (dotimes (index 6)
            (push (test-mevedel-history-search--source
                   workspace (format "dense-%d" index)
                   (mapconcat (lambda (line) (format "Dense evidence %d/%d\n" index line))
                              (number-sequence 0 19) "")) sources))
          (cl-letf (((symbol-function 'mevedel-tool-fs-resource-child-address)
                     (lambda (&rest args)
                       (cl-incf rewrites)
                       (apply native-rewrite args))))
                   (let ((text (plist-get
                                (test-mevedel-history-search--query
                                 workspace 'grep '(:path "history://saved" :pattern "Dense evidence"
                                                         :output_mode "content" :offset 1 :head_limit 2))
                                :result)))
                     (should (string-match-p "history://saved/[^\n]+:[0-9]+:Dense evidence 0/1" text))
                     (should (string-search "Dense evidence 0/2" text))
                     (should-not (string-search "Dense evidence 0/0" text))
                     (should-not (string-search "Dense evidence 0/3" text))
                     (should (string-search "Results truncated" text))
                     ;; Two page lines, one skipped line, one truncation sentinel.
                     (should (<= rewrites 4)))))
      (dolist (source sources)
        (test-mevedel-session-persistence--release-and-kill (cdr source) (car source)))
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-read-sidecar-batch (:quiet t)
  ,test
  (test)
  :doc "batches fresh PID metadata and rejects changed schema with a warm cache"
  (let* ((root (make-temp-file "mevedel-history-pid-metadata-" t))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (mevedel-history-search--cache (make-hash-table :test #'equal))
         (mevedel-history-search--cache-bytes 0)
         (native (symbol-function 'process-file))
         (programs 0)
         sources)
    (unwind-protect
        (progn
          (dotimes (index 6)
            (push (test-mevedel-history-search--source
                   workspace (format "source-%d" index)
                   (format "Batch evidence canary-%d\n" index)) sources))
          (let ((text
                 (plist-get
                  (cl-letf (((symbol-function 'process-file)
                             (lambda (&rest args)
                               (cl-incf programs)
                               (apply native args))))
                           (test-mevedel-history-search--query
                            workspace 'grep '(:path "history://saved" :pattern "Batch evidence"
                                                    :output_mode "content"))) :result)))
            (dotimes (index 6)
              (should (string-search (format "canary-%d" index) text)))
            ;; Control probes, sidecars, transcripts, optional binary probe.
            (should (<= programs 4)))
          (let* ((path (file-name-concat (mevedel-session-save-path (caar sources))
                                         "session.meta.el"))
                 (attributes (file-attributes path))
                 (stamp (file-attribute-modification-time attributes))
                 (size (file-attribute-size attributes))
                 (replacement "(:version 0)"))
            (with-temp-file path
              (insert replacement (make-string (- size (length replacement)) ?\s)))
            (set-file-times path stamp)
            (should (= size (file-attribute-size (file-attributes path))))
            (let ((text (plist-get
                         (test-mevedel-history-search--query
                          workspace 'grep '(:path "history://saved" :pattern "Batch evidence"
                                                  :output_mode "content")) :result)))
              (should-not (string-search "canary-5" text))
              (should (string-search "canary-0" text))
              (should (string-search "Skipped 1" text)))))
      (dolist (source sources)
        (test-mevedel-session-persistence--release-and-kill (cdr source) (car source)))
      (mevedel-execution-teardown-all)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-history-search)
;;; test-mevedel-history-search.el ends here
