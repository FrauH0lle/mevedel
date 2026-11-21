;;; test-niwa-resource-journal.el -- Published journal resources -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises the closed public journal surface through ordinary resource tools.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-store)
(require 'niwa-resource)
(require 'niwa-resource-capf)
(require 'niwa-system)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-fs-search)
(require 'niwa-workspace)

(defun test-niwa-resource-journal--publish (root &optional id)
  "Publish one real digest under ROOT using optional capture ID."
  (niwa-journal-store-publish-digest
   root (list :capture-id (or id (make-string 64 ?a)) :session "source-session"
              :session-name "Source" :workspace (make-string 64 ?b)
              :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
              :turns '(1) :turn-ids (list (make-string 64 ?d))
              :created "2026-09-07T12:00:00Z" :model "provider:model")
   "## Done\n- Observed: The target passed (turn 1).\n\n## Learned\n- User: Prefer the local runner (turn 1).\n\n## Surprised\n- none\n\n## Unfinished\n- none"))

(niwa-deftest niwa-resource-execute/journal-read (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reads validated public records and lists only published entries without a session"
  (let* ((root (make-temp-file "niwa-resource-journal-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root :name "journal"))
         (context (list :workspace workspace))
         (cell (list nil)))
    (unwind-protect
        (let* ((entry (test-niwa-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (niwa-resource-encode-component file)))
               (directory (niwa-journal-store-directory root)))
          (make-directory (niwa-journal-store-state-directory directory) t)
          (write-region "Private evidence" nil (file-name-concat (niwa-journal-store-state-directory directory) "job.md") nil 'silent)
          (write-region "Malformed entry" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (plist-put context :resource-attempts-cell cell)
          (let* ((listing (niwa-resource-execute (niwa-resource-prepare 'read "memory://journal/" context)))
                 (read (niwa-resource-execute (niwa-resource-prepare 'read address context))))
            (should (equal address (plist-get listing :result)))
            (should (string-prefix-p "---\nkind: digest" (plist-get read :result)))
            (should (string-match-p "Prefer the local runner" (plist-get read :result)))
            (should-not (string-match-p (regexp-quote root) (plist-get listing :result)))))
      (niwa-resource-discard-attempts (car cell))
      (delete-directory root t))))

(niwa-deftest niwa-resource-execute/journal-recall-expiry (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "expiry hides retained evidence from exact, prepared, cached and ordinary tool reads"
  (let* ((root (make-temp-file "niwa-journal-recall-" t))
         (workspace (niwa-workspace--create :root root))
         (cell (list nil))
         (context (list :workspace workspace :resource-attempts-cell cell))
         (niwa-journal-max-age-days 14)
         (clock (symbol-function 'float-time))
         (now (float-time (date-to-time "2026-09-21T11:59:59Z"))))
    (unwind-protect
        (let* ((entry (test-niwa-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (niwa-resource-encode-component file))))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional value) (if value (funcall clock value) now))))
            (should (niwa-journal-index-prompt workspace))
            (should (niwa-journal-index-entries workspace t))
            (let ((attempt (niwa-resource-prepare 'read address context)))
              (setq now (1+ now))
              (should-error (niwa-resource-execute attempt)
                            :type 'niwa-resource-unavailable))
            ;; The cached observation crosses the boundary without a new scan.
            (should-not (niwa-journal-index-entries workspace t))
            (should-not (niwa-journal-index-prompt workspace))
            (should-not (plist-get (niwa-resource-completion-metadata
                                   (list :workspace workspace) 'memory) :journal))
            (dolist (operation '(read glob grep))
              (let* ((attempt (niwa-resource-prepare operation "memory://journal/" context))
                     (niwa-resource-current-attempts (list (cons "memory://journal/" attempt)))
                     result done)
                (pcase operation
                  ('read (setq result (niwa-test--read '(:file_path "memory://journal/")) done t))
                  ('glob (niwa-tool-fs-search-glob
                          (lambda (value) (setq result value done t))
                          '(:path "memory://journal/" :pattern "*.md")))
                  ('grep (niwa-tool-fs-search-grep
                          (lambda (value) (setq result value done t))
                          '(:path "memory://journal/" :pattern "runner" :output_mode "content"))))
                (with-timeout (5 (ert-fail "Expired journal query did not settle"))
                  (while (not done) (accept-process-output nil 0.01)))
                (should-not (eq 'error (plist-get result :status)))
                (should-not (string-search file (plist-get result :result)))
                (should-not (string-search "Prefer the local runner" (plist-get result :result)))))
            ;; Ordinary expiry does not erase storage needed by native recovery.
            (should (equal entry (niwa-journal-store-read root file)))
            (let ((niwa-journal-max-age-days nil))
              (should (niwa-journal-index-entries workspace t))
              (should (string-search "Prefer the local runner"
                                     (plist-get (niwa-resource-execute
                                                 (niwa-resource-prepare 'read address context))
                                                :result))))))
      (niwa-resource-discard-attempts (car cell))
      (niwa-execution-teardown-all)
      (delete-directory root t))))

(niwa-deftest niwa-resource-prepare/journal-validation (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "rejects private, nested, traversing, and mutable journal addresses before execution"
  (dolist (address '("memory://journal/state" "memory://journal/state/job.md" "memory://journal/../secret" "memory://journal/a/b"
                     "memory://journal/%2E%2E" "memory://journal/note.md" "memory://journal/#fragment"))
    (should-error (niwa-resource-prepare 'read address nil) :type 'niwa-resource-error))
  (should-error (niwa-resource-prepare 'apply-patch "memory://journal/" nil) :type 'niwa-resource-error))

(niwa-deftest niwa-resource-parse-address/journal-namespace (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reserves the workspace journal branch without retaining the old scheme"
  (let ((file (concat "2026-09-07T12:00:00Z-" (make-string 64 ?a) ".md")))
    (dolist (tail (list "" (niwa-resource-encode-component file)))
      (let* ((address (concat "memory://journal/" tail))
             (parsed (niwa-resource-parse-address address)))
        (should (eq 'memory (plist-get parsed :scheme)))
        (should (equal address (plist-get parsed :canonical)))
        (should (equal "journal" (car (plist-get parsed :components))))
        (should-error (niwa-resource-prepare 'apply-patch address nil)
                      :type 'niwa-resource-error)))
    (dolist (address (list "journal://" (concat "journal://" file)
                           "memory://journal/root/topic.md"
                           "memory://journal//" "memory://journal/state"))
      (should-error (niwa-resource-parse-address address)
                    :type 'niwa-resource-error))))

(defun test-niwa-resource-journal--search (workspace operation args)
  "Run real journal search OPERATION with ARGS in WORKSPACE."
  (let* ((address (plist-get args :path))
         (attempt (niwa-resource-prepare operation address (list :workspace workspace :args args)))
         (niwa-resource-current-attempts (list (cons address attempt)))
         (stop (+ (float-time) 10))
         (start-helper (symbol-function 'niwa-execution-start-helper))
         snapshots result done)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-execution-start-helper)
                     (lambda (&rest arguments)
                       (setq snapshots (nth 3 arguments))
                       (apply start-helper arguments))))
            (funcall (if (eq operation 'glob) #'niwa-tool-fs-search-glob #'niwa-tool-fs-search-grep)
                     (lambda (value) (setq result (plist-get value :result) done t)) args))
          (while (and (not done) (< (float-time) stop)) (accept-process-output nil 0.05))
          (should done)
          (should snapshots)
          (dolist (snapshot snapshots)
            (should (string-prefix-p "niwa-resource-search-" (file-name-nondirectory snapshot)))
            (should-not (file-exists-p snapshot)))
          result)
      (niwa-resource-discard-attempts (list attempt)))))

(niwa-deftest niwa-resource-execute/journal-search (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "ordinary Glob and Grep options search only validated snapshots and return journal addresses"
  (let* ((root (make-temp-file "niwa-resource-journal-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root :name "journal")))
    (unwind-protect
        (let* ((entry (test-niwa-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (niwa-resource-encode-component file)))
               (directory (niwa-journal-store-directory root)))
          (make-directory (niwa-journal-store-state-directory directory) t)
          (write-region "local runner PRIVATE" nil (file-name-concat (niwa-journal-store-state-directory directory) "evidence.md") nil 'silent)
          (write-region "local runner MALFORMED" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (should (equal address (string-trim (test-niwa-resource-journal--search
                                              workspace 'glob '(:path "memory://journal/" :pattern "*.md")))))
          (dolist (mode '("files_with_matches" "count" "content"))
            (let ((result (test-niwa-resource-journal--search
                           workspace 'grep (list :path "memory://journal/" :pattern "LOCAL.*runner" :-i t
                                                 :glob "*.md" :output_mode mode))))
              (should (string-match-p (regexp-quote address) result))
              (should-not (string-match-p "PRIVATE\\|MALFORMED\\|niwa-resource-search-" result))
              (pcase mode
                ("content"
                 (should (string-match-p (concat (regexp-quote address) ":[0-9]+:") result))
                 (should (string-match-p "Prefer the local runner" result)))
                ("count" (should (string-match-p (concat (regexp-quote address) ":[0-9]+") result)))
                ("files_with_matches" (should (equal address (string-trim result)))))))
          (let ((result (test-niwa-resource-journal--search
                         workspace 'grep (list :path address :pattern "Observed" :output_mode "content"))))
            (should (string-match-p (regexp-quote address) result))
            (should-not (string-match-p (regexp-quote (concat address "/")) result))))
      (niwa-execution-teardown-all)
      (delete-directory root t))))

(niwa-deftest niwa-resource-execute/journal-freshness (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "revalidates published bytes after preparation and retains normal Read pagination"
  (let* ((root (make-temp-file "niwa-resource-journal-" t))
         (workspace (niwa-workspace--create :root root))
         (cell (list nil))
         (context (list :workspace workspace :resource-attempts-cell cell)))
    (unwind-protect
        (let* ((entry (test-niwa-resource-journal--publish root))
               (file (plist-get entry :file))
               (path (file-name-concat (niwa-journal-store-directory root) file))
               (address (concat "memory://journal/" (niwa-resource-encode-component file))))
          (let* ((attempt (niwa-resource-prepare 'read address context))
                 (niwa-resource-current-attempts (list (cons address attempt)))
                 (text (plist-get
                        (niwa-test--read (list :file_path address :offset 1 :limit 2))
                        :result)))
            (should (string-match-p "kind: digest" text))
            (should-not (string-match-p "Prefer the local runner" text)))
          (dolist (mutation '(corrupt delete symlink))
            (let ((attempt (niwa-resource-prepare 'read address context)))
              (pcase mutation
                ('corrupt (write-region "Invalid public record" nil path nil 'silent))
                ('delete (delete-file path))
                ('symlink (make-symbolic-link (file-name-concat root "secret") path)))
              (let ((error (should-error (niwa-resource-execute attempt)
                                        :type (if (eq mutation 'symlink)
                                                  'niwa-resource-error
                                                'niwa-resource-unavailable))))
                (should-not (string-match-p (regexp-quote root) (error-message-string error)))
                (should (string-search address (cadr error)))
                (when (eq mutation 'corrupt)
                  (should (string-search "Invalid frontmatter" (cadr error))))
                (when (eq mutation 'delete)
                  (should (string-search "not found" (cadr error))))))))
      (niwa-resource-discard-attempts (car cell))
      (delete-directory root t))))

(niwa-deftest niwa-resource-execute/journal-remote (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reads target-native journal evidence and searches a validated local copy without a session"
  (let ((root (make-temp-file "niwa-resource-journal-remote-" t)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("journal-resource")
          (let* ((entry (test-niwa-resource-journal--publish root))
                 (workspace (niwa-workspace--create
                             :root (format "/niwamock:journal-resource:%s/" root)))
                 (address (concat "memory://journal/" (niwa-resource-encode-component
                                                (plist-get entry :file)))))
            (should (string-match-p
                     (regexp-quote address)
                     (test-niwa-resource-journal--search
                      workspace 'grep '(:path "memory://journal/" :pattern "Prefer" :output_mode "content"))))))
      (niwa-execution-teardown-all)
      (delete-directory root t))))

(niwa-deftest niwa-tool-fs-search--documents (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "destroys the snapshot when real helper admission fails or the helper is torn down"
  (dolist (teardown '(nil t))
    (let* ((start-helper (symbol-function 'niwa-execution-start-helper))
           (descriptor '(:address "memory://journal/" :scheme memory
                         :resource-search-documents (("one.md" . "Observed evidence"))))
           snapshot)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-execution-start-helper)
                     (lambda (&rest arguments)
                       (setq snapshot (car (nth 3 arguments)))
                       (if teardown (apply start-helper arguments)
                         (error "Injected admission failure")))))
            (if teardown
                (progn
                  (niwa-tool-fs-search--documents
                   (lambda (_value) (ert-fail "A torn-down helper must not deliver a result"))
                   '(:pattern "*.md") descriptor #'niwa-tool-fs-search-glob)
                  (niwa-execution-teardown-all))
              (let (results)
                (niwa-tool-fs-search--documents
                 (lambda (value) (push value results))
                 '(:pattern "*.md") descriptor #'niwa-tool-fs-search-glob)
                (should (= 1 (length results)))
                (should (string-prefix-p "Error:" (plist-get (car results) :result)))
                (should (string-search "memory://journal/" (plist-get (car results) :result)))
                (should-not (string-search "Injected admission failure"
                                           (plist-get (car results) :result)))))
            (should snapshot)
            (should-not (file-exists-p snapshot)))
        (niwa-execution-teardown-all)))))

(niwa-deftest niwa-resource-capf--journal (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "offers only previously validated filenames without reading public or private storage"
  (let* ((root (make-temp-file "niwa-resource-journal-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace)))
    (unwind-protect
        (let* ((entry (test-niwa-resource-journal--publish root))
               (address (concat "memory://journal/" (niwa-resource-encode-component (plist-get entry :file)))))
          (should-not (plist-get (niwa-resource-completion-metadata
                                  (list :workspace workspace) 'memory) :journal))
          (should (plist-get (niwa-resource-completion-metadata
                             (list :workspace workspace)) :journal))
          (cl-letf (((symbol-function 'niwa-journal-store-entries)
                     (lambda (&rest _) (ert-fail "Completion read storage"))))
            (with-temp-buffer
              (setq-local niwa--session session)
              (dolist (prefix '("memory://journal/" "memory://journal/2026"))
                (erase-buffer)
                (insert prefix)
                (let ((completion (niwa-resource-capf)))
                  (should (equal (list address) (all-completions "" (nth 2 completion)))))))))
      (delete-directory root t))))

(niwa-deftest niwa-system--resource-roster/journal (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "advertises published evidence even when capture is disabled, excluding pending-only state"
  (let* ((root (make-temp-file "niwa-resource-journal-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :working-directory root))
         (context (niwa-system-context--create
                   :workspace workspace :working-directory root :session session)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "pending") t)
          (should-not (string-match-p "memory://journal/" (niwa-system--resource-roster context)))
          (test-niwa-resource-journal--publish root)
          (setf (niwa-workspace-journal-observation workspace) nil)
          (let ((roster (niwa-system--resource-roster context)))
            (should (string-match-p "memory://journal/" roster))
            (should (string-match-p "dated evidence" roster))))
      (delete-directory root t))))

(provide 'test-niwa-resource-journal)
;;; test-niwa-resource-journal.el ends here
