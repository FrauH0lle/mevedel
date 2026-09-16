;;; test-mevedel-resource-journal.el -- Published journal resources -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises the closed public journal surface through ordinary resource tools.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-store)
(require 'mevedel-resource)
(require 'mevedel-resource-capf)
(require 'mevedel-system)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-workspace)

(defun test-mevedel-resource-journal--publish (root &optional id)
  "Publish one real digest under ROOT using optional capture ID."
  (mevedel-journal-store-publish-digest
   root (list :capture-id (or id (make-string 64 ?a)) :session "source-session"
              :session-name "Source" :workspace (make-string 64 ?b)
              :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
              :turns '(1) :turn-ids (list (make-string 64 ?d))
              :created "2026-09-07T12:00:00Z" :model "provider:model")
   "## Done\n- Observed: The target passed (turn 1).\n\n## Learned\n- User: Prefer the local runner (turn 1).\n\n## Surprised\n- none\n\n## Unfinished\n- none"))

(mevedel-deftest mevedel-resource-execute/journal-read (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reads validated public records and lists only published entries without a session"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root :name "journal"))
         (context (list :workspace workspace))
         (cell (list nil)))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (mevedel-resource-encode-component file)))
               (directory (mevedel-journal-store-directory root)))
          (make-directory (mevedel-journal-store-state-directory directory) t)
          (write-region "Private evidence" nil (file-name-concat (mevedel-journal-store-state-directory directory) "job.md") nil 'silent)
          (write-region "Malformed entry" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (plist-put context :resource-attempts-cell cell)
          (let* ((listing (mevedel-resource-execute (mevedel-resource-prepare 'read "memory://journal/" context)))
                 (read (mevedel-resource-execute (mevedel-resource-prepare 'read address context))))
            (should (equal address (plist-get listing :result)))
            (should (string-prefix-p "---\nkind: digest" (plist-get read :result)))
            (should (string-match-p "Prefer the local runner" (plist-get read :result)))
            (should-not (string-match-p (regexp-quote root) (plist-get listing :result)))))
      (mevedel-resource-discard-attempts (car cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/journal-recall-expiry (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "expiry hides retained evidence from exact, prepared, cached and ordinary tool reads"
  (let* ((root (make-temp-file "mevedel-journal-recall-" t))
         (workspace (mevedel-workspace--create :root root))
         (cell (list nil))
         (context (list :workspace workspace :resource-attempts-cell cell))
         (mevedel-journal-max-age-days 14)
         (clock (symbol-function 'float-time))
         (now (float-time (date-to-time "2026-09-21T11:59:59Z"))))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (mevedel-resource-encode-component file))))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional value) (if value (funcall clock value) now))))
            (should (mevedel-journal-index-prompt workspace))
            (should (mevedel-journal-index-entries workspace t))
            (let ((attempt (mevedel-resource-prepare 'read address context)))
              (setq now (1+ now))
              (should-error (mevedel-resource-execute attempt)
                            :type 'mevedel-resource-unavailable))
            ;; The cached observation crosses the boundary without a new scan.
            (should-not (mevedel-journal-index-entries workspace t))
            (should-not (mevedel-journal-index-prompt workspace))
            (should-not (plist-get (mevedel-resource-completion-metadata
                                   (list :workspace workspace) 'memory) :journal))
            (dolist (operation '(read glob grep))
              (let* ((attempt (mevedel-resource-prepare operation "memory://journal/" context))
                     (mevedel-resource-current-attempts (list (cons "memory://journal/" attempt)))
                     result done)
                (pcase operation
                  ('read (setq result (mevedel-test--read '(:file_path "memory://journal/")) done t))
                  ('glob (mevedel-tool-fs-search-glob
                          (lambda (value) (setq result value done t))
                          '(:path "memory://journal/" :pattern "*.md")))
                  ('grep (mevedel-tool-fs-search-grep
                          (lambda (value) (setq result value done t))
                          '(:path "memory://journal/" :pattern "runner" :output_mode "content"))))
                (with-timeout (5 (ert-fail "Expired journal query did not settle"))
                  (while (not done) (accept-process-output nil 0.01)))
                (should-not (eq 'error (plist-get result :status)))
                (should-not (string-search file (plist-get result :result)))
                (should-not (string-search "Prefer the local runner" (plist-get result :result)))))
            ;; Ordinary expiry does not erase storage needed by native recovery.
            (should (equal entry (mevedel-journal-store-read root file)))
            (let ((mevedel-journal-max-age-days nil))
              (should (mevedel-journal-index-entries workspace t))
              (should (string-search "Prefer the local runner"
                                     (plist-get (mevedel-resource-execute
                                                 (mevedel-resource-prepare 'read address context))
                                                :result))))))
      (mevedel-resource-discard-attempts (car cell))
      (mevedel-execution-teardown-all)
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-prepare/journal-validation (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "rejects private, nested, traversing, and mutable journal addresses before execution"
  (dolist (address '("memory://journal/state" "memory://journal/state/job.md" "memory://journal/../secret" "memory://journal/a/b"
                     "memory://journal/%2E%2E" "memory://journal/note.md" "memory://journal/#fragment"))
    (should-error (mevedel-resource-prepare 'read address nil) :type 'mevedel-resource-error))
  (should-error (mevedel-resource-prepare 'apply-patch "memory://journal/" nil) :type 'mevedel-resource-error))

(mevedel-deftest mevedel-resource-parse-address/journal-namespace (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reserves the workspace journal branch without retaining the old scheme"
  (let ((file (concat "2026-09-07T12:00:00Z-" (make-string 64 ?a) ".md")))
    (dolist (tail (list "" (mevedel-resource-encode-component file)))
      (let* ((address (concat "memory://journal/" tail))
             (parsed (mevedel-resource-parse-address address)))
        (should (eq 'memory (plist-get parsed :scheme)))
        (should (equal address (plist-get parsed :canonical)))
        (should (equal "journal" (car (plist-get parsed :components))))
        (should-error (mevedel-resource-prepare 'apply-patch address nil)
                      :type 'mevedel-resource-error)))
    (dolist (address (list "journal://" (concat "journal://" file)
                           "memory://journal" "memory://journal/root/topic.md"
                           "memory://journal//" "memory://journal/state"))
      (should-error (mevedel-resource-parse-address address)
                    :type 'mevedel-resource-error))))

(defun test-mevedel-resource-journal--search (workspace operation args)
  "Run real journal search OPERATION with ARGS in WORKSPACE."
  (let* ((address (plist-get args :path))
         (attempt (mevedel-resource-prepare operation address (list :workspace workspace :args args)))
         (mevedel-resource-current-attempts (list (cons address attempt)))
         (stop (+ (float-time) 10))
         (start-helper (symbol-function 'mevedel-execution-start-helper))
         snapshots result done)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest arguments)
                       (setq snapshots (nth 3 arguments))
                       (apply start-helper arguments))))
            (funcall (if (eq operation 'glob) #'mevedel-tool-fs-search-glob #'mevedel-tool-fs-search-grep)
                     (lambda (value) (setq result (plist-get value :result) done t)) args))
          (while (and (not done) (< (float-time) stop)) (accept-process-output nil 0.05))
          (should done)
          (should snapshots)
          (dolist (snapshot snapshots)
            (should (string-prefix-p "mevedel-resource-search-" (file-name-nondirectory snapshot)))
            (should-not (file-exists-p snapshot)))
          result)
      (mevedel-resource-discard-attempts (list attempt)))))

(mevedel-deftest mevedel-resource-execute/journal-search (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "ordinary Glob and Grep options search only validated snapshots and return journal addresses"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root :name "journal")))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "memory://journal/" (mevedel-resource-encode-component file)))
               (directory (mevedel-journal-store-directory root)))
          (make-directory (mevedel-journal-store-state-directory directory) t)
          (write-region "local runner PRIVATE" nil (file-name-concat (mevedel-journal-store-state-directory directory) "evidence.md") nil 'silent)
          (write-region "local runner MALFORMED" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (should (equal address (string-trim (test-mevedel-resource-journal--search
                                              workspace 'glob '(:path "memory://journal/" :pattern "*.md")))))
          (dolist (mode '("files_with_matches" "count" "content"))
            (let ((result (test-mevedel-resource-journal--search
                           workspace 'grep (list :path "memory://journal/" :pattern "LOCAL.*runner" :-i t
                                                 :glob "*.md" :output_mode mode))))
              (should (string-match-p (regexp-quote address) result))
              (should-not (string-match-p "PRIVATE\\|MALFORMED\\|mevedel-resource-search-" result))
              (pcase mode
                ("content"
                 (should (string-match-p (concat (regexp-quote address) ":[0-9]+:") result))
                 (should (string-match-p "Prefer the local runner" result)))
                ("count" (should (string-match-p (concat (regexp-quote address) ":[0-9]+") result)))
                ("files_with_matches" (should (equal address (string-trim result)))))))
          (let ((result (test-mevedel-resource-journal--search
                         workspace 'grep (list :path address :pattern "Observed" :output_mode "content"))))
            (should (string-match-p (regexp-quote address) result))
            (should-not (string-match-p (regexp-quote (concat address "/")) result))))
      (mevedel-execution-teardown-all)
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/journal-freshness (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "revalidates published bytes after preparation and retains normal Read pagination"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :root root))
         (cell (list nil))
         (context (list :workspace workspace :resource-attempts-cell cell)))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (file (plist-get entry :file))
               (path (file-name-concat (mevedel-journal-store-directory root) file))
               (address (concat "memory://journal/" (mevedel-resource-encode-component file))))
          (let* ((attempt (mevedel-resource-prepare 'read address context))
                 (mevedel-resource-current-attempts (list (cons address attempt)))
                 (text (plist-get
                        (mevedel-test--read (list :file_path address :offset 1 :limit 2))
                        :result)))
            (should (string-match-p "kind: digest" text))
            (should-not (string-match-p "Prefer the local runner" text)))
          (dolist (mutation '(corrupt delete symlink))
            (let ((attempt (mevedel-resource-prepare 'read address context)))
              (pcase mutation
                ('corrupt (write-region "Invalid public record" nil path nil 'silent))
                ('delete (delete-file path))
                ('symlink (make-symbolic-link (file-name-concat root "secret") path)))
              (let ((error (should-error (mevedel-resource-execute attempt)
                                        :type (if (eq mutation 'symlink)
                                                  'mevedel-resource-error
                                                'mevedel-resource-unavailable))))
                (should-not (string-match-p (regexp-quote root) (error-message-string error)))
                (should (string-search address (cadr error)))
                (when (eq mutation 'corrupt)
                  (should (string-search "Invalid frontmatter" (cadr error))))
                (when (eq mutation 'delete)
                  (should (string-search "not found" (cadr error))))))))
      (mevedel-resource-discard-attempts (car cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/journal-remote (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "reads target-native journal evidence and searches a validated local copy without a session"
  (let ((root (make-temp-file "mevedel-resource-journal-remote-" t)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("journal-resource")
          (let* ((entry (test-mevedel-resource-journal--publish root))
                 (workspace (mevedel-workspace--create
                             :root (format "/mevedelmock:journal-resource:%s/" root)))
                 (address (concat "memory://journal/" (mevedel-resource-encode-component
                                                (plist-get entry :file)))))
            (should (string-match-p
                     (regexp-quote address)
                     (test-mevedel-resource-journal--search
                      workspace 'grep '(:path "memory://journal/" :pattern "Prefer" :output_mode "content"))))))
      (mevedel-execution-teardown-all)
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-fs-search--documents (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "destroys the snapshot when real helper admission fails or the helper is torn down"
  (dolist (teardown '(nil t))
    (let* ((start-helper (symbol-function 'mevedel-execution-start-helper))
           (descriptor '(:address "memory://journal/" :scheme memory
                         :resource-search-documents (("one.md" . "Observed evidence"))))
           snapshot)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest arguments)
                       (setq snapshot (car (nth 3 arguments)))
                       (if teardown (apply start-helper arguments)
                         (error "Injected admission failure")))))
            (if teardown
                (progn
                  (mevedel-tool-fs-search--documents
                   (lambda (_value) (ert-fail "A torn-down helper must not deliver a result"))
                   '(:pattern "*.md") descriptor #'mevedel-tool-fs-search-glob)
                  (mevedel-execution-teardown-all))
              (should-error
               (mevedel-tool-fs-search--documents
                #'ignore '(:pattern "*.md") descriptor #'mevedel-tool-fs-search-glob)))
            (should snapshot)
            (should-not (file-exists-p snapshot)))
        (mevedel-execution-teardown-all)))))

(mevedel-deftest mevedel-resource-capf--journal (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "offers only previously validated filenames without reading public or private storage"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace)))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (address (concat "memory://journal/" (mevedel-resource-encode-component (plist-get entry :file)))))
          (should-not (plist-get (mevedel-resource-completion-metadata
                                  (list :workspace workspace) 'memory) :journal))
          (should (plist-get (mevedel-resource-completion-metadata
                             (list :workspace workspace)) :journal))
          (cl-letf (((symbol-function 'mevedel-journal-store-entries)
                     (lambda (&rest _) (ert-fail "Completion read storage"))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (dolist (prefix '("memory://journal/" "memory://journal/2026"))
                (erase-buffer)
                (insert prefix)
                (let ((completion (mevedel-resource-capf)))
                  (should (equal (list address) (all-completions "" (nth 2 completion)))))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-system--resource-roster/journal (:vars ((mevedel-journal-max-age-days nil)))
  ,test
  (test)
  :doc "advertises published evidence even when capture is disabled, excluding pending-only state"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :working-directory root))
         (context (mevedel-system-context--create
                   :workspace workspace :working-directory root :session session)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "pending") t)
          (should-not (string-match-p "memory://journal/" (mevedel-system--resource-roster context)))
          (test-mevedel-resource-journal--publish root)
          (setf (mevedel-workspace-journal-observation workspace) nil)
          (let ((roster (mevedel-system--resource-roster context)))
            (should (string-match-p "memory://journal/" roster))
            (should (string-match-p "dated evidence" roster))))
      (delete-directory root t))))

(provide 'test-mevedel-resource-journal)
;;; test-mevedel-resource-journal.el ends here
