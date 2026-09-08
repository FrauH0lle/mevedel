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

(mevedel-deftest mevedel-resource-execute/journal-read ()
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
               (address (concat "journal://" (mevedel-resource-encode-component file)))
               (directory (mevedel-journal-store-directory root)))
          (make-directory (file-name-concat directory "state") t)
          (write-region "Private evidence" nil (file-name-concat directory "state" "job.md") nil 'silent)
          (write-region "Malformed entry" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (plist-put context :resource-attempts-cell cell)
          (let* ((listing (mevedel-resource-execute (mevedel-resource-prepare 'read "journal://" context)))
                 (read (mevedel-resource-execute (mevedel-resource-prepare 'read address context))))
            (should (equal address (plist-get listing :result)))
            (should (string-prefix-p "---\nkind: digest" (plist-get read :result)))
            (should (string-match-p "Prefer the local runner" (plist-get read :result)))
            (should-not (string-match-p (regexp-quote root) (plist-get listing :result)))))
      (mevedel-resource-discard-attempts (car cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-prepare/journal-validation ()
  ,test
  (test)
  :doc "rejects private, nested, traversing, and mutable journal addresses before execution"
  (dolist (address '("journal://state" "journal://state/job.md" "journal://../secret" "journal://a/b"
                     "journal://%2E%2E" "journal://note.md" "journal://#fragment"))
    (should-error (mevedel-resource-prepare 'read address nil) :type 'mevedel-resource-error))
  (should-error (mevedel-resource-prepare 'apply-patch "journal://" nil) :type 'mevedel-resource-error))

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

(mevedel-deftest mevedel-resource-execute/journal-search ()
  ,test
  (test)
  :doc "ordinary Glob and Grep options search only validated snapshots and return journal addresses"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root :name "journal")))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (file (plist-get entry :file))
               (address (concat "journal://" (mevedel-resource-encode-component file)))
               (directory (mevedel-journal-store-directory root)))
          (make-directory (file-name-concat directory "state") t)
          (write-region "local runner PRIVATE" nil (file-name-concat directory "state" "evidence.md") nil 'silent)
          (write-region "local runner MALFORMED" nil
                        (file-name-concat directory (string-replace "2026-09-07" "2026-09-08" file)) nil 'silent)
          (should (equal address (string-trim (test-mevedel-resource-journal--search
                                              workspace 'glob '(:path "journal://" :pattern "*.md")))))
          (dolist (mode '("files_with_matches" "count" "content"))
            (let ((result (test-mevedel-resource-journal--search
                           workspace 'grep (list :path "journal://" :pattern "LOCAL.*runner" :-i t
                                                 :glob "*.md" :output_mode mode))))
              (should (string-match-p (regexp-quote address) result))
              (should-not (string-match-p "PRIVATE\\|MALFORMED\\|mevedel-resource-search-" result))
              (pcase mode
                ("content"
                 (should (string-match-p (concat (regexp-quote address) "\n[0-9]+:") result))
                 (should (string-match-p "Prefer the local runner" result)))
                ("count" (should (string-match-p (concat (regexp-quote address) ":[0-9]+") result)))
                ("files_with_matches" (should (equal address (string-trim result)))))))
          (let ((result (test-mevedel-resource-journal--search
                         workspace 'grep (list :path address :pattern "Observed" :output_mode "content"))))
            (should (string-match-p (regexp-quote address) result))
            (should-not (string-match-p (regexp-quote (concat address "/")) result))))
      (mevedel-execution-teardown-all)
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/journal-freshness ()
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
               (address (concat "journal://" (mevedel-resource-encode-component file))))
          (let* ((attempt (mevedel-resource-prepare 'read address context))
                 (text (mevedel-tool-fs-read--resource '(:offset 1 :limit 2) attempt)))
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
                (should-not (string-match-p (regexp-quote root) (error-message-string error)))))))
      (mevedel-resource-discard-attempts (car cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/journal-remote ()
  ,test
  (test)
  :doc "reads target-native journal evidence and searches a validated local copy without a session"
  (let ((root (make-temp-file "mevedel-resource-journal-remote-" t)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("journal-resource")
          (let* ((entry (test-mevedel-resource-journal--publish root))
                 (workspace (mevedel-workspace--create
                             :root (format "/mevedelmock:journal-resource:%s/" root)))
                 (address (concat "journal://" (mevedel-resource-encode-component
                                                (plist-get entry :file)))))
            (should (string-match-p
                     (regexp-quote address)
                     (test-mevedel-resource-journal--search
                      workspace 'grep '(:path "journal://" :pattern "Prefer" :output_mode "content"))))))
      (mevedel-execution-teardown-all)
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-fs-search--documents ()
  ,test
  (test)
  :doc "destroys the snapshot when real helper admission fails or the helper is torn down"
  (dolist (teardown '(nil t))
    (let* ((start-helper (symbol-function 'mevedel-execution-start-helper))
           (descriptor '(:address "journal://" :scheme journal
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

(mevedel-deftest mevedel-resource-capf--journal ()
  ,test
  (test)
  :doc "offers only previously validated filenames without reading public or private storage"
  (let* ((root (make-temp-file "mevedel-resource-journal-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace)))
    (unwind-protect
        (let* ((entry (test-mevedel-resource-journal--publish root))
               (address (concat "journal://" (mevedel-resource-encode-component (plist-get entry :file)))))
          (should-not (plist-get (mevedel-resource-completion-metadata
                                  (list :workspace workspace) 'journal) :journal))
          (should (plist-get (mevedel-resource-completion-metadata
                             (list :workspace workspace)) :journal))
          (cl-letf (((symbol-function 'mevedel-journal-store-entries)
                     (lambda (&rest _) (ert-fail "Completion read storage"))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (insert "journal://2026")
              (let ((completion (mevedel-resource-capf)))
                (should (equal (list address) (all-completions "" (nth 2 completion))))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-system--resource-roster/journal ()
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
          (make-directory (file-name-concat (mevedel-journal-store-directory root) "state" "pending") t)
          (should-not (string-match-p "journal://" (mevedel-system--resource-roster context)))
          (test-mevedel-resource-journal--publish root)
          (setf (mevedel-workspace-journal-observation workspace) nil)
          (let ((roster (mevedel-system--resource-roster context)))
            (should (string-match-p "journal://" roster))
            (should (string-match-p "dated evidence" roster))))
      (delete-directory root t))))

(provide 'test-mevedel-resource-journal)
;;; test-mevedel-resource-journal.el ends here
