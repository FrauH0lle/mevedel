;;; test-mevedel-session-persistence.el --- Tests for session persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-persistence' (Phase 1: serialization).

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-session-persistence)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-mevedel-session-persistence--make-workspace ()
  "Build a workspace struct registered in the global registry."
  (mevedel-workspace-clear-registry)
  (mevedel-workspace-get-or-create
   'project "test-id" "/tmp/test-proj" "test-proj"))

(defun test-mevedel-session-persistence--make-session ()
  "Build a populated session for round-trip testing."
  (let* ((workspace (test-mevedel-session-persistence--make-workspace))
         (session   (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-permission-mode session) 'default)
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/tmp/foo/**" :action allow)
            ("Bash"  :pattern "git log*" :action allow)
            ("Write" :path "/tmp/bar"    :action deny)))
    (setf (mevedel-session-turn-count session) 5)
    (setf (mevedel-session-session-id session) "main-2026-04-23T14-30-a9f2")
    (setf (mevedel-session-save-path session)
          "/tmp/test-proj/.mevedel/sessions/main-2026-04-23T14-30-a9f2/")
    (setf (mevedel-session-created-at session) "2026-04-23T14-30-00")
    (setf (mevedel-session-updated-at session) "2026-04-23T18-22-11")
    (setf (mevedel-session-current-segment session) 2)
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :pos 142 :preview "Refactor X" :timestamp "..."))) ))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/tmp/foo.el"
                   . (:backup-name "abc@v1" :version 1
                      :backup-time "..." :file-mtime "..."))))))
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "Plan refactor" :status 'completed
                 :owner nil :blocks nil :blocked-by nil :metadata nil)
                (mevedel-task--create
                 :id 2 :subject "Implement permission chain"
                 :description "Replace the deprecated specifier handling"
                 :status 'in-progress
                 :owner "main" :blocks '(1) :blocked-by nil
                 :metadata '(:priority high))))
    session))


;;
;;; Workspace round-trip

(mevedel-deftest mevedel-session-persistence--workspace-to-plist ()
  ,test
  (test)
  :doc "captures all four identity fields"
  (let* ((workspace (test-mevedel-session-persistence--make-workspace))
         (plist     (mevedel-session-persistence--workspace-to-plist workspace)))
    (should (eq 'project    (plist-get plist :type)))
    (should (equal "test-id" (plist-get plist :id)))
    (should (equal "/tmp/test-proj" (plist-get plist :root)))
    (should (equal "test-proj"      (plist-get plist :name))))
  :doc "returns nil for a nil workspace"
  (should (null (mevedel-session-persistence--workspace-to-plist nil))))

(mevedel-deftest mevedel-session-persistence--workspace-from-plist ()
  ,test
  (test)
  :doc "round-trips through the registry"
  (mevedel-workspace-clear-registry)
  (let* ((source     (mevedel-workspace-get-or-create
                      'project "abc" "/tmp/p" "p"))
         (plist      (mevedel-session-persistence--workspace-to-plist source))
         (recovered  (mevedel-session-persistence--workspace-from-plist plist)))
    (should (eq source recovered)))
  :doc "registers from saved tuple when registry is empty"
  (mevedel-workspace-clear-registry)
  (let* ((plist (list :type 'project :id "xyz"
                      :root "/tmp/q" :name "q"))
         (recovered (mevedel-session-persistence--workspace-from-plist plist)))
    (should (eq 'project (mevedel-workspace-type recovered)))
    (should (equal "xyz" (mevedel-workspace-id recovered)))
    (should (equal "/tmp/q" (mevedel-workspace-root recovered))))
  :doc "returns nil for nil plist"
  (should (null (mevedel-session-persistence--workspace-from-plist nil))))


;;
;;; Permission rule hygiene

(mevedel-deftest mevedel-session-persistence--filter-permission-rules ()
  ,test
  (test)
  :doc "keeps allow / deny / ask rules"
  (let ((rules '(("Read" :path "/x" :action allow)
                 ("Bash" :pattern "rm" :action deny)
                 ("Write" :path "/y" :action ask))))
    (should (equal rules
                   (mevedel-session-persistence--filter-permission-rules rules))))
  :doc "drops rules with unknown actions"
  (let* ((rules '(("Read"  :path "/x" :action allow)
                  ("Write" :path "/y" :action future-action)
                  ("Bash"  :pattern "ls" :action allow)))
         (filtered (mevedel-session-persistence--filter-permission-rules rules)))
    (should (= 2 (length filtered)))
    (should (equal "Read" (caar filtered)))
    (should (equal "Bash" (caadr filtered))))
  :doc "drops malformed entries"
  (let ((rules '(("Read" :path "/x" :action allow)
                 nil
                 "not a rule"
                 ("Bash" :pattern "echo" :action allow))))
    (should (= 2 (length
                  (mevedel-session-persistence--filter-permission-rules rules))))))


;;
;;; Task round-trip

(mevedel-deftest mevedel-session-persistence--task-to-plist ()
  ,test
  (test)
  :doc "captures all task fields"
  (let* ((task (mevedel-task--create
                :id 7 :subject "S" :description "D"
                :status 'pending :owner "explore"
                :blocks '(8) :blocked-by '(5 6)
                :metadata '(:priority low :tag "x")))
         (plist (mevedel-session-persistence--task-to-plist task)))
    (should (= 7 (plist-get plist :id)))
    (should (equal "S" (plist-get plist :subject)))
    (should (equal "D" (plist-get plist :description)))
    (should (eq 'pending (plist-get plist :status)))
    (should (equal "explore" (plist-get plist :owner)))
    (should (equal '(8) (plist-get plist :blocks)))
    (should (equal '(5 6) (plist-get plist :blocked-by)))
    (should (equal '(:priority low :tag "x")
                   (plist-get plist :metadata)))))

(mevedel-deftest mevedel-session-persistence--task-from-plist ()
  ,test
  (test)
  :doc "rebuilds a task struct from plist"
  (let* ((plist (list :id 3 :subject "X" :description nil
                      :status 'completed :owner nil
                      :blocks nil :blocked-by nil :metadata nil))
         (task (mevedel-session-persistence--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (= 3 (mevedel-task-id task)))
    (should (equal "X" (mevedel-task-subject task)))
    (should (eq 'completed (mevedel-task-status task)))))


;;
;;; Top-level round-trip

(mevedel-deftest mevedel-session-persistence-serialize ()
  ,test
  (test)
  :doc "serializes a fully populated session"
  (let* ((session (test-mevedel-session-persistence--make-session))
         (plist   (mevedel-session-persistence-serialize
                   session
                   :first-user-message "Refactor X"
                   :additional-roots '(("alt" . "/tmp/alt")))))
    (should (equal "v0.5.0" (plist-get plist :version)))
    (should (equal "main-2026-04-23T14-30-a9f2"
                   (plist-get plist :session-id)))
    (should (equal "main" (plist-get plist :session-name)))
    (should (equal 'default (plist-get plist :permission-mode)))
    (should (= 2 (plist-get plist :current-segment)))
    (should (= 5 (plist-get plist :total-turn-count)))
    (should (equal "Refactor X" (plist-get plist :first-user-message)))
    (should (equal '(("alt" . "/tmp/alt"))
                   (plist-get plist :additional-roots)))
    (should (= 3 (length (plist-get plist :permission-rules))))
    (should (= 2 (length (plist-get plist :tasks))))
    (should (plist-get plist :workspace))
    (should (plist-get plist :prompt-index))
    (should (plist-get plist :file-snapshots)))
  :doc "fork fields default nil for a non-fork session"
  (let* ((session (test-mevedel-session-persistence--make-session))
         (plist   (mevedel-session-persistence-serialize session)))
    (should (null (plist-get plist :forked-from-session-id)))
    (should (null (plist-get plist :forked-from-turn)))))

(mevedel-deftest mevedel-session-persistence-deserialize ()
  ,test
  (test)
  :doc "round-trips a populated session"
  (let* ((source  (test-mevedel-session-persistence--make-session))
         (plist   (mevedel-session-persistence-serialize
                   source :first-user-message "Hi"))
         (result  (mevedel-session-persistence-deserialize plist))
         (session (plist-get result :session)))
    (should (mevedel-session-p session))
    (should (equal "main" (mevedel-session-name session)))
    (should (equal "main-2026-04-23T14-30-a9f2"
                   (mevedel-session-session-id session)))
    (should (eq 'default (mevedel-session-permission-mode session)))
    (should (= 5 (mevedel-session-turn-count session)))
    (should (= 2 (mevedel-session-current-segment session)))
    (should (= 2 (length (mevedel-session-tasks session))))
    (should (= 3 (length (mevedel-session-permission-rules session))))
    (should (equal "Hi" (plist-get result :first-user-message)))
    ;; touched-files / mentions-shown reset to empty hash tables
    (should (hash-table-p (mevedel-session-touched-files session)))
    (should (zerop (hash-table-count (mevedel-session-touched-files session))))
    (should (hash-table-p (mevedel-session-mentions-shown session)))
    (should (zerop (hash-table-count (mevedel-session-mentions-shown session))))
    ;; workspace identity recovered
    (let ((workspace (mevedel-session-workspace session)))
      (should (eq 'project (mevedel-workspace-type workspace)))
      (should (equal "test-id" (mevedel-workspace-id workspace)))))
  :doc "drops permission rules with unknown actions"
  (let* ((plist (list :version (mevedel-version)
                      :session-name "x"
                      :permission-rules
                      '(("Read"  :path "/x" :action allow)
                        ("Write" :path "/y" :action future-action))
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil))
         (session (plist-get
                   (mevedel-session-persistence-deserialize plist)
                   :session)))
    (should (= 1 (length (mevedel-session-permission-rules session))))))


;;
;;; Sidecar IO

(mevedel-deftest mevedel-session-persistence-write ()
  ,test
  (test)
  :doc "atomic write produces a readable plist"
  (let ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el")))
    (unwind-protect
        (let* ((plist `(:version ,(mevedel-version)
                                 :session-name "main"
                                 :tasks nil
                                 :permission-rules nil)))
          (mevedel-session-persistence-write tmp plist)
          (should (file-exists-p tmp))
          (let ((readback (mevedel-session-persistence-read tmp)))
            (should (equal "main" (plist-get readback :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp)))))


;;
;;; Version patch

(mevedel-deftest mevedel-session-persistence--patch-sidecar ()
  ,test
  (test)
  :doc "passes through plist on current version"
  (let* ((plist `(:version ,(mevedel-version) :session-name "x"))
         (out   (mevedel-session-persistence--patch-sidecar plist)))
    (should (equal plist out)))
  :doc "stamps current version on missing/old version"
  (let* ((plist '(:session-name "x"))
         (out   (mevedel-session-persistence--patch-sidecar plist)))
    (should (equal (mevedel-version) (plist-get out :version)))))


;;
;;; Phase 2: ID generation, paths, lazy materialization

(mevedel-deftest mevedel-session-persistence--sanitize ()
  ,test
  (test)
  :doc "leaves alphanumerics, underscores, dashes alone"
  (should (equal "main" (mevedel-session-persistence--sanitize "main")))
  (should (equal "alt-1_2"
                 (mevedel-session-persistence--sanitize "alt-1_2")))
  :doc "replaces spaces and slashes with underscores"
  (should (equal "alt_branch"
                 (mevedel-session-persistence--sanitize "alt branch")))
  (should (equal "a_b_c"
                 (mevedel-session-persistence--sanitize "a/b/c")))
  :doc "handles nil input"
  (should (equal "" (mevedel-session-persistence--sanitize nil))))

(mevedel-deftest mevedel-session-persistence--short-uuid ()
  ,test
  (test)
  :doc "returns four hex characters"
  (let ((u (mevedel-session-persistence--short-uuid)))
    (should (= 4 (length u)))
    (should (string-match-p "\\`[0-9a-f]+\\'" u)))
  :doc "produces different values across calls (probabilistically)"
  (let ((seen (make-hash-table :test #'equal)))
    (dotimes (_ 32)
      (puthash (mevedel-session-persistence--short-uuid) t seen))
    ;; With 4 hex chars (65536 possible values) and only 32 samples,
    ;; collisions are vanishingly rare.  Accept any number > 1.
    (should (> (hash-table-count seen) 1))))

(mevedel-deftest mevedel-session-persistence--compute-id ()
  ,test
  (test)
  :doc "generates id matching <name>-<timestamp>-<short-uuid>"
  (let ((id (mevedel-session-persistence--compute-id "main")))
    (should (string-match-p
             "\\`main-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]\\{4\\}\\'"
             id)))
  :doc "sanitizes the name component"
  (let ((id (mevedel-session-persistence--compute-id "my session")))
    (should (string-prefix-p "my_session-" id))))

(mevedel-deftest mevedel-session-persistence--segment-path ()
  ,test
  (test)
  :doc "zero-pads segment number to four digits"
  (should (equal "/x/segment-0001.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 1)))
  (should (equal "/x/segment-0042.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 42)))
  (should (equal "/x/segment-1000.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 1000))))

(mevedel-deftest mevedel-session-persistence--first-user-message ()
  ,test
  (test)
  :doc "extracts first non-blank line of first user region"
  (with-temp-buffer
    (insert "Refactor the permission chain\n\nMore details follow.")
    (should (equal "Refactor the permission chain"
                   (mevedel-session-persistence--first-user-message
                    (current-buffer)))))
  :doc "skips assistant response regions"
  (with-temp-buffer
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "What about edge cases?\n")
    (should (equal "What about edge cases?"
                   (mevedel-session-persistence--first-user-message
                    (current-buffer)))))
  :doc "returns nil for buffers with no user content"
  (with-temp-buffer
    (insert (propertize "All response.\n" 'gptel 'response))
    (should (null (mevedel-session-persistence--first-user-message
                   (current-buffer)))))
  :doc "truncates long lines"
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (let ((preview (mevedel-session-persistence--first-user-message
                    (current-buffer))))
      (should (= 120 (length preview)))
      (should (string-suffix-p "..." preview)))))


;;
;;; Phase 2: write path

(defun test-mevedel-session-persistence--make-tempdir-workspace ()
  "Build a workspace rooted in a fresh tempdir.
Returns (cons WORKSPACE TEMPDIR).  Caller must `delete-directory'
the tempdir on cleanup."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-test-ws-" t)))
         (_       (mevedel-workspace-clear-registry))
         (ws      (mevedel-workspace-get-or-create
                   'project (file-name-nondirectory
                             (directory-file-name tempdir))
                   tempdir
                   "ws")))
    (cons ws tempdir)))

(mevedel-deftest mevedel-session-persistence-ensure-files ()
  ,test
  (test)
  :doc "lazily materializes the session directory tree"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hello LLM\n")
                (let ((path (mevedel-session-persistence-ensure-files
                             session buf)))
                  (should path)
                  (should (file-directory-p path))
                  (should (file-directory-p (file-name-concat path "agents")))
                  (should (file-directory-p
                           (file-name-concat path "file-history")))
                  (should (file-exists-p
                           (file-name-concat path "session.meta.el")))
                  (should (file-exists-p
                           (file-name-concat path "segment-0001.chat.org")))
                  ;; Struct fields populated
                  (should (mevedel-session-session-id session))
                  (should (mevedel-session-created-at session))
                  (should (= 1 (mevedel-session-current-segment session)))
                  ;; Buffer wired to segment file
                  (should (equal (file-name-concat path "segment-0001.chat.org")
                                 buffer-file-name))
                  ;; Idempotent: second call returns same path, no churn
                  (should (equal path
                                 (mevedel-session-persistence-ensure-files
                                  session buf)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "returns nil when persistence is disabled"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let ((mevedel-session-persistence nil)
              (session (mevedel-session-create "main" workspace))
              (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (should (null
                         (mevedel-session-persistence-ensure-files
                          session buf)))
                (should (null (mevedel-session-save-path session))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-save ()
  ,test
  (test)
  :doc "advances updated-at across saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (mevedel-session-persistence-save session buf)
                (let ((first-updated (mevedel-session-updated-at session)))
                  (should first-updated)
                  ;; Force a second-tick gap so the timestamp can advance.
                  (sleep-for 1.1)
                  (insert "Second prompt\n")
                  (mevedel-session-persistence-save session buf)
                  (should-not (equal first-updated
                                     (mevedel-session-updated-at session)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rewritten sidecar reflects current session state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Refactor the permission chain\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "main" (plist-get plist :session-name)))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :first-user-message)))
                  (should (= 1 (plist-get plist :current-segment)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 3: file-history store

(defun test-mevedel-session-persistence--make-materialized-session ()
  "Create a session, materialize it, return (cons SESSION TEMPDIR).
The session's data buffer is `*test-data-buf*' and is left alive — the
caller must `kill-buffer' it during cleanup.  TEMPDIR holds the entire
workspace tree."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buf     (generate-new-buffer "*test-data-buf*")))
      (with-current-buffer buf
        (org-mode)
        (insert "Initial prompt\n")
        (mevedel-session-persistence-ensure-files session buf))
      (cons session tempdir))))

(defun test-mevedel-session-persistence--cleanup (tempdir)
  "Tear down a test session: kill data buffer and remove TEMPDIR."
  (when-let ((buf (get-buffer "*test-data-buf*")))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf))
  (when (file-directory-p tempdir)
    (delete-directory tempdir t))
  (mevedel-workspace-clear-registry))

(mevedel-deftest mevedel-file-history--path-hash ()
  ,test
  (test)
  :doc "returns 16 hex chars"
  (let ((h (mevedel-file-history--path-hash "/tmp/foo.el")))
    (should (= 16 (length h)))
    (should (string-match-p "\\`[0-9a-f]+\\'" h)))
  :doc "is deterministic for a given path"
  (should (equal (mevedel-file-history--path-hash "/tmp/foo.el")
                 (mevedel-file-history--path-hash "/tmp/foo.el")))
  :doc "differs across paths"
  (should-not (equal (mevedel-file-history--path-hash "/tmp/foo.el")
                     (mevedel-file-history--path-hash "/tmp/bar.el"))))

(mevedel-deftest mevedel-file-history--backup-name ()
  ,test
  (test)
  :doc "appends @v<N>"
  (let ((n (mevedel-file-history--backup-name "/tmp/x.el" 3)))
    (should (string-match "@v3\\'" n))
    (should (= 19 (length n)))))   ; 16 hex + "@v" + "3" = 19

(mevedel-deftest mevedel-file-history--latest-version ()
  ,test
  (test)
  :doc "returns 0 for unknown path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (should (= 0 (mevedel-file-history--latest-version
                  session "/tmp/foo")))
    (mevedel-workspace-clear-registry))
  :doc "finds max across multiple turn entries"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/tmp/foo" . (:backup-name "abc@v1" :version 1))))
            (3 . (("/tmp/foo" . (:backup-name "abc@v3" :version 3))
                  ("/tmp/bar" . (:backup-name "def@v1" :version 1))))
            (2 . (("/tmp/foo" . (:backup-name "abc@v2" :version 2))))))
    (should (= 3 (mevedel-file-history--latest-version session "/tmp/foo")))
    (should (= 1 (mevedel-file-history--latest-version session "/tmp/bar")))
    (should (= 0 (mevedel-file-history--latest-version session "/tmp/baz")))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-file-history-snapshot-modified ()
  ,test
  (test)
  :doc "writes a backup for a modified file"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "old content" nil tracked-file nil 'silent)
          (puthash tracked-file "old content" pre)
          (write-region "new content" nil tracked-file nil 'silent)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (= 1 (length written)))
            (let* ((entry (assoc tracked-file
                                 (cdr (assoc 1 (mevedel-session-file-snapshots
                                                session)))))
                   (backup-name (plist-get (cdr entry) :backup-name))
                   (backup-path (mevedel-file-history--backup-path
                                 (mevedel-session-save-path session)
                                 backup-name)))
              (should backup-name)
              (should (file-exists-p backup-path))
              (with-temp-buffer
                (insert-file-contents-literally backup-path)
                (should (equal "new content" (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "skips unchanged files"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "same content" nil tracked-file nil 'silent)
          (puthash tracked-file "same content" pre)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (null written))
            (should-not (mevedel-session-file-snapshots session))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records absent marker when file deleted during turn"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "gone.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file "had content" pre)
          (mevedel-file-history-snapshot-modified session 2 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 2 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (null (plist-get (cdr entry) :backup-name)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "snapshots a created file (pre-content nil, current exists)"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "new.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file nil pre)
          (write-region "fresh" nil tracked-file nil 'silent)
          (mevedel-file-history-snapshot-modified session 3 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 3 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (plist-get (cdr entry) :backup-name))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "skips files exceeding the size cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "huge.el"))
               (pre          (make-hash-table :test #'equal))
               (mevedel-file-history-max-snapshot-bytes 10))
          (write-region (make-string 100 ?x) nil tracked-file nil 'silent)
          (puthash tracked-file nil pre)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (null written))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-file-history-evict ()
  ,test
  (test)
  :doc "drops oldest entries beyond the cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots 2))
          ;; Pre-populate with 3 turns; create dummy backup files.
          (dotimes (i 3)
            (let* ((turn (1+ i))
                   (path (format "/tmp/file-%d.el" turn))
                   (backup-name (format "%s@v1"
                                        (mevedel-file-history--path-hash path))))
              (mevedel-file-history--write-backup
               (mevedel-session-save-path session) backup-name "x")
              (push (cons turn (list (cons path
                                            (list :backup-name backup-name
                                                  :version 1))))
                    (mevedel-session-file-snapshots session))))
          (mevedel-file-history-evict session)
          (let ((kept (mapcar #'car (mevedel-session-file-snapshots session))))
            (should (= 2 (length kept)))
            ;; Oldest dropped: only highest two turn numbers retained.
            (should (memq 2 kept))
            (should (memq 3 kept))
            (should-not (memq 1 kept)))
          ;; GC should have removed the orphaned v1 file for turn 1's path.
          (let ((dir (file-name-concat
                      (mevedel-session-save-path session) "file-history")))
            (should-not
             (member
              (format "%s@v1"
                      (mevedel-file-history--path-hash "/tmp/file-1.el"))
              (directory-files dir nil "[^.]")))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when count is below cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots 100))
          (setf (mevedel-session-file-snapshots session)
                '((1 . (("/x" . (:backup-name "a@v1" :version 1))))))
          (mevedel-file-history-evict session)
          (should (= 1 (length (mevedel-session-file-snapshots session)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots nil))
          (dotimes (i 5)
            (push (cons (1+ i) nil) (mevedel-session-file-snapshots session)))
          (mevedel-file-history-evict session)
          (should (= 5 (length (mevedel-session-file-snapshots session)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(provide 'test-mevedel-session-persistence)

;;; test-mevedel-session-persistence.el ends here
