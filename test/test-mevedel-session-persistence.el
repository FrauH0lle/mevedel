;;; test-mevedel-session-persistence.el --- Tests for session persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-persistence' (Phase 1: serialization).

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-presets)
(require 'mevedel-skills)
(require 'mevedel-reminders)
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-chat)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-repair)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel--preset)
(defvar gptel-system-prompt)
(defvar so-long-predicate)
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))


;;
;;; Helpers

(defun test-mevedel-session-persistence--make-workspace (root)
  "Build a workspace struct registered in the global registry.
ROOT is a temporary directory owned and cleaned up by the caller."
  (mevedel-workspace-clear-registry)
  (make-directory (file-name-concat root "packages" "api") t)
  (mevedel-workspace-get-or-create
   'project "test-id" root (file-name-nondirectory
                            (directory-file-name root))))

(defun test-mevedel-session-persistence--all-gptel-prop-p (start end expected)
  "Return non-nil when START..END all has gptel EXPECTED."
  (let ((pos start)
        (ok t))
    (while (and ok (< pos end))
      (unless (equal (get-text-property pos 'gptel) expected)
        (setq ok nil))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    ok))

(defun test-mevedel-session-persistence--no-tool-prop-p (start end)
  "Return non-nil when START..END has no tool gptel property."
  (let ((pos start)
        (ok t))
    (while (and ok (< pos end))
      (let ((prop (get-text-property pos 'gptel)))
        (when (or (eq prop 'tool)
                  (and (consp prop) (eq (car prop) 'tool)))
          (setq ok nil)))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    ok))

(defun test-mevedel-session-persistence--tool-block-props-p
    (start end tool-prop)
  "Return non-nil when org tool START..END has parseable TOOL-PROP.
The `#+begin_tool' and `#+end_tool' scaffolding must not carry a tool
property, while the readable `(:name ...)' sexp and result body carry
TOOL-PROP."
  (when-let* ((parts (mevedel-session-persistence--org-tool-block-parts
                      start end)))
    (and (test-mevedel-session-persistence--no-tool-prop-p
          (plist-get parts :prefix-start)
          (plist-get parts :prefix-end))
         (test-mevedel-session-persistence--all-gptel-prop-p
          (plist-get parts :tool-start)
          (plist-get parts :tool-end)
          tool-prop)
         (test-mevedel-session-persistence--no-tool-prop-p
          (plist-get parts :suffix-start)
          (plist-get parts :suffix-end)))))

(defun test-mevedel-session-persistence--make-session (root)
  "Build a populated session for ROOT in round-trip cases."
  (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
         (root (mevedel-workspace-root workspace))
         (session   (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-working-directory session)
          (file-name-as-directory
           (file-name-concat root "packages" "api")))
    (setf (mevedel-session-permission-mode session) 'default)
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/tmp/foo/**" :action allow)
            ("Bash"  :pattern "git log*" :action allow)
            ("Write" :path "/tmp/bar"    :action deny)))
    (setf (mevedel-session-turn-count session) 5)
    (setf (mevedel-session-last-task-write-turn session) 4)
    (setf (mevedel-session-task-status-notes session)
          '((nil :note "Main status" :updated-turn 4
                 :updated-at "2026-04-23T18:20:00+0200")
            ("main" :note "Agent status" :updated-turn 4
             :updated-at "2026-04-23T18:21:00+0200")))
    (setf (mevedel-session-skills-snapshot session)
          '(("alpha" . "Alpha helper")))
    (setf (mevedel-session-session-id session) "main-2026-04-23T14-30-a9f2")
    (setf (mevedel-session-save-path session)
          (file-name-as-directory
           (file-name-concat
            root ".mevedel" "sessions" "main-2026-04-23T14-30-a9f2")))
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
                 :completed-turn 3
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
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
               (plist (mevedel-session-persistence--workspace-to-plist workspace)))
          (should (eq 'project    (plist-get plist :type)))
          (should (equal "test-id" (plist-get plist :id)))
          (should (equal root (plist-get plist :root)))
          (should (equal (file-name-nondirectory
                          (directory-file-name root))
                         (plist-get plist :name))))
      (when (file-directory-p root)
        (delete-directory root t))))
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
    (should (equal (expand-file-name "/tmp/q")
                   (mevedel-workspace-root recovered))))
  :doc "expands legacy tilde project paths"
  (mevedel-workspace-clear-registry)
  (let* ((root "~/mevedel-session-root/")
         (expected (expand-file-name root))
         (plist (list :type 'project :id root :root root :name "home-root"))
         (recovered (mevedel-session-persistence--workspace-from-plist plist)))
    (should (equal expected (mevedel-workspace-id recovered)))
    (should (equal expected (mevedel-workspace-root recovered))))
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
                :status 'pending :owner "explorer"
                :blocks '(8) :blocked-by '(5 6)
                :completed-turn 12
                :metadata '(:priority low :tag "x")))
         (plist (mevedel-session-persistence--task-to-plist task)))
    (should (= 7 (plist-get plist :id)))
    (should (equal "S" (plist-get plist :subject)))
    (should (equal "D" (plist-get plist :description)))
    (should (eq 'pending (plist-get plist :status)))
    (should (equal "explorer" (plist-get plist :owner)))
    (should (equal '(8) (plist-get plist :blocks)))
    (should (equal '(5 6) (plist-get plist :blocked-by)))
    (should (= 12 (plist-get plist :completed-turn)))
    (should (equal '(:priority low :tag "x")
                   (plist-get plist :metadata)))))

(mevedel-deftest mevedel-session-persistence--task-from-plist ()
  ,test
  (test)
  :doc "rebuilds a task struct from plist"
  (let* ((plist (list :id 3 :subject "X" :description nil
                      :status 'completed :owner nil
                      :blocks nil :blocked-by nil
                      :completed-turn 9 :metadata nil))
         (task (mevedel-session-persistence--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (= 3 (mevedel-task-id task)))
    (should (equal "X" (mevedel-task-subject task)))
    (should (eq 'completed (mevedel-task-status task)))
    (should (= 9 (mevedel-task-completed-turn task))))

  :doc "normalizes empty owner to nil"
  (let* ((plist (list :id 4 :subject "Y" :description nil
                      :status 'pending :owner ""
                      :blocks nil :blocked-by nil :metadata nil))
         (task (mevedel-session-persistence--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (null (mevedel-task-owner task)))))


;;
;;; Top-level round-trip

(mevedel-deftest mevedel-session-persistence-serialize ()
  ,test
  (test)
  :doc "serializes a fully populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-persistence-serialize
                       session
                       :first-user-message "Refactor X"
                       :latest-user-message "Ship Y"
                       :additional-roots '(("alt" . "/tmp/alt")))))
          (should (equal "v0.5.0" (plist-get plist :version)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (plist-get plist :session-id)))
          (should (equal "main" (plist-get plist :session-name)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (plist-get plist :working-directory)))
          (should (equal 'default (plist-get plist :permission-mode)))
          (should (= 2 (plist-get plist :current-segment)))
          (should (= 5 (plist-get plist :total-turn-count)))
          (should (= 4 (plist-get plist :last-task-write-turn)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (plist-get plist :task-status-notes)))
          (should (equal "Refactor X" (plist-get plist :first-user-message)))
          (should (equal "Ship Y" (plist-get plist :latest-user-message)))
          (should (equal '(("alt" . "/tmp/alt"))
                         (plist-get plist :additional-roots)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (plist-get plist :skills-snapshot)))
          (should (= 3 (length (plist-get plist :permission-rules))))
          (should (= 2 (length (plist-get plist :tasks))))
          (should (plist-get plist :workspace))
          (should (plist-get plist :prompt-index))
          (should (plist-get plist :file-snapshots)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "fork fields default nil for a non-fork session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-persistence-serialize session)))
          (should (null (plist-get plist :forked-from-session-id)))
          (should (null (plist-get plist :forked-from-turn))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-persistence-deserialize ()
  ,test
  (test)
  :doc "round-trips a populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((source (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-persistence-serialize
                       source
                       :first-user-message "Hi"
                       :latest-user-message "Later"))
               (result (mevedel-session-persistence-deserialize plist))
               (session (plist-get result :session)))
          (should (mevedel-session-p session))
          (should (equal "main" (mevedel-session-name session)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (mevedel-session-working-directory session)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (mevedel-session-session-id session)))
          (should (eq 'default (mevedel-session-permission-mode session)))
          (should (= 5 (mevedel-session-turn-count session)))
          (should (= 4 (mevedel-session-last-task-write-turn session)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (mevedel-session-skills-snapshot session)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (mevedel-session-task-status-notes session)))
          (should (= 2 (mevedel-session-current-segment session)))
          (should (= 2 (length (mevedel-session-tasks session))))
          (should (= 3 (mevedel-task-completed-turn
                        (car (mevedel-session-tasks session)))))
          (should (= 3 (length (mevedel-session-permission-rules session))))
          (should (equal "Hi" (plist-get result :first-user-message)))
          (should (equal "Later" (plist-get result :latest-user-message)))
          ;; touched-files / mentions-shown reset to empty hash tables
          (should (hash-table-p (mevedel-session-touched-files session)))
          (should (zerop (hash-table-count (mevedel-session-touched-files session))))
          (should (hash-table-p (mevedel-session-mentions-shown session)))
          (should (zerop (hash-table-count (mevedel-session-mentions-shown session))))
          ;; workspace identity recovered
          (let ((workspace (mevedel-session-workspace session)))
            (should (eq 'project (mevedel-workspace-type workspace)))
            (should (equal "test-id" (mevedel-workspace-id workspace)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "derives latest preview from old prompt indexes"
  (let* ((plist (list :version (mevedel-version)
                      :session-name "x"
                      :first-user-message "First"
                      :tasks nil
                      :prompt-index
                      '((1 . ((:turn 1 :cum-turn 1 :preview "First")
                              (:turn 2 :cum-turn 2 :preview "Newest"))))
                      :file-snapshots nil))
         (result (mevedel-session-persistence-deserialize plist)))
    (should (equal "First" (plist-get result :first-user-message)))
    (should (equal "Newest" (plist-get result :latest-user-message))))
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
    (should (= 1 (length (mevedel-session-permission-rules session)))))

  :doc "restores old sidecars without working directory at workspace root"
  (let* ((root (make-temp-file "mevedel-old-proj-" t))
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id (format "old-id-%s" (gensym))
                                       :root root
                                       :name "old-proj")
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (let ((session (plist-get
                        (mevedel-session-persistence-deserialize plist)
                        :session)))
          (should (equal (file-name-as-directory root)
                         (mevedel-session-working-directory session))))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "preserves relocated working directories under the new workspace root"
  (let* ((old-root (make-temp-file "mevedel-old-root-" t))
         (new-root (make-temp-file "mevedel-new-root-" t))
         (workspace-id (format "relocated-id-%s" (gensym)))
         (old-cwd (file-name-concat old-root "packages/api"))
         (new-cwd (file-name-concat new-root "packages/api"))
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root old-root
                                       :name "relocated-proj")
                      :working-directory old-cwd
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-directory new-cwd t)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "relocated-proj")
          (let ((session (plist-get
                          (mevedel-session-persistence-deserialize plist)
                          :session)))
            (should (equal (file-name-as-directory new-cwd)
                           (mevedel-session-working-directory session)))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (when (file-directory-p new-root)
        (delete-directory new-root t))))

  :doc "preserves saved working directories already under a nested current root"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (workspace-id (format "nested-relocated-id-%s" (gensym)))
         (saved-cwd new-root)
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root old-root
                                       :name "nested-proj")
                      :working-directory saved-cwd
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-directory new-root t)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "nested-proj")
          (let ((session (plist-get
                          (mevedel-session-persistence-deserialize plist)
                          :session)))
            (should (equal new-root
                           (mevedel-session-working-directory session)))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))))

  :doc "rejects restored working directories outside the workspace"
  (let ((plist (list :version (mevedel-version)
                     :session-name "x"
                     :workspace '(:type project
                                  :id "restore-id"
                                  :root "/tmp/restore-proj/"
                                  :name "restore-proj")
                     :working-directory "/tmp/restore-proj-sibling/"
                     :tasks nil
                     :prompt-index nil
                     :file-snapshots nil)))
    (should-error
     (mevedel-session-persistence-deserialize plist)
     :type 'user-error)))

  :doc "rejects restored symlink working directories outside the workspace"
  (let* ((root (make-temp-file "mevedel-restore-root-" t))
         (outside (make-temp-file "mevedel-restore-outside-" t))
         (link (file-name-concat root "linked-cwd"))
         (workspace-id (format "restore-symlink-%s" (gensym)))
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root root
                                       :name "restore-proj")
                      :working-directory link
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should-error
           (mevedel-session-persistence-deserialize plist)
           :type 'user-error))
      (when (file-symlink-p link)
        (delete-file link))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t))))


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
Returns (cons WORKSPACE TEMPDIR).  The workspace's NAME is derived
from the tempdir basename so that different tests never collide on the
chat-buffer name (`*mevedel:NAME@WORKSPACE*'); buffer leakage across
tests would otherwise mask correctness bugs in the live-buffer path of
`mevedel-session-persistence-restore'.  Caller must
`delete-directory' the tempdir on cleanup."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-test-ws-" t)))
         (basename (file-name-nondirectory (directory-file-name tempdir)))
         (_       (mevedel-workspace-clear-registry))
         (ws      (mevedel-workspace-get-or-create
                   'project basename tempdir basename)))
    (cons ws tempdir)))

(defun test-mevedel-session-persistence--release-and-kill (buf session)
  "Release SESSION's lock and kill BUF if alive.
Mirrors the production kill-buffer-hook's lock release for tests
that don't go through `mevedel--chat-buffer-init-common' (which
installs the real hook)."
  (when (and session (mevedel-session-save-path session))
    (mevedel-session-persistence-lock-release
     (mevedel-session-save-path session)))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defun test-mevedel-session-persistence--make-missing-cwd-session ()
  "Return a saved session whose working directory has been deleted.
The result is (WORKSPACE TEMPDIR MISSING-DIR REPLACEMENT-DIR SESSION-DIR)."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((missing-dir (file-name-as-directory
                         (file-name-concat tempdir "deleted-worktree")))
           (replacement-dir (file-name-as-directory
                             (file-name-concat tempdir "replacement")))
           (session (mevedel-session-create "main" workspace missing-dir))
           (buf (generate-new-buffer "*test-data-buf*"))
           session-dir)
      (unwind-protect
          (progn
            (make-directory missing-dir t)
            (make-directory replacement-dir t)
            (with-current-buffer buf
              (org-mode)
              (insert "Missing working directory\n")
              (mevedel-session-persistence-save session buf))
            (setq session-dir (mevedel-session-save-path session))
            (test-mevedel-session-persistence--release-and-kill buf session)
            (setq buf nil)
            (delete-directory missing-dir t)
            (list workspace tempdir missing-dir replacement-dir session-dir))
        (test-mevedel-session-persistence--release-and-kill buf session)))))

(defun test-mevedel-session-persistence--reset-instructions ()
  "Reset global and workspace-scoped instruction state for persistence cases."
  (setf (mevedel--instruction-alist) nil)
  (setf (mevedel--instruction-id-counter) 0)
  (setf (mevedel--instruction-id-usage-map) (make-hash-table))
  (setf (mevedel--instruction-retired-ids) nil)
  (setq mevedel--instruction-states (make-hash-table :test #'equal))
  (setq mevedel--instruction-current-state-key :global))

(mevedel-deftest mevedel-session-persistence--authoritative-buffer ()
  ,test
  (test)
  :doc "returns ordinary data buffers unchanged"
  (let ((buf (generate-new-buffer " *test-data*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (eq buf (mevedel-session-persistence--authoritative-buffer
                           buf))))
      (when (buffer-live-p buf) (kill-buffer buf))))
  :doc "routes interactive view buffers to their data buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (should (eq data-buf
                              (mevedel-session-persistence--authoritative-buffer
                               view-buf)))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not treat transcript inspection views as session segment buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-agent-data*"))
               (view-buf (generate-new-buffer " *test-agent-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf
                                     (list :agent-transcript-p t))
                (with-current-buffer view-buf
                  (should-not
                   (mevedel-session-persistence--authoritative-buffer
                    view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel--instruction-workspace-state ()
  ,test
  (test)
  :doc "keeps instruction alists isolated by workspace"
  (let* ((root-a (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-a-" t)))
         (root-b (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-b-" t)))
         (file-a (file-name-concat root-a "a.el"))
         (file-b (file-name-concat root-b "b.el"))
         (buf-a nil)
         (buf-b nil))
    (unwind-protect
        (progn
          (test-mevedel-session-persistence--reset-instructions)
          (mevedel-workspace-clear-registry)
          (write-region "(message \"a\")\n" nil file-a nil 'silent)
          (write-region "(message \"b\")\n" nil file-b nil 'silent)
          (let ((ws-a (mevedel-workspace-get-or-create
                       'project "a" root-a "a"))
                (ws-b (mevedel-workspace-get-or-create
                       'project "b" root-b "b")))
            (setq buf-a (find-file-noselect file-a))
            (setq buf-b (find-file-noselect file-b))
            (with-current-buffer buf-a
              (setq-local mevedel--workspace ws-a)
              (mevedel--create-reference-in buf-a (point-min) (point-max)))
            (with-current-buffer buf-b
              (setq-local mevedel--workspace ws-b)
              (mevedel--create-reference-in buf-b (point-min) (point-max)))
            (mevedel--instruction-activate-workspace ws-a)
            (should (= 1 (length (alist-get buf-a (mevedel--instruction-alist)))))
            (should-not (assoc buf-b (mevedel--instruction-alist)))
            (mevedel--instruction-activate-workspace ws-b)
            (should (= 1 (length (alist-get buf-b (mevedel--instruction-alist)))))
            (should-not (assoc buf-a (mevedel--instruction-alist)))))
      (when (buffer-live-p buf-a)
        (with-current-buffer buf-a (set-buffer-modified-p nil))
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (with-current-buffer buf-b (set-buffer-modified-p nil))
        (kill-buffer buf-b))
      (delete-directory root-a t)
      (delete-directory root-b t)
      (test-mevedel-session-persistence--reset-instructions)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel--instruction-operation-state-key ()
  ,test
  (test)
  :doc "prefers a dynamic workspace override over the buffer workspace"
  (let ((workspace (mevedel-workspace--create
                    :type 'project :id "buffer" :root "/tmp/buffer/")))
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (let ((mevedel--instruction-state-key-override
             '(project . "explicit")))
        (should (equal '(project . "explicit")
                       (mevedel--instruction-operation-state-key)))))))

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
                  ;; `ensure-files' leaves sidecar writing to `save'
                  ;; (one write instead of two on first materialization).
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
  :doc "backfills diagnostics recorded before first materialization"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               (repair-event
                '(:time "now" :origin "main" :backend backend
                  :model model :tool "Read" :outcome valid
                  :repair-enabled t :rules nil :paths nil
                  :issue-kinds nil :execution executed :result success)))
          (unwind-protect
              (progn
                (mevedel-hooks--log
                 session '(:event UserPromptSubmit :status ok))
                (mevedel-tool-repair-log-event session repair-event)
                (mevedel-permission-log
                 session 'permission-decision :tool-name "Read")
                (with-current-buffer buf
                  (org-mode)
                  (mevedel-session-persistence-ensure-files session buf))
                (dolist (file '("hook-log.el" "repair-log.el"
                                "permission-log.el"))
                  (should
                   (file-readable-p
                    (file-name-concat
                     (mevedel-session-save-path session) file))))
                (should-not
                 (mevedel-session-permission-log-pending session)))
            (when (buffer-live-p buf)
              (kill-buffer buf))))
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
      (mevedel-workspace-clear-registry)))
  :doc "repairs shallowly materialized sessions before saving data buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (wrong-buf (generate-new-buffer "*test-wrong-buf*"))
               (data-buf (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (let ((path (with-current-buffer wrong-buf
                            (org-mode)
                            (mevedel-session-persistence--shallow-ensure-files
                             session wrong-buf))))
                (should path)
                (should-not
                 (file-exists-p
                  (file-name-concat path "segment-0001.chat.org")))
                (with-current-buffer data-buf
                  (org-mode)
                  (insert "Hello after shallow materialization\n")
                  (should-not buffer-file-name)
                  (should (equal path
                                 (mevedel-session-persistence-ensure-files
                                  session data-buf)))
                  (should (equal (file-name-concat
                                  path "segment-0001.chat.org")
                                 buffer-file-name))
                  (should
                   (file-exists-p
                    (file-name-concat path "segment-0001.chat.org")))
                  (let ((segment-file buffer-file-name))
                    (should (string-match-p
                             "Hello after shallow materialization"
                             (with-temp-buffer
                               (insert-file-contents segment-file)
                               (buffer-string)))))))
            (when (buffer-live-p wrong-buf) (kill-buffer wrong-buf))
            (when (buffer-live-p data-buf) (kill-buffer data-buf))))
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
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :latest-user-message)))
                  (should (= 1 (plist-get plist :current-segment)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "latest sidecar preview follows the newest prompt"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (insert (propertize "Assistant response\n" 'gptel 'response))
                (insert "Second prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "First prompt"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Second prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "first sidecar preview stays stable across later saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (erase-buffer)
                (insert "Later prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "Original prompt"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Later prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "view buffers save through their data buffer without becoming files"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Persist data prompt\n"))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Working view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-persistence-save session view-buf))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Persist data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Working view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--kill-emacs-hook ()
  ,test
  (test)
  :doc "modified view buffers are persisted through data buffers on exit"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Exit hook data prompt\n")
                  (set-buffer-modified-p nil))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Exit hook view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame)
                             (list view-buf data-buf)))
                          ((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-persistence--kill-emacs-hook))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Exit hook data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Exit hook view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--instruction-snapshots ()
  ,test
  (test)
  :doc "saves current and per-turn instruction snapshots"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun alpha () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain alpha\n")
              (mevedel-session-persistence-save session data-buf))
            (let ((current-path
                   (mevedel-session-persistence--instructions-current-path
                    (mevedel-session-save-path session)))
                  (turn-path
                   (mevedel-session-persistence--instructions-turn-path
                    (mevedel-session-save-path session) 1)))
              (should (file-exists-p current-path))
              (should (file-exists-p turn-path))
              (let* ((current-save (with-temp-buffer
                                     (insert-file-contents current-path)
                                     (read (current-buffer))))
                     (turn-save (with-temp-buffer
                                  (insert-file-contents turn-path)
                                  (read (current-buffer))))
                     (current-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get current-save :files))))
                     (turn-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get turn-save :files))))
                     (instruction
                      (car (plist-get current-file-plist :instructions)))
                     (turn-instruction
                      (car (plist-get turn-file-plist :instructions)))
                     (properties
                      (plist-get instruction :properties))
                     (anchor (plist-get turn-instruction :anchor)))
                (should (plist-member current-file-plist :original-content))
                (should-not (plist-member turn-file-plist
                                          :original-content))
                (should (= 1 (plist-get turn-file-plist :anchor-schema)))
                (should (plist-get turn-file-plist :content-hash))
                (should (= 1 (plist-get anchor :schema)))
                (should (plist-get anchor :uuid))
                (should (plist-member anchor :bodyless))
                (should (plist-get anchor :text-hash))
                (should (memq 'mevedel-instruction properties))
                (should-not (memq 'before-string properties))
                (should-not (memq 'face properties))
                (should-not (memq 'keymap properties))
                (should-not (memq 'mevedel-bg-color properties)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "restores instruction overlays after clearing live state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (should-not (mevedel--all-instructions))
              (mevedel-session-persistence--load-instructions session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (should (= 1 (length (alist-get source-buf (mevedel--instruction-alist))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "strips transient text properties from persisted instruction strings"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el"))
                 (directive-text (copy-sequence "Fix beta")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (add-text-properties 0 (length directive-text)
                                 `(tabulated-list-id ,source-buf)
                                 directive-text)
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-directive-in
               source-buf (point-min) (point-max) nil directive-text))
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-persistence-save session data-buf))
            (let* ((current-path
                    (mevedel-session-persistence--instructions-current-path
                     (mevedel-session-save-path session)))
                   (save-file (with-temp-buffer
                                (insert-file-contents current-path)
                                (read (current-buffer))))
                   (file-plist (cdr (assoc "source.el"
                                           (plist-get save-file :files))))
                   (instruction
                    (car (plist-get file-plist :instructions)))
                   (properties (plist-get instruction :properties))
                   (directive (plist-get properties 'mevedel-directive)))
              (should (equal "Fix beta" directive))
              (should-not (text-properties-at 0 directive)))
            (with-current-buffer data-buf
              (mevedel--clear-instruction-state workspace)
              (mevedel-session-persistence--load-instructions session data-buf))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ov (car (alist-get source-buf (mevedel--instruction-alist))))
                   (directive (overlay-get ov 'mevedel-directive)))
              (should (equal "Fix beta" directive))
              (should-not (text-properties-at 0 directive))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "loads legacy snapshots with unreadable text-property values"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace))
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-persistence-save session data-buf))
            (let* ((path (mevedel-session-persistence--instructions-current-path
                          (mevedel-session-save-path session)))
                   (end (with-current-buffer source-buf (point-max)))
                   (placeholder 'mevedel-test-corrupt-directive)
                   (save-file
                    `(:version ,(mevedel-version)
                      :ids (:id-counter 1 :used-ids (1) :retired-ids nil)
                      :files (("source.el"
                               :instructions
                               ((:overlay-start 1
                                 :overlay-end ,end
                                 :anchor nil
                                 :properties
                                 (mevedel-instruction t
                                  mevedel-id 1
                                  mevedel-uuid "legacy"
                                  mevedel-instruction-type directive
                                  mevedel-directive ,placeholder)))))))
                   (text
                    (replace-regexp-in-string
                     (regexp-quote (symbol-name placeholder))
                     "#(\"Fix beta\" 0 8 (tabulated-list-id #<buffer source.el>))"
                     (prin1-to-string save-file)
                     t t)))
              (write-region text nil path nil 'silent))
            (with-current-buffer data-buf
              (mevedel--load-instructions-file
               (mevedel-session-persistence--instructions-current-path
                (mevedel-session-save-path session))
               tempdir nil t workspace))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ov (car (alist-get source-buf (mevedel--instruction-alist))))
                   (directive (overlay-get ov 'mevedel-directive)))
              (should (equal "Fix beta" directive))
              (should-not (text-properties-at 0 directive))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "ignores unreadable instruction snapshots during session restore"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((data-buf nil)
          (session nil))
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain gamma\n")
              (mevedel-session-persistence-save session data-buf))
            (let ((path (mevedel-session-persistence--instructions-current-path
                         (mevedel-session-save-path session))))
              (make-directory (file-name-directory path) t)
              (write-region "(:files ((\"source.el\" . #<marker>)))"
                            nil path nil 'silent)
              (should-not
               (mevedel-session-persistence--load-instructions
                session data-buf))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-persistence--instruction-anchor-restore ()
  ,test
  (test)
  :doc "reanchors an instruction after text is inserted before it"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "aaa\nTARGET\nbbb\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (with-current-buffer source-buf
                (should (equal "TARGET\n"
                               (buffer-substring-no-properties
                                (overlay-start ov) (overlay-end ov)))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "uses parent containment to resolve duplicate child text"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "PARENT\nchild\nEND\noutside child\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (let ((parent-start (point)))
                (search-forward "END\n")
                (mevedel--create-reference-in
                 source-buf parent-start (point)))
              (goto-char (point-min))
              (search-forward "child")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain nested target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ovs (alist-get source-buf (mevedel--instruction-alist)))
                   (child (cl-find-if
                           (lambda (ov)
                             (with-current-buffer source-buf
                               (equal "child"
                                      (buffer-substring-no-properties
                                       (overlay-start ov)
                                       (overlay-end ov)))))
                           ovs)))
              (should (= 2 (length ovs)))
              (should child)
              (with-current-buffer source-buf
                (save-excursion
                  (goto-char (overlay-start child))
                  (should (search-backward "PARENT" nil t))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "leaves ambiguous anchors unresolved instead of restoring stale bounds"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil)
          (old-context mevedel-instruction-anchor-context-chars))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (setq mevedel-instruction-anchor-context-chars 0)
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "x\ndup\ny\nx\ndup\ny\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "dup\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain ambiguous target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (with-current-buffer source-buf
              (should-not (mevedel--instructions-in
                           (point-min) (point-max)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (setq mevedel-instruction-anchor-context-chars old-context)
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "reanchors a bodyless directive by surrounding context"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "before TARGET after\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET")
              (mevedel--create-directive-in
               source-buf (match-beginning 0) (match-beginning 0)
               t "Do it"))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain bodyless target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (should (= (overlay-start ov) (overlay-end ov)))
              (with-current-buffer source-buf
                (goto-char (overlay-start ov))
                (should (looking-at-p "TARGET")))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-persistence--sanitize-gptel-bounds ()
  ,test
  (test)
  :doc "clamps stale GPTEL_BOUNDS ranges before gptel restore"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999) (999 1000)) (ignore (1 2)))\n"
            ":END:\n"
            "Body\n")
    (mevedel-session-persistence--sanitize-gptel-bounds)
    (let* ((max (point-max))
           (bounds (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
           (response (alist-get 'response bounds)))
      (should (= max (cadar response)))
      (should (= 1 (length response)))
      (dolist (entry bounds)
        (dolist (range (cdr entry))
          (should (<= (cadr range) max))))))
  :doc "deletes unreadable GPTEL_BOUNDS"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: #<marker>\n"
            ":END:\n"
            "Body\n")
    (mevedel-session-persistence--sanitize-gptel-bounds)
    (should-not (org-entry-get (point-min) "GPTEL_BOUNDS")))
  :doc "does not mark resumed buffers modified while clamping bounds"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999)))\n"
            ":END:\n"
            "Body\n")
    (set-buffer-modified-p nil)
    (mevedel-session-persistence--sanitize-gptel-bounds)
    (should-not (buffer-modified-p))
    (pcase-let ((`((response (,beg ,end)))
                 (read (org-entry-get (point-min) "GPTEL_BOUNDS"))))
      (should (= beg 2))
      (should (= end (point-max))))))

(mevedel-deftest mevedel-session-persistence--restore-gptel-state ()
  ,test
  (test)
  :doc "does not dirty resumed buffers while repairing bounds and properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999)))\n"
            ":END:\n"
            "#+begin_tool\n"
            "(:name \"Bash\" :args (:command \"true\"))\n"
            "ok\n"
            "#+end_tool\n"
            "Focused tests passed\n")
    (setq-local gptel-mode nil)
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'gptel-mode)
               (lambda (&optional _arg)
                 (setq-local gptel-mode t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "#+begin_tool")
                   (let ((tool-start (match-beginning 0)))
                     (search-forward "sed tests")
                     (add-text-properties
                      tool-start (match-beginning 0)
                      '(gptel (tool . "stale"))))
                   (goto-char (point-min))
                   (search-forward "sed tests")
                   (add-text-properties
                    (match-beginning 0) (point-max)
                    '(gptel response))))))
      (mevedel-session-persistence--restore-gptel-state))
    (should-not (buffer-modified-p))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Focused tests")
      (should (eq (get-text-property (match-beginning 0) 'gptel)
                  'response)))))

(mevedel-deftest mevedel-session-persistence--normalize-gptel-properties ()
  ,test
  (test)
  :doc "repairs stale tool, mailbox, and structural transcript properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n*** User prompt\n")
    (let (tool-start tool-end response-start response-end task-start task-end
          waiting-start waiting-end mailbox-start mailbox-end after-start
          after-end reasoning-start reasoning-end)
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff --stat\")\n"
              "(:name \"Bash\" :args (:command \"git diff --stat\"))\n\n"
              " file.el | 1 +\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nThinking text.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      (setq response-start (point))
      (insert "I found uncommitted changes on top of the requested commits.\n")
      (setq response-end (point))
      (setq task-start (point))
      (insert "#+begin_tool (TaskList :status \"in_progress\")\n"
              "(:name \"TaskList\" :args (:status \"in_progress\"))\n\n"
              "Tasks with status in_progress:\n"
              "#3 [in_progress] Run reviewer and verifier on current diff\n"
              "#+end_tool\n")
      (setq task-end (point))
      (setq waiting-start (point))
      (insert "Waiting for the reviewer and verifier results.\n\n")
      (setq waiting-end (point))
      (setq mailbox-start (point))
      (insert "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\" description=\"Review current diff\">\n"
              "{\"overall_correctness\":\"patch is incorrect\"}\n"
              "</agent-result>\n")
      (setq mailbox-end (point))
      (setq after-start (point))
      (insert "The reviewer found a blocking permission-mode issue.\n")
      (setq after-end (point))
      ;; Simulate stale saved-session structure: tool ids exist only on
      ;; fragments and response properties bleed into structural blocks.
      (put-text-property (+ tool-start 54) (- tool-end 12)
                         'gptel '(tool . "call_diff"))
      (put-text-property reasoning-start reasoning-end 'gptel 'ignore)
      (put-text-property (- response-start 20) (- response-end 10)
                         'gptel 'response)
      (put-text-property (+ task-start 50) (- task-end 12)
                         'gptel '(tool . "call_task"))
      (put-text-property waiting-start (- waiting-end 20)
                         'gptel 'response)
      (put-text-property (- mailbox-end 24) (- after-end 12)
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  ok)
              (setq ok t)
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 tool-start tool-end '(tool . "call_diff")))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 task-start task-end '(tool . "call_task")))
        (should (all-prop-p reasoning-start reasoning-end 'ignore))
        (should (all-prop-p response-start (- response-end 10) 'response))
        (should (all-prop-p (- response-end 10) response-end nil))
        (should (all-prop-p waiting-start (- waiting-end 20) 'response))
        (should (all-prop-p (- waiting-end 20) waiting-end nil))
        (should (all-prop-p mailbox-start mailbox-end nil))
        (should (all-prop-p after-start (- after-end 12) 'response))
        (should (all-prop-p (- after-end 12) after-end nil)))))
  :doc "does not promote the org drawer or plain first prompt to response"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:MEVEDEL_SESSION_ID: main\n:END:\n\n")
    (let ((drawer-end (point))
          user-start user-end response-start response-end tool-start tool-end)
      (setq user-start (point))
      (insert "Do a review of the changes. Do not modify code.\n\n")
      (setq user-end (point))
      (setq response-start (point))
      (insert "I will inspect the diff and report findings.\n")
      (setq response-end (point))
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output\n"
              "#+end_tool\n")
      (setq tool-end (point))
      ;; User text is nil-property, while the assistant text has a partial
      ;; response run.  Normalization must not infer missing prose bounds.
      (put-text-property response-start (- response-end 10)
                         'gptel 'response)
      (put-text-property (+ tool-start 45) (- tool-end 12)
                         'gptel '(tool . "call_diff"))
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p (point-min) drawer-end nil))
        (should (all-prop-p user-start user-end nil))
        (should (all-prop-p response-start (- response-end 10) 'response))
        (should (all-prop-p (- response-end 10) response-end nil))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 tool-start tool-end '(tool . "call_diff"))))))
  :doc "leaves pasted tool-shaped text in user prompts unclassified"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (user-start user-end)
      (setq user-start (point))
      (insert "Please inspect this transcript:\n\n"
              "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output copied from another editor\n"
              "#+end_tool\n\n"
              "What happened here?\n")
      (setq user-end (point))
      (mevedel-session-persistence--normalize-gptel-properties)
      (let ((pos user-start)
            (ok t))
        (while (and ok (< pos user-end))
          (when (get-text-property pos 'gptel)
            (setq ok nil))
          (setq pos (or (next-single-property-change pos 'gptel nil user-end)
                        user-end)))
        (should ok))))

  :doc "restores persisted hook audit side channels as ignored text"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n"
            "#+begin_summary mevedel-role=compaction-summary\n"
            "summary\n"
            (substring-no-properties
             (mevedel--format-hook-audit-record
              '(:type compact-context
                :event "PreCompact"
                :context "private note")))
            "#+end_summary\n")
    (goto-char (point-min))
    (search-forward "<!-- mevedel-hook-audit -->")
    (should-not (get-text-property (match-beginning 0) 'gptel))
    (mevedel-session-persistence--normalize-gptel-properties)
    (should (eq (get-text-property (match-beginning 0) 'gptel)
                'ignore)))

  :doc "leaves pasted tool-shaped text inside reasoning unclassified"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (reasoning-start tool-start tool-end reasoning-end)
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nUser pasted transcript starts here.\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output copied by user\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (insert "Pasted transcript tail.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      (mevedel-session-persistence--normalize-gptel-properties)
      (should (eq (get-text-property reasoning-start 'gptel) 'ignore))
      (should-not (text-property-any tool-start tool-end 'gptel '(tool . "")))
      (should (eq (get-text-property (1- reasoning-end) 'gptel) 'ignore))))
  :doc "keeps later plain user prompts outside repaired response runs"
  (with-temp-buffer
    (org-mode)
    (let (user1-start user1-end response1-start response1-end
          user2-start user2-end response2-start response2-end)
      (setq user1-start (point))
      (insert "First plain prompt\n\n")
      (setq user1-end (point))
      (setq response1-start (point))
      (insert "First answer.\n\n")
      (setq response1-end (point))
      (setq user2-start (point))
      (insert "Second plain prompt\n\n")
      (setq user2-end (point))
      (setq response2-start (point))
      (insert "Second answer with missing tail property.\n")
      (setq response2-end (point))
      (put-text-property response1-start response1-end
                         'gptel 'response)
      (put-text-property response2-start (- response2-end 10)
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p user1-start user1-end nil))
        (should (all-prop-p response1-start response1-end 'response))
        (should (all-prop-p user2-start user2-end nil))
        (should (all-prop-p response2-start (- response2-end 10) 'response))
        (should (all-prop-p (- response2-end 10) response2-end nil)))))
  :doc "repairs mid-line response prefixes after structural blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (tool-start tool-end tool-prefix-start tool-response-start
          tool-response-end agent-start agent-end agent-prefix-start
          agent-response-start agent-response-end)
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq tool-prefix-start (point))
      (insert "Focu")
      (setq tool-response-start (point))
      (insert "sed tests are green.\n")
      (setq tool-response-end (point))
      (insert "\n")
      (setq agent-start (point))
      (insert "<agent-result agent-id=\"verifier--1\" type=\"verifier\" description=\"Verify\">\n"
              "VERDICT: PASS\n"
              "</agent-result>\n")
      (setq agent-end (point))
      (setq agent-prefix-start (point))
      (insert "The verifier retur")
      (setq agent-response-start (point))
      (insert "ned PASS.\n")
      (setq agent-response-end (point))
      (put-text-property (+ tool-start 42) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (put-text-property tool-response-start tool-response-end
                         'gptel 'response)
      (put-text-property agent-response-start agent-response-end
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 tool-start tool-end '(tool . "call_read")))
        (should (all-prop-p tool-prefix-start tool-response-end 'response))
        (should (all-prop-p agent-start agent-end nil))
        (should (all-prop-p agent-prefix-start agent-response-end
                            'response)))))
  :doc "does not repair response prefixes across a real prompt"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (first-prompt-start tool-start tool-end second-prompt-start
          response-prefix-start response-start response-end)
      (setq first-prompt-start (point))
      (insert "First prompt\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq second-prompt-start (point))
      (insert "Second prompt\n")
      (setq response-prefix-start (point))
      (insert "Conti")
      (setq response-start (point))
      (insert "nuing the answer.\n")
      (setq response-end (point))
      (put-text-property (+ tool-start 42) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (put-text-property response-start response-end
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (should-not (get-text-property second-prompt-start 'gptel))
      (should-not (get-text-property response-prefix-start 'gptel))
      (should (eq (get-text-property response-start 'gptel) 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 2 (length prompts)))
        (should (= first-prompt-start (plist-get (car prompts) :pos)))
        (should (= second-prompt-start (plist-get (cadr prompts) :pos)))
        (should (equal "Second prompt"
                       (plist-get (cadr prompts) :preview))))))
  :doc "keeps queued user batches out of repaired assistant prefixes"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (user-start user-end prefix-start response-start response-end)
      (setq user-start (point))
      (insert "<system-reminder>\n"
              "Queued messages arrived.\n"
              "</system-reminder>\n\n"
              "<queued-user-message-batch count=\"1\">\n"
              "<queued-user-message index=\"1\">\n"
              "Please keep going.\n"
              "</queued-user-message>\n"
              "</queued-user-message-batch>\n")
      (setq user-end (point))
      (setq prefix-start (point))
      (insert "Conti")
      (setq response-start (point))
      (insert "nuing the answer.\n")
      (setq response-end (point))
      (put-text-property response-start response-end
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p user-start user-end nil))
        (should (all-prop-p prefix-start response-end 'response)))))
  :doc "repairs multi-run response prefixes after reasoning blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (reasoning1-start reasoning1-end reasoning1-prefix-start
          reasoning1-response-start reasoning1-response-end
          reasoning2-start reasoning2-end reasoning2-prefix-start
          reasoning2-nil-start reasoning2-response-start
          reasoning2-response-end)
      (setq reasoning1-start (point))
      (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n")
      (setq reasoning1-end (point))
      (insert "\n")
      (setq reasoning1-prefix-start (point))
      (insert "I")
      (setq reasoning1-response-start (point))
      (insert "’ll start by checking bounds.\n")
      (setq reasoning1-response-end (point))
      (insert "\n")
      (setq reasoning2-start (point))
      (insert "#+begin_reasoning\nMore thinking.\n#+end_reasoning\n")
      (setq reasoning2-end (point))
      (insert "\n")
      (setq reasoning2-prefix-start (point))
      (insert "Whi")
      (setq reasoning2-nil-start (point))
      (insert "l")
      (setq reasoning2-response-start (point))
      (insert "e the agents run, I’ll test.\n")
      (setq reasoning2-response-end (point))
      (put-text-property reasoning1-start reasoning1-end
                         'gptel 'ignore)
      (put-text-property reasoning1-response-start reasoning1-response-end
                         'gptel 'response)
      (put-text-property reasoning2-start reasoning2-nil-start
                         'gptel 'ignore)
      (put-text-property reasoning2-response-start reasoning2-response-end
                         'gptel 'response)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p reasoning1-start reasoning1-end 'ignore))
        (should (all-prop-p reasoning1-prefix-start
                            reasoning1-response-end 'response))
        (should (all-prop-p reasoning2-start reasoning2-end 'ignore))
        (should (all-prop-p reasoning2-prefix-start
                            reasoning2-response-end 'response)))))
  :doc "preserves tool blocks nested inside reasoning blocks"
  (with-temp-buffer
    (org-mode)
    (let (reasoning-start tool-start tool-end reasoning-tail-start
          reasoning-end)
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nBefore tool.\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq reasoning-tail-start (point))
      (insert "After tool.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      ;; Simulate the backend shape where the outer reasoning block and the
      ;; inner tool call both have persisted GPTEL_BOUNDS ranges.  The
      ;; normalization pass must not let the broad reasoning range overwrite
      ;; the nested tool range.
      (put-text-property reasoning-start reasoning-end 'gptel 'ignore)
      (put-text-property (+ tool-start 46) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p reasoning-start tool-start 'ignore))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 tool-start tool-end '(tool . "call_read")))
        (should (all-prop-p reasoning-tail-start reasoning-end 'ignore))))))
  :doc "preserves nested reasoning tools when restore order overwrites props"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((tool (1 999 \"call_read\")))\n"
            ":END:\n")
    (let (reasoning-start tool-start tool-end reasoning-tail-start reasoning-end)
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nBefore tool.\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq reasoning-tail-start (point))
      (insert "After tool.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      (put-text-property reasoning-start reasoning-end 'gptel 'ignore)
      (mevedel-session-persistence--normalize-gptel-properties)
      (cl-labels
          ((all-prop-p
            (start end expected)
            (let ((pos start)
                  (ok t))
              (while (and ok (< pos end))
                (unless (equal (get-text-property pos 'gptel) expected)
                  (setq ok nil))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil end)
                              end)))
              ok)))
        (should (all-prop-p reasoning-start tool-start 'ignore))
        (should (test-mevedel-session-persistence--tool-block-props-p
                 tool-start tool-end '(tool . "call_read")))
        (should (all-prop-p reasoning-tail-start reasoning-end 'ignore)))))

(mevedel-deftest mevedel-session-persistence--normalize-gptel-properties/incomplete ()
  ,test
  (test)
  :doc "returns and leaves unclosed structural openers unclassified"
  (dolist (snippet '("#+begin_reasoning\npartial thought"
                     "#+begin_tool (Bash :command \"date\")\n(:name \"Bash\" :args (:command \"date\"))\n\npartial output"
                     "<system-reminder>\npartial reminder"
                     "<queued-user-message-batch count=\"1\">\npartial queue"
                     "<hook-context>\npartial hook"
                     "<!-- mevedel-render-data -->\n(:kind agent-transcript)"
                     ":PROMPT:\npartial prompt"))
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:END:\n*** User prompt\n\n")
      (let ((start (point)))
        (insert snippet)
        (with-timeout
            (1 (ert-fail
                (format "normalization looped on incomplete block: %S"
                        snippet)))
          (mevedel-session-persistence--normalize-gptel-properties))
        (should-not (get-text-property start 'gptel)))))

  :doc "clears stale tool properties from closed unparseable org tool blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n*** User prompt\n\n")
    (let ((start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\nnot a sexp\n#+end_tool\n")
      (put-text-property start (point) 'gptel '(tool . "call-bad"))
      (mevedel-session-persistence--normalize-gptel-properties)
      (should (test-mevedel-session-persistence--no-tool-prop-p
               start (point))))))

(mevedel-deftest mevedel-session-persistence--dynamic-system-preset-p ()
  ,test
  (test)
  :doc "detects function-valued system presets"
  (let ((gptel--preset 'mevedel-test-dynamic))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-dynamic)
                   `(:system ,(lambda () "Dynamic prompt"))))))
      (should (mevedel-session-persistence--dynamic-system-preset-p))))
  :doc "detects dynamic-spec system presets"
  (let ((gptel--preset 'mevedel-test-dynamic-spec))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-dynamic-spec)
                   '(:system (:eval (mevedel-system-build-prompt)))))))
      (should (mevedel-session-persistence--dynamic-system-preset-p))))
  :doc "ignores static string system presets"
  (let ((gptel--preset 'mevedel-test-static))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-static)
                   '(:system "Static prompt")))))
      (should-not (mevedel-session-persistence--dynamic-system-preset-p)))))

(mevedel-deftest mevedel-session-persistence--save-gptel-state-around ()
  ,test
  (test)
  :doc "removes frozen GPTEL_SYSTEM before delegated save for dynamic presets"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Frozen prompt")
                delegated-system
                system-present-at-delegate
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (setq system-present-at-delegate
                          (org-entry-get (point-min) "GPTEL_SYSTEM"))
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (42 55)))")))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () t)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (should-not delegated-system)
            (should-not system-present-at-delegate)
	    (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))
	    (should-not (org-entry-get (point-min) "GPTEL_BOUNDS"))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "removes accumulated and case-variant GPTEL_SYSTEM properties"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (insert ":PROPERTIES:\n:gptel_system: Lowercase prompt\n:GPTEL_SYSTEM+: Extra prompt\n:OTHER: Keep\n:END:\n")
	  (setq-local mevedel--session
		      (mevedel-session-create
		       "main"
		       (test-mevedel-session-persistence--make-workspace root)))
	  (let ((gptel-system-prompt "Frozen prompt")
		seen-system)
	    (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () t))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () nil)))
	      (mevedel-session-persistence--save-gptel-state-around
	       (lambda ()
		 (setq seen-system (org-entry-get (point-min) "GPTEL_SYSTEM")))))
	    (should-not seen-system)
	    (let ((text (buffer-substring-no-properties
			 (point-min) (point-max))))
	      (should-not (string-match-p "GPTEL_SYSTEM" text))
	      (should-not (string-match-p "gptel_system" text))
	      (should (string-match-p ":OTHER: Keep" text)))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "removes multiline GPTEL_SYSTEM values"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (org-entry-put (point-min) "GPTEL_SYSTEM"
			 "Frozen first\nFrozen second")
	  (goto-char (point-max))
	  (insert "Body\n")
	  (setq-local mevedel--session
		      (mevedel-session-create
		       "main"
		       (test-mevedel-session-persistence--make-workspace root)))
	  (let ((gptel-system-prompt "Frozen prompt"))
	    (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () t))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () nil)))
	      (mevedel-session-persistence--save-gptel-state-around
	       (lambda () nil))))
	  (let ((text (buffer-substring-no-properties
		       (point-min) (point-max))))
	    (should-not (string-match-p "GPTEL_SYSTEM" text))
	    (should-not (string-match-p "Frozen first" text))
	    (should-not (string-match-p "Frozen second" text))
	    (should (string-match-p "Body" text))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "routes top-level property writes around Org entry helpers"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (insert ":PROPERTIES:\n:GPTEL_SYSTEM: Frozen prompt\n:END:\n")
	  (let (start end)
	    (setq start (point))
	    (insert "Assistant body\n")
	    (setq end (point))
	    (add-text-properties start end '(gptel response))
	    (setq-local mevedel--session
			(mevedel-session-create
			 "main"
			 (test-mevedel-session-persistence--make-workspace root)))
	    (let ((orig-fun
		   (lambda ()
		     (org-entry-put (point-min) "GPTEL_MODEL" "fake-model")
		     (org-entry-delete (point-min) "GPTEL_SYSTEM"))))
	      (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () nil))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () `((response (,start ,end)))))
			((symbol-function 'org-entry-put)
			 (lambda (&rest _)
			   (error "Slow org-entry-put should not run")))
			((symbol-function 'org-entry-delete)
			 (lambda (&rest _)
			   (error "Slow org-entry-delete should not run"))))
		(mevedel-session-persistence--save-gptel-state-around
		 orig-fun)))
	    (let ((text (buffer-substring-no-properties
			 (point-min) (point-max))))
	      (should (string-match-p ":GPTEL_MODEL: fake-model" text))
	      (should (string-match-p ":GPTEL_BOUNDS: " text))
	      (should-not (string-match-p ":GPTEL_SYSTEM:" text)))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "stabilizes GPTEL_BOUNDS after delegated save resizes the property drawer"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* main\n")
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((start (point-marker))
                end
                orig-fun)
            (insert "Assistant body\n")
            (setq end (point-marker))
            (add-text-properties start end '(gptel response))
            (setq orig-fun
                  (lambda ()
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (1 2)))")))
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () nil)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (pcase-let ((`((response (,beg ,stored-end)))
                         (read (org-entry-get (point-min) "GPTEL_BOUNDS"))))
              (should (= beg (marker-position start)))
              (should (= stored-end (marker-position end))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "delegates unchanged for non-dynamic presets"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Custom prompt")
                delegated-system
                system-present-at-delegate
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (setq system-present-at-delegate
                          (org-entry-get (point-min) "GPTEL_SYSTEM"))))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () nil)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (should (equal "Custom prompt" delegated-system))
            (should (equal "Frozen prompt" system-present-at-delegate))
            (should (equal "Frozen prompt"
                           (org-entry-get (point-min) "GPTEL_SYSTEM")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-persistence--refresh-restored-buffers ()
  ,test
  (test)
  :doc "reverts unmodified visiting buffers after file restore"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (write-region "new\n" nil file nil 'silent)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'restore :path file))
           (list :succeeded 1))
          (with-current-buffer buf
            (should (equal "new\n"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "reverts modified visiting buffers after confirmed file restore"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (write-region "new\n" nil file nil 'silent)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'restore :path file))
           (list :succeeded 1))
          (with-current-buffer buf
            (should-not (buffer-modified-p))
            (should (equal "new\n"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "kills visiting buffers for deleted restored files"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (delete-file file)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'delete :path file))
           (list :succeeded 1))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--prepare-buffers-for-restore ()
  ,test
  (test)
  :doc "discard marks affected modified buffers unmodified before restore"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?d)))
            (should (equal plan
                           (mevedel-session-persistence--prepare-buffers-for-restore
                            nil 1 plan))))
          (with-current-buffer buf
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "abort returns abort sentinel when affected buffers are modified"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should (eq :abort
                        (mevedel-session-persistence--prepare-buffers-for-restore
                         nil 1 plan))))
          (with-current-buffer buf
            (should (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


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


;;
;;; Phase 4: split-on-compact

(mevedel-deftest mevedel-session-persistence-rotate-segment ()
  ,test
  (test)
  :doc "creates a new segment file and bumps the segment counter"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (_orig-segment buffer-file-name)
               (new-path (mevedel-session-persistence-rotate-segment
                          session buf "Summary of the prior conversation.")))
          (with-current-buffer buf
            (should new-path)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            ;; Old segment file still exists.
            (let ((seg1 (mevedel-session-persistence--segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              ;; Old segment got finalized property
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))
            ;; New buffer points at the new segment file.
            (should (equal new-path buffer-file-name))
            ;; Buffer body contains the summary.
            (should (string-match-p "Summary of the prior conversation."
                                    (buffer-string)))
            ;; Buffer also contains the segment-2 number property.
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before editing"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-persistence-rotate-segment
                   session buf "Summary after stale modtime."))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before deleting pending text"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (goto-char (point-max))
            (insert "\nPending prompt")
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
	          (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
	                     (lambda (&rest _args)
	                       (error "Supersession prompt"))))
            (should (mevedel-session-persistence-rotate-segment
                     session buf "Summary after stale pending text."
                     :pending-text "\nPending prompt")))
          (with-current-buffer buf
            (should (string-suffix-p "Pending prompt\n" (buffer-string)))
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment differs on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (write-region "external edit\n" nil buffer-file-name nil 'silent)
            (should-not (verify-visited-file-modtime buf)))
          (should-error
           (mevedel-session-persistence-rotate-segment
            session buf "Summary should not be written.")
           :type 'error))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment was deleted on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (delete-file buffer-file-name)
            (should-not (file-exists-p buffer-file-name)))
          (should-error
           (mevedel-session-persistence-rotate-segment
            session buf "Summary should not be written.")
           :type 'error)
          (should (= 1 (mevedel-session-current-segment session))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar reflects bumped current-segment after rotation"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment
           session buf "First summary.")
          (let ((plist (mevedel-session-persistence-read
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "two consecutive rotations produce three segment files"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment session buf "Summary A.")
          (mevedel-session-persistence-rotate-segment session buf "Summary B.")
          (let ((dir (mevedel-session-save-path session)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 1)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 2)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 3)))
            (should (= 3 (mevedel-session-current-segment session)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-rotate-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-persistence-rotate-segment
                         session buf "Won't happen."))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-start-fresh-segment ()
  ,test
  (test)
  :doc "creates an empty new segment without a compaction summary"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (new-path (mevedel-session-persistence-start-fresh-segment
                          session buf :initial-text "### ")))
          (with-current-buffer buf
            (should new-path)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            (should (equal new-path buffer-file-name))
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))
            (should (string-suffix-p "### " (buffer-string)))
            (should-not (string-match-p "#\\+begin_summary"
                                        (buffer-string)))
            (with-temp-buffer
              (insert-file-contents new-path)
              (should-not (string-match-p "### " (buffer-string))))
            (let ((seg1 (mevedel-session-persistence--segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "Initial prompt"
                                        (buffer-string)))
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before fresh segment edit"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-persistence-start-fresh-segment
                   session buf :initial-text "### "))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar and prompt index point at the new empty segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-start-fresh-segment
           session buf :initial-text "### ")
          (let ((plist (mevedel-session-persistence-read
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))
            (should-not (assoc 2 (plist-get plist :prompt-index)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes finalized segment prompt index before bumping segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (setf (mevedel-session-prompt-index session)
                '((1 . ((:turn 1 :pos 999 :preview "stale")))))
          (with-current-buffer buf
            (goto-char (point-max))
            (let ((response-start (point)))
              (insert "\nAssistant response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "\nFresh unsaved prompt\n"))
          (mevedel-session-persistence-start-fresh-segment
           session buf :initial-text "### ")
          (let* ((plist (mevedel-session-persistence-read
                         (mevedel-session-persistence--sidecar-path
                          (mevedel-session-save-path session))))
                 (seg1 (cdr (assoc 1 (plist-get plist :prompt-index)))))
            (should seg1)
            (should (equal "Initial prompt" (plist-get (car seg1) :preview)))
            (should (equal "Fresh unsaved prompt"
                           (plist-get (cadr seg1) :preview)))
            (should-not (equal 999 (plist-get (car seg1) :pos)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "fresh-no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-fresh-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-persistence-start-fresh-segment
                         session buf :initial-text "### "))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-rollback ()
  ,test
  (test)
  :doc "rolls live buffer and segment counter back on sidecar write failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name))
               (old-text (with-current-buffer buf
                           (buffer-substring (point-min) (point-max)))))
	          (cl-letf (((symbol-function 'mevedel-session-persistence-write)
	                     (lambda (&rest _)
	                       (error "Sidecar write failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary that will not commit.")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (equal old-segment buffer-file-name))
            (should (equal old-text
                           (buffer-substring (point-min) (point-max)))))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "restores sidecar when failure happens after sidecar publish"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (sidecar (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session))))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--save-instructions)
	                     (lambda (&rest _)
	                       (error "Instruction save failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary that will not commit.")))
          (let ((plist (mevedel-session-persistence-read sidecar)))
            (should (= 1 (mevedel-session-current-segment session)))
            (should (= 1 (plist-get plist :current-segment))))
          (with-current-buffer buf
            (should (equal
                     (mevedel-session-persistence--segment-path
                      (mevedel-session-save-path session) 1)
                     buffer-file-name)))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "restores pending prompt when predecessor save fails"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
	          (cl-letf (((symbol-function 'save-buffer)
	                     (lambda (&rest _)
	                       (error "Save failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary."
              :pending-text "Pending prompt\n")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (equal old-segment buffer-file-name))
            (should (string-match-p "Pending prompt" (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence--summary-block ()
  ,test
  (test)
  :doc "wraps summary in #+begin_summary block"
  (let ((wrapped (mevedel-session-persistence--summary-block "hello")))
    (should (string-match-p "#\\+begin_summary" wrapped))
    (should (string-match-p "#\\+end_summary" wrapped))
    (should (string-match-p "Another language model started" wrapped))
    (should (string-match-p "hello" wrapped)))
  :doc "marker lines carry gptel ignore property"
  (let ((wrapped (mevedel-session-persistence--summary-block "x")))
    ;; The first character is in the begin_summary marker.
    (should (eq 'ignore (get-text-property 0 'gptel wrapped)))))

(mevedel-deftest mevedel-session-persistence--strip-summary-handoff-prefix ()
  ,test
  (test)
  :doc "removes the model-facing handoff prefix before summary reuse"
  (let* ((summary "## Goal\n- continue")
         (prefixed (concat mevedel-session-persistence--summary-handoff-prefix
                           summary)))
    (should (equal summary
                   (mevedel-session-persistence--strip-summary-handoff-prefix
                    prefixed)))
    (should (equal summary
                   (mevedel-session-persistence--strip-summary-handoff-prefix
                    summary)))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-tail ()
  ,test
  (test)
  :doc "rotates into summary followed by preserved tail and pending prompt"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :tail-text "Tail turn.\n"
           :pending-text "Pending prompt.\n")
          (with-current-buffer buf
            (let ((text (buffer-string)))
              (should (string-match-p "#\\+begin_summary mevedel-role=compaction-summary" text))
              (should (string-match-p "Summary\\." text))
              (should (string-match-p "Tail turn\\." text))
              (should (string-match-p "Pending prompt\\." text)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-pending-save ()
  ,test
  (test)
  :doc "pending prompts are not saved before request completion"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Old prompt\n")
              (insert (propertize "Old response\n" 'gptel 'response))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :pending-text "Pending prompt\n")
          (let ((seg1 (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) 1))
                (seg2 (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) 2)))
            (with-temp-buffer
              (insert-file-contents seg1)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents seg2)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-current-buffer buf
              (should (string-match-p "Pending prompt" (buffer-string)))
              (should-not (buffer-modified-p)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-tail-index ()
  ,test
  (test)
  :doc "copied tail prompts do not consume new cumulative turn ids"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*"))
              (tail-text
               (concat
                "Tail prompt 1\n"
                (propertize "Tail response 1\n" 'gptel 'response)
                "Tail prompt 2\n"
                (propertize "Tail response 2\n" 'gptel 'response))))
          (setf (mevedel-session-turn-count session) 10)
          (setf (mevedel-session-prompt-index session)
                (list
                 (cons 1
                       (cl-loop for turn from 1 to 10
                                collect
                                (list :turn turn
                                      :cum-turn turn
                                      :pos turn
                                      :preview (format "Prompt %d" turn))))))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :tail-text tail-text
           :pending-text "Next real prompt\n")
          (mevedel-session-persistence--update-prompt-index session buf)
          (let ((seg2 (cdr (assoc 2 (mevedel-session-prompt-index session)))))
            (should (= 1 (length seg2)))
            (should (= 1 (plist-get (car seg2) :turn)))
            (should (= 3 (plist-get (car seg2) :file-turn)))
            (should (= 11 (plist-get (car seg2) :cum-turn)))
            (should (equal "Next real prompt"
                           (plist-get (car seg2) :preview)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 5: read path

(mevedel-deftest mevedel-session-persistence-load-sidecar ()
  ,test
  (test)
  :doc "reads a current-version sidecar"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp `(:version ,(mevedel-version) :session-name "x"))
          (let ((plist (mevedel-session-persistence-load-sidecar tmp)))
            (should (equal (mevedel-version) (plist-get plist :version)))
            (should (equal "x" (plist-get plist :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "rejects an unsupported sidecar version"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp '(:version "v0.0.0" :session-name "x"))
          (should-not (mevedel-session-persistence-load-sidecar tmp)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(mevedel-deftest mevedel-session-persistence-restore ()
  ,test
  (test)
  :doc "round-trips a single-segment session into a new buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "First user prompt\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                ;; Release the lock + kill the buffer (the test buffer didn't
                ;; go through chat-buffer-init-common so the kill-hook isn't
                ;; installed; we mirror its work manually).
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (should (file-exists-p session-dir))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (derived-mode-p 'org-mode))
                  (should (bound-and-true-p gptel-mode))
                  (should mevedel--session)
                  (should (equal "main"
                                 (mevedel-session-name mevedel--session)))
                  (should (= 1 (mevedel-session-current-segment
                                mevedel--session)))
                  (should (string-match-p "First user prompt"
                                          (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "retargets and persists a missing working directory"
  (cl-destructuring-bind
      (_workspace tempdir _missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (prompt dir default mustmatch &rest _)
                         (should (string-match-p
                                  "deleted-worktree.*missing" prompt))
                         (should (equal tempdir
                                        (file-name-as-directory dir)))
                         (should (equal tempdir
                                        (file-name-as-directory default)))
                         (should mustmatch)
                         replacement-dir)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal replacement-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "does not persist a retargeted directory in read-only mode"
  (cl-destructuring-bind
      (_workspace tempdir missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir))
                      ((symbol-function
                        'mevedel-session-persistence-lock-acquire)
                       (lambda (&rest _) nil)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should mevedel-session--read-only-mode)
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "rejects an invalid replacement before opening the session"
  (cl-destructuring-bind
      (workspace tempdir missing-dir _replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let ((outside (make-temp-file "mevedel-cwd-outside-" t))
          (buf-name (mevedel-session-buffer-name "main" workspace)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) outside)))
              (should-error
               (mevedel-session-persistence-restore session-dir)
               :type 'user-error))
            (should-not
             (file-exists-p
              (mevedel-session-persistence--lock-path session-dir)))
            (should-not (get-buffer buf-name))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (when (file-directory-p outside)
          (delete-directory outside t))
        (mevedel-workspace-clear-registry))))
  :doc "round-trips a multi-segment (compacted) session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Original prompt\n")
                  (mevedel-session-persistence-save session buf)
                  (mevedel-session-persistence-rotate-segment
                   session buf "Summary of segment 1.")
                  (insert "After-compact prompt\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (= 2 (mevedel-session-current-segment
                                mevedel--session)))
                  (should (string-match-p "Summary of segment 1\\."
                                          (buffer-string)))
                  (should (string-match-p "After-compact prompt"
                                          (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "preserves permission rules across resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (setf (mevedel-session-permission-rules session)
                      '(("Read" :path "/tmp/foo/**" :action allow)))
                (with-current-buffer buf
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (equal '(("Read" :path "/tmp/foo/**" :action allow))
                                 (mevedel-session-permission-rules
                                  mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not double-rewrite nested relocated working directories"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (workspace-id (format "nested-restore-id-%s" (gensym)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (old-cwd (file-name-as-directory
                   (file-name-concat old-root "src")))
         (expected-cwd (file-name-as-directory
                        (file-name-concat new-root "src")))
         buf session session-dir restored)
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory expected-cwd t)
          (mevedel-workspace-clear-registry)
          (let ((workspace (mevedel-workspace-get-or-create
                            'project workspace-id old-root "nested-proj")))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-working-directory session) old-cwd))
          (setq buf (generate-new-buffer "*test-data-buf*"))
          (with-current-buffer buf
            (org-mode)
            (insert "Nested relocation\n")
            (mevedel-session-persistence-save session buf))
          (setq session-dir (mevedel-session-save-path session))
          (test-mevedel-session-persistence--release-and-kill
           buf session)
          (setq buf nil)
          (mevedel-workspace-clear-registry)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "nested-proj")
          (setq restored (mevedel-session-persistence-restore
                          session-dir))
          (with-current-buffer restored
            (should (equal expected-cwd
                           (mevedel-session-working-directory
                            mevedel--session)))))
      (test-mevedel-session-persistence--release-and-kill
       buf session)
      (test-mevedel-session-persistence--release-and-kill
       restored
       (and restored (buffer-local-value 'mevedel--session restored)))
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (mevedel-workspace-clear-registry)))
  :doc "switches to a live buffer instead of re-loading"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf-name (mevedel-session-buffer-name "main" workspace))
               (buf      (get-buffer-create buf-name))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Live buffer\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                ;; Restore should return the existing live buffer.
                (should (eq buf restored)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 6: locking

(mevedel-deftest mevedel-session-persistence--same-host-lock-active-p ()
  ,test
  (test)
  :doc "returns nil for dead PIDs"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) nil)))
    (should-not
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when process start predates lock time"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-subtract lock-time 10))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active within timestamp tolerance"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-add lock-time 1))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "treats live PIDs as stale when process start proves PID reuse"
  (let* ((lock-time (time-subtract (current-time) (* 30 24 60 60)))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (current-time))))
      (should-not
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active when process start is unavailable"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) nil)))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when lock time is malformed"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) (current-time))))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345 :emacs-invocation-time "old")))))

(mevedel-deftest mevedel-session-persistence--active-lock-p ()
  ,test
  (test)
  :doc "treats cross-host locks as active without local PID checks"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "old"
                         :buffer "*remote*")
                   (current-buffer)))
          (should (mevedel-session-persistence--active-lock-p tempdir)))
      (delete-directory tempdir t)))
  :doc "treats same-host reused-PID locks as inactive"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should-not
             (mevedel-session-persistence--active-lock-p tempdir))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence-lock-acquire ()
  ,test
  (test)
  :doc "writes a fresh lock when none exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          (should (mevedel-session-persistence-lock-acquire
                   tempdir "*test-buf*"))
          (let ((lock-path
                 (mevedel-session-persistence--lock-path tempdir)))
            (should (file-exists-p lock-path))
            (let ((plist (mevedel-session-persistence--read-lock lock-path)))
              (should (= (emacs-pid) (plist-get plist :pid)))
              (should (equal "*test-buf*" (plist-get plist :buffer))))))
      (delete-directory tempdir t)))
  :doc "unreadable raced lock signals instead of recursing"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--read-lock)
                   (lambda (&rest _) nil))
                  ((symbol-function
                    'mevedel-session-persistence--write-lock-atomic)
                   (lambda (&rest _) nil)))
          (should-error
           (mevedel-session-persistence-lock-acquire tempdir "*test-buf*")
           :type 'user-error))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [b]reak overwrites the lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a live PID on this host.
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?b)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*test-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*test-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [r]ead-only returns nil and preserves lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"))))
          ;; Original lock untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "*other-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [a]bort signals user-error"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*test-buf*")
             :type 'user-error)))
      (delete-directory tempdir t)))
  :doc "same-host reused PID follows the stale-lock confirmation path"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _)
                       (error "Unexpected live-lock prompt")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "breaks a stale lock when user confirms"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a hostname-mismatching PID-alive predicate
          ;; stubbed nil so the stale-lock branch fires deterministically.
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "leaves a stale lock alone when user declines"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*new-buf*")
             :type 'user-error))
          ;; Original lock remains untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= 999999 (plist-get plist :pid)))))
      (delete-directory tempdir t)))
  :doc "cross-host: read-only response returns nil"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"))))
          ;; The remote lock is still in place.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "other-host" (plist-get plist :hostname)))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence-lock-release ()
  ,test
  (test)
  :doc "deletes our own lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (mevedel-session-persistence-lock-acquire tempdir "*x*")
          (should (file-exists-p lock-path))
          (mevedel-session-persistence-lock-release tempdir)
          (should-not (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "leaves alien locks alone"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :buffer "*x*")
                   (current-buffer)))
          (mevedel-session-persistence-lock-release tempdir)
          ;; Lock still present.
          (should (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "is a no-op when no lock exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          ;; Should not error.
          (mevedel-session-persistence-lock-release tempdir)
          (should-not (file-exists-p
                       (mevedel-session-persistence--lock-path tempdir))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--sweep-stale-locks ()
  ,test
  (test)
  :doc "removes same-host dead-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "stale-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock")))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*gone*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "removes same-host reused-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "reused-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock"))
               (lock-time    (time-subtract (current-time) (* 30 24 60 60))))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves same-host live-PID locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (live-dir     (file-name-as-directory
                              (file-name-concat sessions-dir "live-sess")))
               (live-lock    (file-name-concat live-dir ".lock")))
          (make-directory live-dir t)
          (with-temp-file live-lock
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "new"
                         :buffer "*live*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p live-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves cross-host locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (remote-dir   (file-name-as-directory
                              (file-name-concat sessions-dir "remote-sess")))
               (remote-lock  (file-name-concat remote-dir ".lock")))
          (make-directory remote-dir t)
          (with-temp-file remote-lock
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p remote-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-ensure-files-acquires-lock ()
  ,test
  (test)
  :doc "lazy materialization writes the .lock file"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hi\n")
                (let ((path (mevedel-session-persistence-ensure-files
                             session buf)))
                  (should (file-exists-p
                           (mevedel-session-persistence--lock-path path)))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 7: rewind picker

(mevedel-deftest mevedel-session-persistence--collect-prompts ()
  ,test
  (test)
  :doc "extracts user prompt regions in document order"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "Second prompt\n")
    (insert (propertize "Okay.\n" 'gptel 'response))
    (insert "Third prompt\n")
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 3 (length prompts)))
      (should (= 1 (plist-get (nth 0 prompts) :turn)))
      (should (= 2 (plist-get (nth 1 prompts) :turn)))
      (should (= 3 (plist-get (nth 2 prompts) :turn)))
      (should (string-match-p "First prompt"
                              (plist-get (nth 0 prompts) :preview)))
      (should (string-match-p "Third prompt"
                              (plist-get (nth 2 prompts) :preview)))))
  :doc "skips blank-only regions"
  (with-temp-buffer
    (insert "   \n\n  \t\n")
    (insert (propertize "response" 'gptel 'response))
    (insert "Real prompt\n")
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 1 (length prompts)))
      (should (string-match-p "Real prompt"
                              (plist-get (car prompts) :preview)))))
  :doc "skips indented leading property drawer"
  (with-temp-buffer
    (insert "  :PROPERTIES:\n")
    (insert "  :MEVEDEL_SESSION: metadata\n")
    (insert "  :END:\n")
    (let ((prompt-start (point)))
      (insert "Real prompt after metadata\n")
      (insert (propertize "response" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "Real prompt after metadata"
                       (plist-get (car prompts) :preview))))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (insert "Search for docs\n")
    (insert (propertize "Second answer.\n" 'gptel 'response))
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 2 (length prompts)))
      (should (equal "Fetch a page"
                     (plist-get (nth 0 prompts) :preview)))
      (should (equal "Search for docs"
                     (plist-get (nth 1 prompts) :preview)))))
  :doc "keeps user-authored org block marker as prompt start"
  (with-temp-buffer
    (let ((prompt-start (point)))
      (insert "#+begin_src emacs-lisp\n")
      (insert "(message \"hello\")\n")
      (insert "#+end_src\n")
      (insert (propertize "Response.\n" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "#+begin_src emacs-lisp"
                       (plist-get (car prompts) :preview)))))))

(mevedel-deftest mevedel-session-persistence--update-prompt-index ()
  ,test
  (test)
  :doc "updates only the live segment's entry"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                ;; Pre-seed with a finalized segment 1 entry.
                (setf (mevedel-session-prompt-index session)
                      '((1 . ((:turn 1 :pos 1 :preview "old prompt")))))
                (setf (mevedel-session-current-segment session) 2)
                (insert "New live prompt\n")
                (mevedel-session-persistence--update-prompt-index
                 session buf)
                (let ((index (mevedel-session-prompt-index session)))
                  ;; Segment 1 untouched.
                  (should (= 1 (length (cdr (assoc 1 index)))))
                  ;; Segment 2 has the new prompt.
                  (should (assoc 2 index))
                  (should (= 1 (length (cdr (assoc 2 index)))))
                  (should
                   (string-match-p
                    "New live prompt"
                    (plist-get (car (cdr (assoc 2 index))) :preview)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--latest-user-message-from-index ()
  ,test
  (test)
  :doc "returns newest prompt by cumulative turn"
  (should
   (equal "third"
          (mevedel-session-persistence--latest-user-message-from-index
           '((2 . ((:turn 1 :cum-turn 3 :preview "third")))
             (1 . ((:turn 1 :cum-turn 1 :preview "first")
                   (:turn 2 :cum-turn 2 :preview "second")))))))
  :doc "falls back to segment and turn ordering for old sidecars"
  (should
   (equal "newer segment"
          (mevedel-session-persistence--latest-user-message-from-index
           '((1 . ((:turn 2 :preview "older segment")))
             (2 . ((:turn 1 :preview "newer segment")))))))
  :doc "ignores blank previews"
  (should
   (null (mevedel-session-persistence--latest-user-message-from-index
          '((1 . ((:turn 1 :preview "   "))))))))

(mevedel-deftest mevedel-session-persistence--prompt-candidates ()
  ,test
  (test)
  :doc "returns flat alist with unique display strings across segments"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :pos 0 :preview "alpha")
                  (:turn 2 :pos 100 :preview "beta")))
            (2 . ((:turn 1 :pos 0 :preview "alpha")  ; same preview, different segment
                  (:turn 2 :pos 50 :preview "gamma")))))
    (let ((candidates
           (mevedel-session-persistence--prompt-candidates session)))
      (should (= 4 (length candidates)))
      ;; All display strings unique (segment + turn folded in).
      (should (= 4 (length (cl-delete-duplicates
                            (mapcar #'car candidates) :test #'equal))))
      ;; Newest prompt in the newest segment first.
      (let* ((first (car candidates))
             (plist (cdr first)))
        (should (= 2 (plist-get plist :segment)))
        (should (= 2 (plist-get plist :turn)))))
    (mevedel-workspace-clear-registry))
  :doc "preserves raw file turn for compacted segments with copied tail"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((2 . ((:turn 1 :file-turn 3 :cum-turn 11
                   :pos 100 :preview "after tail")))))
    (let* ((candidate
            (car (mevedel-session-persistence--prompt-candidates session)))
           (plist (cdr candidate)))
      (should (= 1 (plist-get plist :turn)))
      (should (= 3 (plist-get plist :file-turn))))
    (mevedel-workspace-clear-registry))
  :doc "derives raw file turn from segment tail metadata for old sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (segment-2
                (file-name-concat tempdir ".mevedel" "sessions"
                                  "main-2026-05-07T00-00-0000"
                                  "segment-0002.chat.org")))
          (setf (mevedel-session-save-path session)
                (file-name-directory segment-2))
          (make-directory (file-name-directory segment-2) t)
          (with-temp-file segment-2
            (insert ":PROPERTIES:\n")
            (insert ":MEVEDEL_SEGMENT_TAIL_PROMPTS: 2\n")
            (insert ":END:\n"))
          (setf (mevedel-session-prompt-index session)
                '((2 . ((:turn 1 :cum-turn 11
                         :pos 100 :preview "after tail")))))
          (let* ((candidate
                  (car (mevedel-session-persistence--prompt-candidates
                        session)))
                 (plist (cdr candidate)))
            (should (= 1 (plist-get plist :turn)))
            (should (= 3 (plist-get plist :file-turn)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--find-turn-cutoff ()
  ,test
  (test)
  :doc "returns position of next user prompt"
  (with-temp-buffer
    (insert "First prompt\n")                               ; pos 1, turn 1
    (let ((next-prompt-pos
           (progn
             (insert (propertize "Response 1.\n" 'gptel 'response))
             (point))))
      (insert "Second prompt\n")                            ; turn 2 starts here
      (insert (propertize "Response 2.\n" 'gptel 'response))
      ;; Cutoff for turn 1 is the start of turn 2's prompt.
      (should (= next-prompt-pos
                 (mevedel-session-persistence--find-turn-cutoff 1)))))
  :doc "returns point-max when turn-n is the last"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Response.\n" 'gptel 'response))
    (insert "Last prompt\n")
    (should (= (point-max)
               (mevedel-session-persistence--find-turn-cutoff 2))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (let ((next-prompt-pos (point)))
      (insert "Search for docs\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (should (= next-prompt-pos
                 (mevedel-session-persistence--find-turn-cutoff 1)))))
  :doc "stays consistent with transcript-repaired assistant fragments"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Initial answer.\n" 'gptel 'response))
    (insert (propertize "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n"
                        'gptel '(tool . "call_1")))
    (insert "Conti")
    (insert (propertize "nuing the answer.\n" 'gptel 'response))
    (let ((next-prompt-pos (point)))
      (insert "Second prompt\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 2 (length prompts)))
        (should (equal "Second prompt"
                       (plist-get (nth 1 prompts) :preview)))
        (should (= next-prompt-pos
                   (mevedel-session-persistence--find-turn-cutoff 1)))))))

(mevedel-deftest mevedel-rewind ()
  ,test
  (test)
  :doc "errors when no current session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error (mevedel-rewind) :type 'user-error)))
  :doc "errors when request in flight"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request 'placeholder))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when no recorded prompts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request nil))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "from view buffer rewinds the data buffer and rerenders the view"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer "*test-data-buf*"))
               (view-buf (generate-new-buffer "*test-view-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local mevedel--session session)
                  (insert "First prompt\n")
                  (insert (propertize "First reply.\n" 'gptel 'response))
                  (insert "Second prompt\n")
                  (insert (propertize "Second reply.\n" 'gptel 'response))
                  (mevedel-session-persistence-save session data-buf))
                (mevedel-view--setup view-buf data-buf)
                (let ((choice
                       (caar (last (mevedel-session-persistence--prompt-candidates
                                    session))))
                      loaded-buffer loaded-segment loaded-turn)
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (&rest _args) choice))
                            ((symbol-function
                              'mevedel-session-persistence--load-truncated)
                             (lambda (_session buffer segment turn
                                               &optional _cum-turn
                                               _logical-turn)
                               (setq loaded-buffer buffer)
                               (setq loaded-segment segment)
                               (setq loaded-turn turn)
                               (with-current-buffer buffer
                                 (let ((inhibit-read-only t))
                                   (erase-buffer)
                                   (insert "First prompt\n")
                                   (insert
                                    (mevedel--format-hook-audit-record
                                     '(:type prompt-rewrite
                                       :event "UserPromptSubmit"
                                       :original "first original"
                                       :submitted "First prompt")))
                                   (insert (propertize
                                            "First reply.\n"
                                            'gptel 'response)))
                                 (setq buffer-file-name nil)
                                 (setq-local
                                  mevedel-session--fork-pending t)
                                 (when-let* ((vb (buffer-local-value
                                                  'mevedel--view-buffer
                                                  buffer))
                                             ((buffer-live-p vb)))
                                   (with-current-buffer vb
                                     (mevedel-view--full-rerender)))))))
                    (with-current-buffer view-buf
                      (mevedel-rewind)))
                  (should (eq loaded-buffer data-buf))
                  (should (= loaded-segment 1))
                  (should (= loaded-turn 1)))
                (with-current-buffer data-buf
                  (should (string-match-p "First prompt" (buffer-string)))
                  (should (string-match-p "First reply" (buffer-string)))
                  (should-not (string-match-p "Second prompt" (buffer-string)))
                  (should mevedel-session--fork-pending))
                (with-current-buffer view-buf
                  (should (derived-mode-p 'mevedel-view-mode))
                  (let ((rendered (buffer-substring-no-properties
                                   (point-min) (point-max))))
                    (should (string-match-p "You" rendered))
                    (should (string-match-p "First prompt" rendered))
                    (should (string-match-p "hook changed prompt" rendered))
                    (should-not (string-match-p "first original" rendered))
                    (should (string-match-p "Assistant" rendered))
                    (should-not (string-match-p ":PROPERTIES:" rendered))
                    (should-not (string-match-p "Second prompt" rendered)))))
            (when (buffer-live-p view-buf) (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--load-truncated ()
  ,test
  (test)
  :doc "disconnects file and sets fork-pending flag"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Lone prompt\n")
                (mevedel-session-persistence-save session buf)
                (mevedel-session-persistence--load-truncated
                 session buf 1 1)
                ;; Content is reloaded from the segment file.
                (should (string-match-p "Lone prompt" (buffer-string)))
                ;; File-name disconnected, fork-pending set, modified flag clear.
                (should (null buffer-file-name))
                (should mevedel-session--fork-pending)
                (should-not (buffer-modified-p)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when target segment file is missing"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hi\n")
                (mevedel-session-persistence-save session buf)
                ;; Ask to load segment 99, which doesn't exist.
                (should-error
                 (mevedel-session-persistence--load-truncated
                  session buf 99 1)
                 :type 'user-error))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "file turn selects prompts after copied compaction tail"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Initial prompt\n")
                (mevedel-session-persistence-save session buf)
                (let ((segment-2
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 2)))
                  (make-directory (file-name-directory segment-2) t)
                  (with-temp-file segment-2
                    (insert ":PROPERTIES:\n")
                    (insert ":MEVEDEL_SEGMENT_TAIL_PROMPTS: 2\n")
                    (insert ":END:\n\n")
                    (insert "#+begin_summary\nSummary\n#+end_summary\n")
                    (insert "Tail prompt 1\nTail response 1\n")
                    (insert "Tail prompt 2\nTail response 2\n")
                    (insert "Actual prompt 1\nActual response 1\n")
                    (insert "Actual prompt 2\nActual response 2\n"))
                  (cl-letf (((symbol-function 'gptel-org--restore-state)
                             (lambda ()
                               (save-excursion
                                 (goto-char (point-min))
                                 (while (re-search-forward
                                         "^.*response [0-9]+$" nil t)
                                   (put-text-property
                                    (line-beginning-position)
                                    (line-end-position)
                                    'gptel 'response))))))
                    (mevedel-session-persistence--load-truncated
                     session buf 2 3))
                  (should (string-match-p "Actual prompt 1"
                                          (buffer-string)))
                  (should-not (string-match-p "Actual prompt 2"
                                              (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 8: file restore plan

(mevedel-deftest mevedel-session-persistence--state-at-turn ()
  ,test
  (test)
  :doc "picks the latest snapshot whose turn is <= cum-turn"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "fooA" :version 1))))
            (3 . (("/abs/foo" . (:backup-name "fooC" :version 3))
                  ("/abs/bar" . (:backup-name "barB" :version 2))))
            (5 . (("/abs/foo" . (:backup-name "fooE" :version 5))))))
    ;; State at turn 4: foo=fooC (turn 3), bar=barB (turn 3).
    (let ((state (mevedel-session-persistence--state-at-turn session 4)))
      (should (= 2 (length state)))
      (should (equal "fooC"
                     (plist-get (cdr (assoc "/abs/foo" state)) :backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state)) :backup-name))))
    ;; State at turn 1: just foo=fooA.
    (let ((state (mevedel-session-persistence--state-at-turn session 1)))
      (should (= 1 (length state)))
      (should (equal "fooA"
                     (plist-get (cdr (assoc "/abs/foo" state)) :backup-name))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence--latest-snapshot-entry ()
  ,test
  (test)
  :doc "returns highest-version entry for the path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "v1" :version 1))))
            (5 . (("/abs/foo" . (:backup-name "v3" :version 3))))
            (3 . (("/abs/foo" . (:backup-name "v2" :version 2))))))
    (let ((latest (mevedel-session-persistence--latest-snapshot-entry
                   session "/abs/foo")))
      (should (equal "v3" (plist-get latest :backup-name))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence-restore-plan ()
  ,test
  (test)
  :doc "noop when current content matches target snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-file-history--backup-name path 1)))
          (write-region "v1" nil path nil 'silent)
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name "v1")
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (null plan))))   ; noop entries filtered
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "create when target has content but file currently absent"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-file-history--backup-name path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name "content")
          ;; File doesn't currently exist.
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'create (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "delete when target is absent but file exists"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "stale.el")))
          (write-region "stale content" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name nil :version 1
                                  :backup-time "..." :file-mtime nil))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'delete (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "overwrite when current content diverges from latest snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (b1   (mevedel-file-history--backup-name path 1))
               (b2   (mevedel-file-history--backup-name path 2)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) b1 "v1")
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) b2 "v2")
          ;; Current file content is something the snapshots have never seen.
          (write-region "external edits" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,b1 :version 1
                                  :backup-time "..." :file-mtime "..."))))
                  (2 . ((,path . (:backup-name ,b2 :version 2
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'overwrite (plist-get (car plan) :action)))
            (should (plist-get (car plan) :diverged))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-execute-restore ()
  ,test
  (test)
  :doc "applies create / delete / restore actions correctly"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((create-path (file-name-concat tempdir "new.el"))
               (delete-path (file-name-concat tempdir "old.el"))
               (restore-path (file-name-concat tempdir "modified.el"))
               (backup-name-create
                (mevedel-file-history--backup-name create-path 1))
               (backup-name-restore
                (mevedel-file-history--backup-name restore-path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name-create "newly created")
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name-restore "original")
          ;; Set up current state: delete-path exists, restore-path has different content
          (write-region "to be deleted" nil delete-path nil 'silent)
          (write-region "diverged" nil restore-path nil 'silent)
          (let* ((plan
                  (list (list :action 'create  :path create-path
                              :backup-name backup-name-create)
                        (list :action 'delete  :path delete-path)
                        (list :action 'overwrite :path restore-path
                              :backup-name backup-name-restore
                              :diverged t)))
                 (result (mevedel-session-persistence-execute-restore
                          session plan)))
            (should (= 3 (plist-get result :succeeded)))
            (should (null (plist-get result :failed)))
            (should (file-exists-p create-path))
            (should-not (file-exists-p delete-path))
            (with-temp-buffer
              (insert-file-contents create-path)
              (should (equal "newly created" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents restore-path)
              (should (equal "original" (buffer-string))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "stops on first failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "fine.el"))
               (bn   (mevedel-file-history--backup-name path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) bn "ok")
          (let* ((plan
                  (list (list :action 'create :path path :backup-name bn)
                        ;; Bogus backup name — read of backup will fail.
                        (list :action 'create
                              :path (file-name-concat tempdir "two.el")
                              :backup-name "nonexistent@v1")
                        ;; Should not be reached.
                        (list :action 'create
                              :path (file-name-concat tempdir "three.el")
                              :backup-name bn)))
                 (result (mevedel-session-persistence-execute-restore
                          session plan)))
            (should (= 1 (plist-get result :succeeded)))
            (should (plist-get result :failed))
            (should-not (file-exists-p
                         (file-name-concat tempdir "three.el")))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 9: fork-on-send + rename-session

(mevedel-deftest mevedel-session-persistence-fork-now ()
  ,test
  (test)
  :doc "creates a fresh session directory and copies predecessor segments"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Original prompt\n")
                (insert
                 (mevedel--format-hook-audit-record
                  '(:type prompt-rewrite
                    :event "UserPromptSubmit"
                    :original "original prompt"
                    :submitted "Original prompt")))
                (mevedel-session-persistence-save session buf)
                (mevedel-session-persistence-rotate-segment
                 session buf "Summary 1.")
                (insert "Live prompt\n")
                (mevedel-session-persistence-save session buf)
                ;; Capture parent state, then simulate a rewind to S1 T1.
                (let ((parent-id   (mevedel-session-session-id session))
                      (parent-path (mevedel-session-save-path session)))
                  (make-directory (file-name-concat parent-path "agents") t)
                  (make-directory (file-name-concat parent-path "plans") t)
                  (write-region
                   "# Parent plan\n" nil
                   (file-name-concat parent-path "plans/current.md")
                   nil 'silent)
                  (write-region
                   "session-local history\n" nil
                   (file-name-concat parent-path "input-history.el")
                   nil 'silent)
                  (setf (mevedel-session-plan-metadata session)
                        '(:path "plans/current.md" :status presented))
                  (write-region
                   "copied transcript\n" nil
                   (file-name-concat parent-path "agents/copy.chat.org")
                   nil 'silent)
                  (write-region
                   "future transcript\n" nil
                   (file-name-concat parent-path "agents/future.chat.org")
                   nil 'silent)
                  (setf (mevedel-session-prompt-index session)
                        '((1 . ((:turn 1 :cum-turn 1)))
                          (2 . ((:turn 1 :cum-turn 2)))))
                  (setf (mevedel-session-agent-transcripts session)
                        '(("copy--1" :parent-turn 1
                           :path "agents/copy.chat.org")
                          ("future--2" :parent-turn 2
                           :path "agents/future.chat.org")
                          ("poison--3" :parent-turn 1
                           :path "../poison.chat.org")))
                  (mevedel-session-persistence--load-truncated
                   session buf 1 1 1)
                  (let ((new-path
                         (mevedel-session-persistence-fork-now buf)))
                    (should new-path)
                    (should-not (equal parent-path new-path))
                    ;; Fork has its own session-id (different from parent).
                    (should-not (equal parent-id
                                       (mevedel-session-session-id session)))
                    ;; Forked-from fields populated.
                    (should (equal parent-id
                                   (mevedel-session-forked-from-session-id
                                    session)))
                    ;; Predecessor segment 1 doesn't exist (we picked
                    ;; segment 1, so there's no segment < 1 to copy).
                    ;; The picked-segment file does exist with the
                    ;; truncated content.
                    (should (file-exists-p
                             (mevedel-session-persistence--segment-path
                              new-path 1)))
                    (with-temp-buffer
                      (insert-file-contents
                       (mevedel-session-persistence--segment-path
                        new-path 1))
                      (should (string-match-p
                               "<!-- mevedel-hook-audit -->"
                               (buffer-string)))
                      (should (string-match-p
                               "original prompt"
                               (buffer-string))))
                    (should (file-exists-p
                             (file-name-concat
                              new-path "plans/current.md")))
                    (should-not (file-exists-p
                                 (file-name-concat
                                  new-path "input-history.el")))
                    (with-temp-buffer
                      (insert-file-contents
                       (file-name-concat new-path "plans/current.md"))
                      (should (equal "# Parent plan\n"
                                     (buffer-string))))
                    ;; Fork-pending cleared.
                    (should-not mevedel-session--fork-pending)
                    (should-not mevedel-session--rewind-context)
                    ;; Buffer-file-name pointing at fork's segment.
                    (should (string-prefix-p
                             new-path
                             (expand-file-name buffer-file-name)))
                    ;; Agent transcript files are copied only when they
                    ;; belong to the forked turn range and pass path
                    ;; validation.  Later transcripts are pruned from
                    ;; the fork's sidecar state.
                    (should (file-exists-p
                             (file-name-concat
                              new-path "agents/copy.chat.org")))
                    (should-not (file-exists-p
                                 (file-name-concat
                                  new-path "agents/future.chat.org")))
                    (should-not (file-exists-p
                                 (expand-file-name
                                  "../poison.chat.org" new-path)))
                    (should (assoc "copy--1"
                                   (mevedel-session-agent-transcripts
                                    session)))
                    (should-not (assoc "future--2"
                                       (mevedel-session-agent-transcripts
                                        session))))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when buffer is not in rewind preview state"
  (with-temp-buffer
    (let ((mevedel-session--fork-pending nil))
      (should-error (mevedel-session-persistence-fork-now (current-buffer))
                    :type 'user-error))))

(mevedel-deftest mevedel-rename-session ()
  ,test
  (test)
  :doc "renames the session-name field and the buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Hi\n")
                (mevedel-session-persistence-save session buf)
                (let ((old-save-path (mevedel-session-save-path session)))
                  (mevedel-rename-session "alt-permissions")
                  (should (equal "alt-permissions"
                                 (mevedel-session-name session)))
                  ;; Old directory gone, new directory exists.
                  (should-not (file-directory-p old-save-path))
                  (should (file-directory-p
                           (mevedel-session-save-path session)))
                  ;; New directory name reflects the new session-name.
                  (should (string-prefix-p
                           "alt-permissions-"
                           (file-name-nondirectory
                            (directory-file-name
                             (mevedel-session-save-path session)))))
                  ;; Buffer renamed per convention.
                  (should (string-match-p
                           "\\`\\*mevedel:alt-permissions@"
                           (buffer-name buf)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 10: resume / list / save commands

(mevedel-deftest mevedel-session-persistence-list-sessions ()
  ,test
  (test)
  :doc "lists materialized sessions, sorted newest-first"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "alpha" workspace))
               (b1 (generate-new-buffer "*test-session-alpha*"))
               (s2 (mevedel-session-create "beta" workspace))
               (b2 (generate-new-buffer "*test-session-beta*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Hello\n")
                  (mevedel-session-persistence-save s1 b1))
                (sleep-for 1.1)   ; ensure :updated-at differs
                (with-current-buffer b2
                  (org-mode)
                  (insert "World\n")
                  (mevedel-session-persistence-save s2 b2))
                (let ((listed (mevedel-session-persistence-list-sessions
                               workspace)))
                  (should (= 2 (length listed)))
                  ;; b2 (beta) was saved last → first in list.
                  (should (equal "beta"
                                 (plist-get
                                  (plist-get (car listed) :summary)
                                  :session-name)))
                  (should (equal "alpha"
                                 (plist-get
                                  (plist-get (cadr listed) :summary)
                                  :session-name)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "returns nil for a workspace with no sessions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (null (mevedel-session-persistence-list-sessions workspace)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "resume completion preserves newest-first session order"
  (let* ((displays '("2h ago       new" "yesterday    old"))
         (collection
          (mevedel-session-persistence--ordered-display-collection
           displays 'mevedel-session))
         (metadata (funcall collection "" nil 'metadata)))
    (should (eq 'identity
                (cdr (assq 'display-sort-function (cdr metadata)))))
    (should (eq 'identity
                (cdr (assq 'cycle-sort-function (cdr metadata)))))))

(mevedel-deftest mevedel-session-persistence--read-summary ()
  ,test
  (test)
  :doc "extracts only picker-relevant fields"
  (let ((tmp (make-temp-file "mevedel-summary-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp `(:version ,(mevedel-version)
                          :session-name "demo"
                          :session-id "demo-1234"
                          :workspace nil
                          :updated-at "2026-04-23T12-00-00"
                          :first-user-message "Hello"
                          :latest-user-message "Latest"
                          :tasks nil
                          :permission-rules nil))
          (let ((s (mevedel-session-persistence--read-summary tmp)))
            (should (equal "demo" (plist-get s :session-name)))
            (should (equal "demo-1234" (plist-get s :session-id)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "derives latest preview from prompt index for old sidecars"
  (let ((tmp (make-temp-file "mevedel-summary-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp `(:version ,(mevedel-version)
                          :session-name "demo"
                          :session-id "demo-1234"
                          :workspace nil
                          :updated-at "2026-04-23T12-00-00"
                          :first-user-message "Hello"
                          :prompt-index
                          ((1 . ((:turn 1 :cum-turn 1 :preview "Hello")))
                           (2 . ((:turn 1 :cum-turn 2 :preview "Latest"))))
                          :tasks nil
                          :permission-rules nil))
          (let ((s (mevedel-session-persistence--read-summary tmp)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "returns nil on unreadable file"
  (should (null (mevedel-session-persistence--read-summary
                 "/nonexistent/path"))))

(mevedel-deftest mevedel-session-persistence--format-session-candidate ()
  ,test
  (test)
  :doc "prefers latest preview over first preview"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 2
                      :total-turn-count 4
                      :first-user-message "Original request"
                      :latest-user-message "Newest request")))))
    (should (string-match-p "Newest request" display))
    (should-not (string-match-p "Original request" display)))
  :doc "falls back to first preview for old summaries"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 1
                      :total-turn-count 1
                      :first-user-message "Original request")))))
    (should (string-match-p "Original request" display))))


;;
;;; Phase 11: relocation, self-heal, save-failure flag

(mevedel-deftest mevedel-session-persistence--reconcile-relocation ()
  ,test
  (test)
  :doc "rewrites permission rules whose :path is under the saved root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id" "/new/root/" "ws"))
         (session   (mevedel-session-create "x" workspace)))
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/old/root/foo/**" :action allow)
            ("Read"  :path "/old/root/bar/baz" :action allow)
            ("Bash"  :pattern "git log*"      :action allow)
            ("Read"  :path "/elsewhere/baz"   :action deny)))
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id" :root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "foo/**")
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "bar/baz")
                     (plist-get (cdr (nth 1 rules)) :path)))
      ;; Bash rule untouched (no :path).
      (should (equal "git log*" (plist-get (cdr (nth 2 rules)) :pattern)))
      ;; Out-of-tree path untouched.
      (should (equal "/elsewhere/baz"
                     (plist-get (cdr (nth 3 rules)) :path))))
    (mevedel-workspace-clear-registry))
  :doc "no-op when saved root matches current"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id2" "/same/root/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/same/root/foo" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id2" :root "/same/root/" :name "ws"))
    (should (equal orig-rules
                   (mevedel-session-permission-rules session)))
    (mevedel-workspace-clear-registry))
  :doc "does not rewrite permission paths already under nested current root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id3" "/old/root/packages/api/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/old/root/packages/api/foo" :action allow)
                       ("Read" :path "/old/root/other" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id3" :root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal "/old/root/packages/api/foo"
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat
                      (expand-file-name "/old/root/packages/api/")
                      "other")
                     (plist-get (cdr (nth 1 rules)) :path))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence--detect-highest-segment ()
  ,test
  (test)
  :doc "returns the maximum segment number on disk"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (progn
          (write-region "" nil
                        (file-name-concat tempdir "segment-0001.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0003.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          ;; Decoy file shouldn't count.
          (write-region "" nil
                        (file-name-concat tempdir "session.meta.el")
                        nil 'silent)
          (should (= 3 (mevedel-session-persistence--detect-highest-segment
                        tempdir))))
      (delete-directory tempdir t)))
  :doc "returns 0 when no segment files exist"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (should (= 0 (mevedel-session-persistence--detect-highest-segment
                      tempdir)))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--self-heal-segment-counter ()
  ,test
  (test)
  :doc "trusts filesystem when sidecar disagrees"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-" t))))
    (unwind-protect
        (let ((session (mevedel-session-create
                        "x"
                        (mevedel-workspace-get-or-create
                         'project "id" "/" "x"))))
          (setf (mevedel-session-current-segment session) 1)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0001.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          ;; Suppress display-warning popup during the test.
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (mevedel-session-persistence--self-heal-segment-counter
             session tempdir))
	          (should (= 2 (mevedel-session-current-segment session))))
	      (delete-directory tempdir t)
	      (mevedel-workspace-clear-registry)))
  :doc "finalizes predecessor when healing upward"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-" t))))
    (unwind-protect
        (let ((session (mevedel-session-create
                        "x"
                        (mevedel-workspace-get-or-create
                         'project "id" "/" "x")))
              (seg1 (file-name-concat tempdir "segment-0001.chat.org")))
          (setf (mevedel-session-current-segment session) 1)
          (write-region "* Chat\n" nil seg1 nil 'silent)
          (write-region "* Chat\n" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (mevedel-session-persistence--self-heal-segment-counter
             session tempdir))
          (with-temp-buffer
            (insert-file-contents seg1)
            (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                    (buffer-string)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 12: auto-cleanup

(mevedel-deftest mevedel-session-persistence--parse-iso-time ()
  ,test
  (test)
  :doc "parses our ISO-with-dashes format"
  (let ((time (mevedel-session-persistence--parse-iso-time
               "2026-04-23T14-30-15")))
    (should time)
    (should (equal "2026-04-23T14-30-15"
                   (format-time-string "%FT%H-%M-%S" time))))
  :doc "returns nil for malformed input"
  (should (null (mevedel-session-persistence--parse-iso-time "not a date")))
  (should (null (mevedel-session-persistence--parse-iso-time nil))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired ()
  ,test
  (test)
  :doc "deletes sessions older than the cap"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               ;; Reset the throttle so tests don't leak.
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s1 (mevedel-session-create "old" workspace))
               (b1 (generate-new-buffer "*test-old-buf*"))
               (s2 (mevedel-session-create "new" workspace))
               (b2 (generate-new-buffer "*test-new-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-persistence-save s1 b1))
                (with-current-buffer b2
                  (org-mode)
                  (insert "New\n")
                  (mevedel-session-persistence-save s2 b2))
                ;; Forge :updated-at on the old session to be 14 days ago.
                (let* ((old-path (mevedel-session-save-path s1))
                       (sidecar  (mevedel-session-persistence--sidecar-path
                                  old-path))
                       (plist    (mevedel-session-persistence-read sidecar))
                       (forged   (format-time-string
                                  "%FT%H-%M-%S"
                                  (time-subtract (current-time)
                                                 (* 14 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist))
                ;; Release locks so cleanup can delete the dirs.
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s1))
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s2))
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 1 deleted))
                  (should-not (file-directory-p
                               (mevedel-session-save-path s1)))
                  (should (file-directory-p
                           (mevedel-session-save-path s2)))))
            (when (buffer-live-p b1)
              (with-current-buffer b1 (set-buffer-modified-p nil))
              (kill-buffer b1))
            (when (buffer-live-p b2)
              (with-current-buffer b2 (set-buffer-modified-p nil))
              (kill-buffer b2))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "skips locked sessions even when expired"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "stuck" workspace))
               (b (generate-new-buffer "*test-stuck-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save s b))
                ;; Forge old :updated-at.
                (let* ((path (mevedel-session-save-path s))
                       (sidecar (mevedel-session-persistence--sidecar-path
                                 path))
                       (plist   (mevedel-session-persistence-read sidecar))
                       (forged  (format-time-string
                                 "%FT%H-%M-%S"
                                 (time-subtract (current-time)
                                                (* 30 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist))
                ;; The lock from save still exists with our PID — live.
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 0 deleted))
                  (should (file-directory-p
                           (mevedel-session-save-path s)))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions whose same-host lock has a reused PID"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "reused" workspace))
               (b (generate-new-buffer "*test-reused-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save s b))
                (let* ((path      (mevedel-session-save-path s))
                       (sidecar   (mevedel-session-persistence--sidecar-path
                                   path))
                       (plist     (mevedel-session-persistence-read sidecar))
                       (old-time  (time-subtract (current-time)
                                                 (* 30 24 60 60)))
                       (forged    (format-time-string "%FT%H-%M-%S"
                                                       old-time))
                       (lock-path (mevedel-session-persistence--lock-path
                                   path)))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist)
                  (with-temp-file lock-path
                    (prin1 (list :pid 12345
                                 :hostname (system-name)
                                 :emacs-invocation-time forged
                                 :buffer "*old-buf*")
                           (current-buffer))))
                (cl-letf (((symbol-function
                            'mevedel-session-persistence--pid-alive-p)
                           (lambda (&rest _) t))
                          ((symbol-function
                            'mevedel-session-persistence--pid-start-time)
                           (lambda (&rest _) (current-time))))
                  (let ((deleted
                         (mevedel-session-persistence-cleanup-expired
                          workspace t)))
                    (should (= 1 deleted))
                    (should-not (file-directory-p
                                 (mevedel-session-save-path s))))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let ((mevedel-session-max-age-days nil))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace t))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "throttled to at most one run per workspace per Emacs"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal)))
          ;; First call returns 0 (no sessions); second call (no force) returns nil.
          (should (= 0 (mevedel-session-persistence-cleanup-expired
                        workspace)))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Integration: pipeline snapshot -> request struct -> session save

(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)

(mevedel-deftest mevedel-session-persistence/file-history-roundtrip ()
  ,test
  (test)
  :doc "a modifying tool routed through the pipeline lands a backup in file-history"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((data-buf (get-buffer "*test-data-buf*"))
               (tracked  (file-name-concat tempdir "tracked.el"))
               ;; Plant pre-edit content so the snapshot has a
               ;; non-nil "original" to compare against at save time.
               (_ (let ((coding-system-for-write 'utf-8-unix))
                    (write-region "ORIGINAL\n" nil tracked nil 'silent)))
               ;; Mock tool with `get-path' so the pipeline's
               ;; snapshot step fires for it.  Handler mutates the
               ;; file to simulate what a real Edit / Write would do.
               (tool (mevedel-tool--create
                      :name "WriteMock"
                      :handler (lambda (args)
                                 (let ((p (plist-get args :path))
                                       (c (plist-get args :content)))
                                   (let ((coding-system-for-write 'utf-8-unix))
                                     (write-region c nil p nil 'silent))
                                   "ok"))
                      :args '((path string :required "Path")
                              (content string :required "Content"))
                      :get-path (lambda (args) (plist-get args :path))
                      :read-only-p nil
                      :async-p nil))
               result)
          ;; Plant the session buffer-locally so
          ;; `mevedel-pipeline-run-tool' captures it as the context.
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (mevedel-session-workspace session))
            ;; Begin a request so tool-fs records the original content.
            (mevedel-request-begin session)
            (unwind-protect
                (progn
                  (mevedel-pipeline-run-tool
                   tool (lambda (r) (setq result r))
                   (list :path tracked :content "MODIFIED\n"))
                  (should (equal "ok" result))
                  ;; Snapshot step captured the pre-edit content.
                  (let ((ht (mevedel-request-file-snapshots
                             mevedel--current-request)))
                    (should (hash-table-p ht))
                    (should (equal "ORIGINAL\n" (gethash tracked ht))))
                  ;; Drive a save (what the DONE terminal handler
                  ;; would do in production) and verify a backup file
                  ;; landed under file-history/.
                  (mevedel-session-persistence-save session data-buf)
                  (let* ((snaps (mevedel-session-file-snapshots session))
                         (turn-entry (cdar snaps))
                         (file-entry (assoc tracked turn-entry))
                         (backup-name (plist-get (cdr file-entry)
                                                 :backup-name))
                         (backup-path (mevedel-file-history--backup-path
                                       (mevedel-session-save-path session)
                                       backup-name)))
                    (should snaps)
                    (should backup-name)
                    (should (file-exists-p backup-path))
                    ;; Backup stores the post-edit content (the state
                    ;; `snapshot-modified' observes at save time).
                    (with-temp-buffer
                      (insert-file-contents-literally backup-path)
                      (should (equal "MODIFIED\n" (buffer-string))))))
              (mevedel-request-end))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; View rerender on resume / rewind

(mevedel-deftest mevedel-session-persistence--find-file-noselect ()
  ,test
  (test)
  :doc "disables so-long predicate while opening persisted files"
  (let ((observed :unset)
        (opened (generate-new-buffer " *mevedel-so-long-open*")))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (_file &rest _args)
                     (setq observed (funcall so-long-predicate))
                     opened)))
          (should (eq opened
                      (mevedel-session-persistence--find-file-noselect
                       "/tmp/session.chat.org")))
          (should (eq observed nil)))
      (when (buffer-live-p opened)
        (kill-buffer opened)))))

(mevedel-deftest mevedel-session-persistence/view-rerender ()
  ,test
  (test)
  :doc "save path calls mevedel-view--full-rerender after buffer save"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--view-buffer vb)
                (with-current-buffer vb
                  (setq-local mevedel--data-buffer buf))
                (insert "prompt before save\n")
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (mevedel-session-persistence-save session buf))
                (should (= rerender-count 1)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path calls mevedel-view--full-rerender"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (rerender-count 0)
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume test\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                ;; The rerender may fire via init-common's view-ensure
                ;; flow (which touches the view buffer).  We only care
                ;; that it fires at least once.
                (should (>= rerender-count 1)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
	  (delete-directory tempdir t)
	  (mevedel-workspace-clear-registry)))

  :doc "resume path renders persisted hook audit records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist
                              '((org-mode . "*** ")))
                  (insert "\n\n*** rewritten prompt")
                  (insert
                   (mevedel--format-hook-audit-record
                    '(:type prompt-rewrite
                      :event "UserPromptSubmit"
                      :original "original prompt"
                      :submitted "rewritten prompt")))
                  (insert "\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (mevedel-view--full-rerender)
                  (let ((text (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "hook changed prompt" text))
                    (should (string-match-p "rewritten prompt" text))
                    (should-not (string-match-p "original prompt" text)))
                  (goto-char (point-min))
                  (search-forward "hook changed prompt")
                  (mevedel-view-toggle-section)
                  (let ((expanded (buffer-substring-no-properties
                                   (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "original prompt" expanded)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path restores view input history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from history resume test\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (let ((history-path
                       (file-name-concat tempdir ".mevedel/input-history.el")))
                  (make-directory (file-name-directory history-path) t)
                  (mevedel-session-persistence-write
                   history-path
                   '(:version 1 :entries ("second" "first"))))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (should (equal '("second" "first")
                                 (mevedel-view-history--entries)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume command displays the companion view buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               restored displayed)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume display test\n")
                  (mevedel-session-persistence-save session buf))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (let ((default-directory tempdir))
                  (cl-letf (((symbol-function 'mevedel-workspace)
                             (lambda (&optional _arg) workspace))
                            ((symbol-function 'completing-read)
                             (lambda (_prompt _collection &optional
                                               _predicate _require-match
                                               _initial-input _hist def
                                               _inherit-input-method)
                               def))
                            ((symbol-function 'display-buffer)
                             (lambda (buffer &optional _action _frame)
                               (setq displayed buffer)
                               buffer)))
                    (setq restored (mevedel-resume))))
                (should (buffer-live-p restored))
                (should (eq displayed
                            (buffer-local-value 'mevedel--view-buffer
                                                restored))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rewind path calls mevedel-view--full-rerender"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--view-buffer vb)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (mevedel-session-persistence--load-truncated
                   session buf 1 1))
                (should (>= rerender-count 1)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; WAIT-handler fork: data-buffer send after rewind

(mevedel-deftest mevedel-session-persistence/wait-handler-fork ()
  ,test
  (test)
  :doc "WAIT handler materializes fork before request-begin when fork-pending"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (fork-calls 0)
               (begin-calls 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel-session--fork-pending t)
                (cl-letf
                    (((symbol-function 'mevedel-session-persistence-fork-now)
                      (lambda (_b) (cl-incf fork-calls)))
                     ((symbol-function 'mevedel-request-begin)
                      (lambda (_s &optional _d) (cl-incf begin-calls))))
                  (let* ((handlers
                          (mevedel-preset--build-handlers
                           '((WAIT) (TYPE) (DONE) (ERRS))))
                         (wait-handler (car (cdr (assq 'WAIT handlers))))
                         (info (list :buffer buf))
                         (fsm (gptel-make-fsm :info info)))
                    (funcall wait-handler fsm)
                    (should (= 1 fork-calls))
                    (should (= 1 begin-calls)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "WAIT handler skips fork when not in rewind preview"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (fork-calls 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (cl-letf
                    (((symbol-function 'mevedel-session-persistence-fork-now)
                      (lambda (_b) (cl-incf fork-calls)))
                     ((symbol-function 'mevedel-request-begin)
                      (lambda (_s &optional _d) nil)))
                  (let* ((handlers
                          (mevedel-preset--build-handlers
                           '((WAIT) (TYPE) (DONE) (ERRS))))
                         (wait-handler (car (cdr (assq 'WAIT handlers))))
                         (fsm (gptel-make-fsm :info (list :buffer buf))))
                    (funcall wait-handler fsm)
                    (should (zerop fork-calls)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; View-send fork gating (empty input / local slash / unknown slash)

(mevedel-deftest mevedel-session-persistence/view-send-fork-gating ()
  ,test
  (test)
  :doc "empty input after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (_b) (cl-incf fork-calls))))
            (with-current-buffer view-buf
              ;; Empty input region.
              (should-error (mevedel-view-send) :type 'user-error)))
          (should (zerop fork-calls)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "local slash command after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0)
        (dispatch-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-slash-commands
                 `(("local" . ,(lambda (&rest _) (cl-incf dispatch-calls))))))
            (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                       (lambda (_b) (cl-incf fork-calls))))
              (with-current-buffer view-buf
                (goto-char (point-max))
                (insert "/local")
                (mevedel-view-send)))
            (should (= 1 dispatch-calls))
            (should (zerop fork-calls))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "compaction in flight blocks view send"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--compaction-in-flight t))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (insert "hello")
            (should-error (mevedel-view-send) :type 'user-error)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "unknown slash command after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-slash-commands nil))
            (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                       (lambda (_b) (cl-incf fork-calls))))
              (with-current-buffer view-buf
                (goto-char (point-max))
                (insert "/no-such-command")
                (mevedel-view-send))))
          (should (zerop fork-calls)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))


;;
;;; Fork releases parent lock

(mevedel-deftest mevedel-session-persistence/fork-releases-parent-lock ()
  ,test
  (test)
  :doc "fork-now deletes the parent session's .lock"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((parent-path (mevedel-session-save-path session))
                       (parent-lock
                        (mevedel-session-persistence--lock-path parent-path)))
                  (should (file-exists-p parent-lock))
                  (mevedel-session-persistence--load-truncated
                   session buf 1 1 1)
                  (mevedel-session-persistence-fork-now buf)
                  (should-not (file-exists-p parent-lock))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Sidecar missing / unreadable fallback on restore

(mevedel-deftest mevedel-session-persistence/sidecar-missing-on-restore ()
  ,test
  (test)
  :doc "deleted sidecar causes restore to synthesize a fresh session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (delete-file
                 (mevedel-session-persistence--sidecar-path session-dir))
                (cl-letf (((symbol-function 'display-warning) #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should mevedel--session)
                  (should (mevedel-session-session-id mevedel--session))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "corrupt sidecar also causes restore to synthesize a fresh session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (write-region "this is not a plist" nil
                              (mevedel-session-persistence--sidecar-path
                               session-dir)
                              nil 'silent)
                (cl-letf (((symbol-function 'display-warning) #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Cross-host cleanup behavior

(mevedel-deftest mevedel-session-persistence/cleanup-cross-host-lock ()
  ,test
  (test)
  :doc "cross-host lock prevents cleanup from deleting an expired session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 1)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello\n")
                  (mevedel-session-persistence-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (lock-path
                        (mevedel-session-persistence--lock-path save-path))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path save-path))
                       (plist (mevedel-session-persistence-read sidecar))
                       (forged (format-time-string
                                "%FT%H-%M-%S"
                                (time-subtract (current-time)
                                               (* 7 24 60 60)))))
                  ;; Forge an expired :updated-at.
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist)
                  ;; Overwrite our lock with a cross-host lock (still
                  ;; active from cleanup's perspective).
                  (with-temp-file lock-path
                    (prin1 (list :pid 99999
                                 :hostname "other-host.example"
                                 :emacs-invocation-time "..."
                                 :buffer "*remote*")
                           (current-buffer)))
                  ;; Run cleanup.
                  (let ((deleted (mevedel-session-persistence-cleanup-expired
                                  workspace t)))
                    (should (= 0 deleted))
                    (should (file-directory-p save-path)))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Same-name sessions in one workspace

(mevedel-deftest mevedel-session-persistence/same-name-sessions ()
  ,test
  (test)
  :doc "restore resolves the right session-id when two sessions share a name"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "main" workspace))
               (b1 (generate-new-buffer "*test-data-1*"))
               (s2 (mevedel-session-create "main" workspace))
               (b2 (generate-new-buffer "*test-data-2*"))
               restored)
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (setq-local mevedel--session s1)
                  (insert "session one\n")
                  (mevedel-session-persistence-save s1 b1))
                ;; Force a visible clock gap so session ids differ.
                (sleep-for 1.1)
                (with-current-buffer b2
                  (org-mode)
                  (setq-local mevedel--session s2)
                  (insert "session two\n")
                  (mevedel-session-persistence-save s2 b2))
                (should-not (equal (mevedel-session-session-id s1)
                                   (mevedel-session-session-id s2)))
                ;; Both buffers share the default
                ;; `*mevedel:main@...*' buffer name (identical session
                ;; name + workspace).  Restore must match session-id,
                ;; not just the buffer name, and return b1 when asked
                ;; to resume s1's dir.
                (setq restored
                      (mevedel-session-persistence-restore
                       (mevedel-session-save-path s1)))
                (should (buffer-live-p restored))
                (should (eq restored b1))
                (with-current-buffer restored
                  (should (equal (mevedel-session-session-id s1)
                                 (mevedel-session-session-id mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)
            (when (and restored (buffer-live-p restored))
              (test-mevedel-session-persistence--release-and-kill
               restored
               (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Session-id collision retry loop

(mevedel-deftest mevedel-session-persistence/id-collision-retry ()
  ,test
  (test)
  :doc "ensure-files retries id generation when the target dir already exists"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir
                (mevedel-session-persistence--sessions-dir workspace))
               ;; Pre-create a directory that a naive `compute-id'
               ;; would collide with.
               (colliding "main-collision-0001")
               (remaining '("main-collision-0002" "main-collision-0003")))
          (make-directory (file-name-concat sessions-dir colliding) t)
          (let ((session (mevedel-session-create "main" workspace))
                (buf     (generate-new-buffer "*test-data-buf*")))
            (unwind-protect
                (cl-letf*
                    ;; First call returns the colliding id, subsequent
                    ;; calls return fresh ids from `remaining'.
                    ((first-call-p t)
                     ((symbol-function
                       'mevedel-session-persistence--compute-id)
                      (lambda (_name)
                        (cond
                         (first-call-p
                          (setq first-call-p nil)
                          colliding)
                         (t (pop remaining))))))
                  (with-current-buffer buf
                    (org-mode)
                    (insert "hi\n")
                    (mevedel-session-persistence-ensure-files session buf)
                    ;; Picked a non-colliding id.
                    (should-not (equal colliding
                                       (mevedel-session-session-id session)))
                    ;; Original colliding dir was not touched.
                    (should (file-directory-p
                             (file-name-concat sessions-dir colliding)))))
              (test-mevedel-session-persistence--release-and-kill
               buf session))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(provide 'test-mevedel-session-persistence)

;;; test-mevedel-session-persistence.el ends here
