;;; test-mevedel-skills.el --- Tests for skills foundation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-skills)
(require 'mevedel-cockpit)
(require 'mevedel-hooks)
(require 'mevedel-plugins)
(require 'mevedel-models)
(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-permissions)
(require 'mevedel-compact)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-agents)
(require 'mevedel-presets)
(require 'mevedel-tools)
(require 'mevedel-worktree)
(require 'tabulated-list)
;; Phase 7: shell injection routes through Bash tool's permission
;; check (`mevedel-tools--check-bash-permission').
(require 'mevedel-tool-exec)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar mevedel-session-persistence)
(defvar mevedel-plugin-extra-roots)
(declare-function mevedel-session-persistence--sidecar-path
                  "mevedel-session-persistence" (save-path))
(declare-function mevedel-session-persistence-read
                  "mevedel-session-persistence" (file))


;;
;;; Helpers

(defmacro mevedel-skills-test--with-model-backends (&rest body)
  "Run BODY with an isolated pair of gptel model backends."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     ,@body))

(defun mevedel-skills-test--write-skill (dir name frontmatter &optional body)
  "Create DIR/NAME/SKILL.md with FRONTMATTER and optional BODY."
  (let* ((skill-dir (file-name-as-directory (file-name-concat dir name)))
         (skill-file (file-name-concat skill-dir "SKILL.md")))
    (make-directory skill-dir t)
    (with-temp-file skill-file
      (insert "---\n")
      (insert frontmatter)
      (unless (string-suffix-p "\n" frontmatter)
        (insert "\n"))
      (insert "---\n")
	    (when body
	      (insert body)))
    skill-file))

(defun mevedel-skills-test--write-plugin-manifest (user-dir repo json)
  "Create a test plugin manifest JSON for REPO under USER-DIR.
Return the plugin root directory."
  (let ((root (file-name-concat user-dir "plugins" repo)))
    (make-directory (file-name-concat root ".codex-plugin") t)
    (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
      (insert json))
    root))

(defun mevedel-skills-test--hook-fn (_event)
  "Test hook used by skill hook normalization."
  '(:additional-context "skill hook ran"))

(defun mevedel-skills-test--expansion-fn (_event)
  "Test hook used by skill expansion."
  '(:updated-input "Expanded by hook"
    :additional-context "expansion context"))

(defun mevedel-skills-test--block-expansion-fn (_event)
  "Test hook that blocks skill expansion."
  '(:continue nil :stop-reason "blocked expansion"))

(defun mevedel-skills-test--make-workspace (root)
  "Return a minimal workspace struct rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "test"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))


;;
;;; Plist coercion helpers

(mevedel-deftest mevedel-skills--coerce-bool ()
  ,test
  (test)
  :doc "nil falls through to the default"
  (should (eq (mevedel-skills--coerce-bool nil t) t))
  (should (eq (mevedel-skills--coerce-bool nil nil) nil))

  :doc "explicit booleans and yaml false sentinel"
  (should (eq (mevedel-skills--coerce-bool t nil) t))
  (should (eq (mevedel-skills--coerce-bool :false t) nil))

  :doc "string coercion for yaml.el scalar output"
  (should (eq (mevedel-skills--coerce-bool "true" nil) t))
  (should (eq (mevedel-skills--coerce-bool "false" t) nil))
  (should (eq (mevedel-skills--coerce-bool "no" t) nil))
  (should (eq (mevedel-skills--coerce-bool "yes" nil) t)))

(mevedel-deftest mevedel-skills--coerce-list ()
  ,test
  (test)
  :doc "nil stays nil, lists pass through, scalars become single-element"
  (should (null (mevedel-skills--coerce-list nil)))
  (should (equal (mevedel-skills--coerce-list '("a" "b"))
                 '("a" "b")))
  (should (equal (mevedel-skills--coerce-list "solo")
                 '("solo"))))

(mevedel-deftest mevedel-skills--coerce-context ()
  ,test
  (test)
  :doc "fork variants resolve to `fork', everything else is inline"
  (should (eq (mevedel-skills--coerce-context "fork") 'fork))
  (should (eq (mevedel-skills--coerce-context 'fork) 'fork))
  (should (eq (mevedel-skills--coerce-context "inline") 'inline))
  (should (eq (mevedel-skills--coerce-context nil) 'inline)))


;;
;;; Discovery + parsing

(mevedel-deftest mevedel-skills-scan ()
  ,test
  (test)
  :doc "scans a single directory and builds a skill struct"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t))
         (_ (mevedel-skills-test--write-skill
             dir "simplify"
             "name: simplify
description: Review changed code for reuse
argument-hint: \"[path]\"
"
             "Body of the simplify skill."))
         (skills (mevedel-skills-scan dir '("." ))))
    (unwind-protect
        (progn
          (should (= 1 (length skills)))
          (let ((skill (car skills)))
            (should (equal "simplify" (mevedel-skill-name skill)))
            (should (equal "Review changed code for reuse"
                           (mevedel-skill-description skill)))
            (should (equal "[path]" (mevedel-skill-argument-hint skill)))
            (should (eq 'inline (mevedel-skill-context skill)))
            (should (eq 'project (mevedel-skill-source skill)))
            (should (mevedel-skill-user-invocable-p skill))
            (should (mevedel-skill-model-invocable-p skill))
            ;; Body is not loaded eagerly.
            (should-not (mevedel-skill-body skill))))
      (delete-directory dir t)))

  :doc "explicit legacy .claude skill directories are ignored"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-legacy-" t))
         (legacy-dir (file-name-concat root ".claude" "skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           legacy-dir "legacy"
           "name: legacy
description: old
" "Legacy body")
          (should-not (mevedel-skills-scan root '(".claude/skills/"))))
      (delete-directory root t)))

  :doc "project/user name conflicts are source-qualified"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-project-" t))
         (project-dir (file-name-concat root ".mevedel" "skills"))
         (user-dir (make-temp-file "mevedel-skills-user-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           user-dir "shared"
           "name: shared
description: From user
" "User body")
          (mevedel-skills-test--write-skill
           project-dir "shared"
           "name: shared
description: From project
" "Project body")
          (let* ((skills (mevedel-skills-scan
                          root (list user-dir ".mevedel/skills")))
                 (names (mapcar #'mevedel-skill-name skills))
                 (project (cl-find "local:shared" skills
                                   :key #'mevedel-skill-name
                                   :test #'equal))
                 (user (cl-find "global:shared" skills
                                :key #'mevedel-skill-name
                                :test #'equal)))
            (should (= 2 (length skills)))
            (should (member "local:shared" names))
            (should (member "global:shared" names))
            (should-not (member "shared" names))
            (should (equal "From project"
                           (mevedel-skill-description project)))
            (should (equal "From user"
                           (mevedel-skill-description user)))))
      (delete-directory root t)
      (delete-directory user-dir t)))

  :doc "skills missing a description fall back to the first body paragraph"
  ;; A missing description uses the first non-empty markdown
  ;; paragraph/header.  The skill loads with the body-derived description
  ;; rather than being skipped.
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "no-desc"
           "name: no-desc
" "Body line one.\n\nLater paragraph.")
          (mevedel-skills-test--write-skill
           dir "ok"
           "name: ok
description: present
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (names (mapcar #'mevedel-skill-name skills))
                 (no-desc (cl-find "no-desc" skills
                                   :key #'mevedel-skill-name :test #'equal)))
            (should (member "ok" names))
            (should (member "no-desc" names))
            (should (equal "Body line one."
                           (mevedel-skill-description no-desc)))))
      (delete-directory dir t)))

  :doc "relative dirs without a workspace root are skipped"
  (let* ((mevedel-skills-include-bundled nil)
         (skills (mevedel-skills-scan nil '(".mevedel/skills/"))))
    (should (null skills)))

  :doc "ccs-compatible fields populate their slots"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t))
         ;; allowed-tools entries now go through the parser and need
         ;; tool-registry hits; mock so the unrelated fields under
         ;; test still load.
         (fake-tools
          `(("Read" . ,(mevedel-tool--create
                        :name "Read" :handler #'ignore
                        :get-path (lambda (_) "")))
            ("Grep" . ,(mevedel-tool--create
                        :name "Grep" :handler #'ignore
                        :get-path (lambda (_) ""))))))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-tool-get)
                   (lambda (n &optional _c) (cdr (assoc n fake-tools)))))
          (mevedel-skills-test--write-skill
           dir "coordinator"
           "name: coordinator
description: Orchestrate workers
context: fork
agent: general-purpose
allowed-tools:
  - Read
  - Grep
disable-model-invocation: false
user-invocable: true
paths:
  - \"*.el\"
" "Coordinator body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (skill (car skills)))
            (should (eq 'fork (mevedel-skill-context skill)))
            (should (equal "general-purpose" (mevedel-skill-agent skill)))
            (should (equal '("Read" "Grep") (mevedel-skill-allowed-tools skill)))
            (should (equal '("*.el") (mevedel-skill-path-patterns skill)))
            ;; path-patterns => dormant until matched
            (should-not (mevedel-skill-active-p skill))
            (should (mevedel-skill-user-invocable-p skill))
            (should (mevedel-skill-model-invocable-p skill))))
      (delete-directory dir t)))

  :doc "bundled coordinator, remember, and git-worktree skills are discoverable by default"
  (mevedel-tool-clear-registry)
  (let* ((skills (mevedel-skills-scan nil nil))
         (worktree (cl-find "git-worktree" skills
                            :key #'mevedel-skill-name :test #'equal))
         (remember (cl-find "remember" skills
                            :key #'mevedel-skill-name :test #'equal))
         (body (and worktree (mevedel-skill-load-body worktree)))
         (remember-body (and remember (mevedel-skill-load-body remember))))
    (should (cl-find-if
             (lambda (s)
               (and (equal "coordinator" (mevedel-skill-name s))
                    (eq 'bundled (mevedel-skill-source s))))
             skills))
    (should remember)
    (should (eq 'bundled (mevedel-skill-source remember)))
    (should (mevedel-skill-user-invocable-p remember))
    (should (equal "[focus]" (mevedel-skill-argument-hint remember)))
    (should (string-match-p "each configured memory root" remember-body))
    (should (string-match-p "\\.agents/memory/" remember-body))
    (should-not (string-match-p "CLAUDE\\.md" remember-body))
    (should worktree)
    (should-not (cl-find "using-git-worktrees" skills
                         :key #'mevedel-skill-name :test #'equal))
    (should (eq 'bundled (mevedel-skill-source worktree)))
    (should (mevedel-skill-model-invocable-p worktree))
    (should-not (mevedel-skill-user-invocable-p worktree))
    (should (equal '("Bash(git rev-parse:*)"
                     "Bash(git status:*)"
                     "Bash(git check-ignore:*)"
                     "Bash(git worktree list:*)"
                     "Bash(printf:*)")
                   (mevedel-skill-allowed-tools worktree)))
    (should (member '("Bash" :pattern "git rev-parse:*" :action allow)
                    (mevedel-skill-allowed-tool-rules worktree)))
    (should (string-match-p "best-effort and read-only" body))
    (should (string-match-p "git worktree list --porcelain" body))
    (should (string-match-p "You cannot invoke slash commands yourself" body)))

  :doc "bundled skills are suppressed when include-bundled is nil"
  (let* ((mevedel-skills-include-bundled nil)
         (skills (mevedel-skills-scan nil nil)))
    (should-not (cl-find-if
                 (lambda (s) (eq 'bundled (mevedel-skill-source s)))
                 skills)))

  :doc "enabled plugin skills are discovered with plugin-prefixed names"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skill-dirs nil)
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-plugin-skills-" t)))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (workspace (mevedel-skills-test--make-workspace
                     (file-name-concat user-dir "workspace")))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Body")
          (mevedel-plugins-enable "demo" workspace)
          (let* ((skills (mevedel-skills-scan nil nil workspace))
                 (skill (cl-find "demo:from-plugin" skills
                                 :key #'mevedel-skill-name :test #'equal)))
            (should skill)
            (should (eq 'plugin (mevedel-skill-source skill)))
            (should (equal "Plugin skill"
                           (mevedel-skill-description skill)))
            (should-not (cl-find "from-plugin" skills
                                 :key #'mevedel-skill-name :test #'equal))))
      (delete-directory user-dir t)))

  :doc "disabled plugin skills are skipped"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skill-dirs nil)
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-plugin-disabled-" t)))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (workspace (mevedel-skills-test--make-workspace
                     (file-name-concat user-dir "workspace")))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           plugin-skills "disabled-skill"
           "name: disabled-skill
description: Disabled plugin skill
" "Body")
          (should-not
           (cl-find "demo:disabled-skill"
                    (mevedel-skills-scan nil nil workspace)
                    :key #'mevedel-skill-name :test #'equal)))
      (delete-directory user-dir t)))

  :doc "explicit scan dirs do not include installed plugin skills"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-plugin-explicit-dirs-" t))
         (configured-skills (file-name-concat root "skills"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           configured-skills "configured"
           "name: configured
description: Configured skill
" "Configured body")
          (mevedel-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Plugin body")
          (let ((skills (mevedel-skills-scan nil (list configured-skills))))
            (should (cl-find "configured" skills
                             :key #'mevedel-skill-name :test #'equal))
            (should-not (cl-find "from-plugin" skills
                                 :key #'mevedel-skill-name :test #'equal))))
      (delete-directory root t)))

  :doc "explicit source-tagged plugin dirs are scanned as plugin skills"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-plugin-source-tagged-" t))
         (plugin-skills (file-name-concat root "plugin-skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Plugin body")
          (let* ((skills (mevedel-skills-scan
                          nil
                          (list (cons plugin-skills '(plugin . "demo")))))
                 (skill (cl-find "demo:from-plugin" skills
                                 :key #'mevedel-skill-name :test #'equal)))
            (should skill)
            (should (eq 'plugin (mevedel-skill-source skill)))))
      (delete-directory root t)))

  :doc "plugin skills are namespaced so configured skills coexist"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-plugin-precedence-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (configured-skills (file-name-concat root "skills"))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-skill-dirs (list configured-skills))
         (workspace (mevedel-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           configured-skills "shared"
           "name: shared
description: Configured skill
" "Configured body")
          (mevedel-skills-test--write-skill
           plugin-skills "shared"
           "name: shared
description: Plugin skill
" "Plugin body")
          (mevedel-plugins-enable "demo" workspace)
          (let* ((skills (mevedel-skills-scan nil nil workspace))
                 (configured (cl-find "shared" skills
                                      :key #'mevedel-skill-name
                                      :test #'equal))
                 (plugin (cl-find "demo:shared" skills
                                  :key #'mevedel-skill-name
                                  :test #'equal)))
            (should configured)
            (should plugin)
            (should (eq 'user (mevedel-skill-source configured)))
            (should (eq 'plugin (mevedel-skill-source plugin)))
            (should (equal "Configured skill"
                           (mevedel-skill-description configured)))
            (should (equal "Plugin skill"
                           (mevedel-skill-description plugin)))))
      (delete-directory root t)))
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-plugin-project-precedence-" t))
         (workspace-root (file-name-concat root "workspace"))
         (project-skills (file-name-concat workspace-root ".mevedel/skills"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (workspace (mevedel-skills-test--make-workspace workspace-root))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           project-skills "shared"
           "name: shared
description: Project skill
" "Project body")
          (mevedel-skills-test--write-skill
           plugin-skills "shared"
           "name: shared
description: Plugin skill
" "Plugin body")
          (mevedel-plugins-enable "demo" workspace)
          (let* ((skills (mevedel-skills-scan
                          workspace-root nil workspace))
                 (project (cl-find "shared" skills
                                   :key #'mevedel-skill-name
                                   :test #'equal))
                 (plugin (cl-find "demo:shared" skills
                                  :key #'mevedel-skill-name
                                  :test #'equal)))
            (should project)
            (should plugin)
            (should (eq 'project (mevedel-skill-source project)))
            (should (eq 'plugin (mevedel-skill-source plugin)))
            (should (equal "Project skill"
                           (mevedel-skill-description project)))
            (should (equal "Plugin skill"
                           (mevedel-skill-description plugin)))))
      (delete-directory root t)))
  (let* ((root (make-temp-file "mevedel-plugin-bundled-precedence-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-skill-dirs nil)
         (workspace (mevedel-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           plugin-skills "coordinator"
           "name: coordinator
description: Plugin coordinator
" "Body")
          (mevedel-plugins-enable "demo" workspace)
          (let* ((skills (mevedel-skills-scan nil nil workspace))
                 (coordinator (cl-find "coordinator" skills
                                       :key #'mevedel-skill-name
                                       :test #'equal))
                 (plugin (cl-find "demo:coordinator" skills
                                  :key #'mevedel-skill-name
                                  :test #'equal)))
            (should coordinator)
            (should plugin)
            (should (eq 'bundled (mevedel-skill-source coordinator)))
            (should (eq 'plugin (mevedel-skill-source plugin)))))
      (delete-directory root t)))
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-plugin-same-skill-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-skill-dirs nil)
         (workspace (mevedel-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-a-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"alpha\",\"skills\":\"skills\"}"))
         (plugin-b-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"beta\",\"skills\":\"skills\"}")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           (file-name-concat plugin-a-root "skills")
           "shared"
           "name: shared
description: Alpha shared
" "Body")
          (mevedel-skills-test--write-skill
           (file-name-concat plugin-b-root "skills")
           "shared"
           "name: shared
description: Beta shared
" "Body")
          (mevedel-plugins-enable "alpha" workspace)
          (mevedel-plugins-enable "beta" workspace)
          (let ((names (mapcar #'mevedel-skill-name
                               (mevedel-skills-scan nil nil workspace))))
            (should (member "alpha:shared" names))
            (should (member "beta:shared" names))
            (should-not (member "shared" names))))
      (delete-directory root t)))

  :doc "frontmatter `name' wins; missing `name' falls back to directory"
  ;; Name resolution prefers frontmatter over directory.
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          ;; Frontmatter overrides directory.
          (mevedel-skills-test--write-skill
           dir "dir-name-a"
           "name: explicit-name
description: present
" "Body")
          ;; Directory used when frontmatter omits :name.
          (mevedel-skills-test--write-skill
           dir "dir-name-b"
           "description: present
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (names (mapcar #'mevedel-skill-name skills)))
            (should (member "explicit-name" names))
            (should (member "dir-name-b" names))
            (should-not (member "dir-name-a" names))))
      (delete-directory dir t)))

  :doc "skills with invalid names are skipped with a warning"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "Bad_Name"
           "description: invalid because of underscore + uppercase
" "Body")
          (mevedel-skills-test--write-skill
           dir "good-name"
           "description: ok
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (names (mapcar #'mevedel-skill-name skills)))
            (should (member "good-name" names))
            (should-not (member "Bad_Name" names))))
      (delete-directory dir t)))

  :doc "arguments frontmatter parsed with numeric-only filtered"
  ;; Numeric-only names cannot shadow $0/$1.
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "args"
           "description: argument names
arguments:
  - foo
  - bar
  - \"1\"
" "Body")
          (mevedel-skills-test--write-skill
           dir "args-string"
           "description: space-separated string
arguments: alpha beta 2 gamma
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (list-skill (cl-find "args" skills
                                      :key #'mevedel-skill-name :test #'equal))
                 (str-skill (cl-find "args-string" skills
                                     :key #'mevedel-skill-name :test #'equal)))
            (should (equal '("foo" "bar")
                           (mevedel-skill-argument-names list-skill)))
            (should (equal '("alpha" "beta" "gamma")
                           (mevedel-skill-argument-names str-skill)))))
      (delete-directory dir t)))

  :doc "effort validates against allowed values"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "good-effort"
           "description: ok
effort: high
" "Body")
          (mevedel-skills-test--write-skill
           dir "bad-effort"
           "description: ok
effort: ludicrous
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (good (cl-find "good-effort" skills
                                :key #'mevedel-skill-name :test #'equal))
                 (bad (cl-find "bad-effort" skills
                               :key #'mevedel-skill-name :test #'equal)))
            (should (eq 'high (mevedel-skill-effort good)))
            ;; Invalid value is dropped, skill loads.
            (should (null (mevedel-skill-effort bad)))))
      (delete-directory dir t)))

  :doc "shell defaults to bash, accepts powershell, warns on unknown"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "default-shell"
           "description: ok
" "Body")
          (mevedel-skills-test--write-skill
           dir "ps-shell"
           "description: ok
shell: powershell
" "Body")
          (mevedel-skills-test--write-skill
           dir "weird-shell"
           "description: ok
shell: fish
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (default (cl-find "default-shell" skills
                                   :key #'mevedel-skill-name :test #'equal))
                 (ps (cl-find "ps-shell" skills
                              :key #'mevedel-skill-name :test #'equal))
                 (weird (cl-find "weird-shell" skills
                                 :key #'mevedel-skill-name :test #'equal)))
            (should (eq 'bash (mevedel-skill-shell default)))
            (should (eq 'powershell (mevedel-skill-shell ps)))
            ;; Unknown shell falls back to bash.
            (should (eq 'bash (mevedel-skill-shell weird)))))
      (delete-directory dir t)))

  :doc "when_to_use wins over when-to-use on conflict"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "wtu-snake"
           "description: ok
when_to_use: snake form
" "Body")
          (mevedel-skills-test--write-skill
           dir "wtu-dash"
           "description: ok
when-to-use: dash form
" "Body")
          (mevedel-skills-test--write-skill
           dir "wtu-conflict"
           "description: ok
when_to_use: snake wins
when-to-use: dash loses
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (snake (cl-find "wtu-snake" skills
                                 :key #'mevedel-skill-name :test #'equal))
                 (dash (cl-find "wtu-dash" skills
                                :key #'mevedel-skill-name :test #'equal))
                 (conflict (cl-find "wtu-conflict" skills
                                    :key #'mevedel-skill-name :test #'equal)))
            (should (equal "snake form" (mevedel-skill-when-to-use snake)))
            (should (equal "dash form" (mevedel-skill-when-to-use dash)))
            (should (equal "snake wins"
                           (mevedel-skill-when-to-use conflict)))))
      (delete-directory dir t)))

  :doc "invalid YAML in frontmatter logs a warning and skips the skill"
  ;; Invalid YAML skips the skill and logs a warning.
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t))
         (warnings nil))
    (unwind-protect
        (progn
          ;; Write a SKILL.md with malformed YAML (unbalanced quote).
          (let ((skill-dir (file-name-as-directory
                            (file-name-concat dir "broken"))))
            (make-directory skill-dir)
            (with-temp-file (file-name-concat skill-dir "SKILL.md")
              (insert "---\nname: broken\ndescription: \"unterminated\n---\nBody")))
          (mevedel-skills-test--write-skill
           dir "intact"
           "name: intact
description: present
" "Body")
          (cl-letf* ((display-warning-orig (symbol-function 'display-warning))
                     ((symbol-function 'display-warning)
                      (lambda (type message &rest args)
                        (when (eq type 'mevedel)
                          (push message warnings))
                        (apply display-warning-orig type message args))))
            (let* ((skills (mevedel-skills-scan dir '(".")))
                   (names (mapcar #'mevedel-skill-name skills)))
              (should (member "intact" names))
              (should-not (member "broken" names))
              (should (cl-some (lambda (m)
                                 (and (string-match-p "broken/SKILL.md" m)
                                      (string-match-p "invalid YAML" m)))
                               warnings)))))
      (delete-directory dir t)))

  :doc "allowed-tools strings are parsed into allowed-tool-rules at scan"
  ;; --from-plist runs each `allowed-tools' string through the parser.
  ;; Malformed entries warn-and-skip; valid entries become rules.
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t))
         (warnings nil)
         ;; Use the same fake-tool-get pattern as the parser tests so
         ;; we don't depend on tool-registry registration order.
         (fake-tools
          `(("Read"     . ,(mevedel-tool--create
                            :name "Read" :handler #'ignore
                            :get-path (lambda (_) "")))
            ("Bash"     . ,(mevedel-tool--create
                            :name "Bash" :handler #'ignore
                            :get-pattern (lambda (_) ""))))))
    (unwind-protect
        (cl-letf* ((display-warning-orig (symbol-function 'display-warning))
                   ((symbol-function 'mevedel-tool-get)
                    (lambda (n &optional _c) (cdr (assoc n fake-tools))))
                   ((symbol-function 'display-warning)
                    (lambda (type message &rest args)
                      (when (eq type 'mevedel) (push message warnings))
                      (apply display-warning-orig type message args))))
          (mevedel-skills-test--write-skill
           dir "ok-rules"
           "name: ok-rules
description: ok
allowed-tools:
  - Read
  - Bash(git status)
" "Body")
          (mevedel-skills-test--write-skill
           dir "with-bad-entry"
           "name: with-bad-entry
description: ok
allowed-tools:
  - Read
  - bash(rm)
  - Bash(echo *)
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (ok (cl-find "ok-rules" skills
                              :key #'mevedel-skill-name :test #'equal))
                 (bad (cl-find "with-bad-entry" skills
                               :key #'mevedel-skill-name :test #'equal)))
            ;; ok-rules: both entries parsed.
            (should (equal '(("Read" :action allow)
                             ("Bash" :pattern "git status" :action allow))
                           (mevedel-skill-allowed-tool-rules ok)))
            ;; A malformed allowed-tools entry skips the whole skill
            ;; rather than dropping individual entries.
            (should (null bad))
            (should (cl-some (lambda (m)
                               (and (string-match-p "with-bad-entry" m)
                                    (string-match-p "bash(rm)" m)))
	                             warnings))))
      (delete-directory dir t)))

  :doc "hooks frontmatter is normalized and usable as request hooks"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "hooked"
           "name: hooked
description: ok
hooks:
  PreToolUse:
    - matcher: Bash
      hooks:
        - type: elisp
          function: mevedel-skills-test--hook-fn
" "Body")
          (let* ((skill (car (mevedel-skills-scan dir '("."))))
                 (hooks (mevedel-skill-hooks skill))
                 (workspace (mevedel-skills-test--make-workspace dir))
                 (session (mevedel-session-create "main" workspace))
                 (request (mevedel-request--create
                           :session session
                           :hook-rules hooks))
                 decision)
            (should (eq 'PreToolUse (caar hooks)))
            (mevedel-hooks-run-event
             'PreToolUse
             (mevedel-hooks-event-plist
              'PreToolUse session workspace
              :tool-name "Bash")
             (lambda (d) (setq decision d))
             session workspace request nil)
            (while (null decision)
              (accept-process-output nil 0.01))
            (should (equal '("skill hook ran")
                           (plist-get decision :additional-context)))))
	  (delete-directory dir t)))

  :doc "fork skill Stop hooks normalize to SubagentStop"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "fork-hooked"
           "name: fork-hooked
description: ok
context: fork
agent: explorer
hooks:
  Stop:
    - matcher: explorer
      hooks:
        - type: elisp
          function: mevedel-skills-test--hook-fn
" "Body")
          (let* ((skill (car (mevedel-skills-scan dir '("."))))
                 (hooks (mevedel-skill-hooks skill)))
            (should (eq 'SubagentStop (caar hooks)))))
      (delete-directory dir t)))

  :doc "display-name defaults to name when frontmatter omits it"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "no-display"
           "description: ok
" "Body")
          (mevedel-skills-test--write-skill
           dir "with-display"
           "description: ok
display-name: Friendly Label
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (no-d (cl-find "no-display" skills
                                :key #'mevedel-skill-name :test #'equal))
                 (with-d (cl-find "with-display" skills
                                  :key #'mevedel-skill-name :test #'equal)))
            (should (equal "no-display" (mevedel-skill-display-name no-d)))
            (should (equal "Friendly Label"
                           (mevedel-skill-display-name with-d)))))
      (delete-directory dir t))))


(mevedel-deftest mevedel-skills--qualify-conflicting-names ()
  ,test
  (test)
  :doc "unique and already-qualified plugin names are preserved"
  (let* ((plugin (mevedel-skill--create
                  :name "superpowers:brainstorming"
                  :source 'plugin))
         (user (mevedel-skill--create
                :name "brainstorming"
                :source 'user))
         (solo (mevedel-skill--create
                :name "solo"
                :source 'project))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list plugin user solo))))
    (should (equal '("superpowers:brainstorming" "brainstorming" "solo")
                   (mapcar #'mevedel-skill-name skills))))

  :doc "same-source duplicates keep the first entry"
  (let* ((first (mevedel-skill--create
                 :name "shared" :description "first" :source 'user))
         (second (mevedel-skill--create
                  :name "shared" :description "second" :source 'user))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list first second))))
    (should (= 1 (length skills)))
    (should (equal "shared" (mevedel-skill-name (car skills))))
    (should (equal "first" (mevedel-skill-description (car skills)))))

  :doc "family-only prefixes are used when family disambiguates"
  (let* ((mevedel (mevedel-skill--create
                   :name "shared" :source 'project :source-family 'mevedel))
         (agents (mevedel-skill--create
                  :name "shared" :source 'project :source-family 'agents))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list mevedel agents))))
    (should (equal '("mevedel:shared" "agents:shared")
                   (mapcar #'mevedel-skill-name skills))))

  :doc "scope-only prefixes are used when scope disambiguates"
  (let* ((local (mevedel-skill--create
                 :name "shared" :source 'project :source-family 'mevedel))
         (global (mevedel-skill--create
                  :name "shared" :source 'user :source-family 'mevedel))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list local global))))
    (should (equal '("local:shared" "global:shared")
                   (mapcar #'mevedel-skill-name skills))))

  :doc "full scope-family prefixes are used for four-way conflicts"
  (let* ((local-mevedel
          (mevedel-skill--create
           :name "shared" :source 'project :source-family 'mevedel))
         (local-agents
          (mevedel-skill--create
           :name "shared" :source 'project :source-family 'agents))
         (global-mevedel
          (mevedel-skill--create
           :name "shared" :source 'user :source-family 'mevedel))
         (global-agents
          (mevedel-skill--create
           :name "shared" :source 'user :source-family 'agents))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list local-mevedel local-agents
                        global-mevedel global-agents))))
    (should (equal '("local-mevedel:shared" "local-agents:shared"
                     "global-mevedel:shared" "global-agents:shared")
                   (mapcar #'mevedel-skill-name skills))))

  :doc "bundled conflicts keep all skills with unique prefixes"
  (let* ((local (mevedel-skill--create
                 :name "review" :source 'project :source-family 'mevedel))
         (bundled (mevedel-skill--create
                   :name "review" :source 'bundled))
         (skills (mevedel-skills--qualify-conflicting-names
                  (list local bundled))))
    (should (equal '("mevedel:review" "bundled:review")
                   (mapcar #'mevedel-skill-name skills)))))


;;
;;; Phase 1 helpers (validators, parsers)

(mevedel-deftest mevedel-skills--valid-name-p ()
  ,test
  (test)
  :doc "lowercase letters, digits, and dashes pass"
  (should (mevedel-skills--valid-name-p "grill-me"))
  (should (mevedel-skills--valid-name-p "review-spec"))
  (should (mevedel-skills--valid-name-p "abc123"))
  (should (mevedel-skills--valid-name-p "x"))

  :doc "uppercase, underscores, dots, slashes, spaces fail"
  (should-not (mevedel-skills--valid-name-p "Bad"))
  (should-not (mevedel-skills--valid-name-p "bad_name"))
  (should-not (mevedel-skills--valid-name-p "bad.name"))
  (should-not (mevedel-skills--valid-name-p "bad/name"))
  (should-not (mevedel-skills--valid-name-p "bad name"))

  :doc "empty and over-length names fail"
  (should-not (mevedel-skills--valid-name-p ""))
  (should-not (mevedel-skills--valid-name-p (make-string 65 ?a)))
  (should (mevedel-skills--valid-name-p (make-string 64 ?a)))

  :doc "non-strings fail"
  (should-not (mevedel-skills--valid-name-p nil))
  (should-not (mevedel-skills--valid-name-p 'symbol)))

(mevedel-deftest mevedel-skills--parse-argument-names ()
  ,test
  (test)
  :doc "list input passes through, filtering numeric-only"
  (should (equal '("foo" "bar")
                 (mevedel-skills--parse-argument-names
                  '("foo" "bar"))))
  (should (equal '("foo" "bar")
                 (mevedel-skills--parse-argument-names
                  '("foo" "1" "bar" "2"))))

  :doc "string input splits on whitespace"
  (should (equal '("alpha" "beta")
                 (mevedel-skills--parse-argument-names "alpha beta")))
  (should (equal '("alpha" "beta")
                 (mevedel-skills--parse-argument-names "  alpha   beta  ")))

  :doc "string with numeric tokens drops them"
  (should (equal '("alpha" "gamma")
                 (mevedel-skills--parse-argument-names "alpha 2 gamma")))

  :doc "nil and empty inputs return nil"
  (should (null (mevedel-skills--parse-argument-names nil)))
  (should (null (mevedel-skills--parse-argument-names "")))
  (should (null (mevedel-skills--parse-argument-names '()))))

(mevedel-deftest mevedel-skills--first-paragraph ()
  ,test
  (test)
  :doc "first paragraph extracted from plain text body"
  (should (equal "Line one of paragraph one.\nLine two of paragraph one."
                 (mevedel-skills--first-paragraph
                  "Line one of paragraph one.
Line two of paragraph one.

Paragraph two should be ignored.")))

  :doc "leading blank lines are skipped"
  (should (equal "First content."
                 (mevedel-skills--first-paragraph
                  "

First content.

More.")))

  :doc "leading # markers are stripped"
  (should (equal "Some Header"
                 (mevedel-skills--first-paragraph "# Some Header\n\nBody.")))
  (should (equal "Sub-section"
                 (mevedel-skills--first-paragraph "## Sub-section\n")))

  :doc "empty body returns nil"
  (should (null (mevedel-skills--first-paragraph "")))
  (should (null (mevedel-skills--first-paragraph "   \n   \n")))
  (should (null (mevedel-skills--first-paragraph nil))))


;;
;;; Lazy body loading

(mevedel-deftest mevedel-skill-load-body ()
  ,test
  (test)
  :doc "returns the markdown body minus frontmatter"
  (let ((dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (let* ((_ (mevedel-skills-test--write-skill
                   dir "simplify"
                   "name: simplify
description: Review changed code
"
                   "Step 1: read.\nStep 2: simplify.\n"))
               (skill (car (mevedel-skills-scan dir '("."))))
               (body (mevedel-skill-load-body skill)))
          (should (stringp body))
          (should (string-match-p "Step 1: read." body))
          ;; Cached on the struct.
          (should (equal body (mevedel-skill-body skill)))
          ;; Second call returns the cached value without re-reading.
          (should (eq body (mevedel-skill-load-body skill))))
      (delete-directory dir t))))


;;
;;; Configuration

(mevedel-deftest mevedel-skill-dirs ()
  ,test
  (test)
  :doc "defaults prefer local mevedel/agents dirs before global dirs"
  (should (equal '(".mevedel/skills/"
                   ".agents/skills/"
                   "~/.mevedel/skills/"
                   "~/.agents/skills/")
                 mevedel-skill-dirs)))


;;
;;; Session installation

(mevedel-deftest mevedel-skills-install ()
  ,test
  (test)
  :doc "skills scanned from the workspace root end up on the session"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-ws-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (skill-root (file-name-concat root ".mevedel/skills/")))
    (unwind-protect
        (let ((mevedel-skill-dirs '(".mevedel/skills/")))
          (mevedel-skills-test--write-skill
           skill-root "grill-me"
           "name: grill-me
description: Interview relentlessly about a plan
" "Body")
          (mevedel-skills-install session)
          (let ((skills (mevedel-session-skills session)))
            (should (= 1 (length skills)))
            (should (equal "grill-me"
                           (mevedel-skill-name (car skills))))
            (should (eq 'project
                        (mevedel-skill-source (car skills)))))
          (should (equal "grill-me"
                         (mevedel-skill-name
                          (mevedel-session-get-skill session "grill-me"))))
          (should-not (mevedel-session-get-skill session "missing")))
      (delete-directory root t)))

  :doc "registers the consumer buffer and clears any prior dirty flag"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-install-reg-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-install-reg*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--reset-watchers)
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: Alpha\n")
          (puthash buf t mevedel-skills--dirty-buffers)
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should (= 1 (length (mevedel-session-skills session))))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name root))
                                 mevedel-skills--dir-buffers))))
      (mevedel-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "registers enabled plugin skill roots for hot reload"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-plugin-watch-root-" t))
         (workspace-root (file-name-concat root "workspace"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-skill-dirs nil)
         (plugin-root
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-as-directory
                         (file-name-concat plugin-root "skills")))
         (ws (mevedel-skills-test--make-workspace workspace-root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-plugin-watch-root*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--reset-watchers)
          (make-directory plugin-skills t)
          (mevedel-plugins-enable "demo" ws)
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should (null (mevedel-session-skills session)))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name plugin-skills))
                                 mevedel-skills--dir-buffers))))
      (mevedel-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "rescans preserve active path-scoped skills"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-install-active-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-install-active*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha"
           "name: alpha
description: Alpha
paths:
  - \"*.el\"
")
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (let ((skill (mevedel-session-get-skill session "alpha")))
            (should skill)
            (should-not (mevedel-skill-active-p skill)))
          (mevedel-skills--maybe-activate session "src/foo.el")
          (should (mevedel-skill-active-p
                   (mevedel-session-get-skill session "alpha")))
          (mevedel-skills-test--write-skill
           root "alpha"
           "name: alpha
description: Alpha updated
paths:
  - \"*.el\"
")
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (let ((skill (mevedel-session-get-skill session "alpha")))
            (should (equal "Alpha updated"
                           (mevedel-skill-description skill)))
            (should (mevedel-skill-active-p skill))))
      (mevedel-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t))))


;;
;;; Phase B — substitution, shell injection, execution


;;
;;; Request-scoped skill context

(mevedel-deftest mevedel-skills--drain-pending-context ()
  ,test
  (test)
  :doc "drain populates request slots from buffer-local stash"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "d" :root "/tmp/d" :name "d"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (records (list (mevedel-skill-invocation-record--create
                         :name "demo" :args "x" :trigger 'user-slash
                         :turn 1 :source-path "/tmp/demo/SKILL.md"
                         :prepared-body "Hello"))))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context
                  (list :permission-rules rules
                        :model (mevedel-model-tier-selector 'fast)
                        :effort 'high
                        :invoked-skills records))
      (mevedel-skills--drain-pending-context request)
      (should (equal rules
                     (mevedel-request-skill-permission-rules request)))
      (should (equal (mevedel-model-tier-selector 'fast)
                     (mevedel-request-skill-model-override request)))
      (should (eq 'high  (mevedel-request-skill-effort-override request)))
      (should (equal records (mevedel-session-invoked-skills session)))
      ;; Stash is cleared after drain.
      (should (null mevedel-skills--pending-request-context))))

  :doc "drain is a no-op when no stash present"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "d" :root "/tmp/d" :name "d"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      ;; No stash.
      (mevedel-skills--drain-pending-context request)
      (should (null (mevedel-request-skill-permission-rules request)))
      (should (null (mevedel-request-skill-model-override request))))))

(mevedel-deftest mevedel-skills--current-model-override ()
  ,test
  (test)
  :doc "returns request override when invocation has none"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "m" :root "/tmp/m" :name "m"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-model-override
                   (mevedel-model-tier-selector 'fast))))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (should (equal (mevedel-model-tier-selector 'fast)
                     (mevedel-skills--current-model-override)))))

  :doc "invocation override wins over request override (innermost)"
  (let* ((agent (mevedel-agent--create :name "tester"))
         (invocation
          (mevedel-agent-invocation--create
           :agent agent
           :skill-model-override (mevedel-model-tier-selector 'strong)))
         (request (mevedel-request--create
                   :skill-model-override
                   (mevedel-model-tier-selector 'fast))))
    (with-temp-buffer
      (setq-local mevedel--current-request request)
      (setq-local mevedel--agent-invocation invocation)
      (should (equal (mevedel-model-tier-selector 'strong)
                     (mevedel-skills--current-model-override)))))

  :doc "no override anywhere returns nil"
  (with-temp-buffer
    (should (null (mevedel-skills--current-model-override)))))

(mevedel-deftest mevedel-skills--pre-realize-model-override ()
  ,test
  (test)
  :doc "returns current request override before pending slash stash"
  (let ((request (mevedel-request--create
                  :skill-model-override
                  (mevedel-model-tier-selector 'strong))))
    (with-temp-buffer
      (setq-local mevedel--current-request request)
      (setq-local mevedel-skills--pending-request-context
                  (list :model (mevedel-model-tier-selector 'fast)))
      (should (equal (mevedel-model-tier-selector 'strong)
                     (mevedel-skills--pre-realize-model-override)))))

  :doc "returns pending slash stash when no current override exists"
  (with-temp-buffer
    (setq-local mevedel-skills--pending-request-context
                (list :model (mevedel-model-tier-selector 'fast)))
    (should (equal (mevedel-model-tier-selector 'fast)
                   (mevedel-skills--pre-realize-model-override)))))

(mevedel-deftest mevedel-skills--transform-apply-model-override ()
  ,test
  (test)
  :doc "pending slash tier sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
    (let ((mevedel-model-tiers
           '((fast . "Fast:fast-model")
             (balanced . "Balanced:balanced-model")
             (strong . nil)))
          (chat (generate-new-buffer " *skill-model-chat*")))
      (unwind-protect
          (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
            (with-current-buffer chat
              (setq-local mevedel-skills--pending-request-context
                          (list :model (mevedel-model-tier-selector 'fast))))
            (with-temp-buffer
              (setq-local gptel-backend (gptel-get-backend "Balanced"))
              (setq-local gptel-model 'balanced-model)
              (mevedel-skills--transform-apply-model-override fsm)
              (should (equal "Fast" (gptel-backend-name gptel-backend)))
              (should (eq 'fast-model gptel-model))))
        (kill-buffer chat))))

  :doc "pending concrete provider sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
    (let ((chat (generate-new-buffer " *skill-model-chat*")))
      (unwind-protect
          (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
            (with-current-buffer chat
              (setq-local mevedel-skills--pending-request-context
                          (list :model
                                (mevedel-model-resolve-provider
                                 "Balanced:balanced-model"))))
            (with-temp-buffer
              (setq-local gptel-backend (gptel-get-backend "Fast"))
              (setq-local gptel-model 'fast-model)
              (mevedel-skills--transform-apply-model-override fsm)
              (should (equal "Balanced" (gptel-backend-name gptel-backend)))
              (should (eq 'balanced-model gptel-model))))
        (kill-buffer chat))))

  :doc "active request override sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
    (let ((chat (generate-new-buffer " *skill-model-chat*")))
      (unwind-protect
          (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
            (with-current-buffer chat
              (setq-local mevedel--current-request
                          (mevedel-request--create
                           :skill-model-override
                           (mevedel-model-resolve-provider
                            "Fast:fast-model"))))
            (with-temp-buffer
              (setq-local gptel-backend (gptel-get-backend "Balanced"))
              (setq-local gptel-model 'balanced-model)
              (mevedel-skills--transform-apply-model-override fsm)
              (should (equal "Fast" (gptel-backend-name gptel-backend)))
              (should (eq 'fast-model gptel-model))))
        (kill-buffer chat)))))


;;
;;; Phase 2 helpers

(mevedel-deftest mevedel-skills--parse-arguments ()
  ,test
  (test)
  :doc "shell-style splitting respects double quotes"
  (should (equal '("foo" "bar baz" "qux")
                 (mevedel-skills--parse-arguments
                  "foo \"bar baz\" qux")))

  :doc "single quotes are not part of Emacs' split-string-and-unquote"
  ;; Emacs' shell-quote splitter only honors double quotes and
  ;; backslash escapes (cf. `combine-and-quote-strings').  Single
  ;; quotes pass through as literal characters; this is acceptable for
  ;; mevedel because skill authors writing portable bodies should use
  ;; double quotes anyway.
  (should (equal '("foo" "'bar" "baz'")
                 (mevedel-skills--parse-arguments "foo 'bar baz'")))

  :doc "unbalanced quotes fall back to whitespace splitting"
  (should (equal '("foo" "\"bar")
                 (mevedel-skills--parse-arguments "foo \"bar")))

  :doc "nil and blank inputs return nil"
  (should (null (mevedel-skills--parse-arguments nil)))
  (should (null (mevedel-skills--parse-arguments "")))
  (should (null (mevedel-skills--parse-arguments "   "))))

(mevedel-deftest mevedel-skills--substitute-vars ()
  ,test
  (test)
  :doc "$ARGUMENTS substitutes the full raw argument string"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "simplify"
                 :source-file "/tmp/simplify/SKILL.md"
                 :source-dir "/tmp/simplify/")))
    (should (equal "args=foo bar baz"
                   (mevedel-skills--substitute-vars
                    "args=$ARGUMENTS" "foo bar baz" session skill))))

  :doc "$0/$1/etc are zero-based"
  ;; No one-based compatibility: $1 means the second token.
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "first=foo second=bar"
                   (mevedel-skills--substitute-vars
                    "first=$0 second=$1" "foo bar" nil skill)))
    ;; Indexed access is also zero-based.
    (should (equal "indexed=baz"
                   (mevedel-skills--substitute-vars
                    "indexed=$ARGUMENTS[2]" "foo bar baz" nil skill))))

  :doc "${CLAUDE_SESSION_ID} and ${CLAUDE_SKILL_DIR} substitute"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "x"
                 :source-dir "/tmp/x/")))
    (should (equal "session=main dir=/tmp/x/"
                   (mevedel-skills--substitute-vars
                    "session=${CLAUDE_SESSION_ID} dir=${CLAUDE_SKILL_DIR}"
                    "" session skill))))

  :doc "${CLAUDE_SESSION_ID} prefers stable session id over session name"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "x")))
    (setf (mevedel-session-session-id session) "main-2026-05-17-abc")
    (should (equal "session=main-2026-05-17-abc"
                   (mevedel-skills--substitute-vars
                    "session=${CLAUDE_SESSION_ID}" "" session skill))))

  :doc "${CLAUDE_EFFORT} substitutes skill effort"
  (let ((skill (mevedel-skill--create :name "x" :effort 'xhigh)))
    (should (equal "effort=xhigh"
                   (mevedel-skills--substitute-vars
                    "effort=${CLAUDE_EFFORT}" "" nil skill))))

  :doc "${MEVEDEL_*} aliases mirror Claude-compatible substitutions"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "x"
                 :source-dir "/tmp/x/"
                 :effort 'high)))
    (setf (mevedel-session-session-id session) "stable-id")
    (should (equal "session=stable-id dir=/tmp/x/ effort=high"
                   (mevedel-skills--substitute-vars
                    "session=${MEVEDEL_SESSION_ID} dir=${MEVEDEL_SKILL_DIR} effort=${MEVEDEL_EFFORT}"
                    "" session skill))))

  :doc "nil session and skill expand literal substitutions to empty strings"
  (should (equal "session= dir= effort= alias="
                 (mevedel-skills--substitute-vars
                  "session=${CLAUDE_SESSION_ID} dir=${CLAUDE_SKILL_DIR} effort=${CLAUDE_EFFORT} alias=${MEVEDEL_EFFORT}"
                  "" nil nil)))

  :doc "literal substitutions do not rewrite user-supplied arguments"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "x"
                 :argument-names '("name")
                 :source-dir "/tmp/x/"
                 :effort 'high)))
    (setf (mevedel-session-session-id session) "stable-id")
    (should (equal "args=${MEVEDEL_SESSION_ID}"
                   (substring-no-properties
                    (mevedel-skills--substitute-vars
                     "args=$ARGUMENTS" "${MEVEDEL_SESSION_ID}"
                     session skill))))
    (should (equal "first=${MEVEDEL_SKILL_DIR}"
                   (substring-no-properties
                    (mevedel-skills--substitute-vars
                     "first=$0" "${MEVEDEL_SKILL_DIR}"
                     session skill))))
    (should (equal "name=${CLAUDE_EFFORT}"
                   (substring-no-properties
                    (mevedel-skills--substitute-vars
                     "name=$name" "${CLAUDE_EFFORT}"
                     session skill)))))

  :doc "escaped placeholders stay literal and do not suppress append-fallback"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("topic"))))
    (should (equal "full=$ARGUMENTS idx=$ARGUMENTS[0] pos=$0 named=$topic

ARGUMENTS: foo bar"
                   (substring-no-properties
                    (mevedel-skills--substitute-vars
                     "full=\\$ARGUMENTS idx=\\$ARGUMENTS[0] pos=\\$0 named=\\$topic"
                     "foo bar" nil skill)))))

  :doc "escaped literal variables stay literal and do not suppress append-fallback"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "x"
                 :source-dir "/tmp/x/")))
    (should (equal "dir=${CLAUDE_SKILL_DIR} session=${MEVEDEL_SESSION_ID}

ARGUMENTS: hello"
                   (substring-no-properties
                    (mevedel-skills--substitute-vars
                     "dir=\\${CLAUDE_SKILL_DIR} session=\\${MEVEDEL_SESSION_ID}"
                     "hello" session skill)))))

  :doc "out-of-range positional args become empty"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "a=foo b="
                   (mevedel-skills--substitute-vars
                    "a=$0 b=$1" "foo" nil skill))))

  :doc "nil argument string does not error"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "args="
                   (mevedel-skills--substitute-vars
                    "args=$ARGUMENTS" nil nil skill))))

  :doc "named arguments substitute by argument-names index"
  ;; ARGUMENT-NAMES[i] maps to PARSED-ARGS[i].
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("path" "depth"))))
    (should (equal "Visit src/foo at level 3"
                   (mevedel-skills--substitute-vars
                    "Visit $path at level $depth" "src/foo 3" nil skill))))

  :doc "named arguments do not match longer identifiers or indexed access"
  ;; ccs regex `\\=$NAME(?![\\=[\\=w])': $foo skips $foobar and $foo[0].
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("foo"))))
    (should (equal "got=hi keep=$foobar idx=$foo[0]"
                   (mevedel-skills--substitute-vars
                    "got=$foo keep=$foobar idx=$foo[0]"
                    "hi" nil skill))))

  :doc "shell-style parsing keeps quoted arguments together"
  ;; Quoted strings stay together, even with whitespace inside.
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("title"))))
    (should (equal "title is hello world"
                   (mevedel-skills--substitute-vars
                    "title is $title" "\"hello world\"" nil skill))))

  :doc "ARGUMENTS: appended when args supplied but no placeholder substituted"
  ;; Append only when no placeholder matched and raw args are non-empty.
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "no placeholders here\n\nARGUMENTS: foo bar"
                   (mevedel-skills--substitute-vars
                    "no placeholders here" "foo bar" nil skill)))
    ;; Body contains $ARGUMENTS → no append even if args are present.
    (should (equal "x=foo bar"
                   (mevedel-skills--substitute-vars
                    "x=$ARGUMENTS" "foo bar" nil skill)))
    ;; Empty/nil args → no append.
    (should (equal "no placeholders here"
                   (mevedel-skills--substitute-vars
                    "no placeholders here" "" nil skill)))
    (should (equal "no placeholders here"
                   (mevedel-skills--substitute-vars
                    "no placeholders here" nil nil skill))))

  :doc "${CLAUDE_*} and ${MEVEDEL_*} substitutions do not trigger append-fallback"
  ;; Literal variable substitutions run AFTER the placeholder check so they
  ;; don't suppress the append.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "x" :effort 'low)))
    (should (equal "id=main effort=low\n\nARGUMENTS: hello"
                   (mevedel-skills--substitute-vars
                    "id=${CLAUDE_SESSION_ID} effort=${MEVEDEL_EFFORT}"
                    "hello" session skill)))))

(defmacro mevedel-skills-test--with-bash-allowed (&rest body)
  "Run BODY with the Bash permission check forced to allow.
Tests need a deterministic permit so they can assert on the
substituted output without depending on the user's defcustom
configuration."
  `(cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
              (lambda (_command &rest _args) 'allow)))
     ,@body))

(defmacro mevedel-skills-test--with-eval-allowed (&rest body)
  "Run BODY with a deterministic trusted Eval allow rule."
  `(let ((mevedel-permission-rules '(("Eval" :action allow))))
     ,@body))

(defun mevedel-skills-test--shell-injections-sync (text)
  "Drive `mevedel-skills--run-body-injections-async' with TEXT synchronously.
Return the outcome plist produced by the async helper."
  (let (outcome)
    (mevedel-skills--run-body-injections-async
     text (lambda (o) (setq outcome o)))
    (while (null outcome)
      (accept-process-output nil 0.01))
    outcome))

(mevedel-deftest mevedel-skills--run-body-injections-async/shell ()
  ,test
  (test)
  :doc "inline !`cmd` is replaced with stdout"
  (mevedel-skills-test--with-bash-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "value=!`echo hello`")))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=hello" (plist-get outcome :body)))))

  :doc "multiple inline injections in the same line"
  (mevedel-skills-test--with-bash-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "a=!`echo 1` b=!`echo 2`")))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "a=1 b=2" (plist-get outcome :body)))))

  :doc "fenced ```! block is replaced with stdout"
  (mevedel-skills-test--with-bash-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "prefix\n```!\necho line1\necho line2\n```\nsuffix")))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\nline1\nline2\nsuffix"
                     (plist-get outcome :body)))))

  :doc "inline shell examples in Markdown code spans are left literal"
  (let ((outcome (mevedel-skills-test--shell-injections-sync
                  "Use `` !`cmd` `` to document shell injection.")))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Use `` !`cmd` `` to document shell injection."
                   (plist-get outcome :body))))

  :doc "ordinary Markdown fences can document inline shell syntax"
  (let ((outcome (mevedel-skills-test--shell-injections-sync
                  "Example:\n```md\n!`cmd`\n```\nDone")))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Example:\n```md\n!`cmd`\n```\nDone"
                   (plist-get outcome :body))))

  :doc "ordinary Markdown fences can document fenced shell syntax"
  (let ((outcome (mevedel-skills-test--shell-injections-sync
                  "Example:\n````md\n```!\necho nope\n```\n````\nDone")))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Example:\n````md\n```!\necho nope\n```\n````\nDone"
                   (plist-get outcome :body))))

  :doc "non-zero exit yields :status error :reason shell-failure"
  (mevedel-skills-test--with-bash-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync "!`false`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'shell-failure (plist-get outcome :reason)))))

  :doc "permission deny yields :status error :reason permission-denied"
  (cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
             (lambda (_c &rest _) 'deny)))
    (let ((outcome (mevedel-skills-test--shell-injections-sync "!`anything`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'permission-denied (plist-get outcome :reason)))))

  :doc "permission ask yields :status error :reason permission-denied"
  (cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
             (lambda (_c &rest _) 'ask)))
    (let ((outcome (mevedel-skills-test--shell-injections-sync "!`anything`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'permission-denied (plist-get outcome :reason))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/elisp ()
  ,test
  (test)
  :doc "inline !el`expr` is replaced with the printed return value"
  (mevedel-skills-test--with-eval-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "value=!el`(+ 1 2)`")))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=3" (plist-get outcome :body)))))

  :doc "fenced ```!el block supports multiline expressions"
  (mevedel-skills-test--with-eval-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "prefix\n```!el\n(progn\n  (princ \"seen\")\n  (+ 2 3))\n```\nsuffix")))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\n5\n\nSTDOUT:\nseen\nsuffix"
                     (plist-get outcome :body)))))

  :doc "inline elisp examples in Markdown code spans are left literal"
  (let ((outcome (mevedel-skills-test--shell-injections-sync
                  "Use `` !el`(+ 1 2)` `` to document elisp injection.")))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Use `` !el`(+ 1 2)` `` to document elisp injection."
                   (plist-get outcome :body))))

  :doc "ordinary Markdown fences can document fenced elisp syntax"
  (let ((outcome (mevedel-skills-test--shell-injections-sync
                  "Example:\n````md\n```!el\n(+ 1 2)\n```\n````\nDone")))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Example:\n````md\n```!el\n(+ 1 2)\n```\n````\nDone"
                   (plist-get outcome :body))))

  :doc "mixed shell and elisp markers execute in source order"
  (mevedel-skills-test--with-bash-allowed
    (mevedel-skills-test--with-eval-allowed
      (let ((outcome (mevedel-skills-test--shell-injections-sync
                      "a=!el`(concat \"x\" \"y\")` b=!`echo z`")))
        (should (eq 'ok (plist-get outcome :status)))
        (should (equal "a=\"xy\" b=z" (plist-get outcome :body))))))

  :doc "Eval errors abort skill preparation"
  (mevedel-skills-test--with-eval-allowed
    (let ((outcome (mevedel-skills-test--shell-injections-sync
                    "!el`(error \"boom\")`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'elisp-failure (plist-get outcome :reason)))))

  :doc "missing Eval allow denies without prompting"
  (let ((mevedel-permission-rules nil)
        enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (let ((outcome (mevedel-skills-test--shell-injections-sync
                      "!el`(+ 1 2)`")))
        (should (eq 'error (plist-get outcome :status)))
        (should (eq 'permission-denied (plist-get outcome :reason)))
        (should-not enqueued)))))

(mevedel-deftest mevedel-skills--run-body-injections-async/substitution-boundary ()
  ,test
  (test)
  :doc "caller-provided inline elisp markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=$ARGUMENTS" "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!el`(+ 1 2)`"
                     (plist-get outcome :body)))))

  :doc "caller-provided fenced elisp markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=$ARGUMENTS"
                  "```!el\n(+ 1 2)\n```"
                  nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=```!el\n(+ 1 2)\n```"
                     (plist-get outcome :body)))))

  :doc "caller-provided shell markers are not trusted"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=$ARGUMENTS" "!`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!`echo unsafe`"
                     (plist-get outcome :body)))))

  :doc "fallback-appended caller markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "body" "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "body\n\nARGUMENTS: !el`(+ 1 2)`"
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/partial-marker-boundary ()
  ,test
  (test)
  :doc "caller text cannot complete an author-written inline elisp prefix"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!$ARGUMENTS" "el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!el`(+ 1 2)`"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written inline shell prefix"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!$ARGUMENTS" "`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!`echo unsafe`"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written fenced elisp prefix"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!$ARGUMENTS" "el\n(+ 1 2)\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n```!el\n(+ 1 2)\n```"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written fenced shell prefix"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!$ARGUMENTS" "\necho unsafe\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n```!\necho unsafe\n```"
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/fence-boundary-provenance ()
  ,test
  (test)
  :doc "caller-provided leading newline cannot activate fenced shell"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "prefix$ARGUMENTS```!\necho unsafe\n```\n" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\n```!\necho unsafe\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided leading newline cannot activate fenced elisp"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "prefix$ARGUMENTS```!el\n(+ 1 2)\n```\n" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\n```!el\n(+ 1 2)\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided trailing newline cannot activate fenced shell"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "```!\necho unsafe\n```$ARGUMENTS" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!\necho unsafe\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided trailing newline cannot activate fenced elisp"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "```!el\n(+ 1 2)\n```$ARGUMENTS" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!el\n(+ 1 2)\n```\n"
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/parameterized-markers ()
  ,test
  (test)
  :doc "author-written inline shell markers may interpolate arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!`printf \"%s\" \"$ARGUMENTS\"`"
                  "hello" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=hello"
                     (plist-get outcome :body)))))

  :doc "author-written inline elisp markers may interpolate arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!el`(concat \"x\" \"$ARGUMENTS\")`"
                  "y" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=\"xy\""
                     (plist-get outcome :body)))))

  :doc "author-written fenced shell markers may interpolate arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!\nprintf \"%s\" \"$ARGUMENTS\"\n```"
                  "hello" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\nhello"
                     (plist-get outcome :body)))))

  :doc "author-written fenced elisp markers may interpolate arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!el\n(concat \"x\" \"$ARGUMENTS\")\n```"
                  "y" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n\"xy\""
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/interpolated-delimiters ()
  ,test
  (test)
  :doc "inline shell skips non-author backticks in interpolated arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!`printf \"%s\" '$ARGUMENTS'`"
                  "a`b" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=a`b"
                     (plist-get outcome :body)))))

  :doc "inline elisp skips non-author backticks in interpolated arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value=!el`(length \"$ARGUMENTS\")`"
                  "a`b" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=3"
                     (plist-get outcome :body)))))

  :doc "fenced shell skips non-author closing fences in interpolated arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!\ncat <<'EOF'\n$ARGUMENTS\nEOF\n```"
                  "a\n```\nb" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\na\n```\nb"
                     (plist-get outcome :body)))))

  :doc "fenced elisp skips non-author closing fences in interpolated arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "value\n```!el\n(length \"$ARGUMENTS\")\n```"
                  "a\n```\nb" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n7"
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--run-body-injections-async/skipped-marker-search ()
  ,test
  (test)
  :doc "skipped inline elisp marker does not hide later author marker"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "arg=$ARGUMENTS author=!el`(+ 2 3)`"
                  "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "arg=!el`(+ 1 2)` author=5"
                     (plist-get outcome :body)))))

  :doc "skipped inline shell marker does not hide later author marker"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "arg=$ARGUMENTS author=!`echo safe`"
                  "!`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "arg=!`echo unsafe` author=safe"
                     (plist-get outcome :body)))))

  :doc "skipped fenced elisp marker does not hide later author marker"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "$ARGUMENTS\n```!el\n(+ 2 3)\n```"
                  "```!el\n(+ 1 2)\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!el\n(+ 1 2)\n```\n5"
                     (plist-get outcome :body)))))

  :doc "skipped fenced shell marker does not hide later author marker"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills--substitute-vars
                  "$ARGUMENTS\n```!\necho safe\n```"
                  "```!\necho unsafe\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!\necho unsafe\n```\nsafe"
                     (plist-get outcome :body))))))

(mevedel-deftest mevedel-skills--invoke-inline/elisp-injection ()
  ,test
  (test)
  :doc "skill allowed-tools [Eval] authorizes elisp body injection end to end"
  (mevedel-tool-exec--register)
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-eval-" t))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         outcome)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "eval-skill"
           "name: eval-skill
allowed-tools:
  - Eval
" "result=!el`(+ 2 4)`")
          (let ((skill (car (mevedel-skills-scan root '(".")))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (mevedel-skills-invoke
               skill nil (lambda (o) (setq outcome o))
               :trigger 'user-slash)
              (while (null outcome)
                (accept-process-output nil 0.01)))
            (should (eq 'ok (plist-get outcome :status)))
            (should (equal "result=6" (plist-get outcome :body)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-skills--check-bash-permission/trust-literal ()
  ,test
  (test)
  :doc ":trust-literal-p t skips dangerous-commands downgrade"
  ;; The dangerous-commands list does not downgrade allow to ask.
  (let ((mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-rules '(("Bash" :pattern "rm *" :action allow))))
    (should (eq 'allow
                (mevedel-tools--check-bash-permission
                 "rm /tmp/foo" :trust-literal-p t)))
    (should (eq 'ask
                (mevedel-tools--check-bash-permission "rm /tmp/foo"))))

  :doc ":trust-literal-p t skips fail-safe-complex-syntax"
  ;; Fail-safe complex-syntax checks are bypassed.
  (let ((mevedel-bash-fail-safe-on-complex-syntax t)
        (mevedel-permission-rules '(("Bash" :pattern "echo *" :action allow))))
    ;; Variable expansion would normally trip fail-safe.
    (should (eq 'allow
                (mevedel-tools--check-bash-permission
                 "echo $VAR" :trust-literal-p t))))

  :doc "explicit deny still wins under :trust-literal-p t"
  (let ((mevedel-permission-rules '(("Bash" :pattern "rm *" :action deny))))
    (should (eq 'deny
                (mevedel-tools--check-bash-permission
                 "rm /tmp/foo" :trust-literal-p t))))

  :doc "skill bucket allows Bash even without session/global rule"
  ;; Reviewer's correctness fix: the Bash permission path must
  ;; consult invocation/request skill buckets so a skill with
  ;; `allowed-tools: [Bash(gh *)]' actually authorizes `gh' calls.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "b" :root "/tmp/b" :name "b"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-permission-rules
                   '(("Bash" :pattern "gh *" :action allow))))
         (mevedel-permission-rules nil))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (should (eq 'allow
                  (mevedel-tools--check-bash-permission
                   "gh issue list" :trust-literal-p t)))
      ;; The same call without :trust-literal-p still consults the
      ;; bucket; default behavior also works.
      (should (eq 'allow
                  (mevedel-tools--check-bash-permission
                   "gh issue list")))))

  :doc "session deny beats invocation/request skill allow on Bash"
  ;; A session deny still wins over a skill-bucket allow.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "b2" :root "/tmp/b2" :name "b2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create
                   "main" ws))
         (mevedel-permission-rules nil))
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "rm *" :action deny)))
    (let ((request (mevedel-request--create
                    :session session
                    :skill-permission-rules
                    '(("Bash" :action allow)))))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request request)
        ;; rm is a dangerous command; with trust-literal-p the
        ;; overlay is suppressed but the session deny still wins.
        (should (eq 'deny
                    (mevedel-tools--check-bash-permission
                     "rm /tmp/foo" :trust-literal-p t)))))))

(mevedel-deftest mevedel-skills--check-bash-permission/plan-mode ()
  ,test
  (test)
  :doc "plan mode suppresses skill-bucket allow on Bash"
  ;; Bash is non-read-only, so under plan mode the bucket-aware
  ;; resolver must skip invocation/request rules in the allow/ask
  ;; pass.  With the skill's Bash allow suppressed, no other bucket
  ;; matches, and plan mode hard-denies instead of prompting.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "p1" :root "/tmp/p1" :name "p1"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-permission-rules
                   '(("Bash" :pattern "gh *" :action allow))))
         (mevedel-permission-rules nil))
    (setf (mevedel-session-permission-mode session) 'plan)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (should (eq 'deny
                  (mevedel-tools--check-bash-permission
                   "gh issue list" :trust-literal-p t)))))

  :doc "plan mode does not suppress session/persistent buckets"
  ;; The same plan mode that suppresses the skill bucket leaves the
  ;; session bucket alone, so a session-level allow still resolves.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "p2" :root "/tmp/p2" :name "p2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (mevedel-permission-rules nil))
    (setf (mevedel-session-permission-mode session) 'plan)
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "ls" :action allow)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should (eq 'allow
                  (mevedel-tools--check-bash-permission
                   "ls" :trust-literal-p t)))))

  :doc "default mode keeps the skill-bucket allow for Bash"
  ;; Sanity check that the suppression only applies under plan.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "p3" :root "/tmp/p3" :name "p3"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-permission-rules
                   '(("Bash" :pattern "gh *" :action allow))))
         (mevedel-permission-rules nil))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (should (eq 'allow
                  (mevedel-tools--check-bash-permission
                   "gh issue list" :trust-literal-p t))))))


;;
;;; mevedel-skills-invoke (unified invocation API)

(mevedel-deftest mevedel-skills-invoke ()
  ,test
  (test)
  :doc "inline skill yields :status ok :kind inline with prepared body"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "i" :root "/tmp/i" :name "i"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "shout"
                 :body "YELL $ARGUMENTS"))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills-invoke
       skill "loudly"
       (lambda (o) (setq outcome o))
       :trigger 'model-skill))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "YELL loudly" (plist-get outcome :body))))

  :doc "user-slash trigger installs the pending stash"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hello"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'user-slash)
      (let ((stash mevedel-skills--pending-request-context))
        (should (equal (mevedel-model-tier-selector 'fast)
                       (plist-get stash :model)))
        (should (equal '(("Read" :action allow))
                       (plist-get stash :permission-rules)))
        (should (= 1 (length (plist-get stash :invoked-skills))))))
	    (should (eq 'ok (plist-get outcome :status))))

  :doc "UserPromptExpansion can rewrite user-slash inline skill output"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "slash-expansion" :root "/tmp/slash-expansion"
              :name "slash-expansion"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         (mevedel-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                        :function mevedel-skills-test--expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'user-slash))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal
             "Expanded by hook\n\n<hook-context>\nexpansion context\n</hook-context>"
             (plist-get outcome :body))))

  :doc "malformed UserPromptExpansion decision does not abort inline slash skill"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "slash-expansion-malformed"
              :root "/tmp/slash-expansion-malformed"
              :name "slash-expansion-malformed"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback 'passed))))
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :trigger 'user-slash)
        (should mevedel-skills--pending-request-context)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Original body" (plist-get outcome :body))))

  :doc "UserPromptExpansion can block user-slash inline skill output"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "slash-expansion-block"
              :root "/tmp/slash-expansion-block"
              :name "slash-expansion-block"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"))
         (mevedel-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                        :function
                        mevedel-skills-test--block-expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'user-slash)
      (should-not mevedel-skills--pending-request-context))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'hook-blocked (plist-get outcome :reason)))
    (should (equal "blocked expansion" (plist-get outcome :message))))

  :doc "user-slash preparation failure clears the pending stash"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "slash-fail" :root "/tmp/slash-fail"
              :name "slash-fail"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hello"
                 :allowed-tool-rules '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-skills--run-body-injections-async)
                 (lambda (_text callback)
                   (funcall callback
                            '(:status error
                              :reason injection-failed
                              :message "boom")))))
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :trigger 'user-slash)
        (should (null mevedel-skills--pending-request-context))
        (should-not (bound-and-true-p mevedel--current-request))))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'injection-failed (plist-get outcome :reason))))

  :doc "model-skill trigger writes directly to the active request"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "t" :root "/tmp/t" :name "t"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hi"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Bash" :pattern "ls" :action allow)))))
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel--current-request request)
      (mevedel-skills-invoke
       skill nil
       (lambda (_) nil)
       :trigger 'model-skill))
    (should (equal (mevedel-model-tier-selector 'fast)
                   (mevedel-request-skill-model-override request)))
    (should (equal '(("Bash" :pattern "ls" :action allow))
                   (mevedel-request-skill-permission-rules request))))

  :doc "user-invocable: false rejects user-slash trigger"
  (let ((skill (mevedel-skill--create
                :name "internal-only"
                :body "X"
                :user-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :trigger 'user-slash)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "disable-model-invocation rejects model-skill trigger"
  (let ((skill (mevedel-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :trigger 'model-skill)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "skip-gates lets internal commands invoke disabled skills"
  (let ((skill (mevedel-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (cl-letf (((symbol-function 'mevedel-skills--skill-enabled-p)
               (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'model-skill
       :skip-gates t))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "X" (plist-get outcome :body))))

  :doc "missing body returns load-failure error"
  (let ((skill (mevedel-skill--create :name "no-body"))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :trigger 'model-skill)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'load-failure (plist-get outcome :reason))))

  :doc "recursion-depth limit yields recursion-limit-exceeded"
  ;; Dynamic let-bound counter; 0 max means even the first invocation
  ;; exceeds.
  (let ((skill (mevedel-skill--create :name "x" :body "X"))
        (mevedel-skills-max-recursion-depth 0)
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :trigger 'internal)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'recursion-limit-exceeded (plist-get outcome :reason))))

  :doc "display-callback receives done event on success"
  (let ((skill (mevedel-skill--create :name "ok" :body "Hi"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :trigger 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'done))
                     events)))

  :doc "display-callback receives error event on failure"
  (let ((skill (mevedel-skill--create :name "no-body"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :trigger 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'error))
                     events))))


;;
;;; Phase 6: build-fork-agent + fork dispatch routing

(mevedel-deftest mevedel-skills--build-fork-agent ()
  ,test
  (test)
  :doc "named-agent path looks up via the registry"
  (let ((agent (mevedel-agent--create :name "explorer" :tools nil
                                      :system-prompt "")))
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent))))
      (let ((skill (mevedel-skill--create
                    :name "demo" :context 'fork :agent "explorer")))
        (should (eq agent (mevedel-skills--build-fork-agent skill))))))

  :doc "named-agent path returns nil for unknown agent"
  (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
    (let ((skill (mevedel-skill--create
                  :name "demo" :context 'fork :agent "missing")))
      (should (null (mevedel-skills--build-fork-agent skill)))))

  :doc "parent-inherited path synthesizes a `skill:<name>' agent"
  ;; The synthetic agent's name is `skill:<skill-name>' and its
  ;; system prompt is captured from the calling buffer's
  ;; `gptel-system-prompt' at spawn time.
  (let ((skill (mevedel-skill--create
                :name "demo" :context 'fork
                :description "A test skill")))
    (with-temp-buffer
      (setq-local gptel-system-prompt "captured-system-prompt")
      (setq-local mevedel-agent-exec--agents nil)
      (let ((agent (mevedel-skills--build-fork-agent skill)))
        (should (mevedel-agent-p agent))
        (should (equal "skill:demo" (mevedel-agent-name agent)))
        (should (equal "captured-system-prompt"
                       (mevedel-agent-system-prompt agent)))
        ;; The synthetic agent is registered into the buffer-local
        ;; `mevedel-agent-exec--agents' so spawn can resolve it.
        (should (assoc-string "skill:demo" mevedel-agent-exec--agents))))))

(mevedel-deftest mevedel-skills-invoke-fork ()
  ,test
  (test)
  :doc "model-skill trigger routes to direct dispatch via mevedel-tools--task"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (dispatched nil)
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body $ARGUMENTS"
                 :allowed-tool-rules
                 '(("Read" :action allow))
                 :model "fast")))
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb a desc prompt &rest args)
                 (setq dispatched
                       (list :agent a :description desc :prompt prompt
                             :keys args))
                 ;; Simulate a foreground completion.
                 (funcall cb "agent finished"))))
      (let (outcome)
        (mevedel-skills-invoke
         skill "the task"
         (lambda (o) (setq outcome o))
         :trigger 'model-skill)
        (should dispatched)
        (should (eq agent (plist-get dispatched :agent)))
        (should (string-match-p "the task" (plist-get dispatched :prompt)))
        (let ((keys (plist-get dispatched :keys)))
          (should (equal '(("Read" :action allow))
                         (plist-get keys :skill-permission-rules)))
          (should (equal (mevedel-model-tier-selector 'fast)
                         (plist-get keys :skill-model-override))))
        (should (eq 'ok (plist-get outcome :status)))
        (should (eq 'fork (plist-get outcome :kind)))
        (should (equal "agent finished" (plist-get outcome :result)))
        ;; When `mevedel-tools--task' delivers a bare string (no
        ;; transcript metadata, e.g. our test mock), the outcome
        ;; falls back to the registry agent's name.  When it
        ;; delivers a `(:result :render-data)' plist, the unique
        ;; invocation agent-id from the render-data wins.
        (should (equal "explorer" (plist-get outcome :agent-id)))
        (should (null (plist-get outcome :render-data))))))

  :doc "fork-direct forwards :render-data when the task callback wraps it"
  ;; Outcome carries :render-data so the renderer can expose the
  ;; transcript-open affordance.
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Body")))
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) agent))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb _agent _desc _prompt &rest _args)
                 (funcall cb
                          (list :result "wrapped"
                                :render-data
                                '(:kind agent-transcript
                                        :agent-id "explorer--abc123"
                                        :transcript-relative-path "p"
                                        :status running))))))
      (let (outcome)
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :trigger 'model-skill)
        (should (equal "wrapped" (plist-get outcome :result)))
        (should (equal "explorer--abc123"
                       (plist-get outcome :agent-id)))
        (should (eq 'agent-transcript
                    (plist-get (plist-get outcome :render-data) :kind))))))

  :doc "user-slash trigger direct-dispatches and returns fork outcome"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"))
         outcome)
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb _agent _desc _prompt &rest _args)
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill "the task"
       (lambda (o) (setq outcome o))
       :trigger 'user-slash))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'fork (plist-get outcome :kind)))
    (should (equal "agent finished" (plist-get outcome :result))))

  :doc "fork additional context is appended to the dispatched prompt"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"))
         captured-prompt)
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb _agent _desc prompt &rest _args)
                 (setq captured-prompt prompt)
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill nil #'ignore
       :trigger 'user-slash
       :additional-context "<hook-context>ctx</hook-context>"))
  (should (string-match-p "Task body" captured-prompt))
  (should (string-match-p "<hook-context>ctx</hook-context>"
                          captured-prompt)))

  :doc "fork dispatch forwards custom description and invocation callback"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"))
         (progress-callback #'ignore)
         captured-description
         captured-on-invocation)
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb _agent desc _prompt &rest args)
                 (setq captured-description desc)
                 (setq captured-on-invocation
                       (plist-get args :on-invocation))
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill nil #'ignore
       :trigger 'user-slash
       :description "target hint"
       :on-invocation progress-callback))
    (should (equal "target hint" captured-description))
    (should (eq progress-callback captured-on-invocation)))

  :doc "fork dispatch errors return an error outcome"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"))
         outcome)
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-tools--task)
               (lambda (&rest _)
                 (error "SubagentStart hook stopped sub-agent"))))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'user-slash))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'agent-dispatch-failed (plist-get outcome :reason)))
    (should (string-match-p "SubagentStart hook stopped sub-agent"
                            (plist-get outcome :message))))

  :doc "user-slash fork hooks are active during body injection"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (hooks '((PreToolUse
                   (:matcher "Bash"
                    :hooks ((:type elisp
                             :function mevedel-skills-test--hook-fn))))))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"
                 :hooks hooks))
         (request (mevedel-request--create))
         saw-hooks)
    (with-temp-buffer
      (setq-local mevedel--current-request request)
      (cl-letf (((symbol-function 'mevedel-agent-get)
                 (lambda (n) (and (equal n "explorer") agent)))
                ((symbol-function 'mevedel-skills--run-body-injections-async)
                 (lambda (_text callback)
                   (setq saw-hooks (mevedel-request-hook-rules request))
                   (funcall callback '(:status error
                                       :reason stop
                                       :message "stop"))))
	                ((symbol-function 'mevedel-tools--task)
	                 (lambda (&rest _)
	                   (error "Should not dispatch"))))
        (mevedel-skills-invoke
         skill nil #'ignore
         :trigger 'user-slash)))
    (should (equal hooks saw-hooks)))

  :doc "unknown agent yields :reason unknown-agent"
  (let ((skill (mevedel-skill--create
                :name "demo" :context 'fork :agent "missing"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :trigger 'model-skill))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'unknown-agent (plist-get outcome :reason))))

  :doc "omitted agent (parent-inherited) dispatches to a synthetic agent"
  ;; Parent-inherited fork uses a synthetic `skill:<name>' agent.
  ;; Mock mevedel-tools--task to assert the dispatch happens with
  ;; the synthetic struct rather than erroring.
  (let* ((skill (mevedel-skill--create
                 :name "demo" :context 'fork
                 :body "Body"))
         (dispatched-agent nil))
    (cl-letf (((symbol-function 'mevedel-tools--task)
               (lambda (cb agent &rest _)
                 (setq dispatched-agent agent)
                 (funcall cb "result"))))
      (with-temp-buffer
        (setq-local mevedel-agent-exec--agents nil)
        (let (outcome)
          (mevedel-skills-invoke
           skill nil
           (lambda (o) (setq outcome o))
           :trigger 'model-skill)
          (should (eq 'ok (plist-get outcome :status)))
          (should (mevedel-agent-p dispatched-agent))
          (should (equal "skill:demo"
                         (mevedel-agent-name dispatched-agent))))))))

(mevedel-deftest mevedel-skills--invoke-handler ()
  ,test
  (test)
  :doc "unknown skill returns an error"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "u" :root "/tmp/u" :name "u"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         received)
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (r) (setq received r))
       (list :name "nope")))
    (should (string-match-p "Unknown skill" received)))

  :doc "known inline skill is dispatched and body returned"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t))
         (ws (mevedel-workspace--create
              :type 'test :id dir :root dir :name "h"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         received)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "shout"
           "name: shout
description: Yell
"
           "YELL $ARGUMENTS")
          (setf (mevedel-session-skills session)
                (mevedel-skills-scan dir '(".")))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r) (setq received r))
             (list :name "shout" :arguments "loudly")))
          (should (equal "YELL loudly" received)))
      (delete-directory dir t)))

  :doc "model-side invocation uses visible prefixed names after conflicts"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (ws (mevedel-workspace--create
              :type 'test :id "collision" :root "/tmp/collision"
              :name "collision"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (local (mevedel-skill--create
                 :name "shared"
                 :source 'project
                 :source-family 'mevedel
                 :body "LOCAL $ARGUMENTS"))
         (global (mevedel-skill--create
                  :name "shared"
                  :source 'user
                  :source-family 'mevedel
                  :body "GLOBAL $ARGUMENTS"))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (mevedel-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r) (setq received r))
             (list :name "local:shared" :arguments "now"))
            (should (equal "LOCAL now" received))
            (setq received nil)
            (mevedel-skills--invoke-handler
             (lambda (r) (setq received r))
             (list :name "shared"))
            (should (string-match-p "Unknown skill 'shared'" received))))
      (delete-directory user-dir t)))

  :doc "disabled skill is rejected before model invocation"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "hidden"
                 :body "should not run"))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-skills--set-enabled "hidden" nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r) (setq received r))
             (list :name "hidden")))
          (should (string-match-p "disabled" received)))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills--list-handler ()
  ,test
  (test)
  :doc "returns active model-invocable enabled skills"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create
                 :name "alpha" :description "Alpha helper"
                 :active-p t :model-invocable-p t))
         (beta (mevedel-skill--create
                :name "beta" :description "Beta helper"
                :active-p t :model-invocable-p t))
         (model-disabled (mevedel-skill--create
                          :name "internal" :description "Internal"
                          :active-p t :model-invocable-p nil))
         (dormant (mevedel-skill--create
                   :name "dormant" :description "Dormant"
                   :active-p nil :model-invocable-p t
                   :path-patterns '("*.el")))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (list alpha beta model-disabled dormant))
          (mevedel-skills--set-enabled "beta" nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r) (setq received r))
             (list :query "alp")))
          (should (string-match-p "alpha: Alpha helper" received))
          (should-not (string-match-p "beta" received))
          (should-not (string-match-p "internal" received))
          (should-not (string-match-p "dormant" received)))
      (delete-directory user-dir t)))

  :doc "refreshes session skills before listing"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (fresh (mevedel-skill--create
                 :name "fresh" :description "Fresh helper"
                 :active-p t :model-invocable-p t))
         refreshed
         received)
    (unwind-protect
        (with-temp-buffer
          (setq mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                     (lambda (_buffer s)
                       (setq refreshed s)
                       (setf (mevedel-session-skills s) (list fresh)))))
            (mevedel-skills--list-handler
             (lambda (r) (setq received r))
             nil))
          (should (eq refreshed session))
          (should (string-match-p "fresh: Fresh helper" received)))
      (delete-directory user-dir t)))

  :doc "returns prefixed visible names that can be used with Skill"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (local (mevedel-skill--create
                 :name "shared"
                 :description "Local helper"
                 :source 'project
                 :source-family 'mevedel
                 :active-p t
                 :model-invocable-p t))
         (global (mevedel-skill--create
                  :name "shared"
                  :description "Global helper"
                  :source 'user
                  :source-family 'mevedel
                  :active-p t
                  :model-invocable-p t))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (mevedel-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r) (setq received r))
             (list :query "shared")))
          (should (string-match-p "local:shared: Local helper" received))
          (should (string-match-p "global:shared: Global helper" received))
          (should-not (string-match-p "\nshared: " received)))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers Skill and ListSkills tools"
  (progn
    (mevedel-skills--register)
    (should (mevedel-tool-get "Skill" "mevedel"))
    (should (mevedel-tool-get "ListSkills" "mevedel"))))


;;
;;; Phase C — slash commands and completion

(defun mevedel-skills-test--make-session (&optional name)
  "Return a throwaway session named NAME with a minimal workspace."
  (let ((ws (mevedel-workspace--create
             :type 'test :id "t" :root "/tmp/t" :name (or name "t")
             :file-cache (mevedel-file-cache--create
                          :table (make-hash-table :test #'equal)
                          :order nil :total-bytes 0))))
    (mevedel-session-create (or name "main") ws)))

(defmacro mevedel-skills-test--with-chat-buffer (session &rest body)
  "Run BODY in a temp buffer that mimics a mevedel chat buffer.
SESSION is bound to buffer-local `mevedel--session', and
`gptel-prompt-prefix-alist' is extended so the buffer's major mode
maps to \"### \"."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((gptel-prompt-prefix-alist
            (cons (cons major-mode "### ")
                  gptel-prompt-prefix-alist)))
       (setq mevedel--session ,session)
       ,@body)))

(defun mevedel-skills-test--open-list (session view-buffer data-buffer)
  "Open a skills cockpit for SESSION owned by VIEW-BUFFER and DATA-BUFFER."
  (with-current-buffer data-buffer
    (setq-local mevedel--session session)
    (setq-local mevedel--view-buffer view-buffer)
    (mevedel-skills-list-open session view-buffer data-buffer data-buffer)))

(defun mevedel-skills-test--cleanup-list (&rest buffers)
  "Kill skills cockpit test buffers and BUFFERS."
  (dolist (name (list mevedel-skills-list-buffer-name
                      "*mevedel skill details*"
                      mevedel-skills-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-skills-test--capf-candidates (capf &optional prefix)
  "Return candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

(mevedel-deftest mevedel-skills--parse-slash-line ()
  ,test
  (test)
  :doc "plain `/command' parses to (name \"\" 0)"
  (should (equal '("help" "" 0)
                 (mevedel-skills--parse-slash-line "/help")))

  :doc "`/command args' parses to (name args 0)"
  (should (equal '("model" "gpt-4" 0)
                 (mevedel-skills--parse-slash-line "/model gpt-4")))

  :doc "plugin-prefixed skill names parse as commands"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (mevedel-skills--parse-slash-line
                  "/superpowers:brainstorming now")))

  :doc "additional lines after the command are appended to ARGS"
  ;; Skill commands (`/coordinator', `/grill-me', ...) take a
  ;; prompt body as ARGS, and prompt bodies are naturally
  ;; multi-line.  Truncating at the first newline produced
  ;; "args contain only the first line" -- the LLM saw a
  ;; truncated task description and could not act on it.
  (should (equal '("coordinator"
                   "Launch three background explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (mevedel-skills--parse-slash-line
                  "/coordinator Launch three background explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("coordinator"
                   "Multi-line task body\nspanning lines"
                   0)
                 (mevedel-skills--parse-slash-line
                  "/coordinator
Multi-line task body
spanning lines")))

  :doc "text not starting with `/' returns nil"
  (should (null (mevedel-skills--parse-slash-line "hello /help")))

  :doc "non-identifier command names are rejected"
  (should (null (mevedel-skills--parse-slash-line "/hi!")))
  (should (null (mevedel-skills--parse-slash-line "/")))

  :doc "leading whitespace is reported via offset"
  (should (equal '("help" "" 3)
                 (mevedel-skills--parse-slash-line "   /help")))

  :doc "leading newlines count toward the offset"
  (should (equal '("help" "" 3)
                 (mevedel-skills--parse-slash-line "\n\n\n/help"))))

(mevedel-deftest mevedel-skills--current-prompt-region ()
  ,test
  (test)
  :doc "uses the `gptel' text property to find the boundary after a response"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "response body")
      (put-text-property (point-min) (point-max) 'gptel 'response)
      (insert "/help")
      (goto-char (point-max))
      (let ((region (mevedel-skills--current-prompt-region)))
        (should region)
        (should (equal "/help"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "falls back to the configured prompt prefix when no property is set"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### older\nplain response\n### /help")
      (goto-char (point-max))
      (let ((region (mevedel-skills--current-prompt-region)))
        (should region)
        (should (equal "/help"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "without prefix or property the whole buffer is the pending prompt"
  (let ((session (mevedel-skills-test--make-session)))
    (with-temp-buffer
      (setq mevedel--session session)
      (insert "/help alone")
      (goto-char (point-max))
      (let ((region (mevedel-skills--current-prompt-region)))
        (should region)
        (should (equal "/help alone"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "empty buffer returns nil"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (should (null (mevedel-skills--current-prompt-region))))))

(mevedel-deftest mevedel-skills--dispatch-slash-command ()
  ,test
  (test)
  :doc "local command runs its handler, deletes the region, returns 'local"
  (let ((session (mevedel-skills-test--make-session))
        (called nil))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (args) (setq called (or args t)))))))
        (insert "### /noop hello")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should (equal "hello" called))
        (should (equal "### " (buffer-string))))))

  :doc "blank mode slash command opens the mode cockpit surface"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (mevedel-skills-test--with-chat-buffer session
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq called area))))
        (insert "### /mode")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should (eq called 'mode))
        (should (equal "### " (buffer-string))))))

  :doc "mode slash command with an argument remains direct"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (mevedel-skills-test--with-chat-buffer session
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq called area))))
        (insert "### /mode accept-edits")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should-not called)
        (should (eq 'accept-edits
                    (mevedel-session-permission-mode session)))
        (should (equal "### " (buffer-string))))))

  :doc "blank model slash command opens the model cockpit surface"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (mevedel-skills-test--with-chat-buffer session
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq called area))))
        (insert "### /model")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should (eq called 'model))
        (should (equal "### " (buffer-string))))))

  :doc "model slash command with an argument remains direct"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### /model gpt-5.5")
      (goto-char (point-max))
      (should (eq 'local (mevedel-skills--dispatch-slash-command)))
      (should (eq 'gpt-5.5 gptel-model))
      (should (equal "### " (buffer-string)))))

  :doc "blank and list skills slash commands open the skills surface"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (dolist (command '("/skills" "/skills list"))
      (setq called nil)
      (mevedel-skills-test--with-chat-buffer session
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area)
                     (setq called area))))
          (insert "### " command)
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (eq called 'skills))
          (should (equal "### " (buffer-string)))))))

  :doc "blank and list tools slash commands open the tools surface"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (mevedel-skills-test--with-chat-buffer session
      (dolist (command '("/tools" "/tools list"))
        (setq called nil)
        (erase-buffer)
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area)
                     (setq called area))))
          (insert "### " command)
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (eq called 'tools))
          (should (equal "### " (buffer-string)))))))

  :doc "worktree slash command opens the worktree surface"
  (let ((session (mevedel-skills-test--make-session))
        surface-buffer)
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             (cons '("worktree" . mevedel-cmd--worktree)
                   mevedel-slash-commands)))
        (cl-letf (((symbol-function 'mevedel-worktree-list-open)
                   (lambda () (setq surface-buffer (current-buffer)))))
          (insert "### /worktree")
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (eq surface-buffer (current-buffer)))
          (should (equal "### " (buffer-string)))))))

  :doc "help slash command opens the help surface"
  (let ((session (mevedel-skills-test--make-session))
        called)
    (mevedel-skills-test--with-chat-buffer session
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq called area))))
        (insert "### /help")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should (eq called 'help))
        (should (equal "### " (buffer-string))))))

  :doc "skills mutation slash commands remain direct"
  (let* ((user-dir (make-temp-file "mevedel-skills-slash-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "visible")))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-skills-test--with-chat-buffer session
            (cl-letf (((symbol-function 'mevedel-skills-list-open)
                       (lambda (_session)
                         (ert-fail "skills surface should not open"))))
              (insert "### /skills disable visible")
              (goto-char (point-max))
              (should (eq 'local (mevedel-skills--dispatch-slash-command)))
              (should-not (mevedel-skills--skill-enabled-p skill))
              (should (equal "### " (buffer-string))))))
      (delete-directory user-dir t)))

  :doc "local command wins over a same-named skill"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "review"
                 :body "skill body"))
         (called nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             `(("review" . ,(lambda (args) (setq called args))))))
        (insert "### /review HEAD")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should (equal "HEAD" called))
        (should (equal "### " (buffer-string))))))

  :doc "unknown slash command returns 'unknown without mutating the buffer"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /bogus")
        (goto-char (point-max))
        (should (eq 'unknown (mevedel-skills--dispatch-slash-command)))
        (should (equal "### /bogus" (buffer-string))))))

  :doc "nil return when no slash command is present"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### plain text")
      (goto-char (point-max))
      (should (null (mevedel-skills--dispatch-slash-command)))))

  :doc "skill expansion inserts prepared body and returns 'skill"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-slash-command)))
        (should (equal "### Hello world!"
                       (mevedel-pipeline--strip-render-data-blocks
                        (buffer-string))))
        (should (string-search "<!-- mevedel-render-data -->"
                               (buffer-string))))))

  :doc "user-invocable: false skill returns 'unknown and aborts the send"
  ;; User-slash invocation of a skill marked `user-invocable: false'
  ;; must message and abort.  Returning `unknown' makes the surrounding
  ;; advice skip gptel-send so the literal `/internal-only' text is not sent.
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "internal-only"
                 :body "ignored"
                 :user-invocable-p nil)))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /internal-only")
        (goto-char (point-max))
        (should (eq 'unknown (mevedel-skills--dispatch-slash-command)))
        ;; Buffer left untouched.
        (should (equal "### /internal-only" (buffer-string))))))

  :doc "fork-context slash skill dispatches directly and inserts result"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "coordinator"))
         (skill (mevedel-skill--create
                 :name "coordinator"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "coordinator"))
         save-called status-called stop-called)
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (require 'mevedel-session-persistence)
        (cl-letf (((symbol-function 'mevedel-agent-get)
                   (lambda (n) (and (equal n "coordinator") agent)))
                  ((symbol-function 'mevedel-tools--task)
                   (lambda (cb _agent _desc _prompt &rest _args)
                     (funcall cb "agent finished")))
                  ((symbol-function 'mevedel-session-persistence-save)
                   (lambda (s b)
                     (setq save-called (list s b))
                     "saved"))
                  ((symbol-function 'mevedel--run-turn-terminal-hook)
                   (lambda (_fsm event status)
                     (setq stop-called
                           (list event status
                                 (not
                                  (null
                                   (bound-and-true-p
                                    mevedel--current-request)))))))
                  ((symbol-function 'gptel--update-status)
                   (lambda (&rest args) (setq status-called args))))
          (let ((gptel-response-separator "\n\n"))
            (insert "### /coordinator do the thing")
            (goto-char (point-max))
            (should (eq 'skill (mevedel-skills--dispatch-slash-command)))
            (let ((buf (buffer-string)))
              (should (string-match-p "/coordinator do the thing" buf))
              (should (string-match-p "agent finished" buf))
              (should-not (string-match-p "Use the `coordinator` agent" buf))
              (should-not (string-match-p "should-not-appear" buf)))
            (should-not mevedel--current-request)
            (should (= 1 (mevedel-session-turn-count session)))
            (should (equal (list session (current-buffer)) save-called))
            (should (equal '(Stop completed t) stop-called))
            (should (equal '(" Ready" success) status-called)))))))

  :doc "no-prefix chat: response followed by /cmd adds a blank line before cursor"
  (let ((session (mevedel-skills-test--make-session))
        (called nil))
    (with-temp-buffer
      (setq mevedel--session session)
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (_args) (setq called t))))))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n/noop")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should called)
        (should (equal "Old response\n\n" (buffer-string)))
        (should (= (point) (point-max))))))

  :doc "no-prefix chat: skill body is placed on a fresh line with blank separator"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (let ((mevedel-slash-commands nil))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n/greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-slash-command)))
        (should (equal "Old response\n\nHello world!"
                       (mevedel-pipeline--strip-render-data-blocks
                        (buffer-string)))))))

  :doc "no-prefix chat: blank lines above the slash command are preserved"
  (let ((session (mevedel-skills-test--make-session))
        (called nil))
    (with-temp-buffer
      (setq mevedel--session session)
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (_args) (setq called t))))))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n\n\n/noop")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should called)
        (should (equal "Old response\n\n\n" (buffer-string))))))

  :doc "prefix chat: slash command after prefix keeps prefix intact"
  (let ((session (mevedel-skills-test--make-session))
        (called nil))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (_args) (setq called t))))))
        (insert "### /noop")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should called)
        (should (equal "### " (buffer-string))))))

  :doc "`/plugin' and `/plugin list' dispatch to the plugin surface"
  (let ((session (mevedel-skills-test--make-session))
        opened
        messages)
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands mevedel-slash-commands))
        (require 'mevedel-menu)
        (dolist (command '("/plugin" "/plugin list"))
          (setq opened nil
                messages nil)
          (erase-buffer)
          (cl-letf (((symbol-function 'mevedel-menu-open)
                     (lambda (area)
                       (setq opened area)
                       nil))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format-message fmt args) messages)
                       nil)))
            (insert "### " command)
            (goto-char (point-max))
            (should (eq 'local (mevedel-skills--dispatch-slash-command)))
            (should (eq opened 'plugins))
            (should-not messages)
            (should (equal "### " (buffer-string))))))))

  :doc "`/clear' asks before clearing a non-materialized session"
  (let ((session (mevedel-skills-test--make-session))
        (asked nil))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands mevedel-slash-commands)
            (mevedel-session-persistence t))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq asked prompt)
                     nil)))
          (insert "Existing transcript\n### /clear")
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (equal "Clear all chat buffer content? " asked))
          (should (equal "Existing transcript\n### "
                         (buffer-string)))))))

  :doc "`/clear' clears a non-materialized session after confirmation"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands mevedel-slash-commands)
            (mevedel-session-persistence t))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t)))
          (insert "Existing transcript\n### /clear")
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (equal "### " (buffer-string)))))))

  :doc "`/clear' falls back with confirmation in a rewind preview buffer"
  (let* ((session (mevedel-skills-test--make-session))
         (tempdir (make-temp-file "mevedel-clear-preview-test-" t))
         (save-path (file-name-as-directory tempdir)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) save-path)
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands mevedel-slash-commands)
                  (mevedel-session-persistence t)
                  (asked nil))
              (setq buffer-file-name nil)
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (prompt)
                           (setq asked prompt)
                           t))
                        ((symbol-function
	                          'mevedel-session-persistence-start-fresh-segment)
	                         (lambda (&rest _args)
	                           (error "Should not rotate preview buffer"))))
                (insert "Rewound transcript\n### /clear")
                (goto-char (point-max))
                (should (eq 'local (mevedel-skills--dispatch-slash-command)))
                (should (equal "Clear all chat buffer content? " asked))
                (should (equal "### " (buffer-string)))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))))

  :doc "`/clear' starts a fresh segment when the session is materialized"
  (let* ((session (mevedel-skills-test--make-session))
         (tempdir (make-temp-file "mevedel-clear-test-" t))
         (save-path (file-name-as-directory tempdir))
         (seg1 (file-name-concat save-path "segment-0001.chat.org")))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-session-session-id session) "clear-test")
          (setf (mevedel-session-current-segment session) 1)
          (setf (mevedel-session-created-at session) "2026-05-08T10-00-00")
          (setf (mevedel-session-updated-at session) "2026-05-08T10-00-00")
          (with-temp-buffer
            (org-mode)
            (let ((gptel-prompt-prefix-alist
                   (cons (cons major-mode "### ")
                         gptel-prompt-prefix-alist))
                  (mevedel-slash-commands mevedel-slash-commands)
                  (mevedel-session-persistence t))
              (setq mevedel--session session)
              (setq buffer-file-name seg1)
              (cl-letf (((symbol-function
                          'mevedel-session-persistence--save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'mevedel-version)
                         (lambda (&rest _args) "test-version")))
                (insert "### /clear")
                (goto-char (point-max))
                (should (eq 'local (mevedel-skills--dispatch-slash-command)))
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))
                (should (string-suffix-p "### " (buffer-string)))
                (with-temp-buffer
                  (insert-file-contents seg1)
                  (should-not (string-match-p "###" (buffer-string))))
                (let* ((sidecar
                        (mevedel-session-persistence--sidecar-path save-path))
                       (plist (mevedel-session-persistence-read sidecar))
                       (seg1-index
                        (cdr (assoc 1 (plist-get plist :prompt-index)))))
                  (should-not seg1-index))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))))

  :doc "`/clear' refreshes stale visited metadata before slash deletion"
  (let* ((session (mevedel-skills-test--make-session))
         (tempdir (make-temp-file "mevedel-clear-stale-test-" t))
         (save-path (file-name-as-directory tempdir))
         (seg1 (file-name-concat save-path "segment-0001.chat.org")))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-session-session-id session) "clear-stale-test")
          (setf (mevedel-session-current-segment session) 1)
          (setf (mevedel-session-created-at session) "2026-05-08T10-00-00")
          (setf (mevedel-session-updated-at session) "2026-05-08T10-00-00")
          (with-temp-buffer
            (org-mode)
            (let ((gptel-prompt-prefix-alist
                   (cons (cons major-mode "### ")
                         gptel-prompt-prefix-alist))
                  (mevedel-slash-commands mevedel-slash-commands)
                  (mevedel-session-persistence t))
              (setq mevedel--session session)
              (setq buffer-file-name seg1)
              (insert "### ")
              (write-region (point-min) (point-max) buffer-file-name nil 'silent)
              (set-visited-file-modtime)
              (set-buffer-modified-p nil)
              (insert "/clear")
              (set-file-times buffer-file-name (time-add (current-time) 5))
              (should-not (verify-visited-file-modtime (current-buffer)))
              (cl-letf (((symbol-function
                          'mevedel-session-persistence--save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'mevedel-version)
                         (lambda (&rest _args) "test-version"))
	                        ((symbol-function 'ask-user-about-supersession-threat)
	                         (lambda (&rest _args)
	                           (error "Supersession prompt"))))
                (goto-char (point-max))
                (should (eq 'local (mevedel-skills--dispatch-slash-command)))
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))
                (should (string-suffix-p "### " (buffer-string)))
                (with-temp-buffer
                  (insert-file-contents seg1)
                  (should-not (string-match-p "###" (buffer-string))))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t)))))

(mevedel-deftest mevedel-cmd--skills ()
  ,test
  (test)
  :doc "list routes through the skills surface and help remains direct"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'project))
         called-area
         message-text)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (with-temp-buffer
            (setq mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-menu-open)
                       (lambda (area)
                         (setq called-area area)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq message-text (apply #'format fmt args)))))
              (mevedel-cmd--skills "list")
              (should (eq called-area 'skills))
              (mevedel-cmd--skills "help visible")
              (should (string-match-p "Visible description"
                                      message-text)))))
      (mevedel-skills-test--cleanup-list)
      (delete-directory user-dir t)))

  :doc "list falls back to a message outside a live cockpit pair"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'project))
         message-text)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (with-temp-buffer
            (setq mevedel--session session)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq message-text (apply #'format fmt args)))))
              (mevedel-cmd--skills "list")
              (should (string-match-p
                       "visible \\[enabled\\] source:project - Visible description"
                       message-text)))))
      (mevedel-skills-test--cleanup-list)
      (delete-directory user-dir t)))

  :doc "disable and enable persist skill state"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "visible"))
         message-text)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (with-temp-buffer
            (setq mevedel--session session)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq message-text (apply #'format fmt args)))))
              (mevedel-cmd--skills "disable visible")
              (should (string-match-p "disabled" message-text))
              (should-not (mevedel-skills--skill-enabled-p skill))
              (mevedel-cmd--skills "enable visible")
              (should (string-match-p "enabled" message-text))
              (should (mevedel-skills--skill-enabled-p skill)))))
      (delete-directory user-dir t)))

  :doc "disable follows a file-backed skill through generated renaming"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (root (make-temp-file "mevedel-skills-state-root-" t))
         (skill-file (mevedel-skills-test--write-skill
                      root "shared" "description: Shared\n" "Body"))
         (other-file (mevedel-skills-test--write-skill
                      root "other" "description: Other\n" "Body"))
         (skill (mevedel-skill--create
                 :name "shared" :source-file skill-file))
         (renamed (mevedel-skill--create
                   :name "user:shared" :source-file skill-file))
         (other (mevedel-skill--create
                 :name "shared" :source-file other-file))
         message-text)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (with-temp-buffer
            (setq mevedel--session session)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq message-text (apply #'format fmt args)))))
              (mevedel-cmd--skills "disable shared")
              (should (string-match-p "disabled" message-text))
              (should-not (mevedel-skills--disabled-names))
              (should (mevedel-skills--disabled-keys))
              (should-not (mevedel-skills--skill-enabled-p renamed))
              (should (mevedel-skills--skill-enabled-p other)))))
      (delete-directory user-dir t)
      (delete-directory root t)))

  :doc "legacy disabled names match generated local prefixes"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (root (make-temp-file "mevedel-skills-state-root-" t))
         (skill-file (mevedel-skills-test--write-skill
                      root "shared" "description: Shared\n" "Body"))
         (skill (mevedel-skill--create
                 :name "user:shared"
                 :source 'user
                 :source-file skill-file)))
    (unwind-protect
        (progn
          (mevedel-skills--set-enabled "shared" nil)
          (should-not (mevedel-skills--skill-enabled-p skill))
          (mevedel-skills--set-enabled skill t)
          (should (mevedel-skills--skill-enabled-p skill))
          (should-not (mevedel-skills--disabled-names)))
      (delete-directory user-dir t)
      (delete-directory root t))))

(mevedel-deftest mevedel-cmd--tools ()
  ,test
  (test)
  :doc "unknown arguments report usage"
  (let ((message-text nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (mevedel-cmd--tools "load Edit")
      (should (string-match-p "Usage: /tools \\[list\\]" message-text))))

  :doc "blank arguments require a session"
  (with-temp-buffer
    (should-error (mevedel-cmd--tools "") :type 'user-error)))

(mevedel-deftest mevedel-skills-list-open ()
  ,test
  (test)
  :doc "renders session skills as a tabulated cockpit"
  (let* ((user-dir (make-temp-file "mevedel-skills-list-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-list-view*"))
         (data-buffer (generate-new-buffer " *skills-list-data*"))
         (source-file (make-temp-file "mevedel-skill-source-"))
         (active (mevedel-skill--create
                  :name "active" :description "Active description"
                  :when-to-use "When active is useful"
                  :source 'project :source-file source-file))
         (disabled (mevedel-skill--create
                    :name "disabled" :description "Disabled description"
                    :source 'user))
         (plugin (mevedel-skill--create
                  :name "plugin:assist" :description "Plugin description"
                  :source 'plugin)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (list active disabled plugin))
          (mevedel-skills--set-enabled disabled nil)
          (let ((buffer (mevedel-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (should (eq major-mode 'mevedel-skills-list-mode))
              (should (eq mevedel-skills-list--session session))
              (should (equal tabulated-list-sort-key '("Name" . nil)))
              (should (string-match-p
                       "2/3 enabled"
                       (mevedel-skills-list--header-line)))
              (should (= 3 (length tabulated-list-entries)))
              (let ((active-row (cadr (assoc "active" tabulated-list-entries)))
                    (disabled-row (cadr (assoc "disabled"
                                               tabulated-list-entries)))
                    (plugin-row (cadr (assoc "plugin:assist"
                                             tabulated-list-entries))))
                (should (equal (substring-no-properties (aref active-row 0))
                               "enabled"))
                (should (equal (aref active-row 1) "active"))
                (should (equal (aref active-row 2) "project"))
                (should (equal (aref active-row 3) "Active description"))
                (should (equal (substring-no-properties (aref disabled-row 0))
                               "disabled"))
                (should (equal (aref disabled-row 2) "user"))
                (should (equal (aref plugin-row 2) "plugin")))
              (mevedel-cockpit-goto-id "active")
              (should (eq (mevedel-skills-list--skill-at-point)
                          active))
              (mevedel-skills-list-details))
            (with-current-buffer "*mevedel skill details*"
              (should (string-match-p "Active description"
                                      (buffer-string)))
              (should (string-match-p "When active is useful"
                                      (buffer-string)))
              (should (string-match-p (regexp-quote source-file)
                                      (buffer-string)))))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer)
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory user-dir t)))

(mevedel-deftest mevedel-skills-list-refresh ()
  ,test
  (test)
  :doc "refresh preserves the selected skill row"
  (let* ((user-dir (make-temp-file "mevedel-skills-refresh-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-refresh-view*"))
         (data-buffer (generate-new-buffer " *skills-refresh-data*"))
         (first (mevedel-skill--create :name "first" :source 'project))
         (second (mevedel-skill--create :name "second" :source 'project)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list first second))
          (let ((buffer (mevedel-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id "second")
              (setf (mevedel-skill-description second) "Updated")
              (mevedel-skills-list-refresh)
              (should (equal (tabulated-list-get-id) "second"))
              (should (equal (aref (cadr (assoc "second"
                                                tabulated-list-entries))
                                  3)
                             "Updated")))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills-list--item-id ()
  ,test
  (test)
  :doc "uses the visible skill name as the row id"
  (let ((skill (mevedel-skill--create :name "plugin:assist")))
    (should (equal (mevedel-skills-list--item-id skill)
                   "plugin:assist"))))

(mevedel-deftest mevedel-skills-list--status-cell ()
  ,test
  (test)
  :doc "formats enabled and disabled status cells"
  (let* ((user-dir (make-temp-file "mevedel-skills-status-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (skill (mevedel-skill--create :name "visible")))
    (unwind-protect
        (progn
          (should (equal (substring-no-properties
                          (mevedel-skills-list--status-cell skill))
                         "enabled"))
          (mevedel-skills--set-enabled skill nil)
          (should (equal (substring-no-properties
                          (mevedel-skills-list--status-cell skill))
                         "disabled")))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills-list--entry ()
  ,test
  (test)
  :doc "builds table cells from skill state, name, source, and description"
  (let* ((user-dir (make-temp-file "mevedel-skills-entry-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (skill (mevedel-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'plugin))
         (entry (unwind-protect
                    (progn
                      (mevedel-skills--set-enabled skill nil)
                      (mevedel-skills-list--entry skill))
                  (delete-directory user-dir t))))
    (should (equal (car entry) "visible"))
    (should (equal (substring-no-properties (aref (cadr entry) 0))
                   "disabled"))
    (should (equal (aref (cadr entry) 1) "visible"))
    (should (equal (aref (cadr entry) 2) "plugin"))
    (should (equal (aref (cadr entry) 3) "Visible description"))))

(mevedel-deftest mevedel-skills-list--session-label ()
  ,test
  (test)
  :doc "returns the rendered session name or unknown"
  (let ((session (mevedel-skills-test--make-session "named")))
    (with-temp-buffer
      (mevedel-skills-list-mode)
      (should (equal (mevedel-skills-list--session-label) "unknown"))
      (setq mevedel-skills-list--session session)
      (should (equal (mevedel-skills-list--session-label) "named")))))

(mevedel-deftest mevedel-skills-list--header-line ()
  ,test
  (test)
  :doc "summarizes the session skill count and key hints"
  (let* ((user-dir (make-temp-file "mevedel-skills-header-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (enabled (mevedel-skill--create :name "enabled"))
         (disabled (mevedel-skill--create :name "disabled")))
    (unwind-protect
        (with-temp-buffer
          (mevedel-skills-list-mode)
          (setq mevedel-skills-list--session session
                mevedel-skills-list--skills (list enabled disabled))
          (mevedel-skills--set-enabled disabled nil)
          (let ((line (mevedel-skills-list--header-line)))
            (should (string-match-p "1/2 enabled" line))
            (should (string-match-p "RET details" line))
            (should (string-match-p "q back" line))))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills-list--require-owner ()
  ,test
  (test)
  :doc "requires live cockpit owner buffers"
  (let ((view-buffer (generate-new-buffer " *skills-owner-view*"))
        (data-buffer (generate-new-buffer " *skills-owner-data*")))
    (unwind-protect
        (let ((buffer (mevedel-skills-test--open-list
                       (mevedel-skills-test--make-session)
                       view-buffer
                       data-buffer)))
          (with-current-buffer buffer
            (should (mevedel-skills-list--require-owner))
            (kill-buffer data-buffer)
            (should-error (mevedel-skills-list--require-owner)
                          :type 'user-error)))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer))))

(mevedel-deftest mevedel-skills--list-message-text ()
  ,test
  (test)
  :doc "formats a fallback list message for visible session skills"
  (let* ((user-dir (make-temp-file "mevedel-skills-message-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'project)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (should (string-match-p
                   "mevedel skills for main"
                   (mevedel-skills--list-message-text session)))
          (should (string-match-p
                   "visible \\[enabled\\] source:project - Visible description"
                   (mevedel-skills--list-message-text session))))
      (delete-directory user-dir t)))

  :doc "reports empty sessions"
  (let ((session (mevedel-skills-test--make-session)))
    (should (string-match-p "No skills available"
                            (mevedel-skills--list-message-text session)))))

(mevedel-deftest mevedel-skills-list-toggle-enabled ()
  ,test
  (test)
  :doc "toggles selected skill state and refreshes the owner view prompt"
  (let* ((user-dir (make-temp-file "mevedel-skills-toggle-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-toggle-view*"))
         (data-buffer (generate-new-buffer " *skills-toggle-data*"))
         (skill (mevedel-skill--create :name "visible" :source 'project))
         refreshed-buffer
         message-text)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (let ((buffer (mevedel-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id "visible")
              (cl-letf (((symbol-function 'mevedel-view-refresh-input-prompt)
                         (lambda ()
                           (setq refreshed-buffer (current-buffer))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (setq message-text (apply #'format fmt args)))))
                (mevedel-skills-list-toggle-enabled)
                (should-not (mevedel-skills--skill-enabled-p skill))
                (should (eq refreshed-buffer view-buffer))
                (should (equal (tabulated-list-get-id) "visible"))
                (should (string-match-p "disabled" message-text))
                (mevedel-skills-list-toggle-enabled)
                (should (mevedel-skills--skill-enabled-p skill))
                (should (string-match-p "enabled" message-text))))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills-list-open-source ()
  ,test
  (test)
  :doc "opens SKILL.md first and falls back to the source directory"
  (let* ((user-dir (make-temp-file "mevedel-skills-source-user-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (root (make-temp-file "mevedel-skills-source-root-" t))
         (source-file (mevedel-skills-test--write-skill
                       root "visible" "description: Visible\n"))
         (source-dir (file-name-directory source-file))
         (session (mevedel-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-source-view*"))
         (data-buffer (generate-new-buffer " *skills-source-data*"))
         (skill (mevedel-skill--create
                 :name "visible"
                 :source 'project
                 :source-file source-file
                 :source-dir source-dir))
         opened)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (let ((buffer (mevedel-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id "visible")
              (cl-letf (((symbol-function 'find-file)
                         (lambda (target &rest _)
                           (setq opened target))))
                (mevedel-skills-list-open-source)
                (should (equal opened source-file))
                (delete-file source-file)
                (mevedel-skills-list-open-source)
                (should (equal opened source-dir))))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills-list-help ()
  ,test
  (test)
  :doc "opens skills cockpit help"
  (unwind-protect
      (progn
        (mevedel-skills-list-help)
        (with-current-buffer mevedel-skills-help-buffer-name
          (should (string-match-p "RET  Show selected skill details"
                                  (buffer-string)))
          (should (string-match-p "/skills enable NAME"
                                  (buffer-string)))))
    (mevedel-skills-test--cleanup-list)))

(mevedel-deftest mevedel-skills-list-quit ()
  ,test
  (test)
  :doc "kills the skills cockpit and reopens the main session cockpit"
  (let* ((user-dir (make-temp-file "mevedel-skills-quit-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-quit-view*"))
         (data-buffer (generate-new-buffer " *skills-quit-data*"))
         (skill (mevedel-skill--create :name "visible"))
         called-buffer)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (let ((buffer (mevedel-skills-test--open-list
                         session view-buffer data-buffer)))
            (cl-letf (((symbol-function 'mevedel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (with-current-buffer buffer
                (mevedel-skills-list-quit)))
            (should-not (buffer-live-p buffer))
            (should (eq called-buffer data-buffer))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills--gptel-send-advice ()
  ,test
  (test)
  :doc "local command aborts the send (orig-fn not called)"
  ;; The advice is `:around', so we assert behavior by checking whether
  ;; the original send is called.
  (let ((session (mevedel-skills-test--make-session))
        (orig-called nil))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (_args) nil)))))
        (insert "### /noop")
        (goto-char (point-max))
        (mevedel-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t)))
        (should-not orig-called))))

  :doc "unknown slash command aborts the send"
  (let ((session (mevedel-skills-test--make-session))
        (orig-called nil))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /bogus")
        (goto-char (point-max))
        (mevedel-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t)))
        (should-not orig-called))))

  :doc "expanded skill lets the send proceed"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "hi" :body "Hi!"))
         (orig-called nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /hi")
        (goto-char (point-max))
        (mevedel-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t)))
        (should orig-called))))

  :doc "plain text always lets the send proceed"
  (let ((session (mevedel-skills-test--make-session))
        (orig-called nil))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### just a normal message")
      (goto-char (point-max))
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called)))

  :doc "no session: advice still calls orig-fn"
  (let ((orig-called nil))
    (with-temp-buffer
      (setq mevedel--session nil)
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called)))

  :doc "stash leaks are cleared after advice returns"
  ;; `unwind-protect' clears the pending stash if the begin handler did
  ;; not drain it (e.g. because gptel-send aborted before WAIT).
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### plain text")
      (goto-char (point-max))
      ;; Simulate a leaked stash (e.g., from a prior failed dispatch).
      (setq-local mevedel-skills--pending-request-context
                  '(:permission-rules nil :model haiku))
      (mevedel-skills--gptel-send-advice (lambda (&rest _) nil))
      (should (null mevedel-skills--pending-request-context))))

  :doc "stash leaks cleared even when orig-fn signals an error"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### plain text")
      (goto-char (point-max))
      (setq-local mevedel-skills--pending-request-context
                  '(:permission-rules nil :model haiku))
	      (ignore-errors
	        (mevedel-skills--gptel-send-advice
	         (lambda (&rest _) (error "Boom"))))
      (should (null mevedel-skills--pending-request-context)))))

(mevedel-deftest mevedel-slash-capf ()
  ,test
  (test)
  :doc "global local commands include plugin and skills"
  (let ((session (mevedel-skills-test--make-session)))
    (should (eq 'mevedel-plugins-slash-command
                (cdr (assoc "plugin" mevedel-slash-commands))))
    (should (eq 'mevedel-cmd--skills
                (cdr (assoc "skills" mevedel-slash-commands))))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### /pl")
      (goto-char (point-max))
      (let* ((capf (mevedel-slash-capf))
             (cands (and capf
                         (mevedel-skills-test--capf-candidates capf "pl")))
             (annot (and capf (plist-get (nthcdr 3 capf)
                                         :annotation-function))))
        (should capf)
        (should (member "plugin" cands))
        (should (string-match-p "list" (funcall annot "plugin"))))))

  :doc "returns local commands and session skills as candidates"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "simplify")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("help" . ignore)
                                      ("tokens" . ignore))))
        (insert "### /")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (cands (and capf
                           (mevedel-skills-test--capf-candidates capf)))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (member "help" cands))
          (should (member "tokens" cands))
          (should (member "simplify" cands))
          (should (equal " [command] no args; list commands and skills"
                         (funcall annot "help")))
          (should (equal " [skill]" (funcall annot "simplify")))))))

  :doc "plugin-prefixed skill candidates complete as one slash name"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "superpowers:brainstorming")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /superpowers:b")
        (goto-char (point-max))
        (let ((capf (mevedel-slash-capf)))
          (should capf)
          (should (equal '("superpowers:brainstorming")
                         (mevedel-skills-test--capf-candidates
                          capf "superpowers:b")))))))

  :doc "special command annotations describe argument behavior"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             '(("tokens" . ignore)
               ("model" . ignore)
               ("compact" . ignore)
               ("plan" . ignore)
               ("mode" . ignore)
               ("auto" . ignore)
               ("clear" . ignore)
               ("help" . ignore)
               ("init" . ignore)
               ("plugin" . ignore)
               ("tools" . ignore)
               ("review" . ignore)
               ("verify" . ignore))))
        (insert "### /")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (dolist (name (mapcar #'car mevedel-slash-commands))
            (should (string-prefix-p " [command]" (funcall annot name)))
            (should-not (equal " [command]" (funcall annot name))))
          (should (string-match-p "default" (funcall annot "mode")))
          (should (string-match-p "list" (funcall annot "plugin")))
          (should (string-match-p "update" (funcall annot "plugin")))
          (should (string-match-p "remove" (funcall annot "plugin")))
          (should (string-match-p "uninstall" (funcall annot "plugin")))
          (should (string-match-p "reload" (funcall annot "plugin")))
          (should (string-match-p "list" (funcall annot "tools")))
          (should (string-match-p "target args"
                                  (funcall annot "review")))
          (should (string-match-p "target args"
                                  (funcall annot "verify")))))))

  :doc "mode command completes first argument options"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("mode" . ignore))))
        (insert "### /mode pl")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("plan")
                         (mevedel-skills-test--capf-candidates
                          capf "pl")))
          (should (member "accept-edits"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (string-match-p "read-only"
                                  (funcall annot "plan")))))))

  :doc "plugin command completes first argument options"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             '(("plugin" . mevedel-plugins-slash-command))))
        (insert "### /plugin l")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("list")
                         (mevedel-skills-test--capf-candidates
                          capf "l")))
          (dolist (candidate '("enable" "disable" "hooks" "install"
                               "update" "reload"))
            (should (member candidate
                            (mevedel-skills-test--capf-candidates capf))))
          (should (string-match-p "installed"
                                  (funcall annot "list")))
          (should (string-match-p "installed plugin"
                                  (funcall annot "update")))
          (should (string-match-p "current session"
                                  (funcall annot "reload")))))))

  :doc "plugin command completes installed plugin names for update"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "mevedel-skills-plugin-capf-" t)))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-plugin-extra-roots nil)
         (session (mevedel-skills-test--make-session)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands
                   '(("plugin" . mevedel-plugins-slash-command))))
              (insert "### /plugin update de")
              (goto-char (point-max))
              (let* ((capf (mevedel-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (equal '("demo")
                               (mevedel-skills-test--capf-candidates
                                capf "de")))
                (should (member "other"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (string-match-p "installed plugin"
                                        (funcall annot "demo")))))))
      (delete-directory user-dir t)))

  :doc "plugin command completes installed plugin names for state and removal commands"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "mevedel-skills-plugin-capf-" t)))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-plugin-extra-roots nil)
         (session (mevedel-skills-test--make-session)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands
                   '(("plugin" . mevedel-plugins-slash-command))))
              (insert "### /plugin enable ")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should capf)
                (should (member "demo"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (member "other"
                                (mevedel-skills-test--capf-candidates capf))))
              (erase-buffer)
              (insert "### /plugin disable o")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (mevedel-skills-test--capf-candidates
                                capf "o"))))
              (erase-buffer)
              (insert "### /plugin remove d")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should capf)
                (should (equal '("demo")
                               (mevedel-skills-test--capf-candidates
                                capf "d"))))
              (erase-buffer)
              (insert "### /plugin uninstall o")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (mevedel-skills-test--capf-candidates
                                capf "o")))))))
      (delete-directory user-dir t)))

  :doc "plugin hooks command completes actions, plugin names, and states"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "mevedel-skills-plugin-capf-" t)))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-plugin-extra-roots nil)
         (session (mevedel-skills-test--make-session)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (mevedel-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands
                   '(("plugin" . mevedel-plugins-slash-command))))
              (insert "### /plugin hooks ")
              (goto-char (point-max))
              (let* ((capf (mevedel-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (member "enable"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (member "disable"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (member "demo"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (string-match-p "hook command"
                                        (funcall annot "enable")))
                (should (string-match-p "installed plugin"
                                        (funcall annot "demo"))))
              (erase-buffer)
              (insert "### /plugin hooks enable o")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (mevedel-skills-test--capf-candidates
                                capf "o"))))
              (erase-buffer)
              (insert "### /plugin hooks demo ")
              (goto-char (point-max))
              (let* ((capf (mevedel-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (member "on"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (member "off"
                                (mevedel-skills-test--capf-candidates capf)))
                (should (string-match-p "hook state"
                                        (funcall annot "on")))))))
      (delete-directory user-dir t)))

  :doc "plugin install target remains freeform"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             '(("plugin" . mevedel-plugins-slash-command))))
        (insert "### /plugin install ")
        (goto-char (point-max))
        (should (null (mevedel-slash-capf))))))

  :doc "skills command completes subcommands"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("skills" . ignore))))
        (insert "### /skills dis")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("disable")
                         (mevedel-skills-test--capf-candidates
                          capf "dis")))
          (should (member "enable"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (string-match-p "disable"
                                  (funcall annot "disable")))))))

  :doc "model command completes current backend model names"
  (mevedel-skills-test--with-model-backends
    (let ((session (mevedel-skills-test--make-session))
          (gptel-backend (gptel-get-backend "Fast"))
          (gptel-model 'manual-model))
      (mevedel-skills-test--with-chat-buffer session
        (let ((mevedel-slash-commands '(("model" . ignore))))
          (insert "### /model f")
          (goto-char (point-max))
          (let ((capf (mevedel-slash-capf)))
            (should capf)
            (should (equal '("fast-model")
                           (mevedel-skills-test--capf-candidates
                            capf "f")))
            (should (member "manual-model"
                            (mevedel-skills-test--capf-candidates
                             capf))))))))

  :doc "review and verify commands complete shared target arguments"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("review" . ignore)
                                      ("verify" . ignore))))
        (insert "### /review cur")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("current")
                         (mevedel-skills-test--capf-candidates capf "cur")))
          (should (member "commit:"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (string-match-p "current changes"
                                  (funcall annot "current"))))
        (erase-buffer)
        (insert "### /verify com")
        (goto-char (point-max))
        (let ((capf (mevedel-slash-capf)))
          (should capf)
          (should (equal '("commit:")
                         (mevedel-skills-test--capf-candidates
                          capf "com")))))))

  :doc "worktree command completes status and create arguments"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("worktree" . ignore))))
        (insert "### /worktree c")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("create")
                         (mevedel-skills-test--capf-candidates capf "c")))
          (should (member "status"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (equal " command" (funcall annot "create")))))))

  :doc "root completion inserts a real separator before ghost hints"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "remember"
                 :argument-names '("focus"))))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /rem")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (exit (and capf (plist-get (nthcdr 3 capf)
                                          :exit-function))))
          (delete-region (nth 0 capf) (nth 1 capf))
          (insert "remember")
          (funcall exit "remember" 'finished)
          (insert "d")
          (should (equal "### /remember d"
                         (buffer-substring-no-properties
                          (point-min) (point-max))))))))

  :doc "returns nil when point is not right after a slash"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### hello world")
      (goto-char (point-max))
      (should (null (mevedel-slash-capf)))))

  :doc "returns nil when no session is present"
  (with-temp-buffer
    (setq mevedel--session nil)
    (insert "/hel")
    (goto-char (point-max))
    (should (null (mevedel-slash-capf))))

  :doc "user-invocable: false skills are omitted from completion"
  ;; User-invocable false skills are not shown in slash completion.
  (let* ((session (mevedel-skills-test--make-session))
         (visible (mevedel-skill--create :name "visible"))
         (hidden (mevedel-skill--create :name "hidden"
                                        :user-invocable-p nil)))
    (setf (mevedel-session-skills session) (list visible hidden))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (cands (and capf
                           (mevedel-skills-test--capf-candidates capf))))
          (should (member "visible" cands))
          (should-not (member "hidden" cands))))))

  :doc "disabled skills are omitted from completion"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (visible (mevedel-skill--create :name "visible"))
         (hidden (mevedel-skill--create :name "hidden")))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list visible hidden))
          (mevedel-skills--set-enabled "hidden" nil)
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands nil))
              (insert "### /")
              (goto-char (point-max))
              (let* ((capf (mevedel-slash-capf))
                     (cands (and capf
                                 (mevedel-skills-test--capf-candidates capf))))
                (should (member "visible" cands))
                (should-not (member "hidden" cands))))))
      (delete-directory user-dir t)))

  :doc "[dormant] annotation appears for path-scoped not-yet-active skills"
  ;; Dormant skills get an annotation.
  (let* ((session (mevedel-skills-test--make-session))
         (active (mevedel-skill--create :name "ready"))
         (dormant (mevedel-skill--create
                   :name "lazy"
                   :path-patterns '("*.el")
                   :active-p nil)))
    (setf (mevedel-session-skills session) (list active dormant))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should (equal " [skill]" (funcall annot "ready")))
          (should (equal " [skill] [dormant]" (funcall annot "lazy")))))))

  :doc "argument hints appear after the [skill] annotation"
  ;; Annotation includes argument hint from `argument-hint' or generated
  ;; from `arguments'.
  (let* ((session (mevedel-skills-test--make-session))
         (with-hint (mevedel-skill--create :name "find"
                                           :argument-hint "[query]"))
         (with-args (mevedel-skill--create :name "review"
                                           :argument-names '("path" "depth"))))
    (setf (mevedel-session-skills session) (list with-hint with-args))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should (equal " [skill] [query]" (funcall annot "find")))
          (should (equal " [skill] [path] [depth]"
                         (funcall annot "review"))))))))

  :doc "candidate table refreshes after external create and delete"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-capf-hot-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-capf-hot*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (let ((gptel-prompt-prefix-alist
                   (cons (cons major-mode "### ")
                         gptel-prompt-prefix-alist)))
              (setq-local mevedel--session session)
              (mevedel-skills-install session buf)
              (insert "### /")
              (goto-char (point-max))
              (let ((capf (mevedel-slash-capf)))
                (should (member "alpha"
                                (mevedel-skills-test--capf-candidates
                                 capf)))
                (mevedel-skills-test--write-skill
                 root "bar" "name: bar\ndescription: B\n")
                (mevedel-skills--mark-buffer-dirty buf)
                (should (member "bar"
                                (mevedel-skills-test--capf-candidates
                                 capf "b")))
                (delete-directory (file-name-concat root "bar") t)
                (mevedel-skills--mark-buffer-dirty buf)
                (should-not (member "bar"
                                    (mevedel-skills-test--capf-candidates
                                     capf "b")))))))
      (when (buffer-live-p buf)
        (mevedel-skills--unregister-buffer buf)
        (kill-buffer buf))
      (delete-directory root t)))

(mevedel-deftest mevedel-skills--progressive-argument-hint ()
  ,test
  (test)
  :doc "argument-hint string takes precedence"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-hint "[query]"
                :argument-names '("a" "b"))))
    (should (equal "[query]"
                   (mevedel-skills--progressive-argument-hint skill))))

  :doc "argument-names produces bracketed sequence when no hint set"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("alpha" "beta" "gamma"))))
    (should (equal "[alpha] [beta] [gamma]"
                   (mevedel-skills--progressive-argument-hint skill))))

  :doc "no hint and no names returns nil"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (null (mevedel-skills--progressive-argument-hint skill)))))

(mevedel-deftest mevedel-skills--remaining-argument-hint ()
  ,test
  (test)
  :doc "argument-hint appears until arguments are typed"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-hint "What should be reviewed?")))
    (should (equal "What should be reviewed?"
                   (mevedel-skills--remaining-argument-hint skill "")))
    (should (equal "What should be reviewed?"
                   (mevedel-skills--remaining-argument-hint skill nil)))
    (should (null (mevedel-skills--remaining-argument-hint
                   skill "current changes"))))

  :doc "argument names display only remaining slots"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("service" "environment" "region"))))
    (should (equal "[service] [environment] [region]"
                   (mevedel-skills--remaining-argument-hint skill "")))
    (should (equal "[environment] [region]"
                   (mevedel-skills--remaining-argument-hint
                    skill "billing")))
    (should (equal "[region]"
                   (mevedel-skills--remaining-argument-hint
                    skill "\"billing api\" staging")))
    (should (null (mevedel-skills--remaining-argument-hint
                   skill "billing staging us")))))

(mevedel-deftest mevedel-cmd--model ()
  ,test
  (test)
  :doc "with args sets buffer-local gptel-model to interned symbol"
  (with-temp-buffer
    (setq-local gptel-model 'default)
    (mevedel-cmd--model "claude-opus-4-6")
    (should (eq 'claude-opus-4-6 gptel-model)))

  :doc "with unresolved colon args preserves direct bare model behavior"
  (with-temp-buffer
    (setq-local gptel-model 'default)
    (mevedel-cmd--model "llama3.1:8b")
    (should (eq 'llama3.1:8b gptel-model)))

  :doc "with provider args sets buffer-local backend and model"
  (mevedel-skills-test--with-model-backends
    (with-temp-buffer
      (mevedel-cmd--model "Fast:fast-model")
      (should (equal "Fast" (gptel-backend-name gptel-backend)))
      (should (eq 'fast-model gptel-model))))

  :doc "with blank args does not change the model"
  (with-temp-buffer
    (let ((gptel-model 'keep))
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (_area) (user-error "No cockpit"))))
        (mevedel-cmd--model ""))
      (should (eq 'keep gptel-model)))))

(mevedel-deftest mevedel-cmd--mode ()
  ,test
  (test)
  :doc "sets a recognized permission mode via setopt; updates session slot"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'default)))
            (setq-local mevedel--session session)
            (mevedel-cmd--mode "plan")
            (should (eq 'plan (mevedel-session-permission-mode session)))
            (should (eq 'plan mevedel-permission-mode))
            (should (eq 'default
                        (plist-get
                         (mevedel-session-plan-metadata session)
                         :previous-permission-mode)))
            (should (memq 'plan-mode
                          (mapcar #'mevedel-reminder-type
                                  (mevedel-session-reminders session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "accepts UI aliases"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "test" :permission-mode 'default)))
      (setq-local mevedel--session session)
      (mevedel-cmd--mode "edit")
      (should (eq 'accept-edits
                  (mevedel-session-permission-mode session)))
      (mevedel-cmd--mode "auto")
      (should (eq 'trust-all
                  (mevedel-session-permission-mode session)))
      (should (memq 'auto-mode
                    (mapcar #'mevedel-reminder-type
                            (mevedel-session-reminders session))))))

  :doc "leaving trust-all via /mode installs auto exit reminder"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "test" :permission-mode 'trust-all)))
      (setf (mevedel-session-reminders session)
            (list (mevedel-reminders-make-auto-mode)))
      (setq-local mevedel--session session)
      (mevedel-cmd--mode "default")
      (let ((types (mapcar #'mevedel-reminder-type
                           (mevedel-session-reminders session))))
        (should (eq 'default
                    (mevedel-session-permission-mode session)))
        (should-not (memq 'auto-mode types))
        (should (memq 'auto-mode-exit types)))))

  :doc "rejects unknown modes"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'default)
    (should-error (mevedel-cmd--mode "bogus") :type 'user-error)
    (should (eq 'default mevedel-permission-mode)))

  :doc "blank args leaves the mode unchanged"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'accept-edits)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (_area) (user-error "No cockpit"))))
      (mevedel-cmd--mode ""))
    (should (eq 'accept-edits mevedel-permission-mode))))

(mevedel-deftest mevedel-cmd--auto ()
  ,test
  (test)
  :doc "toggles trust-all on and installs auto-mode reminder"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'default)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'trust-all
                        (mevedel-session-permission-mode session)))
            (should (memq 'auto-mode
                          (mapcar #'mevedel-reminder-type
                                  (mevedel-session-reminders session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "toggles trust-all off and installs one-shot exit reminder"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'trust-all)))
            (setf (mevedel-session-reminders session)
                  (list (mevedel-reminders-make-auto-mode)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'default
                        (mevedel-session-permission-mode session)))
            (let ((types (mapcar #'mevedel-reminder-type
                                 (mevedel-session-reminders session))))
              (should-not (memq 'auto-mode types))
              (should (memq 'auto-mode-exit types)))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "toggles trust-all on from plan mode through plan exit"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'plan
                          :plan-metadata
                          '(:previous-permission-mode default))))
            (setf (mevedel-session-reminders session)
                  (list (mevedel-reminders-make-plan-mode)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'trust-all
                        (mevedel-session-permission-mode session)))
            (let ((types (mapcar #'mevedel-reminder-type
                                 (mevedel-session-reminders session))))
              (should-not (memq 'plan-mode types))
              (should (memq 'plan-mode-exit types))
              (should (memq 'auto-mode types)))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))


;;
;;; Phase D — skills listing reminder and conditional activation

(mevedel-deftest mevedel-skills--listing-describe ()
  ,test
  (test)
  :doc "short entries are returned as-is"
  (let ((skill (mevedel-skill--create
                :name "simplify"
                :description "Review changed code for reuse")))
    (should (equal "- simplify: Review changed code for reuse"
                   (mevedel-skills--listing-describe skill))))

  :doc "entries longer than the cap are truncated with ellipsis"
  (let* ((mevedel-skills-listing-max-entry-chars 20)
         (skill (mevedel-skill--create
                 :name "n"
                 :description "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
         (entry (mevedel-skills--listing-describe skill)))
    (should (= 20 (length entry)))
    (should (string-suffix-p "..." entry)))

  :doc "when_to_use suffix appended after the description"
  ;; Entry format: `- name: desc - when_to_use'.
  (let ((skill (mevedel-skill--create
                :name "demo" :description "Do a thing"
                :when-to-use "when the user asks for a thing")))
    (should (equal "- demo: Do a thing - when the user asks for a thing"
                   (mevedel-skills--listing-describe skill))))

  :doc "empty when_to_use does not introduce a trailing dash"
  (let ((skill (mevedel-skill--create
                :name "demo" :description "Body"
                :when-to-use "")))
    (should (equal "- demo: Body"
                   (mevedel-skills--listing-describe skill)))))

(mevedel-deftest mevedel-skills--listing-candidates ()
  ,test
  (test)
  :doc "returns only active, model-invocable skills"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "l" :root "/tmp/l" :name "l"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (active-invocable
          (mevedel-skill--create :name "a" :description "A"
                                 :model-invocable-p t :active-p t))
         (dormant
          (mevedel-skill--create :name "b" :description "B"
                                 :model-invocable-p t
                                 :path-patterns '("*.el")
                                 :active-p nil))
         (disabled
          (mevedel-skill--create :name "c" :description "C"
                                 :model-invocable-p nil :active-p t)))
    (setf (mevedel-session-skills session)
          (list active-invocable dormant disabled))
    (let ((names (mapcar #'mevedel-skill-name
                         (mevedel-skills--listing-candidates session))))
      (should (equal '("a") names))))

  :doc "omits user-disabled skills"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (enabled (mevedel-skill--create
                   :name "enabled" :description "E"
                   :model-invocable-p t :active-p t))
         (disabled (mevedel-skill--create
                    :name "disabled" :description "D"
                    :model-invocable-p t :active-p t)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list enabled disabled))
          (mevedel-skills--set-enabled "disabled" nil)
          (let ((names (mapcar #'mevedel-skill-name
                               (mevedel-skills--listing-candidates session))))
            (should (equal '("enabled") names))))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills--format-listing ()
  ,test
  (test)
  :doc "includes header and one line per skill"
  (let* ((skills (list (mevedel-skill--create :name "s1" :description "d1")
                       (mevedel-skill--create :name "s2" :description "d2")))
         (listing (mevedel-skills--format-listing skills)))
    (should (string-match-p "for use with the Skill tool" listing))
    (should (string-match-p "ListSkills" listing))
    (should (string-match-p "^- s1: d1$" listing))
    (should (string-match-p "^- s2: d2$" listing)))

  :doc "budget limits the number of entries emitted"
  (let* ((mevedel-skills-listing-budget 0.0001)
         (mevedel-skills-listing-max-entry-chars 250)
         (skills (list (mevedel-skill--create :name "s1" :description "d1")
                       (mevedel-skill--create :name "s2" :description "d2")
                       (mevedel-skill--create :name "s3" :description "d3")))
         (listing (mevedel-skills--format-listing skills)))
    (should (string-match-p "^- s1: d1$" listing))
    (should-not (string-match-p "^- s3: d3$" listing))))

(mevedel-deftest mevedel-reminders-make-skills-listing ()
  ,test
  (test)
  :doc "returns a firing reminder when model-invocable, active skills exist"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r" :root "/tmp/r" :name "r"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (should (eq 'skills-listing (mevedel-reminder-type reminder)))
    ;; No skills yet: trigger returns nil.
    (should-not (funcall (mevedel-reminder-trigger reminder) session))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "simplify"
                 :description "Review code"
                 :active-p t
                 :model-invocable-p t)))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p "simplify: Review code" body))))

  :doc "disabled-only skills do not cause firing"
  ;; When all skills are model-invocable=nil, the listing has nothing
  ;; to say.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r2" :root "/tmp/r2" :name "r2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "disabled" :description "d"
                 :model-invocable-p nil :active-p t)))
    (should-not (funcall (mevedel-reminder-trigger reminder) session)))

  :doc "dormant model-invocable skills cause firing for the dormant note"
  ;; When the only model-invocable skills are dormant (path-scoped, not
  ;; yet activated), the reminder still fires so it can append the fixed
  ;; dormant-skill note telling the model that direct-by-name invocation
  ;; works.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r3" :root "/tmp/r3" :name "r3"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "dormant" :description "d"
                 :model-invocable-p t :active-p nil
                 :path-patterns '("*.el"))))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p "Additional path-scoped skills" body))
      ;; The dormant skill itself is NOT listed (active-p nil).
      (should-not (string-match-p "dormant: d" body))))

  :doc "listing entry includes when_to_use when set"
  ;; Entry format: `- name: description - when_to_use'.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r4" :root "/tmp/r4" :name "r4"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "demo" :description "Do a thing"
                 :when-to-use "when the user asks for a thing"
                 :active-p t :model-invocable-p t)))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p
               "demo: Do a thing - when the user asks for a thing"
               body))))

  :doc "listing ordering: user > project > bundled"
  ;; Listing precedence.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r5" :root "/tmp/r5" :name "r5"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (setf (mevedel-session-skills session)
          ;; Insert in the OPPOSITE order to verify sorting actually
          ;; reorders rather than relying on insertion order.
          (list (mevedel-skill--create
                 :name "bundled-skill" :description "B"
                 :source 'bundled :active-p t :model-invocable-p t)
                (mevedel-skill--create
                 :name "project-skill" :description "P"
                 :source 'project :active-p t :model-invocable-p t)
                (mevedel-skill--create
                 :name "user-skill" :description "U"
                 :source 'user :active-p t :model-invocable-p t)))
    (let* ((body (funcall (mevedel-reminder-content reminder) session))
           (user-pos (string-match-p "user-skill" body))
           (project-pos (string-match-p "project-skill" body))
           (bundled-pos (string-match-p "bundled-skill" body)))
      (should (and user-pos project-pos bundled-pos))
      (should (< project-pos user-pos))
      (should (< user-pos bundled-pos)))))

(mevedel-deftest mevedel-reminders-make-skills-listing/hot-reload
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "trigger refreshes dirty skills from the chat buffer, not prompt buffer"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-skills-reminder-hot-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-reminder-hot*"))
         (reminder (mevedel-reminders-make-skills-listing)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session buf))
          (should (= 1 (length (mevedel-session-skills session))))
          (mevedel-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (mevedel-skills--mark-buffer-dirty buf)
          ;; Reminder triggers run from gptel's temporary prompt buffer.
          (let ((mevedel-reminders--current-chat-buffer buf))
            (with-temp-buffer
              (should (funcall (mevedel-reminder-trigger reminder) session))))
          (should (= 2 (length (mevedel-session-skills session))))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          (should (cl-find "beta" (mevedel-session-skills session)
                           :key #'mevedel-skill-name :test #'equal)))
      (kill-buffer buf)
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--glob-matches-p ()
  ,test
  (test)
  :doc "matches against basename and full path"
  (should (mevedel-skills--glob-matches-p "*.el" "foo.el"))
  (should (mevedel-skills--glob-matches-p "*.el" "/tmp/foo.el"))
  (should-not (mevedel-skills--glob-matches-p "*.py" "foo.el")))

(mevedel-deftest mevedel-skills--path-matches-p ()
  ,test
  (test)
  :doc "returns non-nil when any pattern matches"
  (should (mevedel-skills--path-matches-p
           "foo.el" '("*.py" "*.el")))

  :doc "nil for empty pattern list or nil path"
  (should-not (mevedel-skills--path-matches-p "foo.el" nil))
  (should-not (mevedel-skills--path-matches-p nil '("*.el"))))

(mevedel-deftest mevedel-skills--maybe-activate ()
  ,test
  (test)
  :doc "flips dormant skills whose patterns match path"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "a" :root "/tmp/a" :name "a"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (elisp (mevedel-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil))
         (python (mevedel-skill--create
                  :name "python" :path-patterns '("*.py") :active-p nil))
         (always (mevedel-skill--create :name "always" :active-p t)))
    (setf (mevedel-session-skills session) (list elisp python always))
    (let ((activated (mevedel-skills--maybe-activate session "src/foo.el")))
      (should (equal '("elisp")
                     (mapcar #'mevedel-skill-name activated))))
    (should (mevedel-skill-active-p elisp))
    (should-not (mevedel-skill-active-p python))
    ;; Second call on the same path no-ops.
    (should (null (mevedel-skills--maybe-activate session "src/bar.el")))))

(mevedel-deftest mevedel-skills--post-tool-activate ()
  ,test
  (test)
  :doc "activates conditional skills using the tool's get-path slot"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "p" :root "/tmp/p" :name "p"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil))
         (fake-tool (mevedel-tool--create
                     :name "Read"
                     :handler #'ignore
                     :get-path (lambda (args) (plist-get args :path)))))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-tool-get)
               (lambda (_name &optional _cat) fake-tool)))
      (with-temp-buffer
        (setq mevedel--session session)
        (mevedel-skills--post-tool-activate
         (list :name "Read" :args '(:path "lib/foo.el")))
        (should (mevedel-skill-active-p skill))))))



;;
;;; Hot reload (modification detection)

(defun mevedel-skills-test--reset-watchers ()
  "Clear the modification-detection global registries.
Called from `:before-each' so cross-test bleed cannot occur."
  (maphash (lambda (_dir desc)
             (ignore-errors (file-notify-rm-watch desc)))
           mevedel-skills--watchers)
  (clrhash mevedel-skills--watchers)
  (clrhash mevedel-skills--dir-buffers)
  (clrhash mevedel-skills--dirty-buffers)
  (clrhash mevedel-skills--mtime-cache))

(defun mevedel-skills-test--wait-for (predicate &optional timeout)
  "Drain notifications until PREDICATE is non-nil or TIMEOUT elapses.
TIMEOUT defaults to 2 seconds.  Return the last predicate value."
  (let ((deadline (+ (float-time) (or timeout 2.0)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    result))

(mevedel-deftest mevedel-skills--before-save-hook
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "saving a SKILL.md under a registered dir marks consumers dirty"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-skills-save-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-save*"))
         (skill-file (mevedel-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          ;; A save in a non-skill file should NOT flip the flag.
          (let ((unrelated (make-temp-file "mevedel-unrelated-" nil ".md")))
            (with-temp-buffer
              (setq buffer-file-name unrelated)
              (mevedel-skills--before-save-hook)
              (should-not (gethash buf mevedel-skills--dirty-buffers)))
            (delete-file unrelated))
          ;; Save of a SKILL.md under the root flips the flag.
          (with-temp-buffer
            (setq buffer-file-name skill-file)
            (mevedel-skills--before-save-hook)
            (should (gethash buf mevedel-skills--dirty-buffers))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "saving a nested SKILL.md marks every containing registered dir dirty"
  (let* ((root (make-temp-file "mevedel-skills-save-overlap-" t))
         (parent (file-name-as-directory
                  (file-name-concat root ".mevedel/skills")))
         (child (file-name-as-directory
                 (file-name-concat parent "team")))
         (parent-buf (generate-new-buffer
                      " *mevedel-test-save-overlap-parent*"))
         (child-buf (generate-new-buffer
                     " *mevedel-test-save-overlap-child*"))
         (skill-file (file-name-concat child "foo/SKILL.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory skill-file) t)
          (puthash parent (list parent-buf) mevedel-skills--dir-buffers)
          (puthash child (list child-buf) mevedel-skills--dir-buffers)
          (with-temp-buffer
            (setq buffer-file-name skill-file)
            (mevedel-skills--before-save-hook))
          (should (gethash parent-buf mevedel-skills--dirty-buffers))
          (should (gethash child-buf mevedel-skills--dirty-buffers)))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
      (when (buffer-live-p child-buf) (kill-buffer child-buf))
      (delete-directory root t)))

  :doc "ensure-fresh consumes the dirty flag and rescans new skills"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-skills-rescan-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-rescan*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should (= 1 (length (mevedel-session-skills session))))
          ;; New skill appears on disk; flip dirty flag and pull.
          (mevedel-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (mevedel-skills--mark-buffer-dirty buf)
          (mevedel-skills--ensure-fresh buf session)
          (should (= 2 (length (mevedel-session-skills session))))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          (should (cl-find "beta" (mevedel-session-skills session)
                           :key #'mevedel-skill-name :test #'equal)))
      (kill-buffer buf)
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--watch-callback
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "callback marks consumers of the firing directory dirty"
  (let* ((dir (file-name-as-directory
               (expand-file-name "fake/" temporary-file-directory)))
         (buf (generate-new-buffer " *mevedel-test-watch*"))
         (descriptor 'fake-descriptor))
    (unwind-protect
        (progn
          (puthash dir descriptor mevedel-skills--watchers)
          (puthash dir (list buf) mevedel-skills--dir-buffers)
          ;; Synthetic event tuple matching `file-notify' shape.
          (mevedel-skills--watch-callback
           (list descriptor 'created (concat dir "alpha")))
          (should (gethash buf mevedel-skills--dirty-buffers))
          ;; `stopped' must not flip the flag.
          (remhash buf mevedel-skills--dirty-buffers)
          (mevedel-skills--watch-callback
           (list descriptor 'stopped (concat dir "alpha")))
          (should-not (gethash buf mevedel-skills--dirty-buffers)))
      (kill-buffer buf))))

(mevedel-deftest mevedel-skills--stat-recheck
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "external mtime change flips the buffer dirty"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "mevedel-skills-stat-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-stat*"))
         (skill-file (mevedel-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          ;; Bump mtime via set-file-times: deterministic, no need to
          ;; sleep for the filesystem clock granularity.
          (set-file-times skill-file (time-add (current-time) 5))
          (mevedel-skills--stat-recheck session buf)
          (should (gethash buf mevedel-skills--dirty-buffers)))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "one buffer's rescan does not hide stat changes from another buffer"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "mevedel-skills-stat-many-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session-a (mevedel-session-create "main-a" ws))
         (session-b (mevedel-session-create "main-b" ws))
         (buf-a (generate-new-buffer " *mevedel-test-stat-a*"))
         (buf-b (generate-new-buffer " *mevedel-test-stat-b*"))
         (skill-file (mevedel-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (mevedel-skills-install session-a buf-a))
          (with-current-buffer buf-b
            (mevedel-skills-install session-b buf-b))
          (set-file-times skill-file (time-add (current-time) 5))
          (mevedel-skills--stat-recheck session-a buf-a)
          (should (gethash buf-a mevedel-skills--dirty-buffers))
          ;; Rescanning A refreshes only A's stat baseline.
          (mevedel-skills--ensure-fresh buf-a session-a)
          (should-not (gethash buf-a mevedel-skills--dirty-buffers))
          ;; B still has its own older baseline and must become dirty.
          (mevedel-skills--stat-recheck session-b buf-b)
          (should (gethash buf-b mevedel-skills--dirty-buffers)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (delete-directory root t)))

  :doc "deleted known skill file flips the buffer dirty"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "mevedel-skills-stat-delete-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-stat-delete*"))
         (skill-file (mevedel-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          (delete-file skill-file)
          (mevedel-skills--stat-recheck session buf)
          (should (gethash buf mevedel-skills--dirty-buffers)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory root t)))

  :doc "unregistering a buffer clears only that buffer's stat baseline"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "mevedel-skills-stat-clear-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session-a (mevedel-session-create "main-a" ws))
         (session-b (mevedel-session-create "main-b" ws))
         (buf-a (generate-new-buffer " *mevedel-test-stat-clear-a*"))
         (buf-b (generate-new-buffer " *mevedel-test-stat-clear-b*"))
         (skill-file (mevedel-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (mevedel-skills-install session-a buf-a))
          (with-current-buffer buf-b
            (mevedel-skills-install session-b buf-b))
          (should (gethash (mevedel-skills--mtime-cache-key buf-a skill-file)
                           mevedel-skills--mtime-cache))
          (should (gethash (mevedel-skills--mtime-cache-key buf-b skill-file)
                           mevedel-skills--mtime-cache))
          (mevedel-skills--unregister-buffer buf-a)
          (should-not (gethash (mevedel-skills--mtime-cache-key buf-a skill-file)
                               mevedel-skills--mtime-cache))
          (should (gethash (mevedel-skills--mtime-cache-key buf-b skill-file)
                           mevedel-skills--mtime-cache)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-rescan
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "manual rescan picks up newly added skills"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-manual-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-manual*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session buf))
          (should (= 1 (length (mevedel-session-skills session))))
          (mevedel-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (with-current-buffer buf
            (mevedel-skills-rescan))
          (should (= 2 (length (mevedel-session-skills session)))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "errors when no session is bound in the buffer"
  (with-temp-buffer
    (should-error (mevedel-skills-rescan) :type 'user-error)))

(mevedel-deftest mevedel-skills--unregister-buffer
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "unregister removes a buffer from every registered directory"
  (let* ((buf-a (generate-new-buffer " *mevedel-test-unregister-a*"))
         (buf-b (generate-new-buffer " *mevedel-test-unregister-b*"))
         (root (file-name-as-directory
                (expand-file-name "mevedel-skills-unregister/"
                                  temporary-file-directory)))
         (parent (file-name-as-directory
                  (file-name-concat root ".mevedel/skills")))
         (child (file-name-as-directory
                 (file-name-concat parent "team"))))
    (unwind-protect
        (progn
          (puthash parent (list buf-a buf-b) mevedel-skills--dir-buffers)
          (puthash child (list buf-a) mevedel-skills--dir-buffers)
          (puthash buf-a t mevedel-skills--dirty-buffers)
          (mevedel-skills--unregister-buffer buf-a)
          (should (equal (gethash parent mevedel-skills--dir-buffers)
                         (list buf-b)))
          (should-not (gethash child mevedel-skills--dir-buffers))
          (should-not (gethash buf-a mevedel-skills--dirty-buffers)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(mevedel-deftest mevedel-skills--release-on-kill
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "kill drops the buffer from the registry and tears down orphan watchers"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-kill-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-kill*"))
         (dir (file-name-as-directory root)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (mevedel-skills-install session buf)
            (add-hook 'kill-buffer-hook
                      #'mevedel-skills--release-on-kill nil t))
          (should (memq buf (gethash dir mevedel-skills--dir-buffers)))
          (kill-buffer buf)
          (should-not (gethash dir mevedel-skills--dir-buffers))
          (should-not (gethash dir mevedel-skills--watchers)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-uninstall-hot-reload
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each
   (progn
     (remove-hook 'before-save-hook #'mevedel-skills--before-save-hook)
     (mevedel-skills-test--reset-watchers)))
  ,test
  (test)
  :doc "removes global hooks, buffer kill hooks, watchers, and registry state"
  (let* ((buf (generate-new-buffer " *mevedel-test-hot-uninstall*"))
         (dir (file-name-as-directory
               (expand-file-name "fake-skill-root/" temporary-file-directory)))
         (file (file-name-concat dir "alpha/SKILL.md"))
         (descriptor 'fake-descriptor))
    (unwind-protect
        (progn
          (add-hook 'before-save-hook #'mevedel-skills--before-save-hook)
          (with-current-buffer buf
            (add-hook 'kill-buffer-hook
                      #'mevedel-skills--release-on-kill nil t))
          (puthash dir descriptor mevedel-skills--watchers)
          (puthash dir (list buf) mevedel-skills--dir-buffers)
          (puthash buf t mevedel-skills--dirty-buffers)
          (puthash (mevedel-skills--mtime-cache-key buf file)
                   (current-time)
                   mevedel-skills--mtime-cache)
          (mevedel-skills-uninstall-hot-reload)
          (should-not (memq #'mevedel-skills--before-save-hook
                            before-save-hook))
          (should (= 0 (hash-table-count mevedel-skills--watchers)))
          (should (= 0 (hash-table-count mevedel-skills--dir-buffers)))
          (should (= 0 (hash-table-count mevedel-skills--dirty-buffers)))
          (should (= 0 (hash-table-count mevedel-skills--mtime-cache)))
          (with-current-buffer buf
            (should-not (memq #'mevedel-skills--release-on-kill
                              kill-buffer-hook))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(mevedel-deftest mevedel-skills--ensure-watcher
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "watch-files install creates a live descriptor for each registered dir"
  (progn
    (skip-unless (mevedel-skills--filenotify-supported-p))
    (let* ((mevedel-skills-include-bundled nil)
           (mevedel-skills-check-for-modifications '(watch-files))
           (root (make-temp-file "mevedel-skills-watcher-" t))
           (mevedel-skill-dirs (list root))
           (ws (mevedel-skills-test--make-workspace root))
           (session (mevedel-session-create "main" ws))
           (buf (generate-new-buffer " *mevedel-test-watcher*")))
      (unwind-protect
          (progn
            (mevedel-skills-test--write-skill
             root "alpha" "name: alpha\ndescription: A\n")
            (with-current-buffer buf
              (mevedel-skills-install session buf))
            (let* ((dir (file-name-as-directory (expand-file-name root)))
                   (desc (gethash dir mevedel-skills--watchers)))
              (should desc)
              (should (file-notify-valid-p desc))))
        (kill-buffer buf)
        (delete-directory root t))))

  :doc "watch-files registers existing empty subdirectories"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-empty-subdir-" t))
         (empty (file-name-as-directory (file-name-concat root "empty")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-empty-subdir*")))
    (unwind-protect
        (progn
          (make-directory empty t)
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should (memq buf (gethash (file-name-as-directory
                                      (expand-file-name root))
                                     mevedel-skills--dir-buffers)))
          (should (memq buf (gethash empty mevedel-skills--dir-buffers))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "registers missing configured roots and their existing parent"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-missing-root-" t))
         (missing (file-name-as-directory
                   (file-name-concat root ".mevedel/skills")))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-missing-root*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should (memq buf (gethash missing mevedel-skills--dir-buffers)))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name root))
                                 mevedel-skills--dir-buffers)))
          (should (mevedel-skills--file-under-watched-dirs
                   (file-name-concat missing "alpha/SKILL.md"))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "ensure-watcher is a no-op without watch-files in the strategy set"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skills-no-watch-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-no-watch*")))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (mevedel-skills-install session buf))
          (should-not (gethash (file-name-as-directory
                                (expand-file-name root))
                               mevedel-skills--watchers)))
      (kill-buffer buf)
      (delete-directory root t))))


(provide 'test-mevedel-skills)
;;; test-mevedel-skills.el ends here
