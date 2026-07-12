;;; test-mevedel-skills-core.el --- Skill core tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the skill model, discovery, installation, and hot reload core.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-hooks)
(require 'mevedel-plugins)
(require 'mevedel-skills-core)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-skills-core-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-skills-scan
                    mevedel-skills-install
                    mevedel-skills--ensure-fresh
                    mevedel-skills--maybe-activate))
    (should (equal "mevedel-skills-core"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


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
    (should (string-match-p
             "You cannot invoke slash commands yourself" body)))

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

  :doc "effort remains opaque until gptel validates the selected model"
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
effort: -1
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (good (cl-find "good-effort" skills
                                :key #'mevedel-skill-name :test #'equal))
                 (bad (cl-find "bad-effort" skills
                               :key #'mevedel-skill-name :test #'equal)))
            (should (eq 'high (mevedel-skill-effort good)))
            (should (= -1 (mevedel-skill-effort bad)))))
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

  :doc "allowed-tools validation works before installation and rejects unknown tools"
  (let* ((mevedel-skills-include-bundled nil)
         (dir (make-temp-file "mevedel-skills-preinstall-" t))
         (warnings nil))
    (unwind-protect
        (progn
          (mevedel-tool-clear-registry)
          (mevedel-skills-test--write-skill
           dir "known-tools"
           "name: known-tools
description: known
allowed-tools:
  - Skill(foo)
  - ListSkills
" "Body")
          (mevedel-skills-test--write-skill
           dir "unknown-tool"
           "name: unknown-tool
description: unknown
allowed-tools:
  - DoesNotExist
" "Body")
          (cl-letf* ((display-warning-orig
                      (symbol-function 'display-warning))
                     ((symbol-function 'display-warning)
                      (lambda (type message &rest args)
                        (when (eq type 'mevedel)
                          (push message warnings))
                        (apply display-warning-orig type message args))))
            (let* ((skills (mevedel-skills-scan dir '(".")))
                   (known (cl-find "known-tools" skills
                                   :key #'mevedel-skill-name :test #'equal)))
              (should known)
              (should
               (equal '(("Skill" :name "foo" :action allow)
                        ("ListSkills" :action allow))
                      (mevedel-skill-allowed-tool-rules known)))
              (should-not (cl-find "unknown-tool" skills
                                   :key #'mevedel-skill-name :test #'equal))
              (should (cl-some
                       (lambda (message)
                         (and (string-match-p "unknown-tool" message)
                              (string-match-p "DoesNotExist" message)))
                       warnings)))))
      (mevedel-tool-clear-registry)
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
;;; Persisted enablement

(mevedel-deftest mevedel-skills--set-enabled
  (:vars* ((user-dir (make-temp-file "mevedel-skills-state-" t))
           (mevedel-user-dir (file-name-as-directory user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "disable and enable persist file-backed skill state"
  (let ((skill (mevedel-skills-test--stateful-skill :name "visible")))
    (mevedel-skills--set-enabled skill nil)
    (should-not (mevedel-skills--skill-enabled-p skill))
    (mevedel-skills--set-enabled skill t)
    (should (mevedel-skills--skill-enabled-p skill)))

  :doc "disable rejects skills without a stable source file"
  (should-error
   (mevedel-skills--set-enabled
    (mevedel-skill--create :name "inline") nil)
   :type 'user-error)

  :doc "stable source identity survives generated renaming"
  (let* ((root (make-temp-file "mevedel-skills-state-root-" t))
         (skill-file (mevedel-skills-test--write-skill
                      root "shared" "description: Shared\n" "Body"))
         (other-file (mevedel-skills-test--write-skill
                      root "other" "description: Other\n" "Body"))
         (skill (mevedel-skill--create
                 :name "shared" :source-file skill-file))
         (renamed (mevedel-skill--create
                   :name "user:shared" :source-file skill-file))
         (other (mevedel-skill--create
                 :name "shared" :source-file other-file)))
    (unwind-protect
        (progn
          (mevedel-skills--set-enabled skill nil)
          (should (mevedel-skills--disabled-keys))
          (should-not (mevedel-skills--skill-enabled-p renamed))
          (should (mevedel-skills--skill-enabled-p other)))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--read-state
  (:vars* ((user-dir (make-temp-file "mevedel-skills-state-" t))
           (mevedel-user-dir (file-name-as-directory user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "obsolete name-based state is rejected"
  (progn
    (make-directory (file-name-directory (mevedel-skills--state-file)) t)
    (with-temp-file (mevedel-skills--state-file)
      (prin1 '(:disabled ("shared")) (current-buffer)))
    (should-error (mevedel-skills--read-state) :type 'error)))


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
      (delete-directory root t)))
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


;;
;;; Path-scoped activation

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



;;
;;; Modification detection and reload

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
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--ensure-fresh
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
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
)

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
      (when (buffer-live-p buf-b) (kill-buffer buf-b))))
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





(provide 'test-mevedel-skills-core)
;;; test-mevedel-skills-core.el ends here
