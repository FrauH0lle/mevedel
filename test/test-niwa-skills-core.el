;;; test-niwa-skills-core.el -- Skill core tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the skill model, discovery, installation, and hot reload core.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-hooks)
(require 'niwa-execution-target)
(require 'niwa-plugins)
(require 'niwa-session-durability)
(require 'niwa-skills-core)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-workspace)

(niwa-deftest niwa-skills-core-ownership ()
  ,test
  (test)
  (dolist (symbol '(niwa-skills-scan
                    niwa-skills-project-files
                    niwa-skills-install
                    niwa-skills-ensure-fresh
                    niwa-skills-maybe-activate))
    (should (equal "niwa-skills-core"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Plist coercion helpers

(niwa-deftest niwa-skills--coerce-bool ()
  ,test
  (test)
  :doc "nil falls through to the default"
  (should (eq (niwa-skills--coerce-bool nil t) t))
  (should (eq (niwa-skills--coerce-bool nil nil) nil))

  :doc "explicit booleans and yaml false sentinel"
  (should (eq (niwa-skills--coerce-bool t nil) t))
  (should (eq (niwa-skills--coerce-bool :false t) nil))

  :doc "string coercion for yaml.el scalar output"
  (should (eq (niwa-skills--coerce-bool "true" nil) t))
  (should (eq (niwa-skills--coerce-bool "false" t) nil))
  (should (eq (niwa-skills--coerce-bool "no" t) nil))
  (should (eq (niwa-skills--coerce-bool "yes" nil) t)))

(niwa-deftest niwa-skills--from-plist ()
  ,test
  (test)
  :doc "normalizes absent, scalar, and list paths and allowed tools"
  (dolist (pair '((nil . nil) ("ListSkills" . ("ListSkills"))
                  (("ListSkills" "Skill") . ("ListSkills" "Skill"))))
    (let ((skill (niwa-skills--from-plist
                  "demo" (list :paths (car pair) :allowed-tools (car pair))
                  "/demo/SKILL.md" 'user)))
      (should (equal (cdr pair) (niwa-skill-path-patterns skill)))
      (should (equal (cdr pair) (niwa-skill-allowed-tools skill)))
      (should (equal (mapcar (lambda (name) (list name :action 'allow))
                            (cdr pair))
                     (niwa-skill-allowed-tool-rules skill))))))

(niwa-deftest niwa-skills--coerce-context ()
  ,test
  (test)
  :doc "fork variants resolve to `fork', everything else is inline"
  (should (eq (niwa-skills--coerce-context "fork") 'fork))
  (should (eq (niwa-skills--coerce-context 'fork) 'fork))
  (should (eq (niwa-skills--coerce-context "inline") 'inline))
  (should (eq (niwa-skills--coerce-context nil) 'inline)))

(niwa-deftest niwa-skills--parse-ptc-primitives ()
  ,test
  (test)

  :doc "distinguishes an omitted restriction from an explicit empty one"
  (progn
    (should (eq :unrestricted
                (niwa-skills--parse-ptc-primitives nil "SKILL.md")))
    (should (null
             (niwa-skills--parse-ptc-primitives
              '(:ptc-primitives nil) "SKILL.md"))))

  :doc "normalizes a declared closed list"
  (should (equal '("Read" "Grep")
                 (niwa-skills--parse-ptc-primitives
                  '(:ptc-primitives ("Read" "Grep" "Read")) "SKILL.md")))

  :doc "rejects scalar, blank, and non-string entries"
  (dolist (value '("Read" ("Read" "") ("Read" Bash)))
    (should-error
     (niwa-skills--parse-ptc-primitives
      (list :ptc-primitives value) "SKILL.md")
     :type 'user-error)))

(niwa-deftest niwa-skills-intersect-ptc-primitives ()
  ,test
  (test)

  :doc "uses unrestricted as identity and empty as absorbing"
  (progn
    (should (equal '("Read")
                   (niwa-skills-intersect-ptc-primitives
                    :unrestricted '("Read"))))
    (should-not (niwa-skills-intersect-ptc-primitives
                 '("Read") nil)))

  :doc "preserves the left restriction's order"
  (should (equal '("Grep" "Read")
                 (niwa-skills-intersect-ptc-primitives
                  '("Grep" "Read" "Bash") '("Read" "Grep")))))


;;
;;; Discovery + parsing

(niwa-deftest niwa-skills-scan (:quiet t)
  ,test
  (test)
  :doc "scans a single directory and builds a skill struct"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t))
         (_ (niwa-skills-test--write-skill
             dir "simplify"
             "name: simplify
description: Review changed code for reuse
argument-hint: \"[path]\"
"
             "Body of the simplify skill."))
         (skills (niwa-skills-scan dir '("." ))))
    (unwind-protect
        (progn
          (should (= 1 (length skills)))
          (let ((skill (car skills)))
            (should (equal "simplify" (niwa-skill-name skill)))
            (should (equal "Review changed code for reuse"
                           (niwa-skill-description skill)))
            (should (equal "[path]" (niwa-skill-argument-hint skill)))
            (should (eq 'inline (niwa-skill-context skill)))
            (should (eq 'project (niwa-skill-source skill)))
            (should (niwa-skill-user-invocable-p skill))
            (should (niwa-skill-model-invocable-p skill))
            ;; Body is not loaded eagerly.
            (should-not (niwa-skill-body skill))))
      (delete-directory dir t)))

  :doc "reads workspace enablement once for every skill in one scan"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-enable-scan-" t))
         (workspace (niwa-skills-test--make-workspace root))
         (reads 0))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "one" "description: One\n" "Body")
          (niwa-skills-test--write-skill
           root "two" "description: Two\n" "Body")
          (cl-letf (((symbol-function 'niwa-skills--read-state)
                     (lambda (_workspace)
                       (cl-incf reads)
                       nil)))
            (should (= 2 (length (niwa-skills-scan
                                  root '(".") workspace)))))
          (should (= 1 reads)))
      (delete-directory root t)))

  :doc "captures discovery roots and resolves authored dependencies on scan"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-dependency-scan-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "child" "description: Child\n" "Child body")
          (niwa-skills-test--write-skill
           root "parent" "description: Parent\n"
           "Before !$child.\n!$child -- raw args\nAfter")
          (let* ((skills (niwa-skills-scan nil (list root)))
                 (parent (cl-find "parent" skills
                                  :key #'niwa-skill-name :test #'equal))
                 (child (cl-find "child" skills
                                 :key #'niwa-skill-name :test #'equal))
                 (dependencies (niwa-skill-dependencies parent)))
            (should (equal (file-name-as-directory (expand-file-name root))
                           (niwa-skill-discovery-root parent)))
            (should (equal '("child" "child")
                           (mapcar #'niwa-skill-dependency-name
                                   dependencies)))
            (should (equal '(nil "raw args")
                           (mapcar #'niwa-skill-dependency-argument-template
                                   dependencies)))
            (should
             (cl-every
              (lambda (dependency)
                (equal (niwa-skills-source-key
                        (niwa-skill-source-file child))
                       (niwa-skill-dependency-source-key dependency)))
              dependencies))
            (should-not (niwa-skill-dependency-diagnostics parent))
            (should (niwa-skill-effective-model-invocable-p parent))
            (should-not (niwa-skill-body parent))))
      (delete-directory root t)))

  :doc "preserves model and effort metadata normalized from YAML"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-policy-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "policy"
           "description: Policy\nmodel: \"OpenAI:gpt-5-mini\"\neffort: high\n"
           "Body")
          (let ((skill (car (niwa-skills-scan dir '(".")))))
            (should (equal "OpenAI:gpt-5-mini"
                           (niwa-skill-model skill)))
            (should (eq 'high (niwa-skill-effort skill)))))
      (delete-directory dir t)))

  :doc "treats unused executable frontmatter fields as inert data"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-inert-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "inert"
           "description: Inert
pre: \"(error 'frontmatter-executed)\"
" "Body")
          (should (equal '("inert")
                         (mapcar #'niwa-skill-name
                                 (niwa-skills-scan dir '("."))))))
      (delete-directory dir t)))

  :doc "records inline-agent diagnostics during discovery"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-warning-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "inline-agent"
           "description: Inline\ncontext: inline\nagent: explorer\n" "Body")
          (niwa-skills-test--write-skill
           dir "fork-agent"
           "description: Fork\ncontext: fork\nagent: explorer\n" "Body")
          (let ((skills (niwa-skills-scan dir '("."))))
            (should (= 1 (length
                          (niwa-skill-warnings
                           (cl-find "inline-agent" skills
                                    :key #'niwa-skill-name :test #'equal)))))
            (should-not
             (niwa-skill-warnings
              (cl-find "fork-agent" skills
                       :key #'niwa-skill-name :test #'equal)))))
      (delete-directory dir t)))

  :doc "project/user name conflicts are source-qualified"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-project-" t))
         (project-dir (file-name-concat root ".niwa" "skills"))
         (user-dir (make-temp-file "niwa-skills-user-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           user-dir "shared"
           "name: shared
description: From user
" "User body")
          (niwa-skills-test--write-skill
           project-dir "shared"
           "name: shared
description: From project
" "Project body")
          (let* ((skills (niwa-skills-scan
                          root (list user-dir ".niwa/skills")))
                 (names (mapcar #'niwa-skill-name skills))
                 (project (cl-find "local:shared" skills
                                   :key #'niwa-skill-name
                                   :test #'equal))
                 (user (cl-find "global:shared" skills
                                :key #'niwa-skill-name
                                :test #'equal)))
            (should (= 2 (length skills)))
            (should (member "local:shared" names))
            (should (member "global:shared" names))
            (should-not (member "shared" names))
            (should (equal "From project"
                           (niwa-skill-description project)))
            (should (equal "From user"
                           (niwa-skill-description user)))
            (should (equal "shared" (niwa-skill-raw-name project)))
            (should (equal "shared" (niwa-skill-raw-name user)))
            (should-not (niwa-skill-plugin-name project))
            (should-not (niwa-skill-plugin-name user))))
      (delete-directory root t)
      (delete-directory user-dir t)))

  :doc "skills missing a description fall back to the first body paragraph"
  ;; A missing description uses the first non-empty markdown
  ;; paragraph/header.  The skill loads with the body-derived description
  ;; rather than being skipped.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "no-desc"
           "name: no-desc
" "Body line one.\n\nLater paragraph.")
          (niwa-skills-test--write-skill
           dir "ok"
           "name: ok
description: present
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (names (mapcar #'niwa-skill-name skills))
                 (no-desc (cl-find "no-desc" skills
                                   :key #'niwa-skill-name :test #'equal)))
            (should (member "ok" names))
            (should (member "no-desc" names))
            (should (equal "Body line one."
                           (niwa-skill-description no-desc)))))
      (delete-directory dir t)))

  :doc "relative dirs without a workspace root are skipped"
  (let* ((niwa-skills-include-bundled nil)
         (skills (niwa-skills-scan nil '(".niwa/skills/"))))
    (should (null skills)))

  :doc "does not discover skills through escaping directory symlinks"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-root-" t))
         (outside (make-temp-file "niwa-skills-outside-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           outside "escaped" "description: Escaped\n" "Body")
          (make-symbolic-link outside (file-name-concat root "linked"))
          (should-not (niwa-skills-scan root '("."))))
      (delete-directory root t)
      (delete-directory outside t)))

  :doc "rejects a configured project root symlink that escapes the workspace"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-root-link-" t))
         (outside (make-temp-file "niwa-skills-linked-root-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           outside "escaped" "description: Escaped\n" "Body")
          (make-symbolic-link outside (file-name-concat root "skills"))
          (should-not (niwa-skills-scan root '("skills"))))
      (delete-directory root t)
      (delete-directory outside t)))

  :doc "does not recurse through an ancestor directory symlink"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-loop-" t))
         (skill-dir (file-name-concat root "kept")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "kept" "description: Kept\n" "Body")
          (make-symbolic-link root (file-name-concat skill-dir "loop"))
          (should (equal '("kept")
                         (mapcar #'niwa-skill-name
                                 (niwa-skills-scan root '("."))))))
      (delete-directory root t)))

  :doc "ccs-compatible fields populate their slots"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t))
         ;; allowed-tools entries now go through the parser and need
         ;; tool-registry hits; mock so the unrelated fields under
         ;; test still load.
         (fake-tools
          `(("Read" . ,(niwa-tool--create
                        :name "Read" :handler #'ignore
                        :get-path (lambda (_) "")))
            ("Grep" . ,(niwa-tool--create
                        :name "Grep" :handler #'ignore
                        :get-path (lambda (_) ""))))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-tool-get)
                   (lambda (n &optional _c) (cdr (assoc n fake-tools)))))
          (niwa-skills-test--write-skill
           dir "delegation"
           "name: delegation
description: Delegate work
context: fork
agent: general-purpose
allowed-tools:
  - Read
  - Grep
disable-model-invocation: false
user-invocable: true
paths:
  - \"*.el\"
" "Delegation body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (skill (car skills)))
            (should (eq 'fork (niwa-skill-context skill)))
            (should (equal "general-purpose" (niwa-skill-agent skill)))
            (should (equal '("Read" "Grep") (niwa-skill-allowed-tools skill)))
            (should (equal '("*.el") (niwa-skill-path-patterns skill)))
            ;; path-patterns => dormant until matched
            (should-not (niwa-skill-active-p skill))
            (should (niwa-skill-user-invocable-p skill))
            (should (niwa-skill-model-invocable-p skill))))
      (delete-directory dir t)))

  :doc "bundled workflows are discoverable, while remember is a command"
  (niwa-tool-clear-registry)
  (let* ((skills (niwa-skills-scan nil nil))
         (frontend (cl-find "frontend" skills
                            :key #'niwa-skill-name :test #'equal))
         (worktree (cl-find "git-worktree" skills
                            :key #'niwa-skill-name :test #'equal))
         (remember (cl-find "remember" skills
                            :key #'niwa-skill-name :test #'equal))
         (learn (cl-find "learn" skills
                         :key #'niwa-skill-name :test #'equal))
         (body (and worktree (niwa-skill-load-body worktree)))
         (learn-body (and learn (niwa-skill-load-body learn))))
    (let ((analyzer (cl-find "analyze-log" skills
                             :key #'niwa-skill-name :test #'equal)))
      (should analyzer)
      (should (niwa-skill-user-invocable-p analyzer))
      (should-not (niwa-skill-model-invocable-p analyzer)))
    (should frontend)
    (should (eq 'bundled (niwa-skill-source frontend)))
    (should (niwa-skill-user-invocable-p frontend))
    (should (niwa-skill-model-invocable-p frontend))
    (should-not (niwa-skill-path-patterns frontend))
    (should (string-search "mobile and desktop"
                           (niwa-skill-load-body frontend)))
    (let ((cleanup (cl-find "clean-work" skills :key #'niwa-skill-name :test #'equal)))
      (should cleanup)
      (should (niwa-skill-user-invocable-p cleanup))
      (should-not (niwa-skill-model-invocable-p cleanup)))
    (let ((pair (cl-find "pair" skills :key #'niwa-skill-name :test #'equal)))
      (should (let ((s (cl-find "schedule" skills :key #'niwa-skill-name :test #'equal)))
                (and s (eq 'bundled (niwa-skill-source s))
                     (niwa-skill-user-invocable-p s)
                     (niwa-skill-model-invocable-p s))))
      (should pair)
      (should (eq 'bundled (niwa-skill-source pair)))
      (should (niwa-skill-user-invocable-p pair))
      (should-not (niwa-skill-model-invocable-p pair)))
    (should learn)
    (should (eq 'bundled (niwa-skill-source learn)))
    (should (niwa-skill-user-invocable-p learn))
    (should (equal "[focus]" (niwa-skill-argument-hint learn)))
    (should (string-match-p "applicable `AGENTS.md`" learn-body))
    (should (string-match-p "existing `user` or" learn-body))
    (should (string-match-p "not a substitute for curated memory" learn-body))
    (should-not (string-match-p "CLAUDE\\.md" learn-body))
    (should-not remember)
    (should worktree)
    (should-not (cl-find "using-git-worktrees" skills
                         :key #'niwa-skill-name :test #'equal))
    (should (eq 'bundled (niwa-skill-source worktree)))
    (should (niwa-skill-model-invocable-p worktree))
    (should-not (niwa-skill-user-invocable-p worktree))
    (should (equal '("Bash(git rev-parse:*)"
                     "Bash(git status:*)"
                     "Bash(git check-ignore:*)"
                     "Bash(git worktree list:*)"
                     "Bash(printf:*)")
                   (niwa-skill-allowed-tools worktree)))
    (should (member '("Bash" :pattern "git rev-parse:*" :action allow)
                    (niwa-skill-allowed-tool-rules worktree)))
    (should (string-match-p "best-effort and read-only" body))
    (should (string-match-p "git worktree list --porcelain" body))
    (should (string-match-p
             "You cannot invoke slash commands yourself" body))
    ;; The artifact skill is bundled, user-invocable as /artifact, and
    ;; carries the Eval grant its directory-lookup injection needs.
    (let* ((artifact (cl-find "artifact" skills
                              :key #'niwa-skill-name :test #'equal))
           (artifact-body (and artifact
                               (niwa-skill-load-body artifact))))
      (should artifact)
      (should (eq 'bundled (niwa-skill-source artifact)))
      (should (niwa-skill-user-invocable-p artifact))
      (should (niwa-skill-model-invocable-p artifact))
      (should (equal '("Eval") (niwa-skill-allowed-tools artifact)))
      (should (string-match-p "Self-contained, always" artifact-body))
      (should (string-match-p "niwa-artifact-store-directory"
                              artifact-body)))
    ;; Native whiteboard guidance is independent of HTML artifact attachments.
    (let ((whiteboard (cl-find "shared-whiteboard" skills
                               :key #'niwa-skill-name :test #'equal)))
      (should whiteboard)
      (should (eq 'bundled (niwa-skill-source whiteboard)))
      (should (niwa-skill-user-invocable-p whiteboard))
      (should (niwa-skill-model-invocable-p whiteboard))
      (should (niwa-skill-effective-model-invocable-p whiteboard))
      (should-not (niwa-skill-path-patterns whiteboard))
      (should-not (niwa-skill-dependencies whiteboard))
      (should-not (niwa-skill-dependency-diagnostics whiteboard)))
    ;; The artifact family inherits through required attachments: each leaf
    ;; declares its bases, and every descendant stays model-invocable so a
    ;; model-origin root resolves the whole graph.  Attachment-only bases stay
    ;; out of direct user completion without blocking a user-origin leaf.
    (dolist (base '("artifact-design" "artifact-diagramming"))
      (let ((skill (cl-find base skills
                            :key #'niwa-skill-name :test #'equal)))
        (should skill)
        (should (eq 'bundled (niwa-skill-source skill)))
        (should (niwa-skill-model-invocable-p skill))
        (should-not (niwa-skill-user-invocable-p skill))
        (should-not (niwa-skill-dependencies skill))))
    (pcase-dolist (`(,name . ,deps)
                   '(("artifact-doc"        "artifact" "artifact-design")
                     ("artifact-dashboard"  "artifact" "artifact-design")
                     ("artifact-data-table" "artifact" "artifact-design")
                     ("artifact-explainer"  "artifact" "artifact-design"
                      "artifact-diagramming")))
      (let* ((leaf (cl-find name skills
                            :key #'niwa-skill-name :test #'equal))
             (leaf-body (and leaf (niwa-skill-load-body leaf))))
        (should leaf)
        (should (eq 'bundled (niwa-skill-source leaf)))
        (should (niwa-skill-user-invocable-p leaf))
        (should (niwa-skill-model-invocable-p leaf))
        (should (niwa-skill-effective-model-invocable-p leaf))
        (should-not (niwa-skill-dependency-diagnostics leaf))
        (should (equal deps
                       (mapcar #'niwa-skill-dependency-name
                               (niwa-skill-dependencies leaf))))
        ;; Every leaf reads its template through ${NIWA_SKILL_DIR}, so
        ;; the file has to sit beside the SKILL.md that names it.
        (should (string-match-p "NIWA_SKILL_DIR" leaf-body))
        (should (file-exists-p
                 (file-name-concat (niwa-skill-source-dir leaf)
                                   "template.html"))))))

  :doc "bundled skills are suppressed when include-bundled is nil"
  (let* ((niwa-skills-include-bundled nil)
         (skills (niwa-skills-scan nil nil)))
    (should-not (cl-find-if
                 (lambda (s) (eq 'bundled (niwa-skill-source s)))
                 skills)))

  :doc "bundled discovery preserves raw identity through conflict qualification"
  (let* ((dir (make-temp-file "niwa-bundled-identity-" t))
         (niwa-skill-dirs (list dir)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "learn" "description: User learn\n" "Body")
          (let ((skill (cl-find "bundled:learn"
                                (niwa-skills-scan nil nil)
                                :key #'niwa-skill-name :test #'equal)))
            (should skill)
            (should (equal "learn" (niwa-skill-raw-name skill)))
            (should-not (niwa-skill-plugin-name skill))))
      (delete-directory dir t)))

  :doc "managed discovery preserves raw identity through conflict qualification"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-managed-identity-" t))
         (user-dir (file-name-concat root "user"))
         (managed-dir (file-name-concat root "managed")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           user-dir "shared" "description: User shared\n" "Body")
          (niwa-skills-test--write-skill
           managed-dir "shared" "description: Managed shared\n" "Body")
          (let ((skill (cl-find
                        "managed:shared"
                        (niwa-skills-scan
                         nil (list user-dir (cons managed-dir 'managed)))
                        :key #'niwa-skill-name :test #'equal)))
            (should skill)
            (should (equal "shared" (niwa-skill-raw-name skill)))
            (should-not (niwa-skill-plugin-name skill))))
      (delete-directory root t)))

  :doc "enabled plugin skills are discovered with plugin-prefixed names"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skill-dirs nil)
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-plugin-skills-" t)))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (workspace (niwa-skills-test--make-workspace
                     (file-name-concat user-dir "workspace")))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Body")
          (niwa-plugins-enable "demo" workspace)
          (let* ((skills (niwa-skills-scan nil nil workspace))
                 (skill (cl-find "demo:from-plugin" skills
                                 :key #'niwa-skill-name :test #'equal)))
            (should skill)
            (should (eq 'plugin (niwa-skill-source skill)))
            (should (equal "from-plugin" (niwa-skill-raw-name skill)))
            (should (equal "demo" (niwa-skill-plugin-name skill)))
            (should (equal "Plugin skill"
                           (niwa-skill-description skill)))
            (should-not (cl-find "from-plugin" skills
                                 :key #'niwa-skill-name :test #'equal))))
      (delete-directory user-dir t)))

  :doc "disabled plugin skills are skipped"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skill-dirs nil)
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-plugin-disabled-" t)))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (workspace (niwa-skills-test--make-workspace
                     (file-name-concat user-dir "workspace")))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           plugin-skills "disabled-skill"
           "name: disabled-skill
description: Disabled plugin skill
" "Body")
          (should-not
           (cl-find "demo:disabled-skill"
                    (niwa-skills-scan nil nil workspace)
                    :key #'niwa-skill-name :test #'equal)))
      (delete-directory user-dir t)))

  :doc "explicit scan dirs do not include installed plugin skills"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-plugin-explicit-dirs-" t))
         (configured-skills (file-name-concat root "skills"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           configured-skills "configured"
           "name: configured
description: Configured skill
" "Configured body")
          (niwa-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Plugin body")
          (let ((skills (niwa-skills-scan nil (list configured-skills))))
            (should (cl-find "configured" skills
                             :key #'niwa-skill-name :test #'equal))
            (should-not (cl-find "from-plugin" skills
                                 :key #'niwa-skill-name :test #'equal))))
      (delete-directory root t)))

  :doc "explicit source-tagged plugin dirs are scanned as plugin skills"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-plugin-source-tagged-" t))
         (plugin-skills (file-name-concat root "plugin-skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           plugin-skills "from-plugin"
           "name: from-plugin
description: Plugin skill
" "Plugin body")
          (let* ((skills (niwa-skills-scan
                          nil
                          (list (cons plugin-skills '(plugin . "demo")))))
                 (skill (cl-find "demo:from-plugin" skills
                                 :key #'niwa-skill-name :test #'equal)))
            (should skill)
            (should (eq 'plugin (niwa-skill-source skill)))))
      (delete-directory root t)))

  :doc "plugin skills are namespaced so configured skills coexist"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-plugin-precedence-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (configured-skills (file-name-concat root "skills"))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-skill-dirs (list configured-skills))
         (workspace (niwa-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           configured-skills "shared"
           "name: shared
description: Configured skill
" "Configured body")
          (niwa-skills-test--write-skill
           plugin-skills "shared"
           "name: shared
description: Plugin skill
" "Plugin body")
          (niwa-plugins-enable "demo" workspace)
          (let* ((skills (niwa-skills-scan nil nil workspace))
                 (configured (cl-find "shared" skills
                                      :key #'niwa-skill-name
                                      :test #'equal))
                 (plugin (cl-find "demo:shared" skills
                                  :key #'niwa-skill-name
                                  :test #'equal)))
            (should configured)
            (should plugin)
            (should (eq 'user (niwa-skill-source configured)))
            (should (eq 'plugin (niwa-skill-source plugin)))
            (should (equal "Configured skill"
                           (niwa-skill-description configured)))
            (should (equal "Plugin skill"
                           (niwa-skill-description plugin)))))
      (delete-directory root t)))
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-plugin-project-precedence-" t))
         (workspace-root (file-name-concat root "workspace"))
         (project-skills (file-name-concat workspace-root ".niwa/skills"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-skill-dirs '(".niwa/skills"))
         (workspace (niwa-skills-test--make-workspace workspace-root))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"plugin-skills\"}"))
         (plugin-skills (file-name-concat plugin-root "plugin-skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           project-skills "shared"
           "name: shared
description: Project skill
" "Project body")
          (niwa-skills-test--write-skill
           plugin-skills "shared"
           "name: shared
description: Plugin skill
" "Plugin body")
          (niwa-plugins-enable "demo" workspace)
          (let* ((skills (niwa-skills-scan
                          workspace-root nil workspace))
                 (project (cl-find "shared" skills
                                   :key #'niwa-skill-name
                                   :test #'equal))
                 (plugin (cl-find "demo:shared" skills
                                  :key #'niwa-skill-name
                                  :test #'equal)))
            (should project)
            (should plugin)
            (should (eq 'project (niwa-skill-source project)))
            (should (eq 'plugin (niwa-skill-source plugin)))
            (should (equal "Project skill"
                           (niwa-skill-description project)))
            (should (equal "Plugin skill"
                           (niwa-skill-description plugin)))))
      (delete-directory root t)))
  (let* ((root (make-temp-file "niwa-plugin-bundled-precedence-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-skill-dirs nil)
         (workspace (niwa-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-concat plugin-root "skills")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           plugin-skills "learn"
           "name: learn
description: Plugin learn
" "Body")
          (niwa-plugins-enable "demo" workspace)
          (let* ((skills (niwa-skills-scan nil nil workspace))
                 (learn (cl-find "learn" skills
                                    :key #'niwa-skill-name
                                    :test #'equal))
                 (plugin (cl-find "demo:learn" skills
                                  :key #'niwa-skill-name
                                  :test #'equal)))
            (should learn)
            (should plugin)
            (should (eq 'bundled (niwa-skill-source learn)))
            (should (eq 'plugin (niwa-skill-source plugin)))))
      (delete-directory root t)))
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-plugin-same-skill-" t))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-skill-dirs nil)
         (workspace (niwa-skills-test--make-workspace
                     (file-name-concat root "workspace")))
         (plugin-a-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"alpha\",\"skills\":\"skills\"}"))
         (plugin-b-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"beta\",\"skills\":\"skills\"}")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           (file-name-concat plugin-a-root "skills")
           "shared"
           "name: shared
description: Alpha shared
" "Body")
          (niwa-skills-test--write-skill
           (file-name-concat plugin-b-root "skills")
           "shared"
           "name: shared
description: Beta shared
" "Body")
          (niwa-plugins-enable "alpha" workspace)
          (niwa-plugins-enable "beta" workspace)
          (let ((names (mapcar #'niwa-skill-name
                               (niwa-skills-scan nil nil workspace))))
            (should (member "alpha:shared" names))
            (should (member "beta:shared" names))
            (should-not (member "shared" names))))
      (delete-directory root t)))

  :doc "frontmatter `name' wins; missing `name' falls back to directory"
  ;; Name resolution prefers frontmatter over directory.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          ;; Frontmatter overrides directory.
          (niwa-skills-test--write-skill
           dir "dir-name-a"
           "name: explicit-name
description: present
" "Body")
          ;; Directory used when frontmatter omits :name.
          (niwa-skills-test--write-skill
           dir "dir-name-b"
           "description: present
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (names (mapcar #'niwa-skill-name skills)))
            (should (member "explicit-name" names))
            (should (member "dir-name-b" names))
            (should-not (member "dir-name-a" names))))
      (delete-directory dir t)))

  :doc "skills with invalid names are skipped with a warning"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "Bad_Name"
           "description: invalid because of underscore + uppercase
" "Body")
          (niwa-skills-test--write-skill
           dir "good-name"
           "description: ok
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (names (mapcar #'niwa-skill-name skills)))
            (should (member "good-name" names))
            (should-not (member "Bad_Name" names))))
      (delete-directory dir t)))

  :doc "arguments frontmatter parsed with numeric-only filtered"
  ;; Numeric-only names cannot shadow $0/$1.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "args"
           "description: argument names
arguments:
  - foo
  - bar
  - \"1\"
" "Body")
          (niwa-skills-test--write-skill
           dir "args-string"
           "description: space-separated string
arguments: alpha beta 2 gamma
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (list-skill (cl-find "args" skills
                                      :key #'niwa-skill-name :test #'equal))
                 (str-skill (cl-find "args-string" skills
                                     :key #'niwa-skill-name :test #'equal)))
            (should (equal '("foo" "bar")
                           (niwa-skill-argument-names list-skill)))
            (should (equal '("alpha" "beta" "gamma")
                           (niwa-skill-argument-names str-skill)))))
      (delete-directory dir t)))

  :doc "effort remains opaque until gptel validates the selected model"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "good-effort"
           "description: ok
effort: high
" "Body")
          (niwa-skills-test--write-skill
           dir "bad-effort"
           "description: ok
effort: -1
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (good (cl-find "good-effort" skills
                                :key #'niwa-skill-name :test #'equal))
                 (bad (cl-find "bad-effort" skills
                               :key #'niwa-skill-name :test #'equal)))
            (should (eq 'high (niwa-skill-effort good)))
            (should (= -1 (niwa-skill-effort bad)))))
      (delete-directory dir t)))

  :doc "warns about any declared shell, which skills do not support"
  ;; Body shell expansion always runs through the session Bash pipeline;
  ;; accepting `powershell' silently ran the author's script under
  ;; different semantics than they wrote.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "default-shell"
           "description: ok
" "Body")
          (niwa-skills-test--write-skill
           dir "ps-shell"
           "description: ok
shell: powershell
" "Body")
          (niwa-skills-test--write-skill
           dir "weird-shell"
           "description: ok
shell: fish
" "Body")
          (let (warnings)
            (niwa-test--with-captured-diagnostics warnings
              (niwa-skills-scan dir '(".")))
            (should (string-match-p "powershell" warnings))
            (should (string-match-p
                     (regexp-quote (format "niwa [%s]:" dir)) warnings))
            (should (string-match-p "fish" warnings))
            (should-not (string-match-p "default-shell" warnings))))
      (delete-directory dir t)))

  :doc "invalid YAML in frontmatter logs a warning and skips the skill"
  ;; Invalid YAML skips the skill and logs a warning.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t))
         (warnings nil))
    (unwind-protect
        (progn
          ;; Write a SKILL.md with malformed YAML (unbalanced quote).
          (let ((skill-dir (file-name-as-directory
                            (file-name-concat dir "broken"))))
            (make-directory skill-dir)
            (with-temp-file (file-name-concat skill-dir "SKILL.md")
              (insert "---\nname: broken\ndescription: \"unterminated\n---\nBody")))
          (niwa-skills-test--write-skill
           dir "intact"
           "name: intact
description: present
" "Body")
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (type message &rest args)
                       (when (eq type 'niwa)
                         (push message warnings))
                       nil)))
            (let* ((skills (niwa-skills-scan dir '(".")))
                   (names (mapcar #'niwa-skill-name skills)))
              (should (member "intact" names))
              (should-not (member "broken" names))
              (should (cl-some (lambda (m)
                                 (and (string-match-p "broken/SKILL.md" m)
                                      (string-prefix-p (format "niwa [%s]:" dir) m)
                                      (string-match-p "invalid YAML" m)))
                               warnings)))))
      (delete-directory dir t)))

  :doc "allowed-tools validation works before installation and rejects unknown tools"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-preinstall-" t))
         (warnings nil))
    (unwind-protect
        (progn
          (niwa-tool-clear-registry)
          (niwa-skills-test--write-skill
           dir "known-tools"
           "name: known-tools
description: known
allowed-tools:
  - Skill(foo)
  - ListSkills
" "Body")
          (niwa-skills-test--write-skill
           dir "unknown-tool"
           "name: unknown-tool
description: unknown
allowed-tools:
  - DoesNotExist
" "Body")
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (type message &rest args)
                       (when (eq type 'niwa)
                         (push message warnings))
                       nil)))
            (let* ((skills (niwa-skills-scan dir '(".")))
                   (known (cl-find "known-tools" skills
                                   :key #'niwa-skill-name :test #'equal)))
              (should known)
              (should
               (equal '(("Skill" :name "foo" :action allow)
                        ("ListSkills" :action allow))
                      (niwa-skill-allowed-tool-rules known)))
              (should-not (cl-find "unknown-tool" skills
                                   :key #'niwa-skill-name :test #'equal))
              (should (cl-some
                       (lambda (message)
                         (and (string-match-p "unknown-tool" message)
                              (string-match-p "DoesNotExist" message)))
                       warnings)))))
      (niwa-tool-clear-registry)
      (delete-directory dir t)))

  :doc "allowed-tools strings are parsed into allowed-tool-rules at scan"
  ;; --from-plist runs each `allowed-tools' string through the parser.
  ;; Malformed entries warn-and-skip; valid entries become rules.
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t))
         (warnings nil)
         ;; Use the same fake-tool-get pattern as the parser tests so
         ;; we don't depend on tool-registry registration order.
         (fake-tools
          `(("Read"     . ,(niwa-tool--create
                            :name "Read" :handler #'ignore
                            :get-path (lambda (_) "")))
            ("Bash"     . ,(niwa-tool--create
                            :name "Bash" :handler #'ignore
                            :get-pattern (lambda (_) ""))))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-tool-get)
                   (lambda (n &optional _c) (cdr (assoc n fake-tools))))
                  ((symbol-function 'display-warning)
                   (lambda (type message &rest args)
                     (when (eq type 'niwa) (push message warnings))
                     nil)))
          (niwa-skills-test--write-skill
           dir "ok-rules"
           "name: ok-rules
description: ok
allowed-tools:
  - Read
  - Bash(git status)
" "Body")
          (niwa-skills-test--write-skill
           dir "with-bad-entry"
           "name: with-bad-entry
description: ok
allowed-tools:
  - Read
  - bash(rm)
  - Bash(echo *)
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (ok (cl-find "ok-rules" skills
                              :key #'niwa-skill-name :test #'equal))
                 (bad (cl-find "with-bad-entry" skills
                               :key #'niwa-skill-name :test #'equal)))
            ;; ok-rules: both entries parsed.
            (should (equal '(("Read" :action allow)
                             ("Bash" :pattern "git status" :action allow))
                           (niwa-skill-allowed-tool-rules ok)))
            ;; A malformed allowed-tools entry skips the whole skill
            ;; rather than dropping individual entries.
            (should (null bad))
            (should (cl-some (lambda (m)
                               (and (string-match-p "with-bad-entry" m)
                                    (string-match-p "bash(rm)" m)))
	                             warnings))))
      (delete-directory dir t)))

  :doc "project skill hooks require exact trusted manifest bytes"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-hook-root-" t))
         (skill-dir (file-name-concat root ".niwa" "skills"))
         (user-dir (make-temp-file "niwa-skills-hook-user-" t))
         (workspace (niwa-skills-test--make-workspace root))
         (niwa-user-dir (file-name-as-directory user-dir))
         (niwa-hooks-require-project-trust t)
         (snapshot-function
          (symbol-function 'niwa-hooks-project-file-snapshot)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           skill-dir "hooked"
           "name: hooked
description: ok
hooks:
  PreToolUse:
    - matcher: Bash
      hooks:
        - type: elisp
          function: niwa-skills-test--hook-fn
" "Body")
          (should-not
           (niwa-skill-hooks
            (car (niwa-skills-scan
                  root '(".niwa/skills") workspace))))
          (niwa-test--with-captured-messages nil
            (niwa-hooks-trust-project workspace))
          (let* ((skill
                  (cl-letf
                      (((symbol-function
                         'niwa-hooks-project-file-snapshot)
                        (lambda (ws file)
                          (let ((snapshot
                                 (funcall snapshot-function ws file)))
                            (with-temp-file file
                              (insert "---\nname: hooked\n"
                                      "description: replacement\n"
                                      "hooks:\n  PreToolUse:\n"
                                      "    - matcher: Bash\n      hooks:\n"
                                      "        - type: elisp\n"
                                      "          function: ignore\n"
                                      "---\nReplacement"))
                            snapshot))))
                    (car (niwa-skills-scan
                          root '(".niwa/skills") workspace))))
                 (hooks (niwa-skill-hooks skill))
                 (session (niwa-session-create "main" workspace))
                 (request (niwa-request--create
                           :session session
                           :hook-rules hooks))
                 decision)
            (should (eq 'PreToolUse (caar hooks)))
            (let ((handler
                   (car (niwa-hooks--matching-handlers
                         'PreToolUse '(:tool-name "Bash") hooks))))
              (should (eq 'project-file (plist-get handler :source)))
              (should (eq 'niwa-skills-test--hook-fn
                          (plist-get handler :function)))
              (should (equal (niwa-skill-source-file skill)
                             (plist-get handler :source-file))))
            (should (equal "ok" (niwa-skill-description skill)))
            (niwa-hooks-run-event
             'PreToolUse
             (niwa-hooks-event-plist
              'PreToolUse session workspace
              :tool-name "Bash")
             (lambda (d) (setq decision d))
             session workspace request nil)
            (while (null decision)
              (accept-process-output nil 0.01))
            (should (equal '("skill hook ran")
                           (plist-get decision :additional-context))))
          (should-not
           (niwa-skill-hooks
            (car (niwa-skills-scan
                  root '(".niwa/skills") workspace)))))
      (niwa-hooks-invalidate-config)
      (delete-directory root t)
      (delete-directory user-dir t)))

  :doc "fork skill Stop hooks normalize to SubagentStop"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
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
          function: niwa-skills-test--hook-fn
" "Body")
          (let* ((skill (car (niwa-skills-scan nil (list dir))))
                 (hooks (niwa-skill-hooks skill)))
            (should (eq 'SubagentStop (caar hooks)))))
      (delete-directory dir t)))

  :doc "display-name defaults to name when frontmatter omits it"
  (let* ((niwa-skills-include-bundled nil)
         (dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "no-display"
           "description: ok
" "Body")
          (niwa-skills-test--write-skill
           dir "with-display"
           "description: ok
display-name: Friendly Label
" "Body")
          (let* ((skills (niwa-skills-scan dir '(".")))
                 (no-d (cl-find "no-display" skills
                                :key #'niwa-skill-name :test #'equal))
                 (with-d (cl-find "with-display" skills
                                  :key #'niwa-skill-name :test #'equal)))
            (should (equal "no-display" (niwa-skill-display-name no-d)))
            (should (equal "Friendly Label"
                           (niwa-skill-display-name with-d)))))
      (delete-directory dir t))))

(niwa-deftest niwa-skills-project-files ()
  ,test
  (test)
  :doc "returns configured project manifests without user or linked trees"
  (let* ((root (make-temp-file "niwa-skill-files-root-" t))
         (outside (make-temp-file "niwa-skill-files-outside-" t))
         (project-dir (file-name-concat root "project-skills"))
         (linked-root (file-name-concat root "linked-root"))
         (niwa-skill-dirs
          (list "project-skills" "linked-root" outside)))
    (unwind-protect
        (progn
          (let ((project-file
                 (niwa-skills-test--write-skill
                  project-dir "project" "description: Project\n" "Body")))
            (niwa-skills-test--write-skill
             outside "user" "description: User\n" "Body")
            (make-symbolic-link outside linked-root)
            (make-symbolic-link outside
                                (file-name-concat project-dir "linked"))
            (should (equal (list project-file)
                           (niwa-skills-project-files root)))))
      (delete-directory root t)
      (delete-directory outside t))))

(niwa-deftest niwa-skills--qualify-conflicting-names ()
  ,test
  (test)
  :doc "unique and already-qualified plugin names are preserved"
  (let* ((plugin (niwa-skill--create
                  :name "superpowers:brainstorming"
                  :source 'plugin))
         (user (niwa-skill--create
                :name "brainstorming"
                :source 'user))
         (solo (niwa-skill--create
                :name "solo"
                :source 'project))
         (skills (niwa-skills--qualify-conflicting-names
                  (list plugin user solo))))
    (should (equal '("superpowers:brainstorming" "brainstorming" "solo")
                   (mapcar #'niwa-skill-name skills))))

  :doc "same-source duplicates keep the first entry"
  (let* ((first (niwa-skill--create
                 :name "shared" :description "first" :source 'user))
         (second (niwa-skill--create
                  :name "shared" :description "second" :source 'user))
         (skills (niwa-skills--qualify-conflicting-names
                  (list first second))))
    (should (= 1 (length skills)))
    (should (equal "shared" (niwa-skill-name (car skills))))
    (should (equal "first" (niwa-skill-description (car skills)))))

  :doc "family-only prefixes are used when family disambiguates"
  (let* ((niwa (niwa-skill--create
                   :name "shared" :source 'project :source-family 'niwa))
         (agents (niwa-skill--create
                  :name "shared" :source 'project :source-family 'agents))
         (skills (niwa-skills--qualify-conflicting-names
                  (list niwa agents))))
    (should (equal '("niwa:shared" "agents:shared")
                   (mapcar #'niwa-skill-name skills))))

  :doc "scope-only prefixes are used when scope disambiguates"
  (let* ((local (niwa-skill--create
                 :name "shared" :source 'project :source-family 'niwa))
         (global (niwa-skill--create
                  :name "shared" :source 'user :source-family 'niwa))
         (skills (niwa-skills--qualify-conflicting-names
                  (list local global))))
    (should (equal '("local:shared" "global:shared")
                   (mapcar #'niwa-skill-name skills))))

  :doc "full scope-family prefixes are used for four-way conflicts"
  (let* ((local-niwa
          (niwa-skill--create
           :name "shared" :source 'project :source-family 'niwa))
         (local-agents
          (niwa-skill--create
           :name "shared" :source 'project :source-family 'agents))
         (global-niwa
          (niwa-skill--create
           :name "shared" :source 'user :source-family 'niwa))
         (global-agents
          (niwa-skill--create
           :name "shared" :source 'user :source-family 'agents))
         (skills (niwa-skills--qualify-conflicting-names
                  (list local-niwa local-agents
                        global-niwa global-agents))))
    (should (equal '("local-niwa:shared" "local-agents:shared"
                     "global-niwa:shared" "global-agents:shared")
                   (mapcar #'niwa-skill-name skills))))

  :doc "bundled conflicts keep all skills with unique prefixes"
  (let* ((local (niwa-skill--create
                 :name "review" :source 'project :source-family 'niwa))
         (bundled (niwa-skill--create
                   :name "review" :source 'bundled))
         (skills (niwa-skills--qualify-conflicting-names
                  (list local bundled))))
    (should (equal '("niwa:review" "bundled:review")
                   (mapcar #'niwa-skill-name skills)))))

(niwa-deftest niwa-skills--resolve-dependencies ()
  ,test
  (test)

  :doc "prefers same-root and same-plugin targets before global raw names"
  (cl-labels
      ((dependency (name)
         (niwa-skill-dependency--create :name name))
       (skill (name raw root dependencies &optional plugin)
         (niwa-skill--create
          :name name :raw-name raw :plugin-name plugin
          :source-file (file-name-concat root (concat name ".md"))
          :discovery-root root :dependencies dependencies
          :model-invocable-p t :enabled-p t)))
    (let* ((root-a "/tmp/niwa-dependency-root-a/")
           (root-b "/tmp/niwa-dependency-root-b/")
           (root-c "/tmp/niwa-dependency-root-c/")
           (same-root-dependency (dependency "child"))
           (plugin-dependency (dependency "child"))
           (qualified-dependency (dependency "other:child"))
           (unique-dependency (dependency "unique"))
           (same-root-parent
            (skill "parent" "parent" root-a (list same-root-dependency)))
           (local-child (skill "local:child" "child" root-a nil))
           (other-child (skill "other:child" "child" root-b nil "other"))
           (plugin-parent
            (skill "demo:parent" "parent" root-c
                   (list plugin-dependency) "demo"))
           (plugin-child
            (skill "demo:child" "child" root-c nil "demo"))
           (qualified-parent
            (skill "qualified" "qualified" root-c
                   (list qualified-dependency)))
           (unique-parent
            (skill "unique-parent" "unique-parent" root-c
                   (list unique-dependency)))
           (unique-child (skill "unique" "unique" root-b nil))
           (skills (list same-root-parent local-child other-child
                         plugin-parent plugin-child qualified-parent
                         unique-parent unique-child)))
      (niwa-skills--resolve-dependencies skills)
      (should
       (equal (niwa-skills-source-key
               (niwa-skill-source-file local-child))
              (niwa-skill-dependency-source-key same-root-dependency)))
      (should
       (equal (niwa-skills-source-key
               (niwa-skill-source-file plugin-child))
              (niwa-skill-dependency-source-key plugin-dependency)))
      (should
       (equal (niwa-skills-source-key
               (niwa-skill-source-file other-child))
              (niwa-skill-dependency-source-key qualified-dependency)))
      (should
       (equal (niwa-skills-source-key
               (niwa-skill-source-file unique-child))
              (niwa-skill-dependency-source-key unique-dependency)))))

  :doc "records deterministic ambiguity and never falls back from qualification"
  (cl-labels
      ((dependency (name)
         (niwa-skill-dependency--create :name name))
       (skill (name raw root dependencies)
         (niwa-skill--create
          :name name :raw-name raw
          :source-file (file-name-concat root (concat name ".md"))
          :discovery-root root :dependencies dependencies
          :model-invocable-p t :enabled-p t)))
    (let* ((ambiguous-dependency (dependency "child"))
           (missing-qualified (dependency "missing:target"))
           (ambiguous-parent
            (skill "ambiguous" "ambiguous" "/tmp/dependency-parent/"
                   (list ambiguous-dependency)))
           (qualified-parent
            (skill "qualified" "qualified" "/tmp/dependency-parent/"
                   (list missing-qualified)))
           (first (skill "a:child" "child" "/tmp/dependency-a/" nil))
           (second (skill "z:child" "child" "/tmp/dependency-z/" nil))
           (fallback (skill "other:target" "target"
                            "/tmp/dependency-target/" nil)))
      (niwa-skills--resolve-dependencies
       (list ambiguous-parent qualified-parent first second fallback))
      (should-not (niwa-skill-dependency-source-key ambiguous-dependency))
      (should
       (equal '("Required skill 'child' is ambiguous: a:child, z:child")
              (niwa-skill-dependency-diagnostics ambiguous-parent)))
      (should-not (niwa-skill-dependency-source-key missing-qualified))
      (should
       (equal '("Required skill 'missing:target' was not found")
              (niwa-skill-dependency-diagnostics qualified-parent)))
      (should-not
       (niwa-skill-effective-model-invocable-p ambiguous-parent))))

  :doc "detects self, direct, and transitive cycles with readable chains"
  (cl-labels
      ((dependency (name)
         (niwa-skill-dependency--create :name name))
       (skill (name dependencies)
         (niwa-skill--create
          :name name :raw-name name
          :source-file (concat "/tmp/dependency-cycle-" name ".md")
          :discovery-root "/tmp/" :dependencies dependencies
          :model-invocable-p t :enabled-p t)))
    (let* ((self (skill "self" (list (dependency "self"))))
           (a (skill "a" (list (dependency "b"))))
           (b (skill "b" (list (dependency "a"))))
           (x (skill "x" (list (dependency "y"))))
           (y (skill "y" (list (dependency "z"))))
           (z (skill "z" (list (dependency "y")))))
      (niwa-skills--resolve-dependencies (list self a b x y z))
      (should
       (member "Required skill dependency cycle: self -> self"
               (niwa-skill-dependency-diagnostics self)))
      (dolist (skill (list a b))
        (should
         (member "Required skill dependency cycle: a -> b -> a"
                 (niwa-skill-dependency-diagnostics skill))))
      (dolist (skill (list x y z))
        (should
         (member "Required skill dependency cycle: y -> z -> y"
                 (niwa-skill-dependency-diagnostics skill)))
        (should-not
         (niwa-skill-effective-model-invocable-p skill)))))

  :doc "model gates and disabled descendants propagate without activation"
  (cl-labels
      ((dependency (name)
         (niwa-skill-dependency--create :name name))
       (skill (name dependencies)
         (niwa-skill--create
          :name name :raw-name name
          :source-file (concat "/tmp/dependency-effective-" name ".md")
          :discovery-root "/tmp/" :dependencies dependencies
          :model-invocable-p t :enabled-p t)))
    (let* ((model-child (skill "model-child" nil))
           (dormant (skill "dormant" nil))
           (middle (skill "middle" (list (dependency "model-child"))))
           (root (skill "root" (list (dependency "middle")
                                     (dependency "dormant"))))
           (disabled (skill "disabled" nil))
           (disabled-parent
            (skill "disabled-parent" (list (dependency "disabled")))))
      (setf (niwa-skill-model-invocable-p model-child) nil)
      (setf (niwa-skill-active-p dormant) nil)
      (setf (niwa-skill-enabled-p disabled) nil)
      (niwa-skills--resolve-dependencies
       (list root middle model-child dormant disabled-parent disabled))
      (should-not (niwa-skill-effective-model-invocable-p model-child))
      (should-not (niwa-skill-effective-model-invocable-p middle))
      (should-not (niwa-skill-effective-model-invocable-p root))
      (should-not (niwa-skill-active-p dormant))
      (should (niwa-skill-effective-model-invocable-p disabled))
      (should-not
       (niwa-skill-effective-model-invocable-p disabled-parent)))))


;;
;;; Phase 1 helpers (validators, parsers)

(niwa-deftest niwa-skills--valid-name-p ()
  ,test
  (test)
  :doc "lowercase letters, digits, and dashes pass"
  (should (niwa-skills--valid-name-p "grill-me"))
  (should (niwa-skills--valid-name-p "review-spec"))
  (should (niwa-skills--valid-name-p "abc123"))
  (should (niwa-skills--valid-name-p "x"))

  :doc "uppercase, underscores, dots, slashes, spaces fail"
  (should-not (niwa-skills--valid-name-p "Bad"))
  (should-not (niwa-skills--valid-name-p "bad_name"))
  (should-not (niwa-skills--valid-name-p "bad.name"))
  (should-not (niwa-skills--valid-name-p "bad/name"))
  (should-not (niwa-skills--valid-name-p "bad name"))

  :doc "empty and over-length names fail"
  (should-not (niwa-skills--valid-name-p ""))
  (should-not (niwa-skills--valid-name-p (make-string 65 ?a)))
  (should (niwa-skills--valid-name-p (make-string 64 ?a)))

  :doc "non-strings fail"
  (should-not (niwa-skills--valid-name-p nil))
  (should-not (niwa-skills--valid-name-p 'symbol)))

(niwa-deftest niwa-skills--parse-argument-names ()
  ,test
  (test)
  :doc "list input passes through, filtering numeric-only"
  (should (equal '("foo" "bar")
                 (niwa-skills--parse-argument-names
                  '("foo" "bar"))))
  (should (equal '("foo" "bar")
                 (niwa-skills--parse-argument-names
                  '("foo" "1" "bar" "2"))))

  :doc "string input splits on whitespace"
  (should (equal '("alpha" "beta")
                 (niwa-skills--parse-argument-names "alpha beta")))
  (should (equal '("alpha" "beta")
                 (niwa-skills--parse-argument-names "  alpha   beta  ")))

  :doc "string with numeric tokens drops them"
  (should (equal '("alpha" "gamma")
                 (niwa-skills--parse-argument-names "alpha 2 gamma")))

  :doc "nil and empty inputs return nil"
  (should (null (niwa-skills--parse-argument-names nil)))
  (should (null (niwa-skills--parse-argument-names "")))
  (should (null (niwa-skills--parse-argument-names '()))))

(niwa-deftest niwa-skills--first-paragraph ()
  ,test
  (test)
  :doc "first paragraph extracted from plain text body"
  (should (equal "Line one of paragraph one.\nLine two of paragraph one."
                 (niwa-skills--first-paragraph
                  "Line one of paragraph one.
Line two of paragraph one.

Paragraph two should be ignored.")))

  :doc "leading blank lines are skipped"
  (should (equal "First content."
                 (niwa-skills--first-paragraph
                  "

First content.

More.")))

  :doc "leading # markers are stripped"
  (should (equal "Some Header"
                 (niwa-skills--first-paragraph "# Some Header\n\nBody.")))
  (should (equal "Sub-section"
                 (niwa-skills--first-paragraph "## Sub-section\n")))

  :doc "empty body returns nil"
  (should (null (niwa-skills--first-paragraph "")))
  (should (null (niwa-skills--first-paragraph "   \n   \n")))
  (should (null (niwa-skills--first-paragraph nil))))


(niwa-deftest niwa-skills--parse-frontmatter ()
  ,test
  (test)
  :doc "frontmatter and body are parsed from the file"
  (let ((dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (let* ((file (niwa-skills-test--write-skill
                      dir "simplify" "name: simplify
description: Review changed code
" "Body text.\n"))
               (plist (niwa-skills--parse-frontmatter file)))
          (should (equal "simplify" (plist-get plist :name)))
          (should (equal "Review changed code" (plist-get plist :description)))
          (should (equal "Body text.\n" (plist-get plist :system))))
      (delete-directory dir t)))

  :doc "an unchanged file reuses its parse instead of running the YAML reader"
  (let ((dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (let* ((file (niwa-skills-test--write-skill
                      dir "simplify" "name: simplify
description: Review changed code
" "Body text.\n"))
               (parses 0))
          ;; Parse once uncounted so the YAML reader is loaded before it is
          ;; replaced, then count what a second and third parse cost.
          (should (niwa-skills--parse-frontmatter file))
          (let ((reader (symbol-function 'yaml-parse-string)))
            (cl-letf (((symbol-function 'yaml-parse-string)
                       (lambda (&rest arguments)
                         (cl-incf parses)
                         (apply reader arguments))))
              (should (equal "simplify"
                             (plist-get (niwa-skills--parse-frontmatter file)
                                        :name)))
              (should (= 0 parses))
              ;; An edited file no longer matches its fingerprint.
              (niwa-skills-test--write-skill
               dir "simplify" "name: simplify
description: Something else entirely
" "Body text.\n")
              (should (equal "Something else entirely"
                             (plist-get (niwa-skills--parse-frontmatter file)
                                        :description)))
              (should (= 1 parses)))))
      (delete-directory dir t)))

  :doc "the reused plist is copied, so a caller may modify its result"
  (let ((dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (let* ((file (niwa-skills-test--write-skill
                      dir "simplify" "name: simplify
description: Review changed code
" "Body text.\n"))
               (first (niwa-skills--parse-frontmatter file)))
          (setq first (plist-put first :description "clobbered"))
          (should (equal "clobbered" (plist-get first :description)))
          (should (equal "Review changed code"
                         (plist-get (niwa-skills--parse-frontmatter file)
                                    :description))))
      (delete-directory dir t)))

  :doc "supplied content is parsed instead of the file and is not cached"
  (let ((dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (let ((file (niwa-skills-test--write-skill
                     dir "simplify" "name: simplify
description: On disk
" "Body text.\n")))
          (should (equal "In memory"
                         (plist-get
                          (niwa-skills--parse-frontmatter
                           file "---\nname: simplify\ndescription: In memory\n---\n")
                          :description)))
          ;; The file, not the content just parsed, still answers for itself.
          (should (equal "On disk"
                         (plist-get (niwa-skills--parse-frontmatter file)
                                    :description))))
      (delete-directory dir t))))


;;
;;; Lazy body loading

(niwa-deftest niwa-skill-load-body ()
  ,test
  (test)
  :doc "returns the markdown body minus frontmatter"
  (let ((dir (make-temp-file "niwa-skills-test-" t)))
    (unwind-protect
        (let* ((_ (niwa-skills-test--write-skill
                   dir "simplify"
                   "name: simplify
description: Review changed code
"
                   "Step 1: read.\nStep 2: simplify.\n"))
               (skill (car (niwa-skills-scan dir '("."))))
               (body (niwa-skill-load-body skill)))
          (should (stringp body))
          (should (string-match-p "Step 1: read." body))
          ;; Cached on the struct.
          (should (equal body (niwa-skill-body skill)))
          ;; Second call returns the cached value without re-reading.
          (should (eq body (niwa-skill-load-body skill))))
      (delete-directory dir t))))


;;
;;; Configuration

(niwa-deftest niwa-skill-dirs ()
  ,test
  (test)
  :doc "defaults prefer local niwa/agents dirs before global dirs"
  (should (equal (list ".niwa/skills/"
                       ".agents/skills/"
                       (file-name-concat niwa-user-dir "skills/")
                       "~/.agents/skills/")
                 niwa-skill-dirs)))

(niwa-deftest niwa-skills--source-family-from-dir ()
  ,test
  (test)
  :doc "skills under a relocated `niwa-user-dir' belong to the niwa family"
  (let ((niwa-user-dir "/srv/state/niwa/"))
    (should (eq 'niwa (niwa-skills--source-family-from-dir
                          "/srv/state/niwa/skills/")))
    (should (eq 'niwa (niwa-skills--source-family-from-dir
                          "/proj/.niwa/skills")))
    (should (eq 'agents (niwa-skills--source-family-from-dir
                         "~/.agents/skills/")))
    (should-not (niwa-skills--source-family-from-dir
                 "/srv/state/other/skills/"))))


;;
;;; Persisted enablement

(niwa-deftest niwa-skills-source-key ()
  ,test
  (test)
  :doc "canonicalizes alternate paths to the same source identity"
  (let* ((root (make-temp-file "niwa-skills-source-key-" t))
         (source (file-name-concat root "SKILL.md"))
         (alias (file-name-concat root "alias.md")))
    (unwind-protect
        (progn
          (with-temp-file source)
          (make-symbolic-link source alias)
          (should (equal (niwa-skills-source-key source)
                         (niwa-skills-source-key alias)))
          (should-not (niwa-skills-source-key nil)))
      (delete-directory root t))))

(niwa-deftest niwa-skills--workspace-source-key ()
  ,test
  (test)
  :doc "uses a portable relative identity for workspace skill sources"
  (let* ((first-root (file-name-as-directory
                      (make-temp-file "niwa-skills-key-first-" t)))
         (second-root (file-name-as-directory
                       (make-temp-file "niwa-skills-key-second-" t)))
         (relative (file-name-concat ".niwa" "skills" "demo" "SKILL.md"))
         (first-workspace (niwa-skills-test--make-workspace first-root))
         (second-workspace (niwa-skills-test--make-workspace second-root)))
    (unwind-protect
        (should
         (equal
          (list (niwa-skills--workspace-source-key
                 (file-name-concat first-root relative) first-workspace)
                (niwa-skills--workspace-source-key
                 (file-name-concat second-root relative) second-workspace))
          (list (concat "workspace:" relative)
                (concat "workspace:" relative))))
      (delete-directory first-root t)
      (delete-directory second-root t)))

  :doc "normalizes an explicit default SSH port for workspace containment"
  (let ((workspace
         (niwa-skills-test--make-workspace "/ssh:host:/repo/")))
    (should
     (equal
      "workspace:.niwa/skills/demo/SKILL.md"
      (niwa-skills--workspace-source-key
       "/ssh:host#22:/repo/.niwa/skills/demo/SKILL.md"
       workspace)))))

(niwa-deftest niwa-skills--write-state
  (:vars* ((user-dir (make-temp-file "niwa-skills-write-" t))
           (niwa-user-dir (file-name-as-directory user-dir))
           (workspace (niwa-skills-test--make-workspace user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "a write that dies after truncating leaves the previous state"
  ;; The reader signals on anything malformed, so a half-written state breaks
  ;; every enablement check after the workspace's next skill rescan.
  (let ((first (niwa-skills-test--stateful-skill
                :name "one" :workspace workspace))
        (second (niwa-skills-test--stateful-skill
                 :name "two" :workspace workspace))
        (real (symbol-function 'write-region)))
    (niwa-skills-set-enabled first nil)
    (should-not (niwa-skills-skill-enabled-p first))
    (cl-letf (((symbol-function 'write-region)
               (lambda (_start _end filename &rest _)
                 (funcall real "" nil filename nil 'silent)
                 (error "Disk full"))))
      (should-error (niwa-skills-set-enabled second nil)))
    (should-not (niwa-skills-skill-enabled-p first)))

  :doc "a completed write leaves no staging file beside the state"
  (let ((skill (niwa-skills-test--stateful-skill
                :name "one" :workspace workspace)))
    (niwa-skills-set-enabled skill nil)
    (should (equal '("skills-state.el")
                   (directory-files (niwa-workspace-state-dir workspace) nil
                                    directory-files-no-dot-files-regexp))))

  :doc "remote skill state refuses to write without its live session"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-skills-remote-state-" t)))
         (remote-root (concat "/niwamock:skills:" root))
         (remote-workspace (niwa-skills-test--make-workspace remote-root))
         (niwa--session nil))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("skills")
          (should-error
           (niwa-skills--write-state
            '(:disabled-keys ("file:/demo")) remote-workspace)
           :type 'user-error)
          (should-not
           (file-exists-p
            (file-name-concat root ".niwa" "skills-state.el"))))
      (delete-directory root t)))

  :doc "remote skill state discloses, leases, and publishes on the target"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-skills-remote-state-" t)))
         (remote-root (concat "/niwamock:skills:" root))
         (remote-workspace (niwa-workspace--create
                            :type 'project :id remote-root :root remote-root
                            :name "test"))
         (session nil)
         (niwa--session nil)
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         prompts)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("skills")
          (setq session (niwa-session-create
                         "main" remote-workspace remote-root)
                niwa--session session)
          (setf (niwa-execution-target-readiness
                 (niwa-session-execution-target session))
                '(:status ready))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (prompt)
                             (push prompt prompts)
                             t)))
                  (niwa-skills--write-state
                   '(:disabled-keys ("file:/demo")) remote-workspace session))
                (should (= 1 (length prompts)))
                (should (niwa-session-save-path session))
                (should (eq 'owned
                            (plist-get (niwa-session-lease session) :state)))
                (should
                 (equal '(:disabled-keys ("file:/demo"))
                        (niwa-skills--read-state remote-workspace))))
            (when (niwa-session-save-path session)
              (niwa-session-durability-lease-release
               (niwa-session-save-path session) session))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-set-enabled
  (:vars* ((user-dir (make-temp-file "niwa-skills-state-" t))
           (niwa-user-dir (file-name-as-directory user-dir))
           (workspace (niwa-skills-test--make-workspace user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "disable and enable persist file-backed skill state"
  (let ((skill (niwa-skills-test--stateful-skill
                :name "visible" :workspace workspace)))
    (niwa-skills-set-enabled skill nil)
    (should-not (niwa-skills-skill-enabled-p skill))
    (niwa-skills-set-enabled skill t)
    (should (niwa-skills-skill-enabled-p skill)))

  :doc "updates cached skill copies in every live workspace session"
  (let* ((first (niwa-skills-test--stateful-skill
                 :name "visible" :workspace workspace))
         (second (niwa-skills-test--stateful-skill
                  :name "visible" :workspace workspace))
         (session (niwa-session--create
                   :workspace workspace :skills (list second)))
         (buffer (generate-new-buffer " *niwa-skills-workspace-state*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (niwa-skills-set-enabled first nil)
          (should-not (niwa-skills-skill-enabled-p second))
          (niwa-skills-set-enabled first t)
          (should (niwa-skills-skill-enabled-p second)))
      (kill-buffer buffer)))

  :doc "recomputes transitive model eligibility after enablement changes"
  (let ((niwa-skills-include-bundled nil))
    (niwa-skills-test--write-skill
     user-dir "child" "description: Child\n" "Child body")
    (niwa-skills-test--write-skill
     user-dir "parent" "description: Parent\n" "!$child")
    (let* ((skills (niwa-skills-scan nil (list user-dir) workspace))
           (session (niwa-session--create
                     :workspace workspace :skills skills))
           (parent (cl-find "parent" skills
                            :key #'niwa-skill-name :test #'equal))
           (child (cl-find "child" skills
                           :key #'niwa-skill-name :test #'equal)))
      (should (niwa-skill-effective-model-invocable-p parent))
      (niwa-skills-set-enabled child nil session)
      (should-not (niwa-skill-effective-model-invocable-p parent))
      (niwa-skills-set-enabled child t session)
      (should (niwa-skill-effective-model-invocable-p parent))))

  :doc "keeps the same external skill isolated between workspaces"
  (let* ((niwa-skills-include-bundled nil)
         (shared-root (make-temp-file "niwa-skills-shared-" t))
         (first-root (make-temp-file "niwa-skills-first-" t))
         (second-root (make-temp-file "niwa-skills-second-" t))
         (first-workspace (niwa-skills-test--make-workspace first-root))
         (second-workspace (niwa-skills-test--make-workspace second-root)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           shared-root "visible" "description: Visible\n" "Body")
          (let ((first (car (niwa-skills-scan
                             first-root (list shared-root) first-workspace)))
                (second (car (niwa-skills-scan
                              second-root (list shared-root) second-workspace))))
            (niwa-skills-set-enabled first nil)
            (should-not (niwa-skills-skill-enabled-p first))
            (should (niwa-skills-skill-enabled-p second))
            (should (file-exists-p
                     (file-name-concat first-root ".niwa"
                                       "skills-state.el")))
            (should-not (file-exists-p
                         (file-name-concat second-root ".niwa"
                                           "skills-state.el")))))
      (delete-directory shared-root t)
      (delete-directory first-root t)
      (delete-directory second-root t)))

  :doc "disable rejects skills without a workspace"
  (should-error
   (niwa-skills-set-enabled
    (niwa-skill--create :name "inline") nil)
   :type 'user-error)

  :doc "disable rejects skills without a stable source file"
  (should-error
   (niwa-skills-set-enabled
    (niwa-skill--create :name "inline" :workspace workspace) nil)
   :type 'user-error)

  :doc "stable source identity survives generated renaming"
  (let* ((root (make-temp-file "niwa-skills-state-root-" t))
         (skill-file (niwa-skills-test--write-skill
                      root "shared" "description: Shared\n" "Body"))
         (other-file (niwa-skills-test--write-skill
                      root "other" "description: Other\n" "Body"))
         (skill (niwa-skill--create
                 :name "shared" :source-file skill-file
                 :workspace workspace))
         (renamed (niwa-skill--create
                   :name "user:shared" :source-file skill-file
                   :workspace workspace))
         (other (niwa-skill--create
                 :name "shared" :source-file other-file
                 :workspace workspace)))
    (unwind-protect
        (progn
          (niwa-skills-set-enabled skill nil)
          (should (niwa-skills--disabled-keys workspace))
          (should (equal (niwa-skills--state-key skill)
                         (niwa-skills--state-key renamed)))
          (should-not (equal (niwa-skills--state-key skill)
                             (niwa-skills--state-key other))))
      (delete-directory root t))))

(niwa-deftest niwa-skills--read-state
  (:vars* ((user-dir (make-temp-file "niwa-skills-state-" t))
           (niwa-user-dir (file-name-as-directory user-dir))
           (workspace (niwa-skills-test--make-workspace user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "obsolete name-based state is rejected"
  (progn
    (make-directory (file-name-directory
                     (niwa-skills--state-file workspace)) t)
    (with-temp-file (niwa-skills--state-file workspace)
      (prin1 '(:disabled ("shared")) (current-buffer)))
    (should-error (niwa-skills--read-state workspace) :type 'error))

  :doc "a second form is rejected instead of silently ignored"
  (progn
    (make-directory (file-name-directory
                     (niwa-skills--state-file workspace)) t)
    (with-temp-file (niwa-skills--state-file workspace)
      (insert "(:disabled-keys nil) (:disabled-keys (\"workspace:x\"))\n"))
    (let ((err (should-error (niwa-skills--read-state workspace))))
      (should (string-match-p "Unsupported skill state format"
                              (error-message-string err))))))


;;
;;; Session installation

(niwa-deftest niwa-skills-install ()
  ,test
  (test)
  :doc "skills scanned from the workspace root end up on the session"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-ws-" t))
         (niwa-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (skill-root (file-name-concat root ".niwa/skills/")))
    (unwind-protect
        (let ((niwa-skill-dirs '(".niwa/skills/")))
          (niwa-skills-test--write-skill
           skill-root "grill-me"
           "name: grill-me
description: Interview relentlessly about a plan
" "Body")
          (niwa-skills-install session)
          (let ((skills (niwa-session-skills session)))
            (should (= 1 (length skills)))
            (should (equal "grill-me"
                           (niwa-skill-name (car skills))))
            (should (eq 'project
                        (niwa-skill-source (car skills)))))
          (should (equal "grill-me"
                         (niwa-skill-name
                          (niwa-session-get-skill session "grill-me"))))
          (should-not (niwa-session-get-skill session "missing")))
      (delete-directory root t)))

  :doc "registers the consumer buffer and clears any prior dirty flag"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-install-reg-" t))
         (niwa-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-install-reg*")))
    (unwind-protect
        (progn
          (niwa-skills-test--reset-watchers)
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: Alpha\n")
          (puthash buf t niwa-skills--dirty-buffers)
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should (= 1 (length (niwa-session-skills session))))
          (should-not (gethash buf niwa-skills--dirty-buffers))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name root))
                                 niwa-skills--dir-buffers))))
      (niwa-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "registers enabled plugin skill roots for hot reload"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-plugin-watch-root-" t))
         (workspace-root (file-name-concat root "workspace"))
         (user-dir (file-name-as-directory
                    (file-name-concat root "user")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-skill-dirs nil)
         (plugin-root
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo" "{\"name\":\"demo\",\"skills\":\"skills\"}"))
         (plugin-skills (file-name-as-directory
                         (file-name-concat plugin-root "skills")))
         (ws (niwa-skills-test--make-workspace workspace-root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-plugin-watch-root*")))
    (unwind-protect
        (progn
          (niwa-skills-test--reset-watchers)
          (make-directory plugin-skills t)
          ;; The plugin root is also the ancestor of a missing configured root.
          (setq niwa-skill-dirs (list (file-name-concat plugin-skills "nested")))
          (niwa-plugins-enable "demo" ws)
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should (null (niwa-session-skills session)))
          (puthash plugin-skills 'plugin-watch niwa-skills--watchers)
          (cl-letf (((symbol-function 'niwa-skills--resolve-dir)
                     (lambda (&rest _) (ert-fail "Notification resolved a path"))))
            (niwa-skills--watch-callback
             (list 'plugin-watch 'created (file-name-concat plugin-skills "new-skill"))))
          (should (gethash buf niwa-skills--dirty-buffers))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name plugin-skills))
                                 niwa-skills--dir-buffers))))
      (niwa-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "rescans preserve active path-scoped skills"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-install-active-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-install-active*")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha"
           "name: alpha
description: Alpha
paths:
  - \"*.el\"
")
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (let ((skill (niwa-session-get-skill session "alpha")))
            (should skill)
            (should-not (niwa-skill-active-p skill)))
          (niwa-skills-maybe-activate session "src/foo.el")
          (should (niwa-skill-active-p
                   (niwa-session-get-skill session "alpha")))
          (niwa-skills-test--write-skill
           root "alpha"
           "name: alpha
description: Alpha updated
paths:
  - \"*.el\"
")
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (let ((skill (niwa-session-get-skill session "alpha")))
            (should (equal "Alpha updated"
                           (niwa-skill-description skill)))
            (should (niwa-skill-active-p skill))))
      (niwa-skills-test--reset-watchers)
      (kill-buffer buf)
      (delete-directory root t)))
  :doc "watch-files install creates a live descriptor for each registered dir"
  (progn
    (skip-unless (niwa-skills--filenotify-supported-p))
    (let* ((niwa-skills-include-bundled nil)
           (niwa-skills-check-for-modifications '(watch-files))
           (root (make-temp-file "niwa-skills-watcher-" t))
           (niwa-skill-dirs (list root))
           (ws (niwa-skills-test--make-workspace root))
           (session (niwa-session-create "main" ws))
           (buf (generate-new-buffer " *niwa-test-watcher*")))
      (unwind-protect
          (progn
            (niwa-skills-test--write-skill
             root "alpha" "name: alpha\ndescription: A\n")
            (with-current-buffer buf
              (niwa-skills-install session buf))
            (let* ((dir (file-name-as-directory (expand-file-name root)))
                   (desc (gethash dir niwa-skills--watchers)))
              (should desc)
              (should (file-notify-valid-p desc))))
        (kill-buffer buf)
        (delete-directory root t))))

  :doc "watch-files registers existing empty subdirectories"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-empty-subdir-" t))
         (empty (file-name-as-directory (file-name-concat root "empty")))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-empty-subdir*")))
    (unwind-protect
        (progn
          (make-directory empty t)
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should (memq buf (gethash (file-name-as-directory
                                      (expand-file-name root))
                                     niwa-skills--dir-buffers)))
          (should (memq buf (gethash empty niwa-skills--dir-buffers))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "registers missing configured roots and their existing parent"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-missing-root-" t))
         (missing (file-name-as-directory
                   (file-name-concat root ".niwa/skills")))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-missing-root*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should (memq buf (gethash missing niwa-skills--dir-buffers)))
          (should (memq buf
                        (gethash (file-name-as-directory
                                  (expand-file-name root))
                                 niwa-skills--dir-buffers)))
          (should (niwa-skills--file-under-watched-dirs
                   (file-name-concat missing "alpha/SKILL.md"))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "ensure-watcher is a no-op without watch-files in the strategy set"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-no-watch-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-no-watch*")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should-not (gethash (file-name-as-directory
                                (expand-file-name root))
                               niwa-skills--watchers)))
      (kill-buffer buf)
      (delete-directory root t))))


;;
;;; File watchers

(niwa-deftest niwa-skills--ensure-watcher
  (:vars ((noninteractive nil))
   :before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "a batch Emacs never installs a watcher"
  (let ((noninteractive t)
        (watch-calls 0))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
              ((symbol-function 'file-notify-add-watch)
               (lambda (&rest _args)
                 (cl-incf watch-calls)
                 'descriptor)))
      (niwa-skills--ensure-watcher
       (file-name-as-directory temporary-file-directory)))
    (should (= 0 watch-calls))
    (should (= 0 (hash-table-count niwa-skills--watchers))))

  :doc "skips unsupported remote watches and reports once per target state"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (dir "/ssh:user@host:/srv/project/.niwa/skills/")
         (watch-calls 0)
         messages)
    (setf (niwa-execution-target-readiness target)
          '(:status ready :capabilities nil))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
              ((symbol-function 'file-notify-add-watch)
               (lambda (&rest _args)
                 (cl-incf watch-calls)
                 'descriptor))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (niwa-skills--ensure-watcher dir target)
      (niwa-skills--ensure-watcher dir target))
    (should (= 0 watch-calls))
    (should (= 1 (length messages)))
    (should (string-match-p "inotifywait or gio" (car messages))))

  :doc "uses native file notification when either remote backend exists"
  (dolist (capability '(gio inotifywait))
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (dir (format "/ssh:user@host:/srv/project/%s/" capability))
           (descriptor (list 'descriptor capability)))
      (setf (niwa-execution-target-readiness target)
            `(:status ready
              :capabilities ((,capability . ,(format "/usr/bin/%s"
                                                      capability)))))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (&rest _args) descriptor)))
        (niwa-skills--ensure-watcher dir target))
      (should (equal descriptor
                     (gethash (file-name-as-directory dir)
                              niwa-skills--watchers)))))

  :doc "still watches local skill directories for a remote session"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (dir (file-name-as-directory temporary-file-directory)))
    (setf (niwa-execution-target-readiness target)
          '(:status ready :capabilities nil))
    (cl-letf (((symbol-function 'file-notify-add-watch)
               (lambda (&rest _args) 'local-descriptor)))
      (niwa-skills--ensure-watcher dir target))
    (should (eq 'local-descriptor
                (gethash dir niwa-skills--watchers))))

  :doc "unexpected watcher warnings preserve the caller's current buffer"
  (let ((origin (generate-new-buffer " *niwa-watch-origin*"))
        (other (generate-new-buffer " *niwa-watch-other*")))
    (unwind-protect
        (with-current-buffer origin
          (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
                    ((symbol-function 'file-notify-add-watch)
                     (lambda (&rest _args) (error "Watcher failed")))
                    ((symbol-function 'display-warning)
                     (lambda (&rest _args) (set-buffer other))))
            (niwa-skills--ensure-watcher
             (file-name-as-directory temporary-file-directory)))
          (should (eq origin (current-buffer))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (when (buffer-live-p other) (kill-buffer other)))))

(niwa-deftest niwa-skills--register-buffer
  (:vars ((noninteractive nil))
   :before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "retains remote directory registration when watching is unavailable"
  (let* ((niwa-skills-check-for-modifications '(watch-files))
         (target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (dir "/ssh:user@host:/srv/project/.niwa/skills/")
         (buffer (generate-new-buffer " *niwa-remote-skill-consumer*")))
    (setf (niwa-execution-target-readiness target)
          '(:status ready :capabilities nil))
    (unwind-protect
        (cl-letf (((symbol-function 'file-directory-p) (lambda (_dir) t))
                  ((symbol-function 'file-notify-add-watch)
                   (lambda (&rest _args)
                     (ert-fail "remote watcher was installed")))
                  ((symbol-function 'message) #'ignore))
          (niwa-skills--register-buffer buffer (list dir) target)
          (should (memq buffer
                        (gethash (file-name-as-directory dir)
                                 niwa-skills--dir-buffers)))
          (should-not (gethash (file-name-as-directory dir)
                               niwa-skills--watchers)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))


;;
;;; Path-scoped activation

(niwa-deftest niwa-skills--glob-matches-p ()
  ,test
  (test)
  :doc "matches against basename and full path"
  (should (niwa-skills--glob-matches-p "*.el" "foo.el"))
  (should (niwa-skills--glob-matches-p "*.el" "/tmp/foo.el"))
  (should-not (niwa-skills--glob-matches-p "*.py" "foo.el")))

(niwa-deftest niwa-skills--path-matches-p ()
  ,test
  (test)
  :doc "returns non-nil when any pattern matches"
  (should (niwa-skills--path-matches-p
           "foo.el" '("*.py" "*.el")))

  :doc "nil for empty pattern list or nil path"
  (should-not (niwa-skills--path-matches-p "foo.el" nil))
  (should-not (niwa-skills--path-matches-p nil '("*.el"))))

(niwa-deftest niwa-skills-maybe-activate ()
  ,test
  (test)
  :doc "flips dormant skills whose patterns match path"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "a" :root "/tmp/a" :name "a"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (elisp (niwa-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil))
         (python (niwa-skill--create
                  :name "python" :path-patterns '("*.py") :active-p nil))
         (always (niwa-skill--create :name "always" :active-p t)))
    (setf (niwa-session-skills session) (list elisp python always))
    (let ((activated (niwa-skills-maybe-activate session "src/foo.el")))
      (should (equal '("elisp")
                     (mapcar #'niwa-skill-name activated))))
    (should (niwa-skill-active-p elisp))
    (should-not (niwa-skill-active-p python))
    ;; Second call on the same path no-ops.
    (should (null (niwa-skills-maybe-activate session "src/bar.el")))))



;;
;;; Modification detection and reload

(niwa-deftest niwa-skills--before-save-hook
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "saving a SKILL.md under a registered dir marks consumers dirty"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "niwa-skills-save-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-save*"))
         (skill-file (niwa-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (niwa-skills-install session buf))
          ;; A save in a non-skill file should NOT flip the flag.
          (let ((unrelated (make-temp-file "niwa-unrelated-" nil ".md")))
            (with-temp-buffer
              (setq buffer-file-name unrelated)
              (niwa-skills--before-save-hook)
              (should-not (gethash buf niwa-skills--dirty-buffers)))
            (delete-file unrelated))
          ;; Save of a SKILL.md under the root flips the flag.
          (with-temp-buffer
            (setq buffer-file-name skill-file)
            (niwa-skills--before-save-hook)
            (should (gethash buf niwa-skills--dirty-buffers))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "saving a nested SKILL.md marks every containing registered dir dirty"
  (let* ((root (make-temp-file "niwa-skills-save-overlap-" t))
         (parent (file-name-as-directory
                  (file-name-concat root ".niwa/skills")))
         (child (file-name-as-directory
                 (file-name-concat parent "team")))
         (parent-buf (generate-new-buffer
                      " *niwa-test-save-overlap-parent*"))
         (child-buf (generate-new-buffer
                     " *niwa-test-save-overlap-child*"))
         (skill-file (file-name-concat child "foo/SKILL.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory skill-file) t)
          (puthash parent (list parent-buf) niwa-skills--dir-buffers)
          (puthash child (list child-buf) niwa-skills--dir-buffers)
          (with-temp-buffer
            (setq buffer-file-name skill-file)
            (niwa-skills--before-save-hook))
          (should (gethash parent-buf niwa-skills--dirty-buffers))
          (should (gethash child-buf niwa-skills--dirty-buffers)))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
      (when (buffer-live-p child-buf) (kill-buffer child-buf))
      (delete-directory root t))))

(niwa-deftest niwa-skills-ensure-fresh
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "ensure-fresh consumes the dirty flag and rescans new skills"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "niwa-skills-rescan-" t))
         (niwa-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-rescan*")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should (= 1 (length (niwa-session-skills session))))
          ;; New skill appears on disk; flip dirty flag and pull.
          (niwa-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (niwa-skills--mark-buffer-dirty buf)
          (niwa-skills-ensure-fresh buf session)
          (should (= 2 (length (niwa-session-skills session))))
          (should-not (gethash buf niwa-skills--dirty-buffers))
          (should (cl-find "beta" (niwa-session-skills session)
                           :key #'niwa-skill-name :test #'equal)))
      (kill-buffer buf)
      (delete-directory root t))))

(niwa-deftest niwa-skills--watch-event-relevant-p ()
  ,test
  (test)
  :doc "remote ancestor matching uses cached roots and both rename paths"
  (with-temp-buffer
    (let ((directory "/ssh:skill-host:/project/")
          (default-directory temporary-file-directory))
      (setq-local niwa-skills--watch-roots
                  '("/ssh:skill-host:/project/.agents/skills/"))
      (cl-letf (((symbol-function 'niwa-skills--resolve-dir)
                 (lambda (&rest _) (ert-fail "Notification resolved a root")))
                ((symbol-function 'file-truename)
                 (lambda (&rest _) (ert-fail "Notification accessed the filesystem"))))
        (should-not (niwa-skills--watch-event-relevant-p
                     (current-buffer) directory '("unrelated")))
        (should-not (niwa-skills--watch-event-relevant-p
                     (current-buffer) directory '(".agents-backup")))
        (should (niwa-skills--watch-event-relevant-p
                 (current-buffer) directory '("temporary" ".agents")))
        (should (niwa-skills--watch-event-relevant-p
                 (current-buffer) directory '(".agents" "temporary")))
        (should (niwa-skills--watch-event-relevant-p
                 (current-buffer) "/ssh:skill-host:/project/.agents/skills/"
                 '("notes.txt")))))))

(niwa-deftest niwa-skills--watch-callback
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "callback marks consumers of the firing directory dirty"
  (let* ((dir (file-name-as-directory
               (expand-file-name "fake/" temporary-file-directory)))
         (buf (generate-new-buffer " *niwa-test-watch*"))
         (descriptor 'fake-descriptor))
    (unwind-protect
        (progn
          (puthash dir descriptor niwa-skills--watchers)
          (puthash dir (list buf) niwa-skills--dir-buffers)
          ;; Synthetic event tuple matching `file-notify' shape.
          (niwa-skills--watch-callback
           (list descriptor 'created (concat dir "alpha")))
          (should (gethash buf niwa-skills--dirty-buffers))
          ;; `stopped' must not flip the flag.
          (remhash buf niwa-skills--dirty-buffers)
          (niwa-skills--watch-callback
           (list descriptor 'stopped (concat dir "alpha")))
          (should-not (gethash buf niwa-skills--dirty-buffers)))
      (kill-buffer buf)))

  :doc "the ancestor watched for a missing root rescans only on its path"
  ;; Regression: every turn writes beside `.niwa/skills/' and in the
  ;; workspace root, and each write rescanned the skills before the next
  ;; provider request.
  (let* ((root (file-name-as-directory (make-temp-file "niwa-skills-watch-" t)))
         (niwa-skill-dirs '(".niwa/skills/" ".agents/skills/"))
         (niwa-skills-check-for-modifications '(watch-files))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-watch-ancestor*"))
         (state (file-name-concat root ".niwa/"))
         (dirs nil))
    (unwind-protect
        (progn
          (make-directory state)
          (with-current-buffer buf
            (setq-local niwa--session session)
            (setq-local niwa-skills--watch-roots
                        (niwa-skills--resolved-roots root)))
          (setq dirs (niwa-skills--collect-roots root nil ws))
          (niwa-skills--register-buffer
           buf dirs nil (mapcar (lambda (raw) (car (niwa-skills--resolve-dir raw root)))
                                niwa-skill-dirs))
          (should (member state dirs))
          (should (member root dirs))
          (cl-flet ((event (dir &rest files)
                      (remhash buf niwa-skills--dirty-buffers)
                      (puthash dir 'descriptor niwa-skills--watchers)
                      (puthash dir (list buf) niwa-skills--dir-buffers)
                      (niwa-skills--watch-callback
                       (append (list 'descriptor 'changed) files))
                      (remhash dir niwa-skills--watchers)
                      (remhash dir niwa-skills--dir-buffers)
                      (gethash buf niwa-skills--dirty-buffers)))
            (should-not (event state (file-name-concat state "input-history.el")))
            (should-not (event root (file-name-concat root "niwa-publication-x")))
            (should (event state (file-name-concat state "skills")))
            (should (event root (file-name-concat root ".agents")))
            (should (event state (file-name-concat state "tmp")
                           (file-name-concat state "skills")))
            ;; Inside a skill directory every change still counts.
            (should (event (file-name-concat state "skills/")
                           (file-name-concat state "skills/notes.txt")))))
      (kill-buffer buf)
      (delete-directory root t))))

(niwa-deftest niwa-skills--stat-recheck
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "external mtime change flips the buffer dirty"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "niwa-skills-stat-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-stat*"))
         (skill-file (niwa-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should-not (gethash buf niwa-skills--dirty-buffers))
          ;; Bump mtime via set-file-times: deterministic, no need to
          ;; sleep for the filesystem clock granularity.
          (set-file-times skill-file (time-add (current-time) 5))
          (niwa-skills--stat-recheck session buf)
          (should (gethash buf niwa-skills--dirty-buffers)))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "one buffer's rescan does not hide stat changes from another buffer"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "niwa-skills-stat-many-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session-a (niwa-session-create "main-a" ws))
         (session-b (niwa-session-create "main-b" ws))
         (buf-a (generate-new-buffer " *niwa-test-stat-a*"))
         (buf-b (generate-new-buffer " *niwa-test-stat-b*"))
         (skill-file (niwa-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (niwa-skills-install session-a buf-a))
          (with-current-buffer buf-b
            (niwa-skills-install session-b buf-b))
          (set-file-times skill-file (time-add (current-time) 5))
          (niwa-skills--stat-recheck session-a buf-a)
          (should (gethash buf-a niwa-skills--dirty-buffers))
          ;; Rescanning A refreshes only A's stat baseline.
          (niwa-skills-ensure-fresh buf-a session-a)
          (should-not (gethash buf-a niwa-skills--dirty-buffers))
          ;; B still has its own older baseline and must become dirty.
          (niwa-skills--stat-recheck session-b buf-b)
          (should (gethash buf-b niwa-skills--dirty-buffers)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (delete-directory root t)))

  :doc "deleted known skill file flips the buffer dirty"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "niwa-skills-stat-delete-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-stat-delete*"))
         (skill-file (niwa-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (niwa-skills-install session buf))
          (should-not (gethash buf niwa-skills--dirty-buffers))
          (delete-file skill-file)
          (niwa-skills--stat-recheck session buf)
          (should (gethash buf niwa-skills--dirty-buffers)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory root t)))
)

(niwa-deftest niwa-skills-rescan
  (:quiet t :before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "manual rescan picks up newly added skills"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-manual-" t))
         (niwa-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-manual*")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (setq-local niwa--session session)
            (niwa-skills-install session buf))
          (should (= 1 (length (niwa-session-skills session))))
          (niwa-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (with-current-buffer buf
            (niwa-skills-rescan))
          (should (= 2 (length (niwa-session-skills session)))))
      (kill-buffer buf)
      (delete-directory root t)))

  :doc "errors when no session is bound in the buffer"
  (with-temp-buffer
    (setq-local niwa--workspace
                (niwa-workspace--create :root "/workspace/rescan/"))
    (should (equal "niwa [/workspace/rescan/]: No niwa session in this buffer"
                   (error-message-string
                    (should-error (niwa-skills-rescan) :type 'user-error))))))

(niwa-deftest niwa-skills--unregister-buffer
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "unregister removes a buffer from every registered directory"
  (let* ((buf-a (generate-new-buffer " *niwa-test-unregister-a*"))
         (buf-b (generate-new-buffer " *niwa-test-unregister-b*"))
         (root (file-name-as-directory
                (expand-file-name "niwa-skills-unregister/"
                                  temporary-file-directory)))
         (parent (file-name-as-directory
                  (file-name-concat root ".niwa/skills")))
         (child (file-name-as-directory
                 (file-name-concat parent "team"))))
    (unwind-protect
        (progn
          (puthash parent (list buf-a buf-b) niwa-skills--dir-buffers)
          (puthash child (list buf-a) niwa-skills--dir-buffers)
          (puthash buf-a t niwa-skills--dirty-buffers)
          (niwa-skills--unregister-buffer buf-a)
          (should (equal (gethash parent niwa-skills--dir-buffers)
                         (list buf-b)))
          (should-not (gethash child niwa-skills--dir-buffers))
          (should-not (gethash buf-a niwa-skills--dirty-buffers)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))))
  :doc "unregistering a buffer clears only that buffer's stat baseline"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(stat-when-checking))
         (root (make-temp-file "niwa-skills-stat-clear-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session-a (niwa-session-create "main-a" ws))
         (session-b (niwa-session-create "main-b" ws))
         (buf-a (generate-new-buffer " *niwa-test-stat-clear-a*"))
         (buf-b (generate-new-buffer " *niwa-test-stat-clear-b*"))
         (skill-file (niwa-skills-test--write-skill
                      root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (niwa-skills-install session-a buf-a))
          (with-current-buffer buf-b
            (niwa-skills-install session-b buf-b))
          (should (gethash (niwa-skills--mtime-cache-key buf-a skill-file)
                           niwa-skills--mtime-cache))
          (should (gethash (niwa-skills--mtime-cache-key buf-b skill-file)
                           niwa-skills--mtime-cache))
          (niwa-skills--unregister-buffer buf-a)
          (should-not (gethash (niwa-skills--mtime-cache-key buf-a skill-file)
                               niwa-skills--mtime-cache))
          (should (gethash (niwa-skills--mtime-cache-key buf-b skill-file)
                           niwa-skills--mtime-cache)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (delete-directory root t))))

(niwa-deftest niwa-skills--release-on-kill
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each (niwa-skills-test--reset-watchers))
  ,test
  (test)
  :doc "kill drops the buffer from the registry and tears down orphan watchers"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-kill-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-kill*"))
         (dir (file-name-as-directory root)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (niwa-skills-install session buf)
            (add-hook 'kill-buffer-hook
                      #'niwa-skills--release-on-kill nil t))
          (should (memq buf (gethash dir niwa-skills--dir-buffers)))
          (kill-buffer buf)
          (should-not (gethash dir niwa-skills--dir-buffers))
          (should-not (gethash dir niwa-skills--watchers)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory root t))))

(niwa-deftest niwa-skills-uninstall-hot-reload
  (:before-each (niwa-skills-test--reset-watchers)
   :after-each
   (progn
     (remove-hook 'before-save-hook #'niwa-skills--before-save-hook)
     (niwa-skills-test--reset-watchers)))
  ,test
  (test)
  :doc "removes global hooks, buffer kill hooks, watchers, and registry state"
  (let* ((buf (generate-new-buffer " *niwa-test-hot-uninstall*"))
         (dir (file-name-as-directory
               (expand-file-name "fake-skill-root/" temporary-file-directory)))
         (file (file-name-concat dir "alpha/SKILL.md"))
         (descriptor 'fake-descriptor))
    (unwind-protect
        (progn
          (add-hook 'before-save-hook #'niwa-skills--before-save-hook)
          (with-current-buffer buf
            (add-hook 'kill-buffer-hook
                      #'niwa-skills--release-on-kill nil t))
          (puthash dir descriptor niwa-skills--watchers)
          (puthash '(:method ssh :host "host") nil
                   niwa-skills--remote-watch-states)
          (puthash dir (list buf) niwa-skills--dir-buffers)
          (puthash buf t niwa-skills--dirty-buffers)
          (puthash (niwa-skills--mtime-cache-key buf file)
                   (current-time)
                   niwa-skills--mtime-cache)
          (puthash file '(fingerprint . parsed)
                   niwa-skills--frontmatter-cache)
          (niwa-skills-uninstall-hot-reload)
          (should-not (memq #'niwa-skills--before-save-hook
                            before-save-hook))
          (should (= 0 (hash-table-count niwa-skills--watchers)))
          (should (= 0 (hash-table-count niwa-skills--dir-buffers)))
          (should (= 0 (hash-table-count niwa-skills--dirty-buffers)))
          (should (= 0 (hash-table-count niwa-skills--mtime-cache)))
          (should (= 0 (hash-table-count
                        niwa-skills--frontmatter-cache)))
          (should (= 0 (hash-table-count
                        niwa-skills--remote-watch-states)))
          (with-current-buffer buf
            (should-not (memq #'niwa-skills--release-on-kill
                              kill-buffer-hook))))
      (when (buffer-live-p buf) (kill-buffer buf)))))





(provide 'test-niwa-skills-core)
;;; test-niwa-skills-core.el ends here
