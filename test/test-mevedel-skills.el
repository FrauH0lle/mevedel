;;; test-mevedel-skills.el --- Tests for skills foundation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-skills)
(require 'gptel)
(require 'mevedel-permissions)
(require 'mevedel-compact)
(require 'mevedel-tool-registry)
(require 'mevedel-agents)
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


;;
;;; Helpers

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

  :doc "earlier directory wins on name conflict"
  (let* ((mevedel-skills-include-bundled nil)
         (root-a (make-temp-file "mevedel-skills-a-" t))
         (root-b (make-temp-file "mevedel-skills-b-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root-a "shared"
           "name: shared
description: From root A
" "A body")
          (mevedel-skills-test--write-skill
           root-b "shared"
           "name: shared
description: From root B
" "B body")
          (let ((skills (mevedel-skills-scan nil (list root-a root-b))))
            (should (= 1 (length skills)))
            (should (equal "From root A"
                           (mevedel-skill-description (car skills))))))
      (delete-directory root-a t)
      (delete-directory root-b t)))

  :doc "skills missing a description fall back to the first body paragraph"
  ;; Spec 22 Failure Modes: \\='Missing description' \xe2\x86\x92 use first non-empty
  ;; markdown paragraph/header.  The skill loads with the body-derived
  ;; description rather than being skipped.
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
         (dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
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

  :doc "bundled coordinator skill is discoverable by default"
  (let ((skills (mevedel-skills-scan nil nil)))
    (should (cl-find-if
             (lambda (s)
               (and (equal "coordinator" (mevedel-skill-name s))
                    (eq 'bundled (mevedel-skill-source s))))
             skills)))

  :doc "bundled skills are suppressed when include-bundled is nil"
  (let* ((mevedel-skills-include-bundled nil)
         (skills (mevedel-skills-scan nil nil)))
    (should-not (cl-find-if
                 (lambda (s) (eq 'bundled (mevedel-skill-source s)))
                 skills)))

  :doc "frontmatter `name' wins; missing `name' falls back to directory"
  ;; Spec 22 Data Model: name resolution prefers frontmatter over directory.
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
  ;; Spec 22 Argument Substitution: numeric-only names cannot shadow $0/$1.
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
  ;; Spec 22 Failure Modes: \\='Invalid YAML' \xe2\x86\x92 skip skill, log warning.
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
  ;; Spec 22 §"Implementation Plan" item 3 final bullet:
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
            ;; with-bad-entry: malformed `bash(rm)' is dropped, valid
            ;; entries survive; the skill still loads.
            (should (equal '(("Read" :action allow)
                             ("Bash" :pattern "echo *" :action allow))
                           (mevedel-skill-allowed-tool-rules bad)))
            (should (cl-some (lambda (m)
                               (string-match-p "bash(rm)" m))
                             warnings))))
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
;;; Session installation

(mevedel-deftest mevedel-skills-install ()
  ,test
  (test)
  :doc "skills scanned from the workspace root end up on the session"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-ws-" t))
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
      (delete-directory root t))))


;;
;;; Phase B — substitution, shell injection, execution


;;
;;; Phase 3: Request-scoped skill context (spec 22)

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
                        :model 'haiku
                        :effort 'high
                        :invoked-skills records))
      (mevedel-skills--drain-pending-context request)
      (should (equal rules
                     (mevedel-request-skill-permission-rules request)))
      (should (eq 'haiku (mevedel-request-skill-model-override request)))
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
                   :skill-model-override 'haiku)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (should (eq 'haiku (mevedel-skills--current-model-override)))))

  :doc "invocation override wins over request override (innermost)"
  (let* ((agent (mevedel-agent--create :name "tester"))
         (invocation
          (mevedel-agent-invocation--create
           :agent agent :skill-model-override 'sonnet))
         (request (mevedel-request--create
                   :skill-model-override 'haiku)))
    (with-temp-buffer
      (setq-local mevedel--current-request request)
      (setq-local mevedel--agent-invocation invocation)
      (should (eq 'sonnet (mevedel-skills--current-model-override)))))

  :doc "no override anywhere returns nil"
  (with-temp-buffer
    (should (null (mevedel-skills--current-model-override)))))


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

  :doc "$0/$1/etc are zero-based per ccs / spec 22"
  ;; Spec 22 Argument Substitution: zero-based, no compatibility for
  ;; one-based.  Under this design $1 means the SECOND token.
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
  ;; Spec 22: quoted strings stay together, even with whitespace inside.
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("title"))))
    (should (equal "title is hello world"
                   (mevedel-skills--substitute-vars
                    "title is $title" "\"hello world\"" nil skill))))

  :doc "ARGUMENTS: appended when args supplied but no placeholder substituted"
  ;; Spec 22 Argument Substitution: append fires only when no placeholder
  ;; matched and raw args are non-empty.
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

  :doc "${CLAUDE_*} substitutions do not trigger append-fallback"
  ;; Mevedel-specific subs run AFTER the placeholder check so they
  ;; don't suppress the append.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "x")))
    (should (equal "id=main\n\nARGUMENTS: hello"
                   (mevedel-skills--substitute-vars
                    "id=${CLAUDE_SESSION_ID}" "hello" session skill)))))

(defmacro mevedel-skills-test--with-bash-allowed (&rest body)
  "Run BODY with the Bash permission check forced to allow.
Spec 22 §\"Shell Injection\" requires `:trust-literal-p t' to skip
the dangerous-commands and fail-safe overlays, but still consults
the rules.  Tests need a deterministic permit so they can assert
on the substituted output without depending on the user's
defcustom configuration."
  `(cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
              (lambda (_command &rest _args) 'allow)))
     ,@body))

(mevedel-deftest mevedel-skills--run-shell-injections ()
  ,test
  (test)
  :doc "inline !`cmd` is replaced with stdout"
  (mevedel-skills-test--with-bash-allowed
    (should (equal "value=hello"
                   (mevedel-skills--run-shell-injections
                    "value=!`echo hello`"))))

  :doc "multiple inline injections in the same line"
  (mevedel-skills-test--with-bash-allowed
    (should (equal "a=1 b=2"
                   (mevedel-skills--run-shell-injections
                    "a=!`echo 1` b=!`echo 2`"))))

  :doc "fenced ```! block is replaced with stdout"
  (mevedel-skills-test--with-bash-allowed
    (should (equal "prefix\nline1\nline2\nsuffix"
                   (mevedel-skills--run-shell-injections
                    "prefix\n```!\necho line1\necho line2\n```\nsuffix"))))

  :doc "non-zero exit aborts via mevedel-skills-shell-abort"
  ;; Spec 22 §"Shell Injection": non-zero exit signals abort with
  ;; reason=shell-failure; the caller (`--invoke-inline') converts
  ;; to a `:status error' outcome.
  (mevedel-skills-test--with-bash-allowed
    (should-error
     (mevedel-skills--run-shell-injections "!`false`")
     :type 'mevedel-skills-shell-abort))

  :doc "permission deny aborts with reason=permission-denied"
  (cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
             (lambda (_c &rest _) 'deny)))
    (let ((err (should-error
                (mevedel-skills--run-shell-injections "!`anything`")
                :type 'mevedel-skills-shell-abort)))
      (should (eq 'permission-denied (nth 1 err)))))

  :doc "permission ask aborts with reason=permission-denied"
  ;; Spec 22 §"Shell Injection": ask is treated like deny because
  ;; skill body shell prep cannot interactively prompt.
  (cl-letf (((symbol-function 'mevedel-tools--check-bash-permission)
             (lambda (_c &rest _) 'ask)))
    (let ((err (should-error
                (mevedel-skills--run-shell-injections "!`anything`")
                :type 'mevedel-skills-shell-abort)))
      (should (eq 'permission-denied (nth 1 err))))))

(mevedel-deftest mevedel-skills--check-bash-permission/trust-literal ()
  ,test
  (test)
  :doc ":trust-literal-p t skips dangerous-commands downgrade"
  ;; Spec 22 §"Shell Injection" §"Effects under :trust-literal-p":
  ;; the dangerous-commands list does NOT downgrade allow to ask.
  (let ((mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-rules '(("Bash" :pattern "rm *" :action allow))))
    (should (eq 'allow
                (mevedel-tools--check-bash-permission
                 "rm /tmp/foo" :trust-literal-p t)))
    (should (eq 'ask
                (mevedel-tools--check-bash-permission "rm /tmp/foo"))))

  :doc ":trust-literal-p t skips fail-safe-complex-syntax"
  ;; Spec 22: fail-safe complex-syntax check is bypassed.
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
                 "rm /tmp/foo" :trust-literal-p t)))))

(mevedel-deftest mevedel-skills--prepare-body ()
  ,test
  (test)
  :doc "prepares a body with lazy body load, substitution, and shell"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t))
         (ws (mevedel-workspace--create
              :type 'test :id dir :root dir :name "prep"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "greeter"
           "name: greeter
description: Greet the user
"
           "Hello $0 from ${CLAUDE_SESSION_ID}: !`echo ready`.")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (skill (car skills))
                 (body (mevedel-skills-test--with-bash-allowed
                        (mevedel-skills--prepare-body
                         skill "world" session))))
            (should (equal "Hello world from main: ready." body))))
      (delete-directory dir t))))

(mevedel-deftest mevedel-skills--prepare-body-fork ()
  ,test
  (test)
  :doc "fork-context skills return the delegation instruction body"
  ;; For fork skills the slash command and Skill tool do NOT inline
  ;; the agent's full SKILL.md (which is the agent's system prompt
  ;; baked in via `:prompt-file' on the agent definition).  Instead
  ;; `--prepare-body' returns a short instruction telling main to
  ;; dispatch the named agent via the Agent tool itself.
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "f" :root "/tmp/f" :name "f"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "coordinator"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "coordinator"))
         (body (mevedel-skills--prepare-body skill "investigate X" session)))
    (should (stringp body))
    (should (string-match-p "Use the `coordinator` agent" body))
    (should (string-match-p "subagent_type=\"coordinator\"" body))
    (should (string-match-p "run_in_background=true" body))
    (should (string-match-p "investigate X" body))
    ;; The agent's SKILL.md body is NOT inlined -- that body is the
    ;; agent's system prompt, double-printing it confuses main.
    (should-not (string-match-p "should-not-appear" body)))

  :doc "inline-context skills still substitute SKILL.md body"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t))
         (ws (mevedel-workspace--create
              :type 'test :id dir :root dir :name "inline"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "simplify"
           "name: simplify
description: Clean it up
"
           "Do $ARGUMENTS cleanly.")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (skill (car skills))
                 (body (mevedel-skills--prepare-body
                        skill "the refactor" session)))
            (should (equal "Do the refactor cleanly." body))))
      (delete-directory dir t))))


;;
;;; Phase 5: mevedel-skills-invoke (unified invocation API)

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
                 :model "haiku"
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
        (should (eq 'haiku (plist-get stash :model)))
        (should (equal '(("Read" :action allow))
                       (plist-get stash :permission-rules)))
        (should (= 1 (length (plist-get stash :invoked-skills))))))
    (should (eq 'ok (plist-get outcome :status))))

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
                 :model "haiku"
                 :allowed-tool-rules
                 '(("Bash" :pattern "ls" :action allow)))))
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel--current-request request)
      (mevedel-skills-invoke
       skill nil
       (lambda (_) nil)
       :trigger 'model-skill))
    (should (eq 'haiku (mevedel-request-skill-model-override request)))
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
  ;; Spec 22 §"Recursion depth": dynamic let-bound counter; 0 max means
  ;; even the first invocation exceeds.
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
  (let ((agent (mevedel-agent--create :name "explore" :tools nil
                                      :system-prompt "")))
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explore") agent))))
      (let ((skill (mevedel-skill--create
                    :name "demo" :context 'fork :agent "explore")))
        (should (eq agent (mevedel-skills--build-fork-agent skill))))))

  :doc "named-agent path returns nil for unknown agent"
  (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
    (let ((skill (mevedel-skill--create
                  :name "demo" :context 'fork :agent "missing")))
      (should (null (mevedel-skills--build-fork-agent skill)))))

  :doc "parent-inherited path (no `agent') currently returns nil"
  ;; Spec 22 §"Fork Skills": parent-inherited fork is reserved.
  ;; Phase 6 stubs it and surfaces an unknown-agent error from the
  ;; calling --invoke-fork-direct.  When implemented, this test
  ;; should be updated to assert the synthetic-struct shape.
  (let ((skill (mevedel-skill--create :name "demo" :context 'fork)))
    (should (null (mevedel-skills--build-fork-agent skill)))))

(mevedel-deftest mevedel-skills--invoke-fork ()
  ,test
  (test)
  :doc "model-skill trigger routes to direct dispatch via mevedel-tools--task"
  (let* ((agent (mevedel-agent--create :name "explore"))
         (dispatched nil)
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explore"
                 :body "Task body $ARGUMENTS"
                 :allowed-tool-rules
                 '(("Read" :action allow))
                 :model "haiku")))
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explore") agent)))
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
          (should (eq 'haiku (plist-get keys :skill-model-override))))
        (should (eq 'ok (plist-get outcome :status)))
        (should (eq 'fork (plist-get outcome :kind)))
        (should (equal "agent finished" (plist-get outcome :result)))
        ;; When `mevedel-tools--task' delivers a bare string (no
        ;; transcript metadata, e.g. our test mock), the outcome
        ;; falls back to the registry agent's name.  When it
        ;; delivers a `(:result :render-data)' plist, the unique
        ;; invocation agent-id from the render-data wins.
        (should (equal "explore" (plist-get outcome :agent-id)))
        (should (null (plist-get outcome :render-data))))))

  :doc "fork-direct forwards :render-data when the task callback wraps it"
  ;; Spec 22 §"Invocation API" line 134: outcome carries :render-data
  ;; so the renderer can expose the transcript-open affordance.
  (let* ((agent (mevedel-agent--create :name "explore"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explore"
                 :body "Body")))
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) agent))
              ((symbol-function 'mevedel-tools--task)
               (lambda (cb _agent _desc _prompt &rest _args)
                 (funcall cb
                          (list :result "wrapped"
                                :render-data
                                '(:kind agent-transcript
                                        :agent-id "explore--abc123"
                                        :transcript-relative-path "p"
                                        :status running))))))
      (let (outcome)
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :trigger 'model-skill)
        (should (equal "wrapped" (plist-get outcome :result)))
        (should (equal "explore--abc123"
                       (plist-get outcome :agent-id)))
        (should (eq 'agent-transcript
                    (plist-get (plist-get outcome :render-data) :kind))))))

  :doc "user-slash trigger uses legacy delegation body (sync outcome)"
  (let* ((skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explore"
                 :body "Task body"))
         outcome)
    (mevedel-skills-invoke
     skill "the task"
     (lambda (o) (setq outcome o))
     :trigger 'user-slash)
    (should (eq 'ok (plist-get outcome :status)))
    ;; Legacy path returns kind=inline because the body is the prose
    ;; instruction inserted into the prompt region, not the agent's
    ;; final result.
    (should (eq 'inline (plist-get outcome :kind)))
    (should (string-match-p "Use the `explore` agent"
                            (plist-get outcome :body))))

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

  :doc "omitted agent (parent-inherited stub) yields :reason unknown-agent"
  (let ((skill (mevedel-skill--create
                :name "demo" :context 'fork))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :trigger 'model-skill)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'unknown-agent (plist-get outcome :reason)))))

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
    (should (string-match-p "unknown skill" received)))

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
      (delete-directory dir t))))


;;
;;; Phase C — slash commands and completion

(defun mevedel-skills-test--make-session (&optional name)
  "Return a throwaway session with a minimal workspace."
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

(mevedel-deftest mevedel-skills--parse-slash-line ()
  ,test
  (test)
  :doc "plain `/command' parses to (name \"\" 0)"
  (should (equal '("help" "" 0)
                 (mevedel-skills--parse-slash-line "/help")))

  :doc "`/command args' parses to (name args 0)"
  (should (equal '("model" "gpt-4" 0)
                 (mevedel-skills--parse-slash-line "/model gpt-4")))

  :doc "additional lines after the command are appended to ARGS"
  ;; Skill commands (`/coordinator', `/grill-me', ...) take a
  ;; prompt body as ARGS, and prompt bodies are naturally
  ;; multi-line.  Truncating at the first newline produced
  ;; "args contain only the first line" -- the LLM saw a
  ;; truncated task description and could not act on it.
  (should (equal '("coordinator"
                   "Launch three background explore agents:\n  (a) ...\n  (b) ..."
                   0)
                 (mevedel-skills--parse-slash-line
                  "/coordinator Launch three background explore agents:
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
        (should (equal "### Hello world!" (buffer-string))))))

  :doc "user-invocable: false skill returns 'unknown and aborts the send"
  ;; Spec 22 §"Invocation Gating": user-slash invocation of a
  ;; skill marked `user-invocable: false' must message and abort.
  ;; Returning `unknown' makes the surrounding advice skip
  ;; gptel-send so the literal `/internal-only' text is not sent.
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

  :doc "fork-context skill expands to a delegation instruction body"
  ;; Fork-context skills (e.g. `/coordinator') do NOT dispatch a
  ;; sub-agent directly from the slash-command path.  They expand
  ;; into a short instruction telling main to delegate via the
  ;; Agent tool -- main reads the instruction and dispatches the
  ;; named agent itself.  The slash command is just a convenience
  ;; expansion; the actual sub-agent launch goes through the same
  ;; Agent tool path the LLM would normally use.
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "coordinator"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "coordinator")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /coordinator do the thing")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-slash-command)))
        (let ((buf (buffer-string)))
          ;; Body inlined: delegation instruction, NOT the SKILL.md.
          (should (string-match-p "Use the `coordinator` agent" buf))
          (should (string-match-p "do the thing" buf))
          (should-not (string-match-p "should-not-appear" buf))))))

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
        (should (equal "Old response\n\nHello world!" (buffer-string))))))

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
        (should (equal "### " (buffer-string)))))))

(mevedel-deftest mevedel-skills--gptel-send-advice ()
  ,test
  (test)
  :doc "local command aborts the send (orig-fn not called)"
  ;; Spec 22 §"Slash invocation lifecycle": advice is `:around', so we
  ;; assert behavior by checking whether the original send is called.
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
  ;; Spec 22 §"Slash invocation lifecycle": unwind-protect clears the
  ;; pending-stash if the begin handler did not drain it (e.g.,
  ;; because gptel-send aborted before WAIT).
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
         (lambda (&rest _) (error "boom"))))
      (should (null mevedel-skills--pending-request-context)))))

(mevedel-deftest mevedel-slash-capf ()
  ,test
  (test)
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
               (cands (and capf (nth 2 capf)))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (member "help" cands))
          (should (member "tokens" cands))
          (should (member "simplify" cands))
          (should (equal " [command]" (funcall annot "help")))
          (should (equal " [skill]" (funcall annot "simplify")))))))

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
    (should (null (mevedel-slash-capf)))))

(mevedel-deftest mevedel-cmd--model ()
  ,test
  (test)
  :doc "with args sets buffer-local gptel-model to interned symbol"
  (with-temp-buffer
    (let ((gptel-model 'default))
      (mevedel-cmd--model "claude-opus-4-6")
      (should (eq 'claude-opus-4-6 gptel-model))))

  :doc "with blank args does not change the model"
  (with-temp-buffer
    (let ((gptel-model 'keep))
      (mevedel-cmd--model "")
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
            (should (eq 'plan mevedel-permission-mode))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "rejects unknown modes"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'default)
    (should-error (mevedel-cmd--mode "bogus") :type 'user-error)
    (should (eq 'default mevedel-permission-mode)))

  :doc "blank args leaves the mode unchanged"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'accept-edits)
    (mevedel-cmd--mode "")
    (should (eq 'accept-edits mevedel-permission-mode))))


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
    (should (string-suffix-p "..." entry))))

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
      (should (equal '("a") names)))))

(mevedel-deftest mevedel-skills--format-listing ()
  ,test
  (test)
  :doc "includes header and one line per skill"
  (let* ((skills (list (mevedel-skill--create :name "s1" :description "d1")
                       (mevedel-skill--create :name "s2" :description "d2")))
         (listing (mevedel-skills--format-listing skills)))
    (should (string-match-p "for use with the Skill tool" listing))
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

  :doc "dormant or disabled skills do not cause firing"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r2" :root "/tmp/r2" :name "r2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-listing)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "dormant" :description "d"
                 :model-invocable-p t :active-p nil
                 :path-patterns '("*.el"))
                (mevedel-skill--create
                 :name "disabled" :description "d"
                 :model-invocable-p nil :active-p t)))
    (should-not (funcall (mevedel-reminder-trigger reminder) session))))

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


(provide 'test-mevedel-skills)
;;; test-mevedel-skills.el ends here
