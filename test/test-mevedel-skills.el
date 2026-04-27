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

  :doc "skills missing a description are skipped"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "no-desc"
           "name: no-desc
" "Body")
          (mevedel-skills-test--write-skill
           dir "ok"
           "name: ok
description: present
" "Body")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (names (mapcar #'mevedel-skill-name skills)))
            (should (member "ok" names))
            (should-not (member "no-desc" names))))
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
                 skills))))


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

(mevedel-deftest mevedel-skills--split-arguments ()
  ,test
  (test)
  :doc "nil and blank inputs return nil"
  (should (null (mevedel-skills--split-arguments nil)))
  (should (null (mevedel-skills--split-arguments "")))
  (should (null (mevedel-skills--split-arguments "   ")))

  :doc "whitespace splits into tokens"
  (should (equal '("a" "b" "c")
                 (mevedel-skills--split-arguments "a b c")))
  (should (equal '("foo" "bar")
                 (mevedel-skills--split-arguments "  foo   bar  "))))

(mevedel-deftest mevedel-skills--substitute-vars ()
  ,test
  (test)
  :doc "substitutes $ARGUMENTS, positional, and env-style placeholders"
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
                    "args=$ARGUMENTS" "foo bar baz" session skill)))
    (should (equal "first=foo second=bar"
                   (mevedel-skills--substitute-vars
                    "first=$1 second=$2" "foo bar" session skill)))
    (should (equal "indexed=baz"
                   (mevedel-skills--substitute-vars
                    "indexed=$ARGUMENTS[3]" "foo bar baz" session skill)))
    (should (equal "session=main dir=/tmp/simplify/"
                   (mevedel-skills--substitute-vars
                    "session=${CLAUDE_SESSION_ID} dir=${CLAUDE_SKILL_DIR}"
                    "" session skill))))

  :doc "missing positional arguments become empty"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "a=foo b="
                   (mevedel-skills--substitute-vars
                    "a=$1 b=$2" "foo" nil skill))))

  :doc "nil argument string does not error"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "args="
                   (mevedel-skills--substitute-vars
                    "args=$ARGUMENTS" nil nil skill)))))

(mevedel-deftest mevedel-skills--run-shell-injections ()
  ,test
  (test)
  :doc "inline !`cmd` is replaced with stdout"
  (should (equal "value=hello"
                 (mevedel-skills--run-shell-injections
                  "value=!`echo hello`")))

  :doc "multiple inline injections in the same line"
  (should (equal "a=1 b=2"
                 (mevedel-skills--run-shell-injections
                  "a=!`echo 1` b=!`echo 2`")))

  :doc "fenced ```! block is replaced with stdout"
  (should (equal "prefix\nline1\nline2\nsuffix"
                 (mevedel-skills--run-shell-injections
                  "prefix\n```!\necho line1\necho line2\n```\nsuffix")))

  :doc "non-zero exit is surfaced with [exit N] prefix"
  (let ((result (mevedel-skills--run-shell-injections "!`false`")))
    (should (string-prefix-p "[exit " result))))

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
           "Hello $1 from ${CLAUDE_SESSION_ID}: !`echo ready`.")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (skill (car skills))
                 (body (mevedel-skills--prepare-body skill "world" session)))
            (should (equal "Hello world from main: ready." body))))
      (delete-directory dir t))))

(mevedel-deftest mevedel-skills--execute-inline ()
  ,test
  (test)
  :doc "inline execution returns the prepared body via callback"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t))
         (ws (mevedel-workspace--create
              :type 'test :id dir :root dir :name "inline"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         received)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "simplify"
           "name: simplify
description: Clean it up
"
           "Do $ARGUMENTS cleanly.")
          (let* ((skills (mevedel-skills-scan dir '(".")))
                 (skill (car skills)))
            (mevedel-skills--execute-inline
             skill "the refactor"
             (lambda (r) (setq received r))
             session)
            (should (equal "Do the refactor cleanly." received))))
      (delete-directory dir t)))

  :doc "empty body yields an informative fallback message"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "e" :root "/tmp/e" :name "e"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "empty"))
         received)
    (mevedel-skills--execute-inline
     skill "" (lambda (r) (setq received r)) session)
    (should (string-match-p "no body" received))))

(mevedel-deftest mevedel-skills--execute-fork ()
  ,test
  (test)
  :doc "fork dispatches the body to the configured agent via task"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "f" :root "/tmp/f" :name "f"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "researcher-task"
                 :body "Research $ARGUMENTS."
                 :context 'fork
                 :agent "researcher"))
         captured)
    (cl-letf (((symbol-function 'mevedel-tools--task)
               (lambda (cb agent desc prompt &optional background)
                 (setq captured (list agent desc prompt background))
                 (funcall cb "ok"))))
      (mevedel-skills--execute-fork
       skill "widgets" #'ignore session))
    (should (equal (nth 0 captured) "researcher"))
    (should (equal (nth 1 captured) "researcher-task"))
    (should (equal (nth 2 captured) "Research widgets.")))

  :doc "fork dispatches BACKGROUND so the caller stays alive concurrently"
  ;; Without background, the caller's FSM parks in TOOL state for the
  ;; whole duration of the skill's sub-agent run -- mid-flight dialog
  ;; via SendMessage is impossible and `context: fork' degenerates to
  ;; "wait for the sub-agent to terminate".
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "fb" :root "/tmp/fb" :name "fb"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "coordinator"
                 :body "Orchestrate $ARGUMENTS."
                 :context 'fork
                 :agent "coordinator"))
         captured)
    (cl-letf (((symbol-function 'mevedel-tools--task)
               (lambda (_cb _agent _desc _prompt &optional background)
                 (setq captured background))))
      (mevedel-skills--execute-fork
       skill "stuff" #'ignore session))
    (should (eq captured t))))

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

  :doc "trailing newline and arguments after the first line are ignored"
  (should (equal '("mode" "plan" 0)
                 (mevedel-skills--parse-slash-line "/mode plan\nignored")))

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
                 :body "Hello $1!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-slash-command)))
        (should (equal "### Hello world!" (buffer-string))))))

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
                 :body "Hello $1!")))
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
  :doc "local command aborts the send (returns nil)"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             `(("noop" . ,(lambda (_args) nil)))))
        (insert "### /noop")
        (goto-char (point-max))
        (should (null (mevedel-skills--gptel-send-advice))))))

  :doc "unknown slash command aborts the send"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /bogus")
        (goto-char (point-max))
        (should (null (mevedel-skills--gptel-send-advice))))))

  :doc "expanded skill lets the send proceed"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "hi" :body "Hi!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /hi")
        (goto-char (point-max))
        (should (mevedel-skills--gptel-send-advice)))))

  :doc "plain text always lets the send proceed"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### just a normal message")
      (goto-char (point-max))
      (should (mevedel-skills--gptel-send-advice))))

  :doc "no session: advice returns t unconditionally"
  (with-temp-buffer
    (setq mevedel--session nil)
    (should (mevedel-skills--gptel-send-advice))))

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
