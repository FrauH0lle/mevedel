;;; test-mevedel-skills-preparation.el -- Skill preparation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests skill argument substitution and body expansion.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-bash-policy)
(require 'mevedel-permission-mode)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-skills-core)
(require 'mevedel-skills-preparation)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-workspace)


;;
;;; Phase 2 helpers

(mevedel-deftest mevedel-skills-preparation-parse-arguments ()
  ,test
  (test)
  :doc "shell-style splitting respects double quotes"
  (should (equal '("foo" "bar baz" "qux")
                 (mevedel-skills-preparation-parse-arguments
                  "foo \"bar baz\" qux")))

  :doc "single quotes are not part of Emacs' split-string-and-unquote"
  ;; Emacs' shell-quote splitter only honors double quotes and
  ;; backslash escapes (cf. `combine-and-quote-strings').  Single
  ;; quotes pass through as literal characters; this is acceptable for
  ;; mevedel because skill authors writing portable bodies should use
  ;; double quotes anyway.
  (should (equal '("foo" "'bar" "baz'")
                 (mevedel-skills-preparation-parse-arguments "foo 'bar baz'")))

  :doc "unbalanced quotes fall back to whitespace splitting"
  (should (equal '("foo" "\"bar")
                 (mevedel-skills-preparation-parse-arguments "foo \"bar")))

  :doc "nil and blank inputs return nil"
  (should (null (mevedel-skills-preparation-parse-arguments nil)))
  (should (null (mevedel-skills-preparation-parse-arguments "")))
  (should (null (mevedel-skills-preparation-parse-arguments "   "))))

(mevedel-deftest mevedel-skills-preparation-substitute ()
  ,test
  (test)
  :doc "$ARGUMENTS substitutes the full raw argument string"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "simplify"
                 :source-file "/tmp/simplify/SKILL.md"
                 :source-dir "/tmp/simplify/")))
    (should (equal "args=foo bar baz"
                   (mevedel-skills-preparation-substitute
                    "args=$ARGUMENTS" "foo bar baz" session skill))))

  :doc "$0/$1/etc are zero-based"
  ;; No one-based compatibility: $1 means the second token.
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "first=foo second=bar"
                   (mevedel-skills-preparation-substitute
                    "first=$0 second=$1" "foo bar" nil skill)))
    ;; Indexed access is also zero-based.
    (should (equal "indexed=baz"
                   (mevedel-skills-preparation-substitute
                    "indexed=$ARGUMENTS[2]" "foo bar baz" nil skill))))

  :doc "${CLAUDE_SESSION_ID} and ${CLAUDE_SKILL_DIR} substitute"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "x"
                 :source-dir "/tmp/x/")))
    (should (equal "session=main dir=/tmp/x/"
                   (mevedel-skills-preparation-substitute
                    "session=${CLAUDE_SESSION_ID} dir=${CLAUDE_SKILL_DIR}"
                    "" session skill))))

  :doc "${CLAUDE_SESSION_ID} prefers stable session id over session name"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "x")))
    (setf (mevedel-session-session-id session) "main-2026-05-17-abc")
    (should (equal "session=main-2026-05-17-abc"
                   (mevedel-skills-preparation-substitute
                    "session=${CLAUDE_SESSION_ID}" "" session skill))))

  :doc "${CLAUDE_EFFORT} substitutes skill effort"
  (let ((skill (mevedel-skill--create :name "x" :effort 'xhigh)))
    (should (equal "effort=xhigh"
                   (mevedel-skills-preparation-substitute
                    "effort=${CLAUDE_EFFORT}" "" nil skill))))

  :doc "${MEVEDEL_*} aliases mirror Claude-compatible substitutions"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
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
                   (mevedel-skills-preparation-substitute
                    "session=${MEVEDEL_SESSION_ID} dir=${MEVEDEL_SKILL_DIR} effort=${MEVEDEL_EFFORT}"
                    "" session skill))))

  :doc "nil session and skill expand literal substitutions to empty strings"
  (should (equal "session= dir= effort= alias="
                 (mevedel-skills-preparation-substitute
                  "session=${CLAUDE_SESSION_ID} dir=${CLAUDE_SKILL_DIR} effort=${CLAUDE_EFFORT} alias=${MEVEDEL_EFFORT}"
                  "" nil nil)))

  :doc "literal substitutions do not rewrite user-supplied arguments"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
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
                    (mevedel-skills-preparation-substitute
                     "args=$ARGUMENTS" "${MEVEDEL_SESSION_ID}"
                     session skill))))
    (should (equal "first=${MEVEDEL_SKILL_DIR}"
                   (substring-no-properties
                    (mevedel-skills-preparation-substitute
                     "first=$0" "${MEVEDEL_SKILL_DIR}"
                     session skill))))
    (should (equal "name=${CLAUDE_EFFORT}"
                   (substring-no-properties
                    (mevedel-skills-preparation-substitute
                     "name=$name" "${CLAUDE_EFFORT}"
                     session skill)))))

  :doc "escaped placeholders stay literal and do not suppress append-fallback"
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("topic"))))
    (should (equal "full=$ARGUMENTS idx=$ARGUMENTS[0] pos=$0 named=$topic

ARGUMENTS: foo bar"
                   (substring-no-properties
                    (mevedel-skills-preparation-substitute
                     "full=\\$ARGUMENTS idx=\\$ARGUMENTS[0] pos=\\$0 named=\\$topic"
                     "foo bar" nil skill)))))

  :doc "escaped literal variables stay literal and do not suppress append-fallback"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
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
                    (mevedel-skills-preparation-substitute
                     "dir=\\${CLAUDE_SKILL_DIR} session=\\${MEVEDEL_SESSION_ID}"
                     "hello" session skill)))))

  :doc "out-of-range positional args become empty"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "a=foo b="
                   (mevedel-skills-preparation-substitute
                    "a=$0 b=$1" "foo" nil skill))))

  :doc "nil argument string does not error"
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "args="
                   (mevedel-skills-preparation-substitute
                    "args=$ARGUMENTS" nil nil skill))))

  :doc "named arguments substitute by argument-names index"
  ;; ARGUMENT-NAMES[i] maps to PARSED-ARGS[i].
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("path" "depth"))))
    (should (equal "Visit src/foo at level 3"
                   (mevedel-skills-preparation-substitute
                    "Visit $path at level $depth" "src/foo 3" nil skill))))

  :doc "named arguments do not match longer identifiers or indexed access"
  ;; ccs regex `\\=$NAME(?![\\=[\\=w])': $foo skips $foobar and $foo[0].
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("foo"))))
    (should (equal "got=hi keep=$foobar idx=$foo[0]"
                   (mevedel-skills-preparation-substitute
                    "got=$foo keep=$foobar idx=$foo[0]"
                    "hi" nil skill))))

  :doc "shell-style parsing keeps quoted arguments together"
  ;; Quoted strings stay together, even with whitespace inside.
  (let ((skill (mevedel-skill--create
                :name "x"
                :argument-names '("title"))))
    (should (equal "title is hello world"
                   (mevedel-skills-preparation-substitute
                    "title is $title" "\"hello world\"" nil skill))))

  :doc "ARGUMENTS: appended when args supplied but no placeholder substituted"
  ;; Append only when no placeholder matched and raw args are non-empty.
  (let ((skill (mevedel-skill--create :name "x")))
    (should (equal "no placeholders here\n\nARGUMENTS: foo bar"
                   (mevedel-skills-preparation-substitute
                    "no placeholders here" "foo bar" nil skill)))
    ;; Body contains $ARGUMENTS, so do not append even when args are present.
    (should (equal "x=foo bar"
                   (mevedel-skills-preparation-substitute
                    "x=$ARGUMENTS" "foo bar" nil skill)))
    ;; Empty or nil args do not append anything.
    (should (equal "no placeholders here"
                   (mevedel-skills-preparation-substitute
                    "no placeholders here" "" nil skill)))
    (should (equal "no placeholders here"
                   (mevedel-skills-preparation-substitute
                    "no placeholders here" nil nil skill))))

  :doc "${CLAUDE_*} and ${MEVEDEL_*} substitutions do not trigger append-fallback"
  ;; Literal variable substitutions run AFTER the placeholder check so they
  ;; don't suppress the append.
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create :name "x" :effort 'low)))
    (should (equal "id=main effort=low\n\nARGUMENTS: hello"
                   (mevedel-skills-preparation-substitute
                    "id=${CLAUDE_SESSION_ID} effort=${MEVEDEL_EFFORT}"
                    "hello" session skill)))))


(defmacro mevedel-skills-test--with-bash-allowed (&rest body)
  "Run BODY with the Bash permission check forced to allow.
Tests need a deterministic permit so they can assert on the
substituted output without depending on the user's defcustom
configuration."
  `(cl-letf (((symbol-function 'mevedel-bash-policy-check-permission)
              (lambda (_command &rest _args) 'allow)))
     ,@body))

(defmacro mevedel-skills-test--with-eval-allowed (&rest body)
  "Run BODY with a deterministic trusted Eval allow rule."
  `(let ((mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules '(("Eval" :action allow))))
     ,@body))

(defun mevedel-skills-test--shell-injections-sync (text)
  "Drive `mevedel-skills-preparation-expand-body' with TEXT synchronously.
Return the outcome plist produced by the async helper."
  (let ((mevedel--session
         (mevedel-skills-test--make-session "injection"))
        outcome)
    (mevedel-skills-preparation-expand-body
     text (lambda (o) (setq outcome o)))
    (while (null outcome)
      (accept-process-output nil 0.01))
    outcome))

(mevedel-deftest mevedel-skills-preparation-expand-body ()
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
  (cl-letf (((symbol-function 'mevedel-bash-policy-check-permission)
             (lambda (_c &rest _) 'deny)))
    (let ((outcome (mevedel-skills-test--shell-injections-sync "!`anything`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'permission-denied (plist-get outcome :reason)))))

  :doc "permission ask yields :status error :reason permission-denied"
  (cl-letf (((symbol-function 'mevedel-bash-policy-check-permission)
             (lambda (_c &rest _) 'ask)))
    (let ((outcome (mevedel-skills-test--shell-injections-sync "!`anything`")))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'permission-denied (plist-get outcome :reason)))))
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

  :doc "an Eval result carrying an error status aborts preparation"
  ;; The pipeline reports the canonical outcome in render data.  Classifying
  ;; from the display text alone lets a failure whose text does not start
  ;; with a known prefix through as legitimate skill body content, and the
  ;; model cannot tell it from an expression that returned that string.
  (mevedel-skills-test--with-eval-allowed
    (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
               (lambda (_tool callback &rest _)
                 (funcall callback
                          (concat
                           "Failed to start Eval batch process: boom"
                           (mevedel-tool-render-data-format
                            '(:status error)))))))
      (let ((outcome (mevedel-skills-test--shell-injections-sync
                      "!el`(+ 1 2)`")))
        (should (eq 'error (plist-get outcome :status)))
        (should (eq 'elisp-failure (plist-get outcome :reason))))))

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
        (should-not enqueued))))
  :doc "caller-provided inline elisp markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=$ARGUMENTS" "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!el`(+ 1 2)`"
                     (plist-get outcome :body)))))

  :doc "caller-provided fenced elisp markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
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
           (body (mevedel-skills-preparation-substitute
                  "value=$ARGUMENTS" "!`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!`echo unsafe`"
                     (plist-get outcome :body)))))

  :doc "fallback-appended caller markers are not trusted"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "body" "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "body\n\nARGUMENTS: !el`(+ 1 2)`"
                     (plist-get outcome :body)))))
  :doc "caller text cannot complete an author-written inline elisp prefix"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!$ARGUMENTS" "el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!el`(+ 1 2)`"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written inline shell prefix"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!$ARGUMENTS" "`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=!`echo unsafe`"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written fenced elisp prefix"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!$ARGUMENTS" "el\n(+ 1 2)\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n```!el\n(+ 1 2)\n```"
                     (plist-get outcome :body)))))

  :doc "caller text cannot complete an author-written fenced shell prefix"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!$ARGUMENTS" "\necho unsafe\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n```!\necho unsafe\n```"
                     (plist-get outcome :body)))))
  :doc "caller-provided leading newline cannot activate fenced shell"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "prefix$ARGUMENTS```!\necho unsafe\n```\n" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\n```!\necho unsafe\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided leading newline cannot activate fenced elisp"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "prefix$ARGUMENTS```!el\n(+ 1 2)\n```\n" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "prefix\n```!el\n(+ 1 2)\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided trailing newline cannot activate fenced shell"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "```!\necho unsafe\n```$ARGUMENTS" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!\necho unsafe\n```\n"
                     (plist-get outcome :body)))))

  :doc "caller-provided trailing newline cannot activate fenced elisp"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "```!el\n(+ 1 2)\n```$ARGUMENTS" "\n" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!el\n(+ 1 2)\n```\n"
                     (plist-get outcome :body)))))
  :doc "author-written inline shell markers may interpolate arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!`printf \"%s\" \"$ARGUMENTS\"`"
                  "hello" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=hello"
                     (plist-get outcome :body)))))

  :doc "author-written inline elisp markers may interpolate arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!el`(concat \"x\" \"$ARGUMENTS\")`"
                  "y" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=\"xy\""
                     (plist-get outcome :body)))))

  :doc "author-written fenced shell markers may interpolate arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!\nprintf \"%s\" \"$ARGUMENTS\"\n```"
                  "hello" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\nhello"
                     (plist-get outcome :body)))))

  :doc "author-written fenced elisp markers may interpolate arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!el\n(concat \"x\" \"$ARGUMENTS\")\n```"
                  "y" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n\"xy\""
                     (plist-get outcome :body)))))

  :doc "parameterized shell injection checks the fully expanded operation"
  (let* ((skill (mevedel-skill--create :name "x"))
         (body (mevedel-skills-preparation-substitute
                "!`echo safe $ARGUMENTS`"
                "&& denied-command" nil skill))
         seen
         outcome)
    (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
               (lambda (_tool callback args)
                 (setq seen args)
                 (funcall callback "ok"))))
      (setq outcome (mevedel-skills-test--shell-injections-sync body)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "echo safe && denied-command"
                   (plist-get seen :command)))
    (should-not (plist-get seen :trust-literal-p)))

  :doc "parameterized Elisp injection checks the fully expanded expression"
  (let* ((skill (mevedel-skill--create :name "x"))
         (body (mevedel-skills-preparation-substitute
                "!el`(list $ARGUMENTS)`" "danger" nil skill))
         seen
         outcome)
    (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
               (lambda (_tool callback args)
                 (setq seen args)
                 (funcall callback "ok"))))
      (setq outcome (mevedel-skills-test--shell-injections-sync body)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "(list danger)" (plist-get seen :expression)))
    (should-not (plist-get seen :trust-literal-p)))
  :doc "inline shell skips non-author backticks in interpolated arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!`printf \"%s\" '$ARGUMENTS'`"
                  "a`b" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=a`b"
                     (plist-get outcome :body)))))

  :doc "inline elisp skips non-author backticks in interpolated arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value=!el`(length \"$ARGUMENTS\")`"
                  "a`b" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value=3"
                     (plist-get outcome :body)))))

  :doc "fenced shell skips non-author closing fences in interpolated arguments"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!\ncat <<'EOF'\n$ARGUMENTS\nEOF\n```"
                  "a\n```\nb" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\na\n```\nb"
                     (plist-get outcome :body)))))

  :doc "fenced elisp skips non-author closing fences in interpolated arguments"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "value\n```!el\n(length \"$ARGUMENTS\")\n```"
                  "a\n```\nb" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "value\n7"
                     (plist-get outcome :body)))))
  :doc "skipped inline elisp marker does not hide later author marker"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "arg=$ARGUMENTS author=!el`(+ 2 3)`"
                  "!el`(+ 1 2)`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "arg=!el`(+ 1 2)` author=5"
                     (plist-get outcome :body)))))

  :doc "skipped inline shell marker does not hide later author marker"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "arg=$ARGUMENTS author=!`echo safe`"
                  "!`echo unsafe`" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "arg=!`echo unsafe` author=safe"
                     (plist-get outcome :body)))))

  :doc "skipped fenced elisp marker does not hide later author marker"
  (mevedel-skills-test--with-eval-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "$ARGUMENTS\n```!el\n(+ 2 3)\n```"
                  "```!el\n(+ 1 2)\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!el\n(+ 1 2)\n```\n5"
                     (plist-get outcome :body)))))

  :doc "skipped fenced shell marker does not hide later author marker"
  (mevedel-skills-test--with-bash-allowed
    (let* ((skill (mevedel-skill--create :name "x"))
           (body (mevedel-skills-preparation-substitute
                  "$ARGUMENTS\n```!\necho safe\n```"
                  "```!\necho unsafe\n```" nil skill))
           (outcome (mevedel-skills-test--shell-injections-sync body)))
      (should (eq 'ok (plist-get outcome :status)))
      (should (equal "```!\necho unsafe\n```\nsafe"
                     (plist-get outcome :body))))))


(provide 'test-mevedel-skills-preparation)
;;; test-mevedel-skills-preparation.el ends here
