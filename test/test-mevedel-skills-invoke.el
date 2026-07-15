;;; test-mevedel-skills-invoke.el --- Skill invocation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests skill preparation, invocation, and tool dispatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-agents)
(require 'mevedel-compact)
(require 'mevedel-file-state)
(require 'mevedel-hooks)
(require 'mevedel-mention-bindings)
(require 'mevedel-mentions)
(require 'mevedel-models)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)
(require 'mevedel-tool-exec)
(require 'mevedel-tools)
(require 'mevedel-transcript)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-skills-invoke-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-skills--drain-pending-context
                    mevedel-skills--transform-apply-model-override
                    mevedel-skills--parse-arguments
                    mevedel-skills--substitute-vars
                    mevedel-skills--run-body-injections-async
                    mevedel-skills-invoke
                    mevedel-skills--build-fork-agent
                    mevedel-skills--invoke-handler
                    mevedel-skills--list-handler
                    mevedel-skills--listing-describe
                    mevedel-skills--listing-candidates
                    mevedel-skills--current-prompt-region
                    mevedel-skills--parse-skill-line
                    mevedel-skills--inline-skill-mentions
                    mevedel-skills--scan-skill-tokens
                    mevedel-skills--dispatch-skill-command
                    mevedel-skills--dispatch-inline-attachments
                    mevedel-skills--transform-expand-inline-attachments))
    (should (equal "mevedel-skills-invoke"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Invocation helpers and core behavior

(defun mevedel-skills-test--expansion-fn (_event)
  "Test hook used by skill expansion."
  '(:updated-input "Expanded by hook"
    :additional-context "expansion context"))

(defun mevedel-skills-test--block-expansion-fn (_event)
  "Test hook that blocks skill expansion."
  '(:continue nil :stop-reason "blocked expansion"))


;;
;;; Phase B -- substitution, shell injection, execution


;;
;;; Request-scoped skill context

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

(mevedel-deftest mevedel-skills--drain-pending-context ()
  ,test
  (test)
  :doc "drain commits non-policy context and clears the buffer-local stash"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "d" :root "/tmp/d" :name "d"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (records (list (mevedel-skill-invocation-record--create
                         :name "demo" :args "x"
                         :role 'command :origin 'user
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
      (should (null (mevedel-request-skill-permission-rules request))))))

(mevedel-deftest mevedel-skills--transform-apply-model-override ()
  ,test
  (test)
  :doc "pending slash tier sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
    (let ((mevedel-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")
             (strong)))
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

  :doc "pending effort uses gptel validation and reaches the prompt buffer"
  (mevedel-skills-test--with-model-backends
    (let ((chat (generate-new-buffer " *skill-effort-chat*"))
          (old-custom (get 'gptel-reasoning-effort 'custom-type))
          (old-effort (get 'fast-model :reasoning-effort)))
      (unwind-protect
          (progn
            (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
            (put 'fast-model :reasoning-effort '(member low high))
            (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
              (with-current-buffer chat
                (setq-local mevedel-skills--pending-request-context
                            (list :model
                                  (mevedel-model-resolve-provider
                                   "Fast:fast-model")
                                  :effort 'high)))
              (with-temp-buffer
                (mevedel-skills--transform-apply-model-override fsm)
                (should (eq 'fast-model gptel-model))
                (should (eq 'high gptel-reasoning-effort)))))
        (put 'gptel-reasoning-effort 'custom-type old-custom)
        (put 'fast-model :reasoning-effort old-effort)
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
    ;; Body contains $ARGUMENTS, so do not append even when args are present.
    (should (equal "x=foo bar"
                   (mevedel-skills--substitute-vars
                    "x=$ARGUMENTS" "foo bar" nil skill)))
    ;; Empty or nil args do not append anything.
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
  `(let ((mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules '(("Eval" :action allow))))
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
                     (plist-get outcome :body)))))

  :doc "parameterized shell injection checks the fully expanded operation"
  (let* ((skill (mevedel-skill--create :name "x"))
         (body (mevedel-skills--substitute-vars
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
         (body (mevedel-skills--substitute-vars
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
    (should-not (plist-get seen :trust-literal-p))))

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
               :origin 'user)
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

;;
;;; mevedel-skills-invoke (unified invocation API)

(mevedel-deftest mevedel-skills--preparation-rejection ()
  ,test
  (test)
  :doc "rejects invalid skill, role, origin, and named fork agent"
  (should (eq 'unknown-skill
              (plist-get (mevedel-skills--preparation-rejection
                          nil 'command 'user)
                         :reason)))
  (let ((skill (mevedel-skill--create :name "alpha" :body "Body")))
    (should (eq 'invalid-role
                (plist-get (mevedel-skills--preparation-rejection
                            skill 'other 'user)
                           :reason)))
    (should (eq 'invalid-origin
                (plist-get (mevedel-skills--preparation-rejection
                            skill 'command 'other)
                           :reason))))
  (let ((skill (mevedel-skill--create
                :name "alpha" :body "Body" :context 'fork :agent "missing")))
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
      (should (eq 'unknown-agent
                  (plist-get (mevedel-skills--preparation-rejection
                              skill 'command 'user)
                             :reason)))))

  :doc "accepts a valid preparation request"
  (let ((skill (mevedel-skill--create :name "alpha" :body "Body")))
    (should-not
     (mevedel-skills--preparation-rejection skill 'command 'model))))

(mevedel-deftest mevedel-skills--preparation-policy ()
  ,test
  (test)
  :doc "an owner merges preset policy and validates its request workload"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :context 'fork :agent "reviewer"
                 :model "fast" :effort 'low))
         (mevedel-model-tiers '((fast) (strong)))
         (mevedel-model-workloads '(($alpha :tier strong :effort high)))
         call
         outcome)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload selector effort)
                 (setq call (list workload selector effort))
                 '(:backend inherited :model inherited :effort high))))
      (setq outcome (mevedel-skills--preparation-policy skill 'user t)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal '(:tier strong) (plist-get outcome :model)))
    (should (eq 'high (plist-get outcome :effort)))
    (should (equal '("reviewer" (:tier strong) high) call)))

  :doc "a model-side inline non-owner reports fields without parsing them"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :context 'inline
                 :model "invalid selector" :effort 'impossible))
         (mevedel-model-workloads
          '(($alpha :tier missing :provider "Missing:model"))))
    (cl-letf (((symbol-function 'mevedel-model-merge-skill-policy)
               (lambda (&rest _) (ert-fail "non-owner merged policy")))
              ((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (ert-fail "non-owner resolved policy"))))
      (let ((outcome
             (mevedel-skills--preparation-policy skill 'model nil)))
        (should (eq 'ok (plist-get outcome :status)))
        (should (equal '(model effort)
                       (plist-get outcome :ignored-fields))))))

  :doc "a non-model non-owner silently retains session policy"
  (let ((skill (mevedel-skill--create
                :name "alpha" :model "invalid" :effort 'impossible)))
    (should (equal '(:status ok :ignored-fields nil)
                   (mevedel-skills--preparation-policy skill 'user nil))))

  :doc "owner validation failures become structured invocation failures"
  (let ((skill (mevedel-skill--create :name "alpha" :model "fast"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (user-error "Unsupported effort"))))
      (setq outcome (mevedel-skills--preparation-policy skill 'user t)))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-policy (plist-get outcome :reason)))
    (should (equal "Unsupported effort" (plist-get outcome :message)))))

(mevedel-deftest mevedel-skills--preparation-settler ()
  ,test
  (test)
  :doc "installs temporary request, restores prior request, and settles once"
  (let* ((session (mevedel-skills-test--make-session))
         (previous (mevedel-request--create :session session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         outcomes)
    (with-temp-buffer
      (setq-local mevedel--current-request previous)
      (let ((settle (mevedel-skills--preparation-settler
                     session rules hooks
                     (lambda (outcome) (push outcome outcomes)))))
        (should-not (eq previous mevedel--current-request))
        (should (eq session
                    (mevedel-request-session mevedel--current-request)))
        (should (equal rules
                       (mevedel-request-skill-permission-rules
                        mevedel--current-request)))
        (should (equal hooks
                       (mevedel-request-hook-rules mevedel--current-request)))
        (funcall settle 'first)
        (should (eq previous mevedel--current-request))
        (funcall settle 'second)
        (should (equal '(first) outcomes))))))

(mevedel-deftest mevedel-skills--preparation-success-outcome ()
  ,test
  (test)
  :doc "builds command body, policy context, and invocation record"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Body" :source-file "/tmp/alpha/SKILL.md"))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         (metadata
          (list :skill skill :arguments "task" :role 'command
                :origin 'user :session session :rules rules :hooks hooks
                :model '(:tier fast) :effort 'high))
         (outcome (mevedel-skills--preparation-success-outcome
                   metadata "original" "expanded" nil))
         (context (plist-get outcome :request-context))
         (record (car (plist-get context :invoked-skills))))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "expanded" (plist-get outcome :body)))
    (should (equal rules (plist-get context :permission-rules)))
    (should (equal hooks (plist-get context :hook-rules)))
    (should (equal '(:tier fast) (plist-get context :model)))
    (should (eq 'high (plist-get context :effort)))
    (should (equal "alpha" (mevedel-skill-invocation-record-name record)))
    (should (equal "task" (mevedel-skill-invocation-record-args record)))
    (should (eq 'command (mevedel-skill-invocation-record-role record)))
    (should (eq 'user (mevedel-skill-invocation-record-origin record)))
    (should (equal "expanded"
                   (mevedel-skill-invocation-record-prepared-body record))))

  :doc "instruction outcome omits command policy"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :body "Body" :context 'fork))
         (metadata (list :skill skill :arguments "" :role 'instruction
                         :origin 'user :rules '(ignored)
                         :model 'ignored :effort 'ignored :hooks '(ignored)))
         (outcome (mevedel-skills--preparation-success-outcome
                   metadata "original" "expanded" nil))
         (context (plist-get outcome :request-context)))
    (should (eq 'instruction (plist-get outcome :kind)))
    (should-not (plist-get context :permission-rules))
    (should-not (plist-get context :model))
    (should-not (plist-get context :effort))
    (should-not (plist-get context :hook-rules))))

(mevedel-deftest mevedel-skills-prepare ()
  ,test
  (test)
  :doc "instruction preparation isolates command metadata and forces empty args"
  (let* ((session (mevedel-skills-test--make-session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (mevedel-skill--create
                 :name "alpha"
                 :body "$ARGUMENTS|$0"
                 :context 'fork
                 :model "fast"
                 :effort 'high
                 :agent "reviewer"
                 :allowed-tool-rules rules
                 :hooks hooks))
         outcome
         injection-rules
         expansion-hooks
         dispatched)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-skills--run-body-injections-async)
                 (lambda (body callback)
                   (setq injection-rules
                         (mevedel-request-skill-permission-rules
                          mevedel--current-request))
                   (funcall callback (list :status 'ok :body body))))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq expansion-hooks
                         (mevedel-request-hook-rules
                          mevedel--current-request))
                   (funcall callback nil)))
                ((symbol-function 'mevedel-agent-runtime-dispatch)
                 (lambda (&rest _args) (setq dispatched t))))
        (mevedel-skills-prepare
         skill "ignored"
         (lambda (value) (setq outcome value))
         :role 'instruction :origin 'user)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'instruction (plist-get outcome :kind)))
    (should (equal "|" (plist-get outcome :body)))
    (should (equal rules injection-rules))
    (should-not expansion-hooks)
    (should-not dispatched)
    (let* ((context (plist-get outcome :request-context))
           (record (car (plist-get context :invoked-skills))))
      (should-not (plist-get context :permission-rules))
      (should-not (plist-get context :model))
      (should-not (plist-get context :effort))
      (should-not (plist-get context :hook-rules))
      (should (eq 'instruction
                  (mevedel-skill-invocation-record-role record)))
      (should (eq 'user
                  (mevedel-skill-invocation-record-origin record)))))

  :doc "command preparation returns policy context without committing it"
  (let* ((session (mevedel-skills-test--make-session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Do $ARGUMENTS"
                 :model "fast" :effort 'high
                 :allowed-tool-rules rules :hooks hooks))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _) '(:model inherited :effort high))))
        (mevedel-skills-prepare
         skill "work"
         (lambda (value) (setq outcome value))
         :role 'command :origin 'user :policy-owner-p t)
        (should-not mevedel-skills--pending-request-context)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "Do work" (plist-get outcome :body)))
    (let* ((context (plist-get outcome :request-context))
           (record (car (plist-get context :invoked-skills))))
      (should (equal rules (plist-get context :permission-rules)))
      (should (equal hooks (plist-get context :hook-rules)))
      (should (equal (mevedel-model-tier-selector 'fast)
                     (plist-get context :model)))
      (should (eq 'high (plist-get context :effort)))
      (should (eq 'command
                  (mevedel-skill-invocation-record-role record)))
      (should (eq 'user
                  (mevedel-skill-invocation-record-origin record))))))

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
       :origin 'model))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "YELL loudly" (plist-get outcome :body))))

  :doc "user origin installs the pending stash"
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
       :origin 'user)
      (let ((stash mevedel-skills--pending-request-context))
        (should (equal (mevedel-model-tier-selector 'fast)
                       (plist-get stash :model)))
        (should (equal '(("Read" :action allow))
                       (plist-get stash :permission-rules)))
        (should (= 1 (length (plist-get stash :invoked-skills))))))
	    (should (eq 'ok (plist-get outcome :status))))

  :doc "UserPromptExpansion can rewrite user-origin inline skill output"
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
       :origin 'user))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal
             "Expanded by hook\n\n<hook-context>\n<hook-event name=\"UserPromptExpansion\">\nexpansion context\n</hook-event>\n</hook-context>"
             (plist-get outcome :body)))
    (let ((audit (car (plist-get outcome :hook-audits))))
      (should (eq (plist-get audit :type) 'prompt-rewrite))
      (should (equal (plist-get audit :event) "UserPromptExpansion"))
      (should (equal (plist-get audit :original) "Original body"))
      (should (equal (plist-get audit :submitted) "Expanded by hook"))))

  :doc "malformed UserPromptExpansion decision does not abort user skill"
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
         :origin 'user)
        (should mevedel-skills--pending-request-context)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Original body" (plist-get outcome :body))))

  :doc "UserPromptExpansion can block user-origin inline skill output"
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
       :origin 'user)
      (should-not mevedel-skills--pending-request-context))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'hook-blocked (plist-get outcome :reason)))
    (should (equal "blocked expansion" (plist-get outcome :message))))

  :doc "user-origin preparation failure leaves the pending stash empty"
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
         :origin 'user)
        (should (null mevedel-skills--pending-request-context))
        (should-not (bound-and-true-p mevedel--current-request))))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'injection-failed (plist-get outcome :reason))))

  :doc "model inline origin installs additive context but ignores policy"
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
                 '(("Bash" :pattern "ls" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel--current-request request)
      (mevedel-skills-invoke
       skill nil
       (lambda (value) (setq outcome value))
       :origin 'model))
    (should (equal '(model)
                   (plist-get outcome :ignored-policy-fields)))
    (should (equal '(("Bash" :pattern "ls" :action allow))
                   (mevedel-request-skill-permission-rules request))))

  :doc "disabled user skill tells the user how to enable or escape it"
  (let ((skill (mevedel-skill--create
                :name "hidden"
                :body "X"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-skills--skill-enabled-p)
               (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason)))
    (should (string-match-p "/skills enable hidden"
                            (plist-get outcome :message)))
    (should (string-search "\\$hidden" (plist-get outcome :message))))

  :doc "user-invocable: false rejects user origin"
  (let ((skill (mevedel-skill--create
                :name "internal-only"
                :body "X"
                :user-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'user)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "disable-model-invocation rejects model origin"
  (let ((skill (mevedel-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
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
       :origin 'model
       :skip-gates t))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "X" (plist-get outcome :body))))

  :doc "missing body returns load-failure error"
  (let ((skill (mevedel-skill--create :name "no-body"))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'load-failure (plist-get outcome :reason))))

  :doc "display-callback receives done event on success"
  (let ((skill (mevedel-skill--create :name "ok" :body "Hi"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'done))
                     events)))

  :doc "display-callback receives error event on failure"
  (let ((skill (mevedel-skill--create :name "no-body"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
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

(mevedel-deftest mevedel-skills-dispatch-prepared-fork ()
  ,test
  (test)
  :doc "dispatches prepared fork context and records the invocation"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "unused"))
         (record (mevedel-skill-invocation-record--create
                  :name "demo" :args "task" :role 'command :origin 'user))
         (context (list :permission-rules '(("Read" :action allow))
                        :model '(:tier fast)
                        :effort 'high
                        :hook-rules '((PreToolUse nil))
                        :invoked-skills (list record)))
         (prepared (list :status 'ok :kind 'fork :skill skill
                         :body "prepared body"
                         :hook-audits '((:event "expansion"))
                         :request-context context))
         dispatched
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-get)
                 (lambda (_) agent))
                ((symbol-function 'mevedel-agent-runtime-dispatch)
                 (lambda (callback actual-agent description prompt &rest keys)
                   (setq dispatched
                         (list :agent actual-agent :description description
                               :prompt prompt :keys keys))
                   (funcall callback
                            '(:result "done"
                              :render-data (:agent-id "explorer--1"))))))
        (mevedel-skills-dispatch-prepared-fork
         prepared (lambda (value) (setq outcome value)))))
    (should (eq agent (plist-get dispatched :agent)))
    (should (equal "prepared body" (plist-get dispatched :prompt)))
    (let ((keys (plist-get dispatched :keys)))
      (should (equal '(("Read" :action allow))
                     (plist-get keys :skill-permission-rules)))
      (should (equal '(:tier fast)
                     (plist-get keys :skill-model-override)))
      (should (eq 'high (plist-get keys :skill-effort-override)))
      (should (equal '((PreToolUse nil))
                     (plist-get keys :skill-hook-rules))))
    (should (equal (list record)
                   (mevedel-session-invoked-skills session)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "done" (plist-get outcome :result)))
    (should (equal "explorer--1" (plist-get outcome :agent-id)))
    (should (equal '((:event "expansion"))
                   (plist-get outcome :hook-audits))))

  :doc "rejects an invalid prepared outcome without dispatching"
  (let (outcome dispatched)
    (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (&rest _) (setq dispatched t))))
      (mevedel-skills-dispatch-prepared-fork
       '(:status error :reason failed)
       (lambda (value) (setq outcome value))))
    (should-not dispatched)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-prepared-fork (plist-get outcome :reason)))))

(mevedel-deftest mevedel-skills-invoke-fork ()
  ,test
  (test)
  :doc "model origin routes to direct dispatch via mevedel-agent-runtime-dispatch"
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
              ((symbol-function 'mevedel-agent-runtime-dispatch)
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
         :origin 'model)
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
        ;; When `mevedel-agent-runtime-dispatch' delivers a bare string (no
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
              ((symbol-function 'mevedel-agent-runtime-dispatch)
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
         :origin 'model)
        (should (equal "wrapped" (plist-get outcome :result)))
        (should (equal "explorer--abc123"
                       (plist-get outcome :agent-id)))
        (should (eq 'agent-transcript
                    (plist-get (plist-get outcome :render-data) :kind))))))

  :doc "user origin direct-dispatches and returns fork outcome"
  (let* ((agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body"))
         outcome)
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent)))
              ((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (cb _agent _desc _prompt &rest _args)
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill "the task"
       (lambda (o) (setq outcome o))
       :origin 'user))
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
              ((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (cb _agent _desc prompt &rest _args)
                 (setq captured-prompt prompt)
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill nil #'ignore
       :origin 'user
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
              ((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (cb _agent desc _prompt &rest args)
                 (setq captured-description desc)
                 (setq captured-on-invocation
                       (plist-get args :on-invocation))
                 (funcall cb "agent finished"))))
      (mevedel-skills-invoke
       skill nil #'ignore
       :origin 'user
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
              ((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (&rest _)
                 (error "SubagentStart hook stopped sub-agent"))))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'agent-dispatch-failed (plist-get outcome :reason)))
    (should (string-match-p "SubagentStart hook stopped sub-agent"
                            (plist-get outcome :message))))

  :doc "user-origin fork hooks are active during body injection"
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
                   (setq saw-hooks
                         (mevedel-request-hook-rules
                          mevedel--current-request))
                   (funcall callback '(:status error
                                       :reason stop
                                       :message "stop"))))
	                ((symbol-function 'mevedel-agent-runtime-dispatch)
	                 (lambda (&rest _)
	                   (error "Should not dispatch"))))
        (mevedel-skills-invoke
         skill nil #'ignore
         :origin 'user)))
    (should (equal hooks saw-hooks)))

  :doc "unknown agent yields :reason unknown-agent"
  (let ((skill (mevedel-skill--create
                :name "demo" :context 'fork :agent "missing"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'model))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'unknown-agent (plist-get outcome :reason))))

  :doc "omitted agent (parent-inherited) dispatches to a synthetic agent"
  ;; Parent-inherited fork uses a synthetic `skill:<name>' agent.
  ;; Mock mevedel-agent-runtime-dispatch to assert the dispatch happens with
  ;; the synthetic struct rather than erroring.
  (let* ((skill (mevedel-skill--create
                 :name "demo" :context 'fork
                 :body "Body"))
         (dispatched-agent nil))
    (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
               (lambda (cb agent &rest _)
                 (setq dispatched-agent agent)
                 (funcall cb "result"))))
      (with-temp-buffer
        (setq-local mevedel-agent-exec--agents nil)
        (let (outcome)
          (mevedel-skills-invoke
           skill nil
           (lambda (o) (setq outcome o))
           :origin 'model)
          (should (eq 'ok (plist-get outcome :status)))
          (should (mevedel-agent-p dispatched-agent))
          (should (equal "skill:demo"
                         (mevedel-agent-name dispatched-agent))))))))

(defun test-mevedel-skills--handler-result (envelope)
  "Return the required result from handler ENVELOPE."
  (should (plist-member envelope :result))
  (plist-get envelope :result))

(mevedel-deftest mevedel-skills--render-skill-tool ()
  ,test
  (test)
  :doc "ignored model-side inline policy renders a warning without changing result"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-policy-warning
            :ignored-policy-fields (model effort)))))
    (should (eq 'warning (plist-get rendering :status)))
    (should (string-match-p "ignored model, effort"
                            (plist-get rendering :header)))
    (should (string-match-p "context: fork" (plist-get rendering :body)))
    (should (string-match-p "Prepared body" (plist-get rendering :body))))

  :doc "warning names only the policy field actually ignored"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-policy-warning
            :ignored-policy-fields (effort)))))
    (should (string-match-p "ignored effort" (plist-get rendering :header)))
    (should-not (string-match-p "ignored model" (plist-get rendering :header))))

  :doc "ordinary skill rendering remains successful and unchanged"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body" nil)))
    (should-not (plist-get rendering :status))
    (should (equal "Prepared body" (plist-get rendering :body)))
    (should-not (string-match-p "ignored" (plist-get rendering :header)))))

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
       (lambda (r)
         (setq received (test-mevedel-skills--handler-result r)))
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
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "shout" :arguments "loudly")))
          (should (equal "YELL loudly" received)))
      (delete-directory dir t)))

  :doc "model-side inline policy is view-only render-data"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "OpenAI:gpt-5-mini" :effort 'low))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "costly")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should (equal '(:kind skill-policy-warning
                     :ignored-policy-fields (model effort))
                   (plist-get envelope :render-data))))

  :doc "registered Skill pipeline separates model result from warning view"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "invalid" :effort 'low))
         (tool (mevedel-tool-ensure "Skill"))
         pipeline-result)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local mevedel--session session
                  mevedel--current-request
                  (mevedel-request--create
                   :session session
                   :file-snapshots (make-hash-table :test #'equal)))
      (mevedel-pipeline-run-tool
       tool (lambda (value) (setq pipeline-result value))
       '(:name "costly")))
    (let* ((parts (mevedel-pipeline-extract-render-data
                   pipeline-result session))
           (visible (car parts))
           (render-data (cdr parts))
           (rendering
            (funcall (mevedel-tool-renderer tool)
                     "Skill" '(:name "costly") visible render-data)))
      (should (equal "Prepared body" visible))
      (should (equal '(:kind skill-policy-warning
                       :ignored-policy-fields (model effort))
                     render-data))
      (should (eq 'warning (plist-get rendering :status)))
      (should (string-match-p "context: fork"
                              (plist-get rendering :body)))))

  :doc "model-side inline skill without policy has no render-data"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "plain" :body "Prepared body" :context 'inline))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "plain")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

  :doc "model-side fork policy owns its child and produces no warning"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "isolated" :body "Prepared body"
                 :context 'fork :model "fast"))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
                 (lambda (callback _agent _description _prompt &rest _args)
                   (funcall callback "Child result"))))
        (mevedel-skills--invoke-handler
         (lambda (value) (setq envelope value))
         '(:name "isolated"))))
    (should (equal "Child result" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

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
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "local:shared" :arguments "now"))
            (should (equal "LOCAL now" received))
            (setq received nil)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "shared"))
            (should (string-match-p "Unknown skill 'shared'" received))))
      (delete-directory user-dir t)))

  :doc "disabled skill is rejected before model invocation"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skills-test--stateful-skill
                 :name "hidden"
                 :body "should not run"))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-skills--set-enabled skill nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
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
         (beta (mevedel-skills-test--stateful-skill
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
          (mevedel-skills--set-enabled beta nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "alp")))
          (should (string-match-p "alpha: Alpha helper" received))
          (should-not (string-match-p "beta" received))
          (should-not (string-match-p "internal" received))
          (should-not (string-match-p "dormant" received))
          (setq received nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "Dormant")))
          (should (string-match-p "dormant \\[dormant path-scoped\\]: Dormant"
                                  received))
          (should-not (string-match-p "internal" received)))
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
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
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
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "shared")))
          (should (string-match-p "local:shared: Local helper" received))
          (should (string-match-p "global:shared: Global helper" received))
          (should-not (string-match-p "\nshared: " received)))
      (delete-directory user-dir t))))


;;
;;; Leading and inline invocation bridges

(mevedel-deftest mevedel-skills--parse-skill-line ()
  ,test
  (test)
  :doc "plain `$skill' parses to (name \"\" 0)"
  (should (equal '("greet" "" 0)
                 (mevedel-skills--parse-skill-line "$greet")))

  :doc "`$skill args' parses to (name args 0)"
  (should (equal '("greet" "world" 0)
                 (mevedel-skills--parse-skill-line "$greet world")))

  :doc "plugin-prefixed skill names parse as one skill name"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (mevedel-skills--parse-skill-line
                  "$superpowers:brainstorming now")))

  :doc "additional lines after the skill name are appended to ARGS"
  (should (equal '("coordinator"
                   "Launch three background explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (mevedel-skills--parse-skill-line
                  "$coordinator Launch three background explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("coordinator"
                   "Multi-line task body\nspanning lines"
                   0)
                 (mevedel-skills--parse-skill-line
                  "$coordinator
Multi-line task body
spanning lines")))

  :doc "text not starting with `$' returns nil"
  (should (null (mevedel-skills--parse-skill-line "hello $greet")))
  (should (null (mevedel-skills--parse-skill-line "/greet")))

  :doc "non-identifier skill names are rejected"
  (should (null (mevedel-skills--parse-skill-line "$hi!")))
  (should (null (mevedel-skills--parse-skill-line "$")))

  :doc "leading whitespace is reported via offset"
  (should (equal '("greet" "" 3)
                 (mevedel-skills--parse-skill-line "   $greet"))))

(mevedel-deftest mevedel-skills--inline-skill-mentions ()
  ,test
  (test)
  :doc "finds inline skills, dedupes, and skips literal forms"
  (let* ((session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create :name "alpha" :body "A"))
         (beta (mevedel-skill--create :name "beta" :body "B")))
    (setf (mevedel-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((mentions
             (mevedel-skills--inline-skill-mentions
              "Use $alpha, \"$beta\", '$beta', \\$beta, `$beta`, $alpha, and $beta."
              session)))
        (should (equal '("alpha" "beta")
                       (mapcar (lambda (item) (plist-get item :name))
                               mentions))))))

  :doc "single-backtick code spans and quoted phrases stay literal"
  (let* ((session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create :name "alpha" :body "A"))
         (beta (mevedel-skill--create :name "beta" :body "B")))
    (setf (mevedel-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills--inline-skill-mentions
        "Use `$beta` here and say \"please $alpha\"." session))
      (should-not
       (mevedel-skills--inline-skill-mentions
        "Use '$beta.' here and \"please $alpha.\"." session))))

  :doc "leading command-style skill is not an inline mention"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "A")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills--inline-skill-mentions "$alpha run this"
                                              session))))

  :doc "inline mention scan ignores unknown dollar text"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "A")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills--inline-skill-mentions
        "Keep $PATH literal" session))
      (should
       (mevedel-skills--inline-skill-mentions
        "Use $alpha here" session))))

  :doc "known fork skill becomes a non-forking instruction mention"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "review" :body "R" :context 'fork)))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((result (mevedel-skills--inline-skill-mentions
                     "Please use $review here" session)))
        (should (equal '("review")
                       (mapcar (lambda (item) (plist-get item :name))
                               result))))))

  :doc "known disabled skill becomes an unavailable inline occurrence"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "disabled" :body "D")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-skills--skill-enabled-p)
               (lambda (_skill) nil)))
      (let ((result (mevedel-skills--inline-skill-mentions
                     "Please use $disabled here" session)))
        (should (equal "disabled" (plist-get (car result) :name)))
        (should (plist-get (car result) :unavailable))
        (should (string-match-p "disabled"
                                (plist-get (car result) :message))))))

  :doc "same-named bindings preserve distinct canonical sources"
  (let* ((root (make-temp-file "mevedel-inline-sources-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (session (mevedel-skills-test--make-session))
         (skill-a (mevedel-skill--create
                   :name "alpha" :body "A" :source-file source-a))
         (skill-b (mevedel-skill--create
                   :name "alpha" :body "B" :source-file source-b))
         (input (copy-sequence "Use $alpha then $alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (mevedel-session-skills session) (list skill-a skill-b))
          (mevedel-mention-bindings-set
           4 10
           (list :kind 'skill :token "$alpha" :source-file source-a)
           input)
          (mevedel-mention-bindings-set
           16 22
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                     (lambda (&rest _) nil)))
            (should (equal (list source-a source-b)
                           (mapcar
                            (lambda (mention)
                              (plist-get mention :source-file))
                            (mevedel-skills--inline-skill-mentions
                             input session))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--scan-skill-tokens ()
  ,test
  (test)
  :doc "centralizes root, inline, quote, escape, and code token classification"
  (let ((lookup (lambda (name _start _end)
                  (and (equal name "alpha") 'skill))))
    (should
     (equal '((:start 33 :end 39 :name "alpha" :value skill))
            (mevedel-skills--scan-skill-tokens
             "$alpha \\$alpha `$alpha` \"$alpha\" $alpha" lookup)))
    (should (= 2 (length (mevedel-skills--scan-skill-tokens
                          "$alpha then $alpha" lookup t)))))

  :doc "a bound shorter name wins before punctuation-like name lookup"
  (let* ((source (make-temp-file "mevedel-bound-scan-" nil ".md"))
         (input (copy-sequence "use $alpha."))
         (start (string-match "\\$alpha" input))
         (resolver
          (lambda (name token-start token-end)
            (let ((binding (mevedel-mention-bindings-starting-at
                            input token-start)))
              (if binding
                  (and (mevedel-mention-bindings-at
                        input token-start token-end 'skill (concat "$" name))
                       'bound-alpha)
                (and (equal name "alpha.") 'wrong-alpha-dot))))))
    (unwind-protect
        (progn
          (mevedel-mention-bindings-set
           start (+ start 6)
           (list :kind 'skill :token "$alpha" :source-file source)
           input)
          (let ((tokens (mevedel-skills--scan-skill-tokens
                         input resolver)))
            (should (equal '(bound-alpha)
                           (mapcar (lambda (token)
                                     (plist-get token :value))
                                   tokens)))))
      (delete-file source))))

(mevedel-deftest mevedel-skills-resolve-user-mention-outcome ()
  ,test
  (test)
  :doc "bound mentions resolve only through their exact canonical source"
  (let* ((root (make-temp-file "mevedel-resolve-bound-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (skill-a (mevedel-skill--create
                   :name "alpha" :source-file source-a))
         (skill-b (mevedel-skill--create
                   :name "alpha" :source-file source-b))
         (session (mevedel-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (mevedel-session-skills session) (list skill-a skill-b))
          (mevedel-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (mevedel-skills-resolve-user-mention-outcome
                            input session 0 6 "alpha")))
              (should (eq 'ok (plist-get outcome :status)))
              (should (eq skill-b (plist-get outcome :skill))))))
      (delete-directory root t)))

  :doc "a missing bound source is unavailable without same-name fallback"
  (let* ((root (make-temp-file "mevedel-resolve-missing-" t))
         (source (file-name-concat root "live.md"))
         (missing (file-name-concat root "missing.md"))
         (skill (mevedel-skill--create :name "alpha" :source-file source))
         (session (mevedel-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source (insert "live"))
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file missing)
           input)
          (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (mevedel-skills-resolve-user-mention-outcome
                            input session 0 6 "alpha")))
              (should (eq 'unavailable (plist-get outcome :status)))
              (should-not (plist-get outcome :skill)))))
      (delete-directory root t)))

  :doc "unknown unbound names remain literal successful text"
  (let ((outcome (mevedel-skills-resolve-user-mention-outcome
                  "$unknown" (mevedel-skills-test--make-session)
                  0 8 "unknown")))
    (should (eq 'ok (plist-get outcome :status)))
    (should-not (plist-get outcome :skill))))

(mevedel-deftest mevedel-skills-refresh-bound-input ()
  ,test
  (test)
  :doc "rescans only bound input and exposes the latest exact source body"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-refresh-bound-" t))
         (skill-dir (file-name-concat root ".mevedel/skills"))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "refresh-bound"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         source
         bound)
    (unwind-protect
        (progn
          (setq source
                (mevedel-skills-test--write-skill
                 skill-dir "alpha"
                 "name: alpha\ndescription: Alpha\ncontext: inline\n"
                 "ALPHA V1"))
          (with-temp-buffer
            (mevedel-skills-install session (current-buffer))
            (let ((installed (mevedel-session-skills session)))
              (mevedel-skills-refresh-bound-input "plain text" session)
              (should (eq installed (mevedel-session-skills session))))
            (setq bound (copy-sequence "use $alpha"))
            (mevedel-mention-bindings-set
             4 10
             (list :kind 'skill :token "$alpha" :source-file source)
             bound)
            (mevedel-skills-test--write-skill
             skill-dir "alpha"
             "name: alpha\ndescription: Alpha\ncontext: inline\n"
             "ALPHA V2")
            (should (eq bound
                        (mevedel-skills-refresh-bound-input bound session)))
            (should (equal
                     "ALPHA V2"
                     (mevedel-skill-load-body
                      (mevedel-session-get-skill-by-source
                       session source))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--dispatch-inline-attachments ()
  ,test
  (test)
  :doc "prepares distinct inline skills, appends metadata, and continues"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Alpha body")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (setq-local mevedel-skills--pending-request-context nil)
      (insert "Please use $alpha and $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                   (lambda (&rest _) nil)))
          (should (eq 'skill
                      (mevedel-skills--dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (= 1 (length mevedel-skills--pending-inline-attachments)))
        (should (equal "Alpha body"
                       (plist-get
                        (car mevedel-skills--pending-inline-attachments)
                        :body)))
        (should (string-search "inline-skill-attachments"
                               (buffer-string)))
        (should (= 1 (length (plist-get
                              mevedel-skills--pending-request-context
                              :invoked-skills)))))))

  :doc "unavailable inline skills stage an annotation and continue"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "Alpha body")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (insert "Please use $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills--skill-enabled-p)
                   (lambda (_skill) nil)))
          (should (eq 'skill
                      (mevedel-skills--dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (plist-get
                 (car mevedel-skills--pending-inline-attachments)
                 :unavailable)))))

  :doc "raw fork-context attachment is an instruction and ignores policy"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "review" :body "Review body" :context 'fork
                 :model "invalid-selector" :effort 'impossible
                 :hooks '((PreToolUse nil)))))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (setq-local mevedel-skills--pending-request-context nil)
      (insert "Please use $review here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills--ensure-fresh)
                   (lambda (&rest _) nil))
                  ((symbol-function 'mevedel-agent-runtime-dispatch)
                   (lambda (&rest _) (ert-fail "instruction forked"))))
          (should (eq 'skill
                      (mevedel-skills--dispatch-inline-attachments
                       (lambda () (setq continued t)))))
          (should continued))
        (should (= 1 (length mevedel-skills--pending-inline-attachments)))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :model))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :effort))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :hook-rules))
        (should (= 1 (length (plist-get
                              mevedel-skills--pending-request-context
                              :invoked-skills))))))))

(mevedel-deftest mevedel-skills--transform-expand-inline-attachments ()
  ,test
  (test)
  :doc "replaces live mentions with placeholders and injects reminders"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body")
                              (list :name "beta" :body "Beta body"))))
          (with-temp-buffer
            (insert (propertize
                     "Use $alpha and \"$alpha\", `$beta`, then $beta."
                     'gptel 'prompt))
            (mevedel-skills--transform-expand-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "\\[skill:alpha -- attached\\]" text))
              (should (string-match-p
                       "\\[skill:beta -- attached\\]" text))
              (should (string-match-p (regexp-quote "\"$alpha\"") text))
              (should (string-match-p (regexp-quote "`$beta`") text))
              (should (string-match-p "Alpha body" text))
              (should (string-match-p "Beta body" text))
              (should (string-match-p "<system-reminder>" text)))
            (goto-char (point-min))
            (search-forward "Use [skill:alpha -- attached]")
            (let ((prompt-start (match-beginning 0)))
              (should (= prompt-start
                         (mevedel-transcript-prompt-transform-start))))
            (should (eq 'prompt
                        (get-text-property
                         (+ (match-beginning 0) (length "Use "))
                         'gptel))))
          (with-current-buffer chat
            (should-not mevedel-skills--pending-inline-attachments)))
      (kill-buffer chat)))

  :doc "same-named bound skills retain distinct canonical sources"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-source-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat)))
         (source-a "/tmp/source-a/SKILL.md")
         (source-b "/tmp/source-b/SKILL.md")
         (input (propertize "Use $alpha then $alpha." 'gptel 'prompt)))
    (mevedel-mention-bindings-set
     4 10 (list :kind 'skill :token "$alpha" :source-file source-a) input)
    (mevedel-mention-bindings-set
     16 22 (list :kind 'skill :token "$alpha" :source-file source-b) input)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local
             mevedel-skills--pending-inline-attachments
             (list (list :name "alpha" :source-file source-a :body "Body A")
                   (list :name "alpha" :source-file source-b :body "Body B"))))
          (with-temp-buffer
            (insert input)
            (mevedel-skills--transform-expand-inline-attachments fsm)
            (should (= 2 (how-many "\\[skill:alpha -- attached\\]"
                                   (point-min) (point-max))))
            (should (string-match-p "Body A" (buffer-string)))
            (should (string-match-p "Body B" (buffer-string)))
            (should (= 2 (how-many "<system-reminder>"
                                   (point-min) (point-max))))))
      (kill-buffer chat)))

  :doc "coexists with earlier mention expansion reminders"
  (let* ((root (make-temp-file "mevedel-inline-skill-mentions-" t))
         (file (file-name-concat root "notes.txt"))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-inline-skill-mention-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "Mention body\n"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert (propertize
                     (format "Read @file:%s and use $alpha." file)
                     'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm)
            (mevedel-skills--transform-expand-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "\\[file:.* -- contents attached above\\]" text))
              (should (string-match-p
                       "\\[skill:alpha -- attached\\]" text))
              (should (string-match-p "Mention body" text))
              (should (string-match-p "Alpha body" text))
              (should (= 2 (how-many "<system-reminder>"
                                      (point-min) (point-max))))))
	      (kill-buffer chat)
	      (delete-directory root t))))

  :doc "does not rewrite earlier system-reminder bodies"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-reminder-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert "<system-reminder>\n"
                    "Reminder text keeps $alpha literal.\n"
                    "</system-reminder>\n\n"
                    "User prompt uses $alpha.")
            (mevedel-skills--transform-expand-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       (regexp-quote "Reminder text keeps $alpha literal.")
                       text))
              (should (= 1 (how-many "\\[skill:alpha -- attached\\]"
                                      (point-min) (point-max))))
              (should (string-match-p "Alpha body" text)))))
      (kill-buffer chat)))

  :doc "replaces unavailable mentions without injecting instructions"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-unavailable-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills--pending-inline-attachments
                        '((:name "alpha" :unavailable t
                           :message "skill $alpha is disabled"))))
          (with-temp-buffer
            (insert (propertize "Use $alpha here" 'gptel 'prompt))
            (mevedel-skills--transform-expand-inline-attachments fsm)
            (should (string-match-p
                     (regexp-quote "[skill:alpha -- unavailable]")
                     (buffer-string)))
            (should-not (string-match-p "<system-reminder>"
                                        (buffer-string)))))
      (kill-buffer chat))))

(mevedel-deftest mevedel-skills--dispatch-skill-command ()
  ,test
  (test)
  :doc "skill expansion inserts prepared body and returns 'skill"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-skill-command)))
        (should (equal "### Hello world!"
                       (mevedel-pipeline--strip-render-data-blocks
                        (buffer-string))))
        (should (string-search "<!-- mevedel-render-data -->"
                               (buffer-string))))))

  :doc "user-invocable: false leading skill falls through to inline planning"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "internal-only"
                 :body "ignored"
                 :user-invocable-p nil)))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $internal-only")
        (goto-char (point-max))
        (should-not (mevedel-skills--dispatch-skill-command))
        (should (string-match-p "\\$internal-only" (buffer-string)))
        (should-not mevedel-skills--pending-inline-attachments))))

  :doc "unknown dollar command is left for normal sending"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $PATH is useful context")
      (goto-char (point-max))
      (should (null (mevedel-skills--dispatch-skill-command)))
      (should (equal "### $PATH is useful context" (buffer-string)))))

  :doc "fork-context skill dispatches directly and inserts result"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "coordinator"))
         (skill (mevedel-skill--create
                 :name "coordinator"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "coordinator"))
         save-called status-called stop-called
         baseline-recorded permission-restored
         queue-drain-scheduled mailbox-cleared post-hook-called)
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (require 'mevedel-session-persistence)
        (cl-letf (((symbol-function 'mevedel-agent-get)
                   (lambda (n) (and (equal n "coordinator") agent)))
                  ((symbol-function 'mevedel-agent-runtime-dispatch)
                   (lambda (cb _agent _desc _prompt &rest _args)
                     (funcall cb "agent finished")))
                  ((symbol-function 'mevedel-session-persistence-save)
                   (lambda (s b)
                     (setq save-called (list s b))
                     "saved"))
                  ((symbol-function 'mevedel--compact-record-token-baseline)
                   (lambda (_fsm) (setq baseline-recorded t)))
                  ((symbol-function 'mevedel--run-turn-terminal-hook)
                   (lambda (_fsm event status)
                     (setq stop-called
                           (list event status
                                 (not
                                  (null
                                   (bound-and-true-p
                                    mevedel--current-request)))))))
                  ((symbol-function
                    'mevedel--implementation-permission-mode-restore)
                   (lambda () (setq permission-restored t)))
                  ((symbol-function
                    'mevedel-view--schedule-queued-user-message-drain)
                   (lambda (_fsm) (setq queue-drain-scheduled t)))
                  ((symbol-function 'mevedel-tools--handle-terminal-mailbox)
                   (lambda (_fsm) (setq mailbox-cleared t)))
                  ((symbol-function 'gptel--update-status)
                   (lambda (&rest args) (setq status-called args))))
          (let ((gptel-response-separator "\n\n")
                (gptel-post-response-functions
                 (list (lambda (_start _end)
                         (setq post-hook-called t)
                         (error "Broken post-response hook")))))
            (insert "### $coordinator do the thing")
            (goto-char (point-max))
            (should (eq 'skill (mevedel-skills--dispatch-skill-command)))
            (let ((buf (buffer-string)))
              (should (string-match-p "\\$coordinator do the thing" buf))
              (should (string-match-p "agent finished" buf))
              (should-not (string-match-p "Use the `coordinator` agent" buf))
              (should-not (string-match-p "should-not-appear" buf)))
            (should-not mevedel--current-request)
            (should (= 1 (mevedel-session-turn-count session)))
            (should (equal (list session (current-buffer)) save-called))
            (should (equal '(Stop completed t) stop-called))
            (should baseline-recorded)
            (should permission-restored)
            (should queue-drain-scheduled)
            (should mailbox-cleared)
            (should post-hook-called)
            (should (equal '(" Ready" success) status-called)))))))

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
        (insert "\n$greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills--dispatch-skill-command)))
        (should (equal "Old response\n\nHello world!"
                       (mevedel-pipeline--strip-render-data-blocks
                        (buffer-string))))))))


;;
;;; ListSkills selection primitives

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

  :doc "empty descriptions still produce a stable entry"
  (let ((skill (mevedel-skill--create :name "demo")))
    (should (equal "- demo: "
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
         (disabled (mevedel-skills-test--stateful-skill
                    :name "disabled" :description "D"
                    :model-invocable-p t :active-p t)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list enabled disabled))
          (mevedel-skills--set-enabled disabled nil)
          (let ((names (mapcar #'mevedel-skill-name
                               (mevedel-skills--listing-candidates session))))
            (should (equal '("enabled") names))))
      (delete-directory user-dir t))))

(provide 'test-mevedel-skills-invoke)
;;; test-mevedel-skills-invoke.el ends here
