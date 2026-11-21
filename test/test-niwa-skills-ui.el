;;; test-niwa-skills-ui.el --- Skill command and editor tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests local skill commands, editor support, and send dispatch.

;;; Code:

(require 'niwa-notes)
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'niwa-chat)
(require 'niwa-cockpit)
(require 'niwa-execution)
(require 'niwa-background-list)
(require 'niwa-file-state)
(require 'niwa-goal)
(require 'niwa-hooks)
(require 'niwa-menu)
(require 'niwa-models)
(require 'niwa-mention-bindings)
(require 'niwa-mentions)
(require 'niwa-permissions)
(require 'niwa-persistence)
(require 'niwa-plugins)
(require 'niwa-plugin-ui)
(require 'niwa-reminders)
(require 'niwa-session-persistence)
(require 'niwa-skills-core)
(require 'niwa-skills-input)
(require 'niwa-skills-invoke)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-worktree)
(require 'tabulated-list)
(require 'niwa-view)
(require 'niwa-view-composer)

(defvar niwa-plugin-extra-roots)
(declare-function niwa-session-artifacts-sidecar-path
                  "niwa-session-persistence" (save-path))
(declare-function niwa-session-codec-read
                  "niwa-session-persistence" (file))

(defun test-niwa-skills-ui--materialize-workspace (session tempdir)
  "Give SESSION an isolated workspace root below TEMPDIR with an identity.
A save on a manually materialized session reads the project-owned
workspace identity from the workspace root, so the root must be a real
per-test directory rather than the shared synthetic helper path."
  (let ((root (file-name-as-directory
               (file-name-concat tempdir "workspace"))))
    (make-directory root t)
    (setf (niwa-workspace-root (niwa-session-workspace session)) root)
    (niwa-workspace-identity-ensure root)
    root))


;;
;;; Module ownership

(niwa-deftest niwa-skills-ui-ownership ()
  ,test
  (test)
  (dolist (symbol '(niwa-cmd--edits
                    niwa-cmd--mode
                    niwa-cmd--model
                    niwa-cmd--ps
                    niwa-cmd--skills
                    niwa-cmd--stop
                    niwa-cmd--tools
                    niwa-skills--dispatch-slash-command
                    niwa-skills--fontify-dollar-keyword
                    niwa-skills--gptel-send-advice
                    niwa-skills-parse-slash-line
                    niwa-skills-slash-capf
                    niwa-skills-count-label
                    niwa-skills-local-command-active-request-p
                    niwa-skills-install-font-lock
                    niwa-skills-install-slash-commands
                    niwa-skills-list-open
                    niwa-skills-uninstall-slash-commands
                    niwa-slash-capf))
    (should (equal "niwa-skills-ui"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Slash commands and completion

(defun niwa-skills-test--open-list (session view-buffer data-buffer)
  "Open a skills cockpit for SESSION owned by VIEW-BUFFER and DATA-BUFFER."
  (with-current-buffer data-buffer
    (setq-local niwa--session session)
    (setq-local niwa--view-buffer view-buffer)
    (niwa-skills-list-open
     (list :view-buffer view-buffer
           :data-buffer data-buffer
           :origin-buffer data-buffer
           :session session
           :workspace (niwa-session-workspace session)))))

(defun niwa-skills-test--cleanup-list (&rest buffers)
  "Kill skills cockpit test buffers and BUFFERS."
  (dolist (name (list niwa-skills-list-buffer-name
                      "*niwa skill details*"
                      niwa-skills-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun niwa-skills-test--capf-candidates (capf &optional prefix)
  "Return candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

(niwa-deftest niwa-skills-parse-slash-line ()
  ,test
  (test)
  :doc "plain `/command' parses to (name \"\" 0)"
  (should (equal '("help" "" 0)
                 (niwa-skills-parse-slash-line "/help")))

  :doc "`/command args' parses to (name args 0)"
  (should (equal '("model" "gpt-4" 0)
                 (niwa-skills-parse-slash-line "/model gpt-4")))

  :doc "colon-qualified names parse as commands"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (niwa-skills-parse-slash-line
                  "/superpowers:brainstorming now")))

  :doc "additional lines after the command are appended to ARGS"
  (should (equal '("delegate"
                   "Launch three explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (niwa-skills-parse-slash-line
                  "/delegate Launch three explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("delegate"
                   "Multi-line task body\nspanning lines"
                   0)
                 (niwa-skills-parse-slash-line
                  "/delegate
Multi-line task body
spanning lines")))

  :doc "text not starting with `/' returns nil"
  (should (null (niwa-skills-parse-slash-line "hello /help")))

  :doc "non-identifier command names are rejected"
  (should (null (niwa-skills-parse-slash-line "/hi!")))
  (should (null (niwa-skills-parse-slash-line "/")))

  :doc "leading whitespace is reported via offset"
  (should (equal '("help" "" 3)
                 (niwa-skills-parse-slash-line "   /help")))

  :doc "leading newlines count toward the offset"
  (should (equal '("help" "" 3)
                 (niwa-skills-parse-slash-line "\n\n\n/help"))))

(niwa-deftest niwa-skills--fontify-dollar-keyword ()
  ,test
  (test)
  :doc "font-lock shares root, inline, quoting, and code-span token rules"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "A")))
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local niwa--session session)
      (insert "$alpha root\nUse `$alpha`, \"$alpha\", and $alpha.")
      (goto-char (point-min))
      (should (niwa-skills--fontify-dollar-keyword (point-max)))
      (should (equal "$alpha" (match-string-no-properties 0)))
      (should (niwa-skills--fontify-dollar-keyword (point-max)))
      (should (equal "$alpha" (match-string-no-properties 0)))
      (should (string-prefix-p "and "
                               (buffer-substring-no-properties
                                (- (match-beginning 0) 4)
                                (match-beginning 0))))
      (should-not (niwa-skills--fontify-dollar-keyword (point-max)))))

  :doc "an installed origin bounds the scan to the draft"
  ;; The matcher runs on every refontification.  Without the bound it
  ;; extracted and scanned the whole transcript prefix per keystroke --
  ;; measured in the hundreds of milliseconds on a long session.
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "A")))
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local niwa--session session)
      (insert (make-string 5000 ?x) "\n$alpha history\n")
      (let ((draft-start (point))
            scanned)
        (insert "> draft $alpha here")
        (setq-local niwa-skills--font-lock-origin
                    (lambda () draft-start))
        (cl-letf* ((real (symbol-function
                          'niwa-skills-input-scan-tokens))
                   ((symbol-function 'niwa-skills-input-scan-tokens)
                    (lambda (text &rest args)
                      (push (length text) scanned)
                      (apply real text args))))
          (goto-char draft-start)
          ;; The draft token still matches.
          (should (niwa-skills--fontify-dollar-keyword (point-max)))
          (should (equal "$alpha" (match-string-no-properties 0)))
          (should (> (match-beginning 0) draft-start))
          ;; The transcript was never extracted.
          (should scanned)
          (dolist (length scanned)
            (should (<= length (- (point-max) draft-start))))
          ;; A token before the origin is deliberately left alone.
          (goto-char (point-min))
          (setq scanned nil)
          (should (niwa-skills--fontify-dollar-keyword (point-max)))
          (should (>= (match-beginning 0) draft-start)))))))

(niwa-deftest niwa-skills--dispatch-slash-command ()
  ,test
  (test)
  :doc "local command runs its handler, deletes the region, returns 'local"
  (let ((session (niwa-skills-test--make-session))
        (called nil))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             `(("noop" . ,(lambda (args) (setq called (or args t)))))))
        (insert "/noop hello")
        (goto-char (point-max))
        (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should (equal "hello" called))
        (should (equal "" (buffer-string))))))

  :doc "local surface commands open their cockpit and clear the command"
  (dolist (case '(("/mode" . mode) ("/model" . model)
                  ("/skills" . skills) ("/skills list" . skills)
                  ("/tools" . tools) ("/tools list" . tools) ("/help" . help)))
    (let ((session (niwa-skills-test--make-session))
          called)
      (niwa-skills-test--with-chat-buffer session
        (cl-letf (((symbol-function 'niwa-menu-open)
                   (lambda (area) (setq called area))))
          (insert (car case))
          (goto-char (point-max))
          (should (eq 'local (niwa-test--with-captured-messages nil
                              (niwa-skills--dispatch-slash-command))))
          (should (eq called (cdr case)))
          (should (equal "" (buffer-string)))))))

  :doc "mode slash command with an argument remains direct"
  (let ((session (niwa-skills-test--make-session))
        called)
    (niwa-skills-test--with-chat-buffer session
      (cl-letf (((symbol-function 'niwa-menu-open)
                 (lambda (area) (setq called area))))
        (insert "/mode edits")
        (goto-char (point-max))
        (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should-not called)
        (should (eq 'edits
                    (niwa-session-permission-mode session)))
        (should (equal "" (buffer-string))))))

  :doc "model slash command with an argument remains direct"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "/model gpt-5.5")
      (goto-char (point-max))
      (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
      (should (eq 'gpt-5.5 gptel-model))
      (should (equal "" (buffer-string)))))

  :doc "worktree slash commands open status and list surfaces"
  (let ((session (niwa-skills-test--make-session))
        status-buffer
        list-buffer)
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             (cons '("worktree" . niwa-cmd--worktree)
                   niwa-slash-commands)))
        (cl-letf (((symbol-function 'niwa-worktree-status-open)
                   (lambda () (setq status-buffer (current-buffer))))
                  ((symbol-function 'niwa-worktree-list-open)
                   (lambda () (setq list-buffer (current-buffer)))))
          (dolist (command '("/worktree" "/worktree status"))
            (setq status-buffer nil
                  list-buffer nil)
            (erase-buffer)
            (insert command)
            (goto-char (point-max))
            (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
            (should (eq status-buffer (current-buffer)))
            (should-not list-buffer)
            (should (equal "" (buffer-string))))
          (erase-buffer)
          (insert "/worktree list")
          (goto-char (point-max))
          (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
          (should (eq list-buffer (current-buffer)))
          (should (equal "" (buffer-string)))))))

  :doc "skills mutation slash commands remain direct"
  (let* ((user-dir (make-temp-file "niwa-skills-slash-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (skill (niwa-skills-test--stateful-skill
                 :name "visible"
                 :workspace (niwa-session-workspace session))))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (niwa-skills-test--with-chat-buffer session
            (cl-letf (((symbol-function 'niwa-skills-list-open)
                       (lambda (_session)
                         (ert-fail "skills surface should not open"))))
              (dolist (action '("disable" "enable"))
                (erase-buffer)
                (insert "/skills " action " visible")
                (goto-char (point-max))
                (should (eq 'local (niwa-test--with-captured-messages nil
                                    (niwa-skills--dispatch-slash-command))))
                (should (eq (niwa-skills-skill-enabled-p skill)
                            (equal action "enable")))
                (should (equal "" (buffer-string)))))))
      (delete-directory user-dir t)))

  :doc "local command wins over a same-named skill"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "review"
                 :body "skill body"))
         (called nil))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             `(("review" . ,(lambda (args) (setq called args))))))
        (insert "/review HEAD")
        (goto-char (point-max))
        (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should (equal "HEAD" called))
        (should (equal "" (buffer-string))))))

  :doc "unknown slash command returns 'unknown without mutating the buffer"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "/bogus")
        (goto-char (point-max))
        (should (eq 'unknown (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should (equal "/bogus" (buffer-string))))))

  :doc "nil return when no slash command is present"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "plain text")
      (goto-char (point-max))
      (should (null (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))))

  :doc "slash syntax no longer invokes same-named skills"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "/greet world")
        (goto-char (point-max))
        (should (eq 'unknown (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should (equal "/greet world" (buffer-string)))))))

(niwa-deftest niwa-skills--dispatch-slash-command/layout ()
  ,test
  (test)
  :doc "response followed by /cmd adds a blank line before cursor"
  (let ((session (niwa-skills-test--make-session))
        (called nil))
    (with-temp-buffer
      (setq niwa--session session)
      (let ((niwa-slash-commands
             `(("noop" . ,(lambda (_args) (setq called t))))))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n/noop")
        (goto-char (point-max))
        (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should called)
        (should (equal "Old response\n\n" (buffer-string)))
        (should (= (point) (point-max))))))

  :doc "blank lines above the slash command are preserved"
  (let ((session (niwa-skills-test--make-session))
        (called nil))
    (with-temp-buffer
      (setq niwa--session session)
      (let ((niwa-slash-commands
             `(("noop" . ,(lambda (_args) (setq called t))))))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n\n\n/noop")
        (goto-char (point-max))
        (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
        (should called)
        (should (equal "Old response\n\n\n" (buffer-string))))))

  :doc "`/plugin' and `/plugin list' dispatch to the plugin surface"
  (let ((session (niwa-skills-test--make-session))
        opened
        messages)
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands niwa-slash-commands))
        (dolist (command '("/plugin" "/plugin list"))
          (setq opened nil
                messages nil)
          (erase-buffer)
          (cl-letf (((symbol-function 'niwa-menu-open)
                     (lambda (area)
                       (setq opened area)
                       nil))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format-message fmt args) messages)
                       nil)))
            (insert command)
            (goto-char (point-max))
            (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
            (should (eq opened 'plugins))
            (should-not messages)
            (should (equal "" (buffer-string))))))))

  :doc "`/clear' asks before clearing a non-materialized session"
  (let ((session (niwa-skills-test--make-session))
        (asked nil))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands niwa-slash-commands))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq asked prompt)
                     nil)))
          (insert (propertize "Existing transcript" 'gptel 'response) "\n/clear")
          (goto-char (point-max))
          (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
          (should (equal "Clear all chat buffer content? " asked))
          (should (equal "Existing transcript\n\n"
                         (buffer-string)))))))

  :doc "`/clear' clears a non-materialized session after confirmation"
  (let ((session (niwa-skills-test--make-session))
        start-source)
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands niwa-slash-commands))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'niwa--run-session-start-hooks)
                   (lambda (source)
                     (setq start-source source)
                     (niwa-hooks-record-session-context
                      session '(:additional-context ("clear context"))
                      'SessionStart))))
          (insert (propertize "Existing transcript" 'gptel 'response) "\n/clear")
          (goto-char (point-max))
          (setq-local niwa-compact-estimation--known-token-baseline
                      (list :tokens 258387 :position (copy-marker (point))))
          (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
          (should (equal "" (buffer-string)))
          (should (< (niwa-compact-estimation-estimate-tokens) 1000))
          (should (equal "clear" start-source))
          (should (niwa-session-hook-context-pending session))))))

  :doc "`/clear' falls back with confirmation in a rewind preview buffer"
  (let* ((session (niwa-skills-test--make-session))
         (tempdir (make-temp-file "niwa-clear-preview-test-" t))
         (save-path (file-name-as-directory tempdir)))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) save-path)
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands niwa-slash-commands)
                  (asked nil))
              (setq buffer-file-name nil)
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (prompt)
                           (setq asked prompt)
                           t))
                        ((symbol-function
                                  'niwa-session-artifacts-start-fresh-segment)
	                         (lambda (&rest _args)
	                           (error "Should not rotate preview buffer"))))
                (insert (propertize "Rewound transcript" 'gptel 'response) "\n/clear")
                (goto-char (point-max))
                (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
                (should (equal "Clear all chat buffer content? " asked))
                (should (equal "" (buffer-string)))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))))

  :doc "`/clear' starts a fresh segment when the session is materialized"
  (let* ((session (niwa-skills-test--make-session))
         (tempdir (make-temp-file "niwa-clear-test-" t))
         (save-path (file-name-as-directory tempdir))
         (seg1 (file-name-concat save-path "segment-0001.chat.org"))
         start-source)
    (unwind-protect
        (progn
          (test-niwa-skills-ui--materialize-workspace session tempdir)
          (setf (niwa-session-save-path session) save-path)
          (setf (niwa-session-session-id session) "clear-test")
          (setf (niwa-session-current-segment session) 1)
          (setf (niwa-session-created-at session) "2026-05-08T10-00-00")
          (setf (niwa-session-updated-at session) "2026-05-08T10-00-00")
          (with-temp-buffer
            (org-mode)
            (let ((niwa-slash-commands niwa-slash-commands))
              (setq niwa--session session)
              (setq buffer-file-name seg1)
              (cl-letf (((symbol-function
                          'niwa-session-artifacts-save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'niwa-version)
                         (lambda (&rest _args) "test-version"))
                        ((symbol-function 'niwa--run-session-start-hooks)
                         (lambda (source)
                           (setq start-source source))))
                (insert (propertize "Old response" 'gptel 'response) "\n/clear")
                (goto-char (point-max))
                (setq-local niwa-compact-estimation--known-token-baseline
                            (list :tokens 258387
                                  :position (copy-marker (point))))
                (should (eq 'local (niwa-test--with-captured-messages nil
           (niwa-skills--dispatch-slash-command))))
                (should (= 2 (niwa-session-current-segment session)))
                (should (< (niwa-compact-estimation-estimate-tokens) 1000))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))
                (should-not (string-match-p "Old response" (buffer-string)))
                (should (equal "clear" start-source))
                (let ((old (niwa-session-artifacts-read-artifact
                            session "segment-0001.chat.org" t)))
                  (should (string-match-p "Old response" old))
                  (should-not (string-match-p "/clear" old)))
                (let* ((sidecar
                        (niwa-session-artifacts-sidecar-path save-path))
                       (plist (niwa-session-codec-read sidecar))
                       (seg1-index
                        (cdr (assoc 1 (plist-get plist :prompt-index)))))
                  (should-not seg1-index))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))))

  :doc "`/clear' rotates directly to a fresh segment"
  (let* ((session (niwa-skills-test--make-session))
         (tempdir (make-temp-file "niwa-clear-direct-stale-test-" t))
         (save-path (file-name-as-directory tempdir))
         (seg1 (file-name-concat save-path "segment-0001.chat.org")))
    (unwind-protect
        (progn
          (test-niwa-skills-ui--materialize-workspace session tempdir)
          (setf (niwa-session-save-path session) save-path)
          (setf (niwa-session-session-id session) "clear-direct-stale-test")
          (setf (niwa-session-current-segment session) 1)
          (setf (niwa-session-created-at session) "2026-05-08T10-00-00")
          (setf (niwa-session-updated-at session) "2026-05-08T10-00-00")
          (with-temp-buffer
            (org-mode)
            (progn
              (setq niwa--session session)
              (setq buffer-file-name seg1)
              (insert "Completed turn\n")
              (cl-letf (((symbol-function
                          'niwa-session-artifacts-save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'niwa-version)
                         (lambda (&rest _args) "test-version"))
                        ((symbol-function 'ask-user-about-supersession-threat)
                         (lambda (&rest _args)
                           (error "Supersession prompt"))))
                (niwa-test--with-captured-messages nil
                  (niwa-cmd--clear nil))
                (should (= 2 (niwa-session-current-segment session)))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t)))))

(niwa-deftest niwa-cmd--clear ()
  ,test
  (test)
  :doc "refuses both pending-input categories before confirmation"
  (dolist (category '(steering follow-up))
    (let ((session (niwa-skills-test--make-session))
          asked)
      (niwa-session-enqueue-pending-input
       session category '(:input "keep me"))
      (with-temp-buffer
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _)
                     (setq asked t))))
          (let ((err (should-error (niwa-cmd--clear nil)
                                   :type 'user-error)))
            (should (string-match-p "Pending Inputs"
                                    (error-message-string err)))
            (should (string-match-p "C-c C-q"
                                    (error-message-string err)))
            (should-not asked)))))))

(niwa-deftest niwa-cmd--skills ()
  ,test
  (test)
  :doc "list routes through the skills surface and help remains direct"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (skill (niwa-skills-test--stateful-skill
                 :name "visible"
                 :description "Visible description"
                 :source 'project))
			called-area)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (with-temp-buffer
            (setq niwa--session session)
            (cl-letf (((symbol-function 'niwa-menu-open)
                       (lambda (area)
					(setq called-area area))))
              (niwa-cmd--skills "list")
              (should (eq called-area 'skills))
              (niwa-cmd--skills "help visible")
			     (with-current-buffer "*niwa skill details*"
			       (should (derived-mode-p 'niwa-report-mode))
			       (should (string-match-p "Visible description" (buffer-string)))))))
      (niwa-skills-test--cleanup-list)
      (delete-directory user-dir t)))

  :doc "list falls back to a message outside a live cockpit pair"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'project))
         message-text)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (with-temp-buffer
            (setq niwa--session session)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq message-text (apply #'format fmt args)))))
              (niwa-cmd--skills "list")
              (should (string-match-p
                       "visible \\[enabled\\] source:project - Visible description"
                       message-text)))))
      (niwa-skills-test--cleanup-list)
      (delete-directory user-dir t)))

  :doc "enable and disable preserve a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-skills-test--make-session nil niwa-user-dir))
           (skill (niwa-skills-test--stateful-skill
                   :name "visible" :source 'project
                   :workspace (niwa-session-workspace session)))
           (draft "> quoted\nsecond line")
           (point-offset 4))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-skills session) (list skill))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft point-offset))
      (dolist (action '("disable" "enable"))
        (with-current-buffer data-buf
          (let (messages)
            (niwa-test--with-captured-messages messages
              (niwa-cmd--skills (concat action " visible")))
            (should (equal (format "Skill visible %sd\n" action) messages))))
        (should (eq (niwa-skills-skill-enabled-p skill)
                    (equal action "enable")))
        (with-current-buffer view-buf
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (should (= point-offset (niwa-view-test--composer-point-offset)))))))

  :doc "enable and disable reject unknown skills"
  (let ((session (niwa-skills-test--make-session)))
    (with-temp-buffer
      (setq niwa--session session)
      (should-error (niwa-cmd--skills "disable missing")
                    :type 'user-error)
      (should-error (niwa-cmd--skills "enable missing")
                    :type 'user-error))))

(niwa-deftest niwa-cmd--tools ()
  ,test
  (test)
  :doc "unknown arguments report usage"
  (let ((message-text nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (niwa-cmd--tools "load Edit")
      (should (string-match-p "Usage: /tools \\[list\\]" message-text))))

  :doc "blank arguments require a session"
  (with-temp-buffer
    (should-error (niwa-cmd--tools "") :type 'user-error)))

(niwa-deftest niwa-cmd--ps ()
  ,test
  (test)
  :doc "blank arguments open the execution cockpit and others show usage"
  (let (opened message-text)
    (cl-letf (((symbol-function 'niwa-background-list-open)
               (lambda (&optional _context) (setq opened t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-text (apply #'format format-string args)))))
      (niwa-cmd--ps nil)
      (should opened)
      (niwa-cmd--ps "extra")
      (should (equal "Usage: /ps" message-text)))))

(niwa-deftest niwa-cmd--stop ()
  ,test
  (test)
  :doc "bare /stop stops every live session execution and reports the count"
  (let ((session (niwa-skills-test--make-session))
        stopped message-text)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-stop-all-user)
                 (lambda (seen-session)
                   (setq stopped seen-session)
                   3))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-text
                         (apply #'format format-string args)))))
        (niwa-cmd--stop nil)))
    (should (eq session stopped))
    (should (equal "niwa: 3 executions stopping" message-text)))
  :doc "/stop ID uses session-wide user authority"
  (let ((session (niwa-skills-test--make-session)) stopped)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-stop-user)
                 (lambda (seen-session id)
                   (setq stopped (list seen-session id)))))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--stop " exec-000009 "))))
    (should (equal (list session "exec-000009") stopped))))

(niwa-deftest niwa-skills-local-command-active-request-p ()
  ,test
  (test)
  :doc "allows process controls and only the safe mid-request goal actions"
  (dolist (command '(("ps" nil) ("stop" "exec-000001")
                     ("goal" "pause") ("goal" "edit new objective")))
    (should (apply #'niwa-skills-local-command-active-request-p command)))
  (dolist (command '(("goal" "resume") ("tools" nil) ("stopper" nil)))
    (should-not
     (apply #'niwa-skills-local-command-active-request-p command))))

(niwa-deftest niwa-skills-count-label
  (:vars* ((user-dir (make-temp-file "niwa-skills-count-" t))
           (niwa-user-dir (file-name-as-directory user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "returns enabled/total skills for a session"
  (let* ((session (niwa-skills-test--make-session nil user-dir))
         (enabled (niwa-skill--create :name "enabled"))
         (disabled (niwa-skills-test--stateful-skill
                    :name "disabled"
                    :workspace (niwa-session-workspace session))))
    (setf (niwa-session-skills session) (list enabled disabled))
    (niwa-skills-set-enabled disabled nil)
    (should (equal "1/2" (niwa-skills-count-label session))))

  :doc "returns 0/0 without a session"
  (should (equal "0/0" (niwa-skills-count-label nil))))

(niwa-deftest niwa-skills-list-open ()
  ,test
  (test)
  :doc "renders session skills as a tabulated cockpit"
  (let* ((user-dir (make-temp-file "niwa-skills-list-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (view-buffer (generate-new-buffer " *skills-list-view*"))
         (data-buffer (generate-new-buffer " *skills-list-data*"))
         (source-file (make-temp-file "niwa-skill-source-"))
         (active (niwa-skill--create
                  :name "active" :description "Active description"
                  :source 'project :source-file source-file))
         (disabled (niwa-skills-test--stateful-skill
                    :name "disabled" :description "Disabled description"
                    :source 'user
                    :workspace (niwa-session-workspace session)))
         (plugin (niwa-skill--create
                  :name "plugin:assist" :description "Plugin description"
                  :source 'plugin)))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session)
                (list active disabled plugin))
          (niwa-skills-set-enabled disabled nil)
          (let ((buffer (niwa-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (should (string-match-p
                       "2/3 enabled"
                       (niwa-cockpit-surface-header-line)))
              (should (= 3 (length tabulated-list-entries)))
              (let ((rows (niwa-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc "active" rows))
                               '("enabled" "active" "project"
                                 "Active description")))
                (should (equal (cdr (assoc "disabled" rows))
                               '("disabled" "disabled" "user"
                                 "Disabled description")))
                (should (equal (cdr (assoc "plugin:assist" rows))
                               '("enabled" "plugin:assist" "plugin"
                                 "Plugin description"))))
              (niwa-cockpit-goto-id "active")
              (niwa-skills-list-details))
            (with-current-buffer "*niwa skill details*"
              (should (string-match-p "Active description"
                                      (buffer-string)))
              (should (string-match-p (regexp-quote source-file)
                                      (buffer-string)))))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer)
      (when (file-exists-p source-file)
        (delete-file source-file))
      (delete-directory user-dir t)))

(niwa-deftest niwa-skills-list-refresh ()
  ,test
  (test)
  :doc "refresh updates visible skill row content"
  (let* ((user-dir (make-temp-file "niwa-skills-refresh-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-refresh-view*"))
         (data-buffer (generate-new-buffer " *skills-refresh-data*"))
         (first (niwa-skill--create :name "first" :source 'project))
         (second (niwa-skill--create :name "second" :source 'project)))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list first second))
          (let ((buffer (niwa-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id "second")
              (setf (niwa-skill-description second) "Updated")
              (niwa-skills-list-refresh)
              (let ((rows (niwa-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc "second" rows))
                               '("enabled" "second" "project"
                                 "Updated")))))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills-list--item-id ()
  ,test
  (test)
  :doc "uses the visible skill name as the row id"
  (let ((skill (niwa-skill--create :name "plugin:assist")))
    (should (equal (niwa-skills-list--item-id skill)
                   "plugin:assist"))))

(niwa-deftest niwa-skills-list--status-cell ()
  ,test
  (test)
  :doc "formats enabled and disabled status cells"
  (let* ((user-dir (make-temp-file "niwa-skills-status-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (workspace (niwa-skills-test--make-workspace user-dir))
         (skill (niwa-skills-test--stateful-skill
                 :name "visible" :workspace workspace)))
    (unwind-protect
        (progn
          (should (equal (substring-no-properties
                          (niwa-skills-list--status-cell skill))
                         "enabled"))
          (niwa-skills-set-enabled skill nil)
          (should (equal (substring-no-properties
                          (niwa-skills-list--status-cell skill))
                         "disabled")))
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills-list--entry ()
  ,test
  (test)
  :doc "builds table cells from skill state, name, source, and description"
  (let* ((user-dir (make-temp-file "niwa-skills-entry-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (workspace (niwa-skills-test--make-workspace user-dir))
         (skill (niwa-skills-test--stateful-skill
                 :name "visible"
                 :description "Visible description"
                 :source 'plugin
                 :workspace workspace))
         (entry (unwind-protect
                    (progn
                      (niwa-skills-set-enabled skill nil)
                      (niwa-skills-list--entry skill))
                  (delete-directory user-dir t))))
    (should (equal (car entry) "visible"))
    (should (equal (niwa-test-tabulated-row-cells entry)
                   '("disabled" "visible" "plugin"
                     "Visible description")))))

(niwa-deftest niwa-skills--skill-detail-report ()
  ,test
  (test)
  :doc "shows stored skill warnings in inspection details"
  (let* ((warning
          "Agent explorer is ignored for inline skills; use context: fork to select an agent")
         (skill (niwa-skill--create
                 :name "inspect" :description "Inspectable"
                 :source 'project :warnings (list warning)))
			(details (niwa-report-test-text (niwa-skills--skill-detail-report skill))))
    (should (string-match-p "Warnings:" details))
    (should (string-match-p (regexp-quote (concat "- " warning)) details)))

  :doc "omits the warnings section when discovery found none"
  (let* ((skill (niwa-skill--create
                 :name "clean" :description "Clean"
                 :source 'project))
			(details (niwa-report-test-text (niwa-skills--skill-detail-report skill))))
    (should-not (string-match-p "Warnings:" details)))

  :doc "shows resolved, dormant, invalid, and transitively disabled dependencies"
  (let* ((child-file "/tmp/child/SKILL.md")
         (child (niwa-skill--create
                 :name "child" :source-file child-file :active-p nil))
         (dependency
          (niwa-skill-dependency--create
           :name "child"
           :source-key (niwa-skills-source-key child-file)))
         (parent (niwa-skill--create
                  :name "parent" :dependencies (list dependency)
                  :dependency-diagnostics '("Required skill is invalid")
                  :model-invocable-p t
                  :effective-model-invocable-p nil))
         (session (niwa-skills-test--make-session "details"))
         details)
    (setf (niwa-session-skills session) (list parent child))
    (setq details
			 (niwa-report-test-text (niwa-skills--skill-detail-report parent (list :session session))))
    (should (string-match-p "Required skills:" details))
    (should (string-match-p "child.*\[dormant\]" details))
    (should (string-match-p "Dependency errors:" details))
    (should (string-match-p "disabled by a required skill" details))))

(niwa-deftest niwa-skills-list--workspace-label ()
  ,test
  (test)
  :doc "returns the rendered workspace root or no workspace"
  (let ((session (niwa-skills-test--make-session "named"))
        (view-buffer (generate-new-buffer " *skills-label-view*"))
        (data-buffer (generate-new-buffer " *skills-label-data*")))
    (with-temp-buffer
      (niwa-skills-list-mode)
      (should (equal (niwa-skills-list--workspace-label) "no workspace")))
    (unwind-protect
        (let ((buffer (niwa-skills-test--open-list
                       session view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (equal (niwa-skills-list--workspace-label
                            (niwa-cockpit-surface-context))
                           (niwa-test--placeholder-root)))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer))))

(niwa-deftest niwa-skills-list--header-line ()
  ,test
  (test)
  :doc "summarizes the session skill count and points at the keys"
  (let* ((user-dir (make-temp-file "niwa-skills-header-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (workspace (niwa-skills-test--make-workspace user-dir))
         (enabled (niwa-skill--create :name "enabled"))
         (disabled (niwa-skills-test--stateful-skill
                    :name "disabled" :workspace workspace)))
    (unwind-protect
        (with-temp-buffer
          (niwa-skills-list-mode)
          (niwa-skills-set-enabled disabled nil)
          (let ((line (niwa-skills-list--header-line
                       (list enabled disabled)
                       nil)))
            (should (string-match-p "niwa: skills" line))
            (should (string-match-p "1/2 enabled" line))
            (should (string-match-p "? keys" line))
            ;; The key list belongs in `?' help, not in the header.
            (should-not (string-match-p "RET details" line))))
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills--list-message-text ()
  ,test
  (test)
  :doc "formats a fallback list message for visible session skills"
  (let* ((user-dir (make-temp-file "niwa-skills-message-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "visible"
                 :description "Visible description"
                 :source 'project)))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (should (string-prefix-p
                   (format "niwa skills for workspace %s\n"
                           (niwa-test--placeholder-root))
                   (niwa-skills--list-message-text session)))
          (should (string-match-p
                   "visible \\[enabled\\] source:project - Visible description"
                   (niwa-skills--list-message-text session))))
      (delete-directory user-dir t)))

  :doc "reports empty sessions"
  (let ((session (niwa-skills-test--make-session)))
    (should (string-match-p "No skills available"
                            (niwa-skills--list-message-text session)))))

(niwa-deftest niwa-skills-list-toggle-enabled ()
  ,test
  (test)
  :doc "toggles selected skill state and refreshes the owner view prompt"
  (let* ((user-dir (make-temp-file "niwa-skills-toggle-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (view-buffer (generate-new-buffer " *skills-toggle-view*"))
         (data-buffer (generate-new-buffer " *skills-toggle-data*"))
         (skill (niwa-skills-test--stateful-skill
                 :name "visible" :source 'project
                 :workspace (niwa-session-workspace session)))
         refreshed-buffer
         message-text)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (let ((buffer (niwa-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id "visible")
              (cl-letf (((symbol-function 'niwa-view-refresh-input-prompt)
                         (lambda ()
                           (setq refreshed-buffer (current-buffer))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (setq message-text (apply #'format fmt args)))))
                (niwa-skills-list-toggle-enabled)
                (should-not (niwa-skills-skill-enabled-p skill))
                (should (eq refreshed-buffer view-buffer))
                (should (equal (cdr (assoc
                                     "visible"
                                     (niwa-test-tabulated-entries-cells)))
                               '("disabled" "visible" "project" "")))
                (should (string-match-p "disabled" message-text))
                (niwa-skills-list-toggle-enabled)
                (should (niwa-skills-skill-enabled-p skill))
                (should (string-match-p "enabled" message-text))))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t)))

  :doc "preserves a multiline leading-> composer draft"
  (let ((draft "> quoted\nsecond line")
        (point-offset 4))
    (niwa-view-test--with-buffers
      (let* ((session (niwa-skills-test--make-session
                       nil niwa-user-dir))
             (skill (niwa-skills-test--stateful-skill
                     :name "visible" :source 'project
                     :workspace (niwa-session-workspace session))))
        (with-current-buffer data-buf
          (setq-local niwa--session session))
        (setf (niwa-session-skills session) (list skill))
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft draft point-offset))
        (niwa-skills-list-open
         (niwa-cockpit-context-for-buffer view-buf))
        (with-current-buffer niwa-skills-list-buffer-name
          (niwa-cockpit-goto-id "visible")
          (niwa-test--with-captured-messages nil
            (niwa-skills-list-toggle-enabled)))
        (with-current-buffer view-buf
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (should (= point-offset (niwa-view-test--composer-point-offset))))))))

(niwa-deftest niwa-skills-list-open-source ()
  ,test
  (test)
  :doc "opens SKILL.md first and falls back to the source directory"
  (let* ((user-dir (make-temp-file "niwa-skills-source-user-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (root (make-temp-file "niwa-skills-source-root-" t))
         (source-file (niwa-skills-test--write-skill
                       root "visible" "description: Visible\n"))
         (source-dir (file-name-directory source-file))
         (session (niwa-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-source-view*"))
         (data-buffer (generate-new-buffer " *skills-source-data*"))
         (skill (niwa-skill--create
                 :name "visible"
                 :source 'project
                 :source-file source-file
                 :source-dir source-dir))
         opened)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (let ((buffer (niwa-skills-test--open-list
                         session view-buffer data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id "visible")
              (cl-letf (((symbol-function 'find-file)
                         (lambda (target &rest _)
                           (setq opened target))))
                (niwa-skills-list-open-source)
                (should (equal opened source-file))
                (delete-file source-file)
                (niwa-skills-list-open-source)
                (should (equal opened source-dir))))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills-list-help ()
  ,test
  (test)
  :doc "opens skills cockpit help"
  (unwind-protect
      (progn
        (niwa-skills-list-help)
        (with-current-buffer niwa-skills-help-buffer-name
          (should (string-match-p "RET  Show selected skill details"
                                  (buffer-string)))
          (should (string-match-p "e    Enable or disable selected skill"
                                  (buffer-string)))
          (should (string-match-p "/skills enable NAME"
                                  (buffer-string)))))
    (niwa-skills-test--cleanup-list)))

(niwa-deftest niwa-skills-list-quit ()
  (let* ((user-dir (make-temp-file "niwa-skills-quit-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (view-buffer (generate-new-buffer " *skills-quit-view*"))
         (data-buffer (generate-new-buffer " *skills-quit-data*"))
         (skill (niwa-skill--create :name "visible"))
         called-buffer)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (with-current-buffer data-buffer
            (setq-local niwa--session session
                        niwa--view-buffer view-buffer))
          (let* ((context
                  (list :view-buffer view-buffer :data-buffer data-buffer
                        :origin-buffer data-buffer :session session
                        :workspace (niwa-session-workspace session)
                        :return-menu (lambda () (setq called-buffer (current-buffer)))))
                 ;; A carried continuation alone is not enough: the opening
                 ;; command must actually be running as a niwa suffix.
                 (buffer
                  (let ((transient-current-command
                         (and ,menu-origin 'niwa-menu--top)))
                    (niwa-skills-list-open context))))
            (cl-letf (((symbol-function 'niwa-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (with-current-buffer buffer
                (niwa-skills-list-quit)))
            (should-not (buffer-live-p buffer))
            (if ,menu-origin
                (should (eq called-buffer data-buffer))
              (should-not called-buffer))))
      (niwa-skills-test--cleanup-list view-buffer data-buffer)
      (delete-directory user-dir t)))
  (menu-origin)
  :doc "direct skills quit kills the table without reopening the menu" nil
  :doc "menu-origin skills quit kills the table and invokes its continuation" t)

(niwa-deftest niwa-skills--gptel-send-advice ()
  ,test
  (test)
  :doc "prefix-zero steering does not dispatch commands from transcript text"
  (let ((session (niwa-skills-test--make-session))
        received)
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands `(("noop" . ,#'ert-fail))))
        (insert "/noop")
        (niwa-skills--gptel-send-advice
         (lambda (&rest args) (setq received args)) 0)
        (should (equal '(0) received)))))

  :doc "a dispatched send passes through with its model input and skill stash"
  (let ((session (niwa-skills-test--make-session))
        received)
    (niwa-skills-test--with-chat-buffer session
      (setq-local niwa-skills--pending-request-context '(:permission-rules nil))
      (insert "$hi")
      (niwa--dispatch-request
       "EXPANDED"
       (lambda ()
         (niwa-skills--gptel-send-advice
          (lambda (&rest _) (setq received niwa--pending-model-input)))))
      (should (equal "EXPANDED" received))
      (should niwa-skills--pending-request-context)
      ;; Composer input is for external engines; gptel keeps its stored prompt.
      (niwa--dispatch-request
       nil (lambda () (setq received (list niwa--pending-model-input)))
       "COMPOSER INPUT")
      (should (equal '(nil) received))))

  :doc "local command aborts the send (orig-fn not called)"
  ;; The advice is `:around', so we assert behavior by checking whether
  ;; the original send is called.
  (let ((session (niwa-skills-test--make-session))
        (orig-called nil))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             `(("noop" . ,(lambda (_args) nil)))))
        (insert "/noop")
        (goto-char (point-max))
        (niwa-test--with-captured-messages nil
          (niwa-skills--gptel-send-advice
           (lambda (&rest _) (setq orig-called t))))
        (should-not orig-called))))

  :doc "unknown slash command aborts the send"
  (let ((session (niwa-skills-test--make-session))
        (orig-called nil))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "/bogus")
        (goto-char (point-max))
        (niwa-test--with-captured-messages nil
          (niwa-skills--gptel-send-advice
           (lambda (&rest _) (setq orig-called t))))
        (should-not orig-called))))

  :doc "expanded `$' skill lets the send proceed"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "hi" :body "Hi!"))
         (orig-called nil))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$hi")
        (goto-char (point-max))
        (niwa-test--with-captured-messages nil
          (niwa-skills--gptel-send-advice
           (lambda (&rest _) (setq orig-called t))))
        (should orig-called))))

  :doc "unknown dollar-prefixed text still sends normally"
  (let ((session (niwa-skills-test--make-session))
        (orig-called nil))
    (niwa-skills-test--with-chat-buffer session
      (insert "$PATH can be mentioned")
      (goto-char (point-max))
      (niwa-test--with-captured-messages nil
        (niwa-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t))))
      (should orig-called)))

  :doc "plain text always lets the send proceed"
  (let ((session (niwa-skills-test--make-session))
        (orig-called nil))
    (niwa-skills-test--with-chat-buffer session
      (insert "just a normal message")
      (goto-char (point-max))
      (niwa-test--with-captured-messages nil
        (niwa-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t))))
      (should orig-called)))

  :doc "unavailable leading skills annotate and let the send proceed"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "alpha" :body "ignored" :user-invocable-p nil))
         orig-called
         staged)
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (insert "$alpha")
      (goto-char (point-max))
      (niwa-test--with-captured-messages nil
        (niwa-skills--gptel-send-advice
         (lambda (&rest _)
           (setq orig-called t
                 staged
                 (copy-tree
                  niwa-skills-input--pending-inline-attachments)))))
      (should orig-called)
      (should (plist-get (car staged) :unavailable))))

  :doc "an unavailable leading skill does not suppress later attachments"
  (let* ((session (niwa-skills-test--make-session))
         (alpha (niwa-skill--create
                 :name "alpha" :body "ignored" :user-invocable-p nil))
         (beta (niwa-skill--create :name "beta" :body "BETA BODY"))
         orig-called
         staged)
    (setf (niwa-session-skills session) (list alpha beta))
    (niwa-skills-test--with-chat-buffer session
      (insert "$alpha then use $beta")
      (goto-char (point-max))
      (niwa-test--with-captured-messages nil
        (niwa-skills--gptel-send-advice
         (lambda (&rest _)
           (setq orig-called t
                 staged
                 (copy-tree
                  niwa-skills-input--pending-inline-attachments)))))
      (should orig-called)
      (should (= 2 (length staged)))
      (should (plist-get (car staged) :unavailable))
      (should (equal "BETA BODY" (plist-get (cadr staged) :body)))))

  :doc "manually typed raw skills bind their canonical source before send"
  (let* ((session (niwa-skills-test--make-session))
         (root (make-temp-file "niwa-raw-skill-" t))
         (source (file-name-concat root "SKILL.md"))
         (skill (niwa-skill--create
                 :name "alpha" :body "ALPHA" :source-file source))
         binding)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "---\nname: alpha\ndescription: Alpha\n---\n\nALPHA\n"))
          (setf (niwa-session-skills session) (list skill))
          (niwa-skills-test--with-chat-buffer session
            (insert "use $alpha")
            (goto-char (point-max))
            (niwa-test--with-captured-messages nil
              (niwa-skills--gptel-send-advice
               (lambda (&rest _)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "$alpha")
                   (setq binding
                         (get-text-property
                          (match-beginning 0)
                          'niwa-mention-binding))))))
            (should (equal source (plist-get binding :source-file)))))
      (delete-directory root t)))

  :doc "manually typed raw direct references bind their UUID before send"
  (let* ((root (make-temp-file "niwa-raw-ref-" t))
         (file (file-name-concat root "reference.txt"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "raw-ref"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         ref-buffer ref binding)
    (unwind-protect
        (progn
          (with-temp-file file (insert "reference body\n"))
          (setq ref-buffer (find-file-noselect file))
          (with-current-buffer ref-buffer
            (setq-local niwa--workspace ws)
            (setq ref (niwa-instruction-test--create-note-in
                       ref-buffer (point-min) (1- (point-max)))))
          (let ((token (format "@ref:%d" (niwa--instruction-id ref))))
            (niwa-skills-test--with-chat-buffer session
              (insert "inspect " token)
              (goto-char (point-max))
              (niwa-test--with-captured-messages nil
                (niwa-skills--gptel-send-advice
                 (lambda (&rest _)
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward token)
                     (setq binding
                           (get-text-property
                            (match-beginning 0)
                            'niwa-mention-binding)))))))
            (should (equal (overlay-get ref 'niwa-uuid)
                           (plist-get binding :reference-uuid)))))
      (when (buffer-live-p ref-buffer)
        (with-current-buffer ref-buffer (set-buffer-modified-p nil))
        (kill-buffer ref-buffer))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "malformed bindings block raw gptel submission"
  (let ((session (niwa-skills-test--make-session))
        orig-called)
    (niwa-skills-test--with-chat-buffer session
      (insert "$alpha")
      (with-silent-modifications
        (put-text-property
         (- (point) 6) (point) 'niwa-mention-binding
         '(:kind skill :token "$alpha")))
      (should-error
       (niwa-test--with-captured-messages nil
         (niwa-skills--gptel-send-advice
          (lambda (&rest _) (setq orig-called t))))
       :type 'user-error)
      (should-not orig-called)))

  :doc "no session: advice still calls orig-fn"
  (let ((orig-called nil))
    (with-temp-buffer
      (setq niwa--session nil)
      (niwa-test--with-captured-messages nil
        (niwa-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t))))
      (should orig-called)))

  :doc "paired view buffers bypass dollar rescanning of planned prompts"
  (let ((session (niwa-skills-test--make-session))
        (view-buffer (generate-new-buffer " *niwa-planned-view*"))
        orig-called
        dispatched)
    (unwind-protect
        (niwa-skills-test--with-chat-buffer session
          (setq-local niwa--view-buffer view-buffer)
          (insert "prepared body says $nested")
          (cl-letf (((symbol-function 'niwa-skills-input-dispatch-command)
                     (lambda (&optional _) (setq dispatched t))))
            (niwa-test--with-captured-messages nil
              (niwa-skills--gptel-send-advice
               (lambda (&rest _) (setq orig-called t)))))
          (should orig-called)
          (should-not dispatched))
      (kill-buffer view-buffer)))

  :doc "stash leaks are cleared after advice returns"
  ;; `unwind-protect' clears the pending stash if the begin handler did
  ;; not drain it (e.g. because gptel-send aborted before WAIT).
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "plain text")
      (goto-char (point-max))
      ;; Simulate a leaked stash (e.g., from a prior failed dispatch).  Its
      ;; model still governs the dispatched send before cleanup.
      (setq-local niwa-skills--pending-request-context
                  (list :permission-rules nil
                        :model (list :backend gptel-backend :model 'haiku)))
      (setq-local niwa-skills-input--pending-inline-attachments
                  (list (list :name "alpha")))
      (let ((baseline gptel-model)
            (gptel-use-tools nil)
            (gptel-use-context nil)
            fsm)
        (niwa-test--with-captured-messages nil
          (niwa-skills--gptel-send-advice
           (lambda (&rest _)
             (should (eq baseline gptel-model))
             (setq fsm
                   (gptel-request nil :dry-run t
                     :transforms
                     '(niwa-skills--transform-apply-request-model-policy))))))
        ;; The model applies to gptel's private prompt and realized payload,
        ;; not a binding around the shared source buffer's send callback.
        (should (eq 'haiku (plist-get (gptel-fsm-info fsm) :model)))
        (should (equal "haiku" (plist-get (plist-get (gptel-fsm-info fsm) :data) :model)))
        (should (eq baseline gptel-model)))
      (should (null niwa-skills--pending-request-context))
      (should (null niwa-skills-input--pending-inline-attachments))))

  :doc "stash leaks cleared even when orig-fn signals an error"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "plain text")
      (goto-char (point-max))
      (setq-local niwa-skills--pending-request-context
                  (list :permission-rules nil
                        :model (list :backend gptel-backend :model 'haiku)))
      (setq-local niwa-skills-input--pending-inline-attachments
                  (list (list :name "alpha")))
      (ignore-errors
        (niwa-test--with-captured-messages nil
          (niwa-skills--gptel-send-advice
           (lambda (&rest _) (error "Boom")))))
      (should (null niwa-skills--pending-request-context))
      (should (null niwa-skills-input--pending-inline-attachments)))))

(niwa-deftest niwa-slash-capf ()
  ,test
  (test)
  :doc "global commands expose the Goal and Plan entry points"
  (let ((session (niwa-skills-test--make-session)))
    (should (eq 'niwa-plugins-slash-command
                (cdr (assoc "plugin" niwa-slash-commands))))
    (should (eq 'niwa-cmd--skills
                (cdr (assoc "skills" niwa-slash-commands))))
    (should (eq 'niwa-cmd--goal
                (cdr (assoc "goal" niwa-slash-commands))))
    (should (eq 'niwa-cmd--plan
                (cdr (assoc "plan" niwa-slash-commands))))
    (should (eq 'niwa-cmd--prompt
                (cdr (assoc "prompt" niwa-slash-commands))))
    (niwa-skills-test--with-chat-buffer session
      (insert "/pl")
      (goto-char (point-max))
      (let* ((capf (niwa-slash-capf))
             (cands (and capf
                         (niwa-skills-test--capf-candidates capf "pl")))
             (annot (and capf (plist-get (nthcdr 3 capf)
                                         :annotation-function))))
        (should capf)
        (should (member "plan" cands))
        (should (member "plugin" cands))
        (should (string-match-p "optional prompt" (funcall annot "plan")))
        (should (string-match-p "list" (funcall annot "plugin"))))))

  :doc "slash root returns local commands only"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "simplify")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("help" . ignore)
                                      ("tokens" . ignore))))
        (insert "/")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (cands (and capf
                           (niwa-skills-test--capf-candidates capf)))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (member "help" cands))
          (should (member "tokens" cands))
          (should-not (member "simplify" cands))
          (should (equal " [command] no args; list commands and skills"
                         (funcall annot "help")))))))

  :doc "dollar root returns session skills only"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "simplify")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("help" . ignore)
                                      ("tokens" . ignore))))
        (insert "$")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (cands (and capf
                           (niwa-skills-test--capf-candidates capf)))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (member "simplify" cands))
          (should-not (member "help" cands))
          (should-not (member "tokens" cands))
          (should (equal " [skill]" (funcall annot "simplify")))))))

  :doc "dollar root includes inline and fork skills"
  (let* ((session (niwa-skills-test--make-session))
         (inline (niwa-skill--create
                  :name "summarize"
                  :context 'inline))
         (fork (niwa-skill--create
                :name "review"
                :context 'fork)))
    (setf (niwa-session-skills session) (list inline fork))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (cands (and capf
                           (niwa-skills-test--capf-candidates capf))))
          (should capf)
          (should (member "summarize" cands))
          (should (member "review" cands))))))

  :doc "inline dollar completion offers inline skills only"
  (let* ((session (niwa-skills-test--make-session))
         (inline (niwa-skill--create
                  :name "summarize"
                  :context 'inline))
         (fork (niwa-skill--create
                :name "review"
                :context 'fork)))
    (setf (niwa-session-skills session) (list inline fork))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "Please use $")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (cands (and capf
                           (niwa-skills-test--capf-candidates capf))))
          (should capf)
          (should (member "summarize" cands))
          (should-not (member "review" cands))))))

  :doc "plugin-prefixed skill candidates complete as one dollar name"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "superpowers:brainstorming")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$superpowers:b")
        (goto-char (point-max))
        (let ((capf (niwa-slash-capf)))
          (should capf)
          (should (equal '("superpowers:brainstorming")
                         (niwa-skills-test--capf-candidates
                          capf "superpowers:b")))))))

  :doc "special command annotations describe argument behavior"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             '(("tokens" . ignore)
               ("model" . ignore)
               ("compact" . ignore)
               ("goal" . ignore)
               ("mode" . ignore)
               ("edits" . ignore)
               ("clear" . ignore)
               ("help" . ignore)
               ("init" . ignore)
               ("plugin" . ignore)
               ("tools" . ignore)
               ("review" . ignore)
               ("verify" . ignore))))
        (insert "/")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (dolist (name (mapcar #'car niwa-slash-commands))
            (should (string-prefix-p " [command]" (funcall annot name)))
            (should-not (equal " [command]" (funcall annot name))))
          (should (string-match-p "ask" (funcall annot "mode")))
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
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("mode" . ignore))))
        (insert "/mode fu")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("full-auto")
                         (niwa-skills-test--capf-candidates
                          capf "fu")))
          (should (member "edits"
                          (niwa-skills-test--capf-candidates capf)))
          (should (string-match-p "auto-allow"
                                  (funcall annot "full-auto")))))))

  :doc "Goal completes the subcommands it still dispatches"
  ;; The removed phase and approval commands were still advertised, and
  ;; `budget\' -- the only way out of a budget-limited Goal -- was not.
  ;; A word that is not a subcommand becomes objective text, so offering
  ;; one silently turns the objective into "approval" or "auto ...".
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("goal" . ignore))))
        (insert "/goal b")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("budget")
                         (niwa-skills-test--capf-candidates capf "b")))
          (let ((all (niwa-skills-test--capf-candidates capf)))
            (should-not (member "auto" all))
            (should-not (member "approval" all)))
          (should (string-match-p "token limit" (funcall annot "budget")))))))

  :doc "plugin command completes first argument options"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             '(("plugin" . niwa-plugins-slash-command))))
        (insert "/plugin l")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("list")
                         (niwa-skills-test--capf-candidates
                          capf "l")))
          (dolist (candidate '("enable" "disable" "hooks" "install"
                               "update" "reload"))
            (should (member candidate
                            (niwa-skills-test--capf-candidates capf))))
          (should (string-match-p "installed"
                                  (funcall annot "list")))
          (should (string-match-p "installed plugin"
                                  (funcall annot "update")))
          (should (string-match-p "current session"
                                  (funcall annot "reload")))))))

  :doc "plugin command completes installed plugin names for update"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "niwa-skills-plugin-capf-" t)))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-plugin-extra-roots nil)
         (session (niwa-skills-test--make-session)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands
                   '(("plugin" . niwa-plugins-slash-command))))
              (insert "/plugin update de")
              (goto-char (point-max))
              (let* ((capf (niwa-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (equal '("demo")
                               (niwa-skills-test--capf-candidates
                                capf "de")))
                (should (member "other"
                                (niwa-skills-test--capf-candidates capf)))
                (should (string-match-p "installed plugin"
                                        (funcall annot "demo")))))))
      (delete-directory user-dir t)))

  :doc "plugin command completes installed plugin names for state and removal commands"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "niwa-skills-plugin-capf-" t)))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-plugin-extra-roots nil)
         (session (niwa-skills-test--make-session)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands
                   '(("plugin" . niwa-plugins-slash-command))))
              (insert "/plugin enable ")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should capf)
                (should (member "demo"
                                (niwa-skills-test--capf-candidates capf)))
                (should (member "other"
                                (niwa-skills-test--capf-candidates capf))))
              (erase-buffer)
              (insert "/plugin disable o")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (niwa-skills-test--capf-candidates
                                capf "o"))))
              (erase-buffer)
              (insert "/plugin remove d")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should capf)
                (should (equal '("demo")
                               (niwa-skills-test--capf-candidates
                                capf "d"))))
              (erase-buffer)
              (insert "/plugin uninstall o")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (niwa-skills-test--capf-candidates
                                capf "o")))))))
      (delete-directory user-dir t)))

  :doc "plugin hooks command completes actions, plugin names, and states"
  (let* ((user-dir (file-name-as-directory
                    (make-temp-file "niwa-skills-plugin-capf-" t)))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-plugin-extra-roots nil)
         (session (niwa-skills-test--make-session)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-a" "{\"name\":\"demo\"}")
          (niwa-skills-test--write-plugin-manifest
           user-dir "repo-b" "{\"name\":\"other\"}")
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands
                   '(("plugin" . niwa-plugins-slash-command))))
              (insert "/plugin hooks ")
              (goto-char (point-max))
              (let* ((capf (niwa-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (member "enable"
                                (niwa-skills-test--capf-candidates capf)))
                (should (member "disable"
                                (niwa-skills-test--capf-candidates capf)))
                (should (member "demo"
                                (niwa-skills-test--capf-candidates capf)))
                (should (string-match-p "hook command"
                                        (funcall annot "enable")))
                (should (string-match-p "installed plugin"
                                        (funcall annot "demo"))))
              (erase-buffer)
              (insert "/plugin hooks enable o")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should capf)
                (should (equal '("other")
                               (niwa-skills-test--capf-candidates
                                capf "o"))))
              (erase-buffer)
              (insert "/plugin hooks demo ")
              (goto-char (point-max))
              (let* ((capf (niwa-slash-capf))
                     (annot (and capf (plist-get (nthcdr 3 capf)
                                                 :annotation-function))))
                (should capf)
                (should (member "on"
                                (niwa-skills-test--capf-candidates capf)))
                (should (member "off"
                                (niwa-skills-test--capf-candidates capf)))
                (should (string-match-p "hook state"
                                        (funcall annot "on")))))))
      (delete-directory user-dir t)))

  :doc "plugin install target remains freeform"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands
             '(("plugin" . niwa-plugins-slash-command))))
        (insert "/plugin install ")
        (goto-char (point-max))
        (should (null (niwa-slash-capf))))))

  :doc "skills command completes subcommands"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("skills" . ignore))))
        (insert "/skills dis")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("disable")
                         (niwa-skills-test--capf-candidates
                          capf "dis")))
          (should (member "enable"
                          (niwa-skills-test--capf-candidates capf)))
          (should (string-match-p "disable"
                                  (funcall annot "disable")))))))

  :doc "model command completes current backend model names"
  (niwa-skills-test--with-model-backends
    (let ((session (niwa-skills-test--make-session))
          (gptel-backend (gptel-get-backend "Fast"))
          (gptel-model 'manual-model))
      (niwa-skills-test--with-chat-buffer session
        (let ((niwa-slash-commands '(("model" . ignore))))
          (insert "/model f")
          (goto-char (point-max))
          (let ((capf (niwa-slash-capf)))
            (should capf)
            (should (equal '("fast-model")
                           (niwa-skills-test--capf-candidates
                            capf "f")))
            (should (member "manual-model"
                            (niwa-skills-test--capf-candidates
                             capf))))))))

  :doc "review and verify commands complete shared target arguments"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("review" . ignore)
                                      ("verify" . ignore))))
        (insert "/review cur")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("current")
                         (niwa-skills-test--capf-candidates capf "cur")))
          (should (member "commit:"
                          (niwa-skills-test--capf-candidates capf)))
          (should (string-match-p "current changes"
                                  (funcall annot "current"))))
        (erase-buffer)
        (insert "/verify com")
        (goto-char (point-max))
        (let ((capf (niwa-slash-capf)))
          (should capf)
          (should (equal '("commit:")
                         (niwa-skills-test--capf-candidates
                          capf "com")))))))

  :doc "worktree command completes status and create arguments"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands '(("worktree" . ignore))))
        (insert "/worktree c")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("create")
                         (niwa-skills-test--capf-candidates capf "c")))
          (should (member "status"
                          (niwa-skills-test--capf-candidates capf)))
          (should (equal " command" (funcall annot "create")))))))

  :doc "root completion inserts a real separator before ghost hints"
  (let* ((session (niwa-skills-test--make-session))
        (skill (niwa-skill--create
                :name "remember"
                :argument-names '("focus"))))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$rem")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (exit (and capf (plist-get (nthcdr 3 capf)
                                          :exit-function))))
          (delete-region (nth 0 capf) (nth 1 capf))
          (insert "remember")
          (funcall exit "remember" 'finished)
          (insert "d")
          (should (equal "$remember d"
                         (buffer-substring-no-properties
                          (point-min) (point-max))))))))

  :doc "returns nil when point is not right after a slash"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "hello world")
      (goto-char (point-max))
      (should (null (niwa-slash-capf)))))

  :doc "returns nil when no session is present"
  (with-temp-buffer
    (setq niwa--session nil)
    (insert "/hel")
    (goto-char (point-max))
    (should (null (niwa-slash-capf))))

  :doc "user-invocable: false skills are omitted from completion"
  ;; User-invocable false skills are not shown in `$' completion.
  (let* ((session (niwa-skills-test--make-session))
         (visible (niwa-skill--create :name "visible"))
         (hidden (niwa-skill--create :name "hidden"
                                        :user-invocable-p nil)))
    (setf (niwa-session-skills session) (list visible hidden))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (cands (and capf
                           (niwa-skills-test--capf-candidates capf))))
          (should (member "visible" cands))
          (should-not (member "hidden" cands))))))

  :doc "disabled skills are omitted from completion"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (visible (niwa-skill--create :name "visible"))
         (hidden (niwa-skills-test--stateful-skill
                  :name "hidden"
                  :workspace (niwa-session-workspace session))))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list visible hidden))
          (niwa-skills-set-enabled hidden nil)
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands nil))
              (insert "$")
              (goto-char (point-max))
              (let* ((capf (niwa-slash-capf))
                     (cands (and capf
                                 (niwa-skills-test--capf-candidates capf))))
                (should (member "visible" cands))
                (should-not (member "hidden" cands))))))
      (delete-directory user-dir t)))

  :doc "[dormant] annotation appears for path-scoped not-yet-active skills"
  ;; Dormant skills get an annotation.
  (let* ((session (niwa-skills-test--make-session))
         (active (niwa-skill--create :name "ready"))
         (dormant (niwa-skill--create
                   :name "lazy"
                   :path-patterns '("*.el")
                   :active-p nil)))
    (setf (niwa-session-skills session) (list active dormant))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should (equal " [skill]" (funcall annot "ready")))
          (should (equal " [skill] [dormant]" (funcall annot "lazy")))))))

  :doc "argument hints appear after the [skill] annotation"
  ;; Annotation includes argument hint from `argument-hint' or generated
  ;; from `arguments'.
  (let* ((session (niwa-skills-test--make-session))
         (with-hint (niwa-skill--create :name "find"
                                           :argument-hint "[query]"))
         (with-args (niwa-skill--create :name "review"
                                           :argument-names '("path" "depth"))))
    (setf (niwa-session-skills session) (list with-hint with-args))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$")
        (goto-char (point-max))
        (let* ((capf (niwa-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should (equal " [skill] [query]" (funcall annot "find")))
          (should (equal " [skill] [path] [depth]"
                         (funcall annot "review")))))))

  :doc "candidate table refreshes after external create and delete"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skills-capf-hot-" t))
         (niwa-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (niwa-skill-dirs (list root))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (buf (generate-new-buffer " *niwa-test-capf-hot*")))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (progn
              (setq-local niwa--session session)
              (niwa-skills-install session buf)
              (insert "$")
              (goto-char (point-max))
              (let ((capf (niwa-slash-capf)))
                (should (member "alpha"
                                (niwa-skills-test--capf-candidates
                                 capf)))
                (niwa-skills-test--write-skill
                 root "bar" "name: bar\ndescription: B\n")
                (niwa-skills--mark-buffer-dirty buf)
                (should (member "bar"
                                (niwa-skills-test--capf-candidates
                                 capf "b")))
                (delete-directory (file-name-concat root "bar") t)
                (niwa-skills--mark-buffer-dirty buf)
                (should-not (member "bar"
                                    (niwa-skills-test--capf-candidates
                                     capf "b")))))))
      (when (buffer-live-p buf)
        (niwa-skills--unregister-buffer buf)
        (kill-buffer buf))
      (delete-directory root t))))

(niwa-deftest niwa-skills--progressive-argument-hint ()
  ,test
  (test)
  :doc "argument-hint string takes precedence"
  (let ((skill (niwa-skill--create
                :name "x"
                :argument-hint "[query]"
                :argument-names '("a" "b"))))
    (should (equal "[query]"
                   (niwa-skills--progressive-argument-hint skill))))

  :doc "argument-names produces bracketed sequence when no hint set"
  (let ((skill (niwa-skill--create
                :name "x"
                :argument-names '("alpha" "beta" "gamma"))))
    (should (equal "[alpha] [beta] [gamma]"
                   (niwa-skills--progressive-argument-hint skill))))

  :doc "no hint and no names returns nil"
  (let ((skill (niwa-skill--create :name "x")))
    (should (null (niwa-skills--progressive-argument-hint skill)))))

(niwa-deftest niwa-skills-remaining-argument-hint ()
  ,test
  (test)
  :doc "argument-hint appears until arguments are typed"
  (let ((skill (niwa-skill--create
                :name "x"
                :argument-hint "What should be reviewed?")))
    (should (equal "What should be reviewed?"
                   (niwa-skills-remaining-argument-hint skill "")))
    (should (equal "What should be reviewed?"
                   (niwa-skills-remaining-argument-hint skill nil)))
    (should (null (niwa-skills-remaining-argument-hint
                   skill "current changes"))))

  :doc "argument names display only remaining slots"
  (let ((skill (niwa-skill--create
                :name "x"
                :argument-names '("service" "environment" "region"))))
    (should (equal "[service] [environment] [region]"
                   (niwa-skills-remaining-argument-hint skill "")))
    (should (equal "[environment] [region]"
                   (niwa-skills-remaining-argument-hint
                    skill "billing")))
    (should (equal "[region]"
                   (niwa-skills-remaining-argument-hint
                    skill "\"billing api\" staging")))
    (should (null (niwa-skills-remaining-argument-hint
                   skill "billing staging us")))))

(niwa-deftest niwa-cmd--model ()
  ,test
  (test)
  :doc "with args sets buffer-local gptel-model to interned symbol"
  (with-temp-buffer
    (setq-local gptel-model 'default)
    (niwa-test--with-captured-messages nil
      (niwa-cmd--model "claude-opus-4-6"))
    (should (eq 'claude-opus-4-6 gptel-model)))

  :doc "with unresolved colon args preserves direct bare model behavior"
  (with-temp-buffer
    (setq-local gptel-model 'default)
    (niwa-test--with-captured-messages nil
      (niwa-cmd--model "llama3.1:8b"))
    (should (eq 'llama3.1:8b gptel-model)))

  :doc "with provider args sets buffer-local backend and model"
  (niwa-skills-test--with-model-backends
    (with-temp-buffer
      (let ((niwa--session
             (niwa-session--create :name "main")))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--model "Fast:fast-model"))
        (should (equal "Fast" (gptel-backend-name gptel-backend)))
        (should (eq 'fast-model gptel-model))
        (should (equal "Fast:fast-model"
                       (niwa-session-model-provider
                        niwa--session))))))

  :doc "with blank args does not change the model"
  (with-temp-buffer
    (let ((gptel-model 'keep))
      (cl-letf (((symbol-function 'niwa-menu-open)
                 (lambda (_area) (user-error "No cockpit"))))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--model "")))
      (should (eq 'keep gptel-model)))))

(niwa-deftest niwa-cmd--mode ()
  ,test
  (test)
  :doc "sets a recognized permission mode via setopt; updates session slot"
  (let ((saved (default-toplevel-value 'niwa-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (setq-local niwa--session session)
            (niwa-test--with-captured-messages nil
              (niwa-cmd--mode "edits"))
            (should (eq 'edits
                        (niwa-session-permission-mode session)))
            (should (eq 'edits niwa-permission-mode))))
      (set-default-toplevel-value 'niwa-permission-mode saved)))

  :doc "rejects superseded UI names"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :name "test" :permission-mode 'ask)))
      (setq-local niwa--session session)
      (dolist (mode '("auto" "edit" "accept-edits" "trust-all"))
        (should-error (niwa-cmd--mode mode) :type 'user-error))
      (should (eq 'ask (niwa-session-permission-mode session)))
      (niwa-test--with-captured-messages nil
        (niwa-cmd--mode "full-auto"))
      (should (eq 'full-auto
                  (niwa-session-permission-mode session)))
      (should (memq 'mode-constraints
                    (mapcar #'niwa-reminder-type
                            (niwa-session-reminders session))))))

  :doc "leaving full-auto via /mode keeps one effective-mode advisory"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :name "test" :permission-mode 'full-auto)))
      (setf (niwa-session-reminders session)
            (list (niwa-reminders-make-mode-constraints)))
      (setq-local niwa--session session)
      (niwa-test--with-captured-messages nil
        (niwa-cmd--mode "ask"))
      (let ((types (mapcar #'niwa-reminder-type
                           (niwa-session-reminders session))))
        (should (eq 'ask
                    (niwa-session-permission-mode session)))
        (should (equal '(mode-constraints) types)))))

  :doc "rejects unknown modes"
  (with-temp-buffer
    (setq-local niwa-permission-mode 'ask)
    (should-error (niwa-cmd--mode "bogus") :type 'user-error)
    (should (eq 'ask niwa-permission-mode)))

  :doc "blank args leaves the mode unchanged"
  (with-temp-buffer
    (setq-local niwa-permission-mode 'edits)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (_area) (user-error "No cockpit"))))
      (niwa-test--with-captured-messages nil
        (niwa-cmd--mode "")))
    (should (eq 'edits niwa-permission-mode))))

(niwa-deftest niwa-cmd--goal ()
  ,test
  (test)
  :doc "starts a Goal from a nonblank objective"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "main"))
    (let (started)
      (cl-letf (((symbol-function 'niwa-goal-start)
                 (lambda (objective) (setq started objective))))
        (should (eq 'niwa-view-sent (niwa-cmd--goal "Fix it"))))
      (should (equal "Fix it" started))))
  :doc "treats former subcommand words as ordinary objective text"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :name "main" :permission-mode 'edits))
          started)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-goal-start)
                 (lambda (objective) (setq started objective))))
        (should (eq 'niwa-view-sent
                    (niwa-cmd--goal "auto Ship safely"))))
      (should (equal "auto Ship safely" started))
      (should (eq 'edits
                  (niwa-session-permission-mode session)))))
  :doc "bare command opens the Goal cockpit with a current Goal"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create
                  :objective "Fix it" :status 'active))
           (session (niwa-session--create :name "main" :goal goal))
           opened)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-menu-open)
                 (lambda (area) (setq opened area))))
        (niwa-cmd--goal nil))
      (should (eq 'goal opened))))
  :doc "bare command opens Goal creation cockpit when no Goal exists"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "main"))
    (let (opened)
      (cl-letf (((symbol-function 'niwa-menu-open)
                 (lambda (area) (setq opened area))))
        (niwa-cmd--goal nil))
      (should (eq 'goal opened))))
  :doc "dispatches budget, edit, pause, resume with steering, and clear"
  (with-temp-buffer
    (let ((goal (niwa-goal--create
                 :id "g1" :objective "Old" :status 'paused))
          calls)
      (setq-local niwa--session
                  (niwa-session--create :name "main" :goal goal))
      (cl-letf (((symbol-function 'niwa-goal-pause)
                 (lambda () (push '(pause) calls)))
                ((symbol-function 'niwa-goal-edit)
                 (lambda (value) (push (list 'edit value) calls)))
                ((symbol-function 'niwa-goal-set-budget)
                 (lambda (value) (push (list 'budget value) calls)))
                ((symbol-function 'niwa-goal-resume)
                 (lambda (value) (push (list 'resume value) calls)))
                ((symbol-function 'niwa-goal-clear)
                 (lambda () (push '(clear) calls))))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--goal "budget 5000"))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--goal "edit New objective"))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--goal "pause"))
        (niwa-test--with-captured-messages nil
          (niwa-cmd--goal "resume new evidence"))
        (niwa-cmd--goal "clear"))
      (should (equal '((clear) (resume "new evidence") (pause)
                       (edit "New objective") (budget "5000"))
                     calls))))

  :doc "dispatches every subcommand it advertises"
  ;; A word that is not a subcommand becomes objective text, so an
  ;; advertised word the dispatcher forgot silently starts a Goal named
  ;; after it.  That is exactly how `auto' and `approval' survived their
  ;; own removal.
  (with-temp-buffer
    (let ((goal (niwa-goal--create
                 :id "g1" :objective "Old" :status 'paused))
          started)
      (setq-local niwa--session
                  (niwa-session--create :name "main" :goal goal))
      (cl-letf (((symbol-function 'niwa-goal-pause) #'ignore)
                ((symbol-function 'niwa-goal-edit) #'ignore)
                ((symbol-function 'niwa-goal-set-budget) #'ignore)
                ((symbol-function 'niwa-goal-resume) #'ignore)
                ((symbol-function 'niwa-goal-clear) #'ignore)
                ((symbol-function 'niwa-goal-start)
                 (lambda (objective) (push objective started))))
        (dolist (candidate niwa-skills--goal-command-candidates)
          (niwa-test--with-captured-messages nil
            (niwa-cmd--goal (car candidate)))))
      (should-not started))))


(niwa-deftest niwa-cmd--plan ()
  ,test
  (test)
  :doc "enters Plan without changing the session permission policy"
  (with-temp-buffer
    ;; Entering Plan admits a new mutation, which resolves the session's
    ;; durable authority from its workspace category.
    (let ((session (niwa-session--create
                    :name "main" :permission-mode 'edits
                    :workspace (niwa-skills-test--make-workspace
                                "/tmp/niwa-plan-mode"))))
      (setq-local niwa--session session)
      (niwa-test--with-captured-messages nil
        (niwa-cmd--plan nil))
      (should (niwa-session-plan-mode session))
      (should (eq 'edits (niwa-session-permission-mode session)))))

  :doc "rejects arguments without changing Plan state"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "main")))
      (setq-local niwa--session session)
      (should-error (niwa-cmd--plan "later") :type 'user-error)
      (should-not (niwa-session-plan-mode session)))))

(niwa-deftest niwa-cmd--edits ()
  ,test
  (test)
  :doc "toggles edits on without authorizing full-auto execution"
  (let ((saved (default-toplevel-value 'niwa-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'ask)))
            (setq-local niwa--session session)
            (niwa-test--with-captured-messages nil
              (niwa-cmd--edits nil))
            (should (eq 'edits
                        (niwa-session-permission-mode session)))
            (should-not
             (memq 'full-auto-mode
                   (mapcar #'niwa-reminder-type
                           (niwa-session-reminders session))))))
      (set-default-toplevel-value 'niwa-permission-mode saved)))

  :doc "toggles edits off to ask"
  (let ((saved (default-toplevel-value 'niwa-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'edits)))
            (setq-local niwa--session session)
            (niwa-test--with-captured-messages nil
              (niwa-cmd--edits nil))
            (should (eq 'ask
                        (niwa-session-permission-mode session)))
            (should (equal '(mode-constraints)
                           (mapcar #'niwa-reminder-type
                                   (niwa-session-reminders session))))))
      (set-default-toplevel-value 'niwa-permission-mode saved)))

  :doc "switches full-auto to edits and records the boundary change"
  (let ((saved (default-toplevel-value 'niwa-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (niwa-session--create
                          :name "test" :permission-mode 'full-auto)))
            (setf (niwa-session-reminders session)
                  (list (niwa-reminders-make-mode-constraints)))
            (setq-local niwa--session session)
            (niwa-test--with-captured-messages nil
              (niwa-cmd--edits nil))
            (should (eq 'edits
                        (niwa-session-permission-mode session)))
            (let ((types (mapcar #'niwa-reminder-type
                                 (niwa-session-reminders session))))
              (should (equal '(mode-constraints) types)))))
      (set-default-toplevel-value 'niwa-permission-mode saved))))



(niwa-deftest niwa-skills-message-command-result ()
  ,test
  (test)
  :doc "projects plugin failures locally and preserves other command results"
  (progn
  (dolist (name '("plugin" "other"))
    (let ((raw "Failed to update plugin x: rejected") echoed)
      (niwa-test--with-captured-messages echoed
        (niwa-skills-message-command-result "/owner/" name raw))
      (should (equal (concat (if (equal name "plugin") "niwa [/owner/]: " "")
                             raw "\n") echoed))))
  (let (echoed)
    (niwa-test--with-captured-messages echoed
      (niwa-skills-message-command-result "/owner/" "plugin" nil))
    (should (string-empty-p echoed)))))

(provide 'test-niwa-skills-ui)

;;; test-niwa-skills-ui.el ends here
