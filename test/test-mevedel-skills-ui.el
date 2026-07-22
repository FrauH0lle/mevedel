;;; test-mevedel-skills-ui.el --- Skill command and editor tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests local skill commands, editor support, and send dispatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-cockpit)
(require 'mevedel-execution)
(require 'mevedel-executions-list)
(require 'mevedel-file-state)
(require 'mevedel-goal)
(require 'mevedel-hooks)
(require 'mevedel-models)
(require 'mevedel-mention-bindings)
(require 'mevedel-mentions)
(require 'mevedel-permissions)
(require 'mevedel-persistence)
(require 'mevedel-plugins)
(require 'mevedel-reminders)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-worktree)
(require 'tabulated-list)

(defvar mevedel-plugin-extra-roots)
(declare-function mevedel-session-persistence--sidecar-path
                  "mevedel-session-persistence" (save-path))
(declare-function mevedel-session-persistence-read
                  "mevedel-session-persistence" (file))


;;
;;; Module ownership

(mevedel-deftest mevedel-skills-ui-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-cmd--auto
                    mevedel-cmd--mode
                    mevedel-cmd--model
                    mevedel-cmd--ps
                    mevedel-cmd--skills
                    mevedel-cmd--stop
                    mevedel-cmd--tools
                    mevedel-skills--dispatch-slash-command
                    mevedel-skills--fontify-dollar-keyword
                    mevedel-skills--gptel-send-advice
                    mevedel-skills--parse-slash-line
                    mevedel-skills--slash-capf
                    mevedel-skills-count-label
                    mevedel-skills-local-command-active-request-p
                    mevedel-skills-install-font-lock
                    mevedel-skills-install-slash-commands
                    mevedel-skills-list-open
                    mevedel-skills-uninstall-slash-commands
                    mevedel-slash-capf))
    (should (equal "mevedel-skills-ui"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Slash commands and completion

(defun mevedel-skills-test--open-list (session view-buffer data-buffer)
  "Open a skills cockpit for SESSION owned by VIEW-BUFFER and DATA-BUFFER."
  (with-current-buffer data-buffer
    (setq-local mevedel--session session)
    (setq-local mevedel--view-buffer view-buffer)
    (mevedel-skills-list-open
     (list :view-buffer view-buffer
           :data-buffer data-buffer
           :origin-buffer data-buffer
           :session session
           :workspace (mevedel-session-workspace session)))))

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

  :doc "colon-qualified names parse as commands"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (mevedel-skills--parse-slash-line
                  "/superpowers:brainstorming now")))

  :doc "additional lines after the command are appended to ARGS"
  (should (equal '("delegate"
                   "Launch three explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (mevedel-skills--parse-slash-line
                  "/delegate Launch three explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("delegate"
                   "Multi-line task body\nspanning lines"
                   0)
                 (mevedel-skills--parse-slash-line
                  "/delegate
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

(mevedel-deftest mevedel-skills--fontify-dollar-keyword ()
  ,test
  (test)
  :doc "font-lock shares root, inline, quoting, and code-span token rules"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "A")))
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (insert "$alpha root\nUse `$alpha`, \"$alpha\", and $alpha.")
      (goto-char (point-min))
      (should (mevedel-skills--fontify-dollar-keyword (point-max)))
      (should (equal "$alpha" (match-string-no-properties 0)))
      (should (mevedel-skills--fontify-dollar-keyword (point-max)))
      (should (equal "$alpha" (match-string-no-properties 0)))
      (should (string-prefix-p "and "
                               (buffer-substring-no-properties
                                (- (match-beginning 0) 4)
                                (match-beginning 0))))
      (should-not (mevedel-skills--fontify-dollar-keyword (point-max))))))

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
        (insert "### /mode auto")
        (goto-char (point-max))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should-not called)
        (should (eq 'auto
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

  :doc "worktree slash commands open status and list surfaces"
  (let ((session (mevedel-skills-test--make-session))
        status-buffer
        list-buffer)
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands
             (cons '("worktree" . mevedel-cmd--worktree)
                   mevedel-slash-commands)))
        (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                   (lambda () (setq status-buffer (current-buffer))))
                  ((symbol-function 'mevedel-worktree-list-open)
                   (lambda () (setq list-buffer (current-buffer)))))
          (dolist (command '("/worktree" "/worktree status"))
            (setq status-buffer nil
                  list-buffer nil)
            (erase-buffer)
            (insert "### " command)
            (goto-char (point-max))
            (should (eq 'local (mevedel-skills--dispatch-slash-command)))
            (should (eq status-buffer (current-buffer)))
            (should-not list-buffer)
            (should (equal "### " (buffer-string))))
          (erase-buffer)
          (insert "### /worktree list")
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (eq list-buffer (current-buffer)))
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
         (skill (mevedel-skills-test--stateful-skill :name "visible")))
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

  :doc "slash syntax no longer invokes same-named skills"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### /greet world")
        (goto-char (point-max))
        (should (eq 'unknown (mevedel-skills--dispatch-slash-command)))
        (should (equal "### /greet world" (buffer-string)))))))

(mevedel-deftest mevedel-skills--dispatch-slash-command/layout ()
  ,test
  (test)
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
      (let ((mevedel-slash-commands mevedel-slash-commands))
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
  (let ((session (mevedel-skills-test--make-session))
        start-source)
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands mevedel-slash-commands))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'mevedel--run-session-start-hooks)
                   (lambda (source)
                     (setq start-source source)
                     (mevedel-hooks-record-session-context
                      session '(:additional-context ("clear context"))
                      'SessionStart))))
          (insert "Existing transcript\n### /clear")
          (goto-char (point-max))
          (should (eq 'local (mevedel-skills--dispatch-slash-command)))
          (should (equal "### " (buffer-string)))
          (should (equal "clear" start-source))
          (should (mevedel-session-hook-context-pending session))))))

  :doc "`/clear' falls back with confirmation in a rewind preview buffer"
  (let* ((session (mevedel-skills-test--make-session))
         (tempdir (make-temp-file "mevedel-clear-preview-test-" t))
         (save-path (file-name-as-directory tempdir)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) save-path)
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands mevedel-slash-commands)
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
         (seg1 (file-name-concat save-path "segment-0001.chat.org"))
         start-source)
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
                  (mevedel-slash-commands mevedel-slash-commands))
              (setq mevedel--session session)
              (setq buffer-file-name seg1)
              (cl-letf (((symbol-function
                          'mevedel-session-persistence--save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'mevedel-version)
                         (lambda (&rest _args) "test-version"))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         (lambda (source)
                           (setq start-source source))))
                (insert "### /clear")
                (goto-char (point-max))
                (should (eq 'local (mevedel-skills--dispatch-slash-command)))
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))
                (should (string-suffix-p "### " (buffer-string)))
                (should (equal "clear" start-source))
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
                  (mevedel-slash-commands mevedel-slash-commands))
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
        (delete-directory tempdir t))))

  :doc "`/clear' refreshes stale visited metadata before direct prefix trim"
  (let* ((session (mevedel-skills-test--make-session))
         (tempdir (make-temp-file "mevedel-clear-direct-stale-test-" t))
         (save-path (file-name-as-directory tempdir))
         (seg1 (file-name-concat save-path "segment-0001.chat.org")))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) save-path)
          (setf (mevedel-session-session-id session) "clear-direct-stale-test")
          (setf (mevedel-session-current-segment session) 1)
          (setf (mevedel-session-created-at session) "2026-05-08T10-00-00")
          (setf (mevedel-session-updated-at session) "2026-05-08T10-00-00")
          (with-temp-buffer
            (org-mode)
            (let ((gptel-prompt-prefix-alist
                   (cons (cons major-mode "### ")
                         gptel-prompt-prefix-alist)))
              (setq mevedel--session session)
              (setq buffer-file-name seg1)
              (insert "Completed turn\n### ")
              (write-region (point-min) (point-max) seg1 nil 'silent)
              (set-visited-file-modtime)
              (set-buffer-modified-p nil)
              (set-file-times seg1 (time-add (current-time) 5))
              (should-not (verify-visited-file-modtime (current-buffer)))
              (cl-letf (((symbol-function
                          'mevedel-session-persistence--save-instructions)
                         (lambda (&rest _args) nil))
                        ((symbol-function 'mevedel-version)
                         (lambda (&rest _args) "test-version"))
                        ((symbol-function 'ask-user-about-supersession-threat)
                         (lambda (&rest _args)
                           (error "Supersession prompt"))))
                (mevedel-cmd--clear nil)
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal (file-name-concat
                                save-path "segment-0002.chat.org")
                               buffer-file-name))))))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t)))))

(mevedel-deftest mevedel-cmd--skills ()
  ,test
  (test)
  :doc "list routes through the skills surface and help remains direct"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skills-test--stateful-skill
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

  :doc "enable and disable reject unknown skills"
  (let ((session (mevedel-skills-test--make-session)))
    (with-temp-buffer
      (setq mevedel--session session)
      (should-error (mevedel-cmd--skills "disable missing")
                    :type 'user-error)
      (should-error (mevedel-cmd--skills "enable missing")
                    :type 'user-error))))

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

(mevedel-deftest mevedel-cmd--ps ()
  ,test
  (test)
  :doc "blank arguments open the execution cockpit and others show usage"
  (let (opened message-text)
    (cl-letf (((symbol-function 'mevedel-executions-list-open)
               (lambda (&optional _context) (setq opened t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-text (apply #'format format-string args)))))
      (mevedel-cmd--ps nil)
      (should opened)
      (mevedel-cmd--ps "extra")
      (should (equal "Usage: /ps" message-text)))))

(mevedel-deftest mevedel-cmd--stop ()
  ,test
  (test)
  :doc "bare /stop opens the execution cockpit"
  (let (opened)
    (cl-letf (((symbol-function 'mevedel-executions-list-open)
               (lambda (&optional _context) (setq opened t))))
      (mevedel-cmd--stop nil))
    (should opened))
  :doc "/stop ID uses session-wide user authority"
  (let ((session (mevedel-skills-test--make-session)) stopped)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-execution-stop-user)
                 (lambda (seen-session id)
                   (setq stopped (list seen-session id)))))
        (mevedel-cmd--stop " exec-000009 ")))
    (should (equal (list session "exec-000009") stopped))))

(mevedel-deftest mevedel-skills-local-command-active-request-p ()
  ,test
  (test)
  :doc "allows process controls and only the safe mid-request goal actions"
  (dolist (command '(("ps" nil) ("stop" "exec-000001")
                     ("goal" "pause") ("goal" "edit new objective")))
    (should (apply #'mevedel-skills-local-command-active-request-p command)))
  (dolist (command '(("goal" "resume") ("tools" nil) ("stopper" nil)))
    (should-not
     (apply #'mevedel-skills-local-command-active-request-p command))))

(mevedel-deftest mevedel-skills-count-label
  (:vars* ((user-dir (make-temp-file "mevedel-skills-count-" t))
           (mevedel-user-dir (file-name-as-directory user-dir)))
   :after-each (delete-directory user-dir t))
  ,test
  (test)

  :doc "returns enabled/total skills for a session"
  (let* ((session (mevedel-skills-test--make-session))
         (enabled (mevedel-skill--create :name "enabled"))
         (disabled (mevedel-skills-test--stateful-skill :name "disabled")))
    (setf (mevedel-session-skills session) (list enabled disabled))
    (mevedel-skills--set-enabled disabled nil)
    (should (equal "1/2" (mevedel-skills-count-label session))))

  :doc "returns 0/0 without a session"
  (should (equal "0/0" (mevedel-skills-count-label nil))))

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
                  :source 'project :source-file source-file))
         (disabled (mevedel-skills-test--stateful-skill
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
              (should (string-match-p
                       "2/3 enabled"
                       (mevedel-cockpit-surface-header-line)))
              (should (= 3 (length tabulated-list-entries)))
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc "active" rows))
                               '("enabled" "active" "project"
                                 "Active description")))
                (should (equal (cdr (assoc "disabled" rows))
                               '("disabled" "disabled" "user"
                                 "Disabled description")))
                (should (equal (cdr (assoc "plugin:assist" rows))
                               '("enabled" "plugin:assist" "plugin"
                                 "Plugin description"))))
              (mevedel-cockpit-goto-id "active")
              (mevedel-skills-list-details))
            (with-current-buffer "*mevedel skill details*"
              (should (string-match-p "Active description"
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
  :doc "refresh updates visible skill row content"
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
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc "second" rows))
                               '("enabled" "second" "project"
                                 "Updated")))))))
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
         (skill (mevedel-skills-test--stateful-skill :name "visible")))
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
         (skill (mevedel-skills-test--stateful-skill
                 :name "visible"
                 :description "Visible description"
                 :source 'plugin))
         (entry (unwind-protect
                    (progn
                      (mevedel-skills--set-enabled skill nil)
                      (mevedel-skills-list--entry skill))
                  (delete-directory user-dir t))))
    (should (equal (car entry) "visible"))
    (should (equal (mevedel-test-tabulated-row-cells entry)
                   '("disabled" "visible" "plugin"
                     "Visible description")))))

(mevedel-deftest mevedel-skills--skill-detail-text ()
  ,test
  (test)
  :doc "shows stored skill warnings in inspection details"
  (let* ((warning
          "Agent explorer is ignored for inline skills; use context: fork to select an agent")
         (skill (mevedel-skill--create
                 :name "inspect" :description "Inspectable"
                 :source 'project :warnings (list warning)))
         (details (mevedel-skills--skill-detail-text skill)))
    (should (string-match-p "Warnings:" details))
    (should (string-match-p (regexp-quote (concat "- " warning)) details)))

  :doc "omits the warnings section when discovery found none"
  (let* ((skill (mevedel-skill--create
                 :name "clean" :description "Clean"
                 :source 'project))
         (details (mevedel-skills--skill-detail-text skill)))
    (should-not (string-match-p "Warnings:" details))))

(mevedel-deftest mevedel-skills-list--session-label ()
  ,test
  (test)
  :doc "returns the rendered session name or unknown"
  (let ((session (mevedel-skills-test--make-session "named"))
        (view-buffer (generate-new-buffer " *skills-label-view*"))
        (data-buffer (generate-new-buffer " *skills-label-data*")))
    (with-temp-buffer
      (mevedel-skills-list-mode)
      (should (equal (mevedel-skills-list--session-label) "unknown")))
    (unwind-protect
        (let ((buffer (mevedel-skills-test--open-list
                       session view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (equal (mevedel-skills-list--session-label
                            (mevedel-cockpit-surface-context))
                           "named"))))
      (mevedel-skills-test--cleanup-list view-buffer data-buffer))))

(mevedel-deftest mevedel-skills-list--header-line ()
  ,test
  (test)
  :doc "summarizes the session skill count and key hints"
  (let* ((user-dir (make-temp-file "mevedel-skills-header-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (enabled (mevedel-skill--create :name "enabled"))
         (disabled (mevedel-skills-test--stateful-skill :name "disabled")))
    (unwind-protect
        (with-temp-buffer
          (mevedel-skills-list-mode)
          (mevedel-skills--set-enabled disabled nil)
          (let ((line (mevedel-skills-list--header-line
                       (list enabled disabled)
                       nil)))
            (should (string-match-p "1/2 enabled" line))
            (should (string-match-p "RET details" line))
            (should (string-match-p "q back" line))))
      (delete-directory user-dir t))))

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
         (skill (mevedel-skills-test--stateful-skill
                 :name "visible" :source 'project))
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
                (should (equal (cdr (assoc
                                     "visible"
                                     (mevedel-test-tabulated-entries-cells)))
                               '("disabled" "visible" "project" "")))
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
          (should (string-match-p "e    Enable or disable selected skill"
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

  :doc "expanded `$' skill lets the send proceed"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "hi" :body "Hi!"))
         (orig-called nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $hi")
        (goto-char (point-max))
        (mevedel-skills--gptel-send-advice
         (lambda (&rest _) (setq orig-called t)))
        (should orig-called))))

  :doc "unknown dollar-prefixed text still sends normally"
  (let ((session (mevedel-skills-test--make-session))
        (orig-called nil))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $PATH can be mentioned")
      (goto-char (point-max))
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called)))

  :doc "plain text always lets the send proceed"
  (let ((session (mevedel-skills-test--make-session))
        (orig-called nil))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### just a normal message")
      (goto-char (point-max))
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called)))

  :doc "unavailable leading skills annotate and let the send proceed"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "alpha" :body "ignored" :user-invocable-p nil))
         orig-called
         staged)
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $alpha")
      (goto-char (point-max))
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _)
         (setq orig-called t
               staged
               (copy-tree mevedel-skills--pending-inline-attachments))))
      (should orig-called)
      (should (plist-get (car staged) :unavailable))))

  :doc "an unavailable leading skill does not suppress later attachments"
  (let* ((session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create
                 :name "alpha" :body "ignored" :user-invocable-p nil))
         (beta (mevedel-skill--create :name "beta" :body "BETA BODY"))
         orig-called
         staged)
    (setf (mevedel-session-skills session) (list alpha beta))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $alpha then use $beta")
      (goto-char (point-max))
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _)
         (setq orig-called t
               staged
               (copy-tree mevedel-skills--pending-inline-attachments))))
      (should orig-called)
      (should (= 2 (length staged)))
      (should (plist-get (car staged) :unavailable))
      (should (equal "BETA BODY" (plist-get (cadr staged) :body)))))

  :doc "manually typed raw skills bind their canonical source before send"
  (let* ((session (mevedel-skills-test--make-session))
         (source (make-temp-file "mevedel-raw-skill-" nil ".md"))
         (skill (mevedel-skill--create
                 :name "alpha" :body "ALPHA" :source-file source))
         binding)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-skills-test--with-chat-buffer session
            (insert "### use $alpha")
            (goto-char (point-max))
            (mevedel-skills--gptel-send-advice
             (lambda (&rest _)
               (save-excursion
                 (goto-char (point-min))
                 (search-forward "$alpha")
                 (setq binding
                       (get-text-property
                        (match-beginning 0)
                        'mevedel-mention-binding)))))
            (should (equal source (plist-get binding :source-file)))))
      (delete-file source)))

  :doc "manually typed raw direct references bind their UUID before send"
  (let* ((root (make-temp-file "mevedel-raw-ref-" t))
         (file (file-name-concat root "reference.txt"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "raw-ref"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         ref-buffer ref binding)
    (unwind-protect
        (progn
          (with-temp-file file (insert "reference body\n"))
          (setq ref-buffer (find-file-noselect file))
          (with-current-buffer ref-buffer
            (setq-local mevedel--workspace ws)
            (setq ref (mevedel--create-reference-in
                       ref-buffer (point-min) (1- (point-max)))))
          (let ((token (format "@ref:%d" (mevedel--instruction-id ref))))
            (mevedel-skills-test--with-chat-buffer session
              (insert "### inspect " token)
              (goto-char (point-max))
              (mevedel-skills--gptel-send-advice
               (lambda (&rest _)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward token)
                   (setq binding
                         (get-text-property
                          (match-beginning 0)
                          'mevedel-mention-binding))))))
            (should (equal (overlay-get ref 'mevedel-uuid)
                           (plist-get binding :reference-uuid)))))
      (when (buffer-live-p ref-buffer)
        (with-current-buffer ref-buffer (set-buffer-modified-p nil))
        (kill-buffer ref-buffer))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "malformed bindings block raw gptel submission"
  (let ((session (mevedel-skills-test--make-session))
        orig-called)
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $alpha")
      (with-silent-modifications
        (put-text-property
         (- (point) 6) (point) 'mevedel-mention-binding
         '(:kind skill :token "$alpha")))
      (should-error
       (mevedel-skills--gptel-send-advice
        (lambda (&rest _) (setq orig-called t)))
       :type 'user-error)
      (should-not orig-called)))

  :doc "no session: advice still calls orig-fn"
  (let ((orig-called nil))
    (with-temp-buffer
      (setq mevedel--session nil)
      (mevedel-skills--gptel-send-advice
       (lambda (&rest _) (setq orig-called t)))
      (should orig-called)))

  :doc "paired view buffers bypass dollar rescanning of planned prompts"
  (let ((session (mevedel-skills-test--make-session))
        (view-buffer (generate-new-buffer " *mevedel-planned-view*"))
        orig-called
        dispatched)
    (unwind-protect
        (mevedel-skills-test--with-chat-buffer session
          (setq-local mevedel--view-buffer view-buffer)
          (insert "### prepared body says $nested")
          (cl-letf (((symbol-function 'mevedel-skills--dispatch-skill-command)
                     (lambda (&optional _) (setq dispatched t))))
            (mevedel-skills--gptel-send-advice
             (lambda (&rest _) (setq orig-called t))))
          (should orig-called)
          (should-not dispatched))
      (kill-buffer view-buffer)))

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
      (setq-local mevedel-skills--pending-inline-attachments
                  (list (list :name "alpha")))
      (mevedel-skills--gptel-send-advice (lambda (&rest _) nil))
      (should (null mevedel-skills--pending-request-context))
      (should (null mevedel-skills--pending-inline-attachments))))

  :doc "stash leaks cleared even when orig-fn signals an error"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### plain text")
      (goto-char (point-max))
      (setq-local mevedel-skills--pending-request-context
                  '(:permission-rules nil :model haiku))
      (setq-local mevedel-skills--pending-inline-attachments
                  (list (list :name "alpha")))
      (ignore-errors
        (mevedel-skills--gptel-send-advice
         (lambda (&rest _) (error "Boom"))))
      (should (null mevedel-skills--pending-request-context))
      (should (null mevedel-skills--pending-inline-attachments)))))

(mevedel-deftest mevedel-slash-capf ()
  ,test
  (test)
  :doc "global commands expose the Goal and Plan entry points"
  (let ((session (mevedel-skills-test--make-session)))
    (should (eq 'mevedel-plugins-slash-command
                (cdr (assoc "plugin" mevedel-slash-commands))))
    (should (eq 'mevedel-cmd--skills
                (cdr (assoc "skills" mevedel-slash-commands))))
    (should (eq 'mevedel-cmd--goal
                (cdr (assoc "goal" mevedel-slash-commands))))
    (should (eq 'mevedel-cmd--plan
                (cdr (assoc "plan" mevedel-slash-commands))))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### /pl")
      (goto-char (point-max))
      (let* ((capf (mevedel-slash-capf))
             (cands (and capf
                         (mevedel-skills-test--capf-candidates capf "pl")))
             (annot (and capf (plist-get (nthcdr 3 capf)
                                         :annotation-function))))
        (should capf)
        (should (member "plan" cands))
        (should (member "plugin" cands))
        (should (string-match-p "optional prompt" (funcall annot "plan")))
        (should (string-match-p "list" (funcall annot "plugin"))))))

  :doc "slash root returns local commands only"
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
          (should-not (member "simplify" cands))
          (should (equal " [command] no args; list commands and skills"
                         (funcall annot "help")))))))

  :doc "dollar root returns session skills only"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "simplify")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("help" . ignore)
                                      ("tokens" . ignore))))
        (insert "### $")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (cands (and capf
                           (mevedel-skills-test--capf-candidates capf)))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (member "simplify" cands))
          (should-not (member "help" cands))
          (should-not (member "tokens" cands))
          (should (equal " [skill]" (funcall annot "simplify")))))))

  :doc "dollar root includes inline and fork skills"
  (let* ((session (mevedel-skills-test--make-session))
         (inline (mevedel-skill--create
                  :name "summarize"
                  :context 'inline))
         (fork (mevedel-skill--create
                :name "review"
                :context 'fork)))
    (setf (mevedel-session-skills session) (list inline fork))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (cands (and capf
                           (mevedel-skills-test--capf-candidates capf))))
          (should capf)
          (should (member "summarize" cands))
          (should (member "review" cands))))))

  :doc "inline dollar completion offers inline skills only"
  (let* ((session (mevedel-skills-test--make-session))
         (inline (mevedel-skill--create
                  :name "summarize"
                  :context 'inline))
         (fork (mevedel-skill--create
                :name "review"
                :context 'fork)))
    (setf (mevedel-session-skills session) (list inline fork))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### Please use $")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (cands (and capf
                           (mevedel-skills-test--capf-candidates capf))))
          (should capf)
          (should (member "summarize" cands))
          (should-not (member "review" cands))))))

  :doc "plugin-prefixed skill candidates complete as one dollar name"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "superpowers:brainstorming")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $superpowers:b")
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
               ("goal" . ignore)
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
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("mode" . ignore))))
        (insert "### /mode fu")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("full-auto")
                         (mevedel-skills-test--capf-candidates
                          capf "fu")))
          (should (member "auto"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (string-match-p "auto-allow"
                                  (funcall annot "full-auto")))))))

  :doc "Goal approval completes only the two canonical policies"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands '(("goal" . ignore))))
        (insert "### /goal approval a")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (annot (and capf (plist-get (nthcdr 3 capf)
                                           :annotation-function))))
          (should capf)
          (should (equal '("automatic")
                         (mevedel-skills-test--capf-candidates capf "a")))
          (should (member "supervised"
                          (mevedel-skills-test--capf-candidates capf)))
          (should (equal " approval policy"
                         (funcall annot "automatic")))))))

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
        (insert "### $rem")
        (goto-char (point-max))
        (let* ((capf (mevedel-slash-capf))
               (exit (and capf (plist-get (nthcdr 3 capf)
                                          :exit-function))))
          (delete-region (nth 0 capf) (nth 1 capf))
          (insert "remember")
          (funcall exit "remember" 'finished)
          (insert "d")
          (should (equal "### $remember d"
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
  ;; User-invocable false skills are not shown in `$' completion.
  (let* ((session (mevedel-skills-test--make-session))
         (visible (mevedel-skill--create :name "visible"))
         (hidden (mevedel-skill--create :name "hidden"
                                        :user-invocable-p nil)))
    (setf (mevedel-session-skills session) (list visible hidden))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $")
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
         (hidden (mevedel-skills-test--stateful-skill :name "hidden")))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list visible hidden))
          (mevedel-skills--set-enabled hidden nil)
          (mevedel-skills-test--with-chat-buffer session
            (let ((mevedel-slash-commands nil))
              (insert "### $")
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
        (insert "### $")
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
        (insert "### $")
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
              (insert "### $")
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
                          :name "test" :permission-mode 'ask)))
            (setq-local mevedel--session session)
            (mevedel-cmd--mode "auto")
            (should (eq 'auto
                        (mevedel-session-permission-mode session)))
            (should (eq 'auto mevedel-permission-mode))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "accepts UI aliases"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "test" :permission-mode 'ask)))
      (setq-local mevedel--session session)
      (mevedel-cmd--mode "edit")
      (should (eq 'auto
                  (mevedel-session-permission-mode session)))
      (mevedel-cmd--mode "full-auto")
      (should (eq 'full-auto
                  (mevedel-session-permission-mode session)))
      (should (memq 'full-auto-mode
                    (mapcar #'mevedel-reminder-type
                            (mevedel-session-reminders session))))))

  :doc "leaving full-auto via /mode installs its exit reminder"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "test" :permission-mode 'full-auto)))
      (setf (mevedel-session-reminders session)
            (list (mevedel-reminders-make-full-auto-mode)))
      (setq-local mevedel--session session)
      (mevedel-cmd--mode "ask")
      (let ((types (mapcar #'mevedel-reminder-type
                           (mevedel-session-reminders session))))
        (should (eq 'ask
                    (mevedel-session-permission-mode session)))
        (should-not (memq 'full-auto-mode types))
        (should (memq 'full-auto-mode-exit types)))))

  :doc "rejects unknown modes"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'ask)
    (should-error (mevedel-cmd--mode "bogus") :type 'user-error)
    (should (eq 'ask mevedel-permission-mode)))

  :doc "blank args leaves the mode unchanged"
  (with-temp-buffer
    (setq-local mevedel-permission-mode 'auto)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (_area) (user-error "No cockpit"))))
      (mevedel-cmd--mode ""))
    (should (eq 'auto mevedel-permission-mode))))

(mevedel-deftest mevedel-cmd--goal ()
  ,test
  (test)
  :doc "starts a supervised Goal from a nonblank objective"
  (with-temp-buffer
    (setq-local mevedel--session (mevedel-session--create :name "main"))
    (let (started)
      (cl-letf (((symbol-function 'mevedel-goal-start)
                 (lambda (objective display)
                   (setq started (list objective display)))))
        (should (eq 'mevedel-view-sent (mevedel-cmd--goal "Fix it"))))
      (should (equal '("Fix it" "Fix it") started))))
  :doc "starts automatic Goals explicitly without changing permission mode"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "main" :permission-mode 'auto))
          started)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-goal-start)
                 (lambda (objective display policy)
                   (setq started (list objective display policy)))))
        (should (eq 'mevedel-view-sent
                    (mevedel-cmd--goal "auto Ship safely"))))
      (should (equal '("Ship safely" "Ship safely" automatic) started))
      (should (eq 'auto
                  (mevedel-session-permission-mode session)))))
  :doc "bare command opens the Goal cockpit with a current Goal"
  (with-temp-buffer
    (require 'mevedel-menu)
    (let* ((goal (mevedel-goal--create
                  :objective "Fix it" :status 'active :phase 'planning))
           (session (mevedel-session--create :name "main" :goal goal))
           opened)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq opened area))))
        (mevedel-cmd--goal nil))
      (should (eq 'goal opened))))
  :doc "bare command opens Goal creation cockpit when no Goal exists"
  (with-temp-buffer
    (setq-local mevedel--session (mevedel-session--create :name "main"))
    (let (opened)
      (cl-letf (((symbol-function 'mevedel-menu-open)
                 (lambda (area) (setq opened area))))
        (mevedel-cmd--goal nil))
      (should (eq 'goal opened))))
  :doc "dispatches edit, pause, resume, and clear as lifecycle actions"
  (with-temp-buffer
    (let ((goal (mevedel-goal--create
                 :id "g1" :objective "Old" :status 'paused
                 :phase 'planning :cycle 1))
          calls)
      (setq-local mevedel--session
                  (mevedel-session--create :name "main" :goal goal))
      (cl-letf (((symbol-function 'mevedel-goal-edit)
                 (lambda (value) (push (list 'edit value) calls)))
                ((symbol-function 'mevedel-goal-pause)
                 (lambda () (push '(pause) calls)))
                ((symbol-function 'mevedel-goal-resume)
                 (lambda (value) (push (list 'resume value) calls)))
                ((symbol-function 'mevedel-goal-clear)
                 (lambda () (push '(clear) calls))))
        (mevedel-cmd--goal "edit New objective")
        (mevedel-cmd--goal "pause")
        (mevedel-cmd--goal "resume new evidence")
        (mevedel-cmd--goal "clear"))
      (should (equal '((clear) (resume "new evidence") (pause)
                       (edit "New objective"))
                     calls))))
  :doc "reports and changes the current Goal approval policy"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :objective "Old" :status 'active
                  :phase 'planning :approval-policy 'supervised :cycle 1))
           (session (mevedel-session--create :name "main" :goal goal))
           calls messages)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-goal-set-approval-policy)
                 (lambda (policy) (push policy calls)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (mevedel-cmd--goal "approval")
        (mevedel-cmd--goal "approval automatic")
        (mevedel-cmd--goal "approval supervised"))
      (should (equal '(supervised automatic) calls))
      (should (equal '("mevedel: Goal approval policy is supervised")
                     messages))))
  :doc "rejects unknown Goal approval policy spellings"
  (with-temp-buffer
    (setq-local mevedel--session
                (mevedel-session--create
                 :name "main"
                 :goal (mevedel-goal--create
                        :id "g1" :status 'active :phase 'planning
                        :approval-policy 'supervised)))
    (should-error (mevedel-cmd--goal "approval auto")
                  :type 'user-error)))

(mevedel-deftest mevedel-cmd--plan ()
  ,test
  (test)
  :doc "enters Plan without changing the session permission policy"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :name "main" :permission-mode 'auto)))
      (setq-local mevedel--session session)
      (mevedel-cmd--plan nil)
      (should (mevedel-session-plan-mode session))
      (should (eq 'auto (mevedel-session-permission-mode session)))))

  :doc "rejects arguments without changing Plan state"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "main")))
      (setq-local mevedel--session session)
      (should-error (mevedel-cmd--plan "later") :type 'user-error)
      (should-not (mevedel-session-plan-mode session)))))

(mevedel-deftest mevedel-cmd--auto ()
  ,test
  (test)
  :doc "toggles auto on without authorizing full-auto execution"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'auto
                        (mevedel-session-permission-mode session)))
            (should-not
             (memq 'full-auto-mode
                   (mapcar #'mevedel-reminder-type
                           (mevedel-session-reminders session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "toggles auto off to ask"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'auto)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'ask
                        (mevedel-session-permission-mode session)))
            (should-not (mevedel-session-reminders session))))
      (set-default-toplevel-value 'mevedel-permission-mode saved)))

  :doc "switches full-auto to auto and records the boundary change"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'full-auto)))
            (setf (mevedel-session-reminders session)
                  (list (mevedel-reminders-make-full-auto-mode)))
            (setq-local mevedel--session session)
            (mevedel-cmd--auto nil)
            (should (eq 'auto
                        (mevedel-session-permission-mode session)))
            (let ((types (mapcar #'mevedel-reminder-type
                                 (mevedel-session-reminders session))))
              (should-not (memq 'full-auto-mode types))
              (should (memq 'full-auto-mode-exit types)))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))



(provide 'test-mevedel-skills-ui)

;;; test-mevedel-skills-ui.el ends here
