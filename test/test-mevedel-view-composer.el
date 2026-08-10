;;; test-mevedel-view-composer.el --- View composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests composer editing, submission, queueing, and send orchestration.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar mevedel-plugin-extra-roots)
(defvar org-mode-hook)

(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-transcript)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-media)
(require 'mevedel-tool-registry)
(require 'mevedel-mentions)
(require 'mevedel-skills-plan)
(require 'mevedel-skills-ui)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-permission-queue)
(require 'mevedel-persistence)
(require 'mevedel-review)
(require 'mevedel-goal)
(require 'mevedel)
(require 'mevedel-agents)
(require 'mevedel-agent-control)
(require 'mevedel-agent-runtime)
(require 'mevedel-hooks)
(require 'mevedel-view-zone)
(require 'mevedel-view-history)
(require 'gptel-request)
(require 'mcp)

(defun mevedel-view-composer-test--owner (symbol)
  "Return the source feature basename that defines SYMBOL."
  (file-name-base (or (symbol-file symbol 'defun) "")))

(mevedel-deftest mevedel-view-composer-ownership ()
  ,test
  (test)
  :doc "owns composer editing and submission"
  (dolist (symbol '(mevedel-view--input-start
                    mevedel-view--input-text
                    mevedel-view--clear-input
                    mevedel-view-refresh-input-prompt
                    mevedel-view-send
                    mevedel-view--send-root
                    mevedel-view--forward-input
                    mevedel-view--run-prompt-submit-hook))
    (should (equal "mevedel-view-composer"
                   (mevedel-view-composer-test--owner symbol))))
  :doc "owns pending-input and request-progress send orchestration"
  (dolist (symbol '(mevedel-view--queue-follow-up
                    mevedel-view-send-follow-up
                    mevedel-view--schedule-follow-up-drain
                    mevedel-view-abort))
    (should (equal "mevedel-view-composer"
                   (mevedel-view-composer-test--owner symbol)))))

(mevedel-deftest mevedel-view-enter-directive-scope
  (:doc "selects Discuss result attempts and rejects unavailable actions")
  (let* ((workspace
          (mevedel-workspace--create
           :type 'test :id "directive-scope" :root "/tmp"
           :name "directive-scope"))
         (record
          (mevedel-directive--create
           :id "directive" :request "Request"
           :attempts
           (list (mevedel-directive-attempt--create
                  :directive-request "Request" :outcome 'success))))
         (data-buffer (generate-new-buffer " *directive-scope-data*"))
         (view-buffer (generate-new-buffer " *directive-scope-view*"))
         captured)
    (unwind-protect
        (progn
          (mevedel-workspace-set-directives workspace (list record))
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel--directive-session-buffer)
                     (lambda (&rest _) (cons data-buffer nil)))
                    ((symbol-function 'mevedel-view--switch-composer-scope)
                     (lambda (scope) (setq captured scope)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) nil)))
            (mevedel-view-enter-directive-scope
             record 'discuss nil workspace)
            (should (= 1 (plist-get captured :attempt-index)))
            (mevedel-view-enter-directive-scope
             record 'request-changes nil workspace)
            (should (eq 'request-changes (plist-get captured :action)))
            (should-error
             (mevedel-view-enter-directive-scope
              record 'retry nil workspace)
             :type 'user-error)
            (setf (mevedel-directive-attempt-outcome
                   (car (mevedel-directive-attempts record)))
                  'failure)
            (mevedel-view-enter-directive-scope
             record 'retry nil workspace)
            (should (eq 'retry (plist-get captured :action)))
            (setf (mevedel-directive-plan record) '(:status draft))
            (mevedel-view-enter-directive-scope
             record 'plan nil workspace)
            (should (eq 'plan (plist-get captured :action)))
            (should-error
             (mevedel-view-enter-directive-scope
              record 'request-changes nil workspace)
             :type 'user-error)))
      (kill-buffer data-buffer)
      (kill-buffer view-buffer))))


;;
;;; Test helpers

(defun mevedel-view-test--capf-candidates (capf &optional prefix)
  "Return completion candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

(defun mevedel-view-test--skill-hint-string ()
  "Return the current skill argument hint overlay string."
  (and (overlayp mevedel-view--skill-argument-hint-overlay)
       (overlay-buffer mevedel-view--skill-argument-hint-overlay)
       (overlay-get mevedel-view--skill-argument-hint-overlay
                    'after-string)))

(defun mevedel-view-test--write-skill (dir name frontmatter &optional body)
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
        (insert "\n" body)
        (unless (string-suffix-p "\n" body)
          (insert "\n"))))
    skill-file))

(defun mevedel-view-test--complete-skill (candidate)
  "Replace the current `$' completion fragment with CANDIDATE."
  (let* ((capf (mevedel-view-slash-capf))
         (exit (and capf (plist-get (nthcdr 3 capf) :exit-function))))
    (should capf)
    (delete-region (nth 0 capf) (nth 1 capf))
    (insert candidate)
    (funcall exit candidate 'finished)))

(defun mevedel-view-test--bound-source-failure-case (mode)
  "Assert unavailable bound source behavior for failure MODE."
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-inline-bound-failure-" t))
         (project-skills (file-name-concat root ".mevedel/skills"))
         (user-skills
          (make-temp-file "mevedel-inline-bound-failure-user-" t))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "inline-bound-failure"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         source-file)
    (unwind-protect
        (progn
          (setq source-file
                (mevedel-view-test--write-skill
                 project-skills "alpha"
                 "name: alpha\ndescription: Project alpha\ncontext: inline\n"
                 "ORIGINAL ALPHA BODY"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws)
              (mevedel-skills-install session data-buf))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "> diagnosis\nPlease use $al")
              (mevedel-view-test--complete-skill "alpha"))
            (pcase mode
              ('missing
               (delete-file source-file))
              ('unreadable
               (set-file-modes source-file 0)
               (when (file-readable-p source-file)
                 (ert-skip "File permissions do not make files unreadable")))
              ('malformed
               (with-temp-file source-file
                 (insert "---\nname: [invalid\n---\n")))
              ('not-user-invocable
               (mevedel-view-test--write-skill
                project-skills "alpha"
                (concat "name: alpha\ndescription: Project alpha\n"
                        "context: inline\nuser-invocable: false\n")
                "ORIGINAL ALPHA BODY"))
              ('disabled
               (with-current-buffer data-buf
                 (mevedel-skills--set-enabled
                  (mevedel-session-get-skill session "alpha") nil))))
            (mevedel-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING ALPHA BODY")
            (setq mevedel-skill-dirs
                  (list ".mevedel/skills" user-skills))
            (with-current-buffer data-buf
              (mevedel-skills-install session data-buf))
            (let (sent message-text raised)
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _)
                           (setq sent
                                 (mevedel-view-test--dry-run-request-data))))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq message-text
                                 (apply #'format format-string args)))))
                (with-current-buffer view-buf
                  (setq raised
                        (condition-case err
                            (progn (mevedel-view-send) nil)
                          (user-error (error-message-string err))))
                  (should sent)
                  (should-not raised)
                  (should (string-match-p
                           (regexp-quote "[skill:alpha -- unavailable]")
                           sent))
                  (should-not (string-match-p "COMPETING ALPHA BODY" sent))
                  (should (string-match-p "mevedel:" message-text))
                  (should (string-empty-p (mevedel-view--input-text)))
                  (should (equal "> diagnosis\nPlease use $alpha"
                                 (car (mevedel-view-history--entries))))
                  (with-current-buffer data-buf
                    (let ((text
                           (mevedel-pipeline--strip-render-data-blocks
                            (buffer-string))))
                      (should (string-match-p
                               (regexp-quote "Please use $alpha")
                               text))
                      (should-not (string-match-p
                                   (regexp-quote
                                    "[skill:alpha -- unavailable]")
                                   text))))))))
      (delete-directory root t)
      (delete-directory user-skills t)))))

(defun mevedel-view-test--stop-prompt-hook (_event)
  "Block prompt submission in view-send cases."
  '(:continue nil :stop-reason "blocked"))

(defvar mevedel-view-test--seen-prompt nil)

(defun mevedel-view-test--rewrite-prompt-hook (event)
  "Capture prompt EVENT and rewrite it in view-send cases."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"))

(defun mevedel-view-test--rewrite-prompt-hook-with-context (event)
  "Capture prompt EVENT, rewrite it, and add hook context."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :additional-context "hook policy context"))

(defun mevedel-view-test--rewrite-prompt-hook-with-message (event)
  "Capture prompt EVENT, rewrite it, and add a user-facing message."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :system-message "changed by test hook"))

(defun mevedel-view-test--add-prompt-hook-context (event)
  "Capture prompt EVENT and add model-visible context."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:additional-context "ordinary steering hook context"))


;;
;;; Composer editing

(mevedel-deftest mevedel-view--input-prompt-string
  (:doc "renders permission mode in the prompt prefix")
  ,test
  (test)

  :doc "ask mode renders an undecorated prompt"
  (let ((prompt (mevedel-view--input-prompt-string 'ask)))
    (should (string= "\n> " prompt))
    (should (eq 'mevedel-view-input-prompt
                (get-text-property 0 'font-lock-face prompt))))

  :doc "edits mode renders its canonical name"
  (let ((prompt (mevedel-view--input-prompt-string 'edits)))
    (should (string= "\n[edits] > " prompt))
    (should (eq 'mevedel-view-permission-mode-edits
                (get-text-property 2 'font-lock-face prompt))))

  :doc "full-auto mode renders its canonical name"
  (let ((prompt (mevedel-view--input-prompt-string 'full-auto)))
    (should (string= "\n[full-auto] > " prompt))
    (should (eq 'mevedel-view-permission-mode-full-auto
                (get-text-property 2 'font-lock-face prompt))))

  :doc "Plan shows the retained permission mode"
  (let ((mevedel--session
         (mevedel-session--create :name "main" :plan-mode t)))
    (should (string= "\n[Plan · full-auto] > "
                     (mevedel-view--input-prompt-string 'full-auto))))

  :doc "directive scope is loud and uses its distinct prompt prefix"
  (let ((mevedel-view--composer-scope
         (list :directive-id "abcdef123456" :action 'request-changes
               :record (mevedel-directive--create
                        :request "Explain this code"))))
    (let ((prompt (mevedel-view--input-prompt-string 'ask)))
      (should
       (string=
        (concat "\n◆ Request changes · Explain this code\n"
                "  isolated from chat · ask · C-c C-k Back\n◆ > ")
        (substring-no-properties prompt)))
      (should (eq 'shadow
                  (get-text-property
                   (string-match "isolated from chat" prompt)
                   'font-lock-face prompt)))
      (should (eq 'mevedel-view-permission-mode-ask
                  (get-text-property
                   (string-match "ask" prompt)
                   'font-lock-face prompt)))
      (should (string-suffix-p "◆ > " prompt)))
    (setq mevedel-view--composer-scope
          (plist-put mevedel-view--composer-scope :action 'retry))
    (let ((case-fold-search nil))
      (should
       (string-match-p
        "◆ Retry ·"
        (mevedel-view--input-prompt-string 'ask))))
    (setq mevedel-view--composer-scope
          (plist-put mevedel-view--composer-scope :action 'plan))
    (should (string-match-p
             "◆ Plan ·" (mevedel-view--input-prompt-string 'ask)))
    (let* ((mevedel--session
            (mevedel-session--create :name "main" :plan-mode t))
           (prompt (mevedel-view--input-prompt-string 'edits)))
      (should (string-match-p "edits · Plan paused" prompt))
      (should (eq 'mevedel-view-permission-mode-edits
                  (get-text-property
                   (string-match "edits" prompt)
                   'font-lock-face prompt)))
      (should (eq 'mevedel-view-plan-mode
                  (get-text-property
                   (string-match "Plan paused" prompt)
                   'font-lock-face prompt)))))

  :doc "directive discussion labels reflect the next scoped action"
  (let* ((record (mevedel-directive--create
                  :request "Explain this code"))
         (mevedel-view--composer-scope
          (list :directive-id "abcdef123456" :action 'discuss
                :record record)))
    (should
     (string-match-p
      "◆ Discuss ·"
      (mevedel-view--input-prompt-string 'ask)))
    (setf (mevedel-directive-discussion record)
          (list (mevedel-directive-discussion-turn--create
                 :sequence 1 :directive-request "Explain this code"
                 :outcome 'success))
          (mevedel-directive-state record) 'discussed)
    (should
     (string-match-p
      "◆ Continue discussion ·"
      (mevedel-view--input-prompt-string 'ask)))
    (setq mevedel-view--composer-scope
          (plist-put mevedel-view--composer-scope :attempt-index 1))
    (should
     (string-match-p
      "◆ Discuss result ·"
      (mevedel-view--input-prompt-string 'ask)))
    (setf (mevedel-directive-request record) "Changed request"
          (mevedel-directive-state record) nil)
    (setq mevedel-view--composer-scope
          (plist-put mevedel-view--composer-scope :attempt-index nil))
    (should
     (string-match-p
      "◆ Discuss ·"
      (mevedel-view--input-prompt-string 'ask)))))

(mevedel-deftest mevedel-view--switch-composer-scope ()
  ,test
  (test)
  :doc "preserves independent chat and directive drafts with exact point"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "scope" :root "/tmp" :name "scope"))
           (session (mevedel-session-create "main" workspace))
           (record (mevedel-directive--create
                    :id "directive-1" :request "Explain this code"))
           (scope (list :directive-id "directive-1" :action 'discuss
                        :record record :workspace workspace))
           (chat-draft ">first editable character\nsecond line"))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert chat-draft)
        (goto-char (+ (mevedel-view--input-start) 7))
        (mevedel-view--switch-composer-scope scope)
        (should (string-empty-p (mevedel-view--input-text)))
        (insert "directive draft")
        (mevedel-view-back-to-chat)
        (should (equal chat-draft
                       (buffer-substring-no-properties
                        (mevedel-view--input-start) (point-max))))
        (should (= 7 (- (point) (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--queue-follow-up
  ()
  ,test
  (test)
  :doc "stores directive scope on queued follow-ups"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "queue-scope" :root "/tmp"
                       :name "queue-scope"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session
                    mevedel-view--composer-scope
                    '(:directive-id "directive-1" :action discuss
                      :attempt-index 2))
        (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                   #'ignore)
                  ((symbol-function
                    'mevedel-view--schedule-late-follow-up-drain)
                   #'ignore))
          (mevedel-view--queue-follow-up "follow up"))
        (let ((scope
               (plist-get (car (mevedel-session-pending-follow-ups session))
                          :scope)))
          (should (equal "directive-1" (plist-get scope :directive-id)))
          (should (eq 'discuss (plist-get scope :action)))
          (should (= 2 (plist-get scope :attempt-index))))))))

(mevedel-deftest mevedel-view--next-permission-mode
  (:doc "cycles permission modes in view order")
  ,test
  (test)

  :doc "ask mode moves to edits"
  (should (eq 'edits
              (mevedel-view--next-permission-mode 'ask)))

  :doc "edits mode moves to full-auto"
  (should (eq 'full-auto
              (mevedel-view--next-permission-mode 'edits)))

  :doc "full-auto mode wraps to ask"
  (should (eq 'ask
              (mevedel-view--next-permission-mode 'full-auto)))

  :doc "unknown workflow states start at edits"
  (should (eq 'edits
              (mevedel-view--next-permission-mode 'plan)))

  :doc "nil mode starts at edits"
  (should (eq 'edits
              (mevedel-view--next-permission-mode nil)))

  :doc "unknown mode starts at edits"
  (should (eq 'edits
              (mevedel-view--next-permission-mode 'bogus))))

(mevedel-deftest mevedel-view--plan-mode-p
  (:doc "reads Plan state from the current view session")
  ,test
  (test)
  (let ((mevedel--session
         (mevedel-session--create :name "main" :plan-mode t)))
    (should (mevedel-view--plan-mode-p))
    (setf (mevedel-session-plan-mode mevedel--session) nil)
    (should-not (mevedel-view--plan-mode-p))))

(mevedel-deftest mevedel-view-cycle-permission-mode
  (:doc "cycles the current session mode and refreshes the prompt")
  ,test
  (test)

  :doc "cycles session mode and refreshes prompt"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (let ((session (mevedel-session--create
                          :name "main"
                          :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf)
              (setq-local mevedel-permission-mode 'ask))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'ask)
              (goto-char (mevedel-view--input-start))
              (insert "> first\nsecond")
              (should (eq 'edits
                          (mevedel-view-cycle-permission-mode)))
              (should (eq 'edits
                          (mevedel-session-permission-mode session)))
              (should (eq 'edits
                          (buffer-local-value
                           'mevedel-permission-mode data-buf)))
              (should (eq 'edits mevedel-permission-mode))
              (should (eq saved
                          (default-toplevel-value 'mevedel-permission-mode)))
              (should (string= "\n[edits] > "
                               (buffer-substring-no-properties
                                mevedel-view--input-marker
                                (mevedel-view--input-start))))
              (should (equal "> first\nsecond"
                             (mevedel-view--input-text))))
            (with-current-buffer view-buf
              (should (eq 'full-auto
                          (mevedel-view-cycle-permission-mode)))
              (should (memq 'full-auto-mode
                            (mapcar #'mevedel-reminder-type
                                    (mevedel-session-reminders session))))
              (should (eq 'ask
                          (mevedel-view-cycle-permission-mode)))
              (let ((types (mapcar #'mevedel-reminder-type
                                   (mevedel-session-reminders session))))
                (should-not (memq 'full-auto-mode types))
                (should (memq 'full-auto-mode-exit types)))
              (should (eq 'ask
                          (mevedel-session-permission-mode session)))
              (should (equal "> first\nsecond"
                             (mevedel-view--input-text))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))

(mevedel-deftest mevedel-view-toggle-plan-mode
  (:doc "toggles Plan independently while retaining permission mode and draft")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "main"
                    :permission-mode 'full-auto)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "> first\nsecond")
        (should (mevedel-view-toggle-plan-mode))
        (should (mevedel-session-plan-mode session))
        (should (eq 'full-auto
                    (mevedel-session-permission-mode session)))
        (should (string= "\n[Plan · full-auto] > "
                         (buffer-substring-no-properties
                          mevedel-view--input-marker
                          (mevedel-view--input-start))))
        (should (equal "> first\nsecond"
                       (mevedel-view--input-text)))
        (should-not (mevedel-view-toggle-plan-mode))
        (should-not (mevedel-session-plan-mode session))
        (should (eq 'full-auto
                    (mevedel-session-permission-mode session)))
        (should (equal "> first\nsecond"
                       (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-view-composer-keymap
  (:doc "Composer commands are bound only inside editable input")
  ,test
  (test)
  (let ((bindings
         '(("C-<tab>" . mevedel-view-toggle-plan-mode)
           ("<backtab>" . mevedel-view-cycle-permission-mode)
           ("S-TAB" . mevedel-view-cycle-permission-mode)
           ("C-c RET" . mevedel-view-send)
           ("C-c TAB" . mevedel-view-send-follow-up)
           ("C-c C-k" . mevedel-view-cancel-composer-state)
           ("C-c C-l" . mevedel-view-history-browse)
           ("C-c C-u" . mevedel-view-history-clear-input)
           ("C-y" . mevedel-view-yank-dwim)
           ("M-p" . mevedel-view-history-previous)
           ("M-n" . mevedel-view-history-next)
           ("M-r" . mevedel-view-history-search)
           ("C-a" . mevedel-view-history-beginning-of-line)
           ("C-z" . mevedel-view-history-beginning-of-line)))
        (was-enabled tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (mevedel-view-test--with-buffers
            (with-current-buffer view-buf
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "C-z") #'move-beginning-of-line)
              (goto-char (mevedel-view--input-start))
              (dolist (binding bindings)
                (should
                 (eq (key-binding (kbd (car binding)))
                     (cdr binding))))
              (insert "draft")
              (goto-char (+ (mevedel-view--input-start) 2))
              (should
               (eq (key-binding (kbd "C-<tab>"))
                   #'mevedel-view-toggle-plan-mode))
              (mevedel-view-refresh-input-prompt)
              (should
               (eq (key-binding (kbd "C-<tab>"))
                   #'mevedel-view-toggle-plan-mode))
              (goto-char (point-min))
              (dolist (binding bindings)
                (should-not
                 (eq (key-binding (kbd (car binding)))
                     (cdr binding))))
              (mevedel-view--setup view-buf data-buf)
              (goto-char (mevedel-view--input-start))
              (should
               (eq (key-binding (kbd "C-<tab>"))
                   #'mevedel-view-toggle-plan-mode))
              (goto-char (point-min))
              (should
               (eq (key-binding (kbd "C-<tab>"))
                   #'tab-next)))))
      (tab-bar-mode (if was-enabled 1 -1)))))

(mevedel-deftest mevedel-view-arm-conversation-fork ()
  ,test
  (test)
  :doc "arms the exact settled turn, renders cancellation, and preserves draft"
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "source"
            :session-id "source-id"
            :current-segment 1))
          (target
           '(:fork-point-id "stable-2" :segment 1 :turn 2 :cum-turn 2))
          checked)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "> existing\nmultiline draft")
        (goto-char (point-min))
        (setq-local mevedel-view--historical-segment-number 1
                    mevedel-view--historical-segment-buffer data-buf)
        (mevedel-view--set-historical-composer-visible nil)
        (cl-letf
            (((symbol-function 'mevedel-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'mevedel-session-persistence--assert-stable-source)
              (lambda (candidate buffer operation)
                (setq checked (list candidate buffer operation)))))
          (mevedel-view-arm-conversation-fork))
        (should (equal (list session data-buf "forking")
                       checked))
        (should (equal "stable-2"
                       (plist-get mevedel-view--armed-session-fork
                                  :fork-point-id)))
        (should (= (point) (point-max)))
        (should-not buffer-read-only)
        (should-not (invisible-p (mevedel-view--input-start)))
        (should (equal "> existing\nmultiline draft"
                       (mevedel-view--input-text)))
        (let ((descriptor
               (gethash 'armed-session-fork
                        mevedel-view--interaction-descriptors)))
          (should (string-match-p
                   "Fork conversation from Assistant turn 2"
                   (plist-get descriptor :body)))
          (should (string-match-p "\\[Cancel\\]"
                                  (plist-get descriptor :body)))
          (should (plist-member descriptor :active-work-paused))
          (should-not (plist-get descriptor :active-work-paused)))
        (mevedel-view-cancel-composer-state)
        (should-not mevedel-view--armed-session-fork)
        (should (mevedel-view-historical-segment-p))
        (should buffer-read-only)
        (should (invisible-p (mevedel-view--input-start)))
        (should (= (point) (point-min)))
        (should-not (gethash 'armed-session-fork
                             mevedel-view--interaction-descriptors))
        (should (equal "> existing\nmultiline draft"
                       (mevedel-view--input-text))))))
  :doc "reports an exact selection error without arming"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'mevedel-view-fork-point-at-point)
                 (lambda ()
                   (user-error "Point is not on an assistant response"))))
        (should-error (mevedel-view-arm-conversation-fork)
                      :type 'user-error))
      (should-not mevedel-view--armed-session-fork)))
  :doc "rejects an inherited point on Child but permits a later response"
  (mevedel-view-test--with-buffers
    (let* ((session
            (mevedel-session--create
             :name "child"
             :session-id "child-id"
             :forked-from-session-id "source-id"
             :forked-from-fork-point-id "inherited-point"
             :fork-type 'conversation
             :current-segment 1))
           (target
            '(:fork-point-id "inherited-point"
              :segment 1 :turn 1 :cum-turn 1)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "> child draft\nsecond line")
        (cl-letf
            (((symbol-function 'mevedel-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'mevedel-session-persistence--assert-stable-source)
              #'ignore))
          (let ((error
                 (should-error
                  (mevedel-view-arm-conversation-fork)
                  :type 'user-error)))
            (should (string-match-p "Source"
                                    (error-message-string error))))
          (should-not mevedel-view--armed-session-fork)
          (plist-put target :fork-point-id "later-point")
          (plist-put target :turn 2)
          (plist-put target :cum-turn 2)
          (mevedel-view-arm-conversation-fork)
          (should (equal "later-point"
                         (plist-get mevedel-view--armed-session-fork
                                    :fork-point-id)))
          (should (equal "> child draft\nsecond line"
                         (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-view-arm-worktree-fork ()
  ,test
  (test)
  :doc "preflights Git and arms the exact settled turn as a Worktree Fork"
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "source"
            :session-id "source-id"
            :current-segment 1))
          (target
           '(:fork-point-id "stable-2" :segment 1 :turn 2 :cum-turn 2))
          preflight
          reserved)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (cl-letf
            (((symbol-function 'mevedel-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'mevedel-session-persistence--assert-stable-source)
              #'ignore)
             ((symbol-function 'mevedel-worktree-fork-preflight)
              (lambda (candidate)
                (setq preflight candidate)
                '(:base-commit "abc123")))
             ((symbol-function 'mevedel-worktree-fork-reservation)
              (lambda (candidate context)
                (should (eq candidate session))
                (should (equal '(:base-commit "abc123") context))
                (setq reserved
                      '(:branch "worktree/source-fork-1"
                        :directory "/repo/.worktrees/source-fork-1/")))))
          (mevedel-view-arm-worktree-fork))
        (should (eq session preflight))
        (should (eq 'worktree
                    (plist-get mevedel-view--armed-session-fork
                               :fork-type)))
        (should (eq reserved
                    (plist-get mevedel-view--armed-session-fork
                               :worktree-reservation)))
        (should (equal "draft" (mevedel-view--input-text)))
        (should
         (string-match-p
          "Fork worktree from Assistant turn 2"
          (plist-get
           (gethash 'armed-session-fork
                    mevedel-view--interaction-descriptors)
           :body)))))))

(mevedel-deftest mevedel-view--retarget-worktree-mention-bindings ()
  ,test
  (test)
  :doc "retargets file and skill bindings without mutating Source text"
  (let* ((source
          (file-name-as-directory
           (make-temp-file "mevedel-mentions-source-" t)))
         (worktree
          (file-name-as-directory
           (make-temp-file "mevedel-mentions-worktree-" t)))
         (session
          (mevedel-session--create
           :worktree-source-root source
           :worktree-directory worktree))
         (text (copy-sequence "Use @file:local and $skill"))
         result)
    (unwind-protect
        (progn
          (string-match "@file:local" text)
          (mevedel-mention-bindings-set
           (match-beginning 0) (match-end 0)
           (list :kind 'file :path (file-name-concat source "local"))
           text)
          (string-match "\\$skill" text)
          (mevedel-mention-bindings-set
           (match-beginning 0) (match-end 0)
           (list :kind 'skill
                 :source-file (file-name-concat source "SKILL.md"))
           text)
          (setq result
                (mevedel-view--retarget-worktree-mention-bindings
                 text session))
          (should
           (equal
            (list (file-name-concat worktree "local")
                  (file-name-concat worktree "SKILL.md"))
            (mapcar
             (lambda (range)
               (let ((binding (plist-get range :binding)))
                 (or (plist-get binding :path)
                     (plist-get binding :source-file))))
             (mevedel-mention-bindings-ranges result))))
          (should
           (equal
            (file-name-concat source "local")
            (plist-get
             (plist-get
              (car (mevedel-mention-bindings-ranges text))
              :binding)
             :path))))
      (delete-directory source t)
      (delete-directory worktree t))))

(mevedel-deftest mevedel-view-send/historical-segment ()
  ,test
  (test)
  :doc "refuses live sends and local commands unless a model fork is armed"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local mevedel-view--historical-segment-number 1
                  mevedel-view--historical-segment-buffer data-buf)
      (goto-char (mevedel-view--input-start))
      (insert "live-tip draft")
      (let ((error (should-error (mevedel-view-send) :type 'user-error)))
        (should (string-match-p
                 "return to latest or fork"
                 (error-message-string error))))
      (mevedel-view--clear-input)
      (goto-char (mevedel-view--input-start))
      (insert "/clear")
      (setq-local
       mevedel-view--armed-session-fork
       '(:fork-point-id "historical" :fork-type conversation))
      (let ((error (should-error (mevedel-view-send) :type 'user-error)))
        (should (string-match-p
                 "Slash commands are unavailable"
                 (error-message-string error)))))))

(mevedel-deftest mevedel-view-send/conversation-fork ()
  ,test
  (test)
  :doc "publishes before Child-owned prompt preparation and preserves ownership"
  (mevedel-view-test--with-source-skills nil
    (let* ((child-session (mevedel-session-create "child" ws))
           (child-data (generate-new-buffer " *fork-child-data*"))
           (child-view (generate-new-buffer " *fork-child-view*"))
           (target
            '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
              :fork-type conversation))
           (attachment "/tmp/fork-attachment.txt")
           (draft (format "> exact\nplease summarize @file:%s"
                          attachment))
           events)
      (unwind-protect
          (progn
            (with-current-buffer child-data
              (org-mode)
              (setq-local mevedel--session child-session
                          mevedel--workspace ws))
            (mevedel-view--setup child-view child-data)
            (with-current-buffer view-buf
              (setq-local mevedel-view--armed-session-fork target)
              (setf (mevedel-session-dropped-file-grants session)
                    (list attachment "/tmp/unrelated.txt"))
              (goto-char (mevedel-view--input-start))
              (insert draft)
              (goto-char (+ (mevedel-view--input-start) 3))
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-conversation-fork)
                    (lambda (buffer fork-target)
                      (push (list 'materialized buffer fork-target) events)
                      child-data))
                   ((symbol-function 'mevedel-view--submit-planned-input)
                    (lambda (input &rest _)
                      (push (list 'prepared (current-buffer) input) events))))
                (mevedel-view-send))
              (should-not mevedel-view--armed-session-fork)
              (should (string-empty-p (mevedel-view--input-text))))
            (should (eq 'materialized (caar (last events))))
            (should (eq child-view (cadr (car events))))
            (with-current-buffer child-view
              (should (equal draft (mevedel-view--input-text)))
              (should (= 3 (- (point)
                              (mevedel-view--input-start)))))
            (should (equal (list attachment)
                           (mevedel-session-dropped-file-grants
                            child-session)))
            (with-current-buffer data-buf
              (should-not
               (string-match-p "please summarize"
                               (buffer-string)))))
        (dolist (buffer (list child-view child-data))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer))))))

  :doc "materialization failure preserves Source draft, grants, and armed point"
  (mevedel-view-test--with-source-skills nil
    (let ((target
           '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
             :fork-type conversation))
          (attachment "/tmp/fork-failure.txt"))
      (with-current-buffer view-buf
        (setq-local mevedel-view--armed-session-fork target)
        (setf (mevedel-session-dropped-file-grants session)
              (list attachment))
        (goto-char (mevedel-view--input-start))
        (insert (format "> exact\n@file:%s" attachment))
        (let ((before (mevedel-view--composer-snapshot session)))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-conversation-fork)
                (lambda (&rest _) (error "materialization failed"))))
            (should-error (mevedel-view-send)
                          :type 'error))
          (should (equal target mevedel-view--armed-session-fork))
          (should (equal (plist-get before :text)
                         (plist-get
                          (mevedel-view--composer-snapshot session)
                          :text)))
          (should (equal (list attachment)
                         (mevedel-session-dropped-file-grants session)))))))

  :doc "Worktree failure keeps its reservation and exact Source composer"
  (mevedel-view-test--with-source-skills nil
    (let* ((reservation
            '(:branch "worktree/source-fork-1"
              :directory "/repo/.worktrees/source-fork-1/"))
           (target
            (list :fork-point-id "stable-1"
                  :segment 1 :turn 1 :cum-turn 1
                  :fork-type 'worktree
                  :worktree-reservation reservation))
           (attachment "/tmp/worktree-fork-failure.txt"))
      (with-current-buffer view-buf
        (setq-local mevedel-view--armed-session-fork target)
        (setf (mevedel-session-dropped-file-grants session)
              (list attachment))
        (goto-char (mevedel-view--input-start))
        (insert (format "> exact\n@file:%s" attachment))
        (let ((before (mevedel-view--composer-snapshot session)))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-worktree-fork)
                (lambda (&rest _) (error "staging failed"))))
            (should-error (mevedel-view-send)))
          (should (eq reservation
                      (plist-get mevedel-view--armed-session-fork
                                 :worktree-reservation)))
          (should (equal before
                         (mevedel-view--composer-snapshot session)))))))

  :doc "failed prompt preflight does not publish a Child"
  (mevedel-view-test--with-source-skills nil
    (let ((target
           '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
             :fork-type conversation))
          materialized)
      (with-current-buffer view-buf
        (setq-local mevedel-view--armed-session-fork target)
        (goto-char (mevedel-view--input-start))
        (insert "invalid skill input")
        (cl-letf
            (((symbol-function 'mevedel-skills-plan-user-input)
              (lambda (&rest _) (user-error "Invalid skill syntax")))
             ((symbol-function
               'mevedel-session-persistence-conversation-fork)
              (lambda (&rest _)
                (setq materialized t))))
          (should-error (mevedel-view-send)
                        :type 'user-error))
        (should-not materialized)
        (should (equal target mevedel-view--armed-session-fork))
        (should (equal "invalid skill input"
                       (mevedel-view--input-text))))))

  :doc "dispatches an armed Worktree Fork through its materializer"
  (mevedel-view-test--with-source-skills nil
    (let* ((child-session (mevedel-session-create "child" ws))
           (child-data (generate-new-buffer " *worktree-child-data*"))
           (child-view (generate-new-buffer " *worktree-child-view*"))
           (target
            '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
              :fork-type worktree))
           called)
      (unwind-protect
          (progn
            (with-current-buffer child-data
              (org-mode)
              (setq-local mevedel--session child-session
                          mevedel--workspace ws))
            (mevedel-view--setup child-view child-data)
            (with-current-buffer view-buf
              (setq-local mevedel-view--armed-session-fork target)
              (goto-char (mevedel-view--input-start))
              (insert "continue here")
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-worktree-fork)
                    (lambda (buffer fork-target)
                      (setq called (list buffer fork-target))
                      child-data))
                   ((symbol-function
                     'mevedel-session-persistence-conversation-fork)
                    (lambda (&rest _)
                      (ert-fail "Conversation materializer called")))
                   ((symbol-function 'mevedel-view--submit-planned-input)
                    #'ignore))
                (mevedel-view-send)))
            (should (equal (list data-buf target) called)))
        (dolist (buffer (list child-view child-data))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(mevedel-deftest mevedel-view--prompt-start-position ()
  ,test
  (test)
  :doc "uses a live marker at the prompt start without a full-buffer scan"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((expected (marker-position mevedel-view--input-marker)))
        (cl-letf (((symbol-function 'text-property-any)
                   (lambda (&rest _)
                     (ert-fail "Live prompt marker triggered a scan"))))
          (should (= expected (mevedel-view--prompt-start-position)))))))

  :doc "walks backward when the live marker points inside the prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((expected (marker-position mevedel-view--input-marker)))
        (set-marker mevedel-view--input-marker (1+ expected))
        (cl-letf (((symbol-function 'text-property-any)
                   (lambda (&rest _)
                     (ert-fail "Prompt-local recovery triggered a scan"))))
          (should (= expected (mevedel-view--prompt-start-position)))))))

  :doc "recovers by property scan from a stale marker in the draft"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((expected (marker-position mevedel-view--input-marker)))
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (set-marker mevedel-view--input-marker (point-max))
        (should (= expected (mevedel-view--prompt-start-position))))))

  :doc "recovers by property scan from a detached marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((expected (marker-position mevedel-view--input-marker)))
        (set-marker mevedel-view--input-marker nil)
        (should (= expected (mevedel-view--prompt-start-position)))))))

(mevedel-deftest mevedel-view-refresh-input-prompt
  (:doc "updates the prompt prefix without disturbing draft input")
  ,test
  (test)

  :doc "setup renders the ask mode prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string= "\n> "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))))

  :doc "refresh preserves input text and updates the mode"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (setq-local mevedel-permission-mode 'full-auto)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[full-auto] > "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))
      (should (string= "draft" (mevedel-view--input-text)))))

  :doc "refresh preserves a multiline draft starting with a literal >"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> quoted\nsecond line")
      (setq-local mevedel-permission-mode 'full-auto)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[full-auto] > "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))
      (should (string= "> quoted\nsecond line" (mevedel-view--input-text)))
      (should-not (get-text-property (mevedel-view--input-start)
                                     'mevedel-view-prompt))))

  :doc "refresh recovers drifted markers before updating the prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (setq-local mevedel-permission-mode 'full-auto)
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (should (string= "\n[full-auto] > "
                         (buffer-substring-no-properties
                          mevedel-view--input-marker
                          (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--call-preserving-input-text
  (:doc "restores composer text after accidental render insertion")
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (mevedel-view--call-preserving-input-text
       (lambda ()
         (goto-char (point-max))
         (insert "\nagent result leaked into composer")))
      (should (string= "draft" (mevedel-view--input-text)))
      (let ((changes 0))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--call-preserving-input-text
         (lambda ()
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (insert "status row\n"))))
        (should (= 0 changes))))))

(mevedel-deftest mevedel-view--call-preserving-window-state
  (:doc "follows new output only from the bottom")
  ,test
  (test)

  :doc "bottom-following windows stay at the bottom"
  (save-window-excursion
    (with-temp-buffer
      (dotimes (index 400)
        (insert (format "line %d\n" index)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (recenter -1)
      (redisplay t)
      (mevedel-view--call-preserving-window-state
       (lambda ()
         (goto-char (point-max))
         (insert "new output\n")))
      (should (= (window-point) (point-max)))
      (should (>= (window-end nil t) (point-max)))))

  :doc "windows browsing older output keep their point and start"
  (save-window-excursion
    (with-temp-buffer
      (dotimes (index 400)
        (insert (format "line %d\n" index)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (set-window-start nil (point-min))
      (redisplay t)
      (let ((point-before (point))
            (start-before (window-start)))
        (mevedel-view--call-preserving-window-state
         (lambda ()
           (goto-char (point-max))
           (insert "new output\n")))
        (should (= (point) point-before))
        (should (= (window-start) start-before)))))

  :doc "active selections survive delete-and-reinsert rendering"
  (dolist (backward '(nil t))
    (with-temp-buffer
      (setq-local transient-mark-mode t)
      (insert "before\nselected\nafter\n")
      (goto-char (point-min))
      (search-forward "selected")
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (goto-char (if backward start end))
        (set-mark (if backward end start)))
      (activate-mark)
      (let ((point-before (point))
            (mark-before (mark))
            (text (buffer-string)))
        (mevedel-view--call-preserving-window-state
         (lambda ()
           (delete-region (point-min) (point-max))
           (insert text)))
        (should mark-active)
        (should (= (point) point-before))
        (should (= (mark) mark-before))
        (should (equal "selected"
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))))))

  :doc "composer selections retain their offsets when history grows"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local transient-mark-mode t)
      (goto-char (mevedel-view--input-start))
      (insert "draft selection")
      (search-backward "selection")
      (set-mark (match-end 0))
      (activate-mark)
      (mevedel-view--call-preserving-window-state
       (lambda ()
         (let ((inhibit-read-only t))
           (goto-char (point-min))
           (insert "new history\n"))))
      (should mark-active)
      (should (equal "selection"
                     (buffer-substring-no-properties
                      (region-beginning) (region-end)))))))

(mevedel-deftest mevedel-view--input-text ()
  ,test
  (test)
  :doc "extracts text from input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string-empty-p (mevedel-view--input-text)))
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (should (equal "hello world" (mevedel-view--input-text)))))

  :doc "clear empties input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (mevedel-view--clear-input)
      (should (string-empty-p (mevedel-view--input-text))))))

(mevedel-deftest mevedel-view-slash-capf
  (:before-each
   (progn
     (clrhash mevedel-skills--dir-buffers)
     (clrhash mevedel-skills--dirty-buffers)
     (clrhash mevedel-skills--mtime-cache))
   :after-each
   (progn
     (clrhash mevedel-skills--dir-buffers)
     (clrhash mevedel-skills--dirty-buffers)
     (clrhash mevedel-skills--mtime-cache)))
  ,test
  (test)
  :doc "view skill completion refreshes after skill saves"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-view-skills-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-skills"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (mevedel-view-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session data-buf))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$")
            (let ((capf (mevedel-view-slash-capf)))
              (should (member "alpha"
                              (mevedel-view-test--capf-candidates capf)))
              (let ((skill-file
                     (mevedel-view-test--write-skill
                      root "bar" "name: bar\ndescription: B\n")))
                (with-temp-buffer
                  (setq buffer-file-name skill-file)
                  (mevedel-skills--before-save-hook)))
	              (should (member "bar"
	                              (mevedel-view-test--capf-candidates
	                               capf "b"))))))
	      (delete-directory root t)))

  :doc "view skill completion binds the exact discovered source"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-view-skill-binding-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-skill-binding"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (source-file
          (mevedel-view-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session data-buf))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$al")
            (let* ((capf (mevedel-view-slash-capf))
                   (exit (plist-get (nthcdr 3 capf) :exit-function)))
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert "alpha")
              (funcall exit "alpha" 'finished))
            (let* ((start (mevedel-view--input-start))
                   (binding (get-text-property
                             start 'mevedel-mention-binding)))
              (should (equal 'skill (plist-get binding :kind)))
              (should (equal "$alpha" (plist-get binding :token)))
              (should (equal source-file
                             (plist-get binding :source-file)))
              (goto-char (point-max))
              (insert "analyze")
              (should (equal binding
                             (get-text-property
                              start 'mevedel-mention-binding))))))
      (delete-directory root t)))

  :doc "view completes Goal commands and permission-mode arguments"
  (let* ((root (make-temp-file "mevedel-view-mode-capf-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-mode-capf"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "/go")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("goal")
                             (mevedel-view-test--capf-candidates
                              capf "go"))))
            (mevedel-view--clear-input)
            (goto-char (mevedel-view--input-start))
            (insert "/mode fu")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("full-auto")
                             (mevedel-view-test--capf-candidates
                              capf "fu"))))))
      (delete-directory root t)))

  :doc "view review command completes target arguments"
  (let* ((root (make-temp-file "mevedel-view-review-capf-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-review-capf"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "/review cur")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("current")
                             (mevedel-view-test--capf-candidates
                              capf "cur"))))))
      (delete-directory root t)))

  :doc "view root completion inserts a real separator before skill hint"
  (let* ((root (make-temp-file "mevedel-view-root-space-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-root-space"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "remember"
                 :argument-names '("focus"))))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$rem")
            (let* ((capf (mevedel-view-slash-capf))
                   (exit (and capf (plist-get (nthcdr 3 capf)
                                              :exit-function))))
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert "remember")
              (funcall exit "remember" 'finished)
              (mevedel-view--refresh-skill-argument-hint)
              (should (string-match-p
                       "\\[focus\\]"
                       (mevedel-view-test--skill-hint-string)))
              (insert "d")
              (should (equal "$remember d"
                             (mevedel-view--input-text))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view--refresh-skill-argument-hint ()
  ,test
  (test)
  :doc "argument-hint appears as overlay text before args"
  (let* ((root (make-temp-file "mevedel-view-skill-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-skill-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$green-loop")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "What change should be validated"
                     (mevedel-view-test--skill-hint-string)))
            (should (equal "$green-loop" (mevedel-view--input-text)))
            (insert " current changes")
            (mevedel-view--refresh-skill-argument-hint)
            (should-not (mevedel-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "argument names show only remaining slots"
  (let* ((root (make-temp-file "mevedel-view-named-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-named-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "deploy-api"
                 :argument-names '("service" "environment"))))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$deploy-api")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[service\\] \\[environment\\]"
                     (mevedel-view-test--skill-hint-string)))
            (insert " billing")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[environment\\]"
                     (mevedel-view-test--skill-hint-string)))
            (should-not (string-match-p
                         "\\[service\\]"
                         (mevedel-view-test--skill-hint-string)))))
      (delete-directory root t)))

  :doc "non-skill input clears the overlay"
  (let* ((root (make-temp-file "mevedel-view-clear-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-clear-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$green-loop")
            (mevedel-view--refresh-skill-argument-hint)
            (should (mevedel-view-test--skill-hint-string))
            (mevedel-view--clear-input)
            (insert "hello")
            (mevedel-view--refresh-skill-argument-hint)
            (should-not (mevedel-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "history-region refresh skips prompt scan"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel-view--input-start)
                 (lambda ()
                   (error "Prompt scan should be skipped"))))
        (mevedel-view--refresh-skill-argument-hint)
        (should-not mevedel-view--skill-argument-hint-overlay))))

  :doc "refresh repairs drifted input marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((prompt-start (mevedel-view--prompt-start-position))
            (input-start (mevedel-view--input-start)))
        (goto-char input-start)
        (insert "draft")
        (set-marker mevedel-view--input-marker (point-max))
        (goto-char input-start)
        (mevedel-view--refresh-skill-argument-hint)
        (should (= prompt-start
                   (marker-position mevedel-view--input-marker)))))))


;;
;;; mevedel-view-send slash-fork integration

(mevedel-deftest mevedel-view-send/local-slash-command ()
  ,test
  (test)
  :doc "local slash command string results are shown"
  (mevedel-view-test--with-buffers
    (let ((mevedel-slash-commands
           (list (cons "fake" (lambda (_args) "fake result"))))
          seen)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq seen (apply #'format fmt args)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "/fake")
          (mevedel-view-send)
          (should (equal "fake result" seen))
          (should (string-empty-p (mevedel-view--input-text)))))))
  :doc "/goal pause reaches the lifecycle handler during an active request"
  (mevedel-view-test--with-buffers
    (let* (seen
           (commands
            (list (cons "goal" (lambda (args) (setq seen args))))))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request t
                    mevedel-slash-commands commands))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "/goal pause")
        (mevedel-view-send)
        (should (equal "pause" seen))
        (should (string-empty-p (mevedel-view--input-text))))))
  :doc "/ps and /stop remain available during an active request"
  (dolist (command '("ps" "stop"))
    (mevedel-view-test--with-buffers
      (let* (seen
             (commands
              (list (cons command (lambda (args) (setq seen args))))))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request t
                      mevedel-slash-commands commands))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert (if (string= command "stop")
                      "/stop exec-000001"
                    "/ps"))
          (mevedel-view-send)
          (should (equal (if (string= command "stop") "exec-000001" "")
                         seen))
          (should (string-empty-p (mevedel-view--input-text))))))))
(mevedel-deftest mevedel-view-send/dollar-text ()
  ,test
  (test)
  :doc "unknown dollar-prefixed input sends as normal prompt text"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vd" :root "/tmp/vd" :name "vd"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$PATH is relevant")
          (mevedel-view-send)
          (should (string-empty-p (mevedel-view--input-text)))))
      (should send-called)
      (with-current-buffer data-buf
        (should (string-match-p "\\$PATH is relevant"
                                (buffer-string))))))

  :doc "history write failure warns without cancelling a valid view send"
  (let ((root (make-temp-file "mevedel-history-send-" t))
        warning
        send-called)
    (unwind-protect
        (let* ((ws (mevedel-workspace--create
                    :type 'test :id root :root root :name "history-send"
                    :file-cache (mevedel-file-cache--create
                                 :table (make-hash-table :test #'equal)
                                 :order nil :total-bytes 0)))
               (session (mevedel-session-create "main" ws)))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws))
            (with-current-buffer view-buf
              (mevedel-view-history-add "older input")
              (cl-letf (((symbol-function
                         'mevedel-session-persistence-write)
                         (lambda (&rest _)
                           (error "Simulated history write failure")))
                        ((symbol-function 'display-warning)
                         (lambda (_type message &rest _)
                           (setq warning message))))
                (mevedel-view-history-save))
              (goto-char (mevedel-view--input-start))
              (insert "valid prompt")
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _) (setq send-called t))))
                (mevedel-view-send)))
            (should (string-match-p "Input history save failed" warning))
            (should send-called)))
      (delete-directory root t))))

(defmacro mevedel-view-test--with-fork-skill (skill-form &rest body)
  "Install a source-backed SKILL-FORM, then run BODY in paired buffers.
Binds `data-buf', `view-buf', `session', and `skill' in scope."
  (declare (indent 1) (debug t))
  `(let* ((mevedel-skills-include-bundled nil)
          (root (make-temp-file "mevedel-view-skill-send-" t))
          (mevedel-skill-dirs (list root))
          (template ,skill-form)
          (source
           (mevedel-view-test--write-skill
            root
            (mevedel-skill-name template)
            (format "name: %s\ndescription: Test skill\ncontext: %s\n"
                    (mevedel-skill-name template)
                    (or (mevedel-skill-context template) 'inline))
            (mevedel-skill-body template)))
          (ws (mevedel-workspace--create
               :type 'test :id root :root root :name "view-skill-send"
               :file-cache (mevedel-file-cache--create
                            :table (make-hash-table :test #'equal)
                            :order nil :total-bytes 0)))
          (session (mevedel-session-create "main" ws)))
     (unwind-protect
         (mevedel-view-test--with-buffers
           (with-current-buffer data-buf
             (setq-local mevedel--session session
                         mevedel--workspace ws)
             (mevedel-skills-install session data-buf))
           (let ((skill (mevedel-session-get-skill
                         session (mevedel-skill-name template))))
             (should (equal source (mevedel-skill-source-file skill)))
             ,@body))
       (delete-directory root t))))

(defmacro mevedel-view-test--with-source-skills (specs &rest body)
  "Install source-backed SPECS and run BODY in paired session buffers.
Each spec is (NAME CONTEXT BODY &optional EXTRA-FRONTMATTER)."
  (declare (indent 1) (debug t))
  `(let* ((mevedel-skills-include-bundled nil)
          (root (make-temp-file "mevedel-view-planned-skills-" t))
          (mevedel-skill-dirs (list root))
          (ws (mevedel-workspace--create
               :type 'test :id root :root root :name "planned-skills"
               :file-cache (mevedel-file-cache--create
                            :table (make-hash-table :test #'equal)
                            :order nil :total-bytes 0)))
          (session (mevedel-session-create "main" ws)))
     (unwind-protect
         (progn
           (dolist (spec ,specs)
             (mevedel-view-test--write-skill
              root (nth 0 spec)
              (format "name: %s\ndescription: Test %s\ncontext: %s\n%s"
                      (nth 0 spec) (nth 0 spec) (nth 1 spec)
                      (or (nth 3 spec) ""))
              (nth 2 spec)))
           (mevedel-view-test--with-buffers
             (with-current-buffer data-buf
               (setq-local mevedel--session session
                           mevedel--workspace ws)
               (mevedel-skills-install session data-buf))
             ,@body))
       (delete-directory root t))))

(defun mevedel-view-test--count-matches (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))


;;
;;; Planned submission helpers

(mevedel-deftest mevedel-view--pending-input-text ()
  ,test
  (test)
  :doc "returns queued input and defaults a missing value to empty text"
  (should (equal "queued" (mevedel-view--pending-input-text
                            '(:input "queued"))))
  (should (equal "" (mevedel-view--pending-input-text nil))))

(mevedel-deftest mevedel-view--pending-input-category-body ()
  ,test
  (test)
  :doc "shows three compact previews and a remaining count"
  (let ((body
         (mevedel-view--pending-input-category-body
          "Steering"
          (mapcar (lambda (n)
                    (list :input
                          (format "message %d\nwith extra whitespace" n)))
                  '(1 2 3 4 5)))))
    (dolist (n '(1 2 3))
      (should (string-match-p (format "message %d with extra" n) body)))
    (should-not (string-match-p "message 4" body))
    (should (string-match-p "2 more" body))))

(mevedel-deftest mevedel-view--cancel-pending-skill-submission ()
  ,test
  (test)
  :doc "marks the active preparation token cancelled and clears ownership"
  (with-temp-buffer
    (let ((token (list :cancelled nil)))
      (setq-local mevedel-view--pending-skill-submission token)
      (mevedel-view--cancel-pending-skill-submission)
      (should (plist-get token :cancelled))
      (should-not mevedel-view--pending-skill-submission))))

(mevedel-deftest mevedel-view--skill-submission-active-p ()
  ,test
  (test)
  :doc "requires live buffers, current ownership, and a non-cancelled token"
  (mevedel-view-test--with-buffers
    (let ((token (list :cancelled nil)))
      (with-current-buffer view-buf
        (setq-local mevedel-view--pending-skill-submission token))
      (should (mevedel-view--skill-submission-active-p
               token view-buf data-buf))
      (setf (plist-get token :cancelled) t)
      (should-not (mevedel-view--skill-submission-active-p
                   token view-buf data-buf))
      (setf (plist-get token :cancelled) nil)
      (with-current-buffer view-buf
        (setq mevedel-view--pending-skill-submission (list :cancelled nil)))
      (should-not (mevedel-view--skill-submission-active-p
                   token view-buf data-buf)))))

(mevedel-deftest mevedel-view--finish-skill-submission ()
  ,test
  (test)
  :doc "clears only the token that owns the pending submission"
  (with-temp-buffer
    (let ((token (list :cancelled nil))
          (other (list :cancelled nil)))
      (setq-local mevedel-view--pending-skill-submission token)
      (mevedel-view--finish-skill-submission other)
      (should (eq token mevedel-view--pending-skill-submission))
      (mevedel-view--finish-skill-submission token)
      (should-not mevedel-view--pending-skill-submission))))

(mevedel-deftest mevedel-view--prepared-fork-outcome ()
  ,test
  (test)
  :doc "returns the first prepared fork outcome and nil when none exists"
  (let ((fork '(:status ok :kind fork :body "fork")))
    (should
     (eq fork
         (mevedel-view--prepared-fork-outcome
          (list :prepared-entries
                (list '(:outcome (:status ok :kind inline))
                      (list :outcome fork))))))
    (should-not
     (mevedel-view--prepared-fork-outcome
      '(:prepared-entries ((:outcome (:status ok :kind instruction))))))))

(mevedel-deftest mevedel-view--block-planned-submission ()
  ,test
  (test)
  :doc "clears ownership, reports preparation failure, and invokes on-block"
  (with-temp-buffer
    (let ((token (list :cancelled nil))
          blocked
          notice)
      (setq-local mevedel-view--pending-skill-submission token)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq notice (apply #'format format-string args)))))
        (mevedel-view--block-planned-submission
         (list :token token :on-block (lambda () (setq blocked t)))
         '(:status error :name "alpha" :message "failed")))
      (should blocked)
      (should-not mevedel-view--pending-skill-submission)
      (should (equal "mevedel: skill $alpha failed: failed" notice)))))

(mevedel-deftest mevedel-view--prepared-plan-outcome ()
  ,test
  (test)
  :doc "returns every prepared model, transcript, policy, and audit component"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (mevedel-skills-plan-user-input input session))
           prepared)
      (with-current-buffer data-buf
        (mevedel-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let ((outcome
             (mevedel-view--prepared-plan-outcome
              (list :input input :plan plan)
              prepared
              (plist-get prepared :model-input)
              "expansion context\n\nhook context"
              '((:type prompt-rewrite)))))
        (should (string-match-p "ALPHA BODY"
                                (plist-get outcome :model-input)))
        (should (string-match-p "hook context"
                                (plist-get outcome :transcript-input)))
        (should (string-match-p "expansion context"
                                (plist-get outcome :transcript-input)))
        (should (plist-get outcome :request-context))
        (let* ((block (plist-get outcome :render-data))
               (data (cdr (mevedel-pipeline-extract-render-data block))))
          (should (equal (plist-get prepared :model-input)
                         (plist-get data :expanded-prompt)))
          (should-not (string-match-p
                       "hook context"
                       (plist-get data :expanded-prompt))))
        (should (equal '((:type prompt-rewrite))
                       (plist-get outcome :hook-audits)))
        (should-not (plist-get outcome :fork-outcome))))))

(mevedel-deftest mevedel-view--dispatch-prepared-plan ()
  ,test
  (test)
  :doc "dispatches a source-backed prepared plan while preserving additions"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (mevedel-skills-plan-user-input input session))
           prepared
           forwarded
           before)
      (with-current-buffer data-buf
        (mevedel-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let* ((token (list :cancelled nil))
             (plan-submission
              (list :token token :input input :plan plan
                    :view-buffer view-buf :data-buffer data-buf
                    :before-send (lambda () (setq before t))))
             (prompt-submission
              (mevedel-prompt-submission-create
               :input (concat (plist-get prepared :model-input) " $literal")
               :display-text input)))
        (with-current-buffer view-buf
          (setq-local mevedel-view--pending-skill-submission token)
          (cl-letf (((symbol-function 'mevedel-view--forward-input)
                     (lambda (&rest args) (setq forwarded args))))
            (mevedel-view--dispatch-prepared-plan
             plan-submission prepared prompt-submission))
          (should-not mevedel-view--pending-skill-submission))
        (should before)
        (should (string-match-p "ALPHA BODY"
                                (plist-get (cdr forwarded) :model-input)))
        (should (string-match-p (regexp-quote "$literal")
                                (plist-get (cdr forwarded) :model-input)))
        (with-current-buffer data-buf
          (should (equal (plist-get prepared :request-context)
                         mevedel-skills--pending-request-context)))))))

(mevedel-deftest mevedel-view--handle-prepared-plan ()
  ,test
  (test)
  :doc "runs the prompt gate and dispatches a successful prepared plan"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (mevedel-skills-plan-user-input input session))
           prepared
           forwarded)
      (with-current-buffer data-buf
        (mevedel-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let* ((token (list :cancelled nil))
             (submission
              (list :token token :input input :plan plan
                    :view-buffer view-buf :data-buffer data-buf)))
        (with-current-buffer view-buf
          (setq-local mevedel-view--pending-skill-submission token)
          (cl-letf (((symbol-function 'mevedel-view--forward-input)
                     (lambda (&rest args) (setq forwarded args))))
            (mevedel-view--handle-prepared-plan submission prepared)))
        (should (string-match-p "ALPHA BODY"
                                (plist-get (cdr forwarded) :model-input)))))))

(mevedel-deftest mevedel-view--submit-planned-input ()
  ,test
  (test)
  :doc "plans and submits source-backed instructions through the view seam"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (sent before blocked)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (mevedel-view--submit-planned-input
           "Use $alpha"
           (lambda () (setq before t))
           (lambda () (setq blocked t)))))
      (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
      (should before)
      (should-not blocked)
      (should (string-match-p "ALPHA BODY" sent))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") sent)))))

(mevedel-deftest mevedel-view--steering-request-context-supported-p ()
  ,test
  (test)
  :doc "allows bookkeeping-only skill context and rejects request policy"
  (should
   (mevedel-view--steering-request-context-supported-p
    '(:permission-rules nil :hook-rules nil :invoked-skills (alpha))))
  (dolist (context '((:permission-rules (rule))
                     (:hook-rules (rule))
                     (:model model)
                     (:effort high)))
    (should-not
     (mevedel-view--steering-request-context-supported-p context)))
  (should-not
   (mevedel-view--steering-request-context-supported-p
    '(:future-policy nil))))

(mevedel-deftest mevedel-view-send/skill-inline ()
  ,test
  (test)
  :doc "inline attachment reaches the model-visible gptel request"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "thermo-review"
       :body "THERMO BODY"
       :context 'inline
       :user-invocable-p t
       :model-invocable-p nil)
    (let (request-data)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq request-data
                         (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Run $thermo-review on the repository")
          (mevedel-view-send)))
      (should (string-search "THERMO BODY" request-data))
      (should (string-search "[skill:thermo-review -- attached]"
                             request-data))))

  :doc "inline skill forwards expanded body with render-data side channel"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let (send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should-not (string-match-p "Expanded hello" text))))
        (with-current-buffer view-buf
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p (regexp-quote "$myskill hello")
                                    expanded))
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "mevedel-render-data" expanded)))
          (mevedel-view-toggle-section))
        (should send-called)
        (with-current-buffer data-buf
          (let* ((text (buffer-string))
                 (visible
                  (mevedel-pipeline--strip-render-data-blocks text)))
            (should (string-match-p (regexp-quote "$myskill hello") text))
            (should-not (string-match-p "Expanded hello" visible))
            (should (string-search "<!-- mevedel-render-data -->" text))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (should (eq 'ignore
                        (get-text-property (match-beginning 0)
                                           'gptel)))
            (should (string-match-p
                     (regexp-quote "$myskill hello")
                     (mevedel-pipeline--strip-render-data-blocks text)))))
	        (with-current-buffer view-buf
	          (mevedel-view--full-rerender)
	          (let ((text (buffer-substring-no-properties
	                       (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "\\$myskill hello" text))
	            (should (string-match-p "Prompt" text))
	            (should-not (string-match-p "Expanded hello" text)))
	          (goto-char (point-min))
	          (search-forward "◆ Prompt")
	          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
	            (should (string-match-p (regexp-quote "$myskill hello")
                                      expanded))
	            (should (string-match-p "Expanded hello" expanded)))))))

  :doc "inline skill expansion rewrites render hook audit and context"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptExpansion
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook changed prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "hook policy context" text)))
          (goto-char (point-min))
          (search-forward "hook changed prompt")
          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "UserPromptExpansion" expanded))
	            (should (string-match-p "Expanded hello" expanded))
	            (should (string-match-p "rewritten prompt" expanded)))
	          (goto-char (point-min))
	          (search-forward "hook changed prompt")
	          (mevedel-view-toggle-section)
	          (goto-char (point-min))
	          (search-forward "◆ Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "hook policy context" expanded)))))
      (with-current-buffer data-buf
        (let* ((text (buffer-string))
               (visible
                (mevedel-pipeline--strip-render-data-blocks text)))
          (should (string-match-p (regexp-quote "$myskill hello") visible))
          (should-not (string-match-p "rewritten prompt" visible))
          (should (string-match-p "hook policy context" visible))
          (should (string-match-p "<!-- mevedel-hook-audit -->" visible))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "hook changed prompt" text))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "hook policy context" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (mevedel-view-toggle-section)
        (should (string-match-p
                 "hook policy context"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "manually typed queued mention keeps its exact source and latest body"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-inline-bound-queue-" t))
         (project-skills (file-name-concat root ".mevedel/skills"))
         (user-skills (make-temp-file "mevedel-inline-bound-user-" t))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "inline-bound-queue"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         source-file
         request-data)
    (unwind-protect
        (progn
          (setq source-file
                (mevedel-view-test--write-skill
                 project-skills "alpha"
                 "name: alpha\ndescription: Project alpha\ncontext: inline\n"
                 "ORIGINAL ALPHA V1"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws
                          mevedel--current-request
                          (mevedel-request--create :session session))
              (mevedel-skills-install session data-buf))
            (cl-letf (((symbol-function
                        'mevedel-view--schedule-late-follow-up-drain)
                       #'ignore)
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (goto-char (mevedel-view--input-start))
                (insert "Please use $alpha for the queued analysis")
                (mevedel-view-send-follow-up))
              (let* ((queue (mevedel-session-pending-follow-ups session))
                     (input (plist-get (car queue) :input))
                     (start (string-match "\\$alpha" input))
                     (binding (and start
                                   (get-text-property
                                    start 'mevedel-mention-binding input))))
                (should (= 1 (length queue)))
                (should (equal source-file
                               (plist-get binding :source-file))))
              (mevedel-view-test--write-skill
               user-skills "alpha"
               "name: alpha\ndescription: User alpha\ncontext: inline\n"
               "COMPETING ALPHA")
              (setq mevedel-skill-dirs
                    (list ".mevedel/skills" user-skills))
              (delete-file source-file)
              (with-current-buffer data-buf
                (setq-local mevedel--current-request nil))
              (mevedel-view--drain-follow-up data-buf))
            (should (string-search "[skill:alpha -- unavailable]"
                                   request-data))
            (should-not (string-search "ORIGINAL ALPHA V1" request-data))
            (should-not (string-search "COMPETING ALPHA" request-data))
            (should-not (mevedel-session-pending-follow-ups session))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "completion binding survives outside edits after a name collision"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-inline-bound-edit-" t))
         (project-skills (file-name-concat root ".mevedel/skills"))
         (user-skills (make-temp-file "mevedel-inline-bound-edit-user-" t))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "inline-bound-edit"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (mevedel-view-test--write-skill
           project-skills "alpha"
           "name: alpha\ndescription: Project alpha\ncontext: inline\n"
           "PROJECT ALPHA BODY")
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws)
              (mevedel-skills-install session data-buf))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Please use $al")
              (mevedel-view-test--complete-skill "alpha")
              (insert "for details"))
            (mevedel-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING ALPHA BODY")
            (setq mevedel-skill-dirs
                  (list ".mevedel/skills" user-skills))
            (with-current-buffer data-buf
              (mevedel-skills-install session data-buf))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (mevedel-view-send)))
            (should (string-search "PROJECT ALPHA BODY" request-data))
            (should-not (string-search "COMPETING ALPHA BODY"
                                       request-data))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "editing inside a completion-bound token resolves the edited skill"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-inline-bound-rebind-" t))
         (skill-root (file-name-concat root ".mevedel/skills"))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "inline-bound-rebind"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (mevedel-view-test--write-skill
           skill-root "alpha"
           "name: alpha\ndescription: Alpha\ncontext: inline\n"
           "ALPHA BODY")
          (mevedel-view-test--write-skill
           skill-root "beta"
           "name: beta\ndescription: Beta\ncontext: inline\n"
           "BETA BODY")
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws)
              (mevedel-skills-install session data-buf))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Please use $al")
              (mevedel-view-test--complete-skill "alpha")
              (goto-char (mevedel-view--input-start))
              (search-forward "$alpha")
              (replace-match "$beta" t t))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (mevedel-view-send)))
            (should (string-search "BETA BODY" request-data))
            (should-not (string-search "ALPHA BODY" request-data))))
      (delete-directory root t)))

  :doc "copy and yank preserve the exact binding on both occurrences"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (first second)
      (with-current-buffer view-buf
        (let ((kill-ring nil))
          (goto-char (mevedel-view--input-start))
          (insert "Use $al")
          (mevedel-view-test--complete-skill "alpha")
          (setq first
                (get-text-property
                 (+ (mevedel-view--input-start) 4)
                 'mevedel-mention-binding))
          (kill-ring-save (+ (mevedel-view--input-start) 4) (point-max))
          (goto-char (point-max))
          (insert " and ")
          (yank)
          (setq second
                (get-text-property
                 (- (point-max) 6) 'mevedel-mention-binding))))
      (should (equal first second))))

  :doc "persisted history recall submits the latest exact source after a collision"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-inline-bound-history-" t))
         (project-skills (file-name-concat root ".mevedel/skills"))
         (user-skills
          (make-temp-file "mevedel-inline-bound-history-user-" t))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "inline-bound-history"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (mevedel-view-test--write-skill
           project-skills "alpha"
           "name: alpha\ndescription: Project alpha\ncontext: inline\n"
           "PROJECT HISTORY BODY V1")
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace ws)
              (mevedel-skills-install session data-buf))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Recall $al")
              (mevedel-view-test--complete-skill "alpha")
              (mevedel-view-history-add (mevedel-view--input-text))
              (mevedel-view-history-save)
              (setq mevedel-view-history--ring nil
                    mevedel-view-history--loaded-entries nil)
              (mevedel-view-history-load session)
              (mevedel-view--clear-input)
              (mevedel-view-history-previous))
            (mevedel-view-test--write-skill
             project-skills "alpha"
             "name: alpha\ndescription: Project alpha\ncontext: inline\n"
             "PROJECT HISTORY BODY V2")
            (mevedel-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING HISTORY BODY")
            (setq mevedel-skill-dirs
                  (list ".mevedel/skills" user-skills))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (mevedel-view-send)))
            (should (string-search "PROJECT HISTORY BODY V2" request-data))
            (should-not (string-search "PROJECT HISTORY BODY V1"
                                       request-data))
            (should-not (string-search "COMPETING HISTORY BODY"
                                       request-data))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "source-backed bindings remain live in a session without a workspace"
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-inline-bound-live-" t))
         (skill-root (file-name-concat root "skills"))
         (mevedel-skill-dirs (list skill-root))
         (session (mevedel-session--create
                   :name "main"
                   :working-directory root
                   :skills-snapshot :uninitialized
                   :turn-count 0))
         source-file
         request-data)
    (unwind-protect
        (progn
          (setq source-file
                (mevedel-view-test--write-skill
                 skill-root "alpha"
                 "name: alpha\ndescription: Live alpha\ncontext: inline\n"
                 "LIVE ALPHA BODY"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (mevedel-skills-install session data-buf))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (goto-char (mevedel-view--input-start))
                (insert "Use $alpha live")
                (mevedel-view-send)
                (let* ((entry (car (mevedel-view-history--entries)))
                       (start (string-match "\\$alpha" entry)))
                  (should (equal source-file
                                 (plist-get
                                  (get-text-property
                                   start 'mevedel-mention-binding entry)
                                  :source-file))))))
            (should (string-search "LIVE ALPHA BODY" request-data))))
      (delete-directory root t)))

  :doc "mixed unavailable targets annotate safely and dispatch once"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA SECRET"))
    (let* ((source (mevedel-skill-source-file
                    (mevedel-session-get-skill session "alpha")))
           (file (file-name-concat root "missing.txt"))
           (prompt
            "Use $alpha @ref:2 @file:missing.txt @mcp:docs:file:///guide")
           (specs
            (list
             (list "$alpha"
                   (list :kind 'skill :token "$alpha" :source-file source))
             (list "@ref:2"
                   '(:kind ref :token "@ref:2" :reference-uuid "uuid-2"))
             (list "@file:missing.txt"
                   (list :kind 'file :token "@file:missing.txt" :path file))
             (list "@mcp:docs:file:///guide"
                   '(:kind mcp :token "@mcp:docs:file:///guide"
                     :server "docs" :uri "file:///guide"))))
           (gptel-prompt-transform-functions
            (cons #'mevedel--transform-expand-mentions
                  (remove #'mevedel--transform-expand-mentions
                          gptel-prompt-transform-functions)))
           request-data messages (send-count 0))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert prompt)
        (dolist (spec specs)
          (goto-char (mevedel-view--input-start))
          (search-forward (car spec))
          (mevedel-mention-bindings-set
           (match-beginning 0) (match-end 0) (cadr spec))))
      (delete-file source)
      (cl-letf (((symbol-function 'mcp-hub-get-servers)
                 (lambda ()
                   (list (list :name "docs" :status 'stop))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (let ((text (apply #'format format-string args)))
                     (when (string-prefix-p "mevedel:" text)
                       (push text messages)))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (cl-incf send-count)
                   (setq request-data
                         (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (mevedel-view-send)))
      (should (= 1 send-count))
      (dolist (annotation '("[skill:alpha -- unavailable]"
                            "[ref:2 -- unavailable]"
                            "[file:missing.txt -- does not exist]"
                            "[mcp:docs:file:///guide -- server"))
        (should (string-search annotation request-data)))
      (should-not (string-search "ALPHA SECRET" request-data))
      (dolist (fragment '("bound skill" "reference" "file" "MCP"))
        (should (seq-some (lambda (text) (string-search fragment text))
                          messages)))
      (with-current-buffer data-buf
        (let ((text (mevedel-pipeline--strip-render-data-blocks
                     (buffer-string))))
          (should (string-search prompt text))
          (should-not (string-search "[skill:alpha -- unavailable]"
                                     text))))
      (should (equal prompt (car (with-current-buffer view-buf
                                   (mevedel-view-history--entries)))))))

  :doc "malformed mixed bindings block submission and preserve the draft"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "alpha"
       :body "ALPHA BODY"
       :context 'inline
       :user-invocable-p t)
    (let (send-called raised)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (let ((start (point))
                (prompt "Use $alpha @file:/tmp/a @mcp:docs:file:///api"))
            (insert prompt)
            (with-silent-modifications
              (add-text-properties
               (+ start 4) (+ start 10)
               '(mevedel-mention-binding
                 (:kind skill :token "$alpha")))
              (dolist (spec
                       '(("@file:/tmp/a"
                          (:kind file :token "@file:/tmp/a" :path "/tmp/a"))
                         ("@mcp:docs:file:///api"
                          (:kind mcp :token "@mcp:docs:file:///api"
                           :server "docs" :uri "file:///api"))))
                (goto-char start)
                (search-forward (car spec))
                (mevedel-mention-bindings-set
                 (match-beginning 0) (match-end 0) (cadr spec)))))
          (setq raised
                (condition-case err
                    (progn (mevedel-view-send) nil)
                  (user-error (error-message-string err))))
          (should (equal "Malformed mention binding" raised))
          (should-not send-called)
          (should (equal "Use $alpha @file:/tmp/a @mcp:docs:file:///api"
                         (mevedel-view--input-text)))))))

  :doc "missing exact source warns, annotates, and sends the multiline turn"
  (mevedel-view-test--bound-source-failure-case 'missing)

  :doc "disabled exact source warns, annotates, and sends the multiline turn"
  (mevedel-view-test--bound-source-failure-case 'disabled)

  :doc "unreadable exact source warns, annotates, and sends the multiline turn"
  (mevedel-view-test--bound-source-failure-case 'unreadable)

  :doc "malformed exact source warns, annotates, and sends the multiline turn"
  (mevedel-view-test--bound-source-failure-case 'malformed)

  :doc "non-invocable exact source warns, annotates, and sends the multiline turn"
  (mevedel-view-test--bound-source-failure-case 'not-user-invocable))

(mevedel-deftest mevedel-view-send/planned-skills ()
  ,test
  (test)

  :doc "embedded mentions are instructions while a leading mention is a command"
  (dolist (case '(("Analyze with $alpha" . "ALPHA ARGS=<>")
                  ("$alpha analyze now" . "ALPHA ARGS=<analyze now>")))
    (mevedel-view-test--with-source-skills
        '(("alpha" "inline" "ALPHA ARGS=<$ARGUMENTS>"))
      (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _)
                     (setq sent (mevedel-view-test--dry-run-request-data)))))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert (car case))
            (mevedel-view-send)))
        (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
        (should (string-match-p (regexp-quote (cdr case)) sent))
        (if (string-prefix-p "$" (car case))
            (should-not (string-match-p "skill:alpha -- attached" sent))
          (should (string-match-p
                   (regexp-quote "[skill:alpha -- attached]") sent))))))

  :doc "one leading command owns the pending request model policy"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA" "model: \"fast\"\n"))
    (let (context)
      (should (equal "fast"
                     (mevedel-skill-model
                      (mevedel-session-get-skill session "alpha"))))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree mevedel-skills--pending-request-context)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$alpha inspect")
          (mevedel-view-send)))
      (should (equal '(:tier fast) (plist-get context :model)))
      (should (plist-member context :effort))
      (should-not (plist-get context :effort))))

  :doc "qualified preset policy reaches the realized prompt request"
  (mevedel-skills-test--with-model-backends
    (let* ((mevedel-skills-include-bundled nil)
           (mevedel-skills-check-for-modifications nil)
           (root (make-temp-file "mevedel-view-skill-policy-" t))
           (project-skills (file-name-concat root ".mevedel/skills"))
           (user-skills (make-temp-file "mevedel-view-skill-policy-user-" t))
           (mevedel-skill-dirs (list ".mevedel/skills" user-skills))
           (mevedel-model-workloads
            '(($local:alpha :provider "Fast:fast-model" :effort high)))
           (ws (mevedel-workspace--create
                :type 'test :id root :root root :name "skill-policy"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (old-custom (get 'gptel-reasoning-effort 'custom-type))
           (old-effort (get 'fast-model :reasoning-effort))
           effective)
      (unwind-protect
          (progn
            (put 'gptel-reasoning-effort 'custom-type '(choice symbol))
            (put 'fast-model :reasoning-effort '(member low high))
            (mevedel-view-test--write-skill
             project-skills "alpha"
             "name: alpha\ndescription: Local alpha\ncontext: inline\nmodel: \"superseded\"\n"
             "LOCAL ALPHA")
            (mevedel-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: Global alpha\ncontext: inline\n"
             "GLOBAL ALPHA")
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--session session
                            mevedel--workspace ws
                            gptel-backend (gptel-get-backend "Balanced")
                            gptel-model 'balanced-model
                            gptel-reasoning-effort 'low
                            gptel-prompt-transform-functions
                            (cons
                             #'mevedel-skills--transform-apply-request-model-policy
                             gptel-prompt-transform-functions))
                (mevedel-skills-install session data-buf))
              (let ((transform
                     (symbol-function
                      'mevedel-skills--transform-apply-request-model-policy)))
                (cl-letf
                    (((symbol-function
                       'mevedel-skills--transform-apply-request-model-policy)
                      (lambda (fsm)
                        (funcall transform fsm)
                        (setq effective
                              (list (gptel-backend-name gptel-backend)
                                    gptel-model
                                    gptel-reasoning-effort))))
                     ((symbol-function 'gptel-send)
                      (lambda (&rest _)
                        (mevedel-view-test--dry-run-request-data))))
                  (with-current-buffer view-buf
                    (goto-char (mevedel-view--input-start))
                    (insert "$local:alpha inspect")
                    (mevedel-view-send))))
              (should (equal '("Fast" fast-model high) effective))))
        (put 'gptel-reasoning-effort 'custom-type old-custom)
        (put 'fast-model :reasoning-effort old-effort)
        (delete-directory root t)
        (delete-directory user-skills t))))

  :doc "command stacks ignore malformed policy and retain the session policy"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"
         "model: \"invalid-alpha\"\neffort: impossible\n")
        ("beta" "inline" "BETA"
         "model: \"invalid-beta\"\neffort: impossible\n"))
    (let (context sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree mevedel-skills--pending-request-context)
                         sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$alpha $beta inspect")
          (mevedel-view-send)))
      (should sent)
      (should-not (plist-member context :model))
      (should-not (plist-member context :effort))))

  :doc "embedded instructions ignore malformed policy and retain session policy"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"
         "model: \"invalid-alpha\"\neffort: impossible\n"))
    (let (context sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree mevedel-skills--pending-request-context)
                         sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha to inspect")
          (mevedel-view-send)))
      (should sent)
      (should-not (plist-member context :model))
      (should-not (plist-member context :effort))))

  :doc "repeated instruction mentions render twice but prepare once"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "UNIQUE ALPHA BODY"))
    (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha, then $alpha again")
          (mevedel-view-send)))
      (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
      (should (= 1 (mevedel-view-test--count-matches
                    "UNIQUE ALPHA BODY" sent)))
      (should (= 2 (mevedel-view-test--count-matches
                    (regexp-quote "[skill:alpha -- attached]") sent)))))

  :doc "command stacks share arguments and -- starts instruction parsing"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA<$ARGUMENTS>")
        ("beta" "inline" "BETA<$ARGUMENTS>")
        ("delta" "inline" "DELTA<$ARGUMENTS>"))
    (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$alpha $alpha $beta -- $delta details")
          (mevedel-view-send)))
      (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
      (should (= 1 (mevedel-view-test--count-matches "ALPHA<" sent)))
      (should (= 1 (mevedel-view-test--count-matches "BETA<" sent)))
      (should (= 1 (mevedel-view-test--count-matches "DELTA<>" sent)))
      (should (= 2 (mevedel-view-test--count-matches
                    (regexp-quote
                     "<[skill:delta -- attached] details>") sent)))))

  :doc "only six distinct leading skills become commands"
  (mevedel-view-test--with-source-skills
      '(("a" "inline" "A<$ARGUMENTS>")
        ("b" "inline" "B<$ARGUMENTS>")
        ("c" "inline" "C<$ARGUMENTS>")
        ("d" "inline" "D<$ARGUMENTS>")
        ("e" "inline" "E<$ARGUMENTS>")
        ("f" "inline" "F<$ARGUMENTS>")
        ("g" "inline" "G<$ARGUMENTS>"))
    (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$a $b $c $d $e $f $g rest")
          (mevedel-view-send)))
      (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
      (dolist (body '("A<" "B<" "C<" "D<" "E<" "F<"))
        (should (string-match-p body sent)))
      (should (string-match-p "G<>" sent))
      (should (= 6 (mevedel-view-test--count-matches
                    (regexp-quote "<[skill:g -- attached] rest>") sent)))))

  :doc "leading forks dispatch once while later forks remain instructions"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA<$ARGUMENTS>")
        ("forker" "fork" "FORK<$ARGUMENTS>"))
    (let (fork-prompt sent)
      (cl-letf (((symbol-function 'mevedel-skills-dispatch-prepared-fork)
                 (lambda (_prepared _callback &rest keys)
                   (setq fork-prompt (plist-get keys :prompt))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$forker inspect with $alpha")
          (mevedel-view-send))
        (should (string-match-p "FORK<inspect with" fork-prompt))
        (should (string-match-p
                 (regexp-quote "[skill:alpha -- attached]") fork-prompt))
        (should-not sent))
      (with-current-buffer data-buf
        (let ((text (mevedel-pipeline--strip-render-data-blocks
                     (buffer-string))))
          (should (string-match-p (regexp-quote "$forker inspect with $alpha")
                                  text))
          (should-not (string-match-p "FORK<" text)))
        (goto-char (point-min))
        (search-forward "$forker")
        (should (get-text-property
                 (match-beginning 0) 'mevedel-mention-binding))))
    (with-current-buffer data-buf
      (setq-local mevedel--current-request nil))
    (with-current-buffer view-buf
      (mevedel-view--stop-request-progress)
      (mevedel-view--clear-input))
    (let (fork-called sent)
      (cl-letf (((symbol-function 'mevedel-skills-dispatch-prepared-fork)
                 (lambda (&rest _) (setq fork-called t)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (mevedel-view--clear-input)
          (goto-char (mevedel-view--input-start))
          (insert "$alpha $forker inspect")
          (mevedel-view-send))
        (should-not fork-called)
        (should (string-match-p "FORK<>" sent)))))

  :doc "fork dispatch failure consumes context already stored in its turn"
  (mevedel-view-test--with-source-skills
      '(("forker" "fork" "FORK<$ARGUMENTS>"))
    (mevedel-hooks-record-session-context
     session '(:additional-context ("fork startup context")) 'SessionStart)
    (cl-letf (((symbol-function 'mevedel-skills-dispatch-prepared-fork)
               (lambda (&rest _) (error "Fork dispatch failed"))))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "$forker inspect")
        (mevedel-view-send)))
    (should-not (mevedel-session-hook-context-pending session))
    (with-current-buffer data-buf
      (should (= 1 (mevedel-view-test--count-matches
                    "fork startup context"
                    (mevedel-pipeline--strip-render-data-blocks
                     (buffer-string)))))))

  :doc "preparation failure preserves the bound draft for retry"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA")
        ("beta" "inline" "BETA"))
    (let ((fail-beta t)
          calls sent first-binding second-binding)
      (cl-letf (((symbol-function 'mevedel-skills-prepare)
                 (lambda (skill _arguments callback &rest _)
                   (push (mevedel-skill-name skill) calls)
                   (funcall callback
                            (if (and fail-beta
                                     (equal "beta"
                                            (mevedel-skill-name skill)))
                                '(:status error :reason blocked
                                  :message "blocked")
                              '(:status ok :kind instruction :body "ALPHA"
                                :request-context (:invoked-skills nil))))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha and $beta")
          (mevedel-view-send)
          (setq first-binding
                (get-text-property
                 (+ (mevedel-view--input-start) 4)
                 'mevedel-mention-binding))
          (should (equal "Use $alpha and $beta"
                         (mevedel-view--input-text)))
          (should-not (mevedel-view-history--entries))
          (setq fail-beta nil
                calls nil)
          (mevedel-view-send)
          (setq second-binding
                (get-text-property
                 4 'mevedel-mention-binding
                 (car (mevedel-view-history--entries))))))
      (should sent)
      (should (equal first-binding second-binding))))

  :doc "mixed locators survive retry, queueing, history, and recall"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let* ((source (mevedel-skill-source-file
                    (mevedel-session-get-skill session "alpha")))
           (file (file-name-concat root "missing.txt"))
           (prompt
            "Use $alpha @ref:2 @file:missing.txt @mcp:docs:file:///guide")
           (specs
            (list
             (list "$alpha"
                   (list :kind 'skill :token "$alpha" :source-file source))
             (list "@ref:2"
                   '(:kind ref :token "@ref:2" :reference-uuid "uuid-2"))
             (list "@file:missing.txt"
                   (list :kind 'file :token "@file:missing.txt" :path file))
             (list "@mcp:docs:file:///guide"
                   '(:kind mcp :token "@mcp:docs:file:///guide"
                     :server "docs" :uri "file:///guide"))))
           (expected (mapcar #'cadr specs))
           (fail t))
      (cl-letf (((symbol-function 'mevedel-skills-prepare)
                 (lambda (_skill _arguments callback &rest _)
                   (funcall callback
                            (if fail
                                '(:status error :reason blocked
                                  :message "blocked")
                              '(:status ok :kind instruction :body "ALPHA"
                                :request-context (:invoked-skills nil))))))
                ((symbol-function
                  'mevedel-view--schedule-late-follow-up-drain)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (error "Gptel-send should not run"))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert prompt)
          (dolist (spec specs)
            (goto-char (mevedel-view--input-start))
            (search-forward (car spec))
            (mevedel-mention-bindings-set
             (match-beginning 0) (match-end 0) (cadr spec)))
          (mevedel-view-send)
          (should (equal prompt (mevedel-view--input-text)))
          (should
           (equal expected
                  (mapcar (lambda (range) (plist-get range :binding))
                          (mevedel-mention-bindings-ranges
                           (mevedel-view--input-text)))))
          (setq fail nil)
          (with-current-buffer data-buf
            (setq-local mevedel--current-request
                        (mevedel-request--create :session session)))
          (mevedel-view-send-follow-up)
          (let ((queued
                 (plist-get
                  (car (mevedel-session-pending-follow-ups session))
                  :input))
                (history (car (mevedel-view-history--entries))))
            (dolist (text (list queued history))
              (should (equal prompt text))
              (should
               (equal expected
                      (mapcar (lambda (range)
                                (plist-get range :binding))
                              (mevedel-mention-bindings-ranges text))))))
          (mevedel-view-history-previous)
          (should
           (equal expected
                  (mapcar (lambda (range) (plist-get range :binding))
                          (mevedel-mention-bindings-ranges
                           (mevedel-view--input-text)))))))))

  :doc "submit hooks see the complete inert prompt and added skill text stays literal"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY")
        ("beta" "inline" "BETA BODY"))
    (let (seen sent)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event event-plist callback &rest _)
                   (if (eq event 'UserPromptSubmit)
                       (progn
                         (setq seen (plist-get event-plist :prompt))
                         (funcall callback
                                  (list :updated-input
                                        (concat seen " $beta"))))
                     (funcall callback nil))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha")
          (mevedel-view-send)))
      (should (string-match-p "ALPHA BODY" seen))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") seen))
      (should (string-match-p (regexp-quote "$beta") sent))
      (should-not (string-match-p "BETA BODY" sent))))

  :doc "submit hook rewrites cannot remove prepared instructions or placeholders"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (sent)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _event-plist callback &rest _)
                   (funcall callback
                            (and (eq event 'UserPromptSubmit)
                                 '(:updated-input "rewritten prompt")))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (mevedel-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha")
          (mevedel-view-send)))
      (setq sent (mevedel-pipeline--strip-render-data-blocks sent))
      (should (string-match-p "ALPHA BODY" sent))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") sent))
      (should-not (string-match-p "rewritten prompt" sent))))

  :doc "aborting asynchronous preparation makes its late callback inert"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let (late-callback sent)
      (cl-letf (((symbol-function 'mevedel-skills-plan-prepare)
                 (lambda (_plan callback &optional _cancelled-p)
                   (setq late-callback callback)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha")
          (mevedel-view-send)
          (should mevedel-view--pending-skill-submission)
          (mevedel-view-abort)
          (funcall late-callback
                   '(:status ok :model-input "late"
                     :request-context nil :prepared-entries nil))
          (should-not mevedel-view--pending-skill-submission)
          (should (equal "Use $alpha" (mevedel-view--input-text)))))
      (should-not sent)))

  :doc "buffer death settles asynchronous preparation once without dispatch"
  (dolist (killed-buffer '(view data))
    (mevedel-view-test--with-source-skills
        '(("alpha" "inline" "ALPHA"))
      (let ((handle-prepared
             (symbol-function 'mevedel-view--handle-prepared-plan))
            late-callback
            token
            (settlements 0)
            (hooks 0)
            (dispatches 0))
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (_skill _arguments callback &rest _)
                     (setq late-callback callback)))
                  ((symbol-function 'mevedel-view--handle-prepared-plan)
                   (lambda (submission prepared)
                     (cl-incf settlements)
                     (funcall handle-prepared submission prepared)))
                  ((symbol-function 'mevedel-hooks-run-event)
                   (lambda (&rest _) (cl-incf hooks)))
                  ((symbol-function 'mevedel-skills-dispatch-prepared-fork)
                   (lambda (&rest _) (cl-incf dispatches)))
                  ((symbol-function 'gptel-send)
                   (lambda (&rest _) (cl-incf dispatches))))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "Use $alpha")
            (mevedel-view-send)
            (setq token mevedel-view--pending-skill-submission))
          (should token)
          (kill-buffer (if (eq killed-buffer 'view) view-buf data-buf))
          (should (plist-get token :cancelled))
          (funcall late-callback
                   '(:status ok :body "late"
                     :request-context (:invoked-skills nil)))
          (funcall late-callback
                   '(:status ok :body "later"
                     :request-context (:invoked-skills nil)))
          (should (= 1 settlements))
          (should (= 0 hooks))
          (should (= 0 dispatches)))))))

(mevedel-deftest mevedel-view-send/plan-model-policy ()
  ,test
  (test)
  :doc "Plan root requests use planning policy without changing session policy"
  (mevedel-skills-test--with-model-backends
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-skills-test--make-session))
             (mevedel-model-tiers
              '((fast :provider "Fast:fast-model")
                (balanced :provider "Balanced:balanced-model")))
             (mevedel-model-workloads '((planning :tier fast)))
             effective)
        (setf (mevedel-session-plan-mode session) t)
        (with-current-buffer data-buf
          (setq-local mevedel--session session
                      gptel-backend (gptel-get-backend "Balanced")
                      gptel-model 'balanced-model
                      gptel-prompt-transform-functions
                      (list
                       #'mevedel-skills--transform-apply-request-model-policy
                       (lambda (_fsm)
                         (push (list (gptel-backend-name gptel-backend)
                                     gptel-model)
                               effective)))))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _)
                     (mevedel-view-test--dry-run-request-data))))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "Plan this change")
            (mevedel-view-send))
          (setq mevedel-model-workloads '((planning :tier balanced)))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "Revise the plan")
            (mevedel-view-send))
          (setf (mevedel-session-plan-mode session) nil)
          (setq mevedel-model-workloads '((planning :tier fast)))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "Implement normally")
            (mevedel-view-send)))
        (should (equal '(("Fast" fast-model)
                         ("Balanced" balanced-model)
                         ("Balanced" balanced-model))
                       (nreverse effective)))
        (with-current-buffer data-buf
          (should (equal "Balanced" (gptel-backend-name gptel-backend)))
          (should (eq 'balanced-model gptel-model)))))))

(mevedel-deftest mevedel-view--forward-input-now ()
  ,test
  (test)
  :doc "clears inherited response metadata from forwarded user prompts"
  (mevedel-view-test--with-buffers
    (let (sent)
      (with-current-buffer data-buf
        (insert (propertize "Assistant answer.\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t))))
        (with-current-buffer view-buf
          (mevedel-view--forward-input-now "On a scale?")))
      (should sent)
      (with-current-buffer data-buf
        (goto-char (point-min))
        (search-forward "On a scale?")
        (let ((pos (line-beginning-position))
              (end (line-end-position)))
          (while (< pos end)
            (should-not (get-text-property pos 'gptel))
            (setq pos (1+ pos)))))))

  :doc "attaches pending SessionStart hook context to the submitted prompt"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vf-session-hook" :root "/tmp/vf"
                :name "vf"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (mevedel-hooks-record-session-context
         session
         '(:additional-context ("PONYTAIL MODE ACTIVE - level: full"))
         'SessionStart))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (goto-char (mevedel-view--input-start))
        (insert "Hello"))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t)))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra)))
        (with-current-buffer view-buf
          (mevedel-view-send)
          (should sent)
          (should-not (mevedel-session-hook-context-pending session))
          (let ((text (buffer-string)))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "PONYTAIL MODE ACTIVE" text)))
          (search-backward "hook context added")
          (mevedel-view-toggle-section)
          (should (string-match-p "SessionStart" (buffer-string)))
          (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                  (buffer-string)))))
      (with-current-buffer data-buf
        (should (string-match-p "<hook-event name=\"SessionStart\">"
                                (buffer-string)))
        (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                  (buffer-string)))))))

(mevedel-deftest mevedel-view--transform-model-input ()
  ,test
  (test)
  :doc "uses and clears the data buffer's one-shot model input"
  (let* ((chat-buffer (generate-new-buffer " *mevedel-model-input-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--pending-model-input "derived prompt"))
          (with-temp-buffer
            (insert "stored prompt")
            (mevedel-view--transform-model-input fsm)
            (should (equal "derived prompt" (buffer-string)))
            (should-not (plist-member (gptel-fsm-info fsm)
                                      :mevedel-model-context))
            (should-not (plist-member (gptel-fsm-info fsm)
                                      :mevedel-model-input)))
          (with-current-buffer chat-buffer
            (should-not mevedel--pending-model-input)))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-view--follow-up-auto-drain-blocked-p ()
  ,test
  (test)
  :doc "blocks fallback drainage for approval and Goal handoff ownership"
  (let ((session (mevedel-session--create
                  :name "main" :pending-plan-approval 'plan)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (should-not
     (mevedel-view--follow-up-auto-drain-blocked-p session)))
  (let ((here
         (mevedel-session--create
          :name "here"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "here-goal"
             :selection (:location here :execution goal)))))
        (source
         (mevedel-session--create
          :name "source"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "target-goal"
             :selection (:location worktree :execution goal)))))
        (target
         (mevedel-session--create
          :name "target"
          :plan-metadata '(:implementation-goal-id "target-goal"))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p here))
    (should (mevedel-view--follow-up-auto-drain-blocked-p source))
    (should (mevedel-view--follow-up-auto-drain-blocked-p target)))
  (let* ((goal (mevedel-goal--create :id "goal" :status 'paused))
         (session
          (mevedel-session--create
           :name "paused" :goal goal
           :pending-follow-ups
           '((:input "held" :queued-at-goal-id "goal")))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-goal-status goal) 'active)
    (should-not
     (mevedel-view--follow-up-auto-drain-blocked-p session))
    (dolist (status '(blocked budget-limited))
      (setf (mevedel-goal-status goal) status)
      (should (mevedel-view--follow-up-auto-drain-blocked-p session))))
  (let ((session (mevedel-session--create
                  :name "failed" :pending-input-failure-paused t)))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session)))
  :doc "holds ordinary input but permits the owning directive Plan follow-up"
  (let ((session
         (mevedel-session--create
          :name "directive-plan"
          :directive-planning '(:directive-id "d1" :phase approval)
          :pending-follow-ups '((:input "ordinary")))))
    (should (mevedel-view--follow-up-auto-drain-blocked-p session))
    (setf (mevedel-session-pending-follow-ups session)
          '((:input "ordinary")
            (:input "revise" :scope (:directive-id "d1" :action plan))))
    (should-not (mevedel-view--follow-up-auto-drain-blocked-p session))))

(mevedel-deftest mevedel-view-send/pending-input ()
  ,test
  (test)

  :doc "C-c TAB queues independent FIFO follow-ups during an active request"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "main")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-follow-up-drain)
                 #'ignore))
        (with-current-buffer view-buf
          (dolist (text '("first" "second"))
            (goto-char (mevedel-view--input-start))
            (insert text)
            (mevedel-view-send-follow-up))
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("second" "first")
                         (mevedel-view-history--entries)))))
      (let ((entries (mevedel-session-pending-follow-ups session)))
        (should (equal '("first" "second")
                       (mapcar (lambda (entry) (plist-get entry :input))
                               entries)))
        (should (equal '(1 2)
                       (mapcar (lambda (entry) (plist-get entry :id))
                               entries)))
        (should (seq-every-p
                 (lambda (entry)
                   (eq 'follow-up (plist-get entry :category)))
                 entries)))))

  :doc "C-c TAB cannot overtake an older follow-up after the request settles"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "main")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-follow-up-drain)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (ert-fail "Newer follow-up bypassed the queue"))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "first")
          (mevedel-view-send-follow-up))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "second")
          (mevedel-view-send-follow-up)))
      (should
       (equal '("first" "second")
              (mapcar
               (lambda (entry) (plist-get entry :input))
               (mevedel-session-pending-follow-ups session))))))

  :doc "C-c RET queues plain steering against the active request"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "main"))
           (request (mevedel-request--create
                     :id "request-1" :session session
                     :fsm (gptel-make-fsm :state 'TOOL))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request request))
      (cl-letf (((symbol-function 'mevedel-agent-control-root-waiting-p)
                 (lambda (_session) nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "steer this turn")
          (mevedel-view-send)
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("steer this turn")
                         (mevedel-view-history--entries)))))
      (let ((entry (car (mevedel-session-pending-steering session))))
        (should (equal "steer this turn" (plist-get entry :input)))
        (should (equal "request-1" (plist-get entry :request-id)))
        (should (eq 'steering (plist-get entry :category)))
        (should-not (mevedel-session-pending-follow-ups session)))))

  :doc "ordinary steering binds a file and reads its latest contents at delivery"
  (let* ((root (make-temp-file "mevedel-steering-file-" t))
         (file (file-name-concat root "target.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'test :id root :root root :name "steering-file"
           :file-cache
           (mevedel-file-cache--create
            :table (make-hash-table :test #'equal)
            :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" workspace))
         (backend
          (gptel-make-openai
           "Steering file" :stream nil :key "unused"
           :host "example.test" :models '(steering-file-test)))
         (data (list :messages []))
         fsm request)
    (unwind-protect
        (progn
          (with-temp-file file (insert "FILE CONTENT V1\n"))
          (mevedel-view-test--with-buffers
            (setq fsm
                  (gptel-make-fsm
                   :state 'TOOL
                   :info (list :buffer data-buf :backend backend :data data
                               :history '(TRET)
                               :mevedel-request-id "request-file")))
            (setq request
                  (mevedel-request--create
                   :id "request-file" :session session :fsm fsm))
            (with-current-buffer data-buf
              (setq-local mevedel--session session
                          mevedel--workspace workspace
                          mevedel--current-request request))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Inspect @file:target.txt")
              (mevedel-view-send))
            (let* ((entry
                    (car (mevedel-session-pending-steering session)))
                   (input (plist-get entry :input))
                   (start (string-match "@file:" input)))
              (should
               (equal file
                      (plist-get
                       (get-text-property
                        start 'mevedel-mention-binding input)
                       :path)))
              (with-temp-file file (insert "FILE CONTENT V2\n"))
              (mevedel-tools--handle-steering-inject fsm)
              (should-not (mevedel-session-pending-steering session))
              (should
               (eq 'committed
                   (mevedel-prompt-submission-state
                    (plist-get entry :submission)))))
            (let ((payload (format "%S" (plist-get data :messages))))
              (should (string-match-p "FILE CONTENT V2" payload))
              (should-not (string-match-p "FILE CONTENT V1" payload)))
            (with-current-buffer data-buf
              (should (string-match-p
                       (regexp-quote "@file:target.txt")
                       (buffer-string))))))
      (delete-directory root t)))

  :doc "ordinary steering accepts an inline skill and prompt hook context"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA ORDINARY STEERING"))
    (let* ((mevedel-hook-rules
            '((UserPromptSubmit
               ((:matcher "*"
                          :hooks
                          ((:type elisp
                                  :function
                                  mevedel-view-test--add-prompt-hook-context)))))))
           (backend
            (gptel-make-openai
             "Steering prepared" :stream nil :key "unused"
             :host "example.test" :models '(steering-prepared-test)))
           (data (list :messages []))
           (fsm
            (gptel-make-fsm
             :state 'TOOL
             :info (list :buffer data-buf :backend backend :data data
                         :history '(TRET)
                         :mevedel-request-id "request-prepared")))
           (request
            (mevedel-request--create
             :id "request-prepared" :session session :fsm fsm)))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request request))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "Use $alpha")
        (mevedel-view-send))
      (let ((entry (car (mevedel-session-pending-steering session))))
        (should (string-match-p
                 "ALPHA ORDINARY STEERING"
                 (plist-get entry :model-input)))
        (should (string-match-p
                 "ordinary steering hook context"
                 (plist-get entry :model-input))))
      (mevedel-tools--handle-steering-inject fsm)
      (let ((payload (format "%S" (plist-get data :messages))))
        (should (string-match-p "ALPHA ORDINARY STEERING" payload))
        (should (string-match-p
                 "ordinary steering hook context" payload)))))

  :doc "a lost ordinary steering race restores the exact composer state"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "steering-race"))
           (fsm (gptel-make-fsm :state 'TOOL))
           (request
            (mevedel-request--create
             :id "request-race" :session session :fsm fsm))
           (grant "/tmp/steering-race.txt")
           snapshot point-offset notice)
      (setf (mevedel-session-dropped-file-grants session) (list grant))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request request))
      (cl-letf
          (((symbol-function 'mevedel-hooks-run-event)
            (lambda (_event _payload callback &rest _)
              (with-current-buffer data-buf
                (setq-local mevedel--current-request nil))
              (funcall callback nil)))
           ((symbol-function 'message)
            (lambda (format-string &rest args)
              (setq notice (apply #'format format-string args)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert (propertize "race prompt" 'test-property 'preserved))
          (goto-char (+ (mevedel-view--input-start) 4))
          (setq snapshot
                (buffer-substring
                 (mevedel-view--input-start) (point-max))
                point-offset (- (point) (mevedel-view--input-start)))
          (mevedel-view-send)
          (should
           (equal-including-properties
            snapshot
            (buffer-substring
             (mevedel-view--input-start) (point-max))))
          (should (= point-offset
                     (- (point) (mevedel-view--input-start))))))
      (should (equal (list grant)
                     (mevedel-session-dropped-file-grants session)))
      (should-not (mevedel-session-pending-steering session))
      (should-not (mevedel-session-pending-follow-ups session))
      (should (string-match-p "C-c TAB" notice))))

  :doc "Here Goal reservation queues post-acceptance input under its identity"
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "handoff"
            :plan-metadata
            '(:implementation-retry
              (:goal-id "reserved"
               :selection (:location here :execution goal))))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (ert-fail "Replaced reserved kickoff")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "steer after kickoff")
          (mevedel-view-send-follow-up))
        (let ((entry (car (mevedel-session-pending-follow-ups session))))
          (should (equal "steer after kickoff" (plist-get entry :input)))
          (should (equal "reserved" (plist-get entry :queued-at-goal-id)))))))

  :doc "C-c TAB at Plan approval queues without demoting the proposal"
  (mevedel-view-test--with-buffers
    (let* ((metadata '(:status proposed :proposal-id (1 2 "hash")))
           (session
            (mevedel-session--create
             :name "plan" :plan-mode t :plan-metadata metadata)))
      (setf (mevedel-session-pending-plan-approval session)
            (list :session session :callback #'ignore :renderer #'ignore))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "after approval")
          (mevedel-view-send-follow-up)))
      (should (mevedel-session-pending-plan-approval session))
      (should (eq 'proposed
                  (plist-get (mevedel-session-plan-metadata session)
                             :status)))
      (should (equal "after approval"
                     (plist-get
                      (car (mevedel-session-pending-follow-ups session))
                      :input)))))

  :doc "special root workflows reject steering but accept a follow-up"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "review"))
           (request
            (mevedel-request--create
             :id "review-request" :session session :origin "/root")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request request))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "later work")
        (should-error (mevedel-view-send) :type 'user-error)
        (should (equal "later work" (mevedel-view--input-text)))
        (mevedel-view-send-follow-up)
        (should (string-empty-p (mevedel-view--input-text)))
        (should (equal "later work"
                       (plist-get
                        (car (mevedel-session-pending-follow-ups session))
                        :input))))))

  :doc "plain input during WaitAgent is injected before its resumed sample"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :buffer data-buf
                             :mevedel-request-id "wait-plain")))
           send-called
           wake-reason)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :id "wait-plain" :session session :fsm fsm)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (mevedel-agent-control-wait
         session (lambda (reason) (setq wake-reason reason)) 10000)
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "follow up")
          (mevedel-view-send)
          (should-not send-called)
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("follow up")
                         (mevedel-view-history--entries)))
          (should (string-match-p
                   "1 input pending"
                   (mevedel-view--interaction-count-label)))))
      (should (eq 'user wake-reason))
      (should-not (mevedel-session-pending-follow-ups session))
      (should-not (mevedel-session-messages session))
      (should (equal "follow up"
                     (plist-get
                      (car (mevedel-session-pending-steering session))
                      :input)))))

  :doc "WaitAgent steering honors UserPromptSubmit rewrites and context"
  (let* ((root (make-temp-file "mevedel-wait-steering-hook-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "wait-steering-hook" root "wait-steering-hook"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks
                        ((:type elisp
                                :function
                                mevedel-view-test--rewrite-prompt-hook-with-context)))))))
         (fsm (gptel-make-fsm
               :state 'TOOL
               :info (list :mevedel-request-id "wait-hook")))
         wake-reason)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel--current-request
                        (mevedel-request--create
                         :id "wait-hook" :session session :fsm fsm)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _) 'fake-timer)))
            (mevedel-agent-control-wait
             session (lambda (reason) (setq wake-reason reason)) 10000)
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "original steering")
              (mevedel-view-send)
              (should (string-empty-p (mevedel-view--input-text)))))
          (should (eq 'user wake-reason))
          (should (equal "original steering"
                         mevedel-view-test--seen-prompt))
          (let* ((entry (car (mevedel-session-pending-steering session)))
                 (payload (plist-get entry :model-input)))
            (should (string-match-p "rewritten prompt" payload))
            (should (string-match-p "hook policy context" payload))
            (should-not (string-match-p "original steering" payload))
            (should (plist-get entry :hook-audits))))
      (delete-directory root t)))

  :doc "WaitAgent steering consumes inline skill text and transcript metadata"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA STEERING BODY"))
    (let* ((fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :mevedel-request-id "wait-skill")))
           wake-reason)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :id "wait-skill" :session session :fsm fsm)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (error "WaitAgent steering must not start a request"))))
        (mevedel-agent-control-wait
         session (lambda (reason) (setq wake-reason reason)) 10000)
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Use $alpha")
          (mevedel-view-send)))
      (should (eq 'user wake-reason))
      (let ((message (car (mevedel-session-pending-steering session))))
        (should (string-match-p "ALPHA STEERING BODY"
                                (plist-get message :model-input)))
        (should (string-match-p "Use \\$alpha"
                                (plist-get message :transcript-payload)))
        (should (string-match-p "mevedel-render-data"
                                (plist-get message :transcript-payload))))
      (should-not (mevedel-session-invoked-skills session))))

  :doc "a WaitAgent ending during preparation leaves steering queued"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "steering-race" :root "/tmp"
                       :name "steering-race"))
           (session (mevedel-session-create "main" workspace))
           (fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :mevedel-request-id "wait-race")))
           (waiting-checks 0)
           (hook-calls 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace
                    mevedel--current-request
                    (mevedel-request--create
                     :id "wait-race" :session session :fsm fsm))
        (mevedel-hooks-record-session-context
         session '(:additional-context ("race context")) 'SessionStart))
      (mevedel-session-set-pending-inputs
       session 'follow-up
       (list (list :input "older prompt" :queued-at-turn 0)))
      (cl-letf (((symbol-function 'mevedel-agent-control-root-waiting-p)
                 (lambda (_session)
                   (= 1 (cl-incf waiting-checks))))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (cl-incf hook-calls)
                   (funcall callback nil)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "race prompt")
          (mevedel-view-send))
        (should (= 1 (length
                      (mevedel-session-pending-follow-ups session))))
        (with-current-buffer view-buf
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("race prompt")
                         (mevedel-view-history--entries))))
        (should (equal "race prompt"
                       (plist-get
                        (car (mevedel-session-pending-steering session))
                        :input)))
        (should (= 1 hook-calls))
        (should-not (mevedel-session-hook-context-pending session)))))

  :doc "a prepared queue entry survives failure before transcript insertion"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "prepared-retry" :root "/tmp"
                       :name "prepared-retry"))
           (entries '((:event SessionStart :body "reserved retry context")))
           (session (mevedel-session-create "main" workspace))
           (submission
            (mevedel-prompt-submission-create
             :input "prepared prompt" :display-text "prepared prompt"
             :context "<hook-context>reserved retry context</hook-context>"
             :session session :context-entries entries :state 'reserved
             :outcome
             '(:model-input "prepared prompt"
               :transcript-input
               "prepared prompt\n\n<hook-context>reserved retry context</hook-context>"
               :hook-input "prepared prompt"
               :hook-context
               "<hook-context>reserved retry context</hook-context>")))
           (entry (list :input "prepared prompt" :submission submission)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace))
      (mevedel-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'mevedel-view--forward-input-now)
                 (lambda (&rest _) (error "Before transcript"))))
        (mevedel-view--drain-follow-up data-buf))
      (should (eq entry (car (mevedel-session-pending-follow-ups session))))
      (should (eq 'reserved (mevedel-prompt-submission-state submission)))
      (should-not (mevedel-session-hook-context-pending session))
      (cl-letf (((symbol-function 'gptel-send) #'ignore))
        (mevedel-view--drain-follow-up data-buf))
      (should-not (mevedel-session-pending-follow-ups session))
      (should (eq 'committed (mevedel-prompt-submission-state submission)))
      (with-current-buffer data-buf
        (should (= 1 (mevedel-view-test--count-matches
                      "reserved retry context" (buffer-string)))))))

  :doc "a queued directive follow-up keeps its scope through dispatch"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "directive-queue" :root "/tmp"
                       :name "directive-queue"))
           (session (mevedel-session-create "main" workspace))
           (scope '(:directive-id "directive-1" :action discuss
                    :attempt-index 2))
           captured)
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace))
      (mevedel-session-set-pending-inputs
       session 'follow-up
       (list (list :input "queued directive question" :scope scope)))
      (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-input)
                 (lambda (queued-scope input)
                   (setq captured (list queued-scope input))))
                ((symbol-function 'mevedel-view--interaction-rebuild)
                 #'ignore))
        (mevedel-view--drain-follow-up data-buf))
      (should (equal (list scope "queued directive question") captured))
      (should-not (mevedel-session-pending-follow-ups session))))

  :doc "queued direct reference keeps its UUID when the number is reused"
  (let* ((root (make-temp-file "mevedel-ref-queue-" t))
         (file (file-name-concat root "reference.txt"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "ref-queue"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         ref-buf ref replacement request-data warning)
    (unwind-protect
        (progn
          (with-temp-file file (insert "original reference body\n"))
          (setq ref-buf (find-file-noselect file))
          (with-current-buffer ref-buf
            (setq-local mevedel--workspace ws)
            (setq ref (mevedel--create-reference-in
                       ref-buf (point-min) (1- (point-max)))))
          (let* ((id (mevedel--instruction-id ref))
                 (uuid (overlay-get ref 'mevedel-uuid))
                 (token (format "@ref:%d" id)))
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--session session
                            mevedel--workspace ws
                            mevedel--current-request
                            (mevedel-request--create :session session)))
              (with-current-buffer view-buf
                (setq-local mevedel--session session))
              (cl-letf (((symbol-function
                          'mevedel-view--schedule-late-follow-up-drain)
                         #'ignore))
                (with-current-buffer view-buf
                  (goto-char (mevedel-view--input-start))
                  (insert "Inspect " token " after the current turn")
                  (mevedel-view-send-follow-up)))
              (let* ((queued
                      (plist-get
                       (car (mevedel-session-pending-follow-ups session))
                       :input))
                     (queued-start (string-match (regexp-quote token) queued))
                     (history
                      (with-current-buffer view-buf
                        (car (mevedel-view-history--entries))))
                     (history-start
                      (string-match (regexp-quote token) history)))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 queued-start 'mevedel-mention-binding queued)
                                :reference-uuid)))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 history-start 'mevedel-mention-binding history)
                                :reference-uuid))))
              (mevedel--delete-instruction ref ref-buf)
              (with-current-buffer ref-buf
                (erase-buffer)
                (insert "replacement reference body\n")
                (setq replacement
                      (mevedel--create-reference-in
                       ref-buf (point-min) (1- (point-max)))))
              (should (= id (mevedel--instruction-id replacement)))
              (with-current-buffer data-buf
                (setq-local mevedel--current-request nil))
              (let ((gptel-prompt-transform-functions
                     (cons #'mevedel--transform-expand-mentions
                           (remove #'mevedel--transform-expand-mentions
                                   gptel-prompt-transform-functions))))
                (cl-letf (((symbol-function 'message)
                           (lambda (format-string &rest args)
                             (let ((text (apply #'format format-string args)))
                               (when (string-prefix-p "mevedel: reference" text)
                                 (setq warning text)))))
                          ((symbol-function 'gptel-send)
                           (lambda (&rest _)
                             (setq request-data
                                   (mevedel-view-test--dry-run-request-data)))))
                  (mevedel-view--drain-follow-up data-buf)))
              (should (string-search (format "[ref:%d -- unavailable]" id)
                                     request-data))
              (should-not (string-search "replacement reference body"
                                         request-data))
              (should (string-match-p "mevedel: reference .* unavailable"
                                      warning))
              (with-current-buffer data-buf
                (goto-char (point-min))
                (should (search-forward token nil t))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 (match-beginning 0)
                                 'mevedel-mention-binding)
                                :reference-uuid))))
              (should-not (mevedel-session-pending-follow-ups session)))))
      (when (buffer-live-p ref-buf)
        (with-current-buffer ref-buf (set-buffer-modified-p nil))
        (kill-buffer ref-buf))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "queued file keeps its pathname and warns softly if deleted"
  (let* ((root (make-temp-file "mevedel-file-queue-" t))
         (file (file-name-concat root "queued.txt"))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "file-queue"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         request-data warning)
    (with-temp-file file (insert "queued file secret\n"))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session
                        mevedel--workspace ws
                        mevedel--current-request
                        (mevedel-request--create :session session)))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function
                      'mevedel-view--schedule-late-follow-up-drain)
                     #'ignore))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Inspect @file:queued.txt")
              (mevedel-view-send-follow-up)))
          (let* ((queued
                  (plist-get
                   (car (mevedel-session-pending-follow-ups session))
                   :input))
                 (queued-start (string-match "@file:" queued))
                 (history
                  (with-current-buffer view-buf
                    (car (mevedel-view-history--entries))))
                 (history-start (string-match "@file:" history)))
            (should (equal file
                           (plist-get
                            (get-text-property
                             queued-start 'mevedel-mention-binding queued)
                            :path)))
            (should (equal file
                           (plist-get
                            (get-text-property
                             history-start 'mevedel-mention-binding history)
                            :path))))
          (delete-file file)
          (with-current-buffer data-buf
            (setq-local mevedel--current-request nil))
          (let ((gptel-prompt-transform-functions
                 (cons #'mevedel--transform-expand-mentions
                       (remove #'mevedel--transform-expand-mentions
                               gptel-prompt-transform-functions))))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (let ((text (apply #'format format-string args)))
                           (when (string-prefix-p "mevedel: file" text)
                             (setq warning text)))))
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (mevedel-view--drain-follow-up data-buf)))
          (should (string-search
                   "[file:queued.txt -- does not exist]" request-data))
          (should-not (string-search "queued file secret" request-data))
          (should (string-match-p
                   "mevedel: file .* unavailable" warning))
          (with-current-buffer data-buf
            (goto-char (point-min))
            (should (search-forward "@file:queued.txt" nil t))
            (should (equal file
                           (plist-get
                            (get-text-property
                             (match-beginning 0) 'mevedel-mention-binding)
                            :path))))
          (should-not (mevedel-session-pending-follow-ups session)))
      (when (file-exists-p file) (delete-file file))
      (delete-directory root t)))

  :doc "queued MCP mention keeps its locator and reads after reconnect"
  (let* ((root (make-temp-file "mevedel-mcp-queue-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "mcp-queue"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (token "@mcp:docs:file:///guide")
         (connections (make-hash-table :test #'equal))
         (connection (make-symbol "mcp-connection"))
         request-data)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session
                        mevedel--workspace ws
                        mevedel--current-request
                        (mevedel-request--create :session session)))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function
                      'mevedel-view--schedule-late-follow-up-drain)
                     #'ignore))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Consult " token)
              (mevedel-view-send-follow-up))
            (let* ((queued
                    (plist-get
                     (car (mevedel-session-pending-follow-ups session))
                     :input))
                   (queued-start (string-match (regexp-quote token) queued))
                   (history
                    (with-current-buffer view-buf
                      (car (mevedel-view-history--entries))))
                   (history-start
                    (string-match (regexp-quote token) history)))
              (dolist (entry (list (cons queued queued-start)
                                   (cons history history-start)))
                (should
                 (equal '(:kind mcp :token "@mcp:docs:file:///guide"
                          :server "docs" :uri "file:///guide")
                        (get-text-property
                         (cdr entry) 'mevedel-mention-binding
                         (car entry)))))))
          (puthash "docs" connection connections)
          (with-current-buffer data-buf
            (setq-local mevedel--current-request nil))
          (let ((gptel-prompt-transform-functions
                 (cons #'mevedel--transform-expand-mentions
                       (remove #'mevedel--transform-expand-mentions
                               gptel-prompt-transform-functions)))
                (mcp-server-connections connections))
            (cl-letf (((symbol-function 'mcp-hub-get-servers)
                       (lambda ()
                         (list (list :name "docs" :status 'connected))))
                      ((symbol-function 'mcp-read-resource)
                       (lambda (actual uri)
                         (should (eq connection actual))
                         (should (equal "file:///guide" uri))
                         (list :contents
                               (vector (list :type "text"
                                             :text "current guide")))))
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (mevedel-view--drain-follow-up data-buf)))
          (should (string-search "current guide" request-data))
          (with-current-buffer data-buf
            (goto-char (point-min))
            (should (search-forward token nil t))
            (should
             (equal "file:///guide"
                    (plist-get
                     (get-text-property
                      (match-beginning 0) 'mevedel-mention-binding)
                     :uri))))
          (should-not (mevedel-session-pending-follow-ups session)))
      (delete-directory root t)))

  :doc "queued follow-up stays visible across incremental in-flight rendering"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-incremental" :root "/tmp/vq"
                :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           data-turn-start)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (insert "*** Prompt\n")
        (setq data-turn-start (copy-marker (point-max) nil))
        (let ((start (point)))
          (insert "First partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "Prompt"))
        (mevedel-view--render-incremental data-buf)
        (goto-char (mevedel-view--input-start))
        (insert "follow up")
        (mevedel-view-send-follow-up)
        (should (string-match-p "follow up"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (with-current-buffer data-buf
          (let ((start (point)))
            (insert "Second partial response.\n")
            (put-text-property start (point) 'gptel 'response)))
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Second partial response" text))
               (prompt (string-match-p "\n> " text)))
          (should queued)
          (should partial)
          (should prompt)
          (should (< partial queued))
          (should (< queued prompt))))))

  :doc "queued follow-up stays visible across in-flight full rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-full-rerender" :root "/tmp/vq"
                :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (insert "*** Prompt\n")
        (let ((start (point)))
          (insert "Partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "follow up"
                        :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min) nil)))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Partial response" text))
               (prompt (string-match-p "\n> " text)))
          (should queued)
          (should partial)
          (should prompt)
          (should (< partial queued))
          (should (< queued prompt))))))

  :doc "pending-input UI shows the queue-management key hint"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hint" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "follow up" :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-rebuild)
        (should (string-match-p "C-c C-e manage pending inputs"
                                (buffer-string))))))

  :doc "queued skill hooks run once at dispatch and not while queueing"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let ((hooks 0)
          (sends 0))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _event-plist callback &rest _)
                   (when (eq event 'UserPromptSubmit)
                     (cl-incf hooks))
                   (funcall callback nil)))
                ((symbol-function
                  'mevedel-view--schedule-late-follow-up-drain)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sends))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Queue $alpha exactly")
          (mevedel-view-send-follow-up))
        (should (= 0 hooks))
        (should (= 0 sends))
        (let* ((input (plist-get
                       (car (mevedel-session-pending-follow-ups session))
                       :input))
               (start (string-match "\\$alpha" input)))
          (should (plist-get
                   (get-text-property start 'mevedel-mention-binding input)
                   :source-file)))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (mevedel-view--drain-follow-up data-buf)
        (should (= 1 hooks))
        (should (= 1 sends))
        (should-not (mevedel-session-pending-follow-ups session)))))

  :doc "an unavailable queued binding annotates, sends, and leaves the queue"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let (sent original)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-follow-up-drain)
                 #'ignore))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Queue $alpha exactly")
          (mevedel-view-send-follow-up)))
      (setq original (car (mevedel-session-pending-follow-ups session)))
      (delete-file
       (plist-get
        (get-text-property
         (string-match "\\$alpha" (plist-get original :input))
         'mevedel-mention-binding (plist-get original :input))
        :source-file))
      (with-current-buffer data-buf
        (setq-local mevedel--current-request nil))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-follow-up data-buf))
      (should sent)
      (should-not (mevedel-session-pending-follow-ups session))))

  :doc "fallback drain preserves queued entries while plan approval is pending"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-fallback-plan" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-pending-plan-approval session)
            (list :body "# Plan" :origin "main"))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (mevedel-view--drain-follow-up data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-pending-follow-ups session))))))

  :doc "directive planning drains its revision before earlier ordinary input"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-directive-plan" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-directive-planning session)
            '(:directive-id "d1" :phase approval)
            (mevedel-session-pending-follow-ups session)
            '((:input "ordinary" :display-text "ordinary")
              (:input "revise" :display-text "revise"
               :scope (:directive-id "d1" :action plan))))
      (cl-letf (((symbol-function 'mevedel-view--dispatch-directive-input)
                 (lambda (_scope input) (setq sent input))))
        (mevedel-view--drain-follow-up data-buf))
      (should (equal "revise" sent))
      (should (equal '("ordinary")
                     (mapcar (lambda (entry) (plist-get entry :input))
                             (mevedel-session-pending-follow-ups session))))))

  :doc "late drain scheduler uses data buffer after request cleanup"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-late-schedule"
                :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           drain-buffer)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat _function &rest args)
                   (setq drain-buffer (car args))
                   'timer)))
        (with-current-buffer view-buf
          (mevedel-view--schedule-late-follow-up-drain)))
      (should (eq drain-buffer data-buf))))

  :doc "interaction rebuild preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-point" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "already queued"
                        :display-text "already queued")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (goto-char (+ (mevedel-view--input-start) 2))
        (mevedel-view--interaction-rebuild)
        (should (string= "draft" (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 2))))))

  :doc "spinner status redraw preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (mevedel-view--start-spinner "Thinking...")
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (mevedel-view--update-spinner "Calling Read...")
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (mevedel-view--stop-spinner)
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string= draft (mevedel-view--input-text))))))

  :doc "full rerender preserves composer when all zone markers drift"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (mevedel-view--full-rerender)
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4))))))

  :doc "slash input during an active request is rejected"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-slash" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "/review")
        (should-error (mevedel-view-send) :type 'user-error)
        (should-not (mevedel-session-pending-follow-ups session))
        (should (string= "/review" (mevedel-view--input-text))))))

  :doc "fallback drain submits one exact FIFO entry per turn"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-fifo" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sent))))
        (mevedel-view--drain-follow-up data-buf)
        (should (= 1 sent))
        (should (equal "second"
                       (plist-get
                        (car (mevedel-session-pending-follow-ups session))
                        :input)))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "first" text))
            (should-not (string-match-p "second" text)))))))

  :doc "queued follow-ups do not drain while the request is still active"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-active" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "pending" :display-text "pending")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-follow-up data-buf)
        (should-not sent)
        (should (mevedel-session-pending-follow-ups session)))))

  :doc "clearing prepared follow-ups restores their reserved context"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-clear" :root "/tmp/vq" :name "vq"))
           (session (mevedel-session-create "main" ws))
           (context-entries '((:event SessionStart :body "clear context")))
           (submission
            (mevedel-prompt-submission-create
             :input "prepared" :display-text "prepared" :session session
             :context-entries context-entries :state 'reserved)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-pending-follow-ups session)
            (list (list :input "prepared" :submission submission)))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (mevedel-pending-inputs-clear)))
      (should-not (mevedel-session-pending-follow-ups session))
      (should (equal context-entries
                     (mevedel-session-hook-context-pending session))))))

(mevedel-deftest mevedel-view--send-local-goal ()
  ,test
  (test)
  :doc "applies prompt-hook context and starts the Goal in the data buffer"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "main"))
          started
          started-buffer)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
                 (lambda (_args _input callback)
                   (funcall
                    callback
                    (mevedel-prompt-submission-create
                     :input "expanded" :display-text "/goal draft"
                     :context "hook context" :session session))))
                ((symbol-function 'mevedel-view-history-add) #'ignore)
                ((symbol-function 'mevedel-view--clear-input) #'ignore)
                ((symbol-function 'mevedel-goal-start)
                 (lambda (objective submission)
                   (setq started
                         (list objective
                               (mevedel-prompt-submission-context submission))
                         started-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-view--send-local-goal "/goal draft" "draft")))
      (should (eq data-buf started-buffer))
      (should (equal '("expanded" "hook context") started))))
  :doc "treats the former auto selector as ordinary objective text"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "main"))
          started)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
                (lambda (objective _input callback)
                   (should (equal "auto ship it" objective))
                   (funcall
                    callback
                    (mevedel-prompt-submission-create
                     :input objective :display-text "/goal auto ship it"
                     :session session))))
                ((symbol-function 'mevedel-view-history-add) #'ignore)
                ((symbol-function 'mevedel-view--clear-input) #'ignore)
                ((symbol-function 'mevedel-goal-start)
                 (lambda (objective _submission) (setq started objective))))
        (with-current-buffer view-buf
          (mevedel-view--send-local-goal
           "/goal auto ship it" "auto ship it")))
      (should (equal "auto ship it" started)))))

(mevedel-deftest mevedel-view-send/user-prompt-hooks ()
  ,test
  (test)

  :doc "blocking UserPromptSubmit does not record history or insert prompt"
  (let* ((root (make-temp-file "mevedel-view-hooks" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks" root "view-hooks"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--stop-prompt-hook))))))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "blocked prompt")
            (mevedel-view-send)
            (should-not (mevedel-view-history--entries))
            (should-not
             (string-match-p
              "blocked prompt"
              (buffer-substring-no-properties
               (point-min) mevedel-view--input-marker))))
	      (with-current-buffer data-buf
		(should (string-empty-p (buffer-string)))))
      (delete-directory root t)))

  :doc "blocked prompt context is consumed once by the next accepted root input"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "blocked-context" :root "/tmp"
                       :name "blocked-context"))
           (session (mevedel-session-create "main" workspace))
           (decisions
            (list '(:continue nil :additional-context ("blocked context"))
                  nil))
           accepted-context
           (blocked-count 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace workspace))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace workspace))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback (pop decisions)))))
        (with-current-buffer view-buf
          (mevedel-view--run-prompt-submit-hook
           "blocked" "blocked"
           (lambda (&rest _)
             (ert-fail "Blocked prompt was accepted"))
           (lambda () (cl-incf blocked-count)))
          (should (= 1 blocked-count))
          (should (mevedel-session-hook-context-pending session))
          (mevedel-view--run-prompt-submit-hook
           "accepted" "accepted"
           (lambda (submission)
             (setq accepted-context
                   (mevedel-prompt-submission-context submission))
             (mevedel-prompt-submission-commit submission))))
        (should (string-match-p "blocked context" accepted-context))
      (should (string-match-p "UserPromptSubmit" accepted-context))
      (should-not (mevedel-session-hook-context-pending session)))))

  :doc "accepted Plan follow-up input immediately invalidates stale approval"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "plan-follow-up" :root "/tmp"
                       :name "plan-follow-up"))
           (selection '(:location here :context current
                        :execution direct :mode edits))
           (session
            (mevedel-session--create
             :name "main" :workspace workspace :plan-mode t
             :plan-metadata
             (list :status 'proposed :proposal-id '(1 2 "h")
                   :selection selection)))
           status-at-callback)
      (setf (mevedel-session-pending-plan-approval session)
            (list :session session :callback #'ignore))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace))
      (with-current-buffer view-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil))))
        (with-current-buffer view-buf
          (mevedel-view--run-prompt-submit-hook
           "Refine this" "Refine this"
           (lambda (_submission)
             (setq status-at-callback
                   (plist-get (mevedel-session-plan-metadata session)
                              :status))))))
      (let ((metadata (mevedel-session-plan-metadata session)))
        (should (eq 'draft status-at-callback))
        (should-not (mevedel-session-pending-plan-approval session))
        (should (equal selection (plist-get metadata :selection)))
        (should-not (plist-member metadata :proposal-id)))))

  :doc "first skill turn orders start, expansion, then submit context"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "ordered-context" :root "/tmp"
                       :name "ordered-context"))
           (session (mevedel-session-create "main" workspace))
           accepted-context)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace workspace)
        (mevedel-hooks-record-session-context
         session '(:additional-context ("start context")) 'SessionStart))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace workspace))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback
                            '(:additional-context ("submit context"))))))
        (with-current-buffer view-buf
          (mevedel-view--run-prompt-submit-hook
           "expanded prompt" "Use $alpha"
           (lambda (submission)
             (setq accepted-context
                   (mevedel-prompt-submission-context submission))
             (mevedel-prompt-submission-commit submission))
           nil
           "<hook-context>expansion context</hook-context>")))
      (let ((start-pos (string-search "start context" accepted-context))
            (expansion-pos
             (string-search "expansion context" accepted-context))
            (submit-pos (string-search "submit context" accepted-context)))
        (should (< start-pos expansion-pos))
        (should (< expansion-pos submit-pos)))))

  :doc "errors before transcript insertion leave pending context for retry"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "callback-rollback" :root "/tmp"
                       :name "callback-rollback"))
           (session (mevedel-session-create "main" workspace))
           retry-context)
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace)
        (mevedel-hooks-record-session-context
         session '(:additional-context ("retry context")) 'SessionStart))
      (with-current-buffer view-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace)
        (should-error
         (mevedel-view--run-prompt-submit-hook
          "first" "first" (lambda (&rest _) (error "Dispatch failed"))))
        (should (mevedel-session-hook-context-pending session))
        (mevedel-view--run-prompt-submit-hook
         "retry" "retry"
         (lambda (submission)
           (setq retry-context
                 (mevedel-prompt-submission-context submission))
           (mevedel-prompt-submission-commit submission))))
      (should (string-match-p "retry context" retry-context))
      (should-not (mevedel-session-hook-context-pending session))))

  :doc "send startup failure does not duplicate context already inserted"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'test :id "send-failure" :root "/tmp"
                       :name "send-failure"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace workspace)
        (mevedel-hooks-record-session-context
         session '(:additional-context ("send startup context"))
         'SessionStart))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (error "Request startup failed"))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "send prompt")
          (should-error (mevedel-view-send))))
      (should-not (mevedel-session-hook-context-pending session))
      (with-current-buffer data-buf
        (should (= 1 (mevedel-view-test--count-matches
                      "send startup context" (buffer-string)))))))

  :doc "/goal prompts run UserPromptSubmit"
  (let* ((root (make-temp-file "mevedel-view-plan-hooks" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-plan-hooks" root "view-plan-hooks"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
	                        :hooks ((:type elisp
	                                       :function
	                                       mevedel-view-test--rewrite-prompt-hook-with-context)))))))
         (mevedel-view-test--seen-prompt nil)
         events)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-goal-start)
                     (lambda (objective submission)
                       (push (list objective
                                   (mevedel-prompt-submission-context submission))
                             events))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "/goal draft")
              (mevedel-view-send)
              (should (equal "draft" mevedel-view-test--seen-prompt))
              (setq events (nreverse events))
              (should (equal "rewritten prompt" (caar events)))
              (should (string-match-p "hook policy context"
                                      (cadar events)))
              (should (string-empty-p (mevedel-view--input-text)))))
          (with-current-buffer view-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should-not (string-match-p "/goal draft" text)))))
      (delete-directory root t)))

  :doc "/plan PROMPT enters Plan and submits PROMPT through UserPromptSubmit"
  (let* ((root (make-temp-file "mevedel-view-plan-command" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-plan-command" root "view-plan-command"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--rewrite-prompt-hook-with-context)))))))
         (mevedel-view-test--seen-prompt nil)
         sent)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session
                        mevedel--workspace workspace))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _) (setq sent t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "/plan refactor X")
              (mevedel-view-send)
              (should sent)
              (should (equal "refactor X" mevedel-view-test--seen-prompt))
              (should (equal "/plan refactor X"
                             (car (mevedel-view-history--entries))))
              (should (string-empty-p (mevedel-view--input-text))))
            (should (mevedel-session-plan-mode session))
            (with-current-buffer data-buf
              (should (string-match-p "rewritten prompt" (buffer-string))))))
      (delete-directory root t)))

  :doc "blocking UserPromptSubmit prevents expanded inline skill send"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
	                         :hooks ((:type elisp
	                                        :function
	                                        mevedel-view-test--stop-prompt-hook)))))))
	          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
	(with-current-buffer view-buf
	  (goto-char (mevedel-view--input-start))
	  (insert "$myskill blocked")
	  (mevedel-view-send)
	          (should-not send-called)
	          (should-not (mevedel-view-history--entries)))
	        (with-current-buffer data-buf
	          (should (string-empty-p (buffer-string)))
	          (should-not (bound-and-true-p
	                       mevedel-skills--pending-request-context))))))

  :doc "inline skill hooks see expanded body but cannot replace it"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook)))))))
          (mevedel-view-test--seen-prompt nil)
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send))
	        (should send-called)
	        (should (string-match-p "Expanded hello"
	                                mevedel-view-test--seen-prompt))
	        (should-not (string-search "<!-- mevedel-render-data -->"
	                                   mevedel-view-test--seen-prompt))
	        (with-current-buffer data-buf
	          (let ((text
                   (mevedel-pipeline--strip-render-data-blocks
                    (mevedel--strip-hook-audit-blocks
                     (buffer-string)))))
	            (should-not (string-match-p "rewritten prompt" text))
            (should (string-match-p (regexp-quote "$myskill hello") text))
            (should-not (string-match-p "Expanded hello" text)))))))

  :doc "prompt rewrites render an expandable hook audit disclosure"
  (let* ((root (make-temp-file "mevedel-view-hooks-audit" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-audit" root "view-hooks-audit"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--rewrite-prompt-hook-with-message)))))))
         (send-called nil))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "original prompt")
              (mevedel-view-send)
              (should send-called)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-match-p "rewritten prompt" text))
                (should (string-match-p "hook changed prompt" text))
                (should-not (string-match-p "original prompt" text))
                (should-not (string-match-p "changed by test hook" text)))
              (goto-char (point-min))
              (search-forward "hook changed prompt")
              (mevedel-view-toggle-section)
              (let ((expanded (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker)))
                (should (string-match-p "UserPromptSubmit" expanded))
                (should (string-match-p "changed by test hook" expanded))
                (should (string-match-p "Original prompt:" expanded))
                (should (string-match-p "original prompt" expanded))
                (should (string-match-p "Submitted prompt:" expanded))
                (should (string-match-p "rewritten prompt" expanded)))))
          (with-current-buffer data-buf
            (let ((text (buffer-string)))
              (should (string-match-p "<!-- mevedel-hook-audit -->" text))
              (should (string-match-p "rewritten prompt" text))
              (goto-char (point-min))
              (search-forward "<!-- mevedel-hook-audit -->")
              (should (eq 'ignore
                          (get-text-property (match-beginning 0)
                                             'gptel)))))
          (with-current-buffer view-buf
            (mevedel-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should (string-match-p "hook changed prompt" text))
              (should-not (string-match-p "original prompt" text)))))
      (delete-directory root t)))

  :doc "inline skill Prompt omits hook context in immediate render"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "hook policy context" text)))
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "rewritten prompt" expanded))
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "hook policy context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (mevedel-pipeline--strip-render-data-blocks
                     (buffer-string))))
          (should-not (string-match-p "rewritten prompt" text))
          (should (string-match-p (regexp-quote "$myskill hello") text))
          (should-not (string-match-p "Expanded hello" text))
          (should (string-match-p "hook policy context" text))))))

  :doc "malformed UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "mevedel-view-hooks-malformed" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-malformed" root
                     "view-hooks-malformed"))
         (session (mevedel-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback :args)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "prompt with malformed hook result")
              (mevedel-view-send))
            (should send-called)))
      (delete-directory root t)))

  :doc "symbol UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "mevedel-view-hooks-symbol" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-symbol" root
                     "view-hooks-symbol"))
         (session (mevedel-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback 'passed)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "prompt with symbol hook result")
              (mevedel-view-send))
            (should send-called)))
      (delete-directory root t)))

  :doc "slow UserPromptSubmit command keeps the send path non-reentrant"
  (let* ((root (make-temp-file "mevedel-view-hooks-pending" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-pending" root "view-hooks-pending"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (send-count 0))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (cl-incf send-count))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "slow prompt")
              (mevedel-view-send)
              (should mevedel-view--prompt-hook-pending)
              (should-error (mevedel-view-send) :type 'user-error)
              (let ((deadline (+ (float-time) 10)))
                (while (and mevedel-view--prompt-hook-pending
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not mevedel-view--prompt-hook-pending)
              (should (= send-count 1)))))
      (delete-directory root t))))

(provide 'test-mevedel-view-composer)
;;; test-mevedel-view-composer.el ends here
