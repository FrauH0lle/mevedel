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
(require 'mevedel-skills)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-permission-queue)
(require 'mevedel-review)
(require 'mevedel-tool-plan)
(require 'mevedel-agents)
(require 'mevedel-agent-runtime)
(require 'mevedel-hooks)
(require 'mevedel-view-zone)
(require 'mevedel-view-history)

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
                    mevedel-view--forward-input
                    mevedel-view--run-prompt-submit-hook))
    (should (equal "mevedel-view-composer"
                   (mevedel-view-composer-test--owner symbol))))
  :doc "owns queued, forked, and request-progress send orchestration"
  (dolist (symbol '(mevedel-view--queue-user-message
                    mevedel-view--fork-if-pending
                    mevedel-view--schedule-queued-user-message-drain
                    mevedel-view-abort))
    (should (equal "mevedel-view-composer"
                   (mevedel-view-composer-test--owner symbol)))))


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

(defun mevedel-view-test--write-skill (dir name frontmatter)
  "Create DIR/NAME/SKILL.md with FRONTMATTER."
  (let* ((skill-dir (file-name-as-directory (file-name-concat dir name)))
         (skill-file (file-name-concat skill-dir "SKILL.md")))
    (make-directory skill-dir t)
    (with-temp-file skill-file
      (insert "---\n")
      (insert frontmatter)
      (unless (string-suffix-p "\n" frontmatter)
        (insert "\n"))
      (insert "---\n"))
	    skill-file))

(defun mevedel-view-test--stop-prompt-hook (_event)
  "Block prompt submission in view-send cases."
  '(:continue nil :stop-reason "blocked"))

(defvar mevedel-view-test--seen-prompt nil)

(defun mevedel-view-test--rewrite-prompt-hook (event)
  "Capture prompt EVENT and rewrite it in view-send cases."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"))

(defun mevedel-view-test--rewrite-prompt-hook-with-context (event)
  "Capture prompt EVENT, rewrite it, and add model-only context."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :additional-context "model-only context"))

(defun mevedel-view-test--rewrite-prompt-hook-with-message (event)
  "Capture prompt EVENT, rewrite it, and add a user-facing message."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :system-message "changed by test hook"))


;;
;;; Composer editing

(mevedel-deftest mevedel-view--input-prompt-string
  (:doc "renders permission mode in the prompt prefix")
  ,test
  (test)

  :doc "default mode renders ask"
  (let ((prompt (mevedel-view--input-prompt-string 'default)))
    (should (string= "\n> " prompt))
    (should (eq 'mevedel-view-input-prompt
                (get-text-property 0 'font-lock-face prompt))))

  :doc "plan mode renders plan"
  (let ((prompt (mevedel-view--input-prompt-string 'plan)))
    (should (string= "\n[plan]  > " prompt))
    (should (eq 'mevedel-view-permission-mode-plan
                (get-text-property 2 'font-lock-face prompt))))

  :doc "accept-edits mode renders edits"
  (let ((prompt (mevedel-view--input-prompt-string 'accept-edits)))
    (should (string= "\n[edits] > " prompt))
    (should (eq 'mevedel-view-permission-mode-accept-edits
                (get-text-property 2 'font-lock-face prompt))))

  :doc "trust-all mode renders auto warning"
  (let ((prompt (mevedel-view--input-prompt-string 'trust-all)))
    (should (string= "\n[auto!] > " prompt))
    (should (eq 'mevedel-view-permission-mode-trust-all
                (get-text-property 2 'font-lock-face prompt)))))

(mevedel-deftest mevedel-view--next-permission-mode
  (:doc "cycles permission modes in view order")
  ,test
  (test)

  :doc "default mode moves to accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode 'default)))

  :doc "accept-edits mode moves to trust-all"
  (should (eq 'trust-all
              (mevedel-view--next-permission-mode 'accept-edits)))

  :doc "trust-all mode moves to plan"
  (should (eq 'plan
              (mevedel-view--next-permission-mode 'trust-all)))

  :doc "plan mode wraps to default"
  (should (eq 'default
              (mevedel-view--next-permission-mode 'plan)))

  :doc "nil mode starts at accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode nil)))

  :doc "unknown mode starts at accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode 'bogus))))

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
                          :permission-mode 'default)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf)
              (setq-local mevedel-permission-mode 'default))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'default)
              (should (eq 'accept-edits
                          (mevedel-view-cycle-permission-mode)))
              (should (eq 'accept-edits
                          (mevedel-session-permission-mode session)))
              (should (eq 'accept-edits
                          (buffer-local-value
                           'mevedel-permission-mode data-buf)))
              (should (eq 'accept-edits mevedel-permission-mode))
              (should (eq saved
                          (default-toplevel-value 'mevedel-permission-mode)))
              (should (string= "\n[edits] > "
                               (buffer-substring-no-properties
                                mevedel-view--input-marker
                                (mevedel-view--input-start)))))
            (with-current-buffer view-buf
              (should (eq 'trust-all
                          (mevedel-view-cycle-permission-mode)))
              (should (memq 'auto-mode
                            (mapcar #'mevedel-reminder-type
                                    (mevedel-session-reminders session))))
              (should (eq 'plan
                          (mevedel-view-cycle-permission-mode)))
              (let ((types (mapcar #'mevedel-reminder-type
                                   (mevedel-session-reminders session))))
                (should (eq 'trust-all
                            (plist-get
                             (mevedel-session-plan-metadata session)
                             :previous-permission-mode)))
                (should (memq 'plan-mode types))
                (should-not (memq 'auto-mode types))
                (should (memq 'auto-mode-exit types)))
              (should (eq 'default
                          (mevedel-view-cycle-permission-mode)))
              (let ((types (mapcar #'mevedel-reminder-type
                                   (mevedel-session-reminders session))))
                (should-not
                 (plist-get (mevedel-session-plan-metadata session)
                            :previous-permission-mode))
                (should-not (memq 'plan-mode types))
                (should (memq 'plan-mode-exit types)))
              (should (eq 'default
                          (mevedel-session-permission-mode session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))

(mevedel-deftest mevedel-view-refresh-input-prompt
  (:doc "updates the prompt prefix without disturbing draft input")
  ,test
  (test)

  :doc "setup renders the default mode prompt"
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
      (setq-local mevedel-permission-mode 'trust-all)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[auto!] > "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))
      (should (string= "draft" (mevedel-view--input-text)))))

  :doc "refresh preserves a multiline draft starting with a literal >"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> quoted\nsecond line")
      (setq-local mevedel-permission-mode 'trust-all)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[auto!] > "
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
        (setq-local mevedel-permission-mode 'trust-all)
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (should (string= "\n[auto!] > "
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

  :doc "view mode command completes first argument options"
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
            (insert "/mode pl")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("plan")
                             (mevedel-view-test--capf-candidates
                              capf "pl")))
              (should (member "trust-all"
                              (mevedel-view-test--capf-candidates capf))))))
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
                                (buffer-string)))))))

(defmacro mevedel-view-test--with-fork-skill (skill-form &rest body)
  "Wire data-buf with a session containing SKILL-FORM, then run BODY.
Binds `data-buf', `view-buf', and `session' in scope.  The skill is
attached via `mevedel-session-skills' so `mevedel-session-get-skill'
finds it during `$' skill dispatch."
  (declare (indent 1) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((ws (mevedel-workspace--create
                 :type 'test :id "vf" :root "/tmp/vf" :name "vf"
                 :file-cache (mevedel-file-cache--create
                              :table (make-hash-table :test #'equal)
                              :order nil :total-bytes 0)))
            (session (mevedel-session-create "main" ws))
            (skill ,skill-form))
       (setf (mevedel-session-skills session) (list skill))
       (with-current-buffer data-buf
         (setq-local mevedel--session session))
       ,@body)))

(mevedel-deftest mevedel-view-send/skill-fork ()
  ,test
  (test)
  :doc "fork skill blocks input, captures the callback, and inserts the result"
  ;; Drive `mevedel-view-send' for a $myfork dispatch with a fork
  ;; skill installed on the session.  We mock `mevedel-skills-invoke'
  ;; to capture the callback so we can verify (a) the view-side spinner
  ;; and turn marker get armed before invocation returns, (b) the input
  ;; is cleared, (c) the eventual fork result is rendered into the data
  ;; buffer with `gptel response' text properties (the contract that
  ;; `--insert-fork-result' relies on for downstream rendering).
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args save-called status-called)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (skill args callback &rest kwargs)
                   (setq captured-args
                         (list :skill skill :args args
                               :callback callback :kwargs kwargs))))
                ((symbol-function 'mevedel-session-persistence-save)
                 (lambda (s b)
                   (setq save-called (list s b))
                   "saved"))
                ((symbol-function 'gptel--update-status)
                 (lambda (&rest args) (setq status-called args))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run a thing")
          (mevedel-view-send)

          ;; The view armed the in-flight turn marker and spinner.
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (marker-position mevedel-view--in-flight-turn-start))
          (should (mevedel-view--request-progress-visible-p))

          ;; The user-message display text appeared in the view above
          ;; the input region.
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myfork run a thing" text))))
        (with-current-buffer data-buf
          (should mevedel--current-request))

        ;; mevedel-skills-invoke was called with the right shape.
        (should captured-args)
        (should (equal "myfork"
                       (mevedel-skill-name (plist-get captured-args :skill))))
        (should (equal "run a thing" (plist-get captured-args :args)))
        (should (eq 'user-skill
                    (plist-get (plist-get captured-args :kwargs) :trigger)))

        ;; Fire the callback with a fork outcome; expect the data buffer
        ;; to grow an assistant response carrying `gptel response'.
        (with-current-buffer data-buf
          (let ((before (buffer-size)))
            (funcall (plist-get captured-args :callback)
                     '(:status ok :kind fork
                               :result "FORK-RESULT-BODY"
                               :agent-id "myfork--1"))
            (should (> (buffer-size) before))
            (let ((text (buffer-string)))
              (should (string-match-p "FORK-RESULT-BODY" text)))
            ;; The inserted region carries `gptel response' so the view
            ;; renderer treats it as an assistant turn.
            (goto-char (point-max))
            (let ((response-pos
                   (text-property-any (point-min) (point-max)
                                      'gptel 'response)))
              (should response-pos)
              (should (string-match-p
                       "FORK-RESULT-BODY"
                       (buffer-substring-no-properties
                        response-pos (point-max)))))))
        (with-current-buffer data-buf
          (should-not mevedel--current-request)
          (should (= 1 (mevedel-session-turn-count session)))
          (should (equal (list session data-buf) save-called))
          (should (equal '(" Ready" success) status-called)))))

  :doc "fork result prompt rewrite audit rerenders on the submitted user turn"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (_skill _args callback &rest _)
                   (setq captured-args (list :callback callback))))
                ((symbol-function 'mevedel-session-persistence-save)
                 (lambda (&rest _) "saved"))
                ((symbol-function 'gptel--update-status)
                 (lambda (&rest _))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (funcall (plist-get captured-args :callback)
                   '(:status ok :kind fork
                             :result "agent body"
                             :agent-id "myfork--audit"
                             :hook-audits
                             ((:type prompt-rewrite
                                     :event "UserPromptExpansion"
                                     :original "Original body"
                                     :submitted "Expanded body"
                                     :reason "expanded"))
                             :render-data
                             (:kind agent-transcript
                                    :agent-id "myfork--audit"
                                    :agent-type "general-purpose"
                                    :description "run"
                                    :status completed
                                    :body "agent body"))))
        (with-current-buffer data-buf
          (let* ((text (buffer-string))
                 (audit-pos (string-search
                             "<!-- mevedel-hook-audit -->" text))
                 (response-pos (and audit-pos
                                    (string-search gptel-response-separator
                                                   text (1+ audit-pos))))
                 (render-data (cdr (mevedel-pipeline-extract-render-data
                                    text))))
            (should (string-search "<!-- mevedel-render-data -->" text))
            (should audit-pos)
            (should response-pos)
            (should (< audit-pos response-pos))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-hook-audit -->")
            (should (eq (get-text-property (match-beginning 0) 'gptel)
                        'ignore))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (should (eq (get-text-property (match-beginning 0) 'gptel)
                        'ignore))
            (should-not (plist-get render-data :hook-audits))))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (let* ((text (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))
                 (hook-pos (string-search "hook changed prompt" text))
                 (agent-pos (string-search
                             "Agent: general-purpose -- run" text)))
            (should (string-match-p "Agent: general-purpose -- run" text))
            (should (string-match-p "hook changed prompt" text))
            (should (< hook-pos agent-pos))
            (should-not (string-match-p "Original body" text)))
          (goto-char (point-min))
          (search-forward "hook changed prompt")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "UserPromptExpansion" text))
            (should (string-match-p "Original body" text))
            (should (string-match-p "Expanded body" text)))))))

  :doc "fork error stops the spinner and does not insert a response"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (_skill _args callback &rest _)
                   (setq captured-args (list :callback callback))))
                ((symbol-function 'message)
                 (lambda (&rest _))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (should mevedel--current-request))
        ;; Drive the error branch.
        (with-current-buffer data-buf
          (let ((before (buffer-string)))
            (funcall (plist-get captured-args :callback)
                     '(:status error :reason boom :message "boom"))
            ;; No response inserted.
            (should (equal before (buffer-string)))
            (should-not mevedel--current-request)))
        ;; Request progress was removed by `--stop-spinner'.
        (with-current-buffer view-buf
          (should-not (mevedel-view--request-progress-visible-p)))))))
)

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
                   (let ((fsm
                          (gptel-request
                              nil
                            :buffer (current-buffer)
                            :transforms
                            '(mevedel-skills--transform-expand-inline-attachments)
                            :dry-run t)))
                     (setq request-data
                           (format "%S"
                                   (plist-get (gptel-fsm-info fsm)
                                              :data)))))))
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
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "mevedel-render-data" expanded)))
          (mevedel-view-toggle-section))
        (should send-called)
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "Expanded hello" text))
            (should (string-search "<!-- mevedel-render-data -->" text))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (should (eq 'ignore
                        (get-text-property (match-beginning 0)
                                           'gptel)))
            (should (equal
                     (mevedel-pipeline--strip-render-data-blocks text)
                     "\n\n*** Expanded hello\n"))))
	        (with-current-buffer view-buf
	          (mevedel-view--full-rerender)
	          (let ((text (buffer-substring-no-properties
	                       (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "\\$myskill hello" text))
	            (should (string-match-p "Prompt" text))
	            (should-not (string-match-p "Expanded hello" text)))
	          (goto-char (point-min))
	          (search-forward "Prompt")
	          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
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
            (should-not (string-match-p "model-only context" text)))
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
	          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "model-only context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "rewritten prompt" text))
          (should (string-match-p "model-only context" text))
          (should (string-match-p "<!-- mevedel-hook-audit -->" text))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
	          (should (string-match-p "hook changed prompt" text))
	          (should (string-match-p "hook context added" text))
	          (should-not (string-match-p "model-only context" text))))))

  :doc "inline attachment failure rolls back echoed prompt"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded"
       :context 'inline
       :user-invocable-p t)
    (let ((path (make-temp-file "mevedel-inline-rollback-"))
          send-called
          message-text)
      (unwind-protect
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest args)
                       (apply #'mevedel-skills--gptel-send-advice
                              (lambda (&rest _)
                                (setq send-called t))
                              args)))
                    ((symbol-function 'mevedel-skills-invoke)
                     (lambda (_skill _args callback &rest _)
                       (funcall callback
                                '(:status error
                                  :reason blocked
                                  :message "blocked"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (mevedel-session-add-dropped-file-grant session path)
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert (format "Please use $myskill @file:%s" path))
              (mevedel-view-send)
              (should-not send-called)
              (should (equal (format "Please use $myskill @file:%s" path)
                             (mevedel-view--input-text)))
              (should-not (mevedel-view-history--entries))
              (should-not (mevedel-view--request-progress-visible-p))
              (let ((history (buffer-substring-no-properties
                              (point-min) mevedel-view--input-marker)))
                (should-not (string-match-p "Please use \\$myskill"
                                            history))))
            (with-current-buffer data-buf
              (should-not (string-match-p "Please use \\$myskill"
                                          (buffer-string))))
            (should (equal (list (expand-file-name path))
                           (mevedel-session-dropped-file-grants session)))
            (should-not (mevedel-session-active-dropped-file-grants session))
            (should (string-match-p "Inline skill failed: blocked"
                                    message-text)))
        (delete-file path)))))

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

(mevedel-deftest mevedel-view-send/queued-user-messages ()
  ,test
  (test)

  :doc "plain input during an active request queues instead of sending"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "follow up")
          (mevedel-view-send)
          (should-not send-called)
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("follow up")
                         (mevedel-view-history--entries)))
          (should (equal "1 queued message pending"
                         (mevedel-view--interaction-count-label)))
          (should (string-match-p "follow up"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      (should (equal "follow up"
                     (plist-get
                      (car (mevedel-session-queued-user-messages session))
                      :input)))
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))

  :doc "queued inline skill waits for request-time transforms"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "alpha"
       :body "Alpha body"
       :context 'inline
       :user-invocable-p t)
    (let* ((data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           position
           fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (setq position (copy-marker (point-max) nil)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 (lambda () nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "please use $alpha")
          (mevedel-view-send)))
      (let ((entry (car (mevedel-session-queued-user-messages session))))
        (should entry)
        (should-not (plist-get entry :requires-request-transform))
        (should (mevedel-view--queued-user-message-requires-transform-p
                 entry session)))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (should (= 1 (length (plist-get data :messages))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message"
                                    (buffer-string))))))

  :doc "queued message stays visible across incremental in-flight rendering"
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
        (mevedel-view-send)
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

  :doc "queued message stays visible across in-flight full rerender"
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
      (setf (mevedel-session-queued-user-messages session)
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

  :doc "queued message UI shows edit-batch and clear key hints"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hint" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "follow up" :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-rebuild)
        (should (string-match-p "C-c C-e edit batch; C-c C-q clear"
                                (buffer-string))))))

  :doc "queue-time UserPromptSubmit stores prepared model input"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           seen-prompt)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event event-plist callback &rest _)
                   (setq seen-prompt (plist-get event-plist :prompt))
                   (funcall callback '(:updated-input "rewritten"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _)
                   "<hook-context>\n<hook-event name=\"UserPromptSubmit\">\nctx\n</hook-event>\n</hook-context>")))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should (string-empty-p (mevedel-view--input-text)))))
      (should (equal "draft" seen-prompt))
      (let ((entry (car (mevedel-session-queued-user-messages session))))
        (should (equal "draft" (plist-get entry :input)))
        (should (equal "draft" (plist-get entry :history-input)))
        (should (equal "rewritten" (plist-get entry :display-text)))
        (should (equal "rewritten\n\n<hook-context>\n<hook-event name=\"UserPromptSubmit\">\nctx\n</hook-event>\n</hook-context>"
                       (plist-get entry :model-input))))))

  :doc "queue-time UserPromptSubmit rewrite audit survives drain and rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-audit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback '(:updated-input "rewritten"
                                       :system-message "queued rewrite"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send))
        (should (= 1 (length (mevedel-session-queued-user-messages session))))
        (should (plist-get (car (mevedel-session-queued-user-messages session))
                           :hook-audits))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should sent))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "<!-- mevedel-hook-audit -->" text))
          (goto-char (point-min))
          (search-forward "<!-- mevedel-hook-audit -->")
          (should (eq 'ignore
                      (get-text-property (match-beginning 0)
                                         'gptel)))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Queued message" text))
          (should (string-match-p "hook changed prompt" text))
          (should-not (string-match-p "queued rewrite" text)))
        (goto-char (point-min))
        (search-forward "hook changed prompt")
        (mevedel-view-toggle-section)
        (let ((expanded (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
          (should (string-match-p "queued rewrite" expanded))
          (should (string-match-p "draft" expanded))
          (should (string-match-p "rewritten" expanded))))))

  :doc "queue-time UserPromptSubmit block leaves composer and queue untouched"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-block" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback '(:continue nil
                                       :stop-reason "blocked"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should (string= "draft" (mevedel-view--input-text)))))
      (should-not (mevedel-session-queued-user-messages session))
      (should-not (mevedel-view-history--entries))))

  :doc "queue-time async hook does not clear a changed composer"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-async" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           hook-callback)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq hook-callback callback)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should mevedel-view--prompt-hook-pending)
          (goto-char (point-max))
          (insert " changed")
          (funcall hook-callback nil)
          (should-not mevedel-view--prompt-hook-pending)
          (should (string= "draft changed"
                           (mevedel-view--input-text)))))
      (should (= 1 (length (mevedel-session-queued-user-messages
                            session))))))

  :doc "queue-time async hook schedules fallback if request already ended"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-late" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           hook-callback
           drain-buffer
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq hook-callback callback)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 (lambda ()
                   (setq drain-buffer mevedel--data-buffer)
                   (mevedel-view--run-queued-user-message-drain
                    drain-buffer)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (with-current-buffer view-buf
          (funcall hook-callback nil)))
      (should (eq drain-buffer data-buf))
      (should send-called)
      (should-not (mevedel-session-queued-user-messages session))
      (with-current-buffer data-buf
        (should (string-match-p "<queued-user-message-batch count=\"1\">"
                                (buffer-string))))))

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
      (setf (mevedel-session-plan-queue session)
            (list (list :body "# Plan" :origin "main")))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message-batch"
                                    (buffer-string))))))

  :doc "fallback drain keeps queued entry when inline fork skill blocks"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "forker"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (sent)
      (with-current-buffer data-buf
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "please use $forker"
                        :display-text "please use $forker"
                        :model-input "please use $forker")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer view-buf
        (should (string-empty-p (mevedel-view--input-text))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message-batch"
                                    (buffer-string))))))

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
          (mevedel-view--schedule-late-queued-user-message-drain)))
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
      (setf (mevedel-session-queued-user-messages session)
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
        (should-not (mevedel-session-queued-user-messages session))
        (should (string= "/review" (mevedel-view--input-text))))))

  :doc "fallback drain submits queued messages as one FIFO batch"
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
      (setf (mevedel-session-queued-user-messages session)
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
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should (= 1 sent))
        (should-not (mevedel-session-queued-user-messages session))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p
                     "<queued-user-message-batch count=\"2\">" text))
            (should (< (string-match-p "first" text)
                       (string-match-p "second" text))))))))

  :doc "queued messages do not drain while the request is still active"
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
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "pending" :display-text "pending")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should-not sent)
        (should (mevedel-session-queued-user-messages session)))))

  :doc "editing queued messages restores the whole uncommitted batch"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-edit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (with-current-buffer view-buf
        (mevedel-view--interaction-rebuild)
        (mevedel-view-edit-last-queued-message)
        (should (string= "first\n\nsecond" (mevedel-view--input-text)))
        (should-not (mevedel-session-queued-user-messages session)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should-not sent))))

  :doc "resubmitting an edited queued batch creates one queued entry"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-edit-resubmit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (setf (mevedel-session-queued-user-messages session)
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
	                 (lambda (&rest _) (error "Gptel-send should not run"))))
        (with-current-buffer view-buf
          (mevedel-view--interaction-rebuild)
          (mevedel-view-edit-last-queued-message)
          (mevedel-view-send)))
      (let ((queue (mevedel-session-queued-user-messages session)))
        (should (= 1 (length queue)))
        (should (equal "first\n\nsecond"
                       (plist-get (car queue) :input)))
        (should (equal "first\n\nsecond"
                       (plist-get (car queue) :model-input))))))

  :doc "WAIT drain injects all prepared queued entries without gptel-send"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-drain" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data)))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :model-input "first prepared")
                  (list :input "audited"
                        :model-input "audited prepared"
                        :hook-audits
                        '((:type prompt-rewrite
                                  :event "UserPromptSubmit"
                                  :original "secret draft"
                                  :submitted "audited prepared"
                                  :reason "queued rewrite")))
                  (list :input "second" :model-input "second prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--handle-queued-user-message-inject fsm)
        (should-not sent)
        (should-not (mevedel-session-queued-user-messages session))
        (let ((msgs (plist-get data :messages)))
          (should (= 2 (length msgs)))
          (let ((content (plist-get (aref msgs 1) :content)))
            (should (string-match-p "first prepared" content))
            (should (string-match-p "second prepared" content))
            (should (string-match-p "audited prepared" content))
            (should-not (string-match-p "<!-- mevedel-hook-audit -->"
                                        content))
            (should-not (string-match-p "secret draft" content))
            (should-not (string-match-p "queued rewrite" content))
            (should (< (string-match-p "first prepared" content)
                       (string-match-p "second prepared" content)))))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "first prepared" text))
            (should (string-match-p "<!-- mevedel-hook-audit -->" text))
            (should-not (string-match-p "secret draft" text))
            (let ((audit (car (mevedel-view--hook-audit-records-from-text
                               text))))
              (should (equal "secret draft"
                             (plist-get audit :original))))))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (should (string-match-p "second prepared"
                                  (buffer-string))))))))

  :doc "WAIT drain preserves queued entries in Plan mode"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-plan" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active plan turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data))))
      (setf (mevedel-session-permission-mode session) 'plan)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (= 1 (length (plist-get data :messages))))
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer data-buf
        (should-not (string-match-p "new feedback prepared"
                                    (buffer-string))))))

  :doc "WAIT drain advances response marker after queued batch insertion"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-marker" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           (position nil)
           (data-turn-start nil)
           (fsm nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (insert "*** active turn\n")
        (setq data-turn-start (copy-marker (point) nil))
        (let ((start (point)))
          (insert "assistant partial\n")
          (put-text-property start (point) 'gptel 'response))
        (setq position (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "active turn"))
        (mevedel-view--render-incremental data-buf))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "queued"
                        :model-input "queued prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (with-current-buffer view-buf
        (should (string-match-p "queued prepared"
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
      (with-current-buffer data-buf
        (goto-char position)
        (insert (propertize "assistant response\n" 'gptel 'response))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (< (string-match-p "<queued-user-message-batch" text)
                     (string-match-p "assistant response" text)))
          (should (string-match-p "queued prepared" text))))))

  :doc "WAIT drain renders queued batch before first assistant text"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-first" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           position
           data-turn-start
           fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (insert "*** active turn\n")
        (setq data-turn-start (copy-marker (point) nil))
        (setq position (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "active turn"))
        (mevedel-view--start-spinner "Working..."))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "queued"
                        :model-input "queued prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (label (string-match-p "Queued message" text))
               (queued (string-match-p "queued prepared" text))
               (prompt (string-match-p "\n> " text)))
          (should label)
          (should queued)
          (should prompt)
          (should (< label queued))
          (should (< queued prompt))
          (should-not (string-match-p "<system-reminder>" text))
          (should-not (string-match-p "queued-user-message" text))))))

  :doc "WAIT drain ignores agent FSMs that share the parent session"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-agent-wait" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (inv (mevedel-agent-invocation--create :background-p t))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "agent turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data
                             :mevedel-agent-invocation inv))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--agent-invocation inv))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "main follow-up"
                        :model-input "main follow-up prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (mevedel-session-queued-user-messages session))
      (should (= 1 (length (plist-get data :messages))))
      (with-current-buffer data-buf
        (should-not (string-match-p "main follow-up prepared"
                                    (buffer-string))))))

(mevedel-deftest mevedel-view-send/user-prompt-hooks ()
  ,test
  (test)

  :doc "blocking UserPromptSubmit does not fork, record history, or insert prompt"
  (let* ((root (make-temp-file "mevedel-view-hooks" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks" root "view-hooks"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--stop-prompt-hook)))))))
         fork-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-session--fork-pending t))
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (&rest _)
                       (setq fork-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "blocked prompt")
              (mevedel-view-send)
              (should-not fork-called)
              (should-not (mevedel-view-history--entries))
              (should-not
               (string-match-p
                "blocked prompt"
                (buffer-substring-no-properties
                 (point-min) mevedel-view--input-marker))))
	      (with-current-buffer data-buf
		(should (string-empty-p (buffer-string))))))
      (delete-directory root t)))

  :doc "/plan prompts run UserPromptSubmit and materialize rewind forks"
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
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-session--fork-pending t))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (&rest _)
                       (push 'fork events)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (push 'send events))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "/plan draft")
              (mevedel-view-send)
              (should (equal "draft" mevedel-view-test--seen-prompt))
              (should (equal '(fork send) (nreverse events)))
              (should (string-empty-p (mevedel-view--input-text))))
            (with-current-buffer data-buf
              (let ((text (buffer-string)))
                (should (string-match-p "rewritten prompt" text))
                (should (string-match-p "model-only context" text))
                (should-not (string-match-p "/plan draft" text)))))
          (with-current-buffer view-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should (string-match-p "rewritten prompt" text))
              (should (string-match-p "hook context added" text))
              (should-not (string-match-p "model-only context" text)))))
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

  :doc "inline skill hooks see expanded body and can rewrite it"
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
	          (let ((text (mevedel--strip-hook-audit-blocks
                         (buffer-string))))
	            (should (string-match-p "rewritten prompt" text))
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
            (should-not (string-match-p "model-only context" text)))
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "model-only context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "rewritten prompt" text))
          (should (string-match-p "model-only context" text))))))

  :doc "rewritten fork skill prompt sends as normal prompt without invoking skill"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook)))))))
          invoke-called
          send-called)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (&rest _)
                   (setq invoke-called t)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork original")
          (mevedel-view-send))
        (should send-called)
        (should-not invoke-called)
        (with-current-buffer data-buf
          (let ((text (mevedel--strip-hook-audit-blocks
                       (buffer-string))))
            (should (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "\\$myfork original" text)))))))

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
              (let ((deadline (+ (float-time) 5)))
                (while (and mevedel-view--prompt-hook-pending
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not mevedel-view--prompt-hook-pending)
              (should (= send-count 1)))))
      (delete-directory root t))))

(provide 'test-mevedel-view-composer)
;;; test-mevedel-view-composer.el ends here
