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
(require 'mevedel-permission-queue)
(require 'mevedel-persistence)
(require 'mevedel-review)
(require 'mevedel-goal)
(require 'mevedel-agents)
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

(defun mevedel-view-test--dry-run-request-data ()
  "Return current gptel request data after normal prompt transforms."
  (let ((fsm
         (gptel-request
           nil
           :buffer (current-buffer)
           :dry-run t
           :transforms
           (cons #'mevedel-view--transform-model-input
                 (remove #'mevedel-view--transform-model-input
                         gptel-prompt-transform-functions)))))
    (format "%S" (plist-get (gptel-fsm-info fsm) :data))))

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
               (set-file-modes source-file 0))
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
                    (should (string-match-p
                             (regexp-quote "Please use $alpha")
                             (buffer-string)))
                    (should-not (string-match-p
                                 (regexp-quote
                                  "[skill:alpha -- unavailable]")
                                 (buffer-string))))))))
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

  :doc "ask mode renders an undecorated prompt"
  (let ((prompt (mevedel-view--input-prompt-string 'ask)))
    (should (string= "\n> " prompt))
    (should (eq 'mevedel-view-input-prompt
                (get-text-property 0 'font-lock-face prompt))))

  :doc "auto mode renders its canonical name"
  (let ((prompt (mevedel-view--input-prompt-string 'auto)))
    (should (string= "\n[auto] > " prompt))
    (should (eq 'mevedel-view-permission-mode-auto
                (get-text-property 2 'font-lock-face prompt))))

  :doc "full-auto mode renders its canonical name"
  (let ((prompt (mevedel-view--input-prompt-string 'full-auto)))
    (should (string= "\n[full-auto] > " prompt))
    (should (eq 'mevedel-view-permission-mode-full-auto
                (get-text-property 2 'font-lock-face prompt)))))

(mevedel-deftest mevedel-view--next-permission-mode
  (:doc "cycles permission modes in view order")
  ,test
  (test)

  :doc "ask mode moves to auto"
  (should (eq 'auto
              (mevedel-view--next-permission-mode 'ask)))

  :doc "auto mode moves to full-auto"
  (should (eq 'full-auto
              (mevedel-view--next-permission-mode 'auto)))

  :doc "full-auto mode wraps to ask"
  (should (eq 'ask
              (mevedel-view--next-permission-mode 'full-auto)))

  :doc "nil mode starts at auto"
  (should (eq 'auto
              (mevedel-view--next-permission-mode nil)))

  :doc "unknown mode starts at auto"
  (should (eq 'auto
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
                          :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf)
              (setq-local mevedel-permission-mode 'ask))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'ask)
              (should (eq 'auto
                          (mevedel-view-cycle-permission-mode)))
              (should (eq 'auto
                          (mevedel-session-permission-mode session)))
              (should (eq 'auto
                          (buffer-local-value
                           'mevedel-permission-mode data-buf)))
              (should (eq 'auto mevedel-permission-mode))
              (should (eq saved
                          (default-toplevel-value 'mevedel-permission-mode)))
              (should (string= "\n[auto] > "
                               (buffer-substring-no-properties
                                mevedel-view--input-marker
                                (mevedel-view--input-start)))))
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
                          (mevedel-session-permission-mode session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))

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
        (should (= (window-start) start-before))))))

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
        (should (string-empty-p (mevedel-view--input-text)))))))

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

(mevedel-deftest mevedel-view--queued-user-message-input ()
  ,test
  (test)
  :doc "returns queued input and defaults a missing value to empty text"
  (should (equal "queued" (mevedel-view--queued-user-message-input
                            '(:input "queued"))))
  (should (equal "" (mevedel-view--queued-user-message-input nil))))

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
             (submission
              (list :token token :input input :plan plan
                    :view-buffer view-buf :data-buffer data-buf
                    :before-send (lambda () (setq before t)))))
        (with-current-buffer view-buf
          (setq-local mevedel-view--pending-skill-submission token)
          (cl-letf (((symbol-function 'mevedel-view--forward-input)
                     (lambda (&rest args) (setq forwarded args))))
            (mevedel-view--dispatch-prepared-plan
             submission prepared
             (concat (plist-get prepared :model-input) " $literal")
             nil nil))
          (should-not mevedel-view--pending-skill-submission))
        (should before)
        (should (string-match-p "ALPHA BODY" (nth 7 forwarded)))
        (should (string-match-p (regexp-quote "$literal")
                                (nth 7 forwarded)))
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
        (should (string-match-p "ALPHA BODY" (nth 7 forwarded)))))))

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
            (should-not (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "mevedel-render-data" expanded)))
          (mevedel-view-toggle-section))
        (should send-called)
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p (regexp-quote "$myskill hello") text))
            (should-not (string-match-p "Expanded hello" text))
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
	          (search-forward "Prompt")
	          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
	            (should (string-match-p (regexp-quote "$myskill hello")
                                      expanded))
	            (should-not (string-match-p "Expanded hello" expanded)))))))

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
          (should (string-match-p (regexp-quote "$myskill hello") text))
          (should-not (string-match-p "rewritten prompt" text))
          (should-not (string-match-p "model-only context" text))
          (should (string-match-p "<!-- mevedel-hook-audit -->" text))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
	          (should (string-match-p "hook changed prompt" text))
	          (should-not (string-match-p "hook context added" text))
	          (should-not (string-match-p "model-only context" text))))))

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
                        'mevedel-view--schedule-late-queued-user-message-drain)
                       #'ignore)
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (goto-char (mevedel-view--input-start))
                (insert "Please use $alpha for the queued analysis")
                (mevedel-view-send))
              (let* ((queue (mevedel-session-queued-user-messages session))
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
              (mevedel-view--drain-queued-user-message data-buf))
            (should (string-search "[skill:alpha -- unavailable]"
                                   request-data))
            (should-not (string-search "ORIGINAL ALPHA V1" request-data))
            (should-not (string-search "COMPETING ALPHA" request-data))
            (should-not (mevedel-session-queued-user-messages session))))
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
        (should (string-search prompt (buffer-string)))
        (should-not (string-search "[skill:alpha -- unavailable]"
                                   (buffer-string))))
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
                             #'mevedel-skills--transform-apply-model-override
                             gptel-prompt-transform-functions))
                (mevedel-skills-install session data-buf))
              (let ((transform
                     (symbol-function
                      'mevedel-skills--transform-apply-model-override)))
                (cl-letf
                    (((symbol-function
                       'mevedel-skills--transform-apply-model-override)
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
        (let ((text (buffer-string)))
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
                  'mevedel-view--schedule-late-queued-user-message-drain)
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
          (mevedel-view-send)
          (let ((queued
                 (plist-get
                  (car (mevedel-session-queued-user-messages session))
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
            (should (equal "derived prompt" (buffer-string))))
          (with-current-buffer chat-buffer
            (should-not mevedel--pending-model-input)))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-view--queued-user-message-auto-drain-blocked-p ()
  ,test
  (test)
  :doc "blocks fallback drainage for plan approval and guardian review"
  (let ((session (mevedel-session--create
                  :name "main" :plan-queue '(plan))))
    (should (mevedel-view--queued-user-message-auto-drain-blocked-p session))
    (setf (mevedel-session-plan-queue session) nil
          (mevedel-session-plan-metadata session) '(:guardian-pending t))
    (should (mevedel-view--queued-user-message-auto-drain-blocked-p session))
    (setf (mevedel-session-plan-metadata session) nil)
    (should-not
     (mevedel-view--queued-user-message-auto-drain-blocked-p session))))

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
                          'mevedel-view--schedule-late-queued-user-message-drain)
                         #'ignore))
                (with-current-buffer view-buf
                  (goto-char (mevedel-view--input-start))
                  (insert "Inspect " token " after the current turn")
                  (mevedel-view-send)))
              (let* ((queued
                      (plist-get
                       (car (mevedel-session-queued-user-messages session))
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
                  (mevedel-view--drain-queued-user-message data-buf)))
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
              (should-not (mevedel-session-queued-user-messages session)))))
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
                      'mevedel-view--schedule-late-queued-user-message-drain)
                     #'ignore))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Inspect @file:queued.txt")
              (mevedel-view-send)))
          (let* ((queued
                  (plist-get
                   (car (mevedel-session-queued-user-messages session))
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
              (mevedel-view--drain-queued-user-message data-buf)))
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
          (should-not (mevedel-session-queued-user-messages session)))
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
                      'mevedel-view--schedule-late-queued-user-message-drain)
                     #'ignore))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "Consult " token)
              (mevedel-view-send))
            (let* ((queued
                    (plist-get
                     (car (mevedel-session-queued-user-messages session))
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
              (mevedel-view--drain-queued-user-message data-buf)))
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
          (should-not (mevedel-session-queued-user-messages session)))
      (delete-directory root t)))

  :doc "plain input during guardian review joins the intervention queue"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-guardian" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           send-called)
      (setf (mevedel-session-plan-metadata session)
            '(:guardian-pending t))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--current-request nil))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 #'ignore))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "stop before implementing")
          (mevedel-view-send)))
      (should-not send-called)
      (should (equal "stop before implementing"
                     (plist-get
                      (car (mevedel-session-queued-user-messages session))
                      :input)))))

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
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sends))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Queue $alpha exactly")
          (mevedel-view-send))
        (should (= 0 hooks))
        (should (= 0 sends))
        (let* ((input (plist-get
                       (car (mevedel-session-queued-user-messages session))
                       :input))
               (start (string-match "\\$alpha" input)))
          (should (plist-get
                   (get-text-property start 'mevedel-mention-binding input)
                   :source-file)))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (mevedel-view--drain-queued-user-message data-buf)
        (should (= 1 hooks))
        (should (= 1 sends))
        (should-not (mevedel-session-queued-user-messages session)))))

  :doc "an unavailable queued binding annotates, sends, and leaves the queue"
  (mevedel-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let (sent original)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 #'ignore))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "Queue $alpha exactly")
          (mevedel-view-send)))
      (setq original (car (mevedel-session-queued-user-messages session)))
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
        (mevedel-view--drain-queued-user-message data-buf))
      (should sent)
      (should-not (mevedel-session-queued-user-messages session))))

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
        (mevedel-view--drain-queued-user-message data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))))

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
        (mevedel-view--drain-queued-user-message data-buf)
        (should (= 1 sent))
        (should (equal "second"
                       (plist-get
                        (car (mevedel-session-queued-user-messages session))
                        :input)))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "first" text))
            (should-not (string-match-p "second" text)))))))

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
        (mevedel-view--drain-queued-user-message data-buf)
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
        (mevedel-view--drain-queued-user-message data-buf)
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
        (should-not (plist-member (car queue) :model-input))))))

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
                   (funcall callback "expanded" "hook context" nil)))
                ((symbol-function 'mevedel-view-history-add) #'ignore)
                ((symbol-function 'mevedel-view--fork-if-pending) #'ignore)
                ((symbol-function 'mevedel-view--clear-input) #'ignore)
                ((symbol-function 'mevedel-goal-start)
                 (lambda (objective display &optional policy hook-context)
                   (setq started (list objective display policy hook-context)
                         started-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-view--send-local-goal "/goal draft" "draft")))
      (should (eq data-buf started-buffer))
      (should (equal '("expanded" "expanded" supervised "hook context")
                     started))))
  :doc "strips the auto selector and starts an automatic Goal"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "main"))
          started)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-view--run-prompt-submit-hook)
                 (lambda (objective _input callback)
                   (should (equal "ship it" objective))
                   (funcall callback objective nil nil)))
                ((symbol-function 'mevedel-view-history-add) #'ignore)
                ((symbol-function 'mevedel-view--fork-if-pending) #'ignore)
                ((symbol-function 'mevedel-view--clear-input) #'ignore)
                ((symbol-function 'mevedel-goal-start)
                 (lambda (objective display policy &optional hook-context)
                   (setq started
                         (list objective display policy hook-context)))))
        (with-current-buffer view-buf
          (mevedel-view--send-local-goal
           "/goal auto ship it" "auto ship it")))
      (should (equal '("ship it" "ship it" automatic nil) started)))))

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

  :doc "/goal prompts run UserPromptSubmit and materialize rewind forks"
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
                    ((symbol-function 'mevedel-goal-start)
                     (lambda (objective display &optional policy hook-context)
                       (push (list objective display policy hook-context)
                             events))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "/goal draft")
              (mevedel-view-send)
              (should (equal "draft" mevedel-view-test--seen-prompt))
              (setq events (nreverse events))
              (should (eq 'fork (car events)))
              (should (equal "rewritten prompt" (cadr (cadr events))))
              (should (string-match-p "rewritten prompt"
                                      (car (cadr events))))
              (should-not (string-match-p "model-only context"
                                          (car (cadr events))))
              (should (string-match-p "model-only context"
                                      (nth 3 (cadr events))))
              (should (string-empty-p (mevedel-view--input-text)))))
          (with-current-buffer view-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should-not (string-match-p "/goal draft" text)))))
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
	          (let ((text (mevedel--strip-hook-audit-blocks
                         (buffer-string))))
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
            (should-not (string-match-p "model-only context" text)))
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "model-only context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should-not (string-match-p "rewritten prompt" text))
          (should (string-match-p (regexp-quote "$myskill hello") text))
          (should-not (string-match-p "Expanded hello" text))
          (should (string-match-p "model-only context" text))))))

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
