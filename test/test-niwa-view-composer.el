;;; test-niwa-view-composer.el --- View composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests composer editing, prompt submission, root dispatch, and send flow.

;;; Code:

(require 'niwa-notes)
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar org-mode-hook)

(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-fontify)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-execution-target)
(require 'niwa-transcript)
(require 'niwa-structs)
(require 'niwa-pipeline)
(require 'niwa-tool-media)
(require 'niwa-tool-registry)
(require 'niwa-tool-render-data)
(require 'niwa-mentions)
(require 'niwa-skills-plan)
(require 'niwa-skills-ui)
(require 'niwa-workspace)
(require 'niwa-worktree)
(require 'niwa-side-conversation)
(require 'niwa-file-state)
(require 'niwa-pending-inputs)
(require 'niwa-session-persistence)
(require 'niwa-tool-ui)
(require 'niwa-tools)
(require 'niwa-permission-queue)
(require 'niwa-pending-inputs)
(require 'niwa-persistence)
(require 'niwa-review)
(require 'niwa-goal)
(require 'niwa-agents)
(require 'niwa-agent-control)
(require 'niwa-agent-runtime)
(require 'niwa-hooks)
(require 'niwa-view-disclosure)
(require 'niwa-view-zone)
(require 'niwa-view-history)
(require 'niwa-resource-capf)
(require 'gptel-request)
(require 'mcp)

(defmacro niwa-view-composer-test--without-admission (&rest body)
  "Run BODY with in-memory fixtures exempt from portable lease admission.
Control-transfer quiescing stays real."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
              #'ignore)
             ((symbol-function 'niwa-session-durability-lease-owned-p)
              (lambda (&rest _) t)))
     ,@body))

(defmacro niwa-view-composer-test--in-composer (view &rest body)
  "Run BODY in VIEW's composer buffer, where raw editing commands act.
Pass VIEW explicitly to composer bounds used inside BODY."
  (declare (indent 1) (debug t))
  `(with-current-buffer (or (niwa-view-composer-buffer ,view)
                            (error "No composer for view"))
     ,@body))

(defun niwa-view-composer-test--status-rows (count)
  "Reconcile COUNT status rows through the real zone preserver."
  (niwa-view-zone-reconcile
   'status (niwa-view--status-anchor) (niwa-view--status-anchor)
   (when (> count 0)
     (list (list :namespace 'status :id 'anchor-test :priority 1
                 :body (mapconcat (lambda (n) (format "  row %d\n" n))
                                  (number-sequence 1 count) ""))))))

(ert-deftest niwa-view-composer-height-budget ()
  "Fit one draft row plus chrome, without giving a tall draft the transcript."
  (should (= 4 (niwa-view-composer-height 0 30 3 1)))
  (should (= 6 (niwa-view-composer-height 3 30 3 1)))
  (should (= 10 (niwa-view-composer-height 40 32 3 1)))
  (should (= 4 (niwa-view-composer-height 40 8 3 1)))
  (should (= 106 (niwa-view-composer-height 900 320 48 16))))

(niwa-deftest niwa-view--history-reader-viewport
  (:doc "A history reader keeps its viewport through status growth and shrink")
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 1)
        (dotimes (n 100) (insert (format "transcript %d\n" n))))
      (niwa-view-composer-insert "> draft\nsecond line")
      (let ((start (save-excursion
                     (goto-char (point-max))
                     (forward-line -10) (point))))
        (set-window-start nil start t)
        (goto-char start)
        (forward-line 2)
        (let ((reading (point))
              (y (niwa-view-test--position-screen-y (point) (selected-window))))
          (dolist (count '(4 1 0))
            (niwa-view-composer-test--status-rows count)
            (should (= reading (point)))
            (should (= start (window-start)))
            (should (= y (niwa-view-test--position-screen-y
                          (point) (selected-window)))))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t))))))))

(niwa-deftest niwa-view--history-reader-windows ()
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "history\n"))
      (goto-char ,position)
      (should (equal ,expected (niwa-view--history-reader-windows)))))
  (position expected)
  :doc "Transcript point is captured before mutation" (point-min) (list (selected-window))
  :doc "Status and interaction chrome are not history readers" niwa-view--status-marker nil)

(niwa-deftest niwa-view--history-reader-scrolled
  (:doc "A scrolled transcript window keeps its start as status grows")
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 1)
        (dotimes (n 200) (insert (format "transcript %d\n" n))))
      (goto-char (point-min))
      (forward-line 20)
      (let ((start (point)))
        (set-window-start nil start)
        (should-not (niwa-view-test--position-screen-y (point-max) (selected-window)))
        (niwa-view-composer-test--status-rows 5)
        (should (= start (window-start)))))))

(niwa-deftest niwa-view--clamp-window-starts ()
  (let ((scenario ,scenario))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (when (memq scenario '(transcript transcript-render))
          (split-window-below -8))
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (forward-line 1)
          (dotimes (n 200) (insert (format "transcript %d\n" n))))
        (niwa-view-composer-insert "> draft\nsecond line")
        (let* ((height (window-body-height))
               (card (niwa-view--interaction-register
                      (list :kind 'ask :id 'viewport-test
                            :body (mapconcat
                                   (lambda (n) (format "option %d" n))
                                   (number-sequence 1 (* 2 height)) "\n")))))
          (goto-char (overlay-start card))
          (set-window-start
           nil (cond
                ((eq scenario 'hidden) (overlay-start card))
                ((memq scenario '(transcript transcript-render))
                 (save-excursion (forward-line -5) (point)))
                (t
                 (save-excursion
                   (goto-char (point-max))
                   (vertical-motion (if (eq scenario 'full) (- 1 height) -5))
                   (point)))) t)
          (if (memq scenario '(hidden transcript transcript-render))
              (should-not (niwa-view-test--position-screen-y (point-max) (selected-window)))
            (should (= (if (eq scenario 'full) (1- height) 5)
                       (niwa-view-test--position-screen-y (point-max) (selected-window)))))
          (let ((saved-start (window-start)))
            (should (= (alist-get (selected-window)
                                  (niwa-view--window-tail-lengths))
                       (- (point-max) saved-start)))
            (when (memq scenario '(transcript transcript-render))
              (should-not (niwa-view-zone-bounds-at saved-start)))
            (pcase scenario
              ((or 'render 'transcript-render)
               (niwa-view--call-preserving-window-state
                (lambda () (niwa-view--interaction-unregister 'viewport-test))))
              ('truncate
               (niwa-view--interaction-register
                (list :kind 'ask :id 'viewport-test :body "shorter card")))
              ('survive
               (niwa-view--call-preserving-window-state #'ignore))
              (_ (niwa-view--interaction-unregister 'viewport-test)))
            (ert-info ((format "viewport after %s settlement" scenario))
              (should (= (window-start)
                         (cond
                          ((eq scenario 'survive) saved-start)
                          (t
                           (save-excursion
                             (goto-char (point-max))
                             (vertical-motion (- (1- height)))
                             (point))))))
              (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))))))))
  (scenario)
  :doc "settlement fills the last page when the transcript end was hidden"
  'hidden
  :doc "settlement fills the last page when the transcript end was high on screen"
  'high
  :doc "removing the card containing a full window start keeps that window full"
  'full
  :doc "outer render restoration fills the page after rows disappear"
  'render
  :doc "truncation past a saved start fills the last page"
  'truncate
  :doc "surviving fragment starts keep deliberate blank space below"
  'survive
  :doc "a split window starting in transcript fills after card removal"
  'transcript
  :doc "outer restoration fills a transcript-start window after card removal"
  'transcript-render)

(niwa-deftest niwa-view--clamp-window-starts-force
  (:doc "only a lost logical start fills old blanks when the character tail did not shrink")
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (dotimes (n 200) (insert (format "line %d\n" n))))
      (let* ((window (selected-window))
             (start (save-excursion (forward-line -5) (point)))
             (tails (list (cons window (- (point-max) start)))))
        (set-window-start window start t)
        (niwa-view--clamp-window-starts tails)
        (should (= start (window-start window)))
        (niwa-view--clamp-window-starts tails (list window))
        (should (= (window-start window)
                   (save-excursion
                     (goto-char (point-max))
                     (vertical-motion (- (1- (window-body-height window))) window)
                     (point))))))))

(defun niwa-view-composer-test--owner (symbol)
  "Return the source feature basename that defines SYMBOL."
  (file-name-base (or (symbol-file symbol 'defun) "")))

(niwa-deftest niwa-view--fontify-composer
  (:before-each (niwa-view--release-markdown-fontify-buffer)
   :after-each (niwa-view--release-markdown-fontify-buffer))
  ,test
  (test)
  :doc "redisplay highlights only the draft and updates multiline syntax"
  ;; Use a built-in fontifier so this contract also runs without TS grammars.
  (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
             (lambda () 'emacs-lisp-mode)))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view-composer-test--in-composer view-buf
          (buffer-enable-undo)
          (niwa-view-composer-insert "> \"first\nsecond\" @file:/tmp/note.md")
          (niwa-mention-bindings-set
           (- (point-max) (length "@file:/tmp/note.md")) (point-max)
           '(:kind file :token "@file:/tmp/note.md" :path "/tmp/note.md")))
        (let* ((start (niwa-view-composer-draft-start view-buf))
               (transcript (buffer-string))
               (draft (niwa-view-composer-draft-text view-buf t))
               (position (niwa-view-composer-test--in-composer view-buf
                           (point)))
               (undo (niwa-view-composer-test--in-composer view-buf
                       buffer-undo-list)))
          (niwa-view-composer-test--in-composer view-buf
            (jit-lock-refontify start)
            (setq undo buffer-undo-list)
            (jit-lock-fontify-now)
            (should (eq (get-text-property (+ start 4) 'font-lock-face)
                        'font-lock-string-face))
            (should (equal (niwa-view-composer-draft-text view-buf t) draft))
            (should (= (point) position))
            (should (equal buffer-undo-list undo))
            (should (equal (get-text-property
                            (1- (niwa-view-composer-draft-end view-buf))
                            'niwa-mention-binding)
                           '(:kind file :token "@file:/tmp/note.md"
                             :path "/tmp/note.md")))
            (should-not (text-property-not-all
                         start (niwa-view-composer-draft-end view-buf)
                         'invisible nil)))
          (should (equal-including-properties
                   transcript (buffer-string)))
          (niwa-view-composer-test--in-composer view-buf
            (goto-char (+ start 2))
            (delete-char 1)
            (jit-lock-fontify-now (line-beginning-position) (line-end-position))
            (should-not (get-text-property (+ start 9) 'font-lock-face)))))))

  :doc "native Markdown highlights the final heading while typing without newline"
  (progn
    (require 'markdown-ts-mode nil t)
    (skip-unless (niwa-view--markdown-fontify-mode))
    (niwa-view-test--with-buffers
      (niwa-view-composer-test--in-composer view-buf
        (niwa-view-composer-insert "> quote\n# H\n")
        (jit-lock-fontify-now)
        (delete-char -1)
        (jit-lock-fontify-now)
        (should (equal "> quote\n# H" (niwa-view-composer-draft-text nil t)))
        (should (memq 'markdown-ts-heading-1
                      (ensure-list (get-text-property
                                    (1- (niwa-view-composer-draft-end))
                                    'font-lock-face))))
        (should-not (text-property-not-all (niwa-view-composer-draft-start)
                                           (niwa-view-composer-draft-end)
                                           'invisible nil)))))

  :doc "missing grammars leave the draft editable and clear stale highlighting"
  (cl-letf (((symbol-function 'niwa-view--markdown-fontify-mode)
             (lambda () nil)))
    (niwa-view-test--with-buffers
      (niwa-view-composer-test--in-composer view-buf
        (niwa-view-composer-insert (propertize "**draft**" 'font-lock-face 'bold))
        (jit-lock-fontify-now)
        (should (equal (niwa-view-composer-draft-text nil t) "**draft**"))
        (should-not (get-text-property (niwa-view-composer-draft-start)
                                       'font-lock-face))))))

(niwa-deftest niwa-view-composer-ownership ()
  ,test
  (test)
  :doc "owns composer editing and submission"
  (dolist (symbol '(niwa-view-composer-draft-start
                    niwa-view--input-text
                    niwa-view--clear-input
                    niwa-view-refresh-input-prompt
                    niwa-view-send
                    niwa-view--send-root
                    niwa-view--forward-input
                    niwa-view--run-prompt-submit-hook))
    (should (equal "niwa-view-composer"
                   (niwa-view-composer-test--owner symbol))))
  :doc "delegates file input and pending-input lifecycle to focused owners"
  (dolist (symbol '(niwa-view--insert-dropped-file-mentions
                    niwa-view--install-dnd
                    niwa-view-yank-dwim))
    (should (equal "niwa-view-input-files"
                   (niwa-view-composer-test--owner symbol))))
  (dolist (symbol '(niwa-view--queue-follow-up
                    niwa-view-send-follow-up
                    niwa-view--schedule-follow-up-drain))
    (should (equal "niwa-pending-inputs"
                   (niwa-view-composer-test--owner symbol))))

  :doc "retains root request cancellation"
  (should (equal "niwa-view-composer"
                 (niwa-view-composer-test--owner
                  'niwa-view-abort))))


;;
;;; Test helpers

(defun niwa-view-test--capf-candidates (capf &optional prefix)
  "Return completion candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

(defun niwa-view-test--skill-hint-string ()
  "Return the current skill argument hint overlay string."
  (and (overlayp niwa-view--skill-argument-hint-overlay)
       (overlay-buffer niwa-view--skill-argument-hint-overlay)
       (overlay-get niwa-view--skill-argument-hint-overlay
                    'after-string)))

(defun niwa-view-test--write-skill (dir name frontmatter &optional body)
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

(defun niwa-view-test--complete-skill (candidate)
  "Replace the current `$' completion fragment with CANDIDATE."
  (with-current-buffer (niwa-view-composer-buffer)
    (let* ((capf (niwa-view-slash-capf))
           (exit (and capf (plist-get (nthcdr 3 capf) :exit-function))))
      (should capf)
      (delete-region (nth 0 capf) (nth 1 capf))
      (insert candidate)
      (funcall exit candidate 'finished))))

(defun niwa-view-test--bound-source-failure-case (mode)
  "Assert unavailable bound source behavior for failure MODE."
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-inline-bound-failure-" t))
         (project-skills (file-name-concat root ".niwa/skills"))
         (user-skills
          (make-temp-file "niwa-inline-bound-failure-user-" t))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "inline-bound-failure"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         source-file)
    (unwind-protect
        (progn
          (setq source-file
                (niwa-view-test--write-skill
                 project-skills "alpha"
                 "name: alpha\ndescription: Project alpha\ncontext: inline\n"
                 "ORIGINAL ALPHA BODY"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws)
              (niwa-skills-install session data-buf))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "> diagnosis\nPlease use $al"
                                             (niwa-view-composer-draft-start))
              (niwa-view-test--complete-skill "alpha"))
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
               (niwa-view-test--write-skill
                project-skills "alpha"
                (concat "name: alpha\ndescription: Project alpha\n"
                        "context: inline\nuser-invocable: false\n")
                "ORIGINAL ALPHA BODY"))
              ('disabled
               (with-current-buffer data-buf
                 (niwa-skills-set-enabled
                  (niwa-session-get-skill session "alpha") nil))))
            (niwa-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING ALPHA BODY")
            (setq niwa-skill-dirs
                  (list ".niwa/skills" user-skills))
            (with-current-buffer data-buf
              (niwa-skills-install session data-buf))
            (let (sent message-text raised)
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _)
                           (setq sent
                                 (niwa-view-test--dry-run-request-data))))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq message-text
                                 (apply #'format format-string args)))))
                (with-current-buffer view-buf
                  (setq raised
                        (condition-case err
                            (progn (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) nil)
                          (user-error (error-message-string err))))
                  (should sent)
                  (should-not raised)
                  (should (string-match-p
                           (regexp-quote "[skill:alpha -- unavailable]")
                           sent))
                  (should-not (string-match-p "COMPETING ALPHA BODY" sent))
                  (should (string-prefix-p (format "niwa [%s]: " root)
                                           message-text))
                  (should (string-empty-p (niwa-view-composer-draft-text nil t)))
                  (should (equal "> diagnosis\nPlease use $alpha"
                                 (car (niwa-view-history--entries))))
                  (with-current-buffer data-buf
                    (let ((text
                           (niwa-tool-render-data-strip
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

(defun niwa-view-test--stop-prompt-hook (_event)
  "Block prompt submission in view-send cases."
  '(:continue nil :stop-reason "blocked"))

(defvar niwa-view-test--seen-prompt nil)

(defun niwa-view-test--rewrite-prompt-hook (event)
  "Capture prompt EVENT and rewrite it in view-send cases."
  (setq niwa-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"))

(defun niwa-view-test--rewrite-prompt-hook-with-context (event)
  "Capture prompt EVENT, rewrite it, and add hook context."
  (setq niwa-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :additional-context "hook policy context"))

(defun niwa-view-test--rewrite-prompt-hook-with-message (event)
  "Capture prompt EVENT, rewrite it, and add a user-facing message."
  (setq niwa-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :system-message "changed by test hook"))

(defun niwa-view-test--add-prompt-hook-context (event)
  "Capture prompt EVENT and add model-visible context."
  (setq niwa-view-test--seen-prompt (plist-get event :prompt))
  '(:additional-context "ordinary steering hook context"))


;;
;;; Composer editing

(niwa-deftest niwa-view--input-prompt-string
  (:doc "separates the prompt from the rail's permission badge")
  ,test
  (test)

  :doc "ask mode shows its badge like every other mode"
  (let ((prompt (niwa-view--input-prompt-string))
        (badge (niwa-view--composer-mode-badge 'ask)))
    (should (string= "> " prompt))
    (should (string= "[ask]" badge))
    (should (eq 'niwa-view-permission-mode-ask
                (get-text-property 2 'font-lock-face badge))))

  :doc "edits mode renders its canonical name"
  (let ((prompt (niwa-view--input-prompt-string))
        (badge (niwa-view--composer-mode-badge 'edits)))
    (should (string= "> " prompt))
    (should (string= "[edits]" badge))
    (should (eq 'niwa-view-permission-mode-edits
                (get-text-property 2 'font-lock-face badge))))

  :doc "full-auto mode renders its canonical name"
  (let ((prompt (niwa-view--input-prompt-string))
        (badge (niwa-view--composer-mode-badge 'full-auto)))
    (should (string= "> " prompt))
    (should (string= "[full-auto]" badge))
    (should (eq 'niwa-view-permission-mode-full-auto
                (get-text-property 2 'font-lock-face badge))))

  :doc "Plan shows the retained permission mode"
  (let ((niwa--session
         (niwa-session--create :authority-mode 'portable :name "main" :plan-mode t)))
    (should (string= "[Plan · full-auto]"
                     (niwa-view--composer-mode-badge 'full-auto)))
    (should (string= "> "
                     (niwa-view--input-prompt-string)))))

(niwa-deftest niwa-view--next-permission-mode
  (:doc "cycles permission modes in view order")
  ,test
  (test)

  :doc "ask mode moves to edits"
  (should (eq 'edits
              (niwa-view--next-permission-mode 'ask)))

  :doc "edits mode moves to full-auto"
  (should (eq 'full-auto
              (niwa-view--next-permission-mode 'edits)))

  :doc "full-auto mode wraps to ask"
  (should (eq 'ask
              (niwa-view--next-permission-mode 'full-auto)))

  :doc "unknown workflow states start at edits"
  (should (eq 'edits
              (niwa-view--next-permission-mode 'plan)))

  :doc "nil mode starts at edits"
  (should (eq 'edits
              (niwa-view--next-permission-mode nil)))

  :doc "unknown mode starts at edits"
  (should (eq 'edits
              (niwa-view--next-permission-mode 'bogus))))

(niwa-deftest niwa-view--plan-mode-p
  (:doc "reads Plan state from the current view session")
  ,test
  (test)
  (let ((niwa--session
         (niwa-session--create :authority-mode 'portable :name "main" :plan-mode t)))
    (should (niwa-view--plan-mode-p))
    (setf (niwa-session-plan-mode niwa--session) nil)
    (should-not (niwa-view--plan-mode-p))))

(niwa-deftest niwa-view--occupied-root-workflow ()
  ,test
  (test)

  :doc "idle session is not occupied"
  (should-not (niwa-view--occupied-root-workflow
               (niwa-session--create :authority-mode 'portable
                                        :name "main")))

  :doc "plan approval names its cause"
  (let ((session (niwa-session--create :authority-mode 'portable
                                          :name "main")))
    (setf (niwa-session-pending-plan-approval session) 'pending)
    (should (eq 'plan-approval
                (niwa-view--occupied-root-workflow session))))

  :doc "retained implementation retry names its cause"
  (let ((session (niwa-session--create :authority-mode 'portable
                                          :name "main")))
    (setf (niwa-session-plan-metadata session)
          '(:implementation-retry (:selection (:location here))))
    (should (eq 'implementation-retry
                (niwa-view--occupied-root-workflow session))))

  :doc "unfinished goal names its cause; complete goal does not occupy"
  (let ((session (niwa-session--create :authority-mode 'portable
                                          :name "main")))
    (setf (niwa-session-goal session)
          (niwa-goal--create :id "g1" :objective "obj" :status 'paused))
    (should (eq 'goal (niwa-view--occupied-root-workflow session)))
    (setf (niwa-goal-status (niwa-session-goal session))
          'budget-limited)
    (should (eq 'goal-budget
                (niwa-view--occupied-root-workflow session)))
    (setf (niwa-goal-status (niwa-session-goal session)) 'complete)
    (should-not (niwa-view--occupied-root-workflow session))))

(niwa-deftest niwa-view--occupied-root-workflow-error ()
  ,test
  (test)

  :doc "implementation retry hints the retry command"
  (should (string-match-p
           "niwa-retry-plan-implementation"
           (cadr (should-error
                  (niwa-view--occupied-root-workflow-error
                  'implementation-retry)
                  :type 'user-error))))

  :doc "plan approval points at the pending approval"
  (should (string-match-p
           "approval"
           (cadr (should-error
                  (niwa-view--occupied-root-workflow-error 'plan-approval)
                  :type 'user-error))))

  :doc "ordinary and budget-limited Goals name valid commands"
  (should (string-match-p
           "/goal resume"
           (cadr (should-error
                  (niwa-view--occupied-root-workflow-error 'goal)
                  :type 'user-error))))
  (should (string-match-p
           "/goal budget"
           (cadr (should-error
                  (niwa-view--occupied-root-workflow-error 'goal-budget)
                  :type 'user-error))))

  :doc "unknown cause keeps the generic follow-up hint"
  (should (string-match-p
           "C-c TAB"
           (cadr (should-error
                  (niwa-view--occupied-root-workflow-error nil)
                  :type 'user-error)))))

(niwa-deftest niwa-view-cycle-permission-mode
  (:quiet t :doc "cycles the current session mode and refreshes the prompt")
  ,test
  (test)

  :doc "cycles session mode and refreshes prompt"
  (let ((saved (default-toplevel-value 'niwa-permission-mode)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let ((session (niwa-session--create
                          :authority-mode 'portable
                          :name "main"
                          :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (setq-local niwa--view-buffer view-buf)
              (setq-local niwa-permission-mode 'ask))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (setq-local niwa-permission-mode 'ask)
              (niwa-view-composer-insert "> first\nsecond"
                                             (niwa-view-composer-draft-start))
              (should (eq 'edits
                          (niwa-view-cycle-permission-mode)))
              (should (eq 'edits
                          (niwa-session-permission-mode session)))
              (should (eq 'edits
                          (buffer-local-value
                           'niwa-permission-mode data-buf)))
              (should (eq 'edits niwa-permission-mode))
              (should (eq saved
                          (default-toplevel-value 'niwa-permission-mode)))
              (should (string-prefix-p "[edits]" (niwa-view-test--rail-text)))
              (should (equal "> first\nsecond"
                             (niwa-view-composer-draft-text nil t))))
            (with-current-buffer view-buf
              (should (eq 'full-auto
                          (niwa-view-cycle-permission-mode)))
              (should (memq 'mode-constraints
                            (mapcar #'niwa-reminder-type
                                    (niwa-session-reminders session))))
              (should (eq 'ask
                          (niwa-view-cycle-permission-mode)))
              (let ((types (mapcar #'niwa-reminder-type
                                   (niwa-session-reminders session))))
                (should (equal '(mode-constraints) types)))
              (should (eq 'ask
                          (niwa-session-permission-mode session)))
              (should (equal "> first\nsecond"
                             (niwa-view-composer-draft-text nil t))))))
      (set-default-toplevel-value 'niwa-permission-mode saved)))

  :doc "cycles only baseline during implementation and preserves multiline draft"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create
                     :authority-mode 'portable :permission-mode 'ask))
           (request (niwa-request--create
                     :session session :permission-mode-override 'full-auto)))
      (with-current-buffer data-buf
        (setq-local niwa--session session niwa--view-buffer view-buf
                    niwa--current-request request))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "> first editable\nsecond line"
                                       (niwa-view-composer-draft-start))
        (should (eq 'edits (niwa-view-cycle-permission-mode)))
        (should (eq 'edits (niwa-session-permission-mode session)))
        (should (eq 'full-auto (niwa-view--effective-permission-mode)))
        (niwa-view-refresh-associated-input-prompt)
        (should (equal "> first editable\nsecond line" (niwa-view-composer-draft-text nil t))))))

  :doc "keeps the committed mode when an incomplete view cannot repaint"
  (let ((data-buffer (generate-new-buffer " *mev-cycle-data*"))
        (view-buffer (generate-new-buffer " *mev-cycle-view*")))
    (unwind-protect
        (let ((session (niwa-session--create
                        :name "main" :permission-mode 'ask)))
          (with-current-buffer data-buffer
            (setq-local niwa--session session
                        niwa--view-buffer view-buffer
                        niwa-permission-mode 'ask))
          (with-current-buffer view-buffer
            (setq-local niwa--data-buffer data-buffer)
            (insert "> quoted\nsecond line")
            (cl-letf (((symbol-function 'niwa-view-refresh-input-prompt)
                       (lambda () (error "Injected repaint failure"))))
              (should (eq 'edits (niwa-view-cycle-permission-mode))))
            (should (equal "> quoted\nsecond line" (buffer-string))))
          (should (eq 'edits (niwa-session-permission-mode session))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(niwa-deftest niwa-view-toggle-plan-mode
  (:quiet t :doc "toggles Plan independently while retaining permission mode and draft")
  (niwa-view-composer-test--without-admission ,test)
  (test)
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create
                    :authority-mode 'portable
                    :name "main"
                    :permission-mode 'full-auto)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "> first\nsecond"
                                       (niwa-view-composer-draft-start))
        (should (niwa-view-toggle-plan-mode))
        (should (niwa-session-plan-mode session))
        (should (eq 'full-auto
                    (niwa-session-permission-mode session)))
        (should (string-prefix-p "[Plan · full-auto]" (niwa-view-test--rail-text)))
        (should (equal "> first\nsecond"
                       (niwa-view-composer-draft-text nil t)))
        (should-not (niwa-view-toggle-plan-mode))
        (should-not (niwa-session-plan-mode session))
        (should (eq 'full-auto
                    (niwa-session-permission-mode session)))
        (should (equal "> first\nsecond"
                       (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view-composer-keymap
  (:doc "Composer commands are bound only inside editable input")
  ,test
  (test)
  (let ((bindings
         '(("C-<tab>" . niwa-view-toggle-plan-mode)
           ("<backtab>" . niwa-view-cycle-permission-mode)
           ("S-TAB" . niwa-view-cycle-permission-mode)
           ("C-c RET" . niwa-view-send)
           ("C-c TAB" . niwa-view-send-follow-up)
           ("C-c C-k" . niwa-view-cancel-composer-state)
           ("C-c C-l" . niwa-view-history-browse)
           ("C-c C-u" . niwa-view-history-clear-input)
           ("C-y" . niwa-view-yank-dwim)
           ("M-p" . niwa-view-history-previous)
           ("M-n" . niwa-view-history-next)
           ("M-r" . niwa-view-history-search)
           ("C-a" . niwa-view-history-beginning-of-line)
           ("C-z" . niwa-view-history-beginning-of-line)))
        (was-enabled tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (niwa-view-test--with-buffers
            (niwa-view-composer-test--in-composer view-buf
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "C-z") #'move-beginning-of-line)
              (dolist (binding bindings)
                (should (eq (key-binding (kbd (car binding))) (cdr binding))))
              (insert "draft")
              (niwa-view-refresh-input-prompt)
              (should (eq (key-binding (kbd "C-<tab>"))
                          #'niwa-view-toggle-plan-mode)))
            (with-current-buffer view-buf
              (dolist (binding bindings)
                (should-not (eq (key-binding (kbd (car binding))) (cdr binding)))))
            (niwa-view--setup view-buf data-buf)
            (niwa-view-composer-test--in-composer view-buf
              (should (eq (key-binding (kbd "C-<tab>"))
                          #'niwa-view-toggle-plan-mode)))
            (with-current-buffer view-buf
              (should (eq (key-binding (kbd "C-<tab>")) #'tab-next)))))
      (tab-bar-mode (if was-enabled 1 -1)))))

(niwa-deftest niwa-view-composer-session-fork-armed-p ()
  ,test
  (test)
  :doc "reports only the current view's armed fork"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (should-not (niwa-view-composer-session-fork-armed-p))
      (setq-local niwa-view--armed-session-fork
                  '(:fork-point-id "stable"))
      (should (niwa-view-composer-session-fork-armed-p)))))

(niwa-deftest niwa-view-composer-undo-across-renders
  (:doc "Transcript redraws do not alter composer undo history")
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      (niwa-view-composer-insert "> quoted\nsecond")
      (undo-boundary)
      (delete-region (niwa-view-composer-draft-start) (point-max)))
    (with-current-buffer view-buf
      (niwa-view-composer-test--status-rows 3)
      (let ((transcript (buffer-string)))
        (should (eq buffer-undo-list t))
        (niwa-view-composer-test--in-composer view-buf
          (undo-start)
          (undo-more 1)
          (should (equal "> quoted\nsecond" (niwa-view-composer-draft-text nil t)))
          (undo-more 1)
          (should (equal "" (niwa-view-composer-draft-text nil t))))
        (should (equal-including-properties transcript (buffer-string)))))))

(niwa-deftest niwa-view-composer-set-historical-visible ()
  ,test
  (test)
  :doc "hides and restores the exact multiline composer draft"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert (propertize "hidden mailbox body\n"
                            'invisible 'niwa-view-mailbox-collapsed)))
      (should (eq buffer-invisibility-spec t))
      (should (invisible-p (point-min)))
      (niwa-view-composer-insert "> exact draft\nsecond line"
                                     (niwa-view-composer-draft-start))
      (niwa-view-composer-set-historical-visible nil)
      (should buffer-read-only)
      (should (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer)))
      (should (eq buffer-invisibility-spec t))
      (should (invisible-p (point-min)))
      (niwa-view-composer-set-historical-visible t)
      (should-not buffer-read-only)
      (should-not (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer)))
      (should (eq buffer-invisibility-spec t))
      (should (invisible-p (point-min)))
      (should (equal "> exact draft\nsecond line"
                     (niwa-view-composer-draft-text nil t))))))

(niwa-deftest niwa-view-arm-conversation-fork ()
  ,test
  (test)
  :doc "arms the exact settled turn, renders cancellation, and preserves draft"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :authority-mode 'portable
            :name "source"
            :session-id "source-id"
            :current-segment 1))
          (target
           '(:fork-point-id "stable-2" :segment 1 :turn 2 :cum-turn 2))
          checked)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "> existing\nmultiline draft"
                                       (niwa-view-composer-draft-start))
        (goto-char (point-min))
        (setq-local niwa-view-segments--number 1
                    niwa-view-segments--buffer data-buf)
        (niwa-view-composer-set-historical-visible nil)
        (cl-letf
            (((symbol-function 'niwa-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'niwa-session-rewind-assert-stable-source)
              (lambda (candidate buffer operation)
                (setq checked (list candidate buffer operation)))))
          (niwa-view-arm-conversation-fork))
        (should (equal (list session data-buf "forking")
                       checked))
        (should (equal "stable-2"
                       (plist-get niwa-view--armed-session-fork
                                  :fork-point-id)))
        (should (= (point) (point-max)))
        (should-not buffer-read-only)
        (should-not (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer)))
        (should (equal "> existing\nmultiline draft"
                       (niwa-view-composer-draft-text nil t)))
        (let ((descriptor
               (gethash 'armed-session-fork
                        niwa-view--interaction-descriptors)))
          (should (string-match-p
                   "Fork conversation from Assistant turn 2"
                   (plist-get descriptor :body)))
          (should (string-match-p "\\[Cancel\\]"
                                  (plist-get descriptor :body)))
          (should (plist-member descriptor :active-work-paused))
          (should-not (plist-get descriptor :active-work-paused)))
        (niwa-view-cancel-composer-state)
        (should-not niwa-view--armed-session-fork)
        (should (niwa-view-historical-segment-p))
        (should buffer-read-only)
        (should (buffer-local-value 'buffer-read-only (niwa-view-composer-buffer)))
        (should (= (point) (point-min)))
        (should-not (gethash 'armed-session-fork
                             niwa-view--interaction-descriptors))
        (should (equal "> existing\nmultiline draft"
                       (niwa-view-composer-draft-text nil t))))))
  :doc "reports an exact selection error without arming"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view-fork-point-at-point)
                 (lambda ()
                   (user-error "Point is not on an assistant response"))))
        (should-error (niwa-view-arm-conversation-fork)
                      :type 'user-error))
      (should-not niwa-view--armed-session-fork)))
  :doc "rejects an inherited point on Child but permits a later response"
  (niwa-view-test--with-buffers
    (let* ((session
            (niwa-session--create
             :authority-mode 'portable
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
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "> child draft\nsecond line"
                                       (niwa-view-composer-draft-start))
        (cl-letf
            (((symbol-function 'niwa-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'niwa-session-rewind-assert-stable-source)
              #'ignore))
          (let ((error
                 (should-error
                  (niwa-view-arm-conversation-fork)
                  :type 'user-error)))
            (should (string-match-p "Source"
                                    (error-message-string error))))
          (should-not niwa-view--armed-session-fork)
          (plist-put target :fork-point-id "later-point")
          (plist-put target :turn 2)
          (plist-put target :cum-turn 2)
          (niwa-view-arm-conversation-fork)
          (should (equal "later-point"
                         (plist-get niwa-view--armed-session-fork
                                    :fork-point-id)))
          (should (equal "> child draft\nsecond line"
                         (niwa-view-composer-draft-text nil t))))))))

(niwa-deftest niwa-view--dim-unforked-turns ()
  ,test
  (test)
  :doc "dims turns after the armed response across redraws until cancel"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :authority-mode 'portable
            :name "source"
            :session-id "source-id"
            :current-segment 1))
          cutoff)
      (niwa-view-test--insert-data data-buf "*** First prompt\n" nil)
      (niwa-view-test--insert-data data-buf "First answer.\n" 'response)
      (setq cutoff (with-current-buffer data-buf (point-max)))
      (niwa-view-test--insert-data data-buf "\n\n*** Second prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Second answer.\n" 'response)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--full-rerender)
        (cl-flet ((dimmed-p (text)
                    (goto-char (point-min))
                    (search-forward text)
                    (seq-some (lambda (overlay)
                                (equal '(:inherit shadow :extend t)
                                       (overlay-get overlay 'face)))
                              (overlays-at (match-beginning 0)))))
          (cl-letf (((symbol-function 'niwa-view-fork-point-at-point)
                     (lambda ()
                       (list :fork-point-id "stable-1" :cum-turn 1
                             :transcript-cutoff cutoff)))
                    ((symbol-function
                      'niwa-session-rewind-assert-stable-source)
                     #'ignore))
            (niwa-view-arm-conversation-fork))
          (should-not (dimmed-p "First answer."))
          (should (dimmed-p "Second prompt"))
          (should (dimmed-p "Second answer."))
          (should-not (dimmed-p "Fork conversation from Assistant turn 1"))
          (niwa-view--full-rerender)
          (should-not (dimmed-p "First answer."))
          (should (dimmed-p "Second answer."))
          (niwa-view-cancel-session-fork)
          (should-not (dimmed-p "Second answer."))
          (should-not niwa-view--armed-session-fork-overlay))))))

(niwa-deftest niwa-view-arm-worktree-fork ()
  ,test
  (test)
  :doc "preflights Git and arms the exact settled turn as a Worktree Fork"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :authority-mode 'portable
            :name "source"
            :session-id "source-id"
            :current-segment 1))
          (target
           '(:fork-point-id "stable-2" :segment 1 :turn 2 :cum-turn 2))
          preflight
          reserved)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "draft"
                                       (niwa-view-composer-draft-start))
        (cl-letf
            (((symbol-function 'niwa-view-fork-point-at-point)
              (lambda () target))
             ((symbol-function
               'niwa-session-rewind-assert-stable-source)
              #'ignore)
             ((symbol-function 'niwa-worktree-fork-preflight)
              (lambda (candidate)
                (setq preflight candidate)
                '(:base-commit "abc123")))
             ((symbol-function 'niwa-worktree-fork-reservation)
              (lambda (candidate context)
                (should (eq candidate session))
                (should (equal '(:base-commit "abc123") context))
                (setq reserved
                      '(:branch "worktree/source-fork-1"
                        :directory "/repo/.worktrees/source-fork-1/")))))
          (niwa-view-arm-worktree-fork))
        (should (eq session preflight))
        (should (eq 'worktree
                    (plist-get niwa-view--armed-session-fork
                               :fork-type)))
        (should (eq reserved
                    (plist-get niwa-view--armed-session-fork
                               :worktree-reservation)))
        (should (equal "draft" (niwa-view-composer-draft-text nil t)))
        (should
         (string-match-p
          "Fork worktree from Assistant turn 2"
          (plist-get
           (gethash 'armed-session-fork
                    niwa-view--interaction-descriptors)
           :body)))))))

(niwa-deftest niwa-view--retarget-worktree-mention-bindings ()
  ,test
  (test)
  :doc "retargets file and skill bindings without mutating Source text"
  (let* ((source
          (file-name-as-directory
           (make-temp-file "niwa-mentions-source-" t)))
         (worktree
          (file-name-as-directory
           (make-temp-file "niwa-mentions-worktree-" t)))
         (session
          (niwa-session--create
           :authority-mode 'portable
           :worktree-source-root source
           :worktree-directory worktree))
         (text (copy-sequence "Use @file:local and $skill"))
         result)
    (unwind-protect
        (progn
          (string-match "@file:local" text)
          (niwa-mention-bindings-set
           (match-beginning 0) (match-end 0)
           (list :kind 'file :path (file-name-concat source "local"))
           text)
          (string-match "\\$skill" text)
          (niwa-mention-bindings-set
           (match-beginning 0) (match-end 0)
           (list :kind 'skill
                 :source-file (file-name-concat source "SKILL.md"))
           text)
          (setq result
                (niwa-view--retarget-worktree-mention-bindings
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
             (niwa-mention-bindings-ranges result))))
          (should
           (equal
            (file-name-concat source "local")
            (plist-get
             (plist-get
              (car (niwa-mention-bindings-ranges text))
              :binding)
             :path))))
      (delete-directory source t)
      (delete-directory worktree t))))

(niwa-deftest niwa-view-send/historical-segment ()
  ,test
  (test)
  :doc "refuses live sends and local commands unless a model fork is armed"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa-view-segments--number 1
                  niwa-view-segments--buffer data-buf)
      (niwa-view-composer-insert "live-tip draft"
                                     (niwa-view-composer-draft-start))
      (let ((error (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)))
        (should (string-match-p
                 "return to latest or fork"
                 (error-message-string error))))
      (niwa-view--clear-input)
      (niwa-view-composer-insert "/clear"
                                     (niwa-view-composer-draft-start))
      (setq-local
       niwa-view--armed-session-fork
       '(:fork-point-id "historical" :fork-type conversation))
      (let ((error (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)))
        (should (string-match-p
                 "Slash commands are unavailable"
                 (error-message-string error)))))))

(niwa-deftest niwa-view-send/conversation-fork ()
  (niwa-view-composer-test--without-admission ,test)
  (test)
  :doc "publishes before Child-owned prompt preparation and preserves ownership"
  (niwa-view-test--with-source-skills nil
    (let* ((child-session (niwa-session-create "child" ws))
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
              (setq-local niwa--session child-session
                          niwa--workspace ws))
            (niwa-view--setup child-view child-data)
            (with-current-buffer view-buf
              (setq-local niwa-view--armed-session-fork target)
              (setf (niwa-session-dropped-file-grants session)
                    (list attachment "/tmp/unrelated.txt"))
              (niwa-view-composer-insert draft
                                             (niwa-view-composer-draft-start))
              (niwa-view-composer-test--in-composer view-buf
                (goto-char (+ (niwa-view-composer-draft-start view-buf) 3)))
              (cl-letf
                  (((symbol-function
                     'niwa-session-fork-create)
                    (lambda (buffer fork-target type)
                      (should (eq type 'conversation))
                      (push (list 'materialized buffer fork-target) events)
                      child-data))
                   ((symbol-function 'niwa-view--submit-planned-input)
                    (lambda (input &rest _)
                      (push (list 'prepared (current-buffer) input) events)))
                   ((symbol-function 'niwa-view-rerender)
                    (lambda (&optional view)
                      (push (list 'scheduled view) events))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send)))
              ;; Source's display rebuild is scheduled, not blocking.
              (should (member (list 'scheduled view-buf) events))
              (should-not niwa-view--armed-session-fork)
              (should (string-empty-p (niwa-view-composer-draft-text nil t))))
            (should (eq 'materialized (caar (last events))))
            (should (eq child-view (cadr (car events))))
            (with-current-buffer child-view
              (should (equal draft (niwa-view-composer-draft-text nil t)))
              (should (= 3 (niwa-view-test--composer-point-offset
                            child-view))))
            (should (equal (list attachment)
                           (niwa-session-dropped-file-grants
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
  (niwa-view-test--with-source-skills nil
    (let ((target
           '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
             :fork-type conversation))
          (attachment "/tmp/fork-failure.txt"))
      (with-current-buffer view-buf
        (setq-local niwa-view--armed-session-fork target)
        (setf (niwa-session-dropped-file-grants session)
              (list attachment))
        (niwa-view-composer-insert (format "> exact\n@file:%s" attachment)
                                       (niwa-view-composer-draft-start))
        (let ((before (niwa-view--composer-snapshot session)))
          (cl-letf
              (((symbol-function
                 'niwa-session-fork-create)
                (lambda (&rest _) (error "materialization failed"))))
            (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send))
                          :type 'error))
          (should (equal target niwa-view--armed-session-fork))
          (should (equal (plist-get before :text)
                         (plist-get
                          (niwa-view--composer-snapshot session)
                          :text)))
          (should (equal (list attachment)
                         (niwa-session-dropped-file-grants session)))))))

  :doc "Worktree failure keeps its reservation and exact Source composer"
  (niwa-view-test--with-source-skills nil
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
        (setq-local niwa-view--armed-session-fork target)
        (setf (niwa-session-dropped-file-grants session)
              (list attachment))
        (niwa-view-composer-insert (format "> exact\n@file:%s" attachment)
                                       (niwa-view-composer-draft-start))
        (let ((before (niwa-view--composer-snapshot session)))
          (cl-letf
              (((symbol-function
                 'niwa-session-fork-create)
                (lambda (&rest _) (error "staging failed"))))
            (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send))))
          (should (eq reservation
                      (plist-get niwa-view--armed-session-fork
                                 :worktree-reservation)))
          (should (equal before
                         (niwa-view--composer-snapshot session)))))))

  :doc "failed prompt preflight does not publish a Child"
  (niwa-view-test--with-source-skills nil
    (let ((target
           '(:fork-point-id "stable-1" :segment 1 :turn 1 :cum-turn 1
             :fork-type conversation))
          materialized)
      (with-current-buffer view-buf
        (setq-local niwa-view--armed-session-fork target)
        (niwa-view-composer-insert "invalid skill input"
                                       (niwa-view-composer-draft-start))
        (cl-letf
            (((symbol-function 'niwa-skills-plan-user-input)
              (lambda (&rest _) (user-error "Invalid skill syntax")))
             ((symbol-function
               'niwa-session-fork-create)
              (lambda (&rest _)
                (setq materialized t))))
          (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send))
                        :type 'user-error))
        (should-not materialized)
        (should (equal target niwa-view--armed-session-fork))
        (should (equal "invalid skill input"
                       (niwa-view-composer-draft-text nil t))))))

  :doc "dispatches an armed Worktree Fork through its materializer"
  (niwa-view-test--with-source-skills nil
    (let* ((child-session (niwa-session-create "child" ws))
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
              (setq-local niwa--session child-session
                          niwa--workspace ws))
            (niwa-view--setup child-view child-data)
            (with-current-buffer view-buf
              (setq-local niwa-view--armed-session-fork target)
              (niwa-view-composer-insert "continue here"
                                             (niwa-view-composer-draft-start))
              (cl-letf
                  (((symbol-function
                     'niwa-session-fork-create)
                    (lambda (buffer fork-target type)
                      (setq called (list buffer fork-target type))
                      child-data))
                   ((symbol-function 'niwa-view--submit-planned-input)
                    #'ignore))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (should (equal (list data-buf target 'worktree) called)))
        (dolist (buffer (list child-view child-data))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(niwa-deftest niwa-view--propertize-input-prompt
  (:doc "Line motion that bypasses the composer remap still stops at the input")
  ,test
  (test)
  :doc "every beginning-of-line style command stops after the prompt"
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      (niwa-view-composer-insert "abc")
      (dolist (command '(beginning-of-line back-to-indentation
                         move-beginning-of-line))
        (niwa-view-composer-test--in-composer view-buf
          (goto-char (niwa-view-composer-draft-end view-buf))
          (call-interactively command)
          (should (= (point) (niwa-view-composer-draft-start view-buf)))))
      (should (equal "abc" (niwa-view-composer-draft-text nil t)))))

  :doc "continuation lines keep their own line start"
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      (niwa-view-composer-test--in-composer view-buf
        (niwa-view-composer-insert "abc\ndef")
        (beginning-of-line)
        (should (= (point) (- (niwa-view-composer-draft-end view-buf) 3))))))

  :doc "a refreshed prompt keeps the field boundary"
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      (niwa-view-composer-insert "abc")
      (niwa-view-refresh-input-prompt)
      (niwa-view-composer-test--in-composer view-buf
        (goto-char (niwa-view-composer-draft-end view-buf))
        (beginning-of-line)
        (should (= (point) (niwa-view-composer-draft-start view-buf))))
      (should (eq 'niwa-view-prompt
                  (get-text-property (1- (niwa-view-composer-draft-start))
                                     'field))))))

(niwa-deftest niwa-view--update-composer-indent
  (:doc "aligns composer continuation lines with the end of the prompt")
  ,test
  (test)

  :doc "graphical alignment and right-edge EOF transitions use compiled composer code"
  (let ((process-environment (copy-sequence process-environment))
        directory)
    (skip-unless (niwa-test--wayland-available-p))
    (setq directory (make-temp-file "niwa-composer-compiled-" t))
    (unwind-protect
        (progn
          ;; The graphical runner forwards load-path; it does not compile it.
          ;; Never leave bytecode beside sources used by concurrent tests.
          (let ((byte-compile-dest-file-function
                 (lambda (file)
                   (file-name-concat
                    directory (concat (file-name-base file) ".elc")))))
            (dolist (file '("niwa-view.el" "niwa-view-composer.el"))
              (should (byte-compile-file
                       (file-name-concat niwa-test--root file)))))
          (setenv "NIWA_TEST_LOAD_PATH"
                  (prin1-to-string (cons directory load-path)))
          (pcase-let ((`(,observed ,status . ,_trace)
                       (niwa-test--run-wayland-fixture
                        "view-composer-wayland.el" 20)))
            (ert-info ((format "Observed: %S" observed))
              (should (= status 0))
              (should (equal '(t t t) (plist-get observed :compiled)))
              (should (= 4 (length (plist-get observed :alignment))))
              (dolist (row (plist-get observed :alignment))
                (let ((x (car (nth 1 row))))
                  (dolist (position (cl-subseq row 2 5))
                    (should (= x (car position))))
                  (should (= x (aref (nth 5 row) 1)))))
              (should (= 20 (length (plist-get observed :edges))))
              (dolist (row (plist-get observed :edges))
                (pcase-let ((`(,mode ,_delta ,reference ,populated ,_always
                                    ,newline ,typed ,untyped ,restored) row))
                  (let ((x (car (nth 1 (assq mode (plist-get observed :alignment))))))
                    (should (equal reference populated))
                    (should (equal reference restored))
                    (should (= x (car newline)))
                    (should (> (cdr newline) (cdr reference)))
                    (should (= (car typed) (+ x (plist-get observed :character-width))))
                    (should (= (cdr typed) (cdr newline)))
                    (should (equal newline untyped)))))
              ;; Confirm the rejected suffix actually wraps at this edge,
              ;; rather than merely moving the cursor right on the same row.
              (should (cl-some (lambda (row) (> (cdr (nth 4 row)) (cdr (nth 2 row))))
                               (plist-get observed :edges))))))
      (delete-directory directory t)))

  :doc "continuation lines are indented by the prompt width, text untouched"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      ;; Type into the empty composer: inserted text joins the overlay.
      (niwa-view-composer-insert "> first\nsecond"
                                     (niwa-view-composer-draft-start))
      (niwa-view-composer-test--in-composer view-buf
        (let* ((first-line (save-excursion
                             (goto-char (niwa-view-composer-draft-start view-buf))
                             (pos-bol)))
               (second-line (save-excursion
                              (goto-char (niwa-view-composer-draft-start view-buf))
                              (forward-line 1)
                              (point))))
          (should-not (get-char-property first-line 'line-prefix))
          (should (equal "  " (get-char-property second-line 'line-prefix)))
          (should (equal "  " (get-char-property second-line 'wrap-prefix)))
          (should (equal "> first\nsecond"
                         (niwa-view-composer-draft-text view-buf t)))))))

  :doc "history redraw and optimistic turns never acquire the composer indent"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "First prompt\n\n" nil)
    (niwa-view-test--insert-data data-buf "First reply\n\n" 'response)
    (with-current-buffer view-buf
      (dolist (draft '("" "> first\nsecond\n"))
        (niwa-view--clear-input)
        (niwa-view-composer-insert draft
                                       (niwa-view-composer-draft-start))
        (dotimes (_ 3)
          (niwa-view--full-rerender)
          (niwa-view--insert-user-message "Next prompt")
          (niwa-view--render-status)
          (niwa-view--interaction-rebuild)
          (let ((start (niwa-view-composer-draft-start view-buf))
                (end (niwa-view-composer-draft-end view-buf)))
            (with-current-buffer (niwa-view-composer-buffer)
              (should (= start (overlay-start niwa-view--composer-display-overlay)))
              (should (= end (overlay-end niwa-view--composer-display-overlay))))
            (should (equal draft (niwa-view-composer-draft-text view-buf t)))
            (should-not (text-property-not-all
                         (point-min) (point-max)
                         'niwa-view-prompt nil))
            (let ((position (point-min)))
              (while (< position (point-max))
                (should-not (get-char-property position 'line-prefix))
                (setq position (next-char-property-change position)))))))))

  :doc "the indent stays two columns when the prompt is rebuilt for a mode"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-insert "draft\nmore"
                                     (niwa-view-composer-draft-start))
      (dolist (case '((full-auto . "> ") (ask . "> ")))
        (setq-local niwa-permission-mode (car case))
        (niwa-view-refresh-input-prompt)
        (niwa-view-composer-test--in-composer view-buf
          (let ((second-line (save-excursion
                               (goto-char (niwa-view-composer-draft-start view-buf))
                               (forward-line 1)
                               (point))))
            (should (equal (make-string (length (cdr case)) ?\s)
                           (get-char-property second-line 'line-prefix)))
            (should (equal "draft\nmore"
                           (niwa-view-composer-draft-text view-buf t))))))))

  :doc "mode redraws keep indentation, draft, point and selection"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa--session (niwa-session--create :name "main"))
      (niwa-view-composer-insert "> first\nsecond\n"
                                     (niwa-view-composer-draft-start))
      (niwa-view-composer-test--in-composer view-buf
        (goto-char (+ (niwa-view-composer-draft-start view-buf) 3))
        (set-mark (+ (niwa-view-composer-draft-start view-buf) 10))
        (setq mark-active t))
      (dolist (state '(ask edits full-auto plan editing ask))
        (setq-local niwa-permission-mode
                    (if (memq state '(ask edits full-auto)) state 'ask))
        (setf (niwa-session-plan-mode niwa--session) (eq state 'plan))
        (setq niwa-view--pending-input-edit
              (and (eq state 'editing) '(:label "queued input")))
        (niwa-view-refresh-input-prompt)
        (niwa-view--render-status)
        (niwa-view--interaction-rebuild)
        (niwa-view-composer-test--in-composer view-buf
          (let ((start (niwa-view-composer-draft-start view-buf))
                (indent "  "))
            (should (equal "> first\nsecond\n"
                           (niwa-view-composer-draft-text view-buf t)))
            (should (= (point) (+ start 3)))
            (should (= (mark) (+ start 10)))
            (should mark-active)
            (should (= (overlay-start niwa-view--composer-display-overlay) start))
            (should (= (overlay-end niwa-view--composer-display-overlay)
                       (niwa-view-composer-draft-end view-buf)))
            (should (equal indent (get-char-property (+ start 8) 'line-prefix)))
            (should (equal indent (get-char-property (+ start 8) 'wrap-prefix)))
            (should-not (get-char-property (1- start) 'line-prefix))))))))

(niwa-deftest niwa-view--update-composer-end ()
  ,test
  (test)
  :doc "only an empty final line gets display text, typing and deletion update it"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-test--in-composer view-buf
        (goto-char (niwa-view-composer-draft-start view-buf))
        (should-not (overlay-get niwa-view--composer-display-overlay 'after-string))
        (insert "> draft\n")
        (let ((display (overlay-get niwa-view--composer-display-overlay 'after-string)))
          (should (equal "   " display))
          (should (get-text-property 2 'cursor display)))
        (should (equal "> draft\n"
                       (niwa-view-composer-draft-text view-buf t)))
        (insert "x")
        (should-not (overlay-get niwa-view--composer-display-overlay 'after-string))
        (delete-char -1)
        (should (overlay-get niwa-view--composer-display-overlay 'after-string))
        (delete-char -1)
        (should-not (overlay-get niwa-view--composer-display-overlay 'after-string)))))

  :doc "silently restored drafts refresh the empty-line display too"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((session (niwa-session--create :name "main")))
        (dolist (text '("draft\n" "other" "\n" ""))
          (niwa-view--restore-composer-snapshot
           (list :text text :point-offset (length text)) session t)
          (with-current-buffer (niwa-view-composer-buffer)
            (should (eq (and (overlay-get niwa-view--composer-display-overlay
                                          'after-string) t)
                        (string-suffix-p "\n" text))))))))

  :doc "buffers without a composer have no display state to update"
  (with-temp-buffer
    (insert "line\n")
    (niwa-view--update-composer-end)
    (should-not (overlays-in (point-min) (point-max)))))

(niwa-deftest niwa-view-refresh-input-prompt
  (:doc "updates the prompt prefix without disturbing draft input")
  ,test
  (test)

  :doc "setup renders the ask mode prompt"
  (let ((niwa-permission-mode 'ask))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (should (string-prefix-p "[ask]" (niwa-view-test--rail-text))))))

  :doc "refresh preserves input text and updates the mode"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-insert "draft"
                                     (niwa-view-composer-draft-start))
      (setq-local niwa-permission-mode 'full-auto)
      (niwa-view-refresh-input-prompt)
      (should (string-prefix-p "[full-auto]" (niwa-view-test--rail-text)))
      (should (string= "draft" (niwa-view-composer-draft-text nil t)))))

  :doc "refresh preserves a multiline draft starting with a literal >"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-insert "> quoted\nsecond line"
                                     (niwa-view-composer-draft-start))
      (setq-local niwa-permission-mode 'full-auto)
      (niwa-view-refresh-input-prompt)
      (should (string-prefix-p "[full-auto]" (niwa-view-test--rail-text)))
      (should (string= "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should-not (get-text-property (niwa-view-composer-draft-start)
                                     'niwa-view-prompt
                                     (niwa-view-composer-buffer))))))

(niwa-deftest niwa-view-composer-visited-row-through-render ()
  (let ((outer ,outer))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (dotimes (n 100) (insert (format "history %d\n" n))))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (let* ((card '(:namespace interaction :id visited :priority 2
                      :body "question\noption one\noption two\nanswer"))
               (lower '(:namespace interaction :id lower :priority 1
                       :body "new lower\ncard row\nlast row"))
               (render (lambda (fragments)
                         (niwa-view-zone-reconcile
                          'interaction niwa-view--interaction-marker
                          (niwa-view-transcript-tail-marker) fragments))))
          (funcall render (list card))
          (goto-char (plist-get (niwa-view-zone-fragment-bounds
                                'interaction 'visited) :start))
          (forward-line 2)
          (set-window-start nil (save-excursion
                                  (goto-char (point-max))
                                  (vertical-motion (- (1- (window-body-height))))
                                  (point)) t)
          (let ((before (niwa-view-test--position-screen-y
                         (point) (selected-window)))
                inside)
            (should before)
            (funcall (if outer #'niwa-view--call-preserving-window-state #'funcall)
                     (lambda ()
                       (funcall render (list card lower))
                       (setq inside (niwa-view-test--position-screen-y
                                     (point) (selected-window)))))
            ;; The outer renderer must not undo a nested zone's visited anchor.
            (should (= before inside))
            (should (looking-at-p "option two"))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
            (should (= before (niwa-view-test--position-screen-y
                               (point) (selected-window)))))))))
  (outer)
  :doc "Direct reconciliation retains the visited row when a lower card arrives" nil
  :doc "The outer render wrapper must not undo the visited row" t)

(niwa-deftest niwa-view--call-preserving-window-state
  (:doc "follows new output only from the bottom")
  ,test
  (test)

  :doc "a region's markers are detached even when restoration fails"
  (save-window-excursion
    (with-temp-buffer
      (dotimes (index 40)
        (insert (format "line %d\n" index)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (forward-line 5)
      (set-mark (point-max))
      (let ((real-copy-marker (symbol-function 'copy-marker))
            markers)
        (cl-letf (((symbol-function 'copy-marker)
                   (lambda (&rest args)
                     (car (push (apply real-copy-marker args) markers))))
                  ((symbol-function 'niwa-view--clamp-window-starts)
                   (lambda (&rest _) (error "Restoration failed"))))
          (should-error
           (niwa-view--call-preserving-window-state
            (lambda () (goto-char 20) (insert "inserted\n"))
            nil (cons 20 30))))
        ;; Region ends plus point, window point and window start markers.
        ;; Unrelated code may copy markers into other buffers meanwhile.
        (let ((buffer (current-buffer)))
          (setq markers (seq-filter (lambda (marker)
                                      (or (null (marker-buffer marker))
                                          (eq buffer (marker-buffer marker))))
                                    markers))
          (should (>= (length markers) 4))
          (should-not (seq-some #'marker-buffer markers))))))

  :doc "a region keeps point after it on the same text"
  (save-window-excursion
    (with-temp-buffer
      (dotimes (index 40)
        (insert (format "line %d\n" index)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward "line 30")
      (let ((start (save-excursion (goto-char (point-min)) (forward-line 2) (point))))
        (niwa-view--call-preserving-window-state
         (lambda ()
           (delete-region start (+ start 7))
           (goto-char start)
           (insert "a much longer replacement line\n"))
         nil (cons start (+ start 7))))
      (should (looking-back "line 30" (line-beginning-position)))))

  :doc "bottom-following windows stay at the bottom"
  (save-window-excursion
    (with-temp-buffer
      (dotimes (index 400)
        (insert (format "line %d\n" index)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (recenter -1)
      (redisplay t)
      (niwa-view-call-preserving-user-view-state
       (lambda ()
         ;; Transcript output in a bare buffer, not a composer draft.
         (goto-char (point-max))
         (insert "new output\n"))
       nil t)
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
        (niwa-view--call-preserving-window-state
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
        (niwa-view--call-preserving-window-state
         (lambda ()
           (delete-region (point-min) (point-max))
           (insert text)))
        (should mark-active)
        (should (= (point) point-before))
        (should (= (mark) mark-before))
        (should (equal "selected"
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))))))

  :doc "composer selections stay active when history grows"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-test--in-composer view-buf
        (setq-local transient-mark-mode t)
      (niwa-view-composer-insert "draft selection"
                                     (niwa-view-composer-draft-start))
      (search-backward "selection")
      (set-mark (match-end 0))
        (activate-mark))
      (niwa-view-call-preserving-user-view-state
       (lambda ()
         (let ((inhibit-read-only t))
           (goto-char (point-min))
           (insert "new history\n"))))
      (niwa-view-composer-test--in-composer view-buf
        (should mark-active)
      (should (equal "selection"
                     (buffer-substring-no-properties
                      (region-beginning) (region-end)))))))

  :doc "restores point into re-rendered content via its source anchor"
  (with-temp-buffer
    (insert "head ")
    (insert (propertize "rendered run" 'niwa-view-source '(40 . 90)))
    ;; Point 4 chars into the run, on the second "e" of "rendered".
    (goto-char (+ (point-min) (length "head ") 4))
    (niwa-view--call-preserving-window-state
     (lambda ()
       (delete-region (point-min) (point-max))
       (insert "a much longer head this time ")
       (insert (propertize "rendered run" 'niwa-view-source '(40 . 90)))))
    (should (= (point)
               (+ (point-min) (length "a much longer head this time ") 4)))
    (should (eq (char-after) ?e)))

  :doc "transcript and composer windows and selection retain their coordinates after unwind"
  (dolist (fail '(nil t))
    (save-window-excursion
      (niwa-view-test--with-buffers
        (switch-to-buffer view-buf)
        (let ((other (split-window-below))
              (draft "> quoted\nsecond line"))
          (set-window-buffer other (niwa-view-composer-buffer view-buf))
          (with-current-buffer view-buf
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (niwa-view--with-render-boundaries-advancing
                (insert (propertize "rendered history\n"
                                    'niwa-view-source '(40 . 90)))))
            (niwa-view-composer-insert draft
                                           (niwa-view-composer-draft-start))
            (set-window-point other (+ (niwa-view-composer-draft-start) 4))
            (niwa-view-composer-test--in-composer view-buf
              (setq-local transient-mark-mode t)
              (set-mark (+ (niwa-view-composer-draft-start) 8))
              (activate-mark))
            (goto-char (+ (point-min) 3))
            (let ((redraw
                   (lambda ()
                     (niwa-view--call-preserving-window-state
                      (lambda ()
                        (let ((inhibit-read-only t))
                          (goto-char (point-min))
                          (insert "new history\n"))
                        (if fail (error "Injected redraw failure") 'result))))))
              (if fail
                  (should-error (funcall redraw) :type 'error)
                (should (eq 'result (funcall redraw)))))
            (should (= (point) (+ (point-min) (length "new history\n") 3)))
            (should (= (window-point other) (+ (niwa-view-composer-draft-start) 4)))
            (niwa-view-composer-test--in-composer view-buf
              (should (= (mark) (+ (niwa-view-composer-draft-start) 8)))
              (should mark-active))
            (should (equal draft (niwa-view-composer-draft-text nil t))))))))

  :doc "a vanished transcript position leaves the separate composer untouched"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Prompt\n" nil)
    (niwa-view-test--insert-data
     data-buf "A long response that is replaced by something much shorter.\n"
     'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4)
      (save-window-excursion
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "something much shorter")
        (set-window-point (selected-window) (point))
        (let ((end (marker-position niwa-view--status-marker)))
          (niwa-view--call-preserving-window-state
           (lambda ()
             (let ((inhibit-read-only t))
               (goto-char (point-min))
               (search-forward "A long response")
               (delete-region (match-beginning 0) end)
               (goto-char (match-beginning 0))
               (insert "Short.\n")))))
        (should (eq (window-buffer) view-buf))
        (should (= (niwa-view-test--composer-point-offset view-buf) 4))
        (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view--render-anchor-identity ()
  ,test
  (test)
  :doc "source markers and standalone roles survive grouping and stale key starts"
  (with-temp-buffer
    (insert " header child")
    (let ((source (cons (copy-marker 2) (copy-marker (point-max)))))
      (add-text-properties
       2 (point-max)
       `(niwa-view-source ,source
         niwa-view-type tool-child
         niwa-view-source-key (source tool-summary 2 (in-flight))))
      (should (equal '(2 tool-summary nil)
                     (niwa-view--render-anchor-identity 3)))
      (goto-char (point-min))
      (insert "prefix")
      (should (equal '(8 tool-summary nil)
                     (niwa-view--render-anchor-identity 9)))
      (put-text-property 8 (point-max) 'niwa-view-type 'tool-summary)
      (put-text-property 8 (point-max) 'niwa-view-source-key
                         '(source tool-summary 8 (tool "call-1")))
      (should (equal '(8 tool-summary nil)
                     (niwa-view--render-anchor-identity 9)))))

  :doc "compound children retain their discriminator independently of content"
  (with-temp-buffer
    (insert (propertize "first" 'niwa-view-source '(40 . 90)
                        'niwa-view-type 'tool-child
                        'niwa-view-source-key
                        '(source tool-child 40 "old-hash" "child-1")))
    (should (equal '(40 tool-child ("child-1"))
                   (niwa-view--render-anchor-identity 1)))
    (put-text-property 1 (point-max) 'niwa-view-source-key
                       '(source tool-child 40 "new-hash" "child-2"))
    (should (equal '(40 tool-child ("child-2"))
                   (niwa-view--render-anchor-identity 1))))

  :doc "ordinary source text uses its rendered type and unannotated text has no identity"
  (with-temp-buffer
    (insert (propertize "response" 'niwa-view-source '(40 . 90)
                        'niwa-view-type 'response))
    (insert "plain")
    (should (equal '(40 response nil) (niwa-view--render-anchor-identity 1)))
    (should-not (niwa-view--render-anchor-identity 9))))

(niwa-deftest niwa-view--render-anchor-run-end ()
  ,test
  (test)
  :doc "source, role and child identity boundaries all limit a reader run"
  (dolist (property '(niwa-view-source niwa-view-type niwa-view-source-key))
    (with-temp-buffer
      (insert "aaaabbbb")
      (put-text-property 1 5 property 'first)
      (put-text-property 5 9 property 'second)
      (should (= 5 (niwa-view--render-anchor-run-end 1 9)))
      (should (= 3 (niwa-view--render-anchor-run-end 1 3)))
      (should (= 9 (niwa-view--render-anchor-run-end 5 9)))))

  :doc "unrelated formatting does not split source runs or their ordinals"
  (with-temp-buffer
    (let ((source (cons 40 90)))
      (insert (propertize "aaaa" 'niwa-view-source source
                          'niwa-view-type 'response))
      (insert " ")
      (insert (propertize "bbbb" 'niwa-view-source source
                          'niwa-view-type 'response))
      (put-text-property 2 3 'face 'bold)
      (put-text-property 3 4 'keymap (make-sparse-keymap))
      (put-text-property 7 8 'face 'italic)
      (should (= 5 (niwa-view--render-anchor-run-end 1 (point-max))))
      (let ((anchor (niwa-view--position-render-anchor 8)))
        (should (equal '(source (40 response nil) 1 2) anchor))
        (should (= 8 (niwa-view--render-anchor-position anchor))))))

  :doc "equal but distinct source and child keys still split a run"
  (dolist (property '(niwa-view-source niwa-view-source-key))
    (with-temp-buffer
      (insert "aaaabbbb")
      (put-text-property 1 5 property (list 'same))
      (put-text-property 5 9 property (list 'same))
      (should-not (eq (get-text-property 1 property)
                      (get-text-property 5 property)))
      (should (= 5 (niwa-view--render-anchor-run-end 1 9)))))

  :doc "constant or absent tracked values respect narrowing and marker limits"
  (with-temp-buffer
    (insert "abcdefghij")
    (put-text-property 4 6 'face 'bold)
    (narrow-to-region 3 9)
    (let ((limit (copy-marker 7)))
      (unwind-protect
          (progn
            (should (= 7 (niwa-view--render-anchor-run-end 3 limit)))
            (should (= 9 (niwa-view--render-anchor-run-end 3 (point-max)))))
        (set-marker limit nil))))

  :doc "probe boundaries and distant runs resolve without truncation"
  (dolist (property '(niwa-view-source niwa-view-type niwa-view-source-key))
    (dolist (size '(63 64 65 127 128 129 5000))
      (with-temp-buffer
        (insert (make-string size ?a) "b")
        (put-text-property 1 (1+ size) property 'first)
        (put-text-property (1+ size) (point-max) property 'second)
        (should (= (1+ size) (niwa-view--render-anchor-run-end 1 (point-max)))))))

  :doc "the closest child boundary wins over later source and role boundaries"
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (put-text-property 1 151 'niwa-view-source 'first)
    (put-text-property 1 81 'niwa-view-type 'response)
    (put-text-property 1 65 'niwa-view-source-key 'child)
    (should (= 65 (niwa-view--render-anchor-run-end 1 (point-max)))))

  :doc "growing probes do not revisit a dense formatting prefix"
  (with-temp-buffer
    (dotimes (i 1000)
      (insert (propertize "x" 'face (if (cl-evenp i) 'bold 'italic))))
    (let ((single (symbol-function 'next-single-property-change))
          (searched 0) boundary)
      (cl-letf (((symbol-function 'next-single-property-change)
                 (lambda (pos property &optional object limit)
                   (let ((end (funcall single pos property object limit)))
                     (cl-incf searched (- end pos))
                     end))))
        (setq boundary (niwa-view--render-anchor-run-end 1 (point-max))))
      (should (= (point-max) boundary))
      ;; Three searches per chunk, with only a one-character overlap.
      (should (<= searched (* 4 (buffer-size))))))

  :doc "a nearby source boundary does not search constant properties to the end"
  (with-temp-buffer
    (dotimes (i 100)
      (insert (propertize "x" 'niwa-view-source i
                          'niwa-view-type 'response)))
    (let ((single (symbol-function 'next-single-property-change))
          (any (symbol-function 'next-property-change))
          (searched 0) boundary)
      (cl-letf (((symbol-function 'next-single-property-change)
                 (lambda (pos property &optional object limit)
                   (let ((end (funcall single pos property object limit)))
                     (cl-incf searched (- end pos))
                     end)))
                ((symbol-function 'next-property-change)
                 (lambda (pos &optional object limit)
                   (let ((end (funcall any pos object limit)))
                     (cl-incf searched (- end pos))
                     end))))
        (setq boundary (niwa-view--render-anchor-run-end 1 (point-max))))
      (should (= 2 boundary))
      (should (<= searched 3)))))

(niwa-deftest niwa-view--position-render-anchor ()
  ,test
  (test)
  :doc "captures fragment coordinates inside a managed zone"
  (with-temp-buffer
    (setq-local niwa-view--status-marker (copy-marker (point-min) t))
    (setq-local niwa-view--interaction-marker (copy-marker (point-min) t))
    (let ((niwa-view--agent-transcript-p t))
      (niwa-view-transcript-tail-initialize))
    (niwa-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "abcdef")))
    (should (equal (list 'fragment
                        (niwa-view-zone-position-anchor (+ (point-min) 3)))
                   (niwa-view--position-render-anchor
                    (+ (point-min) 3)))))

  :doc "captures the source map for rendered transcript text"
  (with-temp-buffer
    (insert "head ")
    (insert (propertize "rendered run" 'niwa-view-source '(40 . 90)))
    (insert " tail\n")
    (should (equal '(source (40 nil nil) 0 4)
                   (niwa-view--position-render-anchor
                    (+ (point-min) (length "head ") 4)))))

  :doc "counts earlier runs sharing the same source start"
  (with-temp-buffer
    (insert (propertize "header" 'niwa-view-source '(40 . 90)))
    (insert "\n")
    (let ((body-start (point)))
      (insert (propertize "body text" 'niwa-view-source (cons 40 90)))
      (insert "\n")
      (should (equal '(source (40 nil nil) 1 2)
                     (niwa-view--position-render-anchor
                      (+ body-start 2))))))

  :doc "a replaced region tracks outside positions by marker and counts runs inside it"
  (with-temp-buffer
    (insert (propertize "header" 'niwa-view-source '(40 . 90)))
    (insert "\n")
    (let ((region-start (point)))
      (insert (propertize "body text" 'niwa-view-source (cons 40 90)))
      (let ((region (cons region-start (point))))
        (insert "\ntail\n")
        (pcase-let ((`(marker ,before) (niwa-view--position-render-anchor 2 nil region))
                    (`(marker ,after) (niwa-view--position-render-anchor
                                       (cdr region) nil region)))
          (should (= 2 before))
          (should-not (marker-insertion-type before))
          (should (= (cdr region) after))
          ;; Text inserted where the region ends stays before AFTER.
          (should (marker-insertion-type after))
          (niwa-view--release-render-anchor (list 'marker before))
          (should-not (marker-buffer before)))
        ;; Unlike a whole-buffer anchor, the header before the region is not
        ;; counted: resolution scans only the replaced region.
        (should (equal (list 'source '(40 nil nil) 0 2 (car region) (cdr region))
                       (niwa-view--position-render-anchor
                        (+ region-start 2) nil region))))))

  :doc "returns nil for unannotated text and out-of-buffer positions"
  (with-temp-buffer
    (insert "plain text\n")
    (should-not (niwa-view--position-render-anchor (point-min)))
    (should-not (niwa-view--position-render-anchor (point-max)))))

(niwa-deftest niwa-view--render-anchor-container ()
  ,test
  (test)
  :doc "returns the narrowest run whose source contains the position"
  (with-temp-buffer
    (insert (propertize "turn\n" 'niwa-view-source '(10 . 100)))
    (let ((inner (point)))
      (insert (propertize "group\n" 'niwa-view-source '(20 . 60)))
      (insert "plain\n")
      (should (= inner (niwa-view--render-anchor-container 30 (point-max))))
      (should (= (point-min) (niwa-view--render-anchor-container 70 (point-max))))
      (should-not (niwa-view--render-anchor-container 200 (point-max)))))
  :doc "ignores runs at or after the limit"
  (with-temp-buffer
    (insert "plain\n")
    (let ((limit (point)))
      (insert (propertize "group\n" 'niwa-view-source '(20 . 60)))
      (should-not (niwa-view--render-anchor-container 30 limit))))
  :doc "starts at ORIGIN"
  (with-temp-buffer
    (insert (propertize "turn\n" 'niwa-view-source '(10 . 100)))
    (let ((origin (point)))
      (insert "plain\n")
      (should-not (niwa-view--render-anchor-container 30 (point-max) origin))
      (should (= (point-min)
                 (niwa-view--render-anchor-container 30 (point-max)))))))

(niwa-deftest niwa-view--release-render-anchor ()
  ,test
  (test)
  :doc "detaches a marker anchor"
  (with-temp-buffer
    (insert "text")
    (let ((marker (copy-marker 2)))
      (niwa-view--release-render-anchor (list 'marker marker))
      (should-not (marker-buffer marker))))
  :doc "ignores anchors without markers"
  (progn
    (should-not (niwa-view--release-render-anchor '(source (1 nil nil) 0 0)))
    (should-not (niwa-view--release-render-anchor nil))))

(niwa-deftest niwa-view--render-anchor-position ()
  ,test
  (test)
  :doc "resolves a fragment anchor and clamps into a shrunken fragment"
  (with-temp-buffer
    (setq-local niwa-view--status-marker (copy-marker (point-min) t))
    (setq-local niwa-view--interaction-marker (copy-marker (point-min) t))
    (let ((niwa-view--agent-transcript-p t))
      (niwa-view-transcript-tail-initialize))
    (niwa-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "abcdef")))
    (let ((anchor (niwa-view--position-render-anchor (+ (point-min) 3))))
      (should (= (+ (point-min) 3)
                 (niwa-view--render-anchor-position anchor)))
      (niwa-view-zone-reconcile
       'status (point-min) (point-min)
       '((:namespace status :id tasks :body "ab")))
      (should (= (+ (point-min) 2)
                 (niwa-view--render-anchor-position anchor)))
      (should-not (niwa-view--render-anchor-position anchor t))))

  :doc "strict pending anchors reject a truncated row offset"
  (with-temp-buffer
    (insert (propertize "pending\n" 'niwa-view-pending-key 'test))
    (should (= 4 (niwa-view--render-anchor-position '(pending test nil 3) t)))
    (should (= 8 (niwa-view--render-anchor-position '(pending test nil 99))))
    (should-not (niwa-view--render-anchor-position '(pending test nil 99) t)))

  :doc "region anchors follow content around and inside a replaced region"
  (with-temp-buffer
    (insert (propertize "header" 'niwa-view-source '(40 . 90)) "\n")
    (let* ((start (point))
           (end (progn (insert (propertize "row" 'niwa-view-source '(40 . 90))
                               (propertize "detail" 'niwa-view-source '(40 . 90)
                                           'niwa-view-type 'body))
                       (point)))
           (region (progn (insert "\ntail\n")
                          (cons (copy-marker start) (copy-marker end t))))
           (before (niwa-view--position-render-anchor 3 nil region))
           (inside (niwa-view--position-render-anchor (+ start 4) nil region))
           (after (niwa-view--position-render-anchor
                   (- (point-max) 4) nil region)))
      (delete-region start end)
      (goto-char start)
      (insert (propertize "longer row" 'niwa-view-source '(40 . 90))
              (propertize "new detail" 'niwa-view-source '(40 . 90)
                          'niwa-view-type 'body))
      ;; A cleanup elsewhere, such as orphaned zone text, still resolves.
      (delete-region 1 2)
      (should (= 2 (niwa-view--render-anchor-position before)))
      (should (equal "ail\n" (buffer-substring
                             (niwa-view--render-anchor-position after)
                             (point-max))))
      ;; Offset 1 into the region's first `body' run: "new detail".
      (should (equal "ew" (buffer-substring
                           (niwa-view--render-anchor-position inside t)
                           (+ 2 (niwa-view--render-anchor-position inside t)))))))

  :doc "full render restoration keeps a tagged row when text before it grows"
  (with-temp-buffer
    (setq-local niwa-view--status-marker (copy-marker (point-min) t))
    (setq-local niwa-view--interaction-marker (copy-marker (point-min) t))
    (let ((niwa-view--agent-transcript-p t))
      (niwa-view-transcript-tail-initialize))
    (niwa-view-zone-reconcile
     'status (point-min) (point-min)
     (list (list :namespace 'status :id 'tasks
                 :body (concat "short\n" (propertize "target row\n"
                                                     'niwa-view-zone-anchor 'target)))))
    (goto-char (point-min))
    (search-forward "target")
    (niwa-view--call-preserving-window-state
     (lambda ()
       (niwa-view-zone-reconcile
        'status (point-min) (point-min)
        (list (list :namespace 'status :id 'tasks
                    :body (concat "much longer first row\nsecond row\n"
                                  (propertize "target row\n"
                                              'niwa-view-zone-anchor 'target)))))))
    (should (looking-back "target" (line-beginning-position))))

  :doc "resolves a source anchor to the run containing the data position"
  (with-temp-buffer
    (insert "a different head ")
    (let ((run-start (point)))
      (insert (propertize "rendered run" 'niwa-view-source '(40 . 90)))
      (insert " tail\n")
      (should (= (+ run-start 4)
                 (niwa-view--render-anchor-position '(source (40 nil nil) 0 4))))
      ;; Offsets past the run clamp to its last character.
      (should (= (+ run-start (1- (length "rendered run")))
                 (niwa-view--render-anchor-position '(source (40 nil nil) 0 99))))
      (should (= (+ run-start 4)
                 (niwa-view--render-anchor-position '(source (40 nil nil) 0 4) t)))
      (should-not
       (niwa-view--render-anchor-position '(source (40 nil nil) 0 99) t))))

  :doc "the ordinal selects among runs sharing one source start"
  (with-temp-buffer
    (insert (propertize "header" 'niwa-view-source '(40 . 90)))
    (insert "\n")
    (let ((body-start (point)))
      (insert (propertize "body text" 'niwa-view-source (cons 40 90)))
      (insert "\n")
      (should (= (+ (point-min) 2)
                 (niwa-view--render-anchor-position '(source (40 nil nil) 0 2))))
      (should (= (+ body-start 2)
                 (niwa-view--render-anchor-position '(source (40 nil nil) 1 2))))))

  :doc "returns nil for unresolvable anchors"
  (with-temp-buffer
    (insert "plain text\n")
    (should-not (niwa-view--render-anchor-position '(source (40 nil nil) 0 4)))
    (should-not (niwa-view--render-anchor-position
                 '(fragment (status :id tasks :offset 0))))
    (should-not (niwa-view--render-anchor-position nil)))

  :doc "a section folded into an enclosing one resolves to that run"
  (with-temp-buffer
    (insert (propertize "Assistant\n" 'niwa-view-source '(10 . 100)))
    (let ((group (point)))
      (insert (propertize "  ✓ Read 4 files\n" 'niwa-view-source '(20 . 60)
                          'niwa-view-type 'tool-group))
      (insert (propertize "Prose.\n" 'niwa-view-source '(60 . 100)))
      (should (= group (niwa-view--render-anchor-position
                        '(source (30 tool-summary nil) 0 5))))))

  :doc "section identities survive separator changes and compound child reordering"
  (with-temp-buffer
    (let ((source '(40 . 90))
          group-anchor child-anchor)
      (insert (propertize "\n" 'niwa-view-source source))
      (insert (propertize "Group\n" 'niwa-view-source source
                          'niwa-view-type 'tool-group))
      (setq group-anchor (niwa-view--position-render-anchor 3))
      (insert (propertize "Child one\n" 'niwa-view-source source
                          'niwa-view-type 'tool-child
                          'niwa-view-source-key
                          '(source tool-child 40 (in-flight) "one")))
      (setq child-anchor (niwa-view--position-render-anchor 9))
      (erase-buffer)
      ;; Source coordinates alone cannot distinguish any of these rows.
      (insert (propertize "Group\n" 'niwa-view-source source
                          'niwa-view-type 'tool-group))
      (insert (propertize "Child two\n" 'niwa-view-source source
                          'niwa-view-type 'tool-child
                          'niwa-view-source-key
                          '(source tool-child 40 (in-flight) "two")))
      (let ((child-start (point)))
        (insert (propertize "Child one\n" 'niwa-view-source source
                            'niwa-view-type 'tool-child
                            'niwa-view-source-key
                            '(source tool-child 40 (tool "settled") "one")))
        (should (= 2 (niwa-view--render-anchor-position group-anchor)))
        (should (= (+ child-start 1)
                   (niwa-view--render-anchor-position child-anchor)))
        (delete-region child-start (point-max))
        ;; A vanished child goes to its enclosing group row, not a sibling.
        (should (= (point-min)
                   (niwa-view--render-anchor-position child-anchor)))))))

(niwa-deftest niwa-view--input-text ()
  ,test
  (test)
  :doc "extracts text from input region"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string-empty-p (niwa-view--input-text)))
      (niwa-view-composer-insert "hello world"
                                     (niwa-view-composer-draft-start))
      (should (equal "hello world" (niwa-view--input-text)))))

  :doc "clear empties input region"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-insert "hello world"
                                     (niwa-view-composer-draft-start))
      (niwa-view--clear-input)
      (should (string-empty-p (niwa-view--input-text))))))

(niwa-deftest niwa-view-slash-capf
  (:before-each
   (progn
     (clrhash niwa-skills--dir-buffers)
     (clrhash niwa-skills--dirty-buffers)
     (clrhash niwa-skills--mtime-cache))
   :after-each
   (progn
     (clrhash niwa-skills--dir-buffers)
     (clrhash niwa-skills--dirty-buffers)
     (clrhash niwa-skills--mtime-cache)))
  ,test
  (test)
  :doc "view skill completion refreshes after skill saves"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "niwa-view-skills-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-skills"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (niwa-view-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (niwa-skills-install session data-buf))
          (niwa-view-composer-test--in-composer view-buf
            (goto-char (niwa-view-composer-draft-start view-buf))
            (insert "$")
            (let ((capf (niwa-view-slash-capf)))
              (should (member "alpha"
                              (niwa-view-test--capf-candidates capf)))
              (let ((skill-file
                     (niwa-view-test--write-skill
                      root "bar" "name: bar\ndescription: B\n")))
                (with-temp-buffer
                  (setq buffer-file-name skill-file)
                  (niwa-skills--before-save-hook)))
	              (should (member "bar"
	                              (niwa-view-test--capf-candidates
	                               capf "b"))))))
	      (delete-directory root t)))

  :doc "view skill completion binds the exact discovered source"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-view-skill-binding-" t))
         (niwa-skill-dirs (list root))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-skill-binding"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (source-file
          (niwa-view-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (niwa-skills-install session data-buf))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$al"
                                           (niwa-view-composer-draft-start))
            (let* ((capf (niwa-view-slash-capf))
                   (exit (plist-get (nthcdr 3 capf) :exit-function)))
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert "alpha")
              (funcall exit "alpha" 'finished))
            (let* ((start (niwa-view-composer-draft-start))
                   (binding (get-text-property
                             start 'niwa-mention-binding)))
              (should (equal 'skill (plist-get binding :kind)))
              (should (equal "$alpha" (plist-get binding :token)))
              (should (equal source-file
                             (plist-get binding :source-file)))
              (niwa-view-composer-insert "analyze")
              (should (equal binding
                             (get-text-property
                              start 'niwa-mention-binding))))))
      (delete-directory root t)))

  :doc "view completes Goal commands and permission-mode arguments"
  (let* ((root (make-temp-file "niwa-view-mode-capf-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-mode-capf"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "/go"
                                           (niwa-view-composer-draft-start))
            (let ((capf (niwa-view-slash-capf)))
              (should capf)
              (should (equal '("goal")
                             (niwa-view-test--capf-candidates
                              capf "go"))))
            (niwa-view-composer-replace "")
            (niwa-view-composer-insert "/mode fu"
                                           (niwa-view-composer-draft-start))
            (let ((capf (niwa-view-slash-capf)))
              (should capf)
              (should (equal '("full-auto")
                             (niwa-view-test--capf-candidates
                              capf "fu"))))))
      (delete-directory root t)))

  :doc "view review command completes target arguments"
  (let* ((root (make-temp-file "niwa-view-review-capf-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-review-capf"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "/review cur"
                                           (niwa-view-composer-draft-start))
            (let ((capf (niwa-view-slash-capf)))
              (should capf)
              (should (equal '("current")
                             (niwa-view-test--capf-candidates
                              capf "cur"))))))
      (delete-directory root t)))

  :doc "view root completion inserts a real separator before skill hint"
  (let* ((root (make-temp-file "niwa-view-root-space-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-root-space"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "remember"
                 :argument-names '("focus"))))
    (setf (niwa-session-skills session) (list skill))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$rem"
                                           (niwa-view-composer-draft-start))
            (let* ((capf (niwa-view-slash-capf))
                   (exit (and capf (plist-get (nthcdr 3 capf)
                                              :exit-function))))
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert "remember")
              (funcall exit "remember" 'finished)
              (niwa-view--refresh-skill-argument-hint)
              (should (string-match-p
                       "\\[focus\\]"
                       (niwa-view-test--skill-hint-string)))
              (insert "d")
              (should (equal "$remember d"
                             (niwa-view-composer-draft-text nil t))))))
      (delete-directory root t))))

(niwa-deftest niwa-view-resource-capf
  (:doc "composer installs inert resource-address completion")
  (let* ((save-path (make-temp-file "niwa-view-resource-capf-" t))
         (session (niwa-session--create
                   :name "resource-capf" :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (with-temp-file
              (file-name-concat save-path "local" "note.md")
            (insert "note"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (niwa-view-composer-test--in-composer view-buf
              (should (memq #'niwa-resource-capf
                            completion-at-point-functions))
              (goto-char (niwa-view-composer-draft-start view-buf))
              (insert "work://")
              (let* ((result (niwa-resource-capf))
                     (candidates (niwa-view-test--capf-candidates result)))
                (should (member "work://note.md" candidates))
                (should-not (get-text-property
                             0 'niwa-mention-binding
                             (car candidates)))))))
      (delete-directory save-path t))))

(niwa-deftest niwa-view-side-resource-capf ()
  ,test
  (test)
  :doc "side composers install resource-address completion"
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      ;; The setup above creates a normal composer.  Reinitialize the same
      ;; editable surface as a side composer after removing the first hook so
      ;; this assertion covers the side-specific initialization branch.
      (remove-hook 'completion-at-point-functions
                   #'niwa-resource-capf t)
      (setq-local niwa-view--side-conversation-p t)
      (niwa-view-composer-initialize)
      (should (memq #'niwa-resource-capf
                    completion-at-point-functions))))

  :doc "side composers invalidate file bindings extended with line ranges"
  (niwa-view-test--with-buffers
    (niwa-view-composer-test--in-composer view-buf
      (remove-hook 'after-change-functions
                   #'niwa-view--refresh-skill-argument-hint-after-change t)
      (setq-local niwa-view--side-conversation-p t)
      (niwa-view-composer-initialize)
      (should (memq #'niwa-view--refresh-skill-argument-hint-after-change
                    after-change-functions))
      (goto-char (niwa-view-composer-draft-start))
      (let* ((workflow-token
              "@file:/tmp/.github/workflows/test.yml")
             (ranged-token (concat workflow-token "#L25"))
             (png-token "@file:/tmp/screenshot.png")
             (workflow-start (point)))
        (insert workflow-token " " png-token)
        (niwa-mention-bindings-set
         workflow-start (+ workflow-start (length workflow-token))
         (list :kind 'file :token workflow-token
               :path "/tmp/.github/workflows/test.yml"))
        (let ((png-start (+ workflow-start (length workflow-token) 1)))
          (niwa-mention-bindings-set
           png-start (+ png-start (length png-token))
           (list :kind 'file :token png-token :path "/tmp/screenshot.png")))
        (goto-char (+ workflow-start (length workflow-token)))
        (insert "#L25")
        (should-not (get-text-property
                     workflow-start 'niwa-mention-binding))
        (should (get-text-property
                 (+ workflow-start (length ranged-token) 1)
                 'niwa-mention-binding))
        (let* ((prepared
                (niwa-mentions-prepare-user-input
                 (niwa-view-composer-draft-text)))
               (tokens
                (mapcar (lambda (range)
                          (plist-get (plist-get range :binding) :token))
                        (niwa-mention-bindings-ranges prepared))))
          (should (equal (list ranged-token png-token) tokens)))))))

(niwa-deftest niwa-view--refresh-skill-argument-hint ()
  ,test
  (test)
  :doc "argument-hint appears as overlay text before args"
  (let* ((root (make-temp-file "niwa-view-skill-hint-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-skill-hint"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (niwa-session-skills session) (list skill))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$green-loop"
                                           (niwa-view-composer-draft-start))
            (niwa-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "What change should be validated"
                     (niwa-view-test--skill-hint-string)))
            (should (equal "$green-loop" (niwa-view-composer-draft-text nil t)))
            (insert " current changes")
            (niwa-view--refresh-skill-argument-hint)
            (should-not (niwa-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "argument names show only remaining slots"
  (let* ((root (make-temp-file "niwa-view-named-hint-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-named-hint"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "deploy-api"
                 :argument-names '("service" "environment"))))
    (setf (niwa-session-skills session) (list skill))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$deploy-api"
                                           (niwa-view-composer-draft-start))
            (niwa-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[service\\] \\[environment\\]"
                     (niwa-view-test--skill-hint-string)))
            (insert " billing")
            (niwa-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[environment\\]"
                     (niwa-view-test--skill-hint-string)))
            (should-not (string-match-p
                         "\\[service\\]"
                         (niwa-view-test--skill-hint-string)))))
      (delete-directory root t)))

  :doc "non-skill input clears the overlay"
  (let* ((root (make-temp-file "niwa-view-clear-hint-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-clear-hint"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (niwa-session-skills session) (list skill))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$green-loop"
                                           (niwa-view-composer-draft-start))
            (niwa-view--refresh-skill-argument-hint)
            (should (niwa-view-test--skill-hint-string))
            (niwa-view-composer-replace "")
            (insert "hello")
            (niwa-view--refresh-skill-argument-hint)
            (should-not (niwa-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "motion neither moves nor hides the hint, and does no prompt scan"
  ;; The hint used to follow point: moving into the draft reflowed the line
  ;; around it, and leaving the composer removed it.
  (let* ((root (make-temp-file "niwa-view-motion-hint-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "view-motion-hint"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "deploy-api"
                 :argument-names '("service" "environment"))))
    (setf (niwa-session-skills session) (list skill))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-composer-insert "$deploy-api billing"
                                           (niwa-view-composer-draft-start))
            (run-hooks 'post-command-hook)
            (let ((hint (niwa-view-test--skill-hint-string)))
              (should (string-match-p "\\[environment\\]" hint))
              (should (= (point-max) (overlay-start
                                      niwa-view--skill-argument-hint-overlay)))
              (cl-letf (((symbol-function 'niwa-view-composer-draft-start)
                         (lambda (&rest _)
                           (error "Motion must not scan the prompt")))
                        ((symbol-function 'niwa-view-transcript-tail)
                         (lambda ()
                           (error "Motion must not scan the prompt"))))
                (dolist (position (list (- (point-max) 9) (point-min)))
                  (goto-char position)
                  (run-hooks 'post-command-hook)
                  (should (equal hint (niwa-view-test--skill-hint-string)))
                  (should (= (point-max)
                             (overlay-start
                              niwa-view--skill-argument-hint-overlay))))))
            (should (equal "$deploy-api billing" (niwa-view-composer-draft-text nil t)))))
      (delete-directory root t))))

(niwa-deftest niwa-view-send/local-slash-command (:quiet t)
  ,test
  (test)
  :doc "local slash command string results are shown"
  (niwa-view-test--with-buffers
    (let ((niwa-slash-commands
           (list (cons "fake" (lambda (_args) "fake result"))))
          seen)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq seen (apply #'format fmt args)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "/fake"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (equal "fake result" seen))
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))))))
  :doc "local shared-command refusals are attributed while shared calls stay raw"
  (dolist (case '(("worktree" "unknown" niwa-cmd--worktree
                   "Unknown /worktree command: unknown" nil)
                  ("btw" "" niwa-cmd--btw
                   "No niwa session in this buffer" t)))
    (niwa-view-test--with-buffers
      (pcase-let ((`(,name ,args ,handler ,reason ,active) case))
        (with-current-buffer data-buf
          (setq-local niwa--workspace
                      (niwa-workspace--create :root "/tmp/command origin/")
                      niwa-slash-commands (list (cons name handler))
                      niwa--current-request (and active (niwa-request--create)))
          (should (equal reason (cadr (should-error (funcall handler args)
                                                   :type 'user-error)))))
        (with-current-buffer view-buf
          (setq-local niwa-slash-commands (list (cons name handler)))
          (let ((input (concat "/" name (unless (string-empty-p args) (concat " " args)))))
            (niwa-view-composer-insert input
                                           (niwa-view-composer-draft-start))
            (should (equal (concat "niwa [/tmp/command origin/]: " reason)
                           (cadr (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error))))
            (should (equal input (niwa-view-composer-draft-text nil t))))))))
  :doc "/goal pause reaches the lifecycle handler during an active request"
  (niwa-view-test--with-buffers
    (let* (seen
           (commands
            (list (cons "goal" (lambda (args) (setq seen args))))))
      (with-current-buffer data-buf
        (setq-local niwa--current-request (niwa-request--create)
                    niwa-slash-commands commands))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "/goal pause"
                                       (niwa-view-composer-draft-start))
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send))
        (should (equal "pause" seen))
        (should (string-empty-p (niwa-view-composer-draft-text nil t))))))
  :doc "/ps and /stop remain available during an active request"
  (dolist (command '("ps" "stop"))
    (niwa-view-test--with-buffers
      (let* (seen
             (commands
              (list (cons command (lambda (args) (setq seen args))))))
        (with-current-buffer data-buf
          (setq-local niwa--current-request (niwa-request--create)
                      niwa-slash-commands commands))
        (with-current-buffer view-buf
          (niwa-view-composer-insert (if (string= command "stop")
                                            "/stop exec-000001"
                                          "/ps")
                                        (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (equal (if (string= command "stop") "exec-000001" "")
                         seen))
          (should (string-empty-p (niwa-view-composer-draft-text nil t))))))))
(niwa-deftest niwa-view-send/dollar-text ()
  (niwa-view-composer-test--without-admission ,test)
  (test)
  :doc "unknown dollar-prefixed input sends as normal prompt text"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vd"
                :root (niwa-test--placeholder-root) :name "vd"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           send-called)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$PATH is relevant"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))))
      (should send-called)
      (with-current-buffer data-buf
        (should (string-match-p "\\$PATH is relevant"
                                (buffer-string))))))

  :doc "C-g during the synchronous start settles as aborted, not error"
  ;; A quit is the user's own cancellation: recording it as a provider
  ;; error polluted failure accounting, and the interrupted start left
  ;; gptel's mode-line at " Waiting" for a request that no longer
  ;; existed.
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           summary statuses)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'niwa-view--append-request-summary)
                 (lambda (_buffer _position plist) (setq summary plist)))
                ((symbol-function 'gptel--update-status)
                 (lambda (msg &rest _) (push msg statuses))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "please do the thing"
                                         (niwa-view-composer-draft-start))
          ;; `should-error' handles only `error' conditions; the
          ;; re-signalled quit needs its own handler.
          (should (eq 'caught
                      (condition-case nil
                          (progn (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) nil)
                        (quit 'caught))))))
      (should (eq 'aborted (plist-get summary :outcome)))
      (should (string-match-p "Interrupted" (plist-get summary :message)))
      (should (member " Ready" statuses))
      (with-current-buffer data-buf
        (should-not niwa--current-request))))

  :doc "history write failure warns without cancelling a valid view send"
  (let ((root (make-temp-file "niwa-history-send-" t))
        warning
        send-called)
    (unwind-protect
        (let* ((ws (niwa-workspace--create
                    :type 'file :id root :root root :name "history-send"
                    :file-cache (niwa-file-cache--create
                                 :table (make-hash-table :test #'equal)
                                 :order nil :total-bytes 0)))
               (session (niwa-session-create "main" ws)))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws))
            (with-current-buffer view-buf
              (niwa-view-history-add "older input")
              (cl-letf (((symbol-function
                         'niwa-session-codec-write)
                         (lambda (&rest _)
                           (error "Simulated history write failure")))
                        ((symbol-function 'display-warning)
                         (lambda (_type message &rest _)
                           (setq warning message))))
                (niwa-view-history-save))
              (niwa-view-composer-insert "valid prompt"
                                             (niwa-view-composer-draft-start))
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _) (setq send-called t))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (should (string-match-p "Input history save failed" warning))
            (should send-called)))
      (delete-directory root t))))

(defmacro niwa-view-test--with-fork-skill (skill-form &rest body)
  "Install a source-backed SKILL-FORM, then run BODY in paired buffers.
Binds `data-buf', `view-buf', `session', and `skill' in scope."
  (declare (indent 1) (debug t))
  `(let* ((niwa-skills-include-bundled nil)
          (root (make-temp-file "niwa-view-skill-send-" t))
          (niwa-skill-dirs (list root))
          (template ,skill-form)
          (source
           (niwa-view-test--write-skill
            root
            (niwa-skill-name template)
            (format "name: %s\ndescription: Test skill\ncontext: %s\n"
                    (niwa-skill-name template)
                    (or (niwa-skill-context template) 'inline))
            (niwa-skill-body template)))
          (ws (niwa-workspace--create
               :type 'file :id root :root root :name "view-skill-send"
               :file-cache (niwa-file-cache--create
                            :table (make-hash-table :test #'equal)
                            :order nil :total-bytes 0)))
          (session (niwa-session-create "main" ws)))
     (unwind-protect
         (niwa-view-test--with-buffers
           (with-current-buffer data-buf
             (setq-local niwa--session session
                         niwa--workspace ws)
             (niwa-skills-install session data-buf))
           (let ((skill (niwa-session-get-skill
                         session (niwa-skill-name template))))
             (should (equal source (niwa-skill-source-file skill)))
             ,@body))
       (delete-directory root t))))

(defmacro niwa-view-test--with-source-skills (specs &rest body)
  "Install source-backed SPECS and run BODY in paired session buffers.
Each spec is (NAME CONTEXT BODY &optional EXTRA-FRONTMATTER)."
  (declare (indent 1) (debug t))
  `(let* ((niwa-skills-include-bundled nil)
          (root (make-temp-file "niwa-view-planned-skills-" t))
          (niwa-skill-dirs (list root))
          (ws (niwa-workspace--create
               :type 'file :id root :root root :name "planned-skills"
               :file-cache (niwa-file-cache--create
                            :table (make-hash-table :test #'equal)
                            :order nil :total-bytes 0)))
          (session (niwa-session-create "main" ws)))
     (unwind-protect
         (progn
           (dolist (spec ,specs)
             (niwa-view-test--write-skill
              root (nth 0 spec)
              (format "name: %s\ndescription: Test %s\ncontext: %s\n%s"
                      (nth 0 spec) (nth 0 spec) (nth 1 spec)
                      (or (nth 3 spec) ""))
              (nth 2 spec)))
           (niwa-view-test--with-buffers
             (with-current-buffer data-buf
               (setq-local niwa--session session
                           niwa--workspace ws)
               (niwa-skills-install session data-buf))
             ,@body))
       (delete-directory root t))))

(defun niwa-view-test--count-matches (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))


;;
;;; Planned submission helpers

(niwa-deftest niwa-view--cancel-pending-submission ()
  ,test
  (test)
  :doc "marks the active preparation token cancelled and clears ownership"
  (with-temp-buffer
    (let ((token (list :cancelled nil)))
      (setq-local niwa-view--pending-skill-submission token)
      (niwa-view--cancel-pending-submission)
      (should (plist-get token :cancelled))
      (should-not niwa-view--pending-skill-submission)))
  :doc "cancels and clears an active prompt-hook submission"
  (with-temp-buffer
    (let ((submission (niwa-prompt-submission-create)))
      (setq-local niwa-view--prompt-hook-pending submission)
      (niwa-view--cancel-pending-submission)
      (should (eq 'cancelled
                  (niwa-prompt-submission-state submission)))
      (should-not niwa-view--prompt-hook-pending))))

(niwa-deftest niwa-view--skill-submission-active-p ()
  ,test
  (test)
  :doc "requires live buffers, current ownership, and a non-cancelled token"
  (niwa-view-test--with-buffers
    (let ((token (list :cancelled nil)))
      (with-current-buffer view-buf
        (setq-local niwa-view--pending-skill-submission token))
      (should (niwa-view--skill-submission-active-p
               token view-buf data-buf))
      (setf (plist-get token :cancelled) t)
      (should-not (niwa-view--skill-submission-active-p
                   token view-buf data-buf))
      (setf (plist-get token :cancelled) nil)
      (with-current-buffer view-buf
        (setq niwa-view--pending-skill-submission (list :cancelled nil)))
      (should-not (niwa-view--skill-submission-active-p
                   token view-buf data-buf)))))

(niwa-deftest niwa-view--finish-skill-submission ()
  ,test
  (test)
  :doc "clears only the token that owns the pending submission"
  (with-temp-buffer
    (let ((token (list :cancelled nil))
          (other (list :cancelled nil)))
      (setq-local niwa-view--pending-skill-submission token)
      (niwa-view--finish-skill-submission other)
      (should (eq token niwa-view--pending-skill-submission))
      (niwa-view--finish-skill-submission token)
      (should-not niwa-view--pending-skill-submission))))

(niwa-deftest niwa-view--prepared-fork-outcome ()
  ,test
  (test)
  :doc "returns the first prepared fork outcome and nil when none exists"
  (let ((fork '(:status ok :kind fork :body "fork")))
    (should
     (eq fork
         (niwa-view--prepared-fork-outcome
          (list :prepared-entries
                (list '(:outcome (:status ok :kind inline))
                      (list :outcome fork))))))
    (should-not
     (niwa-view--prepared-fork-outcome
      '(:prepared-entries ((:outcome (:status ok :kind instruction))))))))

(niwa-deftest niwa-view--block-planned-submission ()
  ,test
  (test)
  :doc "clears ownership, reports preparation failure, and invokes on-block"
  (with-temp-buffer
    (let ((token (list :cancelled nil))
          blocked
          notice)
      (setq-local niwa-view--pending-skill-submission token)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq notice (apply #'format format-string args)))))
        (niwa-view--block-planned-submission
         (list :token token :on-block (lambda () (setq blocked t)))
         '(:status error :name "alpha" :message "failed")))
      (should blocked)
      (should-not niwa-view--pending-skill-submission)
      (should (equal "niwa [workspace unavailable]: failed (skill $alpha failed)"
                     notice)))))

(niwa-deftest niwa-view--prepared-plan-outcome ()
  ,test
  (test)
  :doc "returns every prepared model, transcript, policy, and audit component"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (niwa-skills-plan-user-input input session))
           prepared)
      (with-current-buffer data-buf
        (niwa-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let ((outcome
             (niwa-view--prepared-plan-outcome
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
               (data (cdr (niwa-tool-render-data-extract block))))
          (should (equal "Use [skill:alpha -- attached]"
                         (plist-get data :prompt)))
          (should (equal "alpha"
                         (plist-get
                          (car (plist-get data :attachments)) :name))))
        (should (equal '((:type prompt-rewrite))
                       (plist-get outcome :hook-audits)))
        (should-not (plist-get outcome :fork-outcome))))))

(niwa-deftest niwa-view--dispatch-prepared-plan ()
  ,test
  (test)
  :doc "dispatches a source-backed prepared plan while preserving additions"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (niwa-skills-plan-user-input input session))
           prepared
           forwarded
           before)
      (with-current-buffer data-buf
        (niwa-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let* ((token (list :cancelled nil))
             (plan-submission
              (list :token token :input input :plan plan
                    :view-buffer view-buf :data-buffer data-buf
                    :before-send (lambda () (setq before t))))
             (prompt-submission
              (niwa-prompt-submission-create
               :input (concat (plist-get prepared :model-input) " $literal")
               :display-text input)))
        (with-current-buffer view-buf
          (setq-local niwa-view--pending-skill-submission token)
          (cl-letf (((symbol-function 'niwa-view--forward-input)
                     (lambda (&rest args) (setq forwarded args))))
            (niwa-view--dispatch-prepared-plan
             plan-submission prepared prompt-submission))
          (should-not niwa-view--pending-skill-submission))
        (should before)
        (should (string-match-p "ALPHA BODY"
                                (plist-get (cdr forwarded) :model-input)))
        (should (string-match-p (regexp-quote "$literal")
                                (plist-get (cdr forwarded) :model-input)))
        (with-current-buffer data-buf
          (should (equal (plist-get prepared :request-context)
                         niwa-skills--pending-request-context)))))))

(niwa-deftest niwa-view--handle-prepared-plan ()
  ,test
  (test)
  :doc "runs the prompt gate and dispatches a successful prepared plan"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let* ((input "Use $alpha")
           (plan (niwa-skills-plan-user-input input session))
           prepared
           forwarded)
      (with-current-buffer data-buf
        (niwa-skills-plan-prepare plan (lambda (value) (setq prepared value))))
      (let* ((token (list :cancelled nil))
             (submission
              (list :token token :input input :plan plan
                    :view-buffer view-buf :data-buffer data-buf)))
        (with-current-buffer view-buf
          (setq-local niwa-view--pending-skill-submission token)
          (cl-letf (((symbol-function 'niwa-view--forward-input)
                     (lambda (&rest args) (setq forwarded args))))
            (niwa-view--handle-prepared-plan submission prepared)))
        (should (string-match-p "ALPHA BODY"
                                (plist-get (cdr forwarded) :model-input)))))))

(niwa-deftest niwa-view--submit-planned-input ()
  ,test
  (test)
  :doc "plans and submits source-backed instructions through the view seam"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (sent before blocked)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view--submit-planned-input
           "Use $alpha"
           (lambda () (setq before t))
           (lambda () (setq blocked t)))))
      (setq sent (niwa-tool-render-data-strip sent))
      (should before)
      (should-not blocked)
      (should (string-match-p "ALPHA BODY" sent))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") sent)))))

(niwa-deftest niwa-view-send/skill-inline (:quiet t)
  (niwa-view-composer-test--without-admission ,test)
  (test)
  :doc "inline attachment reaches the model-visible gptel request"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "thermo-review"
       :body "THERMO BODY"
       :context 'inline
       :user-invocable-p t
       :model-invocable-p nil)
    (let (request-data)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq request-data
                         (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Run $thermo-review on the repository"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should (string-search "THERMO BODY" request-data))
      (should (string-search "[skill:thermo-review -- attached]"
                             request-data))))

  :doc "a draft typed while skill preparation runs survives the send"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((prepare (symbol-function 'niwa-skills-plan-prepare))
          (send-count 0))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf send-count)))
                ;; Real preparation is asynchronous whenever a body carries
                ;; a shell or Elisp injection.
                ((symbol-function 'niwa-skills-plan-prepare)
                 (lambda (&rest args)
                   (run-at-time 0 nil
                                (lambda () (apply prepare args))))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Run $myskill now"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (= 0 send-count))
          ;; The user keeps typing while preparation is pending.
          (niwa-view-composer-replace "> quoted\nsecond line")
          (let ((deadline (+ (float-time) 10)))
            (while (and (= 0 send-count) (< (float-time) deadline))
              (accept-process-output nil 0.01)
              (sit-for 0.01)))
          (should (= 1 send-count))
          (should (equal "> quoted\nsecond line"
                         (niwa-view-composer-draft-text nil t)))))))

  :doc "inline skill forwards expanded body with render-data side channel"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let (send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$myskill hello"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (let ((text (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should-not (string-match-p "Expanded hello" text))))
        (with-current-buffer view-buf
          (goto-char (point-min))
          (search-forward "Prompt")
          (niwa-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p (regexp-quote "$myskill hello")
                                    expanded))
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "niwa-render-data" expanded)))
          (niwa-view-toggle-section))
        (should send-called)
        (with-current-buffer data-buf
          (let* ((text (buffer-string))
                 (visible
                  (niwa-tool-render-data-strip text)))
            (should (string-match-p (regexp-quote "$myskill hello") text))
            (should-not (string-match-p "Expanded hello" visible))
            (should (string-search "<!-- niwa-render-data -->" text))
            (goto-char (point-min))
            (search-forward "<!-- niwa-render-data -->")
            (should (eq 'niwa-render-data
                        (get-text-property (match-beginning 0)
                                           'gptel)))
            (should (eq t
                        (get-text-property (match-beginning 0)
                                           'niwa-render-data)))
            (should (string-match-p
                     (regexp-quote "$myskill hello")
                     (niwa-tool-render-data-strip text)))))
	        (with-current-buffer view-buf
	          (niwa-view--full-rerender)
	          (let ((text (buffer-substring-no-properties
	                       (point-min) (niwa-view-transcript-tail))))
	            (should (string-match-p "\\$myskill hello" text))
	            (should (string-match-p "Prompt" text))
	            (should-not (string-match-p "Expanded hello" text)))
	          (goto-char (point-min))
	          (search-forward "◆ Prompt")
	          (niwa-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) (niwa-view-transcript-tail))))
	            (should (string-match-p (regexp-quote "$myskill hello")
                                      expanded))
	            (should (string-match-p "Expanded hello" expanded)))))))

  :doc "inline skill expansion rewrites render hook audit and context"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((niwa-hook-rules
           '((UserPromptExpansion
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        niwa-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$myskill hello"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook changed prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "hook policy context" text)))
          (goto-char (point-min))
          (search-forward "hook changed prompt")
          (niwa-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) (niwa-view-transcript-tail))))
	            (should (string-match-p "UserPromptExpansion" expanded))
	            (should (string-match-p "Expanded hello" expanded))
	            (should (string-match-p "rewritten prompt" expanded)))
	          (goto-char (point-min))
	          (search-forward "hook changed prompt")
	          (niwa-view-toggle-section)
	          (goto-char (point-min))
	          (search-forward "◆ Prompt")
          (niwa-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "hook policy context" expanded)))))
      (with-current-buffer data-buf
        (let* ((text (buffer-string))
               (visible
                (niwa-tool-render-data-strip text)))
          (should (string-match-p (regexp-quote "$myskill hello") visible))
          (should-not (string-match-p "rewritten prompt" visible))
          (should (string-match-p "hook policy context" visible))
          (should (string-match-p "<!-- niwa-hook-audit -->" visible))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "hook changed prompt" text))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "hook policy context" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (niwa-view-toggle-section)
        (should (string-match-p
                 "hook policy context"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))))))

  :doc "manually typed queued mention keeps its exact source and latest body"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-inline-bound-queue-" t))
         (project-skills (file-name-concat root ".niwa/skills"))
         (user-skills (make-temp-file "niwa-inline-bound-user-" t))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "inline-bound-queue"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         source-file
         request-data)
    (unwind-protect
        (progn
          (setq source-file
                (niwa-view-test--write-skill
                 project-skills "alpha"
                 "name: alpha\ndescription: Project alpha\ncontext: inline\n"
                 "ORIGINAL ALPHA V1"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws
                          niwa--current-request
                          (niwa-request--create :session session))
              (niwa-skills-install session data-buf))
            (cl-letf (((symbol-function
                        'niwa-pending-inputs-schedule-held-input-delivery)
                       #'ignore)
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (niwa-view-composer-insert "Please use $alpha for the queued analysis"
                                               (niwa-view-composer-draft-start))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send-follow-up)))
              (let* ((queue (niwa-session-pending-follow-ups session))
                     (input (plist-get (car queue) :input))
                     (start (string-match "\\$alpha" input))
                     (binding (and start
                                   (get-text-property
                                    start 'niwa-mention-binding input))))
                (should (= 1 (length queue)))
                (should (equal source-file
                               (plist-get binding :source-file))))
              (niwa-view-test--write-skill
               user-skills "alpha"
               "name: alpha\ndescription: User alpha\ncontext: inline\n"
               "COMPETING ALPHA")
              (setq niwa-skill-dirs
                    (list ".niwa/skills" user-skills))
              (delete-file source-file)
              (with-current-buffer data-buf
                (setq-local niwa--current-request nil))
              (niwa-view--drain-follow-up data-buf))
            (should (string-search "[skill:alpha -- unavailable]"
                                   request-data))
            (should-not (string-search "ORIGINAL ALPHA V1" request-data))
            (should-not (string-search "COMPETING ALPHA" request-data))
            (should-not (niwa-session-pending-follow-ups session))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "completion binding survives outside edits after a name collision"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-inline-bound-edit-" t))
         (project-skills (file-name-concat root ".niwa/skills"))
         (user-skills (make-temp-file "niwa-inline-bound-edit-user-" t))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "inline-bound-edit"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (niwa-view-test--write-skill
           project-skills "alpha"
           "name: alpha\ndescription: Project alpha\ncontext: inline\n"
           "PROJECT ALPHA BODY")
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws)
              (niwa-skills-install session data-buf))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Please use $al"
                                             (niwa-view-composer-draft-start))
              (niwa-view-test--complete-skill "alpha")
              (insert "for details"))
            (niwa-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING ALPHA BODY")
            (setq niwa-skill-dirs
                  (list ".niwa/skills" user-skills))
            (with-current-buffer data-buf
              (niwa-skills-install session data-buf))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (should (string-search "PROJECT ALPHA BODY" request-data))
            (should-not (string-search "COMPETING ALPHA BODY"
                                       request-data))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "editing inside a completion-bound token resolves the edited skill"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-inline-bound-rebind-" t))
         (skill-root (file-name-concat root ".niwa/skills"))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "inline-bound-rebind"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (niwa-view-test--write-skill
           skill-root "alpha"
           "name: alpha\ndescription: Alpha\ncontext: inline\n"
           "ALPHA BODY")
          (niwa-view-test--write-skill
           skill-root "beta"
           "name: beta\ndescription: Beta\ncontext: inline\n"
           "BETA BODY")
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws)
              (niwa-skills-install session data-buf))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Please use $al"
                                             (niwa-view-composer-draft-start))
              (niwa-view-composer-test--in-composer view-buf
                (niwa-view-test--complete-skill "alpha")
                (goto-char (niwa-view-composer-draft-start view-buf))
                (search-forward "$alpha")
                (replace-match "$beta" t t)))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (should (string-search "BETA BODY" request-data))
            (should-not (string-search "ALPHA BODY" request-data))))
      (delete-directory root t)))

  :doc "copy and yank preserve the exact binding on both occurrences"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (first second)
      (with-current-buffer view-buf
        (let ((kill-ring nil))
          (niwa-view-composer-insert "Use $al"
                                         (niwa-view-composer-draft-start))
          (niwa-view-composer-test--in-composer view-buf
            (niwa-view-test--complete-skill "alpha")
            (setq first
                  (get-text-property
                   (+ (niwa-view-composer-draft-start view-buf) 4)
                   'niwa-mention-binding))
            (kill-ring-save (+ (niwa-view-composer-draft-start view-buf) 4)
                            (niwa-view-composer-draft-end view-buf))
            (niwa-view-composer-insert " and ")
            (yank)
            (setq second
                  (get-text-property
                   (- (niwa-view-composer-draft-end view-buf) 6)
                   'niwa-mention-binding)))))
      (should (equal first second))))

  :doc "persisted history recall submits the latest exact source after a collision"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-inline-bound-history-" t))
         (project-skills (file-name-concat root ".niwa/skills"))
         (user-skills
          (make-temp-file "niwa-inline-bound-history-user-" t))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "inline-bound-history"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         request-data)
    (unwind-protect
        (progn
          (niwa-view-test--write-skill
           project-skills "alpha"
           "name: alpha\ndescription: Project alpha\ncontext: inline\n"
           "PROJECT HISTORY BODY V1")
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace ws)
              (niwa-skills-install session data-buf))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Recall $al"
                                             (niwa-view-composer-draft-start))
              (niwa-view-test--complete-skill "alpha")
              (niwa-view-history-add (niwa-view-composer-draft-text nil t))
              (niwa-view-history-save)
              (setq niwa-view-history--ring nil
                    niwa-view-history--loaded-entries nil)
              (niwa-view-history-load session)
              (niwa-view--clear-input)
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-history-previous)))
            (niwa-view-test--write-skill
             project-skills "alpha"
             "name: alpha\ndescription: Project alpha\ncontext: inline\n"
             "PROJECT HISTORY BODY V2")
            (niwa-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: User alpha\ncontext: inline\n"
             "COMPETING HISTORY BODY")
            (setq niwa-skill-dirs
                  (list ".niwa/skills" user-skills))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (should (string-search "PROJECT HISTORY BODY V2" request-data))
            (should-not (string-search "PROJECT HISTORY BODY V1"
                                       request-data))
            (should-not (string-search "COMPETING HISTORY BODY"
                                       request-data))))
      (delete-directory root t)
      (delete-directory user-skills t)))

  :doc "source-backed bindings remain live in a session without a workspace"
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-inline-bound-live-" t))
         (skill-root (file-name-concat root "skills"))
         (niwa-skill-dirs (list skill-root))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "main"
                   :working-directory root
                   :turn-count 0))
         source-file
         request-data)
    (unwind-protect
        (progn
          (setq source-file
                (niwa-view-test--write-skill
                 skill-root "alpha"
                 "name: alpha\ndescription: Live alpha\ncontext: inline\n"
                 "LIVE ALPHA BODY"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (niwa-skills-install session data-buf))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer view-buf
                (niwa-view-composer-insert "Use $alpha live"
                                               (niwa-view-composer-draft-start))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))
                (let* ((entry (car (niwa-view-history--entries)))
                       (start (string-match "\\$alpha" entry)))
                  (should (equal source-file
                                 (plist-get
                                  (get-text-property
                                   start 'niwa-mention-binding entry)
                                  :source-file))))))
            (should (string-search "LIVE ALPHA BODY" request-data))))
      (delete-directory root t)))

  :doc "mixed unavailable targets annotate safely and dispatch once"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA SECRET"))
    (let* ((source (niwa-skill-source-file
                    (niwa-session-get-skill session "alpha")))
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
            (cons #'niwa--transform-expand-mentions
                  (remove #'niwa--transform-expand-mentions
                          gptel-prompt-transform-functions)))
           request-data messages (send-count 0))
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (niwa-view-composer-insert prompt
                                       (niwa-view-composer-draft-start))
        (dolist (spec specs)
          (goto-char (niwa-view-composer-draft-start))
          (search-forward (car spec))
          (niwa-mention-bindings-set
           (match-beginning 0) (match-end 0) (cadr spec))))
      (delete-file source)
      (cl-letf (((symbol-function 'mcp-hub-get-servers)
                 (lambda ()
                   (list (list :name "docs" :status 'stop))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (let ((text (apply #'format format-string args)))
                     (when (string-prefix-p (format "niwa [%s]: " root) text)
                       (push text messages)))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (cl-incf send-count)
                   (setq request-data
                         (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should (= 1 send-count))
      (dolist (annotation '("[skill:alpha -- unavailable]"
                            "[note:2 -- unavailable]"
                            "[file:missing.txt -- does not exist]"
                            "[mcp:docs:file:///guide -- server"))
        (should (string-search annotation request-data)))
      (should-not (string-search "ALPHA SECRET" request-data))
      (dolist (fragment '("bound skill" "note" "file" "MCP"))
        (should (seq-some (lambda (text) (string-search fragment text))
                          messages)))
      (with-current-buffer data-buf
        (let ((text (niwa-tool-render-data-strip
                     (buffer-string))))
          (should (string-search prompt text))
          (should-not (string-search "[skill:alpha -- unavailable]"
                                     text))))
      (should (equal prompt (car (with-current-buffer view-buf
                                   (niwa-view-history--entries)))))))

  :doc "malformed mixed bindings block submission and preserve the draft"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "alpha"
       :body "ALPHA BODY"
       :context 'inline
       :user-invocable-p t)
    (let (send-called raised)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (goto-char (niwa-view-composer-draft-start))
          (let ((start (point))
                (prompt "Use $alpha @file:/tmp/a @mcp:docs:file:///api"))
            (insert prompt)
            (with-silent-modifications
              (add-text-properties
               (+ start 4) (+ start 10)
               '(niwa-mention-binding
                 (:kind skill :token "$alpha")))
              (dolist (spec
                       '(("@file:/tmp/a"
                          (:kind file :token "@file:/tmp/a" :path "/tmp/a"))
                         ("@mcp:docs:file:///api"
                          (:kind mcp :token "@mcp:docs:file:///api"
                           :server "docs" :uri "file:///api"))))
                (goto-char start)
                (search-forward (car spec))
                (niwa-mention-bindings-set
                 (match-beginning 0) (match-end 0) (cadr spec)))))
          (setq raised
                (condition-case err
                    (progn (niwa-view-send) nil)
                  (user-error (error-message-string err))))
          (should (equal (format "niwa [%s]: Malformed mention binding" root)
                         raised))
          (should-not send-called)
          (should (equal "Use $alpha @file:/tmp/a @mcp:docs:file:///api"
                         (niwa-view-composer-draft-text nil t)))))))

  :doc "missing exact source warns, annotates, and sends the multiline turn"
  (niwa-view-test--bound-source-failure-case 'missing)

  :doc "disabled exact source warns, annotates, and sends the multiline turn"
  (niwa-view-test--bound-source-failure-case 'disabled)

  :doc "unreadable exact source warns, annotates, and sends the multiline turn"
  (niwa-view-test--bound-source-failure-case 'unreadable)

  :doc "malformed exact source warns, annotates, and sends the multiline turn"
  (niwa-view-test--bound-source-failure-case 'malformed)

  :doc "non-invocable exact source warns, annotates, and sends the multiline turn"
  (niwa-view-test--bound-source-failure-case 'not-user-invocable))

(niwa-deftest niwa-view-send/planned-skills (:quiet t)
  (niwa-view-composer-test--without-admission ,test)
  (test)

  :doc "embedded mentions are instructions while a leading mention is a command"
  (dolist (case '(("Analyze with $alpha" . "ALPHA ARGS=<>")
                  ("$alpha analyze now" . "ALPHA ARGS=<analyze now>")))
    (niwa-view-test--with-source-skills
        '(("alpha" "inline" "ALPHA ARGS=<$ARGUMENTS>"))
      (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _)
                     (setq sent (niwa-view-test--dry-run-request-data)))))
          (with-current-buffer view-buf
            (niwa-view-composer-insert (car case)
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send))))
        (setq sent (niwa-tool-render-data-strip sent))
        (should (string-match-p (regexp-quote (cdr case)) sent))
        (if (string-prefix-p "$" (car case))
            (should-not (string-match-p "skill:alpha -- attached" sent))
          (should (string-match-p
                   (regexp-quote "[skill:alpha -- attached]") sent))))))

  :doc "one leading command owns the pending request model policy"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA" "model: \"fast\"\n"))
    (let (context)
      (should (equal "fast"
                     (niwa-skill-model
                      (niwa-session-get-skill session "alpha"))))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree niwa-skills--pending-request-context)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$alpha inspect"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should (equal '(:tier fast) (plist-get context :model)))
      (should (plist-member context :effort))
      (should-not (plist-get context :effort))))

  :doc "qualified preset policy reaches the realized prompt request"
  (niwa-skills-test--with-model-backends
    (let* ((niwa-skills-include-bundled nil)
           (niwa-skills-check-for-modifications nil)
           (root (make-temp-file "niwa-view-skill-policy-" t))
           (project-skills (file-name-concat root ".niwa/skills"))
           (user-skills (make-temp-file "niwa-view-skill-policy-user-" t))
           (niwa-skill-dirs (list ".niwa/skills" user-skills))
           (niwa-model-workloads
            '(($local:alpha :provider "Fast:fast-model" :effort high)))
           (ws (niwa-workspace--create
                :type 'file :id root :root root :name "skill-policy"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           (old-custom (get 'gptel-reasoning-effort 'custom-type))
           (old-effort (get 'fast-model :reasoning-effort))
           effective)
      (unwind-protect
          (progn
            (put 'gptel-reasoning-effort 'custom-type '(choice symbol))
            (put 'fast-model :reasoning-effort '(member low high))
            (niwa-view-test--write-skill
             project-skills "alpha"
             "name: alpha\ndescription: Local alpha\ncontext: inline\nmodel: \"superseded\"\n"
             "LOCAL ALPHA")
            (niwa-view-test--write-skill
             user-skills "alpha"
             "name: alpha\ndescription: Global alpha\ncontext: inline\n"
             "GLOBAL ALPHA")
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--session session
                            niwa--workspace ws
                            gptel-backend (gptel-get-backend "Balanced")
                            gptel-model 'balanced-model
                            gptel-reasoning-effort 'low
                            gptel-prompt-transform-functions
                            (cons
                             #'niwa-skills--transform-apply-request-model-policy
                             gptel-prompt-transform-functions))
                (niwa-skills-install session data-buf))
              (let ((transform
                     (symbol-function
                      'niwa-skills--transform-apply-request-model-policy)))
                (cl-letf
                    (((symbol-function
                       'niwa-skills--transform-apply-request-model-policy)
                      (lambda (fsm)
                        (funcall transform fsm)
                        (setq effective
                              (list (gptel-backend-name gptel-backend)
                                    gptel-model
                                    gptel-reasoning-effort))))
                     ((symbol-function 'gptel-send)
                      (lambda (&rest _)
                        (niwa-view-test--dry-run-request-data))))
                  (with-current-buffer view-buf
                    (niwa-view-composer-insert "$local:alpha inspect"
                                                   (niwa-view-composer-draft-start))
                    (with-current-buffer (niwa-view-composer-buffer)
                      (niwa-view-send)))))
              (should (equal '("Fast" fast-model high) effective))))
        (put 'gptel-reasoning-effort 'custom-type old-custom)
        (put 'fast-model :reasoning-effort old-effort)
        (delete-directory root t)
        (delete-directory user-skills t))))

  :doc "command stacks ignore malformed policy and retain the session policy"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"
         "model: \"invalid-alpha\"\neffort: impossible\n")
        ("beta" "inline" "BETA"
         "model: \"invalid-beta\"\neffort: impossible\n"))
    (let (context sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree niwa-skills--pending-request-context)
                         sent t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$alpha $beta inspect"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should sent)
      (should-not (plist-member context :model))
      (should-not (plist-member context :effort))))

  :doc "embedded instructions ignore malformed policy and retain session policy"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"
         "model: \"invalid-alpha\"\neffort: impossible\n"))
    (let (context sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq context
                         (copy-tree niwa-skills--pending-request-context)
                         sent t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha to inspect"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should sent)
      (should-not (plist-member context :model))
      (should-not (plist-member context :effort))))

  :doc "repeated instruction mentions render twice but prepare once"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "UNIQUE ALPHA BODY"))
    (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha, then $alpha again"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq sent (niwa-tool-render-data-strip sent))
      (should (= 1 (niwa-view-test--count-matches
                    "UNIQUE ALPHA BODY" sent)))
      (should (= 2 (niwa-view-test--count-matches
                    (regexp-quote "[skill:alpha -- attached]") sent)))))

  :doc "command stacks share arguments and -- starts instruction parsing"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA<$ARGUMENTS>")
        ("beta" "inline" "BETA<$ARGUMENTS>")
        ("delta" "inline" "DELTA<$ARGUMENTS>"))
    (let (sent)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$alpha $alpha $beta -- $delta details"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq sent (niwa-tool-render-data-strip sent))
      (should (= 1 (niwa-view-test--count-matches "ALPHA<" sent)))
      (should (= 1 (niwa-view-test--count-matches "BETA<" sent)))
      (should (= 1 (niwa-view-test--count-matches "DELTA<>" sent)))
      (should (= 2 (niwa-view-test--count-matches
                    (regexp-quote
                     "<[skill:delta -- attached] details>") sent)))))

  :doc "only six distinct leading skills become commands"
  (niwa-view-test--with-source-skills
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
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$a $b $c $d $e $f $g rest"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq sent (niwa-tool-render-data-strip sent))
      (dolist (body '("A<" "B<" "C<" "D<" "E<" "F<"))
        (should (string-match-p body sent)))
      (should (string-match-p "G<>" sent))
      (should (= 6 (niwa-view-test--count-matches
                    (regexp-quote "<[skill:g -- attached] rest>") sent)))))

  :doc "a draft typed while a fork skill prepares survives its turn"
  (niwa-view-test--with-source-skills
      '(("forker" "fork" "FORK<$ARGUMENTS>"))
    (let ((prepare (symbol-function 'niwa-skills-plan-prepare))
          (dispatched 0))
      (cl-letf (((symbol-function 'niwa-skills-dispatch-prepared-fork)
                 (lambda (&rest _) (cl-incf dispatched)))
                ;; Real preparation is asynchronous whenever a body carries
                ;; a shell or Elisp injection.
                ((symbol-function 'niwa-skills-plan-prepare)
                 (lambda (&rest args)
                   (run-at-time 0 nil (lambda () (apply prepare args))))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$forker inspect this"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (= 0 dispatched))
          (niwa-view-composer-replace "> quoted\nsecond line")
          (let ((deadline (+ (float-time) 10)))
            (while (and (= 0 dispatched) (< (float-time) deadline))
              (accept-process-output nil 0.01)
              (sit-for 0.01)))
          (should (= 1 dispatched))
          (should (equal "> quoted\nsecond line"
                         (niwa-view-composer-draft-text nil t)))))))

  :doc "leading forks dispatch once while later forks remain instructions"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA<$ARGUMENTS>")
        ("forker" "fork" "FORK<$ARGUMENTS>"))
    (let (fork-prompt sent)
      (cl-letf (((symbol-function 'niwa-skills-dispatch-prepared-fork)
                 (lambda (_prepared _callback &rest keys)
                   (setq fork-prompt (plist-get keys :prompt))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$forker inspect with $alpha"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (should (string-match-p "FORK<inspect with" fork-prompt))
        (should (string-match-p
                 (regexp-quote "[skill:alpha -- attached]") fork-prompt))
        (should-not sent))
      (with-current-buffer data-buf
        (let ((text (niwa-tool-render-data-strip
                     (buffer-string))))
          (should (string-match-p (regexp-quote "$forker inspect with $alpha")
                                  text))
          (should-not (string-match-p "FORK<" text)))
        (goto-char (point-min))
        (search-forward "$forker")
        (should (get-text-property
                 (match-beginning 0) 'niwa-mention-binding))))
    (with-current-buffer data-buf
      (setq-local niwa--current-request nil))
    (with-current-buffer view-buf
      (niwa-view--stop-request-progress)
      (niwa-view--clear-input))
    (let (fork-called sent)
      (cl-letf (((symbol-function 'niwa-skills-dispatch-prepared-fork)
                 (lambda (&rest _) (setq fork-called t)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view--clear-input)
          (niwa-view-composer-insert "$alpha $forker inspect"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (should-not fork-called)
        (should (string-match-p "FORK<>" sent)))))

  :doc "fork dispatch failure consumes context already stored in its turn"
  (niwa-view-test--with-source-skills
      '(("forker" "fork" "FORK<$ARGUMENTS>"))
    (niwa-hooks-record-session-context
     session '(:additional-context ("fork startup context")) 'SessionStart)
    (cl-letf (((symbol-function 'niwa-skills-dispatch-prepared-fork)
               (lambda (&rest _) (error "Fork dispatch failed"))))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "$forker inspect"
                                       (niwa-view-composer-draft-start))
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send))))
    (should-not (niwa-session-hook-context-pending session))
    (with-current-buffer data-buf
      (should (= 1 (niwa-view-test--count-matches
                    "fork startup context"
                    (niwa-tool-render-data-strip
                     (buffer-string)))))))

  :doc "preparation failure preserves the bound draft for retry"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA")
        ("beta" "inline" "BETA"))
    (let ((fail-beta t)
          calls sent first-binding second-binding)
      (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                 (lambda (roots callback &rest _)
                   (setq calls
                         (append calls
                                 (mapcar
                                  (lambda (root)
                                    (niwa-skill-name
                                     (plist-get root :skill)))
                                  roots)))
                   (funcall
                    callback
                    (if (and fail-beta
                             (cl-find "beta" roots
                                      :key (lambda (root)
                                             (niwa-skill-name
                                              (plist-get root :skill)))
                                      :test #'equal))
                        '(:status error :reason blocked :message "blocked")
                      (list
                       :status 'ok
                       :outcomes
                       (mapcar
                        (lambda (_root)
                          '(:status ok :kind instruction :body "ALPHA"
                            :request-context (:invoked-skills nil)))
                        roots)
                       :invoked-skills nil)))))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha and $beta"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (setq first-binding
                (get-text-property 4 'niwa-mention-binding
                                   (niwa-view-composer-draft-text)))
          (should (equal "Use $alpha and $beta"
                         (niwa-view-composer-draft-text nil t)))
          (should-not (niwa-view-history--entries))
          (setq fail-beta nil
                calls nil)
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (setq second-binding
                (get-text-property
                 4 'niwa-mention-binding
                 (car (niwa-view-history--entries))))))
      (should sent)
      (should (equal first-binding second-binding))))

  :doc "mixed locators survive retry, queueing, history, and recall"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let* ((source (niwa-skill-source-file
                    (niwa-session-get-skill session "alpha")))
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
      (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                 (lambda (roots callback &rest _)
                   (funcall
                    callback
                    (if fail
                        '(:status error :reason blocked :message "blocked")
                      (list
                       :status 'ok
                       :outcomes
                       (mapcar
                        (lambda (_root)
                          '(:status ok :kind instruction :body "ALPHA"
                            :request-context (:invoked-skills nil)))
                        roots)
                       :invoked-skills nil)))))
                ((symbol-function
                  'niwa-pending-inputs-schedule-held-input-delivery)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (error "Gptel-send should not run"))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert prompt
                                         (niwa-view-composer-draft-start))
          (dolist (spec specs)
            (niwa-view-composer-test--in-composer view-buf
              (goto-char (niwa-view-composer-draft-start view-buf))
              (search-forward (car spec))
              (niwa-mention-bindings-set
               (match-beginning 0) (match-end 0) (cadr spec))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (equal prompt (niwa-view-composer-draft-text nil t)))
          (should
           (equal expected
                  (mapcar (lambda (range) (plist-get range :binding))
                          (niwa-mention-bindings-ranges
                           (niwa-view-composer-draft-text)))))
          (setq fail nil)
          (with-current-buffer data-buf
            (setq-local niwa--current-request
                        (niwa-request--create :session session)))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up))
          (let ((queued
                 (plist-get
                  (car (niwa-session-pending-follow-ups session))
                  :input))
                (history (car (niwa-view-history--entries))))
            (dolist (text (list queued history))
              (should (equal prompt text))
              (should
               (equal expected
                      (mapcar (lambda (range)
                                (plist-get range :binding))
                              (niwa-mention-bindings-ranges text))))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-history-previous))
          (should
           (equal expected
                  (mapcar (lambda (range) (plist-get range :binding))
                          (niwa-mention-bindings-ranges
                           (niwa-view-composer-draft-text)))))))))

  :doc "submit hooks see the complete inert prompt and added skill text stays literal"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY")
        ("beta" "inline" "BETA BODY"))
    (let (seen sent)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event event-plist callback &rest _)
                   (if (eq event 'UserPromptSubmit)
                       (progn
                         (setq seen (plist-get event-plist :prompt))
                         (funcall callback
                                  (list :updated-input
                                        (concat seen " $beta"))))
                     (funcall callback nil))))
                ((symbol-function 'niwa-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should (string-match-p "ALPHA BODY" seen))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") seen))
      (should (string-match-p (regexp-quote "$beta") sent))
      (should-not (string-match-p "BETA BODY" sent))))

  :doc "submit hook rewrites cannot remove prepared instructions or placeholders"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA BODY"))
    (let (sent)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _event-plist callback &rest _)
                   (funcall callback
                            (and (eq event 'UserPromptSubmit)
                                 '(:updated-input "rewritten prompt")))))
                ((symbol-function 'niwa-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent (niwa-view-test--dry-run-request-data)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq sent (niwa-tool-render-data-strip sent))
      (should (string-match-p "ALPHA BODY" sent))
      (should (string-match-p
               (regexp-quote "[skill:alpha -- attached]") sent))
      (should-not (string-match-p "rewritten prompt" sent))))

  :doc "aborting asynchronous preparation makes its late callback inert"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let (late-callback sent)
      (cl-letf (((symbol-function 'niwa-skills-plan-prepare)
                 (lambda (_plan callback &optional _cancelled-p)
                   (setq late-callback callback)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should niwa-view--pending-skill-submission)
          (niwa-view-abort)
          (funcall late-callback
                   '(:status ok :model-input "late"
                     :request-context nil :prepared-entries nil))
          (should-not niwa-view--pending-skill-submission)
          (should (equal "Use $alpha" (niwa-view-composer-draft-text nil t)))))
      (should-not sent)))

  :doc "buffer death settles asynchronous preparation once without dispatch"
  (dolist (killed-buffer '(view data))
    (niwa-view-test--with-source-skills
        '(("alpha" "inline" "ALPHA"))
      (let ((handle-prepared
             (symbol-function 'niwa-view--handle-prepared-plan))
            late-callback
            token
            (settlements 0)
            (hooks 0)
            (dispatches 0))
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (_roots callback &rest _)
                     (setq late-callback callback)))
                  ((symbol-function 'niwa-view--handle-prepared-plan)
                   (lambda (submission prepared)
                     (cl-incf settlements)
                     (funcall handle-prepared submission prepared)))
                  ((symbol-function 'niwa-hooks-run-event)
                   (lambda (&rest _) (cl-incf hooks)))
                  ((symbol-function 'niwa-skills-dispatch-prepared-fork)
                   (lambda (&rest _) (cl-incf dispatches)))
                  ((symbol-function 'gptel-send)
                   (lambda (&rest _) (cl-incf dispatches))))
          (with-current-buffer view-buf
            (niwa-view-composer-insert "Use $alpha"
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send))
            (setq token niwa-view--pending-skill-submission))
          (should token)
          (kill-buffer (if (eq killed-buffer 'view) view-buf data-buf))
          (should (plist-get token :cancelled))
          (funcall late-callback
                   '(:status ok
                     :outcomes
                     ((:status ok :kind instruction :body "late"
                       :request-context (:invoked-skills nil)))
                     :invoked-skills nil))
          (funcall late-callback
                   '(:status ok
                     :outcomes
                     ((:status ok :kind instruction :body "later"
                       :request-context (:invoked-skills nil)))
                     :invoked-skills nil))
          (should (= 1 settlements))
          (should (= 0 hooks))
          (should (= 0 dispatches)))))))

(niwa-deftest niwa-view-send/plan-model-policy ()
  (niwa-view-composer-test--without-admission ,test)
  (test)
  :doc "Plan root requests use planning policy without changing session policy"
  (niwa-skills-test--with-model-backends
    (niwa-view-test--with-buffers
      (let* ((session (niwa-skills-test--make-session))
             (niwa-model-tiers
              '((fast :provider "Fast:fast-model")
                (balanced :provider "Balanced:balanced-model")))
             (niwa-model-workloads '((planning :tier fast)))
             effective)
        (setf (niwa-session-plan-mode session) t)
        (with-current-buffer data-buf
          (setq-local niwa--session session
                      gptel-backend (gptel-get-backend "Balanced")
                      gptel-model 'balanced-model
                      gptel-prompt-transform-functions
                      (list
                       #'niwa-skills--transform-apply-request-model-policy
                       (lambda (_fsm)
                         (push (list (gptel-backend-name gptel-backend)
                                     gptel-model)
                               effective)))))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _)
                     (niwa-view-test--dry-run-request-data))))
          (with-current-buffer view-buf
            (niwa-view-composer-insert "Plan this change"
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send)))
          (setq niwa-model-workloads '((planning :tier balanced)))
          (with-current-buffer view-buf
            (niwa-view-composer-insert "Revise the plan"
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send)))
          (setf (niwa-session-plan-mode session) nil)
          (setq niwa-model-workloads '((planning :tier fast)))
          (with-current-buffer view-buf
            (niwa-view-composer-insert "Implement normally"
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send))))
        (should (equal '(("Fast" fast-model)
                         ("Balanced" balanced-model)
                         ("Balanced" balanced-model))
                       (nreverse effective)))
        (with-current-buffer data-buf
          (should (equal "Balanced" (gptel-backend-name gptel-backend)))
          (should (eq 'balanced-model gptel-model)))))))

(niwa-deftest niwa-view--forward-input-now ()
  ,test
  (test)
  :doc "folded send puts its summary in the transcript and clears the dock"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((dock (niwa-view-composer-display (selected-window)))
            (draft (mapconcat #'number-to-string (number-sequence 1 8) "\n"))
            (niwa-view-user-input-collapse-line-threshold 3)
            sent)
        (niwa-view-composer-insert draft)
        (should (niwa-view--user-input-fold-p draft))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (setq sent t))))
          (niwa-view--forward-input-now draft :submitted-draft draft))
        (should sent)
        (should (equal "" (niwa-view-composer-draft-text nil t)))
        (should (text-property-any (point-min) (point-max)
                                   'niwa-view-type 'user-input-summary))
        (should (window-live-p dock))
        (should (eq dock (niwa-view-composer-window (selected-window)))))))

  :doc "clears inherited response metadata from forwarded user prompts"
  (niwa-view-test--with-buffers
    (let (sent)
      (with-current-buffer data-buf
        (insert (propertize "Assistant answer.\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t))))
        (with-current-buffer view-buf
          (niwa-view--forward-input-now "On a scale?")))
      (should sent)
      (with-current-buffer data-buf
        (goto-char (point-min))
        (search-forward "On a scale?")
        (let ((pos (line-beginning-position))
              (end (line-end-position)))
          (while (< pos end)
            (should-not (get-text-property pos 'gptel))
            (setq pos (1+ pos)))))))

  :doc "rejects an unready target before mutating the transcript or turn UI"
  (niwa-view-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace
            (niwa-workspace--create
             :type 'project :id "remote"
             :root "/ssh:user@host:/srv/project/" :name "remote"))
           (session
            (niwa-session--create
             :name "main" :workspace workspace :execution-target target
             :working-directory "/ssh:user@host:/srv/project/"))
           (before (with-current-buffer data-buf (buffer-string)))
           sent)
      (setf (niwa-execution-target-readiness target)
            '(:status blocked :reason missing-dependencies
              :missing-dependencies (rg)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "Install rg"
                                       (niwa-view-composer-draft-start)))
      (cl-letf (((symbol-function 'niwa-execution-target-probe) #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&optional _arg)
                   (setq sent t)
                   (niwa-request-begin session))))
        (with-current-buffer view-buf
          (should-error
           (niwa-view--forward-input-now "Install rg")
           :type 'user-error)
          (should (equal "Install rg" (niwa-view-composer-draft-text nil t)))
          (should-not niwa-view--spinner-status)
          (should-not niwa-view--in-flight-turn-start)))
      (should-not sent)
      (with-current-buffer data-buf
        (should (equal before (buffer-string)))
        (setq-local niwa--session nil))
      (with-current-buffer view-buf
        (setq-local niwa--session nil))))

  :doc "stops the turn UI when the send fails after it started"
  (niwa-view-test--with-buffers
    (let* ((workspace
            (niwa-workspace--create
             :type 'project :id "send-failure"
             :root temporary-file-directory :name "send-failure"))
           (session
            (niwa-session--create
             :name "main" :workspace workspace
             :working-directory temporary-file-directory)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "prompt that fails to start"
                                       (niwa-view-composer-draft-start)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   ;; gptel-send already runs in the data buffer, which is
                   ;; where the first WAIT handler begins the request.
                   (let ((fsm (gptel-make-fsm :info (list :buffer data-buf))))
                     (funcall (cadr (assq 'WAIT gptel-send--handlers)) fsm)
                     (plist-put (gptel-fsm-info fsm) :niwa-request
                                (niwa-request-begin session)))
                   (error "Provider refused to start"))))
        (with-current-buffer view-buf
          (should-error
           (niwa-view--forward-input-now "prompt that fails to start")
           :type 'error)
          (should-not niwa-view--in-flight-turn-start)
          (should-not niwa-view--data-turn-start)
          (should-not (timerp niwa-view--spinner-timer))))
      (with-current-buffer data-buf
        (should-not niwa--current-request)
        (should (string-match-p "prompt that fails to start" (buffer-string)))
        ;; The failed start is this turn's only chance to settle, so it
        ;; must leave a retryable terminal record.
        (should (string-match-p ":retry manual" (buffer-string)))
        (should (string-match-p "Provider refused to start" (buffer-string)))
        (setq-local niwa--session nil))
      (with-current-buffer view-buf
        (setq-local niwa--session nil))))

  :doc "attaches pending SessionStart hook context to the submitted prompt"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vf-session-hook"
                :root (niwa-test--placeholder-root)
                :name "vf"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace ws)
        (niwa-hooks-record-session-context
         session
         '(:additional-context ("PONYTAIL MODE ACTIVE - level: full"))
         'SessionStart))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace ws)
        (niwa-view-composer-insert "Hello"
                                       (niwa-view-composer-draft-start)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t)))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra)))
        (with-current-buffer view-buf
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should sent)
          (should-not (niwa-session-hook-context-pending session))
          (let ((text (buffer-string)))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "PONYTAIL MODE ACTIVE" text)))
          (goto-char (point-min))
          (search-forward "hook context added")
          (niwa-view-toggle-section)
          (should (string-match-p "SessionStart" (buffer-string)))
          (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                  (buffer-string)))))
      (with-current-buffer data-buf
        (should (string-match-p "<hook-event name=\"SessionStart\">"
                                (buffer-string)))
        (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                  (buffer-string)))))))

(niwa-deftest niwa-view--transform-model-input ()
  ,test
  (test)
  :doc "uses and clears the data buffer's one-shot model input"
  (let* ((chat-buffer (generate-new-buffer " *niwa-model-input-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local niwa--pending-model-input "derived prompt"))
          (with-temp-buffer
            (insert "stored prompt")
            (niwa-view--transform-model-input fsm)
            (should (equal "derived prompt" (buffer-string)))
            (should-not (plist-member (gptel-fsm-info fsm)
                                      :niwa-model-context))
            (should-not (plist-member (gptel-fsm-info fsm)
                                      :niwa-model-input)))
          (with-current-buffer chat-buffer
            (should-not niwa--pending-model-input)))
      (kill-buffer chat-buffer))))

(niwa-deftest niwa-view--start-fork-skill-turn ()
  ,test
  (test)
  :doc "rejects a late settlement fence before mutating either buffer"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "fork-settlement"))
          (draft "> first line\nsecond line")
          data-before view-before)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--turn-settlements-pending '(settlement))
        (setq data-before (buffer-string)))
      (with-current-buffer view-buf
        (niwa-view-composer-insert draft
                                       (niwa-view-composer-draft-start))
        (setq view-before (buffer-string))
        (should-error
         (niwa-view--start-fork-skill-turn
          "stored" "display" nil draft)
         :type 'user-error)
        (should (equal view-before (buffer-string))))
      (with-current-buffer data-buf
        (should (equal data-before (buffer-string)))))))

(niwa-deftest niwa-view--send-root (:quiet t)
  (niwa-view-composer-test--without-admission ,test)
  (test)
  :doc "external steering prepares skill context and leaves an unrelated draft"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA EXTERNAL STEERING"))
    (let* ((info (list :buffer data-buf :niwa-request-id "external-steer"
                       :backend (gptel--make-openai) :data (list :messages [])
                       :history '(TRET)))
           (fsm (gptel-make-fsm :state 'TOOL :info info))
           (request (niwa-request--create
                     :id "external-steer" :session session :fsm fsm)))
      (with-current-buffer data-buf
        (setq-local niwa--current-request request))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "> unfinished\ncomposer draft")
        (niwa-view--send-root "Use $alpha")
        (should (equal "> unfinished\ncomposer draft"
                       (niwa-view-composer-draft-text nil t))))
      (let ((entry (car (niwa-session-pending-steering session))))
        (should (string-match-p "ALPHA EXTERNAL STEERING"
                                (plist-get entry :model-input)))
        (should (eq 'reserved
                    (niwa-prompt-submission-state
                     (plist-get entry :submission)))))
      (niwa-tools--handle-steering-inject fsm)
      (should-not (niwa-session-pending-steering session))
      (should (string-match-p
               "ALPHA EXTERNAL STEERING"
               (format "%S" (plist-get (plist-get info :data) :messages))))))

  :doc "external steering cannot become an idle turn or slash command"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :authority-mode 'portable :name "main"))
           (request (niwa-request--create
                     :id "steer" :session session
                     :fsm (gptel-make-fsm :state 'TOOL))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "> untouched\ndraft")
        (should-error (niwa-view--send-root "idle") :type 'user-error)
        (with-current-buffer data-buf
          (setq-local niwa--current-request request))
        (should-error (niwa-view--send-root "/goal done") :type 'user-error)
        (should (equal "> untouched\ndraft" (niwa-view-composer-draft-text nil t)))))))

(niwa-deftest niwa-view-send/pending-input (:quiet t)
  (niwa-view-composer-test--without-admission ,test)
  (test)

  :doc "both send keys queue once during settlement before and after request teardown"
  (dolist (engine '(native external torn-down))
    (dolist (send '(niwa-view-send niwa-view-send-follow-up))
      (niwa-view-test--with-buffers
        (let* ((session (niwa-session--create
                         :authority-mode 'portable :name "main"))
               (draft "> 2. **ADR 0091:** may it get that one pointer edit?\nYes")
               (request
                (unless (eq engine 'torn-down)
                  (niwa-request--create
                   :id "finished" :session session
                   :fsm (and (eq engine 'native) (gptel-make-fsm :state 'DONE))
                   :context (and (eq engine 'external)
                                 '(:external-history t :niwa-acp-outcome success))))))
          (with-current-buffer data-buf
            (setq-local niwa--session session
                        niwa--current-request request
                        niwa--turn-settlements-pending '(settlement)))
          (cl-letf (((symbol-function
                      'niwa-pending-inputs-schedule-held-input-delivery)
                     #'ignore)
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _) (ert-fail "Sent before settlement finished"))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert draft
                                             (niwa-view-composer-draft-start))
              (funcall send)
              (should (string-empty-p (niwa-view-composer-draft-text nil t)))
              (should (equal (list draft) (niwa-view-history--entries)))))
          (should-not (niwa-session-pending-steering session))
          (let ((entries (niwa-session-pending-follow-ups session)))
            (should (= 1 (length entries)))
            (should (equal draft (plist-get (car entries) :input))))))))

  :doc "C-c TAB queues independent FIFO follow-ups during an active request"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :authority-mode 'portable :name "main")))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request
                    (niwa-request--create :session session)))
      (cl-letf (((symbol-function
                  'niwa-pending-inputs-schedule-held-input-delivery)
                 #'ignore))
        (with-current-buffer view-buf
          (dolist (text '("first" "second"))
            (niwa-view-composer-insert text
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send-follow-up)))
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))
          (should (equal '("second" "first")
                         (niwa-view-history--entries)))))
      (let ((entries (niwa-session-pending-follow-ups session)))
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
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :authority-mode 'portable :name "main")))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request
                    (niwa-request--create :session session)))
      (cl-letf (((symbol-function
                  'niwa-pending-inputs-schedule-held-input-delivery)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (ert-fail "Newer follow-up bypassed the queue"))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "first"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up)))
        (with-current-buffer data-buf
          (setq-local niwa--current-request nil))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "second"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up))))
      (should
       (equal '("first" "second")
              (mapcar
               (lambda (entry) (plist-get entry :input))
               (niwa-session-pending-follow-ups session))))))

  :doc "C-c RET queues plain steering against the active request"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :authority-mode 'portable :name "main"))
           (request (niwa-request--create
                     :id "request-1" :session session
                     :fsm (gptel-make-fsm :state 'TOOL))))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request request))
      (cl-letf (((symbol-function 'niwa-agent-control-root-waiting-p)
                 (lambda (_session) nil)))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "steer this turn"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))
          (should (equal '("steer this turn")
                         (niwa-view-history--entries)))))
      (let ((entry (car (niwa-session-pending-steering session))))
        (should (equal "steer this turn" (plist-get entry :input)))
        (should (equal "request-1" (plist-get entry :request-id)))
        (should (eq 'steering (plist-get entry :category)))
        (should-not (niwa-session-pending-follow-ups session)))))

  :doc "steering accepted after another keystroke preserves the edited draft"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :authority-mode 'portable :name "main"))
           (request (niwa-request--create
                     :id "request-1" :session session
                     :fsm (gptel-make-fsm :state 'TOOL)))
           accept)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request request))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (if (eq event 'UserPromptSubmit)
                       (setq accept callback)
                     (funcall callback nil)))))
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (goto-char (point-max))
          (insert "steer this turn")
          (niwa-view-send)
          (should accept)
          (should-not (niwa-session-pending-steering session))
          (newline))
        (funcall accept nil))
      (should (equal "steer this turn"
                     (plist-get (car (niwa-session-pending-steering session))
                                :input)))
      (should (equal "steer this turn\n"
                     (niwa-view-composer-draft-text view-buf t)))))

  :doc "ordinary steering binds a file and reads its latest contents at delivery"
  (let* ((root (make-temp-file "niwa-steering-file-" t))
         (file (file-name-concat root "target.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "steering-file"
           :file-cache
           (niwa-file-cache--create
            :table (make-hash-table :test #'equal)
            :order nil :total-bytes 0)))
         (session (niwa-session-create "main" workspace))
         (backend
          (gptel-make-openai
           "Steering file" :stream nil :key "unused"
           :host "example.test" :models '(steering-file-test)))
         (data (list :messages []))
         fsm request)
    (unwind-protect
        (progn
          (with-temp-file file (insert "FILE CONTENT V1\n"))
          (niwa-view-test--with-buffers
            (setq fsm
                  (gptel-make-fsm
                   :state 'TOOL
                   :info (list :buffer data-buf :backend backend :data data
                               :position (with-current-buffer data-buf (point-marker))
                               :history '(TRET)
                               :niwa-request-id "request-file")))
            (setq request
                  (niwa-request--create
                   :id "request-file" :session session :fsm fsm))
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace workspace
                          niwa--current-request request))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Inspect @file:target.txt"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (let* ((entry
                    (car (niwa-session-pending-steering session)))
                   (input (plist-get entry :input))
                   (start (string-match "@file:" input)))
              (should
               (equal file
                      (plist-get
                       (get-text-property
                        start 'niwa-mention-binding input)
                       :path)))
              (with-temp-file file (insert "FILE CONTENT V2\n"))
              (niwa-tools--handle-steering-inject fsm)
              ;; Mention contents ride the reminder injector, which
              ;; runs next in the same WAIT.
              (niwa-reminders--handle-inject fsm)
              (should-not (niwa-session-pending-steering session))
              (should
               (eq 'committed
                   (niwa-prompt-submission-state
                    (plist-get entry :submission)))))
            (let ((payload (format "%S" (plist-get data :messages))))
              (should (string-match-p "FILE CONTENT V2" payload))
              (should-not (string-match-p "FILE CONTENT V1" payload)))
            (with-current-buffer data-buf
              (should (string-match-p
                       (regexp-quote "@file:target.txt")
                       (buffer-string))))))
      (delete-directory root t)))

  :doc "ordinary steering delivers an image mention inside the same turn"
  (let* ((root (make-temp-file "niwa-steering-image-" t))
         (file (file-name-concat root "board.png"))
         (model (make-symbol "steering-image-test"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "steering-image"
           :file-cache
           (niwa-file-cache--create
            :table (make-hash-table :test #'equal)
            :order nil :total-bytes 0)))
         (session (niwa-session-create "main" workspace))
         (backend
          (gptel-make-openai
           "Steering image" :stream nil :key "unused"
           :host "example.test" :models (list model)))
         (data (list :messages []))
         fsm request)
    (put model :capabilities '(media))
    (put model :mime-types '("image/png"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "PIXELS"))
          (niwa-view-test--with-buffers
            (setq fsm
                  (gptel-make-fsm
                   :state 'TOOL
                   :info (list :buffer data-buf :backend backend :data data
                               :model model
                               :position (with-current-buffer data-buf (point-marker))
                               :history '(TRET)
                               :niwa-request-id "request-image")))
            (setq request
                  (niwa-request--create
                   :id "request-image" :session session :fsm fsm))
            (with-current-buffer data-buf
              (setq-local niwa--session session
                          niwa--workspace workspace
                          niwa--current-request request
                          gptel-model model))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Compare @file:board.png"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should (equal "" (niwa-view-composer-draft-text nil t))))
            (should (= 1 (length (niwa-session-pending-steering session))))
            (niwa-tools--handle-steering-inject fsm)
            (niwa-reminders--handle-inject fsm)
            (should-not (niwa-session-pending-steering session))
            (let ((content (plist-get (aref (plist-get data :messages) 0) :content)))
              (should (equal "image_url" (plist-get (aref content 0) :type)))
              (should (string-suffix-p
                       (base64-encode-string "PIXELS" t)
                       (plist-get (plist-get (aref content 0) :image_url) :url))))
            (with-current-buffer data-buf
              (should (string-match-p (regexp-quote "@file:board.png") (buffer-string)))
              (should-not (local-variable-p 'gptel-context)))))
      (delete-directory root t)))

  :doc "ordinary steering accepts an inline skill and prompt hook context"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA ORDINARY STEERING"))
    (let* ((niwa-hook-rules
            '((UserPromptSubmit
               ((:matcher "*"
                          :hooks
                          ((:type elisp
                                  :function
                                  niwa-view-test--add-prompt-hook-context)))))))
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
                         :niwa-request-id "request-prepared")))
           (request
            (niwa-request--create
             :id "request-prepared" :session session :fsm fsm)))
      (with-current-buffer data-buf
        (setq-local niwa--current-request request))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "Use $alpha"
                                       (niwa-view-composer-draft-start))
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send)))
      (let ((entry (car (niwa-session-pending-steering session))))
        (should (string-match-p
                 "ALPHA ORDINARY STEERING"
                 (plist-get entry :model-input)))
        (should (string-match-p
                 "ordinary steering hook context"
                 (plist-get entry :model-input))))
      (niwa-tools--handle-steering-inject fsm)
      (let ((payload (format "%S" (plist-get data :messages))))
        (should (string-match-p "ALPHA ORDINARY STEERING" payload))
        (should (string-match-p
                 "ordinary steering hook context" payload)))))

  :doc "a lost ordinary steering race restores the exact composer state"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :authority-mode 'portable :name "steering-race"))
           (fsm (gptel-make-fsm :state 'TOOL))
           (request
            (niwa-request--create
             :id "request-race" :session session :fsm fsm))
           (grant "/tmp/steering-race.txt")
           snapshot point-offset notice)
      (setf (niwa-session-dropped-file-grants session) (list grant))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request request))
      (cl-letf
          (((symbol-function 'niwa-hooks-run-event)
            (lambda (_event _payload callback &rest _)
              (with-current-buffer data-buf
                (setq-local niwa--current-request nil))
              (funcall callback nil)))
           ((symbol-function 'message)
            (lambda (format-string &rest args)
              (setq notice (apply #'format format-string args)))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert (propertize "race prompt" 'test-property 'preserved)
                                         (niwa-view-composer-draft-start))
          (niwa-view-composer-test--in-composer view-buf
            (goto-char (+ (niwa-view-composer-draft-start view-buf) 4)))
          (setq snapshot
                (niwa-view-composer-draft-text)
                point-offset (niwa-view-test--composer-point-offset))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should
           (equal-including-properties
            snapshot
            (niwa-view-composer-draft-text)))
          (should (= point-offset
                     (niwa-view-test--composer-point-offset)))))
      (should (equal (list grant)
                     (niwa-session-dropped-file-grants session)))
      (should-not (niwa-session-pending-steering session))
      (should-not (niwa-session-pending-follow-ups session))
      (should (string-match-p "C-c TAB" notice))))

  :doc "Here Goal reservation queues post-acceptance input under its identity"
  (niwa-view-test--with-buffers
    (let ((session
           (niwa-session--create
            :authority-mode 'portable
            :name "handoff"
            :plan-metadata
            '(:implementation-retry
              (:goal-id "reserved"
               :selection (:location here :execution goal))))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (ert-fail "Replaced reserved kickoff")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "steer after kickoff"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up)))
        (let ((entry (car (niwa-session-pending-follow-ups session))))
          (should (equal "steer after kickoff" (plist-get entry :input)))
          (should (equal "reserved" (plist-get entry :queued-at-goal-id)))))))

  :doc "C-c TAB at Plan approval queues without demoting the proposal"
  (niwa-view-test--with-buffers
    (let* ((metadata '(:status proposed :proposal-id (1 2 "hash")))
           (session
            (niwa-session--create
             :authority-mode 'portable
             :name "plan" :plan-mode t :plan-metadata metadata)))
      (setf (niwa-session-pending-plan-approval session)
            (list :session session :callback #'ignore :renderer #'ignore))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "after approval"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up))))
      (should (niwa-session-pending-plan-approval session))
      (should (eq 'proposed
                  (plist-get (niwa-session-plan-metadata session)
                             :status)))
      (should (equal "after approval"
                     (plist-get
                      (car (niwa-session-pending-follow-ups session))
                      :input)))))

  :doc "special root workflows reject steering but accept a follow-up"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :authority-mode 'portable :name "review"))
           (request
            (niwa-request--create
             :id "review-request" :session session :origin "/root")))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--current-request request))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "later work"
                                       (niwa-view-composer-draft-start))
        (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)
        (should (equal "later work" (niwa-view-composer-draft-text nil t)))
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send-follow-up))
        (should (string-empty-p (niwa-view-composer-draft-text nil t)))
        (should (equal "later work"
                       (plist-get
                        (car (niwa-session-pending-follow-ups session))
                        :input))))))

  :doc "plain input during WaitAgent is injected before its resumed sample"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           (fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :buffer data-buf
                             :niwa-request-id "wait-plain")))
           send-called
           wake-reason)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create
                     :id "wait-plain" :session session :fsm fsm)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (niwa-agent-control-wait
         session (lambda (reason) (setq wake-reason reason)) 10000)
        (with-current-buffer view-buf
          (niwa-view-composer-insert "follow up"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should-not send-called)
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))
          (should (equal '("follow up")
                         (niwa-view-history--entries)))
          (should (string-match-p
                   "1 input pending"
                   (niwa-view--interaction-count-label)))))
      (should (eq 'user wake-reason))
      (should-not (niwa-session-pending-follow-ups session))
      (should-not (niwa-session-messages session))
      (should (equal "follow up"
                     (plist-get
                      (car (niwa-session-pending-steering session))
                      :input)))))

  :doc "WaitAgent steering honors UserPromptSubmit rewrites and context"
  (let* ((root (make-temp-file "niwa-wait-steering-hook-" t))
         (workspace (niwa-workspace-get-or-create
                     'project "wait-steering-hook" root "wait-steering-hook"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks
                        ((:type elisp
                                :function
                                niwa-view-test--rewrite-prompt-hook-with-context)))))))
         (fsm (gptel-make-fsm
               :state 'TOOL
               :info (list :niwa-request-id "wait-hook")))
         wake-reason)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (setq-local niwa--current-request
                        (niwa-request--create
                         :id "wait-hook" :session session :fsm fsm))
            (niwa-session-set-root-buffer session data-buf))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _) 'fake-timer)))
            (niwa-agent-control-wait
             session (lambda (reason) (setq wake-reason reason)) 10000)
            (with-current-buffer view-buf
              (niwa-view-composer-insert "original steering"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should (string-empty-p (niwa-view-composer-draft-text nil t)))))
          (should (eq 'user wake-reason))
          (should (equal "original steering"
                         niwa-view-test--seen-prompt))
          (let* ((entry (car (niwa-session-pending-steering session)))
                 (payload (plist-get entry :model-input)))
            (should (string-match-p "rewritten prompt" payload))
            (should (string-match-p "hook policy context" payload))
            (should-not (string-match-p "original steering" payload))
            (should (plist-get entry :hook-audits))))
      (delete-directory root t)))

  :doc "WaitAgent steering consumes inline skill text and transcript metadata"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA STEERING BODY"))
    (let* ((fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :niwa-request-id "wait-skill")))
           wake-reason)
      (with-current-buffer data-buf
        (setq-local niwa--current-request
                    (niwa-request--create
                     :id "wait-skill" :session session :fsm fsm)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (error "WaitAgent steering must not start a request"))))
        (niwa-agent-control-wait
         session (lambda (reason) (setq wake-reason reason)) 10000)
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Use $alpha"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (should (eq 'user wake-reason))
      (let ((message (car (niwa-session-pending-steering session))))
        (should (string-match-p "ALPHA STEERING BODY"
                                (plist-get message :model-input)))
        (should (string-match-p "Use \\$alpha"
                                (plist-get message :transcript-payload)))
        (should (string-match-p "niwa-render-data"
                                (plist-get message :transcript-payload))))
      (should-not (niwa-session-invoked-skills session))))

  :doc "a WaitAgent ending during preparation leaves steering queued"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "steering-race" :root (niwa-test--placeholder-root)
                       :name "steering-race"))
           (session (niwa-session-create "main" workspace))
           (fsm (gptel-make-fsm
                 :state 'TOOL
                 :info (list :niwa-request-id "wait-race")))
           (waiting-checks 0)
           (hook-calls 0))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace
                    niwa--current-request
                    (niwa-request--create
                     :id "wait-race" :session session :fsm fsm))
        (niwa-hooks-record-session-context
         session '(:additional-context ("race context")) 'SessionStart))
      (niwa-session-set-pending-inputs
       session 'follow-up
       (list (list :input "older prompt" :queued-at-turn 0)))
      (cl-letf (((symbol-function 'niwa-agent-control-root-waiting-p)
                 (lambda (_session)
                   (= 1 (cl-incf waiting-checks))))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (cl-incf hook-calls)
                   (funcall callback nil)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'fake-timer)))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "race prompt"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (should (= 1 (length
                      (niwa-session-pending-follow-ups session))))
        (with-current-buffer view-buf
          (should (string-empty-p (niwa-view-composer-draft-text nil t)))
          (should (equal '("race prompt")
                         (niwa-view-history--entries))))
        (should (equal "race prompt"
                       (plist-get
                        (car (niwa-session-pending-steering session))
                        :input)))
        (should (= 1 hook-calls))
        (should-not (niwa-session-hook-context-pending session)))))

  :doc "a prepared queue entry survives failure before transcript insertion"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "prepared-retry" :root (niwa-test--placeholder-root)
                       :name "prepared-retry"))
           (entries '((:event SessionStart :body "reserved retry context")))
           (session (niwa-session-create "main" workspace))
           (submission
            (niwa-prompt-submission-create
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
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (niwa-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'niwa-view--forward-input-now)
                 (lambda (&rest _) (error "Before transcript"))))
        (niwa-view--drain-follow-up data-buf))
      (should (eq entry (car (niwa-session-pending-follow-ups session))))
      (should (eq 'reserved (niwa-prompt-submission-state submission)))
      (should-not (niwa-session-hook-context-pending session))
      (cl-letf (((symbol-function 'gptel-send) #'ignore))
        (niwa-view--drain-follow-up data-buf))
      (should-not (niwa-session-pending-follow-ups session))
      (should (eq 'committed (niwa-prompt-submission-state submission)))
      (with-current-buffer data-buf
        (should (= 1 (niwa-view-test--count-matches
                      "reserved retry context" (buffer-string)))))))

  :doc "an undelivered follow-up leaves no active file grant behind"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "grant-retry" :root (niwa-test--placeholder-root)
                       :name "grant-retry"))
           (session (niwa-session-create "main" workspace))
           (dropped (expand-file-name "dropped.txt" "/tmp"))
           (submission
            (niwa-prompt-submission-create
             :input "granted prompt" :display-text "granted prompt"
             :session session :state 'reserved
             :outcome '(:model-input "granted prompt"
                        :transcript-input "granted prompt"
                        :hook-input "granted prompt")))
           (entry (list :input "granted prompt" :submission submission
                        :dropped-file-grants (list dropped))))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (niwa-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'niwa-view--forward-input-now)
                 (lambda (&rest _) (error "Before transcript"))))
        (niwa-view--drain-follow-up data-buf))
      ;; The entry stays pending, so the grant it activated to expand its
      ;; own mentions must not outlive the attempt.
      (should (eq entry (car (niwa-session-pending-follow-ups session))))
      (should-not (niwa-session-active-dropped-file-grants session))))

  :doc "a plain follow-up failing before insertion gives its grant back"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "grant-plain" :root (niwa-test--placeholder-root)
                       :name "grant-plain"))
           (session (niwa-session-create "main" workspace))
           (dropped (expand-file-name "dropped.txt" "/tmp"))
           ;; No :submission and no :scope, which is what
           ;; `niwa-view--queue-follow-up' actually queues.
           (entry (list :input "plain prompt"
                        :dropped-file-grants (list dropped))))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (niwa-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'niwa-view--forward-input-now)
                 (lambda (&rest _) (error "Target not ready"))))
        (should-error (niwa-view--drain-follow-up data-buf)))
      (should (eq entry (car (niwa-session-pending-follow-ups session))))
      (should-not (niwa-session-active-dropped-file-grants session))
      (with-current-buffer view-buf
        (should-not niwa-view--pending-guest-attribution))))

  :doc "a follow-up that fails after insertion keeps its file grant"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "grant-kept" :root (niwa-test--placeholder-root)
                       :name "grant-kept"))
           (session (niwa-session-create "main" workspace))
           (dropped (expand-file-name "kept.txt" "/tmp"))
           (submission
            (niwa-prompt-submission-create
             :input "kept prompt" :display-text "kept prompt"
             :session session :state 'reserved
             :outcome '(:model-input "kept prompt"
                        :transcript-input "kept prompt"
                        :hook-input "kept prompt")))
           (entry (list :input "kept prompt" :submission submission
                        :dropped-file-grants (list dropped))))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (niwa-session-set-pending-inputs session 'follow-up (list entry))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (error "Send failed"))))
        (niwa-view--drain-follow-up data-buf))
      ;; The prompt was inserted and the entry dequeued, so the grant the
      ;; model needs stays active even though the send failed.
      (should-not (niwa-session-pending-follow-ups session))
      (should
       (equal (list dropped)
              (niwa-session-active-dropped-file-grants session)))))

  :doc "queued direct note keeps its UUID despite injected number reuse"
  (let* ((root (make-temp-file "niwa-ref-queue-" t))
         (file (file-name-concat root "reference.txt"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "ref-queue"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         ref-buf ref replacement request-data warning)
    (unwind-protect
        (progn
          (with-temp-file file (insert "original reference body\n"))
          (setq ref-buf (find-file-noselect file))
          (with-current-buffer ref-buf
            (setq-local niwa--workspace ws)
            (setq ref (niwa-instruction-test--create-note-in
                       ref-buf (point-min) (1- (point-max)))))
          (let* ((id (niwa--instruction-id ref))
                 (uuid (overlay-get ref 'niwa-uuid))
                 (token (format "@ref:%d" id)))
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--session session
                            niwa--workspace ws
                            niwa--current-request
                            (niwa-request--create :session session)))
              (with-current-buffer view-buf
                (setq-local niwa--session session))
              (cl-letf (((symbol-function
                          'niwa-pending-inputs-schedule-held-input-delivery)
                         #'ignore))
                (with-current-buffer view-buf
                  (niwa-view-composer-insert
                   (concat "Inspect " token " after the current turn")
                   (niwa-view-composer-draft-start))
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send-follow-up))))
              (let* ((queued
                      (plist-get
                       (car (niwa-session-pending-follow-ups session))
                       :input))
                     (queued-start (string-match (regexp-quote token) queued))
                     (history
                      (with-current-buffer view-buf
                        (car (niwa-view-history--entries))))
                     (history-start
                      (string-match (regexp-quote token) history)))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 queued-start 'niwa-mention-binding queued)
                                :reference-uuid)))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 history-start 'niwa-mention-binding history)
                                :reference-uuid))))
              (niwa--delete-instruction ref ref-buf t)
              (with-current-buffer ref-buf
                (should-not (memq id (niwa--instruction-retired-ids)))
                ;; Force hostile number reuse to exercise UUID binding, not allocation.
                (push id (niwa--instruction-retired-ids))
                (erase-buffer)
                (insert "replacement reference body\n")
                (setq replacement
                      (niwa-instruction-test--create-note-in
                       ref-buf (point-min) (1- (point-max)))))
              (should (= id (niwa--instruction-id replacement)))
              (with-current-buffer data-buf
                (setq-local niwa--current-request nil))
              (let ((gptel-prompt-transform-functions
                     (cons #'niwa--transform-expand-mentions
                           (remove #'niwa--transform-expand-mentions
                                   gptel-prompt-transform-functions))))
                (cl-letf (((symbol-function 'message)
                           (lambda (format-string &rest args)
                             (let ((text (apply #'format format-string args)))
                               (when (string-prefix-p (format "niwa [%s]: note" root) text)
                                 (setq warning text)))))
                          ((symbol-function 'gptel-send)
                           (lambda (&rest _)
                             (setq request-data
                                   (niwa-view-test--dry-run-request-data)))))
                  (niwa-view--drain-follow-up data-buf)))
              (should (string-search (format "[note:%d -- unavailable]" id)
                                     request-data))
              (should-not (string-search "replacement reference body"
                                         request-data))
              (should (string-match-p
                       (concat (regexp-quote (format "niwa [%s]: note " root))
                               ".* unavailable")
                       warning))
              (with-current-buffer data-buf
                (goto-char (point-min))
                (should (search-forward token nil t))
                (should (equal uuid
                               (plist-get
                                (get-text-property
                                 (match-beginning 0)
                                 'niwa-mention-binding)
                                :reference-uuid))))
              (should-not (niwa-session-pending-follow-ups session)))))
      (when (buffer-live-p ref-buf)
        (with-current-buffer ref-buf (set-buffer-modified-p nil))
        (kill-buffer ref-buf))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "queued file keeps its pathname and warns softly if deleted"
  (let* ((root (make-temp-file "niwa-file-queue-" t))
         (file (file-name-concat root "queued.txt"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "file-queue"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         request-data warning)
    (with-temp-file file (insert "queued file secret\n"))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session
                        niwa--workspace ws
                        niwa--current-request
                        (niwa-request--create :session session)))
          (with-current-buffer view-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function
                      'niwa-pending-inputs-schedule-held-input-delivery)
                     #'ignore))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "Inspect @file:queued.txt"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send-follow-up))))
          (let* ((queued
                  (plist-get
                   (car (niwa-session-pending-follow-ups session))
                   :input))
                 (queued-start (string-match "@file:" queued))
                 (history
                  (with-current-buffer view-buf
                    (car (niwa-view-history--entries))))
                 (history-start (string-match "@file:" history)))
            (should (equal file
                           (plist-get
                            (get-text-property
                             queued-start 'niwa-mention-binding queued)
                            :path)))
            (should (equal file
                           (plist-get
                            (get-text-property
                             history-start 'niwa-mention-binding history)
                            :path))))
          (delete-file file)
          (with-current-buffer data-buf
            (setq-local niwa--current-request nil))
          (let ((gptel-prompt-transform-functions
                 (cons #'niwa--transform-expand-mentions
                       (remove #'niwa--transform-expand-mentions
                               gptel-prompt-transform-functions))))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (let ((text (apply #'format format-string args)))
                           (when (string-prefix-p (format "niwa [%s]: file" root) text)
                             (setq warning text)))))
                      ((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (setq request-data
                               (niwa-view-test--dry-run-request-data)))))
              (niwa-view--drain-follow-up data-buf)))
          (should (string-search
                   "[file:queued.txt -- does not exist]" request-data))
          (should-not (string-search "queued file secret" request-data))
          (should (string-match-p
                   (concat (regexp-quote (format "niwa [%s]: file " root))
                           ".* unavailable")
                   warning))
          (with-current-buffer data-buf
            (goto-char (point-min))
            (should (search-forward "@file:queued.txt" nil t))
            (should (equal file
                           (plist-get
                            (get-text-property
                             (match-beginning 0) 'niwa-mention-binding)
                            :path))))
          (should-not (niwa-session-pending-follow-ups session)))
      (when (file-exists-p file) (delete-file file))
      (delete-directory root t)))

  :doc "queued MCP mention keeps its locator and reads after reconnect"
  (let* ((root (make-temp-file "niwa-mcp-queue-" t))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "mcp-queue"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (token "@mcp:docs:file:///guide")
         (connections (make-hash-table :test #'equal))
         (connection (make-symbol "mcp-connection"))
         request-data)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session
                        niwa--workspace ws
                        niwa--current-request
                        (niwa-request--create :session session)))
          (with-current-buffer view-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function
                      'niwa-pending-inputs-schedule-held-input-delivery)
                     #'ignore))
            (with-current-buffer view-buf
              (niwa-view-composer-insert
               (concat "Consult " token)
               (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send-follow-up)))
            (let* ((queued
                    (plist-get
                     (car (niwa-session-pending-follow-ups session))
                     :input))
                   (queued-start (string-match (regexp-quote token) queued))
                   (history
                    (with-current-buffer view-buf
                      (car (niwa-view-history--entries))))
                   (history-start
                    (string-match (regexp-quote token) history)))
              (dolist (entry (list (cons queued queued-start)
                                   (cons history history-start)))
                (should
                 (equal '(:kind mcp :token "@mcp:docs:file:///guide"
                          :server "docs" :uri "file:///guide")
                        (get-text-property
                         (cdr entry) 'niwa-mention-binding
                         (car entry)))))))
          (puthash "docs" connection connections)
          (with-current-buffer data-buf
            (setq-local niwa--current-request nil))
          (let ((gptel-prompt-transform-functions
                 (cons #'niwa--transform-expand-mentions
                       (remove #'niwa--transform-expand-mentions
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
                               (niwa-view-test--dry-run-request-data)))))
              (niwa-view--drain-follow-up data-buf)))
          (should (string-search "current guide" request-data))
          (with-current-buffer data-buf
            (goto-char (point-min))
            (should (search-forward token nil t))
            (should
             (equal "file:///guide"
                    (plist-get
                     (get-text-property
                      (match-beginning 0) 'niwa-mention-binding)
                     :uri))))
          (should-not (niwa-session-pending-follow-ups session)))
      (delete-directory root t)))

  :doc "queued follow-up stays visible across incremental in-flight rendering"
  (let ((niwa-permission-mode 'ask))
    (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-incremental"
                :root (niwa-test--placeholder-root)
                :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           data-turn-start)
      (setf (niwa-session-permission-mode session) 'ask)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create :session session))
        (insert "*** Prompt\n")
        (setq data-turn-start (copy-marker (point-max) nil))
        (let ((start (point)))
          (insert "First partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start data-turn-start)
        (setq niwa-view--in-flight-turn-start
              (niwa-view--insert-user-message "Prompt"))
        (niwa-view-render-live-update data-buf)
        (niwa-view-composer-insert "follow up"
                                       (niwa-view-composer-draft-start))
        (with-current-buffer (niwa-view-composer-buffer)
          (niwa-view-send-follow-up))
        (should (string-match-p "follow up"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (with-current-buffer data-buf
          (let ((start (point)))
            (insert "Second partial response.\n")
            (put-text-property start (point) 'gptel 'response)))
        (niwa-view-render-live-update data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Second partial response" text)))
          (should queued)
          (should partial)
          (should-not (string-match-p "\n> " text))
          (should (< partial queued)))))))

  :doc "queued follow-up stays visible across in-flight full rerender"
  (let ((niwa-permission-mode 'ask))
    (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-full-rerender"
                :root (niwa-test--placeholder-root)
                :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws)))
      (setf (niwa-session-permission-mode session) 'ask)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create :session session))
        (insert "*** Prompt\n")
        (let ((start (point)))
          (insert "Partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "follow up"
                        :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min) nil)))
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail-marker) nil))
        (niwa-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Partial response" text)))
          (should queued)
          (should partial)
          (should-not (string-match-p "\n> " text))
          (should (< partial queued)))))))

  :doc "pending-input UI names queue management, its pause effect and key hint"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-hint"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "follow up" :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view--interaction-rebuild)
        (should (string-search "Manage queue… (pauses delivery; C-c C-e)"
                               (buffer-string))))))

  :doc "queued skill hooks run once at dispatch and not while queueing"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let ((hooks 0)
          (sends 0))
      (with-current-buffer data-buf
        (setq-local niwa--current-request
                    (niwa-request--create :session session)))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _event-plist callback &rest _)
                   (when (eq event 'UserPromptSubmit)
                     (cl-incf hooks))
                   (funcall callback nil)))
                ((symbol-function
                  'niwa-pending-inputs-schedule-held-input-delivery)
                 #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sends))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Queue $alpha exactly"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up)))
        (should (= 0 hooks))
        (should (= 0 sends))
        (let* ((input (plist-get
                       (car (niwa-session-pending-follow-ups session))
                       :input))
               (start (string-match "\\$alpha" input)))
          (should (plist-get
                   (get-text-property start 'niwa-mention-binding input)
                   :source-file)))
        (with-current-buffer data-buf
          (setq-local niwa--current-request nil))
        (niwa-view--drain-follow-up data-buf)
        (should (= 1 hooks))
        (should (= 1 sends))
        (should-not (niwa-session-pending-follow-ups session)))))

  :doc "an unavailable queued binding annotates, sends, and leaves the queue"
  (niwa-view-test--with-source-skills
      '(("alpha" "inline" "ALPHA"))
    (let (sent original)
      (with-current-buffer data-buf
        (setq-local niwa--current-request
                    (niwa-request--create :session session)))
      (cl-letf (((symbol-function
                  'niwa-pending-inputs-schedule-held-input-delivery)
                 #'ignore))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "Queue $alpha exactly"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send-follow-up))))
      (setq original (car (niwa-session-pending-follow-ups session)))
      (delete-file
       (plist-get
        (get-text-property
         (string-match "\\$alpha" (plist-get original :input))
         'niwa-mention-binding (plist-get original :input))
        :source-file))
      (with-current-buffer data-buf
        (setq-local niwa--current-request nil))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (niwa-view--drain-follow-up data-buf))
      (should sent)
      (should-not (niwa-session-pending-follow-ups session))))

  :doc "fallback drain preserves queued entries while plan approval is pending"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-fallback-plan"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace ws)
        (setq-local niwa--current-request nil))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (setf (niwa-session-pending-plan-approval session)
            (list :body "# Plan" :origin "main"))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (niwa-view--drain-follow-up data-buf))
      (should-not sent)
      (should (= 1 (length (niwa-session-pending-follow-ups session))))))

  :doc "held-input scheduler uses data buffer after request cleanup"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-hook-late-schedule"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           drain-buffer)
      (niwa-session-set-pending-inputs
       session 'follow-up (list (list :input "queued")))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace ws)
        (setq-local niwa--current-request nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat _function &rest args)
                   (setq drain-buffer (car args))
                   'timer)))
        (with-current-buffer view-buf
          (niwa-pending-inputs-schedule-held-input-delivery)))
      (should (eq drain-buffer data-buf))))

  :doc "interaction rebuild preserves composer point while drafting"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-point"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "already queued"
                        :display-text "already queued")))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-composer-insert "draft"
                                       (niwa-view-composer-draft-start))
        (niwa-view-composer-test--in-composer view-buf
          (goto-char (+ (niwa-view-composer-draft-start view-buf) 2)))
        (niwa-view--interaction-rebuild)
        (should (string= "draft" (niwa-view-composer-draft-text nil t)))
        (should (= 2 (niwa-view-test--composer-point-offset))))))

  :doc "spinner status redraw preserves composer point while drafting"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (niwa-view-composer-insert draft
                                       (niwa-view-composer-draft-start))
        (niwa-view-composer-test--in-composer view-buf
          (goto-char (+ (niwa-view-composer-draft-start view-buf) 4)))
        (niwa-view--start-spinner "Thinking...")
        (should (= 4 (niwa-view-test--composer-point-offset)))
        (niwa-view--update-spinner "Calling Read...")
        (should (= 4 (niwa-view-test--composer-point-offset)))
        (niwa-view--stop-spinner)
        (should (= 4 (niwa-view-test--composer-point-offset)))
        (should (string= draft (niwa-view-composer-draft-text nil t))))))

  :doc "full rerender preserves composer when the status and interaction markers drift"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (niwa-view-test--insert-data data-buf "*** Prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-composer-insert draft
                                       (niwa-view-composer-draft-start))
        (niwa-view-composer-test--in-composer view-buf
          (goto-char (+ (niwa-view-composer-draft-start) 4)))
        (set-marker niwa-view--status-marker (point-max))
        (set-marker niwa-view--interaction-marker (point-max))
        (niwa-view--full-rerender)
        (niwa-view-refresh-input-prompt)
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-test--composer-point-offset view-buf))))))

  :doc "slash input during an active request is rejected"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-slash"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create :session session)))
      (with-current-buffer view-buf
        (niwa-view-composer-insert "/review"
                                       (niwa-view-composer-draft-start))
        (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)
        (should-not (niwa-session-pending-follow-ups session))
        (should (string= "/review" (niwa-view-composer-draft-text nil t))))))

  :doc "fallback drain submits one exact FIFO entry per turn"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-fifo"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           (sent 0))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'niwa-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sent))))
        (niwa-view--drain-follow-up data-buf)
        (should (= 1 sent))
        (should (equal "second"
                       (plist-get
                        (car (niwa-session-pending-follow-ups session))
                        :input)))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "first" text))
            (should-not (string-match-p "second" text)))))))

  :doc "queued follow-ups do not drain while the request is still active"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-active"
                :root (niwa-test--placeholder-root) :name "vq"
                :file-cache (niwa-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (niwa-session-create "main" ws))
           (sent nil))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--current-request
                    (niwa-request--create :session session)))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "pending" :display-text "pending")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (niwa-view--drain-follow-up data-buf)
        (should-not sent)
        (should (niwa-session-pending-follow-ups session)))))

  :doc "clearing prepared follow-ups restores their reserved context"
  (niwa-view-test--with-buffers
    (let* ((ws (niwa-workspace--create
                :type 'file :id "vq-clear"
                :root (niwa-test--placeholder-root) :name "vq"))
           (session (niwa-session-create "main" ws))
           (context-entries '((:event SessionStart :body "clear context")))
           (submission
            (niwa-prompt-submission-create
             :input "prepared" :display-text "prepared" :session session
             :context-entries context-entries :state 'reserved)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (setf (niwa-session-pending-follow-ups session)
            (list (list :input "prepared" :submission submission)))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (niwa-pending-inputs-clear)))
      (should-not (niwa-session-pending-follow-ups session))
      (should (equal context-entries
                     (niwa-session-hook-context-pending session))))))

(niwa-deftest niwa-view--send-local-goal ()
  ,test
  (test)
  :doc "applies prompt-hook context and starts the Goal in the data buffer"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :authority-mode 'portable :name "main"))
          started
          started-buffer)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'niwa-view--run-prompt-submit-hook)
                 (lambda (_args _input callback)
                   (funcall
                    callback
                    (niwa-prompt-submission-create
                     :input "expanded" :display-text "/goal draft"
                     :context "hook context" :session session))))
                ((symbol-function 'niwa-view-history-add) #'ignore)
                ((symbol-function 'niwa-view--clear-input) #'ignore)
                ((symbol-function 'niwa-goal-start)
                 (lambda (objective submission)
                   (setq started
                         (list objective
                               (niwa-prompt-submission-context submission))
                         started-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-view--send-local-goal "/goal draft" "draft")))
      (should (eq data-buf started-buffer))
      (should (equal '("expanded" "hook context") started))))
  :doc "treats the former auto selector as ordinary objective text"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :authority-mode 'portable :name "main"))
          started)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'niwa-view--run-prompt-submit-hook)
                (lambda (objective _input callback)
                   (should (equal "auto ship it" objective))
                   (funcall
                    callback
                    (niwa-prompt-submission-create
                     :input objective :display-text "/goal auto ship it"
                     :session session))))
                ((symbol-function 'niwa-view-history-add) #'ignore)
                ((symbol-function 'niwa-view--clear-input) #'ignore)
                ((symbol-function 'niwa-goal-start)
                 (lambda (objective _submission) (setq started objective))))
        (with-current-buffer view-buf
          (niwa-view--send-local-goal
           "/goal auto ship it" "auto ship it")))
      (should (equal "auto ship it" started)))))

(niwa-deftest niwa-view-send/user-prompt-hooks (:quiet t)
  (niwa-view-composer-test--without-admission ,test)
  (test)

  :doc "blocking UserPromptSubmit does not record history or insert prompt"
  (let* ((root (make-temp-file "niwa-view-hooks" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks" root "view-hooks"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       niwa-view-test--stop-prompt-hook))))))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (with-current-buffer view-buf
            (niwa-view-composer-insert "blocked prompt"
                                           (niwa-view-composer-draft-start))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send))
            (should-not (niwa-view-history--entries))
            (should-not
             (string-match-p
              "blocked prompt"
              (buffer-substring-no-properties
               (point-min) (niwa-view-transcript-tail)))))
	      (with-current-buffer data-buf
		(should (string-empty-p (buffer-string)))))
      (delete-directory root t)))

  :doc "a settlement arriving in a prompt hook preserves transcript and draft"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "hook-settlement" :root (niwa-test--placeholder-root)
                       :name "hook-settlement"))
           (session (niwa-session-create "main" workspace))
           (draft "> first line\nsecond line")
           data-before)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (setq data-before (buffer-string)))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (niwa-view-composer-insert draft
                                       (niwa-view-composer-draft-start)))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (with-current-buffer data-buf
                     (setq-local niwa--turn-settlements-pending
                                 '(settlement)))
                   (funcall callback nil)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (ert-fail "Settlement fence allowed provider dispatch"))))
        (with-current-buffer view-buf
          (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (should-not (niwa-view-history--entries))))
      (with-current-buffer data-buf
        (should (equal data-before (buffer-string))))))

  :doc "blocked prompt context is consumed once by the next accepted root input"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "blocked-context" :root (niwa-test--placeholder-root)
                       :name "blocked-context"))
           (session (niwa-session-create "main" workspace))
           (decisions
            (list '(:continue nil :additional-context ("blocked context"))
                  nil))
           accepted-context
           (blocked-count 0))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace workspace)
        (niwa-session-set-root-buffer session data-buf))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback (pop decisions)))))
        (with-current-buffer view-buf
          (niwa-view--run-prompt-submit-hook
           "blocked" "blocked"
           (lambda (&rest _)
             (ert-fail "Blocked prompt was accepted"))
           (lambda () (cl-incf blocked-count)))
          (should (= 1 blocked-count))
          (should (niwa-session-hook-context-pending session))
          (niwa-view--run-prompt-submit-hook
           "accepted" "accepted"
           (lambda (submission)
             (setq accepted-context
                   (niwa-prompt-submission-context submission))
             (niwa-prompt-submission-commit submission))))
        (should (string-match-p "blocked context" accepted-context))
      (should (string-match-p "UserPromptSubmit" accepted-context))
      (should-not (niwa-session-hook-context-pending session)))))

  :doc "accepted Plan follow-up input immediately invalidates stale approval"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "plan-follow-up" :root (niwa-test--placeholder-root)
                       :name "plan-follow-up"))
           (selection '(:location here :context current
                        :execution direct :mode edits))
           (session
            (niwa-session--create
             :name "main" :workspace workspace :plan-mode t
             :plan-metadata
             (list :status 'proposed :proposal-id '(1 2 "h")
                   :selection selection)))
           status-at-callback)
      (setf (niwa-session-pending-plan-approval session)
            (list :session session :callback #'ignore))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil))))
        (with-current-buffer view-buf
          (niwa-view--run-prompt-submit-hook
           "Refine this" "Refine this"
           (lambda (_submission)
             (setq status-at-callback
                   (plist-get (niwa-session-plan-metadata session)
                              :status))))))
      (let ((metadata (niwa-session-plan-metadata session)))
        (should (eq 'draft status-at-callback))
        (should-not (niwa-session-pending-plan-approval session))
        (should (equal selection (plist-get metadata :selection)))
        (should-not (plist-member metadata :proposal-id)))))

  :doc "first skill turn orders start, expansion, then submit context"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "ordered-context" :root (niwa-test--placeholder-root)
                       :name "ordered-context"))
           (session (niwa-session-create "main" workspace))
           accepted-context)
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace workspace)
        (niwa-hooks-record-session-context
         session '(:additional-context ("start context")) 'SessionStart))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq-local niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback
                            '(:additional-context ("submit context"))))))
        (with-current-buffer view-buf
          (niwa-view--run-prompt-submit-hook
           "expanded prompt" "Use $alpha"
           (lambda (submission)
             (setq accepted-context
                   (niwa-prompt-submission-context submission))
             (niwa-prompt-submission-commit submission))
           nil
           "<hook-context>expansion context</hook-context>")))
      (let ((start-pos (string-search "start context" accepted-context))
            (expansion-pos
             (string-search "expansion context" accepted-context))
            (submit-pos (string-search "submit context" accepted-context)))
        (should (< start-pos expansion-pos))
        (should (< expansion-pos submit-pos)))))

  :doc "errors before transcript insertion leave pending context for retry"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "callback-rollback" :root (niwa-test--placeholder-root)
                       :name "callback-rollback"))
           (session (niwa-session-create "main" workspace))
           retry-context)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (niwa-hooks-record-session-context
         session '(:additional-context ("retry context")) 'SessionStart))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (should-error
         (niwa-view--run-prompt-submit-hook
          "first" "first" (lambda (&rest _) (error "Dispatch failed"))))
        (should (niwa-session-hook-context-pending session))
        (niwa-view--run-prompt-submit-hook
         "retry" "retry"
         (lambda (submission)
           (setq retry-context
                 (niwa-prompt-submission-context submission))
           (niwa-prompt-submission-commit submission))))
      (should (string-match-p "retry context" retry-context))
      (should-not (niwa-session-hook-context-pending session))))

  :doc "send startup failure does not duplicate context already inserted"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "send-failure" :root (niwa-test--placeholder-root)
                       :name "send-failure"))
           (session (niwa-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (niwa-hooks-record-session-context
         session '(:additional-context ("send startup context"))
         'SessionStart))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (error "Request startup failed"))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "send prompt"
                                         (niwa-view-composer-draft-start))
          (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)))))
      (should-not (niwa-session-hook-context-pending session))
      (with-current-buffer data-buf
        (should (= 1 (niwa-view-test--count-matches
                      "send startup context" (buffer-string)))))))

  :doc "/goal prompts run UserPromptSubmit"
  (let* ((root (make-temp-file "niwa-view-plan-hooks" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-plan-hooks" root "view-plan-hooks"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
	                        :hooks ((:type elisp
	                                       :function
	                                       niwa-view-test--rewrite-prompt-hook-with-context)))))))
         (niwa-view-test--seen-prompt nil)
         events)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (with-current-buffer view-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-goal-start)
                     (lambda (objective submission)
                       (push (list objective
                                   (niwa-prompt-submission-context submission))
                             events))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "/goal draft"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should (equal "draft" niwa-view-test--seen-prompt))
              (setq events (nreverse events))
              (should (equal "rewritten prompt" (caar events)))
              (should (string-match-p "hook policy context"
                                      (cadar events)))
              (should (string-empty-p (niwa-view-composer-draft-text nil t)))))
          (with-current-buffer view-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail))))
              (should-not (string-match-p "/goal draft" text)))))
      (delete-directory root t)))

  :doc "a draft typed while /goal runs its hook survives the command"
  (let* ((root (make-temp-file "niwa-view-goal-redraft" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-goal-redraft" root
                     "view-goal-redraft"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (started 0))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (with-current-buffer view-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-goal-start)
                     (lambda (&rest _) (cl-incf started))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "/goal ship the campaign"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should niwa-view--prompt-hook-pending)
              ;; The user keeps typing while the hook runs.
              (niwa-view-composer-replace "> quoted\nsecond line")
              (let ((deadline (+ (float-time) 10)))
                (while (and niwa-view--prompt-hook-pending
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05))
                (should-not niwa-view--prompt-hook-pending)
                (should (= 1 started))
                (should (equal "> quoted\nsecond line"
                               (niwa-view-composer-draft-text nil t)))))))
      (delete-directory root t)))

  :doc "/plan PROMPT enters Plan and submits PROMPT through UserPromptSubmit"
  (let* ((root (make-temp-file "niwa-view-plan-command" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-plan-command" root "view-plan-command"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       niwa-view-test--rewrite-prompt-hook-with-context)))))))
         (niwa-view-test--seen-prompt nil)
         sent)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session
                        niwa--workspace workspace))
          (with-current-buffer view-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _) (setq sent t))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "/plan refactor X"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should sent)
              (should (equal "refactor X" niwa-view-test--seen-prompt))
              (should (equal "/plan refactor X"
                             (car (niwa-view-history--entries))))
              (should (string-empty-p (niwa-view-composer-draft-text nil t))))
            (should (niwa-session-plan-mode session))
            (with-current-buffer data-buf
              (should (string-match-p "rewritten prompt" (buffer-string))))))
      (delete-directory root t)))

  :doc "blocking UserPromptSubmit prevents expanded inline skill send"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((niwa-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
	                         :hooks ((:type elisp
	                                        :function
	                                        niwa-view-test--stop-prompt-hook)))))))
	          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
	(with-current-buffer view-buf
	  (niwa-view-composer-insert "$myskill blocked"
	                                 (niwa-view-composer-draft-start))
	  (with-current-buffer (niwa-view-composer-buffer)
	    (niwa-view-send))
	          (should-not send-called)
	          (should-not (niwa-view-history--entries)))
	        (with-current-buffer data-buf
	          (should (string-empty-p (buffer-string)))
	          (should-not (bound-and-true-p
	                       niwa-skills--pending-request-context))))))

  :doc "inline skill hooks see expanded body but cannot replace it"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((niwa-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        niwa-view-test--rewrite-prompt-hook)))))))
          (niwa-view-test--seen-prompt nil)
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$myskill hello"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
	        (should send-called)
	        (should (string-match-p "Expanded hello"
	                                niwa-view-test--seen-prompt))
	        (should-not (string-search "<!-- niwa-render-data -->"
	                                   niwa-view-test--seen-prompt))
	        (with-current-buffer data-buf
	          (let ((text
                   (niwa-tool-render-data-strip
                    (niwa--strip-hook-audit-blocks
                     (buffer-string)))))
	            (should-not (string-match-p "rewritten prompt" text))
            (should (string-match-p (regexp-quote "$myskill hello") text))
            (should-not (string-match-p "Expanded hello" text)))))))

  :doc "prompt rewrites render an expandable hook audit disclosure"
  (let* ((root (make-temp-file "niwa-view-hooks-audit" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks-audit" root "view-hooks-audit"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       niwa-view-test--rewrite-prompt-hook-with-message)))))))
         (send-called nil))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "original prompt"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should send-called)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-match-p "rewritten prompt" text))
                (should (string-match-p "hook changed prompt" text))
                (should-not (string-match-p "original prompt" text))
                (should-not (string-match-p "changed by test hook" text)))
              (goto-char (point-min))
              (search-forward "hook changed prompt")
              (niwa-view-toggle-section)
              (let ((expanded (buffer-substring-no-properties
                               (point-min) (niwa-view-transcript-tail))))
                (should (string-match-p "UserPromptSubmit" expanded))
                (should (string-match-p "changed by test hook" expanded))
                (should (string-match-p "Original prompt:" expanded))
                (should (string-match-p "original prompt" expanded))
                (should (string-match-p "Submitted prompt:" expanded))
                (should (string-match-p "rewritten prompt" expanded)))))
          (with-current-buffer data-buf
            (let ((text (buffer-string)))
              (should (string-match-p "<!-- niwa-hook-audit -->" text))
              (should (string-match-p "rewritten prompt" text))
              (goto-char (point-min))
              (search-forward "<!-- niwa-hook-audit -->")
              (should (eq 'niwa-hook-audit
                          (get-text-property (match-beginning 0)
                                             'gptel)))
              (should (eq t
                          (get-text-property (match-beginning 0)
                                             'niwa-hook-audit)))))
          (with-current-buffer view-buf
            (niwa-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail))))
              (should (string-match-p "hook changed prompt" text))
              (should-not (string-match-p "original prompt" text)))))
      (delete-directory root t)))

  :doc "inline skill Prompt omits hook context in immediate render"
  (niwa-view-test--with-fork-skill
      (niwa-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((niwa-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        niwa-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "$myskill hello"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail))))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "hook policy context" text)))
          (goto-char (point-min))
          (search-forward "Prompt")
          (niwa-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
            (should-not (string-match-p "rewritten prompt" expanded))
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "hook policy context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (niwa-tool-render-data-strip
                     (buffer-string))))
          (should-not (string-match-p "rewritten prompt" text))
          (should (string-match-p (regexp-quote "$myskill hello") text))
          (should-not (string-match-p "Expanded hello" text))
          (should (string-match-p "hook policy context" text))))))

  :doc "malformed UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "niwa-view-hooks-malformed" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks-malformed" root
                     "view-hooks-malformed"))
         (session (niwa-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (cl-letf (((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback :args)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "prompt with malformed hook result"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (should send-called)))
      (delete-directory root t)))

  :doc "symbol UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "niwa-view-hooks-symbol" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks-symbol" root
                     "view-hooks-symbol"))
         (session (niwa-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (cl-letf (((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback 'passed)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "prompt with symbol hook result"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (should send-called)))
      (delete-directory root t)))

  :doc "slow UserPromptSubmit command keeps the send path non-reentrant"
  (let* ((root (make-temp-file "niwa-view-hooks-pending" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks-pending" root "view-hooks-pending"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (send-count 0))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (cl-incf send-count))))
            (with-current-buffer view-buf
              (niwa-view-composer-insert "slow prompt"
                                             (niwa-view-composer-draft-start))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))
              (should niwa-view--prompt-hook-pending)
              (should-error (with-current-buffer (niwa-view-composer-buffer) (niwa-view-send)) :type 'user-error)
              (let ((deadline (+ (float-time) 10)))
                (while (and niwa-view--prompt-hook-pending
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not niwa-view--prompt-hook-pending)
              (should (= send-count 1)))))
      (delete-directory root t)))

  :doc "a detached draft typed while UserPromptSubmit runs survives the send"
  (let* ((root (make-temp-file "niwa-view-hooks-redraft" t))
         (workspace (niwa-workspace-get-or-create
                     'project "view-hooks-redraft" root
                     "view-hooks-redraft"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (send-count 0))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (with-current-buffer view-buf
            (let ((draft-buf (niwa-view-composer-buffer)))
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _) (cl-incf send-count))))
                (niwa-view-composer-replace "submitted prompt")
                (with-current-buffer draft-buf (niwa-view-send))
                (should niwa-view--prompt-hook-pending)
                (let ((draft-point
                       (niwa-view-composer-replace "> quoted\nsecond line"))
                      (deadline (+ (float-time) 10)))
                  (niwa-session--set-dropped-file-grants
                   session (list (file-name-concat root "later.png")))
                  (while (and niwa-view--prompt-hook-pending
                              (< (float-time) deadline))
                    (accept-process-output nil 0.05))
                  (should-not niwa-view--prompt-hook-pending)
                  (should (= send-count 1))
                  (should (equal (list (file-name-concat root "later.png"))
                                 (niwa-session-dropped-file-grants session)))
                  (should (equal "> quoted\nsecond line"
                                 (niwa-view-composer-draft-text nil t)))
                  (should (= draft-point (with-current-buffer draft-buf (point)))))))))
      (delete-directory root t)))

  :doc "abort makes a late UserPromptSubmit callback inert"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "abort-prompt-hook" :root (niwa-test--placeholder-root)
                       :name "abort-prompt-hook"))
           (session (niwa-session-create "main" workspace))
           late-callback
           send-called)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq late-callback callback)))
                ((symbol-function 'niwa-view--abort-data-buffer) #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "abort this prompt"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (should niwa-view--prompt-hook-pending)
          (niwa-view-abort)
          (funcall late-callback nil)
          (should-not niwa-view--prompt-hook-pending)
          (should (equal "abort this prompt" (niwa-view-composer-draft-text nil t))))
      (should-not send-called)
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))

  :doc "killing the view cancels a late UserPromptSubmit callback"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "kill-prompt-hook" :root (niwa-test--placeholder-root)
                       :name "kill-prompt-hook"))
           (session (niwa-session-create "main" workspace))
           late-callback
           submission
           send-called)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq late-callback callback)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "kill this prompt"
                                         (niwa-view-composer-draft-start))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))
          (setq submission niwa-view--prompt-hook-pending))
        (kill-buffer view-buf)
        (funcall late-callback nil))
      (should (eq 'cancelled
                  (niwa-prompt-submission-state submission)))
      (should-not send-called))))

(provide 'test-niwa-view-composer)
;;; test-niwa-view-composer.el ends here
