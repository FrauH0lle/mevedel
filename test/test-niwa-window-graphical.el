;;; test-niwa-window-graphical.el --- Child-frame window returns -*- lexical-binding: t -*-

;;; Commentary:

;; Real note-frame display redirection must restore the parent's layout
;; without replacing or losing the invoking child-frame composer.  Batch runs
;; skip these cases; frame-parent stubs cannot exercise cross-frame selection.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-note-frame)
(require 'niwa-menu)
(require 'niwa-recovery)
(require 'niwa-report)
(require 'niwa-session-artifacts)
(require 'niwa-structs)
(require 'niwa-theme-faces)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-pair)
(require 'niwa-window)
(require 'transient)
(require 'org)

(defun niwa-window-graphical-test--layout (frame)
  "Return FRAME's topology, displayed buffers, edges and selected window."
  (list (window-tree frame)
        (mapcar (lambda (window)
                  (list window (window-buffer window) (window-edges window)))
                (window-list frame 'no-minibuf))
        (frame-selected-window frame)))

(defmacro niwa-window-graphical-test--with-child (layout &rest body)
  "Run BODY from a real child composer over the parent's LAYOUT.
Bind parent, child, origin, parent-before and child-before for assertions."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (niwa-view-test--with-buffers
       (let* ((parent (selected-frame))
              (workspace (niwa-workspace--create
                          :type 'file :id "window-graphical"
                          :name "window-graphical" :root default-directory))
              (session (niwa-session-create "window-graphical" workspace))
              (other (generate-new-buffer " *window-graphical-other*"))
              (niwa-note-frame--frame nil)
              child)
         (unwind-protect
             (progn
               (with-current-buffer data-buf
                 (setq-local niwa--session session
                             niwa--workspace workspace))
               (with-current-buffer view-buf
                 (setq-local niwa--session session)
                 (niwa-view-test--insert-composer-draft
                  "> child composer draft\nsecond line" 7))
               (delete-other-windows)
               (switch-to-buffer data-buf)
               (when (eq ,layout 'split)
                 (set-window-buffer (split-window-right) other))
               (setq child
                     (make-frame
                      (append (list (cons 'parent-frame parent))
                              '((name . "niwa-window-test-child")
                                (width . 60) (height . 12)
                                (left . 20) (top . 20) (minibuffer . nil))
                              niwa--child-frame-parameters)))
               (should (eq parent (frame-parent child)))
               (setq niwa-note-frame--frame child)
               (let ((origin (frame-root-window child)))
                 (set-window-buffer origin view-buf)
                 (set-window-dedicated-p origin t)
                 (with-current-buffer view-buf
                   (niwa-note-frame-mode 1)
                   (should (eq 'niwa-note-frame--display-in-parent
                               (car display-buffer-overriding-action))))
                 (select-frame-set-input-focus child)
                 (select-window origin)
                 (redisplay t)
                 (let ((parent-before
                        (niwa-window-graphical-test--layout parent))
                       (child-before
                        (niwa-window-graphical-test--layout child))
                       (draft (niwa-view-composer-draft-text))
                       (draft-point (niwa-view-test--composer-point-offset)))
                   ,@body
                   (should (frame-live-p child))
                   (should (window-live-p origin))
                   (should (eq view-buf (window-buffer origin)))
                   (should (window-dedicated-p origin))
                   (should (eq child (selected-frame)))
                   (should (eq origin (selected-window)))
                   (should (equal parent-before
                                  (niwa-window-graphical-test--layout parent)))
                   (should (equal child-before
                                  (niwa-window-graphical-test--layout child)))
                   (should (equal draft (niwa-view-composer-draft-text)))
                   (should (= draft-point
                              (niwa-view-test--composer-point-offset))))))
           (transient--emergency-exit)
           (when (buffer-live-p view-buf)
             (with-current-buffer view-buf (niwa-note-frame-mode -1)))
           ;; This fixture owns its frame directly, not the note overlay
           ;; lifecycle.  Do not invoke that lifecycle's source restoration.
           (setq niwa-note-frame--frame nil)
           (when (frame-live-p child) (delete-frame child t))
           (when (frame-live-p parent) (select-frame-set-input-focus parent))
           (when (buffer-live-p other) (kill-buffer other)))))))

(defun niwa-window-graphical-test--publish (session path body)
  "Commit archived fixture BODY at PATH under SESSION's real lease."
  (niwa-session-publication-publish
   session
   (list (list :path path :content body)
         (list :path (file-name-concat (niwa-session-save-path session) "session.meta.el")
               :content (niwa-session-artifacts-printed-value
                         (niwa-session-codec-serialize session))
               :commit-marker t))))

(niwa-deftest niwa-window-child-frame-return ()
  ,test
  (test)
  :doc "report open and q restore the parent and return to the child composer"
  (progn
    (skip-unless (display-graphic-p))
    (dolist (layout '(single split))
      (ert-info ((format "Parent layout: %s" layout))
        (niwa-window-graphical-test--with-child layout
          (let ((surface (generate-new-buffer " *window-graphical-report*")))
            (unwind-protect
                (progn
                  ;; The report uses the generic window display/quit owner and
                  ;; supplies its production q binding, without a test command.
                  (niwa-report-show
                   surface '(:title "Child-frame inspection"
                             :sections ((:id body :title "Inspection"
                                         :body "A temporary parent-frame report."))))
                  (should (eq parent (selected-frame)))
                  (should (eq surface (window-buffer (selected-window))))
                  (should (eq view-buf (window-buffer origin)))
                  (should (equal child-before
                                 (niwa-window-graphical-test--layout child)))
                  (should (eq 'niwa-report-quit (key-binding (kbd "q"))))
                  (execute-kbd-macro (kbd "q"))
                  (should-not (buffer-live-p surface)))
              (when (buffer-live-p surface) (kill-buffer surface)))))))))

(niwa-deftest niwa-menu-child-frame-return (:quiet t)
  ,test
  (test)
  :doc "menu open and C-g restore the parent and return to the child composer"
  (progn
    (skip-unless (display-graphic-p))
    (dolist (layout '(single split))
      (ert-info ((format "Parent layout: %s" layout))
        (niwa-window-graphical-test--with-child layout
          (let ((map (copy-keymap (current-local-map)))
                (transient-show-popup t))
            (define-key map (kbd "<f12>") #'niwa-menu)
            (use-local-map map)
            (execute-kbd-macro (kbd "<f12>"))
            (should transient--prefix)
            (should (eq (oref transient--prefix command) 'niwa-menu--top))
            (should (window-live-p transient--window))
            (should (eq parent (window-frame transient--window)))
            (should (eq view-buf (window-buffer origin)))
            (should (equal child-before
                           (niwa-window-graphical-test--layout child)))
            (execute-kbd-macro (kbd "C-g"))
            (should-not transient--prefix)))))))

;;; Note frame pinned prompt

(defmacro niwa-window-graphical-test--with-frame (exchanges &rest body)
  "Run BODY with a real note frame over a view of EXCHANGES.
EXCHANGES is a list of prompts; each answer is long enough to
scroll its prompt away.  The frame is built by the production chrome owner.
Bind parent, frame and window for BODY, which runs in the view buffer."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (niwa-view-test--with-buffers
       (dolist (exchange ,exchanges)
         (niwa-view-test--insert-data data-buf (format "%s\n" exchange) nil)
         (niwa-view-test--insert-data
          data-buf
          (mapconcat (lambda (n) (format "Line %d answering %s.\n" n exchange))
                     (number-sequence 1 15) "")
          'response))
       (with-current-buffer view-buf
         (niwa-view--full-rerender)
         (niwa-view-test--insert-composer-draft "> frame draft\nsecond line"))
       (let* ((parent (selected-frame))
              (niwa-note-frame--frame nil)
              (niwa-note-frame--view-buffer view-buf)
              (niwa-note-frame--note-id "Note 1")
              (frame (niwa-note-frame--make view-buf parent))
              (window (frame-root-window frame)))
         (setq niwa-note-frame--frame frame)
         (unwind-protect
             (progn
               (set-frame-position frame 40 40)
               (make-frame-visible frame)
               (with-current-buffer view-buf ,@body))
           (setq niwa-note-frame--frame nil)
           (when (frame-live-p frame) (delete-frame frame t))
           (select-frame-set-input-focus parent))))))

(defun niwa-window-graphical-test--size (frame columns lines)
  "Size FRAME to COLUMNS by LINES in parent cells and dock its composer."
  (let ((parent (frame-parent frame)))
    (set-frame-size frame (* columns (frame-char-width parent))
                    (* lines (frame-char-height parent)) t))
  ;; The fixture bypasses note display; do not let deferred repair change the
  ;; body height between the pinned and unpinned samples.
  (should (window-live-p (niwa-view-composer-display (frame-first-window frame))))
  (redisplay t)
  (sit-for 0.02)
  (redisplay t))

(defun niwa-window-graphical-test--rows (window)
  "Return WINDOW's displayed chrome after redisplay.
A plist of the tab and header row text and pixel heights, window start,
body height and composer draft."
  (redisplay t)
  (let ((buffer (window-buffer window)))
    (cl-flet ((row (slot)
                (let* ((param (window-parameter window slot))
                       (format (cond ((eq param 'none) nil)
                                     (param)
                                     (t (buffer-local-value slot buffer)))))
                  (and format
                       (with-selected-window window
                         (format-mode-line format nil window))))))
      (list :tab (row 'tab-line-format)
            :header (row 'header-line-format)
            :tab-px (window-tab-line-height window)
            :header-px (window-header-line-height window)
            :start (window-start window)
            :body (window-body-height window t)
            :draft (with-current-buffer buffer (niwa-view-composer-draft-text))))))

(defun niwa-window-graphical-test--start-at (window text &optional offset)
  "Scroll WINDOW so the line holding TEXT, moved OFFSET lines, is on top."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (point-min))
      (search-forward text)
      (forward-line (or offset 0))
      (set-window-start window (line-beginning-position) t)
      (set-window-point window (point))
      (line-beginning-position))))

(niwa-deftest niwa-note-frame-pinned-prompt-graphical ()
  ,test
  (test)
  :doc "narrow and short frames keep the condensed header above the pinned prompt"
  (progn
    (skip-unless (display-graphic-p))
    (niwa-window-graphical-test--with-frame
        (mapcar (lambda (n) (format "Prompt %d asks" n))
                (number-sequence 0 5))
      (dolist (geometry '((40 8) (66 12) (24 8)))
        (ert-info ((format "Geometry: %S" geometry))
          (apply #'niwa-window-graphical-test--size frame geometry)
          (niwa-window-graphical-test--start-at
           window "Line 8 answering Prompt 4")
          (let ((away (niwa-window-graphical-test--rows window)))
            (ert-info ((format "Scrolled away: %S" away))
              (should (string-prefix-p "◆ " (plist-get away :tab)))
              (should (string-search "Note 1" (plist-get away :tab)))
              (should (string-search "Prompt 4" (plist-get away :header)))
              (should (string-prefix-p "Prompt  " (plist-get away :header)))
              (should (<= (string-width (plist-get away :header))
                          (window-body-width window)))
              (should (> (plist-get away :tab-px) 0))
              (should (> (plist-get away :header-px) 0)))
            ;; The original header on screen: nothing pinned, row reserved.
            (niwa-window-graphical-test--start-at window "Prompt 4 asks" -1)
            (let ((on (niwa-window-graphical-test--rows window)))
              (ert-info ((format "On screen: %S" on))
                (should (string-search "Prompt 4 asks"
                                       (buffer-substring (plist-get on :start)
                                                         (window-end window t))))
                (should (equal "" (plist-get on :header)))
                (should (string-prefix-p "◆ " (plist-get on :tab)))
                (should (= (plist-get away :header-px) (plist-get on :header-px)))
                (should (= (plist-get away :body) (plist-get on :body)))
                (should (equal "> frame draft\nsecond line" (plist-get on :draft)))))
            ;; Pinning and unpinning repeatedly leaves the viewport alone.
            (let ((start (niwa-window-graphical-test--start-at
                          window "Line 3 answering Prompt 2")))
              (dotimes (_ 3)
                (let ((rows (niwa-window-graphical-test--rows window)))
                  (should (= start (plist-get rows :start)))
                  (should (string-search "Prompt 2" (plist-get rows :header)))
                  (should (equal "> frame draft\nsecond line"
                                 (plist-get rows :draft)))))))))))

  :doc "the chrome is window-local: the parent's view keeps its strip and row"
  (progn
    (skip-unless (display-graphic-p))
    (niwa-window-graphical-test--with-frame
        '("Prompt 0 asks" "Prompt 1 asks")
      (niwa-window-graphical-test--size frame 40 10)
      (let ((main (frame-selected-window parent)))
        (set-window-buffer main view-buf)
        (should-not (window-parameter main 'tab-line-format))
        (should-not (window-parameter main 'header-line-format))
        (should (equal '(:eval (niwa-view--status-strip)) tab-line-format))
        (should (equal '(:eval (niwa-view--sticky-prompt-line))
                       header-line-format))
        (let ((rows (niwa-window-graphical-test--rows main)))
          (should-not (string-search "◆" (plist-get rows :tab)))
          (should (> (plist-get rows :tab-px) 0))
          (should (> (plist-get rows :header-px) 0))))))

  :doc "a follow-up prompt added in the frame pins once it scrolls away"
  (progn
    (skip-unless (display-graphic-p))
    (niwa-window-graphical-test--with-frame
        '("Note prompt")
      (niwa-window-graphical-test--size frame 50 10)
      (niwa-view-test--insert-data data-buf "Follow-up question\n" nil)
      (niwa-view-test--insert-data
       data-buf (mapconcat (lambda (n) (format "Follow-up line %d.\n" n))
                           (number-sequence 1 15) "")
       'response)
      (niwa-view--full-rerender)
      (niwa-window-graphical-test--start-at window "Line 9 answering Note")
      (should (string-search "Note prompt"
                             (plist-get (niwa-window-graphical-test--rows window)
                                        :header)))
      (niwa-window-graphical-test--start-at window "Follow-up line 6.")
      (should (string-search "Follow-up question"
                             (plist-get (niwa-window-graphical-test--rows window)
                                        :header)))))

  :doc "clicking the frame's preview reveals its prompt in the frame window"
  (progn
    (skip-unless (display-graphic-p))
    (niwa-window-graphical-test--with-frame
        (mapcar (lambda (n) (format "Prompt %d asks" n))
                (number-sequence 0 3))
      (niwa-window-graphical-test--size frame 50 10)
      (let ((main (frame-selected-window parent))
            (main-start nil))
        (set-window-buffer main view-buf)
        (set-window-start main (point-min) t)
        (redisplay t)
        (setq main-start (window-start main))
        (niwa-window-graphical-test--start-at window "Line 10 answering Prompt 2")
        (let* ((line (with-selected-window window
                       (format-mode-line header-line-format nil window)))
               (column (string-search "Prompt 2" line))
               (command (lookup-key (get-text-property column 'local-map line)
                                    [header-line mouse-1]))
               (header (save-excursion
                         (goto-char (point-min))
                         (search-forward "Prompt 2 asks")
                         (search-backward "You\n"))))
          (funcall command (list 'mouse-1 (list window 'header-line '(10 . 0) 0
                                                (cons line column))))
          (let ((rows (niwa-window-graphical-test--rows window)))
            (should (= header (plist-get rows :start)))
            (should (equal "" (plist-get rows :header)))
            (should (equal "> frame draft\nsecond line" (plist-get rows :draft))))
          (should (= main-start (window-start main)))))))

  :doc "compaction keeps the originating prompt in the frame; /clear drops it"
  (progn
    (skip-unless (display-graphic-p))
    (let* ((directory (make-temp-file "niwa-frame-continuation-" t))
           (session (test-niwa-session-persistence--leased-session directory
                   :name "frame-continuation"
                     :current-segment 2))
           indexed)
      (unwind-protect
          (progn
            (with-temp-buffer
              (org-mode)
              (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
              (insert "Original framed prompt\n")
              (setq indexed (car (niwa-session-artifacts-collect-prompts
                                  (current-buffer))))
              (niwa-window-graphical-test--publish
               session (niwa-session-artifacts-segment-path directory 1) (buffer-string)))
            (setf (niwa-session-prompt-index session)
                  (list (cons 1 (list (append indexed '(:cum-turn 1))))))
            (niwa-window-graphical-test--with-frame '()
              (with-current-buffer data-buf
                (setq-local niwa--session session)
                (insert (niwa-session-artifacts-summary-block "summary"))
                (insert (propertize
                         (apply #'concat (make-list 40 "Continuation answer\n"))
                         'gptel 'response)))
              (niwa-view--full-rerender)
              (niwa-window-graphical-test--size frame 50 10)
              (niwa-window-graphical-test--start-at
               window "Continuation answer" 10)
              (should (string-search
                       "Original framed prompt"
                       (plist-get (niwa-window-graphical-test--rows window)
                                  :header)))
              ;; An empty previous segment is a /clear boundary.
              (with-temp-buffer
                (niwa-window-graphical-test--publish
                 session (niwa-session-artifacts-segment-path directory 2) (buffer-string)))
              (setf (niwa-session-current-segment session) 3)
              (niwa-view--full-rerender)
              (niwa-window-graphical-test--start-at
               window "Continuation answer" 10)
              (let ((rows (niwa-window-graphical-test--rows window)))
                (should (equal "" (plist-get rows :header)))
                (should (> (plist-get rows :header-px) 0)))))
        (niwa-session-persistence-lock-release directory session)
        (niwa-workspace-clear-registry)
        (delete-directory directory t)))))

(defun niwa-window-graphical-test--border-px (frame)
  "Return FRAME's child border color as painted."
  (list (face-background 'child-frame-border frame)
        (face-background 'internal-border frame)))

(ert-deftest niwa-note-frame-border-follows-theme-graphical ()
  "A shown note frame repaints its border when the theme changes."
  (skip-unless (display-graphic-p))
  (let* ((parent (selected-frame))
         (saved (list (face-attribute 'default :foreground parent)
                      (face-attribute 'default :background parent))))
    (niwa-window-graphical-test--with-frame '("Border prompt")
      (unwind-protect
          (dolist (theme '(("dark" "#bbc2cf" "#282c34" "#3b4252")
                           ("light" "#24292e" "#ffffff" "#e6eef7")))
            (pcase-let ((`(,name ,fg ,bg ,interior) theme))
              (set-face-attribute 'default parent :foreground fg :background bg)
              (set-face-background 'default interior frame)
              (cl-letf (((symbol-function 'niwa--color-frame) (lambda () parent)))
                (run-hook-with-args 'enable-theme-functions 'border-test))
              (let ((active (face-attribute 'niwa-note-frame-border
                                            :background nil 'default))
                    (inactive (face-attribute 'niwa-note-frame-border-inactive
                                              :background nil 'default)))
                (should (> (niwa--contrast active bg)
                           (+ 0.5 (niwa--contrast inactive bg))))
                (dolist (state `((focused ,active ,#'always)
                                 (unfocused ,inactive ,#'ignore)))
                  (niwa-note-frame--paint-border frame (nth 2 state))
                  (redisplay t)
                  (should (equal (list (nth 1 state) (nth 1 state))
                                 (niwa-window-graphical-test--border-px frame)))))))
        (set-face-attribute 'default parent :foreground (nth 0 saved)
                            :background (nth 1 saved))
        (run-hook-with-args 'enable-theme-functions 'border-test)))))
(provide 'test-niwa-window-graphical)
;;; test-niwa-window-graphical.el ends here
