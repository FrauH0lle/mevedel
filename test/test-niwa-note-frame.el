;;; test-niwa-note-frame.el --- Note frame tests -*- lexical-binding: t -*-

;;; Commentary:

;; Child frame creation is not assertable in batch, so these cover the
;; display-independent halves: the availability gate, placement, source
;; tracking, and teardown.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-conversation)
(require 'niwa-agents)
(require 'niwa-chat)
(require 'niwa-collaboration-share)
(require 'niwa-note-frame)
(require 'niwa-file-state)
(require 'niwa-goal)
(require 'niwa-hooks)
(require 'niwa-mentions)
(require 'niwa-menu)
(require 'niwa-permission-queue)
(require 'niwa-pipeline)
(require 'niwa-review)
(require 'niwa-session-persistence)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-theme-faces)
(require 'niwa-tool-exec)
(require 'niwa-tool-media)
(require 'niwa-tool-registry)
(require 'niwa-tool-repair)
(require 'niwa-tool-task)
(require 'niwa-tool-ui)
(require 'niwa-transcript)
(require 'niwa-transcript-restore)
(require 'niwa-view)
(require 'niwa-view-audit)
(require 'niwa-view-composer)
(require 'niwa-view-history)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-zone)
(require 'niwa-workspace)

;; Bound by the session layer at runtime; the view fixture renders without it.
(defvar niwa-session--read-only-mode nil)
(defvar niwa-collaboration--share-frame nil)

(defun niwa-note-frame-test--reset ()
  "Clear module state so no test leaks a frame."
  ;; Only ever delete a real child frame.  A test may point the variable at
  ;; the initial frame to exercise a branch, and deleting that fails.
  (when (and (frame-live-p niwa-note-frame--frame)
             (frame-parent niwa-note-frame--frame))
    (delete-frame niwa-note-frame--frame))
  (when (markerp niwa-note-frame--origin)
    (set-marker niwa-note-frame--origin nil))
  (niwa-note-frame--unfollow)
  (setq niwa-note-frame--pending-restore nil
        niwa-note-frame--frame nil
        niwa-note-frame--view-buffer nil
        niwa-note-frame--note-id nil
        niwa-note-frame--origin nil
        niwa-note-frame--origin-window nil))

(defun niwa-note-frame-test--note (start end &optional id)
  "Return a note overlay from START to END in the current buffer.
ID is the note number, 1 by default."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'niwa-instruction t)
    (overlay-put overlay 'niwa-instruction-type 'note)
    (overlay-put overlay 'niwa-id (or id 1))
    overlay))

(defmacro niwa-note-frame-test--with-view (&rest body)
  "Run BODY with `view-buf' holding two rendered turns.
BODY runs inside the view buffer."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let* ((root (file-name-as-directory
                   (make-temp-file "niwa-note-frame-" t)))
            (workspace (niwa-workspace--create
                        :type 'file :id root :root root :name "frame"
                        :file-cache (niwa-test-file-cache-create)))
            (session (niwa-session-create "frame" workspace root)))
       (unwind-protect
           (progn
             (with-current-buffer data-buf
               (setq-local default-directory root
                           niwa--workspace workspace
                           niwa--session session)
               (insert "First prompt\n")
               (insert (propertize "First answer\n" 'gptel 'response))
               (insert "Second prompt\n")
               (insert (propertize "Second answer\n" 'gptel 'response)))
             (with-current-buffer view-buf
               (setq-local niwa--session session)
               (niwa-view--full-rerender)
              (should (> (length (niwa-view--rendered-turn-starts)) 1))
               ,@body))
         (niwa-note-frame-test--reset)
         (delete-directory root t)))))

(niwa-deftest niwa-note-frame--available-p
  (:doc "`niwa-note-frame--available-p' gates on a usable display")
  ,test
  (test)
  :doc "reports unavailable in batch, where `make-frame' has no terminal"
  (let ((noninteractive t))
    (should-not (niwa-note-frame--available-p)))

  :doc "reports available in an interactive session, graphical or not"
  (let ((noninteractive nil))
    (should (niwa-note-frame--available-p))))

(niwa-deftest niwa-note-frame--paint-border ()
  (let ((frame (selected-frame))
        (parent (selected-frame))
        (standard-display-table nil)
        (redrawn nil))
    (skip-unless (not (display-graphic-p frame)))
    (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _frame) parent))
              ;; Run the deferred redraw at once and record it.
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args) (apply function args)))
              ((symbol-function 'redraw-frame)
               (lambda (&optional frame) (push frame redrawn))))
      (unwind-protect
          ,test
        (set-face-foreground 'border nil frame))))
  (test)
  :doc "a terminal child's border shows focus in its own border face's foreground"
  (progn
    (niwa-note-frame--paint-border frame #'always)
    (should (equal (face-attribute 'niwa-note-frame-border :background nil 'default)
                   (face-foreground 'border frame)))
    (niwa-note-frame--paint-border frame #'ignore)
    (should (equal (face-attribute 'niwa-note-frame-border-inactive :background
                                   nil 'default)
                   (face-foreground 'border frame)))
    (should-not (face-foreground 'border t))
    (should-not standard-display-table))
  :doc "redraws the parent only when the color changes"
  (progn
    (niwa-note-frame--paint-border frame #'always)
    (niwa-note-frame--paint-border frame #'always)
    (should (equal (list parent) redrawn))
    (niwa-note-frame--paint-border frame #'ignore)
    (should (equal (list parent parent) redrawn))))

(niwa-deftest niwa-note-frame-display
  (:doc "`niwa-note-frame-display' creates and reuses note frames")
  ,test
  (test)
  :doc "reuses a live frame already showing the view instead of rebuilding it"
  (niwa-note-frame-test--with-view
    (let ((view (current-buffer))
          (frames (length (frame-list)))
          (source (generate-new-buffer " *test-source*")))
      (unwind-protect
          (let ((note (with-current-buffer source
                        (insert "note body\n")
                        (niwa-note-frame-test--note
                         (point-min) (point-max) 3))))
            (setq niwa-note-frame--frame (selected-frame)
                  niwa-note-frame--view-buffer view
                  niwa-note-frame--note-id "Note 3")
            (cl-letf (((symbol-function 'niwa-note-frame--available-p)
                       (lambda () t))
                      ;; Terminal note frames fall back to a side window.
                      ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                      ((symbol-function 'niwa-note-frame--make)
                       (lambda (&rest _)
                         (error "Rebuilt a frame that was already showing the view"))))
              (should (eq (selected-frame)
                          (niwa-note-frame-display note view))))
            (should (= frames (length (frame-list)))))
        (kill-buffer source))))

  :doc "same-view reuse retargets identity, origin and teardown handoff"
  (niwa-note-frame-test--with-view
    (let* ((view (current-buffer))
           (file (file-name-concat root "source.el"))
           (source nil)
           note-a note-b
           (makes 0))
      (unwind-protect
          (progn
            (write-region "alpha\nbeta\n" nil file nil 'silent)
            (setq source (find-file-noselect file))
            (with-current-buffer source
              (setq note-a (niwa-note-frame-test--note 1 6 1)
                    note-b (niwa-note-frame-test--note 7 11 2)))
            (save-window-excursion
              (set-window-buffer (selected-window) source)
              (cl-letf (((symbol-function
                          'niwa-note-frame--available-p)
                         (lambda () t))
                        ((symbol-function 'display-graphic-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'niwa-note-frame--anchor)
                         (lambda (&rest _) '(10 . 20)))
                        ((symbol-function 'niwa-note-frame--make)
                         (lambda (&rest _)
                           (cl-incf makes)
                           (selected-frame)))
                        ((symbol-function 'niwa-note-frame--size)
                         (lambda (&rest _) '(100 . 50)))
                        ((symbol-function 'make-frame-visible) #'ignore)
                        ((symbol-function 'select-frame-set-input-focus)
                         #'ignore)
                        ((symbol-function 'set-frame-position) #'ignore)
                        ((symbol-function 'set-frame-size) #'ignore))
                (niwa-note-frame-display note-a view)
                (niwa-note-frame-display note-b view)
                (should (= 2 makes))
                (should (equal "Note 2" niwa-note-frame--note-id))
                (should (eq note-b
                            niwa-note-frame--note))
                (should (= (overlay-start note-b)
                           (marker-position
                            niwa-note-frame--origin)))
                (niwa-test--with-captured-messages nil
                  (niwa-note-frame--on-delete (selected-frame)))
                (should
                 (= (marker-position
                     (plist-get niwa-note-frame--pending-restore
                                :origin))
                    (overlay-start note-b)))
                (with-current-buffer source
                  (niwa-note-frame--after-delete)))))
        (when (buffer-live-p source)
          (with-current-buffer source
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer source))
        (when (file-exists-p file) (delete-file file))))))

(niwa-deftest niwa-note-frame--source-window
  (:doc "`niwa-note-frame--source-window' never returns the frame itself")
  ,test
  (test)
  :doc "returns the window showing the note's source buffer"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (niwa-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (should (eq (selected-window)
                      (niwa-note-frame--source-window note))))
      (kill-buffer buffer)))

  :doc "prefers the selected window when the buffer is on screen twice"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (niwa-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((first (selected-window))
                (other (split-window)))
            (set-window-buffer other buffer)
            (select-window other)
            (should (eq other
                        (niwa-note-frame--source-window note)))
            (select-window first)
            (should (eq first
                        (niwa-note-frame--source-window note)))))
      (kill-buffer buffer)
      (niwa-note-frame-test--reset)))

  :doc "excludes windows inside the note frame, which would make it \
its own parent"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (niwa-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((niwa-note-frame--frame (selected-frame)))
            (should-not
             (niwa-note-frame--source-window note))))
      (kill-buffer buffer)
      (niwa-note-frame-test--reset))))

(niwa-deftest niwa-note-frame--place
  (:doc "`niwa-note-frame--place' keeps the note readable")
  ,test
  (test)
  :doc "opens below the note's last line when it fits"
  (should (equal '(300 . 298)
                 (niwa-note-frame--place
                  300 100 280 18 1200 360 2000 1060)))
  :doc "clamps x so the frame stays inside the parent's right edge"
  (should (= 800 (car (niwa-note-frame--place
                       1500 100 280 18 1200 360 2000 1060))))
  :doc "flips above the first line, not the last, so the note stays visible"
  (let ((y (cdr (niwa-note-frame--place
                 300 520 880 18 1200 360 2000 1060))))
    (should (= 160 y))
    (should (<= (+ y 360) 520)))
  :doc "sits flush above a note when fitted shorter than the maximum"
  (should (= 376 (cdr (niwa-note-frame--place
                       300 520 940 18 1200 144 2000 1060))))
  :doc "takes the larger side, clamped, only when neither side has room"
  (should (= 660 (cdr (niwa-note-frame--place
                       300 200 700 18 1200 400 2000 1060))))
  (should (= 0 (cdr (niwa-note-frame--place
                     300 500 900 18 1200 600 2000 1060)))))

(niwa-deftest niwa-note-frame--fit-height
  (:doc "`niwa-note-frame--fit-height' never resizes width")
  ,test
  (test)
  :doc "fits vertically only, so a long line cannot outgrow the parent frame"
  (let (captured)
    (unwind-protect
        (cl-letf (((symbol-function 'fit-frame-to-buffer)
                   (lambda (&rest args) (setq captured args))))
          (let ((niwa-note-frame--frame (selected-frame)))
            (niwa-note-frame--fit-height)))
      (niwa-note-frame-test--reset))
    (should captured)
    (should (eq 'vertically (nth 5 captured))))

  :doc "does nothing when no frame is live"
  (let (called)
    (cl-letf (((symbol-function 'fit-frame-to-buffer)
               (lambda (&rest _) (setq called t))))
      (let ((niwa-note-frame--frame nil))
        (niwa-note-frame--fit-height)))
    (should-not called)))

(niwa-deftest niwa-note-frame--follow-setup
  (:doc "`niwa-note-frame--follow-setup' tracks the source buffer")
  ,test
  (test)
  :doc "installs local follow hooks and removes them on teardown"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (niwa-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (progn
          (niwa-note-frame--follow-setup note)
          (with-current-buffer buffer
            (should (memq #'niwa-note-frame--follow
                          window-scroll-functions))
            (should (memq #'niwa-note-frame--follow
                          window-configuration-change-hook)))
          (should (eq note niwa-note-frame--note))
          (niwa-note-frame--unfollow)
          (with-current-buffer buffer
            (should-not (memq #'niwa-note-frame--follow
                              window-scroll-functions))
            (should-not (memq #'niwa-note-frame--follow
                              window-configuration-change-hook)))
          (should-not niwa-note-frame--note)
          (should-not niwa-note-frame--source-buffer))
      (kill-buffer buffer)
      (niwa-note-frame-test--reset))))

(niwa-deftest niwa-note-frame--header
  (:doc "`niwa-note-frame--header' leads with note identity")
  ,test
  (test)
  :doc "keeps the full note display number and live request state"
  (niwa-note-frame-test--with-view
    (let* ((file (file-name-concat root "source.el"))
           source)
      (unwind-protect
          (progn
            (write-region "noted code\n" nil file nil 'silent)
            (setq source (find-file-noselect file))
            (with-current-buffer source
              (let ((note (niwa-note-frame-test--note 1 11 1234)))
                (setq niwa-note-frame--note-id
                      (format "Note %s" (overlay-get note 'niwa-id)))))
            (let ((header (substring-no-properties (niwa-note-frame--header))))
              (should (string-search "Note 1234" header))
              (should (string-search "tool" header))
              (should (string-search "C-c C-z close" header))))
        (when (buffer-live-p source) (kill-buffer source))))))

(niwa-deftest niwa-note-frame--display-in-parent
  (:doc "`niwa-note-frame--display-in-parent' keeps pop-ups out of the frame")
  ,test
  (test)
  :doc "declines when no note frame is live, so display falls through"
  (let ((niwa-note-frame--frame nil))
    (should-not
     (niwa-note-frame--display-in-parent (current-buffer) nil)))

  :doc "declines for a frame with no parent rather than displaying into it"
  (let ((niwa-note-frame--frame (selected-frame)))
    (should-not
     (niwa-note-frame--display-in-parent (current-buffer) nil)))

  :doc "re-wraps the alist as an action, which a bare alist would break"
  (let ((buffer (generate-new-buffer " *test-popped*")))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (&rest _) (selected-frame))))
            (let ((niwa-note-frame--frame (selected-frame)))
              ;; The alist `pop-to-buffer' passes: as a bare second argument
              ;; display-buffer reads (inhibit-same-window . t) as an action
              ;; function and signals wrong-type-argument.
              (should (window-live-p
                       (niwa-note-frame--display-in-parent
                        buffer '((inhibit-same-window . t))))))))
      (kill-buffer buffer)
      (niwa-note-frame-test--reset))))

(niwa-deftest niwa-note-frame-mode
  (:doc "`niwa-note-frame-mode' redirects buffer display while on")
  ,test
  (test)
  :doc "installs and removes the buffer-local display override"
  (niwa-note-frame-test--with-view
    (niwa-note-frame-mode 1)
    (should (local-variable-p 'display-buffer-overriding-action))
    (should (eq 'niwa-note-frame--display-in-parent
                (car display-buffer-overriding-action)))
    (niwa-note-frame-mode -1)
    (should-not (local-variable-p 'display-buffer-overriding-action))))

(niwa-deftest niwa-note-frame--on-delete
  (:doc "`niwa-note-frame--on-delete' releases every frame resource")
  ,test
  (test)
  :doc "clears module state for the deleted frame"
  (niwa-note-frame-test--with-view
    (setq niwa-note-frame--view-buffer (current-buffer)
          niwa-note-frame--note-id "Note 1"
          niwa-note-frame--frame (selected-frame))
    (niwa-note-frame-mode 1)
    (niwa-note-frame--on-delete (selected-frame))
    (should-not niwa-note-frame-mode)
    (should-not niwa-note-frame--frame)
    (should-not niwa-note-frame--view-buffer)
    (should-not niwa-note-frame--note-id))

  :doc "ignores a frame that is not the note frame"
  (niwa-note-frame-test--with-view
    (setq niwa-note-frame--view-buffer (current-buffer)
          niwa-note-frame--note-id "Note 1"
          niwa-note-frame--frame nil)
    (niwa-note-frame--on-delete (selected-frame))
    (should (equal "Note 1" niwa-note-frame--note-id)))

  :doc "hands focus and point to the post-deletion phase instead of restoring \
them while the frame is still alive"
  (niwa-note-frame-test--with-view
    (setq niwa-note-frame--view-buffer (current-buffer)
          niwa-note-frame--frame (selected-frame)
          niwa-note-frame--origin-window (selected-window))
    (niwa-note-frame--on-delete (selected-frame))
    (should niwa-note-frame--pending-restore)
    (should (eq (selected-window)
                (plist-get niwa-note-frame--pending-restore :window)))))

(niwa-deftest niwa-note-frame--after-delete
  (:doc "`niwa-note-frame--after-delete' settles focus once the frame is gone")
  ,test
  (test)
  :doc "returns point to the note and consumes the handoff"
  (let* ((buffer (generate-new-buffer " *test-source*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "one\ntwo\nthree\n")
            (goto-char (point-max)))
          (let ((origin (with-current-buffer buffer (copy-marker 5))))
            (setq niwa-note-frame--pending-restore
                  (list :frame (selected-frame)
                        :window (selected-window)
                        :origin origin))
            (niwa-note-frame--after-delete)
            (should-not niwa-note-frame--pending-restore)
            (should (= 5 (with-current-buffer buffer (point))))
            (should-not (marker-position origin))))
      (kill-buffer buffer)
      (niwa-note-frame-test--reset)))

  :doc "does nothing when no deletion handed anything over"
  (let ((niwa-note-frame--pending-restore nil))
    (niwa-note-frame--after-delete)
    (should-not niwa-note-frame--pending-restore)))

(provide 'test-niwa-note-frame)


(ert-deftest niwa-note-frame-border-is-prominent ()
  "The border defaults to 3 px."
  (should (= 3 (eval (car (get 'niwa-note-frame-border-width
                               'standard-value))))))

(defun niwa-note-frame-test--rgb (name &optional _frame)
  "Resolve hex NAME as a graphic frame would; batch maps it to white."
  (mapcar (lambda (i) (/ (string-to-number (substring name i (+ i 2)) 16) 255.0))
          '(1 3 5)))

(defun niwa-note-frame-test--derive (foreground background)
  "Return (ACTIVE . INACTIVE) border colours derived from FOREGROUND on BACKGROUND."
  (let ((niwa--theme-derived-faces nil))
    (cl-letf (((symbol-function 'niwa--color-frame) #'selected-frame)
              ((symbol-function 'face-attribute)
               (lambda (_face attr &rest _)
                 (if (eq attr :foreground) foreground background))))
      (let (applied)
        (cl-letf (((symbol-function 'set-face-attribute)
                   (lambda (face _frame &rest attrs)
                     (push (cons face (plist-get attrs :background)) applied))))
          (niwa-note-frame--register-border-faces)
          (niwa--derive-theme-faces))
        (cons (alist-get 'niwa-note-frame-border applied)
              (alist-get 'niwa-note-frame-border-inactive applied))))))

(ert-deftest niwa-note-frame-border-derives-from-theme ()
  "Active contrasts with the background more than inactive, light and dark."
  (dolist (pair '(("#bbc2cf" . "#282c34") ("#24292e" . "#ffffff")))
   (cl-letf (((symbol-function 'color-name-to-rgb)
              #'niwa-note-frame-test--rgb))
    (pcase-let* ((`(,active . ,inactive)
                  (niwa-note-frame-test--derive (car pair) (cdr pair)))
                 (bg (cdr pair)))
      (should (stringp active))
      (should-not (equal active inactive))
      (should (> (niwa--contrast active bg)
                 (+ 0.5 (niwa--contrast inactive bg))))
      (should (> (niwa--contrast inactive bg) 1.1))))))

(ert-deftest niwa-note-frame-border-leaves-customised-face-alone ()
  (put 'niwa-note-frame-border 'customized-face '((t :background "red")))
  (unwind-protect
      (cl-letf (((symbol-function 'color-name-to-rgb)
                 #'niwa-note-frame-test--rgb))
        (should-not (car (niwa-note-frame-test--derive "#ffffff" "#000000"))))
    (put 'niwa-note-frame-border 'customized-face nil)))

(ert-deftest niwa-note-frame-border-repaints-on-theme-change ()
  "A shown frame is repainted when the theme changes."
  (let ((painted nil))
    (cl-letf (((symbol-function 'niwa-note-frame--paint-border)
               (lambda (&optional frame &rest _) (push (list 'note frame) painted)))
              ((symbol-function 'niwa-collaboration--paint-share-border)
               (lambda (frame) (push (list 'share frame) painted))))
      (let ((niwa-note-frame--frame (selected-frame))
            (niwa-collaboration--share-frame (selected-frame)))
        (niwa-note-frame--repaint-borders 'some-theme)))
    (should (member (list 'note (selected-frame)) painted))
    (should (member (list 'share (selected-frame)) painted))))

;;; test-niwa-note-frame.el ends here
