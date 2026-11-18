;;; test-mevedel-note-frame.el --- Note frame tests -*- lexical-binding: t -*-

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
(require 'mevedel-agent-conversation)
(require 'mevedel-agents)
(require 'mevedel-chat)
(require 'mevedel-note-frame)
(require 'mevedel-file-state)
(require 'mevedel-goal)
(require 'mevedel-hooks)
(require 'mevedel-mentions)
(require 'mevedel-menu)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-review)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-media)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair)
(require 'mevedel-tool-task)
(require 'mevedel-tool-ui)
(require 'mevedel-transcript)
(require 'mevedel-transcript-restore)
(require 'mevedel-view)
(require 'mevedel-view-audit)
(require 'mevedel-view-composer)
(require 'mevedel-view-history)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-view-zone)
(require 'mevedel-workspace)

;; Bound by the session layer at runtime; the view fixture renders without it.
(defvar mevedel-session--read-only-mode nil)

(defun mevedel-note-frame-test--reset ()
  "Clear module state so no test leaks a frame."
  ;; Only ever delete a real child frame.  A test may point the variable at
  ;; the initial frame to exercise a branch, and deleting that fails.
  (when (and (frame-live-p mevedel-note-frame--frame)
             (frame-parent mevedel-note-frame--frame))
    (delete-frame mevedel-note-frame--frame))
  (when (markerp mevedel-note-frame--origin)
    (set-marker mevedel-note-frame--origin nil))
  (mevedel-note-frame--unfollow)
  (setq mevedel-note-frame--pending-restore nil
        mevedel-note-frame--frame nil
        mevedel-note-frame--view-buffer nil
        mevedel-note-frame--note-id nil
        mevedel-note-frame--origin nil
        mevedel-note-frame--origin-window nil))

(defun mevedel-note-frame-test--note (start end &optional id)
  "Return a note overlay from START to END in the current buffer.
ID is the note number, 1 by default."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'mevedel-instruction t)
    (overlay-put overlay 'mevedel-instruction-type 'note)
    (overlay-put overlay 'mevedel-id (or id 1))
    overlay))

(defmacro mevedel-note-frame-test--with-view (&rest body)
  "Run BODY with `view-buf' holding two rendered turns.
BODY runs inside the view buffer."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((root (file-name-as-directory
                   (make-temp-file "mevedel-note-frame-" t)))
            (workspace (mevedel-workspace--create
                        :type 'file :id root :root root :name "frame"
                        :file-cache (mevedel-test-file-cache-create)))
            (session (mevedel-session-create "frame" workspace root)))
       (unwind-protect
           (progn
             (with-current-buffer data-buf
               (setq-local default-directory root
                           mevedel--workspace workspace
                           mevedel--session session)
               (insert "First prompt\n")
               (insert (propertize "First answer\n" 'gptel 'response))
               (insert "Second prompt\n")
               (insert (propertize "Second answer\n" 'gptel 'response)))
             (with-current-buffer view-buf
               (setq-local mevedel--session session)
               (mevedel-view--full-rerender)
              (should (> (length (mevedel-view--rendered-turn-starts)) 1))
               ,@body))
         (mevedel-note-frame-test--reset)
         (delete-directory root t)))))

(mevedel-deftest mevedel-note-frame--available-p
  (:doc "`mevedel-note-frame--available-p' gates on a usable display")
  ,test
  (test)
  :doc "reports unavailable in batch, where `make-frame' has no terminal"
  (let ((noninteractive t))
    (should-not (mevedel-note-frame--available-p)))

  :doc "reports available in an interactive session, graphical or not"
  (let ((noninteractive nil))
    (should (mevedel-note-frame--available-p))))

(mevedel-deftest mevedel-note-frame-display
  (:doc "`mevedel-note-frame-display' creates and reuses note frames")
  ,test
  (test)
  :doc "reuses a live frame already showing the view instead of rebuilding it"
  (mevedel-note-frame-test--with-view
    (let ((view (current-buffer))
          (frames (length (frame-list)))
          (source (generate-new-buffer " *test-source*")))
      (unwind-protect
          (let ((note (with-current-buffer source
                        (insert "note body\n")
                        (mevedel-note-frame-test--note
                         (point-min) (point-max) 3))))
            (setq mevedel-note-frame--frame (selected-frame)
                  mevedel-note-frame--view-buffer view
                  mevedel-note-frame--note-id "Note 3")
            (cl-letf (((symbol-function 'mevedel-note-frame--available-p)
                       (lambda () t))
                      ;; Terminal note frames fall back to a side window.
                      ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                      ((symbol-function 'mevedel-note-frame--make)
                       (lambda (&rest _)
                         (error "Rebuilt a frame that was already showing the view"))))
              (should (eq (selected-frame)
                          (mevedel-note-frame-display note view))))
            (should (= frames (length (frame-list)))))
        (kill-buffer source))))

  :doc "same-view reuse retargets identity, origin and teardown handoff"
  (mevedel-note-frame-test--with-view
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
              (setq note-a (mevedel-note-frame-test--note 1 6 1)
                    note-b (mevedel-note-frame-test--note 7 11 2)))
            (save-window-excursion
              (set-window-buffer (selected-window) source)
              (cl-letf (((symbol-function
                          'mevedel-note-frame--available-p)
                         (lambda () t))
                        ((symbol-function 'display-graphic-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'mevedel-note-frame--anchor)
                         (lambda (&rest _) '(10 . 20)))
                        ((symbol-function 'mevedel-note-frame--make)
                         (lambda (&rest _)
                           (cl-incf makes)
                           (selected-frame)))
                        ((symbol-function 'mevedel-note-frame--size)
                         (lambda (&rest _) '(100 . 50)))
                        ((symbol-function 'make-frame-visible) #'ignore)
                        ((symbol-function 'select-frame-set-input-focus)
                         #'ignore)
                        ((symbol-function 'set-frame-position) #'ignore)
                        ((symbol-function 'set-frame-size) #'ignore))
                (mevedel-note-frame-display note-a view)
                (mevedel-note-frame-display note-b view)
                (should (= 2 makes))
                (should (equal "Note 2" mevedel-note-frame--note-id))
                (should (eq note-b
                            mevedel-note-frame--note))
                (should (= (overlay-start note-b)
                           (marker-position
                            mevedel-note-frame--origin)))
                (mevedel-test--with-captured-messages nil
                  (mevedel-note-frame--on-delete (selected-frame)))
                (should
                 (= (marker-position
                     (plist-get mevedel-note-frame--pending-restore
                                :origin))
                    (overlay-start note-b)))
                (with-current-buffer source
                  (mevedel-note-frame--after-delete)))))
        (when (buffer-live-p source)
          (with-current-buffer source
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer source))
        (when (file-exists-p file) (delete-file file))))))

(mevedel-deftest mevedel-note-frame--source-window
  (:doc "`mevedel-note-frame--source-window' never returns the frame itself")
  ,test
  (test)
  :doc "returns the window showing the note's source buffer"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (mevedel-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (should (eq (selected-window)
                      (mevedel-note-frame--source-window note))))
      (kill-buffer buffer)))

  :doc "prefers the selected window when the buffer is on screen twice"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (mevedel-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((first (selected-window))
                (other (split-window)))
            (set-window-buffer other buffer)
            (select-window other)
            (should (eq other
                        (mevedel-note-frame--source-window note)))
            (select-window first)
            (should (eq first
                        (mevedel-note-frame--source-window note)))))
      (kill-buffer buffer)
      (mevedel-note-frame-test--reset)))

  :doc "excludes windows inside the note frame, which would make it \
its own parent"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (mevedel-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((mevedel-note-frame--frame (selected-frame)))
            (should-not
             (mevedel-note-frame--source-window note))))
      (kill-buffer buffer)
      (mevedel-note-frame-test--reset))))

(mevedel-deftest mevedel-note-frame--place
  (:doc "`mevedel-note-frame--place' keeps the note readable")
  ,test
  (test)
  :doc "opens below the note's last line when it fits"
  (should (equal '(300 . 298)
                 (mevedel-note-frame--place
                  300 100 280 18 1200 360 2000 1060)))
  :doc "clamps x so the frame stays inside the parent's right edge"
  (should (= 800 (car (mevedel-note-frame--place
                       1500 100 280 18 1200 360 2000 1060))))
  :doc "flips above the first line, not the last, so the note stays visible"
  (let ((y (cdr (mevedel-note-frame--place
                 300 520 880 18 1200 360 2000 1060))))
    (should (= 160 y))
    (should (<= (+ y 360) 520)))
  :doc "sits flush above a note when fitted shorter than the maximum"
  (should (= 376 (cdr (mevedel-note-frame--place
                       300 520 940 18 1200 144 2000 1060))))
  :doc "takes the larger side, clamped, only when neither side has room"
  (should (= 660 (cdr (mevedel-note-frame--place
                       300 200 700 18 1200 400 2000 1060))))
  (should (= 0 (cdr (mevedel-note-frame--place
                     300 500 900 18 1200 600 2000 1060)))))

(mevedel-deftest mevedel-note-frame--fit-height
  (:doc "`mevedel-note-frame--fit-height' never resizes width")
  ,test
  (test)
  :doc "fits vertically only, so a long line cannot outgrow the parent frame"
  (let (captured)
    (unwind-protect
        (cl-letf (((symbol-function 'fit-frame-to-buffer)
                   (lambda (&rest args) (setq captured args))))
          (let ((mevedel-note-frame--frame (selected-frame)))
            (mevedel-note-frame--fit-height)))
      (mevedel-note-frame-test--reset))
    (should captured)
    (should (eq 'vertically (nth 5 captured))))

  :doc "does nothing when no frame is live"
  (let (called)
    (cl-letf (((symbol-function 'fit-frame-to-buffer)
               (lambda (&rest _) (setq called t))))
      (let ((mevedel-note-frame--frame nil))
        (mevedel-note-frame--fit-height)))
    (should-not called)))

(mevedel-deftest mevedel-note-frame--follow-setup
  (:doc "`mevedel-note-frame--follow-setup' tracks the source buffer")
  ,test
  (test)
  :doc "installs local follow hooks and removes them on teardown"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (note (with-current-buffer buffer
                 (insert "note body\n")
                 (mevedel-note-frame-test--note (point-min) (point-max)))))
    (unwind-protect
        (progn
          (mevedel-note-frame--follow-setup note)
          (with-current-buffer buffer
            (should (memq #'mevedel-note-frame--follow
                          window-scroll-functions))
            (should (memq #'mevedel-note-frame--follow
                          window-configuration-change-hook)))
          (should (eq note mevedel-note-frame--note))
          (mevedel-note-frame--unfollow)
          (with-current-buffer buffer
            (should-not (memq #'mevedel-note-frame--follow
                              window-scroll-functions))
            (should-not (memq #'mevedel-note-frame--follow
                              window-configuration-change-hook)))
          (should-not mevedel-note-frame--note)
          (should-not mevedel-note-frame--source-buffer))
      (kill-buffer buffer)
      (mevedel-note-frame-test--reset))))

(mevedel-deftest mevedel-note-frame--header
  (:doc "`mevedel-note-frame--header' leads with note identity")
  ,test
  (test)
  :doc "keeps the full note display number and live request state"
  (mevedel-note-frame-test--with-view
    (let* ((file (file-name-concat root "source.el"))
           source)
      (unwind-protect
          (progn
            (write-region "noted code\n" nil file nil 'silent)
            (setq source (find-file-noselect file))
            (with-current-buffer source
              (let ((note (mevedel-note-frame-test--note 1 11 1234)))
                (setq mevedel-note-frame--note-id
                      (format "Note %s" (overlay-get note 'mevedel-id)))))
            (let ((header (substring-no-properties (mevedel-note-frame--header))))
              (should (string-search "Note 1234" header))
              (should (string-search "tool" header))
              (should (string-search "C-c C-z close" header))))
        (when (buffer-live-p source) (kill-buffer source))))))

(mevedel-deftest mevedel-note-frame--display-in-parent
  (:doc "`mevedel-note-frame--display-in-parent' keeps pop-ups out of the frame")
  ,test
  (test)
  :doc "declines when no note frame is live, so display falls through"
  (let ((mevedel-note-frame--frame nil))
    (should-not
     (mevedel-note-frame--display-in-parent (current-buffer) nil)))

  :doc "declines for a frame with no parent rather than displaying into it"
  (let ((mevedel-note-frame--frame (selected-frame)))
    (should-not
     (mevedel-note-frame--display-in-parent (current-buffer) nil)))

  :doc "re-wraps the alist as an action, which a bare alist would break"
  (let ((buffer (generate-new-buffer " *test-popped*")))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (&rest _) (selected-frame))))
            (let ((mevedel-note-frame--frame (selected-frame)))
              ;; The alist `pop-to-buffer' passes: as a bare second argument
              ;; display-buffer reads (inhibit-same-window . t) as an action
              ;; function and signals wrong-type-argument.
              (should (window-live-p
                       (mevedel-note-frame--display-in-parent
                        buffer '((inhibit-same-window . t))))))))
      (kill-buffer buffer)
      (mevedel-note-frame-test--reset))))

(mevedel-deftest mevedel-note-frame-mode
  (:doc "`mevedel-note-frame-mode' redirects buffer display while on")
  ,test
  (test)
  :doc "installs and removes the buffer-local display override"
  (mevedel-note-frame-test--with-view
    (mevedel-note-frame-mode 1)
    (should (local-variable-p 'display-buffer-overriding-action))
    (should (eq 'mevedel-note-frame--display-in-parent
                (car display-buffer-overriding-action)))
    (mevedel-note-frame-mode -1)
    (should-not (local-variable-p 'display-buffer-overriding-action))))

(mevedel-deftest mevedel-note-frame--on-delete
  (:doc "`mevedel-note-frame--on-delete' releases every frame resource")
  ,test
  (test)
  :doc "clears module state for the deleted frame"
  (mevedel-note-frame-test--with-view
    (setq mevedel-note-frame--view-buffer (current-buffer)
          mevedel-note-frame--note-id "Note 1"
          mevedel-note-frame--frame (selected-frame))
    (mevedel-note-frame-mode 1)
    (mevedel-note-frame--on-delete (selected-frame))
    (should-not mevedel-note-frame-mode)
    (should-not mevedel-note-frame--frame)
    (should-not mevedel-note-frame--view-buffer)
    (should-not mevedel-note-frame--note-id))

  :doc "ignores a frame that is not the note frame"
  (mevedel-note-frame-test--with-view
    (setq mevedel-note-frame--view-buffer (current-buffer)
          mevedel-note-frame--note-id "Note 1"
          mevedel-note-frame--frame nil)
    (mevedel-note-frame--on-delete (selected-frame))
    (should (equal "Note 1" mevedel-note-frame--note-id)))

  :doc "hands focus and point to the post-deletion phase instead of restoring \
them while the frame is still alive"
  (mevedel-note-frame-test--with-view
    (setq mevedel-note-frame--view-buffer (current-buffer)
          mevedel-note-frame--frame (selected-frame)
          mevedel-note-frame--origin-window (selected-window))
    (mevedel-note-frame--on-delete (selected-frame))
    (should mevedel-note-frame--pending-restore)
    (should (eq (selected-window)
                (plist-get mevedel-note-frame--pending-restore :window)))))

(mevedel-deftest mevedel-note-frame--after-delete
  (:doc "`mevedel-note-frame--after-delete' settles focus once the frame is gone")
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
            (setq mevedel-note-frame--pending-restore
                  (list :frame (selected-frame)
                        :window (selected-window)
                        :origin origin))
            (mevedel-note-frame--after-delete)
            (should-not mevedel-note-frame--pending-restore)
            (should (= 5 (with-current-buffer buffer (point))))
            (should-not (marker-position origin))))
      (kill-buffer buffer)
      (mevedel-note-frame-test--reset)))

  :doc "does nothing when no deletion handed anything over"
  (let ((mevedel-note-frame--pending-restore nil))
    (mevedel-note-frame--after-delete)
    (should-not mevedel-note-frame--pending-restore)))

(provide 'test-mevedel-note-frame)


(ert-deftest mevedel-note-frame-border-is-prominent ()
  "The border defaults to 3 px; active and inactive faces stay distinct."
  (should (= 3 (eval (car (get 'mevedel-note-frame-border-width
                               'standard-value)))))
  (dolist (bg '(dark light))
    (let (colors)
      (dolist (face '(mevedel-note-frame-border
                      mevedel-note-frame-border-inactive))
        (let ((spec (cdr (assq bg (mapcar (lambda (e) (cons (cadr (caar e)) (cdr e)))
                                          (get face 'face-defface-spec))))))
          (push (plist-get spec :background) colors)))
      (should (cl-every #'stringp colors))
      (should-not (equal (car colors) (cadr colors))))))

;;; test-mevedel-note-frame.el ends here
