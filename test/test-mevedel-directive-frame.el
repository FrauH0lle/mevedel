;;; test-mevedel-directive-frame.el --- Directive frame tests -*- lexical-binding: t -*-

;;; Commentary:

;; Child frame creation is not assertable in batch, so these cover the
;; display-independent halves: the availability gate, turn classification,
;; transcript filtering, and teardown.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel-agent-tools)
(require 'mevedel-agent-conversation)
(require 'mevedel-agents)
(require 'mevedel-chat)
(require 'mevedel-directive)
(require 'mevedel-directive-frame)
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

(defun mevedel-directive-frame-test--reset ()
  "Clear module state so no test leaks a frame or a filter."
  ;; Only ever delete a real child frame.  A test may point the variable at
  ;; the initial frame to exercise a branch, and deleting that fails.
  (when (and (frame-live-p mevedel-directive-frame--frame)
             (frame-parent mevedel-directive-frame--frame))
    (delete-frame mevedel-directive-frame--frame))
  (when (markerp mevedel-directive-frame--origin)
    (set-marker mevedel-directive-frame--origin nil))
  (mevedel-directive-frame--unfollow)
  (setq mevedel-directive-frame--pending-restore nil
        mevedel-directive-frame--frame nil
        mevedel-directive-frame--view-buffer nil
        mevedel-directive-frame--directive-id nil
        mevedel-directive-frame--origin nil
        mevedel-directive-frame--origin-window nil
        mevedel-directive-frame--filtered-p nil))

(defmacro mevedel-directive-frame-test--with-view (&rest body)
  "Run BODY with `view-buf' holding two rendered turns.
The second turn is tagged for directive \"d1\", as the renderer tags a
directive turn.  BODY runs inside the view buffer."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((root (file-name-as-directory
                   (make-temp-file "mevedel-directive-frame-" t)))
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
               (let* ((starts (mevedel-view--rendered-turn-starts))
                      (last-start (car (last starts)))
                      (limit (mevedel-view--input-marker-position)))
                 (should (> (length starts) 1))
                 (with-silent-modifications
                   (let ((inhibit-read-only t))
                     (put-text-property
                      last-start limit 'mevedel-view-directive
                      (list :directive-id "d1")))))
               ,@body))
         (mevedel-directive-frame-test--reset)
         (delete-directory root t)))))

(mevedel-deftest mevedel-directive-frame--available-p
  (:doc "`mevedel-directive-frame--available-p' gates on child frame support")
  ,test
  (test)
  :doc "reports unavailable on a non-graphical Emacs 30"
  (let ((emacs-major-version 30))
    (unless (display-graphic-p)
      (should-not (mevedel-directive-frame--available-p))))

  :doc "reports available on Emacs 31, which supports terminal child frames"
  (let ((emacs-major-version 31))
    (should (mevedel-directive-frame--available-p))))

(mevedel-deftest mevedel-directive-frame--turn-spans
  (:doc "`mevedel-directive-frame--turn-spans' classifies rendered turns")
  ,test
  (test)
  :doc "attributes each turn to its directive, or to nil for ordinary chat"
  (mevedel-directive-frame-test--with-view
    (let ((spans (mevedel-directive-frame--turn-spans)))
      (should (> (length spans) 1))
      (should-not (cddr (car spans)))
      (should (equal "d1" (cddr (car (last spans)))))))

  :doc "leaves content before the first turn, such as the header, uncovered"
  (mevedel-directive-frame-test--with-view
    (let ((spans (mevedel-directive-frame--turn-spans)))
      (should (> (car (car spans)) (point-min))))))

(mevedel-deftest mevedel-directive-frame--apply-filter
  (:doc "`mevedel-directive-frame--apply-filter' hides other directives' turns")
  ,test
  (test)
  :doc "hides non-matching turns and leaves the framed directive visible"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (mevedel-directive-frame--apply-filter)
    (should mevedel-directive-frame--filtered-p)
    (let ((spans (mevedel-directive-frame--turn-spans)))
      (should (eq mevedel-directive-frame--invisible-symbol
                  (get-text-property (car (car spans)) 'invisible)))
      (should-not
       (get-text-property (car (car (last spans))) 'invisible)))
    (should (memq mevedel-directive-frame--invisible-symbol
                  buffer-invisibility-spec)))

  :doc "leaves the header visible"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (mevedel-directive-frame--apply-filter)
    (should-not (get-text-property (point-min) 'invisible)))

  :doc "does nothing when the view is displayed outside the frame"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (mevedel-directive-frame--apply-filter)
      (should-not mevedel-directive-frame--filtered-p)))

  :doc "does nothing when filtering is disabled"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (let ((mevedel-directive-frame-filter nil))
      (mevedel-directive-frame--apply-filter)
      (should-not mevedel-directive-frame--filtered-p))))

(mevedel-deftest mevedel-directive-frame--clear-filter
  (:doc "`mevedel-directive-frame--clear-filter' restores the whole transcript")
  ,test
  (test)
  :doc "removes both the invisible properties and the spec entry"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (mevedel-directive-frame--apply-filter)
    (mevedel-directive-frame--clear-filter)
    (should-not mevedel-directive-frame--filtered-p)
    (should-not (memq mevedel-directive-frame--invisible-symbol
                      buffer-invisibility-spec))
    (let ((spans (mevedel-directive-frame--turn-spans)))
      (should-not (get-text-property (car (car spans)) 'invisible)))))

(mevedel-deftest mevedel-directive-frame--filter-elsewhere-p
  (:doc "`mevedel-directive-frame--filter-elsewhere-p' detects other windows")
  ,test
  (test)
  :doc "is nil when the view is displayed nowhere"
  (mevedel-directive-frame-test--with-view
    (should-not
     (mevedel-directive-frame--filter-elsewhere-p (current-buffer))))

  :doc "is non-nil when an ordinary window shows the view"
  (mevedel-directive-frame-test--with-view
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (should
       (mevedel-directive-frame--filter-elsewhere-p (current-buffer))))))

(mevedel-deftest mevedel-directive-frame-display
  (:doc "`mevedel-directive-frame-display' degrades without child frames")
  ,test
  (test)
  :doc "falls back to an ordinary window and creates no frame"
  (mevedel-directive-frame-test--with-view
    (let ((frames (length (frame-list)))
          (view (current-buffer)))
      (save-window-excursion
        (cl-letf (((symbol-function 'mevedel-directive-frame--available-p)
                   #'ignore))
          (mevedel-directive-frame-display nil view))
        (should (get-buffer-window view t)))
      (should (= frames (length (frame-list))))
      (should-not mevedel-directive-frame--frame)))

  :doc "reuses a live frame already showing the view instead of rebuilding it"
  (mevedel-directive-frame-test--with-view
    (let ((view (current-buffer))
          (frames (length (frame-list))))
      (setq mevedel-directive-frame--frame (selected-frame)
            mevedel-directive-frame--view-buffer view)
      (cl-letf (((symbol-function 'mevedel-directive-frame--available-p)
                 (lambda () t))
                ((symbol-function 'mevedel-directive-frame--make)
                 (lambda (&rest _)
                   (error "Rebuilt a frame that was already showing the view"))))
        (should (eq (selected-frame)
                    (mevedel-directive-frame-display nil view))))
      (should (= frames (length (frame-list)))))))

(mevedel-deftest mevedel-directive-frame--source-window
  (:doc "`mevedel-directive-frame--source-window' never returns the frame itself")
  ,test
  (test)
  :doc "returns the window showing the directive's source buffer"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (directive (with-current-buffer buffer
                      (insert "directive body\n")
                      (make-overlay (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (should (eq (selected-window)
                      (mevedel-directive-frame--source-window directive))))
      (kill-buffer buffer)))

  :doc "prefers the selected window when the buffer is on screen twice"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (directive (with-current-buffer buffer
                      (insert "directive body\n")
                      (make-overlay (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((first (selected-window))
                (other (split-window)))
            (set-window-buffer other buffer)
            (select-window other)
            (should (eq other
                        (mevedel-directive-frame--source-window directive)))
            (select-window first)
            (should (eq first
                        (mevedel-directive-frame--source-window directive)))))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset)))

  :doc "excludes windows inside the directive frame, which would make it \
its own parent"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (directive (with-current-buffer buffer
                      (insert "directive body\n")
                      (make-overlay (point-min) (point-max)))))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((mevedel-directive-frame--frame (selected-frame)))
            (should-not
             (mevedel-directive-frame--source-window directive))))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset))))

(mevedel-deftest mevedel-directive-frame--fit-height
  (:doc "`mevedel-directive-frame--fit-height' never resizes width")
  ,test
  (test)
  :doc "fits vertically only, so a long line cannot outgrow the parent frame"
  (let (captured)
    (unwind-protect
        (cl-letf (((symbol-function 'fit-frame-to-buffer)
                   (lambda (&rest args) (setq captured args))))
          (let ((mevedel-directive-frame--frame (selected-frame)))
            (mevedel-directive-frame--fit-height)))
      (mevedel-directive-frame-test--reset))
    (should captured)
    (should (eq 'vertically (nth 5 captured))))

  :doc "does nothing when no frame is live"
  (let (called)
    (cl-letf (((symbol-function 'fit-frame-to-buffer)
               (lambda (&rest _) (setq called t))))
      (let ((mevedel-directive-frame--frame nil))
        (mevedel-directive-frame--fit-height)))
    (should-not called)))

(mevedel-deftest mevedel-directive-frame--follow-setup
  (:doc "`mevedel-directive-frame--follow-setup' tracks the source buffer")
  ,test
  (test)
  :doc "installs scroll and configuration hooks locally in the source buffer"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (directive (with-current-buffer buffer
                      (insert "directive body\n")
                      (make-overlay (point-min) (point-max)))))
    (unwind-protect
        (progn
          (mevedel-directive-frame--follow-setup directive)
          (with-current-buffer buffer
            (should (memq #'mevedel-directive-frame--follow
                          window-scroll-functions))
            (should (memq #'mevedel-directive-frame--follow
                          window-configuration-change-hook)))
          (should (eq directive mevedel-directive-frame--directive)))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset)))

  :doc "removes both hooks again, so a closed frame leaves nothing behind"
  (let* ((buffer (generate-new-buffer " *test-source*"))
         (directive (with-current-buffer buffer
                      (insert "directive body\n")
                      (make-overlay (point-min) (point-max)))))
    (unwind-protect
        (progn
          (mevedel-directive-frame--follow-setup directive)
          (mevedel-directive-frame--unfollow)
          (with-current-buffer buffer
            (should-not (memq #'mevedel-directive-frame--follow
                              window-scroll-functions))
            (should-not (memq #'mevedel-directive-frame--follow
                              window-configuration-change-hook)))
          (should-not mevedel-directive-frame--directive)
          (should-not mevedel-directive-frame--source-buffer))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset))))

(mevedel-deftest mevedel-directive-frame--header
  (:doc "`mevedel-directive-frame--header' leads with directive identity")
  ,test
  (test)
  :doc "shortens the directive id and keeps live request state"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--directive-id "f041faf5-1234-5678")
    (let ((header (substring-no-properties
                   (mevedel-directive-frame--header))))
      (should (string-search "f041faf5" header))
      (should-not (string-search "f041faf5-1234" header))
      (should (string-search "tool" header))))

  :doc "marks a filtered transcript so it is not mistaken for a short one"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1")
    (should-not (string-search "filtered"
                               (substring-no-properties
                                (mevedel-directive-frame--header))))
    (mevedel-directive-frame--apply-filter)
    (should (string-search "filtered"
                           (substring-no-properties
                            (mevedel-directive-frame--header))))))

(mevedel-deftest mevedel-directive-frame--display-in-parent
  (:doc "`mevedel-directive-frame--display-in-parent' keeps pop-ups out of the frame")
  ,test
  (test)
  :doc "declines when no directive frame is live, so display falls through"
  (let ((mevedel-directive-frame--frame nil))
    (should-not
     (mevedel-directive-frame--display-in-parent (current-buffer) nil)))

  :doc "declines for a frame with no parent rather than displaying into it"
  (let ((mevedel-directive-frame--frame (selected-frame)))
    (should-not
     (mevedel-directive-frame--display-in-parent (current-buffer) nil)))

  :doc "re-wraps the alist as an action, which a bare alist would break"
  (let ((buffer (generate-new-buffer " *test-popped*")))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (&rest _) (selected-frame))))
            (let ((mevedel-directive-frame--frame (selected-frame)))
              ;; The alist `pop-to-buffer' passes: as a bare second argument
              ;; display-buffer reads (inhibit-same-window . t) as an action
              ;; function and signals wrong-type-argument.
              (should (window-live-p
                       (mevedel-directive-frame--display-in-parent
                        buffer '((inhibit-same-window . t))))))))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset))))

(mevedel-deftest mevedel-directive-frame-mode
  (:doc "`mevedel-directive-frame-mode' redirects buffer display while on")
  ,test
  (test)
  :doc "installs and removes the buffer-local display override"
  (mevedel-directive-frame-test--with-view
    (mevedel-directive-frame-mode 1)
    (should (local-variable-p 'display-buffer-overriding-action))
    (should (eq 'mevedel-directive-frame--display-in-parent
                (car display-buffer-overriding-action)))
    (mevedel-directive-frame-mode -1)
    (should-not (local-variable-p 'display-buffer-overriding-action))))

(mevedel-deftest mevedel-directive-frame--on-delete
  (:doc "`mevedel-directive-frame--on-delete' releases every frame resource")
  ,test
  (test)
  :doc "clears filtering and module state for the deleted frame"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1"
          mevedel-directive-frame--frame (selected-frame))
    (mevedel-directive-frame--apply-filter)
    (should mevedel-directive-frame--filtered-p)
    (mevedel-directive-frame--on-delete (selected-frame))
    (should-not mevedel-directive-frame--filtered-p)
    (should-not mevedel-directive-frame--frame)
    (should-not mevedel-directive-frame--view-buffer)
    (should-not mevedel-directive-frame--directive-id))

  :doc "ignores a frame that is not the directive frame"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--directive-id "d1"
          mevedel-directive-frame--frame nil)
    (mevedel-directive-frame--on-delete (selected-frame))
    (should (equal "d1" mevedel-directive-frame--directive-id)))

  :doc "hands focus and point to the post-deletion phase instead of restoring \
them while the frame is still alive"
  (mevedel-directive-frame-test--with-view
    (setq mevedel-directive-frame--view-buffer (current-buffer)
          mevedel-directive-frame--frame (selected-frame)
          mevedel-directive-frame--origin-window (selected-window))
    (mevedel-directive-frame--on-delete (selected-frame))
    (should mevedel-directive-frame--pending-restore)
    (should (eq (selected-window)
                (plist-get mevedel-directive-frame--pending-restore :window)))))

(mevedel-deftest mevedel-directive-frame--after-delete
  (:doc "`mevedel-directive-frame--after-delete' settles focus once the frame is gone")
  ,test
  (test)
  :doc "returns point to the directive and consumes the handoff"
  (let* ((buffer (generate-new-buffer " *test-source*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "one\ntwo\nthree\n")
            (goto-char (point-max)))
          (let ((origin (with-current-buffer buffer (copy-marker 5))))
            (setq mevedel-directive-frame--pending-restore
                  (list :frame (selected-frame)
                        :window (selected-window)
                        :origin origin))
            (mevedel-directive-frame--after-delete)
            (should-not mevedel-directive-frame--pending-restore)
            (should (= 5 (with-current-buffer buffer (point))))
            (should-not (marker-position origin))))
      (kill-buffer buffer)
      (mevedel-directive-frame-test--reset)))

  :doc "does nothing when no deletion handed anything over"
  (let ((mevedel-directive-frame--pending-restore nil))
    (mevedel-directive-frame--after-delete)
    (should-not mevedel-directive-frame--pending-restore)))

(provide 'test-mevedel-directive-frame)

;;; test-mevedel-directive-frame.el ends here
