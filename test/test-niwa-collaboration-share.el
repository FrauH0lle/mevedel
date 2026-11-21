;;; test-niwa-collaboration-share.el --- Collaboration share surface tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises bearer-link rendering, QR fallback, child-frame geometry,
;; copy commands, and room-scoped dismissal for the share surface.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'niwa-collaboration-share)
(require 'qrencode)

(niwa-deftest niwa-collaboration--share-content
  (:doc "renders the selected link's QR, scope, legend, and encoder fallback")
  (let* ((room (list :session-label "share"
                     :key "raw-key"
                     :link-view "http://x/#room.view-secret"
                     :link-full "http://x/#room.full-secret"))
         (view (niwa-collaboration--share-content room 'view))
         (full (niwa-collaboration--share-content room 'full)))
    (should (string-match-p "room\\.view-secret" view))
    (should-not (string-match-p "room\\.full-secret" view))
    (should (string-match-p "read-only" view))
    (should (string-match-p "room\\.full-secret" full))
    (should-not (string-match-p "room\\.view-secret" full))
    (should (string-match-p "FULL CONTROL" full))
    (should (string-match-p "█" view))
    (should-not (equal view full))
    (dolist (content (list view full))
      (should-not (string-match-p "raw-key" content))
      (should (string-match-p "TAB" content))
      (should (string-match-p "bearer" content)))
    (cl-letf (((symbol-function 'qrencode)
               (lambda (&rest _) (error "No QR for you"))))
      (let ((degraded (niwa-collaboration--share-content room 'view)))
        (should (string-match-p "room\\.view-secret" degraded))
        (should (string-match-p "QR unavailable" degraded))
        (should (string-match-p "TAB" degraded))))))

(niwa-deftest niwa-collaboration--share-render
  (:doc "replaces the share buffer from its room and selected link")
  (with-temp-buffer
    (let ((niwa-collaboration--share-room 'room)
          (niwa-collaboration--share-which 'full))
      (insert "stale")
      (setq buffer-read-only t)
      (cl-letf (((symbol-function 'niwa-collaboration--share-content)
                 (lambda (room which) (format "%s:%s" room which))))
        (niwa-collaboration--share-render))
      (should (equal "room:full" (buffer-string)))
      (should (= (point-min) (point))))))

(niwa-deftest niwa-collaboration--share-next-tier
  (:doc "cycles all bearer tiers and falls back to the safest tier")
  (progn
    (should (eq 'full (niwa-collaboration--share-next-tier 'view)))
    (should (eq 'owner (niwa-collaboration--share-next-tier 'full)))
    (should (eq 'view (niwa-collaboration--share-next-tier 'owner)))
    (should (eq 'view (niwa-collaboration--share-next-tier 'unknown)))))

(niwa-deftest niwa-collaboration-share-toggle
  (:doc "cycles the three bearer tiers in ascending authority and redraws")
  (let ((niwa-collaboration--share-which 'view)
        rendered)
    (cl-letf (((symbol-function 'niwa-collaboration--share-render)
               (lambda () (setq rendered t))))
      (niwa-collaboration-share-toggle)
      (should (eq 'full niwa-collaboration--share-which))
      (should rendered)
      (niwa-collaboration-share-toggle)
      (should (eq 'owner niwa-collaboration--share-which))
      ;; Wrapping lands on the safest tier, never on the strongest.
      (niwa-collaboration-share-toggle)
      (should (eq 'view niwa-collaboration--share-which)))))

(niwa-deftest niwa-collaboration-share-copy-owner
  (:doc "copies the owner bearer link")
  (let ((niwa-collaboration--share-room '(:link-owner "owner-link"))
        copied)
    (cl-letf (((symbol-function 'kill-new) (lambda (text) (setq copied text)))
              ((symbol-function 'message) #'ignore))
      (niwa-collaboration-share-copy-owner))
    (should (equal "owner-link" copied))))

(niwa-deftest niwa-collaboration-share-copy-view
  (:doc "copies the view-only bearer link")
  (let ((niwa-collaboration--share-room '(:link-view "view-link"))
        copied)
    (cl-letf (((symbol-function 'kill-new) (lambda (text) (setq copied text)))
              ((symbol-function 'message) #'ignore))
      (niwa-collaboration-share-copy-view))
    (should (equal "view-link" copied))))

(niwa-deftest niwa-collaboration-share-copy-full
  (:doc "copies the full-control bearer link")
  (let ((niwa-collaboration--share-room '(:link-full "full-link"))
        copied)
    (cl-letf (((symbol-function 'kill-new) (lambda (text) (setq copied text)))
              ((symbol-function 'message) #'ignore))
      (niwa-collaboration-share-copy-full))
    (should (equal "full-link" copied))))

(niwa-deftest niwa-collaboration--fit-share-frame
  (:doc "fits the share frame to bounded pixel content and border")
  (let (size)
    (cl-letf (((symbol-function 'frame-root-window) (lambda (_) 'window))
              ((symbol-function 'frame-parameter) (lambda (&rest _) 4))
              ((symbol-function 'frame-pixel-width) (lambda (_) 500))
              ((symbol-function 'frame-pixel-height) (lambda (_) 400))
              ((symbol-function 'window-text-pixel-size)
               (lambda (&rest _) '(300 . 200)))
              ((symbol-function 'set-frame-size)
               (lambda (_frame width height pixelwise)
                 (setq size (list width height pixelwise)))))
      (niwa-collaboration--fit-share-frame 'frame 'parent))
    (should (equal '(308 208 t) size))))

(niwa-deftest niwa-collaboration--center-frame
  (:doc "centres the share frame within its parent")
  (let (position)
    (cl-letf (((symbol-function 'frame-pixel-width)
               (lambda (frame) (if (eq frame 'frame) 200 600)))
              ((symbol-function 'frame-pixel-height)
               (lambda (frame) (if (eq frame 'frame) 100 400)))
              ((symbol-function 'set-frame-position)
               (lambda (_frame left top) (setq position (list left top)))))
      (niwa-collaboration--center-frame 'frame 'parent))
    (should (equal '(200 150) position))))

(niwa-deftest niwa-collaboration--show-share-frame
  (:doc "falls back to a window when child-frame creation signals")
  (let ((room (list :session-label "share"
                    :link-view "http://x/#room.view"
                    :link-full "http://x/#room.full"))
        popped)
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _) t))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _)
                     (error "Face inheritance results in inheritance cycle")))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq popped buffer))))
          (niwa-collaboration--show-share-frame room)
          (should (buffer-live-p popped))
          (should (equal "*niwa share*" (buffer-name popped))))
      (when-let* ((buffer (get-buffer "*niwa share*")))
        (kill-buffer buffer)))))

(niwa-deftest niwa-collaboration-share-quit
  (:doc "closes the tracked frame and its fallback buffer")
  (let ((buffer (get-buffer-create "*niwa share*"))
        (niwa-collaboration--share-frame 'share-frame)
        deleted)
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'delete-frame)
               (lambda (frame &optional _) (setq deleted frame))))
      (niwa-collaboration-share-quit))
    (should (eq 'share-frame deleted))
    (should-not niwa-collaboration--share-frame)
    (should-not (buffer-live-p buffer))))

(niwa-deftest niwa-collaboration-share-present
  (:doc "copies the full link, opens the surface, and keeps links out of messages")
  (let ((room (list :link-full "http://x/#room.full"
                    :link-view "http://x/#room.view"
                    :session-label "share"
                    :key "raw-key"))
        killed shown messages)
    (cl-letf (((symbol-function 'kill-new)
               (lambda (text) (setq killed text)))
              ((symbol-function 'niwa-collaboration--show-share-frame)
               (lambda (shown-room) (setq shown shown-room)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (niwa-collaboration-share-present room))
    (should (equal "http://x/#room.full" killed))
    (should (eq room shown))
    (should-not (string-match-p "room\\.full" (car messages)))
    (should-not (string-match-p "room\\.view" (car messages)))
    (should-not (string-match-p "raw-key" (car messages)))))

(niwa-deftest niwa-collaboration-share-dismiss
  (:doc "closes only the surface presenting the given room")
  (let ((room (list :room-id "presented"))
        (other (list :room-id "other"))
        (buffer (get-buffer-create "*niwa share*"))
        closed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa-collaboration--share-room room))
          (cl-letf (((symbol-function 'niwa-collaboration-share-quit)
                     (lambda () (setq closed t))))
            (niwa-collaboration-share-dismiss other)
            (should-not closed)
            (niwa-collaboration-share-dismiss room)
            (should closed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test-niwa-collaboration-share)
;;; test-niwa-collaboration-share.el ends here
