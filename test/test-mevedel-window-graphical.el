;;; test-mevedel-window-graphical.el --- Child-frame window returns -*- lexical-binding: t -*-

;;; Commentary:

;; Real directive-frame display redirection must restore the parent's layout
;; without replacing or losing the invoking child-frame composer.  Batch runs
;; skip these cases; frame-parent stubs cannot exercise cross-frame selection.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-directive-frame)
(require 'mevedel-menu)
(require 'mevedel-recovery)
(require 'mevedel-report)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-window)
(require 'transient)

(defun mevedel-window-graphical-test--layout (frame)
  "Return FRAME's topology, displayed buffers, edges and selected window."
  (list (window-tree frame)
        (mapcar (lambda (window)
                  (list window (window-buffer window) (window-edges window)))
                (window-list frame 'no-minibuf))
        (frame-selected-window frame)))

(defmacro mevedel-window-graphical-test--with-child (layout &rest body)
  "Run BODY from a real child composer over the parent's LAYOUT.
Bind parent, child, origin, parent-before and child-before for assertions."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (mevedel-view-test--with-buffers
       (let* ((parent (selected-frame))
              (workspace (mevedel-workspace--create
                          :type 'file :id "window-graphical"
                          :name "window-graphical" :root default-directory))
              (session (mevedel-session-create "window-graphical" workspace))
              (other (generate-new-buffer " *window-graphical-other*"))
              (mevedel-directive-frame--frame nil)
              child)
         (unwind-protect
             (progn
               (with-current-buffer data-buf
                 (setq-local mevedel--session session
                             mevedel--workspace workspace))
               (with-current-buffer view-buf
                 (setq-local mevedel--session session)
                 (mevedel-view-test--insert-composer-draft
                  "> child composer draft\nsecond line")
                 (goto-char (+ (mevedel-view--input-start) 7)))
               (delete-other-windows)
               (switch-to-buffer data-buf)
               (when (eq ,layout 'split)
                 (set-window-buffer (split-window-right) other))
               (setq child
                     (make-frame
                      (append (list (cons 'parent-frame parent))
                              '((name . "mevedel-window-test-child")
                                (width . 60) (height . 12)
                                (left . 20) (top . 20) (minibuffer . nil))
                              mevedel--child-frame-parameters)))
               (should (eq parent (frame-parent child)))
               (setq mevedel-directive-frame--frame child)
               (let ((origin (frame-root-window child)))
                 (set-window-buffer origin view-buf)
                 (set-window-dedicated-p origin t)
                 (with-current-buffer view-buf
                   (mevedel-directive-frame-mode 1)
                   (should (eq 'mevedel-directive-frame--display-in-parent
                               (car display-buffer-overriding-action))))
                 (select-frame-set-input-focus child)
                 (select-window origin)
                 (redisplay t)
                 (let ((parent-before
                        (mevedel-window-graphical-test--layout parent))
                       (child-before
                        (mevedel-window-graphical-test--layout child))
                       (draft (mevedel-view--input-text))
                       (draft-point (- (point) (mevedel-view--input-start))))
                   ,@body
                   (should (frame-live-p child))
                   (should (window-live-p origin))
                   (should (eq view-buf (window-buffer origin)))
                   (should (window-dedicated-p origin))
                   (should (eq child (selected-frame)))
                   (should (eq origin (selected-window)))
                   (should (equal parent-before
                                  (mevedel-window-graphical-test--layout parent)))
                   (should (equal child-before
                                  (mevedel-window-graphical-test--layout child)))
                   (should (equal draft (mevedel-view--input-text)))
                   (should (= draft-point
                              (- (point) (mevedel-view--input-start)))))))
           (transient--emergency-exit)
           (when (buffer-live-p view-buf)
             (with-current-buffer view-buf (mevedel-directive-frame-mode -1)))
           ;; This fixture owns its frame directly, not the directive overlay
           ;; lifecycle.  Do not invoke that lifecycle's source restoration.
           (setq mevedel-directive-frame--frame nil)
           (when (frame-live-p child) (delete-frame child t))
           (when (frame-live-p parent) (select-frame-set-input-focus parent))
           (when (buffer-live-p other) (kill-buffer other)))))))

(mevedel-deftest mevedel-window-child-frame-return ()
  ,test
  (test)
  :doc "report open and q restore the parent and return to the child composer"
  (progn
    (skip-unless (display-graphic-p))
    (dolist (layout '(single split))
      (ert-info ((format "Parent layout: %s" layout))
        (mevedel-window-graphical-test--with-child layout
          (let ((surface (generate-new-buffer " *window-graphical-report*")))
            (unwind-protect
                (progn
                  ;; The report uses the generic window display/quit owner and
                  ;; supplies its production q binding, without a test command.
                  (mevedel-report-show
                   surface '(:title "Child-frame inspection"
                             :sections ((:id body :title "Inspection"
                                         :body "A temporary parent-frame report."))))
                  (should (eq parent (selected-frame)))
                  (should (eq surface (window-buffer (selected-window))))
                  (should (eq view-buf (window-buffer origin)))
                  (should (equal child-before
                                 (mevedel-window-graphical-test--layout child)))
                  (should (eq 'mevedel-report-quit (key-binding (kbd "q"))))
                  (execute-kbd-macro (kbd "q"))
                  (should-not (buffer-live-p surface)))
              (when (buffer-live-p surface) (kill-buffer surface)))))))))

(mevedel-deftest mevedel-menu-child-frame-return (:quiet t)
  ,test
  (test)
  :doc "menu open and C-g restore the parent and return to the child composer"
  (progn
    (skip-unless (display-graphic-p))
    (dolist (layout '(single split))
      (ert-info ((format "Parent layout: %s" layout))
        (mevedel-window-graphical-test--with-child layout
          (let ((map (copy-keymap (current-local-map)))
                (transient-show-popup t))
            (define-key map (kbd "<f12>") #'mevedel-menu)
            (use-local-map map)
            (execute-kbd-macro (kbd "<f12>"))
            (should transient--prefix)
            (should (eq (oref transient--prefix command) 'mevedel-menu--top))
            (should (window-live-p transient--window))
            (should (eq parent (window-frame transient--window)))
            (should (eq view-buf (window-buffer origin)))
            (should (equal child-before
                           (mevedel-window-graphical-test--layout child)))
            (execute-kbd-macro (kbd "C-g"))
            (should-not transient--prefix)))))))

(provide 'test-mevedel-window-graphical)
;;; test-mevedel-window-graphical.el ends here
