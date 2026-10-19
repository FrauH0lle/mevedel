;;; test-mevedel-view-composer-viewport.el --- Composer viewport costs -*- lexical-binding: t -*-

;;; Commentary:
;; Conservative fast-path tests alongside the native geometry regressions.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-view--draft-deletion-keeps-height-p ()
  (let ((kind ,kind))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (goto-char (point-max))
        (insert (if (eq kind 'wrapped)
                    (make-string (* 2 (window-body-width)) ?x)
                  "plain draft"))
        (let ((end (point-max)))
          (pcase kind
            ('newline (insert "\n") (setq end (point-max)))
            ('face (put-text-property (- end 2) end 'face 'bold))
            ('display (put-text-property (- end 2) end 'display "tall"))
            ('overlay (overlay-put (make-overlay (- end 2) end) 'display "tall"))
            ('remap (setq-local face-remapping-alist '((default (:height 1.5)))))
            ('tab (insert "\t") (setq end (point-max))))
          (should (eq (not (null (mevedel-view--draft-deletion-keeps-height-p
                                 (1- end) end)))
                      (eq kind 'plain)))))))
  (kind)
  :doc "A short plain deletion cannot change display height." 'plain
  :doc "A wrapped line uses exact geometry." 'wrapped
  :doc "Deleting a newline uses exact geometry." 'newline
  :doc "Face properties use exact geometry." 'face
  :doc "Display properties use exact geometry." 'display
  :doc "Overlay display properties use exact geometry." 'overlay
  :doc "Remapped fonts use exact geometry." 'remap
  :doc "Tabs use exact geometry." 'tab)

(mevedel-deftest mevedel-view-composer-proportional-deletion
  (:doc "A short line in a graphical proportional font retains exact geometry.")
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (goto-char (point-max))
      (insert "plain")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'font-get) (lambda (&rest _) 0)))
        (should-not (mevedel-view--draft-deletion-keeps-height-p
                     (1- (point-max)) (point-max)))))))

(mevedel-deftest mevedel-view-composer-short-backspace
  (:doc "Short plain Backspace does not traverse the viewport and preserves the draft.")
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (goto-char (point-max))
      (insert "> first line\nplain draft")
      (let ((this-command 'backward-delete-char-untabify)
            (start (window-start))
            (calls 0)
            (original (symbol-function 'mevedel-view--position-screen-y)))
        (cl-letf (((symbol-function 'mevedel-view--position-screen-y)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply original args))))
          (run-hooks 'pre-command-hook)
          (delete-char -1)
          (run-hooks 'post-command-hook))
        (should (= calls 0))
        (should (= start (window-start)))
        (should (equal "> first line\nplain draf" (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-view-composer-stale-command ()
  (let ((later ,later)
        (stale ,stale))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (dotimes (n 100) (insert (format "history %d\n" n))))
        (goto-char (point-max))
        (insert "> draft\nsecond\nthird\nfourth\nfifth")
        (let ((height (window-body-height)))
          (set-window-start nil (save-excursion
                                  (vertical-motion (- 3 (1- height))) (point)) t)
          ;; Outside a real command, this-command still names the last one.
          (let ((this-command stale))
            (delete-region (save-excursion
                             (goto-char (mevedel-view--input-start))
                             (forward-line 2) (1- (point)))
                           (point-max)))
          (if later
              (progn
                ;; An unrelated command must not apply an old snapshot to a
                ;; viewport the reader deliberately moved in the meantime.
                (set-window-start nil (save-excursion
                                        (goto-char (point-max))
                                        (vertical-motion (- 8 (1- height))) (point)) t)
                (let ((start (window-start)) (this-command 'ignore))
                  (run-hooks 'pre-command-hook)
                  (run-hooks 'post-command-hook)
                  (should (= start (window-start)))))
            ;; The invariant is in display rows, not native GUI pixels.
            (should (= (- height 4)
                       (mevedel-view-test--position-screen-y
                        (point-max) (selected-window)))))
          (should-not mevedel-view--draft-window-state)
          (should (equal "> draft\nsecond" (mevedel-view--input-text)))))))
  (later stale)
  :doc "Outside-command deletion repairs immediately despite stale yank." nil 'yank
  :doc "Outside-command deletion repairs immediately despite stale self-insert." nil 'self-insert-command
  :doc "An unrelated later command cannot apply a stale draft snapshot." t 'yank)

(mevedel-deftest mevedel-view-composer-command-switch-buffer ()
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (dotimes (n 100) (insert (format "history %d\n" n))))
      (goto-char (point-max))
      (insert "> draft\nsecond\nthird\nfourth\nfifth")
      (let ((window (selected-window))
            (other (split-window-right))
            (this-command 'mevedel-test-edit-and-switch))
        (set-window-buffer other data-buf)
        (set-window-start window (save-excursion
                                  (vertical-motion (- 3 (1- (window-body-height window))))
                                  (point))
                          t)
        (let ((start (window-start window)))
          (run-hooks 'pre-command-hook)
          (delete-region (save-excursion
                           (goto-char (mevedel-view--input-start))
                           (forward-line 2) (1- (point)))
                         (point-max))
          (should (= start (window-start window)))
          (select-window other)
          (let ((repair (symbol-function 'mevedel-view--restore-window-blank-pixels))
                (calls 0))
            (cl-letf (((symbol-function 'mevedel-view--restore-window-blank-pixels)
                       (lambda (&rest args)
                         (should-not mevedel-view--draft-command-buffer)
                         (cl-incf calls)
                         (apply repair args))))
              (if ,next-command
                  ;; Simulate an aborted command that missed its post-hook.
                  (with-current-buffer view-buf (run-hooks 'pre-command-hook))
                (run-hooks 'post-command-hook))
              (should (= 1 calls))
              (run-hooks 'post-command-hook)
              (should (= 1 calls))))
          (should-not mevedel-view--draft-command-buffer)
          (with-current-buffer view-buf
            (should-not mevedel-view--draft-window-state)
            ;; Measure rows explicitly; native geometry uses pixels in GUI.
            (should (= (- (window-body-height window) 4)
                       (mevedel-view-test--position-screen-y (point-max) window)))
            (should (equal "> draft\nsecond" (mevedel-view--input-text))))))))
  (next-command)
  :doc "A command ending in another buffer repairs its original composer once." nil
  :doc "The next view pre-hook drains an aborted command before arming another." t)

(mevedel-deftest mevedel-view-composer-command-scope-gate ()
  (let ((armed ,armed))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (goto-char (point-max))
        (let ((this-command 'self-insert-command)
              (captures 0)
              (measure (symbol-function 'mevedel-view--window-blank-pixels)))
          (when armed (run-hooks 'pre-command-hook))
          (cl-letf (((symbol-function 'mevedel-view--window-blank-pixels)
                     (lambda (window)
                       (cl-incf captures)
                       (funcall measure window))))
            (insert "plain"))
          (should (if armed (= captures 0) (> captures 0)))
          (should-not mevedel-view--draft-window-state)
          (run-hooks 'post-command-hook)
          (should-not mevedel-view--draft-command-buffer)))))
  (armed)
  :doc "A genuine self-insert command skips geometry capture." t
  :doc "Stale self-insert outside command scope does not bypass capture." nil)

(mevedel-deftest mevedel-view--finish-draft-command
  (:doc "Finalization clears command scope without disturbing a render preserver.")
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (run-hooks 'pre-command-hook)
      (let ((mevedel-view--draft-window-state 'preserving))
        (run-hooks 'post-command-hook)
        (should (eq 'preserving mevedel-view--draft-window-state))
        (should-not mevedel-view--draft-command-buffer)))))

(provide 'test-mevedel-view-composer-viewport)
;;; test-mevedel-view-composer-viewport.el ends here
