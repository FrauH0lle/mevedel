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
          (delete-char -1)
          (run-hooks 'post-command-hook))
        (should (= calls 0))
        (should (= start (window-start)))
        (should (equal "> first line\nplain draf" (mevedel-view--input-text)))))))

(provide 'test-mevedel-view-composer-viewport)
;;; test-mevedel-view-composer-viewport.el ends here
