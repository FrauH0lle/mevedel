;;; test-mevedel-view-pair-surfaces-graphical.el --- Native note and /btw docks -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real note and /btw display paths with the native command loop.

;;; Code:

(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-note-conversation)
(require 'mevedel-note-frame)
(require 'mevedel-side-conversation)
(require 'mevedel-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-view-composer-test-helpers"))

(defun mevedel-view-pair-surfaces--assert-dock (transcript composer)
  "Assert COMPOSER is docked immediately beneath TRANSCRIPT."
  (should (window-live-p transcript))
  (should (window-live-p composer))
  (should (eq (window-frame transcript) (window-frame composer)))
  (should (eq (window-parent transcript) (window-parent composer)))
  (let ((upper (window-pixel-edges transcript))
        (lower (window-pixel-edges composer)))
    (should (= (nth 0 upper) (nth 0 lower)))
    (should (= (nth 2 upper) (nth 2 lower)))
    (should (= (nth 3 upper) (nth 1 lower)))))

(defun mevedel-view-pair-surfaces--note (fallback height)
  "Check a native note child frame of HEIGHT or a right-hand FALLBACK."
  (unless (display-graphic-p) (ert-skip "Requires a graphical frame"))
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (delete-other-windows)
        (let* ((origin (selected-window))
               (parent (selected-frame))
               (file (make-temp-file "note-dock-" nil ".txt"))
               (source (progn
                         (write-region "A note.\n" nil file nil 'silent)
                         (find-file-noselect file)))
               (note (with-current-buffer source
                       (let ((overlay (make-overlay (point-min) (1- (point-max)))))
                         (overlay-put overlay 'mevedel-instruction-type 'note)
                         (overlay-put overlay 'mevedel-id 1)
                         overlay)))
               (mevedel-note-frame-height height)
               (mevedel-note-frame-min-height 8))
          (unwind-protect
              (progn
                (switch-to-buffer source)
                (mevedel-view-test--insert-data data-buf "Note prompt.\n" nil)
                (mevedel-view-test--insert-data data-buf "Note reply.\n" 'response)
                (with-current-buffer view-buf (mevedel-view--full-rerender))
                (redisplay t)
                (if fallback
                    ;; Choose the fallback without substituting for native display.
                    (cl-letf (((symbol-function 'mevedel-note-frame--available-p) #'ignore))
                      (mevedel-note-frame-display note view-buf))
                  (mevedel-note-frame-display note view-buf))
                (should (eq (selected-window) origin))
                (let* ((frame (if fallback parent mevedel-note-frame--frame))
                       (transcript (get-buffer-window view-buf frame))
                       (draft (mevedel-view-composer-buffer view-buf))
                       (composer (get-buffer-window draft frame))
                       (opening-height (frame-text-height frame)))
                  (if fallback
                      (should (> (window-pixel-left transcript) (window-pixel-left origin)))
                    (should (eq parent (frame-parent frame)))
                    (mevedel-note-frame-refresh)
                    (redisplay t)
                    (when (> height mevedel-note-frame-min-height)
                      (should (< (frame-text-height frame) opening-height))))
                  (mevedel-view-pair-surfaces--assert-dock transcript composer)
                  (select-frame-set-input-focus frame)
                  (select-window composer)
                  (when (and (not fallback) (> height mevedel-note-frame-min-height))
                    (mevedel-view-composer-graphical--keys "C-h m" 'describe-mode)
                    (should (get-buffer-window "*Help*" parent))
                    (should-not (get-buffer-window "*Help*" frame))
                    (mevedel-view-pair-surfaces--assert-dock transcript composer)
                    (quit-window nil (get-buffer-window "*Help*" parent))
                    (select-frame-set-input-focus frame)
                    (select-window composer))
                  (mevedel-view-composer-graphical--type "> note draft")
                  (should (equal "> note draft" (mevedel-view-composer-draft-text view-buf t)))
                  (if fallback
                      (progn
                        (with-current-buffer data-buf (mevedel-note-conversation-setup))
                        (mevedel-view-composer-graphical--keys "C-c C-z" 'mevedel-note-conversation-close))
                    (mevedel-view-composer-graphical--keys "C-c C-z" 'mevedel-note-frame-close))
                  (should (eq (selected-window) origin))
                  (should-not (get-buffer-window view-buf t))
                  (should-not (get-buffer-window draft t))
                  (should (equal "> note draft" (mevedel-view-composer-draft-text view-buf t)))))
            (when (frame-live-p mevedel-note-frame--frame) (mevedel-note-frame-close))
            (kill-buffer source)
            (delete-file file)))))))

(ert-deftest mevedel-view-pair-surfaces/note-child ()
  "Dock inside an anchored child frame and close it from the draft."
  (mevedel-view-pair-surfaces--note nil 20))

(ert-deftest mevedel-view-pair-surfaces/note-tiny-child ()
  "Reserve room for a composer in a short note frame."
  (mevedel-view-pair-surfaces--note nil 8))

(ert-deftest mevedel-view-pair-surfaces/note-fallback ()
  "Dock the right-hand fallback without widening it or taking source focus."
  (mevedel-view-pair-surfaces--note t 20))

(ert-deftest mevedel-view-pair-surfaces/btw ()
  "Open, type in, and dismiss the native /btw pair from its composer."
  (skip-unless (display-graphic-p))
  (mevedel-test--with-captured-diagnostics nil
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Side fixture" :key "test" :models '(test-model)))
           (gptel-model 'test-model))
      (mevedel-view-composer-graphical--with-chat
        (let* ((frame (selected-frame))
               (root-draft (mevedel-view-composer-buffer view-buf))
               (root-composer (selected-window))
               (root-transcript (get-buffer-window view-buf frame)))
          (mevedel-view-test--insert-data data-buf "*** Parent prompt\n" nil)
          (mevedel-view-composer-graphical--type "/btw")
          (mevedel-view-composer-graphical--keys "C-c RET" 'mevedel-view-send)
          (mevedel-test--await 3 "The /btw composer did not receive focus"
            (and (bound-and-true-p mevedel-composer--view)
                 (bound-and-true-p mevedel-view--side-conversation-p)))
          (let* ((composer (selected-window))
                 (draft (window-buffer composer))
                 (side-view (buffer-local-value 'mevedel-composer--view draft))
                 (transcript (get-buffer-window side-view frame)))
            (should-not (eq draft root-draft))
            (mevedel-view-pair-surfaces--assert-dock transcript composer)
            (mevedel-view-pair-surfaces--assert-dock root-transcript root-composer)
            (mevedel-view-composer-graphical--type "> side draft")
            (should (equal "> side draft" (mevedel-view-composer-draft-text side-view t)))
            (mevedel-view-composer-graphical--keys "C-c C-z" 'mevedel-side-conversation-close)
            (should-not (buffer-live-p draft))
            (should-not (buffer-live-p side-view))
            (should-not (window-live-p composer))
            (should (eq (window-buffer root-composer) root-draft))
            (should (equal "" (mevedel-view-composer-draft-text view-buf t)))))))))

(provide 'test-mevedel-view-pair-surfaces-graphical)
;;; test-mevedel-view-pair-surfaces-graphical.el ends here
