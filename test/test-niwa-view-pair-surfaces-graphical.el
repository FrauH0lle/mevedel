;;; test-niwa-view-pair-surfaces-graphical.el --- Native note and /btw docks -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real note and /btw display paths with the native command loop.

;;; Code:

(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-note-conversation)
(require 'niwa-note-frame)
(require 'niwa-side-conversation)
(require 'niwa-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-view-composer-test-helpers"))

(defun niwa-view-pair-surfaces--assert-dock (transcript composer)
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

(defun niwa-view-pair-surfaces--note (fallback height)
  "Check a native note child frame of HEIGHT or a right-hand FALLBACK."
  (unless (display-graphic-p) (ert-skip "Requires a graphical frame"))
  (niwa-test--with-captured-diagnostics nil
    (niwa-view-test--with-buffers
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
                         (overlay-put overlay 'niwa-instruction-type 'note)
                         (overlay-put overlay 'niwa-id 1)
                         overlay)))
               (niwa-note-frame-height height)
               (niwa-note-frame-min-height 8))
          (unwind-protect
              (progn
                (switch-to-buffer source)
                (niwa-view-test--insert-data data-buf "Note prompt.\n" nil)
                (niwa-view-test--insert-data data-buf "Note reply.\n" 'response)
                (with-current-buffer view-buf (niwa-view--full-rerender))
                (redisplay t)
                (if fallback
                    ;; Choose the fallback without substituting for native display.
                    (cl-letf (((symbol-function 'niwa-note-frame--available-p) #'ignore))
                      (niwa-note-frame-display note view-buf))
                  (niwa-note-frame-display note view-buf))
                (should (eq (selected-window) origin))
                (let* ((frame (if fallback parent niwa-note-frame--frame))
                       (transcript (get-buffer-window view-buf frame))
                       (draft (niwa-view-composer-buffer view-buf))
                       (composer (get-buffer-window draft frame))
                       (opening-height (frame-text-height frame)))
                  (if fallback
                      (should (> (window-pixel-left transcript) (window-pixel-left origin)))
                    (should (eq parent (frame-parent frame)))
                    (niwa-note-frame-refresh)
                    (redisplay t)
                    (when (> height niwa-note-frame-min-height)
                      (should (< (frame-text-height frame) opening-height))))
                  (niwa-view-pair-surfaces--assert-dock transcript composer)
                  (select-frame-set-input-focus frame)
                  (select-window composer)
                  (when (and (not fallback) (> height niwa-note-frame-min-height))
                    (niwa-view-composer-graphical--keys "C-h m" 'describe-mode)
                    (should (get-buffer-window "*Help*" parent))
                    (should-not (get-buffer-window "*Help*" frame))
                    (niwa-view-pair-surfaces--assert-dock transcript composer)
                    (quit-window nil (get-buffer-window "*Help*" parent))
                    (select-frame-set-input-focus frame)
                    (select-window composer))
                  (niwa-view-composer-graphical--type "> note draft")
                  (should (equal "> note draft" (niwa-view-composer-draft-text view-buf t)))
                  (if fallback
                      (progn
                        (with-current-buffer data-buf (niwa-note-conversation-setup))
                        (niwa-view-composer-graphical--keys "C-c C-z" 'niwa-note-conversation-close))
                    (niwa-view-composer-graphical--keys "C-c C-z" 'niwa-note-frame-close))
                  (should (eq (selected-window) origin))
                  (should-not (get-buffer-window view-buf t))
                  (should-not (get-buffer-window draft t))
                  (should (equal "> note draft" (niwa-view-composer-draft-text view-buf t)))))
            (when (frame-live-p niwa-note-frame--frame) (niwa-note-frame-close))
            (kill-buffer source)
            (delete-file file)))))))

(ert-deftest niwa-view-pair-surfaces/note-child ()
  "Dock inside an anchored child frame and close it from the draft."
  (niwa-view-pair-surfaces--note nil 20))

(ert-deftest niwa-view-pair-surfaces/note-tiny-child ()
  "Reserve room for a composer in a short note frame."
  (niwa-view-pair-surfaces--note nil 8))

(ert-deftest niwa-view-pair-surfaces/note-fallback ()
  "Dock the right-hand fallback without widening it or taking source focus."
  (niwa-view-pair-surfaces--note t 20))

(ert-deftest niwa-view-pair-surfaces/btw ()
  "Open, type in, and dismiss the native /btw pair from its composer."
  (skip-unless (display-graphic-p))
  (niwa-test--with-captured-diagnostics nil
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Side fixture" :key "test" :models '(test-model)))
           (gptel-model 'test-model))
      (niwa-view-composer-graphical--with-chat
        (let* ((frame (selected-frame))
               (root-draft (niwa-view-composer-buffer view-buf))
               (root-composer (selected-window))
               (root-transcript (get-buffer-window view-buf frame)))
          (niwa-view-test--insert-data data-buf "*** Parent prompt\n" nil)
          (niwa-view-composer-graphical--type "/btw")
          (niwa-view-composer-graphical--keys "C-c RET" 'niwa-view-send)
          (niwa-test--await 3 "The /btw composer did not receive focus"
            (and (bound-and-true-p niwa-composer--view)
                 (bound-and-true-p niwa-view--side-conversation-p)))
          (let* ((composer (selected-window))
                 (draft (window-buffer composer))
                 (side-view (buffer-local-value 'niwa-composer--view draft))
                 (transcript (get-buffer-window side-view frame)))
            (should-not (eq draft root-draft))
            (niwa-view-pair-surfaces--assert-dock transcript composer)
            (niwa-view-pair-surfaces--assert-dock root-transcript root-composer)
            (niwa-view-composer-graphical--type "> side draft")
            (should (equal "> side draft" (niwa-view-composer-draft-text side-view t)))
            (niwa-view-composer-graphical--keys "C-c C-z" 'niwa-side-conversation-close)
            (should-not (buffer-live-p draft))
            (should-not (buffer-live-p side-view))
            (should-not (window-live-p composer))
            (should (eq (window-buffer root-composer) root-draft))
            (should (equal "" (niwa-view-composer-draft-text view-buf t)))))))))

(provide 'test-niwa-view-pair-surfaces-graphical)
;;; test-niwa-view-pair-surfaces-graphical.el ends here
