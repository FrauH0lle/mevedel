;;; test-niwa-view-pair-graphical.el --- Native transcript and composer pair tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise window commands on a real chat in graphical or terminal Emacs.

;;; Code:

(require 'niwa-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-view-composer-test-helpers"))
(require 'niwa-interaction-prompt)
(require 'niwa-session-persistence)
(require 'niwa-view)
(require 'niwa-view-pair)
(require 'which-key)

(defun niwa-view-pair-graphical--windows (buffer)
  "Return windows displaying BUFFER on the selected frame."
  (get-buffer-window-list buffer nil (selected-frame)))

(defun niwa-view-pair-graphical--pairs (view count)
  "Assert VIEW has COUNT isolated pairs on this frame, without orphan drafts."
  (let (transcripts composers)
    (niwa-test--await 2 (format "Expected %d settled pairs; got %S / %S"
                                    count transcripts composers)
      (progn
        (redisplay t)
        (setq transcripts (niwa-view-pair-graphical--windows view)
              composers (niwa-view-pair-graphical--windows
                         (niwa-view-composer-buffer view)))
        (= count (length transcripts) (length composers))))
    (dolist (transcript transcripts)
      (let ((composer (window-next-sibling transcript)))
        (should (memq composer composers))
        (setq composers (delq composer composers))
        (should (eq composer (window-in-direction 'below transcript)))
        (should (= (window-pixel-left transcript) (window-pixel-left composer)))
        (should (= (window-pixel-width transcript) (window-pixel-width composer)))
        (should (= 2 (window-child-count (window-parent transcript))))
        (should (eq t (window-dedicated-p composer)))
        (dolist (window (list transcript composer))
          (should (eq transcript (niwa-view-transcript-window window)))
          (should (eq composer (niwa-view-composer-window window))))))))

(defun niwa-view-pair-graphical--switch (buffer)
  "Switch to BUFFER with native keys and return its dock at command completion."
  (let* ((unread-command-events (append (kbd "C-x b") (buffer-name buffer) '(?\r)))
         (deadline (run-at-time 5 nil #'exit-recursive-edit))
         finished dock
         (hook (lambda ()
                 ;; The minibuffer and its caller can report the same command.
                 (when (and (not finished) (eq (window-buffer) buffer))
                   (setq dock (niwa-view-composer-window)
                         finished (run-at-time 0 nil #'exit-recursive-edit))))))
    (add-hook 'post-command-hook hook 100)
    (unwind-protect (recursive-edit)
      (remove-hook 'post-command-hook hook)
      (cancel-timer deadline)
      (when finished (cancel-timer finished)))
    (should (eq (window-buffer) buffer))
    dock))

(ert-deftest niwa-view-pair-graphical/final-turn-rule ()
  "Show the plain closing rule only after another turn follows it."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock)))
      (insert "> draft\nsecond line")
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "An earlier response.")
        (let ((answer (line-beginning-position)))
          ;; Count the response and the native end-of-buffer row, no chrome.
          (should (= 2 (count-screen-lines
                        answer (niwa-view-transcript-tail) t transcript)))
          (niwa-view-test--insert-data data-buf "A new prompt.\n" nil)
          (niwa-view--full-rerender)
          (goto-char answer)
          (search-forward "You\nA new prompt.")
          (should (= 3 (count-screen-lines
                        answer (match-beginning 0) nil transcript)))))
      (should (eq dock (selected-window)))
      (should (equal "> draft\nsecond line"
                     (niwa-view-composer-graphical--draft view-buf))))))

(ert-deftest niwa-view-pair-graphical/toggle-latest-tool-row ()
  "TAB on the row above the hidden closing rule keeps the row displayed."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock)))
      (niwa-view-test--insert-data
       data-buf "(:name \"Read\" :args (:file_path \"last.el\"))\n\nTool detail\n"
       '(tool . "last-tool"))
      (with-current-buffer view-buf (niwa-view--full-rerender))
      (select-window transcript)
      (goto-char (point-min))
      (search-forward "Read: last.el")
      (let ((row (line-beginning-position)))
        (cl-flet ((rows ()
                    ;; The row, its body when expanded, and the native
                    ;; end-of-buffer row; the closing rule shows nothing.
                    (count-screen-lines row (niwa-view-transcript-tail) t transcript)))
          (should (= 2 (rows)))
          (goto-char row)
          (niwa-view-composer-graphical--keys "TAB" 'niwa-view-toggle-section)
          (should (search-forward "Tool detail" (niwa-view-transcript-tail) t))
          (should (< 2 (rows)))
          (goto-char row)
          (niwa-view-composer-graphical--keys "TAB" 'niwa-view-toggle-section)
          (should (= 2 (rows))))))))

(ert-deftest niwa-view-pair-graphical/delete-other-windows ()
  "Keeping either half via the native command preserves the pair and draft caret."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (other (split-root-window-right)))
      (set-window-buffer other (get-buffer-create "*scratch*"))
      (niwa-view-composer-graphical--type "Keep this draft")
      (let ((caret (point)))
        (niwa-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
        (should (eq composer (selected-window)))
        (should (window-live-p transcript))
        (should-not (window-live-p other))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (equal "Keep this draft" (niwa-view-composer-graphical--draft view-buf)))
        (should (= caret (point)))
        (niwa-view-composer-graphical--keys "C-x o" 'other-window)
        (should (eq transcript (selected-window)))
        (let ((other (split-root-window-right)))
          (set-window-buffer other (get-buffer-create "*scratch*"))
          (niwa-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
          (should-not (window-live-p other)))
        (should (eq transcript (selected-window)))
        (should (window-live-p composer))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (niwa-view-composer-graphical--keys "C-x o" 'other-window)
        (should (eq composer (selected-window)))
        (should (= caret (window-point composer)))))))

(ert-deftest niwa-view-pair-graphical/delete-neighbor ()
  "The pair counts as one visible window when closing the window above it."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock))
           (other (split-window (frame-root-window) nil 'above)))
      (set-window-buffer other (get-buffer-create "*scratch*"))
      (insert "> keep this draft")
      (select-window other)
      ;; Workspace commands filter out dedicated windows unless marked visible.
      (should (equal (list other transcript)
                     (seq-filter (lambda (window)
                                   (or (window-parameter window 'visible)
                                       (not (window-dedicated-p window))))
                                 (window-list nil 'nomini))))
      (niwa-view-composer-graphical--keys "C-x 0" 'delete-window)
      (should-not (window-live-p other))
      (niwa-view-pair-graphical--pairs view-buf 1)
      (should (equal "> keep this draft" (niwa-view-composer-graphical--draft view-buf))))))

(ert-deftest niwa-view-pair-graphical/quit-sole-pair ()
  "Quitting either member of a frame-filling pair ends as one window."
  :tags '(niwa-docked-composer)
  (dolist (member '(composer transcript))
    (niwa-view-composer-graphical--with-chat
      (let* ((dock (selected-window))
             (draft (window-buffer dock))
             (transcript (get-buffer-window view-buf)))
        (set-window-prev-buffers transcript
                                 (list (list (get-buffer-create "*scratch*")
                                             (point-min-marker) (point-min-marker))))
        (select-window (if (eq member 'composer) dock transcript))
        (if (eq member 'composer)
            (quit-window)
          ;; A literal q in the composer is text, so only the transcript has the key.
          (goto-char (point-min))
          (niwa-view-composer-graphical--keys "q" 'niwa-view-quit))
        (niwa-test--await 2 "Expected the quit pair to settle to one window"
          (progn (niwa-view-composer-graphical--settle)
                 (= 1 (length (window-list nil 'nomini)))))
        (niwa-view-composer-graphical--settle)
        (should (= 1 (length (window-list nil 'nomini))))
        (should (eq (get-buffer "*scratch*") (window-buffer (selected-window))))
        (should (buffer-live-p draft))
        (should-not (get-buffer-window draft t))
        (should-not (get-buffer-window view-buf t))))))

(ert-deftest niwa-view-pair-graphical/kill-dock-requests-close ()
  "Killing the dock buffer asks the view once; a veto keeps view, dock and draft."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((dock (selected-window))
          (draft (current-buffer))
          (asked 0)
          (allow nil))
      (niwa-view-composer-graphical--type "unsent")
      (cl-letf (((symbol-function 'niwa-view--allow-session-close-p)
                 (lambda () (when (eq (current-buffer) view-buf) (cl-incf asked))
                   allow)))
        (execute-kbd-macro (kbd "C-x k RET"))
        (should (= 1 asked))
        (should (buffer-live-p view-buf))
        (should (buffer-live-p draft))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (eq dock (selected-window)))
        (should (equal "unsent" (niwa-view-composer-graphical--draft view-buf)))
        (setq allow t asked 0)
        (execute-kbd-macro (kbd "C-x k RET"))
        (should (= 1 asked))
        (should-not (buffer-live-p view-buf))
        (should-not (buffer-live-p draft))))))

(ert-deftest niwa-view-pair-graphical/settlement-after-return ()
  "A settled card does not rewind a draft the user returned to edit."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock))
           outcome)
      (niwa-view-composer-graphical--type "> draft")
      (niwa-view-composer-graphical--keys "RET" 'newline)
      (niwa-view-composer-graphical--type "next")
      (with-current-buffer data-buf
        (niwa--prompt-user-with-overlay
         "Confirm" "Do the thing" "Proceed?" nil (lambda (value) (setq outcome value))))
      (niwa-view-composer-graphical--keys "C-c C-j" 'niwa-view-jump-to-interaction)
      (should (eq transcript (selected-window)))
      (should (get-text-property (point) 'niwa-prompt-focus))
      (niwa-view-composer-graphical--keys "C-c C-SPC" 'niwa-view-pair-toggle-focus)
      (should (eq dock (selected-window)))
      (niwa-view-composer-graphical--type " line")
      (let ((caret (point)) (start (window-start dock)))
        (with-current-buffer view-buf
          (should (get-text-property (window-point transcript) 'niwa-prompt-focus))
          (niwa--prompt-dismiss-all))
        (niwa-view-composer-graphical--settle)
        (should (eq outcome 'aborted))
        (should (eq dock (selected-window)))
        (should (equal "> draft\nnext line" (niwa-view-composer-graphical--draft view-buf)))
        (should (= start (window-start dock)))
        (should (= caret (point)))))))

(ert-deftest niwa-view-pair-graphical/split-and-delete ()
  "Splitting either member makes two pairs on one draft; deleting removes both halves."
  :tags '(niwa-docked-composer)
  (pcase-dolist (`(,keys ,command ,edge) '(("C-x 2" split-window-below window-pixel-top)
                                          ("C-x 3" split-window-right window-pixel-left)))
    (dolist (from '(transcript composer))
      (ert-info ((format "%s and C-x 0 from %s" keys from))
        (niwa-view-composer-graphical--with-chat
          (let* ((composer (selected-window))
                 (transcript (get-buffer-window view-buf))
                 (draft (window-buffer composer))
                 (origin (if (eq from 'composer) composer transcript)))
            (niwa-view-composer-graphical--type "One draft")
            (select-window origin)
            (let* ((docks nil)
                   (observe (lambda ()
                              (when (eq this-command command)
                                (setq docks (get-buffer-window-list draft nil (selected-frame)))))))
              (add-hook 'post-command-hook observe 100)
              (unwind-protect
                  (niwa-view-composer-graphical--keys keys command)
                (remove-hook 'post-command-hook observe))
              (should (= 2 (length docks))))
            (should (eq origin (selected-window)))
            (niwa-view-pair-graphical--pairs view-buf 2)
            (should (= 4 (length (window-list nil 'nomini))))
            (let* ((new-transcript (car (remq transcript
                                             (niwa-view-pair-graphical--windows view-buf))))
                   (new-composer (window-next-sibling new-transcript)))
              (should (> (funcall edge new-transcript) (funcall edge transcript)))
              (select-window new-composer)
              (niwa-view-composer-graphical--keys "M->" 'end-of-buffer)
              (niwa-view-composer-graphical--type " in both windows")
              (should (eq draft (window-buffer composer)))
              (should (eq draft (window-buffer new-composer)))
              (select-window origin)
              (niwa-view-composer-graphical--keys "C-x 0" 'delete-window)
              (should-not (window-live-p transcript))
              (should-not (window-live-p composer))
              (should (window-live-p new-transcript))
              (should (window-live-p new-composer))
              (niwa-view-pair-graphical--pairs view-buf 1)
              (should (equal "One draft in both windows"
                             (niwa-view-composer-graphical--draft view-buf))))))))))

(ert-deftest niwa-view-pair-graphical/split-fits-wrapped-draft ()
  "The split fits each dock once and grows the original before redisplay."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (fits 0)
           (observe-fit (lambda (&rest _) (cl-incf fits)))
           height after-split
           (observe-split (lambda ()
                            (when (eq this-command 'split-window-right)
                              (setq after-split (window-pixel-height dock))))))
      (insert (make-string (/ (* 3 (window-body-width dock)) 4) ?x))
      (niwa-view-composer-graphical--settle)
      (setq height (window-pixel-height dock))
      (advice-add 'niwa-composer--fit :before observe-fit)
      (add-hook 'post-command-hook observe-split 100)
      (unwind-protect
          (progn
            (niwa-view-composer-graphical--keys "C-x 3" 'split-window-right)
            (niwa-view-composer-graphical--settle)
            (should (> after-split height))
            (should (= after-split (window-pixel-height dock)))
            (should (= 2 fits)))
        (remove-hook 'post-command-hook observe-split)
        (advice-remove 'niwa-composer--fit observe-fit)))))

(ert-deftest niwa-view-pair-graphical/switch-buffer ()
  "Switching buffers replaces the transcript, undocks, and redocks on return."
  :tags '(niwa-docked-composer)
  (dolist (from '(transcript composer))
    (ert-info ((format "C-x b from %s" from))
      (niwa-view-composer-graphical--with-chat
        (let ((composer (selected-window))
              (transcript (get-buffer-window view-buf))
              (scratch (get-buffer-create "*scratch*")))
          (niwa-view-composer-graphical--type "Saved while away")
          (when (eq from 'transcript) (select-window transcript))
          (niwa-view-pair-graphical--switch scratch)
          (should (eq transcript (selected-window)))
          (should (eq scratch (window-buffer transcript)))
          (niwa-view-pair-graphical--pairs view-buf 0)
          (should-not (window-live-p composer))
          (should (window-live-p (niwa-view-pair-graphical--switch view-buf)))
          (should (eq transcript (selected-window)))
          (niwa-view-pair-graphical--pairs view-buf 1)
          (should (equal "Saved while away" (niwa-view-composer-graphical--draft view-buf))))))))

(ert-deftest niwa-view-pair-graphical/restore-configuration ()
  "Restoring a layout with a missing composer docks it without taking focus."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (scratch (get-buffer-create "*scratch*")))
      (niwa-view-composer-graphical--type "Survive layout restore")
      (select-window transcript)
      (let ((caret (point))
            ;; Capture a broken layout before redisplay has a chance to repair it.
            (register-alist nil))
        (let ((ignore-window-parameters t))
          (delete-window composer))
        (should (one-window-p))
        (window-configuration-to-register ?z)
        (niwa-view-pair-graphical--switch scratch)
        (niwa-view-pair-graphical--pairs view-buf 0)
        (niwa-view-composer-graphical--keys "C-x r j z" 'jump-to-register)
        (should (eq transcript (selected-window)))
        (should (= caret (point)))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (equal "Survive layout restore"
                       (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/help-and-restore ()
  "Help opens outside the pair and can be made the sole window without orphans."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (display-buffer-alist '(("\\`\\*Help\\*\\'" (display-buffer-below-selected))))
          (help-window-select nil))
      (niwa-view-composer-graphical--type "Keep while reading Help")
      (niwa-test--with-captured-messages nil
        (niwa-view-composer-graphical--keys "C-h k C-c RET" 'describe-key))
      (let ((help (get-buffer-window "*Help*")))
        (should (window-live-p help))
        (with-current-buffer (window-buffer help)
          (should (string-search "niwa-view-send" (buffer-string))))
        (should (eq composer (selected-window)))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (select-window help)
        (niwa-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
        (should (eq help (selected-window)))
        (niwa-view-pair-graphical--pairs view-buf 0)
        (should (one-window-p))
        (niwa-view-pair-graphical--switch view-buf)
        (should (eq help (selected-window)))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (equal "Keep while reading Help"
                       (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/display-replaces-member ()
  "Repair keeps a display rule's replacement buffer, not stale pair rules."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (replacement (generate-new-buffer "*Replacement*"))
           (scratch (get-buffer-create "*scratch*"))
           (display-buffer-alist '(("\\`\\*Replacement\\*\\'" (display-buffer-same-window)))))
      (unwind-protect
          (progn
            (niwa-view-composer-graphical--type "Preserved draft")
            ;; Emacs permits an explicit display rule to override dedication.
            (set-window-dedicated-p composer nil)
            (should (eq composer (display-buffer replacement)))
            (niwa-view-pair-graphical--pairs view-buf 1)
            (should (eq replacement (window-buffer composer)))
            (should-not (eq composer (niwa-view-composer-window transcript)))
            (should (eq composer (selected-window)))
            (should (= 3 (length (window-list nil 'nomini))))
            (should (equal "Preserved draft" (niwa-view-composer-graphical--draft view-buf)))
            (let ((dock (niwa-view-composer-window transcript)))
              (niwa-view-pair-graphical--switch scratch)
              (niwa-view-pair-graphical--pairs view-buf 1)
              (should (eq scratch (window-buffer composer)))
              (should (eq composer (selected-window)))
              (should (eq dock (niwa-view-composer-window transcript)))
              (should (= 3 (length (window-list nil 'nomini)))))
            (niwa-view-composer-graphical--keys "C-x 0" 'delete-window)
            (should-not (window-live-p composer))
            (niwa-view-pair-graphical--pairs view-buf 1))
        (kill-buffer replacement)))))

(ert-deftest niwa-view-pair-graphical/help-with-two-small-pairs ()
  "Stock Help display leaves both pairs intact when no leaf can split."
  (niwa-view-composer-graphical--with-chat
    (let ((frame (selected-frame))
          (frames (frame-list))
          (dock (selected-window))
          (help-window-select nil))
      (unwind-protect
          (progn
            (niwa-view-composer-graphical--type "Keep this draft")
            (niwa-view-composer-graphical--keys "C-x 3" 'split-window-right)
            (niwa-view-pair-graphical--pairs view-buf 2)
            (dolist (window (window-list frame 'nomini))
              (should-not (window-splittable-p window))
              (should-not (window-splittable-p window t)))
            (niwa-test--with-captured-messages nil
              (niwa-view-composer-graphical--keys "C-h k C-c RET" 'describe-key))
            (let ((popup (get-buffer-window "*Help*" t)))
              (should (window-live-p popup))
              (should-not (niwa-view-transcript-window popup)))
            (with-selected-frame frame
              (should (eq dock (selected-window)))
              (niwa-view-pair-graphical--pairs view-buf 2))
            (should (equal "Keep this draft" (niwa-view-composer-graphical--draft view-buf))))
        (mapc #'delete-frame (seq-difference (frame-list) frames))))))

(ert-deftest niwa-view-pair-graphical/resize ()
  "Growing another window shrinks the transcript, not the composer."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (other (split-root-window-below)))
      (set-window-buffer other (get-buffer-create "*scratch*"))
      (select-window other)
      (niwa-view-composer-graphical--settle)
      (let ((draft-height (window-pixel-height composer))
            (transcript-height (window-pixel-height transcript)))
        (niwa-view-composer-graphical--keys "C-u 5 C-x ^" 'enlarge-window)
        (should (eq other (selected-window)))
        (should (< (window-pixel-height transcript) transcript-height))
        (should (= (window-pixel-height composer) draft-height))
        (niwa-view-pair-graphical--pairs view-buf 1)))))

(ert-deftest niwa-view-pair-graphical/completions ()
  "Native mention completion opens outside the pair and leaves the draft focused."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (completion-auto-help t)
          (display-buffer-alist '(("\\`\\*Completions\\*\\'" (display-buffer-below-selected)))))
      (niwa-view-composer-graphical--type "@")
      (niwa-test--with-captured-messages nil
        (niwa-view-composer-graphical--keys "C-M-i" 'completion-at-point))
      (let ((completions (get-buffer-window "*Completions*")))
        (should (window-live-p completions))
        (with-current-buffer (window-buffer completions)
          (should (string-search "@file:" (buffer-string)))
          (should (string-search "@note:" (buffer-string))))
        (should (eq composer (selected-window)))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (equal "@" (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/which-key ()
  "Built-in which-key stays outside the pair, and its command acts in the draft."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (which-key-idle-delay 0.1)
          (which-key-popup-type 'side-window)
          (was-enabled which-key-mode)
          popup focus outside timer)
      (niwa-view-composer-graphical--type "Clear this draft")
      (unwind-protect
          (progn
            (which-key-mode 1)
            (setq timer
                  (run-at-time 0.02 0.02
                               (lambda ()
                                 (when (setq popup (get-buffer-window which-key-buffer-name))
                                   (setq focus (selected-window)
                                         outside (not (eq (window-parent popup)
                                                          (window-parent composer))))
                                   (cancel-timer timer)
                                   (setq unread-command-events (listify-key-sequence (kbd "C-u")))))))
            (niwa-view-composer-graphical--keys "C-c" 'niwa-view-history-clear-input)
            (should (windowp popup))
            (should outside)
            (should (eq composer focus))
            (should (eq composer (selected-window)))
            (should-not (get-buffer-window which-key-buffer-name))
            (should (equal "" (niwa-view-composer-graphical--draft view-buf)))
            (niwa-view-pair-graphical--pairs view-buf 1))
        (when timer (cancel-timer timer))
        (unless was-enabled (which-key-mode -1))))))

(ert-deftest niwa-view-pair-graphical/resume-session ()
  "Resuming a saved session shows its transcript and a working paired composer."
  :tags '(niwa-docked-composer)
  (niwa-test--with-captured-messages nil
    (niwa-view-composer-graphical--with-chat
      (let (reopened)
        (unwind-protect
            (progn
              (with-current-buffer data-buf (niwa-save-session))
              (should (niwa-session-save-path session))
              (niwa-chat-discard-buffers data-buf t)
              (should-not (buffer-live-p view-buf))
              (setq reopened (niwa-session-persistence-resume-id
                              workspace (niwa-session-session-id session)))
              (should (buffer-live-p reopened))
              (let ((restored (buffer-local-value 'niwa--session reopened)))
                (should-not (eq session restored))
                (should (equal (niwa-session-session-id session)
                               (niwa-session-session-id restored))))
              (let* ((fits 0)
                     (observe (lambda (&rest _) (cl-incf fits))))
                (advice-add 'niwa-composer--fit :before observe)
                (unwind-protect
                    (progn
                      (niwa--display-chat-buffer reopened)
                      (niwa-view-composer-graphical--settle)
                      (should (= 1 fits)))
                  (advice-remove 'niwa-composer--fit observe)))
              (let* ((composer (selected-window))
                     (transcript (niwa-view-transcript-window composer))
                     (view (window-buffer transcript)))
                (niwa-view-pair-graphical--pairs view 1)
                (should (eq composer (niwa-view-composer-window transcript)))
                (niwa-view-composer-graphical--type "Resumed draft")
                (should (equal "Resumed draft" (niwa-view-composer-graphical--draft view)))))
          (when reopened (niwa-chat-discard-buffers reopened t)))))))

(ert-deftest niwa-view-pair-graphical/native-side-slot ()
  "Leave a transcript in a native side slot alone across redisplays."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (side (display-buffer-in-side-window view-buf '((side . right)))))
      (should (eq 'right (window-parameter side 'window-side)))
      (dotimes (_ 3)
        (redisplay t)
        (sit-for 0.02)
        (should (eq (window-buffer side) view-buf))
        (should (eq 'right (window-parameter side 'window-side)))
        (should-not (niwa-view-composer-window side))
        (should (equal (list dock) (niwa-view-pair-graphical--windows
                                    (window-buffer dock)))))
      (delete-window side))))

(ert-deftest niwa-view-pair-graphical/no-steady-repair ()
  "Typing does not run repairs; a root split outside pair rules still does."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (redisplay t)
    (sit-for 0.02)
    (let* ((repairs 0)
           (observe (lambda (&rest _) (cl-incf repairs))))
      (advice-add 'niwa-view-pair--repair :before observe)
      (unwind-protect
          (progn
            (niwa-view-composer-graphical--type "This draft leaves the window tree alone.")
            (redisplay t)
            (sit-for 0.02)
            (should (zerop repairs))
            (select-window (niwa-view-transcript-window))
            (split-root-window-right)
            (niwa-view-pair-graphical--pairs view-buf 2)
            (should (> repairs 0)))
        (advice-remove 'niwa-view-pair--repair observe)))))

(ert-deftest niwa-view-pair-graphical/page-capped-draft ()
  "Paging scrolls the draft, not the transcript, up to the draft's top."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock))
           (draft (concat "> first line\n" (make-string (* 2 (frame-height)) ?\n) "Last line")))
      (insert draft)
      (niwa-view-composer-graphical--keys "M-<" 'beginning-of-buffer)
      (let ((top (window-start dock))
            (transcript-start (window-start transcript))
            (transcript-point (window-point transcript)))
        (should (< (window-end dock) (point-max)))
        (niwa-view-composer-graphical--keys "C-v" 'scroll-up-command)
        (should (> (window-start dock) top))
        (niwa-view-composer-graphical--keys "M-v" 'scroll-down-command)
        (should (= (window-start dock) top))
        (should (eq dock (selected-window)))
        (should (pos-visible-in-window-p (point) dock))
        (should (>= (point) (niwa-view-composer-draft-start)))
        (should (= (window-start transcript) transcript-start))
        (should (= (window-point transcript) transcript-point))
        (should (equal draft (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/page-fitting-draft ()
  "Paging a draft that fits cannot leave the rail offscreen."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((dock (selected-window)))
      (dolist (draft '("" "> short\nsecond line"))
        (insert draft)
        (niwa-view-composer-graphical--settle)
        (let ((inhibit-message t) (message-log-max nil))
          (niwa-view-composer-graphical--keys "C-v" 'scroll-up-command))
        (niwa-view-composer-graphical--settle)
        (should (= (point-min) (window-start dock)))
        (should (eq dock (selected-window)))
        (should (equal draft (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/composer-line-numbers ()
  "The composer starts without line numbers even if enabled in `text-mode'."
  :tags '(niwa-docked-composer)
  (let ((text-mode-hook (cons #'display-line-numbers-mode text-mode-hook)))
    (niwa-view-composer-graphical--with-chat
      (should-not display-line-numbers))))

(ert-deftest niwa-view-pair-graphical/scroll-transcript-to-top ()
  "Reaching the transcript top leaves the rail and end-of-draft caret alone."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (niwa-view-transcript-window dock))
           (draft "> short draft")
           (transcript-top (with-current-buffer view-buf (point-min)))
           (scroll-preserve-screen-position t)
           (mouse-wheel-follow-mouse t)
           (mouse-wheel-progressive-speed nil)
           (mouse-wheel-scroll-amount '(2)))
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list (* 2 (frame-height)) "More output.\n"))
       'response)
      (with-current-buffer view-buf (niwa-view--full-rerender))
      (niwa-view-transcript-show-end transcript)
      (insert draft)
      (niwa-view-composer-graphical--settle)
      (should (> (window-start transcript) transcript-top))
      (let ((limit (* 2 (frame-height)))
            (inhibit-message t) (message-log-max nil))
        (while (and (> (window-start transcript) transcript-top) (> (cl-decf limit) 0))
          (if (display-graphic-p)
              (niwa-view-composer-graphical--events
               (list (list 'wheel-up (posn-at-x-y 10 (/ (window-body-height transcript t) 2)
                                                 transcript)))
               'mwheel-scroll)
            (niwa-view-composer-graphical--keys "C-M-S-v" 'scroll-other-window-down))))
      (niwa-view-composer-graphical--settle)
      (should (= (window-start transcript) transcript-top))
      (should (= (window-start dock) (point-min)))
      (should (zerop (window-vscroll dock t)))
      (should (eq dock (selected-window)))
      (should (= (point) (point-max)))
      (should (equal draft (niwa-view-composer-graphical--draft view-buf))))))

(ert-deftest niwa-view-pair-graphical/minibuffer-quit-returns-to-draft ()
  "Canceling the command prompt restores the dock's window and draft caret."
  :tags '(niwa-docked-composer)
  (niwa-view-composer-graphical--with-chat
    (let ((dock (selected-window)) entered)
      (niwa-view-composer-graphical--type "Keep this draft")
      (backward-char 5)
      (let ((offset (- (point) (niwa-view-composer-draft-start)))
            (minibuffer-setup-hook (cons (lambda () (setq entered t)) minibuffer-setup-hook)))
        (let ((inhibit-message t) (message-log-max nil))
          (niwa-view-composer-graphical--keys "M-x C-g" 'execute-extended-command))
        (should entered)
        (should-not (active-minibuffer-window))
        (should (eq dock (selected-window)))
        (niwa-view-pair-graphical--pairs view-buf 1)
        (should (= offset (- (point) (niwa-view-composer-draft-start))))
        (should (equal "Keep this draft" (niwa-view-composer-graphical--draft view-buf)))))))

(ert-deftest niwa-view-pair-graphical/text-scale-preserves-draft ()
  "Local and global text scaling refit the dock without changing its draft."
  (niwa-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (draft "> keep\nthis draft"))
      (insert draft)
      (backward-char 5)
      (niwa-view-composer-graphical--settle)
      (let ((offset (- (point) (niwa-view-composer-draft-start)))
            (height (window-pixel-height dock)))
        (unwind-protect
            (pcase-dolist (`(,grow ,reset ,command)
                          '(("C-x C-+" "C-x C-0" text-scale-adjust)
                            ("C-x C-M-+" "C-x C-M-0" global-text-scale-adjust)))
              (niwa-view-composer-graphical--keys grow command)
              (niwa-view-composer-graphical--settle)
              (should (> (window-pixel-height dock) height))
              (should (eq dock (selected-window)))
              (should (= offset (- (point) (niwa-view-composer-draft-start))))
              (niwa-view-composer-graphical--keys reset command)
              (niwa-view-composer-graphical--settle)
              (should (= (window-pixel-height dock) height))
              (niwa-view-pair-graphical--pairs view-buf 1)
              (should (eq dock (selected-window)))
              (should (= offset (- (point) (niwa-view-composer-draft-start))))
              (should (equal draft (niwa-view-composer-graphical--draft view-buf))))
          (global-text-scale-adjust 0))))))

(provide 'test-niwa-view-pair-graphical)
;;; test-niwa-view-pair-graphical.el ends here
