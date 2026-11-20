;;; test-mevedel-view-pair-graphical.el --- Native transcript and composer pair tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise window commands on a real chat in graphical or terminal Emacs.

;;; Code:

(require 'mevedel-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-view-composer-test-helpers"))
(require 'mevedel-interaction-prompt)
(require 'mevedel-session-persistence)
(require 'mevedel-view-pair)
(require 'which-key)

(defun mevedel-view-pair-graphical--windows (buffer)
  "Return windows displaying BUFFER on the selected frame."
  (get-buffer-window-list buffer nil (selected-frame)))

(defun mevedel-view-pair-graphical--pairs (view count)
  "Assert VIEW has COUNT isolated pairs on this frame, without orphan drafts."
  (let (transcripts composers)
    (mevedel-test--await 2 (format "Expected %d settled pairs; got %S / %S"
                                    count transcripts composers)
      (progn
        (redisplay t)
        (setq transcripts (mevedel-view-pair-graphical--windows view)
              composers (mevedel-view-pair-graphical--windows
                         (mevedel-view-composer-buffer view)))
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
          (should (eq transcript (mevedel-view-transcript-window window)))
          (should (eq composer (mevedel-view-composer-window window))))))))

(defun mevedel-view-pair-graphical--switch (buffer)
  "Switch to BUFFER with native keys and return its dock at command completion."
  (let* ((unread-command-events (append (kbd "C-x b") (buffer-name buffer) '(?\r)))
         (deadline (run-at-time 5 nil #'exit-recursive-edit))
         finished dock
         (hook (lambda ()
                 ;; The minibuffer and its caller can report the same command.
                 (when (and (not finished) (eq (window-buffer) buffer))
                   (setq dock (mevedel-view-composer-window)
                         finished (run-at-time 0 nil #'exit-recursive-edit))))))
    (add-hook 'post-command-hook hook 100)
    (unwind-protect (recursive-edit)
      (remove-hook 'post-command-hook hook)
      (cancel-timer deadline)
      (when finished (cancel-timer finished)))
    (should (eq (window-buffer) buffer))
    dock))

(ert-deftest mevedel-view-pair-graphical/final-turn-rule ()
  "Show the plain closing rule only after another turn follows it."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock)))
      (insert "> draft\nsecond line")
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "An earlier response.")
        (let ((answer (line-beginning-position)))
          ;; Count the response and the native end-of-buffer row, no chrome.
          (should (= 2 (count-screen-lines
                        answer (mevedel-view-transcript-tail) t transcript)))
          (mevedel-view-test--insert-data data-buf "A new prompt.\n" nil)
          (mevedel-view--full-rerender)
          (goto-char answer)
          (search-forward "You\nA new prompt.")
          (should (= 3 (count-screen-lines
                        answer (match-beginning 0) nil transcript)))))
      (should (eq dock (selected-window)))
      (should (equal "> draft\nsecond line"
                     (mevedel-view-composer-graphical--draft view-buf))))))

(ert-deftest mevedel-view-pair-graphical/delete-other-windows ()
  "Keeping either half via the native command preserves the pair and draft caret."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (other (split-root-window-right)))
      (set-window-buffer other (get-buffer-create "*scratch*"))
      (mevedel-view-composer-graphical--type "Keep this draft")
      (let ((caret (point)))
        (mevedel-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
        (should (eq composer (selected-window)))
        (should (window-live-p transcript))
        (should-not (window-live-p other))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (should (equal "Keep this draft" (mevedel-view-composer-graphical--draft view-buf)))
        (should (= caret (point)))
        (mevedel-view-composer-graphical--keys "C-x o" 'other-window)
        (should (eq transcript (selected-window)))
        (let ((other (split-root-window-right)))
          (set-window-buffer other (get-buffer-create "*scratch*"))
          (mevedel-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
          (should-not (window-live-p other)))
        (should (eq transcript (selected-window)))
        (should (window-live-p composer))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (mevedel-view-composer-graphical--keys "C-x o" 'other-window)
        (should (eq composer (selected-window)))
        (should (= caret (window-point composer)))))))

(ert-deftest mevedel-view-pair-graphical/settlement-after-return ()
  "A settled card does not rewind a draft the user returned to edit."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           outcome)
      (mevedel-view-composer-graphical--type "> draft")
      (mevedel-view-composer-graphical--keys "RET" 'newline)
      (mevedel-view-composer-graphical--type "next")
      (with-current-buffer data-buf
        (mevedel--prompt-user-with-overlay
         "Confirm" "Do the thing" "Proceed?" nil (lambda (value) (setq outcome value))))
      (mevedel-view-composer-graphical--keys "C-c C-j" 'mevedel-view-jump-to-interaction)
      (should (eq transcript (selected-window)))
      (should (get-text-property (point) 'mevedel-prompt-focus))
      (mevedel-view-composer-graphical--keys "C-c C-SPC" 'mevedel-view-pair-toggle-focus)
      (should (eq dock (selected-window)))
      (mevedel-view-composer-graphical--type " line")
      (let ((caret (point)) (start (window-start dock)))
        (with-current-buffer view-buf
          (should (get-text-property (window-point transcript) 'mevedel-prompt-focus))
          (mevedel--prompt-dismiss-all))
        (mevedel-view-composer-graphical--settle)
        (should (eq outcome 'aborted))
        (should (eq dock (selected-window)))
        (should (equal "> draft\nnext line" (mevedel-view-composer-graphical--draft view-buf)))
        (should (= start (window-start dock)))
        (should (= caret (point)))))))

(ert-deftest mevedel-view-pair-graphical/split-and-delete ()
  "Splitting either member makes two pairs on one draft; deleting removes both halves."
  :tags '(mevedel-docked-composer)
  (pcase-dolist (`(,keys ,command ,edge) '(("C-x 2" split-window-below window-pixel-top)
                                          ("C-x 3" split-window-right window-pixel-left)))
    (dolist (from '(transcript composer))
      (ert-info ((format "%s and C-x 0 from %s" keys from))
        (mevedel-view-composer-graphical--with-chat
          (let* ((composer (selected-window))
                 (transcript (get-buffer-window view-buf))
                 (draft (window-buffer composer))
                 (origin (if (eq from 'composer) composer transcript)))
            (mevedel-view-composer-graphical--type "One draft")
            (select-window origin)
            (let* ((docks nil)
                   (observe (lambda ()
                              (when (eq this-command command)
                                (setq docks (get-buffer-window-list draft nil (selected-frame)))))))
              (add-hook 'post-command-hook observe 100)
              (unwind-protect
                  (mevedel-view-composer-graphical--keys keys command)
                (remove-hook 'post-command-hook observe))
              (should (= 2 (length docks))))
            (should (eq origin (selected-window)))
            (mevedel-view-pair-graphical--pairs view-buf 2)
            (should (= 4 (length (window-list nil 'nomini))))
            (let* ((new-transcript (car (remq transcript
                                             (mevedel-view-pair-graphical--windows view-buf))))
                   (new-composer (window-next-sibling new-transcript)))
              (should (> (funcall edge new-transcript) (funcall edge transcript)))
              (select-window new-composer)
              (mevedel-view-composer-graphical--keys "M->" 'end-of-buffer)
              (mevedel-view-composer-graphical--type " in both windows")
              (should (eq draft (window-buffer composer)))
              (should (eq draft (window-buffer new-composer)))
              (select-window origin)
              (mevedel-view-composer-graphical--keys "C-x 0" 'delete-window)
              (should-not (window-live-p transcript))
              (should-not (window-live-p composer))
              (should (window-live-p new-transcript))
              (should (window-live-p new-composer))
              (mevedel-view-pair-graphical--pairs view-buf 1)
              (should (equal "One draft in both windows"
                             (mevedel-view-composer-graphical--draft view-buf))))))))))

(ert-deftest mevedel-view-pair-graphical/split-fits-wrapped-draft ()
  "The split fits each dock once and grows the original before redisplay."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (fits 0)
           (observe-fit (lambda (&rest _) (cl-incf fits)))
           height after-split
           (observe-split (lambda ()
                            (when (eq this-command 'split-window-right)
                              (setq after-split (window-pixel-height dock))))))
      (insert (make-string (/ (* 3 (window-body-width dock)) 4) ?x))
      (mevedel-view-composer-graphical--settle)
      (setq height (window-pixel-height dock))
      (advice-add 'mevedel-composer--fit :before observe-fit)
      (add-hook 'post-command-hook observe-split 100)
      (unwind-protect
          (progn
            (mevedel-view-composer-graphical--keys "C-x 3" 'split-window-right)
            (mevedel-view-composer-graphical--settle)
            (should (> after-split height))
            (should (= after-split (window-pixel-height dock)))
            (should (= 2 fits)))
        (remove-hook 'post-command-hook observe-split)
        (advice-remove 'mevedel-composer--fit observe-fit)))))

(ert-deftest mevedel-view-pair-graphical/switch-buffer ()
  "Switching buffers replaces the transcript, undocks, and redocks on return."
  :tags '(mevedel-docked-composer)
  (dolist (from '(transcript composer))
    (ert-info ((format "C-x b from %s" from))
      (mevedel-view-composer-graphical--with-chat
        (let ((composer (selected-window))
              (transcript (get-buffer-window view-buf))
              (scratch (get-buffer-create "*scratch*")))
          (mevedel-view-composer-graphical--type "Saved while away")
          (when (eq from 'transcript) (select-window transcript))
          (mevedel-view-pair-graphical--switch scratch)
          (should (eq transcript (selected-window)))
          (should (eq scratch (window-buffer transcript)))
          (mevedel-view-pair-graphical--pairs view-buf 0)
          (should-not (window-live-p composer))
          (should (window-live-p (mevedel-view-pair-graphical--switch view-buf)))
          (should (eq transcript (selected-window)))
          (mevedel-view-pair-graphical--pairs view-buf 1)
          (should (equal "Saved while away" (mevedel-view-composer-graphical--draft view-buf))))))))

(ert-deftest mevedel-view-pair-graphical/restore-configuration ()
  "Restoring a layout with a missing composer docks it without taking focus."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (scratch (get-buffer-create "*scratch*")))
      (mevedel-view-composer-graphical--type "Survive layout restore")
      (select-window transcript)
      (let ((caret (point))
            ;; Capture a broken layout before redisplay has a chance to repair it.
            (register-alist nil))
        (let ((ignore-window-parameters t))
          (delete-window composer))
        (should (one-window-p))
        (window-configuration-to-register ?z)
        (mevedel-view-pair-graphical--switch scratch)
        (mevedel-view-pair-graphical--pairs view-buf 0)
        (mevedel-view-composer-graphical--keys "C-x r j z" 'jump-to-register)
        (should (eq transcript (selected-window)))
        (should (= caret (point)))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (should (equal "Survive layout restore"
                       (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-pair-graphical/help-and-restore ()
  "Help opens outside the pair and can be made the sole window without orphans."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (display-buffer-alist '(("\\`\\*Help\\*\\'" (display-buffer-below-selected))))
          (help-window-select nil))
      (mevedel-view-composer-graphical--type "Keep while reading Help")
      (mevedel-test--with-captured-messages nil
        (mevedel-view-composer-graphical--keys "C-h k C-c RET" 'describe-key))
      (let ((help (get-buffer-window "*Help*")))
        (should (window-live-p help))
        (with-current-buffer (window-buffer help)
          (should (string-search "mevedel-view-send" (buffer-string))))
        (should (eq composer (selected-window)))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (select-window help)
        (mevedel-view-composer-graphical--keys "C-x 1" 'delete-other-windows)
        (should (eq help (selected-window)))
        (mevedel-view-pair-graphical--pairs view-buf 0)
        (should (one-window-p))
        (mevedel-view-pair-graphical--switch view-buf)
        (should (eq help (selected-window)))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (should (equal "Keep while reading Help"
                       (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-pair-graphical/display-replaces-member ()
  "Repair keeps a display rule's replacement buffer, not stale pair rules."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (replacement (generate-new-buffer "*Replacement*"))
           (scratch (get-buffer-create "*scratch*"))
           (display-buffer-alist '(("\\`\\*Replacement\\*\\'" (display-buffer-same-window)))))
      (unwind-protect
          (progn
            (mevedel-view-composer-graphical--type "Preserved draft")
            ;; Emacs permits an explicit display rule to override dedication.
            (set-window-dedicated-p composer nil)
            (should (eq composer (display-buffer replacement)))
            (mevedel-view-pair-graphical--pairs view-buf 1)
            (should (eq replacement (window-buffer composer)))
            (should-not (eq composer (mevedel-view-composer-window transcript)))
            (should (eq composer (selected-window)))
            (should (= 3 (length (window-list nil 'nomini))))
            (should (equal "Preserved draft" (mevedel-view-composer-graphical--draft view-buf)))
            (let ((dock (mevedel-view-composer-window transcript)))
              (mevedel-view-pair-graphical--switch scratch)
              (mevedel-view-pair-graphical--pairs view-buf 1)
              (should (eq scratch (window-buffer composer)))
              (should (eq composer (selected-window)))
              (should (eq dock (mevedel-view-composer-window transcript)))
              (should (= 3 (length (window-list nil 'nomini)))))
            (mevedel-view-composer-graphical--keys "C-x 0" 'delete-window)
            (should-not (window-live-p composer))
            (mevedel-view-pair-graphical--pairs view-buf 1))
        (kill-buffer replacement)))))

(ert-deftest mevedel-view-pair-graphical/help-with-two-small-pairs ()
  "Stock Help display leaves both pairs intact when no leaf can split."
  (mevedel-view-composer-graphical--with-chat
    (let ((frame (selected-frame))
          (frames (frame-list))
          (dock (selected-window))
          (help-window-select nil))
      (unwind-protect
          (progn
            (mevedel-view-composer-graphical--type "Keep this draft")
            (mevedel-view-composer-graphical--keys "C-x 3" 'split-window-right)
            (mevedel-view-pair-graphical--pairs view-buf 2)
            (dolist (window (window-list frame 'nomini))
              (should-not (window-splittable-p window))
              (should-not (window-splittable-p window t)))
            (mevedel-test--with-captured-messages nil
              (mevedel-view-composer-graphical--keys "C-h k C-c RET" 'describe-key))
            (let ((popup (get-buffer-window "*Help*" t)))
              (should (window-live-p popup))
              (should-not (mevedel-view-transcript-window popup)))
            (with-selected-frame frame
              (should (eq dock (selected-window)))
              (mevedel-view-pair-graphical--pairs view-buf 2))
            (should (equal "Keep this draft" (mevedel-view-composer-graphical--draft view-buf))))
        (mapc #'delete-frame (seq-difference (frame-list) frames))))))

(ert-deftest mevedel-view-pair-graphical/resize ()
  "Growing another window shrinks the transcript, not the composer."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf))
           (other (split-root-window-below)))
      (set-window-buffer other (get-buffer-create "*scratch*"))
      (select-window other)
      (mevedel-view-composer-graphical--settle)
      (let ((draft-height (window-pixel-height composer))
            (transcript-height (window-pixel-height transcript)))
        (mevedel-view-composer-graphical--keys "C-u 5 C-x ^" 'enlarge-window)
        (should (eq other (selected-window)))
        (should (< (window-pixel-height transcript) transcript-height))
        (should (= (window-pixel-height composer) draft-height))
        (mevedel-view-pair-graphical--pairs view-buf 1)))))

(ert-deftest mevedel-view-pair-graphical/completions ()
  "Native mention completion opens outside the pair and leaves the draft focused."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (completion-auto-help t)
          (display-buffer-alist '(("\\`\\*Completions\\*\\'" (display-buffer-below-selected)))))
      (mevedel-view-composer-graphical--type "@")
      (mevedel-test--with-captured-messages nil
        (mevedel-view-composer-graphical--keys "C-M-i" 'completion-at-point))
      (let ((completions (get-buffer-window "*Completions*")))
        (should (window-live-p completions))
        (with-current-buffer (window-buffer completions)
          (should (string-search "@file:" (buffer-string)))
          (should (string-search "@note:" (buffer-string))))
        (should (eq composer (selected-window)))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (should (equal "@" (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-pair-graphical/which-key ()
  "Built-in which-key stays outside the pair, and its command acts in the draft."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((composer (selected-window))
          (which-key-idle-delay 0.1)
          (which-key-popup-type 'side-window)
          (was-enabled which-key-mode)
          popup focus outside timer)
      (mevedel-view-composer-graphical--type "Clear this draft")
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
            (mevedel-view-composer-graphical--keys "C-c" 'mevedel-view-history-clear-input)
            (should (windowp popup))
            (should outside)
            (should (eq composer focus))
            (should (eq composer (selected-window)))
            (should-not (get-buffer-window which-key-buffer-name))
            (should (equal "" (mevedel-view-composer-graphical--draft view-buf)))
            (mevedel-view-pair-graphical--pairs view-buf 1))
        (when timer (cancel-timer timer))
        (unless was-enabled (which-key-mode -1))))))

(ert-deftest mevedel-view-pair-graphical/resume-session ()
  "Resuming a saved session shows its transcript and a working paired composer."
  :tags '(mevedel-docked-composer)
  (mevedel-test--with-captured-messages nil
    (mevedel-view-composer-graphical--with-chat
      (let (reopened)
        (unwind-protect
            (progn
              (with-current-buffer data-buf (mevedel-save-session))
              (should (mevedel-session-save-path session))
              (mevedel-chat-discard-buffers data-buf t)
              (should-not (buffer-live-p view-buf))
              (setq reopened (mevedel-session-persistence-resume-id
                              workspace (mevedel-session-session-id session)))
              (should (buffer-live-p reopened))
              (let ((restored (buffer-local-value 'mevedel--session reopened)))
                (should-not (eq session restored))
                (should (equal (mevedel-session-session-id session)
                               (mevedel-session-session-id restored))))
              (let* ((fits 0)
                     (observe (lambda (&rest _) (cl-incf fits))))
                (advice-add 'mevedel-composer--fit :before observe)
                (unwind-protect
                    (progn
                      (mevedel--display-chat-buffer reopened)
                      (mevedel-view-composer-graphical--settle)
                      (should (= 1 fits)))
                  (advice-remove 'mevedel-composer--fit observe)))
              (let* ((composer (selected-window))
                     (transcript (mevedel-view-transcript-window composer))
                     (view (window-buffer transcript)))
                (mevedel-view-pair-graphical--pairs view 1)
                (should (eq composer (mevedel-view-composer-window transcript)))
                (mevedel-view-composer-graphical--type "Resumed draft")
                (should (equal "Resumed draft" (mevedel-view-composer-graphical--draft view)))))
          (when reopened (mevedel-chat-discard-buffers reopened t)))))))

(ert-deftest mevedel-view-pair-graphical/native-side-slot ()
  "Leave a transcript in a native side slot alone across redisplays."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (side (display-buffer-in-side-window view-buf '((side . right)))))
      (should (eq 'right (window-parameter side 'window-side)))
      (dotimes (_ 3)
        (redisplay t)
        (sit-for 0.02)
        (should (eq (window-buffer side) view-buf))
        (should (eq 'right (window-parameter side 'window-side)))
        (should-not (mevedel-view-composer-window side))
        (should (equal (list dock) (mevedel-view-pair-graphical--windows
                                    (window-buffer dock)))))
      (delete-window side))))

(ert-deftest mevedel-view-pair-graphical/no-steady-repair ()
  "Typing does not run repairs; a root split outside pair rules still does."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (redisplay t)
    (sit-for 0.02)
    (let* ((repairs 0)
           (observe (lambda (&rest _) (cl-incf repairs))))
      (advice-add 'mevedel-view-pair--repair :before observe)
      (unwind-protect
          (progn
            (mevedel-view-composer-graphical--type "This draft leaves the window tree alone.")
            (redisplay t)
            (sit-for 0.02)
            (should (zerop repairs))
            (select-window (mevedel-view-transcript-window))
            (split-root-window-right)
            (mevedel-view-pair-graphical--pairs view-buf 2)
            (should (> repairs 0)))
        (advice-remove 'mevedel-view-pair--repair observe)))))

(ert-deftest mevedel-view-pair-graphical/page-capped-draft ()
  "Paging scrolls the draft, not the transcript, up to the draft's top."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           (draft (concat "> first line\n" (make-string (* 2 (frame-height)) ?\n) "Last line")))
      (insert draft)
      (mevedel-view-composer-graphical--keys "M-<" 'beginning-of-buffer)
      (let ((top (window-start dock))
            (transcript-start (window-start transcript))
            (transcript-point (window-point transcript)))
        (should (< (window-end dock) (point-max)))
        (mevedel-view-composer-graphical--keys "C-v" 'scroll-up-command)
        (should (> (window-start dock) top))
        (mevedel-view-composer-graphical--keys "M-v" 'scroll-down-command)
        (should (= (window-start dock) top))
        (should (eq dock (selected-window)))
        (should (pos-visible-in-window-p (point) dock))
        (should (>= (point) (mevedel-view-composer-draft-start)))
        (should (= (window-start transcript) transcript-start))
        (should (= (window-point transcript) transcript-point))
        (should (equal draft (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-pair-graphical/minibuffer-quit-returns-to-draft ()
  "Canceling the command prompt restores the dock's window and draft caret."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((dock (selected-window)) entered)
      (mevedel-view-composer-graphical--type "Keep this draft")
      (backward-char 5)
      (let ((offset (- (point) (mevedel-view-composer-draft-start)))
            (minibuffer-setup-hook (cons (lambda () (setq entered t)) minibuffer-setup-hook)))
        (let ((inhibit-message t) (message-log-max nil))
          (mevedel-view-composer-graphical--keys "M-x C-g" 'execute-extended-command))
        (should entered)
        (should-not (active-minibuffer-window))
        (should (eq dock (selected-window)))
        (mevedel-view-pair-graphical--pairs view-buf 1)
        (should (= offset (- (point) (mevedel-view-composer-draft-start))))
        (should (equal "Keep this draft" (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-pair-graphical/text-scale-preserves-draft ()
  "Local and global text scaling refit the dock without changing its draft."
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (draft "> keep\nthis draft"))
      (insert draft)
      (backward-char 5)
      (mevedel-view-composer-graphical--settle)
      (let ((offset (- (point) (mevedel-view-composer-draft-start)))
            (height (window-pixel-height dock)))
        (unwind-protect
            (pcase-dolist (`(,grow ,reset ,command)
                          '(("C-x C-+" "C-x C-0" text-scale-adjust)
                            ("C-x C-M-+" "C-x C-M-0" global-text-scale-adjust)))
              (mevedel-view-composer-graphical--keys grow command)
              (mevedel-view-composer-graphical--settle)
              (should (> (window-pixel-height dock) height))
              (should (eq dock (selected-window)))
              (should (= offset (- (point) (mevedel-view-composer-draft-start))))
              (mevedel-view-composer-graphical--keys reset command)
              (mevedel-view-composer-graphical--settle)
              (should (= (window-pixel-height dock) height))
              (mevedel-view-pair-graphical--pairs view-buf 1)
              (should (eq dock (selected-window)))
              (should (= offset (- (point) (mevedel-view-composer-draft-start))))
              (should (equal draft (mevedel-view-composer-graphical--draft view-buf))))
          (global-text-scale-adjust 0))))))

(provide 'test-mevedel-view-pair-graphical)
;;; test-mevedel-view-pair-graphical.el ends here
