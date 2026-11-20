;;; mevedel-view-pair.el -- Docked transcript and composer windows -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the window combination and its command rules.  Redisplay only
;; queues affected frames; the timer reads the current tree before editing it.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-utilities)

;; `mevedel-view-composer'
(declare-function mevedel-composer--fit "mevedel-view-composer"
                  (dock transcript &optional fresh))
(declare-function mevedel-view-transcript-tail "mevedel-view-composer" ())
(defvar mevedel-composer--view)
(defvar mevedel-view--composer-buffer)

(defvar mevedel-view-pair--frames nil
  "Frames queued for deferred repair.")

(defvar mevedel-view-pair--timer nil
  "Timer coalescing pending frame repairs.")

(defvar-local mevedel-view-pair--hidden-rules nil
  "Window-local overlays hiding this transcript's redundant closing rule.")

(defconst mevedel-view-pair--closing-newline (propertize "\n" 'face 'default)
  "Plain line break replacing the paired transcript's closing chrome.")

(defconst mevedel-view-pair--transcript-rules
  '((mode-line-format . none)
    (split-window . mevedel-view-pair--split)
    (delete-window . mevedel-view-pair--delete)))

(defconst mevedel-view-pair--composer-rules
  '((no-other-window . nil)
    (no-delete-other-windows . t)
    (mevedel-view-geometry-excluded . t)
    (split-window . mevedel-view-pair--split)
    (delete-window . mevedel-view-pair--delete)
    (delete-other-windows . mevedel-view-pair--only)))

(defun mevedel-view-pair--draft (window)
  "Return WINDOW's eligible draft buffer, if any."
  (unless (or (window-minibuffer-p window)
              (window-parameter window 'window-side))
    (with-current-buffer (window-buffer window)
      (and (derived-mode-p 'mevedel-view-mode)
           (not mevedel-view--agent-transcript-p)
           (buffer-live-p mevedel-view--composer-buffer)
           (not (buffer-local-value 'buffer-read-only mevedel-view--composer-buffer))
           mevedel-view--composer-buffer))))

(defun mevedel-view-pair--dock-below (window)
  "Return the lower leaf of WINDOW's two-window combination, if any."
  (and (window-combined-p window)
       (not (window-prev-sibling window))
       (let ((next (window-next-sibling window)))
         (and (window-live-p next) (not (window-next-sibling next)) next))))

(defun mevedel-view-pair--matches-p (transcript dock)
  "Return non-nil when DOCK is in position for TRANSCRIPT's draft."
  (and (window-live-p transcript) (window-live-p dock)
       (eq (mevedel-view-pair--dock-below transcript) dock)
       (eq (mevedel-view-pair--draft transcript) (window-buffer dock))))

(defun mevedel-view-transcript-window (&optional window)
  "Return the transcript window for WINDOW, without changing the layout.
WINDOW defaults to the selected window.  An unpaired view, including a
read-only transcript, is its own transcript window."
  (let ((window (or window (selected-window))))
    (when (window-live-p window)
      (if (with-current-buffer (window-buffer window)
            (derived-mode-p 'mevedel-view-mode))
          window
        (let ((transcript (window-prev-sibling window)))
          (when (mevedel-view-pair--matches-p transcript window)
            transcript))))))

(defun mevedel-view-composer-window (&optional window)
  "Return the dock for WINDOW, without changing the layout.
WINDOW defaults to the selected window and may be either pair member."
  (when-let* ((transcript (mevedel-view-transcript-window window))
              (dock (mevedel-view-pair--dock-below transcript))
              ((mevedel-view-pair--matches-p transcript dock)))
    dock))

(defun mevedel-view-pair-refresh-turn-rule ()
  "Hide the plain rule at the current transcript's end in paired windows.
Use overlays so source anchors and unpaired windows keep the original text."
  (let* ((end (mevedel-view-transcript-tail))
         (start (when (and end (> (1- end) (point-min)) (<= end (point-max))
                           (eq (char-before (1- end)) ?\n)
                           (eq (get-text-property (1- end) 'font-lock-face)
                               'mevedel-view-turn-rule))
                  (- end (if (eq (char-before (- end 2)) ?\n) 3 2)))))
    (when (or start mevedel-view-pair--hidden-rules)
      (let ((windows (and start (get-buffer-window-list (current-buffer) nil t))))
        (dolist (overlay mevedel-view-pair--hidden-rules)
          (let ((window (overlay-get overlay 'window)))
            (if (and (memq window windows)
                     (eq (window-parameter window 'mevedel-view-pair--role) 'transcript)
                     (mevedel-view-composer-window window))
                (progn
                  (setq windows (delq window windows))
                  (unless (and (eql start (overlay-start overlay))
                               (eql end (overlay-end overlay)))
                    (move-overlay overlay start end)))
              (delete-overlay overlay)
              (setq mevedel-view-pair--hidden-rules
                    (delq overlay mevedel-view-pair--hidden-rules)))))
        (dolist (window windows)
          (when (and (eq (window-parameter window 'mevedel-view-pair--role) 'transcript)
                     (mevedel-view-composer-window window))
            (let ((overlay (make-overlay start end)))
              (overlay-put overlay 'window window)
              ;; Keep a displayed newline for native last-page traversal.
              (overlay-put overlay 'display mevedel-view-pair--closing-newline)
              (push overlay mevedel-view-pair--hidden-rules))))))))

(defun mevedel-view-pair-toggle-focus ()
  "Move focus between this transcript and its composer."
  (interactive)
  (when-let* ((window (selected-window))
              (dock (mevedel-view-composer-window window)))
    (select-window (if (eq window dock)
                       (mevedel-view-transcript-window dock)
                     dock))))

(defun mevedel-view-pair--install-rules (window rules)
  "Set RULES on WINDOW, saving previous values for when it is reused."
  (set-window-parameter
   window 'mevedel-view-pair--previous
   (mapcar (lambda (rule)
             (cons (car rule) (window-parameter window (car rule))))
           rules))
  (dolist (rule rules)
    (set-window-parameter window (car rule) (cdr rule)))
  (with-current-buffer (window-buffer window)
    (force-mode-line-update)))

(defun mevedel-view-pair--unbind (window)
  "Set aside mevedel's rules on WINDOW, leaving unrelated rules alone."
  (when-let* ((role (window-parameter window 'mevedel-view-pair--role)))
    (let ((rules (if (eq role 'transcript)
                     mevedel-view-pair--transcript-rules
                   mevedel-view-pair--composer-rules)))
      (dolist (entry (window-parameter window 'mevedel-view-pair--previous))
        (when (eq (window-parameter window (car entry))
                  (alist-get (car entry) rules))
          (set-window-parameter window (car entry) (cdr entry)))))
    (when (eq (window-dedicated-p window) 'mevedel-view-pair)
      (set-window-dedicated-p window nil))
    (when (and (eq role 'composer)
               (buffer-local-value 'mevedel-composer--view (window-buffer window)))
      (window-preserve-size window nil nil)
      (when (eq (window-dedicated-p window) t)
        (set-window-dedicated-p window nil)))
    (set-window-parameter window 'mevedel-view-pair--role nil)
    (set-window-parameter window 'mevedel-view-pair--previous nil)
    (with-current-buffer (window-buffer window)
      (when (eq role 'transcript)
        (mevedel-view-pair-refresh-turn-rule))
      (force-mode-line-update))))

(defun mevedel-view-pair--bind (transcript dock)
  "Install the pair rules for TRANSCRIPT and DOCK once."
  (unless (eq (window-parameter transcript 'mevedel-view-pair--role) 'transcript)
    (mevedel-view-pair--install-rules transcript mevedel-view-pair--transcript-rules)
    (set-window-parameter transcript 'mevedel-view-pair--role 'transcript))
  ;; Weak dedication excludes popup reuse but still lets C-x b switch buffers.
  (unless (window-dedicated-p transcript)
    (set-window-dedicated-p transcript 'mevedel-view-pair))
  (unless (eq (window-parameter dock 'mevedel-view-pair--role) 'composer)
    (mevedel-view-pair--install-rules dock mevedel-view-pair--composer-rules)
    (set-window-parameter dock 'mevedel-view-pair--role 'composer)
    (set-window-dedicated-p dock t)
    (window-preserve-size dock nil t))
  (with-current-buffer (window-buffer transcript)
    (mevedel-view-pair-refresh-turn-rule)))

(defun mevedel-view-pair--split (window size side &optional pixelwise)
  "Split WINDOW's pair using `split-window' SIZE, SIDE, and PIXELWISE."
  (let* ((transcript (mevedel-view-transcript-window window))
         (dock (and transcript (mevedel-view-composer-window transcript)))
         (parent (and dock (window-parent transcript)))
         (new (let ((ignore-window-parameters t))
                (split-window (or parent window) size side pixelwise))))
    (when parent
      ;; An internal-window split can inherit the selected composer's buffer.
      (set-window-buffer new (window-buffer transcript))
      (set-window-start new (window-start transcript))
      (set-window-point new (window-point transcript))
      ;; A display action can still replace NEW with its destination buffer.
      (if (memq this-command '(split-window-below split-window-right))
          (unless noninteractive
            (with-current-buffer (window-buffer dock)
              (mevedel-composer--fit dock transcript t))
            (mevedel-view-composer-display new))
        (mevedel-view-pair--schedule (window-frame transcript))))
    new))

(defun mevedel-view-pair--delete (window)
  "Delete WINDOW's whole pair, subject to the native last-window rule."
  (let* ((transcript (mevedel-view-transcript-window window))
         (ignore-window-parameters t))
    (delete-window (if (and transcript (mevedel-view-composer-window transcript))
                       (window-parent transcript)
                     window))))

(defun mevedel-view-pair--only (window)
  "Keep WINDOW's pair rather than a lone composer."
  (if-let* ((transcript (mevedel-view-transcript-window window)))
      (delete-other-windows transcript)
    (let ((ignore-window-parameters t))
      (delete-other-windows window))))

(defun mevedel-view-pair-switch-buffer ()
  "Switch buffers in the transcript instead of replacing this draft window."
  (interactive)
  (let* ((dock (selected-window))
         (transcript (or (mevedel-view-transcript-window dock)
                         (mevedel-diagnostic-user-error
                          (mevedel-diagnostic-scope (current-buffer))
                          "No transcript for this composer"))))
    (select-window transcript)
    (unwind-protect
        (call-interactively #'switch-to-buffer)
      (when (and (eq (selected-window) transcript)
                 (eq (mevedel-view-composer-window transcript) dock))
        (select-window dock)))))

(defun mevedel-view-pair--make (transcript draft)
  "Dock DRAFT directly below TRANSCRIPT, or return nil if it cannot fit."
  (when-let* ((dock (let ((window-combination-limit t)
                          (ignore-window-parameters t))
                      (ignore-errors (split-window transcript -4 'below)))))
    (set-window-buffer dock draft)
    (mevedel-view-pair--bind transcript dock)
    ;; Do not paint a provisional height only to resize and repaint it.
    (unless noninteractive
      (with-current-buffer draft (mevedel-composer--fit dock transcript t)))
    dock))

(defun mevedel-view-composer-display (view-window)
  "Display the draft for VIEW-WINDOW, without selecting either window.
Return nil for read-only views and when there is not room for a dock."
  (when-let* ((transcript (mevedel-view-transcript-window view-window))
              (draft (mevedel-view-pair--draft transcript)))
    (let* ((frame (window-frame transcript))
           (below (mevedel-view-pair--dock-below transcript))
           (dock (cond
                  ((and below (eq (window-buffer below) draft))
                   (mevedel-view-pair--bind transcript below)
                   below)
                  (t
                   ;; A surface reusing TRANSCRIPT can still have its old dock.
                   (when (and below (buffer-local-value 'mevedel-composer--view
                                                        (window-buffer below)))
                     (mevedel-view-pair--remove below))
                   (mevedel-view-pair--make transcript draft)))))
      (mevedel-view-pair--set-state frame (if dock 'paired 'small))
      dock)))

(defun mevedel-view-pair--remove (dock)
  "Remove orphan DOCK without killing or editing its draft."
  (let ((ignore-window-parameters t))
    (mevedel-view-pair--unbind dock)
    (if (memq dock (list (frame-root-window (window-frame dock))
                         (window-main-window (window-frame dock))))
        (let* ((buffer (window-buffer dock))
               (view (buffer-local-value 'mevedel-composer--view buffer)))
          (set-window-dedicated-p dock nil)
          (set-window-buffer dock (if (buffer-live-p view) view (other-buffer buffer))))
      (delete-window dock))))

(defun mevedel-view-pair-hide (view)
  "Undock the composer owned by VIEW when visiting a read-only segment."
  (dolist (window (get-buffer-window-list view nil t))
    (mevedel-view-pair--unbind window))
  (dolist (dock (get-buffer-window-list
                 (buffer-local-value 'mevedel-view--composer-buffer view) nil t))
    (mevedel-view-pair--remove dock)))

(defun mevedel-view-pair--kill-draft ()
  "Remove the current draft's docks before Emacs deletes dedicated windows."
  (when (buffer-live-p mevedel-composer--view)
    (mevedel-view-pair-hide mevedel-composer--view)))

(defun mevedel-view-pair-plan (transcripts composers)
  "Return the repairs for TRANSCRIPTS and COMPOSERS, without side effects.
Each transcript entry is (WINDOW DRAFT DOCK), where DOCK is the window
in the composer position of its combination, if any.  COMPOSERS maps
composer windows to their buffers.  Return :keep pairs (including pairs
whose rules need reinstalling), :add transcripts, and :remove docks."
  (let (keep add remove)
    (pcase-dolist (`(,window ,draft ,dock) transcripts)
      (if (and dock (eq (alist-get dock composers) draft))
          (push (cons window dock) keep)
        (push window add)))
    (dolist (entry composers)
      (unless (rassq (car entry) keep)
        (push (car entry) remove)))
    (list :keep (nreverse keep) :add (nreverse add) :remove (nreverse remove))))

(defun mevedel-view-pair--repair (frame)
  "Repair FRAME using its layout now, not the layout seen by redisplay."
  (let (transcripts composers owned small)
    (dolist (window (window-list frame 'nomini))
      (when (window-parameter window 'mevedel-view-pair--role)
        (push window owned))
      (if (buffer-local-value 'mevedel-composer--view (window-buffer window))
          (push (cons window (window-buffer window)) composers)
        (when-let* ((draft (mevedel-view-pair--draft window)))
          (push (list window draft (mevedel-view-pair--dock-below window))
                transcripts))))
    (let* ((plan (mevedel-view-pair-plan transcripts composers))
           (keep (plist-get plan :keep))
           (add (plist-get plan :add))
           (remove (plist-get plan :remove)))
      (dolist (window owned)
        (unless (or (assq window keep) (rassq window keep) (memq window remove))
          (mevedel-view-pair--unbind window)))
      (dolist (dock remove)
        (mevedel-view-pair--remove dock))
      (dolist (dock remove)
        (when (and (window-live-p dock) (mevedel-view-pair--draft dock))
          (cl-pushnew dock add)))
      (dolist (pair keep)
        (mevedel-view-pair--bind (car pair) (cdr pair)))
      (dolist (transcript add)
        (unless (mevedel-view-composer-display transcript)
          (setq small t)))
      (mevedel-view-pair--set-state frame (cond (small 'small) ((or keep add) 'paired))))))

(defun mevedel-view-pair--set-state (frame state)
  "Record STATE on FRAME only if changed."
  (unless (eq (frame-parameter frame 'mevedel-view-pair--state) state)
    (set-frame-parameter frame 'mevedel-view-pair--state state)))

(defun mevedel-view-pair--drain ()
  "Apply pending repairs outside redisplay."
  (let ((frames mevedel-view-pair--frames))
    (setq mevedel-view-pair--frames nil
          mevedel-view-pair--timer nil)
    (dolist (frame frames)
      (when (frame-live-p frame)
        (mevedel-view-pair--repair frame)))))

(defun mevedel-view-pair--schedule (frame)
  "Queue one deferred repair of FRAME."
  (unless noninteractive
    (cl-pushnew frame mevedel-view-pair--frames)
    (unless mevedel-view-pair--timer
      (setq mevedel-view-pair--timer (run-at-time 0 nil #'mevedel-view-pair--drain)))))

(defun mevedel-view-pair--damaged-p (window)
  "Return non-nil if WINDOW needs its dock or window rules repaired."
  (let ((role (window-parameter window 'mevedel-view-pair--role)))
    (cond
     ((buffer-local-value 'mevedel-composer--view (window-buffer window))
      (not (and (eq role 'composer) (mevedel-view-transcript-window window))))
     ((mevedel-view-pair--draft window)
      (not (and (eq role 'transcript)
                (window-dedicated-p window)
                (mevedel-view-composer-window window))))
     (role))))

(defun mevedel-view-pair--buffer-change (window)
  "Notice a new transcript or draft, or a stale pair rule on WINDOW."
  (when (mevedel-view-pair--damaged-p window)
    (mevedel-view-pair--schedule (window-frame window))))

(defun mevedel-view-pair--frame-change (frame)
  "Notice changed and deleted windows on a previously paired FRAME."
  ;; Explicit runs of the buffer-local hook can also reach the default value.
  (when (and (framep frame) (frame-parameter frame 'mevedel-view-pair--state))
    (let (paired)
      (when (or (catch 'damaged
                  (walk-window-tree
                   (lambda (window)
                     (when (mevedel-view-pair--damaged-p window)
                       (throw 'damaged t))
                     (when (window-parameter window 'mevedel-view-pair--role)
                       (setq paired t)))
                   frame nil 'nomini)
                  nil)
                (not paired))
        (mevedel-view-pair--schedule frame)))))

(defun mevedel-view-pair--configuration-change ()
  "Notice restored rules and retry docks previously too small to fit."
  (mevedel-view-pair--frame-change (selected-frame)))

(defun mevedel-view-pair--after-switch ()
  "Dock a switched-to transcript once the native display action has finished."
  (when (and (memq this-command '(switch-to-buffer mevedel-view-pair-switch-buffer))
             (eq (window-buffer) (current-buffer))
             (not (mevedel-view-composer-window)))
    (mevedel-view-composer-display (selected-window))))

(defun mevedel-view-pair--window-deletable-p (window _frame)
  "Keep native buffer teardown from deleting a weakly dedicated WINDOW."
  (not (eq (window-dedicated-p window) 'mevedel-view-pair)))

(defun mevedel-view-pair-watch (draft)
  "Observe layout changes for the current view and its DRAFT buffer."
  (add-hook 'post-command-hook #'mevedel-view-pair--after-switch nil t)
  (add-hook 'window-deletable-functions #'mevedel-view-pair--window-deletable-p nil t)
  (add-hook 'window-buffer-change-functions #'mevedel-view-pair--buffer-change nil t)
  (with-current-buffer draft
    (add-hook 'window-buffer-change-functions #'mevedel-view-pair--buffer-change nil t)
    (add-hook 'kill-buffer-hook #'mevedel-view-pair--kill-draft nil t))
  (add-hook 'window-buffer-change-functions #'mevedel-view-pair--frame-change)
  (add-hook 'window-configuration-change-hook #'mevedel-view-pair--configuration-change))

(provide 'mevedel-view-pair)
;;; mevedel-view-pair.el ends here
