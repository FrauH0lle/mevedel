;;; niwa-view-pair.el -- Docked transcript and composer windows -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the window combination and its command rules.  Redisplay only
;; queues affected frames; the timer reads the current tree before editing it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'niwa-structs)
(require 'niwa-utilities)

;; `niwa-view-composer'
(declare-function niwa-composer--fit "niwa-view-composer"
                  (dock transcript &optional fresh))
(declare-function niwa-view-transcript-tail "niwa-view-composer" ())
(defvar niwa-composer--view)
(defvar niwa-view--composer-buffer)

(defvar niwa-view-pair--frames nil
  "Frames queued for deferred repair.")

(defvar niwa-view-pair--timer nil
  "Timer coalescing pending frame repairs.")

(defvar-local niwa-view-pair--hidden-rules nil
  "Window-local overlays hiding this transcript's redundant closing rule.")

(defconst niwa-view-pair--closing-newline (propertize "\n" 'face 'default)
  "Plain line break replacing the paired transcript's closing chrome.")

(defconst niwa-view-pair--transcript-rules
  '((mode-line-format . none)
    ;; Workspace commands count `visible' windows even when dedicated.
    (visible . t)
    (split-window . niwa-view-pair--split)
    (delete-window . niwa-view-pair--delete)))

(defconst niwa-view-pair--composer-rules
  '((no-other-window . nil)
    (no-delete-other-windows . t)
    (niwa-view-geometry-excluded . t)
    (split-window . niwa-view-pair--split)
    (delete-window . niwa-view-pair--delete)
    (delete-other-windows . niwa-view-pair--only)))

(defun niwa-view-pair--draft (window)
  "Return WINDOW's eligible draft buffer, if any."
  (unless (or (window-minibuffer-p window)
              (window-parameter window 'window-side))
    (with-current-buffer (window-buffer window)
      (and (derived-mode-p 'niwa-view-mode)
           (not niwa-view--agent-transcript-p)
           (buffer-live-p niwa-view--composer-buffer)
           (not (buffer-local-value 'buffer-read-only niwa-view--composer-buffer))
           niwa-view--composer-buffer))))

(defun niwa-view-pair--dock-below (window)
  "Return the lower leaf of WINDOW's two-window combination, if any."
  (and (window-combined-p window)
       (not (window-prev-sibling window))
       (let ((next (window-next-sibling window)))
         (and (window-live-p next) (not (window-next-sibling next)) next))))

(defun niwa-view-pair--matches-p (transcript dock)
  "Return non-nil when DOCK is in position for TRANSCRIPT's draft."
  (and (window-live-p transcript) (window-live-p dock)
       (eq (niwa-view-pair--dock-below transcript) dock)
       (eq (niwa-view-pair--draft transcript) (window-buffer dock))))

(defun niwa-view-transcript-window (&optional window)
  "Return the transcript window for WINDOW, without changing the layout.
WINDOW defaults to the selected window.  An unpaired view, including a
read-only transcript, is its own transcript window."
  (let ((window (or window (selected-window))))
    (when (window-live-p window)
      (if (with-current-buffer (window-buffer window)
            (derived-mode-p 'niwa-view-mode))
          window
        (let ((transcript (window-prev-sibling window)))
          (when (niwa-view-pair--matches-p transcript window)
            transcript))))))

(defun niwa-view-composer-window (&optional window)
  "Return the dock for WINDOW, without changing the layout.
WINDOW defaults to the selected window and may be either pair member."
  (when-let* ((transcript (niwa-view-transcript-window window))
              (dock (niwa-view-pair--dock-below transcript))
              ((niwa-view-pair--matches-p transcript dock)))
    dock))

(defun niwa-view-pair-refresh-turn-rule ()
  "Hide the plain rule at the current transcript's end in paired windows.
Use overlays so source anchors and unpaired windows keep the original text."
  (let* ((end (niwa-view-transcript-tail))
         (start (when (and end (> (1- end) (point-min)) (<= end (point-max))
                           (eq (char-before (1- end)) ?\n)
                           (eq (get-text-property (1- end) 'font-lock-face)
                               'niwa-view-turn-rule))
                  (- end (if (eq (char-before (- end 2)) ?\n) 3 2)))))
    (when (or start niwa-view-pair--hidden-rules)
      (let ((windows (and start (get-buffer-window-list (current-buffer) nil t))))
        (dolist (overlay niwa-view-pair--hidden-rules)
          (let ((window (overlay-get overlay 'window)))
            (if (and (memq window windows)
                     (eq (window-parameter window 'niwa-view-pair--role) 'transcript)
                     (niwa-view-composer-window window))
                (progn
                  (setq windows (delq window windows))
                  (unless (and (eql start (overlay-start overlay))
                               (eql end (overlay-end overlay)))
                    (move-overlay overlay start end)))
              (delete-overlay overlay)
              (setq niwa-view-pair--hidden-rules
                    (delq overlay niwa-view-pair--hidden-rules)))))
        (dolist (window windows)
          (when (and (eq (window-parameter window 'niwa-view-pair--role) 'transcript)
                     (niwa-view-composer-window window))
            (let ((overlay (make-overlay start end)))
              (overlay-put overlay 'window window)
              ;; Keep a displayed newline for native last-page traversal.
              (overlay-put overlay 'display niwa-view-pair--closing-newline)
              (push overlay niwa-view-pair--hidden-rules))))))))

(defun niwa-view-pair-toggle-focus ()
  "Move focus between this transcript and its composer."
  (interactive)
  (when-let* ((window (selected-window))
              (dock (niwa-view-composer-window window)))
    (select-window (if (eq window dock)
                       (niwa-view-transcript-window dock)
                     dock))))

(defun niwa-view-pair--install-rules (window rules)
  "Set RULES on WINDOW, saving previous values for when it is reused."
  (set-window-parameter
   window 'niwa-view-pair--previous
   (mapcar (lambda (rule)
             (cons (car rule) (window-parameter window (car rule))))
           rules))
  (dolist (rule rules)
    (set-window-parameter window (car rule) (cdr rule)))
  (with-current-buffer (window-buffer window)
    (force-mode-line-update)))

(defun niwa-view-pair--unbind (window)
  "Set aside niwa's rules on WINDOW, leaving unrelated rules alone."
  (when-let* ((role (window-parameter window 'niwa-view-pair--role)))
    (let ((rules (if (eq role 'transcript)
                     niwa-view-pair--transcript-rules
                   niwa-view-pair--composer-rules)))
      (dolist (entry (window-parameter window 'niwa-view-pair--previous))
        (when (eq (window-parameter window (car entry))
                  (alist-get (car entry) rules))
          (set-window-parameter window (car entry) (cdr entry)))))
    (when (eq (window-dedicated-p window) 'niwa-view-pair)
      (set-window-dedicated-p window nil))
    (when (and (eq role 'composer)
               (buffer-local-value 'niwa-composer--view (window-buffer window)))
      (window-preserve-size window nil nil)
      (when (eq (window-dedicated-p window) t)
        (set-window-dedicated-p window nil)))
    (set-window-parameter window 'niwa-view-pair--role nil)
    (set-window-parameter window 'niwa-view-pair--previous nil)
    (with-current-buffer (window-buffer window)
      (when (eq role 'transcript)
        (niwa-view-pair-refresh-turn-rule))
      (force-mode-line-update))))

(defun niwa-view-pair--bind (transcript dock)
  "Install the pair rules for TRANSCRIPT and DOCK once."
  (unless (eq (window-parameter transcript 'niwa-view-pair--role) 'transcript)
    (niwa-view-pair--install-rules transcript niwa-view-pair--transcript-rules)
    (set-window-parameter transcript 'niwa-view-pair--role 'transcript))
  ;; Weak dedication excludes popup reuse but still lets C-x b switch buffers.
  (unless (window-dedicated-p transcript)
    (set-window-dedicated-p transcript 'niwa-view-pair))
  (unless (eq (window-parameter dock 'niwa-view-pair--role) 'composer)
    (niwa-view-pair--install-rules dock niwa-view-pair--composer-rules)
    (set-window-parameter dock 'niwa-view-pair--role 'composer)
    (set-window-dedicated-p dock t)
    (window-preserve-size dock nil t))
  (with-current-buffer (window-buffer transcript)
    (niwa-view-pair-refresh-turn-rule)))

(defun niwa-view-pair--split (window size side &optional pixelwise)
  "Split WINDOW's pair using `split-window' SIZE, SIDE, and PIXELWISE."
  (let* ((transcript (niwa-view-transcript-window window))
         (dock (and transcript (niwa-view-composer-window transcript)))
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
              (niwa-composer--fit dock transcript t))
            (niwa-view-composer-display new))
        (niwa-view-pair--schedule (window-frame transcript))))
    new))

(defun niwa-view-pair--parent (window)
  "Return the parent of WINDOW's pair when it can be deleted with the pair.
The frame's root and main windows cannot be deleted, so a pair that fills
either has no such parent."
  (when-let* ((transcript (niwa-view-transcript-window window))
              ((niwa-view-composer-window transcript))
              (parent (window-parent transcript))
              ((window-parent parent))
              ((not (eq parent (window-main-window (window-frame parent))))))
    parent))

(defun niwa-view-pair--paired-p (window)
  "Return non-nil when WINDOW is a member of a transcript and dock pair."
  (when-let* ((transcript (niwa-view-transcript-window window)))
    (niwa-view-composer-window transcript)))

(defun niwa-view-pair--delete (window)
  "Delete WINDOW's whole pair; a pair that fills the frame is quit instead.
Its parent cannot be deleted, so the known dock is unpaired directly and
native quit-restore with `bury' shows the previous buffer, never killing."
  (let ((ignore-window-parameters t)
        (dock (niwa-view-pair--paired-p window)))
    (cond ((niwa-view-pair--parent window)
           (delete-window (niwa-view-pair--parent window)))
          (dock
           (niwa-view-pair--unpair-sole dock)
           (quit-restore-window dock 'bury))
          (t (delete-window window)))))

(defun niwa-view-pair--only (window)
  "Keep WINDOW's pair rather than a lone composer."
  (if-let* ((transcript (niwa-view-transcript-window window)))
      (delete-other-windows transcript)
    (let ((ignore-window-parameters t))
      (delete-other-windows window))))

(defun niwa-view-pair-switch-buffer ()
  "Switch buffers in the transcript instead of replacing this draft window."
  (interactive)
  (let* ((dock (selected-window))
         (transcript (or (niwa-view-transcript-window dock)
                         (niwa-diagnostic-user-error
                          (niwa-diagnostic-scope (current-buffer))
                          "No transcript for this composer"))))
    (select-window transcript)
    (unwind-protect
        (call-interactively #'switch-to-buffer)
      (when (and (eq (selected-window) transcript)
                 (eq (niwa-view-composer-window transcript) dock))
        (select-window dock)))))

(defun niwa-view-pair--make (transcript draft)
  "Dock DRAFT directly below TRANSCRIPT, or return nil if it cannot fit."
  (when-let* ((dock (let ((window-combination-limit t)
                          (ignore-window-parameters t))
                      (ignore-errors (split-window transcript -4 'below)))))
    (set-window-buffer dock draft)
    (niwa-view-pair--bind transcript dock)
    ;; Do not paint a provisional height only to resize and repaint it.
    (unless noninteractive
      (with-current-buffer draft (niwa-composer--fit dock transcript t)))
    dock))

(defun niwa-view-composer-display (view-window)
  "Display the draft for VIEW-WINDOW, without selecting either window.
Return nil for read-only views and when there is not room for a dock."
  (when-let* ((transcript (niwa-view-transcript-window view-window))
              (draft (niwa-view-pair--draft transcript)))
    (let* ((frame (window-frame transcript))
           (below (niwa-view-pair--dock-below transcript))
           (dock (cond
                  ((and below (eq (window-buffer below) draft))
                   (niwa-view-pair--bind transcript below)
                   below)
                  (t
                   ;; A surface reusing TRANSCRIPT can still have its old dock.
                   (when (and below (buffer-local-value 'niwa-composer--view
                                                        (window-buffer below)))
                     (niwa-view-pair--remove below))
                   (niwa-view-pair--make transcript draft)))))
      (niwa-view-pair--set-state frame (if dock 'paired 'small))
      dock)))

(defun niwa-view-pair--remove (dock)
  "Remove orphan DOCK without killing or editing its draft."
  (let ((ignore-window-parameters t))
    (niwa-view-pair--unbind dock)
    (if (memq dock (list (frame-root-window (window-frame dock))
                         (window-main-window (window-frame dock))))
        (let* ((buffer (window-buffer dock))
               (view (buffer-local-value 'niwa-composer--view buffer)))
          (set-window-dedicated-p dock nil)
          (set-window-buffer dock (if (buffer-live-p view) view (other-buffer buffer))))
      (delete-window dock))))

(defun niwa-view-pair-hide (view)
  "Undock the composer owned by VIEW when visiting a read-only segment."
  (dolist (window (get-buffer-window-list view nil t))
    (niwa-view-pair--unbind window))
  (dolist (dock (get-buffer-window-list
                 (buffer-local-value 'niwa-view--composer-buffer view) nil t))
    (niwa-view-pair--remove dock)))

(defun niwa-view-pair--kill-draft ()
  "Remove the current draft's docks before Emacs deletes dedicated windows."
  (when (buffer-live-p niwa-composer--view)
    (niwa-view-pair-hide niwa-composer--view)))

(defun niwa-view-pair-plan (transcripts composers)
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

(defun niwa-view-pair--repair (frame)
  "Repair FRAME using its layout now, not the layout seen by redisplay."
  (let (transcripts composers owned small)
    (dolist (window (window-list frame 'nomini))
      (when (window-parameter window 'niwa-view-pair--role)
        (push window owned))
      (if (buffer-local-value 'niwa-composer--view (window-buffer window))
          (push (cons window (window-buffer window)) composers)
        (when-let* ((draft (niwa-view-pair--draft window)))
          (push (list window draft (niwa-view-pair--dock-below window))
                transcripts))))
    (let* ((plan (niwa-view-pair-plan transcripts composers))
           (keep (plist-get plan :keep))
           (add (plist-get plan :add))
           (remove (plist-get plan :remove)))
      (dolist (window owned)
        (unless (or (assq window keep) (rassq window keep) (memq window remove))
          (niwa-view-pair--unbind window)))
      (dolist (dock remove)
        (niwa-view-pair--remove dock))
      (dolist (dock remove)
        (when (and (window-live-p dock) (niwa-view-pair--draft dock))
          (cl-pushnew dock add)))
      (dolist (pair keep)
        (niwa-view-pair--bind (car pair) (cdr pair)))
      (dolist (transcript add)
        (unless (niwa-view-composer-display transcript)
          (setq small t)))
      (niwa-view-pair--set-state frame (cond (small 'small) ((or keep add) 'paired))))))

(defun niwa-view-pair--set-state (frame state)
  "Record STATE on FRAME only if changed."
  (unless (eq (frame-parameter frame 'niwa-view-pair--state) state)
    (set-frame-parameter frame 'niwa-view-pair--state state)))

(defun niwa-view-pair--drain ()
  "Apply pending repairs outside redisplay."
  (let ((frames niwa-view-pair--frames))
    (setq niwa-view-pair--frames nil
          niwa-view-pair--timer nil)
    (dolist (frame frames)
      (when (frame-live-p frame)
        (niwa-view-pair--repair frame)))))

(defun niwa-view-pair--schedule (frame)
  "Queue one deferred repair of FRAME."
  (unless noninteractive
    (cl-pushnew frame niwa-view-pair--frames)
    (unless niwa-view-pair--timer
      (setq niwa-view-pair--timer (run-at-time 0 nil #'niwa-view-pair--drain)))))

(defun niwa-view-pair--damaged-p (window)
  "Return non-nil if WINDOW needs its dock or window rules repaired."
  (let ((role (window-parameter window 'niwa-view-pair--role)))
    (cond
     ((buffer-local-value 'niwa-composer--view (window-buffer window))
      (not (and (eq role 'composer) (niwa-view-transcript-window window))))
     ((niwa-view-pair--draft window)
      (not (and (eq role 'transcript)
                (window-dedicated-p window)
                (niwa-view-composer-window window))))
     (role))))

(defun niwa-view-pair--buffer-change (window)
  "Notice a new transcript or draft, or a stale pair rule on WINDOW."
  (when (niwa-view-pair--damaged-p window)
    (niwa-view-pair--schedule (window-frame window))))

(defun niwa-view-pair--frame-change (frame)
  "Notice changed and deleted windows on a previously paired FRAME."
  ;; Explicit runs of the buffer-local hook can also reach the default value.
  (when (and (framep frame) (frame-parameter frame 'niwa-view-pair--state))
    (let (paired)
      (when (or (catch 'damaged
                  (walk-window-tree
                   (lambda (window)
                     (when (niwa-view-pair--damaged-p window)
                       (throw 'damaged t))
                     (when (window-parameter window 'niwa-view-pair--role)
                       (setq paired t)))
                   frame nil 'nomini)
                  nil)
                (not paired))
        (niwa-view-pair--schedule frame)))))

(defun niwa-view-pair--configuration-change ()
  "Notice restored rules and retry docks previously too small to fit."
  (niwa-view-pair--frame-change (selected-frame)))

(defun niwa-view-pair--after-switch ()
  "Dock a switched-to transcript once the native display action has finished."
  (when (and (memq this-command '(switch-to-buffer niwa-view-pair-switch-buffer))
             (eq (window-buffer) (current-buffer))
             (not (niwa-view-composer-window)))
    (niwa-view-composer-display (selected-window))))

(defun niwa-view-pair--window-deletable-p (window _frame)
  "Keep native teardown from deleting a weak or undeletable pair WINDOW.
Window teardown consults this hook but `delete-window' does not, so explicit
deletion refuses in `niwa-view-pair--delete' and teardown falls back to a
buffer switch."
  (and (not (eq (window-dedicated-p window) 'niwa-view-pair))
       (or (not (niwa-view-pair--paired-p window))
           (niwa-view-pair--parent window))))

(defun niwa-view-pair--unpair-sole (dock)
  "Unpair the pair of DOCK, which fills its frame, so native quit can finish.
DOCK's window survives as the sole window with the transcript window's buffer
history and quit parameters; the view is buried and the draft kept."
  (let* ((transcript (niwa-view-transcript-window dock))
         (view (window-buffer transcript)))
    (niwa-view-pair--unbind dock)
    (niwa-view-pair--unbind transcript)
    (set-window-prev-buffers dock (window-prev-buffers transcript))
    ;; Native quit checks that a record's fourth element is the buffer it
    ;; acts on, which is now the draft rather than the view.  Set both
    ;; parameters unconditionally so the dock's own stale record cannot win.
    (dolist (parameter '(quit-restore quit-restore-prev))
      (let ((record (copy-sequence (window-parameter transcript parameter))))
        (when (eq (nth 3 record) view)
          (setf (nth 3 record) (window-buffer dock)))
        (set-window-parameter dock parameter record)))
    (let ((ignore-window-parameters t))
      (delete-window transcript))
    (bury-buffer view)))

(defun niwa-view-pair--quit-dock ()
  "Before `quit-window' acts on an undeletable pair's dock, unpair it.
Emacs runs this hook in the buffer of the window being quit, so when exactly
one window shows this draft that window is the quit target; with several the
target is unknown and native quit proceeds alone."
  (let ((docks (get-buffer-window-list nil nil t)))
    (when-let* (((null (cdr docks)))
                (dock (car docks))
                ((niwa-view-pair--paired-p dock))
                ((not (niwa-view-pair--parent dock)))
                ((eq (niwa-view-composer-window dock) dock)))
      (niwa-view-pair--unpair-sole dock))))

(defun niwa-view-pair-watch (draft)
  "Observe layout changes for the current view and its DRAFT buffer."
  (add-hook 'post-command-hook #'niwa-view-pair--after-switch nil t)
  (add-hook 'window-deletable-functions #'niwa-view-pair--window-deletable-p nil t)
  (add-hook 'window-buffer-change-functions #'niwa-view-pair--buffer-change nil t)
  (with-current-buffer draft
    (add-hook 'window-buffer-change-functions #'niwa-view-pair--buffer-change nil t)
    (add-hook 'window-deletable-functions #'niwa-view-pair--window-deletable-p nil t)
    (add-hook 'quit-window-hook #'niwa-view-pair--quit-dock nil t)
    (add-hook 'kill-buffer-hook #'niwa-view-pair--kill-draft nil t))
  (add-hook 'window-buffer-change-functions #'niwa-view-pair--frame-change)
  (add-hook 'window-configuration-change-hook #'niwa-view-pair--configuration-change))

(provide 'niwa-view-pair)
;;; niwa-view-pair.el ends here
