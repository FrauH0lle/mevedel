;;; mevedel-buddy.el -- Unasked model feedback on recent edits -*- lexical-binding: t -*-

;;; Commentary:

;; Buddy watches the edits you make and asks a model to comment on them.
;; This file owns the half that runs before any model does: buffer-local
;; change recording, coalescing of nearby edits, and assembly of the
;; consolidated diff that becomes the review payload.
;;
;; Changes are keyed by scope, which is the project root when the buffer
;; has one and the buffer name otherwise, so one review can span several
;; files edited together.  Only the net effect of the recorded edits
;; reaches the model: the original text is reconstructed by reverse
;; applying the records, so text typed and reverted produces no diff.
;;
;; Diff lines carry current buffer line numbers, because the model
;; answers with line numbers.  Removed lines are labelled instead of
;; numbered so they are never mistaken for a place a note can go.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(declare-function gptel-abort "ext:gptel" (buffer))
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest keys))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `mevedel-buddy-note'
(declare-function mevedel-buddy-note-capture-markers "mevedel-buddy-note" (buffer-names))
(declare-function mevedel-buddy-note-release-markers "mevedel-buddy-note" ())
(declare-function mevedel-buddy-note-serialize "mevedel-buddy-note" ())
(declare-function mevedel-buddy-note-tools "mevedel-buddy-note" ())
(defvar mevedel-buddy-note--scope-buffers)

;; `mevedel-chat'
(defvar mevedel--session)

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-system'
(declare-function mevedel-system-build-prompt "mevedel-system"
                  (profile &rest keys))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-record "mevedel-telemetry" (session event &rest props))

;; `diff'
(declare-function diff-no-select "diff"
                  (old new &optional switches no-async buf))

;; `mevedel-workspace'
(declare-function mevedel-workspace--project-workspace "mevedel-workspace" ())


;;
;;; Customization

(defgroup mevedel-buddy nil
  "Unasked model feedback on recent edits."
  :group 'mevedel)

(defcustom mevedel-buddy-severity-floor "significant"
  "Lowest severity Buddy is asked to report.

The floor reaches the model in the prompt rather than filtering its
answers afterwards, so a note below it is never written instead of
being recorded where the user cannot see or dismiss it."
  :type '(choice (const "trivial") (const "significant") (const "critical"))
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-max-iterations 8
  "How many tool rounds one review may take before it is abandoned.

A review that has not settled by then is dropped without recording its
changes as reviewed, so the same edits are offered again next time."
  :type 'natnum
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-tracked-modes '(prog-mode text-mode conf-mode)
  "Major modes Buddy watches, including modes derived from them."
  :type '(repeat symbol)
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-idle-delay 10
  "Seconds of idle time before an automatic review runs."
  :type 'number
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-min-interval 60
  "Least seconds between two automatic reviews of one scope.

Without it a long editing session would fire a request every time the
user paused to think."
  :type 'number
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-coalesce-window 180.0
  "Seconds within which a new edit may merge into the previous record.

An edit that arrives later, or that does not touch the previous
record's region, starts a record of its own."
  :type 'number
  :group 'mevedel)


;;
;;; Recorded changes

(defvar mevedel-buddy--changes (make-hash-table :test #'equal)
  "Hash table mapping scope keys to change records, most recent first.

Each record is a plist with `:time', `:last-time', `:scope-key',
`:buffer', `:mode', `:beg', `:end', `:old-text', and `:new-text'.
Positions and texts are line expanded, so a record always covers whole
lines.")

(defvar-local mevedel-buddy--pending-beg nil
  "Line expanded start of the change currently being made.")

(defvar-local mevedel-buddy--pending-old-text nil
  "Whole lines about to be replaced by the change currently being made.")

(defun mevedel-buddy--scope-key ()
  "Return the current buffer's scope key.

Buffers in a project share that project's key so edits made across
several of its files are reviewed together.  A buffer outside any
project is its own scope."
  (or (cdr (ignore-errors (mevedel-workspace--project-workspace)))
      (concat "buffer:" (buffer-name))))

(defun mevedel-buddy--changes-for-scope (scope-key)
  "Return the recorded changes for SCOPE-KEY, most recent first."
  (gethash scope-key mevedel-buddy--changes))

(defun mevedel-buddy-clear-changes ()
  "Discard every recorded change in every scope."
  (interactive)
  (clrhash mevedel-buddy--changes))

(defun mevedel-buddy--line-end (position)
  "Return the end of POSITION's line, including its newline when present."
  (save-excursion
    (goto-char position)
    (min (1+ (line-end-position)) (point-max))))

(defun mevedel-buddy--merge-region (record beg old-text new-text)
  "Return (BEG OLD NEW) for RECORD extended by a change, or nil.

BEG is where the incoming change starts, OLD-TEXT what it replaced, and
NEW-TEXT what it inserted.  A change merges when it lies inside
RECORD's current region or begins exactly where that region ends or
ends exactly where it begins; anything else belongs to a new record."
  (let* ((record-beg (plist-get record :beg))
         (record-end (plist-get record :end))
         (record-old (plist-get record :old-text))
         (record-new (plist-get record :new-text))
         (old-length (length old-text)))
    (cond
     ;; Inside the region: splice the incoming text into what we hold.
     ((and (>= beg record-beg) (<= (+ beg old-length) record-end))
      (let ((offset (- beg record-beg)))
        (list record-beg
              record-old
              (concat (substring record-new 0 offset)
                      new-text
                      (substring record-new (+ offset old-length))))))
     ;; Abutting on the right.
     ((= beg record-end)
      (list record-beg
            (concat record-old old-text)
            (concat record-new new-text)))
     ;; Abutting on the left.
     ((= (+ beg old-length) record-beg)
      (list beg
            (concat old-text record-old)
            (concat new-text record-new))))))

(defun mevedel-buddy--try-merge (scope-key beg old-text new-text)
  "Merge a change into SCOPE-KEY's newest record for this buffer.

BEG, OLD-TEXT, and NEW-TEXT describe the incoming change.  Return
non-nil when it was absorbed."
  (let* ((records (gethash scope-key mevedel-buddy--changes))
         (record (seq-find (lambda (candidate)
                             (equal (plist-get candidate :buffer)
                                    (buffer-name)))
                           records))
         (now (current-time)))
    (when (and record
               (<= (float-time
                    (time-subtract now (plist-get record :last-time)))
                   mevedel-buddy-coalesce-window))
      (when-let* ((merged (mevedel-buddy--merge-region
                           record beg old-text new-text)))
        (cl-destructuring-bind (new-beg new-old new-new) merged
          (plist-put record :last-time now)
          (plist-put record :beg new-beg)
          (plist-put record :end (+ new-beg (length new-new)))
          (plist-put record :old-text new-old)
          (plist-put record :new-text new-new))
        t))))

(defun mevedel-buddy--before-change (beg end)
  "Remember the whole lines between BEG and END before they change."
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
        (line-end (mevedel-buddy--line-end end)))
    (setq mevedel-buddy--pending-beg line-beg
          mevedel-buddy--pending-old-text
          (buffer-substring-no-properties line-beg line-end))))

(defun mevedel-buddy--after-change (beg end _length)
  "Record the change that replaced the lines around BEG and END."
  (let* ((line-beg (or mevedel-buddy--pending-beg
                       (save-excursion (goto-char beg)
                                       (line-beginning-position))))
         (line-end (mevedel-buddy--line-end end))
         (new-text (buffer-substring-no-properties line-beg line-end))
         (old-text (or mevedel-buddy--pending-old-text ""))
         (scope-key (mevedel-buddy--scope-key)))
    (setq mevedel-buddy--pending-beg nil
          mevedel-buddy--pending-old-text nil)
    (unless (mevedel-buddy--try-merge scope-key line-beg old-text new-text)
      (let ((now (current-time)))
        (puthash scope-key
                 (cons (list :time now
                             :last-time now
                             :scope-key scope-key
                             :buffer (buffer-name)
                             :mode major-mode
                             :beg line-beg
                             :end line-end
                             :old-text old-text
                             :new-text new-text)
                       (gethash scope-key mevedel-buddy--changes))
                 mevedel-buddy--changes)))))

(defun mevedel-buddy--track-buffer ()
  "Start recording changes in the current buffer."
  (add-hook 'before-change-functions #'mevedel-buddy--before-change nil t)
  (add-hook 'after-change-functions #'mevedel-buddy--after-change nil t))

(defun mevedel-buddy--untrack-buffer ()
  "Stop recording changes in the current buffer."
  (remove-hook 'before-change-functions #'mevedel-buddy--before-change t)
  (remove-hook 'after-change-functions #'mevedel-buddy--after-change t))


;;
;;; Diff assembly

(defun mevedel-buddy--live-change-p (change)
  "Return non-nil when CHANGE still belongs to a live buffer."
  (buffer-live-p (get-buffer (plist-get change :buffer))))

(defun mevedel-buddy--reconstruct-original (records)
  "Return the text RECORDS were applied to, or nil.

RECORDS must be oldest first and all belong to one live buffer.  The
records are undone newest first, so the positions older records hold
are still valid when their turn comes."
  (when-let* ((buffer (get-buffer (plist-get (car records) :buffer)))
              ((buffer-live-p buffer)))
    (with-temp-buffer
      (insert (with-current-buffer buffer
                (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (record (reverse records))
        (let ((beg (plist-get record :beg))
              (new-text (plist-get record :new-text))
              (old-text (plist-get record :old-text)))
          (goto-char (min beg (point-max)))
          (delete-char (min (length new-text) (- (point-max) (point))))
          (insert old-text)))
      (buffer-string))))

(defun mevedel-buddy--unified-diff (original current)
  "Return the unified diff hunks between ORIGINAL and CURRENT, or nil."
  (require 'diff)
  (let ((original-buffer (generate-new-buffer " *mevedel-buddy-original*"))
        (current-buffer (generate-new-buffer " *mevedel-buddy-current*"))
        (output-buffer (generate-new-buffer " *mevedel-buddy-diff*")))
    (unwind-protect
        (progn
          (with-current-buffer original-buffer (insert original))
          (with-current-buffer current-buffer (insert current))
          (diff-no-select original-buffer current-buffer "-u" t output-buffer)
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (when (re-search-forward "^@@" nil t)
              (beginning-of-line)
              ;; `diff-no-select' appends its own completion notice; the
              ;; hunks end where that notice begins.
              (let ((start (point))
                    (end (if (re-search-forward "^Diff finished" nil t)
                             (match-beginning 0)
                           (point-max))))
                (string-trim-right
                 (buffer-substring-no-properties start end))))))
      (kill-buffer original-buffer)
      (kill-buffer current-buffer)
      (kill-buffer output-buffer))))

(defun mevedel-buddy--number-diff-lines (diff)
  "Return DIFF with current buffer line numbers on its live lines.

Context and added lines are numbered so the model can name them.
Removed lines are labelled as old text instead, because they are not
in the buffer any more and cannot carry a note."
  (let ((line-number nil)
        (numbered nil))
    (dolist (line (split-string diff "\n"))
      (cond
       ((string-match
         "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
         line)
        (setq line-number (string-to-number (match-string 1 line)))
        (push line numbered))
       ((and line-number (string-prefix-p "+" line))
        (push (format "%6d %s" line-number line) numbered)
        (setq line-number (1+ line-number)))
       ((and line-number (string-prefix-p "-" line))
        (push (format "   old %s" line) numbered))
       ((and line-number (string-prefix-p " " line))
        (push (format "%6d %s" line-number line) numbered)
        (setq line-number (1+ line-number)))
       (t (push line numbered))))
    (string-join (nreverse numbered) "\n")))

(defun mevedel-buddy--buffer-section (buffer-name records)
  "Return the review section for BUFFER-NAME's RECORDS, oldest first, or nil.

The section is a header naming the buffer, its mode, its scope, and the
cursor line, followed by the numbered diff of everything the records
changed.  Edits that cancel out yield no section at all."
  (when-let* ((buffer (get-buffer buffer-name))
              ((buffer-live-p buffer))
              (original (mevedel-buddy--reconstruct-original records))
              (current (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
              ((not (string= original current)))
              (diff (mevedel-buddy--unified-diff original current)))
    (concat (format "=== Buffer: %s  Mode: %s  Scope: %s  Cursor: line %d ===\n"
                    buffer-name
                    (plist-get (car records) :mode)
                    (plist-get (car records) :scope-key)
                    (with-current-buffer buffer
                      (line-number-at-pos (point))))
            (mevedel-buddy--number-diff-lines diff))))

(defun mevedel-buddy--format-changes (changes)
  "Return the review payload for CHANGES, or an empty string.

CHANGES are the recorded changes of one scope, most recent first.  They
are grouped per buffer and each group becomes one section."
  (let ((groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (change (reverse (seq-filter #'mevedel-buddy--live-change-p
                                         changes)))
      (let ((buffer-name (plist-get change :buffer)))
        (unless (gethash buffer-name groups)
          (push buffer-name order))
        (puthash buffer-name
                 (append (gethash buffer-name groups) (list change))
                 groups)))
    (string-join
     (delq nil
           (mapcar (lambda (buffer-name)
                     (mevedel-buddy--buffer-section
                      buffer-name (gethash buffer-name groups)))
                   (nreverse order)))
     "\n")))


;;
;;; Review requests

(defvar mevedel-buddy--running nil
  "Scope key of the review currently in flight, or nil.")

(defvar mevedel-buddy--running-automatic nil
  "Non-nil when the review in flight was started by the idle timer.

An explicitly requested review may preempt one of those; it may not
preempt another explicit request.")

(defvar mevedel-buddy--reviewed-through (make-hash-table :test #'equal)
  "Hash table mapping scope keys to the time their changes were reviewed.")

(defun mevedel-buddy--session ()
  "Return a live session to attribute telemetry to, or nil.

Buddy runs from a source buffer and usually has no session at all.  One
is used when it happens to exist and skipped otherwise."
  (ignore-errors
    (require 'mevedel-telemetry)
    (mevedel-telemetry-current-session)))

(defun mevedel-buddy--telemetry (event &rest props)
  "Record EVENT with PROPS when a session is live to receive it."
  (when-let* ((session (mevedel-buddy--session)))
    (ignore-errors (apply #'mevedel-telemetry-record session event props))))

(defun mevedel-buddy--changed-buffers (changes)
  "Return the live buffer names named by CHANGES."
  (seq-uniq
   (delq nil
         (mapcar (lambda (change)
                   (let ((name (plist-get change :buffer)))
                     (and (buffer-live-p (get-buffer name)) name)))
                 changes))))

(defun mevedel-buddy--settle (scope-key reviewed-through)
  "Finish the review of SCOPE-KEY.

REVIEWED-THROUGH is recorded only when the review settled on its own;
an abandoned run passes nil so its changes are offered again."
  (require 'mevedel-buddy-note)
  (when reviewed-through
    (puthash scope-key reviewed-through mevedel-buddy--reviewed-through))
  (mevedel-buddy-note-release-markers)
  (setq mevedel-buddy-note--scope-buffers nil
        mevedel-buddy--running nil
        mevedel-buddy--running-automatic nil))

(defun mevedel-buddy--preempt ()
  "Abandon an automatic review in flight so an explicit one can run."
  (when mevedel-buddy--running
    (ignore-errors (gptel-abort (current-buffer)))
    (mevedel-buddy--telemetry 'buddy-preempted :scope mevedel-buddy--running)
    (mevedel-buddy--settle mevedel-buddy--running nil)))

(defun mevedel-buddy--request (scope-key profile payload buffer-names
                                         reviewed-through automatic)
  "Send PAYLOAD for SCOPE-KEY using PROFILE and settle the result.

BUFFER-NAMES bounds which buffers the review may touch.  REVIEWED-THROUGH
is recorded when the review settles on its own.  AUTOMATIC marks a run
started by the idle timer, which an explicit request may preempt."
  (require 'gptel)
  (require 'mevedel-buddy-note)
  (require 'mevedel-models)
  (require 'mevedel-system)
  (let ((policy (mevedel-model-resolve-workload 'buddy))
        (rounds 0)
        (settled nil)
        (source (current-buffer)))
    (unless (plist-get policy :model)
      (user-error "No model resolves for the buddy workload"))
    (setq mevedel-buddy--running scope-key
          mevedel-buddy--running-automatic automatic
          mevedel-buddy-note--scope-buffers buffer-names)
    (mevedel-buddy-note-capture-markers buffer-names)
    (mevedel-buddy--telemetry 'buddy-review-started
                              :scope scope-key :profile profile
                              :buffers (length buffer-names))
    (cl-labels
        ((finish (ok)
           (unless settled
             (setq settled t)
             (mevedel-buddy--telemetry 'buddy-review-settled
                                       :scope scope-key :settled ok
                                       :rounds rounds)
             (mevedel-buddy--settle scope-key (and ok reviewed-through)))))
      (condition-case nil
          (let ((gptel-backend (plist-get policy :backend))
                (gptel-model (plist-get policy :model))
                (gptel-reasoning-effort (plist-get policy :effort))
                (gptel-use-context nil)
                (gptel-use-tools t)
                (gptel-tools (mevedel-buddy-note-tools)))
            (gptel-request
             (concat payload (mevedel-buddy-note-serialize))
             :buffer source
             :stream nil
             :transforms nil
             :system (mevedel-system-build-prompt
                      profile
                      :workspace (mevedel-buddy--workspace)
                      :working-directory default-directory)
             :callback
             (lambda (response _info)
               (cond
                ;; Reasoning and streamed fragments are not our business.
                ((and (consp response) (eq (car response) 'reasoning)))
                ((and (consp response) (eq (car response) 'tool-call)))
                ((and (consp response) (eq (car response) 'tool-result))
                 (setq rounds (1+ rounds))
                 (when (> rounds mevedel-buddy-max-iterations)
                   (ignore-errors (gptel-abort source))
                   (finish nil)))
                ;; A turn that calls no tool has nothing left to say.
                ((or (stringp response) (eq response t)) (finish t))
                (t (finish nil))))))
        (error (finish nil))))))

(defun mevedel-buddy--workspace ()
  "Return the current buffer's workspace, or nil when it has none."
  (ignore-errors
    (require 'mevedel-workspace)
    (mevedel-workspace)))

(defun mevedel-buddy--severity-instruction ()
  "Return the sentence stating the configured severity floor."
  (format "\n\nReport nothing below %s severity.\n"
          mevedel-buddy-severity-floor))

(defun mevedel-buddy-review (&optional automatic)
  "Review the edits recorded for the current buffer's scope.

AUTOMATIC marks a run started by the idle timer rather than by the
user.  Returns non-nil when a request was sent."
  (interactive)
  (require 'mevedel-buddy-note)
  (let* ((scope-key (mevedel-buddy--scope-key))
         (changes (seq-filter #'mevedel-buddy--live-change-p
                              (mevedel-buddy--changes-for-scope scope-key)))
         (payload (mevedel-buddy--format-changes changes))
         (reviewed-through (current-time)))
    (cond
     ((null changes) nil)
     (mevedel-buddy--running nil)
     ((string-empty-p payload)
      ;; Everything recorded cancelled out; nothing to review.
      (puthash scope-key reviewed-through mevedel-buddy--reviewed-through)
      nil)
     (t
      (mevedel-buddy--request
       scope-key 'buddy
       (concat payload (mevedel-buddy--severity-instruction))
       (mevedel-buddy--changed-buffers changes)
       reviewed-through automatic)
      t))))


;;
;;; Tracking policy

(defun mevedel-buddy--own-buffer-p (buffer)
  "Return non-nil when BUFFER is one of mevedel's own surfaces.

Annotating a transcript, cockpit, or inspector is never wanted, and
leaving that to the tracked-modes default would be an accident waiting
to happen."
  (with-current-buffer buffer
    (or (bound-and-true-p mevedel--session)
        (bound-and-true-p mevedel--view-buffer)
        (bound-and-true-p mevedel--data-buffer)
        (string-prefix-p "mevedel-" (symbol-name major-mode))
        (string-prefix-p "*mevedel" (buffer-name)))))

(defun mevedel-buddy--tracked-buffer-p (&optional buffer)
  "Return non-nil when BUFFER should have its edits watched."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         ;; Buffers whose name starts with a space are internal.
         (not (string-prefix-p " " (buffer-name buffer)))
         (not (mevedel-buddy--own-buffer-p buffer))
         (with-current-buffer buffer
           (apply #'derived-mode-p mevedel-buddy-tracked-modes)))))


;;
;;; Timers

(defvar-local mevedel-buddy--idle-timer nil
  "Idle timer that will review this buffer's scope, or nil.")

(defvar mevedel-buddy--last-review (make-hash-table :test #'equal)
  "Hash table mapping scope keys to when they were last reviewed.")

(defun mevedel-buddy--due-p (scope-key)
  "Return non-nil when SCOPE-KEY may be reviewed automatically again."
  (let ((last (gethash scope-key mevedel-buddy--last-review)))
    (or (null last)
        (>= (float-time (time-subtract (current-time) last))
            mevedel-buddy-min-interval))))

(defun mevedel-buddy--cancel-timer ()
  "Cancel this buffer's pending automatic review."
  (when mevedel-buddy--idle-timer
    (cancel-timer mevedel-buddy--idle-timer)
    (setq mevedel-buddy--idle-timer nil)))

(defun mevedel-buddy--run-scheduled (buffer)
  "Review BUFFER's scope, now that the user has stopped typing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq mevedel-buddy--idle-timer nil)
      (let ((scope-key (mevedel-buddy--scope-key)))
        (cond
         ;; Something is already talking to a model; try again later.
         (mevedel-buddy--running (mevedel-buddy--schedule))
         ((not (mevedel-buddy--due-p scope-key)) nil)
         (t
          (puthash scope-key (current-time) mevedel-buddy--last-review)
          (mevedel-buddy-review t)))))))

(defun mevedel-buddy--schedule ()
  "Arrange for this buffer's scope to be reviewed once the user pauses."
  (mevedel-buddy--cancel-timer)
  (setq mevedel-buddy--idle-timer
        (run-with-idle-timer mevedel-buddy-idle-delay nil
                             #'mevedel-buddy--run-scheduled
                             (current-buffer))))

(defun mevedel-buddy--after-change-schedule (_beg _end _length)
  "Schedule a review after an edit."
  (mevedel-buddy--schedule))


;;
;;; Modes

;;;###autoload
(define-minor-mode mevedel-buddy-mode
  "Let a model comment on the edits you make in this buffer.

Notes appear beside the lines they concern.  Nothing is written to
disk: notes and the edits behind them live only as long as Emacs does."
  :lighter " Buddy"
  :group 'mevedel-buddy
  (if mevedel-buddy-mode
      (progn
        (mevedel-buddy--track-buffer)
        (add-hook 'after-change-functions
                  #'mevedel-buddy--after-change-schedule nil t))
    (mevedel-buddy--cancel-timer)
    (mevedel-buddy--untrack-buffer)
    (remove-hook 'after-change-functions
                 #'mevedel-buddy--after-change-schedule t)))

(defun mevedel-buddy--maybe-enable ()
  "Enable `mevedel-buddy-mode' when this buffer qualifies."
  (when (mevedel-buddy--tracked-buffer-p)
    (mevedel-buddy-mode 1)))

;;;###autoload
(define-globalized-minor-mode mevedel-buddy-global-mode
  mevedel-buddy-mode mevedel-buddy--maybe-enable
  :group 'mevedel-buddy)


;;
;;; Guidance

(defun mevedel-buddy--guide-payload (beg end)
  "Return the text between BEG and END described for guidance.

Lines are numbered from their real position so `add_note' arguments
line up with the buffer even when only part of it was sent."
  (let* ((first-line (line-number-at-pos beg))
         (text (buffer-substring-no-properties beg end))
         (line first-line)
         numbered)
    (dolist (content (split-string text "\n"))
      (push (format "%6d  %s" line content) numbered)
      (setq line (1+ line)))
    (concat (format "=== Buffer: %s  Mode: %s  Scope: %s  Cursor: line %d ===\n"
                    (buffer-name)
                    major-mode
                    (mevedel-buddy--scope-key)
                    (line-number-at-pos (point)))
            (string-join (nreverse numbered) "\n"))))

;;;###autoload
(defun mevedel-buddy-guide ()
  "Ask a model what to build here, rather than what went wrong.

Sends the active region, or the whole buffer, and annotates it with
suggestions, missing pieces, and the decisions still open.  Works in
any buffer; `mevedel-buddy-mode' need not be enabled.

An automatic review already in flight is abandoned first: the request
you made outranks the one a timer made."
  (interactive)
  (mevedel-buddy--preempt)
  (when mevedel-buddy--running
    (user-error "A buddy request is already running"))
  (mevedel-buddy--request
   (mevedel-buddy--scope-key) 'buddy-guide
   (mevedel-buddy--guide-payload
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max)))
   (list (buffer-name))
   nil nil))

(provide 'mevedel-buddy)
;;; mevedel-buddy.el ends here
