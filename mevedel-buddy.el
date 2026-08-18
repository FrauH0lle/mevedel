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

;; `mevedel-workspace'
(declare-function mevedel-workspace--project-workspace "mevedel-workspace" ())


;;
;;; Customization

(defgroup mevedel-buddy nil
  "Unasked model feedback on recent edits."
  :group 'mevedel)

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

(provide 'mevedel-buddy)
;;; mevedel-buddy.el ends here
