;;; niwa-buddy.el -- Unasked model feedback on recent edits -*- lexical-binding: t -*-

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
;;
;; A scope may also hold a brief, set by `/buddy' from a session: what
;; the programmer is building.  It is prefixed to every request in the
;; scope and, like everything else here, lives only in memory.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib))

(defvar niwa-buddy--idle-timer)

;; `gptel'
(declare-function gptel-abort "ext:gptel" (buffer))
(declare-function gptel-make-fsm "ext:gptel-request" (&rest slots))
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest keys))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-request--handlers)
(defvar gptel-stream)
(defvar gptel-system-prompt)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `niwa-auth'
(declare-function niwa-auth-assert-ready "niwa-auth" (backend))
(autoload 'niwa-auth-assert-ready "niwa-auth")

;; `niwa-buddy-note'
(declare-function niwa-buddy-note-begin-review "niwa-buddy-note"
                  (buffer-names payload level context changes))
(declare-function niwa-buddy-note-end-review "niwa-buddy-note" ())
(declare-function niwa-buddy-note-serialize "niwa-buddy-note" ())
(declare-function niwa-buddy-note-tools "niwa-buddy-note" (currentp))

;; `niwa-chat'
(defvar niwa--session)

;; `niwa-claude-code'
(declare-function niwa-claude-code-request-workload "niwa-claude-code"
                  (prompt system tools callback before-tool boundary))

;; `niwa-engine'
(declare-function niwa-engine-external-p "niwa-engine" (backend))

;; `niwa-models'
(declare-function niwa-model-resolve-workload "niwa-models"
                  (workload &optional explicit-selector explicit-effort))

;; `niwa-notes'
(declare-function niwa-notes--ensure-workspace "niwa-notes" (workspace))
(declare-function niwa-notes-resume-taken "niwa-notes" (workspace))
(declare-function niwa-note-watching-p "niwa-notes" (note &optional workspace))
(autoload 'niwa-note-watching-p "niwa-notes")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-read-file "niwa-session-control-fs"
                  (path &optional coding-system max-bytes))
(declare-function niwa-session-control-fs-write-file "niwa-session-control-fs"
                  (path content &optional coding-system))

;; `niwa-system'
(declare-function niwa-system-build-prompt "niwa-system"
                  (profile &rest keys))

;; `niwa-telemetry'
(declare-function niwa-telemetry-current-session "niwa-telemetry" (&optional buffer))
(declare-function niwa-telemetry-record "niwa-telemetry" (session event &rest props))
(autoload 'niwa-telemetry-current-session "niwa-telemetry")
(autoload 'niwa-telemetry-record "niwa-telemetry")

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))

;; `niwa-utilities'
(declare-function niwa--unified-diff "niwa-utilities" (original current &optional context-lines))

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(declare-function niwa-workspace--project-workspace "niwa-workspace" ())
(autoload 'niwa-workspace "niwa-workspace")
(autoload 'niwa-workspace--project-workspace "niwa-workspace")


;;
;;; Customization

(defgroup niwa-buddy nil
  "Unasked model feedback on recent edits."
  :group 'niwa)

(defcustom niwa-buddy-severity-floor "significant"
  "Lowest severity Buddy is asked to report.

The floor reaches the model in the prompt rather than filtering its
answers afterwards, so a note below it is never written instead of
being recorded where the user cannot see or dismiss it."
  :type '(choice (const "trivial") (const "significant") (const "critical"))
  :group 'niwa-buddy)

(defcustom niwa-buddy-timeout 120
  "Seconds after which a review that has not settled is abandoned.

A request whose callback never arrives would otherwise hold the
one-review-at-a-time slot and its captured markers for the rest of the
session."
  :type 'number
  :group 'niwa-buddy)

(defcustom niwa-buddy-max-iterations 8
  "How many tool rounds one review may take before it is abandoned.

A review that has not settled by then is dropped without recording its
changes as reviewed, so the same edits are offered again next time."
  :type 'natnum
  :group 'niwa-buddy)

(defcustom niwa-buddy-tracked-modes '(prog-mode text-mode conf-mode)
  "Major modes Buddy watches, including modes derived from them."
  :type '(repeat symbol)
  :group 'niwa-buddy)

(defcustom niwa-buddy-idle-delay 10
  "Seconds of idle time before an automatic review runs."
  :type 'number
  :group 'niwa-buddy)

(defcustom niwa-buddy-min-interval 60
  "Least seconds between two automatic reviews of one scope.

Without it a long editing session would fire a request every time the
user paused to think.  Edits that settle sooner are not dropped: their
review waits until the interval has passed."
  :type 'number
  :group 'niwa-buddy)

(defcustom niwa-buddy-coalesce-window 180.0
  "Seconds within which a new edit may merge into the previous record.

An edit that arrives later, or that does not touch the previous
record's region, starts a record of its own."
  :type 'number
  :group 'niwa)


;;
;;; Workspace dial

(defvar niwa-buddy--levels (make-hash-table :test #'equal)
  "Workspace roots mapped to (LEVEL . WORKSPACE) on Buddy activation.")

(defun niwa-buddy-level (&optional workspace)
  "Return WORKSPACE's cached dial level, defaulting to `active'.
Never read a file or discover a workspace; safe during change recording."
  (let ((workspace (or workspace (bound-and-true-p niwa--workspace))))
    (if workspace
        (or (car (gethash (file-name-as-directory (niwa-workspace-root workspace))
                         niwa-buddy--levels)) 'active)
      'active)))

(defun niwa-buddy--load-level (workspace)
  "Load WORKSPACE's dial once at its first review, defaulting to Active."
  (when workspace
    (let ((root (file-name-as-directory (niwa-workspace-root workspace))))
      (unless (gethash root niwa-buddy--levels)
        (require 'niwa-session-control-fs)
        (let ((level
               (condition-case err
                   (let* ((text (string-trim
                                 (niwa-session-control-fs-read-file
                                  (file-name-concat root ".niwa" "buddy-level") nil 16)))
                          (level (cdr (assoc text '(("quiet" . quiet) ("watching" . watching)
                                                   ("active" . active))))))
                     (or level (error "Invalid Buddy dial in %s" root)))
                 (niwa-session-control-fs-absent 'active)
                 (error
                  (niwa-diagnostic-warning
                   (niwa-diagnostic-scope workspace) 'niwa
                   (format "Could not read Buddy dial; using Active: %s"
                           (error-message-string err)) :warning)
                  'active))))
          (puthash root (cons level workspace) niwa-buddy--levels))))))

(defun niwa-buddy--enable-workspace-buffer ()
  "Enable tracking only inside a workspace whose Buddy dial is loaded.
Use exact workspace ownership; never probe unknown remote files."
  (when (and buffer-file-name (niwa-buddy--tracked-buffer-p))
    (when-let* ((workspace (or (bound-and-true-p niwa--workspace)
                              (unless (file-remote-p buffer-file-name nil 'never)
                                (niwa-buddy--workspace))))
                ((gethash (file-name-as-directory (niwa-workspace-root workspace))
                          niwa-buddy--levels)))
      (unless niwa-buddy-mode (niwa-buddy-mode 1)))))

;;;###autoload
(defun niwa-buddy-set-level (level &optional workspace)
  "Set WORKSPACE's Buddy dial to LEVEL and enable its source buffers.
LEVEL is `quiet', `watching' or `active'.  Taken notes are watched at every
level.  The setting is saved in the workspace, not in any conversation."
  (interactive (list (intern (completing-read
                             "Buddy: " '("Quiet" "Watching" "Active") nil t nil nil
                             (capitalize (symbol-name
                                          (niwa-buddy-level (niwa-buddy--workspace))))))))
  (setq level (if (symbolp level) (intern (downcase (symbol-name level))) level)
        workspace (or workspace (niwa-buddy--workspace)))
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
    (unless (memq level '(quiet watching active)) (user-error "Unknown Buddy level: %s" level))
    (unless workspace (user-error "No workspace for Buddy"))
    (require 'niwa-session-control-fs)
    (when buffer-file-name
      (require 'niwa-notes)
      (setq workspace (niwa-notes--ensure-workspace workspace)))
    (niwa-session-control-fs-write-file
     (file-name-concat (niwa-workspace-state-dir workspace) "buddy-level")
     (symbol-name level))
    (puthash (file-name-as-directory (niwa-workspace-root workspace))
             (cons level workspace) niwa-buddy--levels)
    (require 'niwa-notes)
    (niwa-notes-resume-taken workspace)
    (add-hook 'find-file-hook #'niwa-buddy--enable-workspace-buffer t)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (niwa-buddy--enable-workspace-buffer)))
    (message "niwa: Buddy %s" (capitalize (symbol-name level))))
  level)


;;
;;; Recorded changes

(defvar niwa-buddy--changes (make-hash-table :test #'equal)
  "Hash table mapping scope keys to change records, most recent first.

Each record is a plist with `:time', `:last-time', `:scope-key',
`:buffer', `:mode', `:beg', `:end', `:old-text', and `:new-text'.
Positions and texts are line expanded, so a record always covers whole
lines.")

(defvar-local niwa-buddy--pending-beg nil
  "Line expanded start of the change currently being made.")

(defvar-local niwa-buddy--pending-old-text nil
  "Whole lines about to be replaced by the change currently being made.")

(defvar-local niwa-buddy--scope-cache nil
  "This buffer's cached (IDENTITY . SCOPE-KEY).

`project-current' caches its answer only when it finds a project, so a
buffer outside one would re-walk the directory tree to the filesystem
root on every keystroke.  IDENTITY contains the buffer name, visited
file, `default-directory' and pinned workspace root used to derive the key.")

(put 'niwa-buddy--scope-cache 'permanent-local t)

(defun niwa-buddy-directory-scope-key (directory)
  "Return the project scope key for DIRECTORY, or nil outside a project.
Source buffers and sessions in one project derive the same key here."
  (let ((default-directory (file-name-as-directory directory)))
    (when-let* ((root (cdr (ignore-errors (niwa-workspace--project-workspace)))))
      (file-name-as-directory (expand-file-name root)))))

(defun niwa-buddy--scope-key ()
  "Return the current buffer's scope key.

Buffers in a project share that project's key so edits made across
several of its files are reviewed together.  A buffer outside any
project is its own scope.  An already pinned workspace takes precedence."
  (let* ((root (when (bound-and-true-p niwa--workspace)
                 (file-name-as-directory (niwa-workspace-root niwa--workspace))))
         (identity (list (buffer-name) buffer-file-name default-directory root)))
    (if (equal identity (car-safe niwa-buddy--scope-cache))
        (cdr niwa-buddy--scope-cache)
      (when niwa-buddy--scope-cache
        (let ((reschedule
               (and (timerp niwa-buddy--idle-timer)
                    (memq #'niwa-buddy--after-change
                          after-change-functions))))
          (niwa-buddy-forget-buffer
           (car (car niwa-buddy--scope-cache)))
          (niwa-buddy--cancel-timer)
          (when reschedule
            (niwa-buddy--schedule))))
      (let ((scope-key
             (or root (niwa-buddy-directory-scope-key default-directory)
                 (concat "buffer:" (buffer-name)))))
        (setq niwa-buddy--scope-cache (cons identity scope-key))
        scope-key))))

(put 'niwa-buddy--scope-key 'permanent-local-hook t)

(defun niwa-buddy--changes-for-scope (scope-key)
  "Return the recorded changes for SCOPE-KEY, most recent first."
  (gethash scope-key niwa-buddy--changes))

(defun niwa-buddy-clear-changes ()
  "Discard every recorded change in every scope."
  (interactive)
  (clrhash niwa-buddy--changes))

(defun niwa-buddy--line-end (position)
  "Return the end of POSITION's line, including its newline when present."
  (save-excursion
    (goto-char position)
    (min (1+ (line-end-position)) (point-max))))

(defun niwa-buddy--merge-region (record beg old-text new-text)
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

(defun niwa-buddy--try-merge (scope-key beg old-text new-text)
  "Merge a change into SCOPE-KEY's newest record for this buffer.

BEG, OLD-TEXT, and NEW-TEXT describe the incoming change.  Return
non-nil when it was absorbed."
  (let* ((records (gethash scope-key niwa-buddy--changes))
         (record (seq-find (lambda (candidate)
                             (equal (plist-get candidate :buffer)
                                    (buffer-name)))
                           records))
         (now (current-time)))
    (when (and record
               (<= (float-time
                    (time-subtract now (plist-get record :last-time)))
                   niwa-buddy-coalesce-window))
      (when-let* ((merged (niwa-buddy--merge-region
                           record beg old-text new-text)))
        (cl-destructuring-bind (new-beg new-old new-new) merged
          (plist-put record :last-time now)
          (plist-put record :beg new-beg)
          (plist-put record :end (+ new-beg (length new-new)))
          (plist-put record :old-text new-old)
          (plist-put record :new-text new-new))
        t))))

(defun niwa-buddy--before-change (beg end)
  "Remember the whole lines between BEG and END before they change."
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
        (line-end (niwa-buddy--line-end end)))
    (setq niwa-buddy--pending-beg line-beg
          niwa-buddy--pending-old-text
          (buffer-substring-no-properties line-beg line-end))))

(defun niwa-buddy--after-change (beg end _length)
  "Record the change that replaced the lines around BEG and END.

A change whose `before-change-functions' notification was consumed
elsewhere is dropped rather than recorded with an empty replaced text.
Emacs does not guarantee the two notifications pair one to one, and a
record claiming it replaced nothing would make
`niwa-buddy--reconstruct-original' delete real text and show the
model a diff of edits the user never made.  Losing a record only costs
one round of feedback; fabricating one costs trust in every note."
  (let* ((pending-beg niwa-buddy--pending-beg)
         (old-text niwa-buddy--pending-old-text)
         (line-beg (or pending-beg
                       (save-excursion (goto-char beg)
                                       (line-beginning-position))))
         (line-end (niwa-buddy--line-end end))
         (new-text (buffer-substring-no-properties line-beg line-end))
         (scope-key (niwa-buddy--scope-key)))
    (setq niwa-buddy--pending-beg nil
          niwa-buddy--pending-old-text nil)
    (when (and old-text
               (not (niwa-buddy--try-merge
                     scope-key line-beg old-text new-text)))
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
                       (gethash scope-key niwa-buddy--changes))
                 niwa-buddy--changes)))))

(defun niwa-buddy-forget-buffer (buffer-name)
  "Discard every recorded change belonging to BUFFER-NAME.

A record outlives its buffer only to describe edits that no longer
exist.  Worse, a buffer reopened under the same name would have those
stale offsets replayed against fresh content, so the model would be
shown a diff nobody made."
  (maphash
   (lambda (scope-key records)
     (puthash scope-key
              (seq-remove (lambda (record)
                            (equal (plist-get record :buffer) buffer-name))
                          records)
              niwa-buddy--changes))
   niwa-buddy--changes))

(defun niwa-buddy--on-kill-buffer ()
  "Forget the current buffer's recorded changes as it is killed."
  (when niwa-buddy--scope-cache
    (niwa-buddy-forget-buffer (car (car niwa-buddy--scope-cache))))
  (niwa-buddy-forget-buffer (buffer-name)))

(defun niwa-buddy--track-buffer ()
  "Start recording changes in the current buffer."
  (add-hook 'before-change-functions #'niwa-buddy--before-change nil t)
  (add-hook 'after-change-functions #'niwa-buddy--after-change nil t)
  (add-hook 'after-set-visited-file-name-hook #'niwa-buddy--scope-key nil t)
  (add-hook 'kill-buffer-hook #'niwa-buddy--on-kill-buffer nil t))

(defun niwa-buddy--untrack-buffer ()
  "Stop recording changes in the current buffer.

Discards this buffer's records too.  Once the kill hook is gone nothing
would drop them later, and a buffer that reuses the name -- reopened, or
uniquified after a second file of the same name is visited -- would have
those offsets replayed against unrelated content."
  (remove-hook 'before-change-functions #'niwa-buddy--before-change t)
  (remove-hook 'after-change-functions #'niwa-buddy--after-change t)
  (remove-hook 'after-set-visited-file-name-hook #'niwa-buddy--scope-key t)
  (remove-hook 'kill-buffer-hook #'niwa-buddy--on-kill-buffer t)
  (niwa-buddy--on-kill-buffer)
  (setq niwa-buddy--scope-cache nil))


;;
;;; Diff assembly

(defun niwa-buddy--live-change-p (change)
  "Return non-nil when CHANGE still belongs to a live buffer."
  (buffer-live-p (get-buffer (plist-get change :buffer))))

(defun niwa-buddy--reconstruct-original (records)
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

(defun niwa-buddy--number-diff-lines (diff)
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

(defun niwa-buddy--buffer-section (buffer-name records)
  "Return the review section for BUFFER-NAME's RECORDS, oldest first, or nil.

The section is a header naming the buffer, its mode, its scope, and the
cursor line, followed by the numbered diff of everything the records
changed.  Edits that cancel out yield no section at all."
  (when-let* ((buffer (get-buffer buffer-name))
              ((buffer-live-p buffer))
              (original (niwa-buddy--reconstruct-original records))
              (current (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
              ((not (string= original current)))
              (diff
               ;; Six lines of context rather than diff's default three: a
               ;; review covers the region around a change, so that region
               ;; has to reach the model, not just the changed lines.
               (niwa--unified-diff original current 6)))
    (concat (format "=== Buffer: %s  Mode: %s  Scope: %s  Cursor: line %d ===\n"
                    buffer-name
                    (plist-get (car records) :mode)
                    (plist-get (car records) :scope-key)
                    (with-current-buffer buffer
                      (line-number-at-pos (point))))
            (niwa-buddy--number-diff-lines diff))))

(defun niwa-buddy-payload-lines (payload)
  "Return an alist of (BUFFER-NAME . LINE-NUMBERS) shown in PAYLOAD.

Only lines the model was actually shown can carry a note, so only those
need a marker.  Capturing one marker per buffer line instead would put
thousands of markers on the per-buffer marker list Emacs walks on every
insertion, which the user feels as lag while the request is in flight."
  (let ((buffer-name nil)
        (result nil))
    (dolist (line (split-string payload "\n"))
      (cond
       ((string-match "^=== Buffer: \\(.*?\\)  Mode: " line)
        (setq buffer-name (match-string 1 line))
        (unless (assoc buffer-name result)
          (push (cons buffer-name nil) result)))
       ((and buffer-name (string-match "^ *\\([0-9]+\\) " line))
        (let ((entry (assoc buffer-name result)))
          (push (string-to-number (match-string 1 line)) (cdr entry))))))
    (mapcar (lambda (entry) (cons (car entry) (delete-dups (nreverse (cdr entry)))))
            (nreverse result))))

(defun niwa-buddy--format-changes (changes)
  "Return the review payload for CHANGES, or an empty string.

CHANGES are the recorded changes of one scope, most recent first.  They
are grouped per buffer and each group becomes one section."
  (let ((groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (change (reverse (seq-filter #'niwa-buddy--live-change-p
                                         changes)))
      (let ((buffer-name (plist-get change :buffer)))
        (unless (gethash buffer-name groups)
          (push buffer-name order))
        (push change (gethash buffer-name groups))))
    (string-join
     (delq nil
           (mapcar (lambda (buffer-name)
                     (niwa-buddy--buffer-section
                      buffer-name (nreverse (gethash buffer-name groups))))
                   (nreverse order)))
     "\n")))


;;
;;; Review requests

(defconst niwa-buddy--brief-limit 12000
  "Most characters of a brief sent with each request.
Every automatic review pays for the brief again, so it stays bounded.")

(defvar niwa-buddy--briefs (make-hash-table :test #'equal)
  "Hash table mapping scope keys to (SOURCE . TEXT) briefs.
SOURCE is a short phrase naming where TEXT came from.")

(defun niwa-buddy-set-brief (scope-key source text)
  "Give SCOPE-KEY the brief TEXT, described as coming from SOURCE.
A nil TEXT removes the scope's brief.  Briefs live in memory only."
  (if text
      (puthash scope-key (cons source text) niwa-buddy--briefs)
    (remhash scope-key niwa-buddy--briefs)))

(defun niwa-buddy--brief-section (scope-key)
  "Return SCOPE-KEY's brief formatted for a request, or the empty string."
  (if-let* ((brief (gethash scope-key niwa-buddy--briefs)))
      (let ((text (string-trim (cdr brief))))
        (when (> (length text) niwa-buddy--brief-limit)
          (setq text (concat (substring text 0 niwa-buddy--brief-limit)
                             "\n[brief truncated]")))
        (format (concat "=== Brief: what the programmer is building, from %s"
                        " in their niwa session ===\n%s\n"
                        "=== End of brief ===\n\n")
                (car brief) text))
    ""))

(defvar niwa-buddy--running nil
  "Scope key of the review currently in flight, or nil.")

(defvar niwa-buddy--prepare-timer nil
  "Next callback preparing the current review's changed buffers.")

(defvar niwa-buddy--running-automatic nil
  "Non-nil when the review in flight was started by the idle timer.

An explicitly requested review may preempt one of those; it may not
preempt another explicit request.")

(defvar niwa-buddy--request-buffer nil
  "Hidden request buffer that identifies the review for `gptel-abort'.")

(defvar niwa-buddy--generation 0
  "Counter identifying the review in flight.

Each review takes the next generation and its callbacks do nothing once
that number has moved on, so a callback racing with abandonment cannot
retire another review's changes or tear down its state.")

(defvar niwa-buddy--timeout-timer nil
  "Timer that abandons the review in flight if it never settles.")

(defun niwa-buddy--response-action (response)
  "Return what RESPONSE means for a review in flight.

Either `tool-round' for a completed batch of tool calls, which is all
the callback needs in order to bound the loop, or `ignore'.

Settlement deliberately is not decided here.  A streaming callback
receives t when one HTTP response ends, which is not the end of the
review: with tool calls pending, gptel then runs them and issues
another request.  Treating that t as terminal settled the review before
its own tool calls ran, which cleared the annotatable buffer list out
from under them and had every note refused as out of scope.  Only the
request state machine's terminal states know the difference, so
`niwa-buddy--request-fsm' owns settlement."
  (if (and (consp response) (eq (car response) 'tool-result))
      'tool-round
    'ignore))

(defun niwa-buddy--request-fsm (finish)
  "Return a gptel state machine that calls FINISH when its request ends.

FINISH receives non-nil once the whole request has settled, including
every tool round, and nil when it errored or was aborted.  gptel's
handler table is the documented extension point for this; its terminal
states are the only place that distinguishes the end of one HTTP
response from the end of the request."
  (gptel-make-fsm
   :handlers
   (mapcar
    (lambda (entry)
      (pcase (car entry)
        ('DONE (append entry (list (lambda (_fsm) (funcall finish t)))))
        ((or 'ERRS 'ABRT)
         (append entry (list (lambda (_fsm) (funcall finish nil)))))
        (_ entry)))
    gptel-request--handlers)))

(defun niwa-buddy--session ()
  "Return a live session to attribute telemetry to, or nil.

Buddy runs from a source buffer and usually has no session at all.  One
is used when it happens to exist and skipped otherwise."
  (ignore-errors
    (niwa-telemetry-current-session)))

(defun niwa-buddy--telemetry (event &rest props)
  "Record EVENT with PROPS when a session is live to receive it.
A scope key is a project root or a buffer name, so it is hashed here: the
events only need to agree on which scope they belong to, and telemetry does
not carry paths."
  (when-let* ((session (niwa-buddy--session)))
    (when-let* ((scope (plist-get props :scope)))
      (setq props (plist-put (copy-sequence props)
                             :scope (secure-hash 'sha256 scope))))
    (ignore-errors (apply #'niwa-telemetry-record session event props))))

(defun niwa-buddy--changed-buffers (changes)
  "Return the live buffer names named by CHANGES."
  (seq-uniq
   (delq nil
         (mapcar (lambda (change)
                   (let ((name (plist-get change :buffer)))
                     (and (buffer-live-p (get-buffer name)) name)))
                 changes))))

(defun niwa-buddy--current-generation-p (generation)
  "Return non-nil when GENERATION is still the review in flight."
  (= generation niwa-buddy--generation))

(defun niwa-buddy--retire-changes (scope-key reviewed-through)
  "Drop SCOPE-KEY's changes recorded no later than REVIEWED-THROUGH.

Without this every later review would re-send the whole cumulative
diff since Emacs started, asking the model to comment again on edits it
has already seen and growing each request without bound."
  (puthash scope-key
           (seq-filter
            (lambda (record)
              (time-less-p reviewed-through (plist-get record :last-time)))
            (gethash scope-key niwa-buddy--changes))
           niwa-buddy--changes))

(defun niwa-buddy--settle (scope-key reviewed-through)
  "Finish the review of SCOPE-KEY.

REVIEWED-THROUGH retires the changes it covered, and is passed only
when the review settled on its own; an abandoned run passes nil so its
changes are offered again."
  (when niwa-buddy--prepare-timer
    (cancel-timer niwa-buddy--prepare-timer)
    (setq niwa-buddy--prepare-timer nil))
  (when reviewed-through
    (niwa-buddy--retire-changes scope-key reviewed-through))
  (when niwa-buddy--timeout-timer
    (cancel-timer niwa-buddy--timeout-timer)
    (setq niwa-buddy--timeout-timer nil))
  (unwind-protect
      ;; A failing kill must not skip persisting accepted remarks.
      (unwind-protect
          (when (buffer-live-p niwa-buddy--request-buffer)
            (kill-buffer niwa-buddy--request-buffer))
        ;; Preparation can be cancelled before the first request loads note
        ;; tools.
        (when (featurep 'niwa-buddy-note)
          (niwa-buddy-note-end-review)))
    ;; A store flush error must not leave the review marked running; the new
    ;; generation stops straggler tool calls.
    (cl-incf niwa-buddy--generation)
    (setq niwa-buddy--running nil
          niwa-buddy--running-automatic nil
          niwa-buddy--request-buffer nil)))

(defun niwa-buddy--abandon (reason)
  "Abandon the review in flight because of REASON, retiring nothing."
  (when niwa-buddy--running
    (let ((scope-key niwa-buddy--running)
          (buffer niwa-buddy--request-buffer))
      ;; Retire first so the ABRT-driven callback leaves settlement here.
      (cl-incf niwa-buddy--generation)
      (when (buffer-live-p buffer)
        (ignore-errors (gptel-abort buffer)))
      (niwa-buddy--telemetry 'buddy-abandoned
                                :scope scope-key :reason reason)
      (niwa-buddy--settle scope-key nil))))

;;;###autoload
(defun niwa-buddy-abort ()
  "Abandon the Buddy review in flight, if any."
  (interactive)
  (if niwa-buddy--running
      (progn (niwa-buddy--abandon 'user)
             (message "niwa: buddy review abandoned"))
    (message "niwa: no buddy review running")))

(defun niwa-buddy--preempt ()
  "Abandon an automatic review in flight so an explicit one can run.

Only automatic runs are preempted.  One explicit request does not
outrank another, so a second is refused rather than silently replacing
the first."
  (when (and niwa-buddy--running niwa-buddy--running-automatic)
    (niwa-buddy--abandon 'preempted)))

(cl-defun niwa-buddy--request (scope-key profile payload buffer-names
                                         reviewed-through level &optional taken-only)
  "Send PAYLOAD for SCOPE-KEY using PROFILE and settle the result.

BUFFER-NAMES bounds which buffers the review may touch.  REVIEWED-THROUGH
is recorded when the review settles on its own.  LEVEL is the captured
automatic dial level, or nil for an explicitly requested review.
TAKEN-ONLY means PAYLOAD has no edited-code section."
  (require 'gptel)
  (require 'niwa-buddy-note)
  (require 'niwa-engine)
  (require 'niwa-models)
  (require 'niwa-system)
  (let ((policy (niwa-model-resolve-workload 'buddy))
        (automatic (and level t))
        (rounds 0)
        (settled nil)
        (generation (cl-incf niwa-buddy--generation))
        (source (current-buffer))
        (brief (niwa-buddy--brief-section scope-key))
        (request-buffer (generate-new-buffer " *niwa-buddy-request*"))
        taken-notes review-hash)
    (unless (plist-get policy :model)
      (kill-buffer request-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope source) "No model resolves for the buddy workload"))
    (setq niwa-buddy--running scope-key
          niwa-buddy--running-automatic automatic
          niwa-buddy--request-buffer request-buffer)
    (condition-case err
        (progn
          (let ((begun (niwa-buddy-note-begin-review
                        buffer-names payload level (list scope-key level brief)
                        (niwa-buddy--changes-for-scope scope-key))))
            (setq payload (plist-get begun :payload)
                  taken-notes (plist-get begun :taken-notes)
                  review-hash (plist-get begun :review-hash)))
          (when (and automatic taken-only taken-notes
                     (cl-every (lambda (note)
                                 (equal review-hash
                                        (overlay-get note 'niwa-note-buddy-reviewed-hash)))
                               taken-notes))
            (niwa-buddy--settle scope-key reviewed-through)
            (cl-return-from niwa-buddy--request nil)))
      (error
       (niwa-buddy--settle scope-key nil)
       (signal (car err) (cdr err))))
    (niwa-buddy--telemetry 'buddy-review-started
                              :scope scope-key :profile profile
                              :buffers (length buffer-names))
    (cl-labels
        ((finish (ok)
           (unless settled
             (setq settled t)
             ;; A request that outlived its review must not retire that
             ;; review's changes, and must not tear down whichever review
             ;; is running now.
             (when (niwa-buddy--current-generation-p generation)
               (when ok
                 (dolist (note taken-notes)
                   (when (overlay-buffer note)
                     (overlay-put note 'niwa-note-buddy-reviewed-hash review-hash))))
               (niwa-buddy--telemetry 'buddy-review-settled
                                         :scope scope-key :settled ok
                                         :rounds rounds)
               (niwa-buddy--settle scope-key
                                      (and ok reviewed-through))))))
      (setq niwa-buddy--timeout-timer
            (run-at-time niwa-buddy-timeout nil
                         (lambda ()
                           (setq niwa-buddy--timeout-timer nil)
                           (niwa-buddy--abandon 'timeout))))
      (condition-case nil
          (let ((stream gptel-stream)
                (system
                 (niwa-system-build-prompt
                  profile
                  :workspace (niwa-buddy--workspace)
                  :working-directory default-directory)))
            (with-current-buffer request-buffer
              ;; gptel snapshots request configuration from the :buffer
              ;; with `buffer-local-value', which never sees a source
              ;; buffer's local settings.  Keep the whole request policy
              ;; together on the request buffer.
              (setq-local default-directory
                          (buffer-local-value 'default-directory source)
                          gptel-backend (plist-get policy :backend)
                          gptel-model (plist-get policy :model)
                          gptel-reasoning-effort (plist-get policy :effort)
                          gptel-system-prompt system
                          gptel-use-context nil
                          gptel-use-tools t
                          gptel-stream stream
                          gptel-tools
                          (niwa-buddy-note-tools
                           (lambda ()
                             (niwa-buddy--current-generation-p
                              generation))))
              (if (niwa-engine-external-p gptel-backend)
                  (niwa-claude-code-request-workload
                   (concat brief payload (niwa-buddy-note-serialize))
                   system gptel-tools
                   (lambda (response _info)
                     (when (memq response '(t nil abort)) (finish (eq response t))))
                   (lambda (_tool _args)
                     (unless (niwa-buddy--current-generation-p generation)
                       (error "Buddy review has ended")))
                   (lambda ()
                     (cl-incf rounds)
                     (when (> rounds niwa-buddy-max-iterations)
                       (finish nil)
                       (error "Buddy tool-round limit exceeded"))
                     nil))
                (progn (niwa-auth-assert-ready gptel-backend)
                       (gptel-request
			(concat brief payload (niwa-buddy-note-serialize))
			:buffer request-buffer
			:fsm (niwa-buddy--request-fsm #'finish)
			;; Follow the user's streaming setting rather than forcing it
			;; off.  Buddy has no use for streamed prose, but some
			;; providers reject a request with `stream' false outright.
			:stream stream
			:transforms nil
			:system system
			:callback
			(lambda (response _info)
			  (when (eq (niwa-buddy--response-action response)
				    'tool-round)
			    (setq rounds (1+ rounds))
			    (when (> rounds niwa-buddy-max-iterations)
			      ;; Aborting reaches the machine's ABRT state, which
			      ;; settles the review without retiring its changes.
			      (ignore-errors (gptel-abort request-buffer))))))))))
        (error (finish nil))))))

(defun niwa-buddy--workspace ()
  "Return the current buffer's workspace, or nil when it has none."
  (ignore-errors
    (niwa-workspace)))

(defun niwa-buddy--severity-instruction ()
  "Return the sentence stating the configured severity floor."
  (format "\n\nReport nothing below %s severity.\n"
          niwa-buddy-severity-floor))

(defun niwa-buddy--prepare (scope-key changes level &optional taken-names)
  "Prepare CHANGES for SCOPE-KEY one buffer per callback.
LEVEL captures the automatic dial, or nil for an explicit review.
A changed source abandons the
snapshot without retiring edits or sending stale line numbers to a model.
TAKEN-NAMES includes watched buffers even when they have no recorded edits."
  (let* ((source (current-buffer))
         (quiet (eq 'quiet level))
         (automatic (and level t))
         (generation (cl-incf niwa-buddy--generation))
         (through (current-time))
         (names (delete-dups (append taken-names
                                    (unless quiet (niwa-buddy--changed-buffers changes)))))
         (stamps (mapcar
                  (lambda (name)
                    (with-current-buffer name
                      (list (current-buffer) (buffer-chars-modified-tick)
                            (buffer-name) buffer-file-name default-directory)))
                  names))
         (groups (unless quiet (mapcar
                  (lambda (group) (reverse (cdr group)))
                  (seq-group-by (lambda (change) (plist-get change :buffer))
                                (reverse changes)))))
         sections dispatching)
    (setq niwa-buddy--running scope-key
          niwa-buddy--running-automatic automatic)
    (cl-labels
        ((currentp ()
           (and (buffer-live-p source)
                (equal changes (seq-filter #'niwa-buddy--live-change-p
                                           (niwa-buddy--changes-for-scope scope-key)))
                (cl-every
                 (lambda (stamp)
                   (and (buffer-live-p (car stamp))
                        (with-current-buffer (car stamp)
                          (equal (cdr stamp)
                                 (list (buffer-chars-modified-tick) (buffer-name)
                                       buffer-file-name default-directory)))))
                 stamps)))
         (schedule (delay)
           (when (niwa-buddy--current-generation-p generation)
             (setq niwa-buddy--prepare-timer (run-at-time delay nil #'step))))
         (step ()
           (when (niwa-buddy--current-generation-p generation)
             (setq niwa-buddy--prepare-timer nil)
             (cond
              ((not (currentp)) (niwa-buddy--abandon 'source-changed))
              ((niwa-transport-busy-p
                (buffer-local-value 'default-directory source))
               (schedule .1))
              (t
               (condition-case nil
                   (with-current-buffer source
                     (if groups
                         (progn
                           (push (niwa-buddy--format-changes (pop groups)) sections)
                           (schedule .001))
                       (let ((payload (string-join
                                       (seq-remove #'string-empty-p (nreverse sections)) "\n")))
                         (if (and (string-empty-p payload) (null taken-names))
                             (niwa-buddy--settle scope-key through)
                           (setq dispatching t)
                           (niwa-buddy--request
                            scope-key 'buddy
                            (concat payload (niwa-buddy--severity-instruction))
                            names through level (string-empty-p payload))))))
                 (error
                  (when (or dispatching (niwa-buddy--current-generation-p generation))
                    (niwa-buddy--abandon 'preparation-failed)))))))))
      (schedule .001))))

(defun niwa-buddy--taken-buffers (scope-key)
  "Return tracked buffers with taken notes in SCOPE-KEY."
  (let (names)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p niwa-buddy-mode)
                   (equal scope-key (niwa-buddy--scope-key))
                   (save-restriction
                     (widen)
                     (seq-some (lambda (note)
                                 (niwa-note-watching-p note))
                               (overlays-in (point-min) (point-max)))))
          (push (buffer-name) names))))
    names))

(defun niwa-buddy-review (&optional automatic)
  "Review the edits recorded for the current buffer's scope.

AUTOMATIC marks a run started by the idle timer rather than by the
user.  Returns non-nil when preparation was scheduled."
  (interactive)
  (require 'niwa-transport)
  (let* ((workspace (niwa-buddy--workspace))
         (_ (niwa-buddy--load-level workspace))
         (level (and automatic (niwa-buddy-level workspace)))
         (scope-key (niwa-buddy--scope-key))
         (taken (niwa-buddy--taken-buffers scope-key))
         (changes (seq-filter #'niwa-buddy--live-change-p
                              (niwa-buddy--changes-for-scope scope-key))))
    (cond
     ((and (eq 'quiet level) (null taken))
      (niwa-buddy--retire-changes scope-key (current-time))
      nil)
     ((and (null changes) (null taken)) nil)
     (niwa-buddy--running nil)
     (t
      (niwa-buddy--prepare scope-key changes level taken)
      t))))


;;
;;; Tracking policy

(defun niwa-buddy--own-buffer-p (buffer)
  "Return non-nil when BUFFER is one of niwa's own surfaces.

Annotating a transcript, cockpit, or inspector is never wanted, and
leaving that to the tracked-modes default would be an accident waiting
to happen."
  (with-current-buffer buffer
    (or (bound-and-true-p niwa--session)
        (bound-and-true-p niwa--view-buffer)
        (bound-and-true-p niwa--data-buffer)
        (string-prefix-p "niwa-" (symbol-name major-mode))
        (string-prefix-p "*niwa" (buffer-name)))))

(defun niwa-buddy--tracked-buffer-p (&optional buffer)
  "Return non-nil when BUFFER should have its edits watched."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         ;; Buffers whose name starts with a space are internal.
         (not (string-prefix-p " " (buffer-name buffer)))
         (not (niwa-buddy--own-buffer-p buffer))
         (with-current-buffer buffer
           (apply #'derived-mode-p niwa-buddy-tracked-modes)))))


;;
;;; Timers

(defvar-local niwa-buddy--idle-timer nil
  "Idle timer that will review this buffer's scope, or nil.")

(put 'niwa-buddy--idle-timer 'permanent-local t)

(defvar niwa-buddy--last-review (make-hash-table :test #'equal)
  "Hash table mapping scope keys to when they were last reviewed.")

(defun niwa-buddy--interval-remaining (scope-key)
  "Return seconds before SCOPE-KEY may be reviewed automatically again.
Zero means the minimum interval has elapsed and a review is due."
  (let ((last (gethash scope-key niwa-buddy--last-review)))
    (if last
        (max 0 (- niwa-buddy-min-interval
                  (float-time (time-subtract (current-time) last))))
      0)))

(defun niwa-buddy--cancel-timer ()
  "Cancel this buffer's pending automatic review."
  (when niwa-buddy--idle-timer
    (cancel-timer niwa-buddy--idle-timer)
    (setq niwa-buddy--idle-timer nil)))

(defun niwa-buddy--run-scheduled (buffer)
  "Review BUFFER's scope, now that the user has stopped typing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq niwa-buddy--idle-timer nil)
      (let ((scope-key (niwa-buddy--scope-key)))
        (cond
         ;; Something is already talking to a model.  Retry on a plain
         ;; timer: re-arming an idle timer from inside one fires it
         ;; immediately, because Emacs is already idle past the delay.
         (niwa-buddy--running (niwa-buddy--retry))
         (t
          (let ((wait (niwa-buddy--interval-remaining scope-key)))
            (cond
             ;; Reviewed too recently.  The pending edits still deserve a
             ;; review, so look again once the interval has run out; a
             ;; further edit replaces this with a fresh idle timer.
             ((> wait 0) (niwa-buddy--retry wait))
             ((niwa-buddy-review t)
              (puthash scope-key (current-time)
                       niwa-buddy--last-review))))))))))

(defun niwa-buddy--retry (&optional delay)
  "Look again after DELAY seconds, `niwa-buddy-idle-delay' by default."
  (niwa-buddy--cancel-timer)
  (setq niwa-buddy--idle-timer
        (run-at-time (or delay niwa-buddy-idle-delay) nil
                     #'niwa-buddy--run-scheduled (current-buffer))))

(defun niwa-buddy--schedule ()
  "Arrange for this buffer's scope to be reviewed once the user pauses."
  (niwa-buddy--cancel-timer)
  (setq niwa-buddy--idle-timer
        (run-with-idle-timer niwa-buddy-idle-delay nil
                             #'niwa-buddy--run-scheduled
                             (current-buffer))))

(defun niwa-buddy--after-change-schedule (_beg _end _length)
  "Schedule a review after an edit."
  (niwa-buddy--schedule))


;;
;;; Modes

;;;###autoload
(define-minor-mode niwa-buddy-mode
  "Let a model comment on the edits you make in this buffer.

Notes appear beside the lines they concern.  Nothing is written to
source: file-backed remarks persist as workspace notes, while recorded
edits and request state live only in memory."
  :lighter " Buddy"
  :group 'niwa-buddy
  (if niwa-buddy-mode
      (progn
        (niwa-buddy--track-buffer)
        (add-hook 'after-change-functions
                  #'niwa-buddy--after-change-schedule nil t)
        (when (save-restriction
                (widen)
                (seq-some (lambda (note)
                            (niwa-note-watching-p note))
                          (overlays-in (point-min) (point-max))))
          (niwa-buddy--schedule)))
    (niwa-buddy--cancel-timer)
    (niwa-buddy--untrack-buffer)
    (remove-hook 'after-change-functions
                 #'niwa-buddy--after-change-schedule t)))

(defun niwa-buddy--maybe-enable ()
  "Enable `niwa-buddy-mode' when this buffer qualifies."
  (when (niwa-buddy--tracked-buffer-p)
    (niwa-buddy-mode 1)))

;;;###autoload
(define-globalized-minor-mode niwa-buddy-global-mode
  niwa-buddy-mode niwa-buddy--maybe-enable
  :group 'niwa-buddy)


;;
;;; Guidance

(defun niwa-buddy-guide-payload (beg end)
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
                    (niwa-buddy--scope-key)
                    (line-number-at-pos (point)))
            (string-join (nreverse numbered) "\n"))))

;;;###autoload
(defun niwa-buddy-guide ()
  "Ask a model what to build here, rather than what went wrong.

Sends the active region, or the whole buffer, and annotates it with
suggestions, missing pieces, and the decisions still open.  Works in
any buffer; `niwa-buddy-mode' need not be enabled.

A queued or running automatic review is abandoned first: the request you
made outranks one started by a timer."
  (interactive)
  (niwa-buddy--cancel-timer)
  (niwa-buddy--preempt)
  (when niwa-buddy--running
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "A buddy request is already running; %s"
                "abort it with `niwa-buddy-abort'"))
  (niwa-buddy--request
   (niwa-buddy--scope-key) 'buddy-guide
   (niwa-buddy-guide-payload
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max)))
   (list (buffer-name))
   nil nil))

(provide 'niwa-buddy)
;;; niwa-buddy.el ends here
