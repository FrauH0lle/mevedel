;;; mevedel-session-rewind.el --- Transactional session Rewind -*- lexical-binding: t -*-

;;; Commentary:

;; Owns restore plans and the complete transactional Rewind lifecycle.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-agents)
  (require 'mevedel-structs))

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

;; `gptel-org'
(declare-function gptel-org--restore-state "ext:gptel-org" nil)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))

;; `mevedel-chat'
(declare-function mevedel--chat-buffer-disable-org-element-cache "mevedel-chat" nil)
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-directive'
(declare-function mevedel-workspace-rewind-directives "mevedel-directive" (workspace session-id target-turn))
(declare-function mevedel-workspace-set-directives "mevedel-directive" (workspace directives))

;; `mevedel-execution'
(declare-function mevedel-execution-session-live-p "mevedel-execution" (session))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-native-path "mevedel-execution-target" (target path))

;; `mevedel-persistence'
(declare-function mevedel--reset-instructions-preserving-directives "mevedel-persistence" (workspace directives))
(declare-function mevedel--restore-preserved-directives "mevedel-persistence" (workspace))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-backup-path "mevedel-session-artifacts" (save-path backup-name))
(declare-function mevedel-session-artifacts-build-sidecar "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-collect-prompts "mevedel-session-artifacts" (buffer))
(declare-function mevedel-session-artifacts-fork-point-spans "mevedel-session-artifacts" (buffer))
(declare-function mevedel-session-artifacts-instructions-current-path "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-instructions-dir "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-instructions-turn-path "mevedel-session-artifacts" (save-path turn))
(declare-function mevedel-session-artifacts-load-instructions "mevedel-session-artifacts" (session buffer &optional turn directive-records preserve-directives-p))
(declare-function mevedel-session-artifacts-printed-value "mevedel-session-artifacts" (value))
(declare-function mevedel-session-artifacts-read-artifact "mevedel-session-artifacts" (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-read-file-raw "mevedel-session-artifacts" (path))
(declare-function mevedel-session-artifacts-save-instructions "mevedel-session-artifacts" (session buffer &optional current-only))
(declare-function mevedel-session-artifacts-segment-path "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-sidecar-path "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-update-prompt-index "mevedel-session-artifacts" (session buffer))

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-deserialize "mevedel-session-codec" (plist workspace))
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-write "mevedel-session-codec" (path plist))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability" (session function))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-list-sessions "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-notify-session-event "mevedel-session-persistence" (session event &rest args))
(declare-function mevedel-session-persistence-parse-iso-time "mevedel-session-persistence" (str))
(declare-function mevedel-session-persistence-resume-id "mevedel-session-persistence" (workspace session-id))
(declare-function mevedel-session-persistence-write-current-buffer-atomically "mevedel-session-persistence" (path))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-discard-rolled-back "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-logical-path-p "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir))

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery-record-failure "mevedel-session-recovery" (session reason recovery-path))

;; `mevedel-structs'
(declare-function mevedel-directive-attempt-checkpoint "mevedel-structs" (cl-x))
(declare-function mevedel-directive-attempt-untracked-effects "mevedel-structs" (cl-x))
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x))
(declare-function mevedel-directive-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal-status "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-lease "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-input-p "mevedel-structs" (session))
(declare-function mevedel-session-pending-plan-approval "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-log-pending "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-queue "mevedel-structs" (cl-x))
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication-active-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-task-status-notes "mevedel-structs" (cl-x))
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(declare-function mevedel-transcript-restore-sanitize-bounds "mevedel-transcript-restore" nil)

;; `mevedel-view'
(defvar mevedel--data-buffer)

;;
;;; File restore plan

(defun mevedel-session-rewind--latest-snapshot-entry (session path)
  "Return the highest-version snapshot plist for PATH in SESSION, or nil."
  (let ((best nil) (best-version 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  ((not (plist-get (cdr entry) :gap)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best-version)
          (setq best-version v
                best          (cdr entry)))))))

(defun mevedel-session-rewind-read-backup (session backup-name)
  "Return SESSION's BACKUP-NAME as literal bytes.

Portable project Rewind resolves only the committed immutable publication.  File
sessions retain their existing direct file-history read."
         (require 'mevedel-session-artifacts)
         (require 'mevedel-session-codec)
         (if (mevedel-session-codec-portable-authority-p session)
      (mevedel-session-artifacts-read-artifact
       session (file-name-concat "file-history" backup-name) t)
    (mevedel-session-artifacts-read-file-raw
     (mevedel-session-artifacts-backup-path
      (mevedel-session-save-path session) backup-name))))

(defun mevedel-session-rewind-state-at-turn
    (session cum-turn &optional before-turn)
  "Return SESSION tracked-file state at CUM-TURN.

For each path that ever appeared in SESSION's `:file-snapshots',
picks the latest checkpoint through CUM-TURN.  When BEFORE-TURN is non-nil,
picks its earliest checkpoint in the discarded suffix instead."
  (let ((state (make-hash-table :test #'equal)))
    (dolist
        (turn-entry
         (sort (copy-sequence (mevedel-session-file-snapshots session))
               (if before-turn
                   (lambda (a b) (< (car a) (car b)))
                 (lambda (a b) (> (car a) (car b))))))
      (let ((turn (car turn-entry)))
        (when (if before-turn
                  (>= turn cum-turn)
                (<= turn cum-turn))
          (dolist (file-entry (cdr turn-entry))
            (unless (gethash (car file-entry) state)
              (puthash (car file-entry) (cdr file-entry) state))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) state)
      result)))

(defun mevedel-session-rewind--plan-action
    (session path target-plist &optional before-turn)
  "Return SESSION restore action plist for PATH.

TARGET-PLIST is the snapshot entry recorded for PATH at the picked turn
or earlier.  Possible `:action' values are:

  noop       File already matches target state.
  delete     Target state is absent; file currently exists.
  create     Target has content; file currently absent.
  restore    Target has content; file differs but matches its own
             latest snapshot (i.e., no detected external changes).
  overwrite  Target has content; file differs from target AND from
             latest snapshot (external edits will be overwritten)."
  (require 'mevedel-session-artifacts)
  (let* ((target-backup-name
          (plist-get target-plist
                     (if before-turn :pre-backup-name :backup-name)))
         (currently-exists   (file-exists-p path)))
    (cond
     ;; Target says "absent" at the picked turn.
     ((null target-backup-name)
      (if currently-exists
          (list :action 'delete :path path)
        (list :action 'noop :path path)))
     ;; Target has content; file currently absent.
     ((not currently-exists)
      (list :action 'create
            :path path
            :backup-name target-backup-name))
     ;; Target has content; file currently present -- compare.
     (t
      (let* ((target-content
              (mevedel-session-rewind-read-backup
               session target-backup-name))
             (current-content
              (mevedel-session-artifacts-read-file-raw path)))
        (if (string-equal current-content target-content)
            (list :action 'noop :path path)
          ;; Differs from target.  Check vs LATEST snapshot to detect
          ;; external changes since.
          (let* ((latest (mevedel-session-rewind--latest-snapshot-entry
                          session path))
                 (latest-name (and latest (plist-get latest :backup-name)))
                 (latest-content
                  (and latest-name
                       (mevedel-session-rewind-read-backup
                        session latest-name)))
                 (diverged (not (and latest-content
                                     (string-equal current-content
                                                   latest-content)))))
            (list :action (if diverged 'overwrite 'restore)
                  :path path
                  :backup-name target-backup-name
                  :diverged diverged))))))))

(defun mevedel-session-rewind-restore-plan
    (session cum-turn &optional before-turn)
  "Compute SESSION's captured file-restore plan at CUM-TURN.

Returns a list of plan-entry plists (see
`mevedel-session-rewind--plan-action').  An empty list means
nothing to do.  When BEFORE-TURN is non-nil, target the pre-turn checkpoint."
  (require 'cl-lib)
  (let ((target-state
         (mevedel-session-rewind-state-at-turn
          session cum-turn before-turn))
        (plan nil))
    (dolist (entry target-state)
      (unless (plist-get (cdr entry) :gap)
        (push (mevedel-session-rewind--plan-action
               session (car entry) (cdr entry) before-turn)
              plan)))
    (cl-remove-if
     (lambda (e) (eq 'noop (plist-get e :action)))
     (nreverse plan))))

(defun mevedel-session-rewind--checkpoint-gaps (session cum-turn)
  "Return known SESSION checkpoint gaps before CUM-TURN."
  (cl-loop for (path . checkpoint)
           in (mevedel-session-rewind-state-at-turn
               session cum-turn t)
           when (plist-get checkpoint :gap)
           collect (list :path path :reason (plist-get checkpoint :gap))))

(defun mevedel-session-rewind--directive-capture-gaps
    (session target-turn)
  "Return untracked directive effects discarded from TARGET-TURN in SESSION."
  (when-let* ((workspace (mevedel-session-workspace session)))
    (let ((session-id (mevedel-session-session-id session)))
      (cl-loop
       for directive in
       (mevedel-workspace-directives workspace)
       append
       (cl-loop
        for attempt in (mevedel-directive-attempts directive)
        for checkpoint = (mevedel-directive-attempt-checkpoint attempt)
        when (and (equal session-id (plist-get checkpoint :session-id))
                  (>= (or (plist-get checkpoint :turn) 0) target-turn))
        append
        (mapcar
         (lambda (effect)
           (list :path
                 (format "Directive %s via %s"
                         (mevedel-directive-id directive) (car effect))
                 :reason (cdr effect)))
         (mevedel-directive-attempt-untracked-effects attempt)))))))

(defvar-local mevedel-session-rewind--plan-buffer-session nil
  "Buffer-local session for the `*mevedel-rewind-impact*' buffer.
Consumed by `mevedel-session-rewind--plan-row-diff' so `d' on a
plan row can resolve the backup file.")

(defvar mevedel-session-rewind--plan-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "d")
      #'mevedel-session-rewind--plan-row-diff)
    map)
  "Keymap for the `*mevedel-rewind-impact*' buffer.
Adds `d' -- show diff between current file and target snapshot.")

(defun mevedel-session-rewind--plan-row-diff ()
  "Show a diff between the current file and the restore target backup.
Invoked from `*mevedel-rewind-impact*' via the `d' binding.  The row
at point identifies the path; its `mevedel-plan-entry' text property
carries the plan-entry plist."
  (interactive)
  (let* ((entry
          (get-text-property (line-beginning-position) 'mevedel-plan-entry))
         (session mevedel-session-rewind--plan-buffer-session))
    (unless entry
      (user-error "No restore plan row at point"))
    (unless session
      (user-error "Plan buffer has no associated session"))
    (let* ((path (plist-get entry :path))
           (action (plist-get entry :action))
           (backup-name (plist-get entry :backup-name)))
      (pcase action
        ('delete
         (user-error "Row is a delete action; nothing to diff against"))
        ('noop
         (user-error "Row is a noop; nothing to diff"))
        (_
         (let ((backup-path (make-temp-file "mevedel-rewind-backup-")))
           (unwind-protect
               (progn
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert
                    (mevedel-session-rewind-read-backup
                     session backup-name))
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region nil nil backup-path nil 'silent)))
                 (diff (or (and (file-exists-p path) path) "/dev/null")
                       backup-path nil 'no-async))
             (delete-file backup-path))))))))

(defun mevedel-session-rewind--apply-restore-action (session entry)
  "Apply one restore ENTRY (plan-entry plist) for SESSION."
  (let* ((path        (plist-get entry :path))
         (action      (plist-get entry :action))
         (backup-name (plist-get entry :backup-name)))
    (pcase action
      ('noop nil)
      ('delete
       (when (file-exists-p path)
         (delete-file path)))
      ((or 'create 'restore 'overwrite)
       (let ((content
              (mevedel-session-rewind-read-backup
               session backup-name)))
         (let ((dir (file-name-directory path)))
           (when (and dir (not (file-directory-p dir)))
             (make-directory dir t)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region content nil path nil 'silent)))))))

(defun mevedel-session-rewind-execute-restore (session plan)
  "Execute restore PLAN for SESSION, stopping on the first failure.

Returns a plist describing the outcome:
  (:succeeded N :failed PATH-OR-NIL :error STR-OR-NIL :total N)

Successful actions are applied in order; on error, remaining plan
entries are not attempted.  The user-visible report goes to
`*mevedel-restore-results*'."
  (let ((succeeded 0) (failed nil) (err-str nil)
        (total (length plan)))
    (catch 'failed
      (dolist (entry plan)
        (condition-case e
            (progn
              (mevedel-session-rewind--apply-restore-action session entry)
              (cl-incf succeeded))
          (error
           (setq failed   (plist-get entry :path)
                 err-str  (error-message-string e))
           (throw 'failed nil)))))
    (with-current-buffer (get-buffer-create "*mevedel-restore-results*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Restore results: %d/%d files done\n"
                        succeeded total))
        (when failed
          (insert (format "Failed on %s: %s\n" failed err-str)))))
    (list :succeeded succeeded :failed failed
          :error err-str :total total)))

(defun mevedel-session-rewind--modified-buffers-for-plan (plan)
  "Return modified buffers visiting files affected by restore PLAN."
  (require 'cl-lib)
  (let (buffers)
    (dolist (entry plan)
      (when-let* ((path (plist-get entry :path))
                  (buf (find-buffer-visiting path))
                  ((buffer-live-p buf)))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (push buf buffers)))))
    (nreverse (cl-remove-duplicates buffers))))

(defun mevedel-session-rewind--prepare-buffers-for-restore
    (session cum-turn plan)
  "Prepare visiting buffers before restoring PLAN for SESSION at CUM-TURN.

If modified buffers visit affected files, prompt the user to save,
discard, or abort.  Returns the current restore plan, recomputing it
after saves.  Returns nil when the restore should be aborted."
  (let ((current-plan plan)
        done)
    (while (not done)
      (let ((buffers
             (mevedel-session-rewind--modified-buffers-for-plan
              current-plan)))
        (if (null buffers)
            (setq done t)
          (pcase (read-char-choice
                  (format
                   "Rewind affects %d modified buffer%s (%s): [s]ave, [d]iscard, [a]bort? "
                   (length buffers)
                   (if (= 1 (length buffers)) "" "s")
                   (mapconcat #'buffer-name buffers ", "))
                  '(?s ?d ?a))
            (?s
             (save-some-buffers
              nil
              (lambda ()
                (memq (current-buffer) buffers)))
             (setq current-plan
                   (mevedel-session-rewind-restore-plan
                    session cum-turn t)))
            (?d
             (dolist (buf buffers)
               (with-current-buffer buf
                 (revert-buffer t t t)))
             (setq done t))
            (?a
             (setq current-plan :abort
                   done t))))))
    current-plan))

(defun mevedel-session-rewind--refresh-restored-buffers (plan result)
  "Refresh visiting buffers for files restored by PLAN.

RESULT is the plist returned by `mevedel-session-rewind-execute-restore'."
  (let ((remaining (plist-get result :succeeded)))
    (dolist (entry plan)
      (when (> remaining 0)
        (cl-decf remaining)
        (let ((path (plist-get entry :path))
              (action (plist-get entry :action)))
          (pcase action
            ((or 'create 'restore 'overwrite)
             (when-let* (((file-exists-p path))
                         (buf (find-buffer-visiting path)))
               (with-current-buffer buf
                 (revert-buffer t t t))))
            ('delete
             (when-let* ((buf (find-buffer-visiting path)))
               (with-current-buffer buf
                 (set-buffer-modified-p nil))
               (kill-buffer buf)))))))))


;;
;;; Rewind picker

(defun mevedel-session-rewind--prompt-label (prompt)
  "Return PROMPT's concise picker and impact label."
  (if (eq (plist-get prompt :kind) 'directive)
      (let ((id (or (plist-get prompt :directive-id) "directive")))
        (format "◆ %s · %s"
                (substring id 0 (min 8 (length id)))
                (capitalize
                 (replace-regexp-in-string
                  "-" " "
                  (symbol-name (or (plist-get prompt :action) 'directive))))))
    (or (plist-get prompt :preview) "(empty prompt)")))

(defun mevedel-session-rewind--prompt-candidates (session)
  "Return alist entries of DISPLAY to PLIST for SESSION prompt history.

PLIST has `:segment', `:turn', `:file-turn', `:cum-turn', `:pos',
`:preview'.
DISPLAY is unique across the whole session -- segment and turn
numbers are folded into the display string so duplicate previews
do not collide.

Segments are listed newest-first (the live segment at the top of
the picker); within each segment, prompts are listed newest-first so
recent turns appear before older turns."
  (let ((all nil))
    (dolist (segment-entry
             (sort (copy-sequence (mevedel-session-prompt-index session))
                   ;; Newest segment first.
                   (lambda (a b) (> (car a) (car b)))))
      (let ((segment-n (car segment-entry)))
        (dolist (prompt (reverse (cdr segment-entry)))
          (when (plist-get prompt :fork-point-id)
            (let* ((turn (plist-get prompt :turn))
                   (display
                    (format "S%d T%d  %s" segment-n turn
                            (mevedel-session-rewind--prompt-label
                             prompt)))
                   (target (copy-sequence prompt)))
              (plist-put target :segment segment-n)
              (push (cons display target) all))))))
    (nreverse all)))

(defvar mevedel-session-rewind--prompt-history nil
  "History list for `mevedel-rewind' picks.")

(defun mevedel-session-rewind--prompt-collection-fn (candidates lookup)
  "Return a completion table for the rewind picker.

CANDIDATES is the list returned by
`mevedel-session-rewind--prompt-candidates' (each element is
`(DISPLAY . PLIST)').  LOOKUP is a hash-table mapping DISPLAY to
PLIST so the metadata helpers resolve in O(1).

The returned function answers `(metadata)' with:
  - category          = `mevedel-prompt' (consult/marginalia hook);
  - annotation-function inserts a right-aligned S<segment> T<turn>
    marker so picker rows carry context beyond the preview string;
  - group-function groups rows by segment with headings like
    `Segment N'.

Any other action delegates to `complete-with-action' over the raw
DISPLAY strings in CANDIDATES order -- newest segment first, newest
turn first within each segment."
  (let ((displays (mapcar #'car candidates)))
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
        `(metadata
          (category . mevedel-prompt)
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          (annotation-function
           . ,(lambda (s)
                (when-let* ((p (gethash s lookup)))
                  (format "  S%d T%d"
                          (or (plist-get p :segment) 0)
                          (or (plist-get p :turn) 0)))))
          (group-function
           . ,(lambda (s transform)
                (if transform
                    s
                  (when-let* ((p (gethash s lookup)))
                    (format "Segment %d"
                            (or (plist-get p :segment) 0))))))))
       (t
        (complete-with-action action displays string pred))))))

(defun mevedel-session-rewind-format-relative-time (iso)
  "Format ISO (a `YYYY-MM-DDTHH-MM-SS' string) as a relative age.
Returns strings like `2h ago' / `yesterday' / `Apr 22'.  Returns a
placeholder when ISO cannot be parsed."
  (require 'mevedel-session-persistence)
  (let ((t2 (mevedel-session-persistence-parse-iso-time iso)))
    (if (not t2)
        "?"
      (let* ((secs (- (float-time) (float-time t2)))
             (abs  (abs secs)))
        (cond
         ((< abs 60) "just now")
         ((< abs 3600) (format "%dm ago" (/ abs 60)))
         ((< abs (* 24 3600)) (format "%dh ago" (/ abs 3600)))
         ((< abs (* 48 3600)) "yesterday")
         ((< abs (* 7 24 3600)) (format "%dd ago" (/ abs (* 24 3600))))
         (t (format-time-string "%b %d" t2)))))))

(defun mevedel-session-rewind--find-turn-cutoff (turn-n)
  "Return the position right before the (TURN-N + 1)th user prompt.
Returns `point-max' when TURN-N is the final user prompt.  Skips the
leading org property drawer, `#+begin_summary'/`#+end_summary' block
bodies, and gptel org tool/reasoning scaffolding to stay consistent with
`mevedel-session-artifacts-collect-prompts'."
  (require 'mevedel-session-artifacts)
  (save-excursion
    (save-restriction
      (widen)
      (or (plist-get
           (nth turn-n
                (mevedel-session-artifacts-collect-prompts
                 (current-buffer)))
           :pos)
          (point-max)))))

(defun mevedel-session-rewind-load-rewind-target
    (session buffer target &optional before-turn)
  "Load SESSION's TARGET transcript boundary into BUFFER without publishing it.
When BEFORE-TURN is non-nil, discard TARGET itself as well as later text."
  (require 'cl-lib)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (let* ((segment-n (plist-get target :segment))
         (segment-path
          (mevedel-session-artifacts-segment-path
           (mevedel-session-save-path session) segment-n))
         (portable-p
          (mevedel-session-codec-portable-authority-p session))
         (logical (file-name-nondirectory segment-path))
         (content
          (when portable-p
            (condition-case nil
                (mevedel-session-artifacts-read-artifact
                 session logical t)
              (error
               (user-error "Published segment %d is unavailable" segment-n))))))
    (unless (or portable-p (file-exists-p segment-path))
      (user-error "Segment %d file missing: %s" segment-n segment-path))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((buffer-file-name segment-path))
          (if portable-p
              (insert
               (decode-coding-string
                content (or buffer-file-coding-system 'utf-8-unix)))
            (insert-file-contents segment-path))
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
              (mevedel--chat-buffer-disable-org-element-cache))
            ;; Force re-restoration of GPTEL_BOUNDS from the org property.
            (when (fboundp 'gptel-org--restore-state)
              (require 'mevedel-transcript-restore)
              (mevedel-transcript-restore-sanitize-bounds)
              (gptel-org--restore-state))))
        (let* ((id (plist-get target :fork-point-id))
               (fork-point
                (cl-find id
                         (mevedel-session-artifacts-fork-point-spans
                          buffer)
                         :key (lambda (entry)
                                (plist-get entry :fork-point-id))
                         :test #'equal))
               (cutoff (if before-turn
                           (plist-get target :pos)
                         (plist-get fork-point :transcript-cutoff))))
          (unless cutoff
            (error "Rewind target is missing from segment %d" segment-n))
          (when (< cutoff (point-max))
            (delete-region cutoff (point-max))))
        (mevedel-transcript-restore-properties t)
        (mevedel-session-artifacts-stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun mevedel-session-rewind-resolve-fork-target (session target)
  "Resolve TARGET's stable identity against SESSION's current index."
  (require 'cl-lib)
  (let ((id (plist-get target :fork-point-id)))
    (or
     (cl-loop for (segment . prompts)
              in (mevedel-session-prompt-index session)
              for prompt =
              (cl-find id prompts
                       :key (lambda (entry)
                              (plist-get entry :fork-point-id))
                       :test #'equal)
              when prompt
              return (let ((resolved (copy-sequence prompt)))
                       (plist-put resolved :segment segment)
                       (when-let* ((reservation
                                    (plist-get target
                                               :worktree-reservation)))
                         (plist-put resolved
                                    :worktree-reservation reservation))
                       resolved))
     (user-error "Assistant fork point no longer exists"))))

(defun mevedel-session-rewind-assert-stable-source
    (session buffer operation)
  "Refuse OPERATION when SESSION or BUFFER owns live work."
  (when (mevedel-session-pending-input-p session)
    (user-error
     "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before %s"
     operation))
  (when (buffer-local-value 'mevedel--current-request buffer)
    (user-error "Abort the current request before %s" operation))
  (require 'mevedel-execution)
  (when (mevedel-execution-session-live-p session)
    (user-error "Stop live executions with /ps or /stop before %s" operation))
  (require 'mevedel-agent-control)
  (when (mevedel-agent-control-active-turn-p session)
    (user-error "Interrupt active agent turns before %s" operation))
  (when-let* ((goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    (user-error "Pause the active Goal before %s" operation)))

(defun mevedel-session-rewind--rewind-cleared-state (session)
  "Return user-facing names of live SESSION state cleared by Rewind."
  (delq
   nil
   (list
    (and (mevedel-session-tasks session) "tasks")
    (and (mevedel-session-goal session) "Goal")
    (and (mevedel-session-agent-registry session) "agents and mailboxes")
    (and (mevedel-session-pending-plan-approval session) "Plan approval")
    (and (mevedel-session-plan-metadata session) "Plan handoff")
    (and (mevedel-session-pending-input-p session) "pending input")
    (and (mevedel-session-permission-queue session) "permission queue")
    (and (mevedel-session-execution-state session) "execution state"))))

(defun mevedel-session-rewind--staged-file-p (path)
  "Return non-nil when PATH differs in the Git index."
  (let ((directory (file-name-directory path)))
    (when (file-directory-p directory)
      (require 'mevedel-execution-target)
      (let* ((target (mevedel-execution-target-create directory))
             (default-directory (file-name-as-directory directory))
             (remote (file-remote-p default-directory))
             (process-environment
              (unless remote process-environment)))
        (and (executable-find "git" remote)
             (= 1
                (process-file
                 "git" nil nil nil
                 "-C"
                 (mevedel-execution-target-native-path target directory)
                 "diff" "--cached" "--quiet" "--"
                 (mevedel-execution-target-native-path target path))))))))

(defun mevedel-session-rewind--detached-child-count
    (session target-turn)
  "Return direct child count detached by rewinding SESSION to TARGET-TURN."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-persistence)
  (let ((session-id (mevedel-session-session-id session))
        (portable-p
         (mevedel-session-codec-portable-authority-p session))
        (count 0))
    (dolist (entry
             (mevedel-session-persistence-list-sessions
              (mevedel-session-workspace session)))
      (when-let* ((path (plist-get entry :save-path))
                  (sidecar-path
                   (if portable-p
                       (plist-get (plist-get entry :publication) :sidecar)
                     (mevedel-session-artifacts-sidecar-path path)))
                  (sidecar
                   (condition-case nil
                       (mevedel-session-codec-read sidecar-path)
                     (error nil))))
        (when (and (equal session-id
                          (plist-get sidecar :forked-from-session-id))
                   (> (or (plist-get sidecar :forked-from-turn) 0)
                      target-turn))
          (cl-incf count))))
    count))

(defun mevedel-session-rewind--rewind-impact (session target file-plan)
  "Return the complete Rewind impact for SESSION, TARGET, and FILE-PLAN."
  (require 'cl-lib)
  (let* ((target-turn (plist-get target :cum-turn))
         (surviving-turn (1- target-turn)))
    (list
     :target target
     :file-plan file-plan
     :discarded-turns
     (max 0 (1+ (- (or (mevedel-session-turn-count session) 0)
                       target-turn)))
     :discarded-prompts
     (sort
      (cl-loop
       for (segment . prompts) in (mevedel-session-prompt-index session)
       append
       (cl-loop for prompt in prompts
                when (>= (or (plist-get prompt :cum-turn) 0) target-turn)
                collect (plist-put (copy-sequence prompt)
                                   :segment segment)))
      (lambda (a b)
        (< (plist-get a :cum-turn) (plist-get b :cum-turn))))
     :checkpoint-gaps
     (append
      (mevedel-session-rewind--checkpoint-gaps session target-turn)
      (mevedel-session-rewind--directive-capture-gaps
       session target-turn))
     :external-overwrites
     (cl-count 'overwrite file-plan
               :key (lambda (entry) (plist-get entry :action)))
     :staged-files
     (cl-loop for entry in file-plan
              for path = (plist-get entry :path)
              when (mevedel-session-rewind--staged-file-p path)
              collect path)
     :detached-children
     (mevedel-session-rewind--detached-child-count
      session surviving-turn)
     :cleared-state
     (mevedel-session-rewind--rewind-cleared-state session))))

(defun mevedel-session-rewind--rewind-impact-empty-p (impact)
  "Return non-nil when IMPACT would change no Rewind-owned state."
  (and (= 0 (plist-get impact :discarded-turns))
       (null (plist-get impact :file-plan))
       (null (plist-get impact :checkpoint-gaps))
       (null (plist-get impact :cleared-state))))

(defun mevedel-session-rewind--render-rewind-impact (session impact)
  "Display inspectable SESSION Rewind IMPACT."
  (let* ((target (plist-get impact :target))
         (plan (plist-get impact :file-plan))
         (gaps (plist-get impact :checkpoint-gaps))
         (staged (plist-get impact :staged-files))
         (discarded-prompts (plist-get impact :discarded-prompts))
         (cleared (plist-get impact :cleared-state)))
    (with-current-buffer (get-buffer-create "*mevedel-rewind-impact*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Rewind %s before S%d T%d\n\n"
                        (mevedel-session-name session)
                        (plist-get target :segment)
                        (plist-get target :turn)))
        (insert (format "Later turns discarded: %d\n"
                        (plist-get impact :discarded-turns)))
        (insert (format "Captured files restored: %d\n" (length plan)))
        (insert (format "Checkpoint coverage: %s\n"
                        (if gaps
                            (format "incomplete (%d known gap%s)"
                                    (length gaps)
                                    (if (= 1 (length gaps)) "" "s"))
                          "no known gaps")))
        (insert (format "External changes overwritten: %d\n"
                        (plist-get impact :external-overwrites)))
        (insert (format "Staged files left in the index: %d\n"
                        (length staged)))
        (insert (format "Child forks detached: %d\n"
                        (plist-get impact :detached-children)))
        (insert (format "Cleared live state: %s\n"
                        (if cleared
                            (string-join cleared ", ")
                          "none")))
        (insert "Redo: none\n\n")
        (when discarded-prompts
          (insert "Discarded session events:\n")
          (dolist (prompt discarded-prompts)
            (insert (format "  S%d T%d  %s\n"
                            (plist-get prompt :segment)
                            (plist-get prompt :turn)
                            (mevedel-session-rewind--prompt-label
                             prompt))))
          (insert "\n"))
        (dolist (gap gaps)
          (insert (format "gap       %s (%s)\n"
                          (plist-get gap :path)
                          (plist-get gap :reason))))
        (dolist (entry plan)
          (let ((start (point)))
            (insert (format "%-9s %s%s\n"
                            (plist-get entry :action)
                            (plist-get entry :path)
                            (if (member (plist-get entry :path) staged)
                                " (staged index retained)"
                              "")))
            (put-text-property start (point)
                               'mevedel-plan-entry entry))))
      (special-mode)
      (use-local-map mevedel-session-rewind--plan-buffer-map)
      (setq-local mevedel-session-rewind--plan-buffer-session session)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun mevedel-session-rewind-reduce-prompt-index
    (index picked-segment picked-cum-turn &optional before-turn)
  "Return a copy of INDEX trimmed to the picked turn.
Drops segments past PICKED-SEGMENT entirely.  In the picked segment,
keeps only prompts whose `:cum-turn' is `<=' PICKED-CUM-TURN, or all
prompts when PICKED-CUM-TURN is nil.  When BEFORE-TURN is non-nil, drops the
picked prompt too."
  (require 'cl-lib)
  (cl-loop for (seg . prompts) in index
           when (< seg picked-segment)
           collect (cons seg (copy-sequence prompts))
           when (= seg picked-segment)
           collect (cons
                    seg
                    (cl-remove-if-not
                     (lambda (prompt)
                       (let ((turn (plist-get prompt :cum-turn)))
                         (or (null picked-cum-turn)
                             (if before-turn
                                 (< turn picked-cum-turn)
                               (<= turn picked-cum-turn)))))
                     prompts))))

(defun mevedel-session-rewind-reduce-file-snapshots
    (snapshots picked-cum-turn &optional before-turn)
  "Return SNAPSHOTS trimmed at PICKED-CUM-TURN.
SNAPSHOTS is an alist keyed by cumulative turn number.  When
PICKED-CUM-TURN is nil, returns SNAPSHOTS unchanged.  When BEFORE-TURN is
non-nil, drops the picked checkpoint too."
  (require 'cl-lib)
  (if (null picked-cum-turn)
      snapshots
    (cl-remove-if-not
     (lambda (entry)
       (if before-turn
           (< (car entry) picked-cum-turn)
         (<= (car entry) picked-cum-turn)))
     snapshots)))

(defun mevedel-session-rewind-reduce-agent-transcripts
    (entries picked-cum-turn)
  "Return ENTRIES trimmed at PICKED-CUM-TURN.
Entries without an integer `:parent-turn' remain visible."
  (require 'cl-lib)
  (cl-remove-if
   (lambda (entry)
     (let ((parent-turn (plist-get (cdr entry) :parent-turn)))
       (and (integerp parent-turn) (> parent-turn picked-cum-turn))))
   entries))

(defun mevedel-session-rewind--rewind-candidate (session target)
  "Return SESSION state reduced in place semantics to TARGET."
  (require 'cl-lib)
  (require 'mevedel-session-persistence)
  (let* ((candidate (copy-sequence session))
         (target-turn (plist-get target :cum-turn))
         (turn (1- target-turn))
         (segment (plist-get target :segment)))
    (setf
     (mevedel-session-tasks candidate) nil
     (mevedel-session-task-status-notes candidate) nil
     (mevedel-session-last-task-write-turn candidate) nil
     (mevedel-session-touched-files candidate) (make-hash-table :test #'equal)
     (mevedel-session-turn-count candidate) turn
     (mevedel-session-pending-reminders candidate) nil
     (mevedel-session-specialist-nudge-state candidate) nil
     (mevedel-session-deferred-pending candidate) nil
     (mevedel-session-deferred-injected candidate) nil
     (mevedel-session-deferred-used candidate) nil
     (mevedel-session-deferred-expired candidate) nil
     (mevedel-session-messages candidate) nil
     (mevedel-session-agent-registry candidate) nil
     (mevedel-session-agent-reservations candidate) nil
     (mevedel-session-agent-root-activity candidate) 'idle
     (mevedel-session-agent-root-waiter candidate) nil
     (mevedel-session-pending-steering candidate) nil
     (mevedel-session-pending-follow-ups candidate) nil
     (mevedel-session-pending-input-next-id candidate) nil
     (mevedel-session-pending-input-paused candidate) nil
     (mevedel-session-pending-input-failure-paused candidate) nil
     (mevedel-session-dropped-file-grants candidate) nil
     (mevedel-session-active-dropped-file-grants candidate) nil
     (mevedel-session-mentions-shown candidate) (make-hash-table :test #'equal)
     (mevedel-session-workspace-instruction-hashes candidate) nil
     (mevedel-session-hook-log candidate) nil
     (mevedel-session-hook-log-pending candidate) nil
     (mevedel-session-repair-log candidate) nil
     (mevedel-session-repair-log-pending candidate) nil
     (mevedel-session-permission-log-pending candidate) nil
     (mevedel-session-telemetry-pending candidate) nil
     (mevedel-session-hook-context-pending candidate) nil
     (mevedel-session-execution-state candidate) nil
     (mevedel-session-current-segment candidate) segment
     (mevedel-session-updated-at candidate) (format-time-string "%FT%H-%M-%S")
     (mevedel-session-prompt-index candidate)
     (copy-tree
      (mevedel-session-rewind-reduce-prompt-index
       (mevedel-session-prompt-index session) segment target-turn t)
      t)
     (mevedel-session-file-snapshots candidate)
     (copy-tree
      (mevedel-session-rewind-reduce-file-snapshots
       (mevedel-session-file-snapshots session) target-turn t)
      t)
     (mevedel-session-invoked-skills candidate)
     (cl-remove-if
      (lambda (record)
        (> (or (mevedel-skill-invocation-record-turn record) 0) turn))
      (mevedel-session-invoked-skills session))
     (mevedel-session-permission-queue candidate) nil
     (mevedel-session-directive-planning candidate) nil
     (mevedel-session-pending-plan-approval candidate) nil
     (mevedel-session-plan-metadata candidate) nil
     (mevedel-session-goal candidate) nil)
    (setf (mevedel-session-agent-transcripts candidate)
          (mevedel-session-rewind-reduce-agent-transcripts
           (mevedel-session-agent-transcripts candidate) turn))
    candidate))

(defun mevedel-session-rewind-copy-session-state (from to)
  "Copy every cl-struct slot from session FROM into TO."
  (dotimes (index (length from))
    (aset to index (aref from index))))

(defun mevedel-session-rewind--copy-rewind-session-state (from to)
  "Copy Rewind state from FROM into TO without replacing durability runtime."
  (let ((pending (mevedel-session-pending-publication to))
        (publication (mevedel-session-publication to))
        (queue (mevedel-session-publication-queue to))
        (uncommitted
         (mevedel-session-publication-uncommitted-batches to))
        (active (mevedel-session-publication-active-p to))
        (lease (mevedel-session-lease to))
        (timer (mevedel-session-lease-renewal-timer to)))
    (mevedel-session-rewind-copy-session-state from to)
    (setf (mevedel-session-pending-publication to) pending
          (mevedel-session-publication to) publication
          (mevedel-session-publication-queue to) queue
          (mevedel-session-publication-uncommitted-batches to) uncommitted
          (mevedel-session-publication-active-p to) active
          (mevedel-session-lease to) lease
          (mevedel-session-lease-renewal-timer to) timer)))

(defun mevedel-session-rewind-materialize-publication
    (session publication staging-path)
  "Materialize SESSION's committed PUBLICATION below STAGING-PATH.

Only the publication's logical artifacts are copied.  Lease, publication,
recovery, and other control paths are never materialized."
  (require 'mevedel-session-artifacts)
  (unless publication
    (error "Portable project operation requires a committed session publication"))
  (dolist (entry (plist-get publication :artifacts))
    (let* ((logical (car entry))
           (destination (expand-file-name logical staging-path))
           (content
            (mevedel-session-artifacts-read-artifact session logical t)))
      (unless (file-in-directory-p destination staging-path)
        (error "Session artifact escapes staging: %s" logical))
      (make-directory (file-name-directory destination) t)
      (let ((coding-system-for-write 'no-conversion))
        (write-region content nil destination nil 'silent)))))

(defun mevedel-session-rewind--prune-remote-rewind-staging
    (candidate target staging-path)
  "Remove post-target artifacts from CANDIDATE's STAGING-PATH snapshot.
TARGET identifies the retained segment and turn."
  (let ((target-segment (plist-get target :segment))
        (target-turn (1- (plist-get target :cum-turn)))
        (agents
         (delq nil
               (mapcar
                (lambda (entry) (plist-get (cdr entry) :path))
                (mevedel-session-agent-transcripts candidate)))))
    (dolist (path (directory-files-recursively staging-path ".*"))
      (let ((logical (file-relative-name path staging-path)))
        (when
            (or
             (and (string-match
                   "\\`segment-\\([0-9]+\\)\\.chat\\.org\\'" logical)
                  (> (string-to-number (match-string 1 logical))
                     target-segment))
             (and (string-match
                   "\\`instructions/turn-\\([0-9]+\\)\\.el\\'" logical)
                  (> (string-to-number (match-string 1 logical))
                     target-turn))
             (and (string-prefix-p "agents/" logical)
                  (not (member logical agents)))
             (string-prefix-p "plans/" logical)
             (or (member logical '(".lock" ".lease" ".publications"
                                  ".recovery"))
                 (string-prefix-p ".recovery/" logical)))
          (delete-file path))))
    (let ((plans (file-name-concat staging-path "plans")))
      (when (file-directory-p plans)
        (delete-directory plans t)))))

(defun mevedel-session-rewind--stage-rewind
    (session candidate target staging-path staging-buffer
             &optional publication rollback-staging-path)
  "Stage CANDIDATE and TARGET from SESSION under STAGING-PATH.

When PUBLICATION is non-nil, materialize only its immutable logical artifacts;
portable lease and publication control directories are never copied."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-persistence)
  (if publication
      (progn
        (mevedel-session-rewind-materialize-publication
         session publication staging-path)
        (when rollback-staging-path
          (copy-directory staging-path rollback-staging-path nil t t)))
    (copy-directory (mevedel-session-save-path session) staging-path nil t t))
  (let ((source session))
    (when publication
      (setq source (copy-sequence session))
      (setf (mevedel-session-save-path source) staging-path))
    (mevedel-session-rewind-load-rewind-target
     source staging-buffer target t))
  (with-current-buffer staging-buffer
    (setq buffer-file-name
          (mevedel-session-artifacts-segment-path
           staging-path (plist-get target :segment)))
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (mevedel-session-artifacts-update-prompt-index
     candidate staging-buffer)
    (mevedel-session-persistence-write-current-buffer-atomically
     buffer-file-name)
    (set-buffer-modified-p nil))
  (cl-loop for segment from (1+ (plist-get target :segment))
           to (or (mevedel-session-current-segment session) 1)
           for path = (mevedel-session-artifacts-segment-path
                       staging-path segment)
           when (file-exists-p path)
           do (delete-file path))
  (let* ((instructions-dir
          (mevedel-session-artifacts-instructions-dir staging-path))
         (target-instructions
          (mevedel-session-artifacts-instructions-turn-path
           staging-path (1- (plist-get target :cum-turn))))
         (current-instructions
          (mevedel-session-artifacts-instructions-current-path
           staging-path)))
    (when (file-exists-p target-instructions)
      (copy-file target-instructions current-instructions t))
    (when (file-directory-p instructions-dir)
      (dolist (path (directory-files instructions-dir t
                                     "\\`turn-\\([0-9]+\\)\\.el\\'"))
        (let ((name (file-name-nondirectory path)))
          (when (and (string-match
                      "\\`turn-\\([0-9]+\\)\\.el\\'" name)
                     (> (string-to-number (match-string 1 name))
                        (1- (plist-get target :cum-turn))))
            (delete-file path))))))
  (when publication
    (mevedel-session-rewind--prune-remote-rewind-staging
     candidate target staging-path))
  (mevedel-session-codec-write
   (mevedel-session-artifacts-sidecar-path staging-path)
   (mevedel-session-artifacts-build-sidecar candidate staging-buffer))
  (mevedel-session-codec-deserialize
   (mevedel-session-codec-read
    (mevedel-session-artifacts-sidecar-path staging-path))
   (mevedel-session-workspace session)))

(defun mevedel-session-rewind--backup-restore-files (plan directory)
  "Copy current PLAN file state under DIRECTORY for transaction rollback."
  (cl-loop for entry in plan
           for path = (plist-get entry :path)
           for index from 1
           for backup = (file-name-concat directory (format "%06d" index))
           collect
           (if (file-exists-p path)
               (progn
                 (copy-file path backup t t t)
                 (list :path path :existed t :backup backup))
             (list :path path :existed nil))))

(defun mevedel-session-rewind--rollback-restore-files (backups)
  "Restore file BACKUPS captured for a failed Rewind.
Return descriptions of every artifact that could not be restored."
  (let (failures)
    (dolist (entry backups)
      (let ((path (plist-get entry :path)))
        (condition-case err
            (if (plist-get entry :existed)
                (progn
                  (make-directory (file-name-directory path) t)
                  (copy-file (plist-get entry :backup) path t t t))
              (when (file-exists-p path)
                (delete-file path)))
          (error
           (push (format "%s (%s)" path (error-message-string err))
                 failures)))))
    (nreverse failures)))

(defun mevedel-session-rewind-rewind-publication-artifacts
    (session buffer staging-path &optional state)
  "Return STAGING-PATH as SESSION replacement artifacts, sidecar last.

SESSION supplies the owned publication path.  BUFFER supplies live transcript
state.  STATE, when non-nil, supplies the logical sidecar state without
replacing SESSION's live lease runtime."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let* ((save-path (mevedel-session-save-path session))
         (sidecar-name "session.meta.el")
         artifacts)
    (dolist (path (sort (directory-files-recursively staging-path ".*")
                        #'string<))
      (let ((logical (file-relative-name path staging-path)))
        (when (and (not (equal logical sidecar-name))
                   (mevedel-session-publication-logical-path-p logical))
          (push
           (list :path (expand-file-name logical save-path)
                 :content (mevedel-session-artifacts-read-file-raw path))
           artifacts))))
    (append
     (nreverse artifacts)
     (list
      (list
       :path (mevedel-session-artifacts-sidecar-path save-path)
       :content
       (mevedel-session-artifacts-printed-value
        (mevedel-session-artifacts-build-sidecar
         (or state session) buffer))
       :commit-marker t
       :replace t)))))

(defun mevedel-session-rewind--install-rewind-buffer
    (buffer staging-buffer session target)
  "Install STAGING-BUFFER as BUFFER for rewound SESSION at TARGET."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-persistence)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (replace-buffer-contents staging-buffer)
      (setq buffer-file-name
            (mevedel-session-artifacts-segment-path
             (mevedel-session-save-path session)
             (plist-get target :segment))
            buffer-file-truename
            (file-truename buffer-file-name))
      (set-buffer-modified-p nil)
      (set-visited-file-modtime)))
  (mevedel-session-persistence-notify-session-event
   session 'reset-agent-ephemeral-state)
  (mevedel-session-persistence-notify-session-event
   session 'rerender))

(defun mevedel-session-rewind--commit-remote-rewind
    (session buffer target plan)
  "Commit portable project SESSION, BUFFER, TARGET, and file PLAN through one
head CAS."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let* ((workspace (mevedel-session-workspace session))
         (directives (copy-sequence
                      (mevedel-workspace-directives workspace)))
         (save-path (mevedel-session-save-path session))
         (publication
          (or (mevedel-session-publication session)
              (mevedel-session-publication-read save-path)
              (error "Portable project Rewind requires a committed publication")))
         (head-before (plist-get publication :head))
         (temporary-root (make-temp-file "mevedel-remote-rewind-" t))
         (staging-path (file-name-concat temporary-root "staging"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (candidate
          (mevedel-session-rewind--rewind-candidate session target))
         (staging-buffer
          (generate-new-buffer " *mevedel-remote-rewind-staging*"))
         file-backups
         project-restore-started
         publish-attempted
         committed
         post-commit-error
         operation-error
         rollback-failures)
    (unwind-protect
        (condition-case err
            (progn
              (with-current-buffer staging-buffer
                (funcall (buffer-local-value 'major-mode buffer)))
              (make-directory file-backup-dir t)
              ;; Materialization, project backup, recheck, and restore are one
              ;; synchronous target operation.  Timer renewal stays off-target
              ;; until the wrapper proves the same generation still owns it.
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (let ((current
                        (mevedel-session-publication-read
                         save-path)))
                   (unless (and current
                                (equal head-before
                                       (plist-get current :head)))
                     (user-error
                      "Session state changed before portable project Rewind; retry"))
                   (setq publication current)
                   (setf (mevedel-session-publication session) current))
                 (setq file-backups
                       (mevedel-session-rewind--backup-restore-files
                        plan file-backup-dir))
                 (mevedel-session-rewind--stage-rewind
                  session candidate target staging-path staging-buffer
                  publication)
                 (let ((rechecked
                        (mevedel-session-rewind-restore-plan
                         session (plist-get target :cum-turn) t)))
                   (unless
                       (equal
                        (sort (copy-sequence plan)
                              (lambda (a b)
                                (string< (plist-get a :path)
                                         (plist-get b :path))))
                        (sort (copy-sequence rechecked)
                              (lambda (a b)
                                (string< (plist-get a :path)
                                         (plist-get b :path)))))
                     (error
                      "Captured files changed after Rewind confirmation")))
                 (setq project-restore-started t)
                 (let ((result
                        (mevedel-session-rewind-execute-restore
                         session plan)))
                   (when (plist-get result :failed)
                     (error "File restore failed on %s: %s"
                            (plist-get result :failed)
                            (plist-get result :error))))))
              (setq publish-attempted t)
              (condition-case publish-error
                  (mevedel-session-publication-publish
                   session
                   (mevedel-session-rewind-rewind-publication-artifacts
                    session staging-buffer staging-path candidate))
                (error
                 ;; The captured head changes at marker CAS, before final lease
                 ;; normalization.  A changed head is already committed and
                 ;; must never be rolled back as though publication failed.
                 (if (not
                      (equal head-before
                             (plist-get (mevedel-session-publication session)
                                        :head)))
                     (setq committed t
                           post-commit-error publish-error)
                   (signal (car publish-error) (cdr publish-error)))))
              (unless committed
                (unless
                    (not
                     (equal head-before
                            (plist-get (mevedel-session-publication session)
                                       :head)))
                  (error "Portable project Rewind did not commit a publication head"))
                (setq committed t))
              ;; Only the successful head CAS installs logical session state.
              (mevedel-session-rewind--copy-rewind-session-state
               candidate session)
              (mevedel-session-rewind--install-rewind-buffer
               buffer staging-buffer session target)
              (condition-case instruction-error
                  (unless
                      (mevedel-session-artifacts-load-instructions
                       session buffer nil directives t)
                    (error "Published instruction snapshot is unavailable"))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but instructions did not refresh: %s"
                   (error-message-string instruction-error))
                  :warning)))
              (condition-case directive-error
                  (progn
                    (mevedel-workspace-rewind-directives
                     workspace (mevedel-session-session-id session)
                     (plist-get target :cum-turn))
                    (mevedel--restore-preserved-directives workspace))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but directives did not refresh: %s"
                   (error-message-string directive-error))
                  :warning)))
              (condition-case refresh-error
                  (mevedel-session-rewind--refresh-restored-buffers
                   plan (list :succeeded (length plan)))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but buffers did not refresh: %s"
                   (error-message-string refresh-error))
                  :warning))))
          (error (setq operation-error err)))
      (unless committed
        (when project-restore-started
          (condition-case rollback-error
              (setq rollback-failures
                    (mevedel-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       (mevedel-session-rewind--rollback-restore-files
                        file-backups))))
            (error
             (push
              (format "Project rollback authority (%s)"
                      (error-message-string rollback-error))
              rollback-failures))))
        (when (and publish-attempted (null rollback-failures))
          (condition-case discard-error
              (mevedel-session-publication-discard-rolled-back
               session)
            (error
             (push
              (format "Publication recovery cleanup (%s)"
                      (error-message-string discard-error))
              rollback-failures)))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (unless rollback-failures
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t))))
    (when rollback-failures
      (let ((reason
             (format
              "Portable project Rewind rollback incomplete: %s"
              (string-join (nreverse rollback-failures) ", "))))
        (condition-case nil
            (mevedel-session-recovery-record-failure
             session reason temporary-root)
          (error nil))
        (error "%s; recovery data: %s" reason temporary-root)))
    (when operation-error
      (if committed
          (error "Portable project Rewind committed, but local state failed: %s"
                 (error-message-string operation-error))
        (signal (car operation-error) (cdr operation-error))))
    (when post-commit-error
      (error "Portable project Rewind committed, but lease finalization failed: %s"
             (error-message-string post-commit-error)))
    t))

(defun mevedel-session-rewind--commit-rewind
    (session buffer target plan)
  "Commit SESSION, BUFFER, TARGET, and file PLAN as one recoverable Rewind."
  (require 'mevedel-session-codec)
  (if (mevedel-session-codec-portable-authority-p session)
      (mevedel-session-rewind--commit-remote-rewind
       session buffer target plan)
    (mevedel-session-rewind--commit-local-rewind
     session buffer target plan)))

(defun mevedel-session-rewind--commit-local-rewind
    (session buffer target plan)
  "Commit local SESSION, BUFFER, TARGET, and file PLAN as one Rewind."
  (require 'mevedel-directive)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-persistence)
  (let* ((workspace (mevedel-session-workspace session))
         (directives (copy-sequence
                      (mevedel-workspace-directives workspace)))
         (directive-state
          (mapcar
           (lambda (directive)
             (list directive
                   :anchor (copy-tree (mevedel-directive-anchor directive))
                   :state (mevedel-directive-state directive)
                   :subdirectives
                   (mevedel-directive-subdirectives directive)
                   :attempts (mevedel-directive-attempts directive)
                   :discussion (mevedel-directive-discussion directive)))
           directives))
         (save-path (mevedel-session-save-path session))
         (parent (file-name-directory (directory-file-name save-path)))
         (temporary-root
          (make-temp-file
           (expand-file-name ".mevedel-rewind-" parent) t))
         (staging-path (file-name-concat temporary-root "staging"))
         (rollback-path (file-name-concat temporary-root "rollback"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (original-state (copy-sequence session))
         (candidate
          (mevedel-session-rewind--rewind-candidate session target))
         (staging-buffer
          (generate-new-buffer " *mevedel-rewind-staging*"))
         (original-buffer
          (generate-new-buffer " *mevedel-rewind-original*"))
         (original-file-name (buffer-local-value 'buffer-file-name buffer))
         (original-file-truename
          (buffer-local-value 'buffer-file-truename buffer))
         (original-buffer-modified
          (with-current-buffer buffer (buffer-modified-p)))
         (original-point (with-current-buffer buffer (point)))
         (original-turn (mevedel-session-turn-count session))
         file-backups source-moved published session-installed
         file-restore-started buffer-install-started committed
         rollback-failures)
    (with-current-buffer original-buffer
      (insert-buffer-substring buffer))
    (unwind-protect
        (progn
          (with-current-buffer staging-buffer
            (funcall (buffer-local-value 'major-mode buffer)))
          (make-directory file-backup-dir t)
          (setq file-backups
                (mevedel-session-rewind--backup-restore-files
                 plan file-backup-dir))
          (mevedel-session-rewind--stage-rewind
           session candidate target staging-path staging-buffer)
          (let ((rechecked
                 (mevedel-session-rewind-restore-plan
                  session (plist-get target :cum-turn) t)))
            (unless (equal
                     (sort (copy-sequence plan)
                           (lambda (a b)
                             (string< (plist-get a :path)
                                      (plist-get b :path))))
                     (sort (copy-sequence rechecked)
                           (lambda (a b)
                             (string< (plist-get a :path)
                                      (plist-get b :path)))))
              (error "Captured files changed after Rewind confirmation")))
          (setq file-restore-started t)
          (let ((result
                 (mevedel-session-rewind-execute-restore session plan)))
            (when (plist-get result :failed)
              (error "File restore failed on %s: %s"
                     (plist-get result :failed)
                     (plist-get result :error))))
          (rename-file (directory-file-name save-path)
                       rollback-path)
          (setq source-moved t)
          (rename-file staging-path (directory-file-name save-path))
          (setq published t)
          (mevedel-session-rewind-copy-session-state candidate session)
          (setq session-installed t)
          (setq buffer-install-started t)
          (mevedel-session-rewind--install-rewind-buffer
           buffer staging-buffer session target)
          (unless
              (mevedel-session-artifacts-load-instructions
               session buffer (1- (plist-get target :cum-turn)) directives t)
            (error "Instruction restore failed during Rewind"))
          (mevedel-workspace-rewind-directives
           workspace (mevedel-session-session-id session)
           (plist-get target :cum-turn))
          (mevedel-session-artifacts-save-instructions
           session buffer t)
          (delete-directory rollback-path t)
          (setq source-moved nil
                committed t)
          (condition-case err
              (mevedel-session-rewind--refresh-restored-buffers
               plan (list :succeeded (length plan)))
            (error
             (display-warning
              'mevedel
              (format "Rewind committed, but buffers could not refresh: %s"
                      (error-message-string err)))))
          (condition-case err
              (progn
                (mevedel--restore-preserved-directives workspace)
                (mevedel-session-artifacts-save-instructions
                 session buffer t))
            (error
             (display-warning
              'mevedel
              (format "Rewind committed, but directives could not refresh: %s"
                      (error-message-string err)))))
          t)
      (unless committed
        (dolist (entry directive-state)
          (let ((directive (car entry)))
            (setf (mevedel-directive-anchor directive)
                  (copy-tree (plist-get (cdr entry) :anchor))
                  (mevedel-directive-state directive)
                  (plist-get (cdr entry) :state)
                  (mevedel-directive-subdirectives directive)
                  (plist-get (cdr entry) :subdirectives)
                  (mevedel-directive-attempts directive)
                  (plist-get (cdr entry) :attempts)
                  (mevedel-directive-discussion directive)
                  (plist-get (cdr entry) :discussion))))
        (mevedel-workspace-set-directives workspace directives)
        (when session-installed
          (mevedel-session-rewind-copy-session-state
           original-state session))
        (when published
          (condition-case err
              (progn
                (when (file-directory-p save-path)
                  (delete-directory save-path t))
                (setq published nil))
            (error
             (push (format "%s (%s)" save-path (error-message-string err))
                   rollback-failures))))
        (when source-moved
          (condition-case err
              (rename-file rollback-path (directory-file-name save-path))
            (error
             (push (format "%s (%s)" save-path (error-message-string err))
                   rollback-failures))))
        (when file-restore-started
          (setq rollback-failures
                (nconc
                 rollback-failures
                 (mevedel-session-rewind--rollback-restore-files
                  file-backups))))
        (when buffer-install-started
          (condition-case err
              (progn
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (setq buffer-file-name original-file-name
                          buffer-file-truename original-file-truename)
                    (set-visited-file-modtime)
                    (replace-buffer-contents original-buffer)
                    (set-buffer-modified-p original-buffer-modified)
                    (goto-char (min original-point (point-max)))))
                (mevedel-session-artifacts-load-instructions
                 session buffer original-turn directives t)
                (mevedel-session-persistence-notify-session-event
                 session 'rerender))
            (error
             (push (format "%s (%s)"
                           (buffer-name buffer)
                           (error-message-string err))
                   rollback-failures)))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (when (buffer-live-p original-buffer)
        (kill-buffer original-buffer))
      (if rollback-failures
          (let ((reason
                 (format
                  "Rewind rollback incomplete; inconsistent artifacts: %s"
                  (string-join (nreverse rollback-failures) ", "))))
            (when (mevedel-session-codec-portable-authority-p session)
              (require 'mevedel-session-durability)
              (require 'mevedel-session-recovery)
              (mevedel-session-recovery-record-failure
               session reason temporary-root))
            (error "%s; recovery data: %s" reason temporary-root))
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t))))))

(defun mevedel-session-rewind-rewind (buffer target)
  "Rewind BUFFER's session in place to stable assistant TARGET."
  (require 'mevedel-session-artifacts)
  (let ((session (buffer-local-value 'mevedel--session buffer)))
    (unless (and session (plist-get target :fork-point-id))
      (user-error "Rewind requires a settled assistant response"))
    (mevedel-session-artifacts-assert-mutation-authority session buffer)
    (setq target
          (mevedel-session-rewind-resolve-fork-target
           session target))
    (mevedel-session-rewind-assert-stable-source
     session buffer "rewinding")
    (let* ((turn (plist-get target :cum-turn))
           (plan (mevedel-session-rewind-restore-plan session turn t))
           (prepared
            (mevedel-session-rewind--prepare-buffers-for-restore
             session turn plan)))
      (unless (eq prepared :abort)
        (let ((impact
               (mevedel-session-rewind--rewind-impact
                session target prepared)))
          (if (mevedel-session-rewind--rewind-impact-empty-p impact)
              (message "Already at this state")
            (mevedel-session-rewind--render-rewind-impact session impact)
            (let ((confirmed
                   (yes-or-no-p
                    (format
                     "Rewind %s to S%d T%d (%d turns, %d files; no redo)? "
                     (mevedel-session-name session)
                     (plist-get target :segment)
                     (plist-get target :turn)
                     (plist-get impact :discarded-turns)
                     (length prepared)))))
              (when confirmed
                (mevedel-session-artifacts-assert-mutation-authority
                 session buffer)
                (mevedel-session-rewind--commit-rewind
                 session buffer target prepared))
              (when-let* ((impact-buffer
                           (get-buffer "*mevedel-rewind-impact*")))
                (quit-windows-on impact-buffer t)
                (when (buffer-live-p impact-buffer)
                  (kill-buffer impact-buffer)))
              (when confirmed
                (with-current-buffer buffer
                  (mevedel--run-session-start-hooks "rewind"))
                (message "mevedel: rewound %s to S%d T%d"
                         (mevedel-session-name session)
                         (plist-get target :segment)
                         (plist-get target :turn))
                t))))))))

(defun mevedel-session-rewind-rewind-checkpoint
    (workspace checkpoint &optional buffer)
  "Rewind WORKSPACE to CHECKPOINT, resuming its session when needed.
BUFFER is the already-live execution session when available."
  (require 'mevedel-session-persistence)
  (let ((session-id (plist-get checkpoint :session-id))
        (turn (plist-get checkpoint :turn)))
    (unless (and (stringp session-id) (natnump turn))
      (user-error "Malformed implementation checkpoint"))
    (unless buffer
      (let ((records (copy-sequence
                      (mevedel-workspace-directives workspace))))
        (mevedel--reset-instructions-preserving-directives workspace records)
        (unwind-protect
            (setq buffer
                  (mevedel-session-persistence-resume-id
                   workspace session-id))
          (mevedel--reset-instructions-preserving-directives workspace records)
          (mevedel--restore-preserved-directives workspace))))
    (unless buffer
      (user-error "Execution session is unavailable: %s" session-id))
    (let* ((session (buffer-local-value 'mevedel--session buffer))
           (target
            (and session
                 (cl-loop
                  for (_ . candidate) in
                  (mevedel-session-rewind--prompt-candidates session)
                  when (= turn (plist-get candidate :cum-turn))
                  return candidate))))
      (unless target
        (user-error "Implementation checkpoint is unavailable: turn %s" turn))
      (mevedel-session-rewind-rewind buffer target))))

;;;###autoload
(defun mevedel-rewind ()
  "Pick a settled assistant response and Rewind the current session to it."
  (interactive)
  (let* ((buffer
          (cond
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           ((and (boundp 'mevedel--session) mevedel--session)
            (current-buffer))
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session buffer)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (mevedel-session-rewind-assert-stable-source
     session buffer "rewinding")
    (let* ((candidates
            (mevedel-session-rewind--prompt-candidates session)))
      (unless candidates
        (user-error "Session has no recorded user prompts"))
      (let* ((lookup (make-hash-table :test #'equal)))
        (dolist (c candidates)
          (puthash (car c) (cdr c) lookup))
        (let* ((collection
                (mevedel-session-rewind--prompt-collection-fn
                 candidates lookup))
               (default (caar (last candidates)))
               (chosen  (completing-read
                         "Rewind to: " collection nil t
                         nil 'mevedel-session-rewind--prompt-history
                         default))
               (target (gethash chosen lookup)))
          (when target
            (mevedel-session-rewind-rewind buffer target)))))))


(provide 'mevedel-session-rewind)

;;; mevedel-session-rewind.el ends here
