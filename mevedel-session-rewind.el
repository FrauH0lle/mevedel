;;; mevedel-session-rewind.el --- Transactional session Rewind -*- lexical-binding: t -*-

;;; Commentary:

;; Owns restore plans and the complete transactional Rewind and Redo
;; lifecycles.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'mevedel-structs))

;; `backtrace'
(declare-function backtrace-get-frames "backtrace" (&optional base args))
(declare-function backtrace-to-string "backtrace" (&optional frames))
(autoload 'backtrace-get-frames "backtrace")
(autoload 'backtrace-to-string "backtrace")

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))
(autoload 'mevedel-agent-control-active-turn-p "mevedel-agent-control")

;; `mevedel-chat'
(declare-function mevedel--chat-buffer-disable-org-element-cache "mevedel-chat" nil)
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))
(autoload 'mevedel--run-session-start-hooks "mevedel-chat")

;; `mevedel-directive'
(declare-function mevedel-workspace-rewind-directives "mevedel-directive" (workspace session-id target-turn))
(declare-function mevedel-workspace-set-directives "mevedel-directive" (workspace directives))
(autoload 'mevedel-workspace-rewind-directives "mevedel-directive")
(autoload 'mevedel-workspace-set-directives "mevedel-directive")

;; `mevedel-execution'
(declare-function mevedel-execution-session-live-p "mevedel-execution" (session))
(autoload 'mevedel-execution-session-live-p "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-native-path "mevedel-execution-target" (target path))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")

;; `mevedel-persistence'
(declare-function mevedel--reset-instructions-preserving-directives "mevedel-persistence" (workspace directives))
(declare-function mevedel--restore-preserved-directives "mevedel-persistence" (workspace))
(autoload 'mevedel--reset-instructions-preserving-directives
  "mevedel-persistence")
(autoload 'mevedel--restore-preserved-directives "mevedel-persistence")

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
(declare-function mevedel-session-artifacts-replace-transcript-contents "mevedel-session-artifacts" (source))
(declare-function mevedel-session-artifacts-save-instructions "mevedel-session-artifacts" (session buffer &optional current-only))
(declare-function mevedel-session-artifacts-segment-path "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-sidecar-path "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-strip-gptel-config-properties "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-update-prompt-index "mevedel-session-artifacts" (session buffer))
(autoload 'mevedel-session-artifacts-assert-mutation-authority
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-backup-path "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-build-sidecar "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-collect-prompts
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-fork-point-spans
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-instructions-current-path
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-instructions-dir
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-instructions-turn-path
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-load-instructions
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-printed-value
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-artifact
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-file-raw
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-replace-transcript-contents
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-save-instructions
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-segment-path
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sidecar-path
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-stabilize-gptel-bounds
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-strip-gptel-config-properties
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-update-prompt-index
  "mevedel-session-artifacts")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-deserialize "mevedel-session-codec" (plist workspace))
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-write "mevedel-session-codec" (path plist))
(autoload 'mevedel-session-codec-deserialize "mevedel-session-codec")
(autoload 'mevedel-session-codec-portable-authority-p
  "mevedel-session-codec")
(autoload 'mevedel-session-codec-read "mevedel-session-codec")
(autoload 'mevedel-session-codec-write "mevedel-session-codec")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability" (session function))
(autoload 'mevedel-session-durability-call-with-reserved-lease
  "mevedel-session-durability")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-list-sessions "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-notify-session-event "mevedel-session-persistence" (session event &rest args))
(declare-function mevedel-session-persistence-parse-iso-time "mevedel-session-persistence" (str))
(declare-function mevedel-session-persistence-resume-id "mevedel-session-persistence" (workspace session-id))
(declare-function mevedel-session-persistence-write-current-buffer-atomically "mevedel-session-persistence" (path))
(autoload 'mevedel-session-persistence-list-sessions
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-notify-session-event
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-parse-iso-time
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-resume-id
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-write-current-buffer-atomically
  "mevedel-session-persistence")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-discard-rolled-back "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-generation-summaries
                  "mevedel-session-publication" (session-dir &optional limit))
(declare-function mevedel-session-publication-head-facts
                  "mevedel-session-publication" (session-dir head))
(declare-function mevedel-session-publication-logical-path-p "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir &optional head))
(declare-function mevedel-session-publication-settled-summary-p
                  "mevedel-session-publication" (summary))
(autoload 'mevedel-session-publication-discard-rolled-back
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-generation-summaries
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-head-facts
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-logical-path-p
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-publish "mevedel-session-publication")
(autoload 'mevedel-session-publication-read "mevedel-session-publication")

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery-record-failure "mevedel-session-recovery" (session reason recovery-path))
(autoload 'mevedel-session-recovery-record-failure
  "mevedel-session-recovery")

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
(declare-function mevedel-session-set-execution-target "mevedel-structs" (session target))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-task-status-notes "mevedel-structs" (cl-x))
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript--skip-leading-properties-drawer "mevedel-transcript" (pos))
(declare-function mevedel-transcript--skip-leading-summary-block "mevedel-transcript" (pos))
(autoload 'mevedel-transcript--skip-leading-properties-drawer "mevedel-transcript")
(autoload 'mevedel-transcript--skip-leading-summary-block "mevedel-transcript")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

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
    (session cum-turn plan &optional operation before-turn)
  "Prepare visiting buffers before restoring PLAN for SESSION at CUM-TURN.

If modified buffers visit affected files, prompt the user to save,
discard, or abort.  Returns the current restore plan, recomputing it
after saves.  Returns `:abort' when the restore should be abandoned.

OPERATION names the operation in the prompt and defaults to Rewind.
BEFORE-TURN selects the pre-turn checkpoint when the plan is recomputed,
as Rewind needs; a restore targets the state at CUM-TURN itself."
  (let ((current-plan plan)
        (operation (or operation "Rewind"))
        done)
    (while (not done)
      (let ((buffers
             (mevedel-session-rewind--modified-buffers-for-plan
              current-plan)))
        (if (null buffers)
            (setq done t)
          (pcase (read-char-choice
                  (format
                   "%s affects %d modified buffer%s (%s): [s]ave, [d]iscard, [a]bort? "
                   operation
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
                    session cum-turn before-turn)))
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
  (save-excursion
    (save-restriction
      (widen)
      (or (plist-get
           (nth turn-n
                (mevedel-session-artifacts-collect-prompts
                 (current-buffer)))
           :pos)
          (point-max)))))

(defun mevedel-session-rewind--strip-blank-transcript-properties ()
  "Drop transcript properties when no transcript text is retained.

Rewinding to before the first turn keeps only the leading property
drawer and blank lines.  A transcript property still on them belongs to
a discarded turn: it would be reinstalled into the live buffer,
re-persisted as bounds, hide the retained blank lines behind an
invisibility ellipsis, and project a turn with no content."
  (let ((scan-start
         (mevedel-transcript--skip-leading-summary-block
          (mevedel-transcript--skip-leading-properties-drawer (point-min)))))
    (when (string-blank-p
           (buffer-substring-no-properties scan-start (point-max)))
      (with-silent-modifications
        (set-text-properties scan-start (point-max) nil)))))

(defun mevedel-session-rewind-load-rewind-target
    (session buffer target &optional before-turn)
  "Load SESSION's TARGET transcript boundary into BUFFER without publishing it.
When BEFORE-TURN is non-nil, discard TARGET itself as well as later text."
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
            ;; Restore GPTEL_BOUNDS text properties so fork-point spans
            ;; below are locatable.  Only the transcript text matters in
            ;; this scratch buffer, so gptel's org config restore is
            ;; deliberately not involved.
            (mevedel-transcript-restore-properties)))
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
        (mevedel-session-artifacts-strip-gptel-config-properties)
        (mevedel-session-rewind--strip-blank-transcript-properties)
        (mevedel-session-artifacts-stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun mevedel-session-rewind-resolve-fork-target (session target)
  "Resolve TARGET's stable identity against SESSION's current index."
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
  (when (mevedel-execution-session-live-p session)
    (user-error "Stop live executions with /ps or /stop before %s" operation))
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

(defun mevedel-session-rewind--rewind-impact
    (session target file-plan &optional boundary)
  "Return the complete Rewind impact for SESSION, TARGET, and FILE-PLAN.

BOUNDARY is `after' when TARGET's own turn survives, or `before' -- the
default -- when it is discarded with everything later."
  (let* ((target-turn (plist-get target :cum-turn))
         (before-p (not (eq boundary 'after)))
         (surviving-turn (if before-p (1- target-turn) target-turn))
         (first-discarded (1+ surviving-turn)))
    (list
     :target target
     :boundary (if before-p 'before 'after)
     :file-plan file-plan
     :surviving-turns (max 0 surviving-turn)
     :discarded-turns
     (max 0 (1+ (- (or (mevedel-session-turn-count session) 0)
                       first-discarded)))
     :discarded-prompts
     (sort
      (cl-loop
       for (segment . prompts) in (mevedel-session-prompt-index session)
       append
       (cl-loop for prompt in prompts
                when (>= (or (plist-get prompt :cum-turn) 0) first-discarded)
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
        (insert (format "Rewind %s %s S%d T%d\n\n"
                        (mevedel-session-name session)
                        (if (eq (plist-get impact :boundary) 'after)
                            "keeping"
                          "before")
                        (plist-get target :segment)
                        (plist-get target :turn)))
        (insert (format "Turns kept: %d\n"
                        (plist-get impact :surviving-turns)))
        (insert (format "Turns discarded: %d\n"
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
        (insert (format "Redo: %s\n\n"
                        (mevedel-session-rewind--redo-availability session)))
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
  (cl-remove-if
   (lambda (entry)
     (let ((parent-turn (plist-get (cdr entry) :parent-turn)))
       (and (integerp parent-turn) (> parent-turn picked-cum-turn))))
   entries))

(defun mevedel-session-rewind--rewind-candidate (session target &optional boundary)
  "Return SESSION state reduced in place semantics to TARGET.

BOUNDARY is `after' to keep TARGET's turn as the last surviving one, or
`before' -- the default -- to discard TARGET's turn along with every
later turn.  This is the one place the boundary is decided: staging,
pruning, and the transcript cutoff all derive their own answers from the
candidate's surviving turn count."
  (let* ((candidate (copy-sequence session))
         (target-turn (plist-get target :cum-turn))
         (before-p (not (eq boundary 'after)))
         (turn (if before-p (1- target-turn) target-turn))
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
       (mevedel-session-prompt-index session) segment target-turn before-p)
      t)
     (mevedel-session-file-snapshots candidate)
     (copy-tree
      (mevedel-session-rewind-reduce-file-snapshots
       (mevedel-session-file-snapshots session) target-turn before-p)
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
recovery, and other control paths are never materialized.

STAGING-PATH is created when absent: `file-in-directory-p' answers nil for
a directory that does not exist, so the containment check below needs the
staging root present before the first artifact is written."
  (unless publication
    (error "Portable project operation requires a committed session publication"))
  (make-directory staging-path t)
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
        (target-turn (or (mevedel-session-turn-count candidate) 0))
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
  (if publication
      (progn
        (mevedel-session-rewind-materialize-publication
         session publication staging-path)
        (when rollback-staging-path
          (copy-directory staging-path rollback-staging-path nil t t)))
    (copy-directory (mevedel-session-save-path session) staging-path nil t t))
  (let ((source session)
        ;; The candidate already decided which turn survives, so the
        ;; transcript cutoff follows it instead of deciding again.
        (before-p (< (or (mevedel-session-turn-count candidate) 0)
                     (or (plist-get target :cum-turn) 0))))
    (when publication
      (setq source (copy-sequence session))
      (setf (mevedel-session-save-path source) staging-path))
    (mevedel-session-rewind-load-rewind-target
     source staging-buffer target before-p))
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
  (let* ((surviving-turn (or (mevedel-session-turn-count candidate) 0))
         (instructions-dir
          (mevedel-session-artifacts-instructions-dir staging-path))
         (target-instructions
          (mevedel-session-artifacts-instructions-turn-path
           staging-path surviving-turn))
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
                        surviving-turn))
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

(defun mevedel-session-rewind--failure-backtrace ()
  "Return a printable backtrace for the error currently being signaled.

Call this from a `handler-bind' handler, before the stack unwinds.  A
committed portable Rewind cannot be rolled back, so a local application
step that fails afterwards has to name its own frame."
  (ignore-errors
    (backtrace-to-string (backtrace-get-frames nil))))

(defun mevedel-session-rewind--install-rewind-buffer
    (buffer staging-buffer session target)
  "Install STAGING-BUFFER as BUFFER for rewound SESSION at TARGET."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; `replace-region-contents' runs `after-change-functions', where
      ;; org-fold's fragility check walks one character back from a
      ;; folded region.  A transcript whose folded drawer or block starts
      ;; at point-min makes that `backward-char' signal
      ;; `beginning-of-buffer' and abort an already committed Rewind, so
      ;; the wholesale replacement runs with change hooks inhibited.  The
      ;; rerender below rebuilds the display from the new text anyway.
      (let ((inhibit-modification-hooks t))
        (mevedel-session-artifacts-replace-transcript-contents staging-buffer))
      ;; `replace-region-contents' keeps the properties on text it did
      ;; not have to change, so the retained leading blank lines still
      ;; carry the discarded turns' transcript properties.
      (mevedel-session-rewind--strip-blank-transcript-properties)
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
    (session buffer target plan &optional boundary)
  "Commit portable project SESSION, BUFFER, TARGET, and file PLAN through one
head CAS."
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
          (mevedel-session-rewind--rewind-candidate
           session target boundary))
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
                         session (plist-get target :cum-turn)
                         (not (eq boundary 'after)))))
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
              ;; The head is committed and cannot be rolled back, so a
              ;; failed buffer install must not abandon the instruction,
              ;; directive, and buffer refreshes that follow it.
              (let (install-backtrace)
                (condition-case install-error
                    (handler-bind
                        ((error
                          (lambda (_error)
                            (setq install-backtrace
                                  (mevedel-session-rewind--failure-backtrace)))))
                      (mevedel-session-rewind--install-rewind-buffer
                       buffer staging-buffer session target))
                  (error
                   (display-warning
                    'mevedel
                    (concat
                     (format
                      "Portable project Rewind committed, but the transcript buffer did not install: %s"
                      (error-message-string install-error))
                     (and install-backtrace (concat "\n" install-backtrace)))
                    :warning))))
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
    (session buffer target plan &optional boundary)
  "Commit SESSION, BUFFER, TARGET, and file PLAN as one recoverable Rewind."
  (if (mevedel-session-codec-portable-authority-p session)
      (mevedel-session-rewind--commit-remote-rewind
       session buffer target plan boundary)
    (mevedel-session-rewind--commit-local-rewind
     session buffer target plan boundary)))

(defun mevedel-session-rewind--commit-local-rewind
    (session buffer target plan &optional boundary)
  "Commit local SESSION, BUFFER, TARGET, and file PLAN as one Rewind."
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
          (mevedel-session-rewind--rewind-candidate
           session target boundary))
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
                  session (plist-get target :cum-turn)
                  (not (eq boundary 'after)))))
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
                    (mevedel-session-artifacts-replace-transcript-contents
                     original-buffer)
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
              (mevedel-session-recovery-record-failure
               session reason temporary-root))
            (error "%s; recovery data: %s" reason temporary-root))
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t))))))

(defun mevedel-session-rewind--rewind-confirmation (session impact)
  "Return the Rewind confirmation for SESSION and IMPACT.

The prompt names the boundary and both sides of it, because naming only
the selected turn leaves the reader to guess whether it survives."
  (let ((target (plist-get impact :target))
        (plan (plist-get impact :file-plan)))
    (format "Rewind %s %s S%d T%d: keep %d turn%s, discard %d (%d file%s; %s)? "
            (mevedel-session-name session)
            (if (eq (plist-get impact :boundary) 'after) "keeping" "to before")
            (plist-get target :segment)
            (plist-get target :turn)
            (plist-get impact :surviving-turns)
            (if (= 1 (plist-get impact :surviving-turns)) "" "s")
            (plist-get impact :discarded-turns)
            (length plan)
            (if (= 1 (length plan)) "" "s")
            (mevedel-session-rewind--redo-availability session))))

(defun mevedel-session-rewind-rewind (buffer target &optional boundary)
  "Rewind BUFFER's session in place to stable assistant TARGET.

BOUNDARY says which side of TARGET's turn the transcript ends on.  With
`after', TARGET's turn is the last one kept, which is what pointing at a
response asks for.  With `before' -- the default -- TARGET's turn is
discarded with everything later, which is what picking one of the user's
own prompts asks for, since the point of naming a prompt is to return to
just before sending it."
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
           (before-p (not (eq boundary 'after)))
           (plan (mevedel-session-rewind-restore-plan session turn before-p))
           (prepared
            (mevedel-session-rewind--prepare-buffers-for-restore
             session turn plan "Rewind" before-p)))
      (unless (eq prepared :abort)
        (let ((impact
               (mevedel-session-rewind--rewind-impact
                session target prepared boundary)))
          (if (mevedel-session-rewind--rewind-impact-empty-p impact)
              (message "Already at this state")
            (mevedel-session-rewind--render-rewind-impact session impact)
            (let ((confirmed
                   (yes-or-no-p
                    (mevedel-session-rewind--rewind-confirmation
                     session impact))))
              (when confirmed
                (mevedel-session-artifacts-assert-mutation-authority
                 session buffer)
                (mevedel-session-rewind--commit-rewind
                 session buffer target prepared boundary))
              (when-let* ((impact-buffer
                           (get-buffer "*mevedel-rewind-impact*")))
                (quit-windows-on impact-buffer t)
                (when (buffer-live-p impact-buffer)
                  (kill-buffer impact-buffer)))
              (when confirmed
                (with-current-buffer buffer
                  (mevedel--run-session-start-hooks "rewind"))
                (message "mevedel: rewound %s %s S%d T%d, %d turn%s discarded"
                         (mevedel-session-name session)
                         (if (eq (plist-get impact :boundary) 'after)
                             "keeping"
                           "to before")
                         (plist-get target :segment)
                         (plist-get target :turn)
                         (plist-get impact :discarded-turns)
                         (if (= 1 (plist-get impact :discarded-turns)) "" "s"))
                t))))))))


;;
;;; Redo: restoring a published head

(defun mevedel-session-rewind-published-heads (session)
  "Return SESSION's restorable published heads, newest first.

One entry per distinct settled turn state rather than one per published
generation: a generation is written on every committed save, so a turn
that streamed for a minute leaves dozens of heads whose transcripts
differ only in how much of that turn had arrived.  Restoring to any of
them means restoring the same conversation, so the newest generation for
a given turn count and fork point stands for all of them.  The state the
session is already in is excluded for the same reason.

Each entry carries `:head', `:time', `:transcript-bytes', and the turn
facts from the published sidecar.  Only a portable session publishes
immutable heads, so a PID-lock session has none."
  (when (mevedel-session-codec-portable-authority-p session)
    (let* ((session-dir (mevedel-session-save-path session))
           (current
            (or (plist-get (mevedel-session-publication session) :head)
                (mevedel-session-durability-publication-head session-dir)))
           (seen (make-hash-table :test #'equal))
           entries)
      (when session-dir
        (when-let* ((facts (and current
                                (mevedel-session-publication-head-facts
                                 session-dir current)))
                    (turn-count (plist-get facts :turn-count)))
          (puthash (list turn-count (plist-get facts :fork-point-id))
                   t seen))
        (dolist (summary (mevedel-session-publication-generation-summaries
                          session-dir))
          (let* ((head (plist-get summary :head))
                 (key (and (mevedel-session-publication-settled-summary-p
                            summary)
                           (list (plist-get summary :turn-count)
                                 (plist-get summary :fork-point-id)))))
            ;; No key means the head has no settled turn state to offer:
            ;; either its sidecar could not be read or it captured a turn
            ;; mid-stream.
            (when (and key
                       (not (equal head current))
                       (not (gethash key seen)))
              (puthash key t seen)
              (push summary entries)))))
      ;; Ordered by the turn state each head restores, not by when it was
      ;; published: the reader is choosing a point in the conversation.
      (sort (nreverse entries)
            (lambda (left right)
              (let ((left-turn (or (plist-get left :turn-count) 0))
                    (right-turn (or (plist-get right :turn-count) 0)))
                (if (= left-turn right-turn)
                    (time-less-p (plist-get right :time)
                                 (plist-get left :time))
                  (> left-turn right-turn))))))))

(defun mevedel-session-rewind--load-restored-transcript
    (buffer staging-path segment)
  "Load STAGING-PATH's SEGMENT transcript into BUFFER."
  (let ((segment-path
         (mevedel-session-artifacts-segment-path staging-path segment)))
    (unless (file-regular-p segment-path)
      (error "Published head has no segment %d transcript" segment))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((buffer-file-name segment-path))
          (insert-file-contents segment-path)
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
              (mevedel--chat-buffer-disable-org-element-cache))
            (mevedel-transcript-restore-properties)))
        (mevedel-session-artifacts-strip-gptel-config-properties)
        (mevedel-session-artifacts-stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun mevedel-session-rewind--restore-file-plan (candidate)
  "Return the captured file-restore plan for restored CANDIDATE state.

CANDIDATE carries the published head's file snapshots and resolves its
backup bytes through that head's publication, so the plan describes the
captured working-tree state the restored turns owned."
  (mevedel-session-rewind-restore-plan
   candidate (or (mevedel-session-turn-count candidate) 0)))

(defun mevedel-session-rewind--restore-impact (candidate plan)
  "Return the impact of restoring CANDIDATE with file PLAN."
  (list :restored-turns (or (mevedel-session-turn-count candidate) 0)
        :file-plan plan
        :external-overwrites
        (cl-count 'overwrite plan
                  :key (lambda (entry) (plist-get entry :action)))))

(defun mevedel-session-rewind--sorted-file-plan (plan)
  "Return PLAN ordered by path for comparison."
  (sort (copy-sequence plan)
        (lambda (left right)
          (string< (plist-get left :path) (plist-get right :path)))))

(defun mevedel-session-rewind-restore-head
    (session buffer head &optional confirm-function)
  "Restore SESSION and BUFFER to published HEAD through one head CAS.

HEAD is an immutable published generation, as returned by
`mevedel-session-rewind-published-heads'.  Its conversation state --
transcript, sidecar, instruction snapshots, retained agent transcripts,
and persisted tool results -- is republished as a new head, so the
restore is itself an ordinary committed publication rather than a
rollback.  Captured working-tree files are restored from that head's
retained file history in the same transaction; coverage is limited to
what mevedel captured, so uncaptured filesystem effects may remain.

CONFIRM-FUNCTION receives the impact plist from
`mevedel-session-rewind--restore-impact' before anything is mutated and
must return non-nil for the restore to proceed.  Preparation, including
the modified-buffer prompt, happens before the session lease is
reserved; the reserved transaction then rechecks both the current head
and the file plan the caller confirmed."
  (unless (mevedel-session-codec-portable-authority-p session)
    (user-error "Only a portable project session publishes restorable heads"))
  (mevedel-session-rewind-assert-stable-source session buffer "restoring")
  (let* ((save-path (mevedel-session-save-path session))
         (publication
          (or (mevedel-session-publication-read save-path head)
              (user-error "Published head is unavailable: %s" head)))
         (head-before
          (plist-get (mevedel-session-publication session) :head))
         (temporary-root (make-temp-file "mevedel-session-restore-" t))
         (staging-path (file-name-concat temporary-root "staging"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (source (copy-sequence session))
         (staging-buffer (generate-new-buffer " *mevedel-restore-staging*"))
         (workspace (mevedel-session-workspace session))
         (directives (copy-sequence
                      (mevedel-workspace-directives workspace)))
         candidate plan file-backups file-restore-started committed
         rollback-failures)
    (setf (mevedel-session-publication source) publication)
    (unwind-protect
        (progn
          (with-current-buffer staging-buffer
            (funcall (buffer-local-value 'major-mode buffer)))
          ;; Preparation reads immutable published bytes only, so it runs
          ;; before the lease is reserved: the user is not holding session
          ;; authority while a prompt waits.
          (mevedel-session-rewind-materialize-publication
           source publication staging-path)
          (setq candidate
                (or (plist-get
                     (mevedel-session-codec-deserialize
                      (mevedel-session-codec-read
                       (mevedel-session-artifacts-sidecar-path staging-path))
                      workspace)
                     :session)
                    (error "Published head has no readable sidecar")))
          ;; The sidecar carries logical state only; where the session
          ;; lives and what it talks to belong to the live session, as
          ;; they do on resume.  Its publication is the head being
          ;; restored, so retained file history resolves against it.
          (setf (mevedel-session-save-path candidate) save-path
                (mevedel-session-working-directory candidate)
                (mevedel-session-working-directory session)
                (mevedel-session-publication candidate) publication)
          (mevedel-session-set-execution-target
           candidate (mevedel-session-execution-target session))
          (mevedel-session-rewind--load-restored-transcript
           staging-buffer staging-path
           (or (mevedel-session-current-segment candidate) 1))
          (setq plan (mevedel-session-rewind--restore-file-plan candidate))
          (setq plan
                (mevedel-session-rewind--prepare-buffers-for-restore
                 candidate (or (mevedel-session-turn-count candidate) 0)
                 plan "Restore" nil))
          (unless (eq plan :abort)
            (when (or (null confirm-function)
                      (funcall confirm-function
                               (mevedel-session-rewind--restore-impact
                                candidate plan)))
              (make-directory file-backup-dir t)
              ;; Recheck, file backup, and file restore are one
              ;; synchronous target operation under the reserved lease,
              ;; exactly as Rewind stages its own file restore.
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (let ((current
                        (mevedel-session-publication-read save-path)))
                   (unless (and current
                                (equal head-before
                                       (plist-get current :head)))
                     (user-error
                      "Session state changed before restore; retry"))
                   (setf (mevedel-session-publication session) current))
                 (unless (equal
                          (mevedel-session-rewind--sorted-file-plan plan)
                          (mevedel-session-rewind--sorted-file-plan
                           (mevedel-session-rewind--restore-file-plan
                            candidate)))
                   (error "Captured files changed after restore confirmation"))
                 (setq file-backups
                       (mevedel-session-rewind--backup-restore-files
                        plan file-backup-dir))
                 (when plan
                   (setq file-restore-started t)
                   (let ((result
                          (mevedel-session-rewind-execute-restore
                           candidate plan)))
                     (when (plist-get result :failed)
                       (error "File restore failed on %s: %s"
                              (plist-get result :failed)
                              (plist-get result :error)))))))
              (mevedel-session-publication-publish
               session
               (mevedel-session-rewind-rewind-publication-artifacts
                session staging-buffer staging-path candidate)
               t)
              (when (equal head-before
                           (plist-get (mevedel-session-publication session)
                                      :head))
                (error "Restore did not commit a publication head"))
              (setq committed t)
              ;; Only the successful head CAS installs logical session state.
              (mevedel-session-rewind--copy-rewind-session-state
               candidate session)
              (let (install-backtrace)
                (condition-case install-error
                    (handler-bind
                        ((error
                          (lambda (_error)
                            (setq install-backtrace
                                  (mevedel-session-rewind--failure-backtrace)))))
                      (mevedel-session-rewind--install-rewind-buffer
                       buffer staging-buffer session
                       (list :segment
                             (or (mevedel-session-current-segment session) 1))))
                  (error
                   (display-warning
                    'mevedel
                    (concat
                     (format
                      "Session restored, but the transcript buffer did not install: %s"
                      (error-message-string install-error))
                     (and install-backtrace (concat "\n" install-backtrace)))
                    :warning))))
              (condition-case instruction-error
                  (mevedel-session-artifacts-load-instructions
                   session buffer nil directives t)
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Session restored, but instructions did not refresh: %s"
                   (error-message-string instruction-error))
                  :warning)))
              (condition-case refresh-error
                  (progn
                    (when plan
                      (mevedel-session-rewind--refresh-restored-buffers
                       plan (list :succeeded (length plan))))
                    (mevedel-session-recovery-refresh-session-buffers session))
                (error
                 (display-warning
                  'mevedel
                  (format "Session restored, but buffers did not refresh: %s"
                          (error-message-string refresh-error))
                  :warning)))
              t)))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (unless committed
        (when file-restore-started
          (condition-case rollback-error
              (setq rollback-failures
                    (mevedel-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       (mevedel-session-rewind--rollback-restore-files
                        file-backups))))
            (error
             (push
              (format "Restore rollback authority (%s)"
                      (error-message-string rollback-error))
              rollback-failures))))
        (when (and (null rollback-failures)
                   (mevedel-session-pending-publication session))
          (condition-case nil
              (mevedel-session-publication-discard-rolled-back session)
            (error nil))))
      (unless rollback-failures
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t)))
      (when rollback-failures
        (let ((reason
               (format "Restore rollback incomplete: %s"
                       (string-join (nreverse rollback-failures) ", "))))
          (condition-case nil
              (mevedel-session-recovery-record-failure
               session reason temporary-root)
            (error nil))
          (error "%s; recovery data: %s" reason temporary-root))))))

(defun mevedel-session-rewind--restore-confirmation (session entry impact)
  "Return the restore confirmation for SESSION, head ENTRY, and IMPACT.

The prompt names what the restore returns -- turns and captured files --
and how many captured files it would overwrite after an external change,
because coverage is limited to what mevedel captured."
  (let* ((plan (plist-get impact :file-plan))
         (overwrites (plist-get impact :external-overwrites)))
    (format "Restore %s to %s (%d turns, %d captured file%s%s%s)? "
            (mevedel-session-name session)
            (mevedel-session-rewind--published-head-label entry)
            (plist-get impact :restored-turns)
            (length plan)
            (if (= 1 (length plan)) "" "s")
            (if (> overwrites 0)
                (format ", %d externally changed" overwrites)
              "")
            (if plan "; uncaptured effects remain" ""))))

(defun mevedel-session-rewind--published-head-label (entry)
  "Return the completion label for published head ENTRY.

  A head is named by the turn state it restores, so the label reads like
  the Rewind picker's: segment, turn, and that turn's prompt.  A settled
  head without a prompt falls back to its turn and transcript facts."
  (let ((turn (plist-get entry :turn))
        (segment (plist-get entry :segment))
        (prompt (plist-get entry :prompt))
        (stamp (format-time-string "%F %H:%M" (plist-get entry :time))))
    (cond
     ((and turn segment prompt)
      (format "S%d T%d  %s  %s" segment turn
              (mevedel-session-rewind--prompt-label prompt) stamp))
     ((and (null prompt) (equal 0 (plist-get entry :turn-count)))
      (format "%s  before the first turn  %s"
              (if segment (format "S%d" segment) "  ")
              stamp))
     (t
     (format "%s  %s  %s"
              stamp
              (let ((count (plist-get entry :turn-count)))
                (format "%d turn%s" count (if (= 1 count) "" "s")))
              (if-let* ((bytes (plist-get entry :transcript-bytes)))
                  (format "%s transcript" (file-size-human-readable bytes))
                "no transcript"))))))

;;;###autoload
(defun mevedel-redo ()
  "Restore this session's conversation to a previously published head.

This is the conversation-scope inverse of Rewind: every published head
stays immutable, so the state a Rewind moved away from can be
republished.  Captured working-tree files return with that state;
uncaptured filesystem effects and workspace directive records a Rewind
pruned do not come back."
  (interactive)
  (let* ((buffer (mevedel-session-rewind--command-buffer))
         (session (buffer-local-value 'mevedel--session buffer)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (unless (mevedel-session-codec-portable-authority-p session)
      (user-error
       "Only a portable project session publishes restorable heads"))
    (let ((heads (mevedel-session-rewind-published-heads session)))
      (unless heads
        (user-error "Session has no other published head"))
      (let* ((table (mapcar
                     (lambda (entry)
                       (cons (mevedel-session-rewind--published-head-label
                              entry)
                             entry))
                     heads))
             (choice
              (completing-read
               "Restore session to published head: "
               (mapcar #'car table) nil t nil nil (caar table)))
             (entry (cdr (assoc choice table))))
        (unless entry
          (user-error "No published head selected"))
        (when (mevedel-session-rewind-restore-head
               session buffer (plist-get entry :head)
               (lambda (impact)
                 (yes-or-no-p
                  (mevedel-session-rewind--restore-confirmation
                   session entry impact))))
          (with-current-buffer buffer
            (mevedel--run-session-start-hooks "restore"))
          (message "mevedel: restored %s to %s"
                   (mevedel-session-name session)
                   (plist-get entry :head))
          t)))))

(defun mevedel-session-rewind-rewind-checkpoint
    (workspace checkpoint &optional buffer)
  "Rewind WORKSPACE to CHECKPOINT, resuming its session when needed.
BUFFER is the already-live execution session when available."
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
      ;; Rewinding before an implementation discards that attempt, so the
      ;; checkpoint's own turn goes with it.
      (mevedel-session-rewind-rewind buffer target 'before))))

;;;###autoload
(defun mevedel-session-rewind--redo-availability (session)
  "Return how Rewind describes what SESSION can restore afterwards.

A portable session's superseded head stays published, so its
conversation and captured files can be restored with `mevedel-redo'.
The recorded mode is read directly: naming what Rewind offers must not
be the step that rejects a session."
  (if (eq (mevedel-session-authority-mode session) 'portable)
      "conversation and captured-file redo"
    "no redo"))

(defun mevedel-session-rewind--command-buffer ()
  "Return the session data buffer the current buffer commands."
  (cond
   ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
         (buffer-live-p mevedel--data-buffer))
    mevedel--data-buffer)
   ((and (boundp 'mevedel--session) mevedel--session)
    (current-buffer))
   (t (user-error "Not in a mevedel chat or view buffer"))))

(defun mevedel-rewind ()
  "Pick one of your prompts and Rewind the session to just before it.

The prompt you pick is discarded with everything after it, so the
session returns to the moment before you sent it -- which is what makes
re-asking it possible.  To keep a turn and discard only what follows,
Rewind from the view with point on that turn's response."
  (interactive)
  (let* ((buffer (mevedel-session-rewind--command-buffer))
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
                         "Rewind to before prompt: " collection nil t
                         nil 'mevedel-session-rewind--prompt-history
                         default))
               (target (gethash chosen lookup)))
          (when target
            (mevedel-session-rewind-rewind buffer target 'before)))))))


(provide 'mevedel-session-rewind)

;;; mevedel-session-rewind.el ends here
