;;; niwa-session-rewind.el --- Transactional session Rewind -*- lexical-binding: t -*-

;;; Commentary:

;; Owns restore plans and the complete transactional Rewind and Redo
;; lifecycles.

;;; Code:

(require 'niwa-engine)
(require 'niwa-change)

(require 'niwa-utilities)
(require 'niwa-report)

(require 'cl-lib)

(eval-when-compile
  (require 'niwa-agents)
  (require 'niwa-structs))

;; `backtrace'
(declare-function backtrace-get-frames "backtrace" (&optional base args))
(declare-function backtrace-to-string "backtrace" (&optional frames))
(autoload 'backtrace-get-frames "backtrace")
(autoload 'backtrace-to-string "backtrace")

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p "niwa-agent-control" (session))
(autoload 'niwa-agent-control-active-turn-p "niwa-agent-control")

;; `niwa-chat'
(declare-function niwa--chat-buffer-disable-org-element-cache "niwa-chat" nil)
(declare-function niwa--run-session-start-hooks "niwa-chat" (source))
(autoload 'niwa--run-session-start-hooks "niwa-chat")

;; `niwa-claude-code-fork'
(declare-function niwa-claude-code-fork-histories "niwa-claude-code-fork" (histories complete &optional session))
(autoload 'niwa-claude-code-fork-histories "niwa-claude-code-fork")

;; `niwa-execution'
(declare-function niwa-execution-session-live-p "niwa-execution" (session))
(declare-function niwa-execution-stop-services "niwa-execution" (session))
(autoload 'niwa-execution-session-live-p "niwa-execution")
(autoload 'niwa-execution-stop-services "niwa-execution")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-native-path "niwa-execution-target" (target path))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")

;; `niwa-persistence'
(declare-function niwa-instruction-store-rewind-begin "niwa-persistence" (session surviving-turn prune-from))
(declare-function niwa-instruction-store-rewind-end "niwa-persistence" (session committed))
(declare-function niwa-instruction-store-sync "niwa-persistence" (workspace))
(autoload 'niwa-instruction-store-rewind-begin "niwa-persistence")
(autoload 'niwa-instruction-store-rewind-end "niwa-persistence")
(autoload 'niwa-instruction-store-sync "niwa-persistence")
(defvar niwa-instruction-store--inhibit)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--live-sidecar "niwa-session-artifacts" (session &optional data-buffer))
(declare-function niwa-session-artifacts-assert-mutation-authority "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-collect-prompts "niwa-session-artifacts" (buffer &optional preview-length))
(declare-function niwa-session-artifacts-fork-point-spans "niwa-session-artifacts" (buffer))
(declare-function niwa-session-artifacts-load-instructions "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-printed-value "niwa-session-artifacts" (value))
(declare-function niwa-session-artifacts-read-artifact "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-read-backup "niwa-session-artifacts" (session backup-name))
(declare-function niwa-session-artifacts-read-file-raw "niwa-session-artifacts" (path))
(declare-function niwa-session-artifacts-replace-transcript-contents "niwa-session-artifacts" (source))
(declare-function niwa-session-artifacts-segment-path "niwa-session-artifacts" (save-path n))
(declare-function niwa-session-artifacts-sidecar-path "niwa-session-artifacts" (save-path))
(declare-function niwa-session-artifacts-stabilize-gptel-bounds "niwa-session-artifacts" nil)
(declare-function niwa-session-artifacts-strip-gptel-config-properties "niwa-session-artifacts" nil)
(declare-function niwa-session-artifacts-update-prompt-index "niwa-session-artifacts" (session buffer))
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-backup "niwa-session-artifacts")
(autoload 'niwa-session-artifacts--live-sidecar "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-collect-prompts
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-fork-point-spans
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-load-instructions
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-printed-value
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-file-raw
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-replace-transcript-contents
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-path
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-sidecar-path
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-stabilize-gptel-bounds
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-strip-gptel-config-properties
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-update-prompt-index
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-deserialize "niwa-session-codec" (plist workspace))
(declare-function niwa-session-codec-portable-authority-p "niwa-session-codec" (session))
(declare-function niwa-session-codec-read "niwa-session-codec" (path))
(declare-function niwa-session-codec-write "niwa-session-codec" (path plist))
(autoload 'niwa-session-codec-deserialize "niwa-session-codec")
(autoload 'niwa-session-codec-portable-authority-p
  "niwa-session-codec")
(autoload 'niwa-session-codec-read "niwa-session-codec")
(autoload 'niwa-session-codec-write "niwa-session-codec")

;; `niwa-session-durability'
(declare-function niwa-session-durability-call-with-reserved-lease "niwa-session-durability" (session function))
(autoload 'niwa-session-durability-call-with-reserved-lease
  "niwa-session-durability")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-list-sessions "niwa-session-persistence" (workspace &optional cached))
(declare-function niwa-session-persistence-notify-session-event "niwa-session-persistence" (session event &rest args))
(declare-function niwa-session-persistence-parse-iso-time "niwa-session-persistence" (str))
(declare-function niwa-session-persistence-resume-id "niwa-session-persistence" (workspace session-id))
(declare-function niwa-session-persistence-write-current-buffer-atomically "niwa-session-persistence" (path))
(autoload 'niwa-session-persistence-list-sessions
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-notify-session-event
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-parse-iso-time
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-resume-id
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-current-buffer-atomically
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication--files-sha256
                  "niwa-session-publication" (paths))
(declare-function niwa-session-publication-discard-rolled-back "niwa-session-publication" (session))
(declare-function niwa-session-publication-generation-summaries
                  "niwa-session-publication" (session-dir &optional limit))
(declare-function niwa-session-publication-head-facts
                  "niwa-session-publication" (session-dir head))
(declare-function niwa-session-publication-logical-path-p "niwa-session-publication" (path))
(declare-function niwa-session-publication-publish "niwa-session-publication" (session artifacts &optional require-commit))
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(declare-function niwa-session-publication-settled-summary-p
                  "niwa-session-publication" (summary))
(autoload 'niwa-session-publication-discard-rolled-back
  "niwa-session-publication")
(autoload 'niwa-session-publication-generation-summaries
  "niwa-session-publication")
(autoload 'niwa-session-publication-head-facts
  "niwa-session-publication")
(autoload 'niwa-session-publication-logical-path-p
  "niwa-session-publication")
(autoload 'niwa-session-publication-publish "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")

;; `niwa-session-recovery'
(declare-function niwa-session-recovery-record-failure "niwa-session-recovery" (session reason recovery-path))
(declare-function niwa-session-recovery-refresh-session-buffers
                  "niwa-session-recovery" (session))
(autoload 'niwa-session-recovery-record-failure
  "niwa-session-recovery")
(autoload 'niwa-session-recovery-refresh-session-buffers
  "niwa-session-recovery")

;; `niwa-structs'
(declare-function niwa-goal-status "niwa-structs" (cl-x))
(declare-function niwa-session-chapters "niwa-structs" (cl-x))
(declare-function niwa-session-current-segment "niwa-structs" (cl-x))
(declare-function niwa-session-file-snapshots "niwa-structs" (cl-x))
(declare-function niwa-session-untracked-effects "niwa-structs" (cl-x))
(declare-function niwa-session-goal "niwa-structs" (cl-x))
(declare-function niwa-session-lease "niwa-structs" (cl-x))
(declare-function niwa-session-name "niwa-structs" (cl-x))
(declare-function niwa-session-pending-input-p "niwa-structs" (session))
(declare-function niwa-session-pending-plan-approval "niwa-structs" (cl-x))
(declare-function niwa-session-pending-publication "niwa-structs" (cl-x))
(declare-function niwa-session-permission-log-pending "niwa-structs" (cl-x))
(declare-function niwa-session-permission-queue "niwa-structs" (cl-x))
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x))
(declare-function niwa-session-publication "niwa-structs" (cl-x))
(declare-function niwa-session-publication-active-p "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x))
(declare-function niwa-session-set-execution-target "niwa-structs" (session target))
(declare-function niwa-session-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-task-status-notes "niwa-structs" (cl-x))
(declare-function niwa-session-tasks "niwa-structs" (cl-x))
(declare-function niwa-session-turn-count "niwa-structs" (cl-x))
(declare-function niwa-session-updated-at "niwa-structs" (cl-x))
(declare-function niwa-session-workspace "niwa-structs" (cl-x))
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-transcript'
(declare-function niwa-transcript--skip-leading-properties-drawer "niwa-transcript" (pos))
(declare-function niwa-transcript--skip-leading-summary-block "niwa-transcript" (pos))
(autoload 'niwa-transcript--skip-leading-properties-drawer "niwa-transcript")
(autoload 'niwa-transcript--skip-leading-summary-block "niwa-transcript")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa-session-artifacts-backup-cache)

;;
;;; File restore plan

(defun niwa-session-rewind--latest-snapshot-entry (session path)
  "Return the highest-version snapshot plist for PATH in SESSION, or nil."
  (let ((best nil) (best-version 0))
    (dolist (turn-entry (niwa-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  ((not (plist-get (cdr entry) :gap)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best-version)
          (setq best-version v
                best          (cdr entry)))))))

(defun niwa-session-rewind-state-at-turn
    (session cum-turn &optional before-turn)
  "Return SESSION tracked-file state at CUM-TURN.

For each path that ever appeared in SESSION's `:file-snapshots',
picks the latest checkpoint through CUM-TURN.  When BEFORE-TURN is non-nil,
picks its earliest checkpoint in the discarded suffix instead."
  (let ((state (make-hash-table :test #'equal)))
    (dolist
        (turn-entry
         (sort (copy-sequence (niwa-session-file-snapshots session))
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

(defun niwa-session-rewind--plan-action
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
              (niwa-session-artifacts-read-backup
               session target-backup-name))
             (current-content
              (niwa-session-artifacts-read-file-raw path)))
        (if (string-equal current-content target-content)
            (list :action 'noop :path path)
          ;; Differs from target.  Check vs LATEST snapshot to detect
          ;; external changes since.
          (let* ((latest (niwa-session-rewind--latest-snapshot-entry
                          session path))
                 (latest-name (and latest (plist-get latest :backup-name)))
                 (latest-content
                  (and latest-name
                       (niwa-session-artifacts-read-backup
                        session latest-name)))
                 (diverged (not (and latest-content
                                     (string-equal current-content
                                                   latest-content)))))
            (list :action (if diverged 'overwrite 'restore)
                  :path path
                  :backup-name target-backup-name
                  :diverged diverged))))))))

(defun niwa-session-rewind-restore-plan
    (session cum-turn &optional before-turn)
  "Compute SESSION's captured file-restore plan at CUM-TURN.

Returns a list of plan-entry plists (see
`niwa-session-rewind--plan-action').  An empty list means
nothing to do.  When BEFORE-TURN is non-nil, target the pre-turn checkpoint."
  (let ((target-state
         (niwa-session-rewind-state-at-turn
          session cum-turn before-turn))
        (plan nil))
    (dolist (entry target-state)
      (unless (plist-get (cdr entry) :gap)
        (push (niwa-session-rewind--plan-action
               session (car entry) (cdr entry) before-turn)
              plan)))
    (cl-remove-if
     (lambda (e) (eq 'noop (plist-get e :action)))
     (nreverse plan))))

(defun niwa-session-rewind--file-state (path)
  "Read PATH's current existence and literal bytes for checked Rewind."
  (let ((exists (file-exists-p path)))
    (list :path path :exists exists
          :bytes (and exists (niwa-session-artifacts-read-file-raw path)))))

(defun niwa-session-rewind--recorded-state (session path name)
  "Read SESSION's recorded PATH state from backup NAME, or absence."
  (list :path path :exists (and name t)
        :bytes (and name (niwa-session-artifacts-read-backup session name))))

(defvar niwa-session-rewind--merge-resolutions nil
  "Dynamically scoped, checked resolutions while resuming a Rewind merge.")

(autoload 'niwa-session-rewind-merge-open "niwa-session-rewind-merge")

(defun niwa-session-rewind--revert-plan (session cum-turn before-turn &optional resolutions)
  "Prepare reversals of SESSION's discarded changes at CUM-TURN.
BEFORE-TURN includes that turn.  Reverse newest first in memory, preserving
unrelated current edits.  Return ordinary restore entries with literal
`:bytes' and a checked `:before' state; never write during preparation.
RESOLUTIONS supplies checked intermediate states from the merge editor."
  (let ((states (make-hash-table :test #'equal))
        (originals (make-hash-table :test #'equal))
        (conflicts (make-hash-table :test #'equal))
        plan)
    (dolist (turn (sort (copy-sequence (niwa-session-file-snapshots session))
                       (lambda (a b) (> (car a) (car b)))))
      (when (>= (car turn) (if before-turn cum-turn (1+ cum-turn)))
        (dolist (entry (cdr turn))
          (unless (or (plist-get (cdr entry) :gap)
                      (gethash (car entry) conflicts))
            (let* ((path (car entry))
                   (current (or (gethash path states)
                                (let ((state (niwa-session-rewind--file-state path)))
                                  (puthash path state originals)
                                  state)))
                   (before (niwa-session-rewind--recorded-state
                            session path (plist-get (cdr entry) :pre-backup-name)))
                   (after (niwa-session-rewind--recorded-state
                           session path (plist-get (cdr entry) :backup-name)))
                   (resolution (cdr (assoc (cons path (car turn)) resolutions)))
                   (result
                    (cond
                     (resolution
                      (unless (niwa-change-same-p current (plist-get resolution :before))
                        (user-error "Source changed since Rewind merge: %s" path))
                      (list :after (plist-get resolution :after)))
                     ((niwa-change-same-p before current) (list :after current))
                     (t (niwa-change-prepare before after current t)))))
              (if-let* ((found (plist-get result :conflicts)))
                  (puthash path
                           (list :path path :action 'conflict
                                 :turn (car turn) :conflicts found
                                 :current current :base after :proposed before)
                           conflicts)
                (puthash path (plist-get result :after) states)))))))
    (maphash
     (lambda (path state)
       (let ((original (gethash path originals)))
         (unless (or (gethash path conflicts)
                     (niwa-change-same-p original state))
           (push (list :path path
                       :action (cond ((not (plist-get state :exists)) 'delete)
                                     ((not (plist-get original :exists)) 'create)
                                     (t 'restore))
                       :before original :bytes (plist-get state :bytes))
                 plan))))
     states)
    (maphash (lambda (_path entry) (push entry plan)) conflicts)
    (niwa-session-rewind--sorted-file-plan plan)))

(defun niwa-session-rewind--checkpoint-gaps (session cum-turn)
  "Return SESSION gaps from CUM-TURN onwards, grouping tool effects by tool."
  (let (tools)
    (dolist (turn (niwa-session-untracked-effects session))
      (when (>= (car turn) cum-turn)
        (dolist (effect (cdr turn))
          (let ((entry (assoc (car effect) tools)))
            (unless entry
              (push (setq entry (list (car effect) nil nil)) tools))
            (cl-pushnew (car turn) (nth 1 entry))
            (cl-pushnew (cdr effect) (nth 2 entry) :test #'equal)))))
    (append
     (cl-loop for (turn . entries) in (niwa-session-file-snapshots session)
              when (>= turn cum-turn)
              append (cl-loop for (path . checkpoint) in entries
                              when (plist-get checkpoint :gap)
                              collect (list :path path :turn turn
                                            :reason (plist-get checkpoint :gap))))
     (mapcar (lambda (entry)
               (let ((count (length (nth 1 entry))))
                 (list :path (car entry)
                       :reason (format "%d turn%s: %s" count (if (= count 1) "" "s")
                                       (string-join (nreverse (nth 2 entry)) "; ")))))
             (nreverse tools)))))

(defvar-local niwa-session-rewind--plan-buffer-session nil
  "Buffer-local session for the `*niwa-rewind-impact*' buffer.
Consumed by `niwa-session-rewind--plan-row-diff' so `d' on a
plan row can resolve the backup file.")

(defvar niwa-session-rewind--plan-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map niwa-report-mode-map)
    (define-key map (kbd "d")
      #'niwa-session-rewind--plan-row-diff)
    map)
  "Keymap for the `*niwa-rewind-impact*' buffer.
Adds `d' -- show diff between current file and target snapshot.")

(defun niwa-session-rewind--plan-row-diff ()
  "Show a diff between the current file and the restore target backup.
Invoked from `*niwa-rewind-impact*' via the `d' binding.  The row
at point identifies the path; its `niwa-plan-entry' text property
carries the plan-entry plist."
  (interactive)
  (let* ((entry
          (get-text-property (line-beginning-position) 'niwa-plan-entry))
         (session niwa-session-rewind--plan-buffer-session))
    (unless entry
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "No restore plan row at point"))
    (unless session
      (niwa-diagnostic-user-error 'unavailable "Plan buffer has no associated session"))
    (let* ((path (plist-get entry :path))
           (action (plist-get entry :action))
           (backup-name (plist-get entry :backup-name)))
      (pcase action
        ('conflict
         (niwa-diagnostic-user-error
          (niwa-diagnostic-scope session)
          "Rewind conflict; cancel or force snapshot restore"))
        ('delete
         (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Row is a delete action; nothing to diff against"))
        ('noop
         (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Row is a noop; nothing to diff"))
        (_
         (let ((backup-path (make-temp-file "niwa-rewind-backup-")))
           (unwind-protect
               (progn
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert
                    (if (plist-member entry :bytes)
                        (plist-get entry :bytes)
                      (niwa-session-artifacts-read-backup
                       session backup-name)))
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region nil nil backup-path nil 'silent)))
                 (diff (or (and (file-exists-p path) path) "/dev/null")
                       backup-path nil 'no-async))
             (delete-file backup-path))))))))

(defvar niwa-session-rewind--written-states nil
  "Cell of attempted after-states, retained across nonlocal exits.")

(defun niwa-session-rewind--apply-restore-action (session entry)
  "Apply one restore ENTRY (plan-entry plist) for SESSION."
  (let* ((path        (plist-get entry :path))
         (action      (plist-get entry :action))
         (backup-name (plist-get entry :backup-name)))
    (when (and (plist-member entry :before)
               (not (niwa-change-same-p
                     (plist-get entry :before)
                     (niwa-session-rewind--file-state path))))
      (error "Captured file changed before Rewind write: %s" path))
    (pcase action
      ('conflict (error "Unresolved Rewind conflict: %s" path))
      ('noop nil)
      ('delete
       (when niwa-session-rewind--written-states
         (push (list :path path :exists nil :bytes nil)
               (car niwa-session-rewind--written-states)))
       (when (file-exists-p path)
         (delete-file path)))
      ((or 'create 'restore 'overwrite)
       (let ((content
              (if (plist-member entry :bytes)
                  (plist-get entry :bytes)
                (niwa-session-artifacts-read-backup
                 session backup-name))))
         (let ((dir (file-name-directory path)))
           (when (and dir (not (file-directory-p dir)))
             (make-directory dir t)))
         (when niwa-session-rewind--written-states
           (push (list :path path :exists t :bytes content)
                 (car niwa-session-rewind--written-states)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region content nil path nil 'silent)))))))

(defun niwa-session-rewind-execute-restore (session plan)
  "Execute restore PLAN for SESSION, stopping on the first failure.

Returns a plist describing the outcome:
  (:succeeded N :failed PATH-OR-NIL :error STR-OR-NIL :total N)

Successful actions are applied in order; on error, remaining plan
entries are not attempted.  The user-visible report goes to
`*niwa-restore-results*'."
  (let ((succeeded 0) (failed nil) (err-str nil)
        (total (length plan)))
    (catch 'failed
      (dolist (entry plan)
        (condition-case e
            (progn
              (niwa-session-rewind--apply-restore-action session entry)
              (cl-incf succeeded))
          (error
           (setq failed   (plist-get entry :path)
                 err-str  (error-message-string e))
           (throw 'failed nil)))))
    (with-current-buffer (get-buffer-create "*niwa-restore-results*")
      (niwa-report-render
       (list :title "Restore results" :identity session
             :sections
             (list (list :id 'result :title (if failed "Restore failed" "Restored files")
                         :body (concat (format "Restore results: %d/%d files done\n" succeeded total)
                                       (when failed (format "Failed on %s: %s\n" failed err-str))))))))
    (list :succeeded succeeded :failed failed
          :error err-str :total total)))

(defun niwa-session-rewind--modified-buffers-for-plan (plan)
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

(defun niwa-session-rewind--prepare-buffers-for-restore
    (session cum-turn plan &optional operation before-turn replan)
  "Prepare visiting buffers before restoring PLAN for SESSION at CUM-TURN.

If modified buffers visit affected files, prompt the user to save,
discard, or abort.  Returns the current restore plan, recomputing it
after saves.  Returns `:abort' when the restore should be abandoned.

OPERATION names the operation in the prompt and defaults to Rewind.
BEFORE-TURN selects the pre-turn checkpoint when the plan is recomputed,
as Rewind needs; a restore targets the state at CUM-TURN itself.
REPLAN, when non-nil, recomputes the plan after saves instead."
  (let ((current-plan plan)
        (operation (or operation "Rewind"))
        done)
    (while (not done)
      (let ((buffers
             (niwa-session-rewind--modified-buffers-for-plan
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
                   (if replan (funcall replan)
                     (niwa-session-rewind-restore-plan
                      session cum-turn before-turn))))
            (?d
             (dolist (buf buffers)
               (with-current-buffer buf
                 (revert-buffer t t t)))
             (setq done t))
            (?a
             (setq current-plan :abort
                   done t))))))
    current-plan))

(defun niwa-session-rewind--refresh-restored-buffers (plan result)
  "Refresh visiting buffers for files restored by PLAN.

RESULT is the plist returned by `niwa-session-rewind-execute-restore'."
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

(defun niwa-session-rewind--prompt-label (prompt)
  "Return PROMPT's concise picker and impact label."
  (or (plist-get prompt :preview) "(empty prompt)"))

(defun niwa-session-rewind--prompt-candidates (session)
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
             (sort (copy-sequence (niwa-session-prompt-index session))
                   ;; Newest segment first.
                   (lambda (a b) (> (car a) (car b)))))
      (let ((segment-n (car segment-entry)))
        (dolist (prompt (reverse (cdr segment-entry)))
          (when (plist-get prompt :fork-point-id)
            (let* ((turn (plist-get prompt :turn))
                   (display
                    (format "S%d T%d  %s" segment-n turn
                            (niwa-session-rewind--prompt-label
                             prompt)))
                   (target (copy-sequence prompt)))
              (plist-put target :segment segment-n)
              (push (cons display target) all))))))
    (nreverse all)))

(defvar niwa-session-rewind--prompt-history nil
  "History list for `niwa-rewind' picks.")

(defun niwa-session-rewind--prompt-collection-fn (candidates lookup)
  "Return a completion table for the rewind picker.

CANDIDATES is the list returned by
`niwa-session-rewind--prompt-candidates' (each element is
`(DISPLAY . PLIST)').  LOOKUP is a hash-table mapping DISPLAY to
PLIST so the metadata helpers resolve in O(1).

The returned function answers `(metadata)' with:
  - category          = `niwa-prompt' (consult/marginalia hook);
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
          (category . niwa-prompt)
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

(defun niwa-session-rewind-format-relative-time (iso)
  "Format ISO (a `YYYY-MM-DDTHH-MM-SS' string) as a relative age.
Returns strings like `2h ago' / `yesterday' / `Apr 22'.  Returns a
placeholder when ISO cannot be parsed."
  (let ((t2 (niwa-session-persistence-parse-iso-time iso)))
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

(defun niwa-session-rewind--find-turn-cutoff (turn-n)
  "Return the position right before the (TURN-N + 1)th user prompt.
Returns `point-max' when TURN-N is the final user prompt.  Skips the
leading org property drawer, `#+begin_summary'/`#+end_summary' block
bodies, and gptel org tool/reasoning scaffolding to stay consistent with
`niwa-session-artifacts-collect-prompts'."
  (save-excursion
    (save-restriction
      (widen)
      (or (plist-get
           (nth turn-n
                (niwa-session-artifacts-collect-prompts
                 (current-buffer)))
           :pos)
          (point-max)))))

(defun niwa-session-rewind--strip-blank-transcript-properties ()
  "Drop transcript properties when no transcript text is retained.

Rewinding to before the first turn keeps only the leading property
drawer and blank lines.  A transcript property still on them belongs to
a discarded turn: it would be reinstalled into the live buffer,
re-persisted as bounds, hide the retained blank lines behind an
invisibility ellipsis, and project a turn with no content."
  (let ((scan-start
         (niwa-transcript--skip-leading-summary-block
          (niwa-transcript--skip-leading-properties-drawer (point-min)))))
    (when (string-blank-p
           (buffer-substring-no-properties scan-start (point-max)))
      (with-silent-modifications
        (set-text-properties scan-start (point-max) nil)))))

(defun niwa-session-rewind-load-rewind-target
    (session buffer target &optional before-turn)
  "Load SESSION's TARGET transcript boundary into BUFFER without publishing it.
When BEFORE-TURN is non-nil, discard TARGET itself as well as later text."
  (let* ((segment-n (plist-get target :segment))
         (segment-path
          (niwa-session-artifacts-segment-path
           (niwa-session-save-path session) segment-n))
         (logical (file-name-nondirectory segment-path))
         (content
          (condition-case nil
              (niwa-session-artifacts-read-artifact session logical t)
            (error
             (user-error "Published segment %d is unavailable" segment-n)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((buffer-file-name segment-path))
          (insert
           (decode-coding-string
            content (or buffer-file-coding-system 'utf-8-unix)))
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'niwa--chat-buffer-disable-org-element-cache)
              (niwa--chat-buffer-disable-org-element-cache))
            ;; Restore GPTEL_BOUNDS text properties so fork-point spans
            ;; below are locatable.  Only the transcript text matters in
            ;; this scratch buffer, so gptel's org config restore is
            ;; deliberately not involved.
            (niwa-transcript-restore-properties)))
        (let* ((id (plist-get target :fork-point-id))
               (fork-point
                (cl-find id
                         (niwa-session-artifacts-fork-point-spans
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
        (niwa-transcript-restore-properties t)
        (niwa-session-artifacts-strip-gptel-config-properties)
        (niwa-session-rewind--strip-blank-transcript-properties)
        (niwa-session-artifacts-stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun niwa-session-rewind-resolve-fork-target (session target)
  "Resolve TARGET's stable identity against SESSION's current index."
  (let ((id (plist-get target :fork-point-id)))
    (or
     (cl-loop for (segment . prompts)
              in (niwa-session-prompt-index session)
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

(defun niwa-session-rewind-assert-stable-source
    (session buffer operation)
  "Refuse OPERATION when SESSION or BUFFER owns live work."
  (unless (member operation '("rewinding" "restoring"))
    (niwa-engine-assert-local-history session operation))
  (when (niwa-session-pending-input-p session)
    (user-error
     "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before %s"
     operation))
  (when (buffer-local-value 'niwa--current-request buffer)
    (user-error "Abort the current request before %s" operation))
  (when (niwa-execution-session-live-p session)
    (user-error "Stop live executions with /ps or /stop before %s" operation))
  (when (niwa-agent-control-active-turn-p session)
    (user-error "Interrupt active agent turns before %s" operation))
  (when-let* ((goal (niwa-session-goal session))
              ((eq (niwa-goal-status goal) 'active)))
    (user-error "Pause the active Goal before %s" operation)))

(defun niwa-session-rewind--rewind-cleared-state (session)
  "Return user-facing names of live SESSION state cleared by Rewind."
  (delq
   nil
   (list
    (and (niwa-session-tasks session) "tasks")
    (and (niwa-session-goal session) "Goal")
    (and (niwa-session-agent-registry session) "agents and mailboxes")
    (and (niwa-session-pending-plan-approval session) "Plan approval")
    (and (niwa-session-plan-metadata session) "Plan handoff")
    (and (niwa-session-pending-input-p session) "pending input")
    (and (niwa-session-permission-queue session) "permission queue")
    (and (niwa-session-execution-state session) "execution state"))))

(defun niwa-session-rewind--staged-file-p (path)
  "Return non-nil when PATH differs in the Git index."
  (let ((directory (file-name-directory path)))
    (when (file-directory-p directory)
      (let* ((target (niwa-execution-target-create directory))
             (default-directory (file-name-as-directory directory))
             (remote (file-remote-p default-directory))
             (process-environment
              (unless remote process-environment)))
        (and (executable-find "git" remote)
             (= 1
                (process-file
                 "git" nil nil nil
                 "-C"
                 (niwa-execution-target-native-path target directory)
                 "diff" "--cached" "--quiet" "--"
                 (niwa-execution-target-native-path target path))))))))

(defun niwa-session-rewind--detached-child-count
    (session target-turn)
  "Return direct child count detached by rewinding SESSION to TARGET-TURN."
  (let ((session-id (niwa-session-session-id session))
        (count 0))
    (dolist (entry
             (niwa-session-persistence-list-sessions
              (niwa-session-workspace session)))
      (when-let* ((sidecar-path
                   (plist-get (plist-get entry :publication) :sidecar))
                  (sidecar
                   (condition-case nil
                       (niwa-session-codec-read sidecar-path)
                     (error nil))))
        (when (and (equal session-id
                          (plist-get sidecar :forked-from-session-id))
                   (> (or (plist-get sidecar :forked-from-turn) 0)
                      target-turn))
          (cl-incf count))))
    count))

(defun niwa-session-rewind--rewind-impact
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
     (max 0 (1+ (- (or (niwa-session-turn-count session) 0)
                       first-discarded)))
     :discarded-prompts
     (sort
      (cl-loop
       for (segment . prompts) in (niwa-session-prompt-index session)
       append
       (cl-loop for prompt in prompts
                when (>= (or (plist-get prompt :cum-turn) 0) first-discarded)
                collect (plist-put (copy-sequence prompt)
                                   :segment segment)))
      (lambda (a b)
        (< (plist-get a :cum-turn) (plist-get b :cum-turn))))
     :checkpoint-gaps
     (niwa-session-rewind--checkpoint-gaps session first-discarded)
     :external-overwrites
     (cl-count 'overwrite file-plan
               :key (lambda (entry) (plist-get entry :action)))
     :staged-files
     (cl-loop for entry in file-plan
              for path = (plist-get entry :path)
              when (niwa-session-rewind--staged-file-p path)
              collect path)
     :detached-children
     (niwa-session-rewind--detached-child-count
      session surviving-turn)
     :cleared-state
     (niwa-session-rewind--rewind-cleared-state session))))

(defun niwa-session-rewind--rewind-impact-empty-p (impact)
  "Return non-nil when IMPACT would change no Rewind-owned state."
  (and (= 0 (plist-get impact :discarded-turns))
       (null (plist-get impact :file-plan))
       (null (plist-get impact :checkpoint-gaps))
       (null (plist-get impact :cleared-state))))

(defun niwa-session-rewind--render-rewind-impact (session impact)
  "Display inspectable SESSION Rewind IMPACT."
  (let* ((target (plist-get impact :target))
         (plan (plist-get impact :file-plan))
         (restored-count (cl-count-if-not
                          (lambda (entry) (eq (plist-get entry :action) 'conflict))
                          plan))
         (gaps (plist-get impact :checkpoint-gaps))
         (staged (plist-get impact :staged-files))
         (discarded-prompts (plist-get impact :discarded-prompts))
         (cleared (plist-get impact :cleared-state)))
    (let ((buffer
           (niwa-report-show
            "*niwa-rewind-impact*"
            (list :title "Rewind impact" :identity session
                  :subtitle (format "Rewind %s %s S%d T%d"
                                    (niwa-session-name session)
                                    (if (eq (plist-get impact :boundary) 'after) "keeping" "before")
                                    (plist-get target :segment) (plist-get target :turn))
                  :sections
                  (list
                   (list :id 'impact :title "Consequences" :body (with-temp-buffer
								   (insert (format "Turns kept: %d\n"
										   (plist-get impact :surviving-turns)))
								   (insert (format "Turns discarded: %d\n"
										   (plist-get impact :discarded-turns)))
								   (insert (format "Captured files restored: %d\n" restored-count))
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
										   (niwa-session-rewind--redo-availability session)))
								   (buffer-string)))
                   (list :id 'events :title "Discarded session events" :body (with-temp-buffer
									       (when discarded-prompts
										 (insert "Discarded session events:\n")
										 (dolist (prompt discarded-prompts)
										   (insert (format "  S%d T%d  %s\n"
												   (plist-get prompt :segment)
												   (plist-get prompt :turn)
												   (niwa-session-rewind--prompt-label
												    prompt))))
										 (insert "\n"))
									       (unless discarded-prompts (insert "No session events discarded.\n"))
									       (buffer-string)))
                   (list :id 'conflicts :title "Conflicts" :body
                         (with-temp-buffer
                           (dolist (entry plan)
                             (dolist (conflict (plist-get entry :conflicts))
                               (insert (format "%s: lines %d-%d (reverting turn %d)\n"
                                               (plist-get entry :path)
                                               (plist-get conflict :start-line)
                                               (plist-get conflict :end-line)
                                               (plist-get entry :turn)))))
                           (unless (zerop (buffer-size))
                             (insert "Nothing has been changed. Lines refer to the text after newer turns are reverted.\n"))
                           (buffer-string)))
                   (list :id 'files :title "File changes and coverage gaps" :body (with-temp-buffer
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
													   'niwa-plan-entry entry)))
										    (unless (or gaps plan) (insert "No captured file changes.\n"))
										    (buffer-string))))) t)))
      (with-current-buffer buffer
        (use-local-map niwa-session-rewind--plan-buffer-map)
        (setq-local niwa-session-rewind--plan-buffer-session session
                    header-line-format "d inspect file diff · RET fold · TAB next heading · q close"))
      buffer)))

(defun niwa-session-rewind-reduce-prompt-index
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

(defun niwa-session-rewind-reduce-file-snapshots
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

(defun niwa-session-rewind-reduce-agent-transcripts
    (entries picked-cum-turn)
  "Return ENTRIES trimmed at PICKED-CUM-TURN.
Entries without an integer `:parent-turn' remain visible."
  (cl-remove-if
   (lambda (entry)
     (let ((parent-turn (plist-get (cdr entry) :parent-turn)))
       (and (integerp parent-turn) (> parent-turn picked-cum-turn))))
   entries))

(defun niwa-session-rewind--rewind-candidate (session target &optional boundary)
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
     (niwa-session-tasks candidate) nil
     (niwa-session-task-status-notes candidate) nil
     (niwa-session-last-task-write-turn candidate) nil
     (niwa-session-touched-files candidate) (make-hash-table :test #'equal)
     (niwa-session-turn-count candidate) turn
     (niwa-session-pending-reminders candidate) nil
     (niwa-session-messages candidate) nil
     (niwa-session-agent-registry candidate) nil
     (niwa-session-agent-reservations candidate) nil
     (niwa-session-agent-root-activity candidate) 'idle
     (niwa-session-agent-root-waiter candidate) nil
     (niwa-session-pending-steering candidate) nil
     (niwa-session-pending-follow-ups candidate) nil
     (niwa-session-pending-input-next-id candidate) nil
     (niwa-session-pending-input-paused candidate) nil
     (niwa-session-pending-input-failure-paused candidate) nil
     (niwa-session-dropped-file-grants candidate) nil
     (niwa-session-active-dropped-file-grants candidate) nil
     (niwa-session-mentions-shown candidate) (make-hash-table :test #'equal)
     (niwa-session-workspace-instruction-hashes candidate) nil
     (niwa-session-hook-log candidate) nil
     (niwa-session-hook-log-pending candidate) nil
     (niwa-session-repair-log candidate) nil
     (niwa-session-repair-log-pending candidate) nil
     (niwa-session-permission-log-pending candidate) nil
     (niwa-session-telemetry-pending candidate) nil
     (niwa-session-hook-context-pending candidate) nil
     (niwa-session-execution-state candidate) nil
     (niwa-session-current-segment candidate) segment
     (niwa-session-updated-at candidate) (format-time-string "%FT%H-%M-%S")
     ;; Only chats closed before the target segment survive; a later `/clear'
     ;; then appends after them in order.
     (niwa-session-chapters candidate)
     (copy-tree
      (cl-remove-if-not
       (lambda (chapter) (< (plist-get chapter :segment) segment))
       (niwa-session-chapters session)))
     (niwa-session-prompt-index candidate)
     (copy-tree
      (niwa-session-rewind-reduce-prompt-index
       (niwa-session-prompt-index session) segment target-turn before-p)
      t)
     (niwa-session-file-snapshots candidate)
     (copy-tree
      (niwa-session-rewind-reduce-file-snapshots
       (niwa-session-file-snapshots session) target-turn before-p)
      t)
     (niwa-session-untracked-effects candidate)
     (copy-tree
      (niwa-session-rewind-reduce-file-snapshots
       (niwa-session-untracked-effects session) target-turn before-p)
      t)
     (niwa-session-invoked-skills candidate)
     (cl-remove-if
      (lambda (record)
        (> (or (niwa-skill-invocation-record-turn record) 0) turn))
      (niwa-session-invoked-skills session))
     (niwa-session-permission-queue candidate) nil
     (niwa-session-pending-plan-approval candidate) nil
     (niwa-session-plan-metadata candidate) nil
     (niwa-session-goal candidate) nil
     ;; A reduced history must not share its source's mutable checkpoint epoch.
     (niwa-session-checkpoint-state candidate) nil)
    (setf (niwa-session-agent-transcripts candidate)
          (niwa-session-rewind-reduce-agent-transcripts
           (niwa-session-agent-transcripts candidate) turn))
    (setf (niwa-session-external-conversations candidate)
          (niwa-session-rewind--reduce-native-histories candidate turn))
    candidate))

(defun niwa-session-rewind--reduce-native-histories (candidate turn)
  "Reduce CANDIDATE's native boundaries and surviving scopes through TURN."
  (cl-loop
   for (scope . original) in (niwa-session-external-conversations candidate)
   when (or (equal scope "root")
            (cl-find scope (niwa-session-agent-transcripts candidate)
                     :key (lambda (entry) (plist-get (cdr entry) :agent-path))
                     :test #'equal))
   append
   (let* ((record (copy-tree original))
          (boundaries (cl-remove-if
                       (lambda (entry) (> (plist-get entry :turn) turn))
                       (plist-get record :boundaries)))
          (latest (car (last boundaries))))
     (when (and (plist-get record :id) (not (plist-get record :boundaries)))
       (user-error "No recorded native answer boundary for %s; recover its Claude history before Rewind" scope))
     (cond
      ((plist-get latest :detached)
       (list (cons scope (list :engine 'claude-code :state 'unstarted
                               :boundaries boundaries))))
      (latest
       (setq record (plist-put record :boundaries boundaries)
             record (plist-put record :id (plist-get latest :id))
             record (plist-put record :host (plist-get latest :host))
             record (plist-put record :directory (plist-get latest :directory)))
       (unless (eq 'diverged (plist-get record :state))
         (setq record (plist-put record :state
                                 (if (or (and (< (plist-get latest :turn) turn)
                                              (or (equal scope "root")
                                                  (< (length boundaries)
                                                     (length (plist-get original :boundaries)))))
                                         (and (not (equal scope "root"))
                                              (or (memq (plist-get original :state) '(uncertain in-flight))
                                                  (plist-get original :start-failed))))
                                     'uncertain 'ready))))
       (list (cons scope record)))))))

(defvar niwa-session-rewind--native-committed nil
  "Non-nil after the current prepared native transaction commits its head.")

(defun niwa-session-rewind--prepare-native
    (session buffer candidate continue complete)
  "Prepare native CANDIDATE for SESSION and BUFFER, then call CONTINUE.
COMPLETE receives the result and nil, or nil and an error string.  Without
COMPLETE, asynchronous failures are reported in SESSION's diagnostic scope."
  (let ((head (plist-get (niwa-session-publication session) :head))
        (tick (buffer-chars-modified-tick buffer))
        (source (niwa-session-artifacts-printed-value
                 (niwa-session-artifacts--live-sidecar session buffer)))
        (scope (niwa-diagnostic-scope session)))
    (with-current-buffer buffer
      (niwa-claude-code-fork-histories
       (niwa-session-external-conversations candidate)
       (lambda (histories failure cleanup)
         (let (result (niwa-session-rewind--native-committed nil))
           (unwind-protect
               (condition-case err
                   (unless failure
                     (unless (and (buffer-live-p buffer)
                                  (eq session (buffer-local-value 'niwa--session buffer))
                                  (= tick (buffer-chars-modified-tick buffer))
                                  (equal source
                                         (niwa-session-artifacts-printed-value
                                          (niwa-session-artifacts--live-sidecar session buffer)))
                                  (equal head (plist-get
                                               (niwa-session-publication-read
                                                (niwa-session-save-path session)) :head)))
                       (user-error "Session source changed during native history preparation; retry"))
                     (niwa-session-artifacts-assert-mutation-authority session buffer)
                     (niwa-session-rewind-assert-stable-source session buffer "rewinding")
                     (setf (niwa-session-external-conversations candidate) histories)
                     (setq result (funcall continue candidate)))
                 ((error quit) (setq failure (error-message-string err))))
             (unless niwa-session-rewind--native-committed (funcall cleanup)))
           (if complete (funcall complete result failure)
             (when failure
               (niwa-diagnostic-warning scope 'niwa failure)))))
       session))))

(defun niwa-session-rewind-copy-session-state (from to)
  "Copy every cl-struct slot from session FROM into TO."
  (dotimes (index (length from))
    (aset to index (aref from index))))

(defun niwa-session-rewind--copy-rewind-session-state (from to)
  "Copy Rewind state from FROM into TO without replacing durability runtime."
  (let ((pending (niwa-session-pending-publication to))
        (publication (niwa-session-publication to))
        (checkpoint (niwa-session-checkpoint-state to))
        (queue (niwa-session-publication-queue to))
        (uncommitted
         (niwa-session-publication-uncommitted-batches to))
        (active (niwa-session-publication-active-p to))
        (lease (niwa-session-lease to))
        (ownership-epoch (niwa-session-ownership-epoch to))
        (timer (niwa-session-lease-renewal-timer to)))
    (niwa-session-rewind-copy-session-state from to)
    (setf (niwa-session-pending-publication to) pending
          (niwa-session-publication to) publication
          (niwa-session-checkpoint-state to) checkpoint
          (niwa-session-publication-queue to) queue
          (niwa-session-publication-uncommitted-batches to) uncommitted
          (niwa-session-publication-active-p to) active
          (niwa-session-lease to) lease
          (niwa-session-ownership-epoch to) ownership-epoch
          (niwa-session-lease-renewal-timer to) timer)))

(defun niwa-session-rewind-materialize-publication
    (session publication staging-path)
  "Materialize SESSION's committed PUBLICATION below STAGING-PATH.

Only the publication's logical artifacts are copied.  Lease, publication,
recovery, and other control paths are never materialized.

STAGING-PATH is created when absent."
  (unless publication
    (error "Portable project operation requires a committed session publication"))
  (make-directory staging-path t)
  (let ((root (file-name-as-directory (expand-file-name staging-path)))
        copied)
    (dolist (entry (plist-get publication :artifacts))
      (let* ((logical (car entry))
             (destination (expand-file-name logical root))
             (published (plist-get (cdr entry) :published))
             ;; A live owner's newer staged bytes win over the manifest in
             ;; the ordinary reader, so the direct copy needs none pending.
             (local-p (and (stringp published) (not (file-remote-p published))
                           (not (file-remote-p staging-path))
                           (null (niwa-session-publication-uncommitted-batches session))
                           (not (niwa-session-pending-publication session)))))
        ;; Staging is a fresh directory this operation created, so a lexical
        ;; check after `expand-file-name' resolved any `..' is containment;
        ;; resolving true names cost a tenth of a large Save As.
        (unless (string-prefix-p root destination)
          (error "Session artifact escapes staging: %s" logical))
        (make-directory (file-name-directory destination) t)
        (if local-p
            ;; Immutable local bytes are copied by the kernel and verified
            ;; against the manifest, instead of passing through Lisp.
            (progn
              (copy-file published destination t)
              (push (list logical destination (plist-get (cdr entry) :sha256))
                    copied))
          (let ((content (niwa-session-artifacts-read-artifact session logical t))
                (coding-system-for-write 'no-conversion))
            (write-region content nil destination nil 'silent)))))
    ;; One verification pass hashes every copy together.
    (setq copied (nreverse copied))
    (cl-mapc (lambda (copy digest)
               (unless (equal (nth 2 copy) digest)
                 (error "Session artifact does not match its manifest: %s"
                        (car copy))))
             copied
             (niwa-session-publication--files-sha256
              (mapcar #'cadr copied)))))

(defun niwa-session-rewind--prune-remote-rewind-staging
    (candidate target staging-path)
  "Remove post-target artifacts from CANDIDATE's STAGING-PATH snapshot.
TARGET identifies the retained segment and turn."
  (let ((target-segment (plist-get target :segment))
        (target-turn (or (niwa-session-turn-count candidate) 0))
        (agents
         (delq nil
               (mapcar
                (lambda (entry) (plist-get (cdr entry) :path))
                (niwa-session-agent-transcripts candidate)))))
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

(defun niwa-session-rewind--stage-rewind
    (session candidate target staging-path staging-buffer publication)
  "Stage CANDIDATE and TARGET from SESSION under STAGING-PATH.

Materialize only PUBLICATION's immutable logical artifacts; lease and
publication control directories are never copied."
  (niwa-session-rewind-materialize-publication session publication staging-path)
  (let ((source (copy-sequence session))
        ;; The candidate already decided which turn survives, so the
        ;; transcript cutoff follows it instead of deciding again.
        (before-p (< (or (niwa-session-turn-count candidate) 0)
                     (or (plist-get target :cum-turn) 0))))
    (setf (niwa-session-save-path source) staging-path)
    (niwa-session-rewind-load-rewind-target
     source staging-buffer target before-p))
  (with-current-buffer staging-buffer
    (setq buffer-file-name
          (niwa-session-artifacts-segment-path
           staging-path (plist-get target :segment)))
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (niwa-session-artifacts-update-prompt-index
     candidate staging-buffer)
    (niwa-session-persistence-write-current-buffer-atomically
     buffer-file-name)
    (set-buffer-modified-p nil))
  (cl-loop for segment from (1+ (plist-get target :segment))
           to (or (niwa-session-current-segment session) 1)
           for path = (niwa-session-artifacts-segment-path
                       staging-path segment)
           when (file-exists-p path)
           do (delete-file path))
  (niwa-session-rewind--prune-remote-rewind-staging
   candidate target staging-path)
  (niwa-session-codec-write
   (niwa-session-artifacts-sidecar-path staging-path)
   (niwa-session-artifacts--live-sidecar candidate staging-buffer))
  (let ((decoded
         (niwa-session-codec-deserialize
          (niwa-session-codec-read
           (niwa-session-artifacts-sidecar-path staging-path))
          (niwa-session-workspace session))))
    (setf (niwa-session-checkpoint-state candidate)
          (niwa-session-checkpoint-state (plist-get decoded :session)))
    decoded))

(defun niwa-session-rewind--backup-restore-files (plan directory)
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

(defun niwa-session-rewind--rollback-restore-files (backups written)
  "Restore file BACKUPS captured for a failed Rewind.
Only restore WRITTEN after-states that still belong to this transaction.
Return descriptions of every artifact that could not be restored."
  (let (failures)
    (dolist (entry backups)
      (let ((path (plist-get entry :path)))
        (when (cl-find path written :key (lambda (state) (plist-get state :path)) :test #'equal)
          (condition-case err
              (setq failures
                    (nconc failures
                           (niwa-change-rollback
                            (list (list :path path :exists (plist-get entry :existed)
                                        :bytes (when (plist-get entry :existed)
                                                 (niwa-session-artifacts-read-file-raw
                                                  (plist-get entry :backup)))))
                            written #'niwa-session-rewind--file-state
                            (lambda (_before _after)
                              (if (plist-get entry :existed)
                                  (progn
                                    (make-directory (file-name-directory path) t)
                                    (copy-file (plist-get entry :backup) path t t t))
                                (when (file-exists-p path) (delete-file path)))))))
            (error (setq failures (nconc failures (list (cons path err)))))))))
    (mapcar (lambda (failure)
              (format "%s (%s)" (car failure) (error-message-string (cdr failure))))
            failures)))

(defun niwa-session-rewind-rewind-publication-artifacts
    (session buffer staging-path &optional state)
  "Return STAGING-PATH as SESSION replacement artifacts, sidecar last.

SESSION supplies the owned publication path.  BUFFER supplies live transcript
state.  STATE, when non-nil, supplies the logical sidecar state without
replacing SESSION's live lease runtime."
  (niwa--with-gc-busy
    (let* ((save-path (niwa-session-save-path session))
           (sidecar-name "session.meta.el")
           artifacts)
      (dolist (path (sort (directory-files-recursively staging-path ".*")
                          #'string<))
        (let ((logical (file-relative-name path staging-path)))
          (when (and (not (equal logical sidecar-name))
                     (niwa-session-publication-logical-path-p logical))
            ;; The staged file is handed over whole: reading every artifact
            ;; into a string doubled a large Save As's allocation.
            (push
             (list :path (expand-file-name logical save-path)
                   :source-file path)
             artifacts))))
      (append
       (nreverse artifacts)
       (list
        (list
         :path (niwa-session-artifacts-sidecar-path save-path)
         :content
         (niwa-session-artifacts-printed-value
          (niwa-session-artifacts--live-sidecar
           (or state session) buffer))
         :commit-marker t
         :replace t))))))

(defun niwa-session-rewind--failure-backtrace ()
  "Return a printable backtrace for the error currently being signaled.

Call this from a `handler-bind' handler, before the stack unwinds.  A
committed portable Rewind cannot be rolled back, so a local application
step that fails afterwards has to name its own frame."
  (ignore-errors
    (backtrace-to-string (backtrace-get-frames nil))))

(defun niwa-session-rewind--install-rewind-buffer
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
        (niwa-session-artifacts-replace-transcript-contents staging-buffer))
      ;; `replace-region-contents' keeps the properties on text it did
      ;; not have to change, so the retained leading blank lines still
      ;; carry the discarded turns' transcript properties.
      (niwa-session-rewind--strip-blank-transcript-properties)
      (setq buffer-file-name
            (niwa-session-artifacts-segment-path
             (niwa-session-save-path session)
             (plist-get target :segment))
            buffer-file-truename
            (file-truename buffer-file-name))
      (set-buffer-modified-p nil)
      (set-visited-file-modtime)))
  (niwa-session-persistence-notify-session-event
   session 'reset-status-panel)
  (niwa-session-persistence-notify-session-event
   session 'rerender)
  (niwa-session-persistence-notify-session-event
   session 'replace-history))

(defun niwa-session-rewind--end-store-intent (session committed label)
  "Resolve SESSION's instruction store Rewind intent; warn if that fails.
COMMITTED is the Rewind outcome and LABEL names the Rewind in the warning.
An intent left behind names the original turn count, so the next resolution
drops it without pruning anything."
  (condition-case err
      (niwa-instruction-store-rewind-end session committed)
    (error
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope session) 'niwa
      (format (if committed
                  "%s committed, but the instruction store still holds the discarded activity; it is pruned before the next request or at resume: %s"
                "%s rolled back; its instruction store intent is dropped later: %s")
              label (error-message-string err))
      :warning))))

(defun niwa-session-rewind--commit-rewind
    (session buffer target plan &optional boundary replan prepared)
  "Commit SESSION, BUFFER, TARGET, and file PLAN through one head CAS.
BOUNDARY selects the turn side; REPLAN rechecks the confirmed plan.
PREPARED supplies a candidate with independently forked native histories."
  (let* ((workspace (niwa-session-workspace session))
         (save-path (niwa-session-save-path session))
         (publication
          (or (niwa-session-publication session)
              (niwa-session-publication-read save-path)
              (error "Portable project Rewind requires a committed publication")))
         (head-before (plist-get publication :head))
         (temporary-root (make-temp-file "niwa-remote-rewind-" t))
         (staging-path (file-name-concat temporary-root "staging"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (candidate
          (or prepared (niwa-session-rewind--rewind-candidate
                        session target boundary)))
         (staging-buffer
          (generate-new-buffer " *niwa-remote-rewind-staging*"))
         (niwa-instruction-store--inhibit workspace)
         file-backups restore-result
         (niwa-session-rewind--written-states (list nil))
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
              (niwa-instruction-store-rewind-begin
               session (or (niwa-session-turn-count candidate) 0)
               (plist-get target :cum-turn))
              ;; Materialization, project backup, recheck, and restore are one
              ;; synchronous target operation.  Timer renewal stays off-target
              ;; until the wrapper proves the same generation still owns it.
              (niwa-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (let ((current
                        (niwa-session-publication-read
                         save-path)))
                   (unless (and current
                                (equal head-before
                                       (plist-get current :head)))
                     (user-error
                      "Session state changed before portable project Rewind; retry"))
                   (setq publication current)
                   (setf (niwa-session-publication session) current))
                 (setq file-backups
                       (niwa-session-rewind--backup-restore-files
                        plan file-backup-dir))
                 (niwa-session-rewind--stage-rewind
                  session candidate target staging-path staging-buffer
                  publication)
                 (let ((rechecked
                        (if replan (funcall replan)
                          (niwa-session-rewind-restore-plan
                           session (plist-get target :cum-turn)
                           (not (eq boundary 'after))))))
                   (unless
                       (equal
                        (niwa-session-rewind--sorted-file-plan plan)
                        (niwa-session-rewind--sorted-file-plan rechecked))
                     (error
                      "Captured files changed after Rewind confirmation")))
                 ;; Discard definitions before changing their source history.
                 (niwa-execution-stop-services session)
                 (setq project-restore-started t)
                 (setq restore-result (niwa-session-rewind-execute-restore session plan))
                   (when (plist-get restore-result :failed)
                     (error "File restore failed on %s: %s"
                            (plist-get restore-result :failed)
                            (plist-get restore-result :error)))))
              (setq publish-attempted t)
              (condition-case publish-error
                  (niwa-session-publication-publish
                   session
                   (niwa-session-rewind-rewind-publication-artifacts
                    session staging-buffer staging-path candidate))
                (error
                 ;; The captured head changes at marker CAS, before final lease
                 ;; normalization.  A changed head is already committed and
                 ;; must never be rolled back as though publication failed.
                 (if (not
                      (equal head-before
                             (plist-get (niwa-session-publication session)
                                        :head)))
                     (setq committed t
                           niwa-session-rewind--native-committed t
                           post-commit-error publish-error)
                   (signal (car publish-error) (cdr publish-error)))))
              (unless committed
                (unless
                    (not
                     (equal head-before
                            (plist-get (niwa-session-publication session)
                                       :head)))
                  (error "Portable project Rewind did not commit a publication head"))
                (setq committed t))
              (setq niwa-session-rewind--native-committed t)
              ;; Only the successful head CAS installs logical session state.
              (niwa-session-rewind--copy-rewind-session-state
               candidate session)
              ;; The head is committed and cannot be rolled back, so a
              ;; failed buffer install must not abandon the instruction
              ;; and buffer refreshes that follow it.
              (let (install-backtrace)
                (condition-case install-error
                    (handler-bind
                        ((error
                          (lambda (_error)
                            (setq install-backtrace
                                  (niwa-session-rewind--failure-backtrace)))))
                      (niwa-session-rewind--install-rewind-buffer
                       buffer staging-buffer session target))
                  (error
                   (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                    'niwa
                    (concat
                     (format
                      "Portable project Rewind committed, but the transcript buffer did not install: %s"
                      (error-message-string install-error))
                     (and install-backtrace (concat "\n" install-backtrace)))
                    :warning))))
              ;; The workspace store owns the current set: prune the records
              ;; here and in the store.
              (niwa-session-rewind--end-store-intent
               session t "Portable project Rewind")
              (condition-case refresh-error
                  (niwa-session-rewind--refresh-restored-buffers
                   plan (list :succeeded (length plan)))
                (error
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                  'niwa
                  (format
                   "Portable project Rewind committed, but buffers did not refresh: %s"
                   (error-message-string refresh-error))
                  :warning))))
          (error (setq operation-error err)))
      (unless committed
        (niwa-session-rewind--end-store-intent
         session nil "Portable project Rewind")
        (when project-restore-started
          (condition-case rollback-error
              (setq rollback-failures
                    (niwa-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       (niwa-session-rewind--rollback-restore-files
                        file-backups (car niwa-session-rewind--written-states)))))
            (error
             (push
              (format "Project rollback authority (%s)"
                      (error-message-string rollback-error))
              rollback-failures))))
        (when (and publish-attempted (null rollback-failures))
          (condition-case discard-error
              (niwa-session-publication-discard-rolled-back
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
            (niwa-session-recovery-record-failure
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

(defun niwa-session-rewind--rewind-confirmation (session impact)
  "Return the Rewind confirmation for SESSION and IMPACT.

The prompt names the boundary and both sides of it, because naming only
the selected turn leaves the reader to guess whether it survives."
  (let ((target (plist-get impact :target))
        (plan (plist-get impact :file-plan)))
    (format "Rewind %s %s S%d T%d: keep %d turn%s, discard %d (%d file%s; %s)? "
            (niwa-session-name session)
            (if (eq (plist-get impact :boundary) 'after) "keeping" "to before")
            (plist-get target :segment)
            (plist-get target :turn)
            (plist-get impact :surviving-turns)
            (if (= 1 (plist-get impact :surviving-turns)) "" "s")
            (plist-get impact :discarded-turns)
            (length plan)
            (if (= 1 (length plan)) "" "s")
            (niwa-session-rewind--redo-availability session))))

(defun niwa-session-rewind--prepare-rewind-files (session target boundary)
  "Prepare SESSION files for TARGET and BOUNDARY, offering cancel, force or merge.
Return the plan, its recheck function and force choice, or `:abort'."
  (catch 'cancel
    (let* ((turn (plist-get target :cum-turn))
           (before-p (not (eq boundary 'after)))
           (force nil)
           (resolutions niwa-session-rewind--merge-resolutions)
           (replan (lambda ()
                     (if force
                         (niwa-session-rewind-restore-plan session turn before-p)
                       (niwa-session-rewind--revert-plan session turn before-p resolutions))))
           (select
            (lambda ()
              (let ((plan (funcall replan)))
                (when (cl-some (lambda (entry) (plist-get entry :conflicts)) plan)
                  (niwa-session-rewind--render-rewind-impact
                   session (niwa-session-rewind--rewind-impact
                            session target plan boundary))
                  (pcase (read-char-choice
                            "Rewind conflicts; [c]ancel, [f]orce snapshot restore or [m]erge? "
                            '(?c ?f ?m))
                    (?f (setq force t plan (funcall replan)))
                    (?m (throw 'cancel (list :merge plan)))
                    (_ (throw 'cancel :abort))))
                plan)))
           (plan (niwa-session-rewind--prepare-buffers-for-restore
                  session turn (funcall select) "Rewind" before-p select)))
      (if (eq plan :abort) :abort
        (list :plan plan :replan replan :force force)))))

(defun niwa-session-rewind--choose-scope (operation detail)
  "Choose OPERATION's scope before reading working files, naming DETAIL."
  (pcase (car (read-multiple-choice
               (format "%s %s?" operation detail)
               '((?c "conversation only" "Leave working files unchanged; undo with Redo")
                 (?f "conversation and files" "Review captured file changes before proceeding")
                 (?q "cancel" "Change nothing"))))
    (?c 'conversation)
    (?f 'files)
    (_ 'abort)))

(defun niwa-session-rewind-rewind
    (buffer target &optional boundary scope confirm-function complete)
  "Rewind BUFFER to TARGET, selecting BOUNDARY and SCOPE.
CONFIRM-FUNCTION confirms conversation impact without host UI.  COMPLETE
receives (RESULT ERROR), with ERROR nil or a string.  Native preparation
returns `:pending'; local history still completes synchronously."
  (let* ((niwa-session-rewind--native-committed nil)
         (result (niwa-session-rewind--rewind
                  buffer target boundary scope confirm-function complete)))
    (when (and complete (not (eq result :pending)))
      (funcall complete result nil))
    result))

(defun niwa-session-rewind--rewind
    (buffer target &optional boundary scope confirm-function complete)
  "Rewind BUFFER's session in place to stable assistant TARGET.

BOUNDARY says which side of TARGET's turn the transcript ends on.  With
`after', TARGET's turn is the last one kept, which is what pointing at a
response asks for.  With `before' -- the default -- TARGET's turn is
discarded with everything later, which is what picking one of the user's
own prompts asks for, since the point of naming a prompt is to return to
just before sending it.
SCOPE is `conversation' to leave files alone, or `files' to restore them.
With `choose', offer a scope choice if discarded turns have captured files.
Nil preserves the captured-file restore for noninteractive callers.
CONFIRM-FUNCTION, when supplied, confirms a conversation-only impact without
opening host UI; browser callers have already asked their owner.
COMPLETE receives (RESULT ERROR), where ERROR is nil or a string.  Native
preparation returns `:pending'; local history still completes synchronously."
  (let ((session (buffer-local-value 'niwa--session buffer))
        (niwa-session-artifacts-backup-cache
         (or niwa-session-artifacts-backup-cache (make-hash-table :test #'equal))))
    (unless (and session (plist-get target :fork-point-id))
      (user-error "Rewind requires a settled assistant response"))
    (niwa-session-artifacts-assert-mutation-authority session buffer)
    (setq target
          (niwa-session-rewind-resolve-fork-target
           session target))
    (niwa-session-rewind-assert-stable-source
     session buffer "rewinding")
    (when (niwa-session-note-conversation session)
      (niwa-instruction-store-sync (niwa-session-workspace session)))
    (let* ((chosen-scope
            (if (not (eq scope 'choose)) (or scope 'files)
              (or (and (cl-some
                      (lambda (entry)
                        (and (cdr entry)
                             (if (eq boundary 'after)
                                 (> (car entry) (plist-get target :cum-turn))
                               (>= (car entry) (plist-get target :cum-turn)))))
                      (niwa-session-file-snapshots session))
                     (niwa-session-rewind--choose-scope
                      "Rewind" (format "%s S%d T%d"
                                       (if (eq boundary 'after) "keeping" "to before")
                                       (plist-get target :segment)
                                       (plist-get target :turn))))
                  'files)))
           (conversation-only (eq chosen-scope 'conversation))
           (selection (cond
                       ((eq chosen-scope 'abort) :abort)
                       (conversation-only (list :replan (lambda () nil)))
                       (t (niwa-session-rewind--prepare-rewind-files
                           session target boundary))))
           (prepared (unless (eq selection :abort) (plist-get selection :plan))))
      (when (eq selection :abort)
        (when-let* ((impact-buffer (get-buffer "*niwa-rewind-impact*")))
          (quit-windows-on impact-buffer t)
          (when (buffer-live-p impact-buffer)
            (kill-buffer impact-buffer))))
      (if (plist-get (and (listp selection) selection) :merge)
          (progn
            (when-let* ((impact-buffer (get-buffer "*niwa-rewind-impact*")))
              (quit-windows-on impact-buffer t)
              (when (buffer-live-p impact-buffer)
                (kill-buffer impact-buffer)))
            (niwa-session-rewind-merge-open
             buffer target boundary (plist-get selection :merge)
             niwa-session-rewind--merge-resolutions)
            nil)
        (unless (eq selection :abort)
        (let ((impact
               (niwa-session-rewind--rewind-impact
                session target prepared boundary)))
          (if (niwa-session-rewind--rewind-impact-empty-p impact)
              (message "Already at this state")
            (unless conversation-only
              (niwa-session-rewind--render-rewind-impact session impact))
            (let ((confirmed
                   (if conversation-only
                       (if confirm-function (funcall confirm-function impact)
                         (or (eq scope 'choose)
                             (yes-or-no-p
                              "Rewind conversation only, leaving files unchanged (undo with Redo)? ")))
                     (yes-or-no-p
                    (concat
                     (when (plist-get selection :force)
                       "Force snapshot restore (overwrite newer edits). ")
                     (niwa-session-rewind--rewind-confirmation
                      session impact))))))
              (unless confirmed
                (when-let* ((impact-buffer (get-buffer "*niwa-rewind-impact*")))
                  (quit-windows-on impact-buffer t)
                  (when (buffer-live-p impact-buffer) (kill-buffer impact-buffer))))
              (when confirmed
                (niwa-session-artifacts-assert-mutation-authority session buffer)
                (let ((finish
                       (lambda (candidate)
                         (niwa-session-rewind--commit-rewind
                          session buffer target prepared boundary
                          (plist-get selection :replan) candidate)
                         (when-let* ((impact-buffer (get-buffer "*niwa-rewind-impact*")))
                           (quit-windows-on impact-buffer t)
                           (when (buffer-live-p impact-buffer) (kill-buffer impact-buffer)))
                         (with-current-buffer buffer
                           (niwa--run-session-start-hooks "rewind"))
                         (message "niwa: rewound %s %s S%d T%d, %d turn%s discarded"
                                  (niwa-session-name session)
                                  (if (eq boundary 'after) "keeping" "to before")
                                  (plist-get target :segment) (plist-get target :turn)
                                  (plist-get impact :discarded-turns)
                                  (if (= 1 (plist-get impact :discarded-turns)) "" "s"))
                         t)))
                  (if (niwa-session-external-conversations session)
                      (niwa-session-rewind--prepare-native
                       session buffer
                       (niwa-session-rewind--rewind-candidate session target boundary)
                       finish complete)
                    (funcall finish nil))))))))))))


;;
;;; Redo: restoring a published head

(defun niwa-session-rewind-published-heads (session)
  "Return SESSION's restorable published heads, newest first.

One entry per distinct settled turn state rather than one per published
generation: a generation is written on every committed save, so a turn
that streamed for a minute leaves dozens of heads whose transcripts
differ only in how much of that turn had arrived.  Restoring to any of
them means restoring the same conversation, so the newest generation for
a given turn count and fork point stands for all of them.  The state the
session is already in is excluded for the same reason.

Each entry carries `:head', `:time', `:transcript-bytes', and the turn
facts from the published sidecar.  Workspace-free sessions have no heads."
  (when (niwa-session-codec-portable-authority-p session)
    (let* ((session-dir (niwa-session-save-path session))
           (current
            (or (plist-get (niwa-session-publication session) :head)
                (niwa-session-durability-publication-head session-dir)))
           (seen (make-hash-table :test #'equal))
           entries)
      (when session-dir
        (when-let* ((facts (and current
                                (niwa-session-publication-head-facts
                                 session-dir current)))
                    (turn-count (plist-get facts :turn-count)))
          (puthash (list turn-count (plist-get facts :fork-point-id))
                   t seen))
        (dolist (summary (niwa-session-publication-generation-summaries
                          session-dir))
          (let* ((head (plist-get summary :head))
                 (key (and (niwa-session-publication-settled-summary-p
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

(defun niwa-session-rewind--load-restored-transcript
    (buffer staging-path segment)
  "Load STAGING-PATH's SEGMENT transcript into BUFFER."
  (let ((segment-path
         (niwa-session-artifacts-segment-path staging-path segment)))
    (unless (file-regular-p segment-path)
      (error "Published head has no segment %d transcript" segment))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((buffer-file-name segment-path))
          (insert-file-contents segment-path)
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'niwa--chat-buffer-disable-org-element-cache)
              (niwa--chat-buffer-disable-org-element-cache))
            (niwa-transcript-restore-properties)))
        (niwa-session-artifacts-strip-gptel-config-properties)
        (niwa-session-artifacts-stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun niwa-session-rewind--restore-file-plan (candidate)
  "Return the captured file-restore plan for restored CANDIDATE state.

CANDIDATE carries the published head's file snapshots and resolves its
backup bytes through that head's publication, so the plan describes the
captured working-tree state the restored turns owned."
  (niwa-session-rewind-restore-plan
   candidate (or (niwa-session-turn-count candidate) 0)))

(defun niwa-session-rewind--restore-impact (candidate plan)
  "Return the impact of restoring CANDIDATE with file PLAN."
  (list :restored-turns (or (niwa-session-turn-count candidate) 0)
        :file-plan plan
        :external-overwrites
        (cl-count 'overwrite plan
                  :key (lambda (entry) (plist-get entry :action)))))

(defun niwa-session-rewind--sorted-file-plan (plan)
  "Return PLAN ordered by path for comparison."
  (sort (copy-sequence plan)
        (lambda (left right)
          (string< (plist-get left :path) (plist-get right :path)))))

(defun niwa-session-rewind-restore-head
    (session buffer head &optional confirm-function conversation-only complete)
  "Restore SESSION and BUFFER to HEAD, optionally notifying COMPLETE.
CONFIRM-FUNCTION and CONVERSATION-ONLY control the existing restore transaction.
COMPLETE receives (RESULT ERROR), with ERROR nil or a string.  Native history
is forked asynchronously before restoration and returns `:pending'."
  (let* ((niwa-session-rewind--native-committed nil)
         (result
         (catch 'niwa-native-preparation
           (niwa-session-rewind--restore-head
            session buffer head confirm-function conversation-only nil complete))))
    (when (and complete (not (eq result :pending)))
      (funcall complete result nil))
    result))

(defun niwa-session-rewind--restore-head
    (session buffer head &optional confirm-function conversation-only prepared complete confirmed-plan)
  "Restore SESSION and BUFFER to published HEAD through one head CAS.

HEAD is an immutable published generation, as returned by
`niwa-session-rewind-published-heads'.  Its conversation state --
transcript, sidecar, retained agent transcripts,
and persisted tool results -- is republished as a new head, so the
restore is itself an ordinary committed publication rather than a
rollback.  Captured working-tree files are restored from that head's
retained file history in the same transaction; coverage is limited to
what niwa captured, so uncaptured filesystem effects may remain.

CONFIRM-FUNCTION receives the impact plist from
`niwa-session-rewind--restore-impact' before anything is mutated and
must return non-nil for the restore to proceed.  Preparation, including
the modified-buffer prompt, happens before the session lease is
reserved; the reserved transaction then rechecks both the current head
and the file plan the caller confirmed.  CONVERSATION-ONLY skips all working
file reads and restores, including modified-buffer prompts.
PREPARED supplies independently forked native histories; COMPLETE receives
the asynchronous result and error string, if any.  CONFIRMED-PLAN supplies
the file plan already confirmed before native preparation."
  (unless (niwa-session-codec-portable-authority-p session)
    (user-error "A workspace session is required to restore published heads"))
  (niwa-session-rewind-assert-stable-source session buffer "restoring")
  (let* ((save-path (niwa-session-save-path session))
         (publication
          (or (niwa-session-publication-read save-path head)
              (user-error "Published head is unavailable: %s" head)))
         (head-before
          (plist-get (niwa-session-publication session) :head))
         (temporary-root (make-temp-file "niwa-session-restore-" t))
         (staging-path (file-name-concat temporary-root "staging"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (source (copy-sequence session))
         (staging-buffer (generate-new-buffer " *niwa-restore-staging*"))
         (workspace (niwa-session-workspace session))
         candidate plan file-backups restore-result file-restore-started committed
         (niwa-session-rewind--written-states (list nil))
         rollback-failures)
    (setf (niwa-session-publication source) publication)
    (unwind-protect
        (progn
          (with-current-buffer staging-buffer
            (funcall (buffer-local-value 'major-mode buffer)))
          ;; Preparation reads immutable published bytes only, so it runs
          ;; before the lease is reserved: the user is not holding session
          ;; authority while a prompt waits.
          (niwa-session-rewind-materialize-publication
           source publication staging-path)
          (setq candidate
                (or (plist-get
                     (niwa-session-codec-deserialize
                      (niwa-session-codec-read
                       (niwa-session-artifacts-sidecar-path staging-path))
                      workspace)
                     :session)
                    (error "Published head has no readable sidecar")))
          (when prepared
            (setf (niwa-session-external-conversations candidate)
                  (niwa-session-external-conversations prepared)))
          ;; The sidecar carries logical state only; where the session
          ;; lives and what it talks to belong to the live session, as
          ;; they do on resume.  Its publication is the head being
          ;; restored, so retained file history resolves against it.
          ;; Wake-ups are live too: an old head's would fire again.
          (setf (niwa-session-save-path candidate) save-path
                (niwa-session-working-directory candidate)
                (niwa-session-working-directory session)
                (niwa-session-schedules candidate)
                (niwa-session-schedules session)
                (niwa-session-publication candidate) publication)
          (niwa-session-set-execution-target
           candidate (niwa-session-execution-target session))
          (niwa-session-rewind--load-restored-transcript
           staging-buffer staging-path
           (or (niwa-session-current-segment candidate) 1))
          (when prepared (setq plan confirmed-plan))
          (unless (or prepared conversation-only)
            (setq plan (niwa-session-rewind--restore-file-plan candidate))
            (setq plan
                  (niwa-session-rewind--prepare-buffers-for-restore
                   candidate (or (niwa-session-turn-count candidate) 0)
                   plan "Restore" nil)))
          (unless (eq plan :abort)
            (when (or prepared (null confirm-function)
                      (funcall confirm-function
                               (niwa-session-rewind--restore-impact
                                candidate plan)))
              (when (and (not prepared)
                         (niwa-session-external-conversations candidate))
                (throw 'niwa-native-preparation
                       (niwa-session-rewind--prepare-native
                        session buffer candidate
                        (lambda (ready)
                          (niwa-session-rewind--restore-head
                           session buffer head nil conversation-only ready nil plan))
                        complete)))
              (make-directory file-backup-dir t)
              ;; Recheck, file backup, and file restore are one
              ;; synchronous target operation under the reserved lease,
              ;; exactly as Rewind stages its own file restore.
              (niwa-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (let ((current
                        (niwa-session-publication-read save-path)))
                   (unless (and current
                                (equal head-before
                                       (plist-get current :head)))
                     (user-error
                      "Session state changed before restore; retry"))
                   (setf (niwa-session-publication session) current))
                 (unless (or conversation-only (equal
                          (niwa-session-rewind--sorted-file-plan plan)
                          (niwa-session-rewind--sorted-file-plan
                           (niwa-session-rewind--restore-file-plan
                            candidate))))
                   (error "Captured files changed after restore confirmation"))
                 (setq file-backups
                       (niwa-session-rewind--backup-restore-files
                        plan file-backup-dir))
                 ;; Redo replaces history just as Rewind does.
                 (niwa-execution-stop-services session)
                 (when plan
                   (setq file-restore-started t)
                   (setq restore-result (niwa-session-rewind-execute-restore candidate plan))
                     (when (plist-get restore-result :failed)
                       (error "File restore failed on %s: %s"
                              (plist-get restore-result :failed)
                              (plist-get restore-result :error))))))
              (unwind-protect
                  (niwa-session-publication-publish
                   session
                   (niwa-session-rewind-rewind-publication-artifacts
                    session staging-buffer staging-path candidate)
                   t)
                (unless (equal head-before
                               (plist-get (niwa-session-publication session) :head))
                  (setq committed t niwa-session-rewind--native-committed t)))
              (when (equal head-before
                           (plist-get (niwa-session-publication session)
                                      :head))
                (error "Restore did not commit a publication head"))
              (setq committed t)
              ;; Only the successful head CAS installs logical session state.
              (niwa-session-rewind--copy-rewind-session-state
               candidate session)
              (let (install-backtrace)
                (condition-case install-error
                    (handler-bind
                        ((error
                          (lambda (_error)
                            (setq install-backtrace
                                  (niwa-session-rewind--failure-backtrace)))))
                      (niwa-session-rewind--install-rewind-buffer
                       buffer staging-buffer session
                       (list :segment
                             (or (niwa-session-current-segment session) 1))))
                  (error
                   (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                    'niwa
                    (concat
                     (format
                      "Session restored, but the transcript buffer did not install: %s"
                      (error-message-string install-error))
                     (and install-backtrace (concat "\n" install-backtrace)))
                    :warning))))
              (condition-case instruction-error
                  ;; The store owns the notes: settle it for the restored turn
                  ;; count and observe it, instead of loading a historical set.
                  (niwa-session-artifacts-load-instructions session)
                (error
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                  'niwa
                  (format
                   "Session restored, but instructions did not refresh: %s"
                   (error-message-string instruction-error))
                  :warning)))
              (condition-case refresh-error
                  (progn
                    (when plan
                      (niwa-session-rewind--refresh-restored-buffers
                       plan (list :succeeded (length plan))))
                    (niwa-session-recovery-refresh-session-buffers session))
                (error
                 (niwa-diagnostic-warning (niwa-diagnostic-scope session)
                  'niwa
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
                    (niwa-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       (niwa-session-rewind--rollback-restore-files
                        file-backups (car niwa-session-rewind--written-states)))))
            (error
             (push
              (format "Restore rollback authority (%s)"
                      (error-message-string rollback-error))
              rollback-failures))))
        (when (and (null rollback-failures)
                   (niwa-session-pending-publication session))
          (condition-case nil
              (niwa-session-publication-discard-rolled-back session)
            (error nil))))
      (unless rollback-failures
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t)))
      (when rollback-failures
        (let ((reason
               (format "Restore rollback incomplete: %s"
                       (string-join (nreverse rollback-failures) ", "))))
          (condition-case nil
              (niwa-session-recovery-record-failure
               session reason temporary-root)
            (error nil))
          (error "%s; recovery data: %s" reason temporary-root))))))

(defun niwa-session-rewind--restore-confirmation (session entry impact)
  "Return the restore confirmation for SESSION, head ENTRY, and IMPACT.

The prompt names what the restore returns -- turns and captured files --
and how many captured files it would overwrite after an external change,
because coverage is limited to what niwa captured."
  (let* ((plan (plist-get impact :file-plan))
         (overwrites (plist-get impact :external-overwrites)))
    (format "Restore %s to %s (%d turns, %d captured file%s%s%s)? "
            (niwa-session-name session)
            (niwa-session-rewind--published-head-label entry)
            (plist-get impact :restored-turns)
            (length plan)
            (if (= 1 (length plan)) "" "s")
            (if (> overwrites 0)
                (format ", %d externally changed" overwrites)
              "")
            (if plan "; uncaptured effects remain" ""))))

(defun niwa-session-rewind--published-head-label (entry)
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
              (niwa-session-rewind--prompt-label prompt) stamp))
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
(defun niwa-redo ()
  "Restore this session's conversation to a previously published head.

This is the conversation-scope inverse of Rewind: every published head
stays immutable, so the state a Rewind moved away from can be
republished.  Captured working-tree files return with that state;
uncaptured filesystem effects do not come back.  Workspace notes remain
current, rather than returning to their historical state."
  (interactive)
  (let* ((buffer (niwa-session-rewind--command-buffer))
         (session (buffer-local-value 'niwa--session buffer))
         (scope (niwa-diagnostic-scope (or session buffer))))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope buffer) "Active buffer has no niwa session"))
    (niwa-with-diagnostic-errors scope
      (niwa-session-rewind-assert-stable-source session buffer "restoring"))
    (unless (niwa-session-codec-portable-authority-p session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
       "A workspace session is required to restore published heads"))
    (let ((heads (niwa-with-diagnostic-errors scope
                   (niwa-session-rewind-published-heads session))))
      (unless heads
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session has no other published head"))
      (let* ((table (mapcar
                     (lambda (entry)
                       (cons (niwa-session-rewind--published-head-label
                              entry)
                             entry))
                     heads))
             (choice
              (completing-read
               "Restore session to published head: "
               (mapcar #'car table) nil t nil nil (caar table)))
             (entry (cdr (assoc choice table)))
             (scope-choice
              (and entry
                   (niwa-session-rewind--choose-scope
                    "Restore" (niwa-session-rewind--published-head-label entry)))))
        (unless entry
          (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "No published head selected"))
        (unless (eq scope-choice 'abort)
          (niwa-with-diagnostic-errors scope
            (niwa-session-rewind-restore-head
                 session buffer (plist-get entry :head)
                 (unless (eq scope-choice 'conversation)
                   (lambda (impact)
                     (yes-or-no-p
                      (niwa-session-rewind--restore-confirmation
                       session entry impact))))
                 (eq scope-choice 'conversation)
                 (lambda (result failure)
                   (cond
                    (failure (niwa-diagnostic-warning scope 'niwa failure))
                    (result
                     (with-current-buffer buffer
                       (niwa--run-session-start-hooks "restore"))
                     (message "niwa: restored %s to %s"
                              (niwa-session-name session)
                              (plist-get entry :head))))))))))))

(defun niwa-session-rewind-rewind-checkpoint
    (workspace checkpoint &optional buffer)
  "Rewind WORKSPACE to CHECKPOINT, resuming its session when needed.
BUFFER is the already-live execution session when available."
  (let ((session-id (plist-get checkpoint :session-id))
        (turn (plist-get checkpoint :turn)))
    (unless (and (stringp session-id) (natnump turn))
      (user-error "Malformed implementation checkpoint"))
    (unless buffer
      (setq buffer
            (niwa-session-persistence-resume-id workspace session-id)))
    (unless buffer
      (user-error "Execution session is unavailable: %s" session-id))
    (let* ((session (buffer-local-value 'niwa--session buffer))
           (target
            (and session
                 (cl-loop
                  for (_ . candidate) in
                  (niwa-session-rewind--prompt-candidates session)
                  when (= turn (plist-get candidate :cum-turn))
                  return candidate))))
      (unless target
        (user-error "Implementation checkpoint is unavailable: turn %s" turn))
      ;; Rewinding before an implementation discards that attempt, so the
      ;; checkpoint's own turn goes with it.
      (niwa-session-rewind-rewind buffer target 'before))))

;;;###autoload
(defun niwa-session-rewind--redo-availability (session)
  "Return how Rewind describes what SESSION can restore afterwards.

A workspace session's superseded head stays published, so its
conversation and captured files can be restored with `niwa-redo'."
  (if (niwa-session-codec-portable-authority-p session)
      "conversation and captured-file redo"
    "no redo"))

(defun niwa-session-rewind--command-buffer ()
  "Return the session data buffer the current buffer commands."
  (cond
   ((and (boundp 'niwa--data-buffer) niwa--data-buffer
         (buffer-live-p niwa--data-buffer))
    niwa--data-buffer)
   ((and (boundp 'niwa--session) niwa--session)
    (current-buffer))
   (t (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not in a niwa chat or view buffer"))))

(defun niwa-rewind ()
  "Pick one of your prompts and Rewind the session to just before it.

The prompt you pick is discarded with everything after it, so the
session returns to the moment before you sent it -- which is what makes
re-asking it possible.  To keep a turn and discard only what follows,
Rewind from the view with point on that turn's response."
  (interactive)
  (let* ((buffer (niwa-session-rewind--command-buffer))
         (session (buffer-local-value 'niwa--session buffer))
         (scope (niwa-diagnostic-scope (or session buffer))))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope buffer) "Active buffer has no niwa session"))
    (niwa-with-diagnostic-errors scope
      (niwa-session-rewind-assert-stable-source
       session buffer "rewinding"))
    (let* ((candidates
            (niwa-with-diagnostic-errors scope
              (niwa-session-rewind--prompt-candidates session))))
      (unless candidates
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session has no recorded user prompts"))
      (let* ((lookup (make-hash-table :test #'equal)))
        (dolist (c candidates)
          (puthash (car c) (cdr c) lookup))
        (let* ((collection
                (niwa-session-rewind--prompt-collection-fn
                 candidates lookup))
               (default (caar (last candidates)))
               (chosen  (completing-read
                         "Rewind to before prompt: " collection nil t
                         nil 'niwa-session-rewind--prompt-history
                         default))
               (target (gethash chosen lookup)))
          (when target
            (niwa-with-diagnostic-errors scope
              (niwa-session-rewind-rewind buffer target 'before 'choose))))))))


(provide 'niwa-session-rewind)

;;; niwa-session-rewind.el ends here
