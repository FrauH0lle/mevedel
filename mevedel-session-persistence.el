;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persistence layer for mevedel chat sessions.
;;
;; Handles sidecar serialization, lazy materialization, per-turn
;; autosave, per-session file-history snapshots, split-on-compact
;; segment rotation, lock files (with cross-host conflict handling),
;; resume, rewind, fork-on-send, session rename, workspace
;; relocation reconciliation, and auto-cleanup of stale sessions.
;;
;; Sidecar plist shape:
;;
;;   (:version "v0.5.0"
;;    :session-id "main-2026-04-23T14-30-a9f2"
;;    :session-name "main"
;;    :workspace (:type project :id ID :root ROOT :name NAME)
;;    :created-at "..." :updated-at "..."
;;    :current-segment 3 :total-turn-count 47
;;    :first-user-message "..."
;;    :latest-user-message "..."
;;    :task-status-notes ((nil :note "..." :updated-turn 12) ...)
;;    :forked-from-session-id nil :forked-from-turn nil
;;    :permission-mode default
;;    :permission-rules ((TOOL-NAME ...) ...)
;;    :additional-roots (("name" . "/abs/path") ...)
;;    :prompt-index ((SEGMENT-N . ((:turn N :pos POS :preview STR :timestamp STR) ...)) ...)
;;    :file-snapshots ((TURN-N . ((PATH . (:backup-name STR :version INT
;;                                          :backup-time STR :file-mtime STR)) ...)) ...))
;;
;; Hash-table-valued slots on the session struct (`touched-files',
;; `mentions-shown') are NOT persisted.  Both reset to empty hash
;; tables on load; the consequence is that an LLM coming back from a
;; resume may re-Read files that were already read pre-resume (over-
;; dedup is worse than re-expansion).  Tasks are serialized as plists
;; in `:tasks' and deserialized on load.  Owner-scoped task status
;; notes are serialized in `:task-status-notes'.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

(require 'mevedel-transcript)

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-task-status-notes
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-turn "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--create "mevedel-structs" (&rest slots))
(declare-function mevedel-task-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-subject "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-description "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-owner "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-blocks "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-blocked-by "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-completed-turn "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-task--create "mevedel-structs" (&rest slots))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-view-history'
(declare-function mevedel-view-history-load
                  "mevedel-view-history" (&optional session))
(declare-function mevedel-view-history-save
                  "mevedel-view-history" (&optional view-buffer))
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-get-or-create "mevedel-structs"
                  (type id root name))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))
(declare-function mevedel-request-file-snapshots
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-buffer-name
                  "mevedel-structs" (session-name workspace))
(declare-function mevedel--chat-buffer-init-common
                  "mevedel-chat" (buf workspace))
(declare-function mevedel--chat-buffer-disable-org-element-cache
                  "mevedel-chat" ())
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view" (&optional view-buffer))
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-set-activity
                  "mevedel-agents" (invocation activity))
(declare-function mevedel-tools--agent-invocation-at "mevedel-tool-ui" (fsm))
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel--current-request)
(defvar mevedel-workspace-additional-roots)
(defvar mevedel-tools--agents-fsm)
(defvar gptel-mode)
(defvar gptel-display-buffer-action)
(defvar gptel--preset)
(defvar gptel-system-prompt)
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel--get-buffer-bounds "ext:gptel" ())
(declare-function gptel--save-state "ext:gptel" ())
(declare-function advice-member-p "nadvice" (advice symbol))
(declare-function advice-add "nadvice" (symbol where function &optional props))
(defvar so-long-predicate)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-org--restore-state "ext:gptel-org" ())
;; `org'
(defvar org-agenda-file-menu-enabled)
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get
                  "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (epom property value))

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" ())
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)
(defvar mevedel-view--agent-transcript-p)

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

;; `mevedel'
(declare-function mevedel-version "mevedel" (&optional here message))

;; `mevedel-persistence'
(declare-function mevedel--write-instructions-file
                  "mevedel-persistence"
                  (path &optional base-directory write-empty quiet
                        include-original-content))
(declare-function mevedel--load-instructions-file
                  "mevedel-persistence"
                  (path &optional base-directory confirm quiet workspace))


;;
;;; Customization

(defcustom mevedel-sessions-directory (file-name-concat ".mevedel" "sessions")
  "Directory where session persistence files live.

Relative paths resolve against the workspace root at save time;
absolute paths are used as-is.  Mirrors `mevedel-plans-directory'."
  :type 'directory
  :group 'mevedel)

(defcustom mevedel-session-persistence t
  "Master switch for session auto-save and lazy materialization.

When non-nil (default), sessions auto-save on every completed turn
and lazily materialize on first content.  When nil, no on-disk
artifacts appear; explicit `mevedel-save-session' still works."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-file-history-max-snapshots 100
  "Maximum number of per-turn file snapshots retained per session.

Snapshots beyond this cap are evicted oldest-first; backup files no
longer referenced by any retained snapshot are deleted (refcount GC).
`nil' disables eviction (snapshots accumulate indefinitely)."
  :type '(choice (integer :tag "Max snapshots")
          (const :tag "No limit" nil))
  :group 'mevedel)

(defcustom mevedel-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual file snapshots.

Files larger than this are skipped at snapshot time with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions whose `:updated-at' is older than this are eligible for
deletion when `mevedel-resume' runs (throttled per Emacs invocation).
Sessions with active locks are always skipped: any cross-host lock, or
a same-host lock whose PID is live and not known to have been reused.  A
nil value disables auto-cleanup entirely."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'mevedel)


;;
;;; Constants

(defconst mevedel-session-persistence--allowed-permission-actions
  '(allow deny ask)
  "Permission rule actions recognised by this version.
Rules with other actions are dropped on load (a future version may
add more, and we don't want to act on actions we don't understand).")


;;
;;; Workspace serialization

(defun mevedel-session-persistence--workspace-to-plist (workspace)
  "Convert WORKSPACE to a plist for sidecar storage.

Captures only the identity tuple needed to re-register the workspace on
load: type, id, root, and name.  The file cache is process-local and
not persisted."
  (when workspace
    (list :type (mevedel-workspace-type workspace)
          :id   (mevedel-workspace-id workspace)
          :root (mevedel-workspace-root workspace)
          :name (mevedel-workspace-name workspace))))

(defun mevedel-session-persistence--workspace-from-plist (plist)
  "Resolve WORKSPACE from PLIST.

Looks up the workspace in the registry by (type . id); if not found,
registers a fresh one from the saved tuple.  The current registry's
`root' wins if it differs from the saved root (workspace relocation
is handled by the read path, not here)."
  (when plist
    (mevedel-workspace-get-or-create
     (plist-get plist :type)
     (plist-get plist :id)
     (plist-get plist :root)
     (plist-get plist :name))))


;;
;;; Permission rule hygiene

(defun mevedel-session-persistence--filter-permission-rules (rules)
  "Drop RULES whose `:action' is unrecognised.
A rule is `(TOOL-NAME &rest PLIST)' with `:action SYMBOL'."
  (cl-remove-if-not
   (lambda (rule)
     (and (consp rule)
          (memq (plist-get (cdr rule)
                           :action)
                mevedel-session-persistence--allowed-permission-actions)))
   rules))


;;
;;; Working directory restore

(defun mevedel-session-persistence--working-directory-from-plist
    (plist workspace)
  "Return PLIST's restored working directory for WORKSPACE.

Missing `:working-directory' values fall back to the workspace root for
old sidecars.  When WORKSPACE is available, paths saved under a
relocated workspace root are first mapped to the current root unless
they already live under that current root, then checked with the same
containment semantics as session creation."
  (let* ((raw (or (plist-get plist :working-directory)
                  (and workspace
                       (mevedel-workspace-root workspace))))
         (saved-workspace (plist-get plist :workspace))
         (saved-root (plist-get saved-workspace :root))
         (current-root (and workspace
                            (mevedel-workspace-root workspace)))
         (saved-prefix (and saved-root
                            (file-name-as-directory
                             (expand-file-name saved-root))))
         (current-prefix (and current-root
                              (file-name-as-directory
                               (expand-file-name current-root))))
         (dir (and raw
                   (file-name-as-directory
                    (expand-file-name raw)))))
    (when (and dir saved-prefix current-prefix
               (not (equal saved-prefix current-prefix))
               (not (string-prefix-p current-prefix dir))
               (string-prefix-p saved-prefix dir))
      (setq dir (concat current-prefix
                        (substring dir (length saved-prefix)))))
    (when (and dir workspace)
      (let ((root (or current-prefix
                      (file-name-as-directory
                       (expand-file-name (mevedel-workspace-root workspace))))))
        (unless (file-in-directory-p dir root)
          (user-error "Working directory must be inside workspace root %s"
                      root))))
    dir))


;;
;;; Task serialization

(defun mevedel-session-persistence--task-owner-from-plist (plist)
  "Return PLIST's task owner as a non-empty string or nil."
  (let ((owner (plist-get plist :owner)))
    (and (stringp owner)
         (not (string-empty-p owner))
         owner)))

(defun mevedel-session-persistence--task-to-plist (task)
  "Serialize TASK struct to a plist."
  (list :id          (mevedel-task-id task)
        :subject     (mevedel-task-subject task)
        :description (mevedel-task-description task)
        :status      (mevedel-task-status task)
        :owner       (mevedel-task-owner task)
        :blocks      (mevedel-task-blocks task)
        :blocked-by  (mevedel-task-blocked-by task)
        :completed-turn (mevedel-task-completed-turn task)
        :metadata    (mevedel-task-metadata task)))

(defun mevedel-session-persistence--task-from-plist (plist)
  "Reconstruct a `mevedel-task' from PLIST."
  (mevedel-task--create
   :id          (plist-get plist :id)
   :subject     (plist-get plist :subject)
   :description (plist-get plist :description)
   :status      (plist-get plist :status)
   :owner       (mevedel-session-persistence--task-owner-from-plist plist)
   :blocks      (plist-get plist :blocks)
   :blocked-by  (plist-get plist :blocked-by)
   :completed-turn (plist-get plist :completed-turn)
   :metadata    (plist-get plist :metadata)))


;;
;;; Top-level serialize / deserialize

(cl-defun mevedel-session-persistence-serialize (session
                                                 &key
                                                 first-user-message
                                                 latest-user-message
                                                 additional-roots)
  "Serialize SESSION to a sidecar plist.

FIRST-USER-MESSAGE is the legacy cached preview string.
LATEST-USER-MESSAGE is the cached resume picker preview.
ADDITIONAL-ROOTS is the buffer-local value of
`mevedel-workspace-additional-roots' for this session.

The resulting plist is round-trippable via
`mevedel-session-persistence-deserialize'."
  (list
   :version                (mevedel-version)
   :session-id             (mevedel-session-session-id session)
   :session-name           (mevedel-session-name session)
   :workspace              (mevedel-session-persistence--workspace-to-plist
                            (mevedel-session-workspace session))
   :working-directory      (or (mevedel-session-working-directory session)
                               (mevedel-workspace-root
                                (mevedel-session-workspace session)))
   :created-at             (mevedel-session-created-at session)
   :updated-at             (mevedel-session-updated-at session)
   :current-segment        (or (mevedel-session-current-segment session) 1)
   :total-turn-count       (or (mevedel-session-turn-count session) 0)
   :last-task-write-turn   (mevedel-session-last-task-write-turn session)
   :task-status-notes      (mevedel-session-task-status-notes session)
   :first-user-message     first-user-message
   :latest-user-message    latest-user-message
   :forked-from-session-id (mevedel-session-forked-from-session-id session)
   :forked-from-turn       (mevedel-session-forked-from-turn session)
   :permission-mode        (mevedel-session-permission-mode session)
   :permission-rules       (mevedel-session-permission-rules session)
   :last-observed-date     (mevedel-session-last-observed-date session)
   :agent-types-snapshot   (mevedel-session-agent-types-snapshot session)
   :additional-roots       additional-roots
   :tasks                  (mapcar #'mevedel-session-persistence--task-to-plist
                                   (mevedel-session-tasks session))
   :prompt-index           (mevedel-session-prompt-index session)
   :file-snapshots         (mevedel-session-file-snapshots session)
   :agent-transcripts      (mevedel-session-agent-transcripts session)
   :plan-metadata          (mevedel-session-plan-metadata session)
   ;; Inbound mailbox.  Background sub-agents push agent-result
   ;; blocks here when they finalize; if Emacs restarts before the
   ;; parent's next WAIT drains them, the messages would otherwise
   ;; be lost.  Each message is a plist with :from, :body and
   ;; :timestamp -- prin1/read clean.
   :messages               (mevedel-session-messages session)))

(defun mevedel-session-persistence-deserialize (plist)
  "Reconstruct a session from sidecar PLIST.

Returns a plist:
  (:session SESSION
   :first-user-message STR-OR-NIL
   :latest-user-message STR-OR-NIL
   :additional-roots ALIST)

Where SESSION is a freshly-created `mevedel-session' struct populated
from PLIST.  The auxiliary fields (first-user-message,
latest-user-message, additional-roots) are returned alongside because
they are not on the session struct.

The PLIST is run through `mevedel-session-persistence--patch-sidecar'
first for version migration.  Permission rules with unknown actions
are dropped via the hygiene filter."
  (let* ((plist    (mevedel-session-persistence--patch-sidecar plist))
         (workspace (mevedel-session-persistence--workspace-from-plist
                     (plist-get plist :workspace)))
         (working-directory
          (mevedel-session-persistence--working-directory-from-plist
           plist workspace))
         (tasks     (mapcar #'mevedel-session-persistence--task-from-plist
                            (plist-get plist :tasks)))
         (rules     (mevedel-session-persistence--filter-permission-rules
                     (plist-get plist :permission-rules)))
         (prompt-index (plist-get plist :prompt-index))
         (latest-user-message
          (or (plist-get plist :latest-user-message)
              (mevedel-session-persistence--latest-user-message-from-index
               prompt-index)))
         (session   (mevedel-session--create
                     :name             (plist-get plist :session-name)
                     :workspace        workspace
                     :working-directory working-directory
                     :touched-files    (make-hash-table :test #'equal)
                     :mentions-shown   (make-hash-table :test #'equal)
                     :tasks            tasks
                     :permission-rules rules
                     :permission-mode  (or (plist-get plist :permission-mode)
                                           'default)
                     :turn-count       (or (plist-get plist :total-turn-count) 0)
                     :last-observed-date
                     (or (plist-get plist :last-observed-date)
                         (format-time-string "%F"))
                     :agent-types-snapshot
                     (if (plist-member plist :agent-types-snapshot)
                         (plist-get plist :agent-types-snapshot)
                       :uninitialized)
                     :last-task-write-turn
                     (plist-get plist :last-task-write-turn)
                     :task-status-notes
                     (plist-get plist :task-status-notes)
                     :session-id       (plist-get plist :session-id)
                     :created-at       (plist-get plist :created-at)
                     :updated-at       (plist-get plist :updated-at)
                     :current-segment  (or (plist-get plist :current-segment) 1)
                     :forked-from-session-id
                     (plist-get plist :forked-from-session-id)
                     :forked-from-turn (plist-get plist :forked-from-turn)
                     :prompt-index     prompt-index
                     :file-snapshots   (plist-get plist :file-snapshots)
                     :plan-metadata    (plist-get plist :plan-metadata)
                     :agent-transcripts
                     (mevedel-session-persistence--sanitize-agent-transcripts
                      (plist-get plist :agent-transcripts))
                     :messages
                     (mevedel-session-persistence--sanitize-messages
                      (plist-get plist :messages)))))
    (list :session             session
          :first-user-message  (plist-get plist :first-user-message)
          :latest-user-message latest-user-message
          :additional-roots    (plist-get plist :additional-roots))))


;;
;;; Sidecar IO

(defun mevedel-session-persistence-write (path plist)
  "Write sidecar PLIST to PATH atomically.
Uses a temp file created in PATH's own directory so the final
`rename-file' stays within the same filesystem and is POSIX-atomic
even on setups where the workspace lives on a different mount from
the system temp directory."
  (let* ((dir (file-name-directory (expand-file-name path)))
         (tmp (make-temp-file (expand-file-name
                               ".mevedel-session-meta-" dir)
                              nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-length nil)
                  (print-level nil)
                  (print-quoted t))
              (prin1 plist (current-buffer))))
          (rename-file tmp path t))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun mevedel-session-persistence-read (path)
  "Read sidecar plist from PATH.
Returns the raw plist.  Caller is responsible for passing it through
`mevedel-session-persistence-deserialize' (which applies the version
patch and hygiene filters)."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))


;;
;;; Version migration

(defun mevedel-session-persistence--patch-sidecar (plist)
  "Patch sidecar PLIST forward to the current `mevedel-version'.

Each older version branch should rewrite PLIST in place and fall through
to the current version.  When the version matches `mevedel-version',
returns PLIST unchanged.  Mirrors `mevedel--patch-save-file' in
`mevedel-persistence.el'."
  (let ((version (plist-get plist :version)))
    (cond
     ((string= version (mevedel-version))
      plist)
     (t
      ;; No older versions to migrate yet.  Future versions add a
      ;; pcase here that rewrites PLIST and updates :version.
      (plist-put plist :version (mevedel-version))))))


;;
;;; Sub-agent transcript helpers

(defun mevedel-session-persistence--validate-transcript-path (path save-path)
  "Return non-nil if PATH is safe under SAVE-PATH's `agents/' subdir.

Used by sidecar load and by the view renderer when deciding whether
to expose a transcript-open affordance.  Rules: PATH must be a
non-empty relative string with no `..' segments and must end in
`.chat.org'; once resolved against SAVE-PATH it must remain under
`<SAVE-PATH>/agents/'."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (string-match-p "\\(?:^\\|/\\)\\.\\.\\(?:/\\|$\\)" path))
       (string-suffix-p ".chat.org" path)
       (let* ((agents-dir (file-name-as-directory
                           (expand-file-name "agents" save-path)))
              (resolved (expand-file-name path save-path)))
         (string-prefix-p agents-dir
                          (file-name-as-directory
                           (file-name-directory resolved))))))

(defun mevedel-session-persistence--sanitize-messages (raw)
  "Sanitize the inbound mailbox RAW read from a sidecar.

Drops entries that aren't well-formed plists with a :from string
and a :body string.  Preserves arrival order so the next WAIT
delivers them in the order they were originally pushed."
  (cl-loop for entry in (and (listp raw) raw)
           when (and (listp entry)
                     (stringp (plist-get entry :from))
                     (stringp (plist-get entry :body)))
           collect entry))

(defun mevedel-session-persistence--sanitize-agent-transcripts (raw)
  "Sanitize the `:agent-transcripts' alist RAW read from a sidecar.

Drops entries whose paths fail validation.  Coerces unknown status
values to `incomplete'.  Deduplicates duplicate agent-ids by keeping
the entry with the newest `:updated-at'.  Preserves unknown plist
keys for forward compatibility -- they round-trip but are ignored at
render time."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (entry (and (listp raw) raw))
      (when (and (consp entry)
                 (stringp (car entry))
                 (listp (cdr entry)))
        (let* ((id    (car entry))
               (plist (copy-sequence (cdr entry)))
               (status (plist-get plist :status))
               (existing (gethash id seen)))
          (unless (memq status '(running completed error aborted incomplete))
            (setq plist (plist-put plist :status 'incomplete)))
          (cond
           ((null existing)
            (puthash id plist seen)
            (push (cons id plist) out))
           ((let ((a (plist-get plist :updated-at))
                  (b (plist-get existing :updated-at)))
              (and (stringp a) (stringp b) (string> a b)))
            (puthash id plist seen)
            (setf (alist-get id out nil nil #'equal) plist))))))
    (nreverse out)))

(defun mevedel-session-persistence--mark-running-incomplete-on-resume
    (session readonly-p)
  "Rewrite `running' transcript entries on SESSION to `incomplete'.

Skips the rewrite when READONLY-P is non-nil -- another live writer
holds the lock and rewriting would corrupt its audit log."
  (unless readonly-p
    (let ((entries (mevedel-session-agent-transcripts session))
          changed)
      (dolist (entry entries)
        (when (and (consp entry)
                   (eq (plist-get (cdr entry) :status) 'running))
          (setf (cdr entry)
                (plist-put (cdr entry) :status 'incomplete))
          (setq changed t)))
      (when changed
        (setf (mevedel-session-agent-transcripts session) entries)))))

(defun mevedel-session-persistence--prune-agent-transcripts-after-fork
    (session fork-turn)
  "Drop transcript entries whose `:parent-turn' exceeds FORK-TURN."
  (let ((entries (mevedel-session-agent-transcripts session)))
    (setf (mevedel-session-agent-transcripts session)
          (cl-remove-if (lambda (entry)
                          (let ((pt (plist-get (cdr entry) :parent-turn)))
                            (and (integerp pt) (> pt fork-turn))))
                        entries))))

(defun mevedel-session-persistence--shallow-ensure-files (session buffer)
  "Materialize SESSION's session directory and lock without writing the sidecar.

Used by sub-agent allocation: a sub-agent can spawn during
the parent's first turn (before any DONE handler has run), so we
need the session directory and `agents/' subdirectory but must not
write `session.meta.el' yet.  On-disk session state reflects a
completed turn boundary.  The parent's first DONE autosave will
write the sidecar later, picking up any
sub-agent transcript entries that accumulated in the in-memory
slot.

Returns SESSION's `save-path' on success, nil on failure or when
persistence is disabled.  Idempotent."
  (when mevedel-session-persistence
    (or (mevedel-session-save-path session)
      (condition-case err
          (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                                (mevedel-session-workspace session)))
                 (session-id
                  (let ((base (mevedel-session-name session))
                        (attempts 0)
                        candidate)
                    (while (progn
                             (setq candidate
                                   (mevedel-session-persistence--compute-id base))
                             (file-directory-p
                              (file-name-concat sessions-dir candidate)))
                      (cl-incf attempts)
                      (when (> attempts 32)
                        (error "Could not allocate a unique session id after %d attempts"
                               attempts)))
                    candidate))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (mevedel-session-persistence--segment-path
                                save-path 1))
                 (now (format-time-string "%FT%H-%M-%S")))
            (make-directory save-path t)
            (make-directory (file-name-concat save-path "agents") t)
            (make-directory (file-name-concat save-path "file-history") t)
            (mevedel-session-persistence-lock-acquire
             save-path (buffer-name buffer))
            (setf (mevedel-session-session-id session)      session-id)
            (setf (mevedel-session-save-path session)       save-path)
            (setf (mevedel-session-created-at session)      now)
            (setf (mevedel-session-updated-at session)      now)
            (setf (mevedel-session-current-segment session) 1)
            (require 'mevedel-workspace)
            (mevedel-workspace-ensure-generated-state-ignored
             (mevedel-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path)))
            save-path)
        (error
         (message "mevedel: shallow session materialization failed: %S" err)
         nil)))))

(defun mevedel-session-persistence--record-running-transcript
    (session entry)
  "Insert ENTRY into SESSION's agent-transcripts.  ENTRY is (ID . PLIST)."
  (when (and session (consp entry))
    (setf (alist-get (car entry)
                     (mevedel-session-agent-transcripts session)
                     nil nil #'equal)
          (cdr entry))))

(defun mevedel-session-persistence--update-transcript-entry
    (session agent-id updates)
  "Merge UPDATES (a plist) into SESSION's transcript entry for AGENT-ID."
  (when (and session agent-id)
    (let ((existing (alist-get agent-id
                               (mevedel-session-agent-transcripts session)
                               nil nil #'equal)))
      (when existing
        (let ((merged (copy-sequence existing)))
          (cl-loop for (k v) on updates by #'cddr do
                   (setq merged (plist-put merged k v)))
          (setf (alist-get agent-id
                           (mevedel-session-agent-transcripts session)
                           nil nil #'equal)
                merged))))))

(defun mevedel-session-persistence--write-sidecar-now (session buffer)
  "Best-effort sidecar rewrite for SESSION.

Only writes when the sidecar file already exists on disk -- i.e.
the parent's first DONE has fired and a full materialization has
written `session.meta.el'.  Before that, the session is in shallow
materialization mode (directory + lock + agents/ but no sidecar)
and writing now would violate the completed-turn boundary contract.
In that case the write is deferred to the parent's DONE autosave;
the in-memory `agent-transcripts' slot still reflects current state
and will be picked up by that autosave."
  (when (and session (mevedel-session-save-path session))
    (let ((sidecar (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session))))
      (when (file-exists-p sidecar)
        (condition-case err
            (mevedel-session-persistence-write
             sidecar
             (mevedel-session-persistence--build-sidecar session buffer))
          (error
           (message "mevedel: sidecar rewrite failed: %S" err)
           nil)
          (:success t))))))


;;
;;; Session id and paths

(defun mevedel-session-persistence--sanitize (name)
  "Return NAME with everything outside `[A-Za-z0-9_-]' replaced with `_'."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (or name "")))

(defun mevedel-session-persistence--short-uuid ()
  "Return 4 hex chars derived from random + monotonic clock entropy."
  (substring
   (secure-hash 'sha256
                (format "%s-%s-%s"
                        (random most-positive-fixnum)
                        (float-time)
                        (emacs-pid)))
   0 4))

(defun mevedel-session-persistence--compute-id (name)
  "Compute a fresh session id from NAME.

Format: `<sanitized-name>-<ISO-timestamp>-<short-uuid>'.  ISO timestamp
uses dashes throughout (no colons) so it works on every filesystem and
sorts lexicographically."
  (format "%s-%s-%s"
          (mevedel-session-persistence--sanitize name)
          (format-time-string "%FT%H-%M")
          (mevedel-session-persistence--short-uuid)))

(defun mevedel-session-persistence--sessions-dir (workspace)
  "Return the absolute sessions directory for WORKSPACE.

Resolves `mevedel-sessions-directory' against WORKSPACE's root if the
defcustom is relative; otherwise uses it as-is."
  (let ((dir mevedel-sessions-directory))
    (if (file-name-absolute-p dir)
        (expand-file-name dir)
      (expand-file-name dir (mevedel-workspace-root workspace)))))

(defun mevedel-session-persistence--segment-path (save-path n)
  "Return the absolute path to segment number N under SAVE-PATH.

Segments are zero-padded to four digits (`segment-0001.chat.org')."
  (file-name-concat save-path
                    (format "segment-%04d.chat.org" n)))

(defun mevedel-session-persistence--sidecar-path (save-path)
  "Return the absolute path to the session sidecar under SAVE-PATH."
  (file-name-concat save-path "session.meta.el"))


;;
;;; Prompt index (used by the rewind picker)

(defun mevedel-session-persistence--content-start (buffer)
  "Return the first buffer position past BUFFER's leading metadata.
Skips an initial org property drawer (:PROPERTIES: ... :END:) so the
prompt walker does not treat the drawer as a user prompt.  Returns
`point-min' when no drawer is present."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (progn (forward-line 1) (point))
              (point-min))
          (point-min))))))

(defun mevedel-session-persistence--collect-prompts (buffer)
  "Return a list of `(:turn N :pos POS :preview STR)' plists for BUFFER.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Turns are numbered 1, 2, ... in document order.  Used at
save time to refresh the live segment's entry in
`mevedel-session-prompt-index'.

Skips the initial org property drawer (via
`mevedel-session-persistence--content-start') and any content inside
`#+begin_summary' / `#+end_summary' blocks (the compaction summary
has its body stripped of the `gptel' property but is not a user
prompt).  Also skips unpropertized gptel org tool/reasoning block glue."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let ((content-start
                 (mevedel-session-persistence--content-start buffer))
                (turn 0)
                (results nil))
            (dolist (seg (mevedel-transcript--extract-segments
                          content-start
                          (point-max)))
              (pcase-let ((`(,type ,seg-start ,seg-end) seg))
                (when (eq type 'user)
                  (when-let* ((prompt-start
                               (mevedel-transcript--user-prompt-start
                                (max seg-start content-start) seg-end nil)))
                    (let ((text (buffer-substring-no-properties
                                 prompt-start seg-end)))
                      (when (string-match "[^[:space:]].*$" text)
                        (cl-incf turn)
                        (push (list :turn turn
                                    :pos prompt-start
                                    :preview (truncate-string-to-width
                                              (match-string 0 text)
                                              80 nil nil "..."))
                              results)))))))
            (nreverse results)))))))

(defun mevedel-session-persistence--prompt-count-in-text (text)
  "Return the number of user prompts detected in TEXT."
  (if (string-empty-p (or text ""))
    0
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (insert text)
      (length (mevedel-session-persistence--collect-prompts
               (current-buffer))))))

(defun mevedel-session-persistence--segment-tail-prompt-count ()
  "Return the copied-tail prompt count recorded on the current segment."
  (if (derived-mode-p 'org-mode)
      (max 0 (string-to-number
              (or (org-entry-get (point-min)
                                 "MEVEDEL_SEGMENT_TAIL_PROMPTS")
                  "0")))
    0))

(defun mevedel-session-persistence--segment-tail-prompt-count-for-session
    (session segment-n)
  "Return copied-tail prompt count for SESSION's SEGMENT-N file."
  (let ((path (and (mevedel-session-save-path session)
                   (mevedel-session-persistence--segment-path
                    (mevedel-session-save-path session) segment-n))))
    (if (and path (file-exists-p path))
        (with-temp-buffer
          (let ((org-agenda-file-menu-enabled nil))
            (org-mode))
          (insert-file-contents path nil 0 8192)
          (mevedel-session-persistence--segment-tail-prompt-count))
      0)))

(defun mevedel-session-persistence--update-prompt-index (session buffer)
  "Refresh the live segment's prompt list in SESSION from BUFFER's contents.

Operates only on the current segment; previous (finalized) segments
keep their pre-recorded entries.  Idempotent -- safe to call on every
save.

Each prompt plist gets a `:cum-turn' field equal to the prompt's
sequence number across the entire session.  Prompts copied forward as
compaction tail are skipped because they are already indexed in the
predecessor segment.  The cumulative
number is what `:file-snapshots' is keyed by, so the restore plan
can map a picker selection back to the snapshot taken right after
that prompt's response completed."
  (let* ((current-seg (or (mevedel-session-current-segment session) 1))
         (index       (mevedel-session-prompt-index session))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (raw-all     (mevedel-session-persistence--collect-prompts buffer))
         (tail-count  (with-current-buffer buffer
                        (mevedel-session-persistence--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (with-cum    (cl-loop for p in raw
                               for turn from 1
                               collect
                               (let ((copy (copy-sequence p)))
                                 (plist-put copy :turn turn)
                                 (plist-put copy :file-turn
                                            (+ skip-count turn))
                                 (plist-put copy :cum-turn (+ offset turn))
                                 copy)))
         (cell        (assoc current-seg index)))
    (if cell
        (setcdr cell with-cum)
      (setf (mevedel-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))))

(defun mevedel-session-persistence--newer-prompt-p (candidate incumbent)
  "Return non-nil when CANDIDATE is newer than INCUMBENT."
  (if (null incumbent)
      t
    (let ((candidate-cum (plist-get candidate :cum-turn))
          (incumbent-cum (plist-get incumbent :cum-turn))
          (candidate-segment (or (plist-get candidate :segment) 0))
          (incumbent-segment (or (plist-get incumbent :segment) 0))
          (candidate-turn (or (plist-get candidate :turn) 0))
          (incumbent-turn (or (plist-get incumbent :turn) 0)))
      (cond
       ((and (numberp candidate-cum)
             (numberp incumbent-cum)
             (/= candidate-cum incumbent-cum))
        (> candidate-cum incumbent-cum))
       ((/= candidate-segment incumbent-segment)
        (> candidate-segment incumbent-segment))
       (t
        (> candidate-turn incumbent-turn))))))

(defun mevedel-session-persistence--latest-user-message-from-index (index)
  "Return the newest non-empty prompt preview from prompt INDEX, or nil."
  (let (best)
    (dolist (entry index)
      (let ((segment (car entry)))
        (dolist (prompt (cdr entry))
          (when (consp prompt)
            (let ((preview (plist-get prompt :preview)))
              (when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
                (let ((candidate (copy-sequence prompt)))
                  (plist-put candidate :segment segment)
                  (when (mevedel-session-persistence--newer-prompt-p
                         candidate best)
                    (setq best candidate)))))))))
    (plist-get best :preview)))


;;
;;; First user message extraction

(defun mevedel-session-persistence--first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Skips the initial org property drawer and
`#+begin_summary' / `#+end_summary' block bodies, so the picker preview
reflects an actual user prompt rather than metadata, a compaction
summary, or tool/reasoning block glue.  The preview is the first
non-empty line, truncated to 120 characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (catch 'found
            (let ((pos (mevedel-session-persistence--content-start buffer)))
              (while (< pos (point-max))
                (let* ((next (next-single-property-change
                              pos 'gptel nil (point-max)))
                       (prop (get-text-property pos 'gptel)))
                  (when-let* ((prompt-start
                               (mevedel-transcript--user-prompt-start
                                pos next prop)))
                    (let ((text (buffer-substring-no-properties
                                 prompt-start next)))
                      (when (string-match "[^[:space:]].*$" text)
                        (let ((line (match-string 0 text)))
                          (throw 'found
                                 (truncate-string-to-width
                                  line 120 nil nil "..."))))))
                  (setq pos next)))
              nil)))))))


;;
;;; Buffer selection

(defun mevedel-session-persistence--authoritative-buffer (buffer)
  "Return the authoritative session data buffer for BUFFER.
View buffers are reconstructable UI projections and must never become
session segment buffers.  Interactive chat views persist through their
`mevedel--data-buffer'; transcript inspection views are not session
transcript buffers and return nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'mevedel-view-mode)
        (let ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer)))
          (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
               data-buf
               (buffer-live-p data-buf)
               data-buf)))
       (t buffer)))))


;;
;;; Lazy materialization

(defun mevedel-session-persistence-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id,
create the session directory tree (session dir + `agents/' +
`file-history/'), acquire the `.lock' file, set BUFFER's
`buffer-file-name' to the first segment file, and save the buffer.

Does NOT write the sidecar -- the caller (always
`mevedel-session-persistence-save') is expected to do that once it
has updated the prompt-index and snapshot maps.  Keeping the write
in one place avoids double-writing the sidecar on first save.

Idempotent -- if SESSION already has a `save-path', repairs BUFFER's
`buffer-file-name' so it visits the current segment before any save.
Returns SESSION's `save-path' (allocated or existing) on success, nil
when persistence is disabled."
  (when mevedel-session-persistence
    (let* ((existing-save-path (mevedel-session-save-path session))
           (save-path
            (or existing-save-path
                (let* ((sessions-dir
                        (mevedel-session-persistence--sessions-dir
                         (mevedel-session-workspace session)))
                       ;; Regenerate the short UUID suffix until the computed
                       ;; id maps to a non-existent directory.  Prevents
                       ;; silent collision into another session's directory
                       ;; when two sessions generated within the same second
                       ;; happen to pick the same 4-hex UUID.
                       (session-id
                        (let ((base (mevedel-session-name session))
                              (attempts 0)
                              candidate)
                          (while (progn
                                   (setq candidate
                                         (mevedel-session-persistence--compute-id
                                          base))
                                   (file-directory-p
                                    (file-name-concat sessions-dir candidate)))
                            (cl-incf attempts)
                            (when (> attempts 32)
                              (error "Could not allocate a unique session id after %d attempts"
                                     attempts)))
                          candidate))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat sessions-dir session-id)))
                       (now (format-time-string "%FT%H-%M-%S")))
                  (make-directory new-save-path t)
                  (make-directory (file-name-concat new-save-path "agents") t)
                  (make-directory (file-name-concat new-save-path "file-history") t)
                  (mevedel-session-persistence-lock-acquire
                   new-save-path (buffer-name buffer))
                  (setf (mevedel-session-session-id session)      session-id)
                  (setf (mevedel-session-save-path session)       new-save-path)
                  (setf (mevedel-session-created-at session)      now)
                  (setf (mevedel-session-updated-at session)      now)
                  (setf (mevedel-session-current-segment session) 1)
                  (require 'mevedel-workspace)
                  (mevedel-workspace-ensure-generated-state-ignored
                   (mevedel-session-workspace session))
                  new-save-path)))
           (segment-number (or (mevedel-session-current-segment session) 1))
           (segment-path (mevedel-session-persistence--segment-path
                          save-path segment-number)))
      (make-directory save-path t)
      (make-directory (file-name-concat save-path "agents") t)
      (make-directory (file-name-concat save-path "file-history") t)
      (require 'mevedel-workspace)
      (mevedel-workspace-ensure-generated-state-ignored
       (mevedel-session-workspace session))
      (with-current-buffer buffer
        (unless (and buffer-file-name
                     (equal (expand-file-name buffer-file-name)
                            (expand-file-name segment-path)))
          (setq buffer-file-name segment-path))
        (unless (file-exists-p segment-path)
          (set-buffer-modified-p t)
          (save-buffer)))
      save-path)))


;;
;;; Sidecar build helper

(defun mevedel-session-persistence--persisted-first-user-message (session)
  "Return SESSION's already persisted first user preview, or nil."
  (when-let* ((save-path (mevedel-session-save-path session))
              (sidecar (mevedel-session-persistence--sidecar-path save-path))
              (_ (file-exists-p sidecar)))
    (condition-case nil
        (plist-get (mevedel-session-persistence-read sidecar)
                   :first-user-message)
      (error nil))))

(defun mevedel-session-persistence--build-sidecar (session buffer)
  "Build the sidecar plist for SESSION using BUFFER for ancillary fields."
  (let* ((first-preview
          (or (mevedel-session-persistence--persisted-first-user-message
               session)
              (mevedel-session-persistence--first-user-message buffer)))
         (latest-preview
          (or (mevedel-session-persistence--latest-user-message-from-index
               (mevedel-session-prompt-index session))
              first-preview))
         (roots (when (buffer-live-p buffer)
                  (buffer-local-value 'mevedel-workspace-additional-roots
                                      buffer))))
    (mevedel-session-persistence-serialize
     session
     :first-user-message  first-preview
     :latest-user-message latest-preview
     :additional-roots   roots)))


;;
;;; Transcript gptel metadata repair

(defun mevedel-session-persistence--gptel-prop-in-range (start end predicate)
  "Return the first `gptel' property in START..END matching PREDICATE."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (let ((prop (get-text-property pos 'gptel)))
        (when (funcall predicate prop)
          (setq found prop)))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    found))

(defun mevedel-session-persistence--tool-id-in-range (start end)
  "Return the first gptel tool id in START..END, or nil."
  (cdr (mevedel-session-persistence--gptel-prop-in-range
        start end
        (lambda (prop)
          (and (consp prop) (eq (car prop) 'tool))))))

(defun mevedel-session-persistence--tool-prop-in-range-p (start end)
  "Return non-nil when START..END already contains a tool `gptel' prop."
  (mevedel-session-persistence--gptel-prop-in-range
   start end
   (lambda (prop)
     (and (consp prop) (eq (car prop) 'tool)))))

(defun mevedel-session-persistence--tool-bound-id-in-gptel-bounds (start end)
  "Return a non-empty `GPTEL_BOUNDS' tool id overlapping START..END, or nil."
  (save-excursion
    (save-match-data
      (when-let* ((raw (org-entry-get (point-min) "GPTEL_BOUNDS"))
                  (bounds (condition-case nil
                              (read raw)
                            (error nil)))
                  (tools (alist-get 'tool bounds)))
        (catch 'found
          (dolist (range tools)
            (when (and (integerp (car-safe range))
                       (integerp (cadr range))
                       (stringp (caddr range))
                       (not (string-empty-p (caddr range)))
                       (< (car range) end)
                       (> (cadr range) start))
              (throw 'found (caddr range)))))))))

(defun mevedel-session-persistence--org-tool-block-start-p (pos)
  "Return non-nil when POS starts a persisted org tool block."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (skip-chars-forward " \t\n")
    (looking-at-p "(\\s-*:name\\_>")))

(defun mevedel-session-persistence--org-tool-block-parts (start end)
  "Return parseable subranges for an org tool block in START..END.

The return value is a plist with `:prefix-start', `:prefix-end',
`:tool-start', `:tool-end', `:suffix-start', and `:suffix-end'.
The tool range starts at the readable `(:name ...)' sexp and excludes
`#+begin_tool' / `#+end_tool' scaffolding so provider parsers can read
it directly.  Return nil when START..END is not a complete parseable
persisted tool block."
  (save-excursion
    (goto-char start)
    (when (looking-at-p "#\\+begin_tool\\b")
      (forward-line 1)
      (skip-chars-forward " \t\n" end)
      (let ((sexp-start (point)))
        (when (and (< sexp-start end)
                   (looking-at-p "(\\s-*:name\\_>"))
          (condition-case nil
              (progn
                (forward-sexp 1)
                (let ((sexp-end (point)))
                  (when (re-search-forward "^#\\+end_tool[^\n]*\n?" end t)
                    (let ((suffix-start (match-beginning 0))
                          (suffix-end (match-end 0)))
                      (when (<= sexp-end suffix-start)
                        (list :prefix-start start
                              :prefix-end sexp-start
                              :tool-start sexp-start
                              :tool-end suffix-start
                              :suffix-start suffix-start
                              :suffix-end suffix-end))))))
            (error nil)))))))

(defun mevedel-session-persistence--clear-gptel-text-props (start end)
  "Clear stale gptel-related text properties from START to END."
  (remove-text-properties
   start end
   '(gptel nil response nil invisible nil front-sticky nil)))

(defun mevedel-session-persistence--structural-gptel-ranges ()
  "Return structural transcript block ranges for property repair.
Each entry is (START END KIND VALUE).  KIND is `tool', `tool-scaffold',
`ignore', or `user'.  User ranges are cleared but get no `gptel'
property.  Tool VALUE is the gptel tool call id when available."
  (let ((content-start (mevedel-session-persistence--content-start
                        (current-buffer)))
        ranges)
    (when (> content-start (point-min))
      (push (list (point-min) content-start 'user nil) ranges))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^#\\+begin_tool\\b"
                      "\\|^<agent-result\\_>"
                      "\\|^<agent-message\\_>"
                      "\\|^<system-reminder>[ \t]*$"
                      "\\|^<queued-user-message-batch\\_>"
                      "\\|^<hook-context>[ \t]*$"
                      "\\|^<!-- mevedel-render-data -->[ \t]*$"
                      "\\|^#\\+begin_reasoning\\b"
                      "\\|^:PROMPT:[ \t]*$")
              nil t)
        (let ((start (match-beginning 0))
              (next (match-end 0))
              end kind value)
          (goto-char start)
          (cond
           ((looking-at-p "#\\+begin_tool\\b")
            (when (re-search-forward "^#\\+end_tool[^\n]*\n?" nil t)
              (let* ((block-end (match-end 0))
                     (parts (mevedel-session-persistence--org-tool-block-parts
                             start block-end))
                     (tool-bound-id
                      (mevedel-session-persistence--tool-bound-id-in-gptel-bounds
                       start block-end)))
                (when (or (mevedel-session-persistence--tool-prop-in-range-p
                           start block-end)
                          tool-bound-id)
                  (setq end block-end
                        kind 'handled)
                  (if parts
                      (progn
                        (setq value
                              (or (mevedel-session-persistence--tool-id-in-range
                                   start block-end)
                                  tool-bound-id
                                  ""))
                        (push (list (plist-get parts :prefix-start)
                                    (plist-get parts :prefix-end)
                                    'ignore nil)
                              ranges)
                        (push (list (plist-get parts :tool-start)
                                    (plist-get parts :tool-end)
                                    'tool value)
                              ranges)
                        (push (list (plist-get parts :suffix-start)
                                    (plist-get parts :suffix-end)
                                    'tool-scaffold nil)
                              ranges))
                    (push (list start block-end 'ignore nil) ranges))))))
           ((looking-at-p "<agent-result\\_>")
            (when (re-search-forward "^</agent-result>[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'user)))
           ((looking-at-p "<agent-message\\_>")
            (when (re-search-forward "^</agent-message>[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'user)))
           ((looking-at-p "<system-reminder>")
            (when (re-search-forward "^</system-reminder>[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'user)))
           ((looking-at-p "<queued-user-message-batch\\_>")
            (when (re-search-forward "^</queued-user-message-batch>[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'user)))
           ((looking-at-p "<hook-context>")
            (when (re-search-forward "^</hook-context>[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'user)))
           ((looking-at-p "<!-- mevedel-render-data -->")
            (when (re-search-forward "^<!-- /mevedel-render-data -->[ \t]*\n?"
                                     nil t)
              (setq end (match-end 0)
                    kind 'ignore)))
           ((looking-at-p "#\\+begin_reasoning\\b")
            (when (re-search-forward "^#\\+end_reasoning[^\n]*\n?" nil t)
              (let ((reasoning-end (match-end 0))
                    (cursor start))
                (goto-char start)
                (while (re-search-forward "^#\\+begin_tool\\b" reasoning-end t)
                  (let ((tool-start (match-beginning 0))
                        tool-bound-id tool-end tool-value parts)
                    (if (and (mevedel-session-persistence--org-tool-block-start-p
                              tool-start)
                             (re-search-forward "^#\\+end_tool[^\n]*\n?"
                                                reasoning-end t)
                             (progn
                               (setq tool-end (match-end 0)
                                     parts
                                     (mevedel-session-persistence--org-tool-block-parts
                                      tool-start tool-end)
                                     tool-bound-id
                                     (mevedel-session-persistence--tool-bound-id-in-gptel-bounds
                                      tool-start tool-end))
                               (and parts
                                    (or (mevedel-session-persistence--tool-prop-in-range-p
                                         tool-start tool-end)
                                        tool-bound-id))))
                        (progn
                          (setq tool-value
                                (or (mevedel-session-persistence--tool-id-in-range
                                     tool-start tool-end)
                                    tool-bound-id
                                    ""))
                          (when (< cursor tool-start)
                            (push (list cursor tool-start 'ignore nil) ranges))
                          (push (list (plist-get parts :prefix-start)
                                      (plist-get parts :prefix-end)
                                      'ignore nil)
                                ranges)
                          (push (list (plist-get parts :tool-start)
                                      (plist-get parts :tool-end)
                                      'tool tool-value)
                                ranges)
                          (push (list (plist-get parts :suffix-start)
                                      (plist-get parts :suffix-end)
                                      'ignore nil)
                                ranges)
                          (setq cursor tool-end)
                          (goto-char tool-end))
                      (goto-char (min (1+ tool-start) reasoning-end)))))
                (when (< cursor reasoning-end)
                  (push (list cursor reasoning-end 'ignore nil) ranges))
                (setq end reasoning-end
                      kind 'handled))))
           ((looking-at-p ":PROMPT:")
            (when (re-search-forward "^:END:[ \t]*\n?" nil t)
              (setq end (match-end 0)
                    kind 'ignore))))
          (cond
           ((eq kind 'handled)
            (goto-char end))
           ((and end kind)
            (push (list start end kind value) ranges)
            (goto-char end))
           (t
            (goto-char next))))))
    (sort ranges (lambda (a b) (< (car a) (car b))))))

(defun mevedel-session-persistence--apply-block-gptel-props (ranges)
  "Apply structural `gptel' properties for RANGES."
  (dolist (range ranges)
    (pcase-let ((`(,start ,end ,kind ,value) range))
      (mevedel-session-persistence--clear-gptel-text-props start end)
      (pcase kind
        ('tool
         (put-text-property start end 'gptel (cons 'tool value)))
        ('ignore
         (put-text-property start end 'gptel 'ignore))
        ('tool-scaffold
         (put-text-property start end 'gptel 'ignore))))))

(defconst mevedel-session-persistence--response-continuation-max-gap 160
  "Maximum structural-to-response prefix size repaired after restore.")

(defun mevedel-session-persistence--structural-gap-prop-p (prop kind)
  "Return non-nil when PROP is stale structural state after KIND."
  (or (null prop)
      (pcase kind
        ('tool
         (or (eq prop 'tool)
             (and (consp prop) (eq (car prop) 'tool))))
        ('ignore
         (eq prop 'ignore))
        ('tool-scaffold
         (or (eq prop 'ignore)
             (eq prop 'tool)
             (and (consp prop) (eq (car prop) 'tool))))
        (_ nil))))

(defun mevedel-session-persistence--first-nonblank-pos (start end)
  "Return the first non-whitespace position in START..END, or nil."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\n\r" end)
    (when (< (point) end)
      (point))))

(defun mevedel-session-persistence--response-continuation-marker-p
    (start end)
  "Return non-nil when START..END contains new transcript structure."
  (save-excursion
    (goto-char start)
    (re-search-forward
     (concat "\\(?:^\\|[\n\r]\\)"
             "\\(?:#\\+begin_"
             "\\|\\*\\*\\* User prompt"
             "\\|<agent-result\\_>"
             "\\|<agent-message\\_>"
             "\\|<system-reminder>"
             "\\|<queued-user-message-batch\\_>"
             "\\|<hook-context>"
             "\\|:PROMPT:\\)")
     end t)))

(defun mevedel-session-persistence--response-continuation-range
    (start kind)
  "Return a stale response prefix range after structural START and KIND.
The returned cons cell covers only the missing prefix, not the existing
response run that follows it."
  (let ((limit (min (point-max)
                    (+ start
                       mevedel-session-persistence--response-continuation-max-gap)))
        (pos start)
        prefix-start response-start done)
    (while (and (< pos limit) (not response-start) (not done))
      (let* ((prop (get-text-property pos 'gptel))
             (next (or (next-single-property-change pos 'gptel nil limit)
                       limit)))
        (cond
         ((eq prop 'response)
          (setq response-start pos))
         ((mevedel-session-persistence--structural-gap-prop-p prop kind)
          (unless prefix-start
            (setq prefix-start
                  (mevedel-session-persistence--first-nonblank-pos pos next))))
         (t
          (setq done t)))
        (setq pos next)))
    (when (and prefix-start
               response-start
               (< prefix-start response-start)
               (not (memq (char-before response-start) '(?\n ?\r)))
               (not (mevedel-session-persistence--response-continuation-marker-p
                     prefix-start response-start))
               (mevedel-transcript--response-continuation-text-p
                (buffer-substring-no-properties prefix-start response-start)))
      (cons prefix-start response-start))))

(defun mevedel-session-persistence--repair-response-continuation-gaps
    (ranges)
  "Repair stale response prefixes immediately after structural RANGES.
When a tool, reasoning block, or mailbox delivery is followed by nil or
stale structural text and the next `gptel' run is a response starting
mid-line, treat that gap as the missing prefix of the response."
  (dolist (range ranges)
    (pcase-let ((`(,_start ,end ,kind ,_value) range))
      (when (and (memq kind '(tool tool-scaffold ignore user))
                 (< end (point-max)))
        (when-let* ((repair
                     (mevedel-session-persistence--response-continuation-range
                      end kind)))
          (add-text-properties
           (car repair) (cdr repair)
           '(gptel response front-sticky (gptel))))))))

(defun mevedel-session-persistence--normalize-gptel-properties ()
  "Normalize transcript `gptel' text properties before saving or rendering.

gptel persists only text-property bounds.  During long sessions, inserted
tool blocks, mailbox deliveries, and hidden context blocks can leave stale
or sticky properties around structural boundaries.  Only markup-delimited
regions are normalized here; plain prose boundaries between user and
assistant turns are not inferred."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (with-silent-modifications
          (let ((ranges (mevedel-session-persistence--structural-gptel-ranges)))
            (mevedel-session-persistence--apply-block-gptel-props ranges)
            (mevedel-session-persistence--repair-response-continuation-gaps
             ranges)))))))

(defun mevedel-session-persistence--restore-gptel-state ()
  "Restore gptel state without dirtying the visited segment buffer.

Resume-time metadata repair can temporarily rewrite `GPTEL_BOUNDS' so
gptel sees valid character positions, and property restoration can touch
many text-property ranges.  Those repairs are derived from the persisted
transcript; by themselves they should not make a just-resumed segment
look like it needs saving."
  (let ((was-modified (buffer-modified-p)))
    (unwind-protect
        (progn
          (mevedel-session-persistence--sanitize-gptel-bounds)
          (unless (bound-and-true-p gptel-mode)
            (gptel-mode +1))
          (mevedel-session-persistence--normalize-gptel-properties))
      (set-buffer-modified-p was-modified))))

;;
;;; Fast Org property writes

(defun mevedel-session-persistence--top-level-pom-p (pom)
  "Return non-nil when POM points at the top-level property drawer."
  (cond
   ((integerp pom) (= pom (point-min)))
   ((markerp pom) (= (marker-position pom) (point-min)))
   (t nil)))

(defun mevedel-session-persistence--property-drawer-region ()
  "Return (START . END) for the initial Org property drawer, or nil.
END is the position just after the `:END:' line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
        (let ((start (line-beginning-position)))
          (forward-line 1)
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (forward-line 1)
            (cons start (point))))))))

(defun mevedel-session-persistence--ensure-property-drawer ()
  "Return the initial Org property drawer region, creating it if needed."
  (or (mevedel-session-persistence--property-drawer-region)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert ":PROPERTIES:\n:END:\n"))
          (mevedel-session-persistence--property-drawer-region)))))

(defun mevedel-session-persistence--property-delete-direct (property)
  "Delete PROPERTY from the initial Org property drawer without Org parsing."
  (when-let* ((region (mevedel-session-persistence--property-drawer-region)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (regexp (format "^[ \t]*:%s\\+?:[ \t]*.*$"
                              (regexp-quote property)))
              (end-marker
               (copy-marker
                (save-excursion
                  (goto-char (cdr region))
                  (forward-line -1)
                  (line-beginning-position))
                t))
              (drawer-end-marker (copy-marker (cdr region) t))
              (inhibit-read-only t)
              removed)
          (unwind-protect
              (progn
                (goto-char (car region))
                (forward-line 1)
                (while (re-search-forward regexp
                                          (marker-position end-marker) t)
                  (setq removed t)
                  (delete-region
                   (line-beginning-position)
                   (save-excursion
                     (forward-line 1)
                     (while (and (< (point) (marker-position end-marker))
                                 (not (looking-at-p
                                       "[ \t]*:[^: \t\n][^:\n]*:[ \t]*.*$")))
                       (forward-line 1))
                     (point))))
                (when (and removed
                           (= (save-excursion
                                (goto-char (car region))
                                (forward-line 1)
                                (point))
                              (marker-position end-marker)))
                  (delete-region (car region)
                                 (marker-position drawer-end-marker)))
                removed)
            (set-marker end-marker nil)
            (set-marker drawer-end-marker nil)))))))

(defun mevedel-session-persistence--property-put-direct (property value)
  "Set PROPERTY to VALUE in the initial Org property drawer without Org parsing."
  (mevedel-session-persistence--property-delete-direct property)
  (when-let* ((region (mevedel-session-persistence--ensure-property-drawer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (goto-char (car region))
          (forward-line 1)
          (insert (format ":%s: %s\n" property value)))))))

(defun mevedel-session-persistence--with-fast-property-writes (fn)
  "Call FN while routing top-level Org property writes through text helpers."
  (let ((orig-put (symbol-function 'org-entry-put))
        (orig-delete (symbol-function 'org-entry-delete)))
    (cl-letf (((symbol-function 'org-entry-put)
               (lambda (pom property value)
                 (if (mevedel-session-persistence--top-level-pom-p pom)
                     (mevedel-session-persistence--property-put-direct
                      property value)
                   (funcall orig-put pom property value))))
              ((symbol-function 'org-entry-delete)
               (lambda (pom property)
                 (if (mevedel-session-persistence--top-level-pom-p pom)
                     (mevedel-session-persistence--property-delete-direct
                      property)
                   (funcall orig-delete pom property)))))
      (funcall fn))))


(defun mevedel-session-persistence--dynamic-system-preset-p ()
  "Return non-nil if the current gptel preset can recreate the system prompt.

gptel can use function-valued or dynamic-spec `:system' entries at
runtime, but its Org persistence evaluates them into a frozen
`GPTEL_SYSTEM' string.  Mevedel session files should keep the preset
reference and drop that frozen override only when the preset can
recreate the system prompt on restore."
  (when (and (boundp 'gptel--preset)
             gptel--preset
             (fboundp 'gptel-get-preset))
    (when-let* ((preset-spec (gptel-get-preset gptel--preset))
                ((plist-member preset-spec :system)))
      (let ((system (plist-get preset-spec :system)))
        (or (functionp system)
            (and (consp system)
                 (keywordp (car system))))))))

(defun mevedel-session-persistence--stabilize-gptel-bounds ()
  "Rewrite `GPTEL_BOUNDS' until Org property drawer offsets settle.

`gptel-org--save-state' persists absolute buffer positions.  Updating
the Org property drawer can itself move every marked transcript region,
so a single write can save positions that were correct for the previous
drawer size.  Recompute after each write and stop once the serialized
bounds no longer change."
  (when (and (derived-mode-p 'org-mode)
             (require 'org nil t)
             (fboundp 'gptel--get-buffer-bounds))
    (save-excursion
      (save-restriction
        (widen)
        (let ((last nil)
              (done nil)
              (attempts 0))
          (while (and (not done) (< attempts 8))
            (setq attempts (1+ attempts))
            (mevedel-session-persistence--normalize-gptel-properties)
            (let ((serialized
                   (when-let* ((bounds (gptel--get-buffer-bounds)))
                     (prin1-to-string bounds))))
              (cond
               ((null serialized)
                (mevedel-session-persistence--property-delete-direct
                 "GPTEL_BOUNDS")
                (setq done t))
               ((equal serialized last)
                (setq done t))
               (t
                (setq last serialized)
                (mevedel-session-persistence--property-put-direct
                 "GPTEL_BOUNDS" serialized))))))))))

(defun mevedel-session-persistence--save-gptel-state-around (orig-fun &rest args)
  "Save gptel state without freezing dynamic mevedel system prompts.

This is an around-advice for `gptel--save-state'.  For non-mevedel
buffers and static prompts it delegates unchanged.  For mevedel chat
buffers using presets with dynamic `:system' values, it removes any
existing `GPTEL_SYSTEM' first and dynamically binds
`gptel-system-prompt' to nil while gptel writes its Org metadata.
After delegation, it rewrites `GPTEL_BOUNDS' until the saved absolute
positions match the post-drawer-update buffer."
  (let ((mevedel-org-buffer-p
         (and (bound-and-true-p mevedel--session)
              (derived-mode-p 'org-mode))))
    (prog1
        (if mevedel-org-buffer-p
            (mevedel-session-persistence--with-fast-property-writes
             (lambda ()
               (if (and (mevedel-session-persistence--dynamic-system-preset-p)
                        (require 'org nil t))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (mevedel-session-persistence--property-delete-direct
                        "GPTEL_SYSTEM")
                       (let ((gptel-system-prompt nil))
                         (apply orig-fun args))))
                 (apply orig-fun args))))
          (apply orig-fun args))
      (when mevedel-org-buffer-p
        (mevedel-session-persistence--stabilize-gptel-bounds)))))

(defun mevedel-session-persistence--install-gptel-save-state-advice ()
  "Install mevedel's dynamic-system preservation advice for gptel saves."
  (unless (advice-member-p
           #'mevedel-session-persistence--save-gptel-state-around
           'gptel--save-state)
    (advice-add 'gptel--save-state :around
                #'mevedel-session-persistence--save-gptel-state-around)))


;;
;;; Instruction snapshots

(defun mevedel-session-persistence--instructions-dir (save-path)
  "Return the instruction snapshot directory under SAVE-PATH."
  (file-name-concat save-path "instructions"))

(defun mevedel-session-persistence--instructions-current-path (save-path)
  "Return the current instruction snapshot path under SAVE-PATH."
  (file-name-concat
   (mevedel-session-persistence--instructions-dir save-path)
   "current.el"))

(defun mevedel-session-persistence--instructions-turn-path (save-path turn)
  "Return the instruction snapshot path for TURN under SAVE-PATH."
  (file-name-concat
   (mevedel-session-persistence--instructions-dir save-path)
   (format "turn-%06d.el" turn)))

(defun mevedel-session-persistence--save-instructions (session buffer)
  "Persist the current workspace instruction state for SESSION.

Writes both `instructions/current.el' and, when the session has a turn
count, a turn-specific snapshot used by rewind/fork."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (require 'mevedel-persistence)
    (let ((dir (mevedel-session-persistence--instructions-dir save-path))
          (workspace-root (mevedel-workspace-root
                           (mevedel-session-workspace session)))
          (turn (mevedel-session-turn-count session)))
      (make-directory dir t)
      (with-current-buffer buffer
        (mevedel--write-instructions-file
         (mevedel-session-persistence--instructions-current-path save-path)
         workspace-root t t t)
        (when (integerp turn)
          (mevedel--write-instructions-file
           (mevedel-session-persistence--instructions-turn-path save-path turn)
           workspace-root t t nil))))))

(defun mevedel-session-persistence--load-instructions
    (session buffer &optional turn)
  "Restore SESSION instruction snapshot into BUFFER's workspace.

When TURN is non-nil, restore the turn-specific snapshot; otherwise
restore `instructions/current.el'.  Missing snapshots are ignored so
older sessions without instruction persistence still resume."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (let ((path (if turn
                    (mevedel-session-persistence--instructions-turn-path
                     save-path turn)
                  (mevedel-session-persistence--instructions-current-path
                   save-path))))
      (when (file-exists-p path)
        (require 'mevedel-persistence)
        (condition-case err
            (with-current-buffer buffer
              (mevedel--load-instructions-file
               path
               (mevedel-workspace-root (mevedel-session-workspace session))
               nil t
               (mevedel-session-workspace session)))
          (error
           (display-warning
            'mevedel
            (format "Could not restore instruction snapshot %s: %s"
                    path (error-message-string err))
            :warning)
           nil))))))


;;
;;; Per-turn save

(defun mevedel-session-persistence-save (session buffer)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, snapshot any tool-modified
files for this turn, evict old snapshots over the cap, and rewrite the
sidecar.

No-op when `mevedel-session-persistence' is nil."
  (when-let ((buffer (and mevedel-session-persistence
                         (mevedel-session-persistence--authoritative-buffer
                          buffer))))
    (let ((had-save-path (mevedel-session-save-path session))
          (rerender-needed nil))
      (when (mevedel-session-persistence-ensure-files session buffer)
        (unless had-save-path
          (setq rerender-needed t)))
      (setf (mevedel-session-updated-at session)
            (format-time-string "%FT%H-%M-%S"))
      (with-current-buffer buffer
        (when (buffer-modified-p)
          (save-buffer)
          (setq rerender-needed t)))
      ;; Refresh the live segment's prompt list (drives the rewind picker).
      (mevedel-session-persistence--update-prompt-index session buffer)
      ;; Snapshot files modified during the just-completed turn.
      (when (and (boundp 'mevedel--current-request)
                 mevedel--current-request)
        (let ((pre-snapshots
               (mevedel-request-file-snapshots mevedel--current-request)))
          (mevedel-file-history-snapshot-modified
           session
           (or (mevedel-session-turn-count session) 0)
           pre-snapshots)
          (mevedel-file-history-evict session)))
      (mevedel-session-persistence-write
       (mevedel-session-persistence--sidecar-path
        (mevedel-session-save-path session))
       (mevedel-session-persistence--build-sidecar session buffer))
      (mevedel-session-persistence--save-instructions session buffer)
      (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                  ((buffer-live-p vb)))
        (require 'mevedel-view-history)
        (mevedel-view-history-save vb))
      ;; `gptel-org--save-state' rewrites the top-level org property
      ;; drawer during `save-buffer'.  That drawer contains large values
      ;; such as GPTEL_SYSTEM and GPTEL_BOUNDS, so inserting or resizing it
      ;; shifts every content position in the data buffer.  The view stores
      ;; data-buffer source coordinates on collapsed sections; refresh it
      ;; after save-time shifts so expand/collapse does not read from the
      ;; drawer.
      (when rerender-needed
        (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                    ((buffer-live-p vb)))
          (with-current-buffer vb
            (when (and (boundp 'mevedel--data-buffer)
                       (eq mevedel--data-buffer buffer))
              (mevedel-view--full-rerender)))))
      (mevedel-session-save-path session))))


;;
;;; File-history store
;;
;; Per-session on-disk backup store at <save-path>/file-history/.
;; Filename scheme: `<sha256(absolute-filepath)[:16]>@v<N>' where <N>
;; is the sequential per-file version.  Mapping from (turn, path) to
;; backup filename lives in `mevedel-session-file-snapshots' (alist
;; keyed by turn number; inner alist keyed by absolute path).

(defun mevedel-file-history--path-hash (path)
  "Return the first 16 hex chars of SHA-256 of PATH (expanded)."
  (substring (secure-hash 'sha256 (expand-file-name path)) 0 16))

(defun mevedel-file-history--backup-name (path version)
  "Return the backup filename for PATH at VERSION."
  (format "%s@v%d" (mevedel-file-history--path-hash path) version))

(defun mevedel-file-history--backup-path (save-path backup-name)
  "Return the absolute path to BACKUP-NAME under SAVE-PATH's file-history/."
  (file-name-concat save-path "file-history" backup-name))

(defun mevedel-file-history--latest-version (session path)
  "Return the highest version recorded for PATH in SESSION, or 0 if none."
  (let ((best 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best) (setq best v))))))

(defun mevedel-file-history--read-file-raw (path)
  "Return PATH's contents as a unibyte string (no encoding conversion)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-string)))

(defun mevedel-file-history--write-backup (save-path backup-name content)
  "Atomically write CONTENT under `<SAVE-PATH>/file-history/BACKUP-NAME'."
  (let* ((dir  (file-name-concat save-path "file-history"))
         (dest (file-name-concat dir backup-name)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Create the temp file in the destination's own directory so
    ;; `rename-file' stays on one filesystem and remains atomic.
    (let ((tmp (make-temp-file (expand-file-name ".mevedel-fh-" dir))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'no-conversion))
              (write-region content nil tmp nil 'silent))
            (rename-file tmp dest t))
        (when (file-exists-p tmp) (delete-file tmp))))))

(defun mevedel-file-history--maybe-snapshot (session path pre-content)
  "Decide whether PATH's state at turn end warrants a new backup.

PATH is an absolute filesystem path that was touched by the just-completed
turn's tools.  PRE-CONTENT is PATH's content at the start of the turn,
or nil if it did not exist.

Returns a snapshot entry `(PATH . PLIST)' when a state change is recorded
for this turn, otherwise nil.  Side-effect: writes a backup file when
the state is content-bearing.

Skip conditions:
- PATH exists but is not a regular file (devices, sockets, FIFOs).
  Symlinks to regular files pass because `file-regular-p' follows.
- PATH exists and exceeds `mevedel-file-history-max-snapshot-bytes'."
  (let ((current-exists (file-exists-p path)))
    (cond
     ;; Deleted during turn: record an absent marker (no backup file).
     ((and pre-content (not current-exists))
      (cons path
            (list :backup-name nil
                  :version     (1+ (mevedel-file-history--latest-version
                                    session path))
                  :backup-time (format-time-string "%FT%H-%M-%S")
                  :file-mtime  nil)))
     ;; Currently present: create or modify.
     (current-exists
      (cond
       ((not (file-regular-p path)) nil)
       ((let ((attrs (file-attributes path)))
          (and attrs
               (> (file-attribute-size attrs)
                  mevedel-file-history-max-snapshot-bytes)))
        (display-warning
         'mevedel
         (format "Skipping snapshot of %s: exceeds %d bytes"
                 path mevedel-file-history-max-snapshot-bytes)
         :warning)
        nil)
       (t
        (let ((current-content
               (mevedel-file-history--read-file-raw path)))
          (when (or (null pre-content)
                    (not (string-equal pre-content current-content)))
            (let* ((version (1+ (mevedel-file-history--latest-version
                                 session path)))
                   (backup-name
                    (mevedel-file-history--backup-name path version)))
              (mevedel-file-history--write-backup
               (mevedel-session-save-path session)
               backup-name
               current-content)
              (cons path
                    (list :backup-name backup-name
                          :version     version
                          :backup-time (format-time-string "%FT%H-%M-%S")
                          :file-mtime
                          (format-time-string
                           "%FT%H-%M-%S"
                           (file-attribute-modification-time
                            (file-attributes path)))))))))))
     ;; Pre-content was nil and current doesn't exist: nothing to record.
     (t nil))))

(defun mevedel-file-history-snapshot-modified (session turn-n pre-snapshots)
  "Snapshot files modified during TURN-N for SESSION.

PRE-SNAPSHOTS is a hash-table mapping absolute path to the file's content
at turn start (nil for paths that did not yet exist).  Typically the
`file-snapshots' slot of the just-completed request.

For each path where the current on-disk content differs from the pre-turn
content (or the file's existence changed), write a new backup version
and append a snapshot entry under TURN-N in the session's file-snapshots
map.  Returns the list of backup names written."
  (when (and (mevedel-session-save-path session)
             (hash-table-p pre-snapshots))
    (let (entries written)
      (maphash
       (lambda (path pre-content)
         (when-let ((entry (mevedel-file-history--maybe-snapshot
                            session path pre-content)))
           (push entry entries)
           (when-let ((name (plist-get (cdr entry) :backup-name)))
             (push name written))))
       pre-snapshots)
      ;; Sort entries by path so two saves with identical state
      ;; produce byte-identical sidecars (hash-table iteration is
      ;; otherwise non-deterministic).
      (setq entries (sort entries
                          (lambda (a b) (string< (car a) (car b)))))
      (when entries
        (let ((cell (assoc turn-n (mevedel-session-file-snapshots session))))
          (if cell
              (setcdr cell entries)
            (setf (mevedel-session-file-snapshots session)
                  (cons (cons turn-n entries)
                        (mevedel-session-file-snapshots session))))))
      (sort written #'string<))))

(defun mevedel-file-history--gc-orphans (session)
  "Delete backup files under SESSION's file-history dir that are unreferenced.

Walks `:file-snapshots', collects the referenced backup-name set, then
deletes any regular file in `<save-path>/file-history/' not in that set.
Directories and non-regular entries are skipped."
  (let* ((dir        (file-name-concat (mevedel-session-save-path session)
                                       "file-history"))
         (referenced (make-hash-table :test #'equal)))
    (dolist (turn-entry (mevedel-session-file-snapshots session))
      (dolist (file-entry (cdr turn-entry))
        (when-let ((bn (plist-get (cdr file-entry) :backup-name)))
          (puthash bn t referenced))))
    (when (file-directory-p dir)
      (dolist (name (directory-files dir nil "\\`[^.]"))
        (unless (gethash name referenced)
          (let ((full (file-name-concat dir name)))
            (when (file-regular-p full)
              (delete-file full))))))))

(defun mevedel-file-history-evict (session)
  "Evict old snapshots from SESSION's file-history beyond the cap.

When `mevedel-file-history-max-snapshots' is non-nil and the number of
per-turn snapshot entries exceeds it, drops the oldest turn entries and
then garbage-collects orphan backup files.  No-op when the cap is nil
or when the session is under-cap."
  (when (and mevedel-file-history-max-snapshots
             (mevedel-session-save-path session))
    (let* ((snapshots (mevedel-session-file-snapshots session))
           (count     (length snapshots)))
      (when (> count mevedel-file-history-max-snapshots)
        (let* ((sorted (sort (copy-sequence snapshots)
                             (lambda (a b) (< (car a) (car b)))))
               (keep-count mevedel-file-history-max-snapshots)
               (kept    (nthcdr (- count keep-count) sorted)))
          (setf (mevedel-session-file-snapshots session) kept)
          (mevedel-file-history--gc-orphans session))))))


;;
;;; Segment rotation (split-on-compact)

(defvar gptel--markdown-block-map)
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function org-cycle "ext:org" (&optional arg))

(defun mevedel-session-persistence--file-text (file)
  "Return FILE contents as a string using normal text decoding."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mevedel-session-persistence--refresh-visited-file-modtime-or-error
    (&optional expected-texts)
  "Refresh stale visited-file metadata when disk text is expected.

EXPECTED-TEXTS is a string or list of strings that may also match the
visited file.  This covers automatic edits that first remove transient
unsaved text from the live buffer.  If the visited file changed externally
to different text or was deleted, signal a controlled error instead of
letting `save-buffer' ask an interactive supersession question during
automatic segment rotation."
  (when buffer-file-name
    (cond
     ((not (file-exists-p buffer-file-name))
      (when buffer-file-number
        (error "Session segment changed on disk: %s" buffer-file-name)))
     ((not (verify-visited-file-modtime (current-buffer)))
      (let ((file-text (mevedel-session-persistence--file-text buffer-file-name))
            (accepted (cons (buffer-substring-no-properties (point-min) (point-max))
                            (if (listp expected-texts)
                                expected-texts
                              (list expected-texts)))))
        (if (member file-text accepted)
            (set-visited-file-modtime)
          (error "Session segment changed on disk: %s" buffer-file-name)))))))

(defun mevedel-session-persistence--set-visited-segment-file (file)
  "Make the current buffer visit segment FILE without changing its name."
  (setq buffer-file-name file
        buffer-file-truename (file-truename file))
  (when (file-exists-p file)
    (set-visited-file-modtime)))

(defun mevedel-session-persistence--insert-segment-header (session)
  "Insert per-segment org properties at point in current buffer.

Sets `MEVEDEL_SESSION_ID', `MEVEDEL_SEGMENT_NUMBER',
`MEVEDEL_SEGMENT_CREATED_AT', and `MEVEDEL_VERSION'.  Caller is
responsible for ensuring the buffer is in `org-mode' (mevedel data
buffers are locked to org-mode by `mevedel--chat-buffer-setup')."
  (when (derived-mode-p 'org-mode)
    (require 'org)
    (org-entry-put (point-min) "MEVEDEL_VERSION" (mevedel-version))
    (org-entry-put (point-min) "MEVEDEL_SESSION_ID"
                   (or (mevedel-session-session-id session) ""))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_NUMBER"
                   (number-to-string
                    (or (mevedel-session-current-segment session) 1)))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_CREATED_AT"
                   (format-time-string "%FT%H-%M-%S"))))

(defun mevedel-session-persistence--sanitize-gptel-bounds-once ()
  "Run one `GPTEL_BOUNDS' sanitation pass.
Return non-nil when the property drawer changed."
  (when-let* ((raw (org-entry-get (point-min) "GPTEL_BOUNDS")))
    (let* ((invalid (make-symbol "invalid"))
           (bounds (condition-case nil
                       (read raw)
                     (error invalid)))
           changed)
      (if (or (eq bounds invalid)
              (not (listp bounds)))
          (progn
            (org-entry-delete (point-min) "GPTEL_BOUNDS")
            (setq changed t))
        (cl-labels
            ((sanitize-range (range)
               (when (and (consp range)
                          (integerp (car range))
                          (integerp (cadr range)))
                 (let ((start (max (point-min)
                                   (min (car range) (point-max))))
                       (end (max (point-min)
                                 (min (cadr range) (point-max)))))
                   (when (< start end)
                     (append (list start end) (cddr range))))))
             (sanitize-entry (entry)
               (when (consp entry)
                 (let (ranges)
                   (dolist (range (cdr entry))
                     (when-let* ((sanitized (sanitize-range range)))
                       (push sanitized ranges)))
                   (when ranges
                     (cons (car entry) (nreverse ranges)))))))
          (let (sanitized)
            (dolist (entry bounds)
              (when-let* ((sanitized-entry (sanitize-entry entry)))
                (push sanitized-entry sanitized)))
            (setq sanitized (nreverse sanitized))
            (unless (equal sanitized bounds)
              (if sanitized
                  (org-entry-put (point-min) "GPTEL_BOUNDS"
                                 (prin1-to-string sanitized))
                (org-entry-delete (point-min) "GPTEL_BOUNDS"))
              (setq changed t)))))
      changed)))

(defun mevedel-session-persistence--sanitize-gptel-bounds ()
  "Clamp malformed top-level `GPTEL_BOUNDS' ranges to the current buffer.

Older snapshots can contain byte-oriented or otherwise stale bounds.
`gptel-org--restore-state' applies them as character positions, so any
range past `point-max' aborts state restoration during resume.  Rewriting
the drawer can itself move `point-max', so repeat until the serialized
bounds settle."
  (when (derived-mode-p 'org-mode)
    (let ((was-modified (buffer-modified-p))
          (changed nil)
          (again t)
          (attempts 0))
      (unwind-protect
          (while (and again (< attempts 8))
            (setq attempts (1+ attempts)
                  again (mevedel-session-persistence--sanitize-gptel-bounds-once)
                  changed (or changed again)))
        (set-buffer-modified-p was-modified))
      changed)))

(defun mevedel-session-persistence--segment-summary-bounds ()
  "Return bounds for the leading segment compaction summary, or nil.

The returned plist contains `:begin', `:body-begin', `:body-end',
and `:end'.  A summary is accepted only when it is the first top-level
content after the optional org property drawer and whitespace."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin_summary\\b.*$" nil t)
      (let ((begin (match-beginning 0))
            (body-begin (match-end 0)))
        (when (and (save-excursion
                     (goto-char begin)
                     (let ((prefix (buffer-substring-no-properties
                                    (point-min) begin)))
                       (string-match-p
                        "\\`[[:space:]\n]*\\(:PROPERTIES:\n\\(.\\|\n\\)*?:END:\n\\)?[[:space:]\n]*\\'"
                        prefix)))
                   (re-search-forward "^#\\+end_summary\\b.*$" nil t))
          (list :begin begin
                :body-begin (1+ body-begin)
                :body-end (match-beginning 0)
                :end (match-end 0)))))))

(defconst mevedel-session-persistence--summary-handoff-prefix
  "Another language model started to solve this problem and produced a summary of its work. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary:\n\n"
  "Model-facing preface inserted before a compacted segment summary.")

(defun mevedel-session-persistence--strip-summary-handoff-prefix (summary)
  "Return SUMMARY without the model-facing handoff prefix."
  (if (and (stringp summary)
           (string-prefix-p
            mevedel-session-persistence--summary-handoff-prefix summary))
      (substring summary
                 (length mevedel-session-persistence--summary-handoff-prefix))
    summary))

(defun mevedel-session-persistence--summary-block (summary)
  "Return SUMMARY wrapped in an org `#+begin_summary' block.

The block markers are propertized with `gptel \\='ignore' so the LLM sees
only the handoff preface plus SUMMARY -- not the wrapper lines.  The
user\\='s view, by contrast, sees a foldable block."
  (concat (propertize "#+begin_summary mevedel-role=compaction-summary\n"
                      'gptel 'ignore)
          mevedel-session-persistence--summary-handoff-prefix
          summary
          (propertize "\n#+end_summary\n" 'gptel 'ignore)))

(defun mevedel-session-persistence--finalize-segment-file (file)
  "Mark segment FILE finalized on disk."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                     (format-time-string "%FT%H-%M-%S"))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun mevedel-session-persistence--delete-trailing-text (text)
  "Delete trailing TEXT from the current buffer when it is an exact suffix."
  (when (and text
             (not (string-empty-p text))
             (string-suffix-p
              (substring-no-properties text)
              (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (- (point-max) (length text)) (point-max))
    t))

(cl-defun mevedel-session-persistence-rotate-segment
    (session buffer summary &key tail-text pending-text truncated-tail-p)
  "Finalize SESSION's current segment and start a new one with SUMMARY.

Performs the split-on-compact rotation:
  1. Saves the current segment file before replacing the live buffer.
  2. Advances `mevedel-session-current-segment' on SESSION.
  3. Re-points BUFFER's `buffer-file-name' at the new segment path.
  4. Erases BUFFER and re-builds it: per-segment org property drawer
     followed by SUMMARY wrapped in an `#+begin_summary' block, then
     TAIL-TEXT and PENDING-TEXT when supplied.
  5. Saves the new segment file without PENDING-TEXT, then restores
     PENDING-TEXT in the live buffer without marking it saved.
  6. Rewrites the sidecar.
  7. Sets `MEVEDEL_SEGMENT_FINALIZED_AT' on the predecessor segment.

TAIL-TEXT is preserved recent transcript text, including text
properties.  PENDING-TEXT is an inserted-but-unsent prompt region.
TRUNCATED-TAIL-P is recorded as segment metadata when non-nil.

Requires SESSION to have a `save-path' (i.e., to have been lazily
materialized).  Returns the new segment's absolute path on success,
nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (with-current-buffer buffer
      (require 'org)
      ;; 1. Save the current segment before replacing the buffer body.
      (let ((old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (tail-prompt-count
             (mevedel-session-persistence--prompt-count-in-text tail-text))
            new-segment
            tmp-segment
            pending-start-marker
            pending-end-marker)
        (condition-case err
            (progn
              (mevedel-session-persistence--refresh-visited-file-modtime-or-error
               (when (and pending-text
                          (string-suffix-p
                           (substring-no-properties pending-text)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                 (buffer-substring-no-properties
                  (point-min) (- (point-max) (length pending-text)))))
              (when pending-text
                (let ((inhibit-read-only t))
                  (mevedel-session-persistence--delete-trailing-text
                   pending-text)))
              (when (buffer-modified-p) (save-buffer))
              ;; 2. Advance segment counter.
              (cl-incf (mevedel-session-current-segment session))
              ;; 3. Switch buffer-file-name to the new segment.
              (setq new-segment
                    (mevedel-session-persistence--segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (setq tmp-segment (concat new-segment ".tmp"))
              (setq buffer-file-name new-segment)
              ;; 4. Erase and rebuild buffer body.
              (let ((inhibit-read-only t))
                (erase-buffer)
                (mevedel-session-persistence--insert-segment-header session)
                (when truncated-tail-p
                  (org-entry-put (point-min) "MEVEDEL_SEGMENT_TRUNCATED_TAIL" "t"))
                (when (> tail-prompt-count 0)
                  (org-entry-put (point-min) "MEVEDEL_SEGMENT_TAIL_PROMPTS"
                                 (number-to-string tail-prompt-count)))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n")
                (insert (mevedel-session-persistence--summary-block summary))
                (when tail-text
                  (unless (bolp) (insert "\n"))
                  (insert tail-text))
                (when pending-text
                  (unless (bolp) (insert "\n"))
                  (setq pending-start-marker (copy-marker (point) nil))
                  (insert pending-text)
                  (setq pending-end-marker (copy-marker (point) nil)))
                (insert "\n"))
              (set-buffer-modified-p t)
              ;; 5. Save the new segment via a temp file, then atomically publish it.
              (when (and pending-start-marker pending-end-marker)
                (delete-region pending-start-marker pending-end-marker))
              (let ((buffer-file-name tmp-segment))
                (save-buffer))
              (rename-file tmp-segment new-segment t)
              (mevedel-session-persistence--set-visited-segment-file new-segment)
              (set-buffer-modified-p nil)
              ;; 6. Rewrite the sidecar with the bumped current-segment.
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-persistence--build-sidecar session buffer))
              (mevedel-session-persistence--save-instructions session buffer)
              (mevedel-session-persistence--finalize-segment-file old-segment)
              (when pending-start-marker
                (goto-char pending-start-marker)
                (insert pending-text)
                ;; The pending prompt belongs to the in-flight request.
                ;; The DONE autosave will commit it together with the
                ;; assistant response; failure/abort paths must not.
                (set-buffer-modified-p nil))
              new-segment)
          (error
           (setf (mevedel-session-current-segment session) old-current-segment)
           (setf (mevedel-session-updated-at session) old-updated-at)
           (setq buffer-file-name old-segment)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert old-text))
           (goto-char (min old-point (point-max)))
           (set-buffer-modified-p old-modified-p)
           (ignore-errors
             (mevedel-session-persistence-write
              (mevedel-session-persistence--sidecar-path
               (mevedel-session-save-path session))
              (mevedel-session-persistence--build-sidecar session buffer)))
           (when (and tmp-segment (file-exists-p tmp-segment))
             (delete-file tmp-segment))
           (when (and new-segment (file-exists-p new-segment))
             (delete-file new-segment))
           (signal (car err) (cdr err))))))))

(cl-defun mevedel-session-persistence-start-fresh-segment
    (session buffer &key initial-text)
  "Finalize SESSION's current segment and start a blank live segment.

INITIAL-TEXT, when non-nil, is inserted after the new segment's org
metadata.  This is used by `/clear' to leave a fresh prompt prefix in
the data buffer without carrying over any conversation summary.

Requires SESSION to have a `save-path'.  Returns the new segment's
absolute path on success, nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (with-current-buffer buffer
      (require 'org)
      (let ((old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            new-segment
            tmp-segment
            initial-start-marker
            initial-end-marker)
        (unless old-segment
          (error "No current segment file"))
        (condition-case err
            (progn
              (mevedel-session-persistence--refresh-visited-file-modtime-or-error)
              (when (buffer-modified-p) (save-buffer))
              (mevedel-session-persistence--update-prompt-index
               session buffer)
              (cl-incf (mevedel-session-current-segment session))
              (setq new-segment
                    (mevedel-session-persistence--segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (setq tmp-segment (concat new-segment ".tmp"))
              (setq buffer-file-name new-segment)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (mevedel-session-persistence--insert-segment-header session)
                (goto-char (point-max))
                (when (and initial-text
                           (not (string-empty-p initial-text)))
                  (unless (bolp) (insert "\n"))
                  (setq initial-start-marker (copy-marker (point) nil))
                  (insert initial-text)
                  (setq initial-end-marker (copy-marker (point) nil))))
              (set-buffer-modified-p t)
              (when (and initial-start-marker initial-end-marker)
                (delete-region initial-start-marker initial-end-marker))
              (let ((buffer-file-name tmp-segment))
                (save-buffer))
              (rename-file tmp-segment new-segment t)
              (mevedel-session-persistence--set-visited-segment-file new-segment)
              (set-buffer-modified-p nil)
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-persistence--build-sidecar session buffer))
              (mevedel-session-persistence--save-instructions session buffer)
              (mevedel-session-persistence--finalize-segment-file old-segment)
              (when initial-start-marker
                (goto-char initial-start-marker)
                (insert initial-text)
                (set-buffer-modified-p nil))
              (goto-char (point-max))
              (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                          ((buffer-live-p vb)))
                (with-current-buffer vb
                  (mevedel-view--full-rerender)))
              new-segment)
          (error
           (setf (mevedel-session-current-segment session) old-current-segment)
           (setf (mevedel-session-updated-at session) old-updated-at)
           (setq buffer-file-name old-segment)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert old-text))
           (goto-char (min old-point (point-max)))
           (set-buffer-modified-p old-modified-p)
           (ignore-errors
             (mevedel-session-persistence-write
              (mevedel-session-persistence--sidecar-path
               (mevedel-session-save-path session))
              (mevedel-session-persistence--build-sidecar session buffer)))
           (when (and tmp-segment (file-exists-p tmp-segment))
             (delete-file tmp-segment))
           (when (and new-segment (file-exists-p new-segment))
             (delete-file new-segment))
           (signal (car err) (cdr err))))))))


;;
;;; Locking

(defvar mevedel-session-persistence--emacs-invocation-time
  (format-time-string "%FT%H-%M-%S")
  "Wall-clock time when this Emacs process started.
Stamped into `.lock' files as a forensic / tiebreaker field.")

(defun mevedel-session-persistence--lock-path (session-dir)
  "Return the absolute path to SESSION-DIR's `.lock' file."
  (file-name-concat session-dir ".lock"))

(defun mevedel-session-persistence--write-lock (lock-path buffer-name)
  "Write or overwrite a lock file at LOCK-PATH naming BUFFER-NAME as holder.
Prefer `mevedel-session-persistence--write-lock-atomic' when
acquiring a fresh lock; this function is safe only when the caller
already owns (or is replacing) an existing lock."
  (let ((plist (list :pid (emacs-pid)
                     :hostname (system-name)
                     :emacs-invocation-time
                     mevedel-session-persistence--emacs-invocation-time
                     :buffer buffer-name)))
    (with-temp-file lock-path
      (let ((print-length nil) (print-level nil))
        (prin1 plist (current-buffer))))))

(defun mevedel-session-persistence--write-lock-atomic (lock-path buffer-name)
  "Atomically create LOCK-PATH with this Emacs as holder.
Returns t on success, nil when LOCK-PATH already exists (race lost).

Uses `add-name-to-file' which is POSIX link(2) and thus atomic: if
the target already exists it signals `file-already-exists'.  Writes
the payload into a unique temp file in the same directory first so
partial payloads can never appear at LOCK-PATH."
  (let* ((dir (file-name-directory (expand-file-name lock-path)))
         (tmp (make-temp-file (expand-file-name ".mevedel-lock-" dir)))
         (plist (list :pid (emacs-pid)
                      :hostname (system-name)
                      :emacs-invocation-time
                      mevedel-session-persistence--emacs-invocation-time
                      :buffer buffer-name)))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-length nil) (print-level nil))
              (prin1 plist (current-buffer))))
          (condition-case _
              (progn
                (add-name-to-file tmp lock-path nil)
                t)
            (file-already-exists nil)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun mevedel-session-persistence--read-lock (lock-path)
  "Return the plist read from LOCK-PATH, or nil if absent or unparseable."
  (when (file-exists-p lock-path)
    (condition-case _
        (with-temp-buffer
          (insert-file-contents lock-path)
          (goto-char (point-min))
          (read (current-buffer)))
      (error nil))))

(defun mevedel-session-persistence--pid-alive-p (pid)
  "Return non-nil if PID is a live process on the current host."
  (and pid (numberp pid)
       (condition-case _
           (signal-process pid 0)
         (error nil))))

(defconst mevedel-session-persistence--lock-start-time-tolerance 2
  "Seconds of tolerance when comparing a lock timestamp to process start.

The lock timestamp is formatted to whole seconds, while process start
attributes can carry sub-second precision and can be rounded differently
by the host OS.")

(defun mevedel-session-persistence--pid-start-time (pid)
  "Return PID's process start time, or nil if unavailable."
  (when (and pid (numberp pid))
    (condition-case _
        (cdr (assq 'start (process-attributes pid)))
      (error nil))))

(defun mevedel-session-persistence--same-host-lock-active-p (lock-info)
  "Return non-nil if same-host LOCK-INFO describes an active holder.

Dead PIDs are stale.  Live PIDs are considered active unless the live
process start time proves PID reuse: a process that started clearly
after the lock holder's recorded Emacs invocation cannot be the process
that wrote the lock.  When the recorded invocation time or live process
start time is unavailable, keep the lock active rather than risking data
loss."
  (when (mevedel-session-persistence--pid-alive-p
         (plist-get lock-info :pid))
    (let ((holder-start (mevedel-session-persistence--parse-iso-time
                         (plist-get lock-info :emacs-invocation-time)))
          (pid-start    (mevedel-session-persistence--pid-start-time
                         (plist-get lock-info :pid))))
      (or (not holder-start)
          (not pid-start)
          (<= (- (float-time pid-start) (float-time holder-start))
              mevedel-session-persistence--lock-start-time-tolerance)))))

(defun mevedel-session-persistence-lock-acquire (session-dir buffer-name)
  "Acquire SESSION-DIR's `.lock' for BUFFER-NAME.

Returns:
  t   - lock acquired (or broken from a previous holder).
  nil - user chose read-only access; caller should set buffer-read-only.

Signals `user-error' when the user declines to break a stale lock or
aborts any of the 3-way conflict prompts.

Behavior table:
- No existing lock: write a new lock, return t.
- Lock from same host, dead PID: prompt to break (`y-or-n-p').
- Lock from same host, live PID: 3-way prompt -- break / read-only / abort.
- Lock from different host: 3-way prompt -- break / read-only / abort."
  (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
    (cond
     ((null existing)
      ;; Race-free create via `add-name-to-file'.  If another process
      ;; beat us to it between the read above and this write, fall
      ;; through to the existing-lock branches.
      (cond
       ((mevedel-session-persistence--write-lock-atomic lock-path buffer-name)
        t)
       ((mevedel-session-persistence--read-lock lock-path)
        (mevedel-session-persistence-lock-acquire session-dir buffer-name))
       (t
        (user-error "Session lock exists but could not be read: %s"
                    lock-path))))
     ((equal (plist-get existing :hostname) (system-name))
      (cond
       ((mevedel-session-persistence--same-host-lock-active-p existing)
        (let ((response
               (read-char-choice
                (format
                 (concat "Mevedel session locked by a live process on this host:\n"
                         "  PID:    %s\n"
                         "  Since:  %s\n"
                         "  Buffer: %s\n"
                         "[b]reak, [r]ead-only, [a]bort? ")
                 (plist-get existing :pid)
                 (plist-get existing :emacs-invocation-time)
                 (plist-get existing :buffer))
                '(?b ?r ?a))))
          (pcase response
            (?b (mevedel-session-persistence--write-lock lock-path buffer-name)
                t)
            (?r nil)
            (?a (user-error "Session resume aborted")))))
       (t
        (if (y-or-n-p
             (format "Stale mevedel lock (PID %d, buffer %s). Break and proceed? "
                     (plist-get existing :pid)
                     (plist-get existing :buffer)))
            (progn
              (mevedel-session-persistence--write-lock lock-path buffer-name)
              t)
          (user-error "Lock not broken")))))
     (t
      (let ((response
             (read-char-choice
              (format
               (concat "Mevedel session locked by:\n"
                       "  PID:    %s\n"
                       "  Host:   %s\n"
                       "  Since:  %s\n"
                       "  Buffer: %s\n"
                       "[b]reak, [r]ead-only, [a]bort? ")
               (plist-get existing :pid)
               (plist-get existing :hostname)
               (plist-get existing :emacs-invocation-time)
               (plist-get existing :buffer))
              '(?b ?r ?a))))
        (pcase response
          (?b (mevedel-session-persistence--write-lock lock-path buffer-name)
              t)
          (?r nil)
          (?a (user-error "Session resume aborted"))))))))

(defun mevedel-session-persistence-lock-release (session-dir)
  "Delete SESSION-DIR's `.lock' if it was written by this Emacs."
  (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
    (when (and existing
               (eq (plist-get existing :pid) (emacs-pid))
               (equal (plist-get existing :hostname) (system-name)))
      (delete-file lock-path))))

(defun mevedel-session-persistence--sweep-stale-locks (workspace)
  "Silently remove stale `.lock' files in WORKSPACE.

A lock is stale only when its hostname matches this host and the holder
is not active: either its PID is dead, or the PID is live but the live
process start time proves PID reuse.  Cross-host locks are left alone.
Best-effort; any I/O failure is swallowed.

Called opportunistically from `mevedel-resume'."
  (let ((sessions-dir (mevedel-session-persistence--sessions-dir workspace)))
    (when (file-directory-p sessions-dir)
      (dolist (entry (directory-files sessions-dir t "\\`[^.]"))
        (when (file-directory-p entry)
          (let* ((lock-path (mevedel-session-persistence--lock-path entry))
                 (info      (mevedel-session-persistence--read-lock
                             lock-path)))
            (when (and info
                       (equal (plist-get info :hostname) (system-name))
                       (not (mevedel-session-persistence--same-host-lock-active-p
                             info)))
              (condition-case _
                  (delete-file lock-path)
                (error nil)))))))))

(defun mevedel-session-persistence--release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases this session's lock."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when-let ((dir (mevedel-session-save-path mevedel--session)))
      (condition-case _
          (mevedel-session-persistence-lock-release dir)
        (error nil)))))


;;
;;; Workspace relocation reconciliation

(defun mevedel-session-persistence--reconcile-relocation
    (session saved-workspace-plist)
  "Reconcile SESSION's path-bearing fields against workspace relocation.

If SAVED-WORKSPACE-PLIST's `:root' differs from SESSION's current
workspace root, rewrite permission rules whose `:path' starts with
the saved root and is not already under the current root, and prune
touched-files entries pointing at vanished paths.  Logs the rewrite
count to `*Messages*'.

A no-op when the saved root is missing or matches current."
  (let* ((saved-root   (plist-get saved-workspace-plist :root))
         (current-root (mevedel-workspace-root
                        (mevedel-session-workspace session))))
    (when (and saved-root current-root
               (not (equal saved-root current-root)))
      ;; Rewrite path-bearing permission rules.
      (let ((rewrites 0)
            (saved-prefix (file-name-as-directory saved-root))
            (current-prefix (file-name-as-directory current-root)))
        (setf (mevedel-session-permission-rules session)
              (mapcar
               (lambda (rule)
                 (let ((path (plist-get (cdr rule) :path)))
                   (cond
                    ((and path
                          (string-prefix-p saved-prefix path)
                          (not (string-prefix-p current-prefix path)))
                     (cl-incf rewrites)
                     (let ((new-rule (copy-tree rule)))
                       (plist-put
                        (cdr new-rule) :path
                        (concat current-prefix
                                (substring path (length saved-prefix))))
                       new-rule))
                    (t rule))))
               (mevedel-session-permission-rules session)))
        ;; Touched-files resets to an empty hash on load, so there is
        ;; nothing to prune here today; kept as the conceptual place
        ;; if we ever start carrying touched-files across resume.
        (when (> rewrites 0)
          (message "Reconciled %d permission paths from %s to %s"
                   rewrites saved-root current-root))))))


;;
;;; Segment-counter self-heal

(defun mevedel-session-persistence--detect-highest-segment (save-path)
  "Return the highest segment number found on disk under SAVE-PATH, or 0."
  (let ((max-n 0))
    (when (file-directory-p save-path)
      (dolist (f (directory-files save-path nil
                                  "\\`segment-[0-9]+\\.chat\\.org\\'"))
        (when (string-match "segment-\\([0-9]+\\)\\.chat\\.org" f)
          (let ((n (string-to-number (match-string 1 f))))
            (when (> n max-n) (setq max-n n))))))
    max-n))

(defun mevedel-session-persistence--self-heal-segment-counter (session save-path)
  "Reconcile SESSION's `:current-segment' with the filesystem under SAVE-PATH.

If the highest-numbered segment file on disk differs from the sidecar's
recorded `:current-segment', trust the filesystem (the sidecar may be
stale from a crash mid-rotation).  Logs a warning.  When healing upward
after a crash that published a new segment before finalizing its
predecessor, mark the predecessor finalized now."
  (let* ((sidecar-n    (or (mevedel-session-current-segment session) 1))
         (filesystem-n (mevedel-session-persistence--detect-highest-segment
                        save-path)))
    (when (and (> filesystem-n 0)
               (not (= sidecar-n filesystem-n)))
      (display-warning
       'mevedel
       (format "Sidecar :current-segment %d differs from filesystem (%d); using %d"
               sidecar-n filesystem-n filesystem-n)
       :warning)
      (when (> filesystem-n sidecar-n)
        (mevedel-session-persistence--finalize-segment-file
         (mevedel-session-persistence--segment-path save-path
                                                    (1- filesystem-n))))
      (setf (mevedel-session-current-segment session) filesystem-n))))


;;
;;; Save-failure indicator

(defvar-local mevedel-session--save-failed nil
  "Non-nil when the most recent auto-save failed in this buffer.
Set by the DONE-terminal autosave handler on any save error and
cleared on the next successful save.  Surfaced by
`mevedel-session-persistence-header-segment' so the user has a
visible cue in addition to the `display-warning'.")

(defun mevedel-session-persistence-header-segment ()
  "Return a header-line fragment summarising persistence state.
Empty when the session has no unusual state; highlights save
failures and read-only mode when active."
  (let ((parts nil))
    (when (bound-and-true-p mevedel-session--save-failed)
      (push (propertize " [Save failed] " 'face 'error) parts))
    (when (bound-and-true-p mevedel-session--read-only-mode)
      (push (propertize " [Read-only] " 'face 'warning) parts))
    (if parts (apply #'concat parts) "")))


;;
;;; Read path (resume)

(defun mevedel-session-persistence--find-file-noselect (file)
  "Return a buffer visiting persisted mevedel FILE without `so-long'.

Persisted chat and agent transcript files may contain very long org
property lines, especially GPTEL_SYSTEM.  Those lines are expected
data, and letting `so-long' replace `org-mode' breaks gptel/org state
restoration and reveal timers."
  (let ((so-long-predicate (lambda () nil)))
    (find-file-noselect file)))

(defun mevedel-session-persistence-load-sidecar (path)
  "Read sidecar PLIST from PATH, applying version migration.

This is the public read entry point: combines
`mevedel-session-persistence-read' (raw I/O) with
`mevedel-session-persistence--patch-sidecar' (version
forward-migration).  Returns the patched plist, or nil when the
sidecar is missing or unparseable (a warning is logged so the caller
can fall back to a fresh-session treatment per the failure-mode
table)."
  (cond
   ((not (file-exists-p path))
    (display-warning 'mevedel
                     (format "Sidecar missing at %s; treating as fresh session"
                             path)
                     :warning)
    nil)
   (t
    (condition-case err
        (mevedel-session-persistence--patch-sidecar
         (mevedel-session-persistence-read path))
      (error
       (display-warning 'mevedel
                        (format "Sidecar unreadable at %s: %s; treating as fresh session"
                                path (error-message-string err))
                        :warning)
       nil)))))

(defvar-local mevedel-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun mevedel-session-persistence--apply-read-only-mode (buf)
  "Put BUF into read-only session mode.
See `mevedel-session--read-only-mode' for semantics."
  (with-current-buffer buf
    (setq buffer-read-only t)
    (setq-local mevedel-session--read-only-mode t)
    (display-warning
     'mevedel
     "Session opened read-only: another host holds the lock. \
Sends are disabled and nothing will be persisted."
     :warning)))

(defun mevedel-session-persistence--synthesize-session (session-dir workspace)
  "Build a minimal `mevedel-session' when the sidecar is absent.
Used when the sidecar file for SESSION-DIR is missing or unparseable.
WORKSPACE is the current workspace (resolved by the caller)."
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name session-dir)))
         (name (if (string-match
                    "\\`\\(.*?\\)-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]+\\'"
                    dir-name)
                   (match-string 1 dir-name)
                 dir-name))
         (highest (mevedel-session-persistence--detect-highest-segment
                   session-dir))
         (now (format-time-string "%FT%H-%M-%S")))
    (mevedel-session--create
     :name            name
     :workspace       workspace
     :working-directory (mevedel-workspace-root workspace)
     :touched-files   (make-hash-table :test #'equal)
     :mentions-shown  (make-hash-table :test #'equal)
     :session-id      dir-name
     :last-observed-date (format-time-string "%F")
     :agent-types-snapshot :uninitialized
     :save-path       (file-name-as-directory session-dir)
     :current-segment (if (> highest 0) highest 1)
     :created-at      now
     :updated-at      now
     :turn-count      0
     :permission-mode 'default)))

(defun mevedel-session-persistence--find-live-buffer (session-id buf-name)
  "Return the live buffer for SESSION-ID, or nil.
Prefers a buffer whose `mevedel--session' has a matching session-id
so same-named sessions in one workspace do not collide.  Falls back
to a lookup by buffer name for buffers that never materialized a
session-id."
  (or (cl-find-if
       (lambda (b)
         (and (buffer-live-p b)
              (with-current-buffer b
                (and (bound-and-true-p mevedel--session)
                     (equal (mevedel-session-session-id mevedel--session)
                            session-id)))))
       (buffer-list))
      ;; Fallback: buffer-name match only when the buffer has no session
      ;; id yet (e.g. not yet materialized).  If a buffer exists by name
      ;; but its session has a different id, do not return it.
      (let ((candidate (get-buffer buf-name)))
        (when (and candidate
                   (buffer-live-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p mevedel--session))
                         (null (mevedel-session-session-id
                                mevedel--session)))))
          candidate))))

(defun mevedel-session-persistence--maybe-prune-orphan (session-dir segment-path)
  "Offer to delete SESSION-DIR when SEGMENT-PATH is missing.
Called from the restore path when the live segment file cannot be
found.  Returns nil; signals `user-error' to abort the restore."
  (cond
   ((and (file-directory-p session-dir)
         (yes-or-no-p
          (format "Live segment %s is missing.  Delete orphan session directory %s? "
                  segment-path session-dir)))
    (delete-directory session-dir t)
    (user-error "Orphan session directory deleted"))
   (t
    (user-error "Session segment file %s missing" segment-path))))

(defun mevedel-session-persistence-restore (session-dir)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the sidecar (or synthesizes a minimal session when the sidecar
is missing/unreadable), opens the live segment file in a buffer named
per `mevedel-session-buffer-name', enables `org-mode' and
`gptel-mode' (the latter triggers gptel's restore of text-property
bounds and config), hydrates the session struct on the buffer, then
runs `mevedel--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((sidecar-path     (mevedel-session-persistence--sidecar-path session-dir))
         (sidecar          (mevedel-session-persistence-load-sidecar sidecar-path))
         (had-sidecar-p    (not (null sidecar)))
         (result           (when sidecar
                             (mevedel-session-persistence-deserialize sidecar)))
         (session          (or (plist-get result :session)
                               (mevedel-session-persistence--synthesize-session
                                session-dir (mevedel-workspace))))
         (additional-roots (plist-get result :additional-roots))
         (workspace        (mevedel-session-workspace session))
         (sidecar-current-n (and had-sidecar-p
                                 (mevedel-session-current-segment session))))
    ;; `save-path' is intentionally not serialized in the sidecar: the
    ;; session directory itself is the source of truth at restore time.
    (setf (mevedel-session-save-path session)
          (file-name-as-directory session-dir))
    ;; Self-heal: trust the filesystem if sidecar's segment counter is stale.
    (mevedel-session-persistence--self-heal-segment-counter session session-dir)
    ;; Workspace relocation: rewrite path-bearing fields if the saved
    ;; root no longer matches the current workspace's root.
    (when had-sidecar-p
      (mevedel-session-persistence--reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((segment-n    (mevedel-session-current-segment session))
           (segment-path (mevedel-session-persistence--segment-path
                          session-dir segment-n))
           (buf-name     (mevedel-session-buffer-name
                          (mevedel-session-name session)
                          workspace))
           (session-id   (mevedel-session-session-id session))
           ;; Prefer session-id-based lookup so two saved sessions
           ;; named `main' in one workspace don't collide.
           (live         (mevedel-session-persistence--find-live-buffer
                          session-id buf-name))
           ;; Acquire the lock BEFORE opening the segment file.  If the
           ;; user aborts the conflict prompt (`user-error') we unwind
           ;; before any buffer is materialized, so no stray half-
           ;; initialized chat buffer is left behind.  Read-only
           ;; acquisitions return nil; we still open the buffer but
           ;; flip it to read-only below.  `live' skips acquisition
           ;; because the buffer is already owned by this Emacs.
           (acquired     (unless live
                           (mevedel-session-persistence-lock-acquire
                            session-dir buf-name)))
           (buf          (or live
                             (and (file-exists-p segment-path)
                                  (mevedel-session-persistence--find-file-noselect
                                   segment-path))))
           (setup-done   nil))
      (unwind-protect
          (progn
            (unless (and buf (buffer-live-p buf) (file-exists-p segment-path))
              (mevedel-session-persistence--maybe-prune-orphan
               session-dir segment-path))
            (with-current-buffer buf
              ;; Ensure canonical name and file backing.
              (unless (equal (buffer-name) buf-name)
                (rename-buffer buf-name t))
              (unless (equal (expand-file-name buffer-file-name)
                             (expand-file-name segment-path))
                (setq buffer-file-name segment-path))
              (when (and (derived-mode-p 'org-mode)
                         (fboundp 'mevedel--chat-buffer-disable-org-element-cache))
                (mevedel--chat-buffer-disable-org-element-cache))
              (unless live
                ;; Plant the hydrated session struct BEFORE enabling
                ;; `gptel-mode' so any restore-protocol hook that reads
                ;; `mevedel--session' sees the correct value.
                (setq-local mevedel--session session)
                (setq-local mevedel--workspace workspace)
                (setq-local default-directory
                            (mevedel-session-working-directory session))
                (when additional-roots
                  (setq-local mevedel-workspace-additional-roots additional-roots))
                ;; Mode + gptel restore for freshly opened files only;
                ;; live buffers are already initialized.
                (unless (derived-mode-p 'org-mode)
                  (let ((org-agenda-file-menu-enabled nil))
                    (org-mode)))
                (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
                  (mevedel--chat-buffer-disable-org-element-cache))
                (mevedel-session-persistence--restore-gptel-state)
                (unless acquired
                  (mevedel-session-persistence--apply-read-only-mode buf))
                ;; rewrite running -> incomplete unless we
                ;; opened in read-only attach mode (another Emacs is
                ;; the live writer; rewriting would corrupt its log).
                (mevedel-session-persistence--mark-running-incomplete-on-resume
                 session
                 (bound-and-true-p mevedel-session--read-only-mode))
                (mevedel--chat-buffer-init-common buf workspace))
              (unless live
                (mevedel-session-persistence--load-instructions session buf))
              ;; Persist the self-healed segment counter so subsequent
              ;; resumes don't re-detect the mismatch.
              (when (and had-sidecar-p
                         sidecar-current-n
                         (not (= sidecar-current-n segment-n)))
                (condition-case _
                    (mevedel-session-persistence-write
                     (mevedel-session-persistence--sidecar-path session-dir)
                     (mevedel-session-persistence--build-sidecar session buf))
                  (error nil)))
              ;; Re-render the companion view buffer from the restored
              ;; segment.  `init-common' ensures the view buffer exists but
              ;; does not populate it; without this the user sees an empty
              ;; view after resume.
              (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buf))
                          ((buffer-live-p vb)))
                (with-current-buffer vb
                  (unless live
                    (require 'mevedel-view-history)
                    (mevedel-view-history-load session))
                  (mevedel-view--full-rerender))))
            (setq setup-done t)
            buf)
        ;; If any of the setup above failed non-locally, don't leak the
        ;; lock we just acquired or the freshly-opened buffer.
        (unless setup-done
          (when (and acquired (not live))
            (condition-case _
                (mevedel-session-persistence-lock-release session-dir)
              (error nil)))
          (when (and (not live) buf (buffer-live-p buf))
            (condition-case _
                (kill-buffer buf)
              (error nil))))))))


;;
;;; File restore plan

(defun mevedel-session-persistence--latest-snapshot-entry (session path)
  "Return the highest-version snapshot plist for PATH in SESSION, or nil."
  (let ((best nil) (best-version 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best-version)
          (setq best-version v
                best          (cdr entry)))))))

(defun mevedel-session-persistence--state-at-turn (session cum-turn)
  "Return an alist (PATH . PLIST) representing tracked-file state at CUM-TURN.

For each path that ever appeared in SESSION's `:file-snapshots',
picks the latest snapshot whose turn is `<=' CUM-TURN.  This is the
target state for a rewind to CUM-TURN -- files at the moment that
turn's response was saved."
  (let ((state (make-hash-table :test #'equal)))
    (dolist (turn-entry
             (sort (copy-sequence (mevedel-session-file-snapshots session))
                   (lambda (a b) (< (car a) (car b)))))
      (let ((turn (car turn-entry)))
        (when (<= turn cum-turn)
          (dolist (file-entry (cdr turn-entry))
            (puthash (car file-entry) (cdr file-entry) state)))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) state)
      result)))

(defun mevedel-session-persistence--plan-action (session path target-plist)
  "Return a plan-entry plist describing what restore should do for PATH.

TARGET-PLIST is the snapshot entry recorded for PATH at the picked turn
(or earlier).  Possible `:action' values:

  noop       File already matches target state.
  delete     Target state is absent; file currently exists.
  create     Target has content; file currently absent.
  restore    Target has content; file differs but matches its own
             latest snapshot (i.e., no detected external changes).
  overwrite  Target has content; file differs from target AND from
             latest snapshot (external edits will be overwritten)."
  (let* ((target-backup-name (plist-get target-plist :backup-name))
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
              (mevedel-file-history--read-file-raw
               (mevedel-file-history--backup-path
                (mevedel-session-save-path session)
                target-backup-name)))
             (current-content
              (mevedel-file-history--read-file-raw path)))
        (if (string-equal current-content target-content)
            (list :action 'noop :path path)
          ;; Differs from target.  Check vs LATEST snapshot to detect
          ;; external changes since.
          (let* ((latest (mevedel-session-persistence--latest-snapshot-entry
                          session path))
                 (latest-name (and latest (plist-get latest :backup-name)))
                 (latest-content
                  (and latest-name
                       (mevedel-file-history--read-file-raw
                        (mevedel-file-history--backup-path
                         (mevedel-session-save-path session)
                         latest-name))))
                 (diverged (not (and latest-content
                                     (string-equal current-content
                                                   latest-content)))))
            (list :action (if diverged 'overwrite 'restore)
                  :path path
                  :backup-name target-backup-name
                  :diverged diverged))))))))

(defun mevedel-session-persistence-restore-plan (session cum-turn)
  "Compute the file-restore plan for SESSION at cumulative turn CUM-TURN.

Returns a list of plan-entry plists (see
`mevedel-session-persistence--plan-action').  An empty list means
nothing to do."
  (let ((target-state
         (mevedel-session-persistence--state-at-turn session cum-turn))
        (plan nil))
    (dolist (entry target-state)
      (push (mevedel-session-persistence--plan-action
             session (car entry) (cdr entry))
            plan))
    (cl-remove-if
     (lambda (e) (eq 'noop (plist-get e :action)))
     (nreverse plan))))

(defun mevedel-session-persistence--summarize-plan (plan)
  "Return a human-readable one-line summary of restore PLAN."
  (let ((counts (make-hash-table)))
    (dolist (entry plan)
      (cl-incf (gethash (plist-get entry :action) counts 0)))
    (format "%d create, %d restore, %d overwrite, %d delete"
            (gethash 'create counts 0)
            (gethash 'restore counts 0)
            (gethash 'overwrite counts 0)
            (gethash 'delete counts 0))))

(defvar-local mevedel-session-persistence--plan-buffer-session nil
  "Buffer-local session for the `*mevedel-restore-plan*' buffer.
Consumed by `mevedel-session-persistence--plan-row-diff' so `d' on a
plan row can resolve the backup file.")

(defvar mevedel-session-persistence--plan-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "d")
      #'mevedel-session-persistence--plan-row-diff)
    map)
  "Keymap for the `*mevedel-restore-plan*' buffer.
Adds `d' -- show diff between current file and target snapshot.")

(defun mevedel-session-persistence--plan-row-diff ()
  "Show a diff between the current file and the restore target backup.
Invoked from `*mevedel-restore-plan*' via the `d' binding.  The row
at point identifies the path; its `mevedel-plan-entry' text property
carries the plan-entry plist."
  (interactive)
  (let* ((entry
          (get-text-property (line-beginning-position) 'mevedel-plan-entry))
         (session mevedel-session-persistence--plan-buffer-session))
    (unless entry
      (user-error "No restore plan row at point"))
    (unless session
      (user-error "Plan buffer has no associated session"))
    (let* ((path        (plist-get entry :path))
           (action      (plist-get entry :action))
           (backup-name (plist-get entry :backup-name))
           (backup-path (and backup-name
                             (mevedel-file-history--backup-path
                              (mevedel-session-save-path session)
                              backup-name))))
      (pcase action
        ('delete
         (user-error "Row is a delete action; nothing to diff against"))
        ('noop
         (user-error "Row is a noop; nothing to diff"))
        (_
         (unless (and backup-path (file-exists-p backup-path))
           (user-error "Backup file missing: %s" backup-path))
         (diff (or (and (file-exists-p path) path) "/dev/null")
               backup-path nil 'no-async))))))

(defun mevedel-session-persistence--render-plan-buffer (plan &optional session)
  "Render PLAN into `*mevedel-restore-plan*' for user inspection.
When SESSION is non-nil, attach it so the `d' binding can resolve
backup paths for per-row diffs."
  (with-current-buffer (get-buffer-create "*mevedel-restore-plan*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Restore plan: %s\n"
                      (mevedel-session-persistence--summarize-plan plan)))
      (insert "Press `d' on a row to diff current vs. snapshot.\n")
      (insert "================================\n\n")
      (dolist (entry plan)
        (let ((row-start (point)))
          (insert
           (format "  %-9s  %s%s\n"
                   (symbol-name (plist-get entry :action))
                   (plist-get entry :path)
                   (if (plist-get entry :diverged)
                       "  (current != latest snapshot -- external edits)"
                     "")))
          (put-text-property row-start (point)
                             'mevedel-plan-entry entry))))
    (special-mode)
    (use-local-map mevedel-session-persistence--plan-buffer-map)
    (setq-local mevedel-session-persistence--plan-buffer-session session)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun mevedel-session-persistence--apply-restore-action (session entry)
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
       (let* ((backup-path (mevedel-file-history--backup-path
                            (mevedel-session-save-path session)
                            backup-name))
              (content     (mevedel-file-history--read-file-raw backup-path)))
         (let ((dir (file-name-directory path)))
           (when (and dir (not (file-directory-p dir)))
             (make-directory dir t)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region content nil path nil 'silent)))))))

(defun mevedel-session-persistence-execute-restore (session plan)
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
              (mevedel-session-persistence--apply-restore-action session entry)
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

(defun mevedel-session-persistence--modified-buffers-for-plan (plan)
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

(defun mevedel-session-persistence--prepare-buffers-for-restore
    (session cum-turn plan)
  "Prepare visiting buffers before restoring PLAN for SESSION.

If modified buffers visit affected files, prompt the user to save,
discard, or abort.  Returns the current restore plan, recomputing it
after saves.  Returns nil when the restore should be aborted."
  (let ((current-plan plan)
        done)
    (while (not done)
      (let ((buffers
             (mevedel-session-persistence--modified-buffers-for-plan
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
                   (mevedel-session-persistence-restore-plan
                    session cum-turn)))
            (?d
             (dolist (buf buffers)
               (with-current-buffer buf
                 (set-buffer-modified-p nil)))
             (setq done t))
            (?a
             (setq current-plan :abort
                   done t))))))
    current-plan))

(defun mevedel-session-persistence--refresh-restored-buffers (plan result)
  "Refresh visiting buffers for files restored by PLAN.

RESULT is the plist returned by `mevedel-session-persistence-execute-restore'."
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

(defvar-local mevedel-session--fork-pending nil
  "Non-nil when the current buffer is a back-in-time view of a session.

Set by `mevedel-rewind' after loading a truncated segment; cleared by
`mevedel-session-persistence-fork-now' once the next user message
materializes the fork.  When non-nil, `buffer-file-name' is also nil
so saves cannot overwrite the original segment file.")

(defvar-local mevedel-session--rewind-context nil
  "Plist describing this buffer's rewind preview state.

Set by `mevedel-session-persistence--load-truncated' alongside
`mevedel-session--fork-pending'.  Consumed by
`mevedel-session-persistence-fork-now' to materialize the fork
(it needs to know which parent session, segment, and cumulative turn
the picked prompt corresponds to so that predecessor segments and the
referenced file-history backups can be copied into the fork's directory).

Plist keys:
  :parent-session-id  Parent session's id.
  :parent-save-path   Parent's session directory.
  :parent-session-name Parent's session-name (preserved across the fork).
  :picked-segment     Per-segment index of the picked prompt.
  :picked-turn        Per-segment turn of the picked prompt.
  :picked-file-turn   Raw user prompt ordinal in the segment file.
  :picked-cum-turn    Cumulative turn (used as `:file-snapshots' key).")

(defun mevedel-session-persistence--prompt-candidates (session)
  "Return an alist of `(DISPLAY . PLIST)' for SESSION's prompts.

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
      (let* ((segment-n (car segment-entry))
             (tail-count
              (mevedel-session-persistence--segment-tail-prompt-count-for-session
               session segment-n)))
        (dolist (prompt (reverse (cdr segment-entry)))
          (let* ((preview (or (plist-get prompt :preview) "(empty prompt)"))
                 (turn    (plist-get prompt :turn))
                 (file-turn
                  (or (plist-get prompt :file-turn)
                      (and (integerp turn) (+ tail-count turn))
                      turn))
                 (display (format "S%d T%d  %s" segment-n turn preview)))
            (push (cons display
                        (list :segment  segment-n
                              :turn     turn
                              :file-turn file-turn
                              :cum-turn (plist-get prompt :cum-turn)
                              :pos      (plist-get prompt :pos)
                              :preview  preview))
                  all)))))
    (nreverse all)))

(defvar mevedel-session-persistence--prompt-history nil
  "History list for `mevedel-rewind' picks.")

(defun mevedel-session-persistence--prompt-collection-fn (candidates lookup)
  "Return a completion table for the rewind picker.

CANDIDATES is the list returned by
`mevedel-session-persistence--prompt-candidates' (each element is
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

(defun mevedel-session-persistence--format-relative-time (iso)
  "Format ISO (a `YYYY-MM-DDTHH-MM-SS' string) as a relative age.
Returns strings like `2h ago' / `yesterday' / `Apr 22'.  Returns a
placeholder when ISO cannot be parsed."
  (let ((t2 (mevedel-session-persistence--parse-iso-time iso)))
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

(defun mevedel-session-persistence--find-turn-cutoff (turn-n)
  "Return the position right before the (TURN-N + 1)th user prompt.
Returns `point-max' when TURN-N is the final user prompt.  Skips the
leading org property drawer, `#+begin_summary'/`#+end_summary' block
bodies, and gptel org tool/reasoning scaffolding to stay consistent with
`mevedel-session-persistence--collect-prompts'."
  (save-excursion
    (save-restriction
      (widen)
      (or (plist-get
           (nth turn-n
                (mevedel-session-persistence--collect-prompts
                 (current-buffer)))
           :pos)
          (point-max)))))

(defun mevedel-session-persistence--load-truncated
    (session buffer segment-n file-turn-n &optional cum-turn logical-turn-n)
  "Reload BUFFER from SESSION's SEGMENT-N truncated to FILE-TURN-N's response.

Erases BUFFER, re-inserts the segment file's content, restores gptel's
text-property bounds, then truncates everything after the picked
turn's response.  Sets `buffer-file-name' to nil and the buffer-local
`mevedel-session--fork-pending' flag so the next send creates a fork
and so save cannot overwrite the original segment file in the meantime.

CUM-TURN, if provided, is recorded in the rewind context for use by
`mevedel-session-persistence-fork-now'.  LOGICAL-TURN-N, if provided,
is the displayed per-segment turn after copied compaction tail prompts
have been excluded."
  (let ((segment-path (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) segment-n)))
    (unless (file-exists-p segment-path)
      (user-error "Segment %d file missing: %s"
                  segment-n segment-path))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Read with the segment-path temporarily set as buffer-file-name
        ;; so gptel-mode's restore-state path can find the file-locals.
        (let ((buffer-file-name segment-path))
          (insert-file-contents segment-path)
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
              (mevedel--chat-buffer-disable-org-element-cache))
            ;; Force re-restoration of GPTEL_BOUNDS from the org property.
            (when (fboundp 'gptel-org--restore-state)
              (mevedel-session-persistence--sanitize-gptel-bounds)
              (gptel-org--restore-state))))
        (let ((cutoff
               (mevedel-session-persistence--find-turn-cutoff file-turn-n)))
          (when (and cutoff (< cutoff (point-max)))
            (delete-region cutoff (point-max))))
        (mevedel-session-persistence--normalize-gptel-properties))
      ;; Disconnect from the original file so saves can't corrupt it.
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil)
      (setq-local mevedel-session--fork-pending t)
      (setq-local mevedel-session--rewind-context
                  (list :parent-session-id   (mevedel-session-session-id session)
                        :parent-save-path    (mevedel-session-save-path session)
                        :parent-session-name (mevedel-session-name session)
                        :picked-segment      segment-n
                        :picked-turn         (or logical-turn-n file-turn-n)
                        :picked-file-turn    file-turn-n
                        :picked-cum-turn     cum-turn))
      ;; Reset derived in-memory state so it can't reflect the post-
      ;; truncation (pre-rewind) turns.  Tasks and touched-files are
      ;; cleared rather than replayed from the buffer; if the user
      ;; needs the same task list after the rewind they can re-run
      ;; TaskCreate.  mentions-shown is always reset on rewind
      ;; because expanded @mentions from future turns shouldn't dedup
      ;; away re-expansion in the fork.
      (setf (mevedel-session-tasks session) nil)
      (clrhash (mevedel-session-touched-files session))
      (clrhash (mevedel-session-mentions-shown session))
      ;; Re-render the companion view buffer so the user sees the
      ;; truncated conversation instead of stale content.
      (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                  ((buffer-live-p vb)))
        (with-current-buffer vb
          (when (fboundp 'mevedel-view-reset-agent-ephemeral-state)
            (mevedel-view-reset-agent-ephemeral-state))
          (mevedel-view--full-rerender))))))

;;;###autoload
(defun mevedel-rewind ()
  "Pick a previous user prompt in the current session and rewind to it.

Truncates the live buffer back to the picked prompt's response, sets
`buffer-file-name' to nil so saves cannot corrupt the original segment
file, and flags the buffer for fork-on-next-send.  The original
session's segment files are never modified -- every back-in-time pick
is reversible (re-pick the latest prompt to come back).

Refuses with `user-error' when invoked outside a mevedel chat buffer
or while a request is in flight.  Selecting the latest prompt is a
no-op."
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
    (when (buffer-local-value 'mevedel--current-request buffer)
      (user-error "Abort the current request first"))
    ;; Refresh the live segment before presenting the picker.  This
    ;; repairs older sidecars whose prompt-index may include org block
    ;; scaffolding and keeps the picker in sync with manual data-buffer
    ;; edits since the last save.
    (mevedel-session-persistence--update-prompt-index session buffer)
    ;; Live sub-agents would race against the rewind's truncation and
    ;; fork materialization -- abort them first.  Their ABRT handlers
    ;; finalize transcripts and remove themselves from the registry,
    ;; so a quick poll suffices to wait for the teardown to settle.
    (when (and (buffer-local-value 'mevedel-tools--agents-fsm buffer)
               (fboundp 'mevedel-abort))
      (let ((deadline (+ (float-time) 5.0)))
        (mevedel-abort buffer)
        (while (and (buffer-local-value 'mevedel-tools--agents-fsm buffer)
                    (< (float-time) deadline))
          (sit-for 0.05))
        (when (buffer-local-value 'mevedel-tools--agents-fsm buffer)
          (user-error "Sub-agents did not finalize within 5s; \
retry rewind"))))
    (let* ((candidates
            (mevedel-session-persistence--prompt-candidates session)))
      (unless candidates
        (user-error "Session has no recorded user prompts"))
      (let* ((lookup (make-hash-table :test #'equal)))
        (dolist (c candidates)
          (puthash (car c) (cdr c) lookup))
        (let* ((collection
                (mevedel-session-persistence--prompt-collection-fn
                 candidates lookup))
               (default (caar (last candidates)))
               (chosen  (completing-read
                         "Rewind to: " collection nil t
                         nil 'mevedel-session-persistence--prompt-history
                         default))
               (entry   (cons chosen (gethash chosen lookup))))
          (when entry
            (let ((picked-segment  (plist-get (cdr entry) :segment))
                  (picked-turn     (plist-get (cdr entry) :turn))
                  (picked-file-turn
                   (or (plist-get (cdr entry) :file-turn)
                       (plist-get (cdr entry) :turn)))
                  (picked-cum-turn (plist-get (cdr entry) :cum-turn))
                  (current-segment
                   (mevedel-session-current-segment session))
                  (latest-turn
                   (when-let ((live-prompts
                               (cdr (assoc
                                     (mevedel-session-current-segment session)
                                     (mevedel-session-prompt-index session)))))
                     (plist-get (car (last live-prompts)) :turn))))
              (cond
               ((and (eq picked-segment current-segment)
                     (eq picked-turn latest-turn))
                (message "Already at the latest prompt; nothing to rewind."))
               (t
                (mevedel-session-persistence--load-truncated
                 session buffer picked-segment picked-file-turn
                 picked-cum-turn picked-turn)
                ;; Compute and (after confirmation) execute the file restore.
                (when picked-cum-turn
                  (let ((plan (mevedel-session-persistence-restore-plan
                               session picked-cum-turn)))
                    (cond
                     ((null plan)
                      (message "Rewound to S%d T%d.  No file changes to apply."
                               picked-segment picked-turn)
                      (mevedel-session-persistence--load-instructions
                       session buffer picked-cum-turn))
                     (t
                      (mevedel-session-persistence--render-plan-buffer
                       plan session)
                      (if (yes-or-no-p
                           (format "Apply restore plan (%s)? "
                                   (mevedel-session-persistence--summarize-plan
                                    plan)))
                          (let ((prepared-plan
                                 (mevedel-session-persistence--prepare-buffers-for-restore
                                  session picked-cum-turn plan)))
                            (cond
                             ((eq prepared-plan :abort)
                              (message "File restore aborted; conversation rewound only."))
                             ((null prepared-plan)
                              (message "Rewound to S%d T%d.  No file changes to apply."
                                       picked-segment picked-turn)
                              (mevedel-session-persistence--load-instructions
                               session buffer picked-cum-turn))
                             (t
                              (let ((res
                                     (mevedel-session-persistence-execute-restore
                                      session prepared-plan)))
                                (mevedel-session-persistence--refresh-restored-buffers
                                 prepared-plan res)
                                (message
                                 "Rewind: %d/%d files restored%s"
                                 (plist-get res :succeeded)
                                 (plist-get res :total)
                                 (if (plist-get res :failed)
                                     (format "; failed on %s"
                                             (plist-get res :failed))
                                   ""))
                                (mevedel-session-persistence--load-instructions
                                 session buffer picked-cum-turn)))))
                        (message
                         "File restore skipped; conversation rewound only.")))))))))))))))


;;
;;; Fork-on-send and rename

(defun mevedel-session-persistence--agent-files-for-segments
    (prompt-index agent-transcripts picked-segment picked-cum-turn)
  "Return transcript entries whose `:parent-turn' is in copied ranges.

PROMPT-INDEX is the session sidecar's segment prompt index.
PICKED-SEGMENT and PICKED-CUM-TURN describe the rewind target.  The
copied transcript set is derived from concrete segment ranges:
predecessor segments are copied whole; the picked segment is copied
only through PICKED-CUM-TURN.  Entries with non-integer
`:parent-turn' are excluded."
  (let ((ranges nil))
    (dolist (seg-entry prompt-index)
      (let ((seg (car seg-entry))
            (prompts (cdr seg-entry)))
        (when (and (integerp seg)
                   (or (< seg picked-segment)
                       (= seg picked-segment)))
          (let* ((turns (cl-loop for p in prompts
                                 for ct = (plist-get p :cum-turn)
                                 when (and (integerp ct)
                                           (or (< seg picked-segment)
                                               (null picked-cum-turn)
                                               (<= ct picked-cum-turn)))
                                 collect ct))
                 (lo (and turns (apply #'min turns)))
                 (hi (and turns (apply #'max turns))))
            (when (and lo hi)
              (push (cons lo hi) ranges))))))
    (setq ranges (nreverse ranges))
    (cl-loop for entry in agent-transcripts
             for parent-turn = (plist-get (cdr entry) :parent-turn)
             when (and (integerp parent-turn)
                       (cl-some (lambda (range)
                                  (and (<= (car range) parent-turn)
                                       (<= parent-turn (cdr range))))
                                ranges))
             collect entry)))

(defun mevedel-session-persistence-fork-now (buffer)
  "Materialize a fork from BUFFER's rewind preview state.

Creates a fresh fork session whose `:session-name' is inherited from
the parent.  Predecessor segments (1..picked-segment-1) are copied
verbatim; the picked segment becomes the fork's truncated current
segment file (saved from BUFFER's content).  File-history backups
referenced by the target state are copied into the fork's file-history
directory.

The session struct on BUFFER is mutated in place: `:session-id',
`:save-path', `:created-at', `:updated-at', `:current-segment', and
`:forked-from-*' are updated; `:fork-pending' and the rewind context
are cleared.  `buffer-file-name' is repointed at the fork's segment.
The new session's lock is acquired.

Errors when BUFFER is not in rewind preview state.  Returns the
fork's save-path."
  (with-current-buffer buffer
    (unless mevedel-session--fork-pending
      (user-error "Buffer is not in rewind preview state"))
    (unless mevedel-session--rewind-context
      (user-error "Rewind context missing"))
    (let* ((ctx              mevedel-session--rewind-context)
           (parent-id        (plist-get ctx :parent-session-id))
           (parent-save-path (plist-get ctx :parent-save-path))
           (picked-segment   (plist-get ctx :picked-segment))
           (picked-cum-turn  (plist-get ctx :picked-cum-turn))
           (session          mevedel--session)
           (sessions-dir     (mevedel-session-persistence--sessions-dir
                              (mevedel-session-workspace session)))
           (new-id           (mevedel-session-persistence--compute-id
                              (mevedel-session-name session)))
           (new-save-path    (file-name-as-directory
                              (file-name-concat sessions-dir new-id)))
           (new-segment-path (mevedel-session-persistence--segment-path
                              new-save-path picked-segment))
           (now              (format-time-string "%FT%H-%M-%S")))
      ;; Create the fork's directory tree.
      (make-directory new-save-path t)
      (make-directory (file-name-concat new-save-path "agents") t)
      (make-directory (file-name-concat new-save-path "file-history") t)
      (when-let* ((parent-plans-dir
                   (and parent-save-path
                        (file-name-concat parent-save-path "plans")))
                  ((file-directory-p parent-plans-dir)))
        (copy-directory parent-plans-dir
                        (file-name-concat new-save-path "plans")
                        nil t t))
      ;; Copy predecessor segment files (1 .. picked-segment-1).
      (cl-loop for i from 1 below picked-segment do
               (let ((src (mevedel-session-persistence--segment-path
                           parent-save-path i))
                     (dst (mevedel-session-persistence--segment-path
                           new-save-path i)))
                 (when (file-exists-p src)
                   (copy-file src dst))))
      ;; Save the live (truncated) buffer content into the fork's picked-segment.
      (setq buffer-file-name new-segment-path)
      (set-buffer-modified-p t)
      (save-buffer)
      ;; Copy file-history backups referenced by the target state.
      (when picked-cum-turn
        (let ((target-state
               (mevedel-session-persistence--state-at-turn
                session picked-cum-turn)))
          (dolist (entry target-state)
            (when-let* ((bn  (plist-get (cdr entry) :backup-name))
                        (src (mevedel-file-history--backup-path
                              parent-save-path bn))
                        ((file-exists-p src)))
              (let ((dst (mevedel-file-history--backup-path
                          new-save-path bn)))
                (unless (file-exists-p dst)
                  (copy-file src dst)))))))
      ;; Copy agent transcript files for any agent whose
      ;; :parent-turn falls at or before picked-cum-turn (those
      ;; agents were spawned within the segments being copied to
      ;; the fork).  Agents spawned after picked-cum-turn don't
      ;; belong in the fork.  The agent-id list comes from a pure
      ;; helper so the derivation is testable in isolation.
      ;;
      ;; Each entry is gated on
      ;; `mevedel-session-persistence--validate-transcript-path' to
      ;; prevent a poisoned sidecar with `:path "../../etc/passwd"'
      ;; from copying outside the fork's `agents/' directory.
      ;; Per-entry errors warn and continue rather than abort the
      ;; fork mid-way.
      (when (and picked-cum-turn parent-save-path)
        (let ((entries
               (mevedel-session-persistence--agent-files-for-segments
                (mevedel-session-prompt-index session)
                (mevedel-session-agent-transcripts session)
                picked-segment
                picked-cum-turn)))
          (dolist (entry entries)
            (let* ((plist (cdr entry))
                   (rel-path (plist-get plist :path)))
              (when (and rel-path
                         (mevedel-session-persistence--validate-transcript-path
                          rel-path parent-save-path)
                         (mevedel-session-persistence--validate-transcript-path
                          rel-path new-save-path))
                (condition-case err
                    (let ((src (expand-file-name rel-path parent-save-path))
                          (dst (expand-file-name rel-path new-save-path)))
                      (when (file-exists-p src)
                        (let ((dst-dir (file-name-directory dst)))
                          (when (and dst-dir (not (file-directory-p dst-dir)))
                            (make-directory dst-dir t)))
                        (unless (file-exists-p dst)
                          (copy-file src dst))))
                  (error
                   (display-warning
                    'mevedel
                    (format "Fork: failed to copy transcript %s: %S"
                            rel-path err)
                    :warning))))))))
      ;; Release the parent's lock before overwriting `save-path' so
      ;; the kill-buffer hook can't leak it.  Only release our own
      ;; lock (the helper checks PID+hostname and is a no-op if
      ;; another process holds it).
      (when parent-save-path
        (condition-case _
            (mevedel-session-persistence-lock-release parent-save-path)
          (error nil)))
      ;; Reduce session state to the picked turn.  Drop prompt-index
      ;; entries for segments past the picked one and turn entries
      ;; beyond the picked-cum-turn in the live segment.  Trim
      ;; file-snapshots to `<= picked-cum-turn'.  Clamp the turn count
      ;; so the restore-plan math lines up.
      (setf (mevedel-session-prompt-index session)
            (mevedel-session-persistence--reduce-prompt-index
             (mevedel-session-prompt-index session)
             picked-segment picked-cum-turn))
      (setf (mevedel-session-file-snapshots session)
            (mevedel-session-persistence--reduce-file-snapshots
             (mevedel-session-file-snapshots session)
             picked-cum-turn))
      (when picked-cum-turn
        ;; drop transcript entries spawned after the fork point.
        (mevedel-session-persistence--prune-agent-transcripts-after-fork
         session picked-cum-turn)
        (setf (mevedel-session-turn-count session) picked-cum-turn))
      ;; Update the session struct in place.
      (setf (mevedel-session-session-id session)             new-id)
      (setf (mevedel-session-save-path session)              new-save-path)
      (setf (mevedel-session-created-at session)             now)
      (setf (mevedel-session-updated-at session)             now)
      (setf (mevedel-session-current-segment session)        picked-segment)
      (setf (mevedel-session-forked-from-session-id session) parent-id)
      (setf (mevedel-session-forked-from-turn session)       picked-cum-turn)
      ;; Acquire the fork's lock.
      (mevedel-session-persistence-lock-acquire
       new-save-path (buffer-name buffer))
      ;; Write the fork's sidecar.
      (mevedel-session-persistence-write
       (mevedel-session-persistence--sidecar-path new-save-path)
       (mevedel-session-persistence--build-sidecar session buffer))
      (mevedel-session-persistence--save-instructions session buffer)
      ;; Forking changes the branch identity; live activity previews
      ;; belong to the parent branch and must not bleed into the fork.
      (when (boundp 'mevedel-tools--agents-fsm)
        (dolist (pair mevedel-tools--agents-fsm)
          (when-let* ((inv (and (fboundp 'mevedel-tools--agent-invocation-at)
                                (mevedel-tools--agent-invocation-at
                                 (cdr pair)))))
            (mevedel-agent-invocation-set-activity inv nil))))
      (when-let* ((vb (and (boundp 'mevedel--view-buffer) mevedel--view-buffer))
                  ((buffer-live-p vb)))
        (when (fboundp 'mevedel-view-reset-agent-ephemeral-state)
          (mevedel-view-reset-agent-ephemeral-state vb)))
      ;; Clear rewind state.
      (setq mevedel-session--fork-pending nil)
      (setq mevedel-session--rewind-context nil)
      new-save-path)))

(defun mevedel-session-persistence--reduce-prompt-index
    (index picked-segment picked-cum-turn)
  "Return a copy of INDEX trimmed to the fork's picked turn.
Drops segments past PICKED-SEGMENT entirely.  In the picked segment,
keeps only prompts whose `:cum-turn' is `<=' PICKED-CUM-TURN (or all
prompts when PICKED-CUM-TURN is nil, falling back to the
segment-local `:turn' when no cumulative key is available)."
  (cl-loop for (seg . prompts) in index
           when (< seg picked-segment)
           collect (cons seg (copy-sequence prompts))
           when (= seg picked-segment)
           collect (cons
                    seg
                    (cl-remove-if-not
                     (lambda (p)
                       (let ((ct (plist-get p :cum-turn)))
                         (or (null picked-cum-turn)
                             (and ct (<= ct picked-cum-turn))
                             ;; Fallback when cum-turn is absent:
                             ;; keep everything so we don't drop
                             ;; prior turns blindly.
                             (null ct))))
                     prompts))))

(defun mevedel-session-persistence--reduce-file-snapshots
    (snapshots picked-cum-turn)
  "Return SNAPSHOTS trimmed to entries whose turn is `<=' PICKED-CUM-TURN.
SNAPSHOTS is an alist keyed by cumulative turn number.  When
PICKED-CUM-TURN is nil, returns SNAPSHOTS unchanged."
  (if (null picked-cum-turn)
      snapshots
    (cl-remove-if-not
     (lambda (entry) (<= (car entry) picked-cum-turn))
     snapshots)))

;;;###autoload
(defun mevedel-rename-session (new-name)
  "Rename the current session to NEW-NAME.

Updates `:session-name', renames the on-disk session directory (so
its prefix matches the new name), updates `:save-path' / `:session-id'
on the session struct, repoints the buffer's `buffer-file-name' to
the renamed directory, rewrites the sidecar, and renames the chat
buffer per `mevedel-session-buffer-name'.

Works from a chat buffer or a view buffer."
  (interactive "sNew session name: ")
  (let* ((data-buf
          (cond
           ((and (boundp 'mevedel--session) mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (let ((sanitized (mevedel-session-persistence--sanitize new-name)))
      (when (string-empty-p sanitized)
        (user-error "Empty session name"))
      ;; Rename the on-disk directory if the session is materialized.
      (when (mevedel-session-save-path session)
        (let* ((old-save-path (mevedel-session-save-path session))
               (parent-dir    (file-name-directory
                               (directory-file-name old-save-path)))
               (old-id        (or (mevedel-session-session-id session)
                                  (file-name-nondirectory
                                   (directory-file-name old-save-path))))
               (old-name-sanitized
                (mevedel-session-persistence--sanitize
                 (mevedel-session-name session)))
               ;; Derive the suffix by stripping the old sanitized
               ;; name from the start of the id.  This does not
               ;; hard-code the id format beyond "name + dash +
               ;; whatever" -- changes to the timestamp/uuid portion
               ;; do not break rename.  Fallback: rebuild a fresh id
               ;; from scratch.
               (prefix         (concat old-name-sanitized "-"))
               (suffix         (if (and (stringp old-id)
                                        (string-prefix-p prefix old-id))
                                   (substring old-id (length old-name-sanitized))
                                 nil))
               (new-id         (if suffix
                                   (concat sanitized suffix)
                                 (mevedel-session-persistence--compute-id
                                  sanitized)))
               (new-save-path  (file-name-as-directory
                                (file-name-concat parent-dir new-id))))
          (rename-file (directory-file-name old-save-path)
                       (directory-file-name new-save-path))
          (setf (mevedel-session-save-path session) new-save-path)
          (setf (mevedel-session-session-id session) new-id)
          (with-current-buffer data-buf
            (when buffer-file-name
              (setq buffer-file-name
                    (file-name-concat
                     new-save-path
                     (file-name-nondirectory buffer-file-name)))))))
      ;; Update session-name in the struct.
      (setf (mevedel-session-name session) sanitized)
      ;; Rewrite sidecar (if materialized).
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path
          (mevedel-session-save-path session))
         (mevedel-session-persistence--build-sidecar session data-buf)))
      ;; Rename the chat buffer per the convention, and also rename
      ;; the companion view buffer (derived from the chat buffer name).
      (let* ((workspace (mevedel-session-workspace session))
             (new-data-name (mevedel-session-buffer-name sanitized workspace))
             (view-buf (buffer-local-value 'mevedel--view-buffer data-buf)))
        (with-current-buffer data-buf
          (rename-buffer new-data-name t))
        (when (buffer-live-p view-buf)
          (let ((new-view-name
                 (if (string-match "\\*$" new-data-name)
                     (replace-match ":view*" t t new-data-name)
                   (concat new-data-name ":view"))))
            (with-current-buffer view-buf
              (rename-buffer new-view-name t)))))
      (message "Session renamed to %s" sanitized))))


;;
;;; Session listing & resume command

(defun mevedel-session-persistence--read-summary (sidecar-path)
  "Read picker-relevant fields from SIDECAR-PATH; nil on failure.

Cheap by design: only fields displayed in the picker (annotations,
sort key) are extracted.  The full sidecar plist is left on disk
until restore actually reads it."
  (condition-case _
      (let* ((plist (mevedel-session-persistence-read sidecar-path))
             (prompt-index (plist-get plist :prompt-index))
             (latest-user-message
              (or (plist-get plist :latest-user-message)
                  (mevedel-session-persistence--latest-user-message-from-index
                   prompt-index))))
        (list :session-id         (plist-get plist :session-id)
              :session-name       (plist-get plist :session-name)
              :workspace          (plist-get plist :workspace)
              :created-at         (plist-get plist :created-at)
              :updated-at         (plist-get plist :updated-at)
              :current-segment    (plist-get plist :current-segment)
              :total-turn-count   (plist-get plist :total-turn-count)
              :first-user-message (plist-get plist :first-user-message)
              :latest-user-message latest-user-message
              :forked-from-session-id
              (plist-get plist :forked-from-session-id)))
    (error nil)))

(defun mevedel-session-persistence-list-sessions (workspace)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Sessions whose sidecar can't be
parsed are silently dropped."
  (let* ((sessions-dir (mevedel-session-persistence--sessions-dir workspace))
         (results nil))
    (when (file-directory-p sessions-dir)
      (dolist (entry (directory-files sessions-dir t "\\`[^.]"))
        (when (file-directory-p entry)
          (let* ((sidecar (file-name-concat entry "session.meta.el"))
                 (summary (mevedel-session-persistence--read-summary sidecar)))
            (when summary
              (push (list :save-path (file-name-as-directory entry)
                          :summary   summary)
                    results))))))
    (sort results
          (lambda (a b)
            (string-greaterp
             (or (plist-get (plist-get a :summary) :updated-at) "")
             (or (plist-get (plist-get b :summary) :updated-at) ""))))))

(defun mevedel-session-persistence--format-session-candidate (entry)
  "Return a `completing-read' display string for session ENTRY.
Shows a relative-time annotation first so the newest session is
easiest to recognise at a glance."
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (mevedel-session-persistence--format-relative-time updated))
         (name     (or (plist-get s :session-name) "?"))
         (preview  (or (plist-get s :latest-user-message)
                       (plist-get s :first-user-message)
                       ""))
         (segments (or (plist-get s :current-segment) 1))
         (turns    (or (plist-get s :total-turn-count) 0)))
    (format "%-12s  %s  [%d seg, %d turns]  %s"
            relative name segments turns preview)))

(defun mevedel-session-persistence--ordered-display-collection
    (displays category)
  "Return a completion table over DISPLAYS that preserves candidate order.
CATEGORY is exposed as completion metadata for completion UI integrations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action displays string pred))))

;;;###autoload
(defun mevedel-resume ()
  "Resume a saved mevedel session in the current workspace.

Pick a session via `completing-read'. If the picked session's chat
buffer is already alive in Emacs, switch to it instead of re-loading
from disk."
  (interactive)
  ;; Entry point: pull in the rest of mevedel so calling this as the first
  ;; command in a fresh Emacs does not hit void-function errors on
  ;; `mevedel-workspace' and friends.
  (require 'mevedel)
  (let* ((workspace (mevedel-workspace))
         ;; Silently drop `.lock' files left behind by previous Emacs
         ;; invocations on this host before listing or cleaning up.
         (_         (mevedel-session-persistence--sweep-stale-locks workspace))
         ;; Run opportunistic cleanup once per workspace per Emacs invocation.
         (_         (mevedel-session-persistence-cleanup-expired workspace))
         (sessions  (mevedel-session-persistence-list-sessions workspace)))
    (unless sessions
      (user-error "No saved sessions in this workspace"))
    (let* ((target
            (let* ((candidates
                    (mapcar
                     (lambda (e)
                       (cons (mevedel-session-persistence--format-session-candidate e)
                             e))
                     sessions))
                   (displays (mapcar #'car candidates))
                   (collection
                    (mevedel-session-persistence--ordered-display-collection
                     displays 'mevedel-session))
                   (chosen (completing-read
                            "Resume session: "
                            collection nil t
                            nil nil
                            (car displays))))
              (cdr (assoc chosen candidates))))
           (save-path (plist-get target :save-path))
           (buf       (mevedel-session-persistence-restore save-path)))
      (display-buffer (or (buffer-local-value 'mevedel--view-buffer buf)
                          buf)
                      gptel-display-buffer-action)
      buf)))

;;;###autoload
(defun mevedel-save-session (&optional arg)
  "Save the current mevedel session to disk explicitly.

Forces a save even when nothing has changed since the last
auto-save (useful after manual edits to the chat buffer). Triggers lazy
materialization if the session has not yet hit disk.

With a prefix ARG, prompts for a new session name and clones the entire
session directory under a fresh id. The current buffer is repointed at
the clone; the original on-disk session is preserved untouched and can
be reopened with `mevedel-resume'.

To rename the current session in place, use `mevedel-rename-session'."
  (interactive "P")
  (let* ((data-buf
          (cond
           ((and (boundp 'mevedel--session) mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (when (and arg
               (with-current-buffer data-buf
                 (and (boundp 'mevedel-tools--agents-fsm)
                      mevedel-tools--agents-fsm))
               (fboundp 'mevedel-abort))
      ;; Save-as copies the session directory; a live sub-agent
      ;; would still be writing into the source dir mid-copy.
      ;; Auto-abort and wait briefly for finalization.
      (with-current-buffer data-buf
        (let ((deadline (+ (float-time) 5.0)))
          (mevedel-abort data-buf)
          (while (and mevedel-tools--agents-fsm
                      (< (float-time) deadline))
            (sit-for 0.05))
          (when mevedel-tools--agents-fsm
            (user-error "Sub-agents did not finalize within 5s; \
retry save-as")))))
    (cond
     (arg
      (mevedel-session-persistence--save-as session data-buf))
     (t
      (with-current-buffer data-buf
        (set-buffer-modified-p t))
      (mevedel-session-persistence-save session data-buf)
      (message "Session saved.")))))

(defun mevedel-session-persistence--save-as (session data-buf)
  "Clone SESSION's on-disk directory under a fresh id.
Called from `mevedel-save-session' with a prefix arg.  Prompts for a
new session name, copies the existing directory to the new id, and
repoints the DATA-BUF at the clone."
  ;; Force materialization before cloning so there's something to copy.
  (unless (mevedel-session-save-path session)
    (with-current-buffer data-buf (set-buffer-modified-p t))
    (mevedel-session-persistence-save session data-buf))
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (new-name (read-string
                    "Save session as (new name): "
                    (mevedel-session-name session)))
         (sanitized (mevedel-session-persistence--sanitize new-name))
         (_ (when (string-empty-p sanitized)
              (user-error "Empty session name")))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (let ((attempts 0) candidate)
                   (while (progn
                            (setq candidate
                                  (mevedel-session-persistence--compute-id
                                   sanitized))
                            (file-directory-p
                             (file-name-concat parent-dir candidate)))
                     (cl-incf attempts)
                     (when (> attempts 32)
                       (error "Could not allocate a unique session id")))
                   candidate))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Save any pending changes into the parent first.
    (with-current-buffer data-buf
      (when (and buffer-file-name (buffer-modified-p))
        (save-buffer)))
    (mevedel-session-persistence--save-instructions session data-buf)
    ;; Release the parent's lock so the clone can acquire a fresh one.
    (condition-case _
        (mevedel-session-persistence-lock-release old-save-path)
      (error nil))
    ;; Copy directory tree verbatim.  Excludes the .lock file so the
    ;; clone starts unlocked and can be independently acquired.
    (copy-directory old-save-path new-save-path nil t t)
    (let ((lock-in-clone
           (mevedel-session-persistence--lock-path new-save-path)))
      (when (file-exists-p lock-in-clone)
        (delete-file lock-in-clone)))
    ;; Mutate the session struct to point at the clone.
    (setf (mevedel-session-session-id session) new-id)
    (setf (mevedel-session-save-path session) new-save-path)
    (setf (mevedel-session-name session) sanitized)
    (setf (mevedel-session-forked-from-session-id session) old-id)
    (setf (mevedel-session-forked-from-turn session)
          (mevedel-session-turn-count session))
    ;; Repoint buffer-file-name at the cloned segment, acquire new
    ;; lock, rename buffers, rewrite sidecar.
    (with-current-buffer data-buf
      (when buffer-file-name
        (setq buffer-file-name
              (file-name-concat new-save-path
                                (file-name-nondirectory buffer-file-name)))))
    (mevedel-session-persistence-lock-acquire
     new-save-path (buffer-name data-buf))
    (mevedel-session-persistence-write
     (mevedel-session-persistence--sidecar-path new-save-path)
     (mevedel-session-persistence--build-sidecar session data-buf))
    (let* ((workspace (mevedel-session-workspace session))
           (new-data-name (mevedel-session-buffer-name sanitized workspace))
           (view-buf (buffer-local-value 'mevedel--view-buffer data-buf)))
      (with-current-buffer data-buf
        (rename-buffer new-data-name t))
      (when (buffer-live-p view-buf)
        (let ((new-view-name
               (if (string-match "\\*$" new-data-name)
                   (replace-match ":view*" t t new-data-name)
                 (concat new-data-name ":view"))))
          (with-current-buffer view-buf
            (rename-buffer new-view-name t)))))
    (message "Session saved as %s" sanitized)
    new-save-path))


;;
;;; Auto-cleanup

(defvar mevedel-session-persistence--cleanup-throttle
  (make-hash-table :test #'equal)
  "Workspace-key set of cleanup runs already done in this Emacs invocation.

Keyed on `(WORKSPACE-TYPE . WORKSPACE-ID)'.  Reset implicitly on Emacs
restart (defvar starts fresh)."  )

(defun mevedel-session-persistence--parse-iso-time (str)
  "Parse `YYYY-MM-DDTHH-MM-SS' STR to a time value, or nil on failure."
  (when (and str (stringp str)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              str))
    (encode-time
     (string-to-number (match-string 6 str))
     (string-to-number (match-string 5 str))
     (string-to-number (match-string 4 str))
     (string-to-number (match-string 3 str))
     (string-to-number (match-string 2 str))
     (string-to-number (match-string 1 str)))))

(defun mevedel-session-persistence--active-lock-p (save-path)
  "Return non-nil if SAVE-PATH's `.lock' counts as active.
A same-host lock is stale when its PID is dead or when the live process
start time proves PID reuse.  Cross-host locks are always treated as
active because we cannot probe the remote process."
  (when-let* ((lock-info (mevedel-session-persistence--read-lock
                          (mevedel-session-persistence--lock-path save-path))))
    (if (equal (plist-get lock-info :hostname) (system-name))
        (mevedel-session-persistence--same-host-lock-active-p lock-info)
      ;; Cross-host: cannot verify liveness, treat as active.
      t)))

(defun mevedel-session-persistence-cleanup-expired (workspace &optional force)
  "Delete sessions in WORKSPACE older than `mevedel-session-max-age-days'.

Skips sessions with an active lock.  Cross-host locks are active.
Same-host locks are stale when their PID is dead or when the live
process start time proves PID reuse.  Throttled to at most once per
`(workspace-type . workspace-id)' per Emacs invocation; when FORCE is
non-nil the throttle is bypassed.

Returns the number of sessions deleted, or nil when the cap is nil or
the throttle has already fired."
  (when mevedel-session-max-age-days
    (let* ((ws-key (cons (mevedel-workspace-type workspace)
                         (mevedel-workspace-id workspace)))
           (already-ran (gethash ws-key
                                 mevedel-session-persistence--cleanup-throttle)))
      (when (or force (not already-ran))
        (puthash ws-key t mevedel-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* mevedel-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0))
          (dolist (entry (mevedel-session-persistence-list-sessions workspace))
            (let* ((save-path   (plist-get entry :save-path))
                   (summary     (plist-get entry :summary))
                   (updated-str (plist-get summary :updated-at))
                   (parsed-time (mevedel-session-persistence--parse-iso-time
                                 updated-str)))
              (when (and parsed-time
                         (> (- now (float-time parsed-time))
                            threshold-secs)
                         (not
                          (mevedel-session-persistence--active-lock-p
                           save-path)))
                (delete-directory save-path t)
                (cl-incf deleted))))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted)))))


;;
;;; Hook plumbing

;; Per-completed-turn autosave lives as a DONE-state terminal handler
;; installed by `mevedel-preset--build-handlers' (`mevedel-presets.el').
;; That placement is necessary for the completed-turn contract: the
;; handler fires only on success (not on abort/error), runs after the
;; turn-count bump, and runs before `mevedel-request-end' clears the
;; request struct (so `mevedel-request-file-snapshots' is still live).

(defun mevedel-session-persistence--kill-emacs-hook ()
  "Save modified mevedel sessions and release their locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them; session auto-save is gated on
`mevedel-session-persistence'.  Best-effort: individual errors are
swallowed so one bad buffer can't block exit."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (boundp 'mevedel--session)
                   mevedel--session)
          (when (and mevedel-session-persistence
                     (buffer-modified-p))
            (condition-case _
                (mevedel-session-persistence-save mevedel--session buf)
              (error nil)))
          (when-let ((dir (mevedel-session-save-path mevedel--session)))
            (condition-case _
                (mevedel-session-persistence-lock-release dir)
              (error nil))))))))

;; Install at file-load time so locks get released on exit even when
;; the user never called `mevedel-install' this Emacs (e.g. running
;; `mevedel-resume' is the only command invoked).  Duplicate adds are
;; no-ops by `add-hook'.
(add-hook 'kill-emacs-hook #'mevedel-session-persistence--kill-emacs-hook)


(provide 'mevedel-session-persistence)

;;; mevedel-session-persistence.el ends here
