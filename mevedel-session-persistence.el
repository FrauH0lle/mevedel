;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persistence layer for mevedel chat sessions.  See `specs/19-session-persistence.md'
;; for the full design.
;;
;; This file (Phase 1) implements the sidecar serialization layer:
;; converting a `mevedel-session' struct to and from a plist suitable
;; for writing to `session.meta.el' under a session directory.  Later
;; phases add the write path, file-history store, compaction split,
;; read path, locking, rewind, restore, fork, and resume commands.
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
;;    :forked-from-session-id nil :forked-from-turn nil
;;    :permission-mode default
;;    :permission-rules ((TOOL-NAME ...) ...)
;;    :additional-roots (("name" . "/abs/path") ...)
;;    :prompt-index ((SEGMENT-N . ((:turn N :pos POS :preview STR :timestamp STR) ...)) ...)
;;    :file-snapshots ((TURN-N . ((PATH . (:backup-name STR :version INT
;;                                          :backup-time STR :file-mtime STR)) ...)) ...))
;;
;; Hash-table-valued slots on the session struct (`touched-files',
;; `mentions-shown') are NOT persisted.  `touched-files' is rebuilt by
;; replaying tool blocks from the loaded buffer; `mentions-shown'
;; resets on every load.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-task-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-task--create "mevedel-structs" (&rest slots))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-get-or-create "mevedel-structs"
                  (type id root name))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-request-file-snapshots
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-buffer-name
                  "mevedel-structs" (session-name workspace))
(declare-function mevedel--chat-buffer-init-common
                  "mevedel-chat" (buf workspace))
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel--current-request)
(defvar mevedel-workspace-additional-roots)
(defvar gptel-mode)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-org--restore-state "ext:gptel-org" ())

;; `mevedel'
(declare-function mevedel-version "mevedel" (&optional here message))


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
load: type, id, root, and name.  The file cache and hints slots are
process-local and not persisted."
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
;;; Task serialization

(defun mevedel-session-persistence--task-to-plist (task)
  "Serialize TASK struct to a plist."
  (list :id          (mevedel-task-id task)
        :subject     (mevedel-task-subject task)
        :description (mevedel-task-description task)
        :status      (mevedel-task-status task)
        :owner       (mevedel-task-owner task)
        :blocks      (mevedel-task-blocks task)
        :blocked-by  (mevedel-task-blocked-by task)
        :metadata    (mevedel-task-metadata task)))

(defun mevedel-session-persistence--task-from-plist (plist)
  "Reconstruct a `mevedel-task' from PLIST."
  (mevedel-task--create
   :id          (plist-get plist :id)
   :subject     (plist-get plist :subject)
   :description (plist-get plist :description)
   :status      (plist-get plist :status)
   :owner       (plist-get plist :owner)
   :blocks      (plist-get plist :blocks)
   :blocked-by  (plist-get plist :blocked-by)
   :metadata    (plist-get plist :metadata)))


;;
;;; Top-level serialize / deserialize

(cl-defun mevedel-session-persistence-serialize (session
                                                 &key
                                                 first-user-message
                                                 additional-roots)
  "Serialize SESSION to a sidecar plist.

FIRST-USER-MESSAGE is the cached preview string for the picker;
typically captured at first save and unchanged thereafter.
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
   :created-at             (mevedel-session-created-at session)
   :updated-at             (mevedel-session-updated-at session)
   :current-segment        (or (mevedel-session-current-segment session) 1)
   :total-turn-count       (or (mevedel-session-turn-count session) 0)
   :first-user-message     first-user-message
   :forked-from-session-id (mevedel-session-forked-from-session-id session)
   :forked-from-turn       (mevedel-session-forked-from-turn session)
   :permission-mode        (mevedel-session-permission-mode session)
   :permission-rules       (mevedel-session-permission-rules session)
   :additional-roots       additional-roots
   :tasks                  (mapcar #'mevedel-session-persistence--task-to-plist
                                   (mevedel-session-tasks session))
   :prompt-index           (mevedel-session-prompt-index session)
   :file-snapshots         (mevedel-session-file-snapshots session)))

(defun mevedel-session-persistence-deserialize (plist)
  "Reconstruct a session from sidecar PLIST.

Returns a plist:
  (:session SESSION
   :first-user-message STR-OR-NIL
   :additional-roots ALIST)

Where SESSION is a freshly-created `mevedel-session' struct populated
from PLIST.  The auxiliary fields (first-user-message, additional-roots)
are returned alongside because they are not on the session struct.

The PLIST is run through `mevedel-session-persistence--patch-sidecar'
first for version migration.  Permission rules with unknown actions
are dropped via the hygiene filter."
  (let* ((plist    (mevedel-session-persistence--patch-sidecar plist))
         (workspace (mevedel-session-persistence--workspace-from-plist
                     (plist-get plist :workspace)))
         (tasks     (mapcar #'mevedel-session-persistence--task-from-plist
                            (plist-get plist :tasks)))
         (rules     (mevedel-session-persistence--filter-permission-rules
                     (plist-get plist :permission-rules)))
         (session   (mevedel-session--create
                     :name             (plist-get plist :session-name)
                     :workspace        workspace
                     :touched-files    (make-hash-table :test #'equal)
                     :mentions-shown   (make-hash-table :test #'equal)
                     :tasks            tasks
                     :permission-rules rules
                     :permission-mode  (plist-get plist :permission-mode)
                     :turn-count       (or (plist-get plist :total-turn-count) 0)
                     :session-id       (plist-get plist :session-id)
                     :created-at       (plist-get plist :created-at)
                     :updated-at       (plist-get plist :updated-at)
                     :current-segment  (or (plist-get plist :current-segment) 1)
                     :forked-from-session-id
                     (plist-get plist :forked-from-session-id)
                     :forked-from-turn (plist-get plist :forked-from-turn)
                     :prompt-index     (plist-get plist :prompt-index)
                     :file-snapshots   (plist-get plist :file-snapshots))))
    (list :session            session
          :first-user-message (plist-get plist :first-user-message)
          :additional-roots   (plist-get plist :additional-roots))))


;;
;;; Sidecar IO

(defun mevedel-session-persistence-write (path plist)
  "Write sidecar PLIST to PATH atomically.
Uses `write-region' with a temporary file + rename so a partial write
cannot corrupt an existing sidecar."
  (let ((tmp (make-temp-file "mevedel-session-meta-" nil ".el")))
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
      (expand-file-name dir (mevedel-workspace--root workspace)))))

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

(defun mevedel-session-persistence--collect-prompts (buffer)
  "Return a list of `(:turn N :pos POS :preview STR)' plists for BUFFER.

A user prompt is a region whose `gptel' text property is nil and which
contains non-whitespace.  Turns are numbered 1, 2, ... in document
order.  Used at save time to refresh the live segment's entry in
`mevedel-session-prompt-index'.

The recorded position is the start of the prompt region.  The preview
is the first non-blank line truncated to 80 characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let ((pos (point-min))
                (turn 0)
                (results nil))
            (while (< pos (point-max))
              (let* ((next (next-single-property-change
                            pos 'gptel nil (point-max)))
                     (prop (get-text-property pos 'gptel)))
                (when (null prop)
                  (let ((text (buffer-substring-no-properties pos next)))
                    (when (string-match "[^[:space:]].*$" text)
                      (cl-incf turn)
                      (push (list :turn turn
                                  :pos pos
                                  :preview (truncate-string-to-width
                                            (match-string 0 text)
                                            80 nil nil "..."))
                            results))))
                (setq pos next)))
            (nreverse results)))))))

(defun mevedel-session-persistence--update-prompt-index (session buffer)
  "Refresh the live segment's prompt list in SESSION from BUFFER's contents.

Operates only on the current segment; previous (finalized) segments
keep their pre-recorded entries.  Idempotent — safe to call on every
save."
  (let* ((current-seg (or (mevedel-session-current-segment session) 1))
         (prompts     (mevedel-session-persistence--collect-prompts buffer))
         (index       (mevedel-session-prompt-index session))
         (cell        (assoc current-seg index)))
    (if cell
        (setcdr cell prompts)
      (setf (mevedel-session-prompt-index session)
            (cons (cons current-seg prompts) index)))))


;;
;;; First user message extraction

(defun mevedel-session-persistence--first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

A user prompt is a region whose `gptel' text property is nil and which
contains non-whitespace.  The preview is the first non-empty line,
truncated to 120 characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (catch 'found
            (let ((pos (point-min)))
              (while (< pos (point-max))
                (let* ((next (next-single-property-change
                              pos 'gptel nil (point-max)))
                       (prop (get-text-property pos 'gptel)))
                  (when (null prop)
                    (let ((text (buffer-substring-no-properties pos next)))
                      (when (string-match "[^[:space:]].*$" text)
                        (let ((line (match-string 0 text)))
                          (throw 'found
                                 (truncate-string-to-width
                                  line 120 nil nil "..."))))))
                  (setq pos next)))
              nil)))))))


;;
;;; Lazy materialization

(defun mevedel-session-persistence-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id, create
the session directory tree, set BUFFER's `buffer-file-name' to the
first segment file, save the buffer, and write the sidecar.

Idempotent — returns immediately if SESSION already has a `save-path'.
Returns SESSION's `save-path' (allocated or existing) on success, nil
when persistence is disabled."
  (when mevedel-session-persistence
    (or (mevedel-session-save-path session)
      (let* ((session-id   (mevedel-session-persistence--compute-id
                            (mevedel-session-name session)))
             (sessions-dir (mevedel-session-persistence--sessions-dir
                            (mevedel-session-workspace session)))
             (save-path    (file-name-as-directory
                            (file-name-concat sessions-dir session-id)))
             (segment-path (mevedel-session-persistence--segment-path
                            save-path 1))
             (now          (format-time-string "%FT%H-%M-%S")))
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
        (with-current-buffer buffer
          (setq buffer-file-name segment-path)
          (set-buffer-modified-p t)
          (save-buffer))
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path save-path)
         (mevedel-session-persistence--build-sidecar session buffer))
        save-path))))


;;
;;; Sidecar build helper

(defun mevedel-session-persistence--build-sidecar (session buffer)
  "Build the sidecar plist for SESSION using BUFFER for ancillary fields."
  (let ((preview (mevedel-session-persistence--first-user-message buffer))
        (roots   (when (buffer-live-p buffer)
                   (buffer-local-value 'mevedel-workspace-additional-roots
                                       buffer))))
    (mevedel-session-persistence-serialize
     session
     :first-user-message preview
     :additional-roots   roots)))


;;
;;; Per-turn save

(defun mevedel-session-persistence-save (session buffer)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, snapshot any tool-modified
files for this turn, evict old snapshots over the cap, and rewrite the
sidecar.

No-op when `mevedel-session-persistence' is nil."
  (when mevedel-session-persistence
    (when (mevedel-session-persistence-ensure-files session buffer)
      (setf (mevedel-session-updated-at session)
            (format-time-string "%FT%H-%M-%S"))
      (with-current-buffer buffer
        (when (buffer-modified-p)
          (save-buffer)))
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
         (dest (file-name-concat dir backup-name))
         (tmp  (make-temp-file "mevedel-fh-")))
    (unwind-protect
        (progn
          (unless (file-directory-p dir)
            (make-directory dir t))
          (let ((coding-system-for-write 'no-conversion))
            (write-region content nil tmp nil 'silent))
          (rename-file tmp dest t))
      (when (file-exists-p tmp) (delete-file tmp)))))

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
      (when entries
        (let ((cell (assoc turn-n (mevedel-session-file-snapshots session))))
          (if cell
              (setcdr cell entries)
            (setf (mevedel-session-file-snapshots session)
                  (cons (cons turn-n entries)
                        (mevedel-session-file-snapshots session))))))
      written)))

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

(declare-function org-entry-put "ext:org" (epom property value))
(declare-function org-with-wide-buffer "ext:org-macs" (&rest body))
(defvar gptel--markdown-block-map)
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function org-cycle "ext:org" (&optional arg))

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

(defun mevedel-session-persistence--summary-block (summary)
  "Return SUMMARY wrapped in an org `#+begin_summary' block.

The block markers are propertized with `gptel \\='ignore' so the LLM sees
only SUMMARY\\='s text — not the wrapper lines.  The user\\='s view, by
contrast, sees a foldable block."
  (concat (propertize "#+begin_summary\n" 'gptel 'ignore)
          summary
          (propertize "\n#+end_summary\n" 'gptel 'ignore)))

(defun mevedel-session-persistence-rotate-segment (session buffer summary)
  "Finalize SESSION's current segment and start a new one with SUMMARY.

Performs the split-on-compact rotation:
  1. Sets `MEVEDEL_SEGMENT_FINALIZED_AT' on the current segment file
     and saves it.
  2. Advances `mevedel-session-current-segment' on SESSION.
  3. Re-points BUFFER's `buffer-file-name' at the new segment path.
  4. Erases BUFFER and re-builds it: per-segment org property drawer
     followed by SUMMARY wrapped in an `#+begin_summary' block.
  5. Saves the new segment file.
  6. Rewrites the sidecar.

Requires SESSION to have a `save-path' (i.e., to have been lazily
materialized).  Returns the new segment's absolute path on success,
nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (with-current-buffer buffer
      (require 'org)
      ;; 1. Finalize the current segment.
      (when (derived-mode-p 'org-mode)
        (org-with-wide-buffer
         (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                        (format-time-string "%FT%H-%M-%S"))))
      (when (buffer-modified-p) (save-buffer))
      ;; 2. Advance segment counter.
      (cl-incf (mevedel-session-current-segment session))
      ;; 3. Switch buffer-file-name to the new segment.
      (let* ((new-segment (mevedel-session-persistence--segment-path
                           (mevedel-session-save-path session)
                           (mevedel-session-current-segment session))))
        (setq buffer-file-name new-segment)
        ;; 4. Erase and rebuild buffer body.
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mevedel-session-persistence--insert-segment-header session)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (insert (mevedel-session-persistence--summary-block summary))
          (insert "\n"))
        (set-buffer-modified-p t)
        ;; 5. Save the new segment file.
        (save-buffer)
        ;; 6. Rewrite the sidecar with the bumped current-segment.
        (setf (mevedel-session-updated-at session)
              (format-time-string "%FT%H-%M-%S"))
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path
          (mevedel-session-save-path session))
         (mevedel-session-persistence--build-sidecar session buffer))
        new-segment))))


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
  "Write a fresh lock file at LOCK-PATH naming BUFFER-NAME as holder."
  (let ((plist (list :pid (emacs-pid)
                     :hostname (system-name)
                     :emacs-invocation-time
                     mevedel-session-persistence--emacs-invocation-time
                     :buffer buffer-name)))
    (with-temp-file lock-path
      (let ((print-length nil) (print-level nil))
        (prin1 plist (current-buffer))))))

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

(defun mevedel-session-persistence-lock-acquire (session-dir buffer-name)
  "Acquire SESSION-DIR's `.lock' for BUFFER-NAME.

Returns:
  t   - lock acquired (or broken from a stale holder).
  nil - user chose read-only access; caller should set buffer-read-only.

Signals `user-error' when the user declines to break a stale lock or
aborts a cross-host conflict, or when an active live-PID lock on the
same host is held by another buffer.

Behavior table:
- No existing lock: write a new lock, return t.
- Lock from same host, dead PID: prompt to break (`y-or-n-p').
- Lock from same host, live PID: refuse with `user-error'.
- Lock from different host: 3-way prompt — break / read-only / abort."
  (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
    (cond
     ((null existing)
      (mevedel-session-persistence--write-lock lock-path buffer-name)
      t)
     ((equal (plist-get existing :hostname) (system-name))
      (cond
       ((mevedel-session-persistence--pid-alive-p
         (plist-get existing :pid))
        (user-error
         "mevedel: session locked by buffer %s (PID %d) on this host"
         (plist-get existing :buffer) (plist-get existing :pid)))
       (t
        (if (y-or-n-p
             (format "Stale mevedel lock (PID %d, buffer %s). Break and proceed? "
                     (plist-get existing :pid)
                     (plist-get existing :buffer)))
            (progn
              (mevedel-session-persistence--write-lock lock-path buffer-name)
              t)
          (user-error "mevedel: lock not broken")))))
     (t
      (let ((response
             (read-char-choice
              (format
               (concat "mevedel session locked by:\n"
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
          (?a (user-error "mevedel: aborted"))))))))

(defun mevedel-session-persistence-lock-release (session-dir)
  "Delete SESSION-DIR's `.lock' if it was written by this Emacs."
  (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
    (when (and existing
               (eq (plist-get existing :pid) (emacs-pid))
               (equal (plist-get existing :hostname) (system-name)))
      (delete-file lock-path))))

(defun mevedel-session-persistence--release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases this session's lock."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when-let ((dir (mevedel-session-save-path mevedel--session)))
      (condition-case _
          (mevedel-session-persistence-lock-release dir)
        (error nil)))))


;;
;;; Read path (resume)

(defun mevedel-session-persistence-load-sidecar (path)
  "Read sidecar PLIST from PATH, applying version migration.

This is the public read entry point: combines `mevedel-session-persistence-read'
(raw I/O) with `mevedel-session-persistence--patch-sidecar' (version
forward-migration).  Returns the patched plist."
  (mevedel-session-persistence--patch-sidecar
   (mevedel-session-persistence-read path)))

(defun mevedel-session-persistence-restore (session-dir)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the sidecar, opens the live segment file in a buffer named per
the chat-buffer convention (`mevedel-session-buffer-name'), enables
`org-mode' and `gptel-mode' (the latter triggers gptel's restore of
text-property bounds and config), hydrates the session struct on the
buffer, then runs `mevedel--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive, switches to it instead of re-loading.

Tasks and touched-files are deserialized from the sidecar (their
serialized form represents the most recent saved state).  The
mentions-shown dedup hash resets to empty per the locked design."
  (let* ((sidecar-path (mevedel-session-persistence--sidecar-path session-dir))
         (sidecar      (mevedel-session-persistence-load-sidecar sidecar-path))
         (result       (mevedel-session-persistence-deserialize sidecar))
         (session      (plist-get result :session))
         (additional-roots (plist-get result :additional-roots))
         (workspace    (mevedel-session-workspace session))
         (segment-n    (mevedel-session-current-segment session))
         (segment-path (mevedel-session-persistence--segment-path
                        session-dir segment-n))
         (buf-name     (mevedel-session-buffer-name
                        (mevedel-session-name session)
                        workspace))
         ;; Detect "session already alive" first via buffer-name so we
         ;; switch to the existing buffer (Q19/Q20 lock).
         (live         (get-buffer buf-name))
         (buf          (or live
                           (find-file-noselect segment-path))))
    (when (or (not (buffer-live-p buf))
              (not (file-exists-p segment-path)))
      (user-error "Mevedel: session %s segment file %s missing"
                  (mevedel-session-session-id session) segment-path))
    (with-current-buffer buf
      ;; If find-file-noselect reused or created the buffer, ensure the
      ;; canonical name and file backing.
      (unless (equal (buffer-name) buf-name)
        (rename-buffer buf-name t))
      (unless (equal (expand-file-name buffer-file-name)
                     (expand-file-name segment-path))
        (setq buffer-file-name segment-path))
      (unless live
        ;; Mode + gptel restore for freshly opened files only;
        ;; live buffers are already initialized.
        (unless (derived-mode-p 'org-mode) (org-mode))
        (unless (bound-and-true-p gptel-mode) (gptel-mode +1))
        ;; Plant the hydrated session struct so init-common and the
        ;; lock-release kill hook can find it.
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace workspace)
        (when additional-roots
          (setq-local mevedel-workspace-additional-roots additional-roots))
        ;; Acquire the lock before installing init-common so a refused
        ;; lock aborts the restore cleanly.  Read-only acquisition (cross
        ;; host) sets the buffer read-only.
        (let ((acquired
               (mevedel-session-persistence-lock-acquire
                session-dir (buffer-name buf))))
          (unless acquired
            (setq buffer-read-only t)))
        (mevedel--chat-buffer-init-common buf workspace)))
    buf))


;;
;;; Rewind picker

(defvar-local mevedel-session--fork-pending nil
  "Non-nil when the current buffer is a back-in-time view of a session.

Set by `mevedel-rewind' after loading a truncated segment; cleared by
the spec-19 fork code (Phase 9) once the next user message materializes
the fork.  When non-nil, `buffer-file-name' is also nil so saves cannot
overwrite the original segment file.")

(defun mevedel-session-persistence--prompt-candidates (session)
  "Return an alist of `(DISPLAY . PLIST)' for SESSION's prompts.

PLIST has `:segment', `:turn', `:pos', `:preview', and (optionally)
`:timestamp'.  DISPLAY is unique across the whole session — segment
and turn numbers are folded into the display string so duplicate
previews don't collide.

The candidates are presented oldest-first within each segment, with
segments listed in segment-number order."
  (let ((all nil))
    (dolist (segment-entry
             (sort (copy-sequence (mevedel-session-prompt-index session))
                   (lambda (a b) (< (car a) (car b)))))
      (let ((segment-n (car segment-entry)))
        (dolist (prompt (cdr segment-entry))
          (let* ((preview (or (plist-get prompt :preview) "(empty prompt)"))
                 (turn    (plist-get prompt :turn))
                 (display (format "S%d T%d  %s" segment-n turn preview)))
            (push (cons display
                        (list :segment segment-n
                              :turn    turn
                              :pos     (plist-get prompt :pos)
                              :preview preview))
                  all)))))
    (nreverse all)))

(defun mevedel-session-persistence--find-turn-cutoff (turn-n)
  "Return the position right before the (TURN-N + 1)th user prompt.
Returns `point-max' when TURN-N is the final user prompt."
  (save-excursion
    (save-restriction
      (widen)
      (let ((pos (point-min))
            (turn 0)
            (cutoff (point-max)))
        (catch 'done
          (while (< pos (point-max))
            (let* ((next (next-single-property-change
                          pos 'gptel nil (point-max)))
                   (prop (get-text-property pos 'gptel)))
              (when (null prop)
                (let ((text (buffer-substring-no-properties pos next)))
                  (when (string-match-p "[^[:space:]]" text)
                    (cl-incf turn)
                    (when (> turn turn-n)
                      (setq cutoff pos)
                      (throw 'done nil)))))
              (setq pos next))))
        cutoff))))

(defun mevedel-session-persistence--load-truncated
    (session buffer segment-n turn-n)
  "Reload BUFFER from SESSION's SEGMENT-N truncated to TURN-N's response.

Erases BUFFER, re-inserts the segment file's content, restores gptel's
text-property bounds, then truncates everything after the picked
turn's response.  Sets `buffer-file-name' to nil and the buffer-local
`mevedel-session--fork-pending' flag so the next send creates a fork
(Phase 9 wiring) and so save cannot overwrite the original segment
file in the meantime.

Selecting the latest turn is a no-op (we don't disturb the live state)."
  (let ((segment-path (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) segment-n)))
    (unless (file-exists-p segment-path)
      (user-error "mevedel: segment %d file missing: %s"
                  segment-n segment-path))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Read with the segment-path temporarily set as buffer-file-name
        ;; so gptel-mode's restore-state path can find the file-locals.
        (let ((buffer-file-name segment-path))
          (insert-file-contents segment-path)
          (when (derived-mode-p 'org-mode)
            ;; Force re-restoration of GPTEL_BOUNDS from the org property.
            (when (fboundp 'gptel-org--restore-state)
              (gptel-org--restore-state))))
        (let ((cutoff (mevedel-session-persistence--find-turn-cutoff turn-n)))
          (when (and cutoff (< cutoff (point-max)))
            (delete-region cutoff (point-max)))))
      ;; Disconnect from the original file so saves can't corrupt it.
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil)
      (setq-local mevedel-session--fork-pending t))))

;;;###autoload
(defun mevedel-rewind ()
  "Pick a previous user prompt in the current session and rewind to it.

Truncates the live buffer back to the picked prompt's response, sets
`buffer-file-name' to nil so saves cannot corrupt the original segment
file, and flags the buffer for fork-on-next-send.  The original
session's segment files are never modified — every back-in-time pick
is reversible (re-pick the latest prompt to come back).

Refuses with `user-error' when invoked outside a mevedel chat buffer
or while a request is in flight.  Selecting the latest prompt is a
no-op."
  (interactive)
  (unless (and (boundp 'mevedel--session) mevedel--session)
    (user-error "Not in a mevedel chat buffer"))
  (when (and (boundp 'mevedel--current-request)
             mevedel--current-request)
    (user-error "Abort the current request first"))
  (let* ((session   mevedel--session)
         (buffer    (current-buffer))
         (candidates
          (mevedel-session-persistence--prompt-candidates session)))
    (unless candidates
      (user-error "Session has no recorded user prompts"))
    (let* ((default (caar (last candidates)))    ; latest prompt
           (chosen  (completing-read
                     "Rewind to: " (mapcar #'car candidates) nil t
                     nil nil default))
           (entry   (assoc chosen candidates)))
      (when entry
        (let ((picked-segment (plist-get (cdr entry) :segment))
              (picked-turn    (plist-get (cdr entry) :turn))
              (current-segment
               (mevedel-session-current-segment session))
              (latest-turn
               (when-let ((live-prompts
                           (cdr (assoc
                                 (mevedel-session-current-segment session)
                                 (mevedel-session-prompt-index session)))))
                 (plist-get (car (last live-prompts)) :turn))))
          (if (and (eq picked-segment current-segment)
                   (eq picked-turn latest-turn))
              (message "Already at the latest prompt; nothing to rewind.")
            (mevedel-session-persistence--load-truncated
             session buffer picked-segment picked-turn)))))))


;;
;;; Hook plumbing

(defun mevedel-session-persistence--post-response-hook (&rest _ignored)
  "Auto-save the current chat buffer's session.

Hooked buffer-locally to `gptel-post-response-functions' from
`mevedel--chat-buffer-setup'.  The completed-turn contract guarantees
this fires only at terminal FSM states."
  (when (and (boundp 'mevedel--session)
             mevedel--session
             mevedel-session-persistence)
    (condition-case err
        (mevedel-session-persistence-save mevedel--session (current-buffer))
      (error
       (display-warning 'mevedel
                        (format "Session auto-save failed: %s" err)
                        :warning)))))

(defun mevedel-session-persistence--kill-emacs-hook ()
  "Save all live mevedel sessions on Emacs exit. Best-effort."
  (when mevedel-session-persistence
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (boundp 'mevedel--session)
                     mevedel--session
                     (buffer-modified-p))
            (condition-case _
                (mevedel-session-persistence-save mevedel--session buf)
              (error nil))))))))


(provide 'mevedel-session-persistence)

;;; mevedel-session-persistence.el ends here
