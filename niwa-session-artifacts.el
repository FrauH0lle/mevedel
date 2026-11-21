;;; niwa-session-artifacts.el --- Session artifacts and segment persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Owns session paths, immutable artifacts, snapshots, and segment writes.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-bounds)
(require 'niwa-ptc-checkpoint)
(require 'niwa-session-checkpoint)
(require 'niwa-telemetry)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-source)

(eval-when-compile
  (require 'niwa-agents)
  (require 'niwa-session-durability)
  (require 'niwa-structs)
  (require 'niwa-transport)
  (require 'niwa-utilities))

;; `gptel'
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)
(declare-function gptel--save-state "ext:gptel" nil)
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--markdown-block-map)
(defvar gptel-system-prompt)

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-fenced-p
                  "niwa-agent-conversation" (invocation))
(autoload 'niwa-agent-conversation-fenced-p "niwa-agent-conversation")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-transcript-path-p "niwa-agent-persistence" (path save-path))
(autoload 'niwa-agent-persistence-transcript-path-p
  "niwa-agent-persistence")

;; `niwa-agents'
(declare-function niwa-agent-invocation-agent-id "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-buffer "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-data-buffer "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-session "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-sidecar-dirty "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-transcript-relative-path "niwa-agents" (cl-x))

;; `niwa-collaboration'
(declare-function niwa-collaboration-notify-history-changed
                  "niwa-collaboration" (data-buffer))

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-clear-baseline
                  "niwa-compact-estimation" ())
(autoload 'niwa-compact-estimation-clear-baseline
  "niwa-compact-estimation")

;; `niwa-execution-target'
(declare-function niwa-execution-target-acknowledge-incarnation "niwa-execution-target" (target))
(declare-function niwa-execution-target-incarnation-changed-p "niwa-execution-target" (cl-x))
(declare-function niwa-execution-target-native-root "niwa-execution-target" (cl-x))
(declare-function niwa-execution-target-observe-incarnation "niwa-execution-target" (target))
(declare-function niwa-execution-target-prepare-incarnation-acknowledgement "niwa-execution-target" (target))
(declare-function niwa-execution-target-probe "niwa-execution-target" (target &optional refresh sandbox-mode))
(declare-function niwa-execution-target-readiness "niwa-execution-target" (cl-x))
(declare-function niwa-execution-target-refresh-incarnation "niwa-execution-target" (target))
(declare-function niwa-execution-target-remote-p "niwa-execution-target" (target))
(autoload 'niwa-execution-target-acknowledge-incarnation
  "niwa-execution-target")
(autoload 'niwa-execution-target-incarnation-changed-p
  "niwa-execution-target")
(autoload 'niwa-execution-target-native-root "niwa-execution-target")
(autoload 'niwa-execution-target-observe-incarnation
  "niwa-execution-target")
(autoload 'niwa-execution-target-prepare-incarnation-acknowledgement
  "niwa-execution-target")
(autoload 'niwa-execution-target-probe "niwa-execution-target")
(autoload 'niwa-execution-target-readiness "niwa-execution-target")
(autoload 'niwa-execution-target-refresh-incarnation
  "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-file-state'
(declare-function niwa-session-forget-read-ranges "niwa-file-state" (session))
(autoload 'niwa-session-forget-read-ranges "niwa-file-state")

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-checkpoint "niwa-journal-capture" (session buffer))
(declare-function niwa-journal-capture-seal-and-schedule
                  "niwa-journal-capture" (session buffer trigger &optional captures))
(autoload 'niwa-journal-capture-checkpoint "niwa-journal-capture")
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")

;; `niwa-permissions'
(declare-function niwa-permission-invalidate-target-grants "niwa-permissions" (session))
(autoload 'niwa-permission-invalidate-target-grants "niwa-permissions")

;; `niwa-persistence'
(declare-function niwa-instruction-store-applied-backups "niwa-persistence" (session))
(declare-function niwa-instruction-store-restore "niwa-persistence" (session))
(declare-function niwa-instruction-store-sync "niwa-persistence" (workspace))
(declare-function niwa-instruction-store-touch "niwa-persistence" (&optional workspace))
(autoload 'niwa-instruction-store-applied-backups "niwa-persistence")
(autoload 'niwa-instruction-store-restore "niwa-persistence")
(autoload 'niwa-instruction-store-sync "niwa-persistence")
(autoload 'niwa-instruction-store-touch "niwa-persistence")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p "niwa-session-codec" (session))
(declare-function niwa-session-codec-read "niwa-session-codec" (path))
(declare-function niwa-session-codec-serialize
                  "niwa-session-codec"
                  (session &rest keys))
(declare-function niwa-session-codec-write "niwa-session-codec" (path plist))
(autoload 'niwa-session-codec-portable-authority-p
  "niwa-session-codec")
(autoload 'niwa-session-codec-read "niwa-session-codec")
(autoload 'niwa-session-codec-serialize "niwa-session-codec")
(autoload 'niwa-session-codec-write "niwa-session-codec")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-physical-path "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-read-file "niwa-session-control-fs" (path &optional coding-system max-bytes))
(declare-function niwa-session-control-fs-write-file "niwa-session-control-fs" (path content &optional coding-system))
(autoload 'niwa-session-control-fs-physical-path "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-read-file "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-write-file "niwa-session-control-fs")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-observe "niwa-session-control-transfer" (session &optional admission))
(declare-function niwa-session-control-transfer-register-root-buffer "niwa-session-control-transfer" (session buffer))
(autoload 'niwa-session-control-transfer-observe
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-register-root-buffer
  "niwa-session-control-transfer")

;; `niwa-session-durability'
(declare-function niwa-session-durability-disclose "niwa-session-durability" (session))
(declare-function niwa-session-durability-lease-owned-p "niwa-session-durability" (session))
(defvar niwa-session-durability--asserted-directories)
(defvar niwa-session-durability--transaction-clock)
(autoload 'niwa-session-durability-disclose "niwa-session-durability")
(autoload 'niwa-session-durability-lease-owned-p
  "niwa-session-durability")

;; `niwa-session-naming'
(declare-function niwa-session-naming-cancel "niwa-session-naming" ())
(autoload 'niwa-session-naming-cancel "niwa-session-naming")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-authoritative-buffer "niwa-session-persistence" (buffer))
(declare-function niwa-session-persistence-first-user-message "niwa-session-persistence" (buffer))
(declare-function niwa-session-persistence-flush-diagnostic-logs "niwa-session-persistence" (session))
(declare-function niwa-session-persistence-lock-acquire "niwa-session-persistence" (session-dir buffer-name &optional session))
(declare-function niwa-session-persistence-notify-session-event "niwa-session-persistence" (session event &rest args))
(declare-function niwa-session-persistence-root-buffer-for-session "niwa-session-persistence" (session &optional buffer))
(declare-function niwa-session-persistence-root-data-buffer-p "niwa-session-persistence" (buffer))
(declare-function niwa-session-persistence-shallow-ensure-files "niwa-session-persistence" (session buffer))
(declare-function niwa-session-persistence-update-transcript-entry "niwa-session-persistence" (session agent-id updates))
(declare-function niwa-session-persistence-write-current-buffer-atomically "niwa-session-persistence" (path))
(defvar niwa-file-history-max-snapshot-bytes)
(autoload 'niwa-session-persistence-authoritative-buffer
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-first-user-message
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-flush-diagnostic-logs
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-lock-acquire
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-notify-session-event
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-root-buffer-for-session
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-root-data-buffer-p
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-shallow-ensure-files
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-update-transcript-entry
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-current-buffer-atomically
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication--comparable-p "niwa-session-publication" (session))
(declare-function niwa-session-publication--read-staged-sidecar "niwa-session-publication" (source))
(declare-function niwa-session-publication--write-staged-content "niwa-session-publication" (source content coding))
(declare-function niwa-session-publication-barrier "niwa-session-publication" (session &optional token))
(declare-function niwa-session-publication-committed-p "niwa-session-publication" (session artifacts))
(declare-function niwa-session-publication-defer "niwa-session-publication" (session preparation &optional failure token))
(declare-function niwa-session-publication-discard-rolled-back "niwa-session-publication" (session))
(declare-function niwa-session-publication-logical-path-p "niwa-session-publication" (path))
(declare-function niwa-session-publication-prune-committed "niwa-session-publication" (session artifacts))
(declare-function niwa-session-publication-publish "niwa-session-publication" (session artifacts &optional require-commit prepared token))
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(declare-function niwa-session-publication-uncommitted-artifact "niwa-session-publication" (session logical))
(autoload 'niwa-session-publication-committed-p
  "niwa-session-publication")
(autoload 'niwa-session-publication-barrier
  "niwa-session-publication")
(autoload 'niwa-session-publication-defer
  "niwa-session-publication")
(autoload 'niwa-session-publication-discard-rolled-back
  "niwa-session-publication")
(autoload 'niwa-session-publication-logical-path-p
  "niwa-session-publication")
(autoload 'niwa-session-publication-prune-committed
  "niwa-session-publication")
(autoload 'niwa-session-publication-publish "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")
(autoload 'niwa-session-publication-uncommitted-artifact
  "niwa-session-publication")

;; `niwa-session-recovery'
(declare-function niwa-session-recovery-refresh "niwa-session-recovery" (session &optional root-observation))
(defvar niwa-session-recovery--mutation-cache)
(autoload 'niwa-session-recovery-refresh "niwa-session-recovery")

;; `niwa-structs'
(declare-function niwa-request-file-snapshots "niwa-structs" (cl-x))
(declare-function niwa-request-untracked-effects "niwa-structs" (cl-x))
(declare-function niwa-request-turn "niwa-structs" (cl-x))
(declare-function niwa-session-chapters "niwa-structs" (cl-x))
(declare-function niwa-session-control-transfer "niwa-structs" (cl-x))
(declare-function niwa-session-created-at "niwa-structs" (cl-x))
(declare-function niwa-session-current-segment "niwa-structs" (cl-x))
(declare-function niwa-session-execution-target "niwa-structs" (cl-x))
(declare-function niwa-session-file-snapshots "niwa-structs" (cl-x))
(declare-function niwa-session-untracked-effects "niwa-structs" (cl-x))
(declare-function niwa-session-pending-publication "niwa-structs" (cl-x))
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x))
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x))
(declare-function niwa-session-publication "niwa-structs" (cl-x))
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x))
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x))
(declare-function niwa-session-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-turn-count "niwa-structs" (cl-x))
(declare-function niwa-session-updated-at "niwa-structs" (cl-x))
(declare-function niwa-session-working-directory "niwa-structs" (cl-x))
(declare-function niwa-session-workspace "niwa-structs" (cl-x))
(declare-function niwa-workspace-root "niwa-structs" (cl-x))
(defvar niwa--agent-invocation)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-telemetry'
;; `niwa-transcript'
(declare-function niwa-transcript--org-block-depths-before
                  "niwa-transcript" (pos &optional start tool-depth summary-depth))
(declare-function niwa-transcript--render-data-block-at
                  "niwa-transcript" (position &optional limit literal))
(declare-function niwa-transcript--user-prompt-start
                  "niwa-transcript" (pos next prop &optional state))
(declare-function niwa-transcript-cache-create
                  "niwa-transcript" (&rest args))
(declare-function niwa-transcript-cache-restart
                  "niwa-transcript" (cache))
(declare-function niwa-transcript-cached-segments
                  "niwa-transcript" (cache &optional origin))
(declare-function niwa-transcript-leading-property
                  "niwa-transcript" (name))
(declare-function niwa-transcript-normalize-properties
                  "niwa-transcript" ())
(declare-function niwa-transcript-prompt-scan-state
                  "niwa-transcript" ())
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(defvar niwa-transcript--prepared-audit-spans)
(autoload 'niwa-transcript--org-block-depths-before "niwa-transcript")
(autoload 'niwa-transcript--render-data-block-at "niwa-transcript")
(autoload 'niwa-transcript--user-prompt-start "niwa-transcript")
(autoload 'niwa-transcript-cache-create "niwa-transcript")
(autoload 'niwa-transcript-cached-segments "niwa-transcript")
(autoload 'niwa-transcript-leading-property "niwa-transcript")
(autoload 'niwa-transcript-normalize-properties "niwa-transcript")
(autoload 'niwa-transcript-prompt-scan-state "niwa-transcript")
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record "niwa-transcript-audit" (record))
(declare-function niwa-transcript-audit-buffer-spans
                  "niwa-transcript-audit" (&optional type start end))
(defvar niwa--hook-audit-open)
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")
(autoload 'niwa-transcript-audit-buffer-spans "niwa-transcript-audit")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-transport'
(declare-function niwa-transport--call-with-exclusive-connection
                  "niwa-transport" (thunk))
(autoload 'niwa-transport--call-with-exclusive-connection
  "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--forget-place "niwa-utilities" nil)
(declare-function niwa--ordered-completion-table "niwa-utilities" (displays category))
(declare-function niwa--transcript-org-mode "niwa-utilities" nil)
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(declare-function niwa--write-file-atomically
                  "niwa-utilities" (path content &optional coding mode))
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(declare-function niwa-version "niwa-utilities" (&optional here message))
(autoload 'niwa--forget-place "niwa-utilities")
(autoload 'niwa--ordered-completion-table "niwa-utilities")
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa--warn-once "niwa-utilities")
(autoload 'niwa--write-file-atomically "niwa-utilities")
(autoload 'niwa-version "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(declare-function niwa-workspace-ensure-generated-state-ignored "niwa-workspace" (workspace))
(defvar niwa-workspace-additional-roots)
(autoload 'niwa-workspace "niwa-workspace")
(autoload 'niwa-workspace-ensure-generated-state-ignored
  "niwa-workspace")

;; `niwa-workspace-identity'
(declare-function niwa-workspace-identity-ensure "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-ensure "niwa-workspace-identity")

;; `nadvice'
(declare-function advice-add "nadvice" (symbol where function &optional props))
(declare-function advice-member-p "nadvice" (advice symbol))

;; `org'
(declare-function org-cycle "ext:org" (&optional arg))
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (epom property value))
(defvar org-agenda-file-menu-enabled)
(autoload 'org-entry-delete "org")
(autoload 'org-entry-get "org")
(autoload 'org-entry-put "org")
(defvar org-agenda-file-menu-enabled)
(defvar org-element-use-cache)

;; `so-long'
(declare-function so-long-inhibit "so-long" (&optional mode))
(defvar so-long-enabled)
(autoload 'so-long-inhibit "so-long")

(defvar niwa-session-artifacts-require-agent-commit-p nil
  "Non-nil while an acknowledged agent mutation requires immediate commit.")

;;
;;; Buffer persistence

(defun niwa-session-artifacts-inhibit-so-long ()
  "Keep `so-long' from replacing the current session buffer's major mode.

Session transcripts contain expected long GPTEL_BOUNDS property lines.
The permanent local hook reapplies Emacs's native inhibition after every
major-mode change, including inside `set-auto-mode' before `so-long' makes
its post-mode decision."
  (so-long-inhibit)
  (add-hook 'after-change-major-mode-hook
            #'niwa-session-artifacts-inhibit-so-long nil t))

(put 'niwa-session-artifacts-inhibit-so-long 'permanent-local-hook t)

(defun niwa-session-artifacts--run-save-hooks-silently (hook)
  "Run HOOK with save-related echo-area and log messages suppressed.

Publication saves the segment itself, then runs the buffer's save hooks so
third-party integrations see an ordinary save.  Those hooks belong to the
user's configuration and may report to the echo area; a session buffer is
internal state, and a per-turn save report is noise in `*Messages*'."
  (let ((save-silently t)
        (inhibit-message t)
        (message-log-max nil))
    (run-hooks hook)))

(defun niwa-session-artifacts-assert-not-superseded ()
  "Signal unless saving the current buffer would overwrite another writer.
`save-buffer' asks whether to save anyway when its visited file changed
on disk since the visit.  Saves run from timers and turns that nobody in
Emacs may be there to answer, so that is an error here; unchanged disk
text only refreshes the stale modtime.  A file not yet written, as for
a buffer just retargeted, is no conflict."
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (not (verify-visited-file-modtime (current-buffer))))
    (niwa-session-artifacts-refresh-visited-file-modtime-or-error)))

(defun niwa-session-artifacts-save-buffer-silently ()
  "Save the current session buffer without save or hook messages.
A visited file another writer changed is an error, never a question;
see `niwa-session-artifacts-assert-not-superseded'."
  (niwa-session-artifacts-assert-not-superseded)
  (let ((save-silently t)
        (inhibit-message t)
        (message-log-max nil))
    (save-buffer)))

(defun niwa-session-artifacts-replace-transcript-contents (source)
  "Replace the current transcript with SOURCE without an unbounded diff.

The cost bound preserves markers through ordinary edits without spending
minutes comparing unrelated large transcripts.  The generous time bound
is a last-resort plain replacement; callers follow either path with a full
rerender."
  (replace-region-contents (point-min) (point-max) source 5.0 8000))


;;
;;; Session paths

(defcustom niwa-sessions-directory (file-name-concat ".niwa" "sessions")
  "Directory where session persistence files live.

Relative paths resolve against the workspace root at save time;
absolute paths are used as-is."
  :type 'directory
  :group 'niwa)

(defun niwa-session-artifacts-sanitize (name)
  "Return NAME with everything outside `[A-Za-z0-9_-]' replaced with `_'."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (or name "")))

(defun niwa-session-artifacts-sessions-dir (workspace)
  "Return the absolute sessions directory for WORKSPACE.

Resolves `niwa-sessions-directory' against WORKSPACE's root if the
defcustom is relative; otherwise uses it as-is."
  (let ((dir niwa-sessions-directory))
    (if (file-name-absolute-p dir)
        (expand-file-name dir)
      (expand-file-name dir (niwa-workspace-root workspace)))))

(defun niwa-session-artifacts-segment-path (save-path n)
  "Return the absolute path to segment number N under SAVE-PATH.

Segments are zero-padded to four digits (`segment-0001.chat.org')."
  (file-name-concat save-path
                    (format "segment-%04d.chat.org" n)))

(defvar-local niwa-session--inspection-buffer-p nil
  "Non-nil in a read-only archived-segment inspection buffer.")

(defun niwa-session-artifacts--read-published-artifact
    (publication logical)
  "Return verified LOGICAL bytes from validated PUBLICATION."
  (let ((entry (and publication
                    (cdr (assoc logical
                                (plist-get publication :artifacts))))))
    (unless entry
      (error "Session artifact is not published: %s" logical))
    (let* ((content
            (niwa-session-artifacts-read-file-raw
             (plist-get entry :published)))
           (expected (plist-get entry :sha256)))
      (when (and expected
                 (not (equal expected (secure-hash 'sha256 content))))
        (error "Published session artifact failed verification: %s" logical))
      content)))

(defun niwa-session-artifacts-read-artifact
    (session logical &optional committed-only)
  "Return SESSION artifact LOGICAL as literal bytes.

LOGICAL is a normalized session-relative path.  A live owner sees its newest
locally staged write unless COMMITTED-ONLY is non-nil.  Otherwise sessions
resolve only through SESSION's captured immutable publication and verify its
recorded hash; fixed portable caches are never an authority fallback."
  (unless (niwa-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (niwa-session-save-path session)
                       (error "Session has no persistence path"))))
    (if-let* ((source
                 (and (not committed-only)
                      (niwa-session-publication-uncommitted-artifact
                       session logical)))
                ((niwa-session-durability-lease-owned-p session)))
          (niwa-session-artifacts-read-file-raw source)
        (niwa-session-artifacts--read-published-artifact
         (or (niwa-session-publication session)
             (setf (niwa-session-publication session)
                   (niwa-session-publication-read save-path)))
         logical))))

(defun niwa-session-artifacts-segment-number (logical)
  "Return LOGICAL's positive canonical segment number, or nil."
  (when (and (stringp logical)
             (string-match
              "\\`segment-\\([0-9]+\\)\\.chat\\.org\\'" logical))
    (let ((number (string-to-number (match-string 1 logical))))
      (and (> number 0)
           (equal logical (format "segment-%04d.chat.org" number))
           number))))

(defun niwa-session-artifacts--cold-segments (publication)
  "Return authoritative segment logical names in numeric order.

Segments come only from validated PUBLICATION."
  (let (segments)
    (dolist (entry (plist-get publication :artifacts))
      (when-let* ((logical (car-safe entry))
                  (number (niwa-session-artifacts-segment-number logical)))
        (push (cons number logical) segments)))
    (mapcar #'cdr (sort segments :key #'car))))

(defun niwa-session-artifacts--select-cold-segment (segments)
  "Select one logical segment name from ordered SEGMENTS.

Do not prompt for a singleton.  For multiple segments, default to the newest."
  (if (cdr segments)
      (completing-read "Transcript segment: "
                       (niwa--ordered-completion-table
                        segments 'niwa-session-segment)
                       nil t nil nil (car (last segments)))
    (car segments)))

(defun niwa-session-artifacts-inspect-cold-session
    (save-path &optional publication scope)
  "Open an authoritative persisted transcript without hydrating a session.

SAVE-PATH is the persisted session directory.  Inspection uses only the
optional already-validated PUBLICATION capture.  SCOPE is its caller's
explicit diagnostic root.
Return a safe read-only inspection buffer or signal a
`user-error'."
  (let* ((segments
          (or (niwa-session-artifacts--cold-segments publication)
              (niwa-diagnostic-user-error (or scope 'unavailable)
               "No authoritative transcript segment is available: %s"
               save-path)))
         (logical (niwa-session-artifacts--select-cold-segment segments))
         buffer)
    (condition-case err
        (let* ((bytes
                (niwa-session-artifacts--read-published-artifact
                 publication logical))
               (text (decode-coding-string bytes 'utf-8-unix)))
          ;; The decoder turns every malformed sequence into a raw byte or an
          ;; out-of-range character, so one scan rejects them all.
          (when (string-match-p "[^\0-\U0010FFFF]" text)
            (error "Transcript segment is not valid UTF-8: %s" logical))
          (setq buffer
                (generate-new-buffer
                 (format "*niwa inspection %s %s*"
                         (file-name-nondirectory (directory-file-name save-path))
                         logical)))
          (with-current-buffer buffer
            (insert text)
            (niwa--transcript-org-mode)
            (niwa-session-artifacts-inhibit-so-long)
            (setq-local niwa-session--inspection-buffer-p t)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (goto-char (point-min)))
          buffer)
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (niwa-diagnostic-user-error (or scope 'unavailable) "Could not inspect session transcript: %s"
                   (error-message-string err))))))

(defun niwa-session-artifacts-artifact-present-p
    (session logical &optional committed-only)
  "Return non-nil when SESSION has artifact LOGICAL.

Sessions consult a live owner's newest staged write or captured immutable
publication only.
When COMMITTED-ONLY is non-nil, ignore staged portable writes."
  (unless (niwa-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (niwa-session-save-path session)
                       (error "Session has no persistence path"))))
    (or
       (and (not committed-only)
            (niwa-session-publication-uncommitted-artifact session logical)
            (niwa-session-durability-lease-owned-p session))
       (let ((publication
              (or (niwa-session-publication session)
                  (setf
                   (niwa-session-publication session)
                   (niwa-session-publication-read save-path)))))
         (and (assoc logical (plist-get publication :artifacts)) t)))))

(defun niwa-session-artifacts-find-artifact-noselect
    (session logical &optional inspection committed-only)
  "Visit SESSION artifact LOGICAL without exposing immutable storage.

Insert resolver-verified bytes into a buffer visiting the qualified logical
path.  When INSPECTION is non-nil, make the buffer read-only and ineligible
for saving.  COMMITTED-ONLY excludes newer staged writes, as in
`niwa-session-artifacts-read-artifact'."
  (let* ((save-path (or (niwa-session-save-path session)
                        (error "Session has no persistence path")))
         (path (expand-file-name logical save-path))
         ;; Read first so the fixed remote cache can never select the bytes.
         (content (niwa-session-artifacts-read-artifact
                   session logical committed-only))
         (existing
          (and
           (not inspection)
           (cl-find-if
            (lambda (candidate)
              (with-current-buffer candidate
                (and buffer-file-name
                     (equal (expand-file-name buffer-file-name) path)
                     (not (bound-and-true-p
                           niwa-session--inspection-buffer-p)))))
            (buffer-list))))
         (buffer
          (or existing
              (generate-new-buffer
               (format " *niwa artifact %s*"
                       (file-name-nondirectory logical))))))
    (with-current-buffer buffer
      (when (and (not inspection) (buffer-modified-p))
        (error "Session artifact buffer has unsaved changes: %s" path))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (decode-coding-string
          content (or buffer-file-coding-system 'utf-8-unix))))
      (unless existing
        (setq buffer-file-name path
              buffer-file-truename path)
        (niwa-session-artifacts-inhibit-so-long)
        (if (string-suffix-p ".org" logical)
            (let ((org-element-use-cache nil)
                  (org-agenda-file-menu-enabled nil))
              (niwa--transcript-org-mode))
          (let ((enable-local-variables nil)
                (enable-local-eval nil)
                (change-major-mode-hook nil)
                (change-major-mode-after-body-hook nil)
                (so-long-enabled nil))
            (delay-mode-hooks (set-auto-mode))
            (setq delayed-mode-hooks nil))))
      (niwa-session-artifacts-inhibit-so-long)
      (niwa--forget-place)
      (setq-local niwa-session--inspection-buffer-p inspection)
      (setq-local buffer-offer-save (not inspection))
      (setq buffer-read-only inspection)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime 0))
    buffer))

(defun niwa-session-artifacts-segments (session live-buffer)
  "Return ordered segment descriptors for SESSION.

LIVE-BUFFER supplies the current segment even when it has not reached disk.
Each descriptor contains `:number', `:path', `:status', `:current-p',
`:preview', and the `:chat' it belongs to, from
`niwa-session-artifacts-chats'.  Missing and unreadable archived
segments remain in the result."
  (let* ((save-path (niwa-session-save-path session))
         (current (or (niwa-session-current-segment session) 1))
         (index (niwa-session-prompt-index session))
         (chats (niwa-session-artifacts-session-chats session)))
    (cl-loop
     for number from 1 to current
     for path = (and save-path
                     (niwa-session-artifacts-segment-path
                      save-path number))
     for logical = (format "segment-%04d.chat.org" number)
     for current-p = (= number current)
     for prompts = (cdr (assoc number index))
     collect
     (list
      :number number
      :path path
      :status
      (cond
       ((and current-p (buffer-live-p live-buffer)) 'readable)
       ((not path) 'missing)
       ((niwa-session-artifacts-artifact-present-p session logical) 'readable)
       (t 'missing))
      :current-p current-p
      :chat
      (cl-find-if (lambda (chat)
                    (<= (plist-get chat :first-segment) number
                        (plist-get chat :last-segment)))
                  chats)
      :preview
      (cl-loop for prompt in (reverse prompts)
               for preview = (plist-get prompt :preview)
               when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
               return preview)))))

(defun niwa-session-artifacts-chats
    (chapters prompt-index current-segment current-title)
  "Return a session's chats, oldest first.
A chat is the run of segments between two `/clear's; compaction rotates
segments within one chat.  CHAPTERS are the session's closed-chat records,
PROMPT-INDEX its per-segment prompt index, CURRENT-SEGMENT the live
segment, and CURRENT-TITLE the live chat's title.

Each chat is a plist with `:number', `:first-segment', `:last-segment',
`:title', `:closed-at', and `:current-p'.  A closed chat without a recorded
title is labelled by its first prompt.  A record at or past
CURRENT-SEGMENT, which a Fork or Rewind no longer reaches, is ignored.
This reads no files, so session listings may call it freely."
  (let ((first 1) (number 0) chats)
    (dolist (chapter chapters)
      (let ((last (plist-get chapter :segment)))
        (when (and (>= last first) (< last current-segment))
          (setq number (1+ number))
          (push (list :number number
                      :first-segment first
                      :last-segment last
                      :title (or (plist-get chapter :title)
                                 (niwa-session-artifacts--first-preview
                                  prompt-index first last)
                                 "(no prompt)")
                      :closed-at (plist-get chapter :closed-at)
                      :current-p nil)
                chats)
          (setq first (1+ last)))))
    (push (list :number (1+ number)
                :first-segment first
                :last-segment current-segment
                :title current-title
                :closed-at nil
                :current-p t)
          chats)
    (nreverse chats)))

(defun niwa-session-artifacts--first-preview (prompt-index first last)
  "Return the first nonblank prompt preview of segments FIRST to LAST.
PROMPT-INDEX is the session's per-segment prompt index."
  (cl-loop for number from first to last
           thereis (cl-loop for prompt in (cdr (assoc number prompt-index))
                            for preview = (plist-get prompt :preview)
                            when (and (stringp preview)
                                      (string-match-p "\\S-" preview))
                            return (string-trim preview))))

(defun niwa-session-artifacts-session-chats (session)
  "Return SESSION's chats; see `niwa-session-artifacts-chats'."
  (niwa-session-artifacts-chats
   (niwa-session-chapters session)
   (niwa-session-prompt-index session)
   (or (niwa-session-current-segment session) 1)
   (niwa-session-name session)))

(defun niwa-session-artifacts-read-segment (session number)
  "Load SESSION segment NUMBER into a read-only inspection buffer.

The returned buffer restores transcript properties but is never authoritative
session state.  Signal a `user-error' naming the exact path when the segment
cannot be read."
  (let* ((save-path (niwa-session-save-path session))
         (current (or (niwa-session-current-segment session) 1))
         (logical (format "segment-%04d.chat.org" number))
         (path (and save-path
                    (niwa-session-artifacts-segment-path
                     save-path number))))
    (unless (and (integerp number) (<= 1 number) (<= number current))
      (user-error "Unknown session segment: %s" number))
    (unless (and path (niwa-session-artifacts-artifact-present-p session logical))
      (user-error "Segment file is missing: %s" (or path "(unsaved session)")))
    (let ((buffer
           (niwa-session-artifacts-find-artifact-noselect session logical t)))
      (condition-case err
          (with-current-buffer buffer
            (setq-local default-directory
                        (or (niwa-session-working-directory session)
                            save-path))
            (setq-local niwa-session--inspection-buffer-p t)
            (niwa-transcript-restore-properties)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            buffer)
        (error
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (user-error "Could not read segment %s: %s"
                     path (error-message-string err)))))))

(defun niwa-session-artifacts-transcript-segments
    (session live-buffer &optional agent-transcript-p)
  "Return readable history descriptors for LIVE-BUFFER's receiving transcript.
Root transcripts use session segments; agent conversations use their own
numbered compaction archives rather than the parent's session segments.
When AGENT-TRANSCRIPT-P is non-nil, a fileless resident agent has only its
current transcript; never substitute the parent session's root segments."
  (let* ((save-path (niwa-session-save-path session))
         (path (and (buffer-live-p live-buffer)
                    (buffer-local-value 'buffer-file-name live-buffer)))
         (logical (and (stringp save-path) (stringp path)
                       (string-prefix-p
                        (file-name-as-directory (expand-file-name save-path))
                        (expand-file-name path))
                       (file-relative-name path save-path))))
    (if (and (stringp logical)
             (string-prefix-p "agents/" logical)
             (string-suffix-p ".chat.org" logical))
        (let* ((stem (string-remove-suffix ".chat.org" logical))
               (pattern (format "\\`%s\\.compact-\\([0-9]+\\)\\.chat\\.org\\'"
                                (regexp-quote stem)))
               (names
                (append
                     (mapcar #'car
                             (plist-get
                              (or (niwa-session-publication session)
                                  (niwa-session-publication-read save-path))
                              :artifacts))
                     (when (niwa-session-durability-lease-owned-p session)
                       (cl-loop for batch in
                                (append
                                 (niwa-session-publication-uncommitted-batches
                                  session)
                                 (niwa-session-publication-queue session))
                                append (mapcar
                                        (lambda (entry)
                                          (plist-get entry :logical))
                                        (plist-get batch :artifacts))))))
               (numbers
                (sort (delete-dups
                       (delq nil
                             (mapcar (lambda (name)
                                       (when (and (stringp name)
                                                  (string-match pattern name))
                                         (let ((number
                                                (string-to-number
                                                 (match-string 1 name))))
                                           (and (> number 0)
                                                (equal name
                                                       (format "%s.compact-%04d.chat.org"
                                                               stem number))
                                                number))))
                                     names))) #'<))
               archives)
          (dolist (number numbers)
            (let ((name (format "%s.compact-%04d.chat.org" stem number)))
              (when (niwa-session-artifacts-artifact-present-p session name)
                (push (list :number number :logical name
                            :status 'readable :current-p nil)
                      archives))))
          (append (nreverse archives)
                  (list (list :status 'readable :current-p t))))
      (if agent-transcript-p
          (list (list :status 'readable :current-p t))
        (niwa-session-artifacts-segments session live-buffer)))))

(defun niwa-session-artifacts-transcript-segment-identity
    (session descriptor)
  "Return a value that changes whenever DESCRIPTOR's bytes in SESSION do.
It names the same source `niwa-session-artifacts-read-transcript-segment'
reads: a publication's verified hash or a live owner's staged write.
Return nil when the
source cannot be identified without reading it."
  (let* ((save-path (niwa-session-save-path session))
         (number (plist-get descriptor :number))
         (logical (or (plist-get descriptor :logical)
                      (and (integerp number)
                           (format "segment-%04d.chat.org" number))))
         (stat (lambda (path)
                 (when-let* ((attributes (and path (file-attributes path))))
                   (list path
                         (file-attribute-modification-time attributes)
                         (file-attribute-size attributes))))))
    (when (and save-path logical)
      (if-let* ((source (niwa-session-publication-uncommitted-artifact
                           session logical))
                  ((niwa-session-durability-lease-owned-p session)))
            (funcall stat source)
          (when-let* ((publication (niwa-session-publication session))
                      (entry (cdr (assoc logical
                                         (plist-get publication :artifacts))))
                      (hash (plist-get entry :sha256)))
            (list logical hash))))))

(defun niwa-session-artifacts-read-transcript-segment (session descriptor)
  "Read DESCRIPTOR from SESSION into a disposable, read-only transcript buffer."
  (if-let* ((logical (plist-get descriptor :logical)))
      (let ((buffer (niwa-session-artifacts-find-artifact-noselect
                     session logical t)))
        (with-current-buffer buffer
          (niwa-transcript-restore-properties)
          (setq buffer-read-only t))
        buffer)
    (niwa-session-artifacts-read-segment
     session (plist-get descriptor :number))))

(defun niwa-session-artifacts-sidecar-path (save-path)
  "Return the absolute path to the session sidecar under SAVE-PATH."
  (file-name-concat save-path "session.meta.el"))


;;
;;; Prompt index (used by the rewind picker)

(defun niwa-session-artifacts-content-start (buffer)
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

(defun niwa-session-artifacts-collect-prompts (buffer &optional preview-length)
  "Return indexed user prompt plists for BUFFER.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Turns are numbered 1, 2, ... in document order.  Used at
save time to refresh the live segment's entry in
`niwa-session-prompt-index'.

Skips the initial org property drawer (via
`niwa-session-artifacts-content-start') and any content inside
`#+begin_summary' / `#+end_summary' blocks (the compaction summary
has its body stripped of the `gptel' property but is not a user
prompt).  Also skips unpropertized gptel org tool/reasoning block glue and
structural mailbox deliveries using the canonical transcript codec.
PREVIEW-LENGTH defaults to 80 display characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let* ((content-start
                  (niwa-session-artifacts-content-start buffer))
                (turn 0)
                ;; Ditto: one forward pass over the block-depth prefix.
                (scan-state (niwa-transcript-prompt-scan-state))
                (results nil))
            (dolist (seg (niwa-transcript-segments
                          content-start
                          (point-max)))
              (pcase-let ((`(,type ,seg-start ,seg-end) seg))
                (when (eq type 'user)
                  (when-let* ((prompt-start
                               (niwa-transcript--user-prompt-start
                                (max seg-start content-start) seg-end nil
                                scan-state)))
                    ;; Only the first nonblank line is kept, so find it in
                    ;; the buffer: copying each prompt whole cost megabytes
                    ;; per save on a long segment.
                    (let ((preview
                           (save-excursion
                             (goto-char prompt-start)
                             (when (re-search-forward "[^[:space:]]" seg-end t)
                               (buffer-substring-no-properties
                                (match-beginning 0)
                                (min seg-end (line-end-position)))))))
                      (when preview
                        (cl-incf turn)
                        (push (niwa-session-artifacts--prompt-record
                               turn prompt-start
                               (niwa--truncate-display
                                preview (or preview-length 80) "..."))
                              results)))))))
            (nreverse results)))))))

(defun niwa-session-artifacts--prompt-record (turn prompt-start preview)
  "Return the index plist of prompt TURN starting at PROMPT-START.
PREVIEW is its display line."
  (list :turn turn :pos prompt-start :preview preview))

;;;; Incremental prompt index

(cl-defstruct (niwa-session-artifacts--prompt-cache
               (:constructor niwa-session-artifacts--prompt-cache-create)
               (:copier nil))
  "Derived prompt-index state of one transcript buffer, never persisted.
SOURCE proves which suffix changed and SEGMENTS is the parser cache.  ORIGIN
is the body origin of the last derivation, nil before the first.  UNITS holds
one vector per user segment and SPANS every audit span, with positions
relative to ORIGIN.  UNRESOLVED is the earliest relative audit opener that no
span consumed: a later close could still pair it.  FRONTIER is
\(POSITION TOOL SUMMARY), relative to ORIGIN: the org block depths counted up
to a position near the
buffer end, so the next prompt scan resumes there instead of at the last
prompt.  PREFIX is the text before ORIGIN, including its properties."
  source segments origin units spans unresolved frontier prefix)

(defvar-local niwa-session-artifacts--prompt-cache nil
  "Incremental prompt-index state of the current transcript buffer.")

(defun niwa-session-artifacts--prompt-cache-eligible-p ()
  "Return non-nil when the current buffer is accessible whole."
  (and (= (point-min) 1) (= (point-max) (1+ (buffer-size)))))

(defun niwa-session-artifacts--prompt-cache-ensure (origin)
  "Return the current buffer's prompt cache, creating it for ORIGIN."
  (let ((cache niwa-session-artifacts--prompt-cache))
    (unless (and cache
                 (eq (marker-buffer
                      (niwa-transcript-source-origin
                       (niwa-session-artifacts--prompt-cache-source cache)))
                     (current-buffer)))
      (setq cache (niwa-session-artifacts--prompt-cache-create
                   :source (niwa-transcript-source-create origin)
                   :segments (niwa-transcript-cache-create))
            niwa-session-artifacts--prompt-cache cache))
    cache))

(defun niwa-session-artifacts--audit-spans-cached (cache origin d full prefix)
  "Return (SPANS . UNRESOLVED) for the current buffer, updating nothing.
SPANS are every audit span relative to ORIGIN, as one whole-buffer scan finds
them.  D is the first dirty offset relative to ORIGIN, nil when the source is
unchanged; FULL forces a complete scan.  PREFIX is the text before ORIGIN.
A cached span ending at or before the
restart point is reused: its end is where the sequential scan stood, so
scanning on from there is the scan the whole buffer would have run.  The
restart point is the first change or the earliest opener without a span,
whichever comes first, because a later close can pair such an opener."
  (let* ((spans (niwa-session-artifacts--prompt-cache-spans cache))
         (unresolved (niwa-session-artifacts--prompt-cache-unresolved cache))
         ;; Metadata edits before the origin leave the change proof clean, so
         ;; compare the pre-origin text itself: only a change there can add,
         ;; move or remove spans in it.
         (full (or full
                   (not (equal-including-properties
                         prefix
                         (niwa-session-artifacts--prompt-cache-prefix cache)))
                   ;; Offsets of pre-origin records drift with the origin.
                   (and d (or (< (or (caar spans) 0) 0)
                              (< (or unresolved 0) 0)))))
         (old-p (and (not full) (niwa-session-artifacts--prompt-cache-origin cache)))
         (old (and old-p spans)))
    (if (and old-p (null d))
        (cons spans unresolved)
      (let* ((limit (if old-p (min d (or unresolved d)) (- (point-min) origin)))
             (reused (cl-loop for span in old
                              while (<= (nth 1 span) limit)
                              collect span))
             ;; Rescan only from just before the first change, far enough
             ;; back to catch an opener it completes, and no later than
             ;; the first span the change crosses or the last one reused.
             ;; Everything between holds no opener outside a reused span:
             ;; unresolved ones bound LIMIT above.
             (from (max (point-min)
                        (min (max (if reused
                                      (+ origin (nth 1 (car (last reused))))
                                    (point-min))
                                  (- (+ origin limit)
                                     (1- (length niwa--hook-audit-open))))
                             (if-let* ((next (nth (length reused) old)))
                                 (+ origin (nth 0 next))
                               (point-max)))))
             (fresh (niwa-transcript-audit-buffer-spans nil from (point-max)))
             (rest fresh)
             first-open)
        (save-excursion
          (goto-char from)
          (while (and (not first-open)
                      (search-forward niwa--hook-audit-open nil t))
            (let ((open (match-beginning 0)))
              (while (and rest (<= (plist-get (car rest) :end) open))
                (pop rest))
              (unless (and rest (<= (plist-get (car rest) :start) open))
                (setq first-open (- open origin))))))
        (cons (append reused
                      (mapcar (lambda (span)
                                (list (- (plist-get span :start) origin)
                                      (- (plist-get span :end) origin)
                                      (plist-get span :record)))
                              fresh))
              first-open)))))

(defun niwa-session-artifacts--depth-lookahead-closed-p (from to)
  "Return non-nil when no block marker in FROM..TO reads text at or past TO.
The org block depths counted up to TO are then fixed by the text before TO.
Two kinds of marker decide from text after themselves: `#+begin_tool' (the
serialized plist after it, past any whitespace, makes it structural) and a
render-data marker (a complete block is skipped as a whole, an incomplete one
is not).  Both are judged with the depth counter's own grammar: leading
whitespace allowed, any trailing text after the render-data marker."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char from)
      (let ((closed t)
            (regexp (concat "^[ \t]*#\\+begin_tool\\b"
                            "\\|^" (regexp-quote niwa-tool-render-data-open))))
        (while (and closed (< (point) to) (re-search-forward regexp to t))
          (let ((marker (match-beginning 0)))
            (if (eq (char-after marker) ?<)
                (let ((block (niwa-transcript--render-data-block-at
                              marker nil)))
                  (if (and block (<= (cadr block) to))
                      (goto-char (max (point) (cadr block)))
                    (setq closed nil)))
              (when (>= (save-excursion
                          (goto-char marker)
                          (forward-line 1)
                          (skip-chars-forward " \t\n")
                          (forward-char (min 1 (- (point-max) (point))))
                          ;; The discriminator's `\s-*' takes any whitespace
                          ;; syntax, newlines included.
                          (skip-syntax-forward "-")
                          (+ (point) 6))
                        to)
                (setq closed nil)))))
        closed))))

(defun niwa-session-artifacts--prompt-units (cache origin d full)
  "Return user-segment units of the current buffer, deriving only the suffix.
Each unit is [START END STATE-POS TOOL SUMMARY PROMPT PREVIEW] with positions
relative to ORIGIN: the segment, the prompt scan state after it, and the
prompt start and preview, nil when it holds no prompt.  D and FULL are as for
`niwa-session-artifacts--audit-spans-cached'.  A unit is reused when its
segment ends at or before the parser cache's restart point for the change:
segments before it are then as parsed before, and the scan state carried out
of it depends only on earlier text.  Return `mismatch' when a reused unit
does not line up with the fresh segments, so the caller derives everything."
  (let* ((segments (niwa-session-artifacts--prompt-cache-segments cache))
         (old-p (and (not full)
                     (niwa-session-artifacts--prompt-cache-origin cache)))
         (user (cl-loop for seg in (niwa-transcript-cached-segments
                                    segments origin)
                        when (and (eq (car seg) 'user) (> (nth 2 seg) origin))
                        collect seg))
         ;; The parser re-parsed from its restart point in current
         ;; coordinates, after rebasing over any leading metadata change.
         (previous (and old-p (niwa-session-artifacts--prompt-cache-frontier
                               cache)))
         (restart-limit (and d (min d (- (or (niwa-transcript-cache-restart
                                              segments)
                                             (point-min))
                                         origin))))
         ;; Saved scan states count block depth through markers that read
         ;; ahead.  Only text proven closed (the frontier) may anchor them
         ;; after a change; with no proof every unit is derived again.
         (limit (cond ((not old-p) -1)
                      ((null d) most-positive-fixnum)
                      ((null previous) -1)
                      (t (min restart-limit (car previous)))))
         (frontier (and previous restart-limit
                        (<= (car previous) restart-limit)
                        previous))
         (remaining (and old-p
                         (cl-loop for unit in (niwa-session-artifacts--prompt-cache-units cache)
                                  while (<= (aref unit 1) limit)
                                  collect unit)))
         units state result)
    (setq result
     (catch 'mismatch
      (dolist (seg user)
        (let ((unit (car remaining))
              (start (- (nth 1 seg) origin))
              (end (- (nth 2 seg) origin)))
          (cond
           ((and unit (= (aref unit 0) start) (= (aref unit 1) end))
            (setq remaining (cdr remaining)
                  state (list (+ origin (aref unit 2)) (aref unit 3) (aref unit 4)))
            (push unit units))
           (remaining (throw 'mismatch 'mismatch))
           (t
            (let* ((pos (max (nth 1 seg) origin))
                   (prompt-start nil)
                   (preview nil))
              ;; Resume at the depth frontier when it is nearer than the
              ;; last prompt's state: the body between is unchanged.
              (when (and frontier
                         (<= (+ origin (car frontier)) pos)
                         (or (null state)
                             (> (+ origin (car frontier)) (nth 0 state))))
                (setq state (list (+ origin (car frontier))
                                  (nth 1 frontier) (nth 2 frontier))))
              (setq state (or state (niwa-transcript-prompt-scan-state))
                    prompt-start (niwa-transcript--user-prompt-start
                                  pos (nth 2 seg) nil state))
              (when prompt-start
                (setq preview
                      (save-excursion
                        (goto-char prompt-start)
                        (when (re-search-forward "[^[:space:]]" (nth 2 seg) t)
                          (buffer-substring-no-properties
                           (match-beginning 0)
                           (min (nth 2 seg) (line-end-position)))))))
              (push (vector start end
                            (- (nth 0 state) origin) (nth 1 state) (nth 2 state)
                            (and preview (- prompt-start origin))
                            (and preview
                                 (niwa--truncate-display preview 80 "...")))
                    units))))))
      (if remaining 'mismatch (nreverse units))))
    (if (eq result 'mismatch)
        'mismatch
      (cons result
            (let* ((target (save-excursion
                             (goto-char (point-max))
                             (line-beginning-position)))
                   (candidates
                    (delq nil
                          (list state
                                (and frontier
                                     (list (+ origin (car frontier))
                                           (nth 1 frontier)
                                           (nth 2 frontier))))))
                   (base (car (sort (copy-sequence
                                     (or (cl-remove-if
                                          (lambda (candidate)
                                            (> (nth 0 candidate) target))
                                          candidates)
                                         candidates
                                         (list (niwa-transcript-prompt-scan-state))))
                                    (lambda (a b) (> (nth 0 a) (nth 0 b))))))
                   (position (max target (nth 0 base)))
                   (depths (if (> position (nth 0 base))
                               (niwa-transcript--org-block-depths-before
                                position (nth 0 base) (nth 1 base) (nth 2 base))
                             (cons (nth 1 base) (nth 2 base)))))
              ;; Trust the depths only when no marker counted on the way
              ;; reads text that can still change: fail closed otherwise.
              (and (niwa-session-artifacts--depth-lookahead-closed-p
                    (min position
                         (if frontier (+ origin (car frontier)) (point-min)))
                    position)
                   (list (- position origin) (car depths) (cdr depths))))))))

(defun niwa-session-artifacts--prompts-from-units (units origin)
  "Return the prompt index plists for UNITS at ORIGIN, as a whole scan builds."
  (let ((turn 0) results)
    (dolist (unit units)
      (when-let* ((relative (aref unit 5)))
        (cl-incf turn)
        (push (niwa-session-artifacts--prompt-record
               turn (+ origin relative) (aref unit 6))
              results)))
    (nreverse results)))

(defun niwa-session-artifacts--prompt-derive (buffer)
  "Return (PROMPTS . FORK-POINTS) of BUFFER, as the uncached scans find them.
An accessible-whole buffer derives only what changed since the last call, by
the proof its change source holds; any doubt derives everything.  A narrowed
buffer is scanned in full, since its fork points are restriction-relative."
  (with-current-buffer buffer
    (if (not (niwa-session-artifacts--prompt-cache-eligible-p))
        (cons (niwa-session-artifacts-collect-prompts buffer)
              (niwa-session-artifacts-fork-point-spans buffer))
      (let* ((origin (niwa-session-artifacts-content-start buffer))
             (cache (niwa-session-artifacts--prompt-cache-ensure origin))
             (source (niwa-session-artifacts--prompt-cache-source cache))
             (marker (niwa-transcript-source-origin source))
             (derived (niwa-session-artifacts--prompt-cache-origin cache))
             (d (if derived (niwa-transcript-source-change source) 0))
             (full (or (null derived) (eql d 0)
                       (not (eql (marker-position marker) origin)))))
        (when full
          (set-marker marker origin)
          (setq d 0))
        (let* ((prefix (buffer-substring (point-min) origin))
               (audit (niwa-session-artifacts--audit-spans-cached
                       cache origin d full prefix))
               ;; The parser's neighbor repair reads the same fork-point
               ;; spans: hand it the cached ones instead of a whole scan.
               (prepared
                (cons t (cl-loop for span in (car audit)
                                 when (eq (plist-get (nth 2 span) :type)
                                          'fork-point)
                                 collect (list :record (nth 2 span)
                                               :start (+ origin (nth 0 span))
                                               :end (+ origin (nth 1 span))))))
               (derived-units
                (let ((niwa-transcript--prepared-audit-spans prepared))
                  (niwa-session-artifacts--prompt-units
                   cache origin d full)))
               (derived-units
                (if (eq derived-units 'mismatch)
                    (let ((niwa-transcript--prepared-audit-spans prepared))
                      (niwa-session-artifacts--prompt-units
                       cache origin 0 t))
                  derived-units))
               (units (car derived-units)))
          (setf (niwa-session-artifacts--prompt-cache-origin cache) origin
                (niwa-session-artifacts--prompt-cache-units cache) units
                (niwa-session-artifacts--prompt-cache-spans cache) (car audit)
                (niwa-session-artifacts--prompt-cache-prefix cache) prefix
                (niwa-session-artifacts--prompt-cache-unresolved cache) (cdr audit)
                (niwa-session-artifacts--prompt-cache-frontier cache)
                (cdr derived-units))
          (niwa-transcript-source-accept source)
          (cons (niwa-session-artifacts--prompts-from-units
                 units origin)
                (cl-loop for span in (car audit)
                         when (eq (plist-get (nth 2 span) :type) 'fork-point)
                         collect (append
                                  (copy-sequence (nth 2 span))
                                  (list :record-start (+ origin (nth 0 span))
                                        :transcript-cutoff
                                        (+ origin (nth 1 span)))))))))))

(defun niwa-session-artifacts--prompt-count-in-text (text)
  "Return the number of user prompt markers detected in TEXT."
  (if (string-empty-p (or text ""))
    0
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (insert text)
      (length (niwa-session-artifacts-collect-prompts
               (current-buffer))))))

(defun niwa-session-artifacts--segment-tail-prompt-count ()
  "Return the copied-tail prompt count recorded on the current segment."
  (if (derived-mode-p 'org-mode)
      (max 0 (string-to-number
              (or (niwa-transcript-leading-property
                   "NIWA_SEGMENT_TAIL_PROMPTS")
                  "0")))
    0))

(defun niwa-session-artifacts--new-fork-point-id (session)
  "Return a fresh stable fork-point identity for SESSION."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (niwa-session-session-id session)
                  (current-time)
                  (random most-positive-fixnum)
                  (emacs-pid))))
   0 32))

(defun niwa-session-artifacts-fork-point-spans (buffer)
  "Return durable fork-point records and source spans from BUFFER.
Respect BUFFER's current restriction and record provenance."
  (with-current-buffer buffer
    (mapcar
     (lambda (span)
       (append (copy-sequence (plist-get span :record))
               (list :record-start (plist-get span :start)
                     :transcript-cutoff (plist-get span :end))))
     (niwa-transcript-audit-buffer-spans
      'fork-point (point-min) (point-max)))))

(defun niwa-session-artifacts-fork-point-at-source
    (buffer source-start source-end &optional fork-points)
  "Return the stable fork point inside BUFFER source bounds.

SOURCE-START and SOURCE-END must bound one rendered assistant turn.
FORK-POINTS, when supplied, is a projection-scoped vector of provenance-checked
records in source order, built under the projection's source restriction."
  (let* ((points (or fork-points
                     (vconcat (niwa-session-artifacts-fork-point-spans buffer))))
         (low 0)
         (high (length points)))
    ;; Find the first record at or after this turn's start.  Audit spans are
    ;; disjoint and ordered, so if it ends after this turn no later one fits.
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (< (plist-get (aref points middle) :record-start) source-start)
            (setq low (1+ middle))
          (setq high middle))))
    (when (< low (length points))
      (let ((record (aref points low)))
        (when (<= (plist-get record :transcript-cutoff) source-end)
          record)))))

(defun niwa-session-artifacts--prompt-fork-point
    (prompt next-position fork-points)
  "Return PROMPT's fork point before NEXT-POSITION from vector FORK-POINTS.
FORK-POINTS ascend by `:record-start', so the first one after the prompt is
the only candidate: later ones start later still."
  (let ((low 0)
        (high (length fork-points))
        (position (plist-get prompt :pos)))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (> (plist-get (aref fork-points middle) :record-start) position)
            (setq high middle)
          (setq low (1+ middle)))))
    (when (< low (length fork-points))
      (let ((fork-point (aref fork-points low)))
        (and (or (null next-position)
                 (< (plist-get fork-point :record-start) next-position))
             fork-point)))))

(defun niwa-session-artifacts--continued-prompt (session)
  "Return SESSION's latest prompt entry when its turn continues in this segment.
Auto-compaction during a turn carries the turn into a new segment, so its
response ends in a segment that has no prompt of its own and its entry in the
earlier segment never receives a fork point there."
  (let* ((segment (or (niwa-session-current-segment session) 1))
         (index (niwa-session-prompt-index session))
         latest)
    (unless (cdr (assoc segment index))
      (dolist (entry index)
        (when (< (car entry) segment)
          (dolist (prompt (cdr entry))
            (when (or (null latest)
                      (> (plist-get prompt :cum-turn)
                         (plist-get latest :cum-turn)))
              (setq latest prompt)))))
      (and latest (not (plist-get latest :fork-point-id)) latest))))

(defun niwa-session-artifacts--continuation-start (buffer)
  "Return where a turn continued into BUFFER's segment resumes.
That is after the inherited compaction summary, or at the last prompt the
compaction copied forward."
  (with-current-buffer buffer
    (let ((tail (niwa-session-artifacts--segment-tail-prompt-count)))
      (or (and (> tail 0)
               (plist-get (nth (1- tail)
                               (niwa-session-artifacts-collect-prompts
                                buffer))
                          :pos))
          (plist-get (niwa-session-artifacts-segment-summary-bounds) :end)
          (point-min)))))

(defun niwa-session-artifacts--record-continuation
    (session buffer fork-points)
  "Record the continued turn's fork point from FORK-POINTS in SESSION's index.
The entry gains `:continuation' naming the current segment, where in BUFFER
the turn resumes, and its fork point, so a completed turn that spans an
auto-compaction is still indexed.  Other consumers keep the entry unchanged."
  (when-let* ((prompt (niwa-session-artifacts--continued-prompt session))
              ((not (plist-get prompt :continuation)))
              (segment (or (niwa-session-current-segment session) 1))
              (fork-point
               (cl-find-if (lambda (candidate)
                             (and (eql (plist-get candidate :cum-turn)
                                       (plist-get prompt :cum-turn))
                                  (eql (plist-get candidate :segment) segment)))
                           fork-points)))
    (plist-put prompt :continuation
               (list :segment segment
                     :start (niwa-session-artifacts--continuation-start
                             buffer)
                     :fork-point-id (plist-get fork-point :fork-point-id)
                     :transcript-cutoff
                     (plist-get fork-point :transcript-cutoff)))))

(defun niwa-session-artifacts-update-prompt-index (session buffer)
  "Refresh the live segment's prompt list in SESSION from BUFFER's contents.

Operates only on the current segment; previous (finalized) segments
keep their pre-recorded entries.  Idempotent -- safe to call on every
save.

Each prompt plist gets a `:cum-turn' field equal to the prompt's
sequence number across the entire session.  Prompts copied forward as
compaction tail are skipped because they are already indexed in the
predecessor segment.  The cumulative
number is what `:file-snapshots' is keyed by, so the restore plan
can map a picker selection to the checkpoint from immediately before
that prompt's model turn."
  (let* ((current-seg (or (niwa-session-current-segment session) 1))
         (index       (niwa-session-prompt-index session))
         (cell        (assoc current-seg index))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (derived     (niwa-session-artifacts--prompt-derive buffer))
         (raw-all     (car derived))
         (tail-count  (with-current-buffer buffer
                        (niwa-session-artifacts--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (fork-points (cdr derived))
         (fork-vector (vconcat fork-points))
         (with-cum    (cl-loop for remaining on raw
                               for p = (car remaining)
                               for next-position =
                               (plist-get (cadr remaining) :pos)
                               for turn from 1
                               collect
                               (let ((copy (copy-sequence p)))
                                 (plist-put copy :turn turn)
                                 (plist-put copy :file-turn
                                            (+ skip-count turn))
                                 (plist-put copy :cum-turn (+ offset turn))
                                 (when-let* ((fork-point
                                              (niwa-session-artifacts--prompt-fork-point
                                               p next-position fork-vector)))
                                   (dolist (key '(:fork-point-id
                                                  :transcript-cutoff))
                                     (plist-put copy key
                                                (plist-get fork-point key))))
                                 copy))))
    (if cell
        (setcdr cell with-cum)
      (setf (niwa-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))
    (unless with-cum
      (niwa-session-artifacts--record-continuation
       session buffer fork-points))))

(defun niwa-session-artifacts--ensure-latest-fork-point
    (session buffer)
  "Attach a durable fork-point marker to SESSION's latest response in BUFFER.
A turn continued into this segment by auto-compaction gets its marker here
too, after the response it finished with."
  (let* ((segment (or (niwa-session-current-segment session) 1))
         (own (car (last (cdr (assoc
                               segment
                               (niwa-session-prompt-index session))))))
         (continued
          (unless own
            (when-let* ((prompt (niwa-session-artifacts--continued-prompt
                                 session))
                        ((not (plist-get prompt :continuation)))
                        ;; Only the turn settling now continues here.
                        (request (with-current-buffer buffer
                                   (bound-and-true-p niwa--current-request)))
                        ((eql (niwa-request-turn request)
                              (plist-get prompt :cum-turn))))
              prompt)))
         (prompt (or own continued)))
    (when (and prompt (not (plist-get prompt :fork-point-id)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (if continued
                         (niwa-session-artifacts--continuation-start buffer)
                       (plist-get prompt :pos)))
          (when (text-property-search-forward 'gptel 'response t)
            (goto-char (point-max))
            (insert
             (niwa--format-hook-audit-record
              (list :type 'fork-point
                    :fork-point-id
                    (niwa-session-artifacts--new-fork-point-id session)
                    :segment segment
                    :turn (plist-get prompt :turn)
                    :file-turn (plist-get prompt :file-turn)
                    :cum-turn (plist-get prompt :cum-turn)
                    :captured-file-turn (plist-get prompt :cum-turn))))
            t))))))

(defun niwa-session-artifacts--newer-prompt-p (candidate incumbent)
  "Return non-nil when CANDIDATE is newer than INCUMBENT."
  (or (null incumbent)
      (> (plist-get candidate :cum-turn)
         (plist-get incumbent :cum-turn))))

(defun niwa-session-artifacts--latest-user-message-from-index (index)
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
                  (when (niwa-session-artifacts--newer-prompt-p
                         candidate best)
                    (setq best candidate)))))))))
    (plist-get best :preview)))


;;
;;; Lazy materialization

(defun niwa-session-artifacts-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id,
create the session directory tree (session dir + `agents/' +
`file-history/'), acquire its portable lease, set
BUFFER's variable `buffer-file-name' to the first segment file, and save the
buffer.

Does NOT write the sidecar -- the caller (always
`niwa-session-artifacts-save') is expected to do that once it
has updated the prompt-index and snapshot maps.  Keeping the write
in one place avoids double-writing the sidecar on first save.

Idempotent -- if SESSION already has a `save-path', repairs BUFFER's
variable `buffer-file-name' so it visits the current segment before any save.
Returns SESSION's `save-path' (allocated or existing)."
  (let* ((existing-save-path (niwa-session-save-path session))
           (save-path
            (or existing-save-path
                (let* ((_disclosure
                        (when (niwa-execution-target-remote-p
                               (niwa-session-execution-target session))
                          (niwa-session-durability-disclose session)))
                       (_workspace-identity
                        (progn
                          (niwa-workspace-identity-ensure
                           (niwa-workspace-root
                            (niwa-session-workspace session)))))
                       (sessions-dir
                        (niwa-session-artifacts-sessions-dir
                         (niwa-session-workspace session)))
                       (session-id
                        (or (niwa-session-session-id session)
                            (error "Session has no identity")))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat sessions-dir session-id)))
                       (now (format-time-string "%FT%H-%M-%S")))
                  (make-directory sessions-dir t)
                  (make-directory new-save-path)
                  (make-directory (file-name-concat new-save-path "agents") t)
                  (make-directory (file-name-concat new-save-path "file-history") t)
                  (niwa-session-persistence-lock-acquire
                   new-save-path (buffer-name buffer) session)
                  (setf (niwa-session-session-id session)      session-id)
                  (setf (niwa-session-save-path session)       new-save-path)
                  (setf (niwa-session-created-at session)      now)
                  (setf (niwa-session-updated-at session)      now)
                  (setf (niwa-session-current-segment session) 1)
                  (niwa-session-persistence-flush-diagnostic-logs session)
                  (niwa-workspace-ensure-generated-state-ignored
                   (niwa-session-workspace session))
                  (setf (niwa-session-durable-tree-ensured session) t)
                  new-save-path)))
           (segment-number (or (niwa-session-current-segment session) 1))
           (segment-path (niwa-session-artifacts-segment-path
                          save-path segment-number)))
      ;; The tree and the workspace ignore entries are established once per
      ;; process.  Re-establishing them on every save is several target round
      ;; trips plus a re-parse of the ignore file for a settled answer.
      (unless (niwa-session-durable-tree-ensured session)
        (make-directory save-path t)
        (make-directory (file-name-concat save-path "agents") t)
        (make-directory (file-name-concat save-path "file-history") t)
        (niwa-workspace-ensure-generated-state-ignored
         (niwa-session-workspace session))
        (setf (niwa-session-durable-tree-ensured session) t))
      (when (and (buffer-live-p buffer)
                 (eq buffer (niwa-session-root-buffer session)))
        (niwa-session-control-transfer-register-root-buffer session buffer))
      (with-current-buffer buffer
        (unless (and buffer-file-name
                     (equal (expand-file-name buffer-file-name)
                            (expand-file-name segment-path)))
          (setq buffer-file-name segment-path))
        (niwa-session-artifacts-disown-save-machinery)
        (unless (niwa-session-artifacts-artifact-present-p
                 session (file-name-nondirectory segment-path))
          (set-buffer-modified-p t)))
      save-path))

(defvar niwa-session-artifacts--checking-incarnation nil
  "Non-nil while publishing a replacement target incarnation.")

(defun niwa-session-artifacts--observe-target-incarnation
    (target sandbox-mode)
  "Refresh TARGET's incarnation observation and return its readiness.

Admission needs the replacement fingerprint, not the whole readiness suite:
environment, capabilities, and sandbox facts are fixed for the life of a
connection, while the fingerprint is one target command.  A first probe or a
reconnect still runs the full probe, because a new connection may be a new
target.  A failed observation falls back to the full probe, which settles a
blocked readiness the caller reports.  Unavailable child confinement does not
prevent observing target identity."
  (let ((readiness (niwa-execution-target-probe target nil sandbox-mode)))
    (if (not (or (eq 'ready (plist-get readiness :status))
                 (eq 'sandbox-unavailable (plist-get readiness :reason))))
        readiness
      (condition-case nil
          (progn
            (niwa-execution-target-observe-incarnation target)
            readiness)
        (error (niwa-execution-target-probe target t sandbox-mode))))))

(defun niwa-session-artifacts-check-target-incarnation
    (session buffer)
  "Fence and publish a replacement execution target for SESSION.

BUFFER identifies SESSION's live root data buffer.  The fresh probe runs at
every durable mutation boundary; unchanged targets take no durability I/O."
  (unless niwa-session-artifacts--checking-incarnation
    (let ((buffer
           (or buffer
               (and (boundp 'niwa--session)
                    (eq niwa--session session)
                    (current-buffer)))))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (boundp 'niwa--session)
                        (eq niwa--session session)
                        (not (bound-and-true-p niwa--agent-invocation)))))
        (when (or (null (niwa-session-root-buffer session))
                  (eq buffer (niwa-session-root-buffer session)))
          (niwa-session-control-transfer-register-root-buffer
           session buffer)))
      (when-let* ((target (niwa-session-execution-target session)))
        (unless (niwa-execution-target-incarnation-changed-p target)
          (if (niwa-execution-target-remote-p target)
              (let ((readiness
                     (or (niwa-session-artifacts--observe-target-incarnation
                          ;; Reuse connection readiness without changing its
                          ;; confinement requirement at every identity fence.
                          target (or (plist-get (niwa-execution-target-readiness target)
                                                :sandbox-mode)
                                     'off))
                         (niwa-execution-target-readiness target))))
                (unless (or (eq 'ready (plist-get readiness :status))
                            (eq 'sandbox-unavailable (plist-get readiness :reason)))
                  (user-error "Execution target is not ready: %s"
                              (or (plist-get readiness :error)
                                  (plist-get readiness :reason)
                                  "probe failed"))))
            (niwa-execution-target-refresh-incarnation target)))
        (when (niwa-execution-target-incarnation-changed-p target)
          (let ((root-buffer
                 (niwa-session-persistence-root-buffer-for-session
                  session buffer)))
            (unless (buffer-live-p root-buffer)
              (error "Target replacement requires the live root session buffer"))
            (let ((niwa-session-artifacts--checking-incarnation t))
              (niwa-permission-invalidate-target-grants session)
              (niwa-execution-target-prepare-incarnation-acknowledgement
               target)
              (when (niwa-session-save-path session)
                (when (niwa-session-artifacts-artifact-present-p
                       session "session.meta.el" t)
                  (niwa-session-artifacts-publish-sidecar-state
                   session root-buffer)))
              (niwa-execution-target-acknowledge-incarnation target))))))))

(defun niwa-session-artifacts-recover-working-directory (session)
  "Move SESSION to its workspace root when its working directory vanished.

A worktree removed outside niwa leaves the session, its buffers, and any
buffer created from them with a `default-directory' that no longer exists.
Every process started from such a buffer then fails, including publication,
so the session would be stuck behind a pending publication it cannot retry.
Retarget the session and every live buffer still naming the missing
directory, then report the move once.  Remote directories are not probed:
this runs on every mutation and must not add a target round trip.  Return
the new directory when SESSION was moved, nil otherwise."
  (let ((old (niwa-session-working-directory session))
        (workspace (niwa-session-workspace session)))
    (when (and (stringp old)
               (not (file-remote-p old))
               (not (file-directory-p old))
               workspace)
      (let ((root (file-name-as-directory
                   (expand-file-name (niwa-workspace-root workspace))))
            (old (file-name-as-directory (expand-file-name old))))
        (when (and (not (equal old root)) (file-directory-p root))
          (setf (niwa-session-working-directory session) root)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (stringp default-directory)
                         (equal (file-name-as-directory
                                 (expand-file-name default-directory))
                                old))
                (setq default-directory root))))
          (niwa-diagnostic-warning (niwa-diagnostic-scope session)
           'niwa
           (format "Working directory %s no longer exists; session %s moved to %s"
                   old (niwa-session-name session) root)
           :warning)
          root)))))

(defun niwa-session-artifacts-assert-mutation-authority
    (session &optional buffer)
  "Return non-nil when SESSION may start durable mutation from BUFFER.

A session whose working directory vanished first moves to its workspace root.
Pending critical publication blocks every session.  A portable session
also materializes its directory when necessary and must own its live lease.
Every execution target is incarnation-fenced before mutation.  Shallow
materialization uses SESSION's root buffer, or its durable audit parent's."
  (setq session (niwa-session-audit-target session))
  (niwa-session-artifacts-recover-working-directory session)
  (let ((buffer
         (or buffer
             (and (boundp 'niwa--session)
                  (eq niwa--session session)
                  (current-buffer)))))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (boundp 'niwa--session)
                      (eq niwa--session session)
                      (not (bound-and-true-p niwa--agent-invocation))
                      (not (buffer-live-p
                            (niwa-session-root-buffer session))))))
      (niwa-session-control-transfer-register-root-buffer
       session buffer))
    (let ((target (niwa-session-execution-target session)))
      (when (and target
                 (niwa-session-codec-portable-authority-p session))
        (niwa-session-recovery-refresh session)))
    (when (niwa-session-pending-publication session)
      (user-error "Session has pending publication; retry or abandon it first"))
    (let* ((target (niwa-session-execution-target session))
           (portable-p
            (and target
                 (niwa-session-codec-portable-authority-p session))))
      (when (and target portable-p)
        (let* ((root-buffer
                (niwa-session-persistence-root-buffer-for-session
                 session buffer)))
          (niwa-session-durability-disclose session)
          ;; Every portable mutation must have a materialized session
          ;; directory before it is admitted.
          (unless (or (niwa-session-save-path session)
                      (and root-buffer
                           (niwa-session-persistence-shallow-ensure-files
                            session root-buffer)))
            (user-error "Could not materialize portable session state"))
          (unless (or (niwa-session-durability-lease-owned-p session)
                      (niwa-session-persistence-lock-acquire
                       (niwa-session-save-path session)
                       (buffer-name (or root-buffer buffer))
                       session))
            (user-error "Portable session is leased by another client"))
          (niwa-session-artifacts-check-target-incarnation
           session buffer)))
      (when (and target (not portable-p))
        (niwa-session-artifacts-check-target-incarnation
         session (or buffer (current-buffer)))))
    t))

(defun niwa-session-artifacts-assert-new-mutation-authority (session)
  "Reject a new mutation while SESSION is quiescing for control transfer.

Existing requests and durability settlement continue through the ordinary
authority gate; callers use this narrower guard at admission boundaries."
  (setq session (niwa-session-audit-target session))
  ;; Hydrate the durable transfer state before admitting a restarted owner.
  ;; Do not run the owner-side drain/release path here: admission must only
  ;; observe state, and settlement remains responsible for releasing it.
  (let ((state (plist-get (niwa-session-control-transfer session) :state)))
    (when (memq state '(quiescing released))
      (user-error "Session is quiescing for cooperative control transfer")))
  ;; One admission is one durable transaction: the transfer observation's
  ;; target clock answers the ownership checks that follow it.
  (niwa-session-durability-with-transaction
    (when (and (niwa-session-codec-portable-authority-p session)
               (niwa-session-save-path session)
               (niwa-session-session-id session))
      (let ((state
             (plist-get
              (niwa-session-control-transfer-observe session t) :state)))
        (when (memq state '(quiescing released))
          (user-error "Session is quiescing for cooperative control transfer"))))
    (niwa-session-artifacts-assert-mutation-authority session)
    ;; Authority checks can yield to an owner grant or release.  The state
    ;; observed before those checks cannot authorize work after that handoff.
    (when (memq (plist-get (niwa-session-control-transfer session) :state)
                '(quiescing released))
      (user-error "Session is quiescing for cooperative control transfer"))
    ;; Manual release and renewal failure need not create transfer state.
    ;; Reuse this transaction's clock, but never its former lease binding.
    (when (and (niwa-session-execution-target session)
               (niwa-session-codec-portable-authority-p session)
               (not (niwa-session-durability-lease-owned-p session)))
      (user-error "Portable session lease changed during admission checks"))
    t))


;;
;;; Sidecar build helper

(defun niwa-session-artifacts--live-sidecar (session buffer)
  "Serialize live SESSION using BUFFER for ancillary fields.
Only transcript checkpoint writers and explicit history replacement use the
raw root fields.  Ordinary writers use
`niwa-session-artifacts-build-sidecar'."
  (let* ((first-preview
          (or (niwa-session-checkpoint-first-user-message session)
              (niwa-session-persistence-first-user-message buffer)))
         (latest-preview
          (or (niwa-session-artifacts--latest-user-message-from-index
               (niwa-session-prompt-index session))
              first-preview))
         (roots (when (buffer-live-p buffer)
                  (buffer-local-value 'niwa-workspace-additional-roots
                                      buffer))))
    (niwa-session-codec-serialize
     session
     :first-user-message  first-preview
     :latest-user-message latest-preview
     :additional-roots   roots)))

(defun niwa-session-artifacts-build-sidecar (session buffer &optional checkpoint)
  "Project SESSION's sidecar for BUFFER through its committed checkpoint.
CHECKPOINT is the opaque candidate captured with a matching frozen transcript."
  (niwa-session-checkpoint-project
   session (niwa-session-artifacts--live-sidecar session buffer) checkpoint))


;;
;;; Fast Org property writes

(defun niwa-session-artifacts--top-level-pom-p (pom)
  "Return non-nil when POM points at the top-level property drawer."
  (cond
   ((integerp pom) (= pom (point-min)))
   ((markerp pom) (= (marker-position pom) (point-min)))
   (t nil)))

(defun niwa-session-artifacts--property-drawer-region ()
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

(defun niwa-session-artifacts--ensure-property-drawer ()
  "Return the initial Org property drawer region, creating it if needed."
  (or (niwa-session-artifacts--property-drawer-region)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert ":PROPERTIES:\n:END:\n"))
          (niwa-session-artifacts--property-drawer-region)))))

(defun niwa-session-artifacts-property-delete-direct (property &optional keep-drawer)
  "Delete PROPERTY from the initial Org property drawer without Org parsing.
Removing the last property also removes the then empty drawer, unless
KEEP-DRAWER is non-nil: a replacement keeps it, so the transcript's body
start, which follows the drawer, is not collapsed and rewritten."
  (when-let* ((region (niwa-session-artifacts--property-drawer-region)))
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
                           (not keep-drawer)
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

(defun niwa-session-artifacts--property-put-direct (property value)
  "Set PROPERTY to VALUE in the initial Org property drawer without Org parsing."
  (niwa-session-artifacts-property-delete-direct property t)
  (when-let* ((region (niwa-session-artifacts--ensure-property-drawer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (goto-char (car region))
          (forward-line 1)
          (insert (format ":%s: %s\n" property value)))))))

(defun niwa-session-artifacts-strip-gptel-config-properties ()
  "Delete gptel's request-config Org properties from the current buffer.

Strips every top-level `GPTEL_*' property except `GPTEL_BOUNDS', which
carries the transcript span classification niwa itself consumes.
Niwa restores model, effort, preset, system prompt, and tools from
the session sidecar and agent registry, never from Org properties.
Leaving gptel's copies in the drawer is worse than redundant: gptel's
send advice (`gptel-org--send-with-props') prefers them over the live
buffer-locals, so a stale drawer silently overrides a model the user
changed mid-session.  Matching by prefix instead of by name keeps this
correct when gptel grows new config properties."
  (when-let* ((region (niwa-session-artifacts--property-drawer-region)))
    (let ((case-fold-search t)
          names)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (car region))
          (while (re-search-forward
                  "^[ \t]*:\\(GPTEL_\\S-+\\):\\(?:[ \t]+.*\\)?$"
                  (cdr region) t)
            (let* ((raw-name (upcase (match-string-no-properties 1)))
                   (name (if (string-suffix-p "+" raw-name)
                             (substring raw-name 0 -1)
                           raw-name)))
              (unless (equal name "GPTEL_BOUNDS")
                (push name names))))))
      (dolist (name (delete-dups names))
        (niwa-session-artifacts-property-delete-direct name)))))

(defun niwa-session-artifacts--with-fast-property-writes (fn)
  "Call FN while routing top-level Org property writes through text helpers."
  (let ((orig-put (symbol-function 'org-entry-put))
        (orig-delete (symbol-function 'org-entry-delete)))
    (cl-letf (((symbol-function 'org-entry-put)
               (lambda (pom property value)
                 (if (niwa-session-artifacts--top-level-pom-p pom)
                     (niwa-session-artifacts--property-put-direct
                      property value)
                   (funcall orig-put pom property value))))
              ((symbol-function 'org-entry-delete)
               (lambda (pom property)
                 (if (niwa-session-artifacts--top-level-pom-p pom)
                     (niwa-session-artifacts-property-delete-direct
                      property)
                   (funcall orig-delete pom property)))))
      (funcall fn))))

(defvar-local niwa-session-artifacts--raw-utf-8-reported nil
  "Non-nil once this buffer's decoded raw UTF-8 bytes were reported.")

(defun niwa-session-artifacts--decode-raw-utf-8 ()
  "Decode raw UTF-8 byte runs in the current buffer, keeping their properties.
A unibyte string inserted into a multibyte buffer leaves each multibyte
character as several raw-byte characters.  Transcripts are written as UTF-8
and read back decoded, so bounds counted over those bytes drift by every
extra byte.  Invalid sequences stay raw; their length survives the round trip.
Return (COUNT . FIRST): runs whose length changed and the first one's start."
  (let ((count 0) (inhibit-read-only t) first)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[\x3fff80-\x3fffff]+" nil t)
        (let* ((beg (match-beginning 0))
               ;; Decoding drops properties; keep each run inside one `gptel' run.
               (end (copy-marker (next-single-property-change
                                  beg 'gptel nil (match-end 0))
                                 t))
               (length (- end beg))
               (props (text-properties-at beg)))
          (decode-coding-region beg end 'utf-8-unix)
          (set-text-properties beg end props)
          (when (< (- end beg) length)
            (cl-incf count)
            (setq first (or first beg)))
          (goto-char end)
          (set-marker end nil))))
    (cons count first)))

(defun niwa-session-artifacts-stabilize-gptel-bounds ()
  "Rewrite `GPTEL_BOUNDS' until Org property drawer offsets settle.

`gptel-org--save-state' persists absolute buffer positions.  Updating
the Org property drawer can itself move every marked transcript region,
so a single write can save positions that were correct for the previous
drawer size.  Recompute after each write and stop once the serialized
bounds no longer change."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'gptel--get-buffer-bounds))
    ;; The bounds come from the retained run list: each round below only moves
    ;; the drawer, which shifts the body's positions without changing a run.
    (niwa-session-bounds-call-with-cache
     (lambda ()
       (save-excursion
         (save-restriction
           (widen)
           (let ((print-length nil)
                 (print-level nil)
                 (last nil)
                 (done nil)
                 (attempts 0))
             ;; Drawer edits shift existing properties along with the text.
             ;; Structural classification is needed once, not once per offset.
             (pcase-let ((`(,count . ,first) (niwa-session-artifacts--decode-raw-utf-8)))
               (when (and (> count 0) (not niwa-session-artifacts--raw-utf-8-reported))
                 (setq niwa-session-artifacts--raw-utf-8-reported t)
                 (niwa-diagnostic-warning
                  (niwa-diagnostic-scope (current-buffer)) 'niwa
                  (format "Decoded %d raw UTF-8 byte run(s) in the transcript, first at %d; a unibyte string was inserted"
                          count first))))
             (niwa-transcript-normalize-properties)
             (while (and (not done) (< attempts 8))
               (setq attempts (1+ attempts))
               (let ((serialized
                      (when-let* ((bounds (gptel--get-buffer-bounds)))
                        (prin1-to-string bounds))))
                 (cond
                  ((null serialized)
                   (niwa-transcript-source-call-with-metadata-change
                    (lambda ()
                      (niwa-session-artifacts-property-delete-direct
                       "GPTEL_BOUNDS")))
                   (setq done t))
                  ((equal serialized last)
                   (setq done t))
                  (t
                   (setq last serialized)
                   (niwa-transcript-source-call-with-metadata-change
                    (lambda ()
                      (niwa-session-artifacts--property-put-direct
                       "GPTEL_BOUNDS" serialized))))))))))))))

(defun niwa-session-artifacts--save-gptel-state-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, persisting bounds but no request config.

This is an around-advice for `gptel--save-state'.  For non-niwa
buffers it delegates unchanged.  For retained agents and niwa chat
buffers, it dynamically binds `gptel-system-prompt' to nil while gptel
writes its Org metadata, then strips gptel's request-config properties.
The sidecar and agent registry are the durable config sources, and stale
drawer copies would override live buffer-locals through gptel's send
advice.  Finally it rewrites `GPTEL_BOUNDS' until the saved absolute
positions match the post-drawer-update buffer.  If the metadata changed
the buffer size, it shifts the bound view's source coordinates by the
same amount so disclosures continue to address the intended transcript
segments.  Cleanup also runs when gptel's save fails, so a partial write
cannot leave stale request configuration behind."
  (let ((size-before (buffer-size))
        (niwa-org-buffer-p
         (and (bound-and-true-p niwa--session)
              (derived-mode-p 'org-mode))))
    (unwind-protect
        (if niwa-org-buffer-p
            (niwa-session-artifacts--with-fast-property-writes
             (lambda ()
               (let ((gptel-system-prompt nil))
                 ;; gptel writes the leading drawer only, which the change
                 ;; proof exempts, and asks for the bounds more than once.
                 (niwa-session-bounds-call-with-cache
                  (lambda ()
                    (niwa-transcript-source-call-with-metadata-change
                     (lambda () (apply orig-fun args))))))))
          (apply orig-fun args))
      (when niwa-org-buffer-p
        (niwa-transcript-source-call-with-metadata-change
         #'niwa-session-artifacts-strip-gptel-config-properties)
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (let ((delta (- (buffer-size) size-before)))
          (when (/= delta 0)
            (niwa-session-persistence-notify-session-event
             niwa--session 'rebase-data-sources delta)))))))

(defun niwa-session-artifacts-install-gptel-save-state-advice ()
  "Install niwa's gptel state persistence advice."
  (unless (advice-member-p
           #'niwa-session-artifacts--save-gptel-state-around
           'gptel--save-state)
    (advice-add 'gptel--save-state :around
                #'niwa-session-artifacts--save-gptel-state-around)))


;;
;;; Workspace instruction store

(defun niwa-session-artifacts-save-instructions (session)
  "Write SESSION's workspace instructions to the workspace store.
A failure is reported; the session save itself already succeeded."
  (when (and (niwa-session-save-path session)
             (not (eq (bound-and-true-p niwa-instruction-store--inhibit) t)))
    (condition-case err
        (niwa-instruction-store-sync (niwa-session-workspace session))
      (niwa-instruction-store-unreadable
       ;; Reported once; re-reading it at every save would not repair it.
       (niwa-diagnostic-message (niwa-diagnostic-scope session) "%s" (error-message-string err)))
      (error
       (niwa-instruction-store-touch (niwa-session-workspace session))
       (niwa-diagnostic-message (niwa-diagnostic-scope session) "Instructions not written to the store: %s"
                (error-message-string err))))))

(defun niwa-session-artifacts-load-instructions (session)
  "Resume SESSION's workspace instructions from the workspace store.
Return the store restore outcome, or nil when the workspace keeps no store
or the restore failed.  Session instruction snapshots of older sessions are
ignored."
  (when (niwa-session-save-path session)
    (condition-case err
        (niwa-instruction-store-restore session)
      (error
       (niwa-diagnostic-warning (niwa-diagnostic-scope session)
        'niwa
        (format "Could not restore workspace instructions: %s"
                (error-message-string err))
        :warning)
       nil))))


;;
;;; Per-turn save

(defvar niwa-session-artifacts--critical-artifacts nil
  "Dynamically collected target artifacts for one portable publication.")

(defvar niwa-session-artifacts--collecting-critical-artifacts-p nil
  "Non-nil while portable save captures durability-critical file writes.")

(defvar niwa-session-artifacts--preparing-sessions nil
  "Sessions whose checkpoint bytes and metadata are being prepared.")

(defvar niwa-session-artifacts--terminal-sessions nil
  "Sessions with tentative terminal registry and RESULT state.")

(defun niwa-session-artifacts-call-with-checkpoint (session function)
  "Call FUNCTION while excluding nested SESSION transcript checkpoint writers.
The owning writer explicitly admits its final prepared publication.  Ordinary
sidecar writers remain safe through committed projection; terminal settlement
separately excludes them while its tentative state must commit atomically."
  (when (memq session niwa-session-artifacts--preparing-sessions)
    (user-error "Session checkpoint is being prepared"))
  (let ((niwa-session-artifacts--preparing-sessions
         (cons session niwa-session-artifacts--preparing-sessions)))
    (funcall function)))

(defun niwa-session-artifacts--gated-p (session)
  "Return non-nil when SESSION can own asynchronous publication work.
Only materialized portable sessions publish through the session gate."
  (and session
       (niwa-session-save-path session)
       (niwa-session-codec-portable-authority-p session)))

(defun niwa-session-artifacts--barrier (session &optional token)
  "Drain SESSION's queued and in-flight publication work.
TOKEN is the gate token of the running preparation or job, which is exempt.
Call before capturing sidecar, transcript or session state, so an explicitly
synchronous operation never observes or reports queued work as committed.  An
unresolved job signals."
  (when (niwa-session-artifacts--gated-p session)
    (niwa-session-publication-barrier session token)))

(defun niwa-session-artifacts--defer (session preparation &optional failure token)
  "Queue PREPARATION behind SESSION's publication gate when it is owned.
Return non-nil when deferred, nil when the caller may proceed now.  PREPARATION
receives a fresh gate token; FAILURE receives the error of a deferred
preparation.  TOKEN is the caller's own gate token, which bypasses deferral."
  (and (niwa-session-artifacts--gated-p session)
       (niwa-session-publication-defer session preparation failure token)))

(defun niwa-session-artifacts-printed-value (value)
  "Return VALUE in the package's durable Lisp representation."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t))
      (prin1 value (current-buffer)))
    (buffer-string)))

(defun niwa-session-artifacts-publish-text
    (session path content &optional coding prepared token)
  "Publish SESSION's durability-critical CONTENT atomically at PATH.
CONTENT is text, or a unibyte string written as its raw bytes.  CODING
encodes text for publication.

PREPARED is reserved for the owning checkpoint's final write.  TOKEN is the
caller's gate token.
Writes enter the session publication queue.  Return the publication outcome."
  ;; A PREPARED batch was frozen by its owner, which drained the gate first.
  (unless prepared
    (niwa-session-artifacts--barrier session token))
  ;; Admission and publication share recovery and clock observations.
  (niwa-session-durability-with-transaction
    (niwa-session-artifacts-assert-mutation-authority session)
    (niwa-session-publication-publish
     session (list (list :path path :content content :coding coding))
     nil prepared token)))

(defun niwa-session-artifacts--sidecar-publication-artifact
    (session root-buffer &optional checkpoint)
  "Return SESSION's freshly built sidecar marker artifact.

SESSION must be materialized. ROOT-BUFFER must be its live root data buffer,
and its current committed state must contain the sidecar. Mutation authority
is checked before the artifact is built."
  ;; An explicit root argument is the registration seam for callers that
  ;; materialize a session without going through view initialization.
  (when (and (buffer-live-p root-buffer)
             (with-current-buffer root-buffer
               (and (boundp 'niwa--session)
                    (eq niwa--session session)
                    (not (bound-and-true-p niwa--agent-invocation)))))
    (niwa-session-control-transfer-register-root-buffer
     session root-buffer))
  (unless (and (buffer-live-p root-buffer)
               (niwa-session-persistence-root-data-buffer-p root-buffer)
               (eq session
                   (buffer-local-value 'niwa--session root-buffer)))
    (error "Session state publication requires the live root session buffer"))
  (unless (and (niwa-session-save-path session)
               (niwa-session-execution-target session))
    (error "Session state publication requires a materialized session"))
  (unless (niwa-session-artifacts-artifact-present-p
           session "session.meta.el" t)
    (error "Session sidecar is not published"))
  (niwa-session-artifacts-assert-mutation-authority
   session root-buffer)
  (niwa-session-artifacts--sidecar-artifact session root-buffer checkpoint))

(defun niwa-session-artifacts-publish-sidecar-state
    (session root-buffer &optional token)
  "Publish SESSION's freshly built sidecar as one strict commit.

ROOT-BUFFER must be SESSION's live root data buffer.  TOKEN is the caller's
gate token.  Reentrant calls are
rejected before staging. Pre-commit failures propagate; post-commit cleanup
failures remain diagnostic under the publisher's strict commit contract."
  (when (memq session niwa-session-artifacts--terminal-sessions)
    (user-error "Session checkpoint is being prepared"))
  (niwa-session-artifacts--barrier session token)
  (niwa-session-durability-with-transaction
    (let ((artifact (niwa-session-artifacts--sidecar-publication-artifact
                     session root-buffer)))
      (niwa-session-publication-publish session (list artifact) t nil token))))

(defun niwa-session-artifacts-publish-transcript-state
    (session root-buffer transcript-path content checkpoint
     &optional coding prepared token)
  "Publish portable SESSION transcript CONTENT and its sidecar atomically.

ROOT-BUFFER must be SESSION's live root data buffer.  TRANSCRIPT-PATH must be
a logical artifact below SESSION's save path.  The current authoritative
sidecar is rebuilt from SESSION and ROOT-BUFFER and commits the two-artifact
batch.  Reentrant publication is rejected before any bytes are queued.
PREPARED admits only the owning checkpoint's final frozen batch.  TOKEN is the
caller's gate token.
CHECKPOINT was captured when CONTENT was frozen; explicit nil means CONTENT
only patches already committed bytes and retires no live delivery state."
  (unless (stringp content)
    (error "Transcript publication requires string content"))
  (niwa-session-artifacts--barrier session token)
  (let* ((save-path (file-name-as-directory
                     (niwa-session-save-path session)))
         (path (expand-file-name transcript-path))
         (logical (file-relative-name path save-path)))
    (unless (and (file-in-directory-p path save-path)
                 (niwa-session-publication-logical-path-p logical))
      (error "Invalid session transcript path: %s" transcript-path))
    ;; Admission observations must precede capture and remain shared through
    ;; publication: a fresh target read after capture could run a nested writer.
    (niwa-session-durability-with-transaction
      (let ((sidecar-artifact
             (niwa-session-artifacts--sidecar-publication-artifact
              session root-buffer checkpoint)))
        (niwa-session-publication-publish
         session
         (list
          (list :path path :content content :coding coding)
          sidecar-artifact)
         t prepared token)))))

(cl-defun niwa-session-artifacts-publish-agent-terminal-state
    (invocation &optional token)
  "Publish INVOCATION's portable transcript and final session sidecar together.

TOKEN is the caller's gate token.  The authoritative sidecar must already
exist.  Shallow first-turn agent state
continues to wait for the root turn's completed-turn publication boundary.
Return nil if INVOCATION's epoch is superseded during the authority check."
  (unless (niwa-agent-invocation-p invocation)
    (error "Invalid agent invocation"))
  (let* ((session (niwa-agent-invocation-parent-session invocation))
         (parent (niwa-agent-invocation-parent-data-buffer invocation))
         (buffer (niwa-agent-invocation-buffer invocation))
         (save-path (and session (niwa-session-save-path session)))
         (relative
          (niwa-agent-invocation-transcript-relative-path invocation))
         (target (and session (niwa-session-execution-target session)))
         (transcript (and save-path relative
                          (expand-file-name relative save-path))))
    (unless (and session target save-path relative transcript
                 (niwa-session-codec-portable-authority-p session)
                 (buffer-live-p parent)
                 (buffer-live-p buffer)
                 (niwa-agent-persistence-transcript-path-p
                  relative save-path)
                 (niwa-session-artifacts-artifact-present-p
                  session "session.meta.el"))
      (error "Portable terminal agent state is not materialized"))
    (with-current-buffer buffer
      (unless (and buffer-file-name
                   (equal (expand-file-name buffer-file-name) transcript))
        (error "Agent transcript path does not match its invocation"))
      ;; Drain before the transcript, registry and checkpoint are frozen.
      (niwa-session-artifacts--barrier session token)
      (niwa-session-artifacts-call-with-checkpoint
       session
       (lambda ()
         (let ((niwa-session-artifacts--terminal-sessions
                (cons session niwa-session-artifacts--terminal-sessions)))
           (niwa-session-artifacts-assert-mutation-authority session)
           (when (niwa-agent-conversation-fenced-p invocation)
             (cl-return-from niwa-session-artifacts-publish-agent-terminal-state nil))
           (let ((modified-p (buffer-modified-p)) frozen-tick)
             (when modified-p
               (when (bound-and-true-p gptel-mode)
                 (gptel--save-state))
               (let ((before-save-hook
                      (remq 'gptel--save-state before-save-hook)))
                 (niwa-session-artifacts--run-save-hooks-silently
                  'before-save-hook)))
             (niwa-session-persistence-update-transcript-entry
              session (niwa-agent-invocation-agent-id invocation)
              (list :updated-at (format-time-string "%FT%H-%M-%S")))
             (niwa-ptc-checkpoint-clear-settled
              session (niwa-agent-invocation-path invocation) buffer)
             (setq frozen-tick (buffer-chars-modified-tick))
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max)))
                   (checkpoint
                    (niwa-session-checkpoint-capture
                     session (niwa-agent-invocation-path invocation))))
               (niwa-session-publication-publish
                session
                (list
                 (list :path transcript :content content
                       :coding (or buffer-file-coding-system 'utf-8-unix))
                 (niwa-session-artifacts--sidecar-artifact
                  session parent checkpoint))
                t t token))
             (when modified-p
               (condition-case err
                   (progn
                     (set-visited-file-modtime 0)
                     (when (= frozen-tick (buffer-chars-modified-tick))
                       (set-buffer-modified-p nil))
                     (niwa-session-artifacts--run-save-hooks-silently
                      'after-save-hook))
                 (error
                  (niwa--warn-once
                   (niwa-diagnostic-scope session)
                   'artifacts-agent-post-save
                   "Agent transcript post-save cleanup failed: %s"
                   (error-message-string err)))))
             (setf (niwa-agent-invocation-sidecar-dirty invocation) nil)
             t)))))))

(defun niwa-session-artifacts--sidecar-artifact (session buffer &optional checkpoint)
  "Return SESSION's sidecar publication artifact derived from BUFFER.

The sidecar is the commit marker, so callers place it last in a batch.
CHECKPOINT already contains the complete projection frozen with its transcript;
never enrich it from live fields after an I/O or hook boundary."
  (let ((raw (niwa-session-artifacts--live-sidecar session buffer)))
    (list :path
        (niwa-session-artifacts-sidecar-path
         (niwa-session-save-path session))
        :content
        (niwa-session-artifacts-printed-value
         (niwa-session-checkpoint-project session raw checkpoint))
        :checkpoint checkpoint
        :commit-marker t)))

(defun niwa-session-artifacts--obsolete-snapshot-artifacts (session)
  "Return tombstones for backups absent from SESSION's checkpoint index.
Older published heads keep their own references to their immutable bytes."
  (let ((retained (make-hash-table :test #'equal))
        (entries (plist-get (niwa-session-publication session) :artifacts))
        artifacts)
    (dolist (turn (niwa-session-file-snapshots session))
      (dolist (entry (cdr turn))
        (dolist (key '(:backup-name :pre-backup-name))
          (when-let* ((name (plist-get (cdr entry) key)))
            (puthash (file-name-concat "file-history" name) t retained)))))
    (let ((applied (niwa-instruction-store-applied-backups session)))
      (unless (eq applied t)
        (dolist (name applied)
          (puthash (file-name-concat "file-history" name) t retained))
        (dolist (entry entries)
          (when (and (string-prefix-p "file-history/" (car entry))
                     (not (gethash (car entry) retained)))
            (push (list :path (file-name-concat (niwa-session-save-path session)
                                              (car entry))
                        :delete t)
                  artifacts)))))
    (nreverse artifacts)))

(defvar-local niwa-session-artifacts--committed-transcript-proof nil
  "Exact character/coding identity last acknowledged in this data buffer.
This small proof owns no transcript text and is never durable authority.")

(defun niwa-session-artifacts--transcript-proof (session)
  "Return the current buffer's exact saved-byte identity for SESSION.
Check only after save hooks have serialized property-derived metadata into
characters.  The buffer, owner, encoding, restriction and manifest digest all
belong to the proof; a cloned buffer or replaced owner cannot inherit it."
  (when (and buffer-file-name (niwa-session-save-path session))
    (list (current-buffer) (niwa-session-checkpoint-state session)
          (expand-file-name buffer-file-name) (buffer-chars-modified-tick)
          (or buffer-file-coding-system 'utf-8-unix) enable-multibyte-characters
          (point-min) (point-max)
          (plist-get
           (cdr (assoc (file-relative-name buffer-file-name
                                          (niwa-session-save-path session))
                       (plist-get (niwa-session-publication session) :artifacts)))
           :sha256))))

(defun niwa-session-artifacts--select-staged-save (session batch frozen)
  "Select SESSION's digested BATCH using only FROZEN save state.
FROZEN contains manifest entries, comparability proof, forced-save flag and
timestamp captured with the transcript.  Return nil for
an exact no-op, or BATCH with only changed payloads and a stamped marker.
Only SESSION's diagnostic updated-at slot is changed; no live root field is
read.  The caller owns every staged source until acknowledgement."
  (let* ((entries (plist-get frozen :entries))
         (comparable (plist-get frozen :comparable))
         (marker (seq-find (lambda (artifact) (plist-get artifact :commit-marker))
                           (plist-get batch :artifacts)))
         (changed
          (seq-filter
           (lambda (artifact)
             (and (not (plist-get artifact :commit-marker))
                  (or (not comparable)
                      (plist-get artifact :replace)
                      (let ((entry (cdr (assoc (plist-get artifact :logical) entries))))
                        (if (plist-get artifact :delete)
                            entry
                          (not (and entry
                                    (equal (plist-get artifact :source-sha256)
                                           (plist-get entry :sha256)))))))))
           (plist-get batch :artifacts))))
    (unless marker (error "Staged save requires a sidecar marker"))
    (unless (and comparable (not (plist-get frozen :force)) (null changed)
                 (equal (plist-get marker :source-sha256)
                        (plist-get (cdr (assoc "session.meta.el" entries)) :sha256)))
      ;; The staged marker already holds the frozen sidecar as the checkpoint
      ;; owner projected it at stage time, merged recovery rows included.  It
      ;; is a detached local file: reading it never touches live fields.
      (let* ((sidecar (copy-sequence
                       (niwa-session-publication--read-staged-sidecar
                        (plist-get marker :source))))
             (stamp (plist-get frozen :stamp))
             (content
              (encode-coding-string
               (niwa-session-artifacts-printed-value
                (plist-put sidecar :updated-at stamp))
               'utf-8-unix t)))
        (setf (niwa-session-updated-at session) stamp)
        (niwa-session-publication--write-staged-content
         (plist-get marker :source) content 'no-conversion)
        (setf (plist-get marker :source-sha256) (secure-hash 'sha256 content)
              (plist-get batch :artifacts) (append changed (list marker)))
        batch))))

(defun niwa-session-artifacts-prepare-save
    (session buffer &optional settled force defer-comparison)
  "Freeze portable SESSION's save from BUFFER while holding its mutation gate.
When SETTLED is non-nil, update the prompt index and latest fork point.
When FORCE is non-nil, publish even when the state is already committed.
The caller owns checkpoint preparation exclusion and mutation admission.
With DEFER-COMPARISON capture native characters and return a :select-staged
continuation, called with a digested local batch before publication reservation.
Its comparison and stamping use only the captured snapshot, never live fields.
Return (:artifacts ARTIFACTS :finish FUNCTION).  Call FUNCTION without arguments
only after authoritative adoption, or immediately when ARTIFACTS is nil.
Edits made after capture remain modified.  Nothing is published here."
  (niwa-session-artifacts-ensure-files session buffer)
  (when settled
    (niwa-session-artifacts-update-prompt-index session buffer)
    (niwa-session-artifacts--ensure-latest-fork-point session buffer))
  (let ((segment-artifact nil)
        saved-transcript-p frozen-proof unchanged
        frozen-tick frozen-modification-tick
        checkpoint
        (niwa-session-artifacts--critical-artifacts nil)
        (niwa-session-artifacts--collecting-critical-artifacts-p t))
    ;; Finish I/O-backed preparation before freezing the transcript and every
    ;; root field.  Target I/O can run sentinels that advance the live turn.
    (when (and (boundp 'niwa--current-request)
               niwa--current-request)
      (niwa-session-artifacts-snapshot-modified
       session
       (or (niwa-session-turn-count session) 0)
       (niwa-request-file-snapshots niwa--current-request)
       (niwa-request-untracked-effects niwa--current-request)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        ;; The writer can run before chat setup; freeze canonical bounds
        ;; before any deferred view render can normalize them.
        (niwa-session-artifacts-install-gptel-save-state-advice)
        (run-hooks 'before-save-hook)
        (niwa-session-artifacts-update-prompt-index session buffer)
        (setq frozen-tick (buffer-chars-modified-tick)
              frozen-modification-tick (buffer-modified-tick)
              frozen-proof (niwa-session-artifacts--transcript-proof session)
              saved-transcript-p t)
        ;; A parser frontier can advance before a failed save.  Only an ACKed
        ;; character/coding identity whose manifest entry still agrees proves
        ;; that no transcript capture, staging or hashing is necessary.
        (unless (and (niwa-session-publication--comparable-p session)
                     (stringp (nth 8 frozen-proof))
                     (eq (car frozen-proof)
                         (car niwa-session-artifacts--committed-transcript-proof))
                     (eq (cadr frozen-proof)
                         (cadr niwa-session-artifacts--committed-transcript-proof))
                     (equal (cddr frozen-proof)
                            (cddr niwa-session-artifacts--committed-transcript-proof)))
          (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                 (coding (or buffer-file-coding-system 'utf-8-unix))
                 ;; Publication strings that are unibyte already mean literal
                 ;; bytes.  Preserve the root writer's explicit encoding even
                 ;; for an unusual unibyte conversation buffer.
                 (native (and defer-comparison (multibyte-string-p content))))
            (setq segment-artifact
                  (list :path buffer-file-name
                        :content (if native content
                                   (encode-coding-string content coding t))
                        :coding (if native coding 'no-conversion)))))))
    (setq checkpoint
          (niwa-session-checkpoint-capture
           session "/root" (niwa-session-artifacts--live-sidecar session buffer)))
    ;; The first capture creates the owner epoch.  No I/O or character write
    ;; separates it from the transcript freeze above.
    (when frozen-proof
      (setcar (cdr frozen-proof) (niwa-session-checkpoint-state session)))
    ;; `updated-at' is stamped only once this save is known to carry a
    ;; change, because stamping it first would make the sidecar differ from
    ;; the committed one on every call and defeat the comparison below.
    (let* ((sidecar
            (niwa-session-checkpoint-project
             session
             (niwa-session-checkpoint--copy
              (niwa-session-artifacts--live-sidecar session buffer))
             checkpoint))
           (marker (list :path (niwa-session-artifacts-sidecar-path
                                (niwa-session-save-path session))
                         :content (niwa-session-artifacts-printed-value sidecar)
                         :checkpoint checkpoint :commit-marker t))
           (stamp (format-time-string "%FT%H-%M-%S"))
           (frozen (and defer-comparison
                        (list :stamp stamp :force force
                              :comparable (niwa-session-publication--comparable-p session)
                              :entries (niwa-session-checkpoint--copy
                                        (plist-get (niwa-session-publication session)
                                                   :artifacts)))))
           (leading
            (append
             (and segment-artifact (list segment-artifact))
             (nreverse niwa-session-artifacts--critical-artifacts)
             (niwa-session-artifacts--obsolete-snapshot-artifacts session)))
           ;; Compare payloads once.  The same selection decides whether a
           ;; save is needed and supplies its publication; hashing a changed
           ;; transcript again here only duplicates encoding and allocation.
           (changed (if defer-comparison leading
                      (niwa-session-publication-prune-committed session leading))))
      (unless defer-comparison
        (setq unchanged
              (and (not force) (null changed)
                   (niwa-session-publication-committed-p session (list marker))))
        (unless unchanged
          (setf (niwa-session-updated-at session) stamp
                (plist-get marker :content)
                (niwa-session-artifacts-printed-value
                 (plist-put sidecar :updated-at stamp)))))
      (list
       :artifacts
       (unless unchanged
         (append changed (list marker)))
       :select-staged
       (and defer-comparison
            (lambda (batch)
              (let ((selected (niwa-session-artifacts--select-staged-save
                               session batch frozen)))
                (setq unchanged (null selected))
                selected)))
       :finish
       (lambda ()
         ;; Unchanged bytes already belong to the authoritative snapshot.
         ;; Otherwise this cleanup is permitted only after head adoption.
         (when (and saved-transcript-p (buffer-live-p buffer))
           (with-current-buffer buffer
             (condition-case err
                 (progn
                   (set-visited-file-modtime 0)
                   (let ((current (niwa-session-artifacts--transcript-proof session)))
                     (when (and (= frozen-tick (buffer-chars-modified-tick))
                                (= frozen-modification-tick (buffer-modified-tick))
                                (eq (cadr current) (cadr frozen-proof))
                                (equal (butlast current) (butlast frozen-proof)))
                       (setq niwa-session-artifacts--committed-transcript-proof current)
                       (set-buffer-modified-p nil)))
                   (unless unchanged
                     (niwa-session-artifacts--run-save-hooks-silently
                      'after-save-hook)))
               (error
                (niwa--warn-once
                 (niwa-diagnostic-scope session)
                 'artifacts-session-post-save
                 "Session post-save cleanup failed: %s"
                 (error-message-string err))))))
         ;; Input history is diagnostic UI state, not critical publication.
         (niwa-session-persistence-notify-session-event session 'save-history)
         (when (buffer-live-p buffer)
           (niwa-session-artifacts-save-instructions session))
         (niwa-session-save-path session))))))

(defun niwa-session-artifacts--remote-save
    (session buffer settled &optional force token)
  "Publish portable SESSION's save from BUFFER synchronously.
SETTLED, FORCE and TOKEN have the public save entry's meanings."
  (let* ((prepared (niwa-session-artifacts-prepare-save
                    session buffer settled force))
         (artifacts (plist-get prepared :artifacts)))
    (when artifacts
      (niwa-session-publication-publish
       session artifacts niwa-session-artifacts-require-agent-commit-p t token))
    (funcall (plist-get prepared :finish))))

(defun niwa-session-artifacts-save
    (session buffer &optional settled force token)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, checkpoint any tool-modified
files from before this turn, and rewrite the sidecar.  When SETTLED is non-nil,
mark the latest assistant response
  as a stable fork point.

A portable session whose durable state is byte for byte the committed one
performs no target transaction.  FORCE publishes anyway, for a caller that
must materialize a snapshot rather than record a change.  TOKEN is the caller's
gate token."
  ;; Explicitly synchronous: drain async work before anything is captured.
  (niwa-session-artifacts--barrier session token)
  (niwa-session-artifacts-call-with-checkpoint
   session
   (lambda ()
     (niwa-telemetry-measure
      session 'session-save 0
      (lambda ()
        (niwa-session-artifacts--save session buffer settled force token))
      :kind 'full :settled (and settled t)))))

(defun niwa-session-artifacts--save (session buffer settled force &optional token)
  "Save SESSION from BUFFER as `niwa-session-artifacts-save' documents.
SETTLED and FORCE are its settlement and forced-publication flags; TOKEN is
the caller's gate token."
  ;; A direct data-buffer caller establishes the session-owned root before
  ;; the projection boundary is consulted.  View callers must pass their
  ;; registered data buffer, never the view projection itself.
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (boundp 'niwa--session)
                    (eq niwa--session session)
                    (not (bound-and-true-p niwa--agent-invocation)))))
    (when (or (null (niwa-session-root-buffer session))
              (eq buffer (niwa-session-root-buffer session)))
      (niwa-session-control-transfer-register-root-buffer
       session buffer)))
  (when-let* ((buffer (niwa-session-persistence-authoritative-buffer
                      buffer)))
    (when (or (null (niwa-session-root-buffer session))
              (eq buffer (niwa-session-root-buffer session)))
      (niwa-session-control-transfer-register-root-buffer session buffer))
    ;; The mutation gate and the publication entry both consult the target
    ;; recovery marker, and nothing in between can install one.
    (let ((niwa-session-recovery--mutation-cache
           (or (bound-and-true-p niwa-session-recovery--mutation-cache)
               (list nil)))
          ;; One save is one durable transaction, so its lease operations
          ;; share the target clock reading rather than each paying for one.
          ;; The durability module loads lazily, so the outer values are
          ;; read tolerantly: a save may run before anything durable ever
          ;; loaded it.
          (niwa-session-durability--transaction-clock
           (or (bound-and-true-p niwa-session-durability--transaction-clock)
               (list nil)))
          (niwa-session-durability--asserted-directories
           (or (bound-and-true-p
                niwa-session-durability--asserted-directories)
               (list nil))))
      ;; The whole save is one transaction on one connection, and every
      ;; segment write, sidecar rewrite, modtime stat, and file-history read
      ;; below is a command on it.  A foreign idle timer that sends its own
      ;; command in between consumes the reply we are waiting for, so the
      ;; transaction holds the connection rather than merely refusing to nest
      ;; inside somebody else's.
      (niwa--with-gc-batched
      (niwa-transport-with-exclusive-connection
        ;; A save is a real durable mutation even for file-workspace sessions.
        ;; Probe the target before any branch constructs serialized bytes.
        (niwa-session-artifacts-assert-mutation-authority session buffer)
        (prog1
            (niwa-session-artifacts--remote-save session buffer settled force token)
          (niwa-session-persistence-flush-diagnostic-logs session)))))))

(defun niwa-session-artifacts-save-agent-registry (session buffer &optional token)
  "Persist SESSION's sidecar from BUFFER without touching the segment.

Observational agent-state saves -- activity transitions, explicit mailbox
discards -- need only the sidecar's agent registry: the transcript
segment is committed at request settlement anyway.  The full save
transaction wrote the whole segment and scanned snapshots on every
debounced tick, which dominated a profiled multi-agent session's
allocation and produced a visible segment write every few seconds.

A portable session publishes the sidecar with any retained artifact batches:
the manifest overlay keeps every other committed entry, so the root segment
and instruction entries are neither read nor rewritten.  When
`niwa-session-artifacts-require-agent-commit-p' is non-nil, publication
must commit synchronously and refuses reentrant queueing.
The full save was kept for portable sessions on the assumption that its
byte comparison made an unchanged save free; it did not, because the
segment copy and instruction serialization run
before the comparison and the registry a persist is about differs from
the committed one by construction.  Before the first full snapshot has
committed a sidecar there is nothing to overlay, so the save is skipped
and the next critical commit carries the registry.

Returns SESSION's save path on success, nil when SESSION is not yet
materialized or has no committed sidecar, or when the save was deferred behind
the session's publication gate (the deferred preparation repeats this entry).
An acknowledged commit instead drains the gate first.  TOKEN is the caller's
gate token; a deferred preparation receives its own."
  (unless (and (not niwa-session-artifacts-require-agent-commit-p)
               (niwa-session-artifacts--defer
                session
                (lambda (prepared-token)
                  (niwa-session-artifacts-save-agent-registry
                   session buffer prepared-token))
                #'ignore token))
    (when niwa-session-artifacts-require-agent-commit-p
      (niwa-session-artifacts--barrier session token))
    (niwa-telemetry-measure
     session 'session-save 0
     (lambda ()
       (niwa-session-artifacts--save-agent-registry
        session buffer token))
     :kind 'agent-registry)))

(defun niwa-session-artifacts--save-agent-registry (session buffer &optional token)
  "Persist SESSION's sidecar from BUFFER as the public entry documents."
  (when (memq session niwa-session-artifacts--terminal-sessions)
    (user-error "Session checkpoint is being prepared"))
  (when-let* ((save-path (niwa-session-save-path session))
              (buffer (niwa-session-persistence-authoritative-buffer
                       buffer)))
    (let ((niwa-session-recovery--mutation-cache
           (or (bound-and-true-p niwa-session-recovery--mutation-cache)
               (list nil)))
          (niwa-session-durability--transaction-clock
           (or (bound-and-true-p
                niwa-session-durability--transaction-clock)
               (list nil)))
          (niwa-session-durability--asserted-directories
           (or (bound-and-true-p
                niwa-session-durability--asserted-directories)
               (list nil))))
      (when (niwa-session-artifacts-artifact-present-p session "session.meta.el" t)
        (niwa--with-gc-batched
          (niwa-transport-with-exclusive-connection
            (niwa-session-artifacts-assert-mutation-authority
             session buffer)
            (setf (niwa-session-updated-at session)
                  (format-time-string "%FT%H-%M-%S"))
            (niwa-session-publication-publish
             session
             (list (niwa-session-artifacts--sidecar-artifact session buffer))
             niwa-session-artifacts-require-agent-commit-p nil token)
            (niwa-session-persistence-notify-session-event
             session 'save-history)
            save-path))))))


;;
;;; File-history store
;;
;; Per-session on-disk backup store at <save-path>/file-history/.
;; Files are named by the full SHA-256 of their raw contents, so equal
;; pre/post images across files and turns share storage.  Per-file versions
;; belong to the checkpoint index.  Mapping from (turn, path) to
;; backup filename lives in `niwa-session-file-snapshots' (alist
;; keyed by turn number; inner alist keyed by absolute path).

(defun niwa-session-artifacts--file-history-backup-name (content)
  "Return the content-addressed backup filename for raw CONTENT."
  (secure-hash 'sha256 content))

(defun niwa-session-artifacts-backup-path (save-path backup-name)
  "Return the absolute path to BACKUP-NAME under SAVE-PATH's file-history/."
  (file-name-concat save-path "file-history" backup-name))

(defun niwa-session-artifacts--file-history-latest-version (session path)
  "Return the highest version recorded for PATH in SESSION, or 0 if none."
  (let ((best 0))
    (dolist (turn-entry (niwa-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best) (setq best v))))))

(defun niwa-session-artifacts-read-file-raw (path)
  "Return PATH's contents as a unibyte string (no encoding conversion)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-string)))

(defvar niwa-session-artifacts-backup-cache nil
  "Hash table of immutable backup bytes cached by a caller, or nil.
Rewind binds one around a whole operation, including its merge rounds.")

(defun niwa-session-artifacts-read-backup (session backup-name)
  "Return SESSION's BACKUP-NAME as literal bytes.

Resolve only the committed immutable publication."
  (let ((cache (and (string-match-p "\\`[0-9a-f]\\{64\\}\\'" backup-name)
                    niwa-session-artifacts-backup-cache)))
    (or (and cache (gethash backup-name cache))
        (let ((bytes
               (niwa-session-artifacts-read-artifact
                session (file-name-concat "file-history" backup-name) t)))
          (when cache (puthash backup-name bytes cache))
          bytes))))

(defun niwa-session-artifacts--file-history-write-backup (save-path backup-name content)
  "Atomically write CONTENT under `<SAVE-PATH>/file-history/BACKUP-NAME'."
  (let* ((dir  (file-name-concat save-path "file-history"))
         (dest (file-name-concat dir backup-name)))
    (if niwa-session-artifacts--collecting-critical-artifacts-p
        (push (list :path dest :content content)
              niwa-session-artifacts--critical-artifacts)
      ;; Backups are verbatim copies of the user's files inside session
      ;; storage; keep them owner-only rather than inheriting the
      ;; default modes an ordinary artifact gets.
      (niwa--write-file-atomically dest content 'no-conversion #o600))))

(defun niwa-session-artifacts--file-history-maybe-snapshot
    (session path pre-content &optional checkpoint)
  "Return SESSION's pre-turn checkpoint entry for changed PATH.

PATH is an absolute filesystem path that was touched by the just-completed
turn's tools.  PRE-CONTENT is PATH's content at the start of the turn,
or nil if it did not exist.

Returns nil when PATH did not change.  PRE-CONTENT is a string, nil for a
previously absent path, or `(:gap REASON)' when pre-turn capture failed.
CHECKPOINT is this turn's previous capture for PATH.  Reuse its version and
timestamp: another save updates this checkpoint, not the file's turn history."
  (let* ((current-exists (file-exists-p path))
         (version (or (plist-get checkpoint :version)
                      (1+ (niwa-session-artifacts--file-history-latest-version session path))))
         (base (list :version version
                     :backup-time (or (plist-get checkpoint :backup-time)
                                      (format-time-string "%FT%H-%M-%S"))))
         (gap
          (lambda (reason)
            (niwa-diagnostic-warning (niwa-diagnostic-scope session)
             'niwa (format "Checkpoint gap for %s: %s" path reason)
             :warning)
            (cons path (append base (list :gap reason))))))
    (cond
     ((and (listp pre-content) (plist-get pre-content :gap))
      (funcall gap (plist-get pre-content :gap)))
     ((and (stringp pre-content)
           (> (string-bytes pre-content)
              niwa-file-history-max-snapshot-bytes))
      (funcall gap
               (format "pre-turn content exceeds %d bytes"
                       niwa-file-history-max-snapshot-bytes)))
     ((and current-exists (not (file-regular-p path)))
      (funcall gap "post-turn path is not a regular file"))
     (t
      (condition-case err
          (let ((current-content
                 (and current-exists
                      (niwa-session-artifacts-read-file-raw path))))
            (unless (if current-exists
                        (and (stringp pre-content)
                             (string-equal pre-content current-content))
                      (null pre-content))
              (let* ((backup-name
                      (and current-exists
                           (niwa-session-artifacts--file-history-backup-name current-content)))
                     (pre-backup-name
                      (and (stringp pre-content)
                           (niwa-session-artifacts--file-history-backup-name pre-content))))
                (when backup-name
                  (niwa-session-artifacts--file-history-write-backup
                   (niwa-session-save-path session)
                   backup-name current-content))
                (when pre-backup-name
                  (niwa-session-artifacts--file-history-write-backup
                   (niwa-session-save-path session)
                   pre-backup-name pre-content))
                (cons path
                      (append base
                              (list :backup-name backup-name
                                    :pre-backup-name pre-backup-name))))))
        (error (funcall gap (error-message-string err))))))))

(defun niwa-session-artifacts-snapshot-modified
    (session turn-n pre-snapshots &optional untracked)
  "Snapshot files modified during TURN-N for SESSION.

PRE-SNAPSHOTS is a hash-table mapping absolute path to the file's content
at turn start (nil for paths that did not yet exist).  Typically the
`file-snapshots' slot of the just-completed request.
UNTRACKED is the request's alist of tool effects outside those snapshots;
when non-nil it is recorded under TURN-N for Rewind's coverage report.

For each changed path, persist its pre- and post-turn content or absence
under TURN-N.  Known capture failures remain explicit gap entries.  TURN-N
is recorded even when no tracked file changed.  Returns the list of backup
names written."
  (when (and (niwa-session-save-path session)
             (hash-table-p pre-snapshots))
    (when untracked
      (setf (alist-get turn-n (niwa-session-untracked-effects session))
            (copy-tree untracked)))
    (let ((previous (cdr (assoc turn-n (niwa-session-file-snapshots session))))
          entries written)
      (maphash
       (lambda (path pre-content)
         (when-let* ((entry (niwa-session-artifacts--file-history-maybe-snapshot
                            session path pre-content (cdr (assoc path previous)))))
           (push entry entries)
           (when-let* ((name (plist-get (cdr entry) :backup-name)))
             (push name written))
           (when-let* ((name (plist-get (cdr entry) :pre-backup-name)))
             (push name written))))
       pre-snapshots)
      ;; Sort entries by path so two saves with identical state
      ;; produce byte-identical sidecars (hash-table iteration is
      ;; otherwise non-deterministic).
      (setq entries (sort entries
                          (lambda (a b) (string< (car a) (car b)))))
      (let ((cell (assoc turn-n (niwa-session-file-snapshots session))))
        (if cell
            (setcdr cell entries)
          (setf (niwa-session-file-snapshots session)
                (cons (cons turn-n entries)
                      (niwa-session-file-snapshots session)))))
      (sort written #'string<))))

(cl-defun niwa-session-artifacts-record-applied
    (session turn changes &key application-backups then)
  "Record the applied prepared CHANGES under SESSION's TURN, then save.
Each change is a plist with :path, :before and optionally :history-before, as
`niwa-tool-patch-commit' takes.  The source is already written.  TURN's
previous capture supplies each path's turn pre-content: an existing gap is
kept, later edits give an `edited after turn N' gap, and a path never captured
uses the change's before bytes.  Entries for untouched paths stay.

With APPLICATION-BACKUPS also write backups of the change's own before bytes
and pass their entries (without :version and :backup-time) to THEN.  THEN is
called once, synchronously, after publication and before the save.  A failure
leaves THEN uncalled and the session unsaved; the source stays changed."
  (let ((previous (cdr (assq turn (niwa-session-file-snapshots session))))
        (pre (make-hash-table :test #'equal))
        files)
    (dolist (change changes)
      (let* ((path (plist-get change :path))
             (old (cdr (assoc path previous)))
             (bytes (plist-get (or (plist-get change :history-before)
                                   (plist-get change :before))
                               :bytes))
             (name (plist-get old :pre-backup-name)))
        (puthash path (cond ((plist-get old :gap) (list :gap (plist-get old :gap)))
                            ((and old
                                  (not (equal (and bytes (secure-hash 'sha256 bytes))
                                              (plist-get old :backup-name))))
                             (list :gap (format "edited after turn %d" turn)))
                            (old (and name (niwa-session-artifacts-read-backup session name)))
                            (t bytes))
                 pre)))
    (let ((niwa-session-artifacts--collecting-critical-artifacts-p
           (niwa-session-codec-portable-authority-p session))
          (niwa-session-artifacts--critical-artifacts nil))
      (niwa-session-artifacts-snapshot-modified session turn pre)
      (when application-backups
        (dolist (change changes)
          (when-let* ((entry (niwa-session-artifacts--file-history-maybe-snapshot
                              session (plist-get change :path)
                              (plist-get (or (plist-get change :history-before)
                                             (plist-get change :before))
                                         :bytes))))
            (cl-remf (cdr entry) :version)
            (cl-remf (cdr entry) :backup-time)
            (push entry files))))
      (when niwa-session-artifacts--critical-artifacts
        (niwa-session-publication-publish
         session (nreverse niwa-session-artifacts--critical-artifacts) t)))
    (let ((cell (assq turn (niwa-session-file-snapshots session))))
      (setcdr cell (sort (append (cdr cell)
                                 (cl-remove-if
                                  (lambda (entry)
                                    (not (eq 'absent (gethash (car entry) pre 'absent))))
                                  previous))
                         (lambda (a b) (string< (car a) (car b))))))
    (when then (funcall then files))
    (with-current-buffer (niwa-session-root-buffer session)
      (when (and niwa--current-request
                 (= turn (niwa-request-turn niwa--current-request)))
        (maphash (lambda (path bytes)
                   (puthash path bytes (niwa-request-file-snapshots
                                        niwa--current-request)))
                 pre))
      ;; The live turn would otherwise re-snapshot over the merged cell.
      (let ((niwa--current-request nil))
        (niwa-session-artifacts-save session (current-buffer) nil t)))))

;;
;;; Segment rotation (split-on-compact)

(defun niwa-session-artifacts--file-text (file)
  "Return FILE contents as a string using normal text decoding."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun niwa-session-artifacts-refresh-visited-file-modtime-or-error
    (&optional expected-texts)
  "Refresh stale visited-file metadata when disk text is expected.

EXPECTED-TEXTS is a string or list of strings that may also match the
visited file.  This covers automatic edits that first remove transient
unsaved text from the live buffer.  If the visited file changed externally
to different text or was deleted, signal a controlled error instead of
letting `save-buffer' ask an interactive supersession question during
automatic segment rotation.  Portable transcripts have no authoritative
visited file; their caller checks the lease and publication instead."
  (when (and buffer-file-name
             (not (and (bound-and-true-p niwa--session)
                       (niwa-session-codec-portable-authority-p
                        niwa--session))))
    (cond
     ((not (file-exists-p buffer-file-name))
      (when buffer-file-number
        (error "Session segment changed on disk: %s" buffer-file-name)))
     ((not (verify-visited-file-modtime (current-buffer)))
      (let ((file-text (niwa-session-artifacts--file-text buffer-file-name))
            (accepted (cons (buffer-substring-no-properties (point-min) (point-max))
                            (if (listp expected-texts)
                                expected-texts
                              (list expected-texts)))))
        (if (member file-text accepted)
            (set-visited-file-modtime)
          (error "Session segment changed on disk: %s" buffer-file-name)))))))

(defun niwa-session-artifacts-disown-save-machinery ()
  "Keep Emacs backups and file locks off the current buffer's segment.

Both are answers to problems this session already answers better.  A backup is
the immutable publication and the file-history checkpoint; a lock is the
durable lease, which a second client observes before its buffer becomes
writable, where Emacs's lock is only advisory between Emacsen.

On a target they are not free.  The backup is one whole-segment copy over the
connection, and the lock is a symlink created and removed around every
modify-and-save cycle.  They also leave `segment-NNNN.chat.org~' and
`.#segment-NNNN.chat.org' inside a portable session directory that another
client resumes from, where neither means anything."
  (setq-local make-backup-files nil)
  (setq-local create-lockfiles nil))

(defun niwa-session-artifacts--set-visited-segment-file (file &optional frozen-tick)
  "Make the current buffer visit segment FILE without changing its name.
When FROZEN-TICK is supplied, keep edits since that frozen snapshot dirty."
  (let ((name (buffer-name)))
    (set-visited-file-name file t)
    (rename-buffer name t))
  (setq buffer-file-truename (file-truename file))
  (niwa-session-artifacts-disown-save-machinery)
  (set-visited-file-modtime
   (and (bound-and-true-p niwa--session)
        (niwa-session-codec-portable-authority-p niwa--session) 0))
  (set-buffer-modified-p
   (and frozen-tick (/= frozen-tick (buffer-chars-modified-tick)))))

(defun niwa-session-artifacts--insert-segment-header (session)
  "Insert per-segment org properties at point in current buffer.

Sets `NIWA_SESSION_ID', `NIWA_SEGMENT_NUMBER',
`NIWA_SEGMENT_CREATED_AT', and `NIWA_VERSION'.  Caller is
responsible for ensuring the buffer is in `org-mode' (niwa data
buffers are locked to `org-mode' by `niwa--chat-buffer-setup')."
  (when (derived-mode-p 'org-mode)
    (org-entry-put (point-min) "NIWA_VERSION" (niwa-version))
    (org-entry-put (point-min) "NIWA_SESSION_ID"
                   (or (niwa-session-session-id session) ""))
    (org-entry-put (point-min) "NIWA_SEGMENT_NUMBER"
                   (number-to-string
                    (or (niwa-session-current-segment session) 1)))
    (org-entry-put (point-min) "NIWA_SEGMENT_CREATED_AT"
                   (format-time-string "%FT%H-%M-%S"))))

(defun niwa-session-artifacts-segment-summary-bounds ()
  "Return bounds for the leading segment compaction summary, or nil.

The returned plist contains `:begin', `:body-begin', `:body-end',
and `:end'.  A summary is accepted only when it is the first top-level
content after the optional org property drawer and whitespace."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\r\n\f")
    (when (looking-at ":PROPERTIES:\n")
      (search-forward "\n:END:\n" nil 'move)
      (skip-chars-forward " \t\r\n\f"))
    (when (looking-at "^#\\+begin_summary\\b.*$")
      (let ((begin (match-beginning 0))
            (body-begin (match-end 0)))
        (goto-char body-begin)
        (when (re-search-forward "^#\\+end_summary\\b.*$" nil t)
          (list :begin begin
                :body-begin (1+ body-begin)
                :body-end (match-beginning 0)
                :end (match-end 0)))))))

(defconst niwa-session-artifacts--summary-handoff-prefix
  "Another language model started to solve this problem and produced a summary of its work. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary:\n\n"
  "Model-facing preface inserted before a compacted segment summary.")

(defun niwa-session-artifacts-strip-summary-handoff-prefix (summary)
  "Return SUMMARY without the model-facing handoff prefix."
  (if (and (stringp summary)
           (string-prefix-p
            niwa-session-artifacts--summary-handoff-prefix summary))
      (substring summary
                 (length niwa-session-artifacts--summary-handoff-prefix))
    summary))

(defun niwa-session-artifacts-summary-block (summary)
  "Return SUMMARY wrapped in an org `#+begin_summary' block.

The block markers are propertized with `gptel \\='ignore' so the LLM sees
only the handoff preface plus SUMMARY -- not the wrapper lines.  The
user\\='s view, by contrast, sees a foldable block."
  (concat (propertize "#+begin_summary niwa-role=compaction-summary\n"
                      'gptel 'ignore)
          niwa-session-artifacts--summary-handoff-prefix
          summary
          (propertize "\n#+end_summary\n" 'gptel 'ignore)))

(defun niwa-session-artifacts-finalized-segment-text
    (text coding &optional session segment)
  "Return segment TEXT with finalized metadata, using buffer coding CODING.
When SESSION is non-nil, refresh its SEGMENT index against the exact returned
text.  SEGMENT defaults to SESSION's current segment.  Index preparation uses
a private copy, so a failed preparation leaves SESSION unchanged."
  (with-temp-buffer
    (setq buffer-file-coding-system coding)
    (insert text)
    (let ((org-agenda-file-menu-enabled nil))
      (org-mode))
    (niwa-transcript-restore-properties t)
    (org-entry-put (point-min) "NIWA_SEGMENT_FINALIZED_AT"
                   (format-time-string "%FT%H-%M-%S"))
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (when session
      (let* ((segment (or segment (niwa-session-current-segment session)))
             (candidate (copy-sequence session))
             (index (copy-tree (niwa-session-prompt-index session)))
             (fork-points
              (niwa-session-artifacts-fork-point-spans (current-buffer))))
        ;; A refresh must never silently erase a previously indexed completion.
        ;; Continuations can live on an earlier prompt even when this segment
        ;; now contains later prompts of its own.
        (dolist (entry index)
          (dolist (prompt (cdr entry))
            (let* ((continuation (plist-get prompt :continuation))
                   (source (cond
                            ((eql (plist-get continuation :segment) segment)
                             continuation)
                            ((eql (car entry) segment) prompt)))
                   (id (plist-get source :fork-point-id)))
              (when id
                (let ((span (cl-find id fork-points
                                     :key (lambda (record)
                                            (plist-get record :fork-point-id))
                                     :test #'equal)))
                  (unless span
                    (error "Completed turn is missing from finalized segment"))
                  (when (eq source continuation)
                    (plist-put continuation :start
                               (niwa-session-artifacts--continuation-start
                                (current-buffer)))
                    (plist-put continuation :transcript-cutoff
                               (plist-get span :transcript-cutoff))))))))
        (setf (niwa-session-current-segment candidate) segment
              (niwa-session-prompt-index candidate) index)
        (niwa-session-artifacts-update-prompt-index candidate (current-buffer))
        (let ((refreshed (cdr (assoc segment
                                    (niwa-session-prompt-index candidate)))))
          (dolist (prompt (cdr (assoc segment
                                     (niwa-session-prompt-index session))))
            (when-let* ((id (plist-get prompt :fork-point-id)))
              (unless (cl-find id refreshed
                               :key (lambda (entry) (plist-get entry :fork-point-id))
                               :test #'equal)
                (error "Completed turn is no longer indexed in finalized segment")))
            ;; Recovery can finalize a predecessor whose turn has already
            ;; completed in a successor.  Reindexing its prompt must retain
            ;; that separate segment's completion coordinates unchanged.
            (when-let* ((continuation (plist-get prompt :continuation))
                        ((not (eql segment (plist-get continuation :segment)))))
              (let ((replacement
                     (cl-find (plist-get prompt :file-turn) refreshed
                              :key (lambda (entry) (plist-get entry :file-turn)))))
                (unless replacement
                  (error "Continued turn is no longer indexed in finalized segment"))
                (plist-put replacement :continuation (copy-tree continuation))))))
        (setf (niwa-session-prompt-index session)
              (niwa-session-prompt-index candidate))))
    (buffer-string)))

(defun niwa-session-artifacts--publish-remote-segment-transition
    (session buffer old-segment old-text new-segment new-text checkpoint
             &optional require-commit)
  "Publish one portable project SESSION segment transition derived from BUFFER.

OLD-SEGMENT receives already finalized and indexed OLD-TEXT.
NEW-SEGMENT receives NEW-TEXT.
The sidecar joins the same batch as its commit marker.
REQUIRE-COMMIT rejects reentrant publication.
CHECKPOINT carries the identities and root fields frozen with these segments."
  (with-current-buffer buffer
    (let ((coding buffer-file-coding-system))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert new-text))
      ;; Keep the live transition installed if publication becomes pending.
      ;; Retry can then publish the exact state represented in memory.
      (setq buffer-file-name new-segment
            buffer-file-truename nil)
      (let* ((frozen-tick (buffer-chars-modified-tick))
             (sidecar
              (niwa-session-artifacts--sidecar-artifact
               session buffer checkpoint)))
        (unwind-protect
            (progn
              (niwa-session-publication-publish
               session
               (append
                (list
                 (list :path old-segment :content old-text :coding coding)
                 (list :path new-segment :content new-text :coding coding))
                (list sidecar))
               require-commit t)
              (niwa-session-artifacts--set-visited-segment-file new-segment frozen-tick))
          (set-buffer-modified-p (/= frozen-tick (buffer-chars-modified-tick))))))))

(defun niwa-session-artifacts--delete-trailing-text (text)
  "Delete trailing TEXT from the current buffer when it is an exact suffix."
  (when (and text
             (not (string-empty-p text))
             (string-suffix-p
              (substring-no-properties text)
              (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (- (point-max) (length text)) (point-max))
    t))

(cl-defun niwa-session-artifacts-rotate-segment
    (session buffer summary &key tail-text pending-text archive-text
             truncated-tail-p)
  "Finalize SESSION's current segment and start a new one with SUMMARY.

Advance SESSION's segment number and build the successor in a temporary
buffer.  Publish the finalized predecessor, successor, and sidecar in one
immutable head.  Publication
repoints BUFFER's visited-file state; PENDING-TEXT is restored only in the
live buffer afterward.

TAIL-TEXT is preserved recent transcript text, including text
properties.  ARCHIVE-TEXT contains durable hidden records replacing
execution rows removed by compaction.  PENDING-TEXT is an
inserted-but-unsent prompt region.
TRUNCATED-TAIL-P is recorded as segment metadata when non-nil.

Requires SESSION to have a `save-path' (i.e., to have been lazily
materialized).  Returns the new segment's absolute path on success,
nil if SESSION is not yet materialized."
  (when (niwa-session-save-path session)
    (niwa-session-durability-with-transaction
      (niwa-session-artifacts-assert-mutation-authority session buffer)
      (niwa-session-artifacts-call-with-checkpoint
       session
       (lambda ()
         (niwa-session-artifacts--rotate-segment
          session buffer summary :tail-text tail-text :pending-text pending-text
          :archive-text archive-text :truncated-tail-p truncated-tail-p))))))

(cl-defun niwa-session-artifacts--rotate-segment
    (session buffer summary &key tail-text pending-text archive-text
             truncated-tail-p)
  "Perform SESSION's guarded segment rotation in BUFFER using SUMMARY.
TAIL-TEXT, PENDING-TEXT, ARCHIVE-TEXT and TRUNCATED-TAIL-P describe the
successor segment."
  (when (niwa-session-save-path session)
    (with-current-buffer buffer
      ;; 1. Save the current segment before replacing the buffer body.
      (let ((old-segment buffer-file-name)
            (old-current-segment (niwa-session-current-segment session))
            (old-updated-at (niwa-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (tail-prompt-count
             (niwa-session-artifacts--prompt-count-in-text tail-text))
            (old-prompt-index (copy-tree (niwa-session-prompt-index session)))
            checkpoint
            old-freeze-tick
            committed
            old-publish-text
            new-segment
            new-text
            pending-position)
        (let ((telemetry-span
               (niwa-telemetry-start
                session 'segment-rotation
                :old-segment old-current-segment
                :new-segment (1+ old-current-segment))))
          (condition-case err
              (progn
                (niwa-telemetry-record
                 session 'segment-rotation-stage :stage 'old-save-start
                 :old-segment old-current-segment)
                (niwa-session-artifacts-refresh-visited-file-modtime-or-error
                 (when (and pending-text
                            (string-suffix-p
                             (substring-no-properties pending-text)
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
                   (buffer-substring-no-properties
                    (point-min) (- (point-max) (length pending-text)))))
                (when pending-text
                  (let ((inhibit-read-only t))
                    (niwa-session-artifacts--delete-trailing-text
                     pending-text)))
                (setq checkpoint (niwa-session-checkpoint-capture session "/root")
                      old-freeze-tick (buffer-chars-modified-tick))
                (setq old-publish-text
                      (niwa-session-artifacts-finalized-segment-text
                       (buffer-substring (point-min) (point-max))
                       buffer-file-coding-system session old-current-segment))
                (niwa-telemetry-record
                 session 'segment-rotation-stage :stage 'old-saved
                 :old-segment old-current-segment)
                ;; 2. Advance segment counter.
                (cl-incf (niwa-session-current-segment session))
                ;; 3. Build the new segment without changing visited-file state.
                (setq new-segment
                      (niwa-session-artifacts-segment-path
                       (niwa-session-save-path session)
                       (niwa-session-current-segment session)))
                (let ((coding-system buffer-file-coding-system))
                  (setq new-text
                        (with-temp-buffer
                          (setq buffer-file-coding-system coding-system)
                          (org-mode)
                          ;; 4. Build the persisted body in isolation.
                          (niwa-session-artifacts--insert-segment-header
                           session)
                          (when truncated-tail-p
                            (org-entry-put
                             (point-min) "NIWA_SEGMENT_TRUNCATED_TAIL" "t"))
                          (when (> tail-prompt-count 0)
                            (org-entry-put
                             (point-min) "NIWA_SEGMENT_TAIL_PROMPTS"
                             (number-to-string tail-prompt-count)))
                          (goto-char (point-max))
                          (unless (bolp) (insert "\n"))
                          (insert "\n")
                          (insert
                           (niwa-session-artifacts-summary-block summary))
                          (when tail-text
                            (unless (bolp) (insert "\n"))
                            (insert tail-text))
                          (when archive-text
                            (insert archive-text))
                          (niwa-session-artifacts-stabilize-gptel-bounds)
                          (when pending-text
                            (unless (bolp) (insert "\n"))
                            (setq pending-position (point)))
                          (insert "\n")
                          (buffer-string))))
                ;; 5. Publish, then repoint the canonical live buffer.
                (niwa-telemetry-record
                 session 'segment-rotation-stage :stage 'new-publish-start
                 :new-segment (niwa-session-current-segment session))
                (setf (niwa-session-updated-at session)
                      (format-time-string "%FT%H-%M-%S"))
                (setq checkpoint
                      (niwa-session-checkpoint-with-root
                       checkpoint (niwa-session-artifacts--live-sidecar session buffer)))
                (unless (= old-freeze-tick (buffer-chars-modified-tick))
                  ;; Preserve late predecessor edits when aborting preparation.
                  (setq old-text (concat (buffer-substring (point-min) (point-max))
                                         pending-text)
                        old-modified-p t)
                  (error "Transcript changed during segment transition; retry"))
                ;; Do not let an ordinary keyboard quit separate the durable
                ;; commit from its in-memory marker.  Explicit quit signals from
                ;; a failed operation still enter the cleanup below.
                (let ((inhibit-quit t))
                  (niwa-session-artifacts--publish-remote-segment-transition
                   session buffer old-segment old-publish-text
                   new-segment new-text checkpoint)
                  (setq committed t))
                (niwa-telemetry-record
                 session 'segment-rotation-stage :stage 'new-published
                 :new-segment (niwa-session-current-segment session))
                (niwa-session-forget-read-ranges session)
                (when telemetry-span
                  (niwa-telemetry-finish
                   telemetry-span :outcome 'success))
                (when pending-position
                  (let ((modified (buffer-modified-p)))
                    (goto-char pending-position)
                    (insert pending-text)
                    (setq pending-position nil)
                    ;; The pending prompt is not a durable late arrival.
                    (set-buffer-modified-p modified)))
                (when (fboundp 'niwa-collaboration-notify-history-changed)
                  (niwa-collaboration-notify-history-changed buffer))
                new-segment)
            ((error quit)
             (let ((inhibit-quit t))
               (when telemetry-span
                 (niwa-telemetry-finish
                  telemetry-span :outcome 'error :error-class (car-safe err)))
               ;; Ancillary publications may commit during preparation.  Only
               ;; the installed root projection proves this transition committed.
               (if (or committed
                       (niwa-session-pending-publication session)
                       (not (equal old-current-segment
                                   (plist-get (niwa-session-checkpoint-project session nil)
                                              :current-segment))))
                   (progn
                     (when pending-position
                       (let ((modified (buffer-modified-p)))
                         (goto-char pending-position)
                         (insert pending-text)
                         (set-buffer-modified-p modified))))
                 (setf (niwa-session-current-segment session)
                       old-current-segment)
                 (setf (niwa-session-updated-at session) old-updated-at
                       (niwa-session-prompt-index session) old-prompt-index)
                 (when (and new-text
                            (string-prefix-p new-text (buffer-string))
                            (> (buffer-size) (length new-text)))
                   ;; A failed successor write must not discard appended mail.
                   (setq old-text
                         (concat old-text (buffer-substring
                                           (+ (point-min) (length new-text)) (point-max)))
                         old-modified-p t))
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert old-text))
                 (niwa-session-artifacts--set-visited-segment-file
                  old-segment)
                 (goto-char (min old-point (point-max)))
                 (set-buffer-modified-p old-modified-p)
                 (niwa-session-publication-discard-rolled-back session)))
             (signal (car err) (cdr err)))))))))

(cl-defun niwa-session-artifacts-start-fresh-segment
    (session buffer &key clear)
  "Finalize SESSION's current segment and start a blank live segment in BUFFER.

When CLEAR is non-nil, rearm automatic naming in the new segment's sidecar
and seal completed journal evidence only after the transition commits.

Requires SESSION to have a `save-path'.  Returns the new segment's
absolute path on success, nil if SESSION is not yet materialized."
  (when (niwa-session-save-path session)
    (niwa-session-durability-with-transaction
      (niwa-session-artifacts-assert-mutation-authority session buffer)
      (niwa-session-artifacts-call-with-checkpoint
       session
       (lambda ()
         (niwa-session-artifacts--start-fresh-segment session buffer :clear clear))))))

(cl-defun niwa-session-artifacts--start-fresh-segment
    (session buffer &key clear)
  "Perform SESSION's guarded fresh segment transition in BUFFER.
CLEAR also closes the current chat and rearms automatic naming."
  (when (niwa-session-save-path session)
    (with-current-buffer buffer
      (let ((old-segment buffer-file-name)
            (old-current-segment (niwa-session-current-segment session))
            (old-updated-at (niwa-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (old-naming-state (niwa-session-naming-state session))
            (old-naming-input (niwa-session-naming-input session))
            (old-prompt-index (copy-tree (niwa-session-prompt-index session)))
            (old-chapters (copy-tree (niwa-session-chapters session)))
            capture committed checkpoint old-freeze-tick
            old-publish-text
            new-segment
            new-text)
        (unless old-segment
          (error "No current segment file"))
        (when clear
          (condition-case err
              (setq capture (niwa-journal-capture-checkpoint session buffer))
            (error
             (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                              (format "Journal capture checkpoint failed: %s"
                                      (error-message-string err))
                              :warning))))
        (unwind-protect
            (condition-case err
                (progn
                  (niwa-session-artifacts-refresh-visited-file-modtime-or-error)
                  (setq checkpoint (niwa-session-checkpoint-capture session "/root")
                        old-freeze-tick (buffer-chars-modified-tick))
                  (setq old-publish-text
                        (niwa-session-artifacts-finalized-segment-text
                         (buffer-substring (point-min) (point-max))
                         buffer-file-coding-system session old-current-segment))
                  (when (and clear
                             (not (eq (niwa-session-naming-state session) 'explicit)))
                    ;; Old history is never naming input for the new segment.
                    (setf (niwa-session-naming-state session) 'pending
                          (niwa-session-naming-input session) nil))
                  (when clear
                    ;; The closing chat keeps the title it had.  An explicit
                    ;; name titles every chat alike, so it identifies none.
                    (setf (niwa-session-chapters session)
                          (append
                           (niwa-session-chapters session)
                           (list
                            (list :segment old-current-segment
                                  :title (unless (eq old-naming-state 'explicit)
                                           (niwa-session-name session))
                                  :closed-at (format-time-string "%FT%H-%M-%S"))))))
                  (cl-incf (niwa-session-current-segment session))
                  (setq new-segment
                        (niwa-session-artifacts-segment-path
                         (niwa-session-save-path session)
                         (niwa-session-current-segment session)))
                  (let ((coding-system buffer-file-coding-system))
                    (setq new-text
                          (with-temp-buffer
                            (setq buffer-file-coding-system coding-system)
                            (org-mode)
                            (niwa-session-artifacts--insert-segment-header
                             session)
                            (buffer-string))))
                  (setf (niwa-session-updated-at session)
                        (format-time-string "%FT%H-%M-%S"))
                  (setq checkpoint
                        (niwa-session-checkpoint-with-root
                         checkpoint (niwa-session-artifacts--live-sidecar session buffer)))
                  (unless (= old-freeze-tick (buffer-chars-modified-tick))
                    (setq old-text (buffer-substring (point-min) (point-max))
                          old-modified-p t)
                    (error "Transcript changed during segment transition; retry"))
                  ;; Defer keyboard quit through publication and its bookkeeping:
                  ;; the durable commit must not outrun its in-memory marker.
                  (let ((inhibit-quit t))
                    (niwa-session-artifacts--publish-remote-segment-transition
                     session buffer old-segment old-publish-text
                     new-segment new-text checkpoint clear)
                    (setq committed t))
                  (goto-char (point-max))
                  (niwa-compact-estimation-clear-baseline)
                  (niwa-session-persistence-notify-session-event
                   session 'rerender)
                  (when (fboundp 'niwa-collaboration-notify-history-changed)
                    (niwa-collaboration-notify-history-changed buffer))
                  new-segment)
              ((error quit)
               (let ((inhibit-quit t))
                 ;; A nested mail publication is not this clear's commit and
                 ;; must never authorize sealing its journal capture.
                 (cond
                  ((or committed
                       (not (equal old-current-segment
                                   (plist-get (niwa-session-checkpoint-project session nil)
                                              :current-segment))))
                   (setq committed t))
                  ((and (not clear)
                        (niwa-session-pending-publication session))
                   nil)
                  (t
                   (setf (niwa-session-current-segment session)
                         old-current-segment)
                   (setf (niwa-session-updated-at session) old-updated-at
                         (niwa-session-naming-state session) old-naming-state
                         (niwa-session-naming-input session) old-naming-input
                          (niwa-session-prompt-index session) old-prompt-index
                          (niwa-session-chapters session) old-chapters)
                   (when (and new-text
                              (string-prefix-p new-text (buffer-string))
                              (> (buffer-size) (length new-text)))
                     (setq old-text
                           (concat old-text (buffer-substring
                                             (+ (point-min) (length new-text)) (point-max)))
                           old-modified-p t))
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert old-text))
                   (niwa-session-artifacts--set-visited-segment-file
                    old-segment)
                   (goto-char (min old-point (point-max)))
                   (set-buffer-modified-p old-modified-p)
                   (niwa-session-publication-discard-rolled-back session))))
               (signal (car err) (cdr err))))
          (let ((inhibit-quit t))
            (when (and clear committed)
              (niwa-session-naming-cancel)
              (when capture
                (niwa-journal-capture-seal-and-schedule
                 session buffer 'clear (list capture))))))))))


;;
;;; Workspace relocation reconciliation

(defun niwa-session-artifacts-reconcile-relocation
    (session saved-workspace-plist)
  "Reconcile SESSION's path-bearing fields against workspace relocation.

If SAVED-WORKSPACE-PLIST's `:target-native-root' differs from SESSION's current
workspace root, rewrite permission rules whose `:path' starts with
the saved root and is not already under the current root, and prune
touched-files entries pointing at vanished paths.  Logs the rewrite
count to `*Messages*'.

A no-op when the saved root is missing or matches current."
  (let* ((saved-root
          (plist-get saved-workspace-plist :target-native-root))
         (current-root
          (niwa-execution-target-native-root
           (niwa-session-execution-target session)))
         (saved-root-expanded (and saved-root (expand-file-name saved-root)))
         (current-root-expanded (and current-root
                                     (expand-file-name current-root))))
    (when (and saved-root current-root
               (not (equal saved-root-expanded current-root-expanded)))
      ;; Rewrite path-bearing permission rules.
      (let ((rewrites 0)
            (saved-prefix (file-name-as-directory saved-root-expanded))
            (current-prefix (file-name-as-directory current-root-expanded)))
        (setf (niwa-session-permission-rules session)
              (mapcar
               (lambda (rule)
                 (let* ((path (plist-get (cdr rule) :path))
                        (expanded-path (and path (expand-file-name path))))
                   (cond
                    ((and expanded-path
                          (string-prefix-p saved-prefix expanded-path)
                          (not (string-prefix-p current-prefix expanded-path)))
                     (cl-incf rewrites)
                     (let ((new-rule (copy-tree rule)))
                       (plist-put
                        (cdr new-rule) :path
                        (concat current-prefix
                                (substring expanded-path
                                           (length saved-prefix))))
                       new-rule))
                    (t rule))))
               (niwa-session-permission-rules session)))
        ;; Touched-files resets to an empty hash on load, so there is
        ;; nothing to prune here today; kept as the conceptual place
        ;; if we ever start carrying touched-files across resume.
        (when (> rewrites 0)
          (message "Reconciled %d permission paths from %s to %s"
                   rewrites saved-root current-root))))))
(provide 'niwa-session-artifacts)

;;; niwa-session-artifacts.el ends here
