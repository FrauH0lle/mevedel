;;; mevedel-session-artifacts.el --- Session artifacts and segment persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Owns session paths, immutable artifacts, snapshots, and segment writes.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'mevedel-structs)
  (require 'mevedel-transport)
  (require 'mevedel-utilities))

;; `gptel'
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)
(declare-function gptel--save-state "ext:gptel" nil)
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--markdown-block-map)
(defvar gptel-system-prompt)

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-transcript-path-p "mevedel-agent-persistence" (path save-path))
(autoload 'mevedel-agent-persistence-transcript-path-p
  "mevedel-agent-persistence")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-sidecar-dirty "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-transcript-relative-path "mevedel-agents" (cl-x))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-acknowledge-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-incarnation-changed-p "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-native-root "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-observe-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-prepare-incarnation-acknowledgement "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-probe "mevedel-execution-target" (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-readiness "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-refresh-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-remote-p "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-acknowledge-incarnation
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-incarnation-changed-p
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-root "mevedel-execution-target")
(autoload 'mevedel-execution-target-observe-incarnation
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-prepare-incarnation-acknowledgement
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-probe "mevedel-execution-target")
(autoload 'mevedel-execution-target-readiness "mevedel-execution-target")
(autoload 'mevedel-execution-target-refresh-incarnation
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-permissions'
(declare-function mevedel-permission-invalidate-target-grants "mevedel-permissions" (session))
(autoload 'mevedel-permission-invalidate-target-grants "mevedel-permissions")

;; `mevedel-persistence'
(declare-function mevedel--load-instructions-file "mevedel-persistence" (path &optional base-directory confirm quiet workspace directive-records preserve-directives-p))
(declare-function mevedel--reset-instructions-preserving-directives "mevedel-persistence" (workspace directives))
(declare-function mevedel--restore-preserved-directives "mevedel-persistence" (workspace))
(declare-function mevedel--serialize-instructions "mevedel-persistence" (&optional base-directory include-original-content))
(declare-function mevedel--write-instructions-file "mevedel-persistence" (path &optional base-directory write-empty quiet include-original-content))
(autoload 'mevedel--load-instructions-file "mevedel-persistence")
(autoload 'mevedel--reset-instructions-preserving-directives
  "mevedel-persistence")
(autoload 'mevedel--restore-preserved-directives "mevedel-persistence")
(autoload 'mevedel--serialize-instructions "mevedel-persistence")
(autoload 'mevedel--write-instructions-file "mevedel-persistence")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-serialize
                  "mevedel-session-codec"
                  (session &rest keys))
(declare-function mevedel-session-codec-write "mevedel-session-codec" (path plist))
(autoload 'mevedel-session-codec-portable-authority-p
  "mevedel-session-codec")
(autoload 'mevedel-session-codec-read "mevedel-session-codec")
(autoload 'mevedel-session-codec-serialize "mevedel-session-codec")
(autoload 'mevedel-session-codec-write "mevedel-session-codec")

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-observe "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-register-root-buffer "mevedel-session-control-transfer" (session buffer))
(autoload 'mevedel-session-control-transfer-observe
  "mevedel-session-control-transfer")
(autoload 'mevedel-session-control-transfer-register-root-buffer
  "mevedel-session-control-transfer")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-disclose "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-owned-p "mevedel-session-durability" (session))
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--transaction-clock)
(autoload 'mevedel-session-durability-disclose "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-owned-p
  "mevedel-session-durability")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-allocate-session-id "mevedel-session-persistence" (name sessions-dir))
(declare-function mevedel-session-persistence-authoritative-buffer "mevedel-session-persistence" (buffer))
(declare-function mevedel-session-persistence-first-user-message "mevedel-session-persistence" (buffer))
(declare-function mevedel-session-persistence-flush-diagnostic-logs "mevedel-session-persistence" (session))
(declare-function mevedel-session-persistence-lock-acquire "mevedel-session-persistence" (session-dir buffer-name &optional session))
(declare-function mevedel-session-persistence-notify-session-event "mevedel-session-persistence" (session event &rest args))
(declare-function mevedel-session-persistence-root-buffer-for-session "mevedel-session-persistence" (session &optional buffer))
(declare-function mevedel-session-persistence-root-data-buffer-p "mevedel-session-persistence" (buffer))
(declare-function mevedel-session-persistence-shallow-ensure-files "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-update-transcript-entry "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence-write-current-buffer-atomically "mevedel-session-persistence" (path))
(defvar mevedel-file-history-max-snapshot-bytes)
(defvar mevedel-sessions-directory)
(autoload 'mevedel-session-persistence-allocate-session-id
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-authoritative-buffer
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-first-user-message
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-flush-diagnostic-logs
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-lock-acquire
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-notify-session-event
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-root-buffer-for-session
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-root-data-buffer-p
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-shallow-ensure-files
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-update-transcript-entry
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-write-current-buffer-atomically
  "mevedel-session-persistence")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-committed-p "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-logical-path-p "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-prune-committed "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir))
(declare-function mevedel-session-publication-uncommitted-artifact "mevedel-session-publication" (session logical))
(autoload 'mevedel-session-publication-committed-p
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-logical-path-p
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-prune-committed
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-publish "mevedel-session-publication")
(autoload 'mevedel-session-publication-read "mevedel-session-publication")
(autoload 'mevedel-session-publication-uncommitted-artifact
  "mevedel-session-publication")

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery-refresh "mevedel-session-recovery" (session))
(defvar mevedel-session-recovery--mutation-cache)
(autoload 'mevedel-session-recovery-refresh "mevedel-session-recovery")

;; `mevedel-structs'
(declare-function mevedel-request-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-control-transfer "mevedel-structs" (cl-x))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x))
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x))
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x))
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-record "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry" (session event &rest props))

;; `mevedel-transcript'
(declare-function mevedel-transcript--user-prompt-start
                  "mevedel-transcript" (pos next prop &optional state))
(declare-function mevedel-transcript-normalize-properties
                  "mevedel-transcript" ())
(declare-function mevedel-transcript-prompt-scan-state
                  "mevedel-transcript" ())
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))
(autoload 'mevedel-transcript--user-prompt-start "mevedel-transcript")
(autoload 'mevedel-transcript-normalize-properties "mevedel-transcript")
(autoload 'mevedel-transcript-prompt-scan-state "mevedel-transcript")
(autoload 'mevedel-transcript-segments "mevedel-transcript")

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record "mevedel-transcript-audit" (record))
(declare-function mevedel-transcript-audit-spans "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-buffer-directive-ranges "mevedel-transcript-audit" (&optional allow-open))
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")
(autoload 'mevedel-transcript-audit-spans "mevedel-transcript-audit")
(autoload 'mevedel-transcript-buffer-directive-ranges
  "mevedel-transcript-audit")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

;; `mevedel-transport'
(declare-function mevedel-transport--call-with-exclusive-connection
                  "mevedel-transport" (thunk))
(autoload 'mevedel-transport--call-with-exclusive-connection
  "mevedel-transport")

;; `mevedel-utilities'
(declare-function mevedel--forget-place "mevedel-utilities" nil)
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(declare-function mevedel--write-file-atomically
                  "mevedel-utilities" (path content &optional coding mode))
(declare-function mevedel-version "mevedel-utilities" (&optional here message))
(autoload 'mevedel--forget-place "mevedel-utilities")
(autoload 'mevedel--warn-once "mevedel-utilities")
(autoload 'mevedel--write-file-atomically "mevedel-utilities")
(autoload 'mevedel-version "mevedel-utilities")

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored "mevedel-workspace" (workspace))
(defvar mevedel-workspace-additional-roots)
(autoload 'mevedel-workspace "mevedel-workspace")
(autoload 'mevedel-workspace-ensure-generated-state-ignored
  "mevedel-workspace")

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-ensure "mevedel-workspace-identity" (root))
(autoload 'mevedel-workspace-identity-ensure "mevedel-workspace-identity")

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

(defvar mevedel-session-artifacts-require-agent-commit-p nil
  "Non-nil while an acknowledged agent mutation requires immediate commit.")

;;
;;; Session id and paths

(defun mevedel-session-artifacts-sanitize (name)
  "Return NAME with everything outside `[A-Za-z0-9_-]' replaced with `_'."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (or name "")))

(defun mevedel-session-artifacts--short-uuid ()
  "Return 4 hex chars derived from random + monotonic clock entropy."
  (substring
   (secure-hash 'sha256
                (format "%s-%s-%s"
                        (random most-positive-fixnum)
                        (float-time)
                        (emacs-pid)))
   0 4))

(defun mevedel-session-artifacts-compute-id (name)
  "Compute a fresh session id from NAME.

Format: `<sanitized-name>-<ISO-timestamp>-<short-uuid>'.  ISO timestamp
uses dashes throughout (no colons) so it works on every filesystem and
sorts lexicographically."
  (format "%s-%s-%s"
          (mevedel-session-artifacts-sanitize name)
          (format-time-string "%FT%H-%M")
          (mevedel-session-artifacts--short-uuid)))

(defun mevedel-session-artifacts-sessions-dir (workspace)
  "Return the absolute sessions directory for WORKSPACE.

Resolves `mevedel-sessions-directory' against WORKSPACE's root if the
defcustom is relative; otherwise uses it as-is."
  (let ((dir mevedel-sessions-directory))
    (if (file-name-absolute-p dir)
        (expand-file-name dir)
      (expand-file-name dir (mevedel-workspace-root workspace)))))

(defun mevedel-session-artifacts-segment-path (save-path n)
  "Return the absolute path to segment number N under SAVE-PATH.

Segments are zero-padded to four digits (`segment-0001.chat.org')."
  (file-name-concat save-path
                    (format "segment-%04d.chat.org" n)))

(defvar-local mevedel-session--inspection-buffer-p nil
  "Non-nil in a read-only archived-segment inspection buffer.")

(defun mevedel-session-artifacts-read-artifact
    (session logical &optional committed-only)
  "Return SESSION artifact LOGICAL as literal bytes.

LOGICAL is a normalized session-relative path.  PID-lock sessions read their
fixed logical file directly.  A portable live owner sees its newest locally
staged write unless COMMITTED-ONLY is non-nil.  Otherwise portable sessions
resolve only through SESSION's captured immutable publication and verify its
recorded hash; fixed portable caches are never an authority fallback."
  (unless (mevedel-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (mevedel-session-save-path session)
                       (error "Session has no persistence path"))))
    (if (not (mevedel-session-codec-portable-authority-p session))
        (mevedel-session-artifacts-read-file-raw
         (expand-file-name logical save-path))
      (let ((source
             (and (not committed-only)
                  (mevedel-session-durability-lease-owned-p session)
                  (mevedel-session-publication-uncommitted-artifact
                   session logical)))
            expected)
        (unless source
          (let* ((publication
                  (or (mevedel-session-publication session)
                      (setf
                       (mevedel-session-publication session)
                       (mevedel-session-publication-read
                        save-path))))
                 (entry (and publication
                             (cdr (assoc
                                   logical
                                   (plist-get publication :artifacts))))))
            (unless entry
              (error "Session artifact is not published: %s" logical))
            (setq source (plist-get entry :published)
                  expected (plist-get entry :sha256))))
        (let ((content (mevedel-session-artifacts-read-file-raw source)))
          (when (and expected
                     (not (equal expected (secure-hash 'sha256 content))))
            (error
             "Published session artifact failed verification: %s" logical))
          content)))))

(defun mevedel-session-artifacts-artifact-present-p
    (session logical &optional committed-only)
  "Return non-nil when SESSION has artifact LOGICAL.

PID-lock sessions consult the fixed logical path.  Portable sessions consult
a live owner's newest staged write or captured immutable publication only.
When COMMITTED-ONLY is non-nil, ignore staged portable writes."
  (unless (mevedel-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (mevedel-session-save-path session)
                       (error "Session has no persistence path"))))
    (if (not (mevedel-session-codec-portable-authority-p session))
        (file-exists-p (expand-file-name logical save-path))
      (or
       (and (not committed-only)
            (mevedel-session-durability-lease-owned-p session)
            (mevedel-session-publication-uncommitted-artifact session logical))
       (let ((publication
              (or (mevedel-session-publication session)
                  (setf
                   (mevedel-session-publication session)
                   (mevedel-session-publication-read save-path)))))
         (and (assoc logical (plist-get publication :artifacts)) t))))))

(defun mevedel-session-artifacts-find-artifact-noselect
    (session logical &optional inspection)
  "Visit SESSION artifact LOGICAL without exposing immutable storage.

Insert resolver-verified bytes into a buffer visiting the qualified logical
path.  When INSPECTION is non-nil, make the buffer read-only and ineligible
for saving."
  (let* ((save-path (or (mevedel-session-save-path session)
                        (error "Session has no persistence path")))
         (path (expand-file-name logical save-path))
         ;; Read first so the fixed remote cache can never select the bytes.
         (content (mevedel-session-artifacts-read-artifact session logical))
         (existing
          (and
           (not inspection)
           (cl-find-if
            (lambda (candidate)
              (with-current-buffer candidate
                (and buffer-file-name
                     (equal (expand-file-name buffer-file-name) path)
                     (not (bound-and-true-p
                           mevedel-session--inspection-buffer-p)))))
            (buffer-list))))
         (buffer
          (or existing
              (generate-new-buffer
               (format " *mevedel artifact %s*"
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
        (delay-mode-hooks (set-auto-mode)))
      (mevedel--forget-place)
      (setq-local mevedel-session--inspection-buffer-p inspection)
      (setq-local buffer-offer-save (not inspection))
      (setq buffer-read-only inspection)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))
    buffer))

(defun mevedel-session-artifacts-segments (session live-buffer)
  "Return ordered segment descriptors for SESSION.

LIVE-BUFFER supplies the current segment even when it has not reached disk.
Each descriptor contains `:number', `:path', `:status', `:current-p', and
`:preview'.  Missing and unreadable archived segments remain in the result."
  (let* ((save-path (mevedel-session-save-path session))
         (current (or (mevedel-session-current-segment session) 1))
         (index (mevedel-session-prompt-index session)))
    (cl-loop
     for number from 1 to current
     for path = (and save-path
                     (mevedel-session-artifacts-segment-path
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
       ((mevedel-session-codec-portable-authority-p session)
        (if (mevedel-session-artifacts-artifact-present-p session logical)
            'readable
          'missing))
       ((not (file-exists-p path)) 'missing)
       ((and (file-regular-p path) (file-readable-p path)) 'readable)
       (t 'unreadable))
      :current-p current-p
      :preview
      (cl-loop for prompt in (reverse prompts)
               for preview = (plist-get prompt :preview)
               when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
               return preview)))))

(defun mevedel-session-artifacts-read-segment (session number)
  "Load SESSION segment NUMBER into a read-only inspection buffer.

The returned buffer restores transcript properties but is never authoritative
session state.  Signal a `user-error' naming the exact path when the segment
cannot be read."
  (let* ((save-path (mevedel-session-save-path session))
         (current (or (mevedel-session-current-segment session) 1))
         (logical (format "segment-%04d.chat.org" number))
         (path (and save-path
                    (mevedel-session-artifacts-segment-path
                     save-path number))))
    (unless (and (integerp number) (<= 1 number) (<= number current))
      (user-error "Unknown session segment: %s" number))
    (unless (and path
                 (if (mevedel-session-codec-portable-authority-p session)
                     (mevedel-session-artifacts-artifact-present-p
                      session logical)
                   (file-exists-p path)))
      (user-error "Segment file is missing: %s" (or path "(unsaved session)")))
    (unless (or (mevedel-session-codec-portable-authority-p session)
                (and (file-regular-p path) (file-readable-p path)))
      (user-error "Segment file is unreadable: %s" path))
    (let ((buffer
           (if (mevedel-session-codec-portable-authority-p session)
               (mevedel-session-artifacts-find-artifact-noselect
                session logical t)
             (generate-new-buffer
              (format " *mevedel segment %d*" number)))))
      (condition-case err
          (with-current-buffer buffer
            (setq-local default-directory
                        (or (mevedel-session-working-directory session)
                            save-path))
            (unless (mevedel-session-codec-portable-authority-p session)
              (insert-file-contents path)
              (delay-mode-hooks (org-mode)))
            (setq-local mevedel-session--inspection-buffer-p t)
            (mevedel-transcript-restore-properties)
            (unless (mevedel-session-codec-portable-authority-p session)
              (setq buffer-file-name nil))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            buffer)
        (error
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (user-error "Could not read segment %s: %s"
                     path (error-message-string err)))))))

(defun mevedel-session-artifacts-sidecar-path (save-path)
  "Return the absolute path to the session sidecar under SAVE-PATH."
  (file-name-concat save-path "session.meta.el"))


;;
;;; Prompt index (used by the rewind picker)

(defun mevedel-session-artifacts-content-start (buffer)
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

(defun mevedel-session-artifacts-collect-prompts (buffer)
  "Return indexed user prompt plists for BUFFER.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Turns are numbered 1, 2, ... in document order.  Used at
save time to refresh the live segment's entry in
`mevedel-session-prompt-index'.

Skips the initial org property drawer (via
`mevedel-session-artifacts-content-start') and any content inside
`#+begin_summary' / `#+end_summary' blocks (the compaction summary
has its body stripped of the `gptel' property but is not a user
prompt).  Also skips unpropertized gptel org tool/reasoning block glue."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let* ((content-start
                  (mevedel-session-artifacts-content-start buffer))
                (directive-ranges
                 (mevedel-transcript-buffer-directive-ranges))
                (turn 0)
                ;; Ditto: one forward pass over the block-depth prefix.
                (scan-state (mevedel-transcript-prompt-scan-state))
                (results nil))
            (dolist (seg (mevedel-transcript-segments
                          content-start
                          (point-max)))
              (pcase-let ((`(,type ,seg-start ,seg-end) seg))
                (when (eq type 'user)
                  (when-let* ((prompt-start
                               (mevedel-transcript--user-prompt-start
                                (max seg-start content-start) seg-end nil
                                scan-state)))
                    (let* ((text (buffer-substring-no-properties
                                  prompt-start seg-end))
                           (directive
                            (cl-find-if
                             (lambda (range)
                               (and (>= prompt-start
                                        (plist-get range :body-start))
                                    (< prompt-start
                                       (plist-get range :body-end))))
                             directive-ranges)))
                      (when (string-match "[^[:space:]].*$" text)
                        (cl-incf turn)
                        (push (append
                               (list :turn turn
                                     :pos (if directive
                                              (plist-get directive :start)
                                            prompt-start)
                                     :preview
                                     (truncate-string-to-width
                                      (match-string 0 text)
                                      80 nil nil "..."))
                               (and directive
                                    (list
                                     :kind 'directive
                                     :directive-id
                                     (plist-get directive :directive-id)
                                     :action
                                     (plist-get directive :action)
                                     :outcome
                                     (plist-get directive :outcome)
                                     :activity-kind
                                     (plist-get directive :activity-kind)
                                     :sequence
                                     (plist-get directive :sequence)
                                     :reserved-turn
                                     (plist-get directive :turn))))
                              results)))))))
            (nreverse results)))))))

(defun mevedel-session-artifacts--prompt-count-in-text (text)
  "Return the number of user prompt markers detected in TEXT."
  (if (string-empty-p (or text ""))
    0
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (insert text)
      (length (mevedel-session-artifacts-collect-prompts
               (current-buffer))))))

(defun mevedel-session-artifacts--segment-tail-prompt-count ()
  "Return the copied-tail prompt count recorded on the current segment."
  (if (derived-mode-p 'org-mode)
      (max 0 (string-to-number
              (or (org-entry-get (point-min)
                                 "MEVEDEL_SEGMENT_TAIL_PROMPTS")
                  "0")))
    0))

(defun mevedel-session-artifacts--new-fork-point-id (session)
  "Return a fresh stable fork-point identity for SESSION."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (mevedel-session-session-id session)
                  (current-time)
                  (random most-positive-fixnum)
                  (emacs-pid))))
   0 32))

(defvar-local mevedel-session-artifacts--fork-point-spans-cache nil
  "Cached transcript fork-point spans keyed by modification tick.")

(defun mevedel-session-artifacts-fork-point-spans (buffer)
  "Return durable fork-point records and source spans from BUFFER."
  (with-current-buffer buffer
    (let ((tick (buffer-chars-modified-tick)))
      (if (eq tick
              (car-safe
               mevedel-session-artifacts--fork-point-spans-cache))
          (cdr mevedel-session-artifacts--fork-point-spans-cache)
        (let ((spans
               (mapcar
                (lambda (span)
                  (append
                   (copy-sequence (plist-get span :record))
                   (list
                    :record-start
                    (+ (point-min) (plist-get span :start))
                    :transcript-cutoff
                    (+ (point-min) (plist-get span :end)))))
                (mevedel-transcript-audit-spans
                 (buffer-string) 'fork-point))))
          (setq mevedel-session-artifacts--fork-point-spans-cache
                (cons tick spans))
          spans)))))

(defun mevedel-session-artifacts-fork-point-at-source
    (buffer source-start source-end)
  "Return the stable fork point inside BUFFER source bounds.

SOURCE-START and SOURCE-END must bound one rendered assistant turn."
  (cl-find-if
   (lambda (fork-point)
     (and (<= source-start (plist-get fork-point :record-start))
          (<= (plist-get fork-point :transcript-cutoff) source-end)))
   (mevedel-session-artifacts-fork-point-spans buffer)))

(defun mevedel-session-artifacts--prompt-fork-point
    (prompt next-position fork-points)
  "Return PROMPT's fork point before NEXT-POSITION from FORK-POINTS."
  (cl-find-if
   (lambda (fork-point)
     (let ((start (plist-get fork-point :record-start)))
       (and (> start (plist-get prompt :pos))
            (or (null next-position) (< start next-position)))))
   fork-points))

(defun mevedel-session-artifacts-update-prompt-index (session buffer)
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
  (let* ((current-seg (or (mevedel-session-current-segment session) 1))
         (index       (mevedel-session-prompt-index session))
         (cell        (assoc current-seg index))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (raw-all     (mevedel-session-artifacts-collect-prompts buffer))
         (tail-count  (with-current-buffer buffer
                        (mevedel-session-artifacts--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (fork-points
          (mevedel-session-artifacts-fork-point-spans buffer))
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
                                 (when (and (eq (plist-get copy :kind)
                                                'directive)
                                            (/= (plist-get copy
                                                           :reserved-turn)
                                                (plist-get copy :cum-turn)))
                                   (error
                                    "Directive turn %d disagrees with prompt index turn %d"
                                    (plist-get copy :reserved-turn)
                                    (plist-get copy :cum-turn)))
                                 (when-let* ((fork-point
                                              (mevedel-session-artifacts--prompt-fork-point
                                               p next-position fork-points)))
                                   (dolist (key '(:fork-point-id
                                                  :transcript-cutoff))
                                     (plist-put copy key
                                                (plist-get fork-point key))))
                                 copy))))
    (if cell
        (setcdr cell with-cum)
      (setf (mevedel-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))))

(defun mevedel-session-artifacts--ensure-latest-fork-point
    (session buffer)
  "Attach a durable fork-point marker to SESSION's latest response in BUFFER."
  (let* ((segment (or (mevedel-session-current-segment session) 1))
         (prompt (car (last (cdr (assoc
                                  segment
                                  (mevedel-session-prompt-index session)))))))
    (when (and prompt (not (plist-get prompt :fork-point-id)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (plist-get prompt :pos))
          (when (text-property-search-forward 'gptel 'response t)
            (goto-char (point-max))
            (insert
             (mevedel--format-hook-audit-record
              (list :type 'fork-point
                    :fork-point-id
                    (mevedel-session-artifacts--new-fork-point-id session)
                    :segment segment
                    :turn (plist-get prompt :turn)
                    :file-turn (plist-get prompt :file-turn)
                    :cum-turn (plist-get prompt :cum-turn)
                    :captured-file-turn (plist-get prompt :cum-turn))))
            t))))))

(defun mevedel-session-artifacts--newer-prompt-p (candidate incumbent)
  "Return non-nil when CANDIDATE is newer than INCUMBENT."
  (or (null incumbent)
      (> (plist-get candidate :cum-turn)
         (plist-get incumbent :cum-turn))))

(defun mevedel-session-artifacts--latest-user-message-from-index (index)
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
                  (when (mevedel-session-artifacts--newer-prompt-p
                         candidate best)
                    (setq best candidate)))))))))
    (plist-get best :preview)))


;;
;;; Lazy materialization

(defun mevedel-session-artifacts-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id,
create the session directory tree (session dir + `agents/' +
`file-history/'), acquire its explicit mutation authority (a portable
lease for project sessions or a `.lock' for file-workspace sessions), set
BUFFER's variable `buffer-file-name' to the first segment file, and save the
buffer.

Does NOT write the sidecar -- the caller (always
`mevedel-session-artifacts-save') is expected to do that once it
has updated the prompt-index and snapshot maps.  Keeping the write
in one place avoids double-writing the sidecar on first save.

Idempotent -- if SESSION already has a `save-path', repairs BUFFER's
variable `buffer-file-name' so it visits the current segment before any save.
Returns SESSION's `save-path' (allocated or existing)."
  (let* ((existing-save-path (mevedel-session-save-path session))
           (save-path
            (or existing-save-path
                (let* ((_disclosure
                        (when (mevedel-execution-target-remote-p
                               (mevedel-session-execution-target session))
                          (mevedel-session-durability-disclose session)))
                       (_workspace-identity
                        (progn
                          (mevedel-workspace-identity-ensure
                           (mevedel-workspace-root
                            (mevedel-session-workspace session)))))
                       (sessions-dir
                        (mevedel-session-artifacts-sessions-dir
                         (mevedel-session-workspace session)))
                       (session-id
                        (mevedel-session-persistence-allocate-session-id
                         (mevedel-session-name session) sessions-dir))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat sessions-dir session-id)))
                       (now (format-time-string "%FT%H-%M-%S")))
                  (make-directory new-save-path t)
                  (make-directory (file-name-concat new-save-path "agents") t)
                  (make-directory (file-name-concat new-save-path "file-history") t)
                  (mevedel-session-persistence-lock-acquire
                   new-save-path (buffer-name buffer) session)
                  (setf (mevedel-session-session-id session)      session-id)
                  (setf (mevedel-session-save-path session)       new-save-path)
                  (setf (mevedel-session-created-at session)      now)
                  (setf (mevedel-session-updated-at session)      now)
                  (setf (mevedel-session-current-segment session) 1)
                  (mevedel-session-persistence-flush-diagnostic-logs session)
                  (mevedel-workspace-ensure-generated-state-ignored
                   (mevedel-session-workspace session))
                  (setf (mevedel-session-durable-tree-ensured session) t)
                  new-save-path)))
           (segment-number (or (mevedel-session-current-segment session) 1))
           (segment-path (mevedel-session-artifacts-segment-path
                          save-path segment-number)))
      ;; The tree and the workspace ignore entries are established once per
      ;; process.  Re-establishing them on every save is several target round
      ;; trips plus a re-parse of the ignore file for a settled answer.
      (unless (mevedel-session-durable-tree-ensured session)
        (make-directory save-path t)
        (make-directory (file-name-concat save-path "agents") t)
        (make-directory (file-name-concat save-path "file-history") t)
        (mevedel-workspace-ensure-generated-state-ignored
         (mevedel-session-workspace session))
        (setf (mevedel-session-durable-tree-ensured session) t))
      (when (and (buffer-live-p buffer)
                 (eq buffer (mevedel-session-root-buffer session)))
        (mevedel-session-control-transfer-register-root-buffer session buffer))
      (with-current-buffer buffer
        (unless (and buffer-file-name
                     (equal (expand-file-name buffer-file-name)
                            (expand-file-name segment-path)))
          (setq buffer-file-name segment-path))
        (mevedel-session-artifacts-disown-save-machinery)
        (unless (file-exists-p segment-path)
          (set-buffer-modified-p t)
          (unless (mevedel-session-codec-portable-authority-p session)
            (save-buffer))))
      save-path))

(defvar mevedel-session-artifacts--checking-incarnation nil
  "Non-nil while publishing a replacement target incarnation.")

(defun mevedel-session-artifacts--observe-target-incarnation
    (target sandbox-mode)
  "Refresh TARGET's incarnation observation and return its readiness.

Admission needs the replacement fingerprint, not the whole readiness suite:
environment, capabilities, and sandbox facts are fixed for the life of a
connection, while the fingerprint is one target command.  A first probe or a
reconnect still runs the full probe, because a new connection may be a new
target.  A failed observation falls back to the full probe, which settles a
blocked readiness the caller reports."
  (let ((readiness (mevedel-execution-target-probe target nil sandbox-mode)))
    (if (not (eq 'ready (plist-get readiness :status)))
        readiness
      (condition-case nil
          (progn
            (mevedel-execution-target-observe-incarnation target)
            readiness)
        (error (mevedel-execution-target-probe target t sandbox-mode))))))

(defun mevedel-session-artifacts-check-target-incarnation
    (session buffer)
  "Fence and publish a replacement execution target for SESSION.

BUFFER identifies SESSION's live root data buffer.  The fresh probe runs at
every durable mutation boundary; unchanged targets take no durability I/O."
  (unless mevedel-session-artifacts--checking-incarnation
    (let ((buffer
           (or buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (current-buffer)))))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (boundp 'mevedel--session)
                        (eq mevedel--session session)
                        (not (bound-and-true-p mevedel--agent-invocation)))))
        (when (or (null (mevedel-session-root-buffer session))
                  (eq buffer (mevedel-session-root-buffer session)))
          (mevedel-session-control-transfer-register-root-buffer
           session buffer)))
      (when-let ((target (mevedel-session-execution-target session)))
        (unless (mevedel-execution-target-incarnation-changed-p target)
          (if (mevedel-execution-target-remote-p target)
              (let ((readiness
                     (or (mevedel-session-artifacts--observe-target-incarnation
                          target (mevedel-session-sandbox-mode session))
                         (mevedel-execution-target-readiness target))))
                (unless (eq 'ready (plist-get readiness :status))
                  (user-error "Execution target is not ready: %s"
                              (or (plist-get readiness :error)
                                  (plist-get readiness :reason)
                                  "probe failed"))))
            (mevedel-execution-target-refresh-incarnation target)))
        (when (mevedel-execution-target-incarnation-changed-p target)
          (let ((root-buffer
                 (mevedel-session-persistence-root-buffer-for-session
                  session buffer)))
            (unless (buffer-live-p root-buffer)
              (error "Target replacement requires the live root session buffer"))
            (let ((mevedel-session-artifacts--checking-incarnation t))
              (mevedel-permission-invalidate-target-grants session)
              (mevedel-execution-target-prepare-incarnation-acknowledgement
               target)
              (when (mevedel-session-save-path session)
                (if (mevedel-session-codec-portable-authority-p session)
                    (when (mevedel-session-artifacts-artifact-present-p
                           session "session.meta.el" t)
                      (mevedel-session-artifacts-publish-sidecar-state
                       session root-buffer))
                  (let ((sidecar
                         (mevedel-session-artifacts-sidecar-path
                          (mevedel-session-save-path session))))
                    (when (file-exists-p sidecar)
                      (mevedel-session-codec-write
                       sidecar
                       (mevedel-session-artifacts-build-sidecar
                        session root-buffer))))))
              (mevedel-execution-target-acknowledge-incarnation target))))))))

(defun mevedel-session-artifacts-assert-mutation-authority
    (session &optional buffer)
  "Return non-nil when SESSION may start durable mutation from BUFFER.

Pending critical publication blocks every session.  A portable project session
also materializes its directory when necessary and must own its live lease.
Every execution target is incarnation-fenced before mutation.  Shallow
materialization always uses SESSION's root buffer."
  (let ((buffer
         (or buffer
             (and (boundp 'mevedel--session)
                  (eq mevedel--session session)
                  (current-buffer)))))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (boundp 'mevedel--session)
                      (eq mevedel--session session)
                      (not (bound-and-true-p mevedel--agent-invocation))
                      (not (buffer-live-p
                            (mevedel-session-root-buffer session))))))
      (mevedel-session-control-transfer-register-root-buffer
       session buffer))
    (let ((target (mevedel-session-execution-target session)))
      (when (and target
                 (mevedel-session-codec-portable-authority-p session))
        (mevedel-session-recovery-refresh session)))
    (when (mevedel-session-pending-publication session)
      (user-error "Session has pending publication; retry or abandon it first"))
    (let* ((target (mevedel-session-execution-target session))
           (portable-p
            (and target
                 (mevedel-session-codec-portable-authority-p session))))
      (when (and target portable-p)
        (let* ((root-buffer
                (mevedel-session-persistence-root-buffer-for-session
                 session buffer)))
          (mevedel-session-durability-disclose session)
          ;; Every portable mutation must have a materialized session
          ;; directory before it is admitted.
          (unless (or (mevedel-session-save-path session)
                      (and root-buffer
                           (mevedel-session-persistence-shallow-ensure-files
                            session root-buffer)))
            (user-error "Could not materialize portable session state"))
          (unless (or (mevedel-session-durability-lease-owned-p session)
                      (mevedel-session-persistence-lock-acquire
                       (mevedel-session-save-path session)
                       (buffer-name (or root-buffer buffer))
                       session))
            (user-error "Portable session is leased by another client"))
          (mevedel-session-artifacts-check-target-incarnation
           session buffer)))
      (when (and target (not portable-p))
        (mevedel-session-artifacts-check-target-incarnation
         session (or buffer (current-buffer)))))
    t))

(defun mevedel-session-artifacts-assert-new-mutation-authority (session)
  "Reject a new mutation while SESSION is quiescing for control transfer.

Existing requests and durability settlement continue through the ordinary
authority gate; callers use this narrower guard at admission boundaries."
  ;; Hydrate the durable transfer state before admitting a restarted owner.
  ;; Do not run the owner-side drain/release path here: admission must only
  ;; observe state, and settlement remains responsible for releasing it.
  (let ((state (plist-get (mevedel-session-control-transfer session) :state)))
    (when (memq state '(quiescing released))
      (user-error "Session is quiescing for cooperative control transfer")))
  (when (and (mevedel-session-codec-portable-authority-p session)
             (mevedel-session-save-path session)
             (mevedel-session-session-id session))
    (let ((state
           (plist-get
            (mevedel-session-control-transfer-observe session) :state)))
      (when (memq state '(quiescing released))
        (user-error "Session is quiescing for cooperative control transfer"))))
  (mevedel-session-artifacts-assert-mutation-authority session))


;;
;;; Sidecar build helper

(defun mevedel-session-artifacts--persisted-first-user-message (session)
  "Return SESSION's already persisted first user preview, or nil.

The committed field never changes once a session has a first turn, so the
first successful read is cached on the session rather than re-read and
re-verified on every save."
  (or (mevedel-session-persisted-first-user-message session)
      (when-let* ((save-path (mevedel-session-save-path session)))
        (condition-case nil
            (let ((sidecar
                   (if (mevedel-session-codec-portable-authority-p
                        session)
                       (with-temp-buffer
                         (insert
                          (decode-coding-string
                           (mevedel-session-artifacts-read-artifact
                            session "session.meta.el")
                           'utf-8-unix))
                         (read (current-buffer)))
                     (let ((path
                            (mevedel-session-artifacts-sidecar-path
                             save-path)))
                       (and (file-exists-p path)
                            (mevedel-session-codec-read path))))))
              (setf (mevedel-session-persisted-first-user-message session)
                    (plist-get sidecar :first-user-message)))
          (error nil)))))

(defun mevedel-session-artifacts-build-sidecar (session buffer)
  "Build the sidecar plist for SESSION using BUFFER for ancillary fields."
  (let* ((first-preview
          (or (mevedel-session-artifacts--persisted-first-user-message
               session)
              ;; The buffer's first message is exactly what this sidecar
              ;; is about to persist, and the committed field never
              ;; changes afterwards -- so it is authoritative now.
              ;; Without this, an owner whose committed sidecar predates
              ;; its first turn re-reads the whole artifact over TRAMP
              ;; on every save, twice.
              (when-let* ((preview
                           (mevedel-session-persistence-first-user-message
                            buffer)))
                (setf (mevedel-session-persisted-first-user-message
                       session)
                      preview))))
         (latest-preview
          (or (mevedel-session-artifacts--latest-user-message-from-index
               (mevedel-session-prompt-index session))
              first-preview))
         (roots (when (buffer-live-p buffer)
                  (buffer-local-value 'mevedel-workspace-additional-roots
                                      buffer))))
    (mevedel-session-codec-serialize
     session
     :first-user-message  first-preview
     :latest-user-message latest-preview
     :additional-roots   roots)))


;;
;;; Fast Org property writes

(defun mevedel-session-artifacts--top-level-pom-p (pom)
  "Return non-nil when POM points at the top-level property drawer."
  (cond
   ((integerp pom) (= pom (point-min)))
   ((markerp pom) (= (marker-position pom) (point-min)))
   (t nil)))

(defun mevedel-session-artifacts--property-drawer-region ()
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

(defun mevedel-session-artifacts--ensure-property-drawer ()
  "Return the initial Org property drawer region, creating it if needed."
  (or (mevedel-session-artifacts--property-drawer-region)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert ":PROPERTIES:\n:END:\n"))
          (mevedel-session-artifacts--property-drawer-region)))))

(defun mevedel-session-artifacts-property-delete-direct (property)
  "Delete PROPERTY from the initial Org property drawer without Org parsing."
  (when-let* ((region (mevedel-session-artifacts--property-drawer-region)))
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

(defun mevedel-session-artifacts--property-put-direct (property value)
  "Set PROPERTY to VALUE in the initial Org property drawer without Org parsing."
  (mevedel-session-artifacts-property-delete-direct property)
  (when-let* ((region (mevedel-session-artifacts--ensure-property-drawer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (goto-char (car region))
          (forward-line 1)
          (insert (format ":%s: %s\n" property value)))))))

(defun mevedel-session-artifacts-strip-gptel-config-properties ()
  "Delete gptel's request-config Org properties from the current buffer.

Strips every top-level `GPTEL_*' property except `GPTEL_BOUNDS', which
carries the transcript span classification mevedel itself consumes.
Mevedel restores model, effort, preset, system prompt, and tools from
the session sidecar and agent registry, never from Org properties.
Leaving gptel's copies in the drawer is worse than redundant: gptel's
send advice (`gptel-org--send-with-props') prefers them over the live
buffer-locals, so a stale drawer silently overrides a model the user
changed mid-session.  Matching by prefix instead of by name keeps this
correct when gptel grows new config properties."
  (when-let* ((region (mevedel-session-artifacts--property-drawer-region)))
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
        (mevedel-session-artifacts-property-delete-direct name)))))

(defun mevedel-session-artifacts--with-fast-property-writes (fn)
  "Call FN while routing top-level Org property writes through text helpers."
  (let ((orig-put (symbol-function 'org-entry-put))
        (orig-delete (symbol-function 'org-entry-delete)))
    (cl-letf (((symbol-function 'org-entry-put)
               (lambda (pom property value)
                 (if (mevedel-session-artifacts--top-level-pom-p pom)
                     (mevedel-session-artifacts--property-put-direct
                      property value)
                   (funcall orig-put pom property value))))
              ((symbol-function 'org-entry-delete)
               (lambda (pom property)
                 (if (mevedel-session-artifacts--top-level-pom-p pom)
                     (mevedel-session-artifacts-property-delete-direct
                      property)
                   (funcall orig-delete pom property)))))
      (funcall fn))))

(defun mevedel-session-artifacts-stabilize-gptel-bounds ()
  "Rewrite `GPTEL_BOUNDS' until Org property drawer offsets settle.

`gptel-org--save-state' persists absolute buffer positions.  Updating
the Org property drawer can itself move every marked transcript region,
so a single write can save positions that were correct for the previous
drawer size.  Recompute after each write and stop once the serialized
bounds no longer change."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'gptel--get-buffer-bounds))
    (save-excursion
      (save-restriction
        (widen)
        (let ((last nil)
              (done nil)
              (attempts 0))
          (while (and (not done) (< attempts 8))
            (setq attempts (1+ attempts))
            (mevedel-transcript-normalize-properties)
            (let ((serialized
                   (when-let* ((bounds (gptel--get-buffer-bounds)))
                     (prin1-to-string bounds))))
              (cond
               ((null serialized)
                (mevedel-session-artifacts-property-delete-direct
                 "GPTEL_BOUNDS")
                (setq done t))
               ((equal serialized last)
                (setq done t))
               (t
                (setq last serialized)
                (mevedel-session-artifacts--property-put-direct
                 "GPTEL_BOUNDS" serialized))))))))))

(defun mevedel-session-artifacts--save-gptel-state-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, persisting bounds but no request config.

This is an around-advice for `gptel--save-state'.  For non-mevedel
buffers it delegates unchanged.  For retained agents and mevedel chat
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
        (mevedel-org-buffer-p
         (and (bound-and-true-p mevedel--session)
              (derived-mode-p 'org-mode))))
    (unwind-protect
        (if mevedel-org-buffer-p
            (mevedel-session-artifacts--with-fast-property-writes
             (lambda ()
               (let ((gptel-system-prompt nil))
                 (apply orig-fun args))))
          (apply orig-fun args))
      (when mevedel-org-buffer-p
        (mevedel-session-artifacts-strip-gptel-config-properties)
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (let ((delta (- (buffer-size) size-before)))
          (when (/= delta 0)
            (mevedel-session-persistence-notify-session-event
             mevedel--session 'rebase-data-sources delta)))))))

(defun mevedel-session-artifacts-install-gptel-save-state-advice ()
  "Install mevedel's gptel state persistence advice."
  (unless (advice-member-p
           #'mevedel-session-artifacts--save-gptel-state-around
           'gptel--save-state)
    (advice-add 'gptel--save-state :around
                #'mevedel-session-artifacts--save-gptel-state-around)))


;;
;;; Instruction snapshots

(defun mevedel-session-artifacts-instructions-dir (save-path)
  "Return the instruction snapshot directory under SAVE-PATH."
  (file-name-concat save-path "instructions"))

(defun mevedel-session-artifacts-instructions-current-path (save-path)
  "Return the current instruction snapshot path under SAVE-PATH."
  (file-name-concat
   (mevedel-session-artifacts-instructions-dir save-path)
   "current.el"))

(defun mevedel-session-artifacts-instructions-turn-path (save-path turn)
  "Return the instruction snapshot path for TURN under SAVE-PATH."
  (file-name-concat
   (mevedel-session-artifacts-instructions-dir save-path)
   (format "turn-%06d.el" turn)))

(defun mevedel-session-artifacts-save-instructions
    (session buffer &optional current-only)
  "Persist current instruction state for SESSION and BUFFER.

Writes `instructions/current.el' and, unless CURRENT-ONLY is non-nil, a
turn-specific snapshot used by rewind/fork when the session has a turn count."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (let ((dir (mevedel-session-artifacts-instructions-dir save-path))
          (workspace-root (mevedel-workspace-root
                           (mevedel-session-workspace session)))
          (turn (mevedel-session-turn-count session)))
      (make-directory dir t)
      (with-current-buffer buffer
        (mevedel--write-instructions-file
         (mevedel-session-artifacts-instructions-current-path save-path)
         workspace-root t t t)
        (when (and (not current-only) (integerp turn))
          (mevedel--write-instructions-file
           (mevedel-session-artifacts-instructions-turn-path save-path turn)
           workspace-root t t nil))))))

(defun mevedel-session-artifacts-load-instructions
    (session buffer &optional turn directive-records preserve-directives-p)
  "Restore SESSION instruction snapshot into BUFFER's workspace.

When TURN is non-nil, restore the turn-specific snapshot; otherwise restore
`instructions/current.el'.  DIRECTIVE-RECORDS preserves current authored
directive identity and activity while replacing historical presentations.
PRESERVE-DIRECTIVES-P applies that behavior even when the record list is empty.
Missing snapshots clear presentations only in preservation mode."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (let* ((path (if turn
                     (mevedel-session-artifacts-instructions-turn-path
                      save-path turn)
                   (mevedel-session-artifacts-instructions-current-path
                    save-path)))
           (logical (file-relative-name path save-path))
           (portable-p
            (mevedel-session-codec-portable-authority-p session))
           (present-p
            (if portable-p
                (mevedel-session-artifacts-artifact-present-p
                 session logical)
              (file-exists-p path))))
      (if present-p
        (condition-case err
            (let ((source path)
                  inspection
                  temporary)
              (unwind-protect
                  (progn
                    (when portable-p
                      (setq
                       inspection
                       (mevedel-session-artifacts-find-artifact-noselect
                        session logical t)
                       temporary
                       (make-temp-file "mevedel-instructions-" nil ".el")
                       source temporary)
                      (with-current-buffer inspection
                        (let ((coding-system-for-write
                               (or buffer-file-coding-system 'utf-8-unix)))
                          (write-region
                           (point-min) (point-max) temporary nil 'silent))))
                    (with-current-buffer buffer
                      (mevedel--load-instructions-file
                       source
                       (mevedel-workspace-root
                        (mevedel-session-workspace session))
                       nil t
                       (mevedel-session-workspace session)
                       directive-records preserve-directives-p)))
                (when (buffer-live-p inspection)
                  (kill-buffer inspection))
                (when (and temporary (file-exists-p temporary))
                  (delete-file temporary))))
          (error
           (display-warning
            'mevedel
            (format "Could not restore instruction snapshot %s: %s"
                    path (error-message-string err))
            :warning)
           nil))
        (when (or preserve-directives-p directive-records)
          (mevedel--reset-instructions-preserving-directives
           (mevedel-session-workspace session) directive-records)
          (mevedel--restore-preserved-directives
           (mevedel-session-workspace session))
          (list :restored 0 :lost 0 :total 0))))))


;;
;;; Per-turn save

(defvar mevedel-session-artifacts--critical-artifacts nil
  "Dynamically collected target artifacts for one portable publication.")

(defvar mevedel-session-artifacts--collecting-critical-artifacts-p nil
  "Non-nil while portable save captures durability-critical file writes.")

(defun mevedel-session-artifacts-printed-value (value)
  "Return VALUE in the package's durable Lisp representation."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t))
      (prin1 value (current-buffer)))
    (buffer-string)))

(defun mevedel-session-artifacts-publish-text
    (session path content &optional coding)
  "Publish SESSION's durability-critical CONTENT atomically at PATH.

Portable project writes enter the session publication queue.  File-workspace
writes retain the existing same-filesystem temporary-file and rename
behavior.  Return the publication outcome, or PATH after a direct write."
  (if (mevedel-session-codec-portable-authority-p session)
      (progn
        (mevedel-session-artifacts-assert-mutation-authority session)
        (mevedel-session-publication-publish
         session (list (list :path path :content content :coding coding))))
    (progn
      (make-directory (file-name-directory (expand-file-name path)) t)
      (with-temp-buffer
        (setq buffer-file-coding-system (or coding 'utf-8-unix))
        (insert content)
        (mevedel-session-persistence-write-current-buffer-atomically path))
      path)))

(defun mevedel-session-artifacts--sidecar-publication-artifact
    (session root-buffer)
  "Return SESSION's freshly built sidecar marker artifact.

SESSION must use portable authority and be materialized.  ROOT-BUFFER must be
its live root data buffer, and the current immutable publication must contain
the sidecar.  Mutation authority is checked before the artifact is built."
  ;; An explicit root argument is the registration seam for callers that
  ;; materialize a session without going through view initialization.
  (when (and (buffer-live-p root-buffer)
             (with-current-buffer root-buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (not (bound-and-true-p mevedel--agent-invocation)))))
    (mevedel-session-control-transfer-register-root-buffer
     session root-buffer))
  (unless (and (buffer-live-p root-buffer)
               (mevedel-session-persistence-root-data-buffer-p root-buffer)
               (eq session
                   (buffer-local-value 'mevedel--session root-buffer)))
    (error "Session state publication requires the live root session buffer"))
  (unless (and (mevedel-session-save-path session)
               (mevedel-session-execution-target session)
               (mevedel-session-codec-portable-authority-p session))
    (error "Session state publication requires a portable materialized session"))
  (unless (mevedel-session-artifacts-artifact-present-p
           session "session.meta.el" t)
    (error "Portable session sidecar is not published"))
  (mevedel-session-artifacts-assert-mutation-authority
   session root-buffer)
  (list
   :path
   (mevedel-session-artifacts-sidecar-path
    (mevedel-session-save-path session))
   :content
   (mevedel-session-artifacts-printed-value
    (mevedel-session-artifacts-build-sidecar session root-buffer))
   :commit-marker t))

(defun mevedel-session-artifacts-publish-sidecar-state
    (session root-buffer)
  "Publish SESSION's freshly built sidecar as one strict commit.

ROOT-BUFFER must be SESSION's live root data buffer.  Publication and
authority failures are propagated to the caller."
  (let ((result
         (mevedel-session-publication-publish
          session
          (list
           (mevedel-session-artifacts--sidecar-publication-artifact
            session root-buffer)))))
    (when (eq result 'queued)
      (user-error "Session state publication was queued before its commit"))
    result))

(defun mevedel-session-artifacts-publish-transcript-state
    (session root-buffer transcript-path content &optional coding)
  "Publish portable SESSION transcript CONTENT and its sidecar atomically.

ROOT-BUFFER must be SESSION's live root data buffer.  TRANSCRIPT-PATH must be
a logical artifact below SESSION's save path.  The current authoritative
sidecar is rebuilt from SESSION and ROOT-BUFFER and commits the two-artifact
batch."
  (unless (stringp content)
    (error "Transcript publication requires string content"))
  (let* ((sidecar-artifact
          (mevedel-session-artifacts--sidecar-publication-artifact
           session root-buffer))
         (save-path (file-name-as-directory
                     (mevedel-session-save-path session)))
         (path (expand-file-name transcript-path))
         (logical (file-relative-name path save-path)))
    (unless (and (file-in-directory-p path save-path)
                 (mevedel-session-publication-logical-path-p logical))
      (error "Invalid session transcript path: %s" transcript-path))
    (mevedel-session-publication-publish
     session
     (list
      (list :path path :content content :coding coding)
      sidecar-artifact))))

(defun mevedel-session-artifacts-publish-agent-terminal-state (invocation)
  "Publish INVOCATION's portable transcript and final session sidecar together.

The authoritative sidecar must already exist.  Shallow first-turn agent state
continues to wait for the root turn's completed-turn publication boundary."
  (unless (mevedel-agent-invocation-p invocation)
    (error "Invalid agent invocation"))
  (let* ((session (mevedel-agent-invocation-parent-session invocation))
         (parent (mevedel-agent-invocation-parent-data-buffer invocation))
         (buffer (mevedel-agent-invocation-buffer invocation))
         (save-path (and session (mevedel-session-save-path session)))
         (relative
          (mevedel-agent-invocation-transcript-relative-path invocation))
         (target (and session (mevedel-session-execution-target session)))
         (transcript (and save-path relative
                          (expand-file-name relative save-path)))
         (sidecar (and save-path
                       (mevedel-session-artifacts-sidecar-path save-path))))
    (unless (and session target save-path relative transcript sidecar
                 (mevedel-session-codec-portable-authority-p session)
                 (buffer-live-p parent)
                 (buffer-live-p buffer)
                 (mevedel-agent-persistence-transcript-path-p
                  relative save-path)
                 (mevedel-session-artifacts-artifact-present-p
                  session "session.meta.el"))
      (error "Portable terminal agent state is not materialized"))
    (with-current-buffer buffer
      (unless (and buffer-file-name
                   (equal (expand-file-name buffer-file-name) transcript))
        (error "Agent transcript path does not match its invocation"))
      (mevedel-session-artifacts-assert-mutation-authority session)
      (let ((modified-p (buffer-modified-p)))
        (when modified-p
          (when (bound-and-true-p gptel-mode)
            (gptel--save-state))
          (let ((before-save-hook
                 (remq 'gptel--save-state before-save-hook)))
            (run-hooks 'before-save-hook)))
        (mevedel-session-persistence-update-transcript-entry
         session (mevedel-agent-invocation-agent-id invocation)
         (list :updated-at (format-time-string "%FT%H-%M-%S")))
        (mevedel-session-publication-publish
         session
         (list
          (list :path transcript
                :content (buffer-substring-no-properties
                          (point-min) (point-max))
                :coding (or buffer-file-coding-system 'utf-8-unix))
          ;; The sidecar commits transcript metadata and final registry state.
          (list :path sidecar
                :content
                (mevedel-session-artifacts-printed-value
                 (mevedel-session-artifacts-build-sidecar
                  session parent))
                :commit-marker t))
         t)
        (when modified-p
          (condition-case err
              (progn
                (set-visited-file-modtime)
                (set-buffer-modified-p nil)
                (run-hooks 'after-save-hook))
            (error
             (mevedel--warn-once
              'artifacts-agent-post-save
              "Agent transcript post-save cleanup failed: %s"
              (error-message-string err)))))
        (setf (mevedel-agent-invocation-sidecar-dirty invocation) nil)
        t))))

(defun mevedel-session-artifacts--instruction-artifacts (session buffer)
  "Return SESSION instruction snapshot artifacts derived from BUFFER."
  (let* ((save-path (mevedel-session-save-path session))
         (workspace-root
          (mevedel-workspace-root (mevedel-session-workspace session)))
         (turn (mevedel-session-turn-count session)))
    (with-current-buffer buffer
      (append
       (list
        (list :path
              (mevedel-session-artifacts-instructions-current-path
               save-path)
              :content
              (mevedel-session-artifacts-printed-value
               (mevedel--serialize-instructions workspace-root t))))
       (when (integerp turn)
         (list
          (list :path
                (mevedel-session-artifacts-instructions-turn-path
                 save-path turn)
                :content
                (mevedel-session-artifacts-printed-value
                 (mevedel--serialize-instructions workspace-root nil)))))))))

(defun mevedel-session-artifacts--sidecar-artifact (session buffer)
  "Return SESSION's sidecar publication artifact derived from BUFFER.

The sidecar is the commit marker, so callers place it last in a batch."
  (list :path
        (mevedel-session-artifacts-sidecar-path
         (mevedel-session-save-path session))
        :content
        (mevedel-session-artifacts-printed-value
         (mevedel-session-artifacts-build-sidecar session buffer))
        :commit-marker t))

(defun mevedel-session-artifacts--remote-save
    (session buffer settled &optional force)
  "Publish portable project SESSION's durability-critical state from BUFFER.
When SETTLED is non-nil, update the prompt index and latest fork point.
When FORCE is non-nil, publish even when the state is already committed.
The public save entry point has already completed the mutation gate before
calling this serializer."
  (mevedel-session-artifacts-ensure-files session buffer)
  (when settled
    (mevedel-session-artifacts-update-prompt-index session buffer)
    (mevedel-session-artifacts--ensure-latest-fork-point session buffer))
  (let ((segment-artifact nil)
        (mevedel-session-artifacts--critical-artifacts nil)
        (mevedel-session-artifacts--collecting-critical-artifacts-p t))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (run-hooks 'before-save-hook)
        (setq segment-artifact
              (list
               :path buffer-file-name
               :content (buffer-substring-no-properties
                         (point-min) (point-max))
               :coding buffer-file-coding-system))))
    (mevedel-session-artifacts-update-prompt-index session buffer)
    (when (and (boundp 'mevedel--current-request)
               mevedel--current-request)
      (mevedel-session-artifacts-snapshot-modified
       session
       (or (mevedel-session-turn-count session) 0)
       (mevedel-request-file-snapshots mevedel--current-request)))
    ;; `updated-at' is stamped only once this save is known to carry a
    ;; change, because stamping it first would make the sidecar differ from
    ;; the committed one on every call and defeat the comparison below.
    (let* ((leading
            (append
             (and segment-artifact (list segment-artifact))
             (nreverse mevedel-session-artifacts--critical-artifacts)
             (mevedel-session-artifacts--instruction-artifacts
              session buffer)))
           (artifacts
            (append leading
                    (list (mevedel-session-artifacts--sidecar-artifact
                           session buffer))))
           (unchanged
            (and (not force)
                 (mevedel-session-publication-committed-p
                  session artifacts))))
      (if unchanged
          ;; Nothing durable differs from the committed snapshot, so this
          ;; save owes the target no transaction.  A segment artifact that
          ;; compared equal proves the fixed cache already holds these
          ;; bytes, so the buffer is no longer dirty against the target.
          (when segment-artifact
            (with-current-buffer buffer
              (condition-case err
                  (progn
                    (set-visited-file-modtime)
                    (set-buffer-modified-p nil))
                (error
                 (mevedel--warn-once
                  'artifacts-session-post-save
                  "Session post-save cleanup failed: %s"
                  (error-message-string err))))))
        (setf (mevedel-session-updated-at session)
              (format-time-string "%FT%H-%M-%S"))
        (let ((publication
               (mevedel-session-publication-prune-committed
                session
                (append leading
                        (list (mevedel-session-artifacts--sidecar-artifact
                               session buffer))))))
          (if mevedel-session-artifacts-require-agent-commit-p
              (mevedel-session-publication-publish session publication t)
            (mevedel-session-publication-publish session publication)))
        (when segment-artifact
          (with-current-buffer buffer
            (condition-case err
                (progn
                  (set-visited-file-modtime)
                  (set-buffer-modified-p nil)
                  (run-hooks 'after-save-hook))
              (error
               (mevedel--warn-once
                'artifacts-session-post-save
                "Session post-save cleanup failed: %s"
                (error-message-string err))))))))
    ;; Input history is diagnostic UI state: warn/retry independently and
    ;; never turn it into critical publication.
    (mevedel-session-persistence-notify-session-event
     session 'save-history)
    (mevedel-session-save-path session)))

(defun mevedel-session-artifacts-save
    (session buffer &optional settled force)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, checkpoint any tool-modified
files from before this turn, and rewrite the sidecar.  When SETTLED is non-nil,
mark the latest assistant response
  as a stable fork point.

A portable session whose durable state is byte for byte the committed one
performs no target transaction.  FORCE publishes anyway, for a caller that
must materialize a snapshot rather than record a change."
  ;; A direct data-buffer caller establishes the session-owned root before
  ;; the projection boundary is consulted.  View callers must pass their
  ;; registered data buffer, never the view projection itself.
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (not (bound-and-true-p mevedel--agent-invocation)))))
    (when (or (null (mevedel-session-root-buffer session))
              (eq buffer (mevedel-session-root-buffer session)))
      (mevedel-session-control-transfer-register-root-buffer
       session buffer)))
  (when-let ((buffer (mevedel-session-persistence-authoritative-buffer
                      buffer)))
    (when (or (null (mevedel-session-root-buffer session))
              (eq buffer (mevedel-session-root-buffer session)))
      (mevedel-session-control-transfer-register-root-buffer session buffer))
    ;; The mutation gate and the publication entry both consult the target
    ;; recovery marker, and nothing in between can install one.
    (let ((mevedel-session-recovery--mutation-cache
           (or (bound-and-true-p mevedel-session-recovery--mutation-cache)
               (list nil)))
          ;; One save is one durable transaction, so its lease operations
          ;; share the target clock reading rather than each paying for one.
          ;; The durability module loads lazily, so the outer values are
          ;; read tolerantly: a save may run before anything durable ever
          ;; loaded it.
          (mevedel-session-durability--transaction-clock
           (or (bound-and-true-p mevedel-session-durability--transaction-clock)
               (list nil)))
          (mevedel-session-durability--asserted-directories
           (or (bound-and-true-p
                mevedel-session-durability--asserted-directories)
               (list nil))))
      ;; The whole save is one transaction on one connection, and every
      ;; segment write, sidecar rewrite, modtime stat, and file-history read
      ;; below is a command on it.  A foreign idle timer that sends its own
      ;; command in between consumes the reply we are waiting for, so the
      ;; transaction holds the connection rather than merely refusing to nest
      ;; inside somebody else's.
      (mevedel--with-gc-batched
      (mevedel-transport-with-exclusive-connection
        ;; A save is a real durable mutation even for file-workspace sessions.
        ;; Probe the target before any branch constructs serialized bytes.
        (mevedel-session-artifacts-assert-mutation-authority session buffer)
        (prog1
            (if (mevedel-session-codec-portable-authority-p session)
                (mevedel-session-artifacts--remote-save
                 session buffer settled force)
              (mevedel-session-artifacts-ensure-files session buffer)
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (when settled
                (mevedel-session-artifacts-update-prompt-index
                 session buffer)
                (mevedel-session-artifacts--ensure-latest-fork-point
                 session buffer))
              (with-current-buffer buffer
                (when (buffer-modified-p)
                  (save-buffer)))
              (mevedel-session-artifacts-update-prompt-index session buffer)
              ;; Snapshot files modified during the just-completed turn.
              (when (and (boundp 'mevedel--current-request)
                         mevedel--current-request)
                (let ((pre-snapshots
                       (mevedel-request-file-snapshots
                        mevedel--current-request)))
                  (mevedel-session-artifacts-snapshot-modified
                   session
                   (or (mevedel-session-turn-count session) 0)
                   pre-snapshots)))
              (mevedel-session-codec-write
               (mevedel-session-artifacts-sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-artifacts-build-sidecar session buffer))
              (mevedel-session-artifacts-save-instructions session buffer)
              (mevedel-session-persistence-notify-session-event
               session 'save-history)
              (mevedel-session-save-path session))
          (mevedel-session-persistence-flush-diagnostic-logs session)))))))

(defun mevedel-session-artifacts-save-agent-registry (session buffer)
  "Persist SESSION's sidecar from BUFFER without touching the segment.

Observational agent-state saves -- activity transitions, mailbox
consumption -- need only the sidecar's agent registry: the transcript
segment is committed at request settlement anyway.  The full save
transaction wrote the whole segment and scanned snapshots on every
debounced tick, which dominated a profiled multi-agent session's
allocation and produced a visible segment write every few seconds.

Portable sessions keep the full save, whose remote transaction already
elides byte-identical durable state.

Returns SESSION's save path on success, nil when SESSION is not yet
materialized."
  (when (mevedel-session-save-path session)
    (if (mevedel-session-codec-portable-authority-p session)
        (mevedel-session-artifacts-save session buffer)
      (when-let* ((buffer (mevedel-session-persistence-authoritative-buffer
                           buffer)))
        (let ((mevedel-session-recovery--mutation-cache
               (or (bound-and-true-p mevedel-session-recovery--mutation-cache)
                   (list nil)))
              (mevedel-session-durability--transaction-clock
               (or (bound-and-true-p
                    mevedel-session-durability--transaction-clock)
                   (list nil)))
              (mevedel-session-durability--asserted-directories
               (or (bound-and-true-p
                    mevedel-session-durability--asserted-directories)
                   (list nil))))
          (mevedel--with-gc-batched
            (mevedel-transport-with-exclusive-connection
              (mevedel-session-artifacts-assert-mutation-authority
               session buffer)
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (mevedel-session-codec-write
               (mevedel-session-artifacts-sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-artifacts-build-sidecar session buffer))
              (mevedel-session-persistence-notify-session-event
               session 'save-history)
              (mevedel-session-save-path session))))))))


;;
;;; File-history store
;;
;; Per-session on-disk backup store at <save-path>/file-history/.
;; Filename scheme: `<sha256(absolute-filepath)[:16]>@v<N>' where <N>
;; is the sequential per-file version.  Mapping from (turn, path) to
;; backup filename lives in `mevedel-session-file-snapshots' (alist
;; keyed by turn number; inner alist keyed by absolute path).

(defun mevedel-session-artifacts--file-history-path-hash (path)
  "Return the first 16 hex chars of SHA-256 of PATH (expanded)."
  (substring (secure-hash 'sha256 (expand-file-name path)) 0 16))

(defun mevedel-session-artifacts--file-history-backup-name (path version)
  "Return the backup filename for PATH at VERSION."
  (format "%s@v%d" (mevedel-session-artifacts--file-history-path-hash path) version))

(defun mevedel-session-artifacts-backup-path (save-path backup-name)
  "Return the absolute path to BACKUP-NAME under SAVE-PATH's file-history/."
  (file-name-concat save-path "file-history" backup-name))

(defun mevedel-session-artifacts--file-history-latest-version (session path)
  "Return the highest version recorded for PATH in SESSION, or 0 if none."
  (let ((best 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best) (setq best v))))))

(defun mevedel-session-artifacts-read-file-raw (path)
  "Return PATH's contents as a unibyte string (no encoding conversion)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-string)))

(defun mevedel-session-artifacts--file-history-write-backup (save-path backup-name content)
  "Atomically write CONTENT under `<SAVE-PATH>/file-history/BACKUP-NAME'."
  (let* ((dir  (file-name-concat save-path "file-history"))
         (dest (file-name-concat dir backup-name)))
    (if mevedel-session-artifacts--collecting-critical-artifacts-p
        (push (list :path dest :content content)
              mevedel-session-artifacts--critical-artifacts)
      ;; Backups are verbatim copies of the user's files inside session
      ;; storage; keep them owner-only rather than inheriting the
      ;; default modes an ordinary artifact gets.
      (mevedel--write-file-atomically dest content 'no-conversion #o600))))

(defun mevedel-session-artifacts--file-history-maybe-snapshot (session path pre-content)
  "Return SESSION's pre-turn checkpoint entry for changed PATH.

PATH is an absolute filesystem path that was touched by the just-completed
turn's tools.  PRE-CONTENT is PATH's content at the start of the turn,
or nil if it did not exist.

Returns nil when PATH did not change.  PRE-CONTENT is a string, nil for a
previously absent path, or `(:gap REASON)' when pre-turn capture failed."
  (let* ((current-exists (file-exists-p path))
         (version (1+ (mevedel-session-artifacts--file-history-latest-version session path)))
         (base (list :version version
                     :backup-time (format-time-string "%FT%H-%M-%S")))
         (gap
          (lambda (reason)
            (display-warning
             'mevedel (format "Checkpoint gap for %s: %s" path reason)
             :warning)
            (cons path (append base (list :gap reason))))))
    (cond
     ((and (listp pre-content) (plist-get pre-content :gap))
      (funcall gap (plist-get pre-content :gap)))
     ((and (stringp pre-content)
           (> (string-bytes pre-content)
              mevedel-file-history-max-snapshot-bytes))
      (funcall gap
               (format "pre-turn content exceeds %d bytes"
                       mevedel-file-history-max-snapshot-bytes)))
     ((and current-exists (not (file-regular-p path)))
      (funcall gap "post-turn path is not a regular file"))
     (t
      (condition-case err
          (let ((current-content
                 (and current-exists
                      (mevedel-session-artifacts-read-file-raw path))))
            (unless (if current-exists
                        (and (stringp pre-content)
                             (string-equal pre-content current-content))
                      (null pre-content))
              (let* ((backup-name
                      (and current-exists
                           (mevedel-session-artifacts--file-history-backup-name path version)))
                     (pre-backup-name
                      (and (stringp pre-content)
                           (concat
                            (mevedel-session-artifacts--file-history-backup-name path version)
                            ".pre"))))
                (when backup-name
                  (mevedel-session-artifacts--file-history-write-backup
                   (mevedel-session-save-path session)
                   backup-name current-content))
                (when pre-backup-name
                  (mevedel-session-artifacts--file-history-write-backup
                   (mevedel-session-save-path session)
                   pre-backup-name pre-content))
                (cons path
                      (append base
                              (list :backup-name backup-name
                                    :pre-backup-name pre-backup-name))))))
        (error (funcall gap (error-message-string err))))))))

(defun mevedel-session-artifacts-snapshot-modified (session turn-n pre-snapshots)
  "Snapshot files modified during TURN-N for SESSION.

PRE-SNAPSHOTS is a hash-table mapping absolute path to the file's content
at turn start (nil for paths that did not yet exist).  Typically the
`file-snapshots' slot of the just-completed request.

For each changed path, persist its pre- and post-turn content or absence
under TURN-N.  Known capture failures remain explicit gap entries.  TURN-N
is recorded even when no tracked file changed.  Returns the list of backup
names written."
  (when (and (mevedel-session-save-path session)
             (hash-table-p pre-snapshots))
    (let (entries written)
      (maphash
       (lambda (path pre-content)
         (when-let ((entry (mevedel-session-artifacts--file-history-maybe-snapshot
                            session path pre-content)))
           (push entry entries)
           (when-let ((name (plist-get (cdr entry) :backup-name)))
             (push name written))
           (when-let ((name (plist-get (cdr entry) :pre-backup-name)))
             (push name written))))
       pre-snapshots)
      ;; Sort entries by path so two saves with identical state
      ;; produce byte-identical sidecars (hash-table iteration is
      ;; otherwise non-deterministic).
      (setq entries (sort entries
                          (lambda (a b) (string< (car a) (car b)))))
      (let ((cell (assoc turn-n (mevedel-session-file-snapshots session))))
        (if cell
            (setcdr cell entries)
          (setf (mevedel-session-file-snapshots session)
                (cons (cons turn-n entries)
                      (mevedel-session-file-snapshots session)))))
      (sort written #'string<))))

;;
;;; Segment rotation (split-on-compact)

(defun mevedel-session-artifacts--file-text (file)
  "Return FILE contents as a string using normal text decoding."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mevedel-session-artifacts-refresh-visited-file-modtime-or-error
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
      (let ((file-text (mevedel-session-artifacts--file-text buffer-file-name))
            (accepted (cons (buffer-substring-no-properties (point-min) (point-max))
                            (if (listp expected-texts)
                                expected-texts
                              (list expected-texts)))))
        (if (member file-text accepted)
            (set-visited-file-modtime)
          (error "Session segment changed on disk: %s" buffer-file-name)))))))

(defun mevedel-session-artifacts-disown-save-machinery ()
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

(defun mevedel-session-artifacts--set-visited-segment-file (file)
  "Make the current buffer visit segment FILE without changing its name."
  (let ((name (buffer-name)))
    (set-visited-file-name file t)
    (rename-buffer name t))
  (setq buffer-file-truename (file-truename file))
  (mevedel-session-artifacts-disown-save-machinery)
  (set-visited-file-modtime)
  (set-buffer-modified-p nil))

(defun mevedel-session-artifacts--publish-segment-text (file text)
  "Atomically write TEXT to FILE and make the current buffer visit it."
  (let ((coding-system buffer-file-coding-system))
    (with-temp-buffer
      (setq buffer-file-coding-system coding-system)
      (insert text)
      (mevedel-session-persistence-write-current-buffer-atomically file)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text))
  (mevedel-session-artifacts--set-visited-segment-file file))

(defun mevedel-session-artifacts--insert-segment-header (session)
  "Insert per-segment org properties at point in current buffer.

Sets `MEVEDEL_SESSION_ID', `MEVEDEL_SEGMENT_NUMBER',
`MEVEDEL_SEGMENT_CREATED_AT', and `MEVEDEL_VERSION'.  Caller is
responsible for ensuring the buffer is in `org-mode' (mevedel data
buffers are locked to `org-mode' by `mevedel--chat-buffer-setup')."
  (when (derived-mode-p 'org-mode)
    (org-entry-put (point-min) "MEVEDEL_VERSION" (mevedel-version))
    (org-entry-put (point-min) "MEVEDEL_SESSION_ID"
                   (or (mevedel-session-session-id session) ""))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_NUMBER"
                   (number-to-string
                    (or (mevedel-session-current-segment session) 1)))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_CREATED_AT"
                   (format-time-string "%FT%H-%M-%S"))))

(defun mevedel-session-artifacts-segment-summary-bounds ()
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

(defconst mevedel-session-artifacts--summary-handoff-prefix
  "Another language model started to solve this problem and produced a summary of its work. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary:\n\n"
  "Model-facing preface inserted before a compacted segment summary.")

(defun mevedel-session-artifacts-strip-summary-handoff-prefix (summary)
  "Return SUMMARY without the model-facing handoff prefix."
  (if (and (stringp summary)
           (string-prefix-p
            mevedel-session-artifacts--summary-handoff-prefix summary))
      (substring summary
                 (length mevedel-session-artifacts--summary-handoff-prefix))
    summary))

(defun mevedel-session-artifacts-summary-block (summary)
  "Return SUMMARY wrapped in an org `#+begin_summary' block.

The block markers are propertized with `gptel \\='ignore' so the LLM sees
only the handoff preface plus SUMMARY -- not the wrapper lines.  The
user\\='s view, by contrast, sees a foldable block."
  (concat (propertize "#+begin_summary mevedel-role=compaction-summary\n"
                      'gptel 'ignore)
          mevedel-session-artifacts--summary-handoff-prefix
          summary
          (propertize "\n#+end_summary\n" 'gptel 'ignore)))

(defun mevedel-session-artifacts--finalize-segment-file (file)
  "Mark segment FILE finalized on disk."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                     (format-time-string "%FT%H-%M-%S"))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun mevedel-session-artifacts-finalized-segment-text (text coding)
  "Return segment TEXT with finalized metadata, encoded using CODING."
  (with-temp-buffer
    (setq buffer-file-coding-system coding)
    (insert text)
    (let ((org-agenda-file-menu-enabled nil))
      (org-mode))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                   (format-time-string "%FT%H-%M-%S"))
    (buffer-string)))

(defun mevedel-session-artifacts--publish-remote-segment-transition
    (session buffer old-segment old-text new-segment new-text)
  "Publish one portable project SESSION segment transition derived from BUFFER.

OLD-SEGMENT receives finalized OLD-TEXT.  NEW-SEGMENT receives NEW-TEXT.
Instruction snapshots and the sidecar join the same batch, with the sidecar
last as its commit marker."
  (with-current-buffer buffer
    (let ((coding buffer-file-coding-system))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert new-text))
      ;; Keep the live transition installed if publication becomes pending.
      ;; Retry can then publish the exact state represented in memory.
      (setq buffer-file-name new-segment
            buffer-file-truename nil)
      (mevedel-session-publication-publish
       session
       (append
        (list
         (list :path old-segment
               :content
               (mevedel-session-artifacts-finalized-segment-text
                old-text coding)
               :coding coding)
         (list :path new-segment :content new-text :coding coding))
        (mevedel-session-artifacts--instruction-artifacts session buffer)
        (list
         (list
          :path
          (mevedel-session-artifacts-sidecar-path
           (mevedel-session-save-path session))
          :content
          (mevedel-session-artifacts-printed-value
           (mevedel-session-artifacts-build-sidecar session buffer))
          :commit-marker t))))
      (mevedel-session-artifacts--set-visited-segment-file new-segment))))

(defun mevedel-session-artifacts--delete-trailing-text (text)
  "Delete trailing TEXT from the current buffer when it is an exact suffix."
  (when (and text
             (not (string-empty-p text))
             (string-suffix-p
              (substring-no-properties text)
              (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (- (point-max) (length text)) (point-max))
    t))

(cl-defun mevedel-session-artifacts-rotate-segment
    (session buffer summary &key tail-text pending-text archive-text
             truncated-tail-p)
  "Finalize SESSION's current segment and start a new one with SUMMARY.

Performs the split-on-compact rotation:
  1. Saves the current segment file before replacing the live buffer.
  2. Advances `mevedel-session-current-segment' on SESSION.
  3. Builds the new segment in a temporary buffer.
  4. Atomically publishes it and repoints BUFFER's visited-file state.
  5. Restores PENDING-TEXT in the live buffer without marking it saved.
  6. Rewrites the sidecar.
  7. Sets `MEVEDEL_SEGMENT_FINALIZED_AT' on the predecessor segment.

TAIL-TEXT is preserved recent transcript text, including text
properties.  ARCHIVE-TEXT contains durable hidden records replacing
execution rows removed by compaction.  PENDING-TEXT is an
inserted-but-unsent prompt region.
TRUNCATED-TAIL-P is recorded as segment metadata when non-nil.

Requires SESSION to have a `save-path' (i.e., to have been lazily
materialized).  Returns the new segment's absolute path on success,
nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (mevedel-session-artifacts-assert-mutation-authority session buffer)
    (with-current-buffer buffer
      ;; 1. Save the current segment before replacing the buffer body.
      (let ((portable-p
             (mevedel-session-codec-portable-authority-p session))
            (old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (tail-prompt-count
             (mevedel-session-artifacts--prompt-count-in-text tail-text))
            old-publish-text
            new-segment
            new-text
            pending-position)
        (let ((telemetry-span
               (and (fboundp 'mevedel-telemetry-start)
                    (mevedel-telemetry-start
                     session 'segment-rotation
                     :old-segment old-current-segment
                     :new-segment (1+ old-current-segment)))))
        (condition-case err
            (progn
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'old-save-start
                 :old-segment old-current-segment))
              (mevedel-session-artifacts-refresh-visited-file-modtime-or-error
               (when (and pending-text
                          (string-suffix-p
                           (substring-no-properties pending-text)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                 (buffer-substring-no-properties
                  (point-min) (- (point-max) (length pending-text)))))
              (when pending-text
                (let ((inhibit-read-only t))
                  (mevedel-session-artifacts--delete-trailing-text
                   pending-text)))
              (if portable-p
                  (setq old-publish-text
                        (buffer-substring (point-min) (point-max)))
                (when (buffer-modified-p) (save-buffer)))
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'old-saved
                 :old-segment old-current-segment))
              ;; 2. Advance segment counter.
              (cl-incf (mevedel-session-current-segment session))
              ;; 3. Build the new segment without changing visited-file state.
              (setq new-segment
                    (mevedel-session-artifacts-segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (let ((coding-system buffer-file-coding-system))
                (setq new-text
                      (with-temp-buffer
                        (setq buffer-file-coding-system coding-system)
                        (org-mode)
                        ;; 4. Build the persisted body in isolation.
                        (mevedel-session-artifacts--insert-segment-header
                         session)
                        (when truncated-tail-p
                          (org-entry-put
                           (point-min) "MEVEDEL_SEGMENT_TRUNCATED_TAIL" "t"))
                        (when (> tail-prompt-count 0)
                          (org-entry-put
                           (point-min) "MEVEDEL_SEGMENT_TAIL_PROMPTS"
                           (number-to-string tail-prompt-count)))
                        (goto-char (point-max))
                        (unless (bolp) (insert "\n"))
                        (insert "\n")
                        (insert
                         (mevedel-session-artifacts-summary-block summary))
                        (when tail-text
                          (unless (bolp) (insert "\n"))
                          (insert tail-text))
                        (when archive-text
                          (insert archive-text))
                        (mevedel-session-artifacts-stabilize-gptel-bounds)
                        (when pending-text
                          (unless (bolp) (insert "\n"))
                          (setq pending-position (point)))
                        (insert "\n")
                        (buffer-string))))
              ;; 5. Publish, then repoint the canonical live buffer.
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-publish-start
                 :new-segment (mevedel-session-current-segment session)))
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (if portable-p
                  (mevedel-session-artifacts--publish-remote-segment-transition
                   session buffer old-segment old-publish-text
                   new-segment new-text)
                (mevedel-session-artifacts--publish-segment-text
                 new-segment new-text)
                ;; 6. Rewrite the sidecar with the bumped current-segment.
                (mevedel-session-codec-write
                 (mevedel-session-artifacts-sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-artifacts-build-sidecar session buffer))
                (mevedel-session-artifacts-save-instructions session buffer)
                (mevedel-session-artifacts--finalize-segment-file
                 old-segment))
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-published
                 :new-segment (mevedel-session-current-segment session)))
              (when telemetry-span
                (mevedel-telemetry-finish
                 telemetry-span :outcome 'success))
              (when pending-position
                (goto-char pending-position)
                (insert pending-text)
                ;; The pending prompt belongs to the in-flight request.
                ;; The DONE autosave will commit it together with the
                ;; assistant response; failure/abort paths must not.
                (set-buffer-modified-p nil))
              new-segment)
          (error
           (when telemetry-span
             (mevedel-telemetry-finish
              telemetry-span :outcome 'error :error-class (car-safe err)))
           (if (and portable-p
                    (mevedel-session-pending-publication session))
               (progn
                 (when pending-position
                   (goto-char pending-position)
                   (insert pending-text))
                 (set-buffer-modified-p nil))
             (setf (mevedel-session-current-segment session)
                   old-current-segment)
             (setf (mevedel-session-updated-at session) old-updated-at)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert old-text))
             (mevedel-session-artifacts--set-visited-segment-file
              old-segment)
             (goto-char (min old-point (point-max)))
             (set-buffer-modified-p old-modified-p)
             (ignore-errors
               (mevedel-session-codec-write
                (mevedel-session-artifacts-sidecar-path
                 (mevedel-session-save-path session))
                (mevedel-session-artifacts-build-sidecar session buffer)))
             (when (and new-segment (file-exists-p new-segment))
               (delete-file new-segment)))
           (signal (car err) (cdr err)))))))))

(cl-defun mevedel-session-artifacts-start-fresh-segment
    (session buffer &key initial-text)
  "Finalize SESSION's current segment and start a blank live segment in BUFFER.

INITIAL-TEXT, when non-nil, is inserted after the new segment's org
metadata.  This is used by `/clear' to leave a fresh prompt prefix in
the data buffer without carrying over any conversation summary.

Requires SESSION to have a `save-path'.  Returns the new segment's
absolute path on success, nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (mevedel-session-artifacts-assert-mutation-authority session buffer)
    (with-current-buffer buffer
      (let ((portable-p
             (mevedel-session-codec-portable-authority-p session))
            (old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            old-publish-text
            new-segment
            new-text
            initial-position)
        (unless old-segment
          (error "No current segment file"))
        (condition-case err
            (progn
              (mevedel-session-artifacts-refresh-visited-file-modtime-or-error)
              (if portable-p
                  (setq old-publish-text
                        (buffer-substring (point-min) (point-max)))
                (when (buffer-modified-p) (save-buffer)))
              (mevedel-session-artifacts-update-prompt-index
               session buffer)
              (cl-incf (mevedel-session-current-segment session))
              (setq new-segment
                    (mevedel-session-artifacts-segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (let ((coding-system buffer-file-coding-system))
                (setq new-text
                      (with-temp-buffer
                        (setq buffer-file-coding-system coding-system)
                        (org-mode)
                        (mevedel-session-artifacts--insert-segment-header
                         session)
                        (goto-char (point-max))
                        (when (and initial-text
                                   (not (string-empty-p initial-text)))
                          (unless (bolp) (insert "\n"))
                          (setq initial-position (point)))
                        (buffer-string))))
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (if portable-p
                  (mevedel-session-artifacts--publish-remote-segment-transition
                   session buffer old-segment old-publish-text
                   new-segment new-text)
                (mevedel-session-artifacts--publish-segment-text
                 new-segment new-text)
                (mevedel-session-codec-write
                 (mevedel-session-artifacts-sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-artifacts-build-sidecar session buffer))
                (mevedel-session-artifacts-save-instructions session buffer)
                (mevedel-session-artifacts--finalize-segment-file
                 old-segment))
              (when initial-position
                (goto-char initial-position)
                (insert initial-text)
                (set-buffer-modified-p nil))
              (goto-char (point-max))
              (mevedel-session-persistence-notify-session-event
               session 'rerender)
              new-segment)
          (error
           (if (and portable-p
                    (mevedel-session-pending-publication session))
               (progn
                 (when initial-position
                   (goto-char initial-position)
                   (insert initial-text))
                 (set-buffer-modified-p nil))
             (setf (mevedel-session-current-segment session)
                   old-current-segment)
             (setf (mevedel-session-updated-at session) old-updated-at)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert old-text))
             (mevedel-session-artifacts--set-visited-segment-file
              old-segment)
             (goto-char (min old-point (point-max)))
             (set-buffer-modified-p old-modified-p)
             (ignore-errors
               (mevedel-session-codec-write
                (mevedel-session-artifacts-sidecar-path
                 (mevedel-session-save-path session))
                (mevedel-session-artifacts-build-sidecar session buffer)))
             (when (and new-segment (file-exists-p new-segment))
               (delete-file new-segment)))
           (signal (car err) (cdr err))))))))


;;
;;; Workspace relocation reconciliation

(defun mevedel-session-artifacts-reconcile-relocation
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
          (mevedel-execution-target-native-root
           (mevedel-session-execution-target session)))
         (saved-root-expanded (and saved-root (expand-file-name saved-root)))
         (current-root-expanded (and current-root
                                     (expand-file-name current-root))))
    (when (and saved-root current-root
               (not (equal saved-root-expanded current-root-expanded)))
      ;; Rewrite path-bearing permission rules.
      (let ((rewrites 0)
            (saved-prefix (file-name-as-directory saved-root-expanded))
            (current-prefix (file-name-as-directory current-root-expanded)))
        (setf (mevedel-session-permission-rules session)
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
               (mevedel-session-permission-rules session)))
        ;; Touched-files resets to an empty hash on load, so there is
        ;; nothing to prune here today; kept as the conceptual place
        ;; if we ever start carrying touched-files across resume.
        (when (> rewrites 0)
          (message "Reconciled %d permission paths from %s to %s"
                   rewrites saved-root current-root))))))


;;
;;; Segment-counter self-heal

(defun mevedel-session-artifacts-detect-highest-segment (save-path)
  "Return the highest segment number found on disk under SAVE-PATH, or 0."
  (let ((max-n 0))
    (when (file-directory-p save-path)
      (dolist (f (directory-files save-path nil
                                  "\\`segment-[0-9]+\\.chat\\.org\\'"))
        (when (string-match "segment-\\([0-9]+\\)\\.chat\\.org" f)
          (let ((n (string-to-number (match-string 1 f))))
            (when (> n max-n) (setq max-n n))))))
    max-n))

(defun mevedel-session-artifacts-self-heal-segment-counter
    (session save-path &optional defer-finalization-p)
  "Reconcile SESSION's `:current-segment' with the filesystem under SAVE-PATH.

If the highest-numbered segment file on disk differs from the sidecar's
recorded `:current-segment', trust the filesystem (the sidecar may be
stale from a crash mid-rotation).  Logs a warning.  When healing upward
after a crash that published a new segment before finalizing its
predecessor, mark the predecessor finalized now.  When
DEFER-FINALIZATION-P is non-nil, return that predecessor without writing it
so the caller can include finalization in a larger publication batch."
  (let* ((sidecar-n    (or (mevedel-session-current-segment session) 1))
         (filesystem-n (mevedel-session-artifacts-detect-highest-segment
                        save-path))
         predecessor)
    (when (and (> filesystem-n 0)
               (not (= sidecar-n filesystem-n)))
      (display-warning
       'mevedel
       (format "Sidecar :current-segment %d differs from filesystem (%d); using %d"
               sidecar-n filesystem-n filesystem-n)
       :warning)
      (when (> filesystem-n sidecar-n)
        (setq predecessor
              (mevedel-session-artifacts-segment-path
               save-path (1- filesystem-n)))
        (unless defer-finalization-p
          (mevedel-session-artifacts--finalize-segment-file predecessor)))
      (setf (mevedel-session-current-segment session) filesystem-n))
    predecessor))


(provide 'mevedel-session-artifacts)

;;; mevedel-session-artifacts.el ends here
