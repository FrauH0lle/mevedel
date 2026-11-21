;;; niwa-agent-conversation.el --- Retained agent conversations -*- lexical-binding: t -*-

;;; Commentary:

;; Owns retained agent conversation buffers: creation and hydration, frozen
;; request-local installation, transcript saves, final response extraction,
;; activity history, and live presentation refresh.  Provider request/FSM
;; execution remains in `niwa-agent-exec'.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'niwa-agents)
(require 'niwa-ptc-checkpoint)
(require 'niwa-session-checkpoint)

;; `gptel'
(declare-function gptel--save-state "ext:gptel" ())
(defvar gptel-mode)
(defvar gptel-post-response-functions)
(defvar gptel-post-stream-hook)
(defvar gptel-post-tool-call-functions)
(defvar gptel-pre-tool-call-functions)

;; `gptel-org'
(defvar gptel-org-branching-context)
(defvar gptel-org-ignore-elements)

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (&optional buf))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)
(defvar gptel-confirm-tool-calls)
(defvar gptel-org-convert-response)

;; `niwa-engine'
(declare-function niwa-engine-record-history-edit "niwa-engine" (begin end))
(autoload 'niwa-engine-record-history-edit "niwa-engine")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--sidecar-artifact
                  "niwa-session-artifacts" (session buffer &optional checkpoint))
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-call-with-checkpoint
                  "niwa-session-artifacts" (session function))
(declare-function niwa-session-artifacts-find-artifact-noselect
                  "niwa-session-artifacts"
                  (session logical &optional inspection committed-only))
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding prepared token))
(declare-function niwa-session-artifacts-publish-transcript-state
                  "niwa-session-artifacts"
                  (session root-buffer transcript-path content checkpoint &optional coding prepared token))
(declare-function niwa-session-artifacts-stabilize-gptel-bounds
                  "niwa-session-artifacts" ())
(declare-function niwa-session-artifacts-strip-gptel-config-properties
                  "niwa-session-artifacts" nil)
(autoload 'niwa-session-artifacts--sidecar-artifact
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-call-with-checkpoint
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-find-artifact-noselect
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-transcript-state
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-stabilize-gptel-bounds
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-strip-gptel-config-properties
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(autoload 'niwa-session-codec-portable-authority-p
  "niwa-session-codec")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-update-transcript-entry
                  "niwa-session-persistence" (session agent-id updates))
(declare-function niwa-session-persistence-write-sidecar-now
                  "niwa-session-persistence" (session buffer &optional prepared checkpoint token))
(autoload 'niwa-session-persistence-update-transcript-entry
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-sidecar-now
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-barrier
                  "niwa-session-publication" (session &optional token))
(autoload 'niwa-session-publication-barrier "niwa-session-publication")

;; `niwa-skills-prompt'
(declare-function niwa-skills-install-activation-hook
                  "niwa-skills-prompt" ())
(autoload 'niwa-skills-install-activation-hook "niwa-skills-prompt")

;; `niwa-structs'
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa--view-buffer)
(defvar niwa--workspace)

;; `niwa-tool-repair'
(declare-function niwa-tool-repair-clear-ledger
                  "niwa-tool-repair" (&rest args))
(declare-function niwa-tool-repair-post-tool-call
                  "niwa-tool-repair" (&rest args))
(declare-function niwa-tool-repair-pre-tool-call
                  "niwa-tool-repair" (&rest args))
(autoload 'niwa-tool-repair-clear-ledger "niwa-tool-repair")
(autoload 'niwa-tool-repair-post-tool-call "niwa-tool-repair")
(autoload 'niwa-tool-repair-pre-tool-call "niwa-tool-repair")

;; `niwa-transcript'
(declare-function niwa-transcript-project-segments
                  "niwa-transcript"
                  (segments &rest keys))
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(autoload 'niwa-transcript-segments "niwa-transcript")
(autoload 'niwa-transcript-project-segments "niwa-transcript")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-enable-gptel-mode
                  "niwa-transcript-restore" ())
(declare-function niwa-transcript-restore-gptel-state
                  "niwa-transcript-restore" ())
(autoload 'niwa-transcript-enable-gptel-mode
  "niwa-transcript-restore")
(autoload 'niwa-transcript-restore-gptel-state
  "niwa-transcript-restore")

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel delay))
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--insert-user-role-block-at-marker
                  "niwa-utilities" (block &optional marker))
(declare-function niwa--optimize-transcript-buffer
                  "niwa-utilities" ())
(declare-function niwa--same-file-p
                  "niwa-utilities" (file-a file-b))
(declare-function niwa--transcript-org-mode
                  "niwa-utilities" ())
(autoload 'niwa--insert-user-role-block-at-marker "niwa-utilities")
(autoload 'niwa--optimize-transcript-buffer "niwa-utilities")
(autoload 'niwa--same-file-p "niwa-utilities")
(autoload 'niwa--transcript-org-mode "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))

;; `niwa-view-render'
(declare-function niwa-view--refresh-tool-row "niwa-view-render"
                  (data-buffer tool-use-id))

;; `niwa-view-agent'
(declare-function niwa-view-agent-live-transcript-post-tool
                  "niwa-view-agent" (args))
(declare-function niwa-view-agent-live-transcript-pre-tool
                  "niwa-view-agent" (args))
(declare-function niwa-view-agent-live-transcript-stream
                  "niwa-view-agent" ())
(declare-function niwa-view-refresh-agent-rendering
                  "niwa-view-agent" (view-buffer agent-id))
(autoload 'niwa-view-agent-live-transcript-post-tool "niwa-view-agent")
(autoload 'niwa-view-agent-live-transcript-pre-tool "niwa-view-agent")
(autoload 'niwa-view-agent-live-transcript-stream "niwa-view-agent")
(autoload 'niwa-view-refresh-agent-rendering "niwa-view-agent")

;; `org-element'
(declare-function org-element-cache-reset "ext:org-element"
                  (&optional all no-persistence))
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

(require 'niwa-execution-telemetry)
(require 'niwa-execution-transcript)
(require 'niwa-tool-render-data)


(defcustom niwa-agent-conversation-save-debounce 2.0
  "Idle seconds before saving a running retained agent conversation.
A non-positive value saves immediately.  Terminal paths always save now."
  :type 'number
  :group 'niwa)

(defconst niwa-agent-conversation--live-activity-limit 5
  "Most recent activity items an invocation keeps.
Only the latest item is consulted, to keep waiting items sparse; the list
is neither persisted nor rendered, so an unbounded history only cost
memory and a quadratic rewrite of every agent's metadata.")

(defun niwa-agent-conversation--reject-terminal-tool-call (&rest _)
  "Stop a tool call after the current retained invocation has settled."
  (when (and (niwa-agent-invocation-p niwa--agent-invocation)
             (niwa-agent-invocation-runtime-settled-p
              niwa--agent-invocation))
    '(:stop t :stop-reason "Agent turn already settled")))


;;
;;; Buffer lifecycle

(defun niwa-agent-conversation--working-directory (session workspace)
  "Return the project directory for SESSION and WORKSPACE."
  (file-name-as-directory
   (expand-file-name
    (or (and session (niwa-session-working-directory session))
        (and workspace (niwa-workspace-root workspace))
        default-directory))))

(defun niwa-agent-conversation-open
    (invocation parent-data-buffer &optional existing-buffer)
  "Create and configure INVOCATION's conversation below PARENT-DATA-BUFFER.
Use EXISTING-BUFFER when hydrating a persisted logical artifact."
  (let* ((agent-id (niwa-agent-invocation-agent-id invocation))
         (suffix (and (stringp agent-id)
                      (cadr (split-string agent-id "--" t))))
         (short-id (if (stringp suffix)
                       (substring suffix 0 (min 8 (length suffix)))
                     "anon"))
         (buffer
          (or existing-buffer
              ;; Hidden like other internal buffers: agent views and the
              ;; transcript toggle are the user's way in.
              (generate-new-buffer
               (format " *niwa-agent-%s*" short-id))))
         (parent-session
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'niwa--session parent-data-buffer)))
         (parent-workspace
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'niwa--workspace parent-data-buffer)))
         (parent-view
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'niwa--view-buffer parent-data-buffer)))
         (parent-specs
          (and (buffer-live-p parent-data-buffer)
               (niwa-agents-specs parent-data-buffer))))
    (with-current-buffer buffer
      (when parent-session
        (setq-local niwa--session parent-session))
      (when parent-workspace
        (setq-local niwa--workspace parent-workspace))
      (setq default-directory
            (or (niwa-agent-invocation-working-directory invocation)
                (niwa-agent-conversation--working-directory
                 parent-session parent-workspace)))
      (let ((org-element-use-cache nil))
        (niwa--transcript-org-mode))
      (when (fboundp 'org-element-cache-reset)
        (let ((org-element-use-cache t))
          (ignore-errors
            (org-element-cache-reset nil 'no-persistence))))
      (setq-local org-element-use-cache nil)
      (setq-local org-element-cache-persistent nil)
      (setq-local gptel-org-convert-response nil)
      (setq-local gptel-org-branching-context nil)
      (setq-local gptel-org-ignore-elements '(property-drawer))
      (niwa--optimize-transcript-buffer)
      (unless (require 'gptel nil t)
        (kill-buffer buffer)
        (error "Could not load gptel for sub-agent"))
      (condition-case err
          (niwa-transcript-enable-gptel-mode)
        (error
         (kill-buffer buffer)
         (signal (car err) (cdr err))))
      (when (and parent-view (buffer-live-p parent-view))
        (setq-local niwa--view-buffer parent-view))
      ;; Tool calls go through niwa's permission pipeline, not gptel's
      ;; confirmation, which nobody may be there to answer.
      (setq-local gptel-confirm-tool-calls nil)
      (when parent-specs
        (niwa-agents-set-specs parent-specs))
      (setq-local niwa--agent-invocation invocation)
      (setq-local niwa--conversation-working-directory
                  (niwa-agent-invocation-working-directory invocation))
      (add-hook 'before-change-functions #'niwa-engine-record-history-edit nil t)
      (when (require 'niwa-skills-prompt nil t)
        (niwa-skills-install-activation-hook))
      (add-hook 'gptel-pre-tool-call-functions
                #'niwa-agent-conversation--reject-terminal-tool-call -110 t)
      (add-hook 'gptel-pre-tool-call-functions
                #'niwa-tool-repair-pre-tool-call -100 t)
      (add-hook 'gptel-post-tool-call-functions
                #'niwa-tool-repair-post-tool-call -100 t)
      (add-hook 'gptel-post-response-functions
                #'niwa-tool-repair-clear-ledger nil t)
      (add-hook 'kill-buffer-hook #'niwa-tool-repair-clear-ledger nil t)
      (add-hook 'gptel-post-stream-hook
                #'niwa-view-agent-live-transcript-stream nil t)
      (add-hook 'gptel-pre-tool-call-functions
                #'niwa-view-agent-live-transcript-pre-tool nil t)
      (add-hook 'gptel-post-tool-call-functions
                #'niwa-view-agent-live-transcript-post-tool nil t)
      (add-hook 'gptel-post-response-functions
                #'niwa-execution-transcript-retry-terminals nil t)
      (add-hook
       'gptel-pre-tool-call-functions
       (lambda (&rest args)
         (let ((inv niwa--agent-invocation))
           (prog1 nil
             (when (niwa-agent-invocation-p inv)
               (cl-incf (niwa-agent-invocation-call-count inv))
               (let ((tool-name
                      (or (niwa-agent-conversation--activity-tool-name args)
                          "Tool")))
                 (niwa-agent-conversation-record-activity
                  inv (list :type 'tool-start
                            :tool-name tool-name
                            :summary (format "%s(...)" tool-name))
                  t))
               (niwa-agent-conversation-refresh inv)))))
       nil t)
      (add-hook
       'gptel-post-tool-call-functions
       (lambda (&rest args)
         (let ((inv niwa--agent-invocation))
           (prog1 nil
             (when (niwa-agent-invocation-p inv)
               (let ((tool-name
                      (or (niwa-agent-conversation--activity-tool-name args)
                          "Tool")))
                 (niwa-agent-conversation-record-activity
                  inv
                  (if (niwa-agent-conversation--activity-error-p args)
                      (list :type 'tool-error :tool-name tool-name
                            :error (format "%s failed" tool-name))
                    (list :type 'tool-finish :tool-name tool-name
                          :summary (format "%s done" tool-name)))
                  t))
               (niwa-agent-conversation-refresh inv)))))
       nil t)
      (add-hook
       'gptel-post-response-functions
       (lambda (&rest _)
         (let ((inv niwa--agent-invocation))
           (prog1 nil
             (when (niwa-agent-invocation-p inv)
               (niwa-agent-conversation-refresh inv)))))
       nil t)
      (niwa-agent-conversation--install-kill-guards))
    buffer))

(defun niwa-agent-conversation-hydrate
    (invocation parent-data-buffer logical-path &optional inspection)
  "Restore INVOCATION below PARENT-DATA-BUFFER from LOGICAL-PATH.

LOGICAL-PATH is session-relative.  When INSPECTION is non-nil, retain the
resolver's read-only, no-save buffer contract.
Return the hydrated conversation buffer."
  (let ((session (niwa-agent-invocation-parent-session invocation)))
    (unless session
      (error "Agent conversation has no session"))
    (let ((buffer
           (niwa-session-artifacts-find-artifact-noselect
            session logical-path inspection)))
      (condition-case err
          (progn
            (let ((inhibit-read-only t))
              (niwa-agent-conversation-open
               invocation parent-data-buffer buffer))
            (setf (niwa-agent-invocation-buffer invocation) buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t)
                    recovered)
                (niwa-transcript-restore-gptel-state)
                (niwa-agent-conversation-configure invocation)
                (niwa-session-artifacts-strip-gptel-config-properties)
                (unless inspection
                  (setq recovered
                        (niwa-ptc-checkpoint-reconcile
                         session (niwa-agent-invocation-path invocation))))
                (niwa-session-artifacts-stabilize-gptel-bounds)
                ;; Entering the conversation's major mode clears the resolver's
                ;; buffer locals.  Restore its inspection/no-save contract.
                (setq-local niwa-session--inspection-buffer-p inspection
                            buffer-offer-save (not inspection))
                (setq buffer-read-only inspection)
                (set-buffer-modified-p (and recovered (> (car recovered) 0)))))
            buffer)
        (error
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (set-buffer-modified-p nil))
           (kill-buffer buffer))
         (signal (car err) (cdr err)))))))

(defun niwa-agent-conversation-configure (invocation &optional buffer)
  "Install INVOCATION's frozen request locals in BUFFER.
BUFFER defaults to INVOCATION's retained conversation buffer."
  (let ((configuration
         (niwa-agent-invocation-frozen-configuration invocation))
        (buffer (or buffer
                    (niwa-agent-invocation-buffer invocation))))
    (unless (niwa-agent-configuration-p configuration)
      (error "Agent request configuration is not frozen"))
    (let ((locals
           (niwa-agent-configuration-request-locals configuration)))
      (unless (niwa-agent-request-locals-p locals)
        (error "Agent request-local configuration is invalid"))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local niwa--conversation-working-directory
                      (niwa-agent-invocation-working-directory invocation))
          (setq default-directory
                (niwa-conversation-working-directory
                 (niwa-agent-invocation-parent-session invocation)))
          (setq-local niwa--conversation-instruction-root nil)
          (dolist (symbol niwa-agent-request-local-symbols)
            (when-let* ((entry (assq symbol locals)))
              (set (make-local-variable symbol) (cdr entry))))
          ;; A direct Agent advertises the roster this child inherited.
          (niwa-agents-install-agent-schema))))))


;;
;;; User-role injections

(defun niwa-agent-conversation-insert-user-block
    (invocation block &optional marker)
  "Insert BLOCK as a user-role region in INVOCATION's conversation.

Insert at live MARKER or append at point-max.  Clear inherited
assistant-response properties.  The mailbox owner checkpoints the complete
batch with its queue removal.  Return non-nil on success; report errors and
return nil so delivery can refuse to consume missing transcript records."
  (when (and (niwa-agent-invocation-p invocation)
             (stringp block)
             (not (string-empty-p block)))
    (when-let* ((buffer (niwa-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (condition-case err
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (niwa--insert-user-role-block-at-marker block marker)
              t))
        (error
         (niwa-diagnostic-message
          (niwa-diagnostic-scope
           (niwa-agent-invocation-parent-session invocation))
          "Conversation user-block insert failed: %S" err)
         nil)))))


;;
;;; Activity and presentation

(defun niwa-agent-conversation--cache-render-data-bounds
    (invocation beg end)
  "Cache render-data bounds BEG and END on INVOCATION."
  (when (niwa-agent-invocation-p invocation)
    (let ((start (copy-marker beg nil))
          (finish (copy-marker end t)))
      (setf (niwa-agent-invocation-render-data-start-marker invocation)
            start
            (niwa-agent-invocation-render-data-end-marker invocation)
            finish)
      (cons start finish))))

(defun niwa-agent-conversation--cached-render-data-bounds
    (invocation parent-buffer agent-id)
  "Return valid cached bounds for AGENT-ID in PARENT-BUFFER."
  (when (niwa-agent-invocation-p invocation)
    (let ((beg (niwa-agent-invocation-render-data-start-marker invocation))
          (end (niwa-agent-invocation-render-data-end-marker invocation)))
      (when (and (markerp beg) (markerp end)
                 (eq (marker-buffer beg) parent-buffer)
                 (eq (marker-buffer end) parent-buffer)
                 (marker-position beg) (marker-position end)
                 (< (marker-position beg) (marker-position end)))
        (let* ((raw (buffer-substring beg end))
               (parsed
                (niwa-tool-render-data-extract
                 raw nil
                 (niwa-agent-invocation-parent-tool-use-id invocation))))
          (if (and (stringp (car parsed))
                   (string-empty-p (string-trim (car parsed)))
                   (listp (cdr parsed))
                   (equal (plist-get (cdr parsed) :agent-id) agent-id))
              (cons beg end)
            (setf (niwa-agent-invocation-render-data-start-marker
                   invocation) nil
                  (niwa-agent-invocation-render-data-end-marker
                   invocation) nil)
            nil))))))

(defun niwa-agent-conversation--render-data-bounds
    (invocation agent-id)
  "Return current render-data bounds for INVOCATION and AGENT-ID."
  (or (niwa-agent-conversation--cached-render-data-bounds
       invocation (current-buffer) agent-id)
      (when-let* ((bounds
                   (niwa-tool-render-data-find-agent-block
                    agent-id)))
        (niwa-agent-conversation--cache-render-data-bounds
         invocation (car bounds) (cdr bounds)))))

(defun niwa-agent-conversation--activity-tool-name (args)
  "Return the first tool name found in hook ARGS."
  (catch 'name
    (dolist (arg args)
      (cond
       ((and (listp arg) (plist-get arg :name))
        (throw 'name (format "%s" (plist-get arg :name))))
       ((and (listp arg) (plist-get arg :tool-name))
        (throw 'name (format "%s" (plist-get arg :tool-name))))
       ((and (fboundp 'gptel-tool-name)
             (ignore-errors (gptel-tool-name arg)))
        (throw 'name (format "%s" (gptel-tool-name arg))))))))

(defun niwa-agent-conversation--activity-error-p (args)
  "Return non-nil when post-tool hook ARGS report a tool error.
gptel passes one plist whose :result holds the serialized result."
  (cl-some (lambda (arg)
             (let ((result (if (stringp arg) arg
                             (and (plistp arg) (plist-get arg :result)))))
               (and (stringp result) (string-prefix-p "Error:" result))))
           args))

(defun niwa-agent-conversation--activity-sanitize-item (item)
  "Return sanitized activity ITEM, or nil when it is not recordable."
  (when (and (listp item)
             (memq (plist-get item :type)
                   '(tool-start tool-finish tool-error waiting message status)))
    (let ((copy (copy-sequence item)))
      (dolist (key '(:tool-name :summary :from :status :error))
        (when (plist-member copy key)
          (setq copy
                (plist-put
                 copy key
                 (string-trim
                  (replace-regexp-in-string
                   "[\n\r\t ]+" " " (format "%s" (plist-get copy key))))))))
      copy)))

(defun niwa-agent-conversation--sync-entry (invocation)
  "Sync INVOCATION's live metadata into its historical transcript entry."
  (when-let* (((niwa-agent-invocation-p invocation))
              (session (niwa-agent-invocation-parent-session invocation))
              (agent-id (niwa-agent-invocation-agent-id invocation)))
    (let* ((started (niwa-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (niwa-agent-invocation-terminal-reason invocation))
           (verdict (niwa-agent-invocation-verdict invocation))
           (updates
            (list :status
                  (or (niwa-agent-invocation-transcript-status invocation)
                      'running)
                  :calls (or (niwa-agent-invocation-call-count invocation) 0)
                  :updated-at (format-time-string "%FT%H-%M-%S"))))
      (when elapsed
        (setq updates (plist-put updates :elapsed elapsed)))
      (when reason
        (setq updates (plist-put updates :reason reason)))
      (when verdict
        (setq updates (plist-put updates :verdict verdict)))
      (niwa-session-persistence-update-transcript-entry
       session agent-id updates))))

(defun niwa-agent-conversation-record-activity
    (invocation item &optional suppress-rerender)
  "Append activity ITEM to INVOCATION.
When SUPPRESS-RERENDER is non-nil, do not schedule a parent view refresh."
  (when (and (niwa-agent-invocation-p invocation)
             (buffer-live-p
              (niwa-agent-invocation-parent-data-buffer invocation)))
    (when-let* ((clean
                 (niwa-agent-conversation--activity-sanitize-item item)))
      (setf (niwa-agent-invocation-activity invocation)
            (last (append (niwa-agent-invocation-activity invocation)
                          (list (plist-put clean :time (float-time))))
                  niwa-agent-conversation--live-activity-limit))
      (niwa-agent-conversation--sync-entry invocation)
      (unless suppress-rerender
        (when-let* ((parent
                     (niwa-agent-invocation-parent-data-buffer invocation))
                    ((buffer-live-p parent))
                    (path (niwa-agent-invocation-path invocation))
                    (view (buffer-local-value 'niwa--view-buffer parent))
                    ((buffer-live-p view)))
          (niwa-view-refresh-agent-rendering view path))))))

(defun niwa-agent-conversation-refresh (invocation)
  "Persist and redraw INVOCATION's live conversation presentation."
  (let ((parent (niwa-agent-invocation-parent-data-buffer invocation))
        (agent-id (niwa-agent-invocation-agent-id invocation))
        patched-summary-p
        patched-render-data-p)
    (niwa-agent-conversation--sync-entry invocation)
    (when (and (buffer-live-p parent) agent-id)
      (condition-case err
          (with-current-buffer parent
            (when-let* ((tool-use-id
                         (niwa-agent-invocation-parent-tool-use-id
                          invocation))
                        (summary
                         (car
                          (niwa-agent-invocation-sandbox-summary-cell
                           invocation)))
                        ((niwa-execution-telemetry-sandbox-summary-class
                          summary))
                        ((not
                          (equal
                           summary
                           (plist-get
                            (niwa-tool-render-data-for-tool
                             parent tool-use-id)
                            :sandbox-summary)))))
              (let ((updates
                     (list :sandbox-summary (copy-tree summary))))
                (setq patched-summary-p
                      (niwa-tool-render-data-update
                       parent tool-use-id updates))
                (unless patched-summary-p
                  (niwa-execution-transcript-store-pending-terminal
                   parent (list :tool-use-id tool-use-id) updates)
                  ;; A retained agent may outlive the parent's compacted row.
                  ;; The transcript owner settles that update in its archive.
                  (niwa-execution-transcript-retry-pending-terminals parent))))
            (when-let* ((bounds
                         (niwa-agent-conversation--render-data-bounds
                          invocation agent-id)))
              (let* ((beg (car bounds))
                     (end (cdr bounds))
                     (parsed
                      (niwa-tool-render-data-extract
                       (buffer-substring beg end)
                       nil
                       (niwa-agent-invocation-parent-tool-use-id
                        invocation)))
                     (updated (copy-sequence (cdr parsed)))
                     (started
                      (niwa-agent-invocation-started-at invocation))
                     (elapsed
                      (and started
                           (float-time
                            (time-subtract (current-time) started)))))
                (when (listp updated)
                  (setq updated
                        (plist-put
                         updated :status
                         (or (niwa-agent-invocation-transcript-status
                              invocation)
                             'running)))
                  (setq updated
                        (plist-put
                         updated :calls
                         (or (niwa-agent-invocation-call-count invocation)
                             0)))
                  (dolist (pair
                           `((:elapsed . ,elapsed)
                             (:reason . ,(niwa-agent-invocation-terminal-reason
                                          invocation))
                             (:verdict . ,(niwa-agent-invocation-verdict
                                           invocation))))
                    (when (cdr pair)
                      (setq updated
                            (plist-put updated (car pair) (cdr pair)))))
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (niwa-tool-render-data-patch-block
                     beg end updated)
                    (setq patched-render-data-p t)))))
            (when-let* ((view (and (boundp 'niwa--view-buffer)
                                    niwa--view-buffer))
                        ((buffer-live-p view)))
              ;; A retained handle has marker-backed source coordinates, so
              ;; replacing its metadata need not rebuild unrelated history.
              ;; A summary without retained metadata changed only its own
              ;; tool row, which may be a compound call's child: redraw that
              ;; row.  Its counters change with every sandboxed command the
              ;; agent runs, and the full re-render this used to request took
              ;; 1.6 s of a long session's view each time.  A row the view
              ;; cannot find as a tool row, such as a `Started' agent handle,
              ;; gets the coalesced handle refresh; a full projection here
              ;; would remove and re-add the running row on every command.
              (unless (and patched-summary-p (not patched-render-data-p)
                           (with-current-buffer view
                             (niwa-view--refresh-tool-row
                              parent
                              (niwa-agent-invocation-parent-tool-use-id
                               invocation))))
                (niwa-view-refresh-agent-rendering
                 view (niwa-agent-invocation-path invocation)))))
        (error
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope
           (niwa-agent-invocation-parent-session invocation)) 'niwa
          (format "Agent conversation refresh failed for %s: %S"
                  agent-id err)
          :warning))))))


;;
;;; Response and persistence

(defun niwa-agent-conversation--response-property-p (property)
  "Return non-nil when PROPERTY denotes a gptel assistant response."
  (or (eq property 'response)
      (let ((tail property)
            found)
        (while (consp tail)
          (if (eq (car tail) 'response)
              (setq found t tail nil)
            (setq tail (cdr tail))))
        found)))

(defun niwa-agent-conversation-final-response (invocation)
  "Return INVOCATION's final assistant response from its conversation."
  (when-let* (((niwa-agent-invocation-p invocation))
              (buffer (niwa-agent-invocation-buffer invocation))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((position (point-min))
            start end)
        (while (< position (point-max))
          (let* ((property (get-text-property position 'gptel))
                 (next (or (next-single-property-change
                            position 'gptel nil (point-max))
                           (point-max))))
            (when (niwa-agent-conversation--response-property-p property)
              (setq start position end next))
            (setq position next)))
        (when (and start end)
          (let ((text
                 (string-trim
                  (buffer-substring-no-properties start end))))
            (unless (string-empty-p text) text)))))))

(defun niwa-agent-conversation-project-history (buffer &optional session)
  "Return concise redacted Markdown projected from live BUFFER.

Canonical user, assistant, tool, and mailbox transcript segments are projected.
Transcript classification and tool-result projection remain owned by
`niwa-transcript'; ignored control text, provider metadata, Org scaffolding,
and private media paths therefore do not become history content."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((segments
             (cl-loop for segment in
                      (niwa-transcript-segments (point-min) (point-max))
                      when (memq (car segment) '(user response tool mailbox))
                      collect segment)))
        (replace-regexp-in-string
         "; path [^]]+" ""
         (substring-no-properties
          (niwa-transcript-project-segments
           segments
           :tool-results-dir
           (and session
                (niwa-session-save-path session)
                (file-name-concat (niwa-session-save-path session)
                                  "tool-results")))))))))

(defun niwa-agent-conversation-history-markdown (invocation)
  "Return concise Markdown for retained agent INVOCATION's conversation.

Return nil when INVOCATION has no live conversation buffer."
  (when-let* (((niwa-agent-invocation-p invocation))
              (buffer (niwa-agent-invocation-buffer invocation)))
    (niwa-agent-conversation-project-history
     buffer (niwa-agent-invocation-parent-session invocation))))

(defun niwa-agent-conversation-fenced-p (invocation)
  "Return non-nil when INVOCATION's admitted ownership epoch is superseded."
  (when-let* ((session (niwa-agent-invocation-parent-session invocation)))
    (let ((admitted (niwa-agent-invocation-ownership-epoch invocation))
          (current (niwa-session-ownership-epoch session)))
      (or (and admitted current (not (equal admitted current)))
          (and (memq (plist-get (niwa-session-lease session) :state) '(lost foreign))
               (memq (plist-get (niwa-session-control-transfer session) :state)
                     '(taken-over following)))))))

(defun niwa-agent-conversation--cancel-save (invocation)
  "Cancel INVOCATION's pending debounced save."
  (when (niwa-agent-invocation-p invocation)
    (let ((timer
           (niwa-agent-invocation-transcript-save-timer invocation)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (niwa-agent-invocation-transcript-save-timer invocation) nil))))

(defun niwa-agent-conversation--owns-transcript-p (invocation)
  "Return non-nil when INVOCATION's live buffer is its canonical transcript."
  (let ((buffer (niwa-agent-invocation-buffer invocation))
        (relative (niwa-agent-invocation-transcript-relative-path invocation))
        (session (niwa-agent-invocation-parent-session invocation)))
    (and (buffer-live-p buffer) relative session
         (niwa-session-save-path session)
         (with-current-buffer buffer
           (and buffer-file-name
                (niwa--same-file-p
                 buffer-file-name
                 (expand-file-name relative (niwa-session-save-path session))))))))

(defun niwa-agent-conversation--freeze-preamble (invocation session)
  "Settle INVOCATION's tentative state and return its first checkpoint.
Call with the transcript buffer current and SESSION's mutation gate held."
  (niwa-ptc-checkpoint-clear-settled
   session (niwa-agent-invocation-path invocation))
  (prog1 (niwa-session-checkpoint-capture
          session (niwa-agent-invocation-path invocation))
    (niwa-session-persistence-update-transcript-entry
     session (niwa-agent-invocation-agent-id invocation)
     (list :updated-at (format-time-string "%FT%H-%M-%S")))))

(defun niwa-agent-conversation--freeze-content (invocation session)
  "Run save hooks and freeze the current modified transcript buffer.
Return (:content STRING :frozen-tick TICK :checkpoint CHECKPOINT)."
  (when (bound-and-true-p gptel-mode)
    (gptel--save-state))
  (let ((inhibit-message t)
        (message-log-max nil)
        (before-save-hook (remq 'gptel--save-state before-save-hook)))
    (run-hooks 'before-save-hook)
    ;; The full modification tick: a property-only edit after the freeze
    ;; changes `gptel' bounds the frozen content does not carry.
    (list :frozen-tick (buffer-modified-tick)
          :content (buffer-substring-no-properties (point-min) (point-max))
          :checkpoint (niwa-session-checkpoint-capture
                       session (niwa-agent-invocation-path invocation)))))

(defun niwa-agent-conversation--finish-save (frozen-tick)
  "Mark the current transcript buffer saved if unchanged since FROZEN-TICK.
FROZEN-TICK is a `buffer-modified-tick', so text and property edits count."
  (set-visited-file-modtime 0)
  (when (= frozen-tick (buffer-modified-tick))
    (set-buffer-modified-p nil))
  (let ((inhibit-message t) (message-log-max nil))
    (run-hooks 'after-save-hook)))

(defun niwa-agent-conversation-prepare-save (invocation)
  "Freeze INVOCATION's retained transcript for an asynchronous portable commit.
Return nil when the warm portable path does not apply (cold, workspace-free, no
committed sidecar or no live root), so the caller saves synchronously.
Otherwise return (:artifacts ARTIFACTS :finish FUNCTION).  ARTIFACTS is the
transcript, when modified, followed by the sidecar commit marker carrying the
frozen checkpoint.  FUNCTION takes no arguments and runs only after the commit
is adopted: it marks the buffer saved when unchanged since the freeze, runs
`after-save-hook' and clears the sidecar-dirty flag.  A failed commit never
calls it, so the buffer stays modified.  Call with the session's mutation gate
held and its checkpoint exclusion established by the caller; any pending
debounced save is cancelled because this freeze supersedes it."
  (when (niwa-agent-invocation-p invocation)
    (let* ((session (niwa-agent-invocation-parent-session invocation))
           (buffer (niwa-agent-invocation-buffer invocation))
           (parent (niwa-agent-invocation-parent-data-buffer invocation))
           (root (and session (or (niwa-session-root-buffer session) parent)))
           (scope (niwa-diagnostic-scope session)))
      (when (and session
                 (niwa-agent-conversation--owns-transcript-p invocation)
                 (niwa-session-codec-portable-authority-p session)
                 (buffer-live-p root)
                 (niwa-session-artifacts-artifact-present-p
                  session "session.meta.el" t))
        (niwa-agent-conversation--cancel-save invocation)
        (with-current-buffer buffer
          (let* ((checkpoint
                  (niwa-agent-conversation--freeze-preamble
                   invocation session))
                 (frozen (and (buffer-modified-p)
                              (niwa-agent-conversation--freeze-content
                               invocation session)))
                 (frozen-tick (plist-get frozen :frozen-tick))
                 (checkpoint (if frozen (plist-get frozen :checkpoint) checkpoint)))
            (list
             :artifacts
             (append
              (and frozen
                   (list (list :path (expand-file-name buffer-file-name)
                               :content (plist-get frozen :content)
                               :coding 'utf-8-unix)))
              (list (niwa-session-artifacts--sidecar-artifact
                     session root checkpoint)))
             :finish
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when frozen
                     (condition-case err
                         (niwa-agent-conversation--finish-save frozen-tick)
                       (error
                        (niwa--warn-once
                         scope 'agent-conversation-post-save
                         "Agent transcript post-save cleanup failed: %s"
                         (error-message-string err)))))))
               (setf (niwa-agent-invocation-sidecar-dirty invocation) nil)))))))))

(defun niwa-agent-conversation--drain-gate (invocation token)
  "Drain INVOCATION's session publication work before it is frozen.
Return non-nil when the caller may capture.  An unresolved job reports and
returns nil.  TOKEN is the caller's gate ownership, which is exempt."
  (let ((session (niwa-agent-invocation-parent-session invocation)))
    (or (not (and session
                  (niwa-session-save-path session)
                  (niwa-session-codec-portable-authority-p session)))
        (condition-case err
            (progn (niwa-session-publication-barrier session token) t)
          (error
           (setf (niwa-agent-invocation-persistence-failure invocation)
                 (format "publication gate: %s" (error-message-string err)))
           (niwa-diagnostic-message (niwa-diagnostic-scope session)
                    "Conversation save deferred for %s: %s"
                    (niwa-agent-invocation-agent-id invocation)
                    (error-message-string err))
           nil)))))

(defun niwa-agent-conversation--write (invocation &optional token)
  "Write INVOCATION's retained conversation, returning non-nil on success.
The session's publication gate drains first, so a concurrent job cannot have
captured a stale agent candidate.  TOKEN is the caller's publication gate
ownership, handed to gated writers."
  ;; A reason from an earlier save must not explain this one.
  (when (niwa-agent-invocation-p invocation)
    (setf (niwa-agent-invocation-persistence-failure invocation) nil))
  (when (and (niwa-agent-invocation-p invocation)
             (not (niwa-agent-conversation-fenced-p invocation))
             (niwa-agent-conversation--drain-gate invocation token))
    (niwa-session-artifacts-call-with-checkpoint
     (niwa-agent-invocation-parent-session invocation)
     (lambda ()
       (let ((buffer (niwa-agent-invocation-buffer invocation))
             (relative
              (niwa-agent-invocation-transcript-relative-path invocation))
             (session (niwa-agent-invocation-parent-session invocation))
             (parent
              (niwa-agent-invocation-parent-data-buffer invocation)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and buffer-file-name relative session
                        (niwa-session-save-path session)
                        (niwa--same-file-p
                         buffer-file-name
                         (expand-file-name
                          relative (niwa-session-save-path session)))
                        ;; Only durable workspaces publish transcripts.
                        (niwa-session-codec-portable-authority-p session))
               (condition-case err
                   (let ((checkpoint
                          (niwa-agent-conversation--freeze-preamble
                           invocation session)))
                     (when (buffer-modified-p)
                       (let* ((inhibit-message t)
                              (message-log-max nil)
                              (frozen (niwa-agent-conversation--freeze-content
                                       invocation session))
                              (content (plist-get frozen :content))
                              (root (or (niwa-session-root-buffer session) parent)))
                         (setq checkpoint (plist-get frozen :checkpoint))
                         (if (and (buffer-live-p root)
                                  (niwa-session-artifacts-artifact-present-p
                                   session "session.meta.el" t))
                             (progn
                               (niwa-session-artifacts-publish-transcript-state
                                session root buffer-file-name content checkpoint
                                'utf-8-unix t token)
                               (setq checkpoint nil)
                               (setf (niwa-agent-invocation-sidecar-dirty invocation)
                                     nil))
                           (niwa-session-artifacts-publish-text
                            session buffer-file-name content 'utf-8-unix t token))
                         (niwa-agent-conversation--finish-save
                          (plist-get frozen :frozen-tick))))
                     (when (and (or checkpoint
                                    (niwa-agent-invocation-sidecar-dirty invocation))
                                (buffer-live-p parent)
                                (niwa-session-persistence-write-sidecar-now
                                 session parent t checkpoint token))
                       (setf (niwa-agent-invocation-sidecar-dirty invocation)
                             nil))
                     t)
		 (niwa-session-control-fs-busy
		  ;; Reached from a timer or sentinel inside a remote command:
		  ;; the control filesystem refused to nest and nothing was
		  ;; written, so write again once the transport is free.
                  (setf (niwa-agent-invocation-persistence-failure invocation)
                        "control filesystem busy")
		  (niwa-transport-run-when-idle
                   (list 'agent-conversation-save
			 (niwa-agent-invocation-agent-id invocation))
                   (niwa-session-save-path session)
                   (lambda () (niwa-agent-conversation--write invocation)))
		  nil)
		 (error
                  (setf (niwa-agent-invocation-persistence-failure invocation)
                        (error-message-string err))
		  (niwa-diagnostic-message (niwa-diagnostic-scope session)
                           "Conversation save failed for %s: %S"
                           (niwa-agent-invocation-agent-id invocation) err)
		  nil))))))))))

(defun niwa-agent-conversation-save (invocation &optional deferred token)
  "Save INVOCATION's retained conversation.
When DEFERRED is non-nil, coalesce writes through an idle timer.  TOKEN is the
caller's publication gate ownership for an immediate write; a timer write runs
later and never carries it."
  (if (niwa-agent-conversation-fenced-p invocation)
      (niwa-agent-conversation--cancel-save invocation)
    (if (and deferred
             (numberp niwa-agent-conversation-save-debounce)
             (> niwa-agent-conversation-save-debounce 0))
        (progn
          (niwa-agent-conversation--cancel-save invocation)
          (setf
           (niwa-agent-invocation-transcript-save-timer invocation)
           (run-with-idle-timer
            niwa-agent-conversation-save-debounce nil
            (lambda (inv)
              (when (niwa-agent-invocation-p inv)
                (setf (niwa-agent-invocation-transcript-save-timer inv) nil)
                (niwa-agent-conversation--write inv)))
            invocation)))
      (niwa-agent-conversation--cancel-save invocation)
      (niwa-agent-conversation--write invocation token))))

(defun niwa-agent-conversation--owes-p (invocation)
  "Return non-nil when INVOCATION's canonical transcript has unpublished bytes."
  (and (niwa-agent-conversation--owns-transcript-p invocation)
       (with-current-buffer (niwa-agent-invocation-buffer invocation)
         (or (buffer-modified-p)
             (niwa-agent-invocation-sidecar-dirty invocation)))))

(defun niwa-agent-conversation--unsaved-reason (invocation)
  "Save INVOCATION if it owes bytes and return why it still does, or nil.
Return nil when nothing is owed or the save fully published it.  Otherwise
return a user-facing message naming the recovery commands.  The save drains
the session's publication gate first, so an unresolved job refuses here."
  (when (niwa-agent-conversation--owes-p invocation)
    (let ((canceled nil))
      (condition-case nil
          (niwa-agent-conversation-save invocation)
        (error nil)
        (quit (setq canceled t)))
      (when (or canceled (niwa-agent-conversation--owes-p invocation))
        (format (concat "agent transcript %s has unpublished changes (%s); run "
                        "niwa-session-publication-retry or "
                        "niwa-session-publication-abandon, then try again")
                (niwa-agent-invocation-agent-id invocation)
                (if canceled
                    "the save was canceled"
                  "publication failed or is unresolved"))))))

(defun niwa-agent-conversation-allow-buffer-kill-p ()
  "Return non-nil when the current retained agent transcript may be killed.
A `kill-buffer-query-functions' member.  Unpublished bytes are saved first; a
failed or unresolved publication refuses the kill with a message, keeping the
buffer so explicit recovery stays possible."
  (or (not (niwa-agent-invocation-p niwa--agent-invocation))
      (if-let* ((reason (niwa-agent-conversation--unsaved-reason
                         niwa--agent-invocation)))
          (progn
            (niwa-diagnostic-message
             (niwa-diagnostic-scope
              (niwa-agent-invocation-parent-session niwa--agent-invocation))
             "%s" reason)
            nil)
        t)))

(defun niwa-agent-conversation--preserve-unsaved (invocation)
  "Write INVOCATION's unpublished transcript bytes to a recovery file.
Return the file name.  The kill hook calls it when a save failed, so closing
the buffer cannot silently discard the bytes."
  (with-current-buffer (niwa-agent-invocation-buffer invocation)
    (let ((file (make-temp-file "niwa-unsaved-agent-" nil ".chat.org")))
      (write-region (point-min) (point-max) file nil 'silent)
      file)))

(defun niwa-agent-conversation--install-kill-guards ()
  "Install the current agent buffer's kill query and kill hook."
  (add-hook 'kill-buffer-query-functions
            #'niwa-agent-conversation-allow-buffer-kill-p nil t)
  (add-hook 'kill-buffer-hook
            #'niwa-agent-conversation--on-buffer-kill nil t))

(defun niwa-agent-conversation--on-buffer-kill ()
  "Persist and abort any live request owned by the current conversation.
A save that leaves bytes unpublished is reported with a recovery file, never
discarded silently."
  (when (niwa-agent-invocation-p niwa--agent-invocation)
    (condition-case nil
        (niwa-agent-conversation-save niwa--agent-invocation)
      ((error quit) nil))
    (when (niwa-agent-conversation--owes-p niwa--agent-invocation)
      (niwa-diagnostic-warning
       (niwa-diagnostic-scope
        (niwa-agent-invocation-parent-session niwa--agent-invocation)) 'niwa
       (format "Closing %s left unpublished changes; recovered to %s"
               (niwa-agent-invocation-agent-id niwa--agent-invocation)
               (niwa-agent-conversation--preserve-unsaved
                niwa--agent-invocation))
       :warning)))
  (when (and (niwa-agent-invocation-p niwa--agent-invocation)
             (boundp 'gptel--request-alist)
             (cl-some
              (lambda (entry)
                (let* ((fsm (cadr entry))
                       (info (and fsm (gptel-fsm-info fsm))))
                  (eq (and info (plist-get info :buffer))
                      (current-buffer))))
              gptel--request-alist))
    (condition-case nil
        (gptel-abort (current-buffer))
      (error nil))))

(provide 'niwa-agent-conversation)

;;; niwa-agent-conversation.el ends here
