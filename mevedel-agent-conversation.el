;;; mevedel-agent-conversation.el --- Retained agent conversations -*- lexical-binding: t -*-

;;; Commentary:

;; Owns retained agent conversation buffers: creation and hydration, frozen
;; request-local installation, transcript saves, final response extraction,
;; activity history, and live presentation refresh.  Provider request/FSM
;; execution remains in `mevedel-agent-exec'.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-agents)
  (require 'subr-x))

;; `gptel'
(declare-function gptel--save-state "ext:gptel" ())
(declare-function gptel-mode "ext:gptel" (&optional arg))
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
(defvar gptel-org-convert-response)

;; `mevedel-agents'
(declare-function mevedel-agent-configuration-p
                  "mevedel-agents" (cl-x))
(declare-function mevedel-agent-configuration-request-locals
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-frozen-configuration
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-end-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-start-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-save-timer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agents-set-specs
                  "mevedel-agents" (specs))
(declare-function mevedel-agents-specs "mevedel-agents" (&optional buffer))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--find-render-data-block-by-agent-id
                  "mevedel-pipeline" (agent-id))
(declare-function mevedel-pipeline--patch-render-data-block
                  "mevedel-pipeline" (beg end new-plist))
(declare-function mevedel-pipeline-extract-render-data
                  "mevedel-pipeline" (result-string))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--property-delete-direct
                  "mevedel-session-persistence" (property))
(declare-function mevedel-session-persistence--stabilize-gptel-bounds
                  "mevedel-session-persistence" ())
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-skills-prompt'
(declare-function mevedel-skills-install-activation-hook
                  "mevedel-skills-prompt" ())

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-clear-ledger
                  "mevedel-tool-repair" (&rest args))
(declare-function mevedel-tool-repair-post-tool-call
                  "mevedel-tool-repair" (&rest args))
(declare-function mevedel-tool-repair-pre-tool-call
                  "mevedel-tool-repair" (&rest args))

;; `mevedel-transcript'
(declare-function mevedel-transcript-normalize-properties
                  "mevedel-transcript" ())

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
                  "mevedel-transcript-restore" ())

;; `mevedel-utilities'
(declare-function mevedel--insert-user-role-block-at-marker
                  "mevedel-utilities" (block &optional marker))
(declare-function mevedel--optimize-transcript-buffer
                  "mevedel-utilities" ())
(declare-function mevedel--same-file-p
                  "mevedel-utilities" (file-a file-b))
(declare-function mevedel--transcript-org-mode
                  "mevedel-utilities" ())

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-live-transcript-post-tool
                  "mevedel-view-agent" (args))
(declare-function mevedel-view-agent-live-transcript-pre-tool
                  "mevedel-view-agent" (args))
(declare-function mevedel-view-agent-live-transcript-stream
                  "mevedel-view-agent" ())
(declare-function mevedel-view-refresh-agent-rendering
                  "mevedel-view-agent" (view-buffer agent-id))

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-retry-execution-terminals
                  "mevedel-view-stream" (&rest args))

;; `org-element'
(declare-function org-element-cache-reset "ext:org-element"
                  (&optional all no-persistence))
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

;; `undo-tree'
(defvar undo-tree-auto-save-history)


(defcustom mevedel-agent-conversation-save-debounce 2.0
  "Idle seconds before saving a running retained agent conversation.
A non-positive value saves immediately.  Terminal paths always save now."
  :type 'number
  :group 'mevedel)

(defconst mevedel-agent-conversation--live-activity-limit 5
  "Maximum recent activity items mirrored into live agent metadata.")

(defvar-local mevedel--agent-invocation nil
  "Invocation that owns this retained agent conversation buffer.")


;;
;;; Buffer lifecycle

(defun mevedel-agent-conversation-open (invocation parent-data-buffer)
  "Create and configure INVOCATION's conversation below PARENT-DATA-BUFFER."
  (let* ((agent-id (mevedel-agent-invocation-agent-id invocation))
         (suffix (and (stringp agent-id)
                      (cadr (split-string agent-id "--" t))))
         (short-id (if (stringp suffix)
                       (substring suffix 0 (min 8 (length suffix)))
                     "anon"))
         (buffer (generate-new-buffer
                  (format "*mevedel-agent-%s*" short-id)))
         (parent-session
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--session parent-data-buffer)))
         (parent-workspace
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--workspace parent-data-buffer)))
         (parent-view
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--view-buffer parent-data-buffer)))
         (parent-specs
          (and (buffer-live-p parent-data-buffer)
               (mevedel-agents-specs parent-data-buffer))))
    (with-current-buffer buffer
      (let ((org-element-use-cache nil)
            (org-element-cache-persistent nil))
        (require 'mevedel-utilities)
        (mevedel--transcript-org-mode))
      (when (fboundp 'org-element-cache-reset)
        (let ((org-element-use-cache t))
          (ignore-errors
            (org-element-cache-reset nil 'no-persistence))))
      (setq-local org-element-use-cache nil)
      (setq-local org-element-cache-persistent nil)
      (setq-local gptel-org-convert-response nil)
      (setq-local gptel-org-branching-context nil)
      (setq-local gptel-org-ignore-elements '(property-drawer))
      (mevedel--optimize-transcript-buffer)
      (unless (require 'gptel nil t)
        (kill-buffer buffer)
        (error "Could not load gptel for sub-agent"))
      (condition-case err
          (gptel-mode +1)
        (error
         (kill-buffer buffer)
         (signal (car err) (cdr err))))
      (when parent-session
        (setq-local mevedel--session parent-session))
      (when parent-workspace
        (setq-local mevedel--workspace parent-workspace))
      (when (and parent-view (buffer-live-p parent-view))
        (setq-local mevedel--view-buffer parent-view))
      (when parent-specs
        (mevedel-agents-set-specs parent-specs))
      (setq-local mevedel--agent-invocation invocation)
      (when (require 'mevedel-skills-prompt nil t)
        (mevedel-skills-install-activation-hook))
      (require 'mevedel-tool-repair)
      (add-hook 'gptel-pre-tool-call-functions
                #'mevedel-tool-repair-pre-tool-call -100 t)
      (add-hook 'gptel-post-tool-call-functions
                #'mevedel-tool-repair-post-tool-call -100 t)
      (add-hook 'gptel-post-response-functions
                #'mevedel-tool-repair-clear-ledger nil t)
      (add-hook 'kill-buffer-hook #'mevedel-tool-repair-clear-ledger nil t)
      (add-hook 'gptel-post-stream-hook
                #'mevedel-view-agent-live-transcript-stream nil t)
      (add-hook 'gptel-pre-tool-call-functions
                #'mevedel-view-agent-live-transcript-pre-tool nil t)
      (add-hook 'gptel-post-tool-call-functions
                #'mevedel-view-agent-live-transcript-post-tool nil t)
      (require 'mevedel-view-stream)
      (add-hook 'gptel-post-response-functions
                #'mevedel-view-stream-retry-execution-terminals nil t)
      (let ((inv invocation))
        (add-hook
         'gptel-pre-tool-call-functions
         (lambda (&rest args)
           (prog1 nil
             (when (mevedel-agent-invocation-p inv)
               (cl-incf (mevedel-agent-invocation-call-count inv))
               (let ((tool-name
                      (or (mevedel-agent-conversation--activity-tool-name args)
                          "Tool")))
                 (mevedel-agent-conversation-record-activity
                  inv (list :type 'tool-start
                            :tool-name tool-name
                            :summary (format "%s(...)" tool-name))
                  t))
               (mevedel-agent-conversation-refresh inv))))
         nil t)
        (add-hook
         'gptel-post-tool-call-functions
         (lambda (&rest args)
           (prog1 nil
             (when (mevedel-agent-invocation-p inv)
               (let ((tool-name
                      (or (mevedel-agent-conversation--activity-tool-name args)
                          "Tool")))
                 (mevedel-agent-conversation-record-activity
                  inv
                  (if (mevedel-agent-conversation--activity-error-p args)
                      (list :type 'tool-error :tool-name tool-name
                            :error (format "%s failed" tool-name))
                    (list :type 'tool-finish :tool-name tool-name
                          :summary (format "%s done" tool-name)))
                  t))
               (mevedel-agent-conversation-refresh inv))))
         nil t)
        (add-hook
         'gptel-post-response-functions
         (lambda (&rest _)
           (prog1 nil
             (when (mevedel-agent-invocation-p inv)
               (mevedel-agent-conversation-refresh inv))))
         nil t))
      (add-hook 'kill-buffer-hook
                #'mevedel-agent-conversation--on-buffer-kill nil t))
    buffer))

(defun mevedel-agent-conversation-hydrate
    (invocation parent-data-buffer absolute-path)
  "Restore INVOCATION below PARENT-DATA-BUFFER from ABSOLUTE-PATH.
Return the hydrated conversation buffer."
  (let ((buffer (mevedel-agent-conversation-open
                 invocation parent-data-buffer)))
    (condition-case err
        (progn
          (setf (mevedel-agent-invocation-buffer invocation) buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-file-contents absolute-path))
            (set-visited-file-name absolute-path t)
            (require 'mevedel-transcript)
            (require 'mevedel-transcript-restore)
            (mevedel-transcript-restore-gptel-state)
            (mevedel-transcript-normalize-properties)
            (mevedel-agent-conversation-configure invocation)
            (require 'mevedel-session-persistence)
            (mevedel-session-persistence--property-delete-direct
             "GPTEL_SYSTEM")
            (mevedel-session-persistence--stabilize-gptel-bounds)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
          buffer)
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (signal (car err) (cdr err))))))

(defun mevedel-agent-conversation-configure (invocation &optional buffer)
  "Install INVOCATION's frozen request locals in BUFFER.
BUFFER defaults to INVOCATION's retained conversation buffer."
  (let ((configuration
         (mevedel-agent-invocation-frozen-configuration invocation))
        (buffer (or buffer
                    (mevedel-agent-invocation-buffer invocation))))
    (unless (mevedel-agent-configuration-p configuration)
      (error "Agent request configuration is not frozen"))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (entry
                 (mevedel-agent-configuration-request-locals configuration))
          (set (make-local-variable (car entry)) (cdr entry)))))))


;;
;;; User-role injections

(defun mevedel-agent-conversation-insert-user-block
    (invocation block &optional marker)
  "Insert BLOCK as a user-role region in INVOCATION's conversation.

Insert at live MARKER or append at point-max.  Clear inherited
assistant-response properties and save the transcript before the next provider
request.  Buffer-write errors are logged and ignored because the provider
payload remains authoritative."
  (when (and (mevedel-agent-invocation-p invocation)
             (stringp block)
             (not (string-empty-p block)))
    (require 'mevedel-utilities)
    (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (condition-case err
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (mevedel--insert-user-role-block-at-marker block marker)
              (mevedel-agent-conversation-save invocation)))
        (error
         (message "mevedel: conversation user-block insert failed: %S" err))))))


;;
;;; Activity and presentation

(defun mevedel-agent-conversation--cache-render-data-bounds
    (invocation beg end)
  "Cache render-data bounds BEG and END on INVOCATION."
  (when (mevedel-agent-invocation-p invocation)
    (let ((start (copy-marker beg nil))
          (finish (copy-marker end t)))
      (setf (mevedel-agent-invocation-render-data-start-marker invocation)
            start
            (mevedel-agent-invocation-render-data-end-marker invocation)
            finish)
      (cons start finish))))

(defun mevedel-agent-conversation--cached-render-data-bounds
    (invocation parent-buffer agent-id)
  "Return valid cached bounds for AGENT-ID in PARENT-BUFFER."
  (when (mevedel-agent-invocation-p invocation)
    (let ((beg (mevedel-agent-invocation-render-data-start-marker invocation))
          (end (mevedel-agent-invocation-render-data-end-marker invocation)))
      (when (and (markerp beg) (markerp end)
                 (eq (marker-buffer beg) parent-buffer)
                 (eq (marker-buffer end) parent-buffer)
                 (marker-position beg) (marker-position end)
                 (< (marker-position beg) (marker-position end)))
        (let* ((raw (buffer-substring-no-properties beg end))
               (parsed (mevedel-pipeline-extract-render-data raw)))
          (if (and (stringp (car parsed))
                   (string-empty-p (string-trim (car parsed)))
                   (listp (cdr parsed))
                   (equal (plist-get (cdr parsed) :agent-id) agent-id))
              (cons beg end)
            (setf (mevedel-agent-invocation-render-data-start-marker
                   invocation) nil
                  (mevedel-agent-invocation-render-data-end-marker
                   invocation) nil)
            nil))))))

(defun mevedel-agent-conversation--render-data-bounds
    (invocation agent-id)
  "Return current render-data bounds for INVOCATION and AGENT-ID."
  (or (mevedel-agent-conversation--cached-render-data-bounds
       invocation (current-buffer) agent-id)
      (when-let* ((bounds
                   (mevedel-pipeline--find-render-data-block-by-agent-id
                    agent-id)))
        (mevedel-agent-conversation--cache-render-data-bounds
         invocation (car bounds) (cdr bounds)))))

(defun mevedel-agent-conversation--activity-tool-name (args)
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

(defun mevedel-agent-conversation--activity-error-p (args)
  "Return non-nil when hook ARGS contain a tool error."
  (cl-some (lambda (arg)
             (and (stringp arg) (string-prefix-p "Error:" arg)))
           args))

(defun mevedel-agent-conversation--activity-sanitize-item (item)
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

(defun mevedel-agent-conversation--activity-snapshot
    (invocation &optional limit)
  "Return INVOCATION's non-status activity, optionally bounded by LIMIT."
  (when (mevedel-agent-invocation-p invocation)
    (let* ((items
            (cl-remove-if
             (lambda (item) (eq (plist-get item :type) 'status))
             (mevedel-agent-invocation-activity invocation)))
           (items (if (and limit (> (length items) limit))
                      (last items limit)
                    items)))
      (copy-tree items))))

(defun mevedel-agent-conversation-final-activity (invocation)
  "Return INVOCATION's full settled activity history."
  (mevedel-agent-conversation--activity-snapshot invocation))

(defun mevedel-agent-conversation--render-activity (invocation)
  "Return activity metadata appropriate for INVOCATION's current status."
  (if (memq (mevedel-agent-invocation-transcript-status invocation)
            '(completed error aborted))
      (mevedel-agent-conversation-final-activity invocation)
    (mevedel-agent-conversation--activity-snapshot
     invocation mevedel-agent-conversation--live-activity-limit)))

(defun mevedel-agent-conversation--sync-entry (invocation)
  "Sync INVOCATION's live metadata into its historical transcript entry."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (session (mevedel-agent-invocation-parent-session invocation))
              (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (let* ((started (mevedel-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (mevedel-agent-invocation-terminal-reason invocation))
           (verdict (mevedel-agent-invocation-verdict invocation))
           (activity (mevedel-agent-conversation--render-activity invocation))
           (updates
            (list :status
                  (or (mevedel-agent-invocation-transcript-status invocation)
                      'running)
                  :calls (or (mevedel-agent-invocation-call-count invocation) 0)
                  :updated-at (format-time-string "%FT%H-%M-%S"))))
      (when elapsed
        (setq updates (plist-put updates :elapsed elapsed)))
      (when reason
        (setq updates (plist-put updates :reason reason)))
      (when verdict
        (setq updates (plist-put updates :verdict verdict)))
      (when activity
        (setq updates (plist-put updates :activity activity)))
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence--update-transcript-entry
       session agent-id updates))))

(defun mevedel-agent-conversation-record-activity
    (invocation item &optional suppress-rerender)
  "Append activity ITEM to INVOCATION.
When SUPPRESS-RERENDER is non-nil, do not schedule a parent view refresh."
  (when (and (mevedel-agent-invocation-p invocation)
             (buffer-live-p
              (mevedel-agent-invocation-parent-data-buffer invocation)))
    (when-let* ((clean
                 (mevedel-agent-conversation--activity-sanitize-item item)))
      (setf (mevedel-agent-invocation-activity invocation)
            (append (mevedel-agent-invocation-activity invocation)
                    (list (plist-put clean :time (float-time)))))
      (mevedel-agent-conversation--sync-entry invocation)
      (unless suppress-rerender
        (when-let* ((parent
                     (mevedel-agent-invocation-parent-data-buffer invocation))
                    ((buffer-live-p parent))
                    (path (mevedel-agent-invocation-path invocation))
                    (view (buffer-local-value 'mevedel--view-buffer parent))
                    ((buffer-live-p view)))
          (mevedel-view-refresh-agent-rendering view path))))))

(defun mevedel-agent-conversation-refresh (invocation)
  "Persist and redraw INVOCATION's live conversation presentation."
  (let ((parent (mevedel-agent-invocation-parent-data-buffer invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (mevedel-agent-conversation--sync-entry invocation)
    (when (and (buffer-live-p parent) agent-id)
      (condition-case err
          (with-current-buffer parent
            (when-let* ((bounds
                         (mevedel-agent-conversation--render-data-bounds
                          invocation agent-id)))
              (let* ((beg (car bounds))
                     (end (cdr bounds))
                     (parsed
                      (mevedel-pipeline-extract-render-data
                       (buffer-substring-no-properties beg end)))
                     (updated (copy-sequence (cdr parsed)))
                     (started
                      (mevedel-agent-invocation-started-at invocation))
                     (elapsed
                      (and started
                           (float-time
                            (time-subtract (current-time) started)))))
                (when (listp updated)
                  (setq updated
                        (plist-put
                         updated :status
                         (or (mevedel-agent-invocation-transcript-status
                              invocation)
                             'running)))
                  (setq updated
                        (plist-put
                         updated :calls
                         (or (mevedel-agent-invocation-call-count invocation)
                             0)))
                  (dolist (pair
                           `((:elapsed . ,elapsed)
                             (:reason . ,(mevedel-agent-invocation-terminal-reason
                                          invocation))
                             (:verdict . ,(mevedel-agent-invocation-verdict
                                           invocation))
                             (:activity . ,(mevedel-agent-conversation--render-activity
                                            invocation))))
                    (when (cdr pair)
                      (setq updated
                            (plist-put updated (car pair) (cdr pair)))))
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (mevedel-pipeline--patch-render-data-block
                     beg end updated))))
            (when-let* ((view (and (boundp 'mevedel--view-buffer)
                                    mevedel--view-buffer))
                        ((buffer-live-p view)))
              (mevedel-view-refresh-agent-rendering
               view (mevedel-agent-invocation-path invocation)))))
        (error
         (display-warning
          'mevedel
          (format "Agent conversation refresh failed for %s: %S"
                  agent-id err)
          :warning))))))


;;
;;; Response and persistence

(defun mevedel-agent-conversation--response-property-p (property)
  "Return non-nil when PROPERTY denotes a gptel assistant response."
  (or (eq property 'response)
      (let ((tail property)
            found)
        (while (consp tail)
          (if (eq (car tail) 'response)
              (setq found t tail nil)
            (setq tail (cdr tail))))
        found)))

(defun mevedel-agent-conversation-final-response (invocation)
  "Return INVOCATION's final assistant response from its conversation."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (buffer (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((position (point-min))
            start end)
        (while (< position (point-max))
          (let* ((property (get-text-property position 'gptel))
                 (next (or (next-single-property-change
                            position 'gptel nil (point-max))
                           (point-max))))
            (when (mevedel-agent-conversation--response-property-p property)
              (setq start position end next))
            (setq position next)))
        (when (and start end)
          (let ((text
                 (string-trim
                  (buffer-substring-no-properties start end))))
            (unless (string-empty-p text) text)))))))

(defun mevedel-agent-conversation--cancel-save (invocation)
  "Cancel INVOCATION's pending debounced save."
  (when (mevedel-agent-invocation-p invocation)
    (let ((timer
           (mevedel-agent-invocation-transcript-save-timer invocation)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (mevedel-agent-invocation-transcript-save-timer invocation) nil))))

(defun mevedel-agent-conversation--write (invocation)
  "Write INVOCATION's retained conversation, returning non-nil on success."
  (when (mevedel-agent-invocation-p invocation)
    (let ((buffer (mevedel-agent-invocation-buffer invocation))
          (relative
           (mevedel-agent-invocation-transcript-relative-path invocation))
          (session (mevedel-agent-invocation-parent-session invocation))
          (parent
           (mevedel-agent-invocation-parent-data-buffer invocation)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and buffer-file-name relative session
                     (mevedel-session-save-path session)
                     (mevedel--same-file-p
                      buffer-file-name
                      (expand-file-name
                       relative (mevedel-session-save-path session))))
            (condition-case err
                (progn
                  (when (buffer-modified-p)
                    (when (bound-and-true-p gptel-mode)
                      (gptel--save-state))
                    (let ((coding-system-for-write 'utf-8-unix)
                          (save-silently t)
                          (inhibit-message t)
                          (message-log-max nil)
                          (before-save-hook
                           (remq 'gptel--save-state before-save-hook))
                          (undo-tree-auto-save-history nil)
                          (write-file-functions
                           (remq 'undo-tree-save-history-from-hook
                                 write-file-functions)))
                      (basic-save-buffer)))
                  (require 'mevedel-session-persistence)
                  (mevedel-session-persistence--update-transcript-entry
                   session
                   (mevedel-agent-invocation-agent-id invocation)
                   (list :updated-at (format-time-string "%FT%H-%M-%S")))
                  (when (and
                         (mevedel-agent-invocation-sidecar-dirty invocation)
                         (buffer-live-p parent)
                         (mevedel-session-persistence--write-sidecar-now
                          session parent))
                    (setf (mevedel-agent-invocation-sidecar-dirty invocation)
                          nil))
                  t)
              (error
               (message "mevedel: conversation save failed for %s: %S"
                        (mevedel-agent-invocation-agent-id invocation) err)
               nil))))))))

(defun mevedel-agent-conversation-save (invocation &optional deferred)
  "Save INVOCATION's retained conversation.
When DEFERRED is non-nil, coalesce writes through an idle timer."
  (if (and deferred
           (numberp mevedel-agent-conversation-save-debounce)
           (> mevedel-agent-conversation-save-debounce 0))
      (progn
        (mevedel-agent-conversation--cancel-save invocation)
        (setf
         (mevedel-agent-invocation-transcript-save-timer invocation)
         (run-with-idle-timer
          mevedel-agent-conversation-save-debounce nil
          (lambda (inv)
            (when (mevedel-agent-invocation-p inv)
              (setf (mevedel-agent-invocation-transcript-save-timer inv) nil)
              (mevedel-agent-conversation--write inv)))
          invocation)))
    (mevedel-agent-conversation--cancel-save invocation)
    (mevedel-agent-conversation--write invocation)))

(defun mevedel-agent-conversation--on-buffer-kill ()
  "Persist and abort any live request owned by the current conversation."
  (when (and (boundp 'mevedel--agent-invocation)
             (mevedel-agent-invocation-p mevedel--agent-invocation))
    (mevedel-agent-conversation-save mevedel--agent-invocation))
  (when (and (boundp 'mevedel--agent-invocation)
             (mevedel-agent-invocation-p mevedel--agent-invocation)
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

(provide 'mevedel-agent-conversation)

;;; mevedel-agent-conversation.el ends here
