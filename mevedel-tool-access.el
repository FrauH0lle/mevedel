;;; mevedel-tool-access.el -- Directory access tool -*- lexical-binding: t -*-

;;; Commentary:

;; RequestAccess prompt, in-flight request deduplication, result formatting,
;; permission diagnostics, renderer, and tool registration.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-user-with-overlay
                  "mevedel-interaction-prompt"
                  (title content question help-echo-text callback))

;; `mevedel-permission-log'
(declare-function mevedel-permission-log "mevedel-permission-log"
                  (session event &rest props))
(declare-function mevedel-permission-log-current-session
                  "mevedel-permission-log" (&optional buffer))
(declare-function mevedel-permission-log-origin
                  "mevedel-permission-log" (&optional buffer))

;; `mevedel-workspace'
(declare-function mevedel-add-project-root "mevedel-workspace" (directory))
(declare-function mevedel-workspace--file-in-allowed-roots-p
                  "mevedel-workspace" (file &optional buffer))


;;
;;; Directory access dedup

(defvar-local mevedel--pending-access-requests nil
  "Alist of (ROOT . (STATUS . WAITERS)) for in-flight access requests.

STATUS is one of `pending', `approve', `deny', or `aborted'.
WAITERS is a list of callback thunks accumulated while STATUS is
`pending'; they fire once when the first prompt resolves.")

(defun mevedel-tools--request-access--collapse (ui-outcome)
  "Collapse UI-OUTCOME into a reusable access-cache status."
  (pcase ui-outcome
    ('approve 'approve)
    ('aborted 'aborted)
    ((or 'deny `(feedback . ,_)) 'deny)
    (_ 'deny)))

(defun mevedel-tools--request-access--log
    (event root reason &optional buffer &rest props)
  "Persist RequestAccess EVENT for ROOT, REASON, BUFFER, and PROPS."
  (require 'mevedel-permission-log)
  (when-let* ((session (mevedel-permission-log-current-session buffer)))
    (apply #'mevedel-permission-log
           session event
           (append
            (list :origin (mevedel-permission-log-origin buffer)
                  :directory root
                  :reason reason)
            props))))

(defun mevedel-tools--request-access (root reason callback &optional buffer)
  "Request access to ROOT with REASON, delivering outcome to CALLBACK.

Concurrent calls for the same ROOT share one prompt.  BUFFER is the
chat buffer that owns the buffer-local request cache and defaults to
the current buffer."
  (require 'mevedel-workspace)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((allowed-root
            (condition-case nil
                (mevedel-workspace--file-in-allowed-roots-p
                 root (current-buffer))
              (error nil)))
           (entry (assoc root mevedel--pending-access-requests #'string=))
           (status (and entry (cadr entry))))
      (pcase (if allowed-root 'approve status)
        ('approve
         (mevedel-tools--request-access--log
          (if allowed-root
              'request-access-bypassed
            'request-access-cache-hit)
          root reason (current-buffer) :outcome 'approve)
         (funcall callback 'approve))
        ((or 'deny 'aborted)
         (mevedel-tools--request-access--log
          'request-access-cache-hit root reason (current-buffer)
          :outcome status)
         (funcall callback status))
        ('pending
         (mevedel-tools--request-access--log
          'request-access-joined root reason (current-buffer))
         (setcdr entry (cons 'pending
                             (append (cddr entry) (list callback)))))
        (_
         (let ((new-entry (cons root (cons 'pending nil)))
               (chat-buf (current-buffer))
               interaction-id)
           (push new-entry mevedel--pending-access-requests)
           (let ((ov
                  (mevedel--prompt-user-for-access
                   root reason
                   (lambda (ui-outcome)
                     (when (buffer-live-p chat-buf)
                       (with-current-buffer chat-buf
                         (let ((entry
                                (assoc root mevedel--pending-access-requests
                                       #'string=))
                               (cached
                                (mevedel-tools--request-access--collapse
                                 ui-outcome)))
                           (mevedel-tools--request-access--log
                            'request-access-resolved root reason chat-buf
                            :outcome ui-outcome
                            :collapsed cached
                            :interaction-id interaction-id)
                           (when (eq ui-outcome 'approve)
                             (mevedel-add-project-root root))
                           (when entry
                             (let ((waiters (cddr entry)))
                               (setcdr entry (cons cached nil))
                               (ignore-errors (funcall callback ui-outcome))
                               (dolist (waiter waiters)
                                 (ignore-errors
                                   (funcall waiter ui-outcome))))))))))))
             (setq interaction-id
                   (and (overlayp ov)
                        (overlay-get ov 'mevedel-view-interaction-id)))
             (mevedel-tools--request-access--log
              'request-access-created root reason chat-buf
              :interaction-id interaction-id))))))))


;;
;;; Prompt and tool result

(defun mevedel--prompt-user-for-access (root reason callback)
  "Display an access prompt for ROOT and REASON, then call CALLBACK."
  (require 'mevedel-interaction-prompt)
  (let ((content
         (concat
          "The LLM is requesting access to a directory outside the current workspace.\n\n"
          (propertize "Directory: " 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s\n" root)
                      'font-lock-face 'font-lock-constant-face)
          (propertize "Reason: " 'font-lock-face 'font-lock-escape-face)
          (format "%s" reason))))
    (mevedel--prompt-user-with-overlay
     "Directory Access Request"
     content
     "Grant access to this directory?"
     (concat "Directory access request: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding))
     callback)))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access request cache after an LLM response."
  (setq mevedel--pending-access-requests nil))

(defun mevedel-tools--request-access--format-result (path ui-outcome)
  "Translate UI-OUTCOME into the tool-result string for PATH."
  (pcase ui-outcome
    ('approve
     (format "Access granted to %s. You can now read and write files in this directory."
             path))
    ('aborted "Error: aborted")
    (`(feedback . ,text)
     (format "Access denied to %s. Feedback: %s" path text))
    (_
     (format "Access denied to %s. You cannot access files in this directory."
             path))))

(defun mevedel-tool-access--request (callback args)
  "Request directory access described by ARGS and call CALLBACK."
  (let ((directory (plist-get args :directory))
        (reason (plist-get args :reason)))
    (unless (stringp directory)
      (error "Parameter directory is required"))
    (unless (stringp reason)
      (error "Parameter reason is required"))
    (if (not (and (file-readable-p directory) (file-directory-p directory)))
        (funcall callback
                 (list :result
                       (format "Error: Directory '%s' is not readable"
                               directory)))
      (let ((expanded (expand-file-name directory)))
        (mevedel-tools--request-access
         expanded reason
         (lambda (ui-outcome)
           (funcall callback
                    (list :result
                          (mevedel-tools--request-access--format-result
                           expanded ui-outcome)))))))))


;;
;;; Renderer and registration

(defun mevedel-tool-access--render-request-access
    (name args result _render-data)
  "Return rendering plist for RequestAccess NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((directory (or (plist-get args :directory) "?"))
           (status (cond
                    ((string-prefix-p "Access granted" result) "granted")
                    ((string-prefix-p "Access denied" result) "denied")
                    ((string-prefix-p "Error:" result) "error")
                    (t "done"))))
      (list :header (format "%s: %s (%s)"
                            (or name "RequestAccess") directory status)
            :body result
            :body-mode nil
            :status (and (or (string-prefix-p "Error:" result)
                             (string-prefix-p "Access denied" result))
                         'error)
            :initially-collapsed-p t))))

(defun mevedel-tool-access-register ()
  "Register the RequestAccess tool."
  (mevedel-define-tool
    :name "RequestAccess"
    :description "Request access to a directory outside the current allowed project roots."
    :prompt-file "tools/requestaccess.md"
    :handler #'mevedel-tool-access--request
    :args ((directory path :required
                      "Absolute or relative path to the directory you need to access.")
           (reason string :required
                   "Clear explanation of why you need access to this directory."))
    :async-p t
    :groups (util)
    :check-permission (lambda (_tool _args) 'allow)
    :get-path (lambda (args) (plist-get args :directory))
    :read-only-p t
    :renderer #'mevedel-tool-access--render-request-access))

(provide 'mevedel-tool-access)
;;; mevedel-tool-access.el ends here
