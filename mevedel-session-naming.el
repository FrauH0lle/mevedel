;;; mevedel-session-naming.el --- Stable session display names -*- lexical-binding: t -*-

;;; Commentary:

;; Owns metadata-only rename and bounded background titles for automatically
;; named root sessions.  Inference never owns the root conversation request,
;; and it inherits the root buffer's streaming choice so providers that reject
;; a non-streaming request can still answer it.
;;
;; Every terminal attempt is one `session-naming' telemetry event carrying a
;; categorical outcome, an `:error-class', and -- for provider failures -- the
;; provider's status, structured error type, and error code.  Provider message
;; text is never recorded.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'mevedel-structs)
(require 'mevedel-models)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-transport)

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest keys))
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-track-response)

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-discard-rolled-back
                  "mevedel-session-publication" (session))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record "mevedel-telemetry" (session event &rest props))
(autoload 'mevedel-telemetry-record "mevedel-telemetry")

;; `mevedel-view'
(defvar mevedel--data-buffer)

(defvar-local mevedel-session-naming--cancel nil
  "Cancellation thunk for this root buffer's pending title operation.")

(defun mevedel-session-naming-cancel ()
  "Cancel the current root buffer's pending title operation."
  (when mevedel-session-naming--cancel
    (funcall mevedel-session-naming--cancel)))

(defun mevedel-session-naming-normalize (name)
  "Return readable single-line NAME, rejecting an empty name."
  (let ((normalized (string-join (split-string (substring-no-properties name)) " ")))
    (when (string-empty-p normalized)
      (user-error "Empty session name"))
    normalized))

(defun mevedel-session-naming-rename (session buffer name &optional current-p)
  "Set SESSION's display NAME and update its root BUFFER.
Only metadata and presentation change; storage identity stays fixed.
Publish before announcing the name.  Keep a committed name on a post-commit
failure, but restore in-memory metadata when publication did not commit.
CURRENT-P marks an automatic title and rechecks its operation after I/O.
Without CURRENT-P the name is explicit and disables automatic replacement."
  (setq name (mevedel-session-naming-normalize name))
  (unless (and (mevedel-session-persistence-root-data-buffer-p buffer)
               (eq session (buffer-local-value 'mevedel--session buffer)))
    (user-error "Session root buffer is unavailable"))
  (mevedel-session-artifacts-assert-mutation-authority session buffer)
  (unless (and (buffer-live-p buffer)
               (eq session (buffer-local-value 'mevedel--session buffer))
               (or (null current-p) (funcall current-p)))
    (user-error "Session naming operation is no longer current"))
  (when (mevedel-session-publication-active-p session)
    (user-error "Session publication is busy"))
  (with-current-buffer buffer (mevedel-session-naming-cancel))
  (let ((old-name (mevedel-session-name session))
        (old-state (mevedel-session-naming-state session))
        (head (plist-get (mevedel-session-publication session) :head))
        failure)
    (setf (mevedel-session-name session) name
          (mevedel-session-naming-state session) (if current-p 'attempted 'explicit))
    (condition-case err
        (when-let* ((path (mevedel-session-save-path session)))
          (if (mevedel-session-codec-portable-authority-p session)
              (when (mevedel-session-artifacts-artifact-present-p
                     session "session.meta.el" t)
                (mevedel-session-artifacts-publish-sidecar-state session buffer))
            (mevedel-session-artifacts-publish-text
             session (mevedel-session-artifacts-sidecar-path path)
             (mevedel-session-artifacts-printed-value
              (mevedel-session-artifacts-build-sidecar session buffer)))))
      (error
       (if (equal head (plist-get (mevedel-session-publication session) :head))
           (progn
             (setf (mevedel-session-name session) old-name
                   (mevedel-session-naming-state session) old-state)
             (when (mevedel-session-codec-portable-authority-p session)
               (mevedel-session-publication-discard-rolled-back session))
             (signal (car err) (cdr err)))
         (setq failure err))))
    (with-current-buffer buffer
      (rename-buffer (mevedel-session-buffer-name
                      name (mevedel-session-workspace session)) t)
      (mevedel-session-persistence-notify-session-event
       session 'rename (buffer-name)))
    (when failure (signal (car failure) (cdr failure)))))

;;;###autoload
(defun mevedel-rename-session (new-name)
  "Rename the current session to NEW-NAME without changing its identity."
  (interactive "sNew session name: ")
  (let* ((buffer (if (bound-and-true-p mevedel--data-buffer)
                     mevedel--data-buffer (current-buffer)))
         (session (and (buffer-live-p buffer)
                       (buffer-local-value 'mevedel--session buffer))))
    (unless session (user-error "Not in a mevedel chat or view buffer"))
    (mevedel-session-naming-rename session buffer new-name)
    (message "mevedel: session renamed to %s" (mevedel-session-name session))))

(defun mevedel-session-naming--provider-props (info)
  "Return bounded provider diagnostics from gptel INFO for a naming event.

Only categorical facts travel: the status line gptel reports and the
provider's own structured error type and code.  Message text stays out,
because telemetry is durable and `docs/telemetry.md' admits only
classifications and bounded identifiers there."
  (let ((error-data (plist-get info :error)))
    (list :provider-status (plist-get info :status)
          :provider-error-type (and (listp error-data)
                                    (plist-get error-data :type))
          :provider-error-code (and (listp error-data)
                                    (plist-get error-data :code)))))

(defun mevedel-session-naming-consider (session prompt)
  "Consider naming SESSION from its first accepted authored PROMPT.
Call only after accepting model-bound user input in its destination session.
Explicit names are never considered.  Consumed automatic attempts are not
considered again until reset by clear."
  (let ((buffer (and session (mevedel-session-root-buffer session))))
    (when (and session (eq (mevedel-session-naming-state session) 'pending)
               (not (mevedel-session-audit-session session))
               (buffer-live-p buffer)
               (stringp prompt) (not (string-blank-p prompt)))
      (setf (mevedel-session-naming-state session) 'attempted)
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'mevedel-session-naming-cancel nil t)
        (let ((input (substring-no-properties prompt 0 (min 2000 (length prompt))))
              (key (list 'session-naming buffer))
              (segment (or (mevedel-session-current-segment session) 1))
              (path (mevedel-session-working-directory session))
              request-buffer request-finished start-timer timeout-timer settled
              cancel streamed reported)
          (cl-labels
              ((active-p ()
                 (and (not settled) (buffer-live-p buffer)
                      (eq session (buffer-local-value 'mevedel--session buffer))
                      (equal segment (or (mevedel-session-current-segment session) 1))
                      (eq cancel (buffer-local-value 'mevedel-session-naming--cancel buffer))))
               ;; The rename path settles through the cancel thunk before it
               ;; succeeds, so settlement and observation stay separate: this
               ;; records the operation's one terminal event whoever settles it.
               (report (outcome properties)
                 (unless reported
                   (setq reported t)
                   (apply #'mevedel-telemetry-record
                          session 'session-naming
                          :outcome outcome properties)))
               (finish (&optional outcome properties)
                 (unless settled
                   (setq settled t)
                   (when start-timer (cancel-timer start-timer))
                   (when timeout-timer (cancel-timer timeout-timer))
                   (mevedel-transport-cancel-pending key)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (eq cancel mevedel-session-naming--cancel)
                         (setq mevedel-session-naming--cancel nil))))
                   (when (buffer-live-p request-buffer)
                     (unless request-finished (gptel-abort request-buffer))
                     (kill-buffer request-buffer))
                   (when outcome (report outcome properties))))
               (apply-title (response)
                 (when (active-p)
                   (condition-case err
                       (let* ((plain (string-trim response "[ \t\n\r\"`]+" "[ \t\n\r\"`]+"))
                              (title (mevedel-session-naming-normalize plain)))
                         (setq title (string-join
                                      (seq-take (split-string title) 4) " ")
                               title (string-trim-right
                                      (substring title 0 (min 60 (length title)))))
                         (mevedel-session-naming-rename session buffer title #'active-p)
                         (finish)
                         (report 'renamed nil))
                     (error
                      (finish)
                      (report 'error (list :error-class (car-safe err)))))))
               (receive (response info)
                 (when (active-p)
                   (cond
                    ;; Streamed delivery: chunks arrive separately, and the
                    ;; stream ends with `t'.  Bound what is retained.
                    ((and (stringp response) (plist-get info :stream))
                     (setq streamed (concat streamed response))
                     (when (> (length streamed) 4096)
                       (finish 'invalid-response '(:error-class validation))))
                    ((stringp response)
                     (setq request-finished t)
                     (if (> (length response) 4096)
                         (finish 'invalid-response '(:error-class validation))
                       (dispatch (lambda () (apply-title response)))))
                    ((eq response t)
                     (setq request-finished t)
                     (let ((plain streamed))
                       (setq streamed nil)
                       (if (or (null plain) (> (length plain) 4096))
                           (finish 'invalid-response '(:error-class validation))
                         (dispatch (lambda () (apply-title plain))))))
                    ((eq response 'abort)
                     (setq request-finished t)
                     (finish 'aborted '(:error-class aborted)))
                    ((null response)
                     (setq request-finished t)
                     (finish 'provider-error
                             (append '(:error-class provider)
                                     (mevedel-session-naming--provider-props
                                      info)))))))
               (start ()
                 (when (active-p)
                   (condition-case err
                       (let ((policy (with-current-buffer buffer
                                       (mevedel-model-resolve-workload 'naming)))
                             ;; Streaming is a session choice, not this
                             ;; request's: providers reject a non-streaming
                             ;; request outright, and the root buffer already
                             ;; holds the working value.
                             (stream (buffer-local-value 'gptel-stream buffer)))
                         ;; Persist the consumed attempt with the accepted prompt,
                         ;; so resume cannot turn a failed request into a retry.
                         (with-current-buffer buffer
                           (mevedel-session-artifacts-save session buffer))
                         (when (active-p)
                           (setq request-buffer (generate-new-buffer " *mevedel-title*"))
                           (with-current-buffer request-buffer
                             (setq-local
                              gptel-backend (plist-get policy :backend)
                              gptel-model (plist-get policy :model)
                              gptel-reasoning-effort (plist-get policy :effort)
                              gptel-system-prompt
                              "Generate a descriptive session title for the user's request. Use the user's language, 1-4 descriptive words and at most 60 characters. Return only the title, without quotes or Markdown. Do not answer or carry out the request."
                              gptel-use-tools nil gptel-tools nil
                              gptel-use-context nil gptel-track-response nil
                              gptel-stream stream gptel-max-tokens nil)
                             (setq timeout-timer
                                   (run-at-time
                                    30 nil
                                    (lambda ()
                                      (finish 'timeout '(:error-class timeout)))))
                             (gptel-request input :buffer request-buffer
                                            :system gptel-system-prompt
                                            :transforms nil :stream stream
                                            :context
                                            (list :mevedel-telemetry-session session
                                                  :mevedel-telemetry-workload 'naming)
                                            :callback #'receive))))
                     (error
                      (finish)
                      (report 'error (list :error-class (car-safe err)))))))
               (dispatch (function)
                 (when (active-p)
                   (unless (mevedel-transport-run-when-idle
                            key path
                            (lambda ()
                              (when (active-p)
                                (if (mevedel-session-publication-active-p session)
                                    (setq start-timer
                                          (run-at-time
                                           mevedel-transport-retry-seconds nil
                                           (lambda () (dispatch function))))
                                  (funcall function))))
                            cancel)
                     (finish 'cancelled '(:error-class cancelled))))))
            (setq cancel (lambda () (finish))
                  mevedel-session-naming--cancel cancel
                  start-timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (dispatch #'start))))))))))

(provide 'mevedel-session-naming)
;;; mevedel-session-naming.el ends here
