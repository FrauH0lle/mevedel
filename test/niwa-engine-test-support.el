;;; niwa-engine-test-support.el --- Admitted engine fixture -*- lexical-binding: t -*-

;;; Commentary:
;; Real durable sessions for external request and transport tests.

;;; Code:

(require 'acp)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-session-test-support"))
(require 'niwa-agent-control)
(require 'niwa-claude-code)
(require 'niwa-readiness)

;; `acp'
(defvar acp--shutdown-grace-period)

(defconst niwa-engine-test--owned-timer-functions
  '(niwa-goal--scheduled-continuation
    niwa-view--run-follow-up-drain
    niwa-session-persistence--deferred-agent-save)
  "Timer callbacks a settled fixture turn schedules for its own session.")

(defconst niwa-engine-test--global-timer-functions
  '(undo-auto--boundary-timer org-persist--refresh-gc-lock org-element--cache-sync)
  "Emacs and Org housekeeping timers no fixture owns.")

(defun niwa-engine-test--release (session buffer timers)
  "Tear down SESSION and BUFFER; return new timers absent from TIMERS.
Agent control, execution records and the 0-delay Goal continuation, follow-up
drain and debounced save timers a settled turn schedules belong to the
fixture; a survivor would fire inside a later test."
  (niwa-agent-control-teardown-session session)
  (niwa-test--with-captured-diagnostics nil
    (let ((niwa-execution-process--child-kill-delay 0.05))
      (niwa-execution-teardown-session session)))
  ;; ACP can leave its final queued response on a zero-delay drain at shutdown.
  (accept-process-output nil 0.01)
  (dolist (timer (append timer-list timer-idle-list nil))
    (unless (memq timer timers)
      (when (or (memq (timer--function timer) niwa-engine-test--owned-timer-functions)
                (cl-some (lambda (arg) (or (eq arg session) (eq arg buffer)))
                         (timer--args timer)))
        (cancel-timer timer))))
  (test-niwa-session-persistence--release-and-kill buffer session)
  ;; Killing the root buffer can close its retained ACP client too.
  (accept-process-output nil 0.01)
  (niwa-workspace-clear-registry)
  (niwa-test--cancel-stray-lease-timers)
  (cl-remove-if (lambda (timer)
                  (or (memq timer timers)
                      (memq (timer--function timer) niwa-engine-test--global-timer-functions)))
                (append timer-list timer-idle-list nil)))

(defmacro niwa-engine-test--with-session (&rest body)
  "Run BODY in a temporary durable session with an admitted request.
An initial :file-workspace FORM selects real file-workspace storage when
FORM is non-nil; otherwise use project publication.  Use file workspaces
only when the assertions do not depend on portable publication or leases.
Fail when BODY leaves a timer the fixture does not own."
  (declare (indent 0) (debug t))
  (let ((file-workspace (when (eq (car body) :file-workspace)
                         (pop body) (pop body))))
    `(let* ((timers (append timer-list timer-idle-list nil))
            ;; Fixture peers need no grace period for descendant cleanup.
            (acp--shutdown-grace-period 0)
            (file-workspace-p ,file-workspace)
            (root (make-temp-file "niwa-mcp-tools-" t))
            (workspace (if file-workspace-p
                           (test-niwa-session-persistence--make-file-workspace root)
                         (test-niwa-session-persistence--make-workspace root)))
            (session (niwa-session-create "mcp-tools" workspace))
            (buffer (generate-new-buffer " *mcp-tools*"))
            done)
       (unwind-protect
           (prog1
               (with-current-buffer buffer
                 (niwa-chat-prepare-transcript-buffer)
                 (setq-local niwa--session session niwa--workspace workspace
                             default-directory (file-name-as-directory root))
                 (setf (niwa-session-permission-mode session) 'full-auto)
                 (niwa-session-set-root-buffer session buffer)
                 (niwa-session-artifacts-ensure-files session buffer)
                 (should (eq (if file-workspace-p 'file 'project)
                             (niwa-workspace-type workspace)))
                 (should (eq 'portable (niwa-session-authority-mode session)))
                 (let ((request (niwa-request-begin session)))
                   ;; These fixtures exercise turn engines; provider readiness has
                   ;; its own real HTTP/CLI and pre-commit lifecycle tests.
                   (cl-letf (((symbol-function 'niwa-readiness-assert) #'ignore)) ,@body)))
             (setq done t))
         (let ((leaked (unwind-protect
                           (niwa-engine-test--release session buffer timers)
                         (delete-directory root t))))
           ;; Cancel even after a failure so leaks cannot fire in later tests.
           (mapc #'cancel-timer leaked)
           (when (and done leaked)
             (error "Fixture left timers: %S"
                    (mapcar (lambda (timer)
                              (let ((function (timer--function timer)))
                                (if (symbolp function) function 'lambda)))
                            leaked))))))))

(defmacro niwa-engine-test--with-claude-session (&rest body)
  "Run BODY in `niwa-engine-test--with-session' on Claude Code's sonnet.
Claude's backend is registered in an otherwise empty catalog and keeps its
state under the fixture root.  Like `niwa-engine-test--claude-launch',
the installation probe is skipped: fixture launches need no Claude runtime.
An initial :file-workspace FORM is forwarded to the session fixture."
  (declare (indent 0) (debug t))
  (let ((file-workspace (when (eq (car body) :file-workspace)
                         (pop body) (pop body))))
    `(niwa-engine-test--with-session :file-workspace ,file-workspace
       (let ((gptel--known-backends nil)
             (niwa-claude-code-directory (file-name-concat root "claude")))
         (cl-letf (((symbol-function 'niwa-claude-code-unavailable-reason) #'ignore))
           (niwa-claude-code-register)
           (niwa-model-set-session-provider
            session (niwa-model-resolve-provider "Claude Code:sonnet") buffer)
           ,@body)))))

(defun niwa-engine-test--session-info (model)
  "Return the capabilities Claude's adapter reports for MODEL."
  (let ((levels (cdr (assoc model niwa-claude-code--aliases))))
    `((configOptions
       . ,(vconcat
           (list `((id . "model") (category . "model") (type . "select")
                   (currentValue . ,model)
                   (options . ,(vconcat (mapcar (lambda (alias) `((value . ,(car alias))))
                                                niwa-claude-code--aliases)))))
           (when levels
             (list `((id . "effort") (category . "thought_level") (type . "select")
                     (currentValue . "default")
                     (options . ,(vconcat (mapcar (lambda (level) `((value . ,level)))
                                                  (cons "default" (mapcar #'symbol-name levels)))))))))))))

(defun niwa-engine-test--claude-launch (script)
  "Return a `niwa-claude-code-launch' replacement driving the fixture peer.
Each launch builds the real Claude plist, runs the peer in place of the
adapter and skips the installation readiness probe.  SCRIPT receives the
launch arguments and returns a plist: its :meta scripts the peer, joined to
the real options with the hook bridge and the model catalog unless SCRIPT
supplies them, and its other keys replace the real plist's."
  (let ((real (symbol-function 'niwa-claude-code-launch))
        (python (executable-find "python3")))
    (lambda (&rest args)
      (let* ((overrides (apply script args))
             (launch (let ((niwa-claude-code-executable python)
                           (niwa-claude-code-adapter-executable python))
                       (apply real args)))
             (meta (copy-sequence (plist-get overrides :meta))))
        (pcase-dolist (`(,key . ,value)
                       `(,@(when (nth 5 args) `((hookCommand . ,(nth 5 args))))
                         (sessionInfo . ,(niwa-engine-test--session-info (nth 2 args)))
                         ,@(plist-get launch :meta)))
          (unless (assq key meta) (setq meta (append meta (list (cons key value))))))
        (setq launch (plist-put launch :command python))
        (setq launch (plist-put launch :args (list niwa-test--acp-peer)))
        (setq launch (plist-put launch :prepare-launch nil))
        (setq launch (plist-put launch :meta meta))
        (cl-loop for (key value) on overrides by #'cddr
                 unless (eq key :meta) do (setq launch (plist-put launch key value)))
        launch))))

(defun niwa-engine-test--evidence ()
  "Return decoded transcript evidence from the current fixture buffer."
  (niwa-transcript-project-evidence (list (cons (point-min) (point-max)))))

(defun niwa-engine-test--count-evidence (regexp)
  "Count REGEXP in decoded evidence, including delivered reminder records."
  (let ((text (niwa-engine-test--evidence)))
    (with-temp-buffer
      (insert text)
      (how-many regexp (point-min) (point-max)))))

(provide 'niwa-engine-test-support)
;;; niwa-engine-test-support.el ends here
