;;; mevedel-engine-test-support.el --- Admitted engine fixture -*- lexical-binding: t -*-

;;; Commentary:
;; Real published sessions for external request and transport tests.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-agent-control)
(require 'mevedel-claude-code)

(defconst mevedel-engine-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py")
  "The scripted ACP peer standing in for Claude's adapter.")

(defconst mevedel-engine-test--owned-timer-functions
  '(mevedel-goal--scheduled-continuation
    mevedel-view--run-follow-up-drain
    mevedel-session-persistence--deferred-agent-save)
  "Timer callbacks a settled fixture turn schedules for its own session.")

(defconst mevedel-engine-test--global-timer-functions
  '(undo-auto--boundary-timer org-persist--refresh-gc-lock org-element--cache-sync)
  "Emacs and Org housekeeping timers no fixture owns.")

(defun mevedel-engine-test--release (session buffer timers)
  "Tear down SESSION and BUFFER; return new timers absent from TIMERS.
Agent control, execution records and the 0-delay Goal continuation, follow-up
drain and debounced save timers a settled turn schedules belong to the
fixture; a survivor would fire inside a later test."
  (mevedel-agent-control-teardown-session session)
  (mevedel-test--with-captured-diagnostics nil
    (let ((mevedel-execution-process--child-kill-delay 0.05))
      (mevedel-execution-teardown-session session)))
  (dolist (timer (append timer-list timer-idle-list nil))
    (unless (memq timer timers)
      (when (or (memq (timer--function timer) mevedel-engine-test--owned-timer-functions)
                (cl-some (lambda (arg) (or (eq arg session) (eq arg buffer)))
                         (timer--args timer)))
        (cancel-timer timer))))
  (test-mevedel-session-persistence--release-and-kill buffer session)
  (mevedel-workspace-clear-registry)
  (mevedel-test--cancel-stray-lease-timers)
  (cl-remove-if (lambda (timer)
                  (or (memq timer timers)
                      (memq (timer--function timer) mevedel-engine-test--global-timer-functions)))
                (append timer-list timer-idle-list nil)))

(defmacro mevedel-engine-test--with-session (&rest body)
  "Run BODY in a temporary published session with an admitted request.
Fail when BODY leaves a timer the fixture does not own."
  (declare (indent 0) (debug t))
  `(let* ((timers (append timer-list timer-idle-list nil))
          (root (make-temp-file "mevedel-mcp-tools-" t))
          (workspace (test-mevedel-session-persistence--make-workspace root))
          (session (mevedel-session-create "mcp-tools" workspace))
          (buffer (generate-new-buffer " *mcp-tools*"))
          done)
     (unwind-protect
         (prog1
             (with-current-buffer buffer
               (mevedel-chat-prepare-transcript-buffer)
               (setq-local mevedel--session session mevedel--workspace workspace
                           default-directory (file-name-as-directory root))
               (setf (mevedel-session-permission-mode session) 'full-auto)
               (mevedel-session-set-root-buffer session buffer)
               (mevedel-session-artifacts-ensure-files session buffer)
               (let ((request (mevedel-request-begin session))) ,@body))
           (setq done t))
       (let ((leaked (mevedel-engine-test--release session buffer timers)))
         (delete-directory root t)
         (when (and done leaked)
           (mapc #'cancel-timer leaked)
           (error "Fixture left timers: %S"
                  (mapcar (lambda (timer)
                            (let ((function (timer--function timer)))
                              (if (symbolp function) function 'lambda)))
                          leaked)))))))

(defun mevedel-engine-test--session-info (model)
  "Return the capabilities Claude's adapter reports for MODEL."
  (let ((levels (cdr (assoc model mevedel-claude-code--aliases))))
    `((configOptions
       . ,(vconcat
           (list `((id . "model") (category . "model") (type . "select")
                   (currentValue . ,model)
                   (options . ,(vconcat (mapcar (lambda (alias) `((value . ,(car alias))))
                                                mevedel-claude-code--aliases)))))
           (when levels
             (list `((id . "effort") (category . "thought_level") (type . "select")
                     (currentValue . "default")
                     (options . ,(vconcat (mapcar (lambda (level) `((value . ,level)))
                                                  (cons "default" (mapcar #'symbol-name levels)))))))))))))

(defun mevedel-engine-test--claude-launch (script)
  "Return a `mevedel-claude-code-launch' replacement driving the fixture peer.
Each launch builds the real Claude plist, runs the peer in place of the
adapter and skips the installation readiness probe.  SCRIPT receives the
launch arguments and returns a plist: its :meta scripts the peer, joined to
the real options with the hook bridge and the model catalog unless SCRIPT
supplies them, and its other keys replace the real plist's."
  (let ((real (symbol-function 'mevedel-claude-code-launch))
        (python (executable-find "python3")))
    (lambda (&rest args)
      (let* ((overrides (apply script args))
             (launch (let ((mevedel-claude-code-executable python)
                           (mevedel-claude-code-adapter-executable python))
                       (apply real args)))
             (meta (copy-sequence (plist-get overrides :meta))))
        (pcase-dolist (`(,key . ,value)
                       `(,@(when (nth 5 args) `((hookCommand . ,(nth 5 args))))
                         (sessionInfo . ,(mevedel-engine-test--session-info (nth 2 args)))
                         ,@(plist-get launch :meta)))
          (unless (assq key meta) (setq meta (append meta (list (cons key value))))))
        (setq launch (plist-put launch :command python))
        (setq launch (plist-put launch :args (list mevedel-engine-test--peer)))
        (setq launch (plist-put launch :prepare-launch nil))
        (setq launch (plist-put launch :meta meta))
        (cl-loop for (key value) on overrides by #'cddr
                 unless (eq key :meta) do (setq launch (plist-put launch key value)))
        launch))))

(defun mevedel-engine-test--evidence ()
  "Return decoded transcript evidence from the current fixture buffer."
  (mevedel-transcript-project-evidence (list (cons (point-min) (point-max)))))

(defun mevedel-engine-test--count-evidence (regexp)
  "Count REGEXP in decoded evidence, including delivered reminder records."
  (let ((text (mevedel-engine-test--evidence)))
    (with-temp-buffer
      (insert text)
      (how-many regexp (point-min) (point-max)))))

(provide 'mevedel-engine-test-support)
;;; mevedel-engine-test-support.el ends here
