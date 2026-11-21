;;; niwa-compact-test-support.el -- Shared compaction test setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; `niwa-telemetry'
(declare-function niwa-telemetry-flush "niwa-telemetry" (session))
(declare-function niwa-telemetry-path "niwa-telemetry" (session))
(autoload 'niwa-telemetry-flush "niwa-telemetry")
(autoload 'niwa-telemetry-path "niwa-telemetry")

(defconst test-niwa-compact--valid-summary
  "## Scope\n- test\n## Constraints & Preferences\n- none\n## Work & Evidence\n- test\n## Key Decisions\n- none\n## Open Questions & Risks\n- none\n## Critical Context\n- none\n## Relevant Files\n- none\n## Skills Invoked\n- none\n## Next Steps\n- test"
  "Valid continuation summary used by compaction tests.")

(defun test-niwa-compact--read-telemetry (session)
  "Return the telemetry events SESSION has written, oldest first."
  (niwa-telemetry-flush session)
  (when-let* ((path (niwa-telemetry-path session))
              ((file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let (events)
        (condition-case nil
            (while t (push (read (current-buffer)) events))
          (end-of-file nil))
        (nreverse events)))))

(defun test-niwa-compact--failing-hook (_event)
  "Signal the hook failure used by fail-closed compaction tests."
  (error "Hook failed"))

(cl-defmacro test-niwa-compact--with-persisted-buffer
    ((buffer session) &rest body)
  "Run BODY in persisted BUFFER owned by SESSION."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "niwa-compact-test-" t))
          (workspace
           (niwa-workspace-get-or-create
            'project "compact-test" tempdir "compact-test"))
          (,session (niwa-session-create "main" workspace))
          (,buffer (generate-new-buffer " *niwa-compact-test*")))
     (unwind-protect
         (with-current-buffer ,buffer
           (org-mode)
           (setq-local niwa--session ,session)
           (niwa-session-artifacts-ensure-files ,session ,buffer)
           ,@body)
       ;; Release through the session: a bare directory has no authority
       ;; profile of its own.
       (niwa-session-persistence-lock-release
        (or (niwa-session-save-path ,session) tempdir) ,session)
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           (set-buffer-modified-p nil))
         (kill-buffer ,buffer))
       (niwa-workspace-clear-registry)
       (delete-directory tempdir t))))

(cl-defmacro test-niwa-compact--with-persisted-agent
    ((buffer invocation session canonical-path parent-buffer) &rest body)
  "Run BODY in persisted agent BUFFER with INVOCATION and SESSION."
  (declare
   (indent 1)
   (debug ((symbolp symbolp symbolp symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "niwa-compact-agent-test-" t))
          (workspace
           (niwa-workspace-get-or-create
            'project "compact-agent-test" tempdir "compact-agent-test"))
          (,session (niwa-session-create "main" workspace))
          (,parent-buffer
           (generate-new-buffer " *niwa-compact-parent*"))
          (agent (niwa-agent--create :name "explorer"))
          (,invocation (niwa-agent-invocation-create agent))
          (,buffer (generate-new-buffer " *niwa-compact-agent*"))
          (,canonical-path nil))
     (unwind-protect
         (progn
           (with-current-buffer ,parent-buffer
             (org-mode)
             (setq-local niwa--session ,session)
             (niwa-session-artifacts-ensure-files
              ,session parent-buffer))
           (let ((relative-path "agents/explorer-test.chat.org"))
             (setq ,canonical-path
                   (expand-file-name relative-path
                                     (niwa-session-save-path ,session)))
             (make-directory (file-name-directory ,canonical-path) t)
             (setf (niwa-agent-invocation-agent-id ,invocation)
                   "explorer--test")
             (setf (niwa-agent-invocation-path ,invocation)
                   "/root/explorer")
             (setf (niwa-agent-invocation-buffer ,invocation) ,buffer)
             (setf (niwa-agent-invocation-parent-data-buffer ,invocation)
                   ,parent-buffer)
             (setf (niwa-agent-invocation-parent-session ,invocation)
                   ,session)
             (setf
              (niwa-agent-invocation-transcript-relative-path ,invocation)
              relative-path)
             (setf (niwa-agent-invocation-transcript-status ,invocation)
                   'running)
             (with-current-buffer ,buffer
               (org-mode)
               (setq-local niwa--agent-invocation ,invocation)
               (set-visited-file-name ,canonical-path t t)
               ,@body)))
       ;; Release through the session: a bare directory has no authority
       ;; profile of its own.
       (niwa-session-persistence-lock-release
        (or (niwa-session-save-path ,session) tempdir) ,session)
       (dolist (candidate (list ,buffer ,parent-buffer))
         (when (buffer-live-p candidate)
           (with-current-buffer candidate
             (set-buffer-modified-p nil))
           (kill-buffer candidate)))
       (niwa-workspace-clear-registry)
       (delete-directory tempdir t))))

(defun test-niwa-compact--insert-agent-task
    (invocation description prompt)
  "Insert INVOCATION's persisted task heading, DESCRIPTION, and PROMPT."
  (let ((text
         (format "* Agent Task: %s\n:PROPERTIES:\n:%s: %s\n:END:\n\n%s\n"
                 description
                 niwa-agent-task-path-property
                 (niwa-agent-invocation-require-path invocation)
                 prompt)))
    (insert text)
    text))

(provide 'niwa-compact-test-support)

;;; niwa-compact-test-support.el ends here
