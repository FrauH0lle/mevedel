;;; mevedel-compact-test-support.el -- Shared compaction test setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-flush "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-path "mevedel-telemetry" (session))
(autoload 'mevedel-telemetry-flush "mevedel-telemetry")
(autoload 'mevedel-telemetry-path "mevedel-telemetry")

(defconst test-mevedel-compact--valid-summary
  "## Scope\n- test\n## Constraints & Preferences\n- none\n## Work & Evidence\n- test\n## Key Decisions\n- none\n## Open Questions & Risks\n- none\n## Critical Context\n- none\n## Relevant Files\n- none\n## Skills Invoked\n- none\n## Next Steps\n- test"
  "Valid continuation summary used by compaction tests.")

(defun test-mevedel-compact--read-telemetry (session)
  "Return the telemetry events SESSION has written, oldest first."
  (mevedel-telemetry-flush session)
  (when-let* ((path (mevedel-telemetry-path session))
              ((file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let (events)
        (condition-case nil
            (while t (push (read (current-buffer)) events))
          (end-of-file nil))
        (nreverse events)))))

(defun test-mevedel-compact--failing-hook (_event)
  "Signal the hook failure used by fail-closed compaction tests."
  (error "Hook failed"))

(cl-defmacro test-mevedel-compact--with-persisted-buffer
    ((buffer session) &rest body)
  "Run BODY in persisted BUFFER owned by SESSION."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "mevedel-compact-test-" t))
          (workspace
           (mevedel-workspace-get-or-create
            'project "compact-test" tempdir "compact-test"))
          (,session (mevedel-session-create "main" workspace))
          (,buffer (generate-new-buffer " *mevedel-compact-test*")))
     (unwind-protect
         (with-current-buffer ,buffer
           (org-mode)
           (setq-local mevedel--session ,session)
           (mevedel-session-artifacts-ensure-files ,session ,buffer)
           ,@body)
       ;; Release through the session: a bare directory has no authority
       ;; profile of its own.
       (mevedel-session-persistence-lock-release
        (or (mevedel-session-save-path ,session) tempdir) ,session)
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           (set-buffer-modified-p nil))
         (kill-buffer ,buffer))
       (mevedel-workspace-clear-registry)
       (delete-directory tempdir t))))

(cl-defmacro test-mevedel-compact--with-persisted-agent
    ((buffer invocation session canonical-path parent-buffer) &rest body)
  "Run BODY in persisted agent BUFFER with INVOCATION and SESSION."
  (declare
   (indent 1)
   (debug ((symbolp symbolp symbolp symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "mevedel-compact-agent-test-" t))
          (workspace
           (mevedel-workspace-get-or-create
            'project "compact-agent-test" tempdir "compact-agent-test"))
          (,session (mevedel-session-create "main" workspace))
          (,parent-buffer
           (generate-new-buffer " *mevedel-compact-parent*"))
          (agent (mevedel-agent--create :name "explorer"))
          (,invocation (mevedel-agent-invocation-create agent))
          (,buffer (generate-new-buffer " *mevedel-compact-agent*"))
          (,canonical-path nil))
     (unwind-protect
         (progn
           (with-current-buffer ,parent-buffer
             (org-mode)
             (setq-local mevedel--session ,session)
             (mevedel-session-artifacts-ensure-files
              ,session parent-buffer))
           (let ((relative-path "agents/explorer-test.chat.org"))
             (setq ,canonical-path
                   (expand-file-name relative-path
                                     (mevedel-session-save-path ,session)))
             (make-directory (file-name-directory ,canonical-path) t)
             (setf (mevedel-agent-invocation-agent-id ,invocation)
                   "explorer--test")
             (setf (mevedel-agent-invocation-path ,invocation)
                   "/root/explorer")
             (setf (mevedel-agent-invocation-buffer ,invocation) ,buffer)
             (setf (mevedel-agent-invocation-parent-data-buffer ,invocation)
                   ,parent-buffer)
             (setf (mevedel-agent-invocation-parent-session ,invocation)
                   ,session)
             (setf
              (mevedel-agent-invocation-transcript-relative-path ,invocation)
              relative-path)
             (setf (mevedel-agent-invocation-transcript-status ,invocation)
                   'running)
             (with-current-buffer ,buffer
               (org-mode)
               (setq-local mevedel--agent-invocation ,invocation)
               (set-visited-file-name ,canonical-path t t)
               ,@body)))
       ;; Release through the session: a bare directory has no authority
       ;; profile of its own.
       (mevedel-session-persistence-lock-release
        (or (mevedel-session-save-path ,session) tempdir) ,session)
       (dolist (candidate (list ,buffer ,parent-buffer))
         (when (buffer-live-p candidate)
           (with-current-buffer candidate
             (set-buffer-modified-p nil))
           (kill-buffer candidate)))
       (mevedel-workspace-clear-registry)
       (delete-directory tempdir t))))

(defun test-mevedel-compact--insert-agent-task
    (invocation description prompt)
  "Insert INVOCATION's persisted task heading, DESCRIPTION, and PROMPT."
  (let ((text
         (format "* Agent Task: %s\n:PROPERTIES:\n:%s: %s\n:END:\n\n%s\n"
                 description
                 mevedel-agent-task-path-property
                 (mevedel-agent-invocation-require-path invocation)
                 prompt)))
    (insert text)
    text))

(provide 'mevedel-compact-test-support)

;;; mevedel-compact-test-support.el ends here
