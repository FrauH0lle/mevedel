;;; niwa-init.el -- Repository guidance bootstrap command -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the `/init' workflow.  The command sends a repository
;; bootstrap prompt that asks the model to inspect the current workspace
;; and synthesize niwa-native guidance files, skills, and hooks.

;;; Code:

(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-system)

;; `niwa-chat'
(declare-function niwa--active-chat-buffer "niwa-chat" (&optional workspace))
(declare-function niwa--send-request "niwa-chat" (&optional model-input))
(autoload 'niwa--send-request "niwa-chat")

;; `niwa-compact-run'
(defvar niwa-compact-run-in-flight)

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)
(defvar niwa-session--read-only-mode)

;; `niwa-utilities'
(declare-function niwa--insert-user-turn
                  "niwa-utilities" (input))
(autoload 'niwa--insert-user-turn "niwa-utilities")

;; `niwa-view-composer'
(declare-function niwa-view--forward-input
                  "niwa-view-composer" (input &rest args))
(autoload 'niwa-view--forward-input "niwa-view-composer")

;; `niwa-view-history'
(declare-function niwa-view-history-add "niwa-view-history" (text))
(autoload 'niwa-view-history-add "niwa-view-history")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))


;;
;;; Helpers

(defun niwa-init--data-buffer-p (buffer)
  "Return non-nil when BUFFER is a niwa chat data buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'niwa--session)
              niwa--session
              (not (and (boundp 'niwa--data-buffer)
                        niwa--data-buffer))))))

(defun niwa-init--current-data-buffer ()
  "Return the current niwa data buffer, or nil outside niwa."
  (cond
   ((and (boundp 'niwa--data-buffer)
         niwa--data-buffer
         (niwa-init--data-buffer-p niwa--data-buffer))
    niwa--data-buffer)
   ((niwa-init--data-buffer-p (current-buffer))
    (current-buffer))
   ((fboundp 'niwa--active-chat-buffer)
    (niwa--active-chat-buffer))))

(defun niwa-init--session (data-buffer)
  "Return DATA-BUFFER's niwa session."
  (and (buffer-live-p data-buffer)
       (buffer-local-value 'niwa--session data-buffer)))

(defun niwa-init--workspace (data-buffer)
  "Return DATA-BUFFER's workspace."
  (or (and-let* ((session (niwa-init--session data-buffer)))
        (niwa-session-workspace session))
      (with-current-buffer data-buffer
        (niwa-workspace))))

(defun niwa-init--working-directory (data-buffer)
  "Return DATA-BUFFER's effective working directory."
  (file-name-as-directory
   (expand-file-name
    (or (and-let* ((session (niwa-init--session data-buffer)))
          (niwa-session-working-directory session))
        (buffer-local-value 'default-directory data-buffer)))))

(defun niwa-init--workspace-root (data-buffer)
  "Return DATA-BUFFER's workspace root, falling back to its cwd."
  (file-name-as-directory
   (expand-file-name
    (or (and-let* ((workspace (niwa-init--workspace data-buffer)))
          (niwa-workspace-root workspace))
        (niwa-init--working-directory data-buffer)))))

(defun niwa-init--display-text (focus)
  "Return user-facing slash display text for FOCUS."
  (let ((focus (and focus (string-trim focus))))
    (if (and focus (not (string-empty-p focus)))
        (format "/init %s" focus)
      "/init")))

(defun niwa-init--prompt (focus data-buffer)
  "Return the model prompt for `/init' with FOCUS in DATA-BUFFER."
  (let* ((root (niwa-init--workspace-root data-buffer))
         (cwd (niwa-init--working-directory data-buffer))
         (focus (or (and focus (string-trim focus)) "")))
    (niwa-system-render-prompt-file
     "prompts/init/setup.md"
     `(("ARGUMENTS" . ,focus)
       ("WORKSPACE_ROOT" . ,root)
       ("WORKING_DIRECTORY" . ,cwd)
       ("TARGET_FILE" . ,(file-name-concat root "AGENTS.md"))
       ("LOCAL_TARGET_FILE" . ,(file-name-concat root "AGENTS.local.md"))
       ("PROJECT_SKILLS_DIR" . ,(file-name-concat root ".agents" "skills"))
       ("USER_SKILLS_DIR" . ,(file-name-as-directory
                              (expand-file-name "~/.agents/skills")))
       ("PROJECT_MEMORY_DIR" . ,(file-name-concat root ".agents" "memory"))
       ("USER_MEMORY_DIR" . ,(file-name-as-directory
                              (expand-file-name "~/.agents/memory")))
       ("HOOKS_FILE" . ,(file-name-concat root ".niwa" "hooks.json"))
       ("HOOKS_DIR" . ,(file-name-concat root ".niwa" "hooks"))))))

(defun niwa-init--view-buffer (data-buffer)
  "Return DATA-BUFFER's live view buffer, if any."
  (let ((view (and (buffer-live-p data-buffer)
                   (buffer-local-value 'niwa--view-buffer data-buffer))))
    (and (buffer-live-p view) view)))

(defun niwa-init--ensure-sendable ()
  "Signal if the current data buffer cannot send an init turn."
  (when (bound-and-true-p niwa--current-request)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "A request is already active -- wait or abort first"))
  (when (bound-and-true-p niwa-compact-run-in-flight)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Compaction in progress"))
  (when (bound-and-true-p niwa-session--read-only-mode)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Session is open read-only (another host holds the lock)")))

(defun niwa-init--send-direct (prompt data-buffer)
  "Insert PROMPT into DATA-BUFFER and send through its selected engine."
  (with-current-buffer data-buffer
    (niwa-init--ensure-sendable)
    (goto-char (point-max))
    (niwa--insert-user-turn prompt)
    (niwa--send-request prompt)))

(defun niwa-init--dispatch (focus)
  "Dispatch `/init' with optional FOCUS."
  (let* ((data-buffer (or (niwa-init--current-data-buffer)
                          (niwa-diagnostic-user-error 'unavailable "No niwa chat buffer available")))
         (display (niwa-init--display-text focus))
         (prompt (niwa-init--prompt focus data-buffer))
         (view-buffer (niwa-init--view-buffer data-buffer)))
    (if view-buffer
        (progn
          (with-current-buffer view-buffer
            (niwa-view--forward-input
             prompt
             :display-text display
             :before-send
             (lambda ()
               (niwa-view-history-add display))))
          'niwa-view-sent)
      (message "niwa: sending /init")
      (niwa-init--send-direct prompt data-buffer))))


;;
;;; Commands

;;;###autoload
(defun niwa-init (&optional focus)
  "Start the repository guidance bootstrap workflow.
FOCUS is optional free-form guidance for what `/init' should emphasize."
  (interactive
   (list
    (let ((value (read-string "Init focus (optional): ")))
      (unless (string-empty-p value)
        value))))
  (niwa-init--dispatch focus))

(defun niwa-init-install-slash-command ()
  "Install `/init' into `niwa-slash-commands'."
  (setf (alist-get "init" niwa-slash-commands nil nil #'equal)
        #'niwa-init))

(niwa-init-install-slash-command)

(provide 'niwa-init)

;;; niwa-init.el ends here
