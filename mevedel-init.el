;;; mevedel-init.el -- Repository guidance bootstrap command -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the `/init' workflow.  The command sends a repository
;; bootstrap prompt that asks the model to inspect the current workspace
;; and synthesize mevedel-native guidance files, skills, and hooks.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)
(require 'mevedel-skills)
(require 'mevedel-structs)
(require 'mevedel-system)

;; `gptel'
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))

;; `mevedel-view-composer'
(declare-function mevedel-view--fork-if-pending "mevedel-view-composer" ())
(declare-function mevedel-view--forward-input
                  "mevedel-view-composer"
                  (input &optional display-text before-send prompt-checked
                         on-block hook-context hook-audits))
(declare-function mevedel-view-history-add "mevedel-view-history" (text))

;; `mevedel-structs'
(defvar mevedel--compaction-in-flight)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel-session--read-only-mode)


;;
;;; Helpers

(defun mevedel-init--data-buffer-p (buffer)
  "Return non-nil when BUFFER is a mevedel chat data buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'mevedel--session)
              mevedel--session
              (not (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))))))

(defun mevedel-init--current-data-buffer ()
  "Return the current mevedel data buffer, or nil outside mevedel."
  (cond
   ((and (boundp 'mevedel--data-buffer)
         mevedel--data-buffer
         (mevedel-init--data-buffer-p mevedel--data-buffer))
    mevedel--data-buffer)
   ((mevedel-init--data-buffer-p (current-buffer))
    (current-buffer))
   ((fboundp 'mevedel--active-chat-buffer)
    (mevedel--active-chat-buffer))))

(defun mevedel-init--session (data-buffer)
  "Return DATA-BUFFER's mevedel session."
  (and (buffer-live-p data-buffer)
       (buffer-local-value 'mevedel--session data-buffer)))

(defun mevedel-init--workspace (data-buffer)
  "Return DATA-BUFFER's workspace."
  (or (and-let* ((session (mevedel-init--session data-buffer)))
        (mevedel-session-workspace session))
      (with-current-buffer data-buffer
        (mevedel-workspace))))

(defun mevedel-init--working-directory (data-buffer)
  "Return DATA-BUFFER's effective working directory."
  (file-name-as-directory
   (expand-file-name
    (or (and-let* ((session (mevedel-init--session data-buffer)))
          (mevedel-session-working-directory session))
        (buffer-local-value 'default-directory data-buffer)))))

(defun mevedel-init--workspace-root (data-buffer)
  "Return DATA-BUFFER's workspace root, falling back to its cwd."
  (file-name-as-directory
   (expand-file-name
    (or (and-let* ((workspace (mevedel-init--workspace data-buffer)))
          (mevedel-workspace-root workspace))
        (mevedel-init--working-directory data-buffer)))))

(defun mevedel-init--path (root &rest segments)
  "Return path under ROOT from SEGMENTS."
  (apply #'file-name-concat root segments))

(defun mevedel-init--display-text (focus)
  "Return user-facing slash display text for FOCUS."
  (let ((focus (and focus (string-trim focus))))
    (if (and focus (not (string-empty-p focus)))
        (format "/init %s" focus)
      "/init")))

(defun mevedel-init--prompt (focus data-buffer)
  "Return the model prompt for `/init' with FOCUS in DATA-BUFFER."
  (let* ((root (mevedel-init--workspace-root data-buffer))
         (cwd (mevedel-init--working-directory data-buffer))
         (focus (or (and focus (string-trim focus)) "")))
    (mevedel-system-render-prompt-file
     "prompts/init/setup.md"
     `(("ARGUMENTS" . ,focus)
       ("WORKSPACE_ROOT" . ,root)
       ("WORKING_DIRECTORY" . ,cwd)
       ("TARGET_FILE" . ,(mevedel-init--path root "AGENTS.md"))
       ("LOCAL_TARGET_FILE" . ,(mevedel-init--path root "AGENTS.local.md"))
       ("PROJECT_SKILLS_DIR" . ,(mevedel-init--path root ".agents" "skills"))
       ("USER_SKILLS_DIR" . ,(file-name-as-directory
                              (expand-file-name "~/.agents/skills")))
       ("PROJECT_MEMORY_DIR" . ,(mevedel-init--path root ".agents" "memory"))
       ("USER_MEMORY_DIR" . ,(file-name-as-directory
                              (expand-file-name "~/.agents/memory")))
       ("HOOKS_FILE" . ,(mevedel-init--path root ".mevedel" "hooks.json"))
       ("HOOKS_DIR" . ,(mevedel-init--path root ".mevedel" "hooks"))))))

(defun mevedel-init--view-buffer (data-buffer)
  "Return DATA-BUFFER's live view buffer, if any."
  (let ((view (and (buffer-live-p data-buffer)
                   (buffer-local-value 'mevedel--view-buffer data-buffer))))
    (and (buffer-live-p view) view)))

(defun mevedel-init--ensure-sendable ()
  "Signal if the current data buffer cannot send an init turn."
  (when (bound-and-true-p mevedel--current-request)
    (user-error "A request is already active -- wait or abort first"))
  (when (bound-and-true-p mevedel--compaction-in-flight)
    (user-error "Compaction in progress"))
  (when (bound-and-true-p mevedel-session--read-only-mode)
    (user-error "Session is open read-only (another host holds the lock)")))

(defun mevedel-init--send-direct (prompt data-buffer)
  "Insert PROMPT into DATA-BUFFER and send it with `gptel-send'."
  (require 'mevedel-utilities)
  (with-current-buffer data-buffer
    (mevedel-init--ensure-sendable)
    (goto-char (point-max))
    (let ((user-turn-start (point)))
      (insert gptel-response-separator)
      (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (let ((prefix-length (length prefix)))
          (unless (and (>= (point) (+ (point-min) prefix-length))
                       (string= (buffer-substring-no-properties
                                 (- (point) prefix-length) (point))
                                prefix))
            (unless (bolp) (insert "\n"))
            (insert prefix))))
      (insert prompt "\n")
      (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
    (gptel-send)))

(defun mevedel-init--dispatch (focus)
  "Dispatch `/init' with optional FOCUS."
  (let* ((data-buffer (or (mevedel-init--current-data-buffer)
                          (user-error "No mevedel chat buffer available")))
         (display (mevedel-init--display-text focus))
         (prompt (mevedel-init--prompt focus data-buffer))
         (view-buffer (mevedel-init--view-buffer data-buffer)))
    (if view-buffer
        (progn
          (with-current-buffer view-buffer
            (mevedel-view--forward-input
             prompt display
             (lambda ()
               (mevedel-view-history-add display)
               (mevedel-view--fork-if-pending))))
          'mevedel-view-sent)
      (message "mevedel: sending /init")
      (mevedel-init--send-direct prompt data-buffer))))


;;
;;; Commands

;;;###autoload
(defun mevedel-init (&optional focus)
  "Start the repository guidance bootstrap workflow.
FOCUS is optional free-form guidance for what `/init' should emphasize."
  (interactive
   (list
    (let ((value (read-string "Init focus (optional): ")))
      (unless (string-empty-p value)
        value))))
  (mevedel-init--dispatch focus))

(defun mevedel-cmd--init (args)
  "Run `/init' with optional ARGS."
  (mevedel-init args))

(defun mevedel-init-install-slash-command ()
  "Install `/init' into `mevedel-slash-commands'."
  (setf (alist-get "init" mevedel-slash-commands nil nil #'equal)
        #'mevedel-cmd--init))

(mevedel-init-install-slash-command)

(provide 'mevedel-init)

;;; mevedel-init.el ends here
