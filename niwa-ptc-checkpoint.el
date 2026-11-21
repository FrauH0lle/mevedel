;;; niwa-ptc-checkpoint.el --- Durable ToolCall audit checkpoints -*- lexical-binding: t -*-

;;; Commentary:

;; Persists only the model-authored call and its child audit while a ToolCall
;; envelope is open.  Interpreter continuations remain process-local.  Restore
;; turns any surviving checkpoint into an ordinary interrupted or settled
;; ToolCall tool row and never resumes the guest machine.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-checkpoint)
(require 'niwa-structs)

;; `niwa-agents'
(declare-function niwa-agent-invocation-path "niwa-agents" (cl-x))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts" (session buffer &optional settled force))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-root-buffer-for-session
                  "niwa-session-persistence" (session &optional buffer))
(declare-function niwa-session-persistence-write-sidecar-now
                  "niwa-session-persistence" (session buffer))
(autoload 'niwa-session-persistence-root-buffer-for-session
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-sidecar-now
  "niwa-session-persistence")

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data" (render-data &optional tool-use-id))
(declare-function niwa-tool-render-data-segment-bounds
                  "niwa-tool-render-data" (tool-use-id))
(autoload 'niwa-tool-render-data-format "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-segment-bounds
  "niwa-tool-render-data")

;; `org-src'
(declare-function org-escape-code-in-string "org-src" (string))
(autoload 'org-escape-code-in-string "org-src")

;; `seq'
(declare-function seq-find "seq" (predicate sequence &optional default))
(declare-function seq-remove "seq" (predicate sequence))


;;
;;; Checkpoint lifecycle

(defun niwa-ptc-checkpoint--conversation (buffer)
  "Return the retained conversation owning BUFFER, or the root path."
  (let* ((invocation (and (buffer-live-p buffer)
                          (buffer-local-value 'niwa--agent-invocation buffer)))
         (path (if invocation
                   (niwa-agent-invocation-path invocation)
                 "/root")))
    (unless (niwa-agent-path-p path)
      (error "Invalid ToolCall conversation: %S" path))
    path))

(defun niwa-ptc-checkpoint--put (session checkpoint)
  "Replace CHECKPOINT in SESSION by its `:id'."
  (let ((id (plist-get checkpoint :id)))
    (setf (niwa-session-ptc-checkpoints session)
          (cons checkpoint
                (seq-remove
                 (lambda (item) (equal id (plist-get item :id)))
                 (niwa-session-ptc-checkpoints session))))))

(defun niwa-ptc-checkpoint-start (session buffer id script)
  "Durably start ID for SESSION and BUFFER with SCRIPT."
  (when session
    (let ((before (niwa-session-ptc-checkpoints session))
          (root-buffer (or (niwa-session-persistence-root-buffer-for-session
                            session buffer)
                           buffer)))
      (condition-case err
          (progn
            (niwa-ptc-checkpoint--put
             session
             (list :id id :conversation (niwa-ptc-checkpoint--conversation buffer)
                   :args (list :expression script) :state 'running
                   :render-data
                   (list :kind 'ptc :outcome 'running :calls nil)))
            ;; Record the checkpoint before any nested tool can run.  Restore
            ;; rebuilds the row from the checkpoint alone, so an existing
            ;; sidecar is enough; only a fresh session needs the full save
            ;; that materializes it.  That save cost a quarter second per
            ;; call on a long session.
            (or (niwa-session-persistence-write-sidecar-now session root-buffer)
                (niwa-session-artifacts-save session root-buffer nil t)
                (progn
                  (setf (niwa-session-ptc-checkpoints session) before)
                  nil)))
        (error
         (setf (niwa-session-ptc-checkpoints session) before)
         (signal (car err) (cdr err)))))))

(defun niwa-ptc-checkpoint--merge (session id updates)
  "Merge UPDATES into SESSION checkpoint ID in memory.
Return non-nil when the checkpoint exists."
  (when-let* ((checkpoint
               (seq-find
                (lambda (item) (equal id (plist-get item :id)))
                (niwa-session-ptc-checkpoints session))))
    (setq checkpoint (copy-sequence checkpoint))
    (cl-loop for (key value) on updates by #'cddr do
             (setq checkpoint (plist-put checkpoint key value)))
    (niwa-ptc-checkpoint--put session checkpoint)
    t))

(defun niwa-ptc-checkpoint-note (session id updates)
  "Merge UPDATES into SESSION checkpoint ID in memory only.
No sidecar write happens here: per-child durability cost dominated
ToolCall runtime and serialized parallel batches, so intra-script
audit progress is journaled in memory and any unrelated autosave
captures it opportunistically.  The durable writes are the start and
settlement checkpoints.  Return non-nil when SESSION is nil or the
checkpoint exists."
  (if (not session)
      t
    (niwa-ptc-checkpoint--merge session id updates)))

(defun niwa-ptc-checkpoint-update (session buffer id updates)
  "Merge UPDATES into SESSION checkpoint ID and rewrite its sidecar.
Return non-nil when the checkpoint is durable or SESSION is nil."
  (if (not session)
      t
    (let ((before (niwa-session-ptc-checkpoints session))
          (root-buffer (or (niwa-session-persistence-root-buffer-for-session
                            session buffer)
                           buffer)))
      (condition-case err
          (when (niwa-ptc-checkpoint--merge session id updates)
            (or (niwa-session-persistence-write-sidecar-now
                 session root-buffer)
                (progn
                  (setf (niwa-session-ptc-checkpoints session) before)
                  nil)))
        (error
         (setf (niwa-session-ptc-checkpoints session) before)
         (signal (car err) (cdr err)))))))

(defun niwa-ptc-checkpoint-clear-settled (session &optional conversation buffer)
  "Defer settled SESSION ToolCalls owned by CONVERSATION, defaulting to root.
BUFFER defaults to the current transcript.  Only rows already present in
that transcript may retire; post-tool saves can precede the final tool row.
Remove live rows in favor of the transcript, retaining their full records in
the committed checkpoint owner until that transcript actually commits."
  (when session
    (let* ((conversation (or conversation "/root"))
           (buffer (or buffer (current-buffer)))
           (settled (cl-remove-if-not
                     (lambda (item)
                       (and (eq 'settled (plist-get item :state))
                            (equal conversation (plist-get item :conversation))
                            (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (niwa-tool-render-data-segment-bounds
                               (plist-get item :id)))))
                     (niwa-session-ptc-checkpoints session))))
      (niwa-session-checkpoint-defer-ptc session conversation settled)
      (setf (niwa-session-ptc-checkpoints session)
            (seq-remove (lambda (item) (memq item settled))
                        (niwa-session-ptc-checkpoints session))))))


;;
;;; Restore

(defun niwa-ptc-checkpoint--interrupted-render-data (checkpoint)
  "Return recovery render data for unfinished CHECKPOINT."
  (let* ((data (copy-tree (plist-get checkpoint :render-data) t))
         (calls
          (mapcar
           (lambda (call)
             (when (memq (plist-get call :status) '(queued running))
               (plist-put call :status 'interrupted))
             call)
           (plist-get data :calls))))
    (setq data (plist-put data :outcome 'interrupted))
    (setq data (plist-put data :calls calls))
    (plist-put data :nested-call-count (length calls))))

(defun niwa-ptc-checkpoint--insert (checkpoint)
  "Append one recovered CHECKPOINT as a canonical Org tool block."
  (let* ((id (plist-get checkpoint :id))
         (settled-p (eq (plist-get checkpoint :state) 'settled))
         (result (if settled-p
                     (or (plist-get checkpoint :result) "")
                   "Error: script interrupted by Emacs restart"))
         (render-data
          (if settled-p
              (plist-get checkpoint :render-data)
            (niwa-ptc-checkpoint--interrupted-render-data checkpoint)))
         (call (prin1-to-string
                (list :name "ToolCall" :args (plist-get checkpoint :args))))
         (body
          (org-escape-code-in-string
           (concat call "\n\n" result
                   (niwa-tool-render-data-format render-data id))))
         (body-start nil)
         (body-end nil))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n#+begin_tool (ToolCall recovered after restart)\n")
    (setq body-start (point))
    (insert body)
    (setq body-end (point))
    (insert "#+end_tool\n")
    (put-text-property body-start body-end 'gptel (cons 'tool id))))

(defun niwa-ptc-checkpoint-reconcile (session &optional conversation)
  "Settle SESSION ToolCalls belonging to the current CONVERSATION transcript.
CONVERSATION defaults to the current buffer's retained agent path or root.
Return (INSERTED . CONSUMED), counting transcript rows and checkpoints."
  (let* ((conversation (or conversation
                           (niwa-ptc-checkpoint--conversation (current-buffer))))
         (checkpoints
          (cl-remove-if-not
           (lambda (item) (equal conversation (plist-get item :conversation)))
           (niwa-session-ptc-checkpoints session)))
         (inserted 0))
    (dolist (checkpoint (reverse checkpoints))
      (unless (niwa-tool-render-data-segment-bounds
               (plist-get checkpoint :id))
        (niwa-ptc-checkpoint--insert checkpoint)
        (cl-incf inserted)))
    (niwa-session-checkpoint-defer-ptc session conversation checkpoints)
    (setf (niwa-session-ptc-checkpoints session)
          (seq-remove (lambda (item) (memq item checkpoints))
                      (niwa-session-ptc-checkpoints session)))
    (cons inserted (length checkpoints))))

(provide 'niwa-ptc-checkpoint)

;;; niwa-ptc-checkpoint.el ends here
