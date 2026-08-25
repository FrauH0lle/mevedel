;;; mevedel-ptc-checkpoint.el --- Durable ToolScript audit checkpoints -*- lexical-binding: t -*-

;;; Commentary:

;; Persists only the model-authored call and its child audit while a ToolScript
;; envelope is open.  Interpreter continuations remain process-local.  Restore
;; turns any surviving checkpoint into an ordinary interrupted or settled
;; ToolScript tool row and never resumes the guest machine.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts" (session buffer &optional settled force))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(autoload 'mevedel-session-persistence-write-sidecar-now
  "mevedel-session-persistence")

;; `mevedel-structs'
(declare-function mevedel-session-ptc-checkpoints
                  "mevedel-structs" (session))

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data" (render-data &optional tool-use-id))
(declare-function mevedel-tool-render-data-segment-bounds
                  "mevedel-tool-render-data" (tool-use-id))
(autoload 'mevedel-tool-render-data-format "mevedel-tool-render-data")
(autoload 'mevedel-tool-render-data-segment-bounds
  "mevedel-tool-render-data")

;; `org-src'
(declare-function org-escape-code-in-string "org-src" (string))
(autoload 'org-escape-code-in-string "org-src")

;; `seq'
(declare-function seq-find "seq" (predicate sequence &optional default))
(declare-function seq-remove "seq" (predicate sequence))


;;
;;; Checkpoint lifecycle

(defun mevedel-ptc-checkpoint--put (session checkpoint)
  "Replace CHECKPOINT in SESSION by its `:id'."
  (let ((id (plist-get checkpoint :id)))
    (setf (mevedel-session-ptc-checkpoints session)
          (cons checkpoint
                (seq-remove
                 (lambda (item) (equal id (plist-get item :id)))
                 (mevedel-session-ptc-checkpoints session))))))

(defun mevedel-ptc-checkpoint-start (session buffer id script)
  "Durably start ID for SESSION and BUFFER with SCRIPT."
  (when session
    (let ((before (mevedel-session-ptc-checkpoints session)))
      (condition-case err
          (progn
            (mevedel-ptc-checkpoint--put
             session
             (list :id id :args (list :script script) :state 'running
                   :render-data
                   (list :kind 'ptc :outcome 'running :calls nil)))
            ;; The first save both materializes a fresh session and records
            ;; the checkpoint before any nested tool can run.
            (or (mevedel-session-artifacts-save session buffer nil t)
                (progn
                  (setf (mevedel-session-ptc-checkpoints session) before)
                  nil)))
        (error
         (setf (mevedel-session-ptc-checkpoints session) before)
         (signal (car err) (cdr err)))))))

(defun mevedel-ptc-checkpoint--merge (session id updates)
  "Merge UPDATES into SESSION checkpoint ID in memory.
Return non-nil when the checkpoint exists."
  (when-let* ((checkpoint
               (seq-find
                (lambda (item) (equal id (plist-get item :id)))
                (mevedel-session-ptc-checkpoints session))))
    (setq checkpoint (copy-sequence checkpoint))
    (cl-loop for (key value) on updates by #'cddr do
             (setq checkpoint (plist-put checkpoint key value)))
    (mevedel-ptc-checkpoint--put session checkpoint)
    t))

(defun mevedel-ptc-checkpoint-note (session id updates)
  "Merge UPDATES into SESSION checkpoint ID in memory only.
No sidecar write happens here: per-child durability cost dominated
ToolScript runtime and serialized parallel batches, so intra-script
audit progress is journaled in memory and any unrelated autosave
captures it opportunistically.  The durable writes are the start and
settlement checkpoints.  Return non-nil when SESSION is nil or the
checkpoint exists."
  (if (not session)
      t
    (mevedel-ptc-checkpoint--merge session id updates)))

(defun mevedel-ptc-checkpoint-update (session buffer id updates)
  "Merge UPDATES into SESSION checkpoint ID and rewrite its sidecar.
Return non-nil when the checkpoint is durable or SESSION is nil."
  (if (not session)
      t
    (let ((before (mevedel-session-ptc-checkpoints session)))
      (condition-case err
          (when (mevedel-ptc-checkpoint--merge session id updates)
            (or (mevedel-session-persistence-write-sidecar-now
                 session buffer)
                (progn
                  (setf (mevedel-session-ptc-checkpoints session) before)
                  nil)))
        (error
         (setf (mevedel-session-ptc-checkpoints session) before)
         (signal (car err) (cdr err)))))))

(defun mevedel-ptc-checkpoint-clear-settled (session)
  "Remove settled ToolScript checkpoints from SESSION in memory.
The completed-turn save commits this change together with the final tool row."
  (when session
    (setf (mevedel-session-ptc-checkpoints session)
          (seq-remove
           (lambda (item) (eq 'settled (plist-get item :state)))
           (mevedel-session-ptc-checkpoints session)))))


;;
;;; Restore

(defun mevedel-ptc-checkpoint--interrupted-render-data (checkpoint)
  "Return recovery render data for unfinished CHECKPOINT."
  (let* ((data (copy-tree (plist-get checkpoint :render-data) t))
         (calls
          (mapcar
           (lambda (call)
             (let ((call (copy-sequence call)))
               (when (memq (plist-get call :status) '(queued running))
                 (setq call (plist-put call :status 'interrupted)))
               call))
           (plist-get data :calls))))
    (setq data (plist-put data :outcome 'interrupted))
    (setq data (plist-put data :calls calls))
    (setq data (plist-put data :nested-call-count (length calls)))
    data))

(defun mevedel-ptc-checkpoint--insert (checkpoint)
  "Append one recovered CHECKPOINT as a canonical Org tool block."
  (let* ((id (plist-get checkpoint :id))
         (settled-p (eq (plist-get checkpoint :state) 'settled))
         (result (if settled-p
                     (or (plist-get checkpoint :result) "")
                   "Error: script interrupted by Emacs restart"))
         (render-data
          (if settled-p
              (plist-get checkpoint :render-data)
            (mevedel-ptc-checkpoint--interrupted-render-data checkpoint)))
         (call (prin1-to-string
                (list :name "ToolScript" :args (plist-get checkpoint :args))))
         (body
          (org-escape-code-in-string
           (concat call "\n\n" result
                   (mevedel-tool-render-data-format render-data id))))
         (body-start nil)
         (body-end nil))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n#+begin_tool (ToolScript recovered after restart)\n")
    (setq body-start (point))
    (insert body)
    (setq body-end (point))
    (insert "#+end_tool\n")
    (put-text-property body-start body-end 'gptel (cons 'tool id))))

(defun mevedel-ptc-checkpoint-reconcile (session)
  "Settle surviving SESSION ToolScript checkpoints in the current transcript.
Return (INSERTED . CONSUMED), counting transcript rows and checkpoints."
  (let* ((checkpoints (mevedel-session-ptc-checkpoints session))
         (inserted 0))
    (dolist (checkpoint (reverse checkpoints))
      (unless (mevedel-tool-render-data-segment-bounds
               (plist-get checkpoint :id))
        (mevedel-ptc-checkpoint--insert checkpoint)
        (cl-incf inserted)))
    (setf (mevedel-session-ptc-checkpoints session) nil)
    (cons inserted (length checkpoints))))

(provide 'mevedel-ptc-checkpoint)

;;; mevedel-ptc-checkpoint.el ends here
