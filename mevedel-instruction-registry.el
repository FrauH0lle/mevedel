;;; mevedel-instruction-registry.el -- Workspace instruction registry, IDs, and links -*- lexical-binding: t -*-

;;; Commentary:

;; Owns workspace-scoped instruction buckets, traversal, identity lookup, and links.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-directive-source'
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))

;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-with-id
                  "mevedel-instruction-registry"
                  (target-id &optional workspace))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))

;; `mevedel-overlays'
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
(defvar mevedel--highlighted-instruction)

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions
                  "mevedel-persistence"
                  (file &optional message workspace))

;; `mevedel-structs'
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-set-directives
                  "mevedel-structs" (workspace directives))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

(defvar mevedel--instruction-states (make-hash-table :test #'equal)
  "Workspace-keyed instruction state table.

Each value is a plist with keys `:instructions', `:id-counter',
`:id-usage-map', and `:retired-ids'.")

(defvar-local mevedel--instruction-current-state-key nil
  "Workspace key selected for this buffer's instruction operations.")

(defvar mevedel--instruction-state-key-override nil
  "Dynamically bound instruction state key for explicit operations.")

(defun mevedel--instruction-operation-state-key ()
  "Return the explicit or buffer-derived instruction state key."
  (or mevedel--instruction-state-key-override
      (and mevedel--instruction-current-state-key
           (not (eq mevedel--instruction-current-state-key :global))
           mevedel--instruction-current-state-key)
      (when-let* ((workspace
                   (mevedel--instruction-buffer-workspace (current-buffer))))
        (mevedel--instruction-workspace-key workspace))
      mevedel--instruction-current-state-key
      :global))

(defmacro mevedel--instruction-alist ()
  "Return the active workspace's instruction alist as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :instructions))

(defmacro mevedel--instruction-id-counter ()
  "Return the active workspace's instruction ID counter as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :id-counter))

(defmacro mevedel--instruction-id-usage-map ()
  "Return the active workspace's used-ID table as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :id-usage-map))

(defmacro mevedel--instruction-retired-ids ()
  "Return the active workspace's retired IDs as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :retired-ids))

(gv-define-setter mevedel--instruction-state (value &optional key)
  `(puthash (or ,key :global) ,value mevedel--instruction-states))

(defun mevedel--instruction-alist-value ()
  "Return the active workspace's instruction alist."
  (mevedel--instruction-alist))

(defun mevedel--set-instruction-alist-value (value)
  "Set the active workspace's instruction alist to VALUE."
  (setf (mevedel--instruction-alist) value))

(defun mevedel--instruction-workspace-key (&optional workspace)
  "Return the instruction-state key for WORKSPACE."
  (require 'mevedel-structs)
  (if workspace
      (cons (mevedel-workspace-type workspace)
            (mevedel-workspace-id workspace))
    :global))

(defun mevedel--instruction-buffer-workspace (buffer)
  "Return BUFFER's workspace, or nil when it cannot be resolved."
  (require 'mevedel-workspace)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (boundp 'mevedel--workspace)
               (bound-and-true-p mevedel--workspace))
          (ignore-errors (mevedel-workspace buffer))))))

(defun mevedel--instruction-state (&optional key)
  "Return instruction state plist for KEY, creating it if needed."
  (let ((key (or key :global)))
    (or (gethash key mevedel--instruction-states)
        (puthash key
                 (list :instructions nil
                       :id-counter 0
                       :id-usage-map (make-hash-table)
                       :retired-ids nil)
                 mevedel--instruction-states))))

(defun mevedel--instruction-activate-workspace (&optional workspace)
  "Make WORKSPACE's instruction state current."
  (let ((key (mevedel--instruction-workspace-key workspace)))
    (mevedel--instruction-state key)
    (setq mevedel--instruction-current-state-key key))
  mevedel--instruction-current-state-key)

(defun mevedel--instruction-activate-buffer (&optional buffer)
  "Make BUFFER's workspace instruction state current."
  (mevedel--instruction-activate-workspace
   (mevedel--instruction-buffer-workspace (or buffer (current-buffer)))))

(defun mevedel--instruction-state-rollback (workspace)
  "Return a function that restores WORKSPACE's exact instruction state."
  (require 'mevedel-structs)
  (let* ((key (mevedel--instruction-workspace-key workspace))
         (state (copy-tree (mevedel--instruction-state key)))
         (current-key mevedel--instruction-current-state-key)
         (highlighted mevedel--highlighted-instruction)
         (directives (copy-sequence (mevedel-workspace-directives workspace)))
         overlays)
    (dolist (entry (plist-get state :instructions))
      (when (bufferp (car entry))
        (dolist (instruction (cdr entry))
          (when (overlayp instruction)
            (push (list instruction
                        (overlay-buffer instruction)
                        (overlay-start instruction)
                        (overlay-end instruction))
                  overlays)))))
    (lambda ()
      (mevedel--clear-instruction-state workspace)
      (puthash key state mevedel--instruction-states)
      (setq mevedel--instruction-current-state-key current-key
            mevedel--highlighted-instruction highlighted)
      (mevedel-workspace-set-directives workspace directives)
      (dolist (snapshot overlays)
        (when (buffer-live-p (nth 1 snapshot))
          (move-overlay (nth 0 snapshot)
                        (nth 2 snapshot) (nth 3 snapshot)
                        (nth 1 snapshot)))))))

(defun mevedel--clear-instruction-state (&optional workspace)
  "Delete all visible instruction overlays in WORKSPACE and clear its state."
  (require 'mevedel-structs)
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace)))
    (dolist (entry (mevedel--instruction-alist))
      (when (bufferp (car entry))
        (dolist (instr (cdr entry))
          (when (overlayp instr)
            (delete-overlay instr)))))
    (setf (mevedel--instruction-alist) nil)
    (setq mevedel--highlighted-instruction nil)
    (setf (mevedel--instruction-id-counter) 0)
    (setf (mevedel--instruction-id-usage-map) (make-hash-table))
    (setf (mevedel--instruction-retired-ids) nil)
    (when workspace
      (mevedel-workspace-set-directives workspace nil))))

(defmacro mevedel--foreach-instruction (binding &rest body)
  "Iterate over `(mevedel--instruction-alist)' with BINDING as the binding.

Executes BODY inside an existing `cl-loop' form, which means that the
macro is expecting for BODY to be written in the `cl-loop' DSL.

BINDING can either be a symbol to bind the instruction to, or a list
where the `car' is the symbol binding and the `cadr' is a buffer.

If the buffer inside BINDING is non-nil, only iterate over the
instructions that are located inside that buffer.

The purpose of this macro is to be able to iterate over instructions
while also making sure that the iterated instructions are valid, i.e.
have an associated buffer to the overlay.

This macro is the preferred way to iterate over instructions, as it
handles all the internal bookkeeping and cleanup."
  (declare (indent 1))
  ;; "bof" stands for "buffer or file".
  (cl-with-gensyms (cons bof specific-buffer)
    (let ((instr (if (listp binding) (car binding) binding)))
      `(cl-labels ((trashp (instr)
                     (and (null (overlay-buffer instr))
                          (not (overlay-get instr 'mevedel-marked-for-deletion))))
                   (clean-alist-entry (cons)
                     (require 'mevedel-directive-source)
                     (mapc (lambda (instr)
                             (mevedel--delete-instruction instr (car cons)))
                           (cl-remove-if-not #'trashp (cdr cons)))
                     (let ((instrs (cl-remove-if #'trashp (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (mevedel--instruction-activate-workspace
            (if (bufferp ,specific-buffer)
                (mevedel--instruction-buffer-workspace ,specific-buffer)
              (mevedel--instruction-buffer-workspace (current-buffer))))
           (if (null ,specific-buffer)
               (cl-loop for ,cons in (mevedel--instruction-alist)
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (progn
                                   (require 'mevedel-persistence)
                                   (mevedel--restore-file-instructions ,bof))
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when (stringp ,specific-buffer)
               (require 'mevedel-persistence)
               (cl-destructuring-bind (buffer _ _) (mevedel--restore-file-instructions ,specific-buffer)
                 (setq ,specific-buffer buffer)))
             (when-let* ((cons (assoc ,specific-buffer (mevedel--instruction-alist))))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setf (mevedel--instruction-alist) (cl-remove-if (lambda (cons)
                                                       (null (cdr cons)))
                                                     (mevedel--instruction-alist)))
           ;; The instructions alist should now be cleaned of deleted
           ;; instructions.
           (cl-loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer (mevedel--instruction-alist))
                         (flatten-tree
                          (cl-remove nil
                                     (mapcar (lambda (plist-or-instrs)
                                               (if (plist-get plist-or-instrs :instructions)
                                                   nil ; Plist
                                                 plist-or-instrs))
                                             (mapcar #'cdr (mevedel--instruction-alist))))))
                    ,@body))))))

(defun mevedel-link-instructions (from-list to-list)
  "Link instructions with ids in FROM-LIST to those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (mevedel--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link to: "
                                             completion-table nil t))))))
  (require 'mevedel-overlay-ui)
  (mevedel--instruction-activate-buffer)
  (cl-labels
      ((update-links (instr-id num-key update-id)
         (let* ((instr (mevedel--instruction-with-id instr-id))
                (links (overlay-get instr 'mevedel-links))
                (ids (plist-get links num-key)))
           (unless (member update-id ids)
             (setq ids (cons update-id ids))
             (overlay-put instr 'mevedel-links (plist-put links num-key ids))
             t))))
    (let ((new-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (mevedel--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when (/= from-id to-id)
              (when-let* ((to-instr (mevedel--instruction-with-id to-id)))
                (when (and (update-links from-id :to to-id)
                           (update-links to-id :from from-id))
                  (puthash from-instr t involved-instrs)
                  (puthash to-instr t involved-instrs)
                  (cl-incf new-link-count)))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (mevedel--update-instruction-overlay instr))
      (when (called-interactively-p 'interactive)
        (message "Created %d instruction link%s"
                 new-link-count
                 (if (= new-link-count 1) "" "s"))))))

(defun mevedel-unlink-instructions (from-list to-list)
  "Unlink instructions with ids in FROM-LIST from those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (mevedel--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink from: "
                                             completion-table nil t))))))
  (require 'mevedel-overlay-ui)
  (mevedel--instruction-activate-buffer)
  (cl-labels
      ((remove-links (instr-id num-key remove-id)
         (let* ((instr (mevedel--instruction-with-id instr-id))
                (links (overlay-get instr 'mevedel-links))
                (ids (plist-get links num-key)))
           (when (member remove-id ids)
             (setq ids (remove remove-id ids))
             (overlay-put instr 'mevedel-links (plist-put links num-key ids))
             t))))
    (let ((removed-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (mevedel--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when-let* ((to-instr (mevedel--instruction-with-id to-id)))
              (when (and (remove-links from-id :to to-id)
                         (remove-links to-id :from from-id))
                (puthash from-instr t involved-instrs)
                (puthash to-instr t involved-instrs)
                (cl-incf removed-link-count))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (when (buffer-live-p (overlay-buffer instr))
                    (mevedel--update-instruction-overlay instr)))
      (when (called-interactively-p 'interactive)
        (message "Removed %d instruction link%s"
                 removed-link-count
                 (if (= removed-link-count 1) "" "s"))))))

(defun mevedel--instruction-find-unique-live (predicate)
  "Return the unique live instruction satisfying PREDICATE.
Return nil when no instruction matches or when more than one does."
  (let (found ambiguous)
    (maphash
     (lambda (_key state)
       (dolist (entry (plist-get state :instructions))
         (when (bufferp (car entry))
           (dolist (instruction (cdr entry))
             (when (and (overlayp instruction)
                        (buffer-live-p (overlay-buffer instruction))
                        (funcall predicate instruction))
               (if found
                   (setq ambiguous t)
                 (setq found instruction)))))))
     mevedel--instruction-states)
    (and (not ambiguous) found)))

(let ((map (make-hash-table))
      (map-key nil))
  (cl-defun mevedel--instruction-with-id (target-id &optional workspace)
    "Return the instruction with integer TARGET-ID in WORKSPACE.

When WORKSPACE is nil, use the current buffer's workspace and fall back
to a unique live match across workspaces only when that local state is
empty.  Return nil when no unambiguous instruction is found."
    (cl-labels ((entry-live-p (entry)
                  (and (bufferp (car entry))
                       (cl-some (lambda (instr)
                                  (and (overlayp instr)
                                       (buffer-live-p
                                        (overlay-buffer instr))))
                                (cdr entry))))
                (current-state-live-p ()
                  (cl-some #'entry-live-p (mevedel--instruction-alist))))
      (let* ((lookup-workspace
              (or workspace
                  (mevedel--instruction-buffer-workspace (current-buffer))))
             (lookup-key (mevedel--instruction-workspace-key lookup-workspace))
             (mevedel--instruction-state-key-override lookup-key))
        (unless (equal map-key lookup-key)
          (setq map (make-hash-table)
                map-key lookup-key))
        (when-let* ((instr (gethash target-id map)))
          (when (buffer-live-p (overlay-buffer instr))
            (cl-return-from mevedel--instruction-with-id instr)))
        (setq map (make-hash-table))
        (mevedel--foreach-instruction instr
          do (puthash (mevedel--instruction-id instr) instr map))
        (or (gethash target-id map)
            (and (null workspace)
                 (not (current-state-live-p))
                 (mevedel--instruction-find-unique-live
                  (lambda (instruction)
                    (= target-id
                       (mevedel--instruction-id instruction))))))))))

(defun mevedel--instruction-with-uuid (uuid &optional workspace)
  "Return the instruction carrying UUID in WORKSPACE.
Stashed instructions are restored before lookup.  When WORKSPACE is nil,
fall back to a unique live match across workspaces for prompt-copy callers
that have no workspace context."
  (or
   (let ((mevedel--instruction-state-key-override
          (and workspace (mevedel--instruction-workspace-key workspace))))
     (mevedel--foreach-instruction instr
       when (equal uuid (overlay-get instr 'mevedel-uuid))
       return instr))
   (unless workspace
     (mevedel--instruction-find-unique-live
      (lambda (instruction)
        (equal uuid (overlay-get instruction 'mevedel-uuid)))))))

(defun mevedel--instruction-id (instruction)
  "Return unique identifier for INSTRUCTION overlay."
  (overlay-get instruction 'mevedel-id))

(defun mevedel--find-directive-by-uuid (uuid)
  "Find directive overlay with UUID.
Returns the overlay, or nil if not found."
  (require 'mevedel-overlays)
  (when uuid
    (mevedel--foreach-instruction instr
      when (and (mevedel--directivep instr)
                (equal (overlay-get instr 'mevedel-uuid) uuid))
      return instr)))

(defun mevedel--create-id ()
  "Create a unique identifier for an instruction.

Retrieves an unused ID from retired IDs or generates a new one by
incrementing the ID counter.  Tracks ID usage via a hash table."
  (let ((id
         (if (mevedel--instruction-retired-ids)
             (prog1
                 (car (mevedel--instruction-retired-ids))
               (setf (mevedel--instruction-retired-ids) (cdr (mevedel--instruction-retired-ids))))
           (cl-incf (mevedel--instruction-id-counter)))))
    (puthash id t (mevedel--instruction-id-usage-map))
    id))

(defun mevedel--retire-id (id)
  "Retire an ID by removing it from `(mevedel--instruction-id-usage-map)'.
The id is added to `(mevedel--instruction-retired-ids)'"
  (when (gethash id (mevedel--instruction-id-usage-map))
    (remhash id (mevedel--instruction-id-usage-map))
    (push id (mevedel--instruction-retired-ids))))

(defun mevedel--instruction-outlinks (instruction)
  "Return the :to links of INSTRUCTION."
  (plist-get (overlay-get instruction 'mevedel-links) :to))

(defun mevedel--instruction-inlinks (instruction)
  "Return the :from links of INSTRUCTION."
  (plist-get (overlay-get instruction 'mevedel-links) :from))


(provide 'mevedel-instruction-registry)
;;; mevedel-instruction-registry.el ends here
