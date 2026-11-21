;;; niwa-instruction-registry.el -- Workspace instruction registry, IDs, and links -*- lexical-binding: t -*-

;;; Commentary:

;; Owns workspace-scoped instruction buckets, traversal, identity lookup, and links.

;;; Code:

;; `cl-lib' is a runtime dependency, not a compile-time one: these owners
;; call `cl-lib' functions -- `cl-remove', `cl-some', `cl-find' -- and
;; `cl-lib' autoloads none of them.  Loading this module compiled and
;; without the umbrella, as the split-owner contract allows, then hit a
;; void-function; loading it as source only worked because
;; `eval-when-compile' degrades to `progn' in interpreted code.
(require 'cl-lib)

;; `niwa-instruction-registry'
(declare-function niwa--instruction-with-id
                  "niwa-instruction-registry"
                  (target-id &optional workspace))

;; `niwa-note-source'
(declare-function niwa--delete-instruction
                  "niwa-note-source"
                  (instruction &optional buffer))
(autoload 'niwa--delete-instruction "niwa-note-source")

;; `niwa-notes'
(declare-function niwa-notes--refresh-label-stacks-in "niwa-notes" (start end))
(autoload 'niwa-notes--refresh-label-stacks-in "niwa-notes")

;; `niwa-overlay-ui'
(declare-function niwa--update-instruction-overlay
                  "niwa-overlay-ui"
                  (instruction &optional update-children))
(autoload 'niwa--update-instruction-overlay "niwa-overlay-ui")

;; `niwa-overlays'
(defvar niwa--highlighted-instruction)

;; `niwa-persistence'
(declare-function niwa--restore-file-instructions
                  "niwa-persistence"
                  (file &optional message workspace))
(autoload 'niwa--restore-file-instructions "niwa-persistence")

;; `niwa-structs'
(declare-function niwa-workspace-id "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-type "niwa-structs" (cl-x) t)
(autoload 'niwa-workspace-id "niwa-structs")
(autoload 'niwa-workspace-type "niwa-structs")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(autoload 'niwa-workspace "niwa-workspace")

(defvar niwa--instruction-states (make-hash-table :test #'equal)
  "Workspace-keyed instruction state table.

Each value is a plist with keys `:instructions', `:id-counter',
`:id-usage-map', and `:retired-ids', plus the instruction store's
`:store-observation' once the store is used.")

(defvar-local niwa--instruction-current-state-key nil
  "Workspace key selected for this buffer's instruction operations.")

(defvar niwa--instruction-state-key-override nil
  "Dynamically bound instruction state key for explicit operations.")

(defun niwa--instruction-operation-state-key ()
  "Return the explicit or buffer-derived instruction state key."
  (or niwa--instruction-state-key-override
      (and niwa--instruction-current-state-key
           (not (eq niwa--instruction-current-state-key :global))
           niwa--instruction-current-state-key)
      (when-let* ((workspace
                   (niwa--instruction-buffer-workspace (current-buffer))))
        (niwa--instruction-workspace-key workspace))
      niwa--instruction-current-state-key
      :global))

(defmacro niwa--instruction-alist ()
  "Return the active workspace's instruction alist as a settable place."
  ;; Copied, not shared: the byte compiler destructively attaches the
  ;; call position to a macro's returned form, which would corrupt a
  ;; literal for every later expansion.
  (copy-sequence
   '(plist-get (niwa--instruction-state
                (niwa--instruction-operation-state-key))
               :instructions)))

(defmacro niwa--instruction-id-counter ()
  "Return the active workspace's instruction ID counter as a settable place."
  ;; Copied, not shared: the byte compiler destructively attaches the
  ;; call position to a macro's returned form, which would corrupt a
  ;; literal for every later expansion.
  (copy-sequence
   '(plist-get (niwa--instruction-state
                (niwa--instruction-operation-state-key))
               :id-counter)))

(defmacro niwa--instruction-id-usage-map ()
  "Return the active workspace's used-ID table as a settable place."
  ;; Copied, not shared: the byte compiler destructively attaches the
  ;; call position to a macro's returned form, which would corrupt a
  ;; literal for every later expansion.
  (copy-sequence
   '(plist-get (niwa--instruction-state
                (niwa--instruction-operation-state-key))
               :id-usage-map)))

(defmacro niwa--instruction-retired-ids ()
  "Return the active workspace's retired IDs as a settable place."
  ;; Copied, not shared: the byte compiler destructively attaches the
  ;; call position to a macro's returned form, which would corrupt a
  ;; literal for every later expansion.
  (copy-sequence
   '(plist-get (niwa--instruction-state
                (niwa--instruction-operation-state-key))
               :retired-ids)))

(gv-define-setter niwa--instruction-state (value &optional key)
  `(puthash (or ,key :global) ,value niwa--instruction-states))

(defun niwa--instruction-alist-value ()
  "Return the active workspace's instruction alist."
  (niwa--instruction-alist))

(defun niwa--set-instruction-alist-value (value)
  "Set the active workspace's instruction alist to VALUE."
  (setf (niwa--instruction-alist) value))

(defun niwa--instruction-workspace-key (&optional workspace)
  "Return the instruction-state key for WORKSPACE."
  (if workspace
      (cons (niwa-workspace-type workspace)
            (niwa-workspace-id workspace))
    :global))

(defun niwa--instruction-buffer-workspace (buffer)
  "Return BUFFER's workspace, or nil when it cannot be resolved."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (boundp 'niwa--workspace)
               (bound-and-true-p niwa--workspace))
          (ignore-errors (niwa-workspace buffer))))))

(defun niwa--instruction-state (&optional key)
  "Return instruction state plist for KEY, creating it if needed."
  (let ((key (or key :global)))
    (or (gethash key niwa--instruction-states)
        (puthash key
                 (list :instructions nil
                       :id-counter 0
                       :id-usage-map (make-hash-table)
                       :retired-ids nil)
                 niwa--instruction-states))))

(defun niwa--instruction-activate-workspace (&optional workspace)
  "Make WORKSPACE's instruction state current."
  (let ((key (niwa--instruction-workspace-key workspace)))
    (niwa--instruction-state key)
    (setq niwa--instruction-current-state-key key))
  niwa--instruction-current-state-key)

(defun niwa--instruction-activate-buffer (&optional buffer)
  "Make BUFFER's workspace instruction state current."
  (niwa--instruction-activate-workspace
   (niwa--instruction-buffer-workspace (or buffer (current-buffer)))))

(defun niwa--instruction-state-rollback (workspace)
  "Return a function that restores WORKSPACE's exact instruction state."
  (let* ((key (niwa--instruction-workspace-key workspace))
         (state (copy-tree (niwa--instruction-state key)))
         (current-key niwa--instruction-current-state-key)
         (highlighted niwa--highlighted-instruction)
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
      (niwa--clear-instruction-state workspace)
      (puthash key state niwa--instruction-states)
      (setq niwa--instruction-current-state-key current-key
            niwa--highlighted-instruction highlighted)
      (dolist (snapshot overlays)
        (when (buffer-live-p (nth 1 snapshot))
          (move-overlay (nth 0 snapshot)
                        (nth 2 snapshot) (nth 3 snapshot)
                        (nth 1 snapshot)))))))

(defun niwa--delete-instruction-overlay (instruction)
  "Remove INSTRUCTION and its auxiliary presentation overlays."
  (let* ((label (overlay-get instruction 'niwa-label-overlay))
         (buffer (and label (overlay-buffer label)))
         (end (and buffer (overlay-end label)))
         (start (if (and buffer (eq 'line (overlay-get instruction 'niwa-span)))
                    (overlay-start label) end)))
    (dolist (property '(niwa-label-overlay niwa-note-tint-overlay))
      (when-let* ((overlay (overlay-get instruction property)))
        (delete-overlay overlay)
        (overlay-put instruction property nil)))
    (delete-overlay instruction)
    (when buffer
      (with-current-buffer buffer
        (niwa-notes--refresh-label-stacks-in start end)))))

(defun niwa--clear-instruction-state (&optional workspace)
  "Delete all visible instruction overlays in WORKSPACE and clear its state."
  (let ((niwa--instruction-state-key-override
         (niwa--instruction-workspace-key workspace)))
    (dolist (entry (niwa--instruction-alist))
      (when (bufferp (car entry))
        (dolist (instr (cdr entry))
          (when (overlayp instr)
            (niwa--delete-instruction-overlay instr)))))
    (setf (niwa--instruction-alist) nil)
    (setq niwa--highlighted-instruction nil)
    (setf (niwa--instruction-id-counter) 0)
    (setf (niwa--instruction-id-usage-map) (make-hash-table))
    (setf (niwa--instruction-retired-ids) nil)
    (setf (plist-get (niwa--instruction-state
                      (niwa--instruction-operation-state-key))
                     :closed-notes)
          nil)
    ;; Cleared state no longer matches the last observed instruction store,
    ;; so the next store write merges instead of overwriting it.
    (setf (plist-get (niwa--instruction-state
                      (niwa--instruction-operation-state-key))
                     :store-observation)
          nil)))

(defmacro niwa--foreach-instruction (binding &rest body)
  "Iterate over `(niwa--instruction-alist)' with BINDING as the binding.

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
                          (not (overlay-get instr 'niwa-marked-for-deletion))))
                   (clean-alist-entry (cons)
                     ;; `seq-remove' and `seq-filter', not their `cl-lib'
                     ;; equivalents: these calls survive macro expansion
                     ;; into every caller, and `cl-remove-if' is a
                     ;; function `cl-lib' does not autoload -- a compiled
                     ;; caller loaded without `cl-lib' hit a
                     ;; void-function at runtime.
                     (mapc (lambda (instr)
                             (niwa--delete-instruction instr (car cons)))
                           (seq-filter #'trashp (cdr cons)))
                     (let ((instrs (seq-remove #'trashp (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (niwa--instruction-activate-workspace
            (if (bufferp ,specific-buffer)
                (niwa--instruction-buffer-workspace ,specific-buffer)
              (niwa--instruction-buffer-workspace (current-buffer))))
           (if (null ,specific-buffer)
               (cl-loop for ,cons in (niwa--instruction-alist)
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (progn
                                   (niwa--restore-file-instructions ,bof))
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when (stringp ,specific-buffer)
               (cl-destructuring-bind (buffer _ _) (niwa--restore-file-instructions ,specific-buffer)
                 (setq ,specific-buffer buffer)))
             (when-let* ((cons (assoc ,specific-buffer (niwa--instruction-alist))))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setf (niwa--instruction-alist)
                 (seq-remove (lambda (cons) (null (cdr cons)))
                             (niwa--instruction-alist)))
           ;; The instructions alist should now be cleaned of deleted
           ;; instructions.
           (cl-loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer (niwa--instruction-alist))
                         (flatten-tree
                          (mapcar (lambda (plist-or-instrs)
                                    (if (plist-get plist-or-instrs :instructions)
                                        nil ; Plist
                                      plist-or-instrs))
                                  (mapcar #'cdr (niwa--instruction-alist)))))
                    ,@body))))))

(defun niwa-link-instructions (from-list to-list)
  "Link instructions with ids in FROM-LIST to those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (niwa--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (niwa--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link to: "
                                             completion-table nil t))))))
  (niwa--instruction-activate-buffer)
  (cl-labels
      ((update-links (instr-id num-key update-id)
         (let* ((instr (niwa--instruction-with-id instr-id))
                (links (overlay-get instr 'niwa-links))
                (ids (plist-get links num-key)))
           (unless (member update-id ids)
             (setq ids (cons update-id ids))
             (overlay-put instr 'niwa-links (plist-put links num-key ids))
             t))))
    (let ((new-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (niwa--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when (/= from-id to-id)
              (when-let* ((to-instr (niwa--instruction-with-id to-id)))
                (when (and (update-links from-id :to to-id)
                           (update-links to-id :from from-id))
                  (puthash from-instr t involved-instrs)
                  (puthash to-instr t involved-instrs)
                  (cl-incf new-link-count)))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (niwa--update-instruction-overlay instr))
      (when (called-interactively-p 'interactive)
        (message "Created %d instruction link%s"
                 new-link-count
                 (if (= new-link-count 1) "" "s"))))))

(defun niwa-unlink-instructions (from-list to-list)
  "Unlink instructions with ids in FROM-LIST from those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (niwa--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (niwa--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink from: "
                                             completion-table nil t))))))
  (niwa--instruction-activate-buffer)
  (cl-labels
      ((remove-links (instr-id num-key remove-id)
         (let* ((instr (niwa--instruction-with-id instr-id))
                (links (overlay-get instr 'niwa-links))
                (ids (plist-get links num-key)))
           (when (member remove-id ids)
             (setq ids (remove remove-id ids))
             (overlay-put instr 'niwa-links (plist-put links num-key ids))
             t))))
    (let ((removed-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (niwa--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when-let* ((to-instr (niwa--instruction-with-id to-id)))
              (when (and (remove-links from-id :to to-id)
                         (remove-links to-id :from from-id))
                (puthash from-instr t involved-instrs)
                (puthash to-instr t involved-instrs)
                (cl-incf removed-link-count))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (when (buffer-live-p (overlay-buffer instr))
                    (niwa--update-instruction-overlay instr)))
      (when (called-interactively-p 'interactive)
        (message "Removed %d instruction link%s"
                 removed-link-count
                 (if (= removed-link-count 1) "" "s"))))))

(defun niwa--instruction-find-unique-live (predicate)
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
     niwa--instruction-states)
    (and (not ambiguous) found)))

(let ((map (make-hash-table))
      (map-key nil))
  (cl-defun niwa--instruction-with-id (target-id &optional workspace)
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
                  (cl-some #'entry-live-p (niwa--instruction-alist))))
      (let* ((lookup-workspace
              (or workspace
                  (niwa--instruction-buffer-workspace (current-buffer))))
             (lookup-key (niwa--instruction-workspace-key lookup-workspace))
             (niwa--instruction-state-key-override lookup-key))
        (unless (equal map-key lookup-key)
          (setq map (make-hash-table)
                map-key lookup-key))
        (when-let* ((instr (gethash target-id map)))
          (when (and (buffer-live-p (overlay-buffer instr))
                     (= target-id (niwa--instruction-id instr)))
            (cl-return-from niwa--instruction-with-id instr)))
        (setq map (make-hash-table))
        (niwa--foreach-instruction instr
          do (puthash (niwa--instruction-id instr) instr map))
        (or (gethash target-id map)
            (and (null workspace)
                 (not (current-state-live-p))
                 (niwa--instruction-find-unique-live
                  (lambda (instruction)
                    (= target-id
                       (niwa--instruction-id instruction))))))))))

(defun niwa--instruction-with-uuid (uuid &optional workspace)
  "Return the instruction carrying UUID in WORKSPACE.
Stashed instructions are restored before lookup.  When WORKSPACE is nil,
fall back to a unique live match across workspaces for prompt-copy callers
that have no workspace context."
  (or
   (let ((niwa--instruction-state-key-override
          (and workspace (niwa--instruction-workspace-key workspace))))
     (niwa--foreach-instruction instr
       when (equal uuid (overlay-get instr 'niwa-uuid))
       return instr))
   (unless workspace
     (niwa--instruction-find-unique-live
      (lambda (instruction)
        (equal uuid (overlay-get instruction 'niwa-uuid)))))))

(defun niwa--instruction-id (instruction)
  "Return unique identifier for INSTRUCTION overlay."
  (overlay-get instruction 'niwa-id))

(defun niwa--create-id ()
  "Create a unique identifier for an instruction.

Retrieves an unused ID from retired IDs or generates a new one by
incrementing the ID counter.  Tracks ID usage via a hash table."
  (let ((id
         (if (niwa--instruction-retired-ids)
             (prog1
                 (car (niwa--instruction-retired-ids))
               (setf (niwa--instruction-retired-ids) (cdr (niwa--instruction-retired-ids))))
           (cl-incf (niwa--instruction-id-counter)))))
    (puthash id t (niwa--instruction-id-usage-map))
    id))

(defun niwa--retire-id (id)
  "Retire an ID by removing it from `(niwa--instruction-id-usage-map)'.
The id is added to `(niwa--instruction-retired-ids)'"
  (when (gethash id (niwa--instruction-id-usage-map))
    (remhash id (niwa--instruction-id-usage-map))
    (push id (niwa--instruction-retired-ids))))

(defun niwa--instruction-outlinks (instruction)
  "Return the :to links of INSTRUCTION."
  (plist-get (overlay-get instruction 'niwa-links) :to))

(defun niwa--instruction-inlinks (instruction)
  "Return the :from links of INSTRUCTION."
  (plist-get (overlay-get instruction 'niwa-links) :from))


(provide 'niwa-instruction-registry)
;;; niwa-instruction-registry.el ends here
