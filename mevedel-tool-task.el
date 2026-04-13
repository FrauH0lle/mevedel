;;; mevedel-tool-task.el -- Session task tracking tools -*- lexical-binding: t -*-

;;; Commentary:

;; Task CRUD tools (TaskCreate, TaskUpdate, TaskList, TaskGet) and the
;; session task overlay.  Tasks live on `mevedel-session' and replace
;; the legacy TodoWrite/TodoRead flat checklist: they carry IDs, status,
;; optional owner and dependency information, so simple sessions can
;; use them as a plain checklist while coordinator sessions get the
;; full dependency graph.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'mevedel-structs)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--fsm-last)


;;
;;; Status helpers

(defconst mevedel-tool-task--statuses '(pending in-progress completed)
  "Valid task status symbols.")

(defun mevedel-tool-task--parse-status (value)
  "Parse VALUE into a task status symbol.
Accepts symbols, strings (\"pending\", \"in_progress\",
\"in-progress\", \"completed\"), or nil (defaults to `pending').
Signals an error on unknown values."
  (pcase value
    ((or 'nil "" :json-false) 'pending)
    ((pred symbolp)
     (if (memq value mevedel-tool-task--statuses)
         value
       (error "Invalid task status: %S" value)))
    ((pred stringp)
     (pcase (downcase value)
       ("pending" 'pending)
       ((or "in_progress" "in-progress") 'in-progress)
       ("completed" 'completed)
       (_ (error "Invalid task status: %S" value))))
    (_ (error "Invalid task status: %S" value))))

(defun mevedel-tool-task--status-string (status)
  "Render STATUS symbol as the string shown to the LLM."
  (pcase status
    ('pending "pending")
    ('in-progress "in_progress")
    ('completed "completed")
    (_ (format "%s" status))))


;;
;;; Session access

(defun mevedel-tool-task--session ()
  "Return the active session, or signal an error."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (error "No active mevedel session in %s" (buffer-name))))

(defun mevedel-tool-task--next-id (session)
  "Return the next unused task ID for SESSION."
  (1+ (apply #'max 0 (mapcar #'mevedel-task-id
                             (mevedel-session-tasks session)))))

(defun mevedel-tool-task--find (session id)
  "Return the task in SESSION with ID, or nil."
  (cl-find id (mevedel-session-tasks session)
           :key #'mevedel-task-id))

(defun mevedel-tool-task--normalize-id-list (value)
  "Normalize VALUE to a list of integer task IDs.
Accepts nil, a vector, or a list.  Signals an error on non-integers."
  (let ((lst (cond
              ((null value) nil)
              ((vectorp value) (append value nil))
              ((listp value) value)
              (t (error "Expected an array of task IDs, got %S" value)))))
    (dolist (id lst)
      (unless (integerp id)
        (error "Task ID must be an integer, got %S" id)))
    lst))

(defun mevedel-tool-task--plist-get-any (plist &rest keys)
  "Return the first non-nil value in PLIST matching any of KEYS."
  (cl-loop for key in keys
           for v = (plist-get plist key)
           when v return v))

(defun mevedel-tool-task--object-to-plist (obj)
  "Convert OBJ (alist or plist) to a plist keyed by symbol keywords.
The JSON object from gptel may arrive as either form; normalize once."
  (cond
   ((null obj) nil)
   ;; Already a plist: first element is a keyword.
   ((and (listp obj) (keywordp (car obj))) obj)
   ;; Alist of (KEY . VALUE) pairs.
   ((and (listp obj) (consp (car obj)))
    (let (plist)
      (dolist (pair obj)
        (let ((k (car pair)))
          (push (cond
                 ((keywordp k) k)
                 ((symbolp k) (intern (format ":%s" k)))
                 ((stringp k) (intern (format ":%s" k)))
                 (t (error "Invalid task key: %S" k)))
                plist)
          (push (cdr pair) plist)))
      (nreverse plist)))
   (t (error "Expected task object, got %S" obj))))


;;
;;; Task mutations

(defun mevedel-tool-task--create-one (session spec)
  "Create a single task in SESSION from SPEC plist.
Returns the new `mevedel-task' struct."
  (let* ((p (mevedel-tool-task--object-to-plist spec))
         (subject (mevedel-tool-task--plist-get-any p :subject))
         (description (mevedel-tool-task--plist-get-any p :description))
         (status-raw (mevedel-tool-task--plist-get-any p :status))
         (owner (mevedel-tool-task--plist-get-any p :owner))
         (blocks-raw (mevedel-tool-task--plist-get-any p :blocks))
         (blocked-by-raw (mevedel-tool-task--plist-get-any
                          p :blockedBy :blocked_by :blocked-by))
         (metadata (mevedel-tool-task--plist-get-any p :metadata)))
    (unless (and (stringp subject) (not (string-empty-p subject)))
      (error "Task subject is required and must be a non-empty string"))
    (let ((task (mevedel-task--create
                 :id (mevedel-tool-task--next-id session)
                 :subject subject
                 :description (and (stringp description) description)
                 :status (mevedel-tool-task--parse-status status-raw)
                 :owner (and (stringp owner) owner)
                 :blocks (mevedel-tool-task--normalize-id-list blocks-raw)
                 :blocked-by (mevedel-tool-task--normalize-id-list blocked-by-raw)
                 :metadata metadata)))
      (setf (mevedel-session-tasks session)
            (append (mevedel-session-tasks session) (list task)))
      task)))

(defun mevedel-tool-task--update-one (session id updates)
  "Apply UPDATES plist to the task with ID in SESSION.
Returns the updated task.  Signals an error if ID is unknown."
  (let ((task (mevedel-tool-task--find session id))
        (p (mevedel-tool-task--object-to-plist updates)))
    (unless task
      (error "No task with id %s" id))
    (when-let* ((subject (mevedel-tool-task--plist-get-any p :subject)))
      (unless (stringp subject)
        (error "Task subject must be a string"))
      (setf (mevedel-task-subject task) subject))
    (when (plist-member p :description)
      (let ((d (plist-get p :description)))
        (setf (mevedel-task-description task)
              (and (stringp d) d))))
    (let ((status-raw (mevedel-tool-task--plist-get-any p :status)))
      (when (and status-raw
                 (not (and (stringp status-raw)
                           (string-empty-p status-raw))))
        (let ((new-status (mevedel-tool-task--parse-status status-raw))
              (old-status (mevedel-task-status task)))
          (setf (mevedel-task-status task) new-status)
          (when (and (eq new-status 'completed)
                     (not (eq old-status 'completed)))
            (mevedel-tool-task--propagate-completion session task)))))
    (when (plist-member p :owner)
      (let ((o (plist-get p :owner)))
        (setf (mevedel-task-owner task)
              (and (stringp o) o))))
    (when (plist-member p :blocks)
      (setf (mevedel-task-blocks task)
            (mevedel-tool-task--normalize-id-list (plist-get p :blocks))))
    (when (or (plist-member p :blockedBy)
              (plist-member p :blocked_by)
              (plist-member p :blocked-by))
      (setf (mevedel-task-blocked-by task)
            (mevedel-tool-task--normalize-id-list
             (mevedel-tool-task--plist-get-any
              p :blockedBy :blocked_by :blocked-by))))
    (when (plist-member p :metadata)
      (setf (mevedel-task-metadata task) (plist-get p :metadata)))
    task))

(defun mevedel-tool-task--propagate-completion (session task)
  "Remove TASK's ID from the `blocked-by' slot of every task it blocks.
Also clears TASK's `blocks' slot once propagated, since downstream
dependencies no longer point back to it."
  (let ((id (mevedel-task-id task)))
    (dolist (other (mevedel-session-tasks session))
      (when (memq id (mevedel-task-blocked-by other))
        (setf (mevedel-task-blocked-by other)
              (delq id (mevedel-task-blocked-by other)))))))


;;
;;; Formatting helpers

(defun mevedel-tool-task--format-one (task)
  "Format TASK as a single display line (propertized)."
  (let* ((status (mevedel-task-status task))
         (id (mevedel-task-id task))
         (subject (mevedel-task-subject task))
         (owner (mevedel-task-owner task))
         (blocked-by (mevedel-task-blocked-by task))
         (icon (pcase status
                 ('completed   "✓")
                 ('in-progress "→")
                 (_            "○")))
         (face (pcase status
                 ('completed   '(:inherit success :strike-through t))
                 ('in-progress '(:inherit bold :inherit warning))
                 (_            'default)))
         (suffix (concat
                  (when owner
                    (propertize (format " @%s" owner)
                                'face 'font-lock-comment-face))
                  (when blocked-by
                    (propertize (format " (blocked by %s)"
                                        (mapconcat #'number-to-string
                                                   blocked-by ", "))
                                'face 'font-lock-comment-face)))))
    (concat icon " "
            (propertize (format "#%d " id) 'face 'font-lock-comment-face)
            (propertize subject 'face face)
            suffix)))

(defun mevedel-tool-task--format-for-llm (tasks)
  "Return a plain-text summary of TASKS for the LLM."
  (if (null tasks)
      "No tasks recorded."
    (mapconcat
     (lambda (task)
       (let ((parts (list (format "#%d [%s] %s"
                                  (mevedel-task-id task)
                                  (mevedel-tool-task--status-string
                                   (mevedel-task-status task))
                                  (mevedel-task-subject task)))))
         (when-let* ((owner (mevedel-task-owner task)))
           (push (format "owner=%s" owner) parts))
         (when-let* ((bb (mevedel-task-blocked-by task)))
           (push (format "blockedBy=[%s]"
                         (mapconcat #'number-to-string bb ","))
                 parts))
         (when-let* ((bl (mevedel-task-blocks task)))
           (push (format "blocks=[%s]"
                         (mapconcat #'number-to-string bl ","))
                 parts))
         (string-join (nreverse parts) " ")))
     tasks "\n")))


;;
;;; Overlay display

(defconst mevedel-tool-task--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
  "Horizontal rule used around the task overlay.")

(defun mevedel-toggle-tasks ()
  "Toggle the display of the session task list overlay."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'mevedel-tool-task)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'mevedel-tool-task nil (point-min))
                    'mevedel-tool-task))))
    (cond
     ((null ov) (message "No task list overlay here"))
     ((overlay-get ov 'after-string)
      (overlay-put ov 'mevedel-tool-task--stashed
                   (overlay-get ov 'after-string))
      (overlay-put ov 'after-string nil))
     (t
      (overlay-put ov 'after-string
                   (or (overlay-get ov 'mevedel-tool-task--stashed)
                       (and (stringp prop-value) prop-value)))))))

(defun mevedel-tool-task--display-overlay ()
  "Display the current session's task list as an overlay.
Positions the overlay over the last assistant response, mirroring
the old todo overlay placement."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (info (and (boundp 'gptel--fsm-last)
                    gptel--fsm-last
                    (gptel-fsm-info gptel--fsm-last)))
         (marker (and info (plist-get info :tracking-marker))))
    (when (and session info marker)
      (let* ((where-to marker)
             (where-from (previous-single-property-change
                          where-to 'gptel nil (point-min))))
        (unless (= where-from where-to)
          (let ((ov (mevedel-session-task-overlay session)))
            (unless (and (overlayp ov) (overlay-buffer ov))
              (setq ov (make-overlay where-from where-to nil t))
              (overlay-put ov 'mevedel-tool-task t)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority -40)
              (overlay-put ov 'keymap
                           (define-keymap
                             "<tab>" #'mevedel-toggle-tasks
                             "TAB"   #'mevedel-toggle-tasks))
              (setf (mevedel-session-task-overlay session) ov))
            (move-overlay ov where-from where-to)
            (let* ((tasks (mevedel-session-tasks session))
                   (body (if tasks
                             (mapconcat #'mevedel-tool-task--format-one
                                        tasks "\n")
                           (propertize "No tasks."
                                       'face 'font-lock-comment-face)))
                   (display
                    (concat
                     (unless (= (char-before (overlay-end ov)) ?\n) "\n")
                     mevedel-tool-task--hrule
                     (propertize "Tasks: [ "
                                 'face '(:inherit font-lock-comment-face
                                                  :inherit bold))
                     (save-excursion
                       (goto-char (1- (overlay-end ov)))
                       (propertize
                        (substitute-command-keys "\\[mevedel-toggle-tasks]")
                        'face 'help-key-binding))
                     (propertize " to toggle display ]\n"
                                 'face 'font-lock-comment-face)
                     body "\n"
                     mevedel-tool-task--hrule)))
              (overlay-put ov 'after-string display)
              (overlay-put ov 'mevedel-tool-task--stashed nil))))))))


;;
;;; Tool handlers

(defun mevedel-tool-task--tasks-arg (args)
  "Return the `:tasks' argument from ARGS as a list of task specs.
Accepts a vector, list, or single task object; always returns a list."
  (let ((raw (plist-get args :tasks)))
    (cond
     ((null raw) (error "Parameter tasks is required"))
     ((vectorp raw) (append raw nil))
     ;; Single task object passed as a plist/alist -- wrap in list.
     ((and (listp raw) (or (keywordp (car raw))
                           (and (consp (car raw)) (atom (caar raw)))))
      (list raw))
     ((listp raw) raw)
     (t (error "Parameter tasks must be an array, got %S" raw)))))

(defun mevedel-tool-task--handle-create (args)
  "Handler for TaskCreate.  ARGS has :tasks."
  (let* ((session (mevedel-tool-task--session))
         (specs (mevedel-tool-task--tasks-arg args))
         (created nil))
    (dolist (spec specs)
      (push (mevedel-tool-task--create-one session spec) created))
    (setq created (nreverse created))
    (mevedel-tool-task--display-overlay)
    (format "Created %d task%s:\n%s"
            (length created)
            (if (= 1 (length created)) "" "s")
            (mevedel-tool-task--format-for-llm created))))

(defun mevedel-tool-task--handle-update (args)
  "Handler for TaskUpdate.  ARGS has :id and zero or more update fields."
  (let* ((session (mevedel-tool-task--session))
         (id (plist-get args :id))
         ;; Build an updates plist from the remaining keys.
         (updates (cl-loop for (k v) on args by #'cddr
                           unless (eq k :id)
                           nconc (list k v))))
    (unless (integerp id)
      (error "Parameter id is required and must be an integer"))
    (let ((task (mevedel-tool-task--update-one session id updates)))
      (mevedel-tool-task--display-overlay)
      (format "Updated task:\n%s"
              (mevedel-tool-task--format-for-llm (list task))))))

(defun mevedel-tool-task--handle-list (args)
  "Handler for TaskList.  ARGS may include :status filter."
  (let* ((session (mevedel-tool-task--session))
         (filter-raw (plist-get args :status))
         (filter (and filter-raw
                      (mevedel-tool-task--parse-status filter-raw)))
         (tasks (mevedel-session-tasks session))
         (filtered (if filter
                       (cl-remove-if-not
                        (lambda (t1) (eq (mevedel-task-status t1) filter))
                        tasks)
                     tasks)))
    (mevedel-tool-task--display-overlay)
    (if filter
        (format "Tasks with status %s:\n%s"
                (mevedel-tool-task--status-string filter)
                (mevedel-tool-task--format-for-llm filtered))
      (mevedel-tool-task--format-for-llm filtered))))

(defun mevedel-tool-task--handle-get (args)
  "Handler for TaskGet.  ARGS has :id."
  (let* ((session (mevedel-tool-task--session))
         (id (plist-get args :id)))
    (unless (integerp id)
      (error "Parameter id is required and must be an integer"))
    (let ((task (mevedel-tool-task--find session id)))
      (unless task
        (error "No task with id %s" id))
      (with-temp-buffer
        (insert (format "#%d [%s] %s\n"
                        (mevedel-task-id task)
                        (mevedel-tool-task--status-string
                         (mevedel-task-status task))
                        (mevedel-task-subject task)))
        (when-let* ((d (mevedel-task-description task)))
          (insert (format "Description: %s\n" d)))
        (when-let* ((o (mevedel-task-owner task)))
          (insert (format "Owner: %s\n" o)))
        (when-let* ((bb (mevedel-task-blocked-by task)))
          (insert (format "Blocked by: %s\n"
                          (mapconcat #'number-to-string bb ", "))))
        (when-let* ((bl (mevedel-task-blocks task)))
          (insert (format "Blocks: %s\n"
                          (mapconcat #'number-to-string bl ", "))))
        (when-let* ((m (mevedel-task-metadata task)))
          (insert (format "Metadata: %S\n" m)))
        (buffer-string)))))


;;
;;; Registration

(defun mevedel-tool-task--register ()
  "Register the task tracking tools."

  (mevedel-define-tool
    :name "TaskCreate"
    :description "Create one or more tasks in the session task list."
    :prompt-file "tools/taskcreate.md"
    :handler #'mevedel-tool-task--handle-create
    :args ((tasks array :required
                  "Array of task objects. Each object has: subject (string, required), description (string, optional), status (\"pending\"|\"in_progress\"|\"completed\", optional), owner (string, optional), blockedBy (array of task IDs, optional), blocks (array of task IDs, optional), metadata (object, optional)."))
    :read-only-p t
    :groups (util))

  (mevedel-define-tool
    :name "TaskUpdate"
    :description "Update the status or fields of an existing task."
    :prompt-file "tools/taskupdate.md"
    :handler #'mevedel-tool-task--handle-update
    :args ((id integer :required
               "The integer ID of the task to update.")
           (subject string :optional
                    "New subject line for the task.")
           (description string :optional
                         "New description. Pass an empty string to clear.")
           (status string :optional
                   "New status: \"pending\", \"in_progress\", or \"completed\". When set to completed, the task is removed from the blocked-by lists of any dependent tasks.")
           (owner string :optional
                  "New owner (agent name). Pass an empty string to unassign.")
           (blocks array :optional
                   "Array of task IDs this task blocks.")
           (blockedBy array :optional
                      "Array of task IDs blocking this task.")
           (metadata object :optional
                     "Free-form metadata object."))
    :read-only-p t
    :groups (util))

  (mevedel-define-tool
    :name "TaskList"
    :description "List the tasks currently tracked in the session."
    :prompt-file "tools/tasklist.md"
    :handler #'mevedel-tool-task--handle-list
    :args ((status string :optional
                   "Optional filter: \"pending\", \"in_progress\", or \"completed\"."))
    :read-only-p t
    :groups (util))

  (mevedel-define-tool
    :name "TaskGet"
    :description "Retrieve full details for a single task by ID."
    :prompt-file "tools/taskget.md"
    :handler #'mevedel-tool-task--handle-get
    :args ((id integer :required
               "The integer ID of the task to retrieve."))
    :read-only-p t
    :groups (util)))

(provide 'mevedel-tool-task)
;;; mevedel-tool-task.el ends here
