;;; mevedel-tool-task.el -- Session task tracking tools -*- lexical-binding: t -*-

;;; Commentary:

;; Task CRUD tools (TaskCreate, TaskUpdate, TaskList, TaskGet), TaskNote,
;; and the session task status fragment.  Tasks live on `mevedel-session'
;; with IDs, status, optional owner, and dependency information.

;;; Code:

;; `mevedel-utilities'
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(autoload 'mevedel--truncate-display "mevedel-utilities")

(require 'cl-lib)
(require 'seq)

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'subr-x)
(require 'mevedel-structs)
(require 'mevedel-turn)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--fsm-last)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-turn'
(declare-function mevedel-current-turn "mevedel-turn" (session))

;; `mevedel-view'
(defvar mevedel-view--status-marker)
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--zone-separator "mevedel-view" (label))

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state
                  "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-set-collapse-state
                  "mevedel-view-zone" (key collapsed))


;;
;;; Status helpers

(defface mevedel-tool-task-in-progress
  '((t :inherit success :weight bold))
  "Face for the task currently in progress."
  :group 'mevedel)

(defface mevedel-tool-task-completed
  '((t :inherit shadow))
  "Face for completed task rows.
The check glyph and the `done' section already mark these rows, so the
face stays quiet rather than striking a whole section through."
  :group 'mevedel)

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

(defun mevedel-tool-task--current-agent-owner (&optional session)
  "Return the current agent's canonical task owner, or nil at root."
  (and (boundp 'mevedel--agent-invocation)
       (mevedel-agent-invocation-p mevedel--agent-invocation)
       (mevedel-task-normalize-owner
        (mevedel-agent-invocation-require-path mevedel--agent-invocation)
        (and session (mevedel-session-agent-registry session)))))

(defun mevedel-tool-task--argument-value-present-p (args key)
  "Return non-nil when ARGS has KEY with a non-nil value.
Optional tool arguments may arrive as explicit nil placeholders; treat
those the same as omitted arguments."
  (and (plist-member args key)
       (let ((value (plist-get args key)))
         (not (or (null value) (eq value :json-false))))))

(defun mevedel-tool-task--optional-argument (args key)
  "Return ARGS' KEY value, or nil when the caller meant to omit it.
An empty string and `:json-false' both read as absent.  This is the
opposite of `mevedel-tool-task--argument-value-present-p', whose callers
give a blank value a meaning of its own: an empty owner selects Main and
an empty note clears one.  Here a blank is a placeholder for an argument
the caller did not supply, and reading it as a value silently answers a
different question than the one asked."
  (let ((value (plist-get args key)))
    (unless (or (null value)
                (eq value :json-false)
                (and (stringp value) (string-empty-p value)))
      value)))

(defun mevedel-tool-task--owner-from-args (session args keys)
  "Return a normalized owner in SESSION from ARGS using KEYS.
When none of KEYS has a value, default to the current caller owner.
An explicitly empty string targets the main session owner."
  (catch 'found
    (dolist (key keys)
      (when (mevedel-tool-task--argument-value-present-p args key)
        (throw 'found
               (mevedel-task-normalize-owner
                (plist-get args key)
                (mevedel-session-agent-registry session)))))
    (mevedel-tool-task--current-agent-owner session)))

(defun mevedel-tool-task--normalize-note (note)
  "Normalize NOTE to a compact non-empty string, or nil to clear it."
  (cond
   ((null note) nil)
   ((not (stringp note))
    (error "Parameter note must be a string"))
   (t
    (let ((trimmed (string-clean-whitespace note)))
      (unless (string-empty-p trimmed)
        trimmed)))))

(defun mevedel-tool-task--normalize-subject (value)
  "Return VALUE as the single non-blank line a task subject must be."
  (let ((subject (and (stringp value) (string-clean-whitespace value))))
    (when (or (null subject) (string-empty-p subject))
      (error "Task subject must be a non-empty string"))
    subject))

(defun mevedel-tool-task--status-note-entry (session owner)
  "Return SESSION's status note entry for OWNER."
  (cl-assoc owner (mevedel-session-task-status-notes session)
            :test #'equal))

(defun mevedel-tool-task--status-note (session owner)
  "Return SESSION's status note text for OWNER, or nil."
  (when-let* ((entry (mevedel-tool-task--status-note-entry session owner)))
    (plist-get (cdr entry) :note)))

(defun mevedel-tool-task--set-status-note (session owner note)
  "Set SESSION's status NOTE for OWNER.
OWNER is nil for the main session.  A nil NOTE clears the entry."
  (let* ((owner
          (mevedel-task-normalize-owner
           owner (mevedel-session-agent-registry session)))
         (note (mevedel-tool-task--normalize-note note))
         (notes (mevedel-session-task-status-notes session))
         (entry (mevedel-tool-task--status-note-entry session owner)))
    (if note
        (let ((data (list :note note
                          :updated-turn
                          (mevedel-tool-task--write-turn session)
                          :updated-at
                          (format-time-string "%FT%T%z"))))
          (if entry
              (setcdr entry data)
            (push (cons owner data) notes))
          (setf (mevedel-session-task-status-notes session) notes)
          note)
      (setf (mevedel-session-task-status-notes session)
            (cl-remove owner notes :key #'car :test #'equal))
      nil)))

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

(defun mevedel-tool-task--write-turn (session)
  "Return the task-write turn for SESSION."
  (mevedel-current-turn session))

(defun mevedel-tool-task--mark-write (session)
  "Record a successful task write on SESSION.
Also drops dependency edges the write resolved.  Every task mutation
lands here, which is why the invariant lives here: a task created
blocked by an already-completed ID, or updated into one, would otherwise
keep that edge forever -- nothing clears it, because unblocking only
ever fired on a task's transition to completed."
  (mevedel-task-prune-resolved-dependencies (mevedel-session-tasks session))
  (setf (mevedel-session-last-task-write-turn session)
        (mevedel-tool-task--write-turn session)))

(defun mevedel-tool-task--create-one (session spec &optional id)
  "Return a task for SESSION from SPEC plist, using ID when non-nil.
Returns the new `mevedel-task' struct."
  (let* ((p (mevedel-tool-task--object-to-plist spec))
         (subject (mevedel-tool-task--plist-get-any p :subject))
         (description (mevedel-tool-task--plist-get-any p :description))
         (status-raw (mevedel-tool-task--plist-get-any p :status))
         (owner (if (plist-member p :owner)
                    (plist-get p :owner)
                  (mevedel-tool-task--current-agent-owner session)))
         (blocked-by-raw (mevedel-tool-task--plist-get-any
                          p :blockedBy :blocked_by :blocked-by))
         (metadata (mevedel-tool-task--plist-get-any p :metadata)))
    (setq subject (mevedel-tool-task--normalize-subject subject))
    (let* ((status (mevedel-tool-task--parse-status status-raw))
           (task (mevedel-task--create
                  :id (or id (mevedel-tool-task--next-id session))
                  :subject subject
                  :description (and (stringp description) description)
                  :status status
                  :owner
                  (mevedel-task-normalize-owner
                   owner (mevedel-session-agent-registry session))
                  :blocked-by (mevedel-tool-task--normalize-id-list
                               blocked-by-raw)
                  :completed-turn (and (eq status 'completed)
                                       (mevedel-tool-task--write-turn
                                        session))
                  :metadata metadata)))
      task)))

(defun mevedel-tool-task--update-one (session id updates)
  "Return task ID in SESSION after applying replacement fields.
The argument UPDATES is the plist of replacement fields.
Returns the updated task.  Signals an error if ID is unknown."
  (let ((task (mevedel-tool-task--find session id))
        (p (mevedel-tool-task--object-to-plist updates)))
    (unless task
      (error "No task with id %s" id))
    (let (subject subject-p
          description description-p
          new-status status-p
          owner owner-p
          blocked-by blocked-by-p
          metadata metadata-p)
      (when-let* ((subject-value
                   (mevedel-tool-task--plist-get-any p :subject)))
        (setq subject (mevedel-tool-task--normalize-subject subject-value)
              subject-p t))
      (when (plist-member p :description)
        (let ((d (plist-get p :description)))
          (setq description (and (stringp d) d)
                description-p t)))
      (when-let* ((status-raw (mevedel-tool-task--optional-argument
                               p :status)))
        (setq new-status (mevedel-tool-task--parse-status status-raw)
              status-p t))
      (when (plist-member p :owner)
        (setq owner (mevedel-task-normalize-owner
                     (plist-get p :owner)
                     (mevedel-session-agent-registry session))
              owner-p t))
      (when (or (plist-member p :blockedBy)
                (plist-member p :blocked_by)
                (plist-member p :blocked-by))
        (setq blocked-by
              (mevedel-tool-task--normalize-id-list
               (mevedel-tool-task--plist-get-any
                p :blockedBy :blocked_by :blocked-by))
              blocked-by-p t))
      (when (plist-member p :metadata)
        (setq metadata (plist-get p :metadata)
              metadata-p t))
      (when subject-p
        (setf (mevedel-task-subject task) subject))
      (when description-p
        (setf (mevedel-task-description task) description))
      (when status-p
        (let ((old-status (mevedel-task-status task)))
          (setf (mevedel-task-status task) new-status)
          (when (and (eq new-status 'completed)
                     (not (eq old-status 'completed)))
            (setf (mevedel-task-completed-turn task)
                  (mevedel-tool-task--write-turn session)))))
      (when owner-p
        (setf (mevedel-task-owner task) owner))
      (when blocked-by-p
        (setf (mevedel-task-blocked-by task) blocked-by))
      (when metadata-p
        (setf (mevedel-task-metadata task) metadata)))
    task))

(defun mevedel-tool-task-finalize-owner (session owner status)
  "Reconcile SESSION tasks owned by OWNER for terminal agent STATUS.
When STATUS is `completed' and OWNER is a canonical non-root path, mark all
open tasks for that owner completed and clear that owner's task status
note.  Dependents unblock through `mevedel-tool-task--mark-write'.
Return non-nil when SESSION changed."
  (when (and (eq status 'completed)
             (mevedel-agent-path-p owner)
             (not (equal owner "/root")))
    (let ((changed nil)
          (turn (mevedel-tool-task--write-turn session)))
      (dolist (task (mevedel-session-tasks session))
        (when (and (equal owner (mevedel-task-owner task))
                   (mevedel-tool-task--active-p task))
          (setf (mevedel-task-status task) 'completed)
          (setf (mevedel-task-completed-turn task) turn)
          (setq changed t)))
      (when (mevedel-tool-task--status-note-entry session owner)
        (mevedel-tool-task--set-status-note session owner nil)
        (setq changed t))
      (when changed
        (mevedel-tool-task--clear-inactive-status-notes session)
        (mevedel-tool-task--mark-write session))
      changed)))


;;
;;; Formatting helpers

(defun mevedel-tool-task--owner-label (owner)
  "Return the display label for OWNER."
  (if (and (stringp owner) (not (string-empty-p owner)))
      owner
    "Main"))

(defun mevedel-tool-task--owner-display-label (owner)
  "Return OWNER's label as the task panel renders it.
Canonical agent paths drop the `/root/' prefix every agent shares.
Bucket strings and the main session's label are unchanged; tool
feedback keeps the full label through `mevedel-tool-task--owner-label'."
  (string-remove-prefix "/root/" (mevedel-tool-task--owner-label owner)))

(defun mevedel-tool-task--metadata-get (metadata &rest keys)
  "Return the first value in METADATA matching one of KEYS."
  (catch 'found
    (dolist (key keys)
      (let* ((bare (and (keywordp key)
                        (substring (symbol-name key) 1)))
             (symbol-key (and bare (intern bare)))
             (string-hit (and bare (listp metadata)
                              (assoc bare metadata))))
        (when (and (listp metadata) (keywordp (car metadata))
                   (plist-member metadata key))
          (throw 'found (plist-get metadata key)))
        (when (and (listp metadata) (assq key metadata))
          (throw 'found (cdr (assq key metadata))))
        (when (and symbol-key (listp metadata)
                   (assq symbol-key metadata))
          (throw 'found (cdr (assq symbol-key metadata))))
        (when string-hit
          (throw 'found (cdr string-hit)))))
    nil))

(defun mevedel-tool-task--activity-summary (task)
  "Return TASK's display activity, or nil.
Only agent-owned tasks carry one: the main session's current work is
already visible in the transcript.  Truncation happens at the single
row-text site in `mevedel-tool-task--format-one'."
  (when (and (eq (mevedel-task-status task) 'in-progress)
             (mevedel-agent-path-p (mevedel-task-owner task)))
    (let ((activity (mevedel-tool-task--metadata-get
                     (mevedel-task-metadata task)
                     :activity :activeForm :active-form :active_form)))
      (when (and (stringp activity)
                 (not (string-empty-p activity)))
        (string-clean-whitespace activity)))))

(defun mevedel-tool-task--format-id-list (ids)
  "Return IDS as a compact task-reference list."
  (mapconcat (lambda (id) (format "#%d" id)) ids ", "))

(defun mevedel-tool-task--propertize-row-part (text face)
  "Return TEXT with FACE applied for overlay and font-lock rendering."
  (propertize text 'face face 'font-lock-face face))

(defun mevedel-tool-task--dim (text)
  "Return TEXT propertized as dim panel chrome."
  (mevedel-tool-task--propertize-row-part text 'font-lock-comment-face))

(defun mevedel-tool-task--active-p (task)
  "Return non-nil when TASK is not completed."
  (not (eq (mevedel-task-status task) 'completed)))

(defun mevedel-tool-task-session-has-active-p (session)
  "Return non-nil when SESSION has at least one non-completed task."
  (cl-some #'mevedel-tool-task--active-p
           (mevedel-session-tasks session)))

(defun mevedel-tool-task--owner-has-active-p (session owner)
  "Return non-nil when OWNER has at least one open task in SESSION."
  (let ((owner
         (mevedel-task-normalize-owner
          owner (mevedel-session-agent-registry session))))
    (cl-some
     (lambda (task)
       (and (equal owner (mevedel-task-owner task))
            (mevedel-tool-task--active-p task)))
     (mevedel-session-tasks session))))

(defun mevedel-tool-task--clear-inactive-status-notes (session)
  "Drop SESSION status notes for owners with no open tasks."
  (setf (mevedel-session-task-status-notes session)
        (cl-remove-if-not
         (lambda (entry)
           (mevedel-tool-task--owner-has-active-p session (car entry)))
         (mevedel-session-task-status-notes session))))

(defun mevedel-tool-task--format-status-note-line (session owner
                                                          &optional standalone)
  "Return OWNER's status note display line for SESSION, or nil.
The note hangs off the header or row above it unless STANDALONE, which
opens the panel with the main session's note and so has nothing to hang
from.  A note only renders while OWNER still has an open task, so a
completed-only owner never keeps a line on the panel."
  (when (mevedel-tool-task--owner-has-active-p session owner)
    (when-let* ((note (mevedel-tool-task--status-note session owner)))
      (propertize (if standalone note (format "  └ %s" note))
                  'font-lock-face '(:inherit italic)))))

(defun mevedel-tool-task--blocked-suffix (task tasks)
  "Return TASK's blocked-by suffix given the session's TASKS, or nil.
Two or more blockers that are all in progress collapse to a count;
anything else lists the blocking IDs, which a reader still has to
resolve but at least can."
  (when-let* ((blocked-by (mevedel-task-blocked-by task)))
    (mevedel-tool-task--dim
     (if (and (cdr blocked-by)
              (cl-every
               (lambda (id)
                 (when-let* ((blocker (cl-find id tasks
                                               :key #'mevedel-task-id)))
                   (eq (mevedel-task-status blocker) 'in-progress)))
               blocked-by))
         (format " · blocked by %d running" (length blocked-by))
       (format " · blocked by %s"
               (mevedel-tool-task--format-id-list blocked-by))))))

(defun mevedel-tool-task--format-one (task &optional owner-prefix tasks)
  "Format TASK as a single display line (propertized).
OWNER-PREFIX is a display label rendered dim ahead of the task text,
or nil for the main session and for rows already sitting under an
owner header.  TASKS is the session task list, used to describe
blockers."
  (let* ((status (mevedel-task-status task))
         (id (mevedel-task-id task))
         (activity (mevedel-tool-task--activity-summary task))
         ;; A running task's activeForm is the news; its subject is
         ;; already known.  Both share one row budget, because a very
         ;; long single line soft-wraps past the status fragment's
         ;; height cap.
         (text (mevedel--truncate-display
                (or activity
                    (string-clean-whitespace (mevedel-task-subject task)))
                72 t))
         (icon (pcase status
                 ('completed   "✔")
                 ('in-progress "→")
                 (_            "○")))
         (face (pcase status
                 ('completed   'mevedel-tool-task-completed)
                 ('in-progress 'mevedel-tool-task-in-progress)
                 (_            'default))))
    (concat (mevedel-tool-task--propertize-row-part icon face) " "
            (mevedel-tool-task--propertize-row-part
             (format "#%d " id) face)
            (when owner-prefix
              (mevedel-tool-task--dim (concat owner-prefix " · ")))
            (mevedel-tool-task--propertize-row-part text face)
            (mevedel-tool-task--blocked-suffix task tasks))))

(defun mevedel-tool-task--active-priority (task)
  "Return display priority for active TASK."
  (pcase (mevedel-task-status task)
    ('in-progress 0)
    ('pending (if (mevedel-task-blocked-by task) 2 1))
    (_ 3)))

(defun mevedel-tool-task--sort-active-tasks (tasks)
  "Return active TASKS in compact display priority order."
  (mapcar
   (lambda (entry) (nth 3 entry))
   (sort
    (cl-loop for task in tasks
             for index from 0
             collect (list (mevedel-tool-task--active-priority task)
                           (mevedel-task-id task)
                           index
                           task))
    (lambda (a b)
      (or (< (car a) (car b))
          (and (= (car a) (car b))
               (or (< (cadr a) (cadr b))
                   (and (= (cadr a) (cadr b))
                        (< (nth 2 a) (nth 2 b))))))))))

(defun mevedel-tool-task--sort-completed-tasks (tasks)
  "Return completed TASKS in stable display order."
  (mapcar
   #'cdr
   (sort
    (cl-loop for task in tasks
             for index from 0
             collect (cons (list (mevedel-task-id task) index) task))
    (lambda (a b)
      (let ((ak (car a))
            (bk (car b)))
        (or (< (car ak) (car bk))
            (and (= (car ak) (car bk))
                 (< (cadr ak) (cadr bk)))))))))

(defun mevedel-tool-task--summary-line (open completed)
  "Return one indented summary for omitted OPEN and COMPLETED tasks."
  (mevedel-tool-task--dim
   (concat "  "
           (cond
            ((and (> open 0) (> completed 0))
             (format "… %d more open · %d completed" open completed))
            ((> open 0) (format "… %d more open" open))
            (t (format "… %d completed" completed))))))

(defun mevedel-tool-task--render-rows (rows max-lines)
  "Join ROWS into a panel body, capping the result at MAX-LINES.
ROWS are (TEXT OPEN-COUNT COMPLETED-COUNT) triples whose counts sum to
the tasks a truncation drops."
  (cond
   ((null rows)
    (mevedel-tool-task--dim "No open tasks."))
   ((or (null max-lines) (<= (length rows) max-lines))
    (string-join (mapcar #'car rows) "\n"))
   (t
    (let* ((visible (cl-subseq rows 0 (max 0 (1- max-lines))))
           (omitted (nthcdr (length visible) rows))
           (open (cl-loop for row in omitted sum (nth 1 row)))
           (completed (cl-loop for row in omitted sum (nth 2 row))))
      ;; A cut landing right after a header leaves that header with
      ;; nothing under it; drop it rather than dangle it.
      (while (and visible
                  (zerop (nth 1 (car (last visible))))
                  (zerop (nth 2 (car (last visible)))))
        (setq visible (butlast visible)))
      (string-join
       (append (mapcar #'car visible)
               ;; An all-chrome remainder drops no tasks, so there is
               ;; nothing to summarize.
               (when (or (> open 0) (> completed 0))
                 (list (mevedel-tool-task--summary-line open completed))))
       "\n")))))

(defun mevedel-tool-task--format-groups (session &optional show-completed
                                                 max-lines)
  "Return the task panel body for SESSION.
Open tasks are ordered once by `mevedel-tool-task--sort-active-tasks'.
An owner holding a single open task renders inline behind a dim owner
prefix; an owner holding several renders under one dim header placed
at that owner's best-ordered task, which keeps its rows adjacent and
so bends the global order for the rest of the group.  The main session
renders without a prefix and never takes a header, and its status note
opens the panel because it has no header to hang from.  With
SHOW-COMPLETED, completed tasks follow in a single `done' section.
MAX-LINES caps the rendered body without changing task storage."
  (let* ((tasks (mevedel-session-tasks session))
         (active (mevedel-tool-task--sort-active-tasks
                  (cl-remove-if-not #'mevedel-tool-task--active-p tasks)))
         (buckets (seq-group-by #'mevedel-task-owner active))
         (grouped (make-hash-table :test #'equal))
         rows)
    (cl-flet ((note-row (owner)
                (when owner
                  (when-let*
                      ((line (mevedel-tool-task--format-status-note-line
                              session owner)))
                    (list line 0 0)))))
      (when-let* ((line (mevedel-tool-task--format-status-note-line
                         session nil t)))
        (push (list line 0 0) rows))
      (dolist (task active)
        (let* ((owner (mevedel-task-owner task))
               (bucket (alist-get owner buckets nil nil #'equal)))
          (cond
           ;; The main session is the default owner, so a "Main"
           ;; header names nothing its unprefixed rows do not already
           ;; say; and a sole open task's header would only repeat the
           ;; row beneath it.
           ((or (null owner) (null (cdr bucket)))
            (push (list (mevedel-tool-task--format-one
                         task
                         (and owner
                              (mevedel-tool-task--owner-display-label owner))
                         tasks)
                        1 0)
                  rows)
            (when-let* ((row (note-row owner)))
              (push row rows)))
           ;; Already emitted with its group.
           ((gethash owner grouped))
           (t
            (puthash owner t grouped)
            (push (list (mevedel-tool-task--dim
                         (mevedel-tool-task--owner-display-label owner))
                        0 0)
                  rows)
            (when-let* ((row (note-row owner)))
              (push row rows))
            (dolist (member bucket)
              (push (list (concat "  " (mevedel-tool-task--format-one
                                        member nil tasks))
                          1 0)
                    rows)))))))
    (when show-completed
      (when-let* ((completed
                   (mevedel-tool-task--sort-completed-tasks
                    (cl-remove-if #'mevedel-tool-task--active-p tasks))))
        (push (list (mevedel-tool-task--dim "done") 0 0) rows)
        (dolist (task completed)
          (let ((owner (mevedel-task-owner task)))
            (push (list (concat "  "
                                (mevedel-tool-task--format-one
                                 task
                                 (and owner
                                      (mevedel-tool-task--owner-display-label
                                       owner))
                                 tasks))
                        0 1)
                  rows)))))
    (mevedel-tool-task--render-rows (nreverse rows) max-lines)))

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
         (string-join (nreverse parts) " ")))
     tasks "\n")))

(defun mevedel-tool-task-format-active-for-llm (session)
  "Return SESSION's non-completed tasks in the TaskList text shape."
  (mevedel-tool-task--format-for-llm
   (cl-remove-if-not #'mevedel-tool-task--active-p
                     (mevedel-session-tasks session))))


;;
;;; Status display

(defconst mevedel-tool-task--status-min-body-lines 4
  "Minimum body lines reserved for task status.")

(defconst mevedel-tool-task--status-max-body-lines 12
  "Maximum body lines reserved for task status.")

(defconst mevedel-tool-task--status-expanded-max-body-lines 40
  "Maximum body lines reserved for task status while expanded.")

(defun mevedel-tool-task--status-line-budget (&optional expanded)
  "Return the current task status body-line budget, or nil.
EXPANDED widens the budget to make room for the completed section.  It
always exceeds the collapsed budget, because the expanded row list is
the collapsed one plus a `done' suffix and a truncation reserves a line
for its summary -- an equal budget would make expanding drop an open row
and reveal no completed row at all."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (let* ((height (window-body-height window))
           (collapsed
            (min mevedel-tool-task--status-max-body-lines
                 (max mevedel-tool-task--status-min-body-lines
                      (floor (* height 0.25))))))
      (if expanded
          (max (1+ collapsed)
               (min mevedel-tool-task--status-expanded-max-body-lines
                    (floor (* height 0.6))))
        collapsed))))

(defvar mevedel-tool-task-status-keymap
  (define-keymap
    "<tab>" #'mevedel-toggle-tasks
    "TAB"   #'mevedel-toggle-tasks
    "<return>" #'mevedel-toggle-tasks
    "RET"   #'mevedel-toggle-tasks)
  "Keymap installed on the task status fragment.
Shared with the fragment's header label via `where-is-internal' so the
displayed key matches the actual binding -- see
`mevedel-tool-task--toggle-key-label'.")

(defun mevedel-tool-task--toggle-key-label ()
  "Return the `key-description' string for toggling the task fragment.
Looks up `mevedel-toggle-tasks' directly in the fragment's keymap so
the label is correct regardless of where point is when the display
string is built.  Falls back to `M-x mevedel-toggle-tasks' if the
command has somehow lost its binding."
  (if-let* ((keys (where-is-internal 'mevedel-toggle-tasks
                                     mevedel-tool-task-status-keymap
                                     t)))
      (key-description keys)
    "M-x mevedel-toggle-tasks"))

(defun mevedel-tool-task--fragment-position (&optional position)
  "Return POSITION when it is on a status task fragment.
When POSITION is just after task fragment text, also accept the previous
character so keybindings on trailing newlines still toggle the fragment."
  (let ((pos (or position (point))))
    (or (and (eq (get-text-property pos 'mevedel-view-zone-namespace)
                 'status)
             (eq (get-text-property pos 'mevedel-view-zone-id) 'tasks)
             pos)
        (and (> pos (point-min))
             (let ((prev (1- pos)))
               (and (eq (get-text-property prev
                                           'mevedel-view-zone-namespace)
                        'status)
                    (eq (get-text-property prev 'mevedel-view-zone-id)
                        'tasks)
                    prev))))))

(defun mevedel-toggle-tasks ()
  "Toggle completed task visibility in the session task list."
  (interactive)
  (if-let* ((pos (mevedel-tool-task--fragment-position))
            (collapse-key (get-text-property
                           pos 'mevedel-view-zone-collapse-key)))
      (progn
        (mevedel-view-zone-set-collapse-state
         collapse-key
         (not (mevedel-view-zone-collapse-state
               collapse-key
               (get-text-property pos 'mevedel-view-zone-collapsed))))
        (mevedel-view--render-status))
    (message "No task list here")))

(defun mevedel-tool-task--tallies (session)
  "Return SESSION's task tallies as one compact label, or nil when empty.
Zero terms are omitted so the separator rule stays short in the common
case."
  (let ((running 0)
        (blocked 0)
        (open 0)
        (done 0))
    (dolist (task (mevedel-session-tasks session))
      (pcase (mevedel-task-status task)
        ('completed (cl-incf done))
        ('in-progress (cl-incf running))
        (_ (if (mevedel-task-blocked-by task)
               (cl-incf blocked)
             (cl-incf open)))))
    (when-let* ((parts
                 (delq nil
                       (list (and (> running 0) (format "%d running" running))
                             (and (> blocked 0) (format "%d blocked" blocked))
                             (and (> open 0) (format "%d open" open))
                             (and (> done 0) (format "%d done" done))))))
      (string-join parts " · "))))

(defun mevedel-tool-task--task-label (session show-completed)
  "Return the status-zone task label for SESSION.
The tallies live here rather than on per-owner headers, so the panel
states each count once.  SHOW-COMPLETED selects the toggle wording."
  (let ((separator (propertize " · " 'face 'mevedel-view-zone-separator))
        (tallies (mevedel-tool-task--tallies session))
        (toggle-key
         (propertize (mevedel-tool-task--toggle-key-label)
                     'face 'help-key-binding)))
    (concat
     (propertize "tasks" 'face 'mevedel-view-zone-separator)
     (when tallies
       (concat separator
               (propertize tallies 'face 'mevedel-view-zone-separator)))
     separator
     toggle-key
     (propertize (if show-completed " to collapse" " to expand")
                 'face 'mevedel-view-zone-separator))))

(defun mevedel-tool-task-display-string (session show-completed)
  "Return task display for SESSION.
SHOW-COMPLETED controls whether completed task detail is included."
  (let* ((body (mevedel-tool-task--format-groups
                session show-completed
                (mevedel-tool-task--status-line-budget show-completed)))
         (separator (mevedel-view--zone-separator
                     (mevedel-tool-task--task-label
                      session show-completed))))
    (concat separator body "\n\n")))

(defun mevedel-tool-task-refresh-display ()
  "Render the current session's task status in its view buffer."
  (when-let* ((session (and (boundp 'mevedel--session) mevedel--session))
              (view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer
                             (buffer-live-p mevedel--view-buffer)
                             mevedel--view-buffer)))
    (with-current-buffer view-buf
      (let ((mevedel--session session))
        (mevedel-view--render-status)))))


;;
;;; Tool handlers

(defun mevedel-tool-task--task-container-p (value)
  "Return non-nil when VALUE matches a task object in a list batch."
  (and (listp value)
       (or (and (keywordp (car value))
                (consp (cdr value)))
           (and (consp (car value))
                (atom (caar value))))))

(defun mevedel-tool-task--tasks-arg (args)
  "Return the `:tasks' argument from ARGS as a list of task specs.
Accepts a vector, list, or single task object; always returns a list."
  (let ((raw (plist-get args :tasks)))
    (cond
     ((null raw) (error "Parameter tasks is required"))
     ((vectorp raw) (append raw nil))
     ;; Single task object passed as a plist -- wrap in list.
     ((and (listp raw) (keywordp (car raw)))
      (list raw))
     ;; Lisp callers may pass a list batch instead of a JSON vector.
     ((and (listp raw) (cl-every #'mevedel-tool-task--task-container-p raw))
      raw)
     ;; Single task object passed as an alist -- wrap in list.
     ((and (listp raw) (consp (car raw)) (atom (caar raw)))
      (list raw))
     ((listp raw) raw)
     (t (error "Parameter tasks must be an array, got %S" raw)))))

(defconst mevedel-tool-task--note-owner-keys
  '(:noteOwner :note_owner :note-owner)
  "Accepted top-level owner keys for task status notes.")

(defun mevedel-tool-task--note-feedback (owner note stored)
  "Return feedback for OWNER status NOTE when STORED."
  (let ((label (mevedel-tool-task--owner-label owner)))
    (cond
     (stored
      (format "Status note for %s: %s" label note))
     (note
      (format "Status note for %s not shown because that owner has no open tasks."
              label))
     (t
      (format "Cleared status note for %s." label)))))

(defun mevedel-tool-task--prepare-note-arg (session args owner-keys)
  "Return (OWNER . NOTE) for ARGS' optional top-level :note in SESSION.
OWNER-KEYS names the optional owner argument aliases.  Returns nil when
:note is absent.  Both the owner and the note text are resolved here, so
an unresolvable owner signals before any task has been mutated; NOTE is
nil when the note clears the owner's entry."
  (when (mevedel-tool-task--argument-value-present-p args :note)
    (cons (mevedel-tool-task--owner-from-args session args owner-keys)
          (mevedel-tool-task--normalize-note (plist-get args :note)))))

(defun mevedel-tool-task--apply-note-arg (session prepared)
  "Store PREPARED, a `mevedel-tool-task--prepare-note-arg' result, in SESSION.
Returns a feedback string when PREPARED is non-nil, otherwise nil.
Whether a note is storable depends on the owner having an open task, so
this runs after the task mutation it accompanies."
  (when prepared
    (let ((owner (car prepared))
          (note (cdr prepared))
          stored)
      (if (and note (mevedel-tool-task--owner-has-active-p session owner))
          (progn
            (mevedel-tool-task--set-status-note session owner note)
            (setq stored t))
        (mevedel-tool-task--set-status-note session owner nil))
      (mevedel-tool-task--clear-inactive-status-notes session)
      (mevedel-tool-task--note-feedback owner note stored))))

(defun mevedel-tool-task--handle-create (args)
  "Handler for TaskCreate.  ARGS has :tasks."
  (let* ((session (mevedel-tool-task--session))
         (specs (mevedel-tool-task--tasks-arg args))
         (next-id (mevedel-tool-task--next-id session))
         (created nil)
         (prepared-note
          (mevedel-tool-task--prepare-note-arg
           session args mevedel-tool-task--note-owner-keys))
         note-feedback)
    (dolist (spec specs)
      (push (mevedel-tool-task--create-one session spec next-id) created)
      (cl-incf next-id))
    (setq created (nreverse created))
    (setf (mevedel-session-tasks session)
          (append (mevedel-session-tasks session) created))
    (setq note-feedback
          (mevedel-tool-task--apply-note-arg session prepared-note))
    (mevedel-tool-task--mark-write session)
    (mevedel-tool-task-refresh-display)
    (let ((base (format "Created %d task%s:\n%s"
                        (length created)
                        (if (= 1 (length created)) "" "s")
                        (mevedel-tool-task--format-for-llm created))))
      (list :result
            (if note-feedback
                (concat base "\n" note-feedback)
              base)))))

(defun mevedel-tool-task--handle-update (args)
  "Handler for TaskUpdate.  ARGS has :id and zero or more update fields."
  (let* ((session (mevedel-tool-task--session))
         (id (plist-get args :id))
         ;; Build an updates plist from the remaining keys.
         (updates (cl-loop for (k v) on args by #'cddr
                           unless (memq k (cons :id
                                                (cons :note
                                                      mevedel-tool-task--note-owner-keys)))
                           nconc (list k v))))
    (unless (integerp id)
      (error "Parameter id is required and must be an integer"))
    (let* ((prepared-note
            (mevedel-tool-task--prepare-note-arg
             session args mevedel-tool-task--note-owner-keys))
           (task (mevedel-tool-task--update-one session id updates))
           (note-feedback
            (mevedel-tool-task--apply-note-arg session prepared-note)))
      (mevedel-tool-task--clear-inactive-status-notes session)
      (mevedel-tool-task--mark-write session)
      (mevedel-tool-task-refresh-display)
      (let ((base (format "Updated task:\n%s"
                          (mevedel-tool-task--format-for-llm
                           (list task)))))
        (list :result
              (if note-feedback
                  (concat base "\n" note-feedback)
                base))))))

(defun mevedel-tool-task--handle-note (args)
  "Handler for TaskNote.  ARGS has :note and optional :owner."
  (let ((session (mevedel-tool-task--session)))
    (unless (mevedel-tool-task--argument-value-present-p args :note)
      (error "Parameter note is required"))
    (let* ((feedback
            (mevedel-tool-task--apply-note-arg
             session
             (mevedel-tool-task--prepare-note-arg session args '(:owner)))))
      (mevedel-tool-task--mark-write session)
      (mevedel-tool-task-refresh-display)
      (list :result feedback))))

(defun mevedel-tool-task--handle-list (args)
  "Handler for TaskList.  ARGS may include :status filter."
  (let* ((session (mevedel-tool-task--session))
         (filter-raw (mevedel-tool-task--optional-argument args :status))
         (filter (and filter-raw
                      (mevedel-tool-task--parse-status filter-raw)))
         (tasks (mevedel-session-tasks session))
         (filtered (if filter
                       (cl-remove-if-not
                        (lambda (t1) (eq (mevedel-task-status t1) filter))
                        tasks)
                     tasks)))
    (mevedel-tool-task-refresh-display)
    (list :result
          (if filter
              (format "Tasks with status %s:\n%s"
                      (mevedel-tool-task--status-string filter)
                      (mevedel-tool-task--format-for-llm filtered))
            (mevedel-tool-task--format-for-llm filtered)))))

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
        (when-let* ((m (mevedel-task-metadata task)))
          (insert (format "Metadata: %S\n" m)))
        (list :result (buffer-string))))))


;;
;;; Tool result renderers

(defun mevedel-tool-task--result-status (result)
  "Return renderer status for RESULT."
  (and (stringp result)
       (string-prefix-p "Error:" result)
       'error))

(defun mevedel-tool-task--first-result-line (result)
  "Return the first non-empty line from RESULT."
  (or (car (split-string (or result "") "\n" t))
      "done"))

(defun mevedel-tool-task--task-line-count (result)
  "Return the number of task detail lines in RESULT."
  (if (stringp result)
      (cl-loop for line in (split-string result "\n" t)
               count (string-prefix-p "#" (string-trim-left line)))
    0))

(defun mevedel-tool-task--render-event (name _args result _render-data)
  "Return compact event renderer for NAME and RESULT."
  (when (stringp result)
    (list :header (format "%s: %s"
                          (or name "Task")
                          (mevedel-tool-task--first-result-line result))
          :status (mevedel-tool-task--result-status result)
          :expandable-p nil)))

(defun mevedel-tool-task--render-mutation (name _args result _render-data)
  "Return collapsible mutation renderer for NAME and RESULT."
  (when (stringp result)
    (let* ((tool-name (or name "Task"))
           (status (mevedel-tool-task--result-status result))
           (fallback (format "%s: %s"
                             tool-name
                             (mevedel-tool-task--first-result-line result)))
           (task-lines
            (cl-loop for line in (split-string result "\n" t)
                     for trimmed = (string-trim-left line)
                     when (string-match
                           "\\`#\\([0-9]+\\) \\[\\([^]]+\\)\\]" trimmed)
                     collect (cons (string-to-number (match-string 1 trimmed))
                                   (match-string 2 trimmed))))
           (first (car task-lines))
           (last-id (caar (last task-lines)))
           (header
            (cond
             (status fallback)
             ((and (equal name "TaskCreate") first)
              (format "%s: %d created · %s"
                      tool-name (length task-lines)
                      (if (= (car first) last-id)
                          (format "#%d" (car first))
                        (format "#%d–#%d" (car first) last-id))))
             ((and (equal name "TaskUpdate") first)
              (format "%s: #%d updated · %s"
                      tool-name (car first) (cdr first)))
             (t fallback))))
      (list :header header
            :body result
            :body-mode nil
            :status status
            :initially-collapsed-p t))))

(defun mevedel-tool-task--render-list (name args result _render-data)
  "Return rendering plist for TaskList NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((status-filter (mevedel-tool-task--optional-argument
                           args :status))
           (count (mevedel-tool-task--task-line-count result))
           (suffix (if status-filter
                       (format "%s, %d %s"
                               status-filter count
                               (if (= count 1) "task" "tasks"))
                     (format "%d %s"
                             count
                             (if (= count 1) "task" "tasks")))))
      (list :header (format "%s: %s" (or name "TaskList") suffix)
            :body result
            :body-mode nil
            :status (mevedel-tool-task--result-status result)
            :initially-collapsed-p t))))

(defun mevedel-tool-task--render-get (name args result _render-data)
  "Return rendering plist for TaskGet NAME, ARGS, and RESULT."
  (when (stringp result)
    (let ((id (plist-get args :id)))
      (list :header (format "%s: #%s"
                            (or name "TaskGet")
                            (or id "?"))
            :body result
            :body-mode nil
            :status (mevedel-tool-task--result-status result)
            :initially-collapsed-p t))))


;;
;;; Registration

(defun mevedel-tool-task--register ()
  "Register the task tracking tools."

  (mevedel-define-tool
    :name "TaskCreate"
    :description "Create one or more tasks in the session task list."
    :prompt-file "prompts/tools/taskcreate.md"
    :handler #'mevedel-tool-task--handle-create
    :args ((tasks array :required
                  "Array of task objects. Each object has: subject (string, required), description (string, optional), status (\"pending\"|\"in_progress\"|\"completed\", optional), owner (canonical agent path or bucket string, optional; use subjects/descriptions for workstream names), blockedBy (array of task IDs, optional), metadata (object, optional)."
                  :items (:type object)
                  :minItems 1)
           (note string :optional
                 "Optional owner-scoped status note to show above that owner's open tasks. Empty string intentionally clears it.")
           (noteOwner string :optional
                      "Owner for note. Omit for the current caller, pass an empty string for Main."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-task--render-mutation)

  (mevedel-define-tool
    :name "TaskUpdate"
    :description "Update the status or fields of an existing task."
    :prompt-file "prompts/tools/taskupdate.md"
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
                  "New canonical agent path or bucket. Pass an empty string to unassign; prefer subjects/descriptions for workstream names.")
           (blockedBy array :optional
                      "Array of task IDs blocking this task."
                      :items (:type integer))
           (metadata object :optional
                     "Free-form metadata object.")
           (note string :optional
                 "Optional owner-scoped status note to show above that owner's open tasks. Empty string intentionally clears it.")
           (noteOwner string :optional
                      "Owner for note. Omit for the current caller, pass an empty string for Main."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-task--render-mutation)

  (mevedel-define-tool
    :name "TaskNote"
    :description "Set or clear the current status note for an owner task group."
    :prompt-file "prompts/tools/tasknote.md"
    :handler #'mevedel-tool-task--handle-note
    :args ((note string :required
                 "Owner-scoped status note to show above open tasks. Pass an empty string only when intentionally clearing.")
           (owner string :optional
                  "Owner for the note. Omit for the current caller, pass an empty string for Main."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-task--render-event)

  (mevedel-define-tool
    :name "TaskList"
    :description "List the tasks currently tracked in the session."
    :prompt-file "prompts/tools/tasklist.md"
    :handler #'mevedel-tool-task--handle-list
    :args ((status string :optional
                   "Optional filter: \"pending\", \"in_progress\", or \"completed\"."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-task--render-list)

  (mevedel-define-tool
    :name "TaskGet"
    :description "Retrieve full details for a single task by ID."
    :prompt-file "prompts/tools/taskget.md"
    :handler #'mevedel-tool-task--handle-get
    :args ((id integer :required
               "The integer ID of the task to retrieve."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-task--render-get))

(provide 'mevedel-tool-task)
;;; mevedel-tool-task.el ends here
