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

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)

;; `mevedel-view'
(defvar mevedel-view--input-marker)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)
(declare-function mevedel-view--zone-separator "mevedel-view" (label))

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--display-label-from-canonical
                  "mevedel-tool-ui" (agent-id))


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

(defun mevedel-tool-task--normalize-owner (owner)
  "Normalize OWNER to a non-empty string or nil."
  (and (stringp owner)
       (not (string-empty-p owner))
       owner))

(defun mevedel-tool-task--current-agent-owner ()
  "Return the current sub-agent owner label, or nil in the main session."
  (and (boundp 'mevedel--agent-invocation)
       (mevedel-agent-invocation-p mevedel--agent-invocation)
       (mevedel-tool-task--normalize-owner
        (mevedel-agent-invocation-agent-id mevedel--agent-invocation))))

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
  (1+ (or (mevedel-session-turn-count session) 0)))

(defun mevedel-tool-task--mark-write (session)
  "Record a successful task write on SESSION."
  (setf (mevedel-session-last-task-write-turn session)
        (mevedel-tool-task--write-turn session)))

(defun mevedel-tool-task--create-one (session spec &optional id)
  "Return a task for SESSION from SPEC plist.
Returns the new `mevedel-task' struct."
  (let* ((p (mevedel-tool-task--object-to-plist spec))
         (subject (mevedel-tool-task--plist-get-any p :subject))
         (description (mevedel-tool-task--plist-get-any p :description))
         (status-raw (mevedel-tool-task--plist-get-any p :status))
         (owner (if (plist-member p :owner)
                    (plist-get p :owner)
                  (mevedel-tool-task--current-agent-owner)))
         (blocks-raw (mevedel-tool-task--plist-get-any p :blocks))
         (blocked-by-raw (mevedel-tool-task--plist-get-any
                          p :blockedBy :blocked_by :blocked-by))
         (metadata (mevedel-tool-task--plist-get-any p :metadata)))
    (unless (and (stringp subject) (not (string-empty-p subject)))
      (error "Task subject is required and must be a non-empty string"))
    (let* ((status (mevedel-tool-task--parse-status status-raw))
           (task (mevedel-task--create
                  :id (or id (mevedel-tool-task--next-id session))
                  :subject subject
                  :description (and (stringp description) description)
                  :status status
                  :owner (mevedel-tool-task--normalize-owner owner)
                  :blocks (mevedel-tool-task--normalize-id-list blocks-raw)
                  :blocked-by (mevedel-tool-task--normalize-id-list
                               blocked-by-raw)
                  :completed-turn (and (eq status 'completed)
                                       (mevedel-tool-task--write-turn
                                        session))
                  :metadata metadata)))
      task)))

(defun mevedel-tool-task--update-one (session id updates)
  "Apply UPDATES plist to the task with ID in SESSION.
Returns the updated task.  Signals an error if ID is unknown."
  (let ((task (mevedel-tool-task--find session id))
        (p (mevedel-tool-task--object-to-plist updates)))
    (unless task
      (error "No task with id %s" id))
    (let (subject subject-p
          description description-p
          new-status status-p
          owner owner-p
          blocks blocks-p
          blocked-by blocked-by-p
          metadata metadata-p)
      (when-let* ((subject-value
                   (mevedel-tool-task--plist-get-any p :subject)))
        (unless (stringp subject-value)
          (error "Task subject must be a string"))
        (setq subject subject-value
              subject-p t))
      (when (plist-member p :description)
        (let ((d (plist-get p :description)))
          (setq description (and (stringp d) d)
                description-p t)))
      (let ((status-raw (mevedel-tool-task--plist-get-any p :status)))
        (when (and status-raw
                   (not (and (stringp status-raw)
                             (string-empty-p status-raw))))
          (setq new-status (mevedel-tool-task--parse-status status-raw)
                status-p t)))
      (when (plist-member p :owner)
        (setq owner (mevedel-tool-task--normalize-owner
                     (plist-get p :owner))
              owner-p t))
      (when (plist-member p :blocks)
        (setq blocks (mevedel-tool-task--normalize-id-list
                      (plist-get p :blocks))
              blocks-p t))
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
                  (mevedel-tool-task--write-turn session))
            (mevedel-tool-task--propagate-completion session task))))
      (when owner-p
        (setf (mevedel-task-owner task) owner))
      (when blocks-p
        (setf (mevedel-task-blocks task) blocks))
      (when blocked-by-p
        (setf (mevedel-task-blocked-by task) blocked-by))
      (when metadata-p
        (setf (mevedel-task-metadata task) metadata)))
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

(defun mevedel-tool-task--owner-label (owner)
  "Return the display label for OWNER."
  (if (and (stringp owner) (not (string-empty-p owner)))
      (or (and (fboundp 'mevedel-tool-ui--display-label-from-canonical)
               (mevedel-tool-ui--display-label-from-canonical owner))
          (if-let* ((sep (string-search "--" owner)))
              (let* ((type (substring owner 0 sep))
                     (suffix (substring owner (+ sep 2)))
                     (short (substring suffix 0 (min 8 (length suffix)))))
                (concat type "--" short))
            owner))
    "Main"))

(defun mevedel-tool-task--owner-sort-label (owner)
  "Return a stable sort label for OWNER groups."
  (downcase (mevedel-tool-task--owner-label owner)))

(defun mevedel-tool-task--group-tasks (tasks)
  "Return TASKS grouped by owner with Main first.
Each element is (OWNER . TASK-LIST), preserving task order within each
group and sorting agent-owned groups by owner label."
  (let ((table (make-hash-table :test #'equal))
        owners)
    (dolist (task tasks)
      (let ((owner (mevedel-task-owner task)))
        (unless (gethash owner table)
          (push owner owners))
        (puthash owner (append (gethash owner table) (list task)) table)))
    (let* ((agent-owners
            (sort (cl-remove-if #'null owners)
                  (lambda (a b)
                    (string< (mevedel-tool-task--owner-sort-label a)
                             (mevedel-tool-task--owner-sort-label b))))))
      (mapcar (lambda (owner)
                (cons owner (gethash owner table)))
              (append (and (gethash nil table) (list nil))
                      agent-owners)))))

(defun mevedel-tool-task--group-header (owner tasks)
  "Return the display header for OWNER and TASKS."
  (let ((active 0)
        (done 0))
    (dolist (task tasks)
      (if (eq (mevedel-task-status task) 'completed)
          (cl-incf done)
        (cl-incf active)))
    (propertize
     (format "%s · %d active · %d done"
             (mevedel-tool-task--owner-label owner)
             active
             done)
     'face 'font-lock-comment-face)))

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
  "Return TASK's short display activity, or nil."
  (when (and (eq (mevedel-task-status task) 'in-progress)
             (mevedel-task-owner task))
    (let ((activity (mevedel-tool-task--metadata-get
                     (mevedel-task-metadata task)
                     :activity :activeForm :active-form :active_form)))
      (when (and (stringp activity)
                 (not (string-empty-p activity)))
        (truncate-string-to-width
         (replace-regexp-in-string "[\n\r\t ]+" " " activity)
         72 nil nil t)))))

(defun mevedel-tool-task--format-id-list (ids)
  "Return IDS as a compact task-reference list."
  (mapconcat (lambda (id) (format "#%d" id)) ids ", "))

(defun mevedel-tool-task--active-p (task)
  "Return non-nil when TASK is not completed."
  (not (eq (mevedel-task-status task) 'completed)))

(defun mevedel-tool-task--session-has-active-p (session)
  "Return non-nil when SESSION has at least one non-completed task."
  (cl-some #'mevedel-tool-task--active-p
           (mevedel-session-tasks session)))

(defun mevedel-tool-task--format-one (task)
  "Format TASK as a single display line (propertized)."
  (let* ((status (mevedel-task-status task))
         (id (mevedel-task-id task))
         (subject (mevedel-task-subject task))
         (blocked-by (mevedel-task-blocked-by task))
         (activity (mevedel-tool-task--activity-summary task))
         (icon (pcase status
                 ('completed   "✓")
                 ('in-progress "→")
                 (_            "○")))
         (face (pcase status
                 ('completed   '(:inherit success :strike-through t))
                 ('in-progress '(:inherit bold :inherit warning))
                 (_            'default)))
         (suffix (concat
                  (when blocked-by
                    (propertize
                     (format " · blocked by %s"
                             (mevedel-tool-task--format-id-list blocked-by))
                     'face 'font-lock-comment-face))
                  (when activity
                    (propertize (format " · %s" activity)
                                'face 'font-lock-comment-face)))))
    (concat icon " "
            (propertize (format "#%d " id) 'face 'font-lock-comment-face)
            (propertize subject 'face face)
            suffix)))

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

(defun mevedel-tool-task--summary-line (format count)
  "Return an indented omitted-row summary using FORMAT and COUNT."
  (propertize (concat "  " (format format count))
              'face 'font-lock-comment-face))

(cl-defstruct (mevedel-tool-task--render-group
               (:constructor mevedel-tool-task--render-group--create))
  "A temporary owner group used while applying the overlay line cap."
  owner tasks index active completed selected-active selected-completed
  summary-only)

(defun mevedel-tool-task--render-groups (groups)
  "Return task render GROUPS for the capped overlay path."
  (cl-loop for group in groups
           for index from 0
           for owner = (car group)
           for tasks = (cdr group)
           for active = (mevedel-tool-task--sort-active-tasks
                         (cl-remove-if-not
                          #'mevedel-tool-task--active-p tasks))
           for completed = (mevedel-tool-task--sort-completed-tasks
                            (cl-remove-if-not
                             (lambda (task)
                               (eq (mevedel-task-status task) 'completed))
                             tasks))
           collect (mevedel-tool-task--render-group--create
                    :owner owner
                    :tasks tasks
                    :index index
                    :active active
                    :completed completed)))

(defun mevedel-tool-task--candidate-sort (a b)
  "Return non-nil when candidate A should render before B."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
           (or (< (nth 1 a) (nth 1 b))
               (and (= (nth 1 a) (nth 1 b))
                    (< (nth 2 a) (nth 2 b)))))))

(defun mevedel-tool-task--active-candidates (groups)
  "Return capped-render candidates for active tasks in GROUPS."
  (sort
   (cl-loop for group in groups append
            (cl-loop for task in
                     (mevedel-tool-task--render-group-active group)
                     for task-index from 0
                     collect (list
                              (mevedel-tool-task--active-priority task)
                              (mevedel-tool-task--render-group-index group)
                              task-index group task)))
   #'mevedel-tool-task--candidate-sort))

(defun mevedel-tool-task--completed-candidates (groups)
  "Return capped-render candidates for completed tasks in GROUPS."
  (cl-loop for group in groups append
           (cl-loop for task in
                    (mevedel-tool-task--render-group-completed group)
                    for task-index from 0
                    collect (list 0
                                  (mevedel-tool-task--render-group-index
                                   group)
                                  task-index group task))))

(defun mevedel-tool-task--group-selected-p (group)
  "Return non-nil when GROUP already has selected detail rows."
  (or (mevedel-tool-task--render-group-selected-active group)
      (mevedel-tool-task--render-group-selected-completed group)))

(defun mevedel-tool-task--select-candidates (candidates remaining slot)
  "Select CANDIDATES into SLOT while REMAINING lines allow it.
Return (SELECTED . REMAINING)."
  (let (selected touched)
    (dolist (candidate candidates)
      (let* ((group (nth 3 candidate))
             (task (nth 4 candidate))
             (cost (if (mevedel-tool-task--group-selected-p group) 1 2)))
        (when (and (> remaining 0) (<= cost remaining))
          (push candidate selected)
          (cl-pushnew group touched)
          (pcase slot
            ('active
             (push task
                   (mevedel-tool-task--render-group-selected-active
                    group)))
            ('completed
             (push task
                   (mevedel-tool-task--render-group-selected-completed
                    group))))
          (setq remaining (- remaining cost)))))
    (dolist (group touched)
      (pcase slot
        ('active
         (setf (mevedel-tool-task--render-group-selected-active group)
               (nreverse
                (mevedel-tool-task--render-group-selected-active group))))
        ('completed
         (setf (mevedel-tool-task--render-group-selected-completed group)
               (nreverse
                (mevedel-tool-task--render-group-selected-completed group))))))
    (cons selected remaining)))

(defun mevedel-tool-task--first-candidates-per-group (candidates)
  "Return the first active candidate for each owner group in CANDIDATES."
  (let (seen result)
    (dolist (candidate candidates)
      (let ((group (nth 3 candidate)))
        (unless (memq group seen)
          (push group seen)
          (push candidate result))))
    (nreverse result)))

(defun mevedel-tool-task--active-selection-candidates (candidates)
  "Return active CANDIDATES ordered for compact owner-fair selection."
  (let* ((first-candidates
          (mevedel-tool-task--first-candidates-per-group candidates))
         (rest-candidates
          (cl-remove-if (lambda (candidate)
                          (memq candidate first-candidates))
                        candidates)))
    (append first-candidates rest-candidates)))

(defun mevedel-tool-task--select-active-candidates
    (candidates remaining)
  "Select active CANDIDATES into render groups.
The first visible row for each owner is selected before extra rows from
already visible owners, so one busy owner cannot consume the whole cap."
  (mevedel-tool-task--select-candidates
   (mevedel-tool-task--active-selection-candidates candidates)
   remaining 'active))

(defun mevedel-tool-task--selected-active-count (group)
  "Return the number of selected active rows in GROUP."
  (length (mevedel-tool-task--render-group-selected-active group)))

(defun mevedel-tool-task--group-hidden-active-count (group)
  "Return the number of active rows hidden in GROUP."
  (- (length (mevedel-tool-task--render-group-active group))
     (mevedel-tool-task--selected-active-count group)))

(defun mevedel-tool-task--hidden-active-count (groups)
  "Return the number of active rows hidden in GROUPS."
  (cl-loop for group in groups
           sum (mevedel-tool-task--group-hidden-active-count group)))

(defun mevedel-tool-task--selected-active-priority (group)
  "Return GROUP's best selected active priority."
  (apply #'min 99
         (mapcar #'mevedel-tool-task--active-priority
                 (mevedel-tool-task--render-group-selected-active
                  group))))

(defun mevedel-tool-task--render-group-sort (a b)
  "Return non-nil when capped render group A should appear before B."
  (let ((pa (mevedel-tool-task--selected-active-priority a))
        (pb (mevedel-tool-task--selected-active-priority b)))
    (or (< pa pb)
        (and (= pa pb)
             (< (mevedel-tool-task--render-group-index a)
                (mevedel-tool-task--render-group-index b))))))

(defun mevedel-tool-task--reserve-active-summary
    (selected candidates groups remaining)
  "Reserve one line for a hidden-active summary when needed.
Return (HIDDEN . REMAINING)."
  (let ((hidden (mevedel-tool-task--hidden-active-count groups)))
    (when (and (> hidden 0) (<= remaining 0) selected)
      (let ((hidden-priority
             (apply #'min 99
                    (mapcar #'car
                            (cl-set-difference candidates selected
                                               :test #'eq)))))
        (when-let* ((candidate
                     (cl-find-if
                      (lambda (candidate)
                        (let ((group (nth 3 candidate)))
                          (and (>= (car candidate) hidden-priority)
                               (> (mevedel-tool-task--selected-active-count
                                   group)
                                  1))))
                      selected)))
          (let* ((group (nth 3 candidate))
                 (task (nth 4 candidate))
                 (selected-tasks
                  (mevedel-tool-task--render-group-selected-active
                   group))
                 (only-row (= 1 (length selected-tasks))))
            (setf (mevedel-tool-task--render-group-selected-active group)
                  (cl-remove task selected-tasks :count 1 :test #'eq))
            (setq remaining (+ remaining (if only-row 2 1)))
            (setq hidden
                  (mevedel-tool-task--hidden-active-count groups))))))
    (unless (> remaining 0)
      (setq hidden 0))
    (cons hidden remaining)))

(defun mevedel-tool-task--group-rendered-p (group)
  "Return non-nil when GROUP will render at least one visible line."
  (or (mevedel-tool-task--render-group-selected-active group)
      (mevedel-tool-task--render-group-selected-completed group)
      (mevedel-tool-task--render-group-summary-only group)))

(defun mevedel-tool-task--hidden-completed-count (groups)
  "Return completed rows in GROUPS not represented by visible headers."
  (cl-loop for group in groups
           unless (mevedel-tool-task--group-rendered-p group)
           sum (length (mevedel-tool-task--render-group-completed group))))

(defun mevedel-tool-task--append-inline-summary
    (header format count)
  "Return HEADER with an inline omitted-row summary using FORMAT and COUNT."
  (concat header
          (propertize (format format count)
                      'face 'font-lock-comment-face)))

(defun mevedel-tool-task--format-groups-capped
    (groups show-completed active-only max-lines)
  "Return formatted GROUPS capped to MAX-LINES body lines."
  (let* ((render-groups (mevedel-tool-task--render-groups groups))
         (active-candidates
          (mevedel-tool-task--active-candidates render-groups))
         (active-result
          (mevedel-tool-task--select-active-candidates
           active-candidates max-lines))
         (selected-active (car active-result))
         (remaining (cdr active-result))
         (summary-result
          (mevedel-tool-task--reserve-active-summary
           selected-active active-candidates render-groups remaining))
         (hidden-active (car summary-result))
         (inline-hidden-active 0)
         (remaining (cdr summary-result))
         hidden-completed
         completed-summary-reserved
         inline-active-summary-used
         inline-hidden-completed
         inline-completed-summary-used
         sections)
    (setq inline-hidden-active
          (and (= hidden-active 0)
               (mevedel-tool-task--hidden-active-count render-groups)))
    (when (and (> hidden-active 0) (> remaining 0))
      (setq remaining (1- remaining)))
    (when (and show-completed (not active-only) (> remaining 0))
      (let* ((completed-candidates
              (mevedel-tool-task--completed-candidates render-groups))
             (completed-total (length completed-candidates))
             (reserve-summary (> completed-total remaining))
             (selection-budget
              (if reserve-summary
                  (max 0 (1- remaining))
                remaining))
             (completed-result
              (mevedel-tool-task--select-candidates
               completed-candidates selection-budget 'completed)))
        (setq remaining (cdr completed-result)
              hidden-completed (- completed-total
                                  (length (car completed-result)))
              completed-summary-reserved
              (and (> hidden-completed 0)
                   (or reserve-summary (> remaining 0))))
        (when (and (> hidden-completed 0)
                   (not completed-summary-reserved))
          (setq inline-hidden-completed hidden-completed))))
    (when (and (not show-completed) (not active-only) (> remaining 0))
      (dolist (group render-groups)
        (when (and (> remaining 0)
                   (null (mevedel-tool-task--render-group-active group))
                   (mevedel-tool-task--render-group-completed group))
          (setf (mevedel-tool-task--render-group-summary-only group) t)
          (setq remaining (1- remaining)))))
    (when (and (not active-only)
               (not completed-summary-reserved)
               (not inline-hidden-completed))
      (setq inline-hidden-completed
            (mevedel-tool-task--hidden-completed-count render-groups)))
    (dolist (group (sort (copy-sequence render-groups)
                         #'mevedel-tool-task--render-group-sort))
      (when (mevedel-tool-task--group-rendered-p group)
        (let* ((header
                (mevedel-tool-task--group-header
                 (mevedel-tool-task--render-group-owner group)
                 (mevedel-tool-task--render-group-tasks group))))
          (when (and (not inline-active-summary-used)
                     inline-hidden-active
                     (> inline-hidden-active 0))
            (setq inline-active-summary-used t)
            (setq header
                  (mevedel-tool-task--append-inline-summary
                   header
                   " · … %d more active"
                   inline-hidden-active)))
          (when (and (not inline-completed-summary-used)
                     inline-hidden-completed
                     (> inline-hidden-completed 0))
            (setq inline-completed-summary-used t)
            (setq header
                  (mevedel-tool-task--append-inline-summary
                   header
                   " · … %d completed"
                   inline-hidden-completed)))
          (let ((lines (list header)))
          (dolist (task
                   (mevedel-tool-task--render-group-selected-active group))
            (push (concat "  " (mevedel-tool-task--format-one task))
                  lines))
          (dolist (task
                   (mevedel-tool-task--render-group-selected-completed
                    group))
            (push (concat "  " (mevedel-tool-task--format-one task))
                  lines))
          (push (string-join (nreverse lines) "\n") sections)))))
    (when (> hidden-active 0)
      (push (mevedel-tool-task--summary-line
             "… %d more active" hidden-active)
            sections))
    (when completed-summary-reserved
      (push (mevedel-tool-task--summary-line
             "… %d completed" hidden-completed)
            sections))
    (if sections
        (string-join (nreverse sections) "\n")
      (propertize "No tasks." 'face 'font-lock-comment-face))))

(defun mevedel-tool-task--format-groups (session &optional show-completed
                                                 active-only max-lines)
  "Return a grouped task display string for SESSION.
When SHOW-COMPLETED is non-nil, include completed tasks after active
tasks.  When ACTIVE-ONLY is non-nil, omit groups with no active tasks.
MAX-LINES caps the rendered body without changing task storage."
  (let ((groups (mevedel-tool-task--group-tasks
                 (mevedel-session-tasks session)))
        sections)
    (if max-lines
        (mevedel-tool-task--format-groups-capped
         groups show-completed active-only max-lines)
      (dolist (group groups)
        (pcase-let* ((`(,owner . ,tasks) group)
                     (active-tasks
                      (mevedel-tool-task--sort-active-tasks
                       (cl-remove-if-not
                        #'mevedel-tool-task--active-p tasks)))
                     (completed-tasks
                      (and show-completed
                           (not active-only)
                           (mevedel-tool-task--sort-completed-tasks
                            (cl-remove-if-not
                             (lambda (task)
                               (eq (mevedel-task-status task)
                                   'completed))
                             tasks))))
                     (visible (append active-tasks completed-tasks))
                     (lines (list (mevedel-tool-task--group-header
                                   owner tasks))))
          (when (or (not active-only) active-tasks)
            (dolist (task visible)
              (push (concat "  " (mevedel-tool-task--format-one task))
                    lines))
            (push (string-join (nreverse lines) "\n") sections))))
      (if sections
          (string-join (nreverse sections) "\n")
        (propertize "No tasks." 'face 'font-lock-comment-face)))))

(defun mevedel-tool-task-format-active-groups-for-reminder (session)
  "Return current non-completed tasks in SESSION grouped for reminders."
  (mevedel-tool-task--format-groups session nil t))

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

(defconst mevedel-tool-task--overlay-min-body-lines 4
  "Minimum body lines reserved for the task overlay.")

(defconst mevedel-tool-task--overlay-max-body-lines 12
  "Maximum body lines reserved for the task overlay.")

(defun mevedel-tool-task--overlay-line-budget ()
  "Return the current task overlay body-line budget, or nil."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (let ((budget (floor (* (window-body-height window) 0.25))))
      (min mevedel-tool-task--overlay-max-body-lines
           (max mevedel-tool-task--overlay-min-body-lines budget)))))

(defvar mevedel-tool-task--overlay-keymap
  (define-keymap
    "<tab>" #'mevedel-toggle-tasks
    "TAB"   #'mevedel-toggle-tasks
    "<return>" #'mevedel-toggle-tasks
    "RET"   #'mevedel-toggle-tasks)
  "Keymap installed on the task-list overlay.
Shared with the overlay's header label via `where-is-internal' so the
displayed key matches the actual binding -- see
`mevedel-tool-task--toggle-key-label'.")

(defun mevedel-tool-task--toggle-key-label ()
  "Return the key-description string for toggling the task overlay.
Looks up `mevedel-toggle-tasks' directly in the overlay's keymap so
the label is correct regardless of where point is when the display
string is built.  Falls back to `M-x mevedel-toggle-tasks' if the
command has somehow lost its binding."
  (if-let* ((keys (where-is-internal 'mevedel-toggle-tasks
                                     mevedel-tool-task--overlay-keymap
                                     t)))
      (key-description keys)
    "M-x mevedel-toggle-tasks"))

(defun mevedel-toggle-tasks ()
  "Toggle whether the session task list shows completed tasks."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'mevedel-tool-task)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'mevedel-tool-task nil (point-min))
                    'mevedel-tool-task))))
    (if (null ov)
        (message "No task list overlay here")
      (overlay-put ov 'mevedel-tool-task--show-completed
                   (not (overlay-get ov
                                     'mevedel-tool-task--show-completed)))
      (if-let* ((refresh (overlay-get ov 'mevedel-tool-task--refresh)))
          (funcall refresh)
        (let ((display (if (overlay-get ov
                                        'mevedel-tool-task--show-completed)
                           (overlay-get ov
                                        'mevedel-tool-task--expanded-string)
                         (or (overlay-get ov
                                          'mevedel-tool-task--compact-string)
                             (and (stringp prop-value) prop-value)))))
          (if (= (overlay-start ov) (overlay-end ov))
              (overlay-put ov 'before-string display)
            (overlay-put ov 'after-string display)))))))

(defalias 'mevedel-toggle-todos #'mevedel-toggle-tasks)

(defun mevedel-tool-task--task-label ()
  "Return the status-zone task label."
  (let ((toggle-key
         (propertize (mevedel-tool-task--toggle-key-label)
                     'face 'help-key-binding)))
    (concat
     (propertize "tasks" 'face 'mevedel-view-zone-separator)
     (propertize " · " 'face 'mevedel-view-zone-separator)
     toggle-key
     (propertize " to toggle" 'face 'mevedel-view-zone-separator))))

(defun mevedel-tool-task--display-string (session show-completed view-p)
  "Return task display for SESSION.
SHOW-COMPLETED controls whether completed task detail is included.
VIEW-P means use view-buffer separator formatting."
  (let* ((body (mevedel-tool-task--format-groups
                session show-completed nil
                (mevedel-tool-task--overlay-line-budget)))
         (separator
          (if (and view-p (fboundp 'mevedel-view--zone-separator))
              (mevedel-view--zone-separator
               (mevedel-tool-task--task-label))
            mevedel-tool-task--hrule)))
    (concat separator
            body "\n"
            (if (and view-p (fboundp 'mevedel-view--zone-separator))
                ""
              mevedel-tool-task--hrule)
            "\n")))

(defun mevedel-tool-task--delete-materialized-region (session)
  "Delete SESSION's materialized task text when it exists."
  (let ((ov (mevedel-session-task-overlay session)))
    (when (and (overlayp ov)
               (overlay-buffer ov)
               (overlay-get ov 'mevedel-tool-task--materialized))
      (let ((start (overlay-start ov))
            (end (overlay-end ov))
            (buffer (overlay-buffer ov)))
        (delete-overlay ov)
        (setf (mevedel-session-task-overlay session) nil)
        (when (and buffer (buffer-live-p buffer)
                   start end (< start end))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (delete-region start end))))))))

(defun mevedel-tool-task--delete-overlay (session)
  "Delete SESSION's task overlay, including materialized view text."
  (let ((ov (mevedel-session-task-overlay session)))
    (when (overlayp ov)
      (if (and (overlay-buffer ov)
               (overlay-get ov 'mevedel-tool-task--materialized))
          (mevedel-tool-task--delete-materialized-region session)
        (when (overlay-buffer ov)
          (delete-overlay ov))
        (setf (mevedel-session-task-overlay session) nil)))))

(defun mevedel-tool-task--display-overlay ()
  "Display the current session's task list as an overlay.
When a view buffer exists, positions the overlay at the
status marker so the task block lives in the
dedicated status zone above the input prompt.  Otherwise falls
back to the tracking-marker region in the data buffer."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (info (and (boundp 'gptel--fsm-last)
                    gptel--fsm-last
                    (gptel-fsm-info gptel--fsm-last)))
         (marker (and info (plist-get info :tracking-marker)))
         (view-buf (and (boundp 'mevedel--view-buffer)
                        mevedel--view-buffer
                        (buffer-live-p mevedel--view-buffer)
                        mevedel--view-buffer)))
    (unless view-buf
      (when (and (boundp 'mevedel-view--status-marker)
                 (markerp mevedel-view--status-marker)
                 (eq (marker-buffer mevedel-view--status-marker)
                     (current-buffer)))
        (setq view-buf (current-buffer))))
    (cond
     ((and session
           (not (mevedel-tool-task--session-has-active-p session)))
      (mevedel-tool-task--delete-overlay session))
     ((and session view-buf)
      (with-current-buffer view-buf
        (when-let* ((anchor
                     (or (and (boundp 'mevedel-view--status-marker)
                              (markerp mevedel-view--status-marker)
                              (marker-position
                               mevedel-view--status-marker))
                         (and (boundp 'mevedel-view--input-marker)
                              (markerp mevedel-view--input-marker)
                              (marker-position
                               mevedel-view--input-marker)))))
          (let* ((old-ov (mevedel-session-task-overlay session))
                 (show-completed
                  (and (overlayp old-ov)
                       (overlay-get old-ov
                                    'mevedel-tool-task--show-completed))))
            (mevedel-tool-task--delete-materialized-region session)
            (setq anchor
                  (or (and (boundp 'mevedel-view--status-marker)
                           (markerp mevedel-view--status-marker)
                           (marker-position mevedel-view--status-marker))
                      anchor))
            (save-excursion
              (goto-char anchor)
              (let* ((start (point))
                     (display (mevedel-tool-task--display-string
                               session show-completed t))
                     (inhibit-read-only t)
                     (status-type
                      (and (markerp mevedel-view--status-marker)
                           (marker-insertion-type
                            mevedel-view--status-marker)))
                     (interaction-type
                      (and (markerp mevedel-view--interaction-marker)
                           (marker-insertion-type
                            mevedel-view--interaction-marker)))
                     (input-type
                      (and (markerp mevedel-view--input-marker)
                           (marker-insertion-type
                            mevedel-view--input-marker))))
                (unwind-protect
                    (progn
                      (when (markerp mevedel-view--status-marker)
                        (set-marker-insertion-type
                         mevedel-view--status-marker nil))
                      (when (markerp mevedel-view--interaction-marker)
                        (set-marker-insertion-type
                         mevedel-view--interaction-marker t))
                      (when (markerp mevedel-view--input-marker)
                        (set-marker-insertion-type
                         mevedel-view--input-marker t))
                      (add-text-properties
                       0 (length display)
                       '(mevedel-tool-task t
                         read-only t
                         front-sticky (read-only)
                         rear-nonsticky
                         (read-only font-lock-face))
                       display)
                      (insert display)
                      (let ((ov (make-overlay start (point)
                                              (current-buffer)
                                              t nil)))
                        (overlay-put ov 'mevedel-tool-task t)
                        (overlay-put ov
                                     'mevedel-tool-task--materialized
                                     t)
                        (overlay-put ov
                                     'mevedel-tool-task--show-completed
                                     show-completed)
                        (overlay-put ov 'priority 100)
                        (overlay-put ov 'keymap
                                     mevedel-tool-task--overlay-keymap)
                        (overlay-put
                         ov 'mevedel-tool-task--refresh
                         (lambda ()
                           (mevedel-tool-task--display-overlay)))
                        (overlay-put ov 'evaporate nil)
                        (setf (mevedel-session-task-overlay session)
                              ov)))
                  (when (markerp mevedel-view--status-marker)
                    (set-marker-insertion-type
                     mevedel-view--status-marker status-type))
                  (when (markerp mevedel-view--interaction-marker)
                    (set-marker-insertion-type
                     mevedel-view--interaction-marker interaction-type))
                  (when (markerp mevedel-view--input-marker)
                    (set-marker-insertion-type
                     mevedel-view--input-marker input-type)))))))))
     ((and session info marker)
      (let* ((where-to marker)
               (where-from (previous-single-property-change
                            where-to 'gptel nil (point-min))))
          (when (and where-from (not (= where-from where-to)))
            (let* ((old-ov (mevedel-session-task-overlay session))
                   (show-completed
                    (and (overlayp old-ov)
                         (overlay-get old-ov
                                      'mevedel-tool-task--show-completed)))
                   (ov old-ov))
              (unless (and (overlayp ov)
                           (eq (overlay-buffer ov) (current-buffer)))
                (when (and (overlayp ov) (overlay-buffer ov))
                  (delete-overlay ov))
                (setq ov (make-overlay where-from where-to nil t t))
                (overlay-put ov 'mevedel-tool-task t)
                (overlay-put ov 'evaporate nil)
                (overlay-put ov 'priority 100)
                (overlay-put ov 'keymap
                             mevedel-tool-task--overlay-keymap)
                (setf (mevedel-session-task-overlay session) ov))
              (move-overlay ov where-from where-to)
              (let ((expanded
                     (mevedel-tool-task--display-string session t nil))
                    (compact-display
                     (mevedel-tool-task--display-string session nil nil)))
                (add-text-properties 0 (length expanded)
                                     '(mevedel-tool-task t)
                                     expanded)
                (add-text-properties 0 (length compact-display)
                                     '(mevedel-tool-task t)
                                     compact-display)
                (overlay-put ov 'mevedel-tool-task--show-completed
                             show-completed)
                (overlay-put ov 'mevedel-tool-task--expanded-string
                             expanded)
                (overlay-put ov 'mevedel-tool-task--compact-string
                             compact-display)
                (overlay-put ov 'after-string
                             (if show-completed
                                 expanded
                               compact-display))))))))))


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
         (next-id (mevedel-tool-task--next-id session))
         (created nil))
    (dolist (spec specs)
      (push (mevedel-tool-task--create-one session spec next-id) created)
      (cl-incf next-id))
    (setq created (nreverse created))
    (setf (mevedel-session-tasks session)
          (append (mevedel-session-tasks session) created))
    (mevedel-tool-task--mark-write session)
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
      (mevedel-tool-task--mark-write session)
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
                  "Array of task objects. Each object has: subject (string, required), description (string, optional), status (\"pending\"|\"in_progress\"|\"completed\", optional), owner (string, optional), blockedBy (array of task IDs, optional), blocks (array of task IDs, optional), metadata (object, optional)."
                  :items (:type object)))
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
                   "Array of task IDs this task blocks."
                   :items (:type integer))
           (blockedBy array :optional
                      "Array of task IDs blocking this task."
                      :items (:type integer))
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
