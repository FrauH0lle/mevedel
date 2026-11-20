;;; mevedel-workspace-schedule.el --- Workspace schedules and their runs -*- lexical-binding: t -*-

;;; Commentary:

;; A workspace schedule is a schedule whose target is the workspace: each run
;; starts a new session with the schedule's prompt, base preset and
;; implementation policy.  The workspace state is the source of truth:
;;
;;   .mevedel/schedules/ID/schedule.el        the schedule
;;   .mevedel/schedules/ID/runs/SECONDS.el    one record per run
;;   .mevedel/schedules/ID/notify-receipts.json
;;
;; A run is keyed by its schedule and scheduled time.  Every change runs as
;; one target program under a lock on `.mevedel/schedules/', proving the bytes
;; it read before writing, so Emacs instances sharing a workspace never lose
;; each other's updates.  A firing exclusively creates its run record in the
;; same program that moves the schedule to its next time: exactly one Emacs
;; fires each run, and it is recorded before its session starts, so a crash
;; cannot repeat it.  There is no holder that could go stale.
;;
;; `mevedel-schedule' owns the one timer; it asks this module to fire a
;; workspace when its earliest active schedule is due.  An index under
;; `mevedel-user-dir' names the workspaces with active schedules, so they
;; are restored at startup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-implementation-policy)
(require 'mevedel-schedule)
(require 'mevedel-session-control-fs)
(require 'mevedel-structs)
(require 'mevedel-workspace)

;; `mevedel-chat'
(declare-function mevedel--chat-buffer "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--submit-generated-turn "mevedel-chat"
                  (prompt &optional display-text prompt-submission prepared-outcome trigger))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--submit-generated-turn "mevedel-chat")
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-set-session-effort "mevedel-models"
                  (session effort &optional buffer))
(declare-function mevedel-model-set-session-provider "mevedel-models"
                  (session provider &optional buffer))
(autoload 'mevedel-model-set-session-effort "mevedel-models")
(autoload 'mevedel-model-set-session-provider "mevedel-models")

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-transition "mevedel-permission-mode" (mode))
(autoload 'mevedel-permission-mode-transition "mevedel-permission-mode")

;; `mevedel-presets'
(declare-function mevedel-preset-apply "mevedel-presets" (name &optional buffer))
(defvar mevedel-preset--registry)
(autoload 'mevedel-preset-apply "mevedel-presets")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts"
                  (session buffer &optional settled force token))
(declare-function mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts"
                  (workspace))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts")

;; `mevedel-session-durability'
(autoload 'mevedel-session-durability--holder "mevedel-session-durability")

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-turn-busy-p "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(declare-function mevedel-diagnostic-warning "mevedel-utilities"
                  (scope type message &optional level buffer-name))

(defconst mevedel-workspace-schedule-max 50
  "Most active or paused workspace schedules one workspace may hold.")

(defconst mevedel-workspace-schedule-max-name-length 80
  "Most characters a workspace schedule's name may contain.")

(defconst mevedel-workspace-schedule-statuses '(active paused finished expired)
  "Statuses of a workspace schedule.  Only an `active' one fires.")

(defconst mevedel-workspace-schedule-run-statuses
  '(claimed launched skipped expired setup-error)
  "Statuses this module records for a run.")

(define-error 'mevedel-workspace-schedule-missing
  "No such workspace schedule record" 'user-error)

(autoload 'mevedel-workspace-schedule-run-fired "mevedel-workspace-schedule-runs")
(autoload 'mevedel-workspace-schedule-run-settled "mevedel-workspace-schedule-runs")
(autoload 'mevedel-workspace-schedule-reconcile-run "mevedel-workspace-schedule-runs")
(autoload 'mevedel-workspace-schedule-run-blocks-p "mevedel-workspace-schedule-runs")

(defvar mevedel-workspace-schedule-fired-functions '(mevedel-workspace-schedule-run-fired)
  "Abnormal hook run once a firing has recorded its run.
Each function receives the workspace and the run record, whatever its
status: `launched', `skipped', `expired' or `setup-error'.")

(defvar mevedel-workspace-schedule-run-settled-functions nil
  "Abnormal hook run when a run's first root request settles.
Each function receives the run, as in `mevedel-workspace-schedule-run', and
the terminal outcome, such as `success', `error' or `abort'.  It runs in the
run session's data buffer, for native and external engines alike.")

(defvar mevedel-workspace-schedule-changed-functions nil
  "Abnormal hook run after a committed change to a workspace's schedules.
Each function receives the workspace.  Creating, changing, deleting and
firing a schedule and updating a run record all run it.")

(defvar-local mevedel-workspace-schedule-run nil
  "The workspace schedule run this root data buffer was started for, or nil.
A plist of `:workspace' (its root), `:schedule-id', `:scheduled-at' and
`:session-id'.  It is not persisted: a reopened run session is ordinary.")
(put 'mevedel-workspace-schedule-run 'permanent-local t)

(defvar-local mevedel-workspace-schedule--run-active nil
  "Non-nil until this run session's first root request settles.")
(put 'mevedel-workspace-schedule--run-active 'permanent-local t)


;;
;;; Store

(defun mevedel-workspace-schedule--path (workspace &rest names)
  "Return the file NAMES name under WORKSPACE's schedule store."
  (apply #'file-name-concat (mevedel-workspace-state-dir workspace) "schedules" names))

(defun mevedel-workspace-schedule-id-p (id)
  "Return non-nil when ID is a canonical workspace schedule id, s-xxxxxx.
The x are lowercase hexadecimal digits whatever `case-fold-search' says."
  (and (stringp id)
       (let ((case-fold-search nil))
         (string-match-p "\\`s-[0-9a-f]\\{6\\}\\'" id))))

(defun mevedel-workspace-schedule--id (id)
  "Return ID when it is a canonical workspace schedule id, else signal.
Ids name store directories, so nothing else may reach a store path."
  (if (mevedel-workspace-schedule-id-p id)
      id
    (user-error "Invalid workspace schedule id: %S" id)))

(defun mevedel-workspace-schedule--run-path (workspace id scheduled-at)
  "Return the record of WORKSPACE's schedule ID run due at SCHEDULED-AT."
  (mevedel-workspace-schedule--path
   workspace (mevedel-workspace-schedule--id id) "runs"
   (format "%d.el" scheduled-at)))

(defun mevedel-workspace-schedule--bytes (value)
  "Return VALUE printed in full, as stored."
  (let ((print-length nil) (print-level nil))
    (prin1-to-string value)))

(defun mevedel-workspace-schedule--parse (bytes)
  "Return the value BYTES print, or nil when they do not read."
  (ignore-errors (car (read-from-string bytes))))

(defun mevedel-workspace-schedule--read (path)
  "Return (VALUE . BYTES) of store file PATH, or nil when it is absent."
  (condition-case nil
      (let ((bytes (mevedel-session-control-fs-read-file path)))
        (cons (mevedel-workspace-schedule--parse bytes) bytes))
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-workspace-schedule--commit (workspace operations)
  "Run store OPERATIONS under WORKSPACE's schedule lock.
Return nil when every required operation succeeded, else the status of the
first that did not: `mismatch' or `absent' for a failed proof, `conflict'
for an exclusive creation another writer won.  Other failures signal."
  (let ((results (mevedel-session-control-fs-run-program
                  operations (mevedel-workspace-schedule--path workspace))))
    (cl-loop for operation in operations
             for result in results
             for status = (plist-get result :status)
             unless (or (eq status 'ok) (plist-get operation :optional))
             return (if (memq status '(mismatch absent conflict))
                        status
                      (mevedel-session-control-fs-program-value result)))))

(defun mevedel-workspace-schedule-entry-p (entry)
  "Return non-nil when ENTRY is a well-formed stored workspace schedule.
Keys this module does not own are allowed."
  (and (proper-list-p entry)
       (cl-evenp (length entry))
       (mevedel-workspace-schedule-id-p (plist-get entry :id))
       (stringp (plist-get entry :name))
       (stringp (plist-get entry :prompt))
       (integerp (plist-get entry :created-at))
       (mevedel-schedule-timing-p (plist-get entry :timing) (plist-get entry :created-at))
       (memq (plist-get entry :status) mevedel-workspace-schedule-statuses)
       (let ((due (plist-get entry :due)))
         (if (eq 'active (plist-get entry :status)) (integerp due) (or (null due) (integerp due))))
       (let ((expires (plist-get entry :expires-at)))
         (or (null expires) (integerp expires)))
       (let ((preset (plist-get entry :preset)))
         (and preset (symbolp preset)))
       (plist-get entry :policy)
       (ignore-errors (mevedel-implementation-policy-validate (plist-get entry :policy)))
       (eq 'skip (plist-get entry :overlap))))

(defun mevedel-workspace-schedule-run-p (run)
  "Return non-nil when RUN is a well-formed stored run record.
Statuses and keys added by other modules are allowed."
  (and (proper-list-p run)
       (cl-evenp (length run))
       (stringp (plist-get run :schedule-id))
       (integerp (plist-get run :scheduled-at))
       (integerp (plist-get run :fired-at))
       (plist-get run :status)
       (symbolp (plist-get run :status))
       (let ((session-id (plist-get run :session-id)))
         (or (null session-id) (stringp session-id)))))

(defun mevedel-workspace-schedule-list (workspace)
  "Return WORKSPACE's workspace schedules, oldest first."
  (let (schedules)
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (mevedel-workspace-schedule--path workspace)
                        "\\`s-[0-9a-f]\\{6\\}\\'"))
      (when-let* ((entry (car (mevedel-workspace-schedule--read
                               (file-name-concat directory "schedule.el"))))
                  ((mevedel-workspace-schedule-entry-p entry))
                  ((equal (plist-get entry :id) (file-name-nondirectory directory))))
        (push entry schedules)))
    (sort schedules (lambda (a b) (< (plist-get a :created-at) (plist-get b :created-at))))))

(defun mevedel-workspace-schedule-get (workspace id)
  "Return WORKSPACE's workspace schedule ID, or nil.
Signal for an ID that is not a canonical schedule id."
  (let ((entry (car (mevedel-workspace-schedule--read
                     (mevedel-workspace-schedule--path
                      workspace (mevedel-workspace-schedule--id id) "schedule.el")))))
    (and (mevedel-workspace-schedule-entry-p entry)
         (equal id (plist-get entry :id))
         entry)))

(defun mevedel-workspace-schedule-runs (workspace id)
  "Return the run records of WORKSPACE's schedule ID, oldest first."
  (let* ((paths (mevedel-session-control-fs-list-directory
                 (mevedel-workspace-schedule--path
                  workspace (mevedel-workspace-schedule--id id) "runs")
                 "\\`[0-9]+\\.el\\'"))
         (results (and paths
                       (mevedel-session-control-fs-run-program
                        (mapcar (lambda (path) (list :op 'read :path path :optional t))
                                paths))))
         runs)
    (dolist (result results)
      (when-let* (((eq 'ok (plist-get result :status)))
                  (run (mevedel-workspace-schedule--parse
                        (mevedel-session-control-fs-program-value result)))
                  ((mevedel-workspace-schedule-run-p run)))
        (push run runs)))
    (sort runs (lambda (a b) (< (plist-get a :scheduled-at) (plist-get b :scheduled-at))))))

(defun mevedel-workspace-schedule--update-file (workspace path valid-p function)
  "Replace store file PATH of WORKSPACE with FUNCTION's result.
FUNCTION receives a copy of the current value and returns the new one; an
`equal' result writes nothing.  VALID-P must accept the result.  A value
changed meanwhile by another writer is read again and FUNCTION called again.
Return the committed value."
  (cl-loop
   repeat 5
   do (pcase-let* ((`(,value . ,bytes)
                    (or (mevedel-workspace-schedule--read path)
                        (signal 'mevedel-workspace-schedule-missing (list path))))
                   (new (funcall function (copy-tree value))))
        (cond
         ((equal new value) (cl-return value))
         ((not (funcall valid-p new)) (error "Invalid schedule record: %S" new))
         ((not (mevedel-workspace-schedule--commit
                workspace
                (list (list :op 'verify :path path :content bytes)
                      (list :op 'write :path path
                            :content (mevedel-workspace-schedule--bytes new)))))
          (mevedel-workspace-schedule--refresh workspace)
          (cl-return new))))
   finally (error "Schedule record %s keeps changing; try again" path)))

(defun mevedel-workspace-schedule-update (workspace id function)
  "Replace WORKSPACE's schedule ID with FUNCTION's result and return it.
FUNCTION receives the current schedule and returns the new one, and may be
called again when another writer changed it meanwhile.  The scheduler and
index follow the committed schedule; no session is needed."
  (mevedel-workspace-schedule--update-file
   workspace (mevedel-workspace-schedule--path
              workspace (mevedel-workspace-schedule--id id) "schedule.el")
   (lambda (entry)
     (and (mevedel-workspace-schedule-entry-p entry) (equal id (plist-get entry :id))))
   function))

(defun mevedel-workspace-schedule-update-run (workspace id scheduled-at function)
  "Replace the record of WORKSPACE's schedule ID run due at SCHEDULED-AT.
FUNCTION receives the current record and returns the new one, as for
`mevedel-workspace-schedule-update'.  Return the committed record."
  (mevedel-workspace-schedule--update-file
   workspace (mevedel-workspace-schedule--run-path workspace id scheduled-at)
   #'mevedel-workspace-schedule-run-p function))

(defun mevedel-workspace-schedule-delete (workspace id)
  "Delete WORKSPACE's schedule ID with its run records."
  (unless (mevedel-workspace-schedule-get workspace id)
    (user-error "No workspace schedule with id %s" id))
  (when (mevedel-workspace-schedule--commit
         workspace (list (list :op 'delete-directory
                               :path (mevedel-workspace-schedule--path workspace id))))
    (user-error "No workspace schedule with id %s" id))
  (mevedel-workspace-schedule--refresh workspace))


;;
;;; Proposals

(defun mevedel-workspace-schedule--new-id (workspace)
  "Return a workspace schedule identity unused in WORKSPACE."
  (let (id)
    (while (or (null id) (mevedel-workspace-schedule-get workspace id))
      (setq id (format "s-%06x" (random #x1000000))))
    id))

(defun mevedel-workspace-schedule-propose (args &optional previous)
  "Return the schedule ARGS describe, without storing it, or signal `user-error'.
ARGS is a plist of `:name', `:prompt', `:when' and `:expires' (timing
texts), `:preset' (a symbol, default `mevedel-implement') and `:policy' (an
implementation policy).  For a change, PREVIOUS is the stored schedule and
supplies whatever ARGS omits; it keeps its timing and next run unless ARGS
gives `:when'.  Workspace schedules have no default expiry."
  (let* ((now (mevedel-schedule-now))
         (field (lambda (key) (or (plist-get args key) (plist-get previous key))))
         (name (string-trim (or (funcall field :name) "")))
         (prompt (string-trim (or (funcall field :prompt) "")))
         (when-text (plist-get args :when))
         (timing (if when-text
                     (mevedel-schedule-parse-timing when-text now)
                   (plist-get previous :timing)))
         (due (if when-text
                  (mevedel-schedule-next-run timing now)
                (plist-get previous :due)))
         (expires (plist-get args :expires))
         (expires-at (if expires
                         (or (plist-get (mevedel-schedule-parse-timing expires now) :at)
                             (user-error "Schedule expiry must be a duration or local time: %s"
                                         expires))
                       (plist-get previous :expires-at)))
         (preset (or (funcall field :preset) 'mevedel-implement))
         (policy (funcall field :policy))
         (status (cond ((null previous) 'active)
                       ((eq 'paused (plist-get previous :status)) 'paused)
                       (t 'active))))
    (when (string-empty-p name)
      (user-error "A workspace schedule needs a name"))
    (when (or (> (length name) mevedel-workspace-schedule-max-name-length)
              (string-match-p "[[:cntrl:]]" name))
      (user-error "Schedule name must be one line of at most %d characters"
                  mevedel-workspace-schedule-max-name-length))
    (when (string-empty-p prompt)
      (user-error "A workspace schedule needs a prompt"))
    (when (> (length prompt) mevedel-schedule-max-note-length)
      (user-error "Schedule prompt exceeds %d characters" mevedel-schedule-max-note-length))
    (unless timing
      (user-error "A workspace schedule needs `when'"))
    (unless (and due (> due now))
      (user-error "This schedule has no future run; give a new `when'"))
    (when (and expires-at (<= expires-at due))
      (user-error "Schedule expiry %s is not after its next run %s"
                  (mevedel-schedule-format-time expires-at)
                  (mevedel-schedule-format-time due)))
    (unless (and (symbolp preset) (assq preset (bound-and-true-p mevedel-preset--registry)))
      (user-error "Unknown preset %s" preset))
    (unless (and policy (ignore-errors (mevedel-implementation-policy-validate policy)))
      (user-error "A workspace schedule needs a model and permission mode"))
    (append (list :id (or (plist-get previous :id) (format "s-%06x" (random #x1000000)))
                  :name name
                  :prompt prompt
                  :timing timing
                  :created-at (or (plist-get previous :created-at) now)
                  :due due
                  :expires-at expires-at
                  :preset preset
                  :policy (copy-tree policy)
                  :overlap 'skip
                  :status status)
            ;; Keys owned by other modules survive a change.
            (cl-loop for (key value) on previous by #'cddr
                     unless (memq key '(:id :name :prompt :timing :created-at :due
                                            :expires-at :preset :policy :overlap :status))
                     append (list key value)))))

(defun mevedel-workspace-schedule-create (workspace entry)
  "Store proposed schedule ENTRY in WORKSPACE and return the stored entry.
ENTRY comes from `mevedel-workspace-schedule-propose'; the caller has had it
approved.  A workspace holds at most `mevedel-workspace-schedule-max'
active or paused schedules."
  (when (>= (cl-count-if (lambda (schedule)
                           (memq (plist-get schedule :status) '(active paused)))
                         (mevedel-workspace-schedule-list workspace))
            mevedel-workspace-schedule-max)
    (user-error "This workspace already has %d workspace schedules; delete one first"
                mevedel-workspace-schedule-max))
  (let ((entry (if (mevedel-workspace-schedule-get workspace (plist-get entry :id))
                   (plist-put (copy-sequence entry) :id
                              (mevedel-workspace-schedule--new-id workspace))
                 entry)))
    (unless (mevedel-workspace-schedule-entry-p entry)
      (error "Invalid workspace schedule: %S" entry))
    (mevedel-session-control-fs-make-directory
     (mevedel-workspace-schedule--path workspace (plist-get entry :id) "runs") t)
    (when (mevedel-workspace-schedule--commit
           workspace
           (list (list :op 'create
                       :path (mevedel-workspace-schedule--path
                              workspace (plist-get entry :id) "schedule.el")
                       :content (mevedel-workspace-schedule--bytes entry))))
      (error "Workspace schedule %s already exists" (plist-get entry :id)))
    (mevedel-workspace-schedule--refresh workspace)
    entry))


;;
;;; Scheduling

(defun mevedel-workspace-schedule--root (workspace)
  "Return WORKSPACE's root in the one spelling runs and the index store."
  (mevedel-schedule--directory-key (mevedel-workspace-root workspace)))

(defun mevedel-workspace-schedule-workspace (root)
  "Return the workspace whose root is ROOT."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory root))
    (or (mevedel-workspace) (error "No mevedel workspace at %s" root))))

(defun mevedel-workspace-schedule--index-file (root)
  "Return the index file naming workspace ROOT."
  (file-name-concat (mevedel-schedule-workspace-index-directory)
                    (concat (md5 root) ".el")))

(defun mevedel-workspace-schedule--index (root)
  "Name workspace ROOT in the index unless it already is."
  (let ((file (mevedel-workspace-schedule--index-file root)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file (prin1 root (current-buffer))))))

(defun mevedel-workspace-schedule--refresh (workspace)
  "Schedule WORKSPACE's earliest active schedule and index the workspace.
Return its due time, or nil when nothing is active.  Only startup restore
removes index entries: two Emacs instances sharing `mevedel-user-dir' could
otherwise drop each other's entry."
  (let* ((root (mevedel-workspace-schedule--root workspace))
         (dues (cl-loop for schedule in (mevedel-workspace-schedule-list workspace)
                        when (eq 'active (plist-get schedule :status))
                        collect (plist-get schedule :due)))
         (due (and dues (apply #'min dues))))
    (setq mevedel-schedule--workspaces
          (cl-remove root mevedel-schedule--workspaces :key #'car :test #'equal))
    (when due
      (push (cons root due) mevedel-schedule--workspaces))
    (condition-case err
        (when due (mevedel-workspace-schedule--index root))
      (error
       (mevedel-diagnostic-warning
        root 'mevedel
        (format "Workspace schedule index for %s could not be updated: %s"
                root (error-message-string err))
        :warning)))
    (mevedel-schedule--rearm)
    (run-hook-with-args 'mevedel-workspace-schedule-changed-functions workspace)
    due))

(defun mevedel-workspace-schedule--restore-workspace (workspace)
  "Reconcile WORKSPACE's runs and timer.  Return non-nil if still indexed."
  (let (pending)
    (dolist (job (mevedel-workspace-schedule-list workspace))
      (dolist (run (mevedel-workspace-schedule-runs workspace (plist-get job :id)))
        (unless (or (plist-get run :outcome)
                    (mevedel-workspace-schedule-reconcile-run workspace run))
          (setq pending t))))
    (or (mevedel-workspace-schedule--refresh workspace)
        (when pending
          (mevedel-workspace-schedule--index (mevedel-workspace-schedule--root workspace))
          t))))

(defun mevedel-workspace-schedule--prune (workspace file)
  "Delete WORKSPACE's index FILE after its store read as inactive.
The store is read again afterwards, as `mevedel-schedule--prune' does for
wake-ups: a schedule another Emacs committed in between is indexed again,
and a re-read that signals puts the entry back before the error propagates."
  (delete-file file)
  (let (confirmed)
    (unwind-protect
        (progn (mevedel-workspace-schedule--restore-workspace workspace)
               (setq confirmed t))
      (unless confirmed
        (mevedel-workspace-schedule--index (mevedel-workspace-schedule--root workspace))))))

;;;###autoload
(defun mevedel-workspace-schedule-restore ()
  "Schedule the active workspace schedules of every indexed workspace.
Run at startup.  Only entries proven stale are removed: malformed ones,
and those whose workspace is provably gone or whose readable store holds
no active schedule.  An entry that cannot be read now is kept."
  (let ((inhibit-interaction t))
    (dolist (file (directory-files (mevedel-schedule-workspace-index-directory) t "\\.el\\'"))
      (let ((root (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))))
        (condition-case err
            (cond
             ((not (stringp root)) (delete-file file))
             ((mevedel-schedule--absent-p root) (delete-file file))
             ((not (file-accessible-directory-p root))
              (error "%s is not reachable" root))
             (t (let ((workspace (mevedel-workspace-schedule-workspace root)))
                  (unless (mevedel-workspace-schedule--restore-workspace workspace)
                    (mevedel-workspace-schedule--prune workspace file)))))
          (error
           (mevedel-diagnostic-warning
            'global 'mevedel
            (format "Workspace schedules of %s were not restored: %s"
                    root (error-message-string err))
            :warning)))))))

(defun mevedel-workspace-schedule-run-due (root now)
  "Fire workspace ROOT's schedules due by clock seconds NOW."
  (condition-case err
      (let ((workspace (mevedel-workspace-schedule-workspace root)))
        (dolist (schedule (mevedel-workspace-schedule-list workspace))
          (when (and (eq 'active (plist-get schedule :status))
                     (<= (plist-get schedule :due) now))
            (condition-case err
                (mevedel-workspace-schedule--fire workspace (plist-get schedule :id) now)
              (error
               (mevedel-diagnostic-warning
                root 'mevedel
                (format "Workspace schedule %s did not fire: %s"
                        (plist-get schedule :name) (error-message-string err))
                :warning)))))
        (unless (mevedel-workspace-schedule--refresh workspace)
          (mevedel-schedule--rearm)))
    (error
     (push (cons root (+ now mevedel-schedule-retry-delay)) mevedel-schedule--workspaces)
     (mevedel-diagnostic-warning
      root 'mevedel
      (format "Workspace schedules of %s could not be read (%s); retrying in %ds"
              root (error-message-string err) mevedel-schedule-retry-delay)
      :warning))))


;;
;;; Runs

(defun mevedel-workspace-schedule--previous-busy-p (workspace id due)
  "Return non-nil when the latest run of WORKSPACE's schedule ID still works.
Check an open session locally; otherwise use its lease or its claim at DUE."
  (when-let* ((run (cl-find-if (lambda (run) (or (plist-get run :session-id)
                                                 (eq 'claimed (plist-get run :status))))
                               (mevedel-workspace-schedule-runs workspace id)
                               :from-end t)))
    (if-let* ((session-id (plist-get run :session-id))
              (open (cl-find-if
                     (lambda (entry)
                       (equal session-id (mevedel-session-session-id
                                          (buffer-local-value 'mevedel--session (cdr entry)))))
                     (mevedel--workspace-sessions workspace))))
        (or (buffer-local-value 'mevedel-workspace-schedule--run-active (cdr open))
            (mevedel-turn-busy-p (cdr open)))
      (mevedel-workspace-schedule-run-blocks-p workspace run due))))

(defun mevedel-workspace-schedule--fire (workspace id now)
  "Fire WORKSPACE's schedule ID, due by clock seconds NOW, at most once.
Return the recorded run, or nil when there was nothing to fire or another
Emacs fired it."
  (cl-loop
   repeat 5
   do (pcase-let* ((path (mevedel-workspace-schedule--path workspace id "schedule.el"))
                   (`(,schedule . ,bytes) (mevedel-workspace-schedule--read path)))
        (unless (and (mevedel-workspace-schedule-entry-p schedule)
                     (eq 'active (plist-get schedule :status))
                     (<= (plist-get schedule :due) now))
          (cl-return nil))
        (let* ((due (plist-get schedule :due))
               (expires (plist-get schedule :expires-at))
               (late (and expires (> now expires)))
               (next (and (not late) (mevedel-schedule-next-run (plist-get schedule :timing) now)))
               (status (cond (late 'expired)
                             ((null next) 'finished)
                             ((and expires (> next expires)) 'expired)
                             (t 'active)))
               (updated (plist-put (plist-put (plist-put (copy-sequence schedule)
                                                         :status status)
                                              :due (and (eq status 'active) next))
                                   :last-scheduled-at due))
               (run (list :schedule-id id :scheduled-at due :fired-at now
                          :status (cond (late 'expired)
                                        ((mevedel-workspace-schedule--previous-busy-p workspace id due)
                                         'skipped)
                                        (t 'claimed)))))
          (when (eq 'claimed (plist-get run :status))
            (setf (plist-get run :holder) (mevedel-session-durability--holder)))
          (pcase (mevedel-workspace-schedule--commit
                  workspace
                  (list (list :op 'verify :path path :content bytes)
                        (list :op 'create
                              :path (mevedel-workspace-schedule--run-path workspace id due)
                              :content (mevedel-workspace-schedule--bytes run))
                        (list :op 'write :path path
                              :content (mevedel-workspace-schedule--bytes updated))))
            ('mismatch nil)
            ((or 'absent 'conflict) (cl-return nil))
            (_
             (when (eq 'claimed (plist-get run :status))
               (setq run (mevedel-workspace-schedule--launch workspace updated run)))
             (run-hook-with-args 'mevedel-workspace-schedule-fired-functions workspace run)
             (cl-return run)))))
   finally (error "Schedule %s keeps changing while it fires" id)))

(defun mevedel-workspace-schedule--record (workspace run &rest keys)
  "Store KEYS into claimed RUN of WORKSPACE and return the committed record."
  (mevedel-workspace-schedule-update-run
   workspace (plist-get run :schedule-id) (plist-get run :scheduled-at)
   (lambda (current)
     (unless (and (plist-member keys :status) (plist-get current :outcome))
       (cl-loop for (key value) on keys by #'cddr
                do (setq current (plist-put current key value))))
     current)))

(defun mevedel-workspace-schedule--prompt (schedule run)
  "Return the first message of SCHEDULE's RUN, noting a late start."
  (let ((late (- (plist-get run :fired-at) (plist-get run :scheduled-at))))
    (if (< late mevedel-schedule-overdue-threshold)
        (plist-get schedule :prompt)
      (format "%s\n\n(Scheduled for %s; started overdue by %s.)"
              (plist-get schedule :prompt)
              (mevedel-schedule-format-time (plist-get run :scheduled-at))
              (mevedel--duration-label late)))))

(cl-defun mevedel-workspace-schedule--launch (workspace schedule run)
  "Start claimed RUN of SCHEDULE in a new session of WORKSPACE.
The session's own preset, model, effort and permission mode start as the
approved ones, so every turn in it runs under them until someone changes
them there.  It is saved before its first message is submitted, without a
window.  Return the committed run record, `setup-error' on failure."
  (let ((inhibit-interaction t)
        (root (mevedel-workspace-schedule--root workspace))
        (name (format "%s %s" (plist-get schedule :name)
                      (format-time-string "%F %R" (plist-get run :scheduled-at))))
        buffer)
    (condition-case err
        (progn
          (let ((current (car (mevedel-workspace-schedule--read
                               (mevedel-workspace-schedule--run-path
                                workspace (plist-get run :schedule-id) (plist-get run :scheduled-at))))))
            (unless (and (eq 'claimed (plist-get current :status)) (not (plist-get current :outcome)))
              (cl-return-from mevedel-workspace-schedule--launch current)))
          ;; An open session with this name, as from a same-named schedule
          ;; due at the same time, must not receive this run.
          (let ((base name) (count 1))
            (while (assoc name (mevedel--workspace-sessions workspace))
              (setq name (format "%s (%d)" base (cl-incf count)))))
          (setq buffer (mevedel--chat-buffer name t workspace (mevedel-workspace-root workspace)))
          (with-current-buffer buffer
            (let ((session mevedel--session)
                  (policy (plist-get schedule :policy)))
              (mevedel-preset-apply (plist-get schedule :preset) buffer)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider (plist-get policy :model-provider))
               buffer)
              (mevedel-model-set-session-effort
               session (plist-get policy :reasoning-effort) buffer)
              (mevedel-permission-mode-transition (plist-get policy :permission-mode))
              (mevedel-session-artifacts-save session buffer nil t)
              (setq run (mevedel-workspace-schedule--record
                         workspace run :status 'launched
                         :session-id (mevedel-session-session-id session)))
              (when (plist-get run :outcome)
                (cl-return-from mevedel-workspace-schedule--launch run))
              (setq-local mevedel-workspace-schedule-run
                          (list :workspace root :schedule-id (plist-get run :schedule-id)
                                :scheduled-at (plist-get run :scheduled-at)
                                :session-id (plist-get run :session-id))
                          mevedel-workspace-schedule--run-active t)
              (mevedel--submit-generated-turn
               (mevedel-workspace-schedule--prompt schedule run)
               nil nil nil (list :turn-trigger 'workspace-schedule
                                 :workspace-schedule-run mevedel-workspace-schedule-run))))
          run)
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq-local mevedel-workspace-schedule--run-active nil)))
       (mevedel-workspace-schedule--record
        workspace run :status 'setup-error :error (error-message-string err))))))

(defun mevedel-workspace-schedule--turn-settled (outcome)
  "Record and report the current run session's first settled root request.
OUTCOME is its terminal outcome.  The record's `:settled-at' tells another
Emacs that the run no longer blocks the next one."
  (when (and mevedel-workspace-schedule--run-active mevedel-workspace-schedule-run)
    (setq-local mevedel-workspace-schedule--run-active nil)
    (mevedel-workspace-schedule-run-settled mevedel-workspace-schedule-run outcome)
    (run-hook-with-args 'mevedel-workspace-schedule-run-settled-functions
                        mevedel-workspace-schedule-run outcome)))

(add-hook 'mevedel-turn-settled-functions #'mevedel-workspace-schedule--turn-settled)

(defun mevedel-workspace-schedule-notify-receipt-file (session)
  "Return the Notify receipt file for SESSION's active run, or nil.
Until its first root request settles, a run session's notices share one
receipt file per schedule, so a repeated notice from a later run is dropped."
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer))
              ((buffer-local-value 'mevedel-workspace-schedule--run-active buffer))
              (run (buffer-local-value 'mevedel-workspace-schedule-run buffer)))
    (mevedel-workspace-schedule--path (mevedel-session-workspace session)
                                      (plist-get run :schedule-id)
                                      "notify-receipts.json")))

(defun mevedel-workspace-schedule-record-notice ()
  "Mark the current root's active run as having emitted a Notify notice."
  (when (and mevedel-workspace-schedule--run-active mevedel-workspace-schedule-run)
    (condition-case err
        (mevedel-workspace-schedule--record
         (mevedel-session-workspace mevedel--session) mevedel-workspace-schedule-run :notified t)
      (error (mevedel-diagnostic-warning
              (mevedel-diagnostic-scope mevedel--session) 'mevedel
              (format "Could not record the run's delivered notice: %s" (error-message-string err))
              :warning)))))

(provide 'mevedel-workspace-schedule)
;;; mevedel-workspace-schedule.el ends here
