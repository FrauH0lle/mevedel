;;; niwa-workspace-schedule.el --- Workspace schedules and their runs -*- lexical-binding: t -*-

;;; Commentary:

;; A workspace schedule is a schedule whose target is the workspace: each run
;; starts a new session with the schedule's prompt, base preset and
;; implementation policy.  The workspace state is the source of truth:
;;
;;   .niwa/schedules/ID/schedule.el        the schedule
;;   .niwa/schedules/ID/runs/SECONDS.el    one record per run
;;   .niwa/schedules/ID/notify-receipts.json
;;
;; A run is keyed by its schedule and scheduled time.  Every change runs as
;; one target program under a lock on `.niwa/schedules/', proving the bytes
;; it read before writing, so Emacs instances sharing a workspace never lose
;; each other's updates.  A firing exclusively creates its run record in the
;; same program that moves the schedule to its next time: exactly one Emacs
;; fires each run, and it is recorded before its session starts, so a crash
;; cannot repeat it.  There is no holder that could go stale.
;;
;; `niwa-schedule' owns the one timer; it asks this module to fire a
;; workspace when its earliest active schedule is due.  An index under
;; `niwa-user-dir' names the workspaces with active schedules, so they
;; are restored at startup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-implementation-policy)
(require 'niwa-schedule)
(require 'niwa-session-control-fs)
(require 'niwa-structs)
(require 'niwa-workspace)

;; `niwa-chat'
(declare-function niwa--chat-buffer "niwa-chat"
                  (session-name &optional create workspace working-directory))
(declare-function niwa--submit-generated-turn "niwa-chat"
                  (prompt &optional display-text prompt-submission prepared-outcome trigger))
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--submit-generated-turn "niwa-chat")
(autoload 'niwa--workspace-sessions "niwa-chat")

;; `niwa-models'
(declare-function niwa-model-resolve-provider "niwa-models" (spec &optional noerror))
(declare-function niwa-model-set-session-effort "niwa-models"
                  (session effort &optional buffer))
(declare-function niwa-model-set-session-provider "niwa-models"
                  (session provider &optional buffer))
(autoload 'niwa-model-set-session-effort "niwa-models")
(autoload 'niwa-model-set-session-provider "niwa-models")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-transition "niwa-permission-mode" (mode))
(autoload 'niwa-permission-mode-transition "niwa-permission-mode")

;; `niwa-presets'
(declare-function niwa-preset-apply "niwa-presets" (name &optional buffer))
(defvar niwa-preset--registry)
(autoload 'niwa-preset-apply "niwa-presets")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save "niwa-session-artifacts"
                  (session buffer &optional settled force token))
(declare-function niwa-session-artifacts-sessions-dir "niwa-session-artifacts"
                  (workspace))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-sessions-dir "niwa-session-artifacts")

;; `niwa-session-durability'
(autoload 'niwa-session-durability--holder "niwa-session-durability")

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(declare-function niwa-diagnostic-warning "niwa-utilities"
                  (scope type message &optional level buffer-name))

(defconst niwa-workspace-schedule-max 50
  "Most active or paused workspace schedules one workspace may hold.")

(defconst niwa-workspace-schedule-max-name-length 80
  "Most characters a workspace schedule's name may contain.")

(defconst niwa-workspace-schedule-statuses '(active paused finished expired)
  "Statuses of a workspace schedule.  Only an `active' one fires.")

(defconst niwa-workspace-schedule-run-statuses
  '(claimed launched skipped expired setup-error)
  "Statuses this module records for a run.")

(define-error 'niwa-workspace-schedule-missing
  "No such workspace schedule record" 'user-error)

(autoload 'niwa-workspace-schedule-run-fired "niwa-workspace-schedule-runs")
(autoload 'niwa-workspace-schedule-run-settled "niwa-workspace-schedule-runs")
(autoload 'niwa-workspace-schedule-reconcile-run "niwa-workspace-schedule-runs")
(autoload 'niwa-workspace-schedule-run-blocks-p "niwa-workspace-schedule-runs")

(defvar niwa-workspace-schedule-fired-functions '(niwa-workspace-schedule-run-fired)
  "Abnormal hook run once a firing has recorded its run.
Each function receives the workspace and the run record, whatever its
status: `launched', `skipped', `expired' or `setup-error'.")

(defvar niwa-workspace-schedule-run-settled-functions nil
  "Abnormal hook run when a run's first root request settles.
Each function receives the run, as in `niwa-workspace-schedule-run', and
the terminal outcome, such as `success', `error' or `abort'.  It runs in the
run session's data buffer, for native and external engines alike.")

(defvar niwa-workspace-schedule-changed-functions nil
  "Abnormal hook run after a committed change to a workspace's schedules.
Each function receives the workspace.  Creating, changing, deleting and
firing a schedule and updating a run record all run it.")

(defvar-local niwa-workspace-schedule-run nil
  "The workspace schedule run this root data buffer was started for, or nil.
A plist of `:workspace' (its root), `:schedule-id', `:scheduled-at' and
`:session-id'.  It is not persisted: a reopened run session is ordinary.")
(put 'niwa-workspace-schedule-run 'permanent-local t)

(defvar-local niwa-workspace-schedule--run-active nil
  "Non-nil until this run session's first root request settles.")
(put 'niwa-workspace-schedule--run-active 'permanent-local t)


;;
;;; Store

(defun niwa-workspace-schedule--path (workspace &rest names)
  "Return the file NAMES name under WORKSPACE's schedule store."
  (apply #'file-name-concat (niwa-workspace-state-dir workspace) "schedules" names))

(defun niwa-workspace-schedule-id-p (id)
  "Return non-nil when ID is a canonical workspace schedule id, s-xxxxxx.
The x are lowercase hexadecimal digits whatever `case-fold-search' says."
  (and (stringp id)
       (let ((case-fold-search nil))
         (string-match-p "\\`s-[0-9a-f]\\{6\\}\\'" id))))

(defun niwa-workspace-schedule--id (id)
  "Return ID when it is a canonical workspace schedule id, else signal.
Ids name store directories, so nothing else may reach a store path."
  (if (niwa-workspace-schedule-id-p id)
      id
    (user-error "Invalid workspace schedule id: %S" id)))

(defun niwa-workspace-schedule--run-path (workspace id scheduled-at)
  "Return the record of WORKSPACE's schedule ID run due at SCHEDULED-AT."
  (niwa-workspace-schedule--path
   workspace (niwa-workspace-schedule--id id) "runs"
   (format "%d.el" scheduled-at)))

(defun niwa-workspace-schedule--bytes (value)
  "Return VALUE printed in full, as stored."
  (let ((print-length nil) (print-level nil))
    (prin1-to-string value)))

(defun niwa-workspace-schedule--parse (bytes)
  "Return the value BYTES print, or nil when they do not read."
  (ignore-errors (car (read-from-string bytes))))

(defun niwa-workspace-schedule--read (path)
  "Return (VALUE . BYTES) of store file PATH, or nil when it is absent."
  (condition-case nil
      (let ((bytes (niwa-session-control-fs-read-file path)))
        (cons (niwa-workspace-schedule--parse bytes) bytes))
    (niwa-session-control-fs-absent nil)))

(defun niwa-workspace-schedule--commit (workspace operations)
  "Run store OPERATIONS under WORKSPACE's schedule lock.
Return nil when every required operation succeeded, else the status of the
first that did not: `mismatch' or `absent' for a failed proof, `conflict'
for an exclusive creation another writer won.  Other failures signal."
  (let ((results (niwa-session-control-fs-run-program
                  operations (niwa-workspace-schedule--path workspace))))
    (cl-loop for operation in operations
             for result in results
             for status = (plist-get result :status)
             unless (or (eq status 'ok) (plist-get operation :optional))
             return (if (memq status '(mismatch absent conflict))
                        status
                      (niwa-session-control-fs-program-value result)))))

(defun niwa-workspace-schedule--holds-slot-p (entry)
  "Return non-nil when schedule ENTRY counts against the workspace's capacity."
  (memq (plist-get entry :status) '(active paused)))

(defun niwa-workspace-schedule-admit (workspace &optional id)
  "Return store operations admitting one more schedule slot in WORKSPACE.
Signal `user-error' when WORKSPACE already holds
`niwa-workspace-schedule-max' active or paused schedules.  The operations
prove the count under the store lock: they fail as `mismatch' once another
writer added a schedule or changed one holding no slot, so a commit that
starts with them cannot exceed the limit.  ID names a schedule being
created, whose directory the same commit writes and so cannot prove."
  (let* ((store (niwa-workspace-schedule--path workspace))
         (names (niwa-session-control-fs-list-directory store ""))
         (proof (list (niwa-session-control-fs-listing-proof store names)))
         (count 0))
    (dolist (directory names)
      (when (and (niwa-workspace-schedule-id-p (file-name-nondirectory directory))
                 (not (equal id (file-name-nondirectory directory))))
        ;; List before reading: a record published after the listing makes
        ;; the listing proof fail, while one read as absent and published
        ;; before a later listing would pass it uncounted.
        (let* ((file (file-name-concat directory "schedule.el"))
               (entries (niwa-session-control-fs-list-directory directory ""))
               (stored (and (member file entries) (niwa-workspace-schedule--read file))))
          (cond
           ;; A creation may have made its directory but not yet its record;
           ;; a record that vanished after the listing fails this proof.
           ((null stored)
            (push (niwa-session-control-fs-listing-proof directory entries) proof))
           ((and (niwa-workspace-schedule-entry-p (car stored))
                 (niwa-workspace-schedule--holds-slot-p (car stored)))
            (cl-incf count))
           (t (push (list :op 'verify :path file :content (cdr stored)) proof))))))
    (when (>= count niwa-workspace-schedule-max)
      (user-error "This workspace already has %d workspace schedules; delete one first"
                  niwa-workspace-schedule-max))
    proof))

(defun niwa-workspace-schedule-entry-p (entry)
  "Return non-nil when ENTRY is a well-formed stored workspace schedule.
Keys this module does not own are allowed."
  (and (proper-list-p entry)
       (cl-evenp (length entry))
       (niwa-workspace-schedule-id-p (plist-get entry :id))
       (stringp (plist-get entry :name))
       (stringp (plist-get entry :prompt))
       (integerp (plist-get entry :created-at))
       (niwa-schedule-timing-p (plist-get entry :timing) (plist-get entry :created-at))
       (memq (plist-get entry :status) niwa-workspace-schedule-statuses)
       (let ((due (plist-get entry :due)))
         (if (eq 'active (plist-get entry :status)) (integerp due) (or (null due) (integerp due))))
       (let ((expires (plist-get entry :expires-at)))
         (or (null expires) (integerp expires)))
       (let ((preset (plist-get entry :preset)))
         (and preset (symbolp preset)))
       (plist-get entry :policy)
       (ignore-errors (niwa-implementation-policy-validate (plist-get entry :policy)))
       (eq 'skip (plist-get entry :overlap))))

(defun niwa-workspace-schedule-run-p (run)
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

(defun niwa-workspace-schedule-list (workspace)
  "Return WORKSPACE's workspace schedules, oldest first."
  (let (schedules)
    (dolist (directory (niwa-session-control-fs-list-directory
                        (niwa-workspace-schedule--path workspace)
                        "\\`s-[0-9a-f]\\{6\\}\\'"))
      (when-let* ((entry (car (niwa-workspace-schedule--read
                               (file-name-concat directory "schedule.el"))))
                  ((niwa-workspace-schedule-entry-p entry))
                  ((equal (plist-get entry :id) (file-name-nondirectory directory))))
        (push entry schedules)))
    (sort schedules (lambda (a b) (< (plist-get a :created-at) (plist-get b :created-at))))))

(defun niwa-workspace-schedule-get (workspace id)
  "Return WORKSPACE's workspace schedule ID, or nil.
Signal for an ID that is not a canonical schedule id."
  (let ((entry (car (niwa-workspace-schedule--read
                     (niwa-workspace-schedule--path
                      workspace (niwa-workspace-schedule--id id) "schedule.el")))))
    (and (niwa-workspace-schedule-entry-p entry)
         (equal id (plist-get entry :id))
         entry)))

(defun niwa-workspace-schedule-runs (workspace id)
  "Return the run records of WORKSPACE's schedule ID, oldest first."
  (let* ((paths (niwa-session-control-fs-list-directory
                 (niwa-workspace-schedule--path
                  workspace (niwa-workspace-schedule--id id) "runs")
                 "\\`[0-9]+\\.el\\'"))
         (results (and paths
                       (niwa-session-control-fs-run-program
                        (mapcar (lambda (path) (list :op 'read :path path :optional t))
                                paths))))
         runs)
    (dolist (result results)
      (when-let* (((eq 'ok (plist-get result :status)))
                  (run (niwa-workspace-schedule--parse
                        (niwa-session-control-fs-program-value result)))
                  ((niwa-workspace-schedule-run-p run)))
        (push run runs)))
    (sort runs (lambda (a b) (< (plist-get a :scheduled-at) (plist-get b :scheduled-at))))))

(defun niwa-workspace-schedule--update-file (workspace path valid-p function
                                                          &optional slot-p)
  "Replace store file PATH of WORKSPACE with FUNCTION's result.
FUNCTION receives a copy of the current value and returns the new one; an
`equal' result writes nothing.  VALID-P must accept the result.  A value
changed meanwhile by another writer is read again and FUNCTION called again.
When SLOT-P says a value counts against capacity and the new one does but
the current one does not, the change is admitted within capacity.  Return
the committed value."
  (cl-loop
   repeat 5
   do (pcase-let* ((`(,value . ,bytes)
                    (or (niwa-workspace-schedule--read path)
                        (signal 'niwa-workspace-schedule-missing (list path))))
                   (new (funcall function (copy-tree value)))
                   (operations (list (list :op 'verify :path path :content bytes)
                                     (list :op 'write :path path
                                           :content (niwa-workspace-schedule--bytes new)))))
        (cond
         ((equal new value) (cl-return value))
         ((not (funcall valid-p new)) (error "Invalid schedule record: %S" new))
         ((not (niwa-workspace-schedule--commit
                workspace
                (if (and slot-p (funcall slot-p new) (not (funcall slot-p value)))
                    (append (niwa-workspace-schedule-admit workspace) operations)
                  operations)))
          (niwa-workspace-schedule--refresh workspace)
          (cl-return new))))
   finally (error "Schedule record %s keeps changing; try again" path)))

(defun niwa-workspace-schedule-update (workspace id function)
  "Replace WORKSPACE's schedule ID with FUNCTION's result and return it.
FUNCTION receives the current schedule and returns the new one, and may be
called again when another writer changed it meanwhile.  The scheduler and
index follow the committed schedule; no session is needed.  A change that
makes a finished or expired schedule active or paused again must fit
within `niwa-workspace-schedule-max'."
  (niwa-workspace-schedule--update-file
   workspace (niwa-workspace-schedule--path
              workspace (niwa-workspace-schedule--id id) "schedule.el")
   (lambda (entry)
     (and (niwa-workspace-schedule-entry-p entry) (equal id (plist-get entry :id))))
   function #'niwa-workspace-schedule--holds-slot-p))

(defun niwa-workspace-schedule-update-run (workspace id scheduled-at function)
  "Replace the record of WORKSPACE's schedule ID run due at SCHEDULED-AT.
FUNCTION receives the current record and returns the new one, as for
`niwa-workspace-schedule-update'.  Return the committed record."
  (niwa-workspace-schedule--update-file
   workspace (niwa-workspace-schedule--run-path workspace id scheduled-at)
   #'niwa-workspace-schedule-run-p function))

(defun niwa-workspace-schedule-delete (workspace id)
  "Delete WORKSPACE's schedule ID with its run records."
  (unless (niwa-workspace-schedule-get workspace id)
    (user-error "No workspace schedule with id %s" id))
  (when (niwa-workspace-schedule--commit
         workspace (list (list :op 'delete-directory
                               :path (niwa-workspace-schedule--path workspace id))))
    (user-error "No workspace schedule with id %s" id))
  (niwa-workspace-schedule--refresh workspace))


;;
;;; Proposals

(defun niwa-workspace-schedule--new-id (workspace)
  "Return a workspace schedule identity unused in WORKSPACE."
  (let (id)
    (while (or (null id) (niwa-workspace-schedule-get workspace id))
      (setq id (format "s-%06x" (random #x1000000))))
    id))

(defun niwa-workspace-schedule-propose (args &optional previous)
  "Return the schedule ARGS describe, without storing it, or signal `user-error'.
ARGS is a plist of `:name', `:prompt', `:when' and `:expires' (timing
texts; see `niwa-schedule-parse-expiry'), `:preset' (a symbol, default
`niwa-implement') and `:policy' (an implementation policy).  For a
change, PREVIOUS is the stored schedule and supplies whatever ARGS omits;
it keeps its timing and next run unless ARGS gives `:when', and an
`:expires' of \"never\" removes its expiry.  Workspace schedules have no
default expiry."
  (let* ((now (niwa-schedule-now))
         (field (lambda (key) (or (plist-get args key) (plist-get previous key))))
         (name (string-trim (or (funcall field :name) "")))
         (prompt (string-trim (or (funcall field :prompt) "")))
         (when-text (plist-get args :when))
         (timing (if when-text
                     (niwa-schedule-parse-timing when-text now)
                   (plist-get previous :timing)))
         (due (if when-text
                  (niwa-schedule-next-run timing now)
                (plist-get previous :due)))
         (expires (plist-get args :expires))
         (expires-at (if expires
                         (niwa-schedule-parse-expiry expires now)
                       (plist-get previous :expires-at)))
         (preset (or (funcall field :preset) 'niwa-implement))
         (policy (funcall field :policy))
         (status (cond ((null previous) 'active)
                       ((eq 'paused (plist-get previous :status)) 'paused)
                       (t 'active))))
    (when (string-empty-p name)
      (user-error "A workspace schedule needs a name"))
    (when (or (> (length name) niwa-workspace-schedule-max-name-length)
              (string-match-p "[[:cntrl:]]" name))
      (user-error "Schedule name must be one line of at most %d characters"
                  niwa-workspace-schedule-max-name-length))
    (when (string-empty-p prompt)
      (user-error "A workspace schedule needs a prompt"))
    (when (> (length prompt) niwa-schedule-max-note-length)
      (user-error "Schedule prompt exceeds %d characters" niwa-schedule-max-note-length))
    (unless timing
      (user-error "A workspace schedule needs `when'"))
    (unless (and due (> due now))
      (user-error "This schedule has no future run; give a new `when'"))
    (when (and expires-at (<= expires-at due))
      (user-error "Schedule expiry %s is not after its next run %s"
                  (niwa-schedule-format-time expires-at)
                  (niwa-schedule-format-time due)))
    (unless (and (symbolp preset) (assq preset (bound-and-true-p niwa-preset--registry)))
      (user-error "Unknown preset %s" preset))
    (unless (and policy (ignore-errors (niwa-implementation-policy-validate policy)))
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

(defun niwa-workspace-schedule-create (workspace entry)
  "Store proposed schedule ENTRY in WORKSPACE and return the stored entry.
ENTRY comes from `niwa-workspace-schedule-propose'; the caller has had it
approved.  A workspace holds at most `niwa-workspace-schedule-max'
active or paused schedules."
  (let ((entry (if (niwa-workspace-schedule-get workspace (plist-get entry :id))
                   (plist-put (copy-sequence entry) :id
                              (niwa-workspace-schedule--new-id workspace))
                 entry)))
    (unless (niwa-workspace-schedule-entry-p entry)
      (error "Invalid workspace schedule: %S" entry))
    (niwa-session-control-fs-make-directory
     (niwa-workspace-schedule--path workspace (plist-get entry :id) "runs") t)
    (cl-loop
     repeat 5
     for status = (niwa-workspace-schedule--commit
                   workspace
                   (append (niwa-workspace-schedule-admit workspace (plist-get entry :id))
                           (list (list :op 'create
                                       :path (niwa-workspace-schedule--path
                                              workspace (plist-get entry :id) "schedule.el")
                                       :content (niwa-workspace-schedule--bytes entry)))))
     until (null status)
     when (eq status 'conflict)
     do (error "Workspace schedule %s already exists" (plist-get entry :id))
     finally (when status (error "Workspace schedules keep changing; try again")))
    (niwa-workspace-schedule--refresh workspace)
    entry))


;;
;;; Scheduling

(defun niwa-workspace-schedule--root (workspace)
  "Return WORKSPACE's root in the one spelling runs and the index store."
  (niwa-schedule--directory-key (niwa-workspace-root workspace)))

(defun niwa-workspace-schedule-workspace (root)
  "Return the workspace whose root is ROOT."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory root))
    (or (niwa-workspace) (error "No niwa workspace at %s" root))))

(defun niwa-workspace-schedule--index-file (root)
  "Return the index file naming workspace ROOT."
  (file-name-concat (niwa-schedule-workspace-index-directory)
                    (concat (md5 root) ".el")))

(defun niwa-workspace-schedule--index (root)
  "Name workspace ROOT in the index unless it already is."
  (let ((file (niwa-workspace-schedule--index-file root)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file (prin1 root (current-buffer))))))

(defun niwa-workspace-schedule--refresh (workspace)
  "Schedule WORKSPACE's earliest active schedule and index the workspace.
Return its due time, or nil when nothing is active.  Only startup restore
removes index entries: two Emacs instances sharing `niwa-user-dir' could
otherwise drop each other's entry."
  (let* ((root (niwa-workspace-schedule--root workspace))
         (dues (cl-loop for schedule in (niwa-workspace-schedule-list workspace)
                        when (eq 'active (plist-get schedule :status))
                        collect (plist-get schedule :due)))
         (due (and dues (apply #'min dues))))
    (setq niwa-schedule--workspaces
          (cl-remove root niwa-schedule--workspaces :key #'car :test #'equal))
    (when due
      (push (cons root due) niwa-schedule--workspaces))
    (condition-case err
        (when due (niwa-workspace-schedule--index root))
      (error
       (niwa-diagnostic-warning
        root 'niwa
        (format "Workspace schedule index for %s could not be updated: %s"
                root (error-message-string err))
        :warning)))
    (niwa-schedule--rearm)
    (run-hook-with-args 'niwa-workspace-schedule-changed-functions workspace)
    due))

(defun niwa-workspace-schedule--restore-workspace (workspace)
  "Reconcile WORKSPACE's runs and timer.  Return non-nil if still indexed."
  (let (pending)
    (dolist (job (niwa-workspace-schedule-list workspace))
      (dolist (run (niwa-workspace-schedule-runs workspace (plist-get job :id)))
        (unless (or (plist-get run :outcome)
                    (niwa-workspace-schedule-reconcile-run workspace run))
          (setq pending t))))
    (or (niwa-workspace-schedule--refresh workspace)
        (when pending
          (niwa-workspace-schedule--index (niwa-workspace-schedule--root workspace))
          t))))

(defun niwa-workspace-schedule--prune (workspace file)
  "Delete WORKSPACE's index FILE after its store read as inactive.
The store is read again afterwards, as `niwa-schedule--prune' does for
wake-ups: a schedule another Emacs committed in between is indexed again,
and a re-read that signals puts the entry back before the error propagates."
  (delete-file file)
  (let (confirmed)
    (unwind-protect
        (progn (niwa-workspace-schedule--restore-workspace workspace)
               (setq confirmed t))
      (unless confirmed
        (niwa-workspace-schedule--index (niwa-workspace-schedule--root workspace))))))

;;;###autoload
(defun niwa-workspace-schedule-restore ()
  "Schedule the active workspace schedules of every indexed workspace.
Run at startup.  Only entries proven stale are removed: malformed ones,
and those whose workspace is provably gone or whose readable store holds
no active schedule.  An entry that cannot be read now is kept."
  (let ((inhibit-interaction t))
    (dolist (file (directory-files (niwa-schedule-workspace-index-directory) t "\\.el\\'"))
      (let (root)
        (condition-case err
            (progn
              ;; An unreadable entry file signals here and is kept.
              (setq root (with-temp-buffer
                           (insert-file-contents file)
                           (ignore-errors (read (current-buffer)))))
              (cond
               ((not (stringp root)) (delete-file file))
               ((niwa-schedule--absent-p root) (delete-file file))
               ((not (file-accessible-directory-p root))
                (error "%s is not reachable" root))
               (t (let ((workspace (niwa-workspace-schedule-workspace root)))
                    (unless (niwa-workspace-schedule--restore-workspace workspace)
                      (niwa-workspace-schedule--prune workspace file))))))
          (error
           (niwa-diagnostic-warning
            'global 'niwa
            (format "Workspace schedules of %s were not restored: %s"
                    (or root file) (error-message-string err))
            :warning)))))))

(defun niwa-workspace-schedule-run-due (root now)
  "Fire workspace ROOT's schedules due by clock seconds NOW.
A schedule that fails to fire is retried after `niwa-schedule-retry-delay'."
  (condition-case err
      (let ((workspace (niwa-workspace-schedule-workspace root))
            failed)
        (dolist (schedule (niwa-workspace-schedule-list workspace))
          (when (and (eq 'active (plist-get schedule :status))
                     (<= (plist-get schedule :due) now))
            (condition-case err
                (niwa-workspace-schedule--fire workspace (plist-get schedule :id) now)
              (error
               (setq failed t)
               (niwa-diagnostic-warning
                root 'niwa
                (format "Workspace schedule %s did not fire: %s; retrying in %ds"
                        (plist-get schedule :name) (error-message-string err)
                        niwa-schedule-retry-delay)
                :warning)))))
        ;; The failed schedule is still due, so its past due time would
        ;; re-arm the timer at once.
        (when (and (niwa-workspace-schedule--refresh workspace) failed)
          (setf (alist-get root niwa-schedule--workspaces nil nil #'equal)
                (+ now niwa-schedule-retry-delay))
          (niwa-schedule--rearm)))
    (error
     (push (cons root (+ now niwa-schedule-retry-delay)) niwa-schedule--workspaces)
     (niwa-diagnostic-warning
      root 'niwa
      (format "Workspace schedules of %s could not be read (%s); retrying in %ds"
              root (error-message-string err) niwa-schedule-retry-delay)
      :warning))))


;;
;;; Runs

(defun niwa-workspace-schedule--previous-busy-p (workspace id due)
  "Return non-nil when the latest run of WORKSPACE's schedule ID still works.
Check an open session locally; otherwise use its lease or its claim at DUE."
  (when-let* ((run (cl-find-if (lambda (run) (or (plist-get run :session-id)
                                                 (eq 'claimed (plist-get run :status))))
                               (niwa-workspace-schedule-runs workspace id)
                               :from-end t)))
    (if-let* ((session-id (plist-get run :session-id))
              (open (cl-find-if
                     (lambda (entry)
                       (equal session-id (niwa-session-session-id
                                          (buffer-local-value 'niwa--session (cdr entry)))))
                     (niwa--workspace-sessions workspace))))
        (or (buffer-local-value 'niwa-workspace-schedule--run-active (cdr open))
            (niwa-turn-busy-p (cdr open)))
      (niwa-workspace-schedule-run-blocks-p workspace run due))))

(defun niwa-workspace-schedule--fire (workspace id now)
  "Fire WORKSPACE's schedule ID, due by clock seconds NOW, at most once.
Return the recorded run, or nil when there was nothing to fire or another
Emacs fired it."
  (cl-loop
   repeat 5
   do (pcase-let* ((path (niwa-workspace-schedule--path workspace id "schedule.el"))
                   (`(,schedule . ,bytes) (niwa-workspace-schedule--read path)))
        (unless (and (niwa-workspace-schedule-entry-p schedule)
                     (eq 'active (plist-get schedule :status))
                     (<= (plist-get schedule :due) now))
          (cl-return nil))
        (let* ((due (plist-get schedule :due))
               (expires (plist-get schedule :expires-at))
               (late (and expires (> now expires)))
               (next (and (not late) (niwa-schedule-next-run (plist-get schedule :timing) now)))
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
                                        ((niwa-workspace-schedule--previous-busy-p workspace id due)
                                         'skipped)
                                        (t 'claimed)))))
          (when (eq 'claimed (plist-get run :status))
            (setf (plist-get run :holder) (niwa-session-durability--holder)))
          (pcase (niwa-workspace-schedule--commit
                  workspace
                  (list (list :op 'verify :path path :content bytes)
                        (list :op 'create
                              :path (niwa-workspace-schedule--run-path workspace id due)
                              :content (niwa-workspace-schedule--bytes run))
                        (list :op 'write :path path
                              :content (niwa-workspace-schedule--bytes updated))))
            ('mismatch nil)
            ('absent (cl-return nil))
            ;; This run was recorded by a firing whose schedule write never
            ;; landed, so nobody launched it: move the schedule on and settle
            ;; the run, which no later firing of a one-shot would retire.
            ('conflict
             (unless (eq 'mismatch (niwa-workspace-schedule--commit
                                    workspace
                                    (list (list :op 'verify :path path :content bytes)
                                          (list :op 'write :path path
                                                :content (niwa-workspace-schedule--bytes
                                                          updated)))))
               (when-let* ((recorded (car (niwa-workspace-schedule--read
                                           (niwa-workspace-schedule--run-path workspace id due))))
                           ((not (plist-get recorded :outcome))))
                 (run-hook-with-args 'niwa-workspace-schedule-fired-functions
                                     workspace recorded))
               (cl-return nil)))
            (_
             (when (eq 'claimed (plist-get run :status))
               (setq run (niwa-workspace-schedule--launch workspace updated run)))
             (run-hook-with-args 'niwa-workspace-schedule-fired-functions workspace run)
             (cl-return run)))))
   finally (error "Schedule %s keeps changing while it fires" id)))

(defun niwa-workspace-schedule--record (workspace run &rest keys)
  "Store KEYS into claimed RUN of WORKSPACE and return the committed record."
  (niwa-workspace-schedule-update-run
   workspace (plist-get run :schedule-id) (plist-get run :scheduled-at)
   (lambda (current)
     (unless (and (plist-member keys :status) (plist-get current :outcome))
       (cl-loop for (key value) on keys by #'cddr
                do (setq current (plist-put current key value))))
     current)))

(defun niwa-workspace-schedule--prompt (schedule run)
  "Return the first message of SCHEDULE's RUN, noting a late start."
  (let ((late (- (plist-get run :fired-at) (plist-get run :scheduled-at))))
    (if (< late niwa-schedule-overdue-threshold)
        (plist-get schedule :prompt)
      (format "%s\n\n(Scheduled for %s; started overdue by %s.)"
              (plist-get schedule :prompt)
              (niwa-schedule-format-time (plist-get run :scheduled-at))
              (niwa--duration-label late)))))

(cl-defun niwa-workspace-schedule--launch (workspace schedule run)
  "Start claimed RUN of SCHEDULE in a new session of WORKSPACE.
The session's own preset, model, effort and permission mode start as the
approved ones, so every turn in it runs under them until someone changes
them there.  It is saved before its first message is submitted, without a
window.  Return the committed run record, `setup-error' on failure."
  (let ((inhibit-interaction t)
        (root (niwa-workspace-schedule--root workspace))
        (name (format "%s %s" (plist-get schedule :name)
                      (format-time-string "%F %R" (plist-get run :scheduled-at))))
        buffer)
    (condition-case err
        (progn
          (let ((current (car (niwa-workspace-schedule--read
                               (niwa-workspace-schedule--run-path
                                workspace (plist-get run :schedule-id) (plist-get run :scheduled-at))))))
            (unless (and (eq 'claimed (plist-get current :status)) (not (plist-get current :outcome)))
              (cl-return-from niwa-workspace-schedule--launch current)))
          ;; An open session with this name, as from a same-named schedule
          ;; due at the same time, must not receive this run.
          (let ((base name) (count 1))
            (while (assoc name (niwa--workspace-sessions workspace))
              (setq name (format "%s (%d)" base (cl-incf count)))))
          (setq buffer (niwa--chat-buffer name t workspace (niwa-workspace-root workspace)))
          (with-current-buffer buffer
            (let ((session niwa--session)
                  (policy (plist-get schedule :policy)))
              (niwa-preset-apply (plist-get schedule :preset) buffer)
              (niwa-model-set-session-provider
               session (niwa-model-resolve-provider (plist-get policy :model-provider))
               buffer)
              (niwa-model-set-session-effort
               session (plist-get policy :reasoning-effort) buffer)
              (niwa-permission-mode-transition (plist-get policy :permission-mode))
              (niwa-session-artifacts-save session buffer nil t)
              (setq run (niwa-workspace-schedule--record
                         workspace run :status 'launched
                         :session-id (niwa-session-session-id session)))
              (when (plist-get run :outcome)
                (cl-return-from niwa-workspace-schedule--launch run))
              (setq-local niwa-workspace-schedule-run
                          (list :workspace root :schedule-id (plist-get run :schedule-id)
                                :scheduled-at (plist-get run :scheduled-at)
                                :session-id (plist-get run :session-id))
                          niwa-workspace-schedule--run-active t)
              (niwa--submit-generated-turn
               (niwa-workspace-schedule--prompt schedule run)
               nil nil nil (list :turn-trigger 'workspace-schedule
                                 :workspace-schedule-run niwa-workspace-schedule-run))))
          run)
      (error
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq-local niwa-workspace-schedule--run-active nil)))
       (niwa-workspace-schedule--record
        workspace run :status 'setup-error :error (error-message-string err))))))

(defun niwa-workspace-schedule--turn-settled (outcome)
  "Record and report the current run session's first settled root request.
OUTCOME is its terminal outcome.  The record's `:settled-at' tells another
Emacs that the run no longer blocks the next one."
  (when (and niwa-workspace-schedule--run-active niwa-workspace-schedule-run)
    (setq-local niwa-workspace-schedule--run-active nil)
    (niwa-workspace-schedule-run-settled niwa-workspace-schedule-run outcome)
    (run-hook-with-args 'niwa-workspace-schedule-run-settled-functions
                        niwa-workspace-schedule-run outcome)))

(add-hook 'niwa-turn-settled-functions #'niwa-workspace-schedule--turn-settled)

(defun niwa-workspace-schedule-notify-receipt-file (session)
  "Return the Notify receipt file for SESSION's active run, or nil.
Until its first root request settles, a run session's notices share one
receipt file per schedule, so a repeated notice from a later run is dropped."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer))
              ((buffer-local-value 'niwa-workspace-schedule--run-active buffer))
              (run (buffer-local-value 'niwa-workspace-schedule-run buffer)))
    (niwa-workspace-schedule--path (niwa-session-workspace session)
                                      (plist-get run :schedule-id)
                                      "notify-receipts.json")))

(defun niwa-workspace-schedule-record-notice ()
  "Mark the current root's active run as having emitted a Notify notice."
  (when (and niwa-workspace-schedule--run-active niwa-workspace-schedule-run)
    (condition-case err
        (niwa-workspace-schedule--record
         (niwa-session-workspace niwa--session) niwa-workspace-schedule-run :notified t)
      (error (niwa-diagnostic-warning
              (niwa-diagnostic-scope niwa--session) 'niwa
              (format "Could not record the run's delivered notice: %s" (error-message-string err))
              :warning)))))

(provide 'niwa-workspace-schedule)
;;; niwa-workspace-schedule.el ends here
