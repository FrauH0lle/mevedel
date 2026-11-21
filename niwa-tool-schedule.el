;;; niwa-tool-schedule.el --- Schedule tool -*- lexical-binding: t -*-

;;; Commentary:

;; One model tool creates, lists, changes and cancels the root conversation's
;; one-shot and recurring wake-ups, and creates, changes, lists and deletes workspace
;; schedules.  It is visible to root requests only; a child agent asks the
;; root to schedule instead.  Creating or changing a workspace schedule
;; grants standing authority, so it always asks the person through a card,
;; whatever the session's permission mode.  `niwa-schedule' and
;; `niwa-workspace-schedule' own timing, storage and firing.

;;; Code:

(require 'niwa-schedule)
(require 'niwa-tool-registry)
(require 'niwa-workspace-schedule)

;; `niwa-models'
(declare-function niwa-model--provider-label "niwa-models" (provider))
(declare-function niwa-model-parse-effort "niwa-models" (value))
(declare-function niwa-model-parse-selector "niwa-models" (value))
(declare-function niwa-model-resolve-workload "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function niwa-model-selection-delta "niwa-models" (model effort))

;; `niwa-permission-queue'
(declare-function niwa-permission-request "niwa-permission-queue"
                  (entry &optional session))
(autoload 'niwa-permission-request "niwa-permission-queue")

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())
(autoload 'niwa-current-origin "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))

(defconst niwa-tool-schedule--modes '(ask edits full-auto)
  "Permission modes from least to most permissive.")

(defun niwa-tool-schedule--entry-json (entry)
  "Return wake-up ENTRY as a JSON-serializable plist.
Its model and effort are listed resolved, only where they differ from the
session's."
  (let ((expires (plist-get entry :expires-at)))
    (append (list :id (plist-get entry :id)
                  :target "session"
                  :time (niwa-schedule-format-time (plist-get entry :due))
                  :repeat (niwa-schedule-repeat-label (plist-get entry :timing))
                  :note (plist-get entry :note)
                  :expires (and expires (niwa-schedule-format-time expires))
                  :status (symbol-name (plist-get entry :status)))
            (and (or (plist-get entry :model) (plist-get entry :effort))
                 (niwa-model-selection-delta (plist-get entry :model)
                                                (plist-get entry :effort))))))

(defun niwa-tool-schedule--workspace-json (entry)
  "Return workspace schedule ENTRY as a JSON-serializable plist."
  (let ((policy (plist-get entry :policy))
        (due (plist-get entry :due))
        (expires (plist-get entry :expires-at)))
    (list :id (plist-get entry :id)
          :target "workspace"
          :name (plist-get entry :name)
          :time (and due (niwa-schedule-format-time due))
          :repeat (niwa-schedule-repeat-label (plist-get entry :timing))
          :note (plist-get entry :prompt)
          :preset (symbol-name (plist-get entry :preset))
          :model (plist-get policy :model-provider)
          :effort (let ((effort (plist-get policy :reasoning-effort)))
                    (and effort (symbol-name effort)))
          :permission_mode (symbol-name (plist-get policy :permission-mode))
          :expires (and expires (niwa-schedule-format-time expires))
          :status (symbol-name (plist-get entry :status)))))

(defun niwa-tool-schedule--result (key value)
  "Return a tool result reporting KEY as VALUE with the current local time."
  (list :result
        (json-serialize
         (list key value
               :now (niwa-schedule-format-time (niwa-schedule-now)))
         :null-object nil)))

(defun niwa-tool-schedule--policy (args previous session)
  "Return the implementation policy ARGS request for a workspace schedule.
PREVIOUS is the schedule being changed, or nil.  The model and effort
default to PREVIOUS's, else the current buffer's session selection; the
permission mode defaults to PREVIOUS's, else SESSION's current mode."
  (let* ((model (plist-get args :model))
         (effort (plist-get args :effort))
         (mode (plist-get args :permission_mode))
         (stored (plist-get previous :policy))
         (selection
          (if (and stored (not model) (not effort))
              stored
            (let ((resolved (niwa-model-resolve-workload
                             nil
                             (niwa-model-parse-selector
                              (or model (plist-get stored :model-provider)))
                             (niwa-model-parse-effort effort))))
              (list :model-provider (niwa-model--provider-label resolved)
                    :reasoning-effort (plist-get resolved :effort))))))
    (list :model-provider (plist-get selection :model-provider)
          :reasoning-effort (plist-get selection :reasoning-effort)
          :permission-mode (cond (mode (intern mode))
                                 (stored (plist-get stored :permission-mode))
                                 (t (niwa-session-permission-mode session))))))

(defun niwa-tool-schedule--card (entry previous session)
  "Return the card facts for approving workspace schedule ENTRY.
PREVIOUS is the schedule ENTRY changes, or nil for a creation.  A mode above
PREVIOUS's, or for a creation above SESSION's current one, is an escalation."
  (let* ((policy (plist-get entry :policy))
         (mode (plist-get policy :permission-mode))
         (baseline (if previous
                       (plist-get (plist-get previous :policy) :permission-mode)
                     (niwa-session-permission-mode session)))
         (expires (plist-get entry :expires-at))
         (changed (and previous
                       (cl-loop for key in '(:name :prompt :timing :expires-at :preset :policy)
                                unless (equal (plist-get entry key) (plist-get previous key))
                                collect (substring (symbol-name key) 1)))))
    (list :card-title (if previous
                          (format "Change Workspace Schedule %s" (plist-get entry :id))
                        "Create Workspace Schedule")
          :card-warning
          (when (> (cl-position mode niwa-tool-schedule--modes)
                   (or (cl-position baseline niwa-tool-schedule--modes) 0))
            (format "Escalation: %s -> %s for every unattended run" baseline mode))
          :card-fields
          (append
           (and previous (list (cons "Changed" (if changed (string-join changed ", ") "nothing"))))
           (list (cons "Name" (plist-get entry :name))
                 (cons "Timing" (or (niwa-schedule-repeat-label (plist-get entry :timing))
                                    "once"))
                 (cons "Next run" (niwa-schedule-format-time (plist-get entry :due)))
                 (cons "Preset" (plist-get entry :preset))
                 (cons "Model" (plist-get policy :model-provider))
                 (cons "Effort" (or (plist-get policy :reasoning-effort) "model default"))
                 (cons "Permission mode" mode)
                 (cons "Overlap" "skip a run while the previous run is still working")
                 (cons "Expires" (if expires (niwa-schedule-format-time expires) "never"))))
          :card-text (plist-get entry :prompt))))

(defun niwa-tool-schedule--ask (callback session buffer args &optional previous)
  "Ask through a card to store the workspace schedule ARGS describe.
SESSION is the root session and BUFFER its data buffer.  PREVIOUS is the
stored schedule ARGS change, or nil to create one.  CALLBACK receives the
tool result once the person answers; nothing is stored unless approved."
  (let* ((workspace (niwa-session-workspace session))
         (preset (plist-get args :preset))
         (entry (niwa-workspace-schedule-propose
                 (list :name (plist-get args :name) :prompt (plist-get args :note)
                       :when (plist-get args :when) :expires (plist-get args :expires)
                       :preset (and preset (intern preset))
                       :policy (niwa-tool-schedule--policy args previous session))
                 previous))
         (request niwa--current-request))
    ;; The store admits it again on approval; this spares the person a card
    ;; that could only fail.
    (when (and (niwa-workspace-schedule--holds-slot-p entry)
               (not (niwa-workspace-schedule--holds-slot-p previous)))
      (niwa-workspace-schedule-admit workspace))
    (niwa-permission-request
     (append
      (list :kind 'schedule :tool-name "Schedule" :origin "/root"
            :data-buffer buffer :workspace workspace :request request
            :request-id (and request (niwa-request-id request))
            :callback
            (lambda (outcome)
              (funcall
               callback
               (condition-case err
                   (if (not (eq outcome 'allow-once))
                       (list :result (format "Error: The user declined this workspace schedule%s"
                                             (if (eq 'feedback (car-safe outcome))
                                                 (format ": %s" (cdr outcome))
                                               "."))
                             :status 'error)
                     (niwa-tool-schedule--result
                      (if previous :changed :created)
                      (niwa-tool-schedule--workspace-json
                       (if previous
                           (niwa-workspace-schedule-update
                            workspace (plist-get previous :id)
                            (lambda (current)
                              (unless (equal current previous)
                                (user-error "Schedule %s changed while awaiting approval; propose the change again"
                                            (plist-get previous :id)))
                              entry))
                         (niwa-workspace-schedule-create workspace entry)))))
                 (error (list :result (format "Error: %s" (error-message-string err))
                              :status 'error))))))
      (niwa-tool-schedule--card entry previous session))
     session)
    nil))

(defun niwa-tool-schedule--handle (callback args)
  "Create, list, change or cancel schedules as ARGS request; answer CALLBACK."
  (unless (equal (niwa-current-origin) "/root")
    (error "Schedule is available only to /root; ask the root to schedule"))
  (let* ((session (or (bound-and-true-p niwa--session)
                      (error "Schedule needs a niwa session")))
         (buffer (if (buffer-live-p (niwa-session-root-buffer session))
                     (niwa-session-root-buffer session)
                   (current-buffer)))
         (workspace (niwa-session-workspace session))
         (id (let ((id (plist-get args :id)))
               (and (stringp id) (string-match-p "\\S-" id) (string-trim id))))
         (stored (lambda ()
                   (or (and id (niwa-workspace-schedule-get workspace id))
                       (user-error "No workspace schedule with id %s" id)))))
    (pcase (plist-get args :action)
      ((and "create" (guard (equal "workspace" (plist-get args :target))))
       (niwa-tool-schedule--ask callback session buffer args))
      ((and "change" (guard (and id (string-prefix-p "s-" id))))
       (niwa-tool-schedule--ask callback session buffer args (funcall stored)))
      ("change"
       (unless id
         (user-error "Schedule change requires an id"))
       (funcall callback
                (niwa-tool-schedule--result
                 :changed
                 (niwa-tool-schedule--entry-json
                  (niwa-schedule-change session buffer id (plist-get args :when)
                                           (plist-get args :note) (plist-get args :expires)
                                           (plist-get args :model) (plist-get args :effort))))))
      ("create"
       (funcall callback
                (niwa-tool-schedule--result
                 :created
                 (niwa-tool-schedule--entry-json
                  (niwa-schedule-create session buffer (plist-get args :when)
                                           (plist-get args :note) (plist-get args :expires)
                                           (plist-get args :model) (plist-get args :effort))))))
      ("list"
       (funcall callback
                (niwa-tool-schedule--result
                 :schedules
                 (vconcat (mapcar #'niwa-tool-schedule--entry-json
                                  (niwa-session-schedules session))
                          (mapcar #'niwa-tool-schedule--workspace-json
                                  (niwa-workspace-schedule-list workspace))))))
      ("cancel"
       (unless id
         (user-error "Schedule cancel requires an id"))
       (funcall callback
                (niwa-tool-schedule--result
                 :cancelled
                 (if (string-prefix-p "s-" id)
                     (let ((entry (funcall stored)))
                       (niwa-workspace-schedule-delete workspace id)
                       (niwa-tool-schedule--workspace-json entry))
                   (niwa-tool-schedule--entry-json
                    (niwa-schedule-cancel session buffer id))))))
      (action (user-error "Unknown Schedule action: %S" action)))
    nil))

(defun niwa-tool-schedule--register ()
  "Register the Schedule tool."
  (niwa-define-tool
   :name "Schedule"
   :description "Wake this conversation later, or run a prompt in a new session on a workspace schedule; list, change or cancel schedules."
   :prompt-file "prompts/tools/schedule.md"
   :handler #'niwa-tool-schedule--handle
   :async-p t
   :args ((action string :required "What to do."
                  :enum ["create" "list" "change" "cancel"])
          (target string :optional
                  "For create: session (default) wakes this conversation; workspace runs each time in a new session."
                  :enum ["session" "workspace"])
          (when string :optional
                "For create or change: a duration such as 20m or a local time such as 2026-11-21 10:00; every 20m or a cron expression such as 0 9 * * 1-5 to repeat.")
          (note string :optional
                "For create or change: what the woken turn should do, written for yourself; for a workspace schedule, each run's first message.")
          (expires string :optional
                   "For create or change: optional duration or local time after which a run is dropped; never removes it.")
          (name string :optional
                "For a workspace schedule: a short name; runs are named after it.")
          (preset string :optional
                  "For a workspace schedule: the base preset of its runs. Default niwa-implement.")
          (permission_mode string :optional
                           "For a workspace schedule: the permission mode of its runs. Default this session's."
                           :enum ["ask" "edits" "full-auto"])
          (model string :optional
                 "For create or change: run on this model: a tier (fast, balanced, strong) or BACKEND:MODEL. Omit for the session's.")
          (effort string :optional
                  "For create or change: reasoning effort. Omit for the session's.")
          (id string :optional "For change or cancel: the schedule id."))
   ;; Keeps wake-ups card-free in Ask and Edits; workspace schedules ask themselves.
   :read-only-p t
   :groups (util)))

(provide 'niwa-tool-schedule)
;;; niwa-tool-schedule.el ends here
