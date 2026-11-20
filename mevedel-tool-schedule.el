;;; mevedel-tool-schedule.el --- Schedule tool -*- lexical-binding: t -*-

;;; Commentary:

;; One model tool creates, lists and cancels the root conversation's one-shot
;; and recurring wake-ups, and creates, changes, lists and deletes workspace
;; schedules.  It is visible to root requests only; a child agent asks the
;; root to schedule instead.  Creating or changing a workspace schedule
;; grants standing authority, so it always asks the person through a card,
;; whatever the session's permission mode.  `mevedel-schedule' and
;; `mevedel-workspace-schedule' own timing, storage and firing.

;;; Code:

(require 'mevedel-schedule)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace-schedule)

;; `mevedel-models'
(declare-function mevedel-model-parse-effort "mevedel-models" (value))
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-workload "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-permission-queue'
(declare-function mevedel-permission-request "mevedel-permission-queue"
                  (entry &optional session))
(autoload 'mevedel-permission-request "mevedel-permission-queue")

;; `mevedel-structs'
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))

(defconst mevedel-tool-schedule--modes '(ask edits full-auto)
  "Permission modes from least to most permissive.")

(defun mevedel-tool-schedule--entry-json (entry)
  "Return wake-up ENTRY as a JSON-serializable plist.
Its model and effort are listed only when they differ from the session's."
  (let ((expires (plist-get entry :expires-at))
        (model (plist-get entry :model))
        (effort (plist-get entry :effort)))
    (append (list :id (plist-get entry :id)
                  :target "session"
                  :time (mevedel-schedule-format-time (plist-get entry :due))
                  :repeat (mevedel-schedule-repeat-label (plist-get entry :timing))
                  :note (plist-get entry :note)
                  :expires (and expires (mevedel-schedule-format-time expires))
                  :status (symbol-name (plist-get entry :status)))
            (and model
                 (not (equal model (and gptel-backend gptel-model
                                        (format "%s:%s" (gptel-backend-name gptel-backend)
                                                (gptel--model-name gptel-model)))))
                 (list :model model))
            (and effort
                 (not (equal effort (and (bound-and-true-p gptel-reasoning-effort)
                                         (format "%s" gptel-reasoning-effort))))
                 (list :effort effort)))))

(defun mevedel-tool-schedule--workspace-json (entry)
  "Return workspace schedule ENTRY as a JSON-serializable plist."
  (let ((policy (plist-get entry :policy))
        (due (plist-get entry :due))
        (expires (plist-get entry :expires-at)))
    (list :id (plist-get entry :id)
          :target "workspace"
          :name (plist-get entry :name)
          :time (and due (mevedel-schedule-format-time due))
          :repeat (mevedel-schedule-repeat-label (plist-get entry :timing))
          :note (plist-get entry :prompt)
          :preset (symbol-name (plist-get entry :preset))
          :model (plist-get policy :model-provider)
          :effort (let ((effort (plist-get policy :reasoning-effort)))
                    (and effort (symbol-name effort)))
          :permission_mode (symbol-name (plist-get policy :permission-mode))
          :expires (and expires (mevedel-schedule-format-time expires))
          :status (symbol-name (plist-get entry :status)))))

(defun mevedel-tool-schedule--result (key value)
  "Return a tool result reporting KEY as VALUE with the current local time."
  (list :result
        (json-serialize
         (list key value
               :now (mevedel-schedule-format-time (mevedel-schedule-now)))
         :null-object nil)))

(defun mevedel-tool-schedule--policy (args previous session)
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
            (let ((resolved (mevedel-model-resolve-workload
                             nil
                             (mevedel-model-parse-selector
                              (or model (plist-get stored :model-provider)))
                             (mevedel-model-parse-effort effort))))
              (list :model-provider (format "%s:%s"
                                            (gptel-backend-name (plist-get resolved :backend))
                                            (gptel--model-name (plist-get resolved :model)))
                    :reasoning-effort (plist-get resolved :effort))))))
    (list :model-provider (plist-get selection :model-provider)
          :reasoning-effort (plist-get selection :reasoning-effort)
          :permission-mode (cond (mode (intern mode))
                                 (stored (plist-get stored :permission-mode))
                                 (t (mevedel-session-permission-mode session))))))

(defun mevedel-tool-schedule--card (entry previous session)
  "Return the card facts for approving workspace schedule ENTRY.
PREVIOUS is the schedule ENTRY changes, or nil for a creation.  A mode above
PREVIOUS's, or for a creation above SESSION's current one, is an escalation."
  (let* ((policy (plist-get entry :policy))
         (mode (plist-get policy :permission-mode))
         (baseline (if previous
                       (plist-get (plist-get previous :policy) :permission-mode)
                     (mevedel-session-permission-mode session)))
         (expires (plist-get entry :expires-at))
         (changed (and previous
                       (cl-loop for key in '(:name :prompt :timing :expires-at :preset :policy)
                                unless (equal (plist-get entry key) (plist-get previous key))
                                collect (substring (symbol-name key) 1)))))
    (list :card-title (if previous
                          (format "Change Workspace Schedule %s" (plist-get entry :id))
                        "Create Workspace Schedule")
          :card-warning
          (when (> (cl-position mode mevedel-tool-schedule--modes)
                   (or (cl-position baseline mevedel-tool-schedule--modes) 0))
            (format "Escalation: %s -> %s for every unattended run" baseline mode))
          :card-fields
          (append
           (and previous (list (cons "Changed" (if changed (string-join changed ", ") "nothing"))))
           (list (cons "Name" (plist-get entry :name))
                 (cons "Timing" (or (mevedel-schedule-repeat-label (plist-get entry :timing))
                                    "once"))
                 (cons "Next run" (mevedel-schedule-format-time (plist-get entry :due)))
                 (cons "Preset" (plist-get entry :preset))
                 (cons "Model" (plist-get policy :model-provider))
                 (cons "Effort" (or (plist-get policy :reasoning-effort) "model default"))
                 (cons "Permission mode" mode)
                 (cons "Overlap" "skip a run while the previous run is still working")
                 (cons "Expires" (if expires (mevedel-schedule-format-time expires) "never"))))
          :card-text (plist-get entry :prompt))))

(defun mevedel-tool-schedule--ask (callback session buffer args &optional previous)
  "Ask through a card to store the workspace schedule ARGS describe.
SESSION is the root session and BUFFER its data buffer.  PREVIOUS is the
stored schedule ARGS change, or nil to create one.  CALLBACK receives the
tool result once the person answers; nothing is stored unless approved."
  (let* ((workspace (mevedel-session-workspace session))
         (preset (plist-get args :preset))
         (entry (mevedel-workspace-schedule-propose
                 (list :name (plist-get args :name) :prompt (plist-get args :note)
                       :when (plist-get args :when) :expires (plist-get args :expires)
                       :preset (and preset (intern preset))
                       :policy (mevedel-tool-schedule--policy args previous session))
                 previous))
         (request mevedel--current-request))
    (mevedel-permission-request
     (append
      (list :kind 'schedule :tool-name "Schedule" :origin "/root"
            :data-buffer buffer :workspace workspace :request request
            :request-id (and request (mevedel-request-id request))
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
                     (mevedel-tool-schedule--result
                      (if previous :changed :created)
                      (mevedel-tool-schedule--workspace-json
                       (if previous
                           (mevedel-workspace-schedule-update
                            workspace (plist-get previous :id)
                            (lambda (current)
                              (unless (equal current previous)
                                (user-error "Schedule %s changed while awaiting approval; propose the change again"
                                            (plist-get previous :id)))
                              entry))
                         (mevedel-workspace-schedule-create workspace entry)))))
                 (error (list :result (format "Error: %s" (error-message-string err))
                              :status 'error))))))
      (mevedel-tool-schedule--card entry previous session))
     session)
    nil))

(defun mevedel-tool-schedule--handle (callback args)
  "Create, list, change or cancel schedules as ARGS request; answer CALLBACK."
  (unless (equal (mevedel-current-origin) "/root")
    (error "Schedule is available only to /root; ask the root to schedule"))
  (let* ((session (or (bound-and-true-p mevedel--session)
                      (error "Schedule needs a mevedel session")))
         (buffer (if (buffer-live-p (mevedel-session-root-buffer session))
                     (mevedel-session-root-buffer session)
                   (current-buffer)))
         (workspace (mevedel-session-workspace session))
         (id (let ((id (plist-get args :id)))
               (and (stringp id) (string-match-p "\\S-" id) (string-trim id))))
         (stored (lambda ()
                   (or (and id (mevedel-workspace-schedule-get workspace id))
                       (user-error "No workspace schedule with id %s" id)))))
    (pcase (plist-get args :action)
      ((and "create" (guard (equal "workspace" (plist-get args :target))))
       (mevedel-tool-schedule--ask callback session buffer args))
      ("change"
       (mevedel-tool-schedule--ask callback session buffer args (funcall stored)))
      ("create"
       (funcall callback
                (mevedel-tool-schedule--result
                 :created
                 (mevedel-tool-schedule--entry-json
                  (mevedel-schedule-create session buffer (plist-get args :when)
                                           (plist-get args :note) (plist-get args :expires)
                                           (plist-get args :model) (plist-get args :effort))))))
      ("list"
       (funcall callback
                (mevedel-tool-schedule--result
                 :schedules
                 (vconcat (mapcar #'mevedel-tool-schedule--entry-json
                                  (mevedel-session-schedules session))
                          (mapcar #'mevedel-tool-schedule--workspace-json
                                  (mevedel-workspace-schedule-list workspace))))))
      ("cancel"
       (unless id
         (user-error "Schedule cancel requires an id"))
       (funcall callback
                (mevedel-tool-schedule--result
                 :cancelled
                 (if (string-prefix-p "s-" id)
                     (let ((entry (funcall stored)))
                       (mevedel-workspace-schedule-delete workspace id)
                       (mevedel-tool-schedule--workspace-json entry))
                   (mevedel-tool-schedule--entry-json
                    (mevedel-schedule-cancel session buffer id))))))
      (action (user-error "Unknown Schedule action: %S" action)))
    nil))

(defun mevedel-tool-schedule--register ()
  "Register the Schedule tool."
  (mevedel-define-tool
   :name "Schedule"
   :description "Wake this conversation later, or run a prompt in a new session on a workspace schedule; list, change or cancel schedules."
   :prompt-file "prompts/tools/schedule.md"
   :handler #'mevedel-tool-schedule--handle
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
                   "For create or change: optional duration or local time after which a run is dropped.")
          (name string :optional
                "For a workspace schedule: a short name; runs are named after it.")
          (preset string :optional
                  "For a workspace schedule: the base preset of its runs. Default mevedel-implement.")
          (permission_mode string :optional
                           "For a workspace schedule: the permission mode of its runs. Default this session's."
                           :enum ["ask" "edits" "full-auto"])
          (model string :optional
                 "For create or change: run on this model: a tier (fast, balanced, strong) or BACKEND:MODEL. Omit for the session's.")
          (effort string :optional
                  "For create or change: reasoning effort. Omit for the session's.")
          (id string :optional "For change or cancel: the schedule id."))
   :read-only-p t
   :groups (util)))

(provide 'mevedel-tool-schedule)
;;; mevedel-tool-schedule.el ends here
