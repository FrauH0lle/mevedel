;;; mevedel-tool-schedule.el --- Schedule tool -*- lexical-binding: t -*-

;;; Commentary:

;; One model tool creates, lists and cancels the root conversation's one-shot
;; and recurring wake-ups.  It is visible to root requests only; a child agent
;; asks the root to schedule instead.  `mevedel-schedule' owns timing, storage
;; and firing.

;;; Code:

(require 'mevedel-schedule)
(require 'mevedel-tool-registry)

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))

(defun mevedel-tool-schedule--entry-json (entry)
  "Return wake-up ENTRY as a JSON-serializable plist.
Its model and effort are listed only when they differ from the session's."
  (let ((expires (plist-get entry :expires-at))
        (timing (plist-get entry :timing))
        (model (plist-get entry :model))
        (effort (plist-get entry :effort)))
    (append (list :id (plist-get entry :id)
                  :time (mevedel-schedule-format-time (plist-get entry :due))
                  :repeat (pcase (plist-get timing :kind)
                            ('interval (concat "every " (mevedel--duration-label
                                                         (plist-get timing :seconds))))
                            ('cron (plist-get timing :expression)))
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

(defun mevedel-tool-schedule--result (key value)
  "Return a tool result reporting KEY as VALUE with the current local time."
  (list :result
        (json-serialize
         (list key value
               :now (mevedel-schedule-format-time (mevedel-schedule-now)))
         :null-object nil)))

(defun mevedel-tool-schedule--handle (args)
  "Create, list or cancel the root session's wake-ups as ARGS request."
  (unless (equal (mevedel-current-origin) "/root")
    (error "Schedule is available only to /root; ask the root to schedule"))
  (let* ((session (or (bound-and-true-p mevedel--session)
                      (error "Schedule needs a mevedel session")))
         (buffer (if (buffer-live-p (mevedel-session-root-buffer session))
                     (mevedel-session-root-buffer session)
                   (current-buffer))))
    (pcase (plist-get args :action)
      ("create"
       (mevedel-tool-schedule--result
        :created
        (mevedel-tool-schedule--entry-json
         (mevedel-schedule-create session buffer (plist-get args :when)
                                  (plist-get args :note) (plist-get args :expires)
                                  (plist-get args :model) (plist-get args :effort)))))
      ("list"
       (mevedel-tool-schedule--result
        :schedules
        (vconcat (mapcar #'mevedel-tool-schedule--entry-json
                         (mevedel-session-schedules session)))))
      ("cancel"
       (let ((id (plist-get args :id)))
         (unless (and (stringp id) (string-match-p "\\S-" id))
           (user-error "Schedule cancel requires an id"))
         (mevedel-tool-schedule--result
          :cancelled
          (mevedel-tool-schedule--entry-json
           (mevedel-schedule-cancel session buffer (string-trim id))))))
      (action (user-error "Unknown Schedule action: %S" action)))))

(defun mevedel-tool-schedule--register ()
  "Register the Schedule tool."
  (mevedel-define-tool
   :name "Schedule"
   :description "Schedule a wake-up of this conversation with a note for later, or list or cancel wake-ups."
   :prompt-file "prompts/tools/schedule.md"
   :handler #'mevedel-tool-schedule--handle
   :args ((action string :required "What to do."
                  :enum ["create" "list" "cancel"])
          (when string :optional
                "For create: a duration such as 20m or a local time such as 2026-11-21 10:00; every 20m or a cron expression such as 0 9 * * 1-5 to repeat.")
          (note string :optional
                "For create: what the woken turn should do, written for yourself.")
          (expires string :optional
                   "For create: optional duration or local time after which an undelivered wake-up is dropped.")
          (model string :optional
                 "For create: run the woken turn on this model: a tier (fast, balanced, strong) or BACKEND:MODEL. Omit for the session's.")
          (effort string :optional
                  "For create: reasoning effort for the woken turn. Omit for the session's.")
          (id string :optional "For cancel: the wake-up id."))
   :read-only-p t
   :groups (util)))

(provide 'mevedel-tool-schedule)
;;; mevedel-tool-schedule.el ends here
