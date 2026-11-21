;;; test-niwa-tool-schedule.el --- Schedule tool through the pipeline -*- lexical-binding: t -*-

;;; Commentary:
;; One-shot wake-ups created, listed and cancelled through ToolCall, with the
;; clock injected and persistence read back through the session codec.

;;; Code:

(require 'niwa)
(require 'niwa-schedule)
(require 'niwa-tool-schedule)
(require 'niwa-tools)
(require 'niwa-tool-ptc)
(require 'niwa-pipeline)
(require 'niwa-session-codec)
(require 'niwa-workspace-identity)
(require 'niwa-workspace-schedule)
(require 'niwa-permission-mode)
(require 'niwa-presets)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))

(defconst niwa-tool-schedule-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro niwa-tool-schedule-test--with-request (&rest body)
  "Run BODY in a persisted root session with a root FSM and a fixed clock.
`now' holds the clock's current seconds; set it to advance time."
  (declare (indent 0) (debug t))
  `(let* ((gptel--known-backends nil)
          (niwa-workspace-identity--cache (make-hash-table :test #'equal))
          (root (make-temp-file "niwa-schedule-tool-" t))
          (niwa-user-dir (file-name-as-directory (file-name-concat root "user")))
          (session (niwa-skills-test--make-session "schedules" root))
          (now niwa-tool-schedule-test--now)
          (niwa-schedule-clock (lambda () now))
          ;; An ordinary buffer: temporary buffers skip the kill hook that
          ;; unregisters a closed root.
          (buffer (generate-new-buffer " *schedule-root*"))
          (zone (getenv "TZ")))
     (set-time-zone-rule "Europe/Zurich")
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       niwa--session session)
           (niwa-workspace-identity-ensure root)
           (setf (niwa-session-save-path session) root
                 (niwa-session-root-buffer session) (current-buffer))
           (let* ((fsm (gptel-make-fsm
                        :info (list :buffer (current-buffer)
                                    :backend (gptel-make-openai "schedule-test"
                                               :key "test" :models '(test))
                                    :data (list :messages []))))
                  (niwa-tools--current-engine fsm))
             (setq-local niwa--current-request
                         (niwa-request--create :session session :fsm fsm))
             (niwa-tool-schedule--register)
             (setf (niwa-session-tool-catalog session)
                   '((("niwa" "Schedule") . "Schedule wake-ups")))
             ,@body))
       (kill-buffer buffer)
       ;; A closed session's pending wake-ups stay scheduled; forget them.
       (setq niwa-schedule--closed nil)
       (niwa-schedule--rearm)
       (should-not niwa-schedule--timer)
       (set-time-zone-rule zone)
       (delete-directory root t))))

(defun niwa-tool-schedule-test--call (expression)
  "Run ToolCall EXPRESSION through the pipeline and return its outcome."
  (let (outcome)
    (niwa-tool-ptc--handler (lambda (value) (setq outcome value))
                               (list :expression expression))
    ;; Nested calls run from timers after the dispatch binding unwinds.
    (let ((niwa-tools--current-engine nil))
      (niwa-test--await 5 "Schedule did not answer" outcome))
    outcome))

(defun niwa-tool-schedule-test--persisted (root session)
  "Return the schedules SESSION's sidecar under ROOT restores."
  (niwa-session-schedules
   (plist-get (niwa-session-codec-deserialize
               (niwa-session-codec-read (file-name-concat root "session.meta.el"))
               (niwa-session-workspace session))
              :session)))

(niwa-deftest niwa-tool-schedule--handle ()
  ,test
  (test)
  :doc "creates a relative one-shot wake-up and persists it with the session"
  (niwa-tool-schedule-test--with-request
    (let* ((outcome (niwa-tool-schedule-test--call
                     "(Schedule :action \"create\" :when \"20m\" :note \"Check CI; tell the user only if it failed\")"))
           (entry (car (niwa-session-schedules session))))
      (should (eq 'success (plist-get outcome :status)))
      (should (= 1 (length (niwa-session-schedules session))))
      (should (equal "Check CI; tell the user only if it failed"
                     (plist-get entry :note)))
      (should (= (+ niwa-tool-schedule-test--now 1200) (plist-get entry :due)))
      (should (string-search "2026-11-20T13:20:00+01:00" (plist-get outcome :result)))
      (should (string-search (plist-get entry :id) (plist-get outcome :result)))
      (should (equal (list entry)
                     (niwa-tool-schedule-test--persisted root session)))))

  :doc "stores a wake-up's model and effort, lists them only when they differ from the session's, and rejects bad ones"
  (niwa-tool-schedule-test--with-request
    (put 'schedule-test-cheap :reasoning-effort '(member low high))
    (gptel-make-openai "schedule-models" :key "test"
                       :models '(schedule-test-cheap schedule-test-main))
    (setq-local gptel-backend (gptel-get-backend "schedule-models")
                gptel-model 'schedule-test-main)
    (let ((outcome (niwa-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"20m\" :note \"poll\" :model \"schedule-models:schedule-test-cheap\" :effort \"low\")"))
          (entry (car (niwa-session-schedules session))))
      (ert-info ((format "%S" outcome))
        (should (eq 'success (plist-get outcome :status))))
      (should (string-search "schedule-test-cheap" (plist-get outcome :result)))
      (should (equal '("schedule-models:schedule-test-cheap" "low")
                     (list (plist-get entry :model) (plist-get entry :effort))))
      (should (equal (niwa-session-schedules session)
                     (niwa-tool-schedule-test--persisted root session))))
    ;; The session's own model is not worth listing.
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"1h\" :note \"same\" :model \"schedule-models:schedule-test-main\")")
    (let ((listed (plist-get (niwa-tool-schedule-test--call "(Schedule :action \"list\")")
                             :result)))
      (should (string-match-p "\"model\":\"schedule-models:schedule-test-cheap\"" listed))
      (should (string-match-p "\"effort\":\"low\"" listed))
      (should (= 2 (length (split-string listed "\"model\":")))))
    (dolist (case '(("(Schedule :action \"create\" :when \"5m\" :note \"x\" :model \"schedule-models:missing\")" . "not registered")
                    ("(Schedule :action \"create\" :when \"5m\" :note \"x\" :model \"schedule-models:schedule-test-cheap\" :effort \"extreme\")" . "unsupported")
                    ("(Schedule :action \"create\" :when \"5m\" :note \"x\" :effort \"low\")" . "does not support")))
      (ert-info ((car case))
        (let ((outcome (niwa-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should (= 2 (length (niwa-session-schedules session)))))

  :doc "list shows a tier wake-up's resolved model and effort, only where they differ from the session's"
  (niwa-tool-schedule-test--with-request
    (put 'schedule-test-cheap :reasoning-effort '(member low high))
    (gptel-make-openai "schedule-models" :key "test"
                       :models '(schedule-test-cheap schedule-test-main))
    (setq-local gptel-backend (gptel-get-backend "schedule-models")
                gptel-model 'schedule-test-main
                gptel-reasoning-effort nil)
    (let ((niwa-model-tiers '((fast :provider "schedule-models:schedule-test-cheap" :effort low)
                                 (strong))))
      (niwa-tool-schedule-test--call "(Schedule :action \"create\" :when \"20m\" :note \"cheap\" :model \"fast\")")
      (niwa-tool-schedule-test--call "(Schedule :action \"create\" :when \"1h\" :note \"same\" :model \"strong\")")
      (let ((listed (plist-get (json-parse-string
                                (plist-get (niwa-tool-schedule-test--call "(Schedule :action \"list\")")
                                           :result)
                                :object-type 'plist :array-type 'list)
                               :schedules)))
        (should (equal '(("schedule-models:schedule-test-cheap" "low") (nil nil))
                       (mapcar (lambda (entry) (list (plist-get entry :model) (plist-get entry :effort)))
                               listed))))))

  :doc "accepts an absolute local time and an expiry after it"
  (niwa-tool-schedule-test--with-request
    (let ((outcome (niwa-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"2026-11-21 10:00\" :note \"Push the release branch\" :expires \"2026-11-21 12:00\")"))
          (entry (car (niwa-session-schedules session))))
      (should (eq 'success (plist-get outcome :status)))
      ;; 2026-11-21T09:00:00Z and 11:00:00Z.
      (should (= 1795251600 (plist-get entry :due)))
      (should (= 1795258800 (plist-get entry :expires-at)))))

  :doc "a recurring wake-up persists its timing and expires after 7 days by default"
  (niwa-tool-schedule-test--with-request
    (let* ((outcome (niwa-tool-schedule-test--call
                     "(Schedule :action \"create\" :when \"every 20m\" :note \"Babysit PR 412\")"))
           (cron (niwa-tool-schedule-test--call
                  "(Schedule :action \"create\" :when \"0 9 * * 1-5\" :note \"Morning summary\" :expires \"30d\")"))
           (entries (niwa-session-schedules session)))
      (should (eq 'success (plist-get outcome :status)))
      (should (string-search "\"repeat\":\"every 20m 00s\"" (plist-get outcome :result)))
      (should (string-search "\"repeat\":\"0 9 * * 1-5\"" (plist-get cron :result)))
      (should (equal '((:kind interval :seconds 1200) (:kind cron :expression "0 9 * * 1-5"))
                     (mapcar (lambda (entry) (plist-get entry :timing)) entries)))
      ;; 7 and 30 days after 2026-11-20 13:00.
      (should (equal '(1795780800 1797768000)
                     (mapcar (lambda (entry) (plist-get entry :expires-at)) entries)))
      (should (equal entries (niwa-tool-schedule-test--persisted root session)))))

  :doc "short intervals, cron names, a recurring expiry past 30 days and a recurring expiry are tool errors"
  (niwa-tool-schedule-test--with-request
    (dolist (case '(("(Schedule :action \"create\" :when \"every 30s\" :note \"x\")" . "at least one minute")
                    ("(Schedule :action \"create\" :when \"0 9 * * MON\" :note \"x\")" . "names")
                    ("(Schedule :action \"create\" :when \"every 1h\" :note \"x\" :expires \"31d\")" . "30 days")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"x\" :expires \"every 1h\")" . "expiry")))
      (ert-info ((car case))
        (let ((outcome (niwa-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should-not (niwa-session-schedules session)))

  :doc "invalid timing, a past time, a blank note, a late expiry and a 51st wake-up are tool errors"
  (niwa-tool-schedule-test--with-request
    (dolist (case '(("(Schedule :action \"create\" :when \"soon\" :note \"x\")" . "Unrecognized")
                    ("(Schedule :action \"create\" :when \"0m\" :note \"x\")" . "positive")
                    ("(Schedule :action \"create\" :when \"2026-11-20 12:59\" :note \"x\")" . "not in the future")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"  \")" . "blank")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"x\" :expires \"10m\")" . "expiry")
                    ("(Schedule :action \"cancel\" :id \"w-missing\")" . "No schedule")))
      (ert-info ((car case))
        (let ((outcome (niwa-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should-not (niwa-session-schedules session))
    (setf (niwa-session-schedules session)
          (cl-loop for index below 50
                   collect (list :id (format "w-%d" index) :note "x"
                                 :timing (list :kind 'once :at (+ now 600))
                                 :created-at now :due (+ now 600)
                                 :expires-at nil :status 'pending)))
    (let ((outcome (niwa-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"20m\" :note \"one too many\")")))
      (should (string-search "50 pending" (plist-get outcome :result)))
      (should (= 50 (length (niwa-session-schedules session))))))

  :doc "lists and cancels wake-ups, persisting the cancellation"
  (niwa-tool-schedule-test--with-request
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"20m\" :note \"first\")")
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"1h\" :note \"second\")")
    (let* ((listed (json-parse-string
                    (plist-get (niwa-tool-schedule-test--call
                                "(Schedule :action \"list\")")
                               :result)
                    :object-type 'plist :array-type 'list))
           (first (car (plist-get listed :schedules))))
      (should (equal "2026-11-20T13:00:00+01:00" (plist-get listed :now)))
      (should (equal '("first" "second")
                     (mapcar (lambda (entry) (plist-get entry :note))
                             (plist-get listed :schedules))))
      (should (eq 'success
                  (plist-get (niwa-tool-schedule-test--call
                              (format "(Schedule :action \"cancel\" :id %S)"
                                      (plist-get first :id)))
                             :status)))
      (should (equal '("second")
                     (mapcar (lambda (entry) (plist-get entry :note))
                             (niwa-tool-schedule-test--persisted root session)))))))

(niwa-deftest niwa-schedule-change ()
  ,test
  (test)
  :doc "change rewrites a pending wake-up's timing, note, expiry, model and effort, and persists it"
  (niwa-tool-schedule-test--with-request
    (put 'schedule-test-cheap :reasoning-effort '(member low high))
    (gptel-make-openai "schedule-models" :key "test"
                       :models '(schedule-test-cheap schedule-test-main))
    (setq-local gptel-backend (gptel-get-backend "schedule-models")
                gptel-model 'schedule-test-main)
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"20m\" :note \"first\" :expires \"1h\")")
    (let* ((id (plist-get (car (niwa-session-schedules session)) :id))
           (outcome (niwa-tool-schedule-test--call
                     (format "(Schedule :action \"change\" :id %S :when \"1h\" :note \"second\" :expires \"2h\" :model \"schedule-models:schedule-test-cheap\" :effort \"low\")" id)))
           (entry (car (niwa-session-schedules session))))
      (ert-info ((format "%S" outcome))
        (should (eq 'success (plist-get outcome :status))))
      (should (string-search "\"changed\"" (plist-get outcome :result)))
      (should (equal (list id "second" (+ now 3600) (+ now 7200)
                           "schedule-models:schedule-test-cheap" "low")
                     (mapcar (lambda (key) (plist-get entry key))
                             '(:id :note :due :expires-at :model :effort))))
      (should (equal (list entry) (niwa-tool-schedule-test--persisted root session)))
      ;; Omitted fields stay; never removes a one-shot's expiry.
      (niwa-tool-schedule-test--call
       (format "(Schedule :action \"change\" :id %S :expires \"never\")" id))
      (let ((changed (car (niwa-session-schedules session))))
        (should (equal '("second" nil "low")
                       (list (plist-get changed :note) (plist-get changed :expires-at)
                             (plist-get changed :effort))))
        (should (= (+ now 3600) (plist-get changed :due))))
      ;; Turning it into a repeat applies the default recurring expiry.
      (niwa-tool-schedule-test--call
       (format "(Schedule :action \"change\" :id %S :when \"every 20m\")" id))
      (should (= (+ now (* 7 86400))
                 (plist-get (car (niwa-session-schedules session)) :expires-at)))))

  :doc "a change is validated like a creation and leaves the wake-up as it was when refused"
  (niwa-tool-schedule-test--with-request
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"every 20m\" :note \"poll\")")
    (let* ((before (niwa-session-schedules session))
           (id (plist-get (car before) :id)))
      (dolist (case `((,(format "(Schedule :action \"change\" :id %S :when \"every 30s\")" id) . "at least one minute")
                      (,(format "(Schedule :action \"change\" :id %S :note \" \")" id) . "blank")
                      (,(format "(Schedule :action \"change\" :id %S :expires \"31d\")" id) . "30 days")
                      (,(format "(Schedule :action \"change\" :id %S :expires \"never\")" id) . "30 days")
                      (,(format "(Schedule :action \"change\" :id %S :model \"nobody:none\")" id) . "not known")
                      ("(Schedule :action \"change\" :id \"w-missing\" :note \"x\")" . "No pending wake-up")
                      ("(Schedule :action \"change\" :note \"x\")" . "requires an id")))
        (ert-info ((car case))
          (let ((outcome (niwa-tool-schedule-test--call (car case))))
            (should-not (eq 'success (plist-get outcome :status)))
            (should (string-search (cdr case) (plist-get outcome :result))))))
      (should (equal before (niwa-session-schedules session)))
      (should (equal before (niwa-tool-schedule-test--persisted root session)))))

  :doc "the 30-day cap counts from creation, so changes cannot extend a recurring wake-up"
  (niwa-tool-schedule-test--with-request
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"every 5d\" :note \"poll\" :expires \"30d\")")
    (let* ((before (niwa-session-schedules session))
           (id (plist-get (car before) :id)))
      (setq now (+ now 86400))
      (let ((outcome (niwa-tool-schedule-test--call
                      (format "(Schedule :action \"change\" :id %S :expires \"30d\")" id))))
        (should-not (eq 'success (plist-get outcome :status)))
        (should (string-search "30 days" (plist-get outcome :result))))
      (should (equal before (niwa-session-schedules session)))
      ;; A new timing without an expiry keeps the default within the cap.
      (niwa-tool-schedule-test--call
       (format "(Schedule :action \"change\" :id %S :when \"every 1d\" :expires \"29d\")" id))
      (should (= (+ now (* 29 86400))
                 (plist-get (car (niwa-session-schedules session)) :expires-at))))))

(niwa-deftest niwa-tools-visibility/schedule ()
  ,test
  (test)
  :doc "Schedule is offered to the root and refused to a child agent"
  (niwa-tool-schedule-test--with-request
    (should (funcall (niwa-tools-visibility fsm) "Schedule"))
    (let ((niwa--agent-invocation
           (niwa-agent-invocation--create :path "/root/worker"
                                             :parent-session session)))
      (setq-local niwa--current-request nil)
      (should-not (funcall (niwa-tools-visibility niwa--agent-invocation)
                           "Schedule"))
      (should-error (niwa-tool-schedule--handle
                     #'ignore '(:action "create" :when "20m" :note "x")))
      (should-not (niwa-session-schedules session)))))

(defmacro niwa-tool-schedule-test--with-workspace-card (steps &rest body)
  "Run BODY in a root request whose cards a person answers with STEPS.
The session runs on `ws-models:ws-main' at effort `low'; the standard
presets are registered.  `workspace' is the session's workspace."
  (declare (indent 1) (debug t))
  `(niwa-tool-schedule-test--with-request
     (let ((niwa-preset--registry niwa-preset--registry)
           (gptel--known-presets gptel--known-presets)
           (workspace (niwa-session-workspace session)))
       (niwa-tools-register)
       (niwa--define-presets)
       (put 'ws-main :reasoning-effort '(member low high))
       (gptel-make-openai "ws-models" :key "test" :models '(ws-main ws-cheap))
       (setq-local gptel-backend (gptel-get-backend "ws-models")
                   gptel-model 'ws-main
                   gptel-reasoning-effort 'low)
       (unwind-protect
           (niwa-pq-test--answering (,steps :here t)
             ,@body)
         (setq niwa-schedule--workspaces nil)))))

(defun niwa-tool-schedule-test--card-texts ()
  "Return the texts of the cards answered so far, oldest first."
  (mapcar (lambda (card) (plist-get card :text)) (reverse niwa-pq-test--driven)))

(niwa-deftest niwa-tool-schedule--handle/workspace ()
  ,test
  (test)
  :doc "creating a workspace schedule in Full Access still shows the card, and approving stores it"
  (niwa-tool-schedule-test--with-workspace-card '(allow-once)
    (setf (niwa-session-permission-mode session) 'full-auto)
    (let* ((outcome (niwa-tool-schedule-test--call
                     "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage new issues; Notify only release blockers.\")"))
           (stored (car (niwa-workspace-schedule-list workspace)))
           (card (car (niwa-tool-schedule-test--card-texts))))
      (should (eq 'success (plist-get outcome :status)))
      (should (= 1 (length niwa-pq-test--driven)))
      (dolist (text '("Triage" "0 7 * * 1-5" "2026-11-23T07:00:00+01:00" "niwa-implement"
                      "ws-models:ws-main" "low" "full-auto"
                      "Triage new issues; Notify only release blockers."
                      "previous run" "never"))
        (ert-info (text) (should (string-search text card))))
      (should-not (string-search "Escalation" card))
      (should (equal '(:model-provider "ws-models:ws-main" :reasoning-effort low
                                       :permission-mode full-auto)
                     (plist-get stored :policy)))
      (should (eq 'niwa-implement (plist-get stored :preset)))
      (should-not (plist-get stored :expires-at))
      (should (string-search (plist-get stored :id) (plist-get outcome :result)))))

  :doc "a mode above the creating session's is flagged, and denying creates nothing"
  (niwa-tool-schedule-test--with-workspace-card '(deny-once)
    (let ((outcome (niwa-tool-schedule-test--call
                    "(Schedule :action \"create\" :target \"workspace\" :name \"Nightly\" :when \"every 1d\" :note \"Run the slow suite.\" :permission_mode \"full-auto\" :model \"ws-models:ws-cheap\")")))
      (should-not (eq 'success (plist-get outcome :status)))
      (should (string-search "declined" (plist-get outcome :result)))
      (should (string-search "Escalation: ask -> full-auto"
                             (car (niwa-tool-schedule-test--card-texts))))
      (should-not (niwa-workspace-schedule-list workspace))
      (should-not niwa-schedule--workspaces)))

  :doc "every change shows a new card that flags an escalation; denial leaves the schedule unchanged"
  (niwa-tool-schedule-test--with-workspace-card
      (lambda (number _card) (if (= number 2) '(deny-once) '(allow-once)))
    (let* ((created (niwa-tool-schedule-test--call
                     "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage.\")"))
           (id (plist-get (car (niwa-workspace-schedule-list workspace)) :id))
           (before (niwa-workspace-schedule-get workspace id)))
      (should (eq 'success (plist-get created :status)))
      (let ((denied (niwa-tool-schedule-test--call
                     (format "(Schedule :action \"change\" :id %S :permission_mode \"full-auto\")" id))))
        (should-not (eq 'success (plist-get denied :status)))
        (should (string-search "Escalation: ask -> full-auto"
                               (nth 1 (niwa-tool-schedule-test--card-texts))))
        (should (equal before (niwa-workspace-schedule-get workspace id))))
      (let ((changed (niwa-tool-schedule-test--call
                      (format "(Schedule :action \"change\" :id %S :note \"Triage and label.\" :when \"0 8 * * 1-5\")" id)))
            (after (niwa-workspace-schedule-get workspace id)))
        (should (eq 'success (plist-get changed :status)))
        (should (= 3 (length niwa-pq-test--driven)))
        (should (string-search "Triage and label." (nth 2 (niwa-tool-schedule-test--card-texts))))
        (should (equal "Triage and label." (plist-get after :prompt)))
        (should (equal "2026-11-23T08:00:00+01:00"
                       (niwa-schedule-format-time (plist-get after :due))))
        (should (equal (plist-get before :policy) (plist-get after :policy))))))

  :doc "at capacity, a creation or a change reactivating a finished schedule fails before any card"
  (niwa-tool-schedule-test--with-workspace-card (lambda (_number _card) '(allow-once))
    (let ((niwa-workspace-schedule-max 1))
      (niwa-tool-schedule-test--call
       "(Schedule :action \"create\" :target \"workspace\" :name \"Once\" :when \"30m\" :note \"Once.\")")
      (let ((id (plist-get (car (niwa-workspace-schedule-list workspace)) :id)))
        (niwa-workspace-schedule-update
         workspace id (lambda (entry) (plist-put (plist-put entry :status 'finished) :due nil)))
        (niwa-tool-schedule-test--call
         "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage.\")")
        (should (= 2 (length niwa-pq-test--driven)))
        (dolist (expression (list (format "(Schedule :action \"change\" :id %S :when \"1h\")" id)
                                  "(Schedule :action \"create\" :target \"workspace\" :name \"More\" :when \"1h\" :note \"More.\")"))
          (ert-info (expression)
            (let ((outcome (niwa-tool-schedule-test--call expression)))
              (should-not (eq 'success (plist-get outcome :status)))
              (should (string-search "already has 1" (plist-get outcome :result))))))
        (should (= 2 (length niwa-pq-test--driven)))
        (should (eq 'finished (plist-get (niwa-workspace-schedule-get workspace id) :status))))))

  :doc "a change can remove an expiry, and the card shows it"
  (niwa-tool-schedule-test--with-workspace-card (lambda (_number _card) '(allow-once))
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage.\" :expires \"30d\")")
    (let ((id (plist-get (car (niwa-workspace-schedule-list workspace)) :id)))
      (should (plist-get (niwa-workspace-schedule-get workspace id) :expires-at))
      (should (eq 'success (plist-get (niwa-tool-schedule-test--call
                                       (format "(Schedule :action \"change\" :id %S :expires \"never\")" id))
                                      :status)))
      (let ((card (nth 1 (niwa-tool-schedule-test--card-texts))))
        (should (string-search "expires-at" card))
        (should (string-search "never" card)))
      (should-not (plist-get (niwa-workspace-schedule-get workspace id) :expires-at))))

  :doc "list shows workspace schedules beside wake-ups, and cancel deletes one without a card"
  (niwa-tool-schedule-test--with-workspace-card '(allow-once)
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"20m\" :note \"wake-up\")")
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage.\")")
    (let* ((listed (json-parse-string
                    (plist-get (niwa-tool-schedule-test--call "(Schedule :action \"list\")")
                               :result)
                    :object-type 'plist :array-type 'list))
           (entries (plist-get listed :schedules))
           (workspace-entry (cadr entries)))
      (should (equal '("session" "workspace")
                     (mapcar (lambda (entry) (plist-get entry :target)) entries)))
      (should (equal "Triage" (plist-get workspace-entry :name)))
      (should (equal "ask" (plist-get workspace-entry :permission_mode)))
      (should (eq 'success
                  (plist-get (niwa-tool-schedule-test--call
                              (format "(Schedule :action \"cancel\" :id %S)"
                                      (plist-get workspace-entry :id)))
                             :status)))
      (should (= 1 (length niwa-pq-test--driven)))
      (should-not (niwa-workspace-schedule-list workspace))))

  :doc "an id that is not a canonical workspace schedule key never reaches the store"
  (niwa-tool-schedule-test--with-workspace-card '(allow-once)
    (niwa-tool-schedule-test--call
     "(Schedule :action \"create\" :target \"workspace\" :name \"Triage\" :when \"0 7 * * 1-5\" :note \"Triage.\")")
    (let* ((stored (car (niwa-workspace-schedule-list workspace)))
           (store (niwa-workspace-schedule--path workspace))
           (decoy (file-name-concat (niwa-workspace-state-dir workspace) "decoy"))
           (alias (file-name-concat store "s-aaaaaa")))
      ;; A schedule record outside the store, and one filed under another id.
      (copy-directory (file-name-concat store (plist-get stored :id)) decoy)
      (copy-directory (file-name-concat store (plist-get stored :id)) alias)
      (dolist (id (list "s-123456/../../decoy" "../decoy" "s-ABCDEF"))
        (ert-info (id)
          (dolist (action '("cancel" "change"))
            (let ((outcome (niwa-tool-schedule-test--call
                            (format "(Schedule :action %S :id %S :note \"x\")" action id))))
              (should-not (eq 'success (plist-get outcome :status)))))))
      (should (file-directory-p decoy))
      (should-not (niwa-workspace-schedule-get workspace "s-aaaaaa"))
      (should (equal (list stored) (niwa-workspace-schedule-list workspace)))
      (should (= 1 (length niwa-pq-test--driven))))))

(provide 'test-niwa-tool-schedule)
;;; test-niwa-tool-schedule.el ends here
