;;; test-mevedel-tool-schedule.el --- Schedule tool through the pipeline -*- lexical-binding: t -*-

;;; Commentary:
;; One-shot wake-ups created, listed and cancelled through ToolCall, with the
;; clock injected and persistence read back through the session codec.

;;; Code:

(require 'mevedel)
(require 'mevedel-schedule)
(require 'mevedel-tool-schedule)
(require 'mevedel-tools)
(require 'mevedel-tool-ptc)
(require 'mevedel-pipeline)
(require 'mevedel-session-codec)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defconst mevedel-tool-schedule-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defmacro mevedel-tool-schedule-test--with-request (&rest body)
  "Run BODY in a persisted root session with a root FSM and a fixed clock.
`now' holds the clock's current seconds; set it to advance time."
  (declare (indent 0) (debug t))
  `(let* ((gptel--known-backends nil)
          (mevedel-workspace-identity--cache (make-hash-table :test #'equal))
          (root (make-temp-file "mevedel-schedule-tool-" t))
          (session (mevedel-skills-test--make-session "schedules" root))
          (now mevedel-tool-schedule-test--now)
          (mevedel-schedule-clock (lambda () now))
          ;; An ordinary buffer: temporary buffers skip the kill hook that
          ;; unregisters a closed root.
          (buffer (generate-new-buffer " *schedule-root*"))
          (zone (getenv "TZ")))
     (set-time-zone-rule "Europe/Zurich")
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       mevedel--session session)
           (mevedel-workspace-identity-ensure root)
           (setf (mevedel-session-save-path session) root
                 (mevedel-session-root-buffer session) (current-buffer))
           (let* ((fsm (gptel-make-fsm
                        :info (list :buffer (current-buffer)
                                    :backend (gptel-make-openai "schedule-test"
                                               :key "test" :models '(test))
                                    :data (list :messages []))))
                  (mevedel-tools--current-engine fsm))
             (setq-local mevedel--current-request
                         (mevedel-request--create :session session :fsm fsm))
             (mevedel-tool-schedule--register)
             (setf (mevedel-session-tool-catalog session)
                   '((("mevedel" "Schedule") . "Schedule wake-ups")))
             ,@body))
       (kill-buffer buffer)
       ;; Killing the root buffer unregisters it; nothing may stay armed.
       (should-not mevedel-schedule--timer)
       (set-time-zone-rule zone)
       (delete-directory root t))))

(defun mevedel-tool-schedule-test--call (expression)
  "Run ToolCall EXPRESSION through the pipeline and return its outcome."
  (let (outcome)
    (mevedel-tool-ptc--handler (lambda (value) (setq outcome value))
                               (list :expression expression))
    ;; Nested calls run from timers after the dispatch binding unwinds.
    (let ((mevedel-tools--current-engine nil))
      (mevedel-test--await 5 "Schedule did not answer" outcome))
    outcome))

(defun mevedel-tool-schedule-test--persisted (root session)
  "Return the schedules SESSION's sidecar under ROOT restores."
  (mevedel-session-schedules
   (plist-get (mevedel-session-codec-deserialize
               (mevedel-session-codec-read (file-name-concat root "session.meta.el"))
               (mevedel-session-workspace session))
              :session)))

(mevedel-deftest mevedel-tool-schedule--handle ()
  ,test
  (test)
  :doc "creates a relative one-shot wake-up and persists it with the session"
  (mevedel-tool-schedule-test--with-request
    (let* ((outcome (mevedel-tool-schedule-test--call
                     "(Schedule :action \"create\" :when \"20m\" :note \"Check CI; tell the user only if it failed\")"))
           (entry (car (mevedel-session-schedules session))))
      (should (eq 'success (plist-get outcome :status)))
      (should (= 1 (length (mevedel-session-schedules session))))
      (should (equal "Check CI; tell the user only if it failed"
                     (plist-get entry :note)))
      (should (= (+ mevedel-tool-schedule-test--now 1200) (plist-get entry :due)))
      (should (string-search "2026-11-20T13:20:00+01:00" (plist-get outcome :result)))
      (should (string-search (plist-get entry :id) (plist-get outcome :result)))
      (should (equal (list entry)
                     (mevedel-tool-schedule-test--persisted root session)))))

  :doc "stores a wake-up's model and effort, lists them only when they differ from the session's, and rejects bad ones"
  (mevedel-tool-schedule-test--with-request
    (put 'schedule-test-cheap :reasoning-effort '(member low high))
    (gptel-make-openai "schedule-models" :key "test"
                       :models '(schedule-test-cheap schedule-test-main))
    (setq-local gptel-backend (gptel-get-backend "schedule-models")
                gptel-model 'schedule-test-main)
    (let ((outcome (mevedel-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"20m\" :note \"poll\" :model \"schedule-models:schedule-test-cheap\" :effort \"low\")"))
          (entry (car (mevedel-session-schedules session))))
      (ert-info ((format "%S" outcome))
        (should (eq 'success (plist-get outcome :status))))
      (should (string-search "schedule-test-cheap" (plist-get outcome :result)))
      (should (equal '("schedule-models:schedule-test-cheap" "low")
                     (list (plist-get entry :model) (plist-get entry :effort))))
      (should (equal (mevedel-session-schedules session)
                     (mevedel-tool-schedule-test--persisted root session))))
    ;; The session's own model is not worth listing.
    (mevedel-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"1h\" :note \"same\" :model \"schedule-models:schedule-test-main\")")
    (let ((listed (plist-get (mevedel-tool-schedule-test--call "(Schedule :action \"list\")")
                             :result)))
      (should (string-match-p "\"model\":\"schedule-models:schedule-test-cheap\"" listed))
      (should (string-match-p "\"effort\":\"low\"" listed))
      (should (= 2 (length (split-string listed "\"model\":")))))
    (dolist (case '(("(Schedule :action \"create\" :when \"5m\" :note \"x\" :model \"schedule-models:missing\")" . "not registered")
                    ("(Schedule :action \"create\" :when \"5m\" :note \"x\" :model \"schedule-models:schedule-test-cheap\" :effort \"extreme\")" . "unsupported")
                    ("(Schedule :action \"create\" :when \"5m\" :note \"x\" :effort \"low\")" . "does not support")))
      (ert-info ((car case))
        (let ((outcome (mevedel-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should (= 2 (length (mevedel-session-schedules session)))))

  :doc "accepts an absolute local time and an expiry after it"
  (mevedel-tool-schedule-test--with-request
    (let ((outcome (mevedel-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"2026-11-21 10:00\" :note \"Push the release branch\" :expires \"2026-11-21 12:00\")"))
          (entry (car (mevedel-session-schedules session))))
      (should (eq 'success (plist-get outcome :status)))
      ;; 2026-11-21T09:00:00Z and 11:00:00Z.
      (should (= 1795251600 (plist-get entry :due)))
      (should (= 1795258800 (plist-get entry :expires-at)))))

  :doc "a recurring wake-up persists its timing and expires after 7 days by default"
  (mevedel-tool-schedule-test--with-request
    (let* ((outcome (mevedel-tool-schedule-test--call
                     "(Schedule :action \"create\" :when \"every 20m\" :note \"Babysit PR 412\")"))
           (cron (mevedel-tool-schedule-test--call
                  "(Schedule :action \"create\" :when \"0 9 * * 1-5\" :note \"Morning summary\" :expires \"30d\")"))
           (entries (mevedel-session-schedules session)))
      (should (eq 'success (plist-get outcome :status)))
      (should (string-search "\"repeat\":\"every 20m 00s\"" (plist-get outcome :result)))
      (should (string-search "\"repeat\":\"0 9 * * 1-5\"" (plist-get cron :result)))
      (should (equal '((:kind interval :seconds 1200) (:kind cron :expression "0 9 * * 1-5"))
                     (mapcar (lambda (entry) (plist-get entry :timing)) entries)))
      ;; 7 and 30 days after 2026-11-20 13:00.
      (should (equal '(1795780800 1797768000)
                     (mapcar (lambda (entry) (plist-get entry :expires-at)) entries)))
      (should (equal entries (mevedel-tool-schedule-test--persisted root session)))))

  :doc "short intervals, cron names, a recurring expiry past 30 days and a recurring expiry are tool errors"
  (mevedel-tool-schedule-test--with-request
    (dolist (case '(("(Schedule :action \"create\" :when \"every 30s\" :note \"x\")" . "at least one minute")
                    ("(Schedule :action \"create\" :when \"0 9 * * MON\" :note \"x\")" . "names")
                    ("(Schedule :action \"create\" :when \"every 1h\" :note \"x\" :expires \"31d\")" . "30 days")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"x\" :expires \"every 1h\")" . "expiry")))
      (ert-info ((car case))
        (let ((outcome (mevedel-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should-not (mevedel-session-schedules session)))

  :doc "invalid timing, a past time, a blank note, a late expiry and a 51st wake-up are tool errors"
  (mevedel-tool-schedule-test--with-request
    (dolist (case '(("(Schedule :action \"create\" :when \"soon\" :note \"x\")" . "Unrecognized")
                    ("(Schedule :action \"create\" :when \"0m\" :note \"x\")" . "positive")
                    ("(Schedule :action \"create\" :when \"2026-11-20 12:59\" :note \"x\")" . "not in the future")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"  \")" . "blank")
                    ("(Schedule :action \"create\" :when \"20m\" :note \"x\" :expires \"10m\")" . "expiry")
                    ("(Schedule :action \"cancel\" :id \"w-missing\")" . "No schedule")))
      (ert-info ((car case))
        (let ((outcome (mevedel-tool-schedule-test--call (car case))))
          (should-not (eq 'success (plist-get outcome :status)))
          (should (string-search (cdr case) (plist-get outcome :result))))))
    (should-not (mevedel-session-schedules session))
    (setf (mevedel-session-schedules session)
          (cl-loop for index below 50
                   collect (list :id (format "w-%d" index) :note "x"
                                 :timing (list :kind 'once :at (+ now 600))
                                 :created-at now :due (+ now 600)
                                 :expires-at nil :status 'pending)))
    (let ((outcome (mevedel-tool-schedule-test--call
                    "(Schedule :action \"create\" :when \"20m\" :note \"one too many\")")))
      (should (string-search "50 pending" (plist-get outcome :result)))
      (should (= 50 (length (mevedel-session-schedules session))))))

  :doc "lists and cancels wake-ups, persisting the cancellation"
  (mevedel-tool-schedule-test--with-request
    (mevedel-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"20m\" :note \"first\")")
    (mevedel-tool-schedule-test--call
     "(Schedule :action \"create\" :when \"1h\" :note \"second\")")
    (let* ((listed (json-parse-string
                    (plist-get (mevedel-tool-schedule-test--call
                                "(Schedule :action \"list\")")
                               :result)
                    :object-type 'plist :array-type 'list))
           (first (car (plist-get listed :schedules))))
      (should (equal "2026-11-20T13:00:00+01:00" (plist-get listed :now)))
      (should (equal '("first" "second")
                     (mapcar (lambda (entry) (plist-get entry :note))
                             (plist-get listed :schedules))))
      (should (eq 'success
                  (plist-get (mevedel-tool-schedule-test--call
                              (format "(Schedule :action \"cancel\" :id %S)"
                                      (plist-get first :id)))
                             :status)))
      (should (equal '("second")
                     (mapcar (lambda (entry) (plist-get entry :note))
                             (mevedel-tool-schedule-test--persisted root session)))))))

(mevedel-deftest mevedel-tools-visibility/schedule ()
  ,test
  (test)
  :doc "Schedule is offered to the root and refused to a child agent"
  (mevedel-tool-schedule-test--with-request
    (should (funcall (mevedel-tools-visibility fsm) "Schedule"))
    (let ((mevedel--agent-invocation
           (mevedel-agent-invocation--create :path "/root/worker"
                                             :parent-session session)))
      (setq-local mevedel--current-request nil)
      (should-not (funcall (mevedel-tools-visibility mevedel--agent-invocation)
                           "Schedule"))
      (should-error (mevedel-tool-schedule--handle
                     '(:action "create" :when "20m" :note "x")))
      (should-not (mevedel-session-schedules session)))))

(provide 'test-mevedel-tool-schedule)
;;; test-mevedel-tool-schedule.el ends here
