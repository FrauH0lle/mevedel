;;; test-mevedel-workspace-schedule.el --- Workspace schedules fire runs -*- lexical-binding: t -*-

;;; Commentary:
;; The scheduler seam for workspace schedules: store an approved schedule,
;; advance the injectable clock and run the scheduler.  A run creates a new
;; saved session; the provider request is replaced by recording the submission.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-notify)
(require 'mevedel-presets)
(require 'mevedel-pipeline)
(require 'mevedel-schedule)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tool-notify)
(require 'mevedel-turn)
(require 'mevedel-workspace-schedule)
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))

(defconst mevedel-workspace-schedule-test--now 1795176000
  "2026-11-20 13:00:00 in Europe/Zurich (12:00 UTC).")

(defconst mevedel-workspace-schedule-test--policy
  '(:model-provider "ws-test:ws-model" :reasoning-effort nil :permission-mode full-auto)
  "An approved implementation policy.")

(defmacro mevedel-workspace-schedule-test--with-workspace (&rest body)
  "Run BODY with `workspace' at `root', a fixed clock `now' and recorded turns.
`submitted' holds (PROMPT TRIGGER POLICY BUFFER) for each started run, newest
first.  `restart' drops every in-memory trace of the scheduler and the run
sessions, as an Emacs restart would."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-workspace-schedule-" t))
          (mevedel-user-dir (file-name-as-directory (file-name-concat root "user")))
          (workspace (test-mevedel-session-persistence--make-workspace
                      (file-name-concat root "workspace")))
          (now mevedel-workspace-schedule-test--now)
          (mevedel-schedule-clock (lambda () now))
          (zone (getenv "TZ"))
          (before (buffer-list))
          ;; The standard presets, without leaking them to other tests.
          (mevedel-preset--registry mevedel-preset--registry)
          (gptel--known-presets gptel--known-presets)
          (gptel--known-backends gptel--known-backends)
          submitted)
     (set-time-zone-rule "Europe/Zurich")
     (mevedel-tools-register)
     (mevedel--define-presets)
     (put 'ws-model :reasoning-effort '(member low high))
     (gptel-make-openai "ws-test" :key "test" :models '(ws-model ws-other))
     (unwind-protect
         (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                    ;; A real turn would send a provider request.
                    (lambda (prompt &optional _display _submission _outcome trigger)
                      (push (list prompt trigger
                                  (mevedel-workspace-schedule-test--selection)
                                  (current-buffer))
                            submitted))))
           (cl-flet ((restart ()
                       (mevedel-workspace-schedule-test--kill-runs before)
                       (setq mevedel-schedule--workspaces nil)
                       (mevedel-schedule--rearm)))
             ,@body))
       (mevedel-workspace-schedule-test--kill-runs before)
       (setq mevedel-schedule--workspaces nil)
       (mevedel-schedule--rearm)
       (should-not mevedel-schedule--timer)
       (set-time-zone-rule zone)
       (delete-directory root t)
       (mevedel-workspace-clear-registry))))

(defun mevedel-workspace-schedule-test--kill-runs (before)
  "Kill the session buffers created since buffer list BEFORE."
  (dolist (buffer (buffer-list))
    (unless (memq buffer before)
      (when-let* ((session (buffer-local-value 'mevedel--session buffer)))
        (when (eq buffer (mevedel-session-root-buffer session))
          (test-mevedel-session-persistence--release-and-kill buffer session))))
    (when (and (buffer-live-p buffer) (not (memq buffer before)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun mevedel-workspace-schedule-test--selection ()
  "Return the current buffer's model, effort and mode as a policy plist."
  (list :model-provider (format "%s:%s" (gptel-backend-name gptel-backend)
                                (gptel--model-name gptel-model))
        :reasoning-effort gptel-reasoning-effort
        :permission-mode (mevedel-permission-mode-effective mevedel--session
                                                            (current-buffer))))

(defun mevedel-workspace-schedule-test--create (workspace &rest overrides)
  "Store an approved schedule in WORKSPACE with OVERRIDES and return it."
  (mevedel-workspace-schedule-create
   workspace
   (mevedel-workspace-schedule-propose
    (append overrides
            (list :name "Triage" :when "0 7 * * 1-5"
                  :prompt "Triage new GitHub issues; Notify only release blockers."
                  :preset 'mevedel-implement
                  :policy mevedel-workspace-schedule-test--policy)))))

(mevedel-deftest mevedel-workspace-schedule-run-due ()
  ,test
  (test)
  :doc "a due schedule starts a new saved session with its preset, policy and prompt, quietly and without a window"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace))
           (id (plist-get schedule :id))
           (windows (window-list nil t))
           (selected (selected-window)))
      ;; Friday 2026-11-20 13:00; next weekday 07:00 is Monday 2026-11-23.
      (should (equal "2026-11-23T07:00:00+01:00"
                     (mevedel-schedule-format-time (plist-get schedule :due))))
      (should mevedel-schedule--timer)
      (setq now (+ (plist-get schedule :due) 5))
      (mevedel-schedule-run-due)
      (should (= 1 (length submitted)))
      (pcase-let ((`(,prompt ,trigger ,policy ,buffer) (car submitted)))
        (should (equal "Triage new GitHub issues; Notify only release blockers." prompt))
        (should (eq 'workspace-schedule (plist-get trigger :turn-trigger)))
        (should (eq (buffer-local-value 'mevedel-workspace-schedule-run buffer)
                    (plist-get trigger :workspace-schedule-run)))
        (should (equal mevedel-workspace-schedule-test--policy policy))
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (should (equal "Triage 2026-11-23 07:00" (mevedel-session-name session)))
          (should (eq 'mevedel-implement (mevedel-session-preset-name session)))
          (should (mevedel-session-save-path session))
          (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
            (should (equal (mevedel-session-session-id session) (plist-get run :session-id)))
            (should (eq 'launched (plist-get run :status)))
            (should (= (plist-get schedule :due) (plist-get run :scheduled-at)))
            (should (= now (plist-get run :fired-at))))))
      (should (equal windows (window-list nil t)))
      (should (eq selected (selected-window)))
      ;; The next run is Tuesday 07:00, timed from this firing.
      (should (equal "2026-11-24T07:00:00+01:00"
                     (mevedel-schedule-format-time
                      (plist-get (mevedel-workspace-schedule-get workspace id) :due))))))

  :doc "a later background-event turn in a run session runs under the approved policy"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((policy '(:model-provider "ws-test:ws-model" :reasoning-effort high
                                     :permission-mode edits))
           (schedule (mevedel-workspace-schedule-test--create
                      workspace :when "30m" :policy policy))
           (global-model (default-value 'gptel-model))
           (global-mode (default-value 'mevedel-permission-mode)))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (let* ((buffer (nth 3 (car submitted)))
             (session (buffer-local-value 'mevedel--session buffer)))
        (should (equal policy (nth 2 (car submitted))))
        ;; The run itself schedules a wake-up; it fires as a background event.
        (with-current-buffer buffer
          (mevedel-schedule-create session buffer "20m" "Check again"))
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (mevedel-test--await 5 "The wake-up did not start a turn"
          (= 2 (length submitted)))
        (pcase-let ((`(,_prompt ,trigger ,selection ,turn-buffer) (car submitted)))
          (should (eq 'scheduled-wake (plist-get trigger :turn-trigger)))
          (should (eq buffer turn-buffer))
          (should (equal policy selection))))
      (should (eq global-model (default-value 'gptel-model)))
      (should (eq global-mode (default-value 'mevedel-permission-mode)))))

  :doc "a run due while the previous run's session is still working is skipped and recorded"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get schedule :id)))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (let ((first (nth 3 (car submitted))))
        ;; Submission can wait for provider admission without an active request.
        (should-not (mevedel-turn-busy-p first))
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (should (= 1 (length submitted)))
        (should (equal '(launched skipped)
                       (mapcar (lambda (run) (plist-get run :status))
                               (mevedel-workspace-schedule-runs workspace id))))
        (should-not (plist-get (cadr (mevedel-workspace-schedule-runs workspace id))
                               :session-id))
        (mevedel--turn-record-settlement (gptel-make-fsm :info (list :buffer first)) 'success)
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (should (= 2 (length submitted))))))

  :doc "two same-named schedules due together each start their own run session"
  (mevedel-workspace-schedule-test--with-workspace
    (let ((first (mevedel-workspace-schedule-test--create workspace))
          (second (mevedel-workspace-schedule-test--create workspace)))
      (setq now (plist-get first :due))
      (mevedel-schedule-run-due)
      (should (= 2 (length submitted)))
      (should (equal '("Triage 2026-11-23 07:00" "Triage 2026-11-23 07:00 (2)")
                     (sort (mapcar (lambda (entry)
                                     (mevedel-session-name
                                      (buffer-local-value 'mevedel--session (nth 3 entry))))
                                   submitted)
                           #'string<)))
      (dolist (schedule (list first second))
        (should (eq 'launched (plist-get (car (mevedel-workspace-schedule-runs
                                               workspace (plist-get schedule :id)))
                                         :status))))))

  :doc "a previous run still working in another Emacs is skipped until its first request settles"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get schedule :id)))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (let* ((buffer (nth 3 (car submitted)))
             (session (buffer-local-value 'mevedel--session buffer))
             (directory (mevedel-session-save-path session))
             (first (car (mevedel-workspace-schedule-runs workspace id))))
        ;; The run session moves to another live Emacs.
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
          (should (mevedel-session-durability-lease-acquire directory "elsewhere")))
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (should (= 1 (length submitted)))
        (should (eq 'skipped (plist-get (car (last (mevedel-workspace-schedule-runs workspace id)))
                                        :status)))
        (mevedel-schedule-restore)
        (should-not (plist-get (car (mevedel-workspace-schedule-runs workspace id)) :outcome))
        ;; That Emacs records the first request's settlement.
        (mevedel-workspace-schedule-update-run
         workspace id (plist-get first :scheduled-at)
         (lambda (run) (plist-put run :settled-at now)))
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (should (= 2 (length submitted))))))

  :doc "a run past the schedule's expiry is recorded as expired without a session"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create
                      workspace :when "every 1h" :expires "90m"))
           (id (plist-get schedule :id)))
      ;; Emacs slept through the first run and the expiry.
      (setq now (+ now 7200))
      (mevedel-schedule-run-due)
      (should-not submitted)
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'expired (plist-get run :status)))
        (should (= (plist-get schedule :due) (plist-get run :scheduled-at))))
      (should (eq 'expired (plist-get (mevedel-workspace-schedule-get workspace id) :status)))
      (should-not mevedel-schedule--workspaces)))

  :doc "a run another Emacs already fired is not fired again"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (stale (copy-sequence mevedel-schedule--workspaces)))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      ;; A second Emacs still remembers the old due time.
      (setq mevedel-schedule--workspaces stale)
      (mevedel-schedule-run-due)
      (should (= 1 (length submitted)))
      (should (= 1 (length (mevedel-workspace-schedule-runs workspace (plist-get schedule :id)))))))

  :doc "a firing whose schedule write fails backs off, then moves on and settles the unlaunched run"
  (dolist (timing '("every 20m" "30m"))
    (mevedel-workspace-schedule-test--with-workspace
      (let* (notices warnings
             (schedule (mevedel-workspace-schedule-test--create workspace :when timing))
             (id (plist-get schedule :id))
             (directory (mevedel-workspace-schedule--path workspace id))
             (file (file-name-concat directory "schedule.el"))
             (mevedel-notify-function (lambda (kind details)
                                        (push (cons kind (plist-get details :text)) notices))))
        (ert-info (timing)
          (setq now (plist-get schedule :due))
          ;; The run record lands in runs/, but the schedule cannot be rewritten.
          (set-file-modes file #o444)
          (set-file-modes directory #o555)
          (unwind-protect
              (mevedel-test--with-captured-diagnostics warnings
                (mevedel-schedule-run-due))
            (set-file-modes directory #o755)
            (set-file-modes file #o644))
          (should (string-search "did not fire" warnings))
          (should (= 1 (length (mevedel-workspace-schedule-runs workspace id))))
          (should (= now (plist-get (mevedel-workspace-schedule-get workspace id) :due)))
          ;; The timer is not re-armed at the past due time.
          (should (equal (list (+ now mevedel-schedule-retry-delay))
                         (mapcar #'cdr mevedel-schedule--workspaces)))
          (setq now (+ now mevedel-schedule-retry-delay))
          (mevedel-schedule-run-due)
          (should-not submitted)
          (let ((runs (mevedel-workspace-schedule-runs workspace id))
                (entry (mevedel-workspace-schedule-get workspace id)))
            (should (= 1 (length runs)))
            (should (equal '(failed "Run interrupted before launch")
                           (list (plist-get (car runs) :outcome) (plist-get (car runs) :error))))
            (should (= 1 (plist-get entry :failures)))
            (if (equal timing "30m")
                (progn (should (eq 'finished (plist-get entry :status)))
                       (should-not mevedel-schedule--timer))
              (should (= (+ now 1200) (plist-get entry :due)))))
          (should (equal '(failure) (mapcar #'car notices)))
          (should (string-search "interrupted before launch" (cdar notices)))))))

  :doc "a run that cannot start keeps its run key as a setup error"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (id (plist-get schedule :id)))
      (setq now (plist-get schedule :due))
      (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                 (lambda (&rest _) (error "Disk full"))))
        (mevedel-schedule-run-due))
      (should-not submitted)
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'setup-error (plist-get run :status)))
        (should (equal "Disk full" (plist-get run :error)))
        (should (= (plist-get schedule :due) (plist-get run :scheduled-at))))
      ;; A one-shot schedule is finished either way.
      (should (eq 'finished (plist-get (mevedel-workspace-schedule-get workspace id) :status))))))

(mevedel-deftest mevedel-workspace-schedule-create ()
  ,test
  (test)
  :doc "a creation or a reactivating change past capacity fails, even when another writer takes the last slot meanwhile"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((mevedel-workspace-schedule-max 2)
           (commit (symbol-function 'mevedel-workspace-schedule--commit))
           (finish (lambda (entry) (plist-put (plist-put entry :status 'finished) :due nil)))
           (reactivate (lambda (entry) (plist-put (plist-put entry :status 'active)
                                                  :due (+ now 600))))
           (active (plist-get (mevedel-workspace-schedule-test--create workspace) :id))
           (finished (plist-get (mevedel-workspace-schedule-test--create workspace) :id))
           raced)
      (mevedel-workspace-schedule-update workspace finished finish)
      ;; Another Emacs reactivates the finished schedule after this one counted.
      (cl-letf (((symbol-function 'mevedel-workspace-schedule--commit)
                 (lambda (&rest args)
                   (unless raced
                     (setq raced t)
                     (mevedel-workspace-schedule-update workspace finished reactivate))
                   (apply commit args))))
        (should-error (mevedel-workspace-schedule-test--create workspace) :type 'user-error))
      (should raced)
      (should (= 2 (length (mevedel-workspace-schedule-list workspace))))
      (mevedel-workspace-schedule-update workspace finished finish)
      (should (mevedel-workspace-schedule-test--create workspace))
      (should-error (mevedel-workspace-schedule-update workspace finished reactivate)
                    :type 'user-error)
      (should (eq 'finished (plist-get (mevedel-workspace-schedule-get workspace finished) :status)))
      (should (= 2 (cl-count-if (lambda (entry) (eq 'active (plist-get entry :status)))
                                (mevedel-workspace-schedule-list workspace))))
      (should (eq 'active (plist-get (mevedel-workspace-schedule-get workspace active) :status)))))

  :doc "a schedule published after its absence was read is counted, not proven by a later listing"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((mevedel-workspace-schedule-max 1)
           (list-directory (symbol-function 'mevedel-session-control-fs-list-directory))
           (other (plist-put (mevedel-workspace-schedule-propose
                              (list :name "Other" :when "1h" :prompt "Other."
                                    :policy mevedel-workspace-schedule-test--policy))
                             :id "s-bbbbbb"))
           (other-directory (mevedel-workspace-schedule--path workspace "s-bbbbbb"))
           published)
      ;; Another Emacs has made its schedule's directory but not its record...
      (make-directory (file-name-concat other-directory "runs") t)
      (cl-letf (((symbol-function 'mevedel-session-control-fs-list-directory)
                 (lambda (directory regexp)
                   ;; ...and publishes the record while this one inspects it.
                   (when (and (not published)
                              (equal (file-name-as-directory directory)
                                     (file-name-as-directory other-directory)))
                     (setq published t)
                     (should-not (mevedel-workspace-schedule--commit
                                  workspace
                                  (list (list :op 'create
                                              :path (file-name-concat other-directory "schedule.el")
                                              :content (mevedel-workspace-schedule--bytes other))))))
                   (funcall list-directory directory regexp))))
        (should-error (mevedel-workspace-schedule-test--create workspace) :type 'user-error))
      (should published)
      (should (equal '("s-bbbbbb")
                     (mapcar (lambda (entry) (plist-get entry :id))
                             (mevedel-workspace-schedule-list workspace)))))))

(mevedel-deftest mevedel-workspace-schedule-id-p ()
  ,test
  (test)
  :doc "only lowercase s-xxxxxx ids are canonical, whatever the case folding"
  (let ((case-fold-search t))
    (should (mevedel-workspace-schedule-id-p "s-0a1b2c"))
    (dolist (id '("s-ABCDEF" "S-abcdef" "s-abcde" "s-abcdef/.." "../s-abcdef" nil))
      (ert-info ((format "%S" id))
        (should-not (mevedel-workspace-schedule-id-p id))
        (should-error (mevedel-workspace-schedule--id id) :type 'user-error)))
    (let ((entry (list :id "s-ABCDEF" :name "x" :prompt "y" :created-at 0
                       :timing '(:kind once :at 60) :status 'active :due 60
                       :preset 'mevedel-implement :overlap 'skip
                       :policy mevedel-workspace-schedule-test--policy)))
      (should-not (mevedel-workspace-schedule-entry-p entry))
      (should (mevedel-workspace-schedule-entry-p (plist-put (copy-sequence entry)
                                                             :id "s-abcdef"))))))

(mevedel-deftest mevedel-workspace-schedule--turn-settled ()
  ,test
  (test)
  :doc "a run's first settled root request reports the run once, then Notify is the session's own"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           settled)
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (let* ((buffer (nth 3 (car submitted)))
             (session (buffer-local-value 'mevedel--session buffer))
             (fsm (gptel-make-fsm :info (list :buffer buffer)))
             (mevedel-workspace-schedule-run-settled-functions
              (list (lambda (run outcome) (push (list run outcome) settled)))))
        (should (equal (mevedel-workspace-schedule--path
                        workspace (plist-get schedule :id) "notify-receipts.json")
                       (mevedel-tool-notify--receipt-file session)))
        (mevedel--turn-record-settlement fsm 'error)
        (mevedel--turn-record-settlement fsm 'success)
        (let ((record (car (mevedel-workspace-schedule-runs workspace (plist-get schedule :id)))))
          (should (equal now (plist-get record :settled-at)))
          (should (eq 'failed (plist-get record :outcome))))
        (should (equal `(((:workspace ,(directory-file-name
                                        (expand-file-name (mevedel-workspace-root workspace)))
                                      :schedule-id ,(plist-get schedule :id)
                                      :scheduled-at ,(plist-get schedule :due)
                                      :session-id ,(mevedel-session-session-id session))
                          error))
                       settled))
        (should (equal (file-name-concat (mevedel-session-save-path session)
                                         "notify-receipts.json")
                       (mevedel-tool-notify--receipt-file session))))))

  :doc "a run whose schedule was deleted meanwhile settles without a warning"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           settled warnings)
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (mevedel-workspace-schedule-delete workspace (plist-get schedule :id))
      (let ((mevedel-workspace-schedule-run-settled-functions
             (list (lambda (_run outcome) (push outcome settled)))))
        (mevedel-test--with-captured-diagnostics warnings
          (mevedel--turn-record-settlement
           (gptel-make-fsm :info (list :buffer (nth 3 (car submitted)))) 'success)))
      (should (equal '(success) settled))
      (should (equal "" warnings)))))

(mevedel-deftest mevedel-workspace-schedule-run-notified ()
  ,test
  (test)
  :doc "only an emitted Notify during the scheduled turn marks the run"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get job :id)) notices
           (mevedel-notify-function (lambda (kind _details) (push kind notices))))
      (cl-flet ((call (fsm)
                  (let ((mevedel--current-request
                         (mevedel-request--create :session mevedel--session :fsm fsm :origin "/root"))
                        answer)
                    (mevedel-pipeline-run-tool-outcome
                     (mevedel-tool-for-call "Notify") (lambda (value) (setq answer value))
                     '(:text "CI red") (list :engine fsm :projection 'provider))
                    (mevedel-test--await 3 "Notify did not answer" answer)
                    (plist-get answer :result))))
        (dolist (expected '("Notified: CI red" "Duplicate notice suppressed."))
          (setq now (plist-get (mevedel-workspace-schedule-get workspace id) :due))
          (mevedel-schedule-run-due)
          (with-current-buffer (nth 3 (car submitted))
            (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
              (should (equal expected (call fsm)))
              (mevedel--turn-record-settlement fsm 'success))
            ;; A later user turn has its own receipt and sends, but cannot mark the run.
            (should (equal "Notified: CI red" (call (gptel-make-fsm :info (list :buffer (current-buffer))))))))
        (should (equal '(notify notify notify) notices))
        (should (equal '(t nil) (mapcar (lambda (run) (plist-get run :notified))
                                        (mevedel-workspace-schedule-runs workspace id))))))))

(mevedel-deftest mevedel-workspace-schedule-restore ()
  ,test
  (test)
  :doc "an active schedule fires after a restart and stale index entries are dropped"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (index (mevedel-schedule-workspace-index-directory))
           (gone (file-name-concat index "gone.el")))
      (with-temp-file gone (prin1 (file-name-concat root "missing") (current-buffer)))
      (restart)
      (should-not mevedel-schedule--timer)
      (mevedel-schedule-restore)
      (should mevedel-schedule--timer)
      (should-not (file-exists-p gone))
      (should (= 1 (length (directory-files index nil "\\.el\\'"))))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (should (= 1 (length submitted)))
      ;; Once its first request settles, a finished job leaves the index.
      (mevedel-workspace-schedule-runs-test--settle submitted 'success)
      (should-not mevedel-schedule--workspaces)
      (should (directory-files index nil "\\.el\\'"))
      (mevedel-schedule-restore)
      (should-not (directory-files index nil "\\.el\\'"))))

  :doc "a schedule created while restore prunes the workspace keeps its index entry"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (index (mevedel-schedule-workspace-index-directory))
           (delete (symbol-function 'delete-file))
           created)
      ;; The workspace reads as inactive at restore...
      (mevedel-workspace-schedule-delete workspace (plist-get schedule :id))
      (restart)
      ;; ...and another Emacs commits a schedule just before the entry goes.
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (file &rest args)
                   (when (and (not created) (string-prefix-p index file))
                     (setq created (mevedel-workspace-schedule-test--create
                                    workspace :when "1h")))
                   (apply delete file args))))
        (mevedel-schedule-restore))
      (should created)
      (should (directory-files index nil "\\.el\\'"))
      (should (equal (list (plist-get created :due))
                     (mapcar #'cdr mevedel-schedule--workspaces)))))

  :doc "an index entry that cannot be read now is kept and restored once readable"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (file (car (directory-files (mevedel-schedule-workspace-index-directory) t "\\.el\\'")))
           warnings)
      (restart)
      (set-file-modes file #o000)
      (unwind-protect
          (mevedel-test--with-captured-diagnostics warnings
            (mevedel-schedule-restore))
        (set-file-modes file #o644))
      (should (string-search "not restored" warnings))
      (should (file-exists-p file))
      (should-not mevedel-schedule--workspaces)
      (mevedel-schedule-restore)
      (should (equal (list (plist-get schedule :due))
                     (mapcar #'cdr mevedel-schedule--workspaces)))))

  :doc "a workspace behind an unsearchable parent keeps its index entry"
  (mevedel-workspace-schedule-test--with-workspace
    ;; The index must stay readable while the workspace's parent is not.
    (let ((mevedel-user-dir (file-name-as-directory (make-temp-file "mevedel-user-" t))))
      (unwind-protect
          (progn
            (mevedel-workspace-schedule-test--create workspace :when "30m")
            (let* ((index (mevedel-schedule-workspace-index-directory))
                   (parent (directory-file-name (file-name-directory
                                                 (directory-file-name
                                                  (mevedel-workspace-root workspace)))))
                   (modes (file-modes parent))
                   warnings)
              (restart)
              (set-file-modes parent #o000)
              (unwind-protect
                  (mevedel-test--with-captured-diagnostics warnings
                    (mevedel-schedule-restore))
                (set-file-modes parent modes))
              (should (string-search "not restored" warnings))
              (should (directory-files index nil "\\.el\\'"))))
        (delete-directory mevedel-user-dir t)))))

(defun mevedel-workspace-schedule-runs-test--settle (submitted outcome &optional reason)
  "Settle the newest turn in SUBMITTED with OUTCOME and provider REASON."
  (let ((fsm (gptel-make-fsm :info (list :buffer (nth 3 (car submitted))
                                        :error (and reason (list :message reason))))))
    (mevedel--turn-record-settlement fsm outcome)))

(mevedel-deftest mevedel-workspace-schedule-runs-success ()
  ,test
  (test)
  :doc "a successful first settlement survives restart without a notice"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace))
           (id (plist-get schedule :id)) notices
           (mevedel-notify-function (lambda (&rest notice) (push notice notices))))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (mevedel-workspace-schedule-runs-test--settle submitted 'success)
      (restart)
      (should-not notices)
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'done (plist-get run :outcome)))
        (should (plist-member run :notified))
        (should-not (plist-get run :notified)))
      (should (eq 'done (plist-get (mevedel-workspace-schedule-get workspace id) :last-outcome))))))

(mevedel-deftest mevedel-workspace-schedule-runs-streak ()
  ,test
  (test)
  :doc "success resets failures, interruption preserves them, and failure three pauses and notifies once"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get schedule :id)) notices
           (mevedel-notify-function (lambda (kind payload) (push (cons kind payload) notices))))
      (dolist (step '((error 1) (success 0) (error 1) (abort 1) (error 2) (error 3)))
        (setq now (plist-get (mevedel-workspace-schedule-get workspace id) :due))
        (mevedel-schedule-run-due)
        (mevedel-workspace-schedule-runs-test--settle submitted (car step) "Login expired")
        (should (= (cadr step) (plist-get (mevedel-workspace-schedule-get workspace id) :failures))))
      (let ((entry (mevedel-workspace-schedule-get workspace id))
            (runs (mevedel-workspace-schedule-runs workspace id)))
        (should (eq 'paused (plist-get entry :status)))
        (should (string-match-p "Login expired" (plist-get entry :pause-reason)))
        (should (equal "Login expired" (plist-get (car (last runs)) :error)))
        (should (equal '(failed done failed skipped failed failed)
                       (mapcar (lambda (r) (plist-get r :outcome)) runs)))
        (should (equal '(t nil t nil t t) (mapcar (lambda (r) (plist-get r :notified)) runs))))
      (should-not mevedel-schedule--timer)
      (should (equal '(failure failure failure failure) (mapcar #'car notices)))
      (should (eq (buffer-local-value 'mevedel--session (nth 3 (car submitted)))
                  (plist-get (cdar notices) :session)))
      (should (string-match-p "paused" (plist-get (cdar notices) :text)))
      (should (string-match-p "Login expired" (plist-get (cdar notices) :text))))))

(mevedel-deftest mevedel-workspace-schedule-runs-pre-admission-failure (:quiet t)
  ,test
  (test)
  :doc "a deferred compaction failure before WAIT fails only its own launched run"
  (let ((submit (symbol-function 'mevedel--submit-generated-turn))
        (gptel--known-backends (copy-sequence gptel--known-backends)))
    (require 'mevedel-compact)
    (require 'mevedel-compact-evidence)
    (require 'mevedel-compact-run)
    (gptel-make-openai "ws-test" :host "127.0.0.1:1" :key "test" :models '(ws-model))
    (dolist (replaced '(t nil))
      (mevedel-workspace-schedule-test--with-workspace
        (let* ((job (mevedel-workspace-schedule-test--create workspace))
               (id (plist-get job :id)) callback machine notices
               (mevedel-notify-function (lambda (kind _details) (push kind notices))))
          (setq now (plist-get job :due))
          (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                     (lambda (&rest args)
                       (let ((gptel-prompt-transform-functions '(mevedel--compact-transform-auto)))
                         (setq machine (apply submit args)))))
                    ((symbol-function 'mevedel--compact-should-compact-p) (lambda (_) '(:target-pressure t)))
                    ((symbol-function 'mevedel-compact-evidence-find-boundary) #'point-min)
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args) (setq callback (plist-get args :callback)))))
            (mevedel-schedule-run-due))
          (should (functionp callback))
          (with-current-buffer (plist-get (gptel-fsm-info machine) :buffer)
            (should mevedel-workspace-schedule--run-active)
            (should-not mevedel--current-request)
            (when replaced
              (setq mevedel-workspace-schedule-run
                    (plist-put (copy-sequence mevedel-workspace-schedule-run) :scheduled-at (1+ now))))
            (funcall callback "Summary unavailable")
            (should (eq 'ERRS (gptel-fsm-state machine)))
            (should (eq 'error (plist-get (gptel-fsm-info machine) :mevedel-terminal-status)))
            (should-not (plist-get (gptel-fsm-info machine) :mevedel-request-id))
            (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
              (should (equal (if replaced '(nil t nil) '(failed nil (failure)))
                             (list (plist-get run :outcome)
                                   mevedel-workspace-schedule--run-active notices))))))))))

(mevedel-deftest mevedel-workspace-schedule-runs-pre-admission-abort (:quiet t)
  ,test
  (test)
  :doc "aborting a scheduled turn while its compaction summary is in flight skips without a failure"
  (let ((submit (symbol-function 'mevedel--submit-generated-turn))
        (gptel--known-backends (copy-sequence gptel--known-backends)))
    (require 'mevedel-compact)
    (require 'mevedel-compact-estimation)
    (require 'mevedel-compact-evidence)
    (gptel-make-openai "ws-test" :host "127.0.0.1:1" :key "test" :models '(ws-model))
    (mevedel-workspace-schedule-test--with-workspace
      (let* ((job (mevedel-workspace-schedule-test--create workspace)) (id (plist-get job :id))
             (mevedel-compact-estimation-token-threshold 0.00001) (mevedel-compact-evidence-tail-turns 0)
             (gptel-use-curl t) gptel--request-alist machine summary cancelled notices
             (mevedel-notify-function (lambda (kind _details) (push kind notices))))
        (setq now (plist-get job :due))
        (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                   (lambda (&rest args)
                     (goto-char (point-max))
                     (insert "Prior question\n" (propertize "Prior answer\n" 'gptel 'response))
                     (let ((gptel-prompt-transform-functions '(mevedel--compact-transform-auto)))
                       (setq machine (apply submit args)))))
                  ((symbol-function 'gptel-curl-get-response)
                   (lambda (fsm)
                     (setq summary (plist-get (gptel-fsm-info fsm) :context))
                     (push (cons 'held (cons fsm (lambda () (setq cancelled t)))) gptel--request-alist))))
          (mevedel-schedule-run-due)
          (should (plist-get summary :mevedel-context-summary))
          (with-current-buffer (plist-get (gptel-fsm-info machine) :buffer)
            (should mevedel-compact-run-in-flight)
            (should-not mevedel--current-request)
            (mevedel-abort)
            (should cancelled)
            (should (eq 'ABRT (gptel-fsm-state machine)))
            (should (eq 'aborted (plist-get (gptel-fsm-info machine) :mevedel-terminal-status)))
            (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
              (should (equal '(skipped nil 0 nil)
                             (list (plist-get run :outcome) mevedel-workspace-schedule--run-active
                                   (or (plist-get (mevedel-workspace-schedule-get workspace id) :failures) 0)
                                   notices))))))))))

(mevedel-deftest mevedel-workspace-schedule-runs-setup-failure ()
  ,test
  (test)
  :doc "a setup failure notifies the workspace once without pausing a finished one-shot"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (id (plist-get schedule :id)) notices
           (mevedel-notify-function (lambda (kind payload) (push (cons kind payload) notices))))
      (mevedel-workspace-schedule-update workspace id (lambda (job) (plist-put job :failures 2)))
      (setq now (plist-get schedule :due))
      (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                 (lambda (&rest _) (error "Disk full"))))
        (mevedel-schedule-run-due))
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'failed (plist-get run :outcome)))
        (should (plist-get run :notified))
        (run-hook-with-args 'mevedel-workspace-schedule-fired-functions workspace run))
      (should (equal '(failure) (mapcar #'car notices)))
      (should (file-equal-p (mevedel-workspace-root workspace)
                            (mevedel-workspace-root (plist-get (cdar notices) :workspace))))
      (should (string-match-p "Disk full" (plist-get (cdar notices) :text)))
      (should (eq 'finished (plist-get (mevedel-workspace-schedule-get workspace id) :status))))))

(mevedel-deftest mevedel-workspace-schedule-runs-late ()
  ,test
  (test)
  :doc "a failure after an overlap skip counts, but history still shows the newer skip"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((schedule (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get schedule :id)))
      (setq now (plist-get schedule :due))
      (mevedel-schedule-run-due)
      (with-current-buffer (nth 3 (car submitted))
        (setq-local mevedel--turn-settlements-pending t))
      (cl-incf now 1200)
      (mevedel-schedule-run-due)
      (should (= 1 (length submitted)))
      (mevedel-workspace-schedule-runs-test--settle submitted 'error)
      (let ((job (mevedel-workspace-schedule-get workspace id)))
        (should (eq 'skipped (plist-get job :last-outcome)))
        (should (= 1 (plist-get job :failures))))
      (should (equal '(failed skipped)
                     (mapcar (lambda (r) (plist-get r :outcome))
                             (mevedel-workspace-schedule-runs workspace id)))))))

(mevedel-deftest mevedel-workspace-schedule-runs-interrupted ()
  ,test
  (test)
  :doc "restore retains a live one-shot, then records its abandoned first turn once"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace :when "30m"))
           (id (plist-get job :id)) notices
           (mevedel-notify-function (lambda (&rest notice) (push notice notices))))
      (setq now (plist-get job :due))
      (mevedel-schedule-run-due)
      (mevedel-schedule-restore)
      (should-not notices)
      (restart)
      (mevedel-schedule-restore)
      (mevedel-schedule-restore)
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'failed (plist-get run :outcome)))
        (should (string-match-p "interrupted" (plist-get run :error)))
        (should (integerp (plist-get run :settled-at))))
      (should (equal '(failure) (mapcar #'car notices)))
      (should (eq 'finished (plist-get (mevedel-workspace-schedule-get workspace id) :status))))))

(mevedel-deftest mevedel-workspace-schedule-runs-abandoned-claim ()
  ,test
  (test)
  :doc "an interrupted launch fails only after the claiming process provably exits"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get job :id))
           (process (start-process "schedule-holder" nil "sleep" "60")))
      (unwind-protect
          (let* ((holder (mevedel-session-durability--process-holder (process-id process)))
                 (mevedel-session-durability--holder-cache (cons (emacs-pid) holder)))
            (skip-unless holder)
            (setq now (plist-get job :due))
            (catch 'interrupted
              (cl-letf (((symbol-function 'mevedel-workspace-schedule--launch)
                         (lambda (&rest _) (throw 'interrupted nil))))
                (mevedel-schedule-run-due)))
            (mevedel-schedule-restore)
            (should-not (plist-get (car (mevedel-workspace-schedule-runs workspace id)) :outcome))
            (delete-process process)
            (cl-incf now 1200)
            (mevedel-schedule-run-due)
            (should (= 1 (length submitted)))
            (should (eq 'failed (plist-get (car (mevedel-workspace-schedule-runs workspace id)) :outcome))))
        (when (process-live-p process) (delete-process process))))))

(mevedel-deftest mevedel-workspace-schedule-runs-superseded-claim ()
  ,test
  (test)
  :doc "a later firing retires an unidentified claim; its resumed launcher cannot submit or fail it"
  (dolist (race '(before-setup during-setup setup-error))
    (mevedel-workspace-schedule-test--with-workspace
      (let* ((job (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
             (id (plist-get job :id))
             (mevedel-session-durability--holder-cache (cons (emacs-pid) nil))
             (args (catch 'interrupted
                     (cl-letf (((symbol-function 'mevedel-workspace-schedule--launch)
                                (lambda (&rest args) (throw 'interrupted args))))
                       (setq now (plist-get job :due))
                       (mevedel-schedule-run-due)))))
        (restart)
        (mevedel-schedule-restore)
        (cl-incf now 1200)
        (if (eq race 'before-setup)
            (progn
              (mevedel-schedule-run-due)
              (apply #'mevedel-workspace-schedule--launch args)
              (should (= 1 (length (directory-files (mevedel-session-artifacts-sessions-dir workspace)
                                                    nil directory-files-no-dot-files-regexp)))))
          (let ((preset (symbol-function 'mevedel-preset-apply)) fired)
            (cl-letf (((symbol-function 'mevedel-preset-apply)
                       (lambda (&rest params)
                         (unless fired
                           (setq fired t)
                           (mevedel-schedule-run-due)
                           (when (eq race 'setup-error) (error "Setup failed")))
                         (apply preset params))))
              (apply #'mevedel-workspace-schedule--launch args))))
        (should (= 1 (length submitted)))
        (let ((old (car (mevedel-workspace-schedule-runs workspace id))))
          (should-not (plist-get old :session-id))
          (should (eq 'skipped (plist-get old :outcome)))
          (should-not (eq 'setup-error (plist-get old :status))))))))

(mevedel-deftest mevedel-workspace-schedule-runs-expired-lease ()
  ,test
  (test)
  :doc "a foreign lease blocks while live, but expiry admits a new run without inventing failure"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
           (id (plist-get job :id))
           (clock (- (floor (float-time)) 2400)))
      (setq now (plist-get job :due))
      (mevedel-schedule-run-due)
      (let ((dir (mevedel-session-save-path
                  (buffer-local-value 'mevedel--session (nth 3 (car submitted))))))
        (restart)
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time) (lambda (&rest _) clock)))
          (let ((mevedel-session-durability--client-id (make-string 64 ?e))
                (mevedel-session-durability--holder-cache (cons (emacs-pid) nil)))
            (should (mevedel-session-durability-lease-acquire dir "Other client")))
          (mevedel-schedule-restore)
          (cl-incf now 1200)
          (mevedel-schedule-run-due)
          (should (= 1 (length submitted)))
          (cl-incf clock 2400)
          (cl-incf now 1200)
          (should (eq 'expired (plist-get (mevedel-session-durability-cleanup-assessment dir) :state)))
          (mevedel-schedule-run-due)
          (should (= 2 (length submitted)))
          (should-not (plist-get (car (mevedel-workspace-schedule-runs workspace id)) :outcome)))))))

(mevedel-deftest mevedel-workspace-schedule-runs-reboot ()
  ,test
  (test)
  :doc "only a valid holder from this machine's former boot proves an interrupted claim"
  (dolist (identity '(previous-boot other-machine malformed))
    (mevedel-workspace-schedule-test--with-workspace
      (let* ((job (mevedel-workspace-schedule-test--create workspace :when "every 20m"))
             (id (plist-get job :id))
             (machine (make-string 32 ?a))
             (boot "fafafafa-abcd-1111-aaaa-123456789abc")
             (suffix (concat "/" machine "/pid:[1]"))
             (mevedel-session-durability--host-cache
              (cons (emacs-pid) (concat "eeeeeeee-abcd-2222-bbbb-123456789def" suffix)))
             (holder (list :host (concat boot (if (eq identity 'other-machine)
                                                   (concat "/" (make-string 32 ?b) "/pid:[1]") suffix))
                           :uid (user-uid) :pid 1 :process (if (eq identity 'malformed) "?" (concat boot ":123"))))
             (mevedel-session-durability--holder-cache (cons (emacs-pid) holder)))
        (setq now (plist-get job :due))
        (catch 'interrupted
          (cl-letf (((symbol-function 'mevedel-workspace-schedule--launch)
                     (lambda (&rest _) (throw 'interrupted nil))))
            (mevedel-schedule-run-due)))
        (restart)
        (mevedel-schedule-restore)
        (cl-incf now 1200)
        (mevedel-schedule-run-due)
        (should (= 1 (length submitted)))
        (should (eq (if (eq identity 'previous-boot) 'failed 'skipped)
                    (plist-get (car (mevedel-workspace-schedule-runs workspace id)) :outcome)))))))

(mevedel-deftest mevedel-workspace-schedule-runs-expired ()
  ,test
  (test)
  :doc "a missed expiry stays quiet and terminal in history"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace :when "30m" :expires "45m"))
           (id (plist-get job :id)))
      (cl-incf now 3600)
      (mevedel-schedule-run-due)
      (let ((run (car (mevedel-workspace-schedule-runs workspace id))))
        (should (eq 'expired (plist-get run :outcome)))
        (should-not (plist-get run :notified)))
      (should (eq 'expired (plist-get (mevedel-workspace-schedule-get workspace id) :last-outcome))))))

(mevedel-deftest mevedel-workspace-schedule-runs-deleted ()
  ,test
  (test)
  :doc "a queued callback cannot resurrect a deleted schedule or notify for it"
  (mevedel-workspace-schedule-test--with-workspace
    (let* ((job (mevedel-workspace-schedule-test--create workspace))
           (id (plist-get job :id)) notices
           (mevedel-notify-function (lambda (&rest notice) (push notice notices))))
      (setq now (plist-get job :due))
      (mevedel-schedule-run-due)
      (mevedel-workspace-schedule-delete workspace id)
      (with-current-buffer (nth 3 (car submitted))
        (run-hook-with-args 'mevedel-workspace-schedule-run-settled-functions
                            mevedel-workspace-schedule-run 'error))
      (should-not notices)
      (should-not (mevedel-workspace-schedule-get workspace id))
      (should-not (mevedel-workspace-schedule-runs workspace id)))))

(provide 'test-mevedel-workspace-schedule)
;;; test-mevedel-workspace-schedule.el ends here
