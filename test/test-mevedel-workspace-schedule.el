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
(require 'mevedel-presets)
(require 'mevedel-schedule)
(require 'mevedel-session-artifacts)
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
        (should (eq 'workspace-schedule trigger))
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
          (should (eq 'scheduled-wake trigger))
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
        ;; The first run's request is still in flight.
        (with-current-buffer first
          (setq-local mevedel--turn-settlements-pending t))
        (setq now (+ now 1200))
        (mevedel-schedule-run-due)
        (should (= 1 (length submitted)))
        (should (equal '(launched skipped)
                       (mapcar (lambda (run) (plist-get run :status))
                               (mevedel-workspace-schedule-runs workspace id))))
        (should-not (plist-get (cadr (mevedel-workspace-schedule-runs workspace id))
                               :session-id))
        ;; Once it is idle the next run starts again.
        (with-current-buffer first
          (setq-local mevedel--turn-settlements-pending nil))
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
      ;; Finished: the workspace leaves the scheduler, and the index at the
      ;; next start.
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

(provide 'test-mevedel-workspace-schedule)
;;; test-mevedel-workspace-schedule.el ends here
