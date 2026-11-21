;;; test-niwa-turn-ownership.el --- Deferred terminal ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Deterministic terminal continuation races, with real session publication.

;;; Code:

(require 'niwa)
(require 'niwa-presets)
(require 'niwa-review)
(require 'niwa-view)
(require 'niwa-workspace-identity)
(require 'niwa-tools)
(require 'niwa-agent-exec)
(require 'niwa-compact)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-turn-ownership-test--with-session (&rest body)
  "Run BODY with an isolated session, buffer, request and FSM."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-turn-ownership-" t))
          (workspace (niwa-workspace-get-or-create
                      'project root root "ownership"))
          (_identity (niwa-workspace-identity-ensure root))
          (session (niwa-session-create "main" workspace))
          (buffer (generate-new-buffer " *turn-ownership*"))
          request fsm)
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       niwa--session session)
           (setf (niwa-session-root-buffer session) buffer)
           (setq request (niwa-request-begin session)
                 fsm (gptel-make-fsm
                      :info (list :buffer buffer
                                  :niwa-request-id
                                  (niwa-request-id request))))
           (setf (niwa-request-fsm request) fsm)
           (insert "Old response must remain durable.\n")
           ,@body)
       (niwa-transport-cancel-pending)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (niwa-workspace-clear-registry)
       (delete-directory root t))))

(niwa-deftest niwa-request-begin-implementation-authority (:quiet t)
  ,test
  (test)
  :doc "admission captures copied full policy and never restores baseline"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (let ((policy (list :model-provider "Provider:model" :reasoning-effort nil
                        :permission-mode 'full-auto)))
      (setf (niwa-session-permission-mode session) 'ask)
      (setq niwa--pending-implementation-policy policy)
      (let ((admitted (niwa-request-begin session)))
        (should (equal policy (niwa-request-implementation-policy admitted)))
        (should-not (eq policy (niwa-request-implementation-policy admitted)))
        (should (eq 'full-auto (niwa-request-permission-mode-override admitted)))
        (should-not niwa--pending-implementation-policy)
        (should (eq 'ask (niwa-session-permission-mode session)))
        (setf (niwa-session-permission-mode session) 'edits)
        (niwa-request-end)
        (should (eq 'edits (niwa-session-permission-mode session))))))

  :doc "stale request teardown leaves incoming pending policy for admission"
  (niwa-turn-ownership-test--with-session
    (let ((policy (list :model-provider "Provider:model" :reasoning-effort nil
                        :permission-mode 'edits)))
      (setq niwa--pending-implementation-policy policy)
      (let ((admitted (niwa-request-begin session)))
        (should (niwa-request-cancelled-p request))
        (should (equal policy (niwa-request-implementation-policy admitted)))
        (should (eq 'edits (niwa-request-permission-mode-override admitted)))
        (should-not niwa--pending-implementation-policy))))

  :doc "pending mode survives major mode and is consumed without session writes"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (setf (niwa-session-permission-mode session) 'full-auto)
    (setq niwa--pending-permission-mode 'ask)
    (org-mode)
    (let ((admitted (niwa-request-begin session)))
      (should (eq 'ask (niwa-request-permission-mode-override admitted)))
      (should-not (niwa-request-implementation-policy admitted))
      (should-not niwa--pending-permission-mode)
      (niwa-request-end)
      (should (eq 'full-auto (niwa-session-permission-mode session)))))

  :doc "explicit authority leaves pending root mode and records its own mode"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (let (events)
      (setq niwa--pending-permission-mode 'edits)
      (cl-letf (((symbol-function 'niwa-telemetry-record)
                 (lambda (_session event &rest fields)
                   (push (cons event fields) events))))
        (let ((admitted (niwa-request-begin
                         session '(:permission-mode full-auto :plan-read-only t))))
          (should (eq 'full-auto (niwa-request-permission-mode-override admitted)))
          (should (niwa-request-plan-read-only admitted))
          (should (eq 'edits niwa--pending-permission-mode))
          (dolist (event '(request-queued request-start))
            (should (eq 'full-auto
                        (plist-get (alist-get event events) :permission-mode))))))))

  :doc "explicit nil authority does not inherit shared preparation state"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (setq niwa--pending-permission-mode 'full-auto)
    (let ((admitted (niwa-request-begin
                     session '(:permission-mode nil :plan-read-only nil))))
      (should-not (niwa-request-permission-mode-override admitted))
      (should-not (niwa-request-plan-read-only admitted))
      (should (eq 'full-auto niwa--pending-permission-mode))))

  :doc "nested readiness admission cannot consume an outer request's explicit authority"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (let ((original (symbol-function 'niwa-request-assert-target-ready))
          (options '(:permission-mode full-auto :plan-read-only t))
          entered replacement)
      (setq niwa--pending-permission-mode 'edits)
      (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (unless entered
                       (setq entered t
                             replacement (niwa-request-begin session)))))))
        (should-error (niwa-request-begin session options)
                      :type 'user-error))
      (should (eq replacement niwa--current-request))
      (should-not (niwa-request-cancelled-p replacement))
      (should (eq 'edits (niwa-request-permission-mode-override replacement)))
      (should-not (niwa-request-plan-read-only replacement))
      (niwa-request-end)
      (let ((retry (niwa-request-begin session options)))
        (should (eq 'full-auto (niwa-request-permission-mode-override retry)))
        (should (niwa-request-plan-read-only retry)))))

  :doc "child admission captures inherited permission but no full model policy"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (setq niwa--agent-invocation
          (niwa-agent-invocation--create
           :parent-session session :path "/root/child"
           :permission-mode-override 'edits))
    (let ((admitted (niwa-request-begin session)))
      (should (eq 'edits (niwa-request-permission-mode-override admitted)))
      (should-not (niwa-request-implementation-policy admitted)))))

(niwa-deftest niwa-request-begin-ownership (:quiet t)
  ,test
  (test)
  :doc "reentrant admission from a canceller survives the outer admission"
  (niwa-turn-ownership-test--with-session
    (let (replacement replacement-cancelled)
      (niwa-request-push-canceller
       request
       (lambda ()
         (setq replacement (niwa-request-begin session))
         (niwa-request-push-canceller
          replacement (lambda () (setq replacement-cancelled t)))))
      (should-error (niwa-request-begin session) :type 'user-error)
      (should (niwa-request-p replacement))
      (should (eq replacement niwa--current-request))
      (should-not replacement-cancelled)
      (should (eq 'running (niwa-session-agent-root-activity session)))
      (should-not niwa--turn-settlements-pending)
      (niwa-request-end)
      (should replacement-cancelled)
      (should-not niwa--current-request)))

  :doc "reentrant admission during readiness or authority checks is not cancelled"
  (dolist (check '(niwa-request-assert-target-ready
                   niwa-session-artifacts-assert-mutation-authority))
    (niwa-turn-ownership-test--with-session
      (let ((original (symbol-function check))
            entered replacement replacement-cancelled)
        (cl-letf (((symbol-function check)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (unless entered
                         (setq entered t
                               replacement (niwa-request-begin session))
                         (niwa-request-push-canceller
                          replacement
                          (lambda () (setq replacement-cancelled t))))))))
          (should-error (niwa-request-begin session) :type 'user-error))
        (should (niwa-request-p replacement))
        (should (eq replacement niwa--current-request))
        (should-not replacement-cancelled)
        (niwa-request-end))))

  :doc "admission rechecks a settlement hold acquired during cancellation"
  (niwa-turn-ownership-test--with-session
    (niwa-request-push-canceller request (lambda () (niwa--turn-hold fsm)))
    (should-error (niwa-request-begin session) :type 'user-error)
    (should-not niwa--current-request)
    (should (memq fsm niwa--turn-settlements-pending))
    (niwa--turn-release fsm)
    (should-not niwa--turn-settlements-pending)
    (should (niwa-request-p (niwa-request-begin session)))
    (niwa-request-end)))

(niwa-deftest niwa--complete-turn/responsiveness (:quiet t)
  ,test
  (test)
  :doc "services input between real publication and checkpoint while admission stays fenced"
  (niwa-turn-ownership-test--with-session
    (let* (input timer checkpoint-responsive admission-fenced
           (saved (lambda (&rest _)
                    (setq timer (run-at-time
                                 0 nil
                                 (lambda ()
                                   (setq input t
                                         admission-fenced
                                         (condition-case nil
                                             (progn (niwa-request-begin session) nil)
                                           (user-error t))))))))
           (checkpoint (lambda (&rest _) (setq checkpoint-responsive input))))
      (unwind-protect
          (progn
            (advice-add 'niwa-session-artifacts-save :after saved)
            (advice-add 'niwa-journal-capture-checkpoint :before checkpoint)
            (niwa--complete-turn fsm)
            (let ((deadline (+ (float-time) 5)))
              (while (and (niwa-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
            (should checkpoint-responsive)
            (should admission-fenced)
            (should-not (niwa-turn-busy-p buffer))
            (should (niwa-session-artifacts-artifact-present-p
                     session (format "segment-%04d.chat.org"
                                     (niwa-session-current-segment session)) t)))
        (when timer (cancel-timer timer))
        (advice-remove 'niwa-session-artifacts-save saved)
        (advice-remove 'niwa-journal-capture-checkpoint checkpoint)))))

(niwa-deftest niwa--defer-turn-steps-ownership (:quiet t)
  ,test
  (test)
  :doc "source death cancels a pending completion slice and releases its hold"
  (niwa-turn-ownership-test--with-session
    (niwa--complete-turn fsm)
    (should (niwa-turn-busy-p buffer))
    (should (gethash (list 'turn-settlement (niwa-request-id request)) niwa-transport--pending))
    (kill-buffer buffer)
    (should-not (gethash (list 'turn-settlement (niwa-request-id request)) niwa-transport--pending))
    (should (= 0 (plist-get (gptel-fsm-info fsm) :niwa-settlement-holds))))

  :doc "abort retains the terminal reservation until its real publication completes"
  (dolist (outcome '(success error aborted))
    (niwa-turn-ownership-test--with-session
      (let (resume)
        (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (_key _path thunk &optional _cancel _delay)
                     (setq resume thunk) t)))
          (if (eq outcome 'success)
              (niwa--complete-turn fsm)
            (niwa--fail-turn fsm outcome)))
        (should (functionp resume))
        (niwa-abort buffer)
        (should (niwa-turn-busy-p buffer))
        (should-error (niwa-request-begin session) :type 'user-error)
        (should (eq request niwa--current-request))
        (funcall resume)
        (let ((deadline (+ (float-time) 5)))
          (while (and (niwa-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
        (should-not (niwa-turn-busy-p buffer))
        (with-temp-buffer
          (insert (niwa-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (niwa-session-current-segment session)) t))
          (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
        (let ((replacement (niwa-request-begin session)))
          (funcall resume)
          (should (eq replacement niwa--current-request))
          (niwa-request-end)))))

  :doc "replacement during a publication cannot be ended by the outer continuation"
  (niwa-turn-ownership-test--with-session
    (let ((replacement (niwa-request--create
                        :id "replacement" :session session :turn 2
                        :origin "/root" :file-snapshots (make-hash-table)))
          (save (symbol-function 'niwa--turn-save)))
      (cl-letf (((symbol-function 'niwa--turn-save)
                 (lambda (machine)
                   (funcall save machine)
                   ;; Inject the reentrant replacement at the publication seam.
                   (setq niwa--current-request replacement)
                   (setf (niwa-request-permission-mode-override replacement) 'edits)
                   (setf (niwa-session-permission-mode session) 'full-auto))))
        (niwa--complete-turn fsm)
        (niwa-test--await 5 "Deferred publication did not reach the replacement"
          (eq replacement niwa--current-request)))
      (should (eq replacement niwa--current-request))
      (should (eq 'full-auto (niwa-session-permission-mode session)))
      (should (eq 'edits (niwa-request-permission-mode-override replacement)))
      (should-not niwa--turn-settlements-pending)
      (should (niwa-session-artifacts-artifact-present-p
                     session (format "segment-%04d.chat.org"
                                     (niwa-session-current-segment session)) t))))

  :doc "transport cancellation releases success and failure admission exactly once"
  (dolist (outcome '(success error aborted))
    (niwa-turn-ownership-test--with-session
      (let (cancel resume)
        (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (_key _path thunk &optional on-cancel _delay)
                     (setq resume thunk cancel on-cancel) t)))
          (if (eq outcome 'success)
              (niwa--complete-turn fsm)
            (niwa--fail-turn fsm outcome)))
        (should (functionp cancel))
        (funcall cancel)
        (should-not (niwa-turn-busy-p buffer))
        (let ((replacement (niwa-request-begin session)))
          (funcall cancel)
          (funcall resume)
          (should (eq replacement niwa--current-request)))))))

(niwa-deftest niwa-preset--apply-final-patch-ownership (:quiet t)
  ,test
  (test)
  :doc "late final patch uses its captured request without replacing current presentation"
  (niwa-turn-ownership-test--with-session
    (let ((remote (niwa-workspace--create
                   :type 'project :id "/niwamock:host:/srv/p/"
                   :root "/niwamock:host:/srv/p/" :name "p"))
          (replacement (niwa-request--create :id "new" :session session))
          resume generated displayed (continued 0))
      (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path thunk &optional _cancel _delay) (setq resume thunk) t))
                ((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace captured callback)
                   (setq generated captured)
                   (funcall callback "old diff\n" nil)))
                ((symbol-function 'niwa--replace-patch-buffer)
                 (lambda (_) (setq displayed t))))
        (niwa-preset--apply-final-patch
         fsm buffer remote request (lambda (_) (cl-incf continued)))
        (setq niwa--current-request replacement)
        (funcall resume)
        (funcall resume)
        (should (eq request generated))
        (should-not displayed)
        (should (= 1 continued))
        (should (eq replacement niwa--current-request)))))

  :doc "cancelled final patch releases its hold after durable abort settlement"
  (niwa-turn-ownership-test--with-session
    (let ((remote (niwa-workspace--create
                   :type 'project :id "/niwamock:host:/srv/p/"
                   :root "/niwamock:host:/srv/p/" :name "p"))
          resume cancel settle generated callbacks)
      (setf (gptel-fsm-state fsm) 'DONE
            (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm) :niwa-request-callback
                       (lambda (status _machine) (push status callbacks))))
      (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (key _path thunk &optional on-cancel _delay)
                   (if (eq (car key) 'final-patch)
                       (setq resume thunk cancel on-cancel)
                     (setq settle thunk))
                   t))
                ((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (setq generated t)
                   (funcall callback "unwanted" nil))))
        (niwa-preset--apply-final-patch
         fsm buffer remote request #'niwa-preset--settle-terminal)
        (niwa-abort buffer)
        (should (eq request niwa--current-request))
        (should-error (niwa-request-begin session) :type 'user-error)
        (funcall cancel)
        (should (functionp settle))
        (should (equal '(abort) callbacks))
        (should (memq fsm niwa--turn-settlements-pending)))
      (funcall settle)
      (let ((deadline (+ (float-time) 5)))
        (while (and (niwa-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
      (should-not niwa--turn-settlements-pending)
      (should-not niwa--current-request)
      (with-temp-buffer
        (insert (niwa-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (niwa-session-current-segment session)) t))
        (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
      (let ((replacement (niwa-request-begin session)))
        (funcall cancel)
        (funcall resume)
        (should-not generated)
        (should (equal '(abort) callbacks))
        (should (eq replacement niwa--current-request)))))

  :doc "patch generation error still publishes the old response and releases admission"
  (niwa-turn-ownership-test--with-session
    (setf (gptel-fsm-state fsm) 'DONE)
    (cl-letf (((symbol-function 'niwa--generate-final-patch)
               (lambda (&rest _) (error "Injected patch failure"))))
      (niwa-preset--apply-final-patch
       fsm buffer workspace request #'niwa-preset--settle-terminal))
    (let ((deadline (+ (float-time) 5)))
      (while (and (niwa-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
    (should-not niwa--turn-settlements-pending)
    (should-not niwa--current-request)
    (with-temp-buffer
      (insert (niwa-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (niwa-session-current-segment session)) t))
      (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
    (should (niwa-request-p (niwa-request-begin session)))
    (niwa-request-end)))

(niwa-deftest niwa-request-call-owned (:quiet t)
  ,test
  (test)
  :doc "callbacks run in their buffer and ignore cancelled dead or replaced owners"
  (niwa-turn-ownership-test--with-session
    (should (eq buffer (niwa-request-call-owned request buffer #'current-buffer)))
    (should (eq 'done (niwa-request-call-owned request buffer #'identity 'done)))
    (should-error (niwa-request-call-owned request buffer #'error "Callback failed"))
    (setf (niwa-request-cancelled-p request) t)
    (niwa-request-call-owned request buffer #'ert-fail "Cancelled callback ran")
    (setf (niwa-request-cancelled-p request) nil)
    (let ((replacement (niwa-request-begin session)))
      (niwa-request-call-owned request buffer #'ert-fail "Stale callback ran")
      (should (eq replacement niwa--current-request)))
    (kill-buffer buffer)
    (niwa-request-call-owned request buffer #'ert-fail "Dead callback ran")))

(niwa-deftest niwa-view--forward-input-now/ownership (:quiet t)
  ,test
  (test)
  :doc "startup cleanup keeps the first FSM and cannot end a nested replacement"
  (dolist (replace '(send summary))
    (dolist (failure '(error quit))
      (niwa-turn-ownership-test--with-session
        (niwa-request-end)
        (niwa-view-test--with-buffers
          (setf (niwa-session-root-buffer session) data-buf)
          (with-current-buffer data-buf
            (setq-local niwa--session session niwa--workspace workspace
                        default-directory (file-name-as-directory root)))
          (let (replacement caught)
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (let ((fsm (gptel-make-fsm :info (list :buffer data-buf))))
                           (funcall (cadr (assq 'WAIT gptel-send--handlers)) fsm)
                           (plist-put (gptel-fsm-info fsm) :niwa-request
                                      (niwa-request-begin session)))
                         (when (eq replace 'send)
                           (setq replacement (niwa-request-begin session))
                           (funcall
                            (cadr (assq 'WAIT gptel-send--handlers))
                            (gptel-make-fsm
                             :info (list :buffer data-buf :niwa-request replacement))))
                         (signal failure '("Injected send failure"))))
                      ((symbol-function 'niwa-view--append-request-summary)
                       (lambda (&rest _)
                         (should (eq replace 'summary))
                         (setq replacement (niwa-request-begin session)))))
              (with-current-buffer view-buf
                (condition-case err
                    (niwa-view--forward-input-now "Inspect safely")
                  ((error quit) (setq caught err)))))
            (should (eq failure (car caught)))
            (should replacement)
            (should (eq replacement (buffer-local-value 'niwa--current-request data-buf)))
            (should-not (niwa-request-cancelled-p replacement))))))))

(niwa-deftest niwa-view--dispatch-prepared-outcome/ownership (:quiet t)
  ,test
  (test)
  :doc "fork delivery after replacement leaves the newer transcript and draft alone"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (niwa-view-test--with-buffers
      (setf (niwa-session-root-buffer session) data-buf)
      (with-current-buffer data-buf
        (setq-local niwa--session session niwa--workspace workspace
                    default-directory (file-name-as-directory root)))
      (let (callback replacement before)
        (cl-letf (((symbol-function 'niwa-skills-dispatch-prepared-fork)
                   (lambda (_outcome fn &rest _) (setq callback fn))))
          (with-current-buffer view-buf
            (niwa-view--dispatch-prepared-outcome
             (niwa-prompt-submission-create
              :display-text "$forker inspect"
              :outcome
              (list :model-input "Inspect" :transcript-input "Inspect"
                    :hook-input "Inspect" :fork-outcome
                    (list :skill (niwa-skill--create :name "forker"))))
             data-buf)))
        (should callback)
        (with-current-buffer data-buf
          (setq replacement (niwa-request-begin session)
                before (buffer-string)))
        (with-current-buffer view-buf
          (niwa-view-composer-insert "> Keep this draft\nand its second line"))
        (funcall callback '(:status ok :kind fork :result "Late answer"))
        (funcall callback '(:status error :message "Late error"))
        (should (eq replacement (buffer-local-value 'niwa--current-request data-buf)))
        (with-current-buffer data-buf (should (equal before (buffer-string))))
        (with-current-buffer view-buf
          (should (equal "> Keep this draft\nand its second line"
                         (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-review--run-task/ownership (:quiet t)
  ,test
  (test)
  :doc "late review preparation and results cannot reach a replacement owner"
  (dolist (stage '(preparation result))
    (niwa-turn-ownership-test--with-session
      (let (prepare result spawned delivered)
        (cl-letf (((symbol-function 'niwa-review--write-target-package)
                   (lambda (_cwd _target callback) (setq prepare callback) #'ignore))
                  ((symbol-function 'niwa-agent-resolve-role)
                   (lambda (&rest _) (niwa-agent--create :name "reviewer")))
                  ((symbol-function 'niwa-agent-control-spawn)
                   (lambda (_session _name _message _prepared &rest keys)
                     (setq spawned t result (plist-get keys :result-handler))
                     #'ignore)))
          (niwa-review--run-task
           "Inspect" "Review" (lambda (_) (setq delivered t))
           nil nil nil (and (eq stage 'preparation) root)
           (and (eq stage 'preparation) 'target))
          ;; Replace without draining cancellers to exercise the identity fence
          ;; independently of cooperative cancellation at the sender.
          (setq-local niwa--current-request (niwa-request--create :session session))
          (if (eq stage 'preparation)
              (progn (should prepare) (funcall prepare nil) (should-not spawned))
            (should result)
            (funcall result '(:outcome completed :payload "Late answer")))
          (should-not delivered))))))

(niwa-deftest niwa-review--record-direct-turn/ownership (:quiet t)
  ,test
  (test)
  :doc "review insertion cannot dispatch under a replacement admitted by a hook"
  (niwa-turn-ownership-test--with-session
    (niwa-request-end)
    (let (replacement dispatched)
      (cl-letf (((symbol-function 'niwa--insert-user-turn)
                 (lambda (_display)
                   (insert "User prompt")
                   (setq replacement (niwa-request-begin session)))))
        (niwa-review--record-direct-turn
         "Inspect" buffer (lambda () (setq dispatched t))))
      (should replacement)
      (should (eq replacement niwa--current-request))
      (should-not dispatched))))

(niwa-deftest niwa-skills-input-insert-fork-result (:quiet t)
  ,test
  (test)
  :doc "fork result hooks cannot settle a replacement request"
  (niwa-turn-ownership-test--with-session
    (let* (replacement
           (gptel-post-response-functions
            (list (lambda (&rest _)
                    (setq replacement (niwa-request-begin session))))))
      (niwa-skills-input-insert-fork-result '(:result "Answer"))
      (should replacement)
      (should (eq replacement niwa--current-request))
      (should-not (niwa-request-cancelled-p replacement))
      (should (= 0 (or (niwa-session-turn-count session) 0))))))

(niwa-deftest niwa-agent-runtime-dispatch/ownership (:quiet t)
  ,test
  (test)
  :doc "interrupted provider startup rolls back its unpublished child buffer"
  (niwa-turn-ownership-test--with-session
    (setq-local niwa--workspace workspace)
    (let* ((agent (niwa-agent--create :name "worker" :frozen-p t))
           (configuration (niwa-agent-configuration--create :agent agent))
           child caught)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-agent-exec-run)
                     (lambda (_callback _role _description _invocation child-buffer)
                       (setq child child-buffer)
                       (signal 'quit nil))))
            (condition-case err
                (niwa-agent-runtime-dispatch
                 agent "Work" "Inspect" :path "/root/failure"
                 :frozen-configuration configuration :prepared-turn '(:prompt "Inspect"))
              (quit (setq caught err)))
            (should (eq 'quit (car caught)))
            (should child)
            (should-not (buffer-live-p child)))
        (when (buffer-live-p child) (kill-buffer child))))))

(provide 'test-niwa-turn-ownership)
;;; test-niwa-turn-ownership.el ends here
