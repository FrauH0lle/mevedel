;;; test-mevedel-turn-ownership.el --- Deferred terminal ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Deterministic terminal continuation races, with real session publication.

;;; Code:

(require 'mevedel)
(require 'mevedel-presets)
(require 'mevedel-review)
(require 'mevedel-view)
(require 'mevedel-workspace-identity)
(require 'mevedel-directive-request)
(require 'mevedel-tools)
(require 'mevedel-agent-exec)
(require 'mevedel-compact)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-turn-ownership-test--with-session (&rest body)
  "Run BODY with an isolated session, buffer, request and FSM."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-turn-ownership-" t))
          (workspace (mevedel-workspace-get-or-create
                      'project root root "ownership"))
          (_identity (mevedel-workspace-identity-ensure root))
          (session (mevedel-session-create "main" workspace))
          (buffer (generate-new-buffer " *turn-ownership*"))
          request fsm)
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       mevedel--session session)
           (setf (mevedel-session-root-buffer session) buffer)
           (setq request (mevedel-request-begin session)
                 fsm (gptel-make-fsm
                      :info (list :buffer buffer
                                  :mevedel-request-id
                                  (mevedel-request-id request))))
           (setf (mevedel-request-fsm request) fsm)
           (insert "Old response must remain durable.\n")
           ,@body)
       (mevedel-transport-cancel-pending)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (mevedel-workspace-clear-registry)
       (delete-directory root t))))

(mevedel-deftest mevedel-request-begin-ownership (:quiet t)
  ,test
  (test)
  :doc "reentrant admission from a canceller survives the outer admission"
  (mevedel-turn-ownership-test--with-session
    (let (replacement replacement-cancelled)
      (mevedel-request-push-canceller
       request
       (lambda ()
         (setq replacement (mevedel-request-begin session))
         (mevedel-request-push-canceller
          replacement (lambda () (setq replacement-cancelled t)))))
      (should-error (mevedel-request-begin session) :type 'user-error)
      (should (mevedel-request-p replacement))
      (should (eq replacement mevedel--current-request))
      (should-not replacement-cancelled)
      (should (eq 'running (mevedel-session-agent-root-activity session)))
      (should-not mevedel--turn-settlements-pending)
      (mevedel-request-end)
      (should replacement-cancelled)
      (should-not mevedel--current-request)))

  :doc "reentrant admission during readiness or authority checks is not cancelled"
  (dolist (check '(mevedel-request-assert-target-ready
                   mevedel-session-artifacts-assert-mutation-authority))
    (mevedel-turn-ownership-test--with-session
      (let ((original (symbol-function check))
            entered replacement replacement-cancelled)
        (cl-letf (((symbol-function check)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (unless entered
                         (setq entered t
                               replacement (mevedel-request-begin session))
                         (mevedel-request-push-canceller
                          replacement
                          (lambda () (setq replacement-cancelled t))))))))
          (should-error (mevedel-request-begin session) :type 'user-error))
        (should (mevedel-request-p replacement))
        (should (eq replacement mevedel--current-request))
        (should-not replacement-cancelled)
        (mevedel-request-end))))

  :doc "admission rechecks a settlement hold acquired during cancellation"
  (mevedel-turn-ownership-test--with-session
    (mevedel-request-push-canceller request (lambda () (mevedel--turn-hold fsm)))
    (should-error (mevedel-request-begin session) :type 'user-error)
    (should-not mevedel--current-request)
    (should (memq fsm mevedel--turn-settlements-pending))
    (mevedel--turn-release fsm)
    (should-not mevedel--turn-settlements-pending)
    (should (mevedel-request-p (mevedel-request-begin session)))
    (mevedel-request-end)))

(mevedel-deftest mevedel--complete-turn/responsiveness (:quiet t)
  ,test
  (test)
  :doc "services input between real publication and checkpoint while admission stays fenced"
  (mevedel-turn-ownership-test--with-session
    (let* (input timer checkpoint-responsive admission-fenced
           (saved (lambda (&rest _)
                    (setq timer (run-at-time
                                 0 nil
                                 (lambda ()
                                   (setq input t
                                         admission-fenced
                                         (condition-case nil
                                             (progn (mevedel-request-begin session) nil)
                                           (user-error t))))))))
           (checkpoint (lambda (&rest _) (setq checkpoint-responsive input))))
      (unwind-protect
          (progn
            (advice-add 'mevedel-session-artifacts-save :after saved)
            (advice-add 'mevedel-journal-capture-checkpoint :before checkpoint)
            (mevedel--complete-turn fsm)
            (let ((deadline (+ (float-time) 5)))
              (while (and (mevedel-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
            (should checkpoint-responsive)
            (should admission-fenced)
            (should-not (mevedel-turn-busy-p buffer))
            (should (mevedel-session-artifacts-artifact-present-p
                     session (format "segment-%04d.chat.org"
                                     (mevedel-session-current-segment session)) t)))
        (when timer (cancel-timer timer))
        (advice-remove 'mevedel-session-artifacts-save saved)
        (advice-remove 'mevedel-journal-capture-checkpoint checkpoint)))))

(mevedel-deftest mevedel--defer-turn-steps-ownership (:quiet t)
  ,test
  (test)
  :doc "source death cancels a pending completion slice and releases its hold"
  (mevedel-turn-ownership-test--with-session
    (mevedel--complete-turn fsm)
    (should (mevedel-turn-busy-p buffer))
    (should (gethash (list 'turn-settlement (mevedel-request-id request)) mevedel-transport--pending))
    (kill-buffer buffer)
    (should-not (gethash (list 'turn-settlement (mevedel-request-id request)) mevedel-transport--pending))
    (should (= 0 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds))))

  :doc "abort retains the terminal reservation until its real publication completes"
  (dolist (outcome '(success error aborted))
    (mevedel-turn-ownership-test--with-session
      (let (resume)
        (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                   (lambda (_key _path thunk &optional _cancel _delay)
                     (setq resume thunk) t)))
          (if (eq outcome 'success)
              (mevedel--complete-turn fsm)
            (mevedel--fail-turn fsm outcome)))
        (should (functionp resume))
        (mevedel-abort buffer)
        (should (mevedel-turn-busy-p buffer))
        (should-error (mevedel-request-begin session) :type 'user-error)
        (should (eq request mevedel--current-request))
        (funcall resume)
        (let ((deadline (+ (float-time) 5)))
          (while (and (mevedel-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
        (should-not (mevedel-turn-busy-p buffer))
        (with-temp-buffer
          (insert (mevedel-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (mevedel-session-current-segment session)) t))
          (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
        (let ((replacement (mevedel-request-begin session)))
          (funcall resume)
          (should (eq replacement mevedel--current-request))
          (mevedel-request-end)))))

  :doc "replacement during a publication cannot be ended by the outer continuation"
  (mevedel-turn-ownership-test--with-session
    (let ((replacement (mevedel-request--create
                        :id "replacement" :session session :turn 2
                        :origin "/root" :file-snapshots (make-hash-table)))
          (save (symbol-function 'mevedel--turn-save)))
      (cl-letf (((symbol-function 'mevedel--turn-save)
                 (lambda (machine)
                   (funcall save machine)
                   ;; Inject the reentrant replacement at the publication seam.
                   (setq mevedel--current-request replacement
                         mevedel--implementation-permission-mode-saved '(ask))
                   (setf (mevedel-session-permission-mode session) 'full-auto))))
        (mevedel--complete-turn fsm))
      (should (eq replacement mevedel--current-request))
      (should (eq 'full-auto (mevedel-session-permission-mode session)))
      (should (equal '(ask) mevedel--implementation-permission-mode-saved))
      (should-not mevedel--turn-settlements-pending)
      (should (mevedel-session-artifacts-artifact-present-p
                     session (format "segment-%04d.chat.org"
                                     (mevedel-session-current-segment session)) t))))

  :doc "transport cancellation releases success and failure admission exactly once"
  (dolist (outcome '(success error aborted))
    (mevedel-turn-ownership-test--with-session
      (let (cancel resume)
        (cl-letf (((symbol-function 'mevedel-transport-run-when-idle)
                   (lambda (_key _path thunk &optional on-cancel _delay)
                     (setq resume thunk cancel on-cancel) t)))
          (if (eq outcome 'success)
              (mevedel--complete-turn fsm)
            (mevedel--fail-turn fsm outcome)))
        (should (functionp cancel))
        (funcall cancel)
        (should-not (mevedel-turn-busy-p buffer))
        (let ((replacement (mevedel-request-begin session)))
          (funcall cancel)
          (funcall resume)
          (should (eq replacement mevedel--current-request)))))))

(mevedel-deftest mevedel-preset--apply-final-patch-ownership (:quiet t)
  ,test
  (test)
  :doc "late final patch retains its captured patch without replacing current presentation"
  (mevedel-turn-ownership-test--with-session
    (let ((remote (mevedel-workspace--create
                   :type 'project :id "/mevedelmock:host:/srv/p/"
                   :root "/mevedelmock:host:/srv/p/" :name "p"))
          (replacement (mevedel-request--create :id "new" :session session))
          resume generated displayed (continued 0))
      (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (&optional _) t))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (_key _path thunk &optional _cancel _delay) (setq resume thunk) t))
                ((symbol-function 'mevedel--generate-final-patch)
                 (lambda (_workspace captured callback)
                   (setq generated captured)
                   (funcall callback "old diff\n" nil)))
                ((symbol-function 'mevedel--replace-patch-buffer)
                 (lambda (_) (setq displayed t))))
        (mevedel-preset--apply-final-patch
         fsm buffer remote request (lambda (_) (cl-incf continued)))
        (setq mevedel--current-request replacement)
        (funcall resume)
        (funcall resume)
        (should (eq request generated))
        (should (equal "old diff\n" (plist-get (gptel-fsm-info fsm) :mevedel-directive-patch)))
        (should-not displayed)
        (should (= 1 continued))
        (should (eq replacement mevedel--current-request)))))

  :doc "cancelled final patch releases its hold after durable abort settlement"
  (mevedel-turn-ownership-test--with-session
    (let ((remote (mevedel-workspace--create
                   :type 'project :id "/mevedelmock:host:/srv/p/"
                   :root "/mevedelmock:host:/srv/p/" :name "p"))
          resume cancel settle generated callbacks)
      (setf (gptel-fsm-state fsm) 'DONE
            (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm) :mevedel-request-callback
                       (lambda (status _machine) (push status callbacks))))
      (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (&optional _) t))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (key _path thunk &optional on-cancel _delay)
                   (if (eq (car key) 'final-patch)
                       (setq resume thunk cancel on-cancel)
                     (setq settle thunk))
                   t))
                ((symbol-function 'mevedel--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (setq generated t)
                   (funcall callback "unwanted" nil))))
        (mevedel-preset--apply-final-patch
         fsm buffer remote request #'mevedel-preset--settle-terminal)
        (mevedel-abort buffer)
        (should (eq request mevedel--current-request))
        (should-error (mevedel-request-begin session) :type 'user-error)
        (funcall cancel)
        (should (functionp settle))
        (should (equal '(abort) callbacks))
        (should (memq fsm mevedel--turn-settlements-pending)))
      (funcall settle)
      (let ((deadline (+ (float-time) 5)))
        (while (and (mevedel-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
      (should-not mevedel--turn-settlements-pending)
      (should-not mevedel--current-request)
      (with-temp-buffer
        (insert (mevedel-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (mevedel-session-current-segment session)) t))
        (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
      (let ((replacement (mevedel-request-begin session)))
        (funcall cancel)
        (funcall resume)
        (should-not generated)
        (should (equal '(abort) callbacks))
        (should (eq replacement mevedel--current-request)))))

  :doc "patch generation error still publishes the old response and releases admission"
  (mevedel-turn-ownership-test--with-session
    (setf (gptel-fsm-state fsm) 'DONE)
    (cl-letf (((symbol-function 'mevedel--generate-final-patch)
               (lambda (&rest _) (error "Injected patch failure"))))
      (mevedel-preset--apply-final-patch
       fsm buffer workspace request #'mevedel-preset--settle-terminal))
    (let ((deadline (+ (float-time) 5)))
      (while (and (mevedel-turn-busy-p buffer) (< (float-time) deadline)) (sleep-for .002)))
    (should-not mevedel--turn-settlements-pending)
    (should-not mevedel--current-request)
    (with-temp-buffer
      (insert (mevedel-session-artifacts-read-artifact
                   session (format "segment-%04d.chat.org"
                                   (mevedel-session-current-segment session)) t))
      (goto-char (point-min))
          (should (search-forward "Old response must remain durable." nil t)))
    (should (mevedel-request-p (mevedel-request-begin session)))
    (mevedel-request-end)))

(mevedel-deftest mevedel--process-directive-ownership (:quiet t)
  ,test
  (test)
  :doc "obsolete directive completion archives only its response and preserves the new turn"
  (let* ((root (make-temp-file "mevedel-directive-ownership-" t))
         (file (file-name-concat root "sample.txt"))
         (source (find-file-noselect file))
         chat machine called)
    (unwind-protect
        (with-current-buffer source
          (insert "alpha\n")
          (save-buffer)
          (let ((directive (mevedel--create-directive-in
                            source (point-min) (1- (point-max)) nil "Change alpha.")))
            (overlay-put directive 'mevedel-directive-action 'implement)
            (cl-letf (((symbol-function 'save-some-buffers) #'ignore)
                      ((symbol-function 'display-buffer) #'ignore)
                      ((symbol-function 'gptel--apply-preset) #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (setq chat (plist-get args :buffer)
                               machine (plist-get args :fsm))
                         (setf (gptel-fsm-info machine)
                               (list :buffer chat :position (plist-get args :position)))
                         machine)))
              (mevedel--process-directive
               directive '(:system "test") #'mevedel--implement-directive-prompt
               (lambda (&rest _) (setq called t)))
              (with-current-buffer chat
                (goto-char (point-max))
                (insert "Old answer.\n")
                (setf (gptel-fsm-info machine)
                      (plist-put (gptel-fsm-info machine) :mevedel-response-end
                                 (copy-marker (point-max) nil)))
                (let* ((replacement (mevedel-request--create
                                     :id "new-directive" :session mevedel--session))
                       (record (mevedel--directive-record directive)))
                  (setq mevedel--current-request replacement
                        mevedel--current-directive-uuid "new-directive"
                        mevedel--directive-read-only-request-p t)
                  (insert "NEW TURN MUST NOT ENTER OLD RESULT\n")
                  (funcall (plist-get (gptel-fsm-info machine) :mevedel-request-callback)
                           nil machine)
                  (should (eq replacement mevedel--current-request))
                  (should (equal "new-directive" mevedel--current-directive-uuid))
                  (should mevedel--directive-read-only-request-p)
                  (should-not called)
                  (let ((attempt (car (mevedel-directive-attempts record))))
                    (should (string-match-p "Old answer" (mevedel-directive-attempt-result attempt)))
                    (should-not (string-match-p "NEW TURN" (mevedel-directive-attempt-result attempt)))))))))
      (when (buffer-live-p chat)
        (let ((view (buffer-local-value 'mevedel--view-buffer chat)))
          (when (buffer-live-p view) (kill-buffer view)))
        (kill-buffer chat))
      (when (buffer-live-p source) (kill-buffer source))
      (mevedel-workspace-clear-registry)
      (delete-directory root t))))


(mevedel-deftest mevedel-request-call-owned (:quiet t)
  ,test
  (test)
  :doc "callbacks run in their buffer and ignore cancelled dead or replaced owners"
  (mevedel-turn-ownership-test--with-session
    (should (eq buffer (mevedel-request-call-owned request buffer #'current-buffer)))
    (should (eq 'done (mevedel-request-call-owned request buffer #'identity 'done)))
    (should-error (mevedel-request-call-owned request buffer #'error "Callback failed"))
    (setf (mevedel-request-cancelled-p request) t)
    (mevedel-request-call-owned request buffer #'ert-fail "Cancelled callback ran")
    (setf (mevedel-request-cancelled-p request) nil)
    (let ((replacement (mevedel-request-begin session)))
      (mevedel-request-call-owned request buffer #'ert-fail "Stale callback ran")
      (should (eq replacement mevedel--current-request)))
    (kill-buffer buffer)
    (mevedel-request-call-owned request buffer #'ert-fail "Dead callback ran")))

(mevedel-deftest mevedel-view--forward-input-now/ownership (:quiet t)
  ,test
  (test)
  :doc "startup cleanup keeps the first FSM and cannot end a nested replacement"
  (dolist (replace '(send summary))
    (dolist (failure '(error quit))
      (mevedel-turn-ownership-test--with-session
        (mevedel-request-end)
        (mevedel-view-test--with-buffers
          (setf (mevedel-session-root-buffer session) data-buf)
          (with-current-buffer data-buf
            (setq-local mevedel--session session mevedel--workspace workspace
                        default-directory (file-name-as-directory root)))
          (let (replacement caught)
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&rest _)
                         (let ((fsm (gptel-make-fsm :info (list :buffer data-buf))))
                           (funcall (cadr (assq 'WAIT gptel-send--handlers)) fsm)
                           (plist-put (gptel-fsm-info fsm) :mevedel-request
                                      (mevedel-request-begin session)))
                         (when (eq replace 'send)
                           (setq replacement (mevedel-request-begin session))
                           (funcall
                            (cadr (assq 'WAIT gptel-send--handlers))
                            (gptel-make-fsm
                             :info (list :buffer data-buf :mevedel-request replacement))))
                         (signal failure '("Injected send failure"))))
                      ((symbol-function 'mevedel-view--append-request-summary)
                       (lambda (&rest _)
                         (should (eq replace 'summary))
                         (setq replacement (mevedel-request-begin session)))))
              (with-current-buffer view-buf
                (condition-case err
                    (mevedel-view--forward-input-now "Inspect safely")
                  ((error quit) (setq caught err)))))
            (should (eq failure (car caught)))
            (should replacement)
            (should (eq replacement (buffer-local-value 'mevedel--current-request data-buf)))
            (should-not (mevedel-request-cancelled-p replacement))))))))

(mevedel-deftest mevedel-view--dispatch-prepared-outcome/ownership (:quiet t)
  ,test
  (test)
  :doc "fork delivery after replacement leaves the newer transcript and draft alone"
  (mevedel-turn-ownership-test--with-session
    (mevedel-request-end)
    (mevedel-view-test--with-buffers
      (setf (mevedel-session-root-buffer session) data-buf)
      (with-current-buffer data-buf
        (setq-local mevedel--session session mevedel--workspace workspace
                    default-directory (file-name-as-directory root)))
      (let (callback replacement before)
        (cl-letf (((symbol-function 'mevedel-skills-dispatch-prepared-fork)
                   (lambda (_outcome fn &rest _) (setq callback fn))))
          (with-current-buffer view-buf
            (mevedel-view--dispatch-prepared-outcome
             (mevedel-prompt-submission-create
              :display-text "$forker inspect"
              :outcome
              (list :model-input "Inspect" :transcript-input "Inspect"
                    :hook-input "Inspect" :fork-outcome
                    (list :skill (mevedel-skill--create :name "forker"))))
             data-buf)))
        (should callback)
        (with-current-buffer data-buf
          (setq replacement (mevedel-request-begin session)
                before (buffer-string)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "> Keep this draft\nand its second line"))
        (funcall callback '(:status ok :kind fork :result "Late answer"))
        (funcall callback '(:status error :message "Late error"))
        (should (eq replacement (buffer-local-value 'mevedel--current-request data-buf)))
        (with-current-buffer data-buf (should (equal before (buffer-string))))
        (with-current-buffer view-buf
          (should (equal "> Keep this draft\nand its second line"
                         (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-review--run-task/ownership (:quiet t)
  ,test
  (test)
  :doc "late review preparation and results cannot reach a replacement owner"
  (dolist (stage '(preparation result))
    (mevedel-turn-ownership-test--with-session
      (let (prepare result spawned delivered)
        (cl-letf (((symbol-function 'mevedel-review--write-target-package)
                   (lambda (_cwd _target callback) (setq prepare callback) #'ignore))
                  ((symbol-function 'mevedel-agent-resolve-role)
                   (lambda (&rest _) (mevedel-agent--create :name "reviewer")))
                  ((symbol-function 'mevedel-agent-control-spawn)
                   (lambda (_session _name _message _prepared &rest keys)
                     (setq spawned t result (plist-get keys :result-handler))
                     #'ignore)))
          (mevedel-review--run-task
           "Inspect" "Review" (lambda (_) (setq delivered t))
           nil nil nil (and (eq stage 'preparation) root)
           (and (eq stage 'preparation) 'target))
          ;; Replace without draining cancellers to exercise the identity fence
          ;; independently of cooperative cancellation at the sender.
          (setq-local mevedel--current-request (mevedel-request--create :session session))
          (if (eq stage 'preparation)
              (progn (should prepare) (funcall prepare nil) (should-not spawned))
            (should result)
            (funcall result '(:outcome completed :payload "Late answer")))
          (should-not delivered))))))

(mevedel-deftest mevedel-review--record-direct-turn/ownership (:quiet t)
  ,test
  (test)
  :doc "review insertion cannot dispatch under a replacement admitted by a hook"
  (mevedel-turn-ownership-test--with-session
    (mevedel-request-end)
    (let (replacement dispatched)
      (cl-letf (((symbol-function 'mevedel--insert-user-turn)
                 (lambda (_display)
                   (insert "User prompt")
                   (setq replacement (mevedel-request-begin session)))))
        (mevedel-review--record-direct-turn
         "Inspect" buffer (lambda () (setq dispatched t))))
      (should replacement)
      (should (eq replacement mevedel--current-request))
      (should-not dispatched))))

(mevedel-deftest mevedel-skills-input-insert-fork-result (:quiet t)
  ,test
  (test)
  :doc "fork result hooks cannot settle a replacement request"
  (mevedel-turn-ownership-test--with-session
    (let* (replacement
           (gptel-post-response-functions
            (list (lambda (&rest _)
                    (setq replacement (mevedel-request-begin session))))))
      (mevedel-skills-input-insert-fork-result '(:result "Answer"))
      (should replacement)
      (should (eq replacement mevedel--current-request))
      (should-not (mevedel-request-cancelled-p replacement))
      (should (= 0 (or (mevedel-session-turn-count session) 0))))))

(mevedel-deftest mevedel--process-directive/ownership (:quiet t)
  ,test
  (test)
  :doc "directive admission rollback does not end a replacement request"
  (mevedel-turn-ownership-test--with-session
    (mevedel-request-end)
    (let ((file (file-name-concat root "source.txt")) source replacement)
      (with-temp-file file (insert "source\n"))
      (setq source (find-file-noselect file))
      (unwind-protect
          (with-current-buffer source
            (setq-local mevedel--workspace workspace)
            (let ((directive
                   (mevedel--create-directive-in
                    source (point-min) (1- (point-max)) nil "Change it")))
              (overlay-put directive 'mevedel-directive-action 'implement)
              (cl-letf (((symbol-function 'mevedel--directive-session-buffer)
                         (lambda (&rest _) (cons buffer nil)))
                        ((symbol-function 'save-some-buffers) #'ignore)
                        ((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'gptel--apply-preset) #'ignore)
                        ((symbol-function 'gptel-request)
                         (lambda (&rest _)
                           (setq replacement (mevedel-request-begin session))
                           (error "Injected directive startup failure"))))
                (should-error
                 (mevedel--process-directive
                  directive '(:system "test")
                  #'mevedel--implement-directive-prompt nil))))
            (should replacement)
            (should (eq replacement (buffer-local-value 'mevedel--current-request buffer)))
            (should-not (mevedel-request-cancelled-p replacement)))
        (when (buffer-live-p source) (kill-buffer source))))))

(mevedel-deftest mevedel-agent-runtime-dispatch/ownership (:quiet t)
  ,test
  (test)
  :doc "interrupted provider startup rolls back its unpublished child buffer"
  (mevedel-turn-ownership-test--with-session
    (setq-local mevedel--workspace workspace)
    (let* ((agent (mevedel-agent--create :name "worker" :frozen-p t))
           (configuration (mevedel-agent-configuration--create :agent agent))
           child caught)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                     (lambda (_callback _role _description _invocation child-buffer)
                       (setq child child-buffer)
                       (signal 'quit nil))))
            (condition-case err
                (mevedel-agent-runtime-dispatch
                 agent "Work" "Inspect" :path "/root/failure"
                 :frozen-configuration configuration :prepared-turn '(:prompt "Inspect"))
              (quit (setq caught err)))
            (should (eq 'quit (car caught)))
            (should child)
            (should-not (buffer-live-p child)))
        (when (buffer-live-p child) (kill-buffer child))))))

(provide 'test-mevedel-turn-ownership)
;;; test-mevedel-turn-ownership.el ends here
