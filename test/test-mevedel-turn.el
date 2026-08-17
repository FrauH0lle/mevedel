;;; test-mevedel-turn.el --- Tests for mevedel-turn.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests canonical turn settlement independently of preset construction.

;;; Code:

(require 'gptel)
(require 'mevedel)
(require 'mevedel-hooks)
(require 'mevedel-permission-queue)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-turn)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--fsm-error-message ()
  ,test
  (test)
  :doc "prefers the backend message and falls back through type and status"
  (dolist (case '(((:error "raw provider failure"
                    :status rejected) "raw provider failure")
                  ((:error (:type "api" :message "failed")
                    :status rejected) "failed")
                  ((:error (:type "api") :status rejected) "api: rejected")
                  ((:error (:type "api")) "api")
                  ((:status rejected) "rejected")
                  (nil nil)))
    (should (equal (cadr case)
                   (mevedel--fsm-error-message
                    (gptel-make-fsm :info (car case)))))))

(mevedel-deftest mevedel--turn-record-request-failure ()
  ,test
  (test)
  :doc "persists complete provider failure details in ignored render-data"
  (let* ((session (mevedel-session--create :name "turn-failure"))
         (request (mevedel-request--create
                   :id "request-1" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *mevedel-turn-failure*"))
         (backend
          (gptel-make-openai
           "Codex failure test" :key "test" :models '(test-model)))
         (message-text "An error occurred while processing your request.")
         (error-data
          `(:type "server_error"
            :code "server_error"
            :message ,message-text
            :request_id "provider-request-123"))
         (fsm
          (gptel-make-fsm
           :info
           (list :buffer chat-buf
                 :position (with-current-buffer chat-buf
                             (copy-marker (point-min)))
                 :backend backend
                 :status "HTTP/2 200"
                 :error error-data))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request request)
            (insert "Partial response\n"))
          (mevedel--turn-record-request-failure fsm)
          (with-current-buffer chat-buf
            (let* ((block-start
                    (progn
                      (goto-char (point-min))
                      (search-forward "<!-- mevedel-render-data -->")
                      (match-beginning 0)))
                   (data
                    (cdr
                     (mevedel-pipeline-extract-render-data
                      (buffer-substring-no-properties
                       block-start (point-max))))))
              (should (eq 'request-summary (plist-get data :kind)))
              (should (eq 'error (plist-get data :outcome)))
              (should (equal (gptel-backend-name backend)
                             (plist-get data :backend)))
              (should (equal "HTTP/2 200" (plist-get data :status)))
              (should (equal "server_error"
                             (plist-get data :error-type)))
              (should (equal "server_error"
                             (plist-get data :error-code)))
              (should (equal error-data (plist-get data :error-data)))
              (should (equal message-text (plist-get data :message)))
              (should (eq 'manual (plist-get data :retry)))
              (should (eq 'ignore
                          (get-text-property block-start 'gptel))))))
      (kill-buffer chat-buf)))

  :doc "persists plain-string provider errors without losing their text"
  (let* ((session (mevedel-session--create :name "turn-string-failure"))
         (request (mevedel-request--create
                   :id "request-2" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *mevedel-turn-string-failure*"))
         (message-text "Raw provider failure")
         (fsm
          (gptel-make-fsm
           :info
           (list :buffer chat-buf
                 :position (with-current-buffer chat-buf
                             (copy-marker (point-min)))
                 :status "Transport failure"
                 :error message-text))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request request)
            (insert "Partial response\n"))
          (mevedel--turn-record-request-failure fsm)
          (with-current-buffer chat-buf
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (let ((data
                   (cdr
                    (mevedel-pipeline-extract-render-data
                     (buffer-substring-no-properties
                      (match-beginning 0) (point-max))))))
              (should (equal message-text (plist-get data :error-data)))
              (should (equal message-text (plist-get data :message))))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-record-settlement
  (:doc "correlates terminal provider tokens with the active request")
  (let* ((session (mevedel-session--create :name "turn-telemetry"))
         (request (mevedel-request--create
                   :id "request-1" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *mevedel-turn-telemetry*"))
         captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request request))
          (cl-letf (((symbol-function 'mevedel-telemetry-record)
                     (lambda (_session event &rest props)
                       (setq captured (cons event props)))))
            (mevedel--turn-record-settlement
             (gptel-make-fsm
              :info (list :buffer chat-buf :status 200
                          :tokens-full '(:input 10 :output 3 :cached 2)))
             'success))
          (should (eq 'request-settled (car captured)))
          (should (equal "request-1" (plist-get (cdr captured) :request-id)))
          (should (= 10 (plist-get (cdr captured) :input-tokens)))
          (should (= 3 (plist-get (cdr captured) :output-tokens))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--run-turn-terminal-hook
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "reports Stop and StopFailure without coupling the hook to the ending request"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (chat-buf (generate-new-buffer " *mevedel-turn-hook*"))
         captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request request))
          (cl-letf (((symbol-function 'mevedel-workspace)
                     (lambda (&optional _buffer) ws))
                    ((symbol-function 'mevedel-hooks-event-plist)
                     (lambda (event _session _workspace &rest extra)
                       (cons event extra)))
                    ((symbol-function 'mevedel-hooks-run-event)
                     (lambda (event event-plist callback
                                    &optional session-arg workspace-arg
                                    request-arg invocation)
                       (push (list event event-plist session-arg
                                   workspace-arg request-arg invocation)
                             captured)
                       (funcall callback nil))))
            (let ((fsm (gptel-make-fsm
                        :info (list :buffer chat-buf
                                    :error '(:type "api"
                                             :message "backend failed")))))
              (mevedel--run-turn-terminal-hook fsm 'Stop 'completed)
              (mevedel--run-turn-terminal-hook fsm 'StopFailure 'aborted)))
          (let ((stop (cadr captured))
                (failure (car captured)))
            (should (eq 'Stop (car stop)))
            (should (equal "completed"
                           (plist-get (cdr (cadr stop)) :status)))
            (should-not (plist-get (cdr (cadr stop)) :terminal-reason))
            ;; No request: the turn's own teardown drains the request's
            ;; cancellers right after this hook, which would kill the
            ;; hook's process before it settles.
            (should-not (nth 4 stop))
            (should (eq 'StopFailure (car failure)))
            (should (equal "aborted"
                           (plist-get (cdr (cadr failure)) :status)))
            (should (equal "backend failed"
                           (plist-get (cdr (cadr failure))
                                      :terminal-reason)))
            (should-not (nth 4 failure))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-commit
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "commits the request's reserved turn exactly once"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-turn-count*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create
                         :session session :turn 1)))
          (mevedel--turn-commit fsm)
          (should (= 1 (mevedel-session-turn-count session)))
          (should-error (mevedel--turn-commit fsm) :type 'error))
      (kill-buffer chat-buf)))

  :doc "rejects drift between the committed and reserved clocks"
  (let* ((session (mevedel-session--create :turn-count 3))
         (chat-buf (generate-new-buffer " *mevedel-turn-drift*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create
                         :session session :turn 5)))
          (should-error (mevedel--turn-commit fsm) :type 'error)
          (should (= 3 (mevedel-session-turn-count session))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-autosave
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "saves writable sessions and skips read-only sessions"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-turn-save*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf)))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-session--read-only-mode nil))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (saved-session saved-buffer &optional settled)
                       (push (list saved-session saved-buffer settled)
                             saved))))
            (mevedel--turn-autosave fsm)
            (with-current-buffer chat-buf
              (setq-local mevedel-session--read-only-mode t))
            (mevedel--turn-autosave fsm))
          (should (equal (list (list session chat-buf t)) saved)))
      (kill-buffer chat-buf)))
  :doc "refreshes settled fork metadata without changing the composer draft"
  (let ((root (make-temp-file "mevedel-turn-fork-" t)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (let* ((root-dir (file-name-as-directory root))
                 (workspace
                  (mevedel-workspace-get-or-create
                   'project root-dir root-dir "fork"))
                 (session (mevedel-session-create "main" workspace))
                 (fsm (gptel-make-fsm :info (list :buffer data-buf)))
                 (mevedel-view-rerender-debounce 0)
                 (draft "> quoted\nsecond line"))
            (setf (mevedel-session-turn-count session) 1)
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (insert "*** Prompt\n")
              (insert
               (propertize "Settled response.\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (mevedel-view--full-rerender)
              (mevedel-view-test--insert-composer-draft draft 4)
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Settled response." nil t))
                (should-error (mevedel-view-fork-point-at-point)
                              :type 'user-error)))
            (mevedel--turn-autosave fsm)
            (with-current-buffer view-buf
              (should (equal draft (mevedel-view--input-text)))
              (should (= (point) (+ (mevedel-view--input-start) 4)))
              (goto-char (point-min))
              (should (search-forward "Settled response." nil t))
              (let ((target (mevedel-view-fork-point-at-point)))
                (should (stringp (plist-get target :fork-point-id)))
                (should (= 1 (plist-get target :cum-turn)))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel--turn-restore-permission-mode ()
  ,test
  (test)
  :doc "restores permission mode in the live request buffer"
  (let ((chat-buf (generate-new-buffer " *mevedel-turn-permission*"))
        called-buffer)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel--implementation-permission-mode-restore)
                   (lambda () (setq called-buffer (current-buffer)))))
          (mevedel--turn-restore-permission-mode
           (gptel-make-fsm :info (list :buffer chat-buf)))
          (should (eq chat-buf called-buffer)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-end-request ()
  ,test
  (test)
  :doc "ends the request in the live request buffer"
  (let ((chat-buf (generate-new-buffer " *mevedel-turn-end*"))
        called-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-request-end)
                   (lambda () (setq called-buffer (current-buffer)))))
          (mevedel--turn-end-request
           (gptel-make-fsm :info (list :buffer chat-buf)))
          (should (eq chat-buf called-buffer)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--run-turn-steps ()
  ,test
  (test)
  :doc "preserves step order and isolates an error from later steps"
  (let (events warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest warning) (push warning warnings))))
      (mevedel--run-turn-steps
       'fsm
       (list (lambda (_fsm) (push 'first events))
             (lambda (_fsm)
               (push 'broken events)
               (error "Step failed"))
             (lambda (_fsm) (push 'last events)))))
    (should (equal '(first broken last) (nreverse events)))
    (should (= 1 (length warnings)))))

(mevedel-deftest mevedel--complete-turn ()
  ,test
  (test)
  :doc "runs the successful transaction in order and drains after request end"
  (let ((chat-buf (generate-new-buffer " *mevedel-turn-complete*"))
        events)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--current-request 'live))
          (cl-letf (((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'mevedel--turn-commit)
                     (lambda (_fsm) (push 'turn events)))
                    ((symbol-function 'mevedel--compact-record-token-baseline)
                     (lambda (_fsm) (push 'baseline events)))
                    ((symbol-function 'mevedel--turn-autosave)
                     (lambda (_fsm) (push 'save events)))
                    ((symbol-function
                      'mevedel-plan-handoff-settle-request)
                     (lambda (_fsm status &optional _reason)
                       (push (list 'plan status) events)))
                    ((symbol-function 'mevedel--run-turn-terminal-hook)
                     (lambda (_fsm event status)
                       (push (list event status
                                   (with-current-buffer chat-buf
                                     mevedel--current-request))
                             events)))
                    ((symbol-function 'mevedel--turn-restore-permission-mode)
                     (lambda (_fsm) (push 'restore events)))
                    ((symbol-function 'mevedel--turn-end-request)
                     (lambda (_fsm)
                       (push 'request-end events)
                       (with-current-buffer chat-buf
                         (setq mevedel--current-request nil))))
                    ((symbol-function
                      'mevedel-view--schedule-follow-up-drain)
                     (lambda (_fsm)
                       (push (list 'drain
                                   (with-current-buffer chat-buf
                                     (null mevedel--current-request)))
                             events))))
            (mevedel--complete-turn
             (gptel-make-fsm :info (list :buffer chat-buf)))))
          (should (equal (nreverse events)
                         '(turn (plan success) baseline save
                           (Stop completed live)
                           restore request-end (drain t))))
          (with-current-buffer chat-buf
            (should-not mevedel--current-request)))
      (kill-buffer chat-buf)))

(mevedel-deftest mevedel--turn-after-publication ()
  ,test
  (test)
  :doc "blocks continuation while critical publication recovery is pending"
  (let* ((chat-buf (generate-new-buffer " *mevedel-publication-pending*"))
         (session (mevedel-session--create
                   :pending-publication '(:reason "offline")))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf)))
         (calls 0))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session))
          (mevedel--turn-after-publication
           (lambda (_fsm) (cl-incf calls)) fsm)
          (should (= 0 calls))
          (setf (mevedel-session-pending-publication session) nil)
          (mevedel--turn-after-publication
           (lambda (_fsm) (cl-incf calls)) fsm)
          (should (= 1 calls)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--fail-turn ()
  ,test
  (test)
  :doc "errors persist once while aborts skip autosave and follow-up drainage"
  (let (events drained)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'mevedel--turn-commit)
               (lambda (_fsm) (push 'turn events)))
              ((symbol-function 'mevedel--compact-record-token-baseline)
               (lambda (_fsm) (push 'baseline events)))
              ((symbol-function 'mevedel-goal-settle-failure)
               (lambda (_fsm &optional _status)
                 (push 'goal-failure events)))
              ((symbol-function 'mevedel-plan-handoff-settle-request)
               (lambda (_fsm status &optional _reason)
                 (push (list 'plan status) events)))
              ((symbol-function 'mevedel--fsm-error-message)
               (lambda (_fsm) "Provider failed"))
              ((symbol-function 'mevedel-goal-persist-failure)
               (lambda (_fsm) (push 'goal-save events)))
              ((symbol-function 'mevedel-goal-dispatch-after-turn)
               (lambda (_fsm) (push 'goal-retry events)))
              ((symbol-function 'mevedel--turn-record-request-failure)
               (lambda (_fsm) (push 'failure-record events)))
              ((symbol-function 'mevedel--turn-autosave)
               (lambda (_fsm) (push 'save events)))
              ((symbol-function 'mevedel--run-turn-terminal-hook)
               (lambda (_fsm event status)
                 (push (list event status) events)))
              ((symbol-function 'mevedel--turn-restore-permission-mode)
               (lambda (_fsm) (push 'restore events)))
              ((symbol-function 'mevedel--turn-fail-pending-input)
               (lambda (_fsm) (push 'pending-input-failure events)))
              ((symbol-function 'mevedel--turn-end-request)
               (lambda (_fsm) (push 'request-end events)))
              ((symbol-function
                'mevedel-view--schedule-follow-up-drain)
               (lambda (_fsm) (setq drained t))))
      (dolist (case '((error) (aborted)))
        (setq events nil)
        (mevedel--fail-turn 'fsm (car case))
        (should
         (equal
          (nreverse events)
          (append
           `(turn (plan ,(car case)) baseline goal-failure)
           (and (eq (car case) 'error)
                '(failure-record save))
           `((StopFailure ,(car case))
             restore pending-input-failure
             request-end goal-save goal-retry)))))
    (should-not drained)))

  :doc "error settlement tears down only its request-owned state"
  (let* ((session (mevedel-session--create
                   :name "turn-failure-isolation"
                   :agent-root-activity 'running))
         (request (mevedel-request--create
                   :id "request-current"
                   :session session
                   :origin "/root"
                   :started-at (current-time)))
         (registry '(("agent-storage" . retained-agent)))
         (chat-buf (generate-new-buffer " *mevedel-turn-isolation*"))
         (fsm
          (gptel-make-fsm
           :info
           (list :buffer chat-buf
                 :position (with-current-buffer chat-buf
                             (copy-marker (point-min)))
                 :status "HTTP/2 200"
                 :error '(:type "server_error" :message "Failed"))))
         current-outcome
         unrelated-outcome
         (cancellations 0)
         (records 0)
         (saves 0)
         (hooks 0)
         drained)
    (setf (mevedel-session-agent-registry session) registry
          (mevedel-session-permission-queue session)
          (list
           (list :kind 'generic
                 :request-id "request-current"
                 :origin "/root"
                 :callback (lambda (outcome)
                             (setq current-outcome outcome)))
           (list :kind 'generic
                 :request-id "request-unrelated"
                 :origin "/root/agent"
                 :callback (lambda (outcome)
                             (setq unrelated-outcome outcome)))))
    (mevedel-request-push-canceller
     request (lambda () (cl-incf cancellations)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request request))
          (cl-letf (((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'mevedel-telemetry-record) #'ignore)
                    ((symbol-function 'mevedel--turn-commit) #'ignore)
                    ((symbol-function 'mevedel--turn-record-settlement)
                     #'ignore)
                    ((symbol-function
                      'mevedel--compact-record-token-baseline)
                     #'ignore)
                    ((symbol-function 'mevedel-goal-settle-failure) #'ignore)
                    ((symbol-function 'mevedel--turn-record-request-failure)
                     (lambda (_fsm) (cl-incf records)))
                    ((symbol-function 'mevedel--turn-autosave)
                     (lambda (_fsm) (cl-incf saves)))
                    ((symbol-function 'mevedel--run-turn-terminal-hook)
                     (lambda (_fsm _event _status) (cl-incf hooks)))
                    ((symbol-function
                      'mevedel--turn-restore-permission-mode)
                     #'ignore)
                    ((symbol-function 'mevedel--turn-fail-pending-input)
                     #'ignore)
                    ((symbol-function 'mevedel-goal-persist-failure) #'ignore)
                    ((symbol-function 'mevedel-goal-dispatch-after-turn)
                     #'ignore)
                    ((symbol-function
                      'mevedel-permission-queue--render-entry)
                     #'ignore)
                    ((symbol-function
                      'mevedel-view--schedule-follow-up-drain)
                     (lambda (_fsm) (setq drained t))))
            (mevedel--fail-turn fsm 'error))
          (with-current-buffer chat-buf
            (should-not mevedel--current-request))
          (should (= 1 cancellations))
          (should (= 1 records))
          (should (= 1 saves))
          (should (= 1 hooks))
          (should (eq 'aborted current-outcome))
          (should-not unrelated-outcome)
          (should (equal registry
                         (mevedel-session-agent-registry session)))
          (should (eq 'idle
                      (mevedel-session-agent-root-activity session)))
          (let ((queue (mevedel-session-permission-queue session)))
            (should (= 1 (length queue)))
            (should (equal "request-unrelated"
                           (plist-get (car queue) :request-id))))
          (should-not drained))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-fail-pending-input ()
  ,test
  (test)

  :doc "error marks only remaining steering for the failed request"
  (let* ((chat-buf (generate-new-buffer " *mevedel-turn-input-failure*"))
         (view-buf (generate-new-buffer " *mevedel-turn-input-view*"))
         (session (mevedel-session--create))
         (request (mevedel-request--create :id "failed" :session session))
         (failed
          '(:id 1 :category steering :input "remaining"
            :request-id "failed"))
         (other
          '(:id 2 :category steering :input "other"
            :request-id "other"))
         (follow-ups
          '((:id 3 :category follow-up :input "later one")
            (:id 4 :category follow-up :input "later two")))
         redrawn)
    (unwind-protect
        (progn
          (setf (mevedel-session-pending-steering session)
                (list failed other)
                (mevedel-session-pending-follow-ups session)
                follow-ups)
          (with-current-buffer chat-buf
            (setq-local mevedel--session session
                        mevedel--current-request request
                        mevedel--view-buffer view-buf))
          (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () (setq redrawn t))))
            (mevedel--turn-fail-pending-input
             (gptel-make-fsm :info (list :buffer chat-buf))))
          (let ((entries (mevedel-session-pending-steering session)))
            (should (eq 'failed-turn (plist-get (car entries) :state)))
            (should (eq other (cadr entries))))
          (should (eq follow-ups
                      (mevedel-session-pending-follow-ups session)))
          (should
           (mevedel-session-pending-input-failure-paused session))
          (should redrawn))
      (kill-buffer chat-buf)
      (kill-buffer view-buf)))

  :doc "abort with no undelivered matching steering does not pause"
  (let* ((chat-buf (generate-new-buffer " *mevedel-turn-input-clean*"))
         (session (mevedel-session--create))
         (request (mevedel-request--create :id "finished" :session session))
         (other
          '(:id 2 :category steering :input "other"
            :request-id "other")))
    (unwind-protect
        (progn
          (setf (mevedel-session-pending-steering session) (list other))
          (with-current-buffer chat-buf
            (setq-local mevedel--session session
                        mevedel--current-request request))
          (mevedel--turn-fail-pending-input
           (gptel-make-fsm :info (list :buffer chat-buf)))
          (should (equal (list other)
                         (mevedel-session-pending-steering session)))
          (should-not
           (mevedel-session-pending-input-failure-paused session)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--handler-name ()
  ,test
  (test)
  :doc "formats named, anonymous, and non-function handlers compactly"
  (should (equal "ignore" (mevedel--handler-name #'ignore)))
  (should (equal "#<function>"
                 (mevedel--handler-name (lambda (_fsm) nil))))
  (should (equal "42" (mevedel--handler-name 42))))

(mevedel-deftest mevedel--safe-fsm-handler ()
  ,test
  (test)
  :doc "returns handler values and converts errors to warnings"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest warning) (push warning warnings))))
      (should (eq 'ok
                  (funcall (mevedel--safe-fsm-handler
                            (lambda (_fsm) 'ok))
                           'fsm)))
      (should-not
       (funcall (mevedel--safe-fsm-handler
                 (lambda (_fsm) (error "Handler failed")))
                'fsm)))
    (should (= 1 (length warnings)))))

(provide 'test-mevedel-turn)
;;; test-mevedel-turn.el ends here
