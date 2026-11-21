;;; test-niwa-turn.el --- Tests for niwa-turn.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests canonical turn settlement independently of preset construction.

;;; Code:

(require 'gptel)
(require 'niwa-hooks)
(require 'niwa-permission-queue)
(require 'niwa-permission-mode)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-tool-render-data)
(require 'niwa-transport)
(require 'niwa-turn)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-turn-end-at-boundary ()
  ,test
  (test)
  :doc "records the first reason once and reports it to the transition rule"
  (let ((fsm (gptel-make-fsm :info (list :buffer nil))))
    (should-not (niwa-turn-end-requested-p (gptel-fsm-info fsm)))
    (should (niwa-turn-end-at-boundary fsm 'first))
    (should-not (niwa-turn-end-at-boundary fsm 'second))
    (should (eq 'first (niwa-turn-end-requested-p (gptel-fsm-info fsm))))
    (should-not (niwa-turn-end-at-boundary nil 'none))))

(niwa-deftest niwa--turn-stamp-settled ()
  ,test
  (test)
  :doc "writes a monotone settlement stamp on the real FSM info"
  (let ((fsm (gptel-make-fsm :info '(:buffer ignored))))
    (niwa--turn-stamp-settled fsm)
    (should (plist-get (gptel-fsm-info fsm) :niwa-turn-settled))
    (niwa--turn-stamp-settled fsm)
    (should (plist-get (gptel-fsm-info fsm) :niwa-turn-settled))))

(niwa-deftest niwa--fsm-error-message ()
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
                   (niwa--fsm-error-message
                    (gptel-make-fsm :info (car case)))))))

(niwa-deftest niwa--turn-record-request-failure ()
  ,test
  (test)
  :doc "persists complete provider failure details in ignored render-data"
  (let* ((session (niwa-session--create :name "turn-failure"))
         (request (niwa-request--create
                   :id "request-1" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *niwa-turn-failure*"))
         (backend
          (gptel-make-openai
           "Codex failure test" :key "test" :models '(test-model)))
         (message-text "An error occurred while processing your request.")
         (retry-errors
          '((:attempt 1 :http-status "503" :type "server_error"
             :code "server_error" :message "Model at capacity")
            (:attempt 2 :http-status "200" :type "server_error"
             :code "server_error" :message "Still unavailable")))
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
                 :niwa-retry-errors retry-errors
                 :niwa-retry-exhausted t
                 :error error-data))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request request)
            (insert "Partial response\n"))
          (niwa--turn-record-request-failure fsm)
          (with-current-buffer chat-buf
            (let* ((block-start
                    (progn
                      (goto-char (point-min))
                      (search-forward "<!-- niwa-render-data -->")
                      (match-beginning 0)))
                   (data
                    (cdr
                     (niwa-tool-render-data-extract
                      (buffer-substring block-start (point-max))))))
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
              (should (equal retry-errors (plist-get data :retry-errors)))
              (should (eq t (plist-get data :retry-exhausted)))
              (should (eq 'manual (plist-get data :retry)))
              (should (eq 'niwa-render-data
                          (get-text-property block-start 'gptel)))
              (should (eq t
                          (get-text-property
                           block-start 'niwa-render-data))))))
      (kill-buffer chat-buf)))

  :doc "persists plain-string provider errors without losing their text"
  (let* ((session (niwa-session--create :name "turn-string-failure"))
         (request (niwa-request--create
                   :id "request-2" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *niwa-turn-string-failure*"))
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
            (setq-local niwa--session session)
            (setq-local niwa--current-request request)
            (insert "Partial response\n"))
          (niwa--turn-record-request-failure fsm)
          (with-current-buffer chat-buf
            (goto-char (point-min))
            (search-forward "<!-- niwa-render-data -->")
            (let ((data
                   (cdr
                    (niwa-tool-render-data-extract
                     (buffer-substring
                      (match-beginning 0) (point-max))))))
              (should (equal message-text (plist-get data :error-data)))
              (should (equal message-text (plist-get data :message)))
              (should-not (plist-get data :retry-errors))
              (should-not (plist-get data :retry-exhausted)))))
      (kill-buffer chat-buf)))

  :doc "reports provider failures as informational; only a structured model miss moves the session"
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai "Failing" :key "test" :models '(gone)))
         (fallback (gptel-make-openai "Fallback" :key "test" :models '(kept)))
         (niwa-model-fallback-provider "Fallback:kept")
         (chat-buf (generate-new-buffer " *niwa-turn-model-failure*"))
         (session (niwa-session--create :name "turn-model-failure"
                                           :root-buffer chat-buf
                                           :model-provider "Failing:gone")))
    (ignore fallback)
    (unwind-protect
        (with-current-buffer chat-buf
          (setq-local niwa--session session gptel-backend backend gptel-model 'gone)
          (cl-flet ((fail (error status)
                      (niwa--turn-record-request-failure
                       (gptel-make-fsm
                        :info (list :buffer chat-buf :backend backend :model 'gone
                                    :position (copy-marker (point-max))
                                    :http-status status :error error)))))
            ;; Overload text naming the model is transient, not a missing model.
            (fail '(:type "overloaded" :message "The model is unavailable, retry later") "503")
            (should-not (niwa-recovery-blocker session))
            (should (equal "Failing:gone" (niwa-session-model-provider session)))
            ;; Credential text is a labelled, non-blocking notice.
            (fail '(:message "No auth credentials found") "401")
            (should-not (niwa-recovery-blocker session))
            (should (equal "authentication"
                           (plist-get (car (niwa-session-recovery-issues session)) :category)))
            (should-not (string-search "credentials"
                                       (plist-get (car (niwa-session-recovery-issues session)) :message)))
            (niwa-test--with-captured-messages nil
              (fail '(:code "model_not_found" :message "Model gone does not exist") "404"))
            (should (equal "Fallback:kept" (niwa-session-model-provider session)))
            (should (eq 'kept gptel-model))
            (should (string-search "using Fallback:kept"
                                   (plist-get (cl-find "model" (niwa-session-recovery-issues session)
                                                       :key (lambda (issue) (plist-get issue :id))
                                                       :test #'equal)
                                              :message)))
            (should-not (niwa-recovery-blocker session))))
      (kill-buffer chat-buf)))

  :doc "superseded selections never change the session model"
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai "Scoped" :key "test" :models '(gone chosen)))
         (fallback (gptel-make-openai "Fallback" :key "test" :models '(kept)))
         (niwa-model-fallback-provider "Fallback:kept")
         (selection "Scoped:chosen"))
    (ignore fallback)
    (with-temp-buffer
      (let* ((session (niwa-session--create
                       :name "scoped-failure" :root-buffer (current-buffer)
                       :model-provider selection))
             (request (niwa-request--create :session session)))
        (setq-local niwa--session session
                    gptel-backend backend gptel-model 'chosen)
        (niwa-test--with-captured-messages nil
          (niwa--turn-record-request-failure
           (gptel-make-fsm
            :info (list :buffer (current-buffer) :backend backend :model 'gone
                        :niwa-request request :position (point-marker)
                        :http-status "404" :error '(:code "model_not_found")))))
        (should (equal selection (niwa-session-model-provider session)))
        (should (eq 'chosen gptel-model))))))

(niwa-deftest niwa--turn-record-settlement
  ()
  ,test
  (test)
  :doc "correlates terminal provider tokens with the active request"
  (let* ((session (niwa-session--create :name "turn-telemetry"))
         (request (niwa-request--create
                   :id "request-1" :session session :origin "/root"
                   :started-at (current-time)))
         (chat-buf (generate-new-buffer " *niwa-turn-telemetry*"))
         captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request request))
          (cl-letf (((symbol-function 'niwa-telemetry-record)
                     (lambda (_session event &rest props)
                       (setq captured (cons event props)))))
            (niwa--turn-record-settlement
             (gptel-make-fsm
              :info (list :buffer chat-buf :status 200
                          :tokens-full '(:input 10 :output 3 :cached 2)))
             'success))
          (should (eq 'request-settled (car captured)))
          (should (equal "request-1" (plist-get (cdr captured) :request-id)))
          (should (= 10 (plist-get (cdr captured) :input-tokens)))
          (should (= 3 (plist-get (cdr captured) :output-tokens))))
      (kill-buffer chat-buf)))
  :doc "runs the settled functions in the chat buffer, past a failing one"
  (let* ((chat-buf (generate-new-buffer " *niwa-turn-settled*"))
         seen messages)
    (unwind-protect
        (let ((niwa-turn-settled-functions
               (list (lambda (_outcome) (error "Broken step"))
                     (lambda (outcome) (push (cons outcome (current-buffer)) seen)))))
          (niwa-test--with-captured-messages messages
            (niwa--turn-record-settlement
             (gptel-make-fsm :info (list :buffer chat-buf)) 'error))
          (should (string-match-p "Broken step" messages))
          (should (equal (list (cons 'error chat-buf)) seen)))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--run-turn-terminal-hook
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "reports Stop and StopFailure with the ending request's hook context"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (request (niwa-request--create :session session))
         (chat-buf (generate-new-buffer " *niwa-turn-hook*"))
         captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request request))
          (cl-letf (((symbol-function 'niwa-workspace)
                     (lambda (&optional _buffer) ws))
                    ((symbol-function 'niwa-hooks-event-plist)
                     (lambda (event _session _workspace &rest extra)
                       (cons event extra)))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (event event-plist callback
                                    &optional session-arg workspace-arg
                                    request-arg invocation)
                       (push (list event event-plist session-arg
                                   workspace-arg request-arg invocation
                                   (bound-and-true-p
                                    niwa--current-request))
                             captured)
                       (funcall callback nil))))
            (let ((fsm (gptel-make-fsm
                        :info (list :buffer chat-buf
                                    :error '(:type "api"
                                             :message "backend failed")))))
              (niwa--run-turn-terminal-hook fsm 'Stop 'completed)
              (niwa--run-turn-terminal-hook fsm 'StopFailure 'aborted)))
          (let ((stop (cadr captured))
                (failure (car captured)))
            (should (eq 'Stop (car stop)))
            (should (equal "completed"
                           (plist-get (cdr (cadr stop)) :status)))
            (should-not (plist-get (cdr (cadr stop)) :terminal-reason))
            ;; The runner needs the request's skill rules and payload
            ;; context; it detaches only the handlers' teardown ownership.
            (should (eq request (nth 4 stop)))
            (should (eq request (nth 6 stop)))
            (should (eq request (nth 6 failure)))
            (should (eq request
                        (buffer-local-value 'niwa--current-request
                                            chat-buf)))
            (should (eq 'StopFailure (car failure)))
            (should (equal "aborted"
                           (plist-get (cdr (cadr failure)) :status)))
            (should (equal "backend failed"
                           (plist-get (cdr (cadr failure))
                                      :terminal-reason)))
            (should (eq request (nth 4 failure)))))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--turn-settled-p ()
  ,test
  (test)
  :doc "a turn with a live request still has settlement work"
  (let* ((session (niwa-session--create :turn-count 0))
         (chat-buf (generate-new-buffer " *niwa-turn-settled*"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :niwa-request-id "live"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create
                         :id "live" :session session :turn 1)))
          (should-not (niwa--turn-settled-p fsm)))
      (kill-buffer chat-buf)))

  :doc "a second terminal transition finds its own settlement stamp"
  (let* ((session (niwa-session--create :turn-count 1))
         (chat-buf (generate-new-buffer " *niwa-turn-resettled*"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :niwa-request-id "done"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request nil))
          (niwa--turn-stamp-settled fsm)
          (should (niwa--turn-settled-p fsm))
          (should-not (niwa--turn-lost-p fsm))
          ;; The commit fence still signals for a direct caller; the
          ;; terminal entry points return before reaching it.
          (niwa--fail-turn fsm 'error)
          (niwa--complete-turn fsm)
          (should (= 1 (niwa-session-turn-count session))))
      (kill-buffer chat-buf)))

  :doc "a request-id-less machine never began a niwa turn"
  (let* ((session (niwa-session--create :turn-count 1))
         (chat-buf (generate-new-buffer " *niwa-turn-unbegun*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request nil))
          (should (niwa--turn-settled-p fsm))
          (niwa--complete-turn fsm)
          (should (plist-get (gptel-fsm-info fsm)
                             :niwa-turn-settled))
          (should (= 1 (niwa-session-turn-count session))))
      (kill-buffer chat-buf)))

  :doc "a delayed terminal transition cannot settle a newer request"
  (let* ((session (niwa-session--create :turn-count 1))
         (chat-buf (generate-new-buffer " *niwa-turn-stale*"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf :niwa-request-id "old")))
         (request (niwa-request--create
                   :id "new" :session session :turn 2)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request request))
          (should (niwa--turn-settled-p fsm))
          (niwa--fail-turn fsm 'error)
          (niwa--complete-turn fsm)
          (should (= 1 (niwa-session-turn-count session)))
          (should (plist-get (gptel-fsm-info fsm)
                             :niwa-turn-settled))
          (should (eq request
                      (buffer-local-value 'niwa--current-request
                                          chat-buf)))
          (with-current-buffer chat-buf
            (setq-local niwa--current-request nil))
          (niwa--fail-turn fsm 'error)
          (should (= 1 (niwa-session-turn-count session))))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--turn-lost-p ()
  ,test
  (test)
  :doc "an identified empty request slot without the stamp is lost"
  (let* ((session (niwa-session--create :turn-count 1))
         (chat-buf (generate-new-buffer " *niwa-turn-lost*"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf
                           :niwa-request-id "lost"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request nil))
          (should-not (niwa--turn-settled-p fsm))
          (should (niwa--turn-lost-p fsm)))
      (kill-buffer chat-buf)))

  :doc "an unbegun machine and a dead request buffer are not lost"
  (let* ((chat-buf (generate-new-buffer " *niwa-turn-unbegun*"))
         (unbegun (gptel-make-fsm :info (list :buffer chat-buf)))
         (identified
          (gptel-make-fsm
           :info (list :buffer chat-buf :niwa-request-id "dead"))))
    (should-not (niwa--turn-lost-p unbegun))
    (kill-buffer chat-buf)
    (should-not (niwa--turn-lost-p identified))))

(niwa-deftest niwa--turn-record-lost-settlement ()
  ,test
  (test)
  :doc "records the machine-owned request identity as a real telemetry event"
  (let* ((session (niwa-session--create
                   :name "lost-telemetry" :session-id "lost-telemetry"))
         (chat-buf (generate-new-buffer " *niwa-turn-lost-telemetry*"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf
                           :niwa-request-id "lost-1"
                           :status "finished"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session))
          (niwa--turn-record-lost-settlement fsm)
          (should (eq 'lost (niwa-session-last-outcome session)))
          (should (equal "request" (plist-get (car (niwa-session-recovery-issues session)) :id)))
          (let ((entry (car (niwa-session-telemetry-pending session))))
            (should (eq 'request-settled (plist-get entry :event)))
            (should (equal "lost-1" (plist-get entry :request-id)))
            (should (eq 'lost (plist-get entry :outcome)))
            (should (equal "finished" (plist-get entry :provider-status)))))
      (kill-buffer chat-buf))))


(niwa-deftest niwa--turn-settle-lost
  (:quiet t
   :before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "distinct lost turns defer independently and block admission until settled"
  (let* ((root (make-temp-file "niwa-turn-settle-lost-" t))
         (workspace
          (niwa-workspace-get-or-create
           'project root root "lost-settlement"))
         (_identity (niwa-workspace-identity-ensure root))
         (session (niwa-session-create "main" workspace))
         (chat-buf (generate-new-buffer " *niwa-turn-settle-lost*"))
         (fsms (mapcar
                (lambda (id)
                  (gptel-make-fsm
                   :info (list :buffer chat-buf
                               :niwa-request-id id
                               :status "finished")))
                '("lost-1" "lost-2")))
         (niwa-transport-retry-seconds 0.01)
         hook-events)
    (unwind-protect
        (progn
          (setf (niwa-session-turn-count session) 1
                (niwa-session-agent-root-activity session) 'running
                (niwa-session-permission-mode session) 'full-auto)
          (with-current-buffer chat-buf
            (org-mode)
            (setq-local niwa--session session)
            (setq-local niwa--current-request nil)
            (insert "Lost response\n"))
          (let ((niwa-stop-failure-functions
                 (list (lambda (event) (push event hook-events)))))
            (niwa-transport--handler-advice
             (lambda (&rest _)
               (dolist (fsm fsms)
                 (niwa--fail-turn fsm 'error))
               (with-current-buffer chat-buf
                 (should (= 2 (length niwa--turn-settlements-pending)))
                 (should (niwa-turn-busy-p))
                 (should (equal "settling" (niwa-request-state-label)))
                 (should-error (niwa-request-begin session)
                               :type 'user-error))
               (should (= 2 (hash-table-count niwa-transport--pending))))
             'file-exists-p "/ssh:test:/lost")
            (niwa-transport-cancel-pending)
            (with-current-buffer chat-buf
              (should-not (niwa-turn-busy-p))
              (should (eq 'full-auto (niwa-session-permission-mode session))))
            (dolist (fsm fsms)
              (should-not (plist-get (gptel-fsm-info fsm)
                                     :niwa-turn-settled))
              (niwa--fail-turn fsm 'error))
            (let ((deadline (+ (float-time) 5)))
              (while (and (niwa-turn-busy-p chat-buf) (< (float-time) deadline)) (sleep-for .002))))
          (should (niwa-session-save-path session))
          ;; Failure without undelivered steering leaves delivery running.
          (should-not (niwa-session-pending-input-failure-paused session))
          (should (niwa-session-artifacts-artifact-present-p
                       session (format "segment-%04d.chat.org"
                                       (niwa-session-current-segment session)) t))
          (with-temp-buffer
            (insert-file-contents
             (file-name-concat (niwa-session-save-path session)
                               "telemetry-log.el"))
            (let (settled)
              (condition-case nil
                  (while t
                    (let ((entry (read (current-buffer))))
                      (when (eq 'request-settled (plist-get entry :event))
                        (push entry settled))))
                (end-of-file))
              (should (= 2 (length settled)))
              (should (equal '("lost-1" "lost-2")
                             (sort (mapcar
                                    (lambda (entry)
                                      (plist-get entry :request-id))
                                    settled)
                                   #'string-lessp)))
              (should (seq-every-p
                       (lambda (entry)
                         (eq 'lost (plist-get entry :outcome)))
                       settled))))
          (should (= 2 (length hook-events)))
          (should (seq-every-p
                   (lambda (event)
                     (and (equal "lost" (plist-get event :status))
                          (equal "lost"
                                 (plist-get event :terminal-reason))))
                   hook-events))
          (should (eq 'idle (niwa-session-agent-root-activity session)))
          ;; Lost implementation teardown cannot restore over a live baseline.
          (should (eq 'full-auto (niwa-session-permission-mode session)))
          (should (= 1 (niwa-session-turn-count session)))
          (should-not (buffer-local-value
                       'niwa--turn-settlements-pending chat-buf))
          (dolist (fsm fsms)
            (should (niwa--turn-settled-p fsm))
            (niwa--complete-turn fsm))
          (should (= 2 (length hook-events))))
      (when (buffer-live-p chat-buf)
        (set-buffer chat-buf)
        (set-buffer-modified-p nil)
        (kill-buffer chat-buf))
      (niwa-transport-cancel-pending)
      (delete-directory root t))))

(niwa-deftest niwa--turn-commit
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "commits the request's reserved turn exactly once"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-turn-count*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create
                         :session session :turn 1)))
          (niwa--turn-commit fsm)
          (should (= 1 (niwa-session-turn-count session)))
          (should-error (niwa--turn-commit fsm) :type 'error))
      (kill-buffer chat-buf)))

  :doc "rejects drift between the committed and reserved clocks"
  (let* ((session (niwa-session--create :turn-count 3))
         (chat-buf (generate-new-buffer " *niwa-turn-drift*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create
                         :session session :turn 5)))
          (should-error (niwa--turn-commit fsm) :type 'error)
          (should (= 3 (niwa-session-turn-count session))))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--turn-save
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "saves writable sessions and skips read-only sessions"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (niwa-session-create "main" ws))
         (chat-buf (generate-new-buffer " *niwa-turn-save*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf)))
         saved offered cleanup recovered)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa-session--read-only-mode nil))
          (setf (niwa-session-root-buffer session) chat-buf)
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (saved-session saved-buffer &optional settled)
                       (push (list saved-session saved-buffer settled)
                             saved)))
                    ((symbol-function 'niwa-journal-cleanup-schedule)
                     (lambda (workspace) (push workspace cleanup)))
                    ((symbol-function 'niwa-memory-decision-schedule-recovery)
                     (lambda (workspace &optional callback)
                       (push workspace recovered)
                       (when callback (funcall callback))))
                    ((symbol-function 'niwa-memory-pass-schedule)
                     (lambda (workspace) (push workspace offered))))
            (niwa--run-turn-steps fsm '(niwa--turn-save niwa--turn-checkpoint))
            (with-current-buffer chat-buf
              (setq-local niwa-session--read-only-mode t))
            (niwa--run-turn-steps fsm '(niwa--turn-save niwa--turn-checkpoint)))
          (should (equal (list (list session chat-buf t)) saved))
          (should (equal (list ws) offered))
          (should (equal (list ws) cleanup))
          (should (equal (list ws) recovered)))
      (kill-buffer chat-buf)))
  :doc "restores settled ToolCall checkpoints when the co-commit fails"
  (let* ((session
          (niwa-session--create
           :name "failed-save"
           :ptc-checkpoints '((:id "ptc" :conversation "/root" :state settled))))
         (chat-buf (generate-new-buffer " *niwa-turn-failed-save*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) (error "Disk failed"))))
            (niwa-test--with-captured-diagnostics nil
              (niwa--run-turn-steps fsm '(niwa--turn-save niwa--turn-checkpoint))))
          (should (equal '((:id "ptc" :conversation "/root" :state settled))
                         (niwa-session-ptc-checkpoints session))))
      (kill-buffer chat-buf)))
  :doc "refreshes settled fork metadata without changing the composer draft"
  (let ((root (make-temp-file "niwa-turn-fork-" t)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let* ((root-dir (file-name-as-directory root))
                 (workspace
                  (niwa-workspace-get-or-create
                   'project root-dir root-dir "fork"))
                 (session (niwa-session-create "main" workspace))
                 (fsm (gptel-make-fsm :info (list :buffer data-buf)))
                 (niwa-view-rerender-debounce 0)
                 (draft "> quoted\nsecond line"))
            (setf (niwa-session-turn-count session) 1)
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (insert "*** Prompt\n")
              (insert
               (propertize "Settled response.\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft draft 4)
              (save-excursion
                (goto-char (point-min))
                (should (search-forward "Settled response." nil t))
                (should-error (niwa-view-fork-point-at-point)
                              :type 'user-error)))
            (niwa--run-turn-steps fsm '(niwa--turn-save niwa--turn-checkpoint))
            (with-current-buffer view-buf
              (should (equal draft (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))
              (goto-char (point-min))
              (should (search-forward "Settled response." nil t))
              (let ((target (niwa-view-fork-point-at-point)))
                (should (stringp (plist-get target :fork-point-id)))
                (should (= 1 (plist-get target :cum-turn)))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "refreshes only the newest turn instead of rebuilding history"
  (let ((root (make-temp-file "niwa-turn-fork-" t)))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let* ((root-dir (file-name-as-directory root))
                 (workspace
                  (niwa-workspace-get-or-create
                   'project root-dir root-dir "fork"))
                 (session (niwa-session-create "main" workspace))
                 (fsm (gptel-make-fsm :info (list :buffer data-buf)))
                 (niwa-view-rerender-debounce 0)
                 (draft "> quoted\nsecond line")
                 full earlier)
            (setf (niwa-session-turn-count session) 2)
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (insert "*** Earlier prompt\n")
              (insert (propertize "Earlier response.\n" 'gptel 'response))
              (insert "*** Prompt\n")
              (insert (propertize "Settled response.\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft draft 4)
              (save-excursion
                (goto-char (point-min))
                (search-forward "Earlier response.")
                (setq earlier (get-text-property (point) 'niwa-view-turn-id))))
            (cl-letf (((symbol-function 'niwa-view-render--full-now)
                       (lambda (&rest _) (setq full t)))
                      ((symbol-function 'niwa-view-render--start-batch)
                       (lambda (&rest _) (setq full t))))
              (niwa--run-turn-steps
               fsm '(niwa--turn-save niwa--turn-checkpoint)))
            (with-current-buffer view-buf
              (should-not full)
              (should (equal draft (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))
              (goto-char (point-min))
              (search-forward "Earlier response.")
              (should (eq earlier
                          (get-text-property (point) 'niwa-view-turn-id)))
              (search-forward "Settled response.")
              (should (= 2 (plist-get (niwa-view-fork-point-at-point)
                                      :cum-turn))))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa--turn-clear-pending-policy ()
  ,test
  (test)
  :doc "clears unadmitted policy only for its exact submission identity"
  (let ((chat-buf (generate-new-buffer " *niwa-turn-permission*"))
        (policy (list :permission-mode 'edits)))
    (unwind-protect
        (with-current-buffer chat-buf
          (setq-local niwa--pending-implementation-policy policy)
          (niwa--turn-clear-pending-policy
           (gptel-make-fsm :info (list :buffer chat-buf
                                      :niwa-pending-implementation-policy (copy-sequence policy))))
          (should (eq policy niwa--pending-implementation-policy))
          (niwa--turn-clear-pending-policy
           (gptel-make-fsm :info (list :buffer chat-buf
                                      :niwa-pending-implementation-policy policy)))
          (should-not niwa--pending-implementation-policy))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--turn-end-request ()
  ,test
  (test)
  :doc "ends the request in the live request buffer"
  (let ((chat-buf (generate-new-buffer " *niwa-turn-end*"))
        called-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-request-end)
                   (lambda () (setq called-buffer (current-buffer)))))
          (niwa--turn-end-request
           (gptel-make-fsm :info (list :buffer chat-buf)))
          (should (eq chat-buf called-buffer)))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--run-turn-steps ()
  ,test
  (test)
  :doc "preserves step order and isolates an error from later steps"
  (let (events warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest warning) (push warning warnings))))
      (niwa--run-turn-steps
       (gptel-make-fsm :info nil)
       (list (lambda (_fsm) (push 'first events))
             (lambda (_fsm)
               (push 'broken events)
               (error "Step failed"))
             (lambda (_fsm) (push 'last events)))))
    (should (equal '(first broken last) (nreverse events)))
    (should (= 1 (length warnings)))))

(niwa-deftest niwa--complete-turn
  (:vars ((niwa-transport--enabled-p t)))
  ,test
  (test)
  :doc "runs the successful transaction in order and drains after request end"
  (let ((chat-buf (generate-new-buffer " *niwa-turn-complete*"))
        (request (niwa-request--create :id "complete"))
        events)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--current-request request))
          (cl-letf (((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'niwa--turn-commit)
                     (lambda (_fsm) (push 'turn events)))
                    ((symbol-function 'niwa-compact-estimation-record-token-baseline)
                     (lambda (_fsm) (push 'baseline events)))
                    ((symbol-function 'niwa--turn-save)
                     (lambda (_fsm) (push 'save events)))
                    ((symbol-function
                      'niwa-plan-handoff-settle-request)
                     (lambda (_fsm status &optional _reason)
                       (push (list 'plan status) events)))
                    ((symbol-function 'niwa--run-turn-terminal-hook)
                     (lambda (_fsm event status)
                       (push (list event status
                                   (with-current-buffer chat-buf
                                     (and niwa--current-request 'live)))
                             events)))
                    ((symbol-function 'niwa--turn-end-request)
                     (lambda (_fsm)
                       (push 'request-end events)
                       (with-current-buffer chat-buf
                         (setq niwa--current-request nil))))
                    ((symbol-function
                      'niwa-view--schedule-follow-up-drain)
                     (lambda (_fsm)
                       (push (list 'drain
                                   (with-current-buffer chat-buf
                                     (null niwa--current-request)))
                             events)))
                    ((symbol-function
                      'niwa-pending-inputs-offer-event-after-turn)
                     (lambda (_fsm) (push 'event-offer events))))
            (niwa--complete-turn
             (gptel-make-fsm
              :info (list :buffer chat-buf
                          :niwa-request-id "complete")))))
          (should (equal (nreverse events)
                         '(turn (plan success) baseline save
                           (Stop completed live)
                           request-end (drain t) event-offer)))
          (with-current-buffer chat-buf
            (should-not niwa--current-request)))
      (kill-buffer chat-buf)))

(niwa-deftest niwa--turn-after-publication ()
  ,test
  (test)
  :doc "blocks continuation while critical publication recovery is pending"
  (let* ((chat-buf (generate-new-buffer " *niwa-publication-pending*"))
         (session (niwa-session--create
                   :pending-publication '(:reason "offline")))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf)))
         (calls 0))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session))
          (niwa--turn-after-publication
           (lambda (_fsm) (cl-incf calls)) fsm)
          (should (= 0 calls))
          (setf (niwa-session-pending-publication session) nil)
          (niwa--turn-after-publication
           (lambda (_fsm) (cl-incf calls)) fsm)
          (should (= 1 calls)))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--fail-turn ()
  ,test
  (test)
  :doc "errors and aborts persist once without follow-up drainage"
  (let (events drained event-offered)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'niwa--turn-commit)
               (lambda (_fsm) (push 'turn events)))
              ((symbol-function 'niwa-compact-estimation-record-token-baseline)
               (lambda (_fsm) (push 'baseline events)))
              ((symbol-function 'niwa-goal-settle-failure)
               (lambda (_fsm &optional _status)
                 (push 'goal-failure events)))
              ((symbol-function 'niwa-plan-handoff-settle-request)
               (lambda (_fsm status &optional _reason)
                 (push (list 'plan status) events)))
              ((symbol-function 'niwa--fsm-error-message)
               (lambda (_fsm) "Provider failed"))
              ((symbol-function 'niwa-goal-persist-failure)
               (lambda (_fsm) (push 'goal-save events)))
              ((symbol-function 'niwa-goal-dispatch-after-turn)
               (lambda (_fsm) (push 'goal-retry events)))
              ((symbol-function 'niwa--turn-record-request-failure)
               (lambda (_fsm) (push 'failure-record events)))
              ((symbol-function 'niwa--turn-save)
               (lambda (_fsm) (push 'save events)))
              ((symbol-function 'niwa--run-turn-terminal-hook)
               (lambda (_fsm event status)
                 (push (list event status) events)))
              ((symbol-function 'niwa--turn-fail-pending-input)
               (lambda (_fsm &optional _status) (push 'pending-input-failure events)))
              ((symbol-function 'niwa--turn-end-request)
               (lambda (_fsm) (push 'request-end events)))
              ((symbol-function
                'niwa-view--schedule-follow-up-drain)
               (lambda (_fsm) (setq drained t)))
              ;; A failed turn must not wake again for background work, or
              ;; a failing wake would loop.
              ((symbol-function 'niwa-pending-inputs-offer-event-after-turn)
               (lambda (_fsm) (setq event-offered t))))
      (dolist (case '((error) (aborted)))
        (setq events nil)
        (niwa--fail-turn
         (gptel-make-fsm
          :info (list :niwa-request-id (symbol-name (car case))))
         (car case))
        (should
         (equal
          (nreverse events)
          (append
           `(turn (plan ,(car case)) baseline goal-failure)
           (and (eq (car case) 'error)
                '(failure-record))
           `(pending-input-failure save (StopFailure ,(car case))
             request-end goal-save goal-retry)))))
    (should-not drained)
    (should-not event-offered)))

  :doc "transport rejection still performs local failure admission cleanup"
  (let* ((chat-buf (generate-new-buffer " *niwa-turn-rejected*"))
         (request (niwa-request--create :id "rejected"))
         (fsm (gptel-make-fsm
               :info (list :buffer chat-buf
                           :niwa-request-id "rejected")))
         events)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--current-request request))
          (cl-letf (((symbol-function 'niwa--turn-commit) #'ignore)
                    ((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (&rest _) nil))
                    ((symbol-function 'niwa--turn-fail-pending-input)
                     (lambda (_ &optional _status) (push 'pending-input-failure events)))
                    ((symbol-function 'niwa--turn-end-request)
                     (lambda (_)
                       (push 'request-end events)
                       (with-current-buffer chat-buf
                         (setq niwa--current-request nil)))))
            (niwa--fail-turn fsm 'aborted))
          (should (equal '(pending-input-failure request-end)
                         (nreverse events)))
          (with-current-buffer chat-buf
            (should-not niwa--current-request)))
      (kill-buffer chat-buf)))

  :doc "error settlement tears down only its request-owned state"
  (let* ((session (niwa-session--create
                   :name "turn-failure-isolation"
                   :agent-root-activity 'running))
         (request (niwa-request--create
                   :id "request-current"
                   :session session
                   :origin "/root"
                   :started-at (current-time)))
         (registry '(("agent-storage" . retained-agent)))
         (chat-buf (generate-new-buffer " *niwa-turn-isolation*"))
         (fsm
          (gptel-make-fsm
           :info
           (list :buffer chat-buf
                 :niwa-request-id "request-current"
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
    (setf (niwa-session-agent-registry session) registry
          (niwa-session-permission-queue session)
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
    (niwa-request-push-canceller
     request (lambda () (cl-incf cancellations)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request request))
          (cl-letf (((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'niwa-telemetry-record) #'ignore)
                    ((symbol-function 'niwa--turn-commit) #'ignore)
                    ((symbol-function 'niwa--turn-record-settlement)
                     #'ignore)
                    ((symbol-function
                      'niwa-compact-estimation-record-token-baseline)
                     #'ignore)
                    ((symbol-function 'niwa-goal-settle-failure) #'ignore)
                    ((symbol-function 'niwa--turn-record-request-failure)
                     (lambda (_fsm) (cl-incf records)))
                    ((symbol-function 'niwa--turn-save)
                     (lambda (_fsm) (cl-incf saves)))
                    ((symbol-function 'niwa--run-turn-terminal-hook)
                     (lambda (_fsm _event _status) (cl-incf hooks)))
                    ((symbol-function 'niwa--turn-fail-pending-input)
                     #'ignore)
                    ((symbol-function 'niwa-goal-persist-failure) #'ignore)
                    ((symbol-function 'niwa-goal-dispatch-after-turn)
                     #'ignore)
                    ((symbol-function
                      'niwa-permission-queue--render-entry)
                     #'ignore)
                    ((symbol-function
                      'niwa-view--schedule-follow-up-drain)
                     (lambda (_fsm) (setq drained t))))
            (niwa--fail-turn fsm 'error))
          (with-current-buffer chat-buf
            (should-not niwa--current-request))
          (should (= 1 cancellations))
          (should (= 1 records))
          (should (= 1 saves))
          (should (= 1 hooks))
          (should (eq 'aborted current-outcome))
          (should-not unrelated-outcome)
          (should (equal registry
                         (niwa-session-agent-registry session)))
          (should (eq 'idle
                      (niwa-session-agent-root-activity session)))
          (let ((queue (niwa-session-permission-queue session)))
            (should (= 1 (length queue)))
            (should (equal "request-unrelated"
                           (plist-get (car queue) :request-id))))
          (should-not drained))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--turn-fail-pending-input ()
  ,test
  (test)

  :doc "error marks only remaining steering for the failed request"
  (let* ((chat-buf (generate-new-buffer " *niwa-turn-input-failure*"))
         (view-buf (generate-new-buffer " *niwa-turn-input-view*"))
         (session (niwa-session--create))
         (request (niwa-request--create :id "failed" :session session))
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
          (setf (niwa-session-pending-steering session)
                (list failed other)
                (niwa-session-pending-follow-ups session)
                follow-ups)
          (with-current-buffer chat-buf
            (setq-local niwa--session session
                        niwa--current-request request
                        niwa--view-buffer view-buf))
          (cl-letf (((symbol-function 'niwa-view--interaction-rebuild)
                     (lambda () (setq redrawn t))))
            (niwa--turn-fail-pending-input
             (gptel-make-fsm :info (list :buffer chat-buf))))
          (let ((entries (niwa-session-pending-steering session)))
            (should (eq 'failed-turn (plist-get (car entries) :state)))
            (should (eq other (cadr entries))))
          (should (eq follow-ups
                      (niwa-session-pending-follow-ups session)))
          (should
           (niwa-session-pending-input-failure-paused session))
          (should redrawn))
      (kill-buffer chat-buf)
      (kill-buffer view-buf)))

  :doc "abort with no undelivered matching steering does not pause"
  (let* ((chat-buf (generate-new-buffer " *niwa-turn-input-clean*"))
         (session (niwa-session--create))
         (request (niwa-request--create :id "finished" :session session))
         (other
          '(:id 2 :category steering :input "other"
            :request-id "other")))
    (unwind-protect
        (progn
          (setf (niwa-session-pending-steering session) (list other))
          (with-current-buffer chat-buf
            (setq-local niwa--session session
                        niwa--current-request request))
          (niwa--turn-fail-pending-input
           (gptel-make-fsm :info (list :buffer chat-buf)))
          (should (equal (list other)
                         (niwa-session-pending-steering session)))
          (should-not
           (niwa-session-pending-input-failure-paused session)))
      (kill-buffer chat-buf))))

(niwa-deftest niwa--handler-name ()
  ,test
  (test)
  :doc "formats named, anonymous, and non-function handlers compactly"
  (should (equal "ignore" (niwa--handler-name #'ignore)))
  (should (equal "#<function>"
                 (niwa--handler-name (lambda (_fsm) nil))))
  (should (equal "42" (niwa--handler-name 42))))

(niwa-deftest niwa--safe-fsm-handler ()
  ,test
  (test)
  :doc "returns handler values and converts errors to warnings"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest warning) (push warning warnings))))
      (should (eq 'ok
                  (funcall (niwa--safe-fsm-handler
                            (lambda (_fsm) 'ok))
                           (gptel-make-fsm :info nil))))
      (should-not
       (funcall (niwa--safe-fsm-handler
                 (lambda (_fsm) (error "Handler failed")))
                (gptel-make-fsm :info nil))))
    (should (= 1 (length warnings)))))

(provide 'test-niwa-turn)
;;; test-niwa-turn.el ends here
