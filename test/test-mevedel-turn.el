;;; test-mevedel-turn.el --- Tests for mevedel-turn.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests canonical turn settlement independently of preset construction.

;;; Code:

(require 'gptel)
(require 'mevedel-hooks)
(require 'mevedel-structs)
(require 'mevedel-turn)
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
  (dolist (case '(((:error (:type "api" :message "failed")
                    :status rejected) "failed")
                  ((:error (:type "api") :status rejected) "api: rejected")
                  ((:error (:type "api")) "api")
                  ((:status rejected) "rejected")
                  (nil nil)))
    (should (equal (cadr case)
                   (mevedel--fsm-error-message
                    (gptel-make-fsm :info (car case)))))))

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
  :doc "reports Stop and StopFailure while the request is still live"
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
            (should (eq request (nth 4 stop)))
            (should (eq 'StopFailure (car failure)))
            (should (equal "aborted"
                           (plist-get (cdr (cadr failure)) :status)))
            (should (equal "backend failed"
                           (plist-get (cdr (cadr failure))
                                      :terminal-reason)))
            (should (eq request (nth 4 failure)))))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--turn-increment
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "increments the request buffer session turn count"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-turn-count*"))
         (fsm (gptel-make-fsm :info (list :buffer chat-buf))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session))
          (dotimes (_ 3)
            (mevedel--turn-increment fsm))
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
                     (lambda (saved-session saved-buffer)
                       (push (list saved-session saved-buffer) saved))))
            (mevedel--turn-autosave fsm)
            (with-current-buffer chat-buf
              (setq-local mevedel-session--read-only-mode t))
            (mevedel--turn-autosave fsm))
          (should (equal (list (list session chat-buf)) saved)))
      (kill-buffer chat-buf))))

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
                    ((symbol-function 'mevedel--turn-increment)
                     (lambda (_fsm) (push 'turn events)))
                    ((symbol-function 'mevedel--compact-record-token-baseline)
                     (lambda (_fsm) (push 'baseline events)))
                    ((symbol-function 'mevedel--turn-autosave)
                     (lambda (_fsm) (push 'save events)))
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
                      'mevedel-view--schedule-queued-user-message-drain)
                     (lambda (_fsm)
                       (push (list 'drain
                                   (with-current-buffer chat-buf
                                     (null mevedel--current-request)))
                             events))))
            (mevedel--complete-turn
             (gptel-make-fsm :info (list :buffer chat-buf)))))
          (should (equal (nreverse events)
                         '(turn baseline save
                           (Stop completed live)
                           restore request-end (drain t))))
          (with-current-buffer chat-buf
            (should-not mevedel--current-request)))
      (kill-buffer chat-buf)))

(mevedel-deftest mevedel--fail-turn ()
  ,test
  (test)
  :doc "failure statuses skip autosave and queued-message drainage"
  (let (events saved drained)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'mevedel--turn-increment)
               (lambda (_fsm) (push 'turn events)))
              ((symbol-function 'mevedel--compact-record-token-baseline)
               (lambda (_fsm) (push 'baseline events)))
              ((symbol-function 'mevedel-goal-settle-failure)
               (lambda (_fsm &optional _status)
                 (push 'goal-failure events)))
              ((symbol-function 'mevedel-goal-persist-failure)
               (lambda (_fsm) (push 'goal-save events)))
              ((symbol-function 'mevedel-goal-dispatch-after-failure)
               (lambda (_fsm) (push 'goal-retry events)))
              ((symbol-function 'mevedel--turn-autosave)
               (lambda (_fsm) (setq saved t)))
              ((symbol-function 'mevedel--run-turn-terminal-hook)
               (lambda (_fsm event status)
                 (push (list event status) events)))
              ((symbol-function 'mevedel--turn-restore-permission-mode)
               (lambda (_fsm) (push 'restore events)))
              ((symbol-function 'mevedel--turn-end-request)
               (lambda (_fsm) (push 'request-end events)))
              ((symbol-function
                'mevedel-view--schedule-queued-user-message-drain)
               (lambda (_fsm) (setq drained t))))
      (dolist (case '((error) (aborted)))
        (setq events nil)
        (mevedel--fail-turn 'fsm (car case))
        (should (equal (nreverse events)
                       `(turn baseline goal-failure
                                (StopFailure ,(car case))
                                restore request-end goal-save goal-retry))))
    (should-not saved)
    (should-not drained))))

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
