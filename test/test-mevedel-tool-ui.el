;;; test-mevedel-tool-ui.el --- Tests for mevedel-tool-ui.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-ui)
(require 'gptel)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Ask User

(mevedel-deftest mevedel-tools--ask-user
  (:doc "advances after answers and submits from the review page")
  (let ((data-buffer (generate-new-buffer " *mev-ask-data*"))
        (view-buffer (generate-new-buffer " *mev-ask-view*"))
        (choices '("Yes" "No"))
        rendered-body
        rendered-keymap
        result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--prompt--data-buffer)
                   (lambda () data-buffer))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (&optional _data-buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (descriptor)
                     (setq rendered-body (plist-get descriptor :body))
                     (setq rendered-keymap (plist-get descriptor :keymap))
                     (make-overlay (point-min) (point-min)
                                   (current-buffer) nil t)))
                  ((symbol-function 'mevedel--prompt--register-canceller)
                   #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (pop choices))))
          (with-current-buffer view-buffer
            (setq-local mevedel--prompt-overlays nil))
          (mevedel-tools--ask-user
           (lambda (value) (setq result value))
           [(:question "Use cache?" :options ["Yes" "No"])
            (:question "Run tests?" :options ["Yes" "No"])])
          (should (string-match-p "Question 1/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Question 2/2" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Review Your Answers" rendered-body))
          (call-interactively (lookup-key rendered-keymap (kbd "RET")))
          (should (string-match-p "Q1: Use cache\\?" result))
          (should (string-match-p "A1: Yes" result))
          (should (string-match-p "Q2: Run tests\\?" result))
          (should (string-match-p "A2: No" result)))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer)))))


;;
;;; Agent stop control

(mevedel-deftest mevedel-tools-stop-agent
  (:doc "stops a running background agent and resumes parent BWAIT")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-stop-parent*"))
         (agent-buf (generate-new-buffer " *mev-stop-agent*"))
         (agent (mevedel-agent--create :name "reviewer"
                                       :description "Review"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--735123142194f47363852069e3f42083"
               :description "review current diff"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :handlers nil
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv
                                 :callback
                                 (lambda (resp _info)
                                   (when (eq resp 'abort)
                                     (setf
                                      (mevedel-agent-invocation-transcript-status
                                       inv)
                                      'aborted)
                                     (mevedel-tools--complete-background-agent
                                      inv "abort callback body"))))
                     :handlers nil
                     :state 'WAIT))
         (gptel--request-alist
          (list (cons 'fake-process (cons child-fsm #'ignore))))
         aborted
         result)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-parent-fsm inv) parent-fsm)
          (setf (mevedel-session-background-agents session)
                '("reviewer--735123142194f47363852069e3f42083"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--735123142194f47363852069e3f42083"
                           . ,child-fsm))))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (&optional _buf)
                       (setq aborted t)
                       (funcall (plist-get (gptel-fsm-info child-fsm)
                                           :callback)
                                'abort (gptel-fsm-info child-fsm))))
                    ((symbol-function
                      'mevedel-agent-exec--save-transcript-buffer)
                     (lambda (_invocation) t))
                    ((symbol-function 'mevedel-agent-exec--handle-update)
                     (lambda (_invocation) nil))
                    ((symbol-function 'mevedel-agent-exec--run-stop-hook)
                     (lambda (_invocation _status) nil))
                    ((symbol-function
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (_session _agent-id _updates) nil))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (_session _buffer) t)))
            (setq result
                  (with-current-buffer parent-buf
                    (mevedel-tools-stop-agent
                     "reviewer--73512314" "stranded in BWAIT"))))
          (should (eq 'running (plist-get result :previous-status)))
          (should (eq 'aborted (plist-get result :status)))
          (should (plist-get result :resumed-bwait))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status inv)))
          (should (equal "stranded in BWAIT"
                         (mevedel-agent-invocation-terminal-reason inv)))
          (should (null (mevedel-session-background-agents session)))
          (should (= 1 (length (mevedel-session-messages session))))
          (let ((body (plist-get (car (mevedel-session-messages session))
                                 :body)))
            (should (string-match-p "<agent-result agent-id=\"reviewer--735123142194f47363852069e3f42083\"" body))
            (should (string-match-p "was stopped" body))
            (should (string-match-p "stranded in BWAIT" body)))
          (with-current-buffer parent-buf
            (should-not (assoc "reviewer--735123142194f47363852069e3f42083"
                               mevedel-tools--agents-fsm)))
          (should (eq 'WAIT (gptel-fsm-state parent-fsm)))
          (should aborted))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools--resolve-agent-stop-target
  (:doc "accepts exact ids and rejects ambiguous displayed short ids")
  (let* ((parent-buf (generate-new-buffer " *mev-stop-resolve-parent*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv-a (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa111111111111111111111111"
                 :transcript-status 'running))
         (inv-b (mevedel-agent-invocation--create
                 :agent agent
                 :agent-id "reviewer--aaaaaaaa222222222222222222222222"
                 :transcript-status 'running))
         (fsm-a (gptel-make-fsm
                 :info (list :mevedel-agent-invocation inv-a)
                 :state 'WAIT))
         (fsm-b (gptel-make-fsm
                 :info (list :mevedel-agent-invocation inv-b)
                 :state 'WAIT)))
    (unwind-protect
        (progn
          (with-current-buffer parent-buf
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--aaaaaaaa111111111111111111111111"
                           . ,fsm-a)
                          ("reviewer--aaaaaaaa222222222222222222222222"
                           . ,fsm-b))))
          (should (equal "reviewer--aaaaaaaa111111111111111111111111"
                         (car (mevedel-tools--resolve-agent-stop-target
                               "reviewer--aaaaaaaa111111111111111111111111"
                               parent-buf))))
          (should-error
           (mevedel-tools--resolve-agent-stop-target
            "reviewer--aaaaaaaa" parent-buf)))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

(mevedel-deftest mevedel-tools--bwait-watchdog-expire
  (:doc "running-agent warning advertises StopAgent recovery")
  (let* ((session (mevedel-session--create :name "main"))
         (parent-buf (generate-new-buffer " *mev-watchdog-parent*"))
         (agent-buf (generate-new-buffer " *mev-watchdog-agent*"))
         (agent (mevedel-agent--create :name "reviewer"))
         (inv (mevedel-agent-invocation--create
               :agent agent
               :agent-id "reviewer--WATCHDOG"
               :parent-context session
               :parent-data-buffer parent-buf
               :buffer agent-buf
               :background-p t
               :transcript-status 'running))
         (parent-fsm (gptel-make-fsm
                      :info (list :buffer parent-buf)
                      :state 'BWAIT))
         (child-fsm (gptel-make-fsm
                     :info (list :buffer agent-buf
                                 :mevedel-agent-invocation inv)
                     :state 'WAIT))
         logged)
    (unwind-protect
        (progn
          (setf (mevedel-session-background-agents session)
                '("reviewer--WATCHDOG"))
          (with-current-buffer parent-buf
            (setq-local mevedel--session session)
            (setq-local mevedel-tools--agents-fsm
                        `(("reviewer--WATCHDOG" . ,child-fsm))))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq logged (apply #'format fmt args))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args) nil)))
            (let ((mevedel-agent-background-timeout 600))
              (mevedel-tools--bwait-watchdog-expire parent-fsm)))
          (should (string-match-p "StopAgent" logged))
          (should (string-match-p "mevedel-stop-agent" logged)))
      (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
      (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))


(provide 'test-mevedel-tool-ui)
;;; test-mevedel-tool-ui.el ends here
