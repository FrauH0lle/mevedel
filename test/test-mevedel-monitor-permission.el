;;; test-mevedel-monitor-permission.el --- Monitor authority -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the registered Monitor through the pipeline permission boundary.
;; Commands never execute and automatic review uses an offline provider reply.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-permission-review)
(require 'mevedel-permissions)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-tool-exec-registration)
(require 'mevedel-tool-permission)

(mevedel-deftest mevedel-permission-policy-tool-name ()
  (should (equal ,expected (mevedel-permission-policy-tool-name ,name)))
  (name expected)
  :doc "Monitor uses Bash policy identity"
  "Monitor" "Bash"
  :doc "Bash policy identity is unchanged"
  "Bash" "Bash"
  :doc "Eval policy identity is unchanged"
  "Eval" "Eval"
  :doc "Unrelated native tool identity is unchanged"
  "Read" "Read")

(mevedel-deftest mevedel-tool-permission-step/monitor
  (:before-each (mevedel-tool-exec--register)
   :quiet t)
  (let* ((root (make-temp-file "mevedel-monitor-permission-" t))
         (session (mevedel-session--create
                   :name "monitor-permission" :permission-mode 'ask
                   :sandbox-mode 'off :authority-mode 'pid-lock
                   :working-directory root))
         (request (mevedel-request--create
                   :id "monitor-request" :session session))
         (invocation (mevedel-agent-invocation--create
                      :path "/root/watcher" :parent-session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-hook-rules nil)
         (mevedel-permission-reviewer 'user)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      default-directory (file-name-as-directory root))
          ;; The owning request must not be replaced by the newer ambient turn.
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session :permission-mode-override 'full-auto))
          (let* ((args '(:command "make test" :description "Watch test results"))
                 (context (list :tool (mevedel-tool-ensure "Monitor")
                                :args args :session session :request request
                                :invocation invocation :buffer (current-buffer)
                                :tool-use-id "monitor/1" :call-source 'ptc))
                 allowed denied queued)
            ,test))
      (mevedel-permission-queue-abort-all session)
      (delete-directory root t)))
  (test)

  :doc "Monitor: Bash deny is absolute in Full Access and logs no command arguments"
  (progn
  (setf (mevedel-session-permission-mode session) 'full-auto
        (mevedel-session-permission-rules session)
        '(("Bash" :action deny) ("Monitor" :action allow)))
  (setq args '(:command "printf monitor-secret-token" :description "Watch output")
        context (plist-put context :args args))
  (cl-letf (((symbol-function 'mevedel-permission--enqueue)
             (lambda (&rest _) (setq queued t))))
    (mevedel-tool-permission-step
     context (lambda (_) (setq allowed t))
     (lambda (&rest _) (setq denied t))))
  (should denied)
  (should-not allowed)
  (should-not queued)
  (let ((entry (car (last (mevedel-session-permission-log-pending session)))))
    (should (equal "Monitor" (plist-get entry :tool-name)))
    (should (eq 'deny (plist-get entry :outcome)))
    (should (equal "printf" (plist-get entry :specifier-value)))
    (should-not (string-match-p "monitor-secret-token" (prin1-to-string entry)))))

  :doc "Monitor: request and invocation buckets reach the real Bash checker"
  (progn
  (setf (mevedel-session-permission-rules session) '(("Bash" :action ask))
        (mevedel-request-skill-permission-rules request)
        '(("Bash" :pattern "make test" :action allow)))
  (cl-letf (((symbol-function 'mevedel-permission--enqueue)
             (lambda (&rest _) (setq queued t))))
    (mevedel-tool-permission-step
     context (lambda (_) (setq allowed t))
     (lambda (&rest _) (setq denied t)))
    (should allowed)
    (should-not denied)
    (should-not queued)
    ;; Invocation rules are nearer than request rules, not the ambient turn.
    (setf (mevedel-request-skill-permission-rules request)
          '(("Bash" :action ask))
          (mevedel-agent-invocation-skill-permission-rules invocation)
          '(("Bash" :pattern "make test" :action allow)))
    (setq allowed nil)
    (mevedel-tool-permission-step
     context (lambda (_) (setq allowed t))
     (lambda (&rest _) (setq denied t)))
    (should allowed)
    (should-not denied)
    (should-not queued)))

  :doc "Monitor: checker and one-shot card retain captured request and invocation"
  (progn
  (setf (mevedel-request-one-shot-mutations-p request) t
        (mevedel-request-skill-permission-rules request)
        '(("Bash" :pattern "make test" :action allow)))
  (let ((checker (symbol-function 'mevedel-tool-exec-permission-check-bash-async))
        checker-context
        (admissions 0))
    (cl-letf (((symbol-function 'mevedel-tool-exec-permission-check-bash-async)
               (lambda (tool input callback)
                 (setq checker-context (plist-get input :permission-context))
                 (funcall checker tool input callback)))
              ((symbol-function 'mevedel-hooks-run-event)
               (lambda (event _payload callback &rest _)
                 (when (eq event 'PermissionRequest) (cl-incf admissions))
                 (funcall callback nil)))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session) (setq queued entry))))
      (mevedel-tool-permission-step
       context (lambda (_) (setq allowed t))
       (lambda (&rest _) (setq denied t)))
      (should (eq request (plist-get checker-context :request)))
      (should (eq invocation (plist-get checker-context :invocation)))
      (should (eq session (plist-get checker-context :session)))
      (should (eq 'ask (plist-get checker-context :mode)))
      (should (plist-get checker-context :one-shot-mutations-p))
      (should (functionp (plist-get checker-context :permission-request)))
      (should (= 1 admissions))
      (should-not allowed)
      (should-not denied)
      (should (equal "Monitor" (plist-get queued :tool-name)))
      (should (eq request (plist-get queued :request)))
      (should (eq invocation (plist-get queued :invocation)))
      (should (equal "monitor-request" (plist-get queued :request-id)))
      (should (equal "monitor/1" (plist-get queued :tool-use-id)))
      (should (plist-get queued :once-only))
      (should-not (plist-get queued :include-always))
      (should-not (plist-get queued :reusable-operation-p))
      (funcall (plist-get queued :callback) 'allow-session)
      (should allowed)
      (should-not denied)
      (should-not (mevedel-session-permission-rules session))
      (should-not (mevedel-session-resource-grants session)))))

  :doc "Monitor: PermissionRequest hook denial blocks queue and automatic reviewer"
  (progn
  (setf (mevedel-request-skill-permission-rules request) '(("Bash" :action ask)))
  (let ((mevedel-permission-reviewer 'auto)
        (admissions 0) reviewed hook-tool)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (event payload callback &rest _)
                 (if (eq event 'PermissionRequest)
                     (progn
                       (cl-incf admissions)
                       (setq hook-tool (plist-get payload :tool-name))
                       (funcall callback '(:permission-decision deny
                                          :permission-reason "Watcher not authorized")))
                   (funcall callback nil))))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq queued t)))
              ((symbol-function 'mevedel-permission-review--model)
               (lambda (&rest _) (setq reviewed t))))
      (mevedel-tool-permission-step
       context (lambda (_) (setq allowed t))
       (lambda (&rest _) (setq denied t))))
    (should (= 1 admissions))
    (should (equal "Monitor" hook-tool))
    (should denied)
    (should-not allowed)
    (should-not queued)
    (should-not reviewed)))

  :doc "Monitor: generic hook card rechecks live Bash denies before late approval"
  (progn
  (setf (mevedel-request-skill-permission-rules request)
        '(("Bash" :pattern "make test" :action allow)))
  (setq context (plist-put context :hook-permission-decision 'ask))
  (cl-letf (((symbol-function 'mevedel-permission--enqueue)
             (lambda (entry &optional owner)
               (setq queued (plist-put entry :session owner)))))
    (mevedel-tool-permission-step
     context (lambda (_) (setq allowed t))
     (lambda (&rest _) (setq denied t)))
    (should queued)
    (should (eq 'generic (plist-get queued :kind)))
    (should (equal "Monitor" (plist-get queued :tool-name)))
    (should (eq 'ask (mevedel-permission-queue--reevaluate queued)))
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "make test" :action deny)))
    (should (eq 'deny (mevedel-permission-queue--reevaluate queued)))
    (funcall (plist-get queued :callback) 'allow-once)
    (should denied)
    (should-not allowed)))

  :doc "Monitor: queued human card retains the actual tool name in its title"
  (progn
  (setf (mevedel-request-skill-permission-rules request) '(("Bash" :action ask)))
  (let (body settle)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-card)
               (lambda (build _include callback &rest _)
                 (setq body (funcall build nil)
                       settle callback))))
      (mevedel-tool-permission-step
       context (lambda (_) (setq allowed t))
       (lambda (&rest _) (setq denied t)))
      (should (string-match-p "Monitor Command Execution Request" body))
      (should-not (string-match-p "Bash Command Execution Request" body))
      (should-not allowed)
      (should-not denied)
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (funcall settle 'deny-once)
      (should denied)
      (should-not allowed)
      (should-not (mevedel-session-permission-queue session)))))

  :doc "Monitor: automatic review sees execution evidence and approves only this call"
  (progn
  (org-mode)
  (insert "Watch the requested test command.\n")
  (setf (mevedel-session-root-buffer session) (current-buffer)
        (mevedel-session-permission-rules session)
        '(("Bash" :pattern "make test" :action allow :network t))
        (mevedel-request-skill-permission-rules request) '(("Bash" :action ask)))
  (let ((mevedel-permission-reviewer 'auto)
        (reviews 0) evidence accepted)
    (cl-letf (((symbol-function 'mevedel-permission-review--model)
               (lambda (facts callback &optional _stream)
                 (cl-incf reviews)
                 (setq evidence facts)
                 (funcall callback
                          "{\"decision\":\"allow-once\",\"reason\":\"Requested watcher\"}")
                 #'ignore))
              ((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (&rest _) (setq queued t))))
      (mevedel-tool-permission-step
       context (lambda (updated) (setq allowed t accepted updated))
       (lambda (&rest _) (setq denied t))))
    (should (= 1 reviews))
    (should allowed)
    (should-not denied)
    (should-not queued)
    (let ((operation (plist-get evidence :operation)))
      (should (equal "Monitor" (plist-get operation :tool-name)))
      (should (equal args (plist-get operation :args)))
      (should (equal "make test" (plist-get operation :command))))
    (should (eq 'ask (plist-get evidence :permission-mode)))
    (should (eq 'off (plist-get (plist-get evidence :confinement) :sandbox)))
    (let ((candidates (plist-get (plist-get evidence :saved-rule-candidates)
                                :candidates)))
      (should (= 1 (length candidates)))
      (should (eq :session (plist-get (car candidates) :bucket)))
      (should (equal '("Bash" :pattern "make test" :action allow :network t)
                     (plist-get (car candidates) :rule)))
      (should (eq 'matches-operation (plist-get (car candidates) :basis)))
      (should (equal '(:network t) (plist-get (car candidates) :profile))))
    (should (equal '((:source reviewer :outcome allow-once
                             :reason "Requested watcher"))
                   (plist-get accepted :permission-approvals)))
    (should-not (mevedel-session-permission-queue session))
    (should (equal '(("Bash" :pattern "make test" :action allow :network t))
                   (mevedel-session-permission-rules session)))
    (should-not (mevedel-session-resource-grants session))
    (should-not mevedel-permission-review--pending)))

  :doc "Monitor: ordinary Plan request denies mutations and allows read-only Bash"
  (progn
    (setq-local mevedel--current-request request)
    (setf (mevedel-request-plan-read-only request) t
          (mevedel-request-permission-mode-override request) 'full-auto)
    (setq context
          (plist-put context :args
                     '(:command "make test" :description "Watch tests"
                       :sandbox_permissions "require_escalated"
                       :justification "Run the build outside confinement")))
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq queued t))))
      (dolist (name '("Bash" "Monitor"))
        (ert-info ((format "Plan authority for %s" name))
          (setq allowed nil denied nil queued nil
                context (plist-put context :tool (mevedel-tool-ensure name)))
          (mevedel-tool-permission-step
           context (lambda (_) (setq allowed t))
           (lambda (&rest _) (setq denied t)))
          (should denied)
          (should-not allowed)
          (should-not queued)))
      (setq context (plist-put context :args
                               '(:command "pwd" :description "Watch location")))
      (dolist (name '("Bash" "Monitor"))
        (ert-info ((format "Plan read-only authority for %s" name))
          (setq allowed nil denied nil queued nil
                context (plist-put context :tool (mevedel-tool-ensure name)))
          (mevedel-tool-permission-step
           context (lambda (_) (setq allowed t))
           (lambda (&rest _) (setq denied t)))
          (should allowed)
          (should-not denied)
          (should-not queued)))))

  :doc "Monitor: unrelated ambient Plan stamps cannot restrict the owning request"
  (progn
    (setf (mevedel-request-permission-mode-override request) 'full-auto)
    (setq-local mevedel--agent-invocation
                (mevedel-agent-invocation--create
                 :path "/root/unrelated" :parent-session session))
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq queued t))))
      (dolist (stamp '(request invocation))
        (setf (mevedel-request-plan-read-only mevedel--current-request)
              (eq stamp 'request)
              (mevedel-agent-invocation-plan-read-only mevedel--agent-invocation)
              (eq stamp 'invocation))
        (dolist (name '("Bash" "Monitor"))
          (ert-info ((format "Unrelated %s Plan authority for %s" stamp name))
            (setq allowed nil denied nil queued nil
                  context (plist-put context :tool (mevedel-tool-ensure name)))
            (mevedel-tool-permission-step
             context (lambda (_) (setq allowed t))
             (lambda (reason &rest _) (setq denied reason)))
            (ert-info ((format "Denial: %S" denied))
              (should allowed))
            (should-not denied)
            (should-not queued))))))

  :doc "Monitor: captured request or invocation Plan stamps survive ambient drift"
  (let (outcomes)
    (setf (mevedel-request-permission-mode-override request) 'full-auto)
    (setq context
          (plist-put context :args
                     '(:command "make test" :description "Watch tests"
                       :sandbox_permissions "require_escalated"
                       :justification "Run the build outside confinement")))
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq queued t))))
      (dolist (stamp '(request invocation))
        (setf (mevedel-request-plan-read-only request) (eq stamp 'request)
              (mevedel-agent-invocation-plan-read-only invocation)
              (eq stamp 'invocation))
        (dolist (name '("Bash" "Monitor"))
          (setq allowed nil denied nil queued nil
                context (plist-put context :tool (mevedel-tool-ensure name)))
          (mevedel-tool-permission-step
           context (lambda (_) (setq allowed t))
           (lambda (&rest _) (setq denied t)))
          (push (list stamp name :denied denied :allowed allowed :queued queued)
                outcomes))))
    (should (equal '((request "Bash" :denied t :allowed nil :queued nil)
                     (request "Monitor" :denied t :allowed nil :queued nil)
                     (invocation "Bash" :denied t :allowed nil :queued nil)
                     (invocation "Monitor" :denied t :allowed nil :queued nil))
                   (nreverse outcomes)))))

(provide 'test-mevedel-monitor-permission)
;;; test-mevedel-monitor-permission.el ends here
