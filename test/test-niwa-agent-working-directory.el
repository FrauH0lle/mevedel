;;; test-niwa-agent-working-directory.el --- Agent cwd integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the registered Agent provider tool through spawn and conversation
;; setup.  Only the final provider request is replaced; paths and state are real.

;;; Code:

(require 'gptel)
(require 'gptel-openai-responses)
(require 'json)
(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-agent-runtime)
(require 'niwa-agents)
(require 'niwa-claude-code-context)
(require 'niwa-context-delivery)
(require 'niwa-reminders)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-system)
(require 'niwa-tool-render-data)
(require 'niwa-tool-ui)
(require 'niwa-tools)
(require 'niwa-workspace)
(require 'niwa-tool-exec)
(require 'niwa-execution)
(require 'niwa-execution-telemetry)
(require 'niwa-turn)
(require 'niwa-sandbox)
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-agent-control-spawn/working-directory
  (:before-each (niwa-tools-register)
   :after-each (niwa-workspace-clear-registry)
   :quiet t)
  ,test
  (test)
  :doc "registered Agent dispatch isolates canonical cwd and inherits it through grandchildren"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-agent-cwd-dispatch-" t)))
         (session-root (file-name-as-directory (file-name-concat root "session")))
         (child-root (file-name-as-directory (file-name-concat session-root "child")))
         (nested-root (file-name-as-directory (file-name-concat child-root "nested")))
         (alias (file-name-concat session-root "alias"))
         (linked-root (file-name-as-directory (make-temp-file "niwa-agent-linked-cwd-" t)))
         (linked-subdir (file-name-as-directory (file-name-concat linked-root "src")))
         (workspace (niwa-workspace-get-or-create 'project root root "cwd"))
         (session (niwa-session-create "main" workspace session-root))
         (parent (generate-new-buffer " *agent-cwd-dispatch-parent*"))
         (provider (niwa-tool-gptel-tool (niwa-tool-get "Agent")))
         (rules '(("Read" :path "/not-granted/**" :action deny)))
         (grants '((:path "/already-granted.txt" :access read)))
         launches)
    (unwind-protect
        (progn
          (make-directory nested-root t)
          (make-symbolic-link child-root alias)
          (with-temp-file (file-name-concat root "AGENTS.md")
            (insert "Root checkout rules: ROOT-ONLY-CWD-RULE.\n"))
          (dolist (args '(("init" "--initial-branch=main")
                          ("add" "AGENTS.md")
                          ("-c" "user.name=Cwd Test" "-c" "user.email=cwd@example.test"
                           "commit" "-m" "Initial cwd fixture")))
            (should (zerop (apply #'call-process "git" nil nil nil "-C" root args))))
          (should (zerop (call-process "git" nil nil nil "-C" root
                                      "worktree" "add" "-b" "cwd-linked" linked-root)))
          (make-directory linked-subdir)
          (with-temp-file (file-name-concat linked-root "AGENTS.md")
            (insert "Linked checkout rules: LINKED-ONLY-CWD-RULE.\n"))
          (setf (niwa-session-permission-mode session) 'full-auto
                (niwa-session-permission-rules session) rules
                (niwa-session-resource-grants session) grants)
          (with-current-buffer parent
            (setq-local niwa--session session niwa--workspace workspace
                        default-directory root gptel-tools (list provider)))
          (cl-letf (((symbol-function 'niwa-agent-exec-run)
                     (lambda (callback _role _description invocation buffer &optional _configure)
                       (push (list :callback callback :invocation invocation :buffer buffer) launches)
                       'provider-request)))
            (cl-labels
                ((launch (buffer task expected-cwd expected-directory &optional directory)
                   (let* ((args (list :task_name task :message "Inspect the local files."
                                      :role "" :context "" :model "" :effort ""
                                      :working_directory (or directory "")))
                          (before (length launches)) result)
                     (with-current-buffer buffer
                       (apply (gptel-tool-function provider)
                              (lambda (value &rest _) (setq result value))
                              (mapcar (lambda (arg)
                                        (plist-get args (intern (concat ":" (plist-get arg :name)))))
                                      (gptel-tool-args provider))))
                     (should (= (1+ before) (length launches)))
                     (let* ((path (gethash "path" (json-parse-string
                                                   (niwa-tool-render-data-strip result))))
                            (record (cdr (assoc path (niwa-session-agent-registry session))))
                            (invocation (plist-get (car launches) :invocation))
                            (child (plist-get (car launches) :buffer)))
                       (should (equal expected-cwd (niwa-agent-record-working-directory record)))
                       (should (equal expected-cwd (niwa-agent-invocation-working-directory invocation)))
                       (should (equal expected-cwd
                                      (buffer-local-value 'niwa--conversation-working-directory child)))
                       (should (equal expected-directory (buffer-local-value 'default-directory child)))
                       (should (equal expected-directory (niwa-conversation-working-directory session child)))
                       (should (eq session (buffer-local-value 'niwa--session child)))
                       (should (string-search
                                "Inspect the local files."
                                (decode-coding-string
                                 (niwa-session-artifacts-read-artifact
                                  session (niwa-agent-record-conversation-location record))
                                 'utf-8-unix)))
                       (should (equal session-root (niwa-session-working-directory session)))
                       (should (eq rules (niwa-session-permission-rules session)))
                       (should (eq grants (niwa-session-resource-grants session)))
                       child))))
              (let* ((child (launch parent "child" child-root child-root "child"))
                     (sibling (launch parent "sibling" nil session-root))
                     (inherited (launch child "inherited" child-root child-root))
                     (nested (launch child "nested" nested-root nested-root "nested")))
                (launch nested "deep" nested-root nested-root)
                ;; Canonicalize a real symlink as well as dot segments.
                (launch parent "absolute" child-root child-root
                        (file-name-concat alias "nested" ".."))
                (should (equal root (buffer-local-value 'default-directory parent)))
                (should-not (buffer-local-value 'niwa--conversation-working-directory parent))
                (should (equal session-root (buffer-local-value 'default-directory sibling)))
                (should (equal child-root (buffer-local-value 'default-directory child)))
                (should (equal "/root/child/inherited"
                               (niwa-agent-invocation-path
                                (buffer-local-value 'niwa--agent-invocation inherited))))
                (let ((linked (launch parent "linked" linked-subdir linked-subdir linked-subdir)))
                  (with-current-buffer linked
                    (should (equal linked-root niwa--conversation-instruction-root))
                    (should (eq workspace niwa--workspace))
                    (let* ((context (niwa-system--make-context
                                     workspace linked-subdir session linked))
                           (native (niwa-claude-code-context-system
                                    niwa--agent-invocation "Agent instructions."))
                           (native-content
                            (mapconcat (lambda (part) (or (alist-get 'text part) ""))
                                       (niwa-claude-code-context-prompt
                                        niwa--agent-invocation
                                        [((type . "text") (text . "Inspect cwd."))]) "\n"))
                           (backend (let ((gptel--known-backends nil))
                                      (gptel-make-openai-responses "cwd-context" :key "test"
                                                                   :models '(cwd-test))))
                           (data (list :input [(:role "user" :content "Inspect cwd.")]))
                           (fsm (gptel-make-fsm
                                 :info (list :buffer linked :data data
                                             :backend backend :model 'cwd-test
                                             :position (point-marker)))))
                      (should (eq session (niwa-system--context-session context)))
                      (niwa-context-delivery-stage fsm)
                      (should (seq-find
                               (lambda (entry) (eq 'context-environment (plist-get entry :type)))
                               (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
                      (niwa-reminders--handle-inject fsm)
                      (should (> (length (plist-get data :input)) 1))
                      (let ((delivered (mapconcat
                                        (lambda (message) (plist-get message :content))
                                        (append (plist-get data :input) nil) "\n")))
                        (should (equal "Agent instructions." native))
                        (dolist (text (list native-content delivered))
                          (should (string-search (concat "Working directory: " linked-subdir) text))
                          (should (string-search "LINKED-ONLY-CWD-RULE" text))
                          (should-not (string-search "ROOT-ONLY-CWD-RULE" text))
                          ;; Cwd selection must not detach session resources.
                          (should (string-search "`work://`" text)))))))
                ;; Validation failure must not reserve an identity or dispatch.
                (let ((before (length launches)) result)
                  (with-current-buffer child
                    (apply (gptel-tool-function provider)
                           (lambda (value &rest _) (setq result value))
                           (mapcar
                            (lambda (arg)
                              (plist-get '(:task_name "invalid" :message "Must not launch."
                                           :working_directory "missing")
                                         (intern (concat ":" (plist-get arg :name)))))
                            (gptel-tool-args provider))))
                  (should (string-prefix-p "Error:" result))
                  (should (string-search "does not exist" result))
                  (should (= before (length launches)))
                  (should-not (assoc "/root/child/invalid" (niwa-session-agent-registry session))))))
            (dolist (launch launches)
              (funcall (plist-get launch :callback) "Directory inspected."))
            (let* ((record (cdr (assoc "/root/child" (niwa-session-agent-registry session))))
                   (child (niwa-agent-record-conversation-buffer record))
                   (sibling (niwa-agent-record-conversation-buffer
                             (cdr (assoc "/root/sibling" (niwa-session-agent-registry session)))))
                   (send (niwa-tool-gptel-tool (niwa-tool-get "SendMessage")))
                   (before (length launches))
                   result)
              (with-current-buffer parent
                (apply (gptel-tool-function send)
                       (lambda (value &rest _) (setq result value))
                       (mapcar
                        (lambda (arg)
                          (plist-get '(:target "/root/child" :message "Inspect the retained cwd again."
                                       :working_directory "")
                                     (intern (concat ":" (plist-get arg :name)))))
                        (gptel-tool-args send))))
              (should result)
              (should (= (1+ before) (length launches)))
              (let ((invocation (plist-get (car launches) :invocation)))
                (should (equal child-root (niwa-agent-invocation-working-directory invocation)))
                (should (eq child (niwa-agent-invocation-buffer invocation))))
              (should (equal child-root (buffer-local-value 'default-directory child)))
              (should (equal child-root (niwa-conversation-working-directory session child)))
              (should (equal root (buffer-local-value 'default-directory parent)))
              (should (equal session-root (buffer-local-value 'default-directory sibling)))
              (should-not (buffer-local-value 'niwa--conversation-working-directory parent))
              (should-not (buffer-local-value 'niwa--conversation-working-directory sibling))
              (should (equal session-root (niwa-session-working-directory session)))
              (with-current-buffer child
                (apply (gptel-tool-function send)
                       (lambda (value &rest _) (setq result value))
                       (mapcar
                        (lambda (arg)
                          (plist-get '(:target "" :message "Mail to the parent."
                                       :working_directory "")
                                     (intern (concat ":" (plist-get arg :name)))))
                        (gptel-tool-args send))))
              (should (equal "Queued for /root."
                             (niwa-tool-render-data-strip result)))
              (funcall (plist-get (car launches) :callback) "Retained directory inspected."))))
      (dolist (launch launches)
        (let ((buffer (plist-get launch :buffer)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (when (niwa-session-save-path session)
        (niwa-session-persistence-lock-release (niwa-session-save-path session)))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory linked-root t)
      (delete-directory root t))))

(niwa-deftest niwa-agent-control-send-message/sandbox-lifetime
  (:before-each (niwa-tools-register)
   :after-each (niwa-workspace-clear-registry)
   :quiet t)
  ,test
  (test)
  :doc "turn finalization preserves idle Eval, kills active Eval, and moved turns restart"
  (let* ((probe (niwa-sandbox-probe))
         (_ (unless (plist-get probe :available) (ert-skip (plist-get probe :reason))))
         (root (file-name-as-directory (make-temp-file "agent-sandbox-lifetime-" t)))
         (moved (file-name-as-directory (file-name-concat root "moved")))
         (workspace (niwa-workspace-get-or-create 'project root root "lifetime"))
         (session (niwa-session-create "main" workspace root))
         (parent (generate-new-buffer " *agent-sandbox-lifetime*"))
         (niwa-eval-sandbox-idle-timeout nil)
         (start-service (symbol-function 'niwa-execution-start-service))
         child terminal services)
    (unwind-protect
        (progn
          (make-directory moved)
          (setf (niwa-session-permission-mode session) 'full-auto)
          (with-current-buffer parent
            (setq-local niwa--session session niwa--workspace workspace
                        default-directory root))
          (cl-letf (((symbol-function 'niwa-execution-start-service)
                     (lambda (&rest args)
                       (push (apply start-service args) services)
                       (car services)))
                    ((symbol-function 'niwa-agent-exec-run)
                     (lambda (callback _role _description _invocation buffer &optional _configure)
                       (setq terminal callback child buffer)
                       (with-current-buffer child (niwa-request-begin session))
                       'provider-request)))
            (cl-labels
                ((evaluate (expression)
                   (let (reply)
                     (with-current-buffer child
                       (niwa-tool-exec--eval
                        (lambda (value) (setq reply value))
                        (list :expression expression :mode "sandbox" :yield_time_ms 10000)))
                     (test-niwa-execution--wait (lambda () reply) 15)
                     (should-not (eq 'error (plist-get reply :status)))
                     (plist-get reply :result)))
                 (finish ()
                   (funcall terminal "Done.")
                   (should (eq 'idle (niwa-agent-record-activity
                                      (cdr (assoc "/root/probe" (niwa-session-agent-registry session))))))
                   (should-not (buffer-local-value 'niwa--current-request child))
                   (should-not (niwa-execution-owner-live-p session "/root/probe"))
                   (should-not (niwa-execution-session-live-p session))
                   (should (niwa-execution-service-live-p (car services)))
                   (should (plist-get (niwa-eval-sandbox-status session "/root/probe") :features))))
              (with-current-buffer parent
                (niwa-agent-control-spawn session "probe" "Inspect." #'ignore))
              (let ((invocation (buffer-local-value 'niwa--agent-invocation child)))
                (setf (niwa-agent-invocation-parent-tool-use-id invocation) "sandbox-card")
                (with-current-buffer parent
                  (let ((start (point)))
                    (insert "(:name \"Agent\" :args (:task_name \"probe\"))\n\nStarted.\n")
                    (insert (niwa-tool-render-data-format
                             (list :kind 'collaboration-event :event 'started :path "/root/probe"
                                   :agent-id (niwa-agent-invocation-agent-id invocation)
                                   :status 'running) "sandbox-card"))
                    (put-text-property start (point) 'gptel '(tool . "sandbox-card")))))
              (should (string-search "Result:\n73" (evaluate "(setq lifetime-marker 73)")))
              (let* ((invocation (buffer-local-value 'niwa--agent-invocation child))
                     (summary (car (niwa-agent-invocation-sandbox-summary-cell invocation))))
                (should (= 1 (plist-get summary :attempt-count) (plist-get summary :started-count)))
                (should-not (niwa-execution-telemetry-sandbox-summary-class summary))
                (niwa-agent-conversation-refresh invocation)
                (let ((card (niwa-tool-render-data-for-tool parent "sandbox-card")))
                  (should card)
                  (should-not (plist-get card :sandbox-summary))))
              (finish)
              (with-current-buffer parent
                (should (eq 'started (plist-get
                                      (niwa-agent-control-send-message session "/root/probe" "Again.")
                                      :delivery))))
              (let ((text (evaluate "lifetime-marker")))
                (should (string-search "Result:\n73" text))
                (should (string-search "Sandbox reused" text)))
              (should (= 1 (length services)))
              (finish)
              (with-current-buffer parent
                (should (eq 'started (plist-get
                                      (niwa-agent-control-send-message
                                       session "/root/probe" "Move." :working-directory moved)
                                      :delivery))))
              (let ((text (evaluate "(list (boundp 'lifetime-marker) default-directory)")))
                (should (string-match "Result:\n\\([^\n]+\\)" text))
                (let ((value (read (match-string 1 text))))
                  (should-not (car value))
                  (should (equal moved (cadr value))))
                (should (string-search "Earlier definitions are gone" text)))
              (should (= 2 (length services)))
              (should-not (niwa-execution-service-live-p (cadr services)))
              (finish)
              (with-current-buffer parent
                (should (eq 'started (plist-get
                                      (niwa-agent-control-send-message session "/root/probe" "Wait.")
                                      :delivery))))
              (let (reply)
                (with-current-buffer child
                  (niwa-tool-exec--eval
                   (lambda (value) (setq reply value))
                   '(:expression "(sleep-for 30)" :mode "sandbox" :yield_time_ms 250)))
                (test-niwa-execution--wait (lambda () reply) 5)
                (let ((facts (plist-get reply :render-data)))
                  (should (eq 'running (plist-get facts :state)))
                  (should (stringp (plist-get facts :execution-id))))
                (should (niwa-execution-owner-live-p session "/root/probe"))
                (should (niwa-execution-working-p))
                ;; A normal result callback waits for owned work; force the
                ;; real finalizer while the yielded call still owns the service.
                (with-current-buffer child
                  (niwa-agent-runtime--finalize niwa--agent-invocation 'aborted))
                (test-niwa-execution--wait
                 (lambda () (not (niwa-execution-service-live-p (car services)))) 5)
                (should-not (buffer-local-value 'niwa--current-request child))
                (should-not (niwa-execution-owner-live-p session "/root/probe"))
                (should-not (niwa-execution-session-live-p session))
                (should-not (niwa-execution-working-p))))))
      (niwa-execution-teardown-session session)
      (dolist (buffer (list child parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (niwa-session-save-path session)
        (niwa-session-persistence-lock-release (niwa-session-save-path session)))
      (delete-directory root t))))

(provide 'test-niwa-agent-working-directory)
;;; test-niwa-agent-working-directory.el ends here
