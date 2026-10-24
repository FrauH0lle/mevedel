;;; test-mevedel-agent-working-directory.el --- Agent cwd integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the registered Agent provider tool through spawn and conversation
;; setup.  Only the final provider request is replaced; paths and state are real.

;;; Code:

(require 'gptel)
(require 'gptel-openai-responses)
(require 'json)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-agents)
(require 'mevedel-claude-code-context)
(require 'mevedel-context-delivery)
(require 'mevedel-reminders)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-tool-render-data)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-agent-control-spawn/working-directory
  (:before-each (mevedel-tools-register)
   :after-each (mevedel-workspace-clear-registry)
   :quiet t)
  ,test
  (test)
  :doc "registered Agent dispatch isolates canonical cwd and inherits it through grandchildren"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-agent-cwd-dispatch-" t)))
         (session-root (file-name-as-directory (file-name-concat root "session")))
         (child-root (file-name-as-directory (file-name-concat session-root "child")))
         (nested-root (file-name-as-directory (file-name-concat child-root "nested")))
         (alias (file-name-concat session-root "alias"))
         (linked-root (file-name-as-directory (make-temp-file "mevedel-agent-linked-cwd-" t)))
         (linked-subdir (file-name-as-directory (file-name-concat linked-root "src")))
         (workspace (mevedel-workspace-get-or-create 'project root root "cwd"))
         (session (mevedel-session-create "main" workspace session-root))
         (parent (generate-new-buffer " *agent-cwd-dispatch-parent*"))
         (provider (mevedel-tool-gptel-tool (mevedel-tool-get "Agent")))
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
          (setf (mevedel-session-permission-mode session) 'full-auto
                (mevedel-session-permission-rules session) rules
                (mevedel-session-resource-grants session) grants)
          (with-current-buffer parent
            (setq-local mevedel--session session mevedel--workspace workspace
                        default-directory root gptel-tools (list provider)))
          (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback _role _description invocation buffer &optional _configure)
                       (push (list :callback callback :invocation invocation :buffer buffer) launches)
                       'provider-request)))
            (cl-labels
                ((launch (buffer task expected-cwd expected-directory &optional directory)
                   (let* ((args (append (list :task_name task :message "Inspect the local files.")
                                        (and directory (list :working_directory directory))))
                          (before (length launches)) result)
                     (with-current-buffer buffer
                       (apply (gptel-tool-function provider)
                              (lambda (value &rest _) (setq result value))
                              (mapcar (lambda (arg)
                                        (plist-get args (intern (concat ":" (plist-get arg :name)))))
                                      (gptel-tool-args provider))))
                     (should (= (1+ before) (length launches)))
                     (let* ((path (gethash "path" (json-parse-string
                                                   (mevedel-tool-render-data-strip result))))
                            (record (cdr (assoc path (mevedel-session-agent-registry session))))
                            (invocation (plist-get (car launches) :invocation))
                            (child (plist-get (car launches) :buffer)))
                       (should (equal expected-cwd (mevedel-agent-record-working-directory record)))
                       (should (equal expected-cwd (mevedel-agent-invocation-working-directory invocation)))
                       (should (equal expected-cwd
                                      (buffer-local-value 'mevedel--conversation-working-directory child)))
                       (should (equal expected-directory (buffer-local-value 'default-directory child)))
                       (should (equal expected-directory (mevedel-conversation-working-directory session child)))
                       (should (eq session (buffer-local-value 'mevedel--session child)))
                       (should (string-search
                                "Inspect the local files."
                                (decode-coding-string
                                 (mevedel-session-artifacts-read-artifact
                                  session (mevedel-agent-record-conversation-location record))
                                 'utf-8-unix)))
                       (should (equal session-root (mevedel-session-working-directory session)))
                       (should (eq rules (mevedel-session-permission-rules session)))
                       (should (eq grants (mevedel-session-resource-grants session)))
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
                (should-not (buffer-local-value 'mevedel--conversation-working-directory parent))
                (should (equal session-root (buffer-local-value 'default-directory sibling)))
                (should (equal child-root (buffer-local-value 'default-directory child)))
                (should (equal "/root/child/inherited"
                               (mevedel-agent-invocation-path
                                (buffer-local-value 'mevedel--agent-invocation inherited))))
                (let ((linked (launch parent "linked" linked-subdir linked-subdir linked-subdir)))
                  (with-current-buffer linked
                    (should (equal linked-root mevedel--conversation-instruction-root))
                    (should (eq workspace mevedel--workspace))
                    (let* ((context (mevedel-system--make-context
                                     workspace linked-subdir session linked))
                           (native (mevedel-claude-code-context-system
                                    mevedel--agent-invocation "Agent instructions."))
                           (backend (let ((gptel--known-backends nil))
                                      (gptel-make-openai-responses "cwd-context" :key "test"
                                                                   :models '(cwd-test))))
                           (data (list :input [(:role "user" :content "Inspect cwd.")]))
                           (fsm (gptel-make-fsm
                                 :info (list :buffer linked :data data
                                             :backend backend :model 'cwd-test
                                             :position (point-marker)))))
                      (should (eq session (mevedel-system--context-session context)))
                      (mevedel-context-delivery-stage fsm)
                      (should (seq-find
                               (lambda (entry) (eq 'context-environment (plist-get entry :type)))
                               (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)))
                      (mevedel-reminders--handle-inject fsm)
                      (should (> (length (plist-get data :input)) 1))
                      (let ((delivered (mapconcat
                                        (lambda (message) (plist-get message :content))
                                        (append (plist-get data :input) nil) "\n")))
                        (dolist (text (list native delivered))
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
                  (should-not (assoc "/root/child/invalid" (mevedel-session-agent-registry session))))))
            (dolist (launch launches)
              (funcall (plist-get launch :callback) "Directory inspected."))
            (let* ((record (cdr (assoc "/root/child" (mevedel-session-agent-registry session))))
                   (child (mevedel-agent-record-conversation-buffer record))
                   (sibling (mevedel-agent-record-conversation-buffer
                             (cdr (assoc "/root/sibling" (mevedel-session-agent-registry session)))))
                   (send (mevedel-tool-gptel-tool (mevedel-tool-get "SendMessage")))
                   (before (length launches))
                   result)
              (with-current-buffer parent
                (apply (gptel-tool-function send)
                       (lambda (value &rest _) (setq result value))
                       (mapcar
                        (lambda (arg)
                          (plist-get '(:target "/root/child" :message "Inspect the retained cwd again.")
                                     (intern (concat ":" (plist-get arg :name)))))
                        (gptel-tool-args send))))
              (should result)
              (should (= (1+ before) (length launches)))
              (let ((invocation (plist-get (car launches) :invocation)))
                (should (equal child-root (mevedel-agent-invocation-working-directory invocation)))
                (should (eq child (mevedel-agent-invocation-buffer invocation))))
              (should (equal child-root (buffer-local-value 'default-directory child)))
              (should (equal child-root (mevedel-conversation-working-directory session child)))
              (should (equal root (buffer-local-value 'default-directory parent)))
              (should (equal session-root (buffer-local-value 'default-directory sibling)))
              (should-not (buffer-local-value 'mevedel--conversation-working-directory parent))
              (should-not (buffer-local-value 'mevedel--conversation-working-directory sibling))
              (should (equal session-root (mevedel-session-working-directory session)))
              (funcall (plist-get (car launches) :callback) "Retained directory inspected."))))
      (dolist (launch launches)
        (let ((buffer (plist-get launch :buffer)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release (mevedel-session-save-path session)))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory linked-root t)
      (delete-directory root t))))

(provide 'test-mevedel-agent-working-directory)
;;; test-mevedel-agent-working-directory.el ends here
