;;; test-mevedel-hooks.el -- Tests for hook execution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'mevedel-hooks)
(require 'mevedel-plugins)
(require 'mevedel-agent-control)
(require 'mevedel-execution-target)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-session-persistence)
(require 'mevedel-telemetry)
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'tramp)

(defvar mevedel--agent-invocation)
(defvar mevedel-hooks-test--elisp-origin nil)


;;
;;; Helpers

(defun mevedel-hooks-test--await (starter)
  "Call STARTER with a callback and wait for its async result."
  (let ((done nil)
        result
        (deadline (+ (float-time) 5)))
    (funcall starter
             (lambda (value)
               (setq result value
                     done t)))
    (while (and (not done)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless done
      (ert-fail "Timed out waiting for hook callback"))
    result))

(defun mevedel-hooks-test--workspace (root)
  "Return a fresh workspace for ROOT."
  (mevedel-workspace-get-or-create
   'project (format "hooks-test:%s" root) root "hooks-test"))

(defun mevedel-hooks-test--emacs-command (form)
  "Return a shell command running this Emacs in batch and evaluating FORM."
  (mapconcat #'shell-quote-argument
             (list (expand-file-name invocation-name invocation-directory)
                   "--quick" "--batch" "--eval" form)
             " "))

(defun mevedel-hooks-test--session (root)
  "Return a session rooted at ROOT."
  (mevedel-session-create
   "hooks-test" (mevedel-hooks-test--workspace root) root))

(defun mevedel-hooks-test--write-plugin-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun mevedel-hooks-test--clear-plugin-env ()
  "Remove plugin compatibility variables from local `process-environment'."
  (dolist (name '("PLUGIN_ROOT"
                  "CLAUDE_PLUGIN_ROOT"
                  "PLUGIN_DATA"
                  "CLAUDE_PLUGIN_DATA"
                  "MEVEDEL_PLUGIN_ROOT"
                  "MEVEDEL_PLUGIN_DATA"))
    (setenv name nil)))

(defun mevedel-hooks-test--deny-fn (_event)
  "Return a deny decision for hook cases."
  '(:permission-decision deny :permission-reason "blocked"))

(defun mevedel-hooks-test--rewrite-fn (_event)
  "Return updated input for hook cases."
  '(:updated-input (:command "echo rewritten")))

(defun mevedel-hooks-test--capture-elisp-origin (event)
  "Capture EVENT and local `default-directory' for origin tests."
  (setq mevedel-hooks-test--elisp-origin
        (list :event event :default-directory default-directory))
  nil)

(mevedel-deftest mevedel-hooks--telemetry-handler-id
  (:doc "is stable for one handler and changes with its executable identity")
  (let ((handler '(:type command :source project :command "one")))
    (should (equal (mevedel-hooks--telemetry-handler-id handler)
                   (mevedel-hooks--telemetry-handler-id handler)))
    (should-not
     (equal (mevedel-hooks--telemetry-handler-id handler)
            (mevedel-hooks--telemetry-handler-id
             '(:type command :source project :command "two"))))))

(mevedel-deftest mevedel-hooks--command-event-plist
  (:doc "converts every command-hook filesystem fact to target-native paths")
  (let* ((target
          (mevedel-execution-target-create "/ssh:hook-target:/workspace/"))
         (session
          (mevedel-session--create :name "main" :execution-target target))
         (payload
          (mevedel-hooks--command-event-plist
           '(:cwd "/ssh:hook-target:/workspace/src/"
             :workspace-root "/ssh:hook-target:/workspace/"
             :transcript-path
             "/ssh:hook-target:/workspace/.mevedel/sessions/main/"
             :tool-input
             (:file_path "/ssh:hook-target:/workspace/src/a.el"
              :paths ("/ssh:hook-target:/workspace/a.el"
                      "/ssh:hook-target:/workspace/b.el")
              :command "echo /ssh:hook-target:/workspace/a.el"))
           session)))
    (should (equal "/workspace/src/" (plist-get payload :cwd)))
    (should (equal "/workspace/" (plist-get payload :workspace-root)))
    (should
     (equal "/workspace/.mevedel/sessions/main/"
            (plist-get payload :transcript-path)))
    (should
     (equal '(:file_path "/workspace/src/a.el"
              :paths ("/workspace/a.el" "/workspace/b.el")
              :command "echo /ssh:hook-target:/workspace/a.el")
            (plist-get payload :tool-input)))))

(mevedel-deftest mevedel-hooks--command-process-environment
  (:doc "keeps client variables local while adding deliberate target plugin variables")
  (let* ((process-environment
          (cons "MEVEDEL_CLIENT_ONLY=secret"
                (copy-sequence process-environment)))
         (local-root (make-temp-file "mevedel-hook-environment-" t))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) local-root))
         (plugin-root (file-name-concat remote-root "plugin/"))
         (plugin-data
          (file-name-concat remote-root ".mevedel/plugin-data/demo/"))
         (target (mevedel-execution-target-create remote-root))
         (session
          (mevedel-session--create :name "main" :execution-target target)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (should-not
           (mevedel-hooks--command-process-environment
            '(:type command :source project-file) session))
          (should
           (eq process-environment
               (mevedel-hooks--command-process-environment
                '(:type command :source user-file) session)))
          (let ((environment
                 (mevedel-hooks--command-process-environment
                  (list :type 'command :source 'plugin
                        :plugin-root plugin-root :plugin-data plugin-data)
                  session)))
            (should-not (member "MEVEDEL_CLIENT_ONLY=secret" environment))
            (should
             (member (concat "MEVEDEL_PLUGIN_ROOT=" local-root "/plugin/")
                     environment))
            (should
             (member
              (concat "MEVEDEL_PLUGIN_DATA=" local-root
                      "/.mevedel/plugin-data/demo/")
              environment))
            (should (file-directory-p plugin-data))))
      (delete-directory local-root t))))

(mevedel-deftest mevedel-hooks--log
  ()
  ,test
  (test)

  :doc "keeps detailed side logs transient and forwards value-free audit"
  (let* ((parent (mevedel-session--create :name "parent"))
         (side (mevedel-session--create
                :name "side" :audit-session parent))
         (entry '(:event PreToolUse
                  :handler (:type command :source project
                            :command "private-command")
                  :status allowed :elapsed 0.25 :exit-status 0
                  :stdout "private-output" :reason "private-reason"))
         calls)
    (cl-letf (((symbol-function 'mevedel-telemetry-record)
               (lambda (session event &rest props)
                 (push (list session event props) calls))))
      (mevedel-hooks--log side entry))
    (should (equal (list entry) (mevedel-session-hook-log side)))
    (should-not (mevedel-session-hook-log parent))
    (pcase-let ((`((,session ,event ,props)) calls))
      (should (eq parent session))
      (should (eq 'hook-handler event))
      (should (eq 'btw (plist-get props :conversation-scope)))
      (should-not
       (string-match-p
        (regexp-opt '("private-command" "private-output" "private-reason"))
        (prin1-to-string props)))))
  :doc "defers a materialized remote append until session settlement"
  (let* ((root (make-temp-file "mevedel-hook-log-remote-" t))
         (target
          (mevedel-execution-target-create "/ssh:hook-host:/workspace/"))
         (session
          (mevedel-session--create
           :name "main" :execution-target target :save-path root))
         (entry '(:event Stop :status completed))
         (later-entry '(:event SessionEnd :status completed))
         (mevedel-hooks-persist-log t)
         calls)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function
                 'mevedel-session-publication-append-diagnostic)
                (lambda (_session path content)
                  (push (list path content) calls)
                  t)))
            (mevedel-hooks--log session entry)
            (mevedel-hooks--log session later-entry)
            (should (equal (list entry later-entry)
                           (mevedel-session-hook-log-pending session)))
            (should-not calls)
            (should-not (file-exists-p (mevedel-hooks-log-path session)))
            (mevedel-hooks-flush-log session))
          (should-not (mevedel-session-hook-log-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should (equal (mevedel-hooks-log-path session) path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (equal entry (read (current-buffer))))
              (should (equal later-entry (read (current-buffer)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-flush-log
  (:doc "retains a failed append and persists it on a later flush")
  (let* ((root (make-temp-file "mevedel-hook-log-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (mevedel-session--create :name "main"))
         (entry '(:event Stop :status completed))
         (later-entry '(:event SessionEnd :status completed))
         (mevedel-hooks-persist-log t)
         warning)
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (mevedel-session-save-path session) blocked)
          (mevedel-test--with-captured-diagnostics warning
            (mevedel-hooks--log session entry))
          (should (string-match-p "persistence failed" warning))
          (should (equal (list entry)
                         (mevedel-session-hook-log-pending session)))
          (setf (mevedel-session-save-path session) restored)
          (mevedel-hooks--log session later-entry)
          (should (equal (list entry later-entry)
                         (mevedel-session-hook-log-pending session)))
          (mevedel-hooks-flush-log session)
          (should-not (mevedel-session-hook-log-pending session))
          (with-temp-buffer
            (insert-file-contents (mevedel-hooks-log-path session))
            (should (equal entry (read (current-buffer))))
            (should (equal later-entry (read (current-buffer))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks--run-handlers
  (:doc "emits a paired handler lifecycle span with context size")
  (let* ((root (make-temp-file "mevedel-hook-handler-span-" t))
         (session (mevedel-hooks-test--session root))
         starts finishes result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-telemetry-detailed-p)
                   (lambda (_session) t))
                  ((symbol-function 'mevedel-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) starts)
                     '(:span handler)))
                  ((symbol-function 'mevedel-telemetry-finish)
                   (lambda (_span &rest props) (push props finishes)))
                  ((symbol-function 'mevedel-telemetry-record)
                   (lambda (&rest _))))
          (mevedel-hooks--run-handlers
           'UserPromptSubmit
           (list (list :type 'elisp :source 'project
                       :function (lambda (_) '(:additional-context "abc"))))
           nil session nil nil (lambda (decision) (setq result decision)))
          (should (eq 'hook-handler-lifecycle (caar starts)))
          (should (eq 'continued (plist-get (car finishes) :outcome)))
          (should (> (plist-get (car finishes) :context-chars) 0))
          (should (equal '("abc")
                         (plist-get result :additional-context))))
      (delete-directory root t))))

(defvar mevedel-hooks-test--read-eval-ran nil)
(defvar mevedel-hooks-test--native-seen-event nil)
(defvar mevedel-hooks-test--seen-event nil)
(defvar mevedel-hooks-test--seen-buffer nil)
(defvar mevedel-hooks-test--execution-order nil)

(defun mevedel-hooks-test--capture-fn (event)
  "Capture EVENT for serial hook cases."
  (setq mevedel-hooks-test--seen-event event)
  nil)

(defun mevedel-hooks-test--native-rewrite-fn (event)
  "Record native execution and rewrite input for unified-engine cases."
  (setq mevedel-hooks-test--native-seen-event event)
  (setq mevedel-hooks-test--execution-order
        (append mevedel-hooks-test--execution-order '(native)))
  '(:updated-input (:command "native rewrite")))

(defun mevedel-hooks-test--declarative-capture-fn (event)
  "Record declarative execution and capture EVENT for unified-engine cases."
  (setq mevedel-hooks-test--execution-order
        (append mevedel-hooks-test--execution-order '(declarative)))
  (setq mevedel-hooks-test--seen-event event)
  nil)

(defun mevedel-hooks-test--suppress-output-fn (_event)
  "Return a reserved decision field for native hook cases."
  '(:suppress-output t))

(defun mevedel-hooks-test--stop-fn (_event)
  "Return a stop decision for terminal-behavior cases."
  '(:continue nil :stop-reason "stop"))

(defun mevedel-hooks-test--malformed-fn (_event)
  "Return a malformed decision for hook boundary cases."
  :args)

(defun mevedel-hooks-test--malformed-file-symbol-fn (_event)
  "Return a malformed non-keyword symbol decision."
  (intern "test-mevedel-view.el."))

(defun mevedel-hooks-test--context-fn (_event)
  "Return additional context for terminal-behavior cases."
  '(:additional-context ("later")))

(defun mevedel-hooks-test--first-context-fn (_event)
  "Return the first attributed context contribution."
  '(:system-message "first reason"
    :additional-context ("first")))

(defun mevedel-hooks-test--second-context-fn (_event)
  "Return the second attributed context contribution."
  '(:additional-context ("second-a" "second-b")))

(defun mevedel-hooks-test--allow-fn (_event)
  "Return an allow decision for permission hook cases."
  '(:permission-decision allow))

(defun mevedel-hooks-test--system-message-fn (_event)
  "Return a system-message decision for reminder cases."
  '(:system-message "remember the project rule"))

(defun mevedel-hooks-test--buffer-fn (_event)
  "Capture the current buffer for async continuation cases."
  (setq mevedel-hooks-test--seen-buffer (current-buffer))
  nil)


;;
;;; Config and matching

(mevedel-deftest mevedel-hooks-normalize-rules
  (:doc "normalizes Lisp hook rules and drops invalid handlers")
  (should
   (equal
    (mevedel-hooks-normalize-rules
     '((PreToolUse
        ((:matcher "Bash"
          :hooks ((:type command :command "echo ok")
                  (:type nope :command "ignored")
                  (:type elisp :function mevedel-hooks-test--deny-fn)))))
       (NoSuchEvent
        ((:matcher "*" :hooks ((:type command :command "ignored")))))))
    '((PreToolUse
       (:matcher "Bash"
        :hooks ((:type command :command "echo ok")
                (:type elisp :function mevedel-hooks-test--deny-fn))))))))

(mevedel-deftest mevedel-hooks-normalize-rules/malformed
  (:doc "drops malformed top-level rule values")
  (should-not (mevedel-hooks-normalize-rules
               (intern "test-mevedel-view.el."))))

(mevedel-deftest mevedel-hooks-normalize-rules/scoped-stop
  (:doc "normalizes agent-scoped Stop to SubagentStop")
  (should
   (equal
    (mevedel-hooks-normalize-rules
     '((Stop
        ((:matcher "*"
          :hooks ((:type elisp
                   :function mevedel-hooks-test--context-fn))))))
     'agent)
    '((SubagentStop
       (:matcher "*"
        :hooks ((:type elisp
                 :function mevedel-hooks-test--context-fn))))))))

(mevedel-deftest mevedel-hooks--read-json-file
		 (:doc "reads Claude/Codex-style JSON hook config")
		 (let ((file (make-temp-file "mevedel-hooks" nil ".json")))
		   (unwind-protect
		       (progn
			 (with-temp-file file
			   (insert "{\n"
				   "  \"hooks\": {\n"
				   "    \"PreToolUse\": [{\n"
				   "      \"matcher\": \"Bash\",\n"
				   "      \"hooks\": [{\n"
				   "        \"type\": \"command\",\n"
				   "        \"command\": \"echo ok\",\n"
				   "        \"failClosed\": true\n"
				   "      }]\n"
				   "    }]\n"
				   "  }\n"
				   "}\n"))
			 (should
			  (equal
			   (mevedel-hooks--read-config-file file)
			   '((PreToolUse
			      (:matcher "Bash"
					:hooks ((:type command
						       :command "echo ok"
						       :fail-closed t))))))))
		     (delete-file file))))

(mevedel-deftest mevedel-hooks--read-lisp-file
		 (:quiet t :doc "reads Lisp hook files with read evaluation disabled")
		 (let ((file (make-temp-file "mevedel-hooks" nil ".el"))
		       (mevedel-hooks-test--read-eval-ran nil))
		   (unwind-protect
		       (progn
			 (with-temp-file file
			   (insert "#.(progn "
				   "(setq mevedel-hooks-test--read-eval-ran t) "
				   "nil)"))
			 (should-not (mevedel-hooks--read-config-file file))
			 (should-not mevedel-hooks-test--read-eval-ran))
		     (delete-file file))))

(mevedel-deftest mevedel-hooks-effective-rules
		 (:doc "merges defcustom, user el/json, and trusted project el/json layers")
		 (let* ((root (make-temp-file "mevedel-hooks-ws" t))
			(user-dir (make-temp-file "mevedel-hooks-user" t))
			(workspace (mevedel-hooks-test--workspace root))
			(session (mevedel-session-create "main" workspace root))
			(mevedel-user-dir (file-name-as-directory user-dir))
			(mevedel-hooks-require-project-trust t)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-concat root ".mevedel") t)
			 (make-directory user-dir t)
			 (with-temp-file (file-name-concat user-dir "hooks.el")
			   (prin1
			    '((PreToolUse
			       ((:matcher "Bash"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat user-dir "hooks.json")
			   (insert "{\"hooks\":{\"PostToolUse\":[{\"matcher\":\"Bash\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo ok\"}]}]}}"))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
			   (prin1
			    '((PermissionRequest
			       ((:matcher "*"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.json")
			   (insert "{\"hooks\":{\"PermissionDenied\":[{\"matcher\":\"*\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo deny\"}]}]}}"))
			 ;; Untrusted project configs are reported as they are
			 ;; skipped, which is the behavior under test here.
			 (let (diagnostics)
			   (mevedel-test--with-captured-diagnostics diagnostics
			     (should-not
			      (assq 'PermissionRequest
				    (mevedel-hooks-effective-rules
				     session workspace))))
			   (should (string-match-p "is not trusted" diagnostics)))
			 (mevedel-test--with-captured-diagnostics nil
			   (mevedel-hooks-trust-project workspace))
			 (let* ((rules (mevedel-hooks-effective-rules session workspace))
				(defcustom-handler
				 (car (mevedel-hooks--matching-handlers
				       'PreToolUse '(:tool-name "Read") rules)))
				(user-handler
				 (car (mevedel-hooks--matching-handlers
				       'PostToolUse '(:tool-name "Bash") rules)))
				(project-handler
				 (car (mevedel-hooks--matching-handlers
				       'PermissionDenied '(:tool-name "Bash") rules))))
			   (should (assq 'PreToolUse rules))
			   (should (assq 'PostToolUse rules))
			   (should (assq 'PermissionRequest rules))
			   (should (assq 'PermissionDenied rules))
			   (should (eq 'user
				       (plist-get defcustom-handler :source)))
			   (should (equal (file-name-as-directory user-emacs-directory)
					  (plist-get defcustom-handler :source-root)))
			   (should
			    (equal (file-name-concat user-dir "hooks.json")
				   (plist-get user-handler :source-file)))
			   (should
			    (equal (file-name-as-directory user-dir)
				   (plist-get user-handler :source-root)))
			   (should
			    (equal (file-name-concat root ".mevedel" "hooks.json")
				   (plist-get project-handler :source-file)))
			   (should
			    (equal (file-name-as-directory root)
				   (plist-get project-handler :source-root))))
			 (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
			   (prin1
			    '((SessionStart
			       ((:matcher "startup"
					  :hooks ((:type elisp
							 :function mevedel-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (should-not
			  (assq 'SessionStart
				(mevedel-hooks-effective-rules session workspace)))
			 (delete-file (file-name-concat root ".mevedel" "hooks.json"))
			 (mevedel-test--with-captured-messages nil
			   (mevedel-hooks-trust-project workspace))
			 (should-not
			  (assq 'PermissionDenied
				(mevedel-hooks-effective-rules session workspace))))
		     (delete-directory root t)
		     (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks--config-rules
  (:doc "memoizes the configured layers until an explicit invalidation")
  (let* ((root (make-temp-file "mevedel-hooks-cache-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-cache-user" t)))
         (workspace (mevedel-hooks-test--workspace root))
         (mevedel-user-dir user-dir)
         (mevedel-hooks-require-project-trust t)
         (reads 0)
         (project-files-function
          (symbol-function 'mevedel-hooks--project-config-files)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".mevedel") t)
          (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "Bash"
                  :hooks ((:type command :command "echo project"))))))
             (current-buffer)))
          (mevedel-test--with-captured-messages nil
            (mevedel-hooks-trust-project workspace))
          (cl-letf (((symbol-function 'mevedel-hooks--project-config-files)
                     (lambda (ws)
                       (cl-incf reads)
                       (funcall project-files-function ws))))
            (should (= 1 (length (mevedel-hooks--matching-handlers
                                  'PreToolUse '(:tool-name "Bash")
                                  (mevedel-hooks-effective-rules
                                   nil workspace)))))
            (mevedel-hooks-effective-rules nil workspace)
            (should (= 1 reads))
            (mevedel-test--with-captured-messages nil
              (mevedel-hooks-reload))
            (mevedel-hooks-effective-rules nil workspace)
            (should (= 2 reads))
            ;; Trusting the project invalidates too.
            (mevedel-test--with-captured-messages nil
              (mevedel-hooks-trust-project workspace))
            (mevedel-hooks-effective-rules nil workspace)
            (should (= 3 reads))))
      (clrhash mevedel-hooks--config-rules-cache)
      (delete-directory root t)
      (when (file-directory-p user-dir)
        (delete-directory user-dir t)))))

(mevedel-deftest mevedel-hooks-effective-rules/agents-hook-roots
  (:doc "loads standalone agents hook roots in precedence order and trusts project agents files")
  (let* ((root (make-temp-file "mevedel-hooks-agents-ws" t))
         (home (make-temp-file "mevedel-hooks-agents-home" t))
         (user-dir (file-name-as-directory
                    (file-name-concat home ".mevedel")))
         (workspace (mevedel-hooks-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat home ".agents" "plugins"))
         (mevedel-hooks-require-project-trust t)
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (cl-labels
            ((write-el
              (dir command)
              (make-directory dir t)
              (with-temp-file (file-name-concat dir "hooks.el")
                (prin1
                 `((PreToolUse
                    ((:matcher "Bash"
                      :hooks ((:type command :command ,command))))))
                 (current-buffer))))
             (write-json
              (dir command)
              (make-directory dir t)
              (with-temp-file (file-name-concat dir "hooks.json")
                (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                        "\"hooks\":[{\"type\":\"command\",\"command\":\""
                        command
                        "\"}]}]}}")))
             (commands
              ()
              ;; Resolution reports every untrusted project config it
              ;; skips; this probe runs before and after trust is given.
              (mevedel-test--with-captured-diagnostics nil
                (mapcar (lambda (handler)
                          (plist-get handler :command))
                        (mevedel-hooks--matching-handlers
                         'PreToolUse
                         '(:tool-name "Bash")
                         (mevedel-hooks-effective-rules
                          session workspace))))))
          (setenv "HOME" home)
          (let ((global-agents (file-name-concat home ".agents"))
                (global-mevedel user-dir)
                (project-agents (file-name-concat root ".agents"))
                (project-mevedel (file-name-concat root ".mevedel"))
                (plugin-root (file-name-as-directory
                              (file-name-concat
                               mevedel-plugin-install-directory "repo"))))
            (write-el global-agents "echo global-agents-el")
            (write-json global-agents "echo global-agents-json")
            (write-el global-mevedel "echo global-mevedel-el")
            (write-json global-mevedel "echo global-mevedel-json")
            (make-directory (file-name-concat plugin-root "hooks") t)
            (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo plugin\"}]}]}}"))
            (mevedel-hooks-test--write-plugin-manifest
             plugin-root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (mevedel-plugins-enable "demo" workspace))
            (write-el project-agents "echo project-agents-el")
            (write-json project-agents "echo project-agents-json")
            (write-el project-mevedel "echo project-mevedel-el")
            (write-json project-mevedel "echo project-mevedel-json")
            (should
             (equal '("echo global-agents-el"
                      "echo global-agents-json"
                      "echo global-mevedel-el"
                      "echo global-mevedel-json"
                      "echo plugin")
                    (commands)))
            (mevedel-test--with-captured-messages nil
              (mevedel-hooks-trust-project workspace))
            (should
             (equal '("echo global-agents-el"
                      "echo global-agents-json"
                      "echo global-mevedel-el"
                      "echo global-mevedel-json"
                      "echo plugin"
                      "echo project-agents-el"
                      "echo project-agents-json"
                      "echo project-mevedel-el"
                      "echo project-mevedel-json")
                    (commands)))))
      (delete-directory root t)
      (delete-directory home t))))

(mevedel-deftest mevedel-hooks-effective-rules/malformed-trust-db
  (:quiet t :doc "ignores malformed trusted hook entries")
  (let* ((root (make-temp-file "mevedel-hooks-trust" t))
         (user-dir (make-temp-file "mevedel-hooks-user" t))
         (workspace (mevedel-hooks-test--workspace root))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (mevedel-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".mevedel") t)
          (make-directory user-dir t)
          (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "*"
                  :hooks ((:type elisp
                           :function mevedel-hooks-test--deny-fn))))))
             (current-buffer)))
          (with-temp-file (file-name-concat user-dir "trusted-hooks.el")
            (prin1 (list (intern "test-mevedel-view.el."))
                   (current-buffer)))
          (should-not (assq 'PreToolUse
                            (mevedel-hooks-effective-rules nil workspace))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks-effective-rules/plugin-hooks
  (:doc "loads enabled plugin hook files with metadata between user and project layers")
  (let* ((root (make-temp-file "mevedel-hooks-plugin-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-plugin-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (workspace (mevedel-hooks-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory user-dir t)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (make-directory (file-name-concat root ".mevedel") t)
          (with-temp-file (file-name-concat user-dir "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":\"echo user\"}]}]}}"))
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":\"echo plugin\","
                    "\"timeout\":7,\"statusMessage\":\"plugin status\"}]}]}}"))
          (mevedel-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (with-temp-file (file-name-concat root ".mevedel" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "Bash"
                  :hooks ((:type command :command "echo project"))))))
             (current-buffer)))
          (mevedel-test--with-captured-messages nil
            (mevedel-hooks-trust-project workspace))
          (let ((handlers (mevedel-hooks--matching-handlers
                           'PreToolUse
                           '(:tool-name "Bash")
                           (mevedel-hooks-effective-rules
                            session workspace))))
            (should (equal '(user-file project-file)
                           (mapcar (lambda (handler)
                                     (plist-get handler :source))
                                   handlers))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (mevedel-plugins-enable "demo" workspace))
          (let* ((handlers (mevedel-hooks--matching-handlers
                            'PreToolUse
                            '(:tool-name "Bash")
                            (mevedel-hooks-effective-rules
                             session workspace)))
                 (plugin-handler (cadr handlers)))
            (should (equal '(user-file plugin project-file)
                           (mapcar (lambda (handler)
                                     (plist-get handler :source))
                                   handlers)))
            (should (equal "demo"
                           (plist-get plugin-handler :plugin-name)))
            (should (equal plugin-root
                           (plist-get plugin-handler :plugin-root)))
            (should
             (equal (file-name-concat plugin-root "hooks" "hooks.json")
                    (plist-get plugin-handler :source-file)))
            (should (equal plugin-root
                           (plist-get plugin-handler :source-root)))
            (should (equal (file-name-concat root ".mevedel"
                                             "plugin-data" "demo")
                           (plist-get plugin-handler :plugin-data)))
            (should (= 7 (plist-get plugin-handler :timeout)))
            (should (equal "plugin status"
                           (plist-get plugin-handler :status-message))))
          (mevedel-plugins-disable-hooks "demo" workspace)
          (let ((handlers (mevedel-hooks--matching-handlers
                           'PreToolUse
                           '(:tool-name "Bash")
                           (mevedel-hooks-effective-rules
                            session workspace))))
            (should (equal '(user-file project-file)
                           (mapcar (lambda (handler)
                                     (plist-get handler :source))
                                   handlers)))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks-effective-rules/plugin-hook-manifest-shapes
  (:doc "loads default and explicit string plugin hook files")
  (let* ((root (make-temp-file "mevedel-hooks-plugin-shapes-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-plugin-shapes-user" t)))
         (workspace (mevedel-hooks-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins")))
    (unwind-protect
        (progn
          (let ((default-root (file-name-as-directory
                               (file-name-concat
                                mevedel-plugin-install-directory "default"))))
            (make-directory (file-name-concat default-root "hooks") t)
            (with-temp-file (file-name-concat default-root "hooks" "hooks.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo default\"}]}]}}"))
            (mevedel-hooks-test--write-plugin-manifest
             default-root "{\"name\":\"default\"}")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) t)))
              (mevedel-plugins-enable "default" workspace)))
          (let ((path-root (file-name-as-directory
                            (file-name-concat
                             mevedel-plugin-install-directory "path"))))
            (make-directory (file-name-concat path-root "hooks") t)
            (with-temp-file (file-name-concat path-root "hooks" "a.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo path-a\"}]}]}}"))
            (mevedel-hooks-test--write-plugin-manifest
             path-root
             "{\"name\":\"path\",\"hooks\":\"./hooks/a.json\"}")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) t)))
              (mevedel-plugins-enable "path" workspace)))
          (let* ((handlers (mevedel-hooks--matching-handlers
                            'PreToolUse
                            '(:tool-name "Bash")
                            (mevedel-hooks-effective-rules
                             session workspace)))
                 (commands (sort (mapcar (lambda (handler)
                                            (plist-get handler :command))
                                          handlers)
                                 #'string<)))
            (should (equal '("echo default"
                             "echo path-a")
                           commands))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks-effective-rules/superpowers-manifest-hooks
  (:doc "uses manifest hooks for plugin named superpowers")
  (let* ((root (make-temp-file "mevedel-hooks-superpowers-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-superpowers-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (workspace (mevedel-hooks-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory (file-name-concat plugin-root "hooks") t)
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"SessionStart\":[{\"matcher\":\"startup\","
                    "\"hooks\":[{\"type\":\"command\","
                    "\"command\":\"\\\"${PLUGIN_ROOT}/hooks/run-hook.cmd\\\" session-start-codex\"},"
                    "{\"type\":\"command\","
                    "\"command\":\"superpowers-extra\"}]}]}}"))
          (mevedel-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"superpowers\",\"hooks\":\"hooks/hooks.json\"}")
          (let ((rules (mevedel-hooks-effective-rules session workspace)))
            (should-not
             (mevedel-hooks--matching-handlers
              'SessionStart '(:source "startup") rules)))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (mevedel-plugins-enable "superpowers" workspace))
          (let* ((rules (mevedel-hooks-effective-rules session workspace))
                 (handlers (mevedel-hooks--matching-handlers
                            'SessionStart '(:source "startup") rules)))
            (should (= 2 (length handlers)))
            (should (equal '("\"${PLUGIN_ROOT}/hooks/run-hook.cmd\" session-start-codex"
                             "superpowers-extra")
                           (sort (mapcar (lambda (handler)
                                            (plist-get handler :command))
                                          handlers)
                                 #'string<)))
            (should (cl-every (lambda (handler)
                                (equal "superpowers"
                                       (plist-get handler :plugin-name)))
                              handlers))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(ert-deftest mevedel-hooks-matcher-matches-p ()
  "Match wildcard, exact alternatives, regex, and symbols."
  (dolist (case '((nil "Bash" t)
                  ("*" "Bash" t)
                  ("Bash|Read" "Read" t)
                  ("Bash|Read" "Edit" nil)
                  ("B.*" "Bash" t)
                  (Bash "Bash" t)))
    (pcase-let ((`(,matcher ,target ,expected) case))
      (should (eq (not (null (mevedel-hooks-matcher-matches-p
                              matcher target)))
	                  expected)))))

(mevedel-deftest mevedel-hooks--matching-handlers
  (:doc "ignores malformed normalized rule entries and groups")
  (let* ((handler '(:type elisp
                    :function mevedel-hooks-test--deny-fn))
         (handlers
          (mevedel-hooks--matching-handlers
           'UserPromptSubmit
           '(:prompt "hello")
           `((Docs)
             Docs
             (UserPromptSubmit . Docs)
             (UserPromptSubmit
              Docs
              (:matcher "*" :hooks Docs)
              (:matcher "*" :hooks (Docs (:type nope) ,handler)))))))
    (should (equal handlers (list handler)))))

(mevedel-deftest mevedel-hooks--handlers-for-event ()
  ,test
  (test)
  :doc "prepends native functions as executable Elisp handler records"
  (let ((mevedel-pre-tool-use-functions
         '(mevedel-hooks-test--native-rewrite-fn))
        (rules
         '((PreToolUse
            (:matcher "Bash"
             :hooks ((:type elisp
                      :function mevedel-hooks-test--declarative-capture-fn)))))))
    (should
     (equal
      (mevedel-hooks--handlers-for-event
       'PreToolUse '(:tool-name "Bash") rules)
      '((:type elisp
         :function mevedel-hooks-test--native-rewrite-fn
         :source native)
        (:type elisp
         :function mevedel-hooks-test--declarative-capture-fn)))))

  :doc "preserves buffer-local hook order and the global inheritance marker"
  (let ((mevedel-pre-tool-use-functions
         '(mevedel-hooks-test--native-rewrite-fn)))
    (with-temp-buffer
      (setq-local mevedel-pre-tool-use-functions
                  '(mevedel-hooks-test--declarative-capture-fn t))
      (should
       (equal
        (mapcar (lambda (handler) (plist-get handler :function))
                (mevedel-hooks--handlers-for-event
                 'PreToolUse '(:tool-name "Bash") nil))
        '(mevedel-hooks-test--declarative-capture-fn
          mevedel-hooks-test--native-rewrite-fn))))))

(mevedel-deftest mevedel-hooks--event-json
		 (:doc "serializes Lisp booleans and nil optional fields as JSON values")
		 (let* ((payload (json-parse-string
				  (mevedel-hooks--event-json
				   '(:hook-event-name SubagentStart
		     :read-only nil
		     :aggressive t
		     :agent-path nil
		     :tool-input (:command "true"
				   :dry-run :json-false)))
		  :object-type 'alist
		  :array-type 'list
		  :null-object :null
		  :false-object :false)))
		   (should (eq (alist-get 'read_only payload) :false))
		   (should (eq (alist-get 'aggressive payload) t))
		   (should (eq (alist-get 'agent_path payload) :null))
		   (should (eq (alist-get 'dry_run
					  (alist-get 'tool_input payload))
			       :false)))
		 (let ((table (make-hash-table :test #'equal)))
		   (puthash "nested_false" :json-false table)
		   (let* ((payload (json-parse-string
				    (mevedel-hooks--event-json
				     (list :tool-name "Read" :tool-input table))
				    :object-type 'alist
				    :array-type 'list
				    :false-object :false))
			  (tool-input (alist-get 'tool_input payload)))
		     (should (eq (alist-get 'nested_false tool-input nil nil
					    #'equal)
				 :false)))))

(mevedel-deftest mevedel-hooks--event-json/raw-values
		 (:doc "normalizes raw byte strings and stringifies Lisp objects")
		 ,test
		 (test)
		 (let* ((raw (string (unibyte-char-to-multibyte #x80)))
			(payload (json-parse-string
				  (mevedel-hooks--event-json
				   (list :result raw
					 :callback
					 (lambda () 'not-json)))
				  :object-type 'alist
				  :array-type 'list)))
		   (should (equal "\\x80" (alist-get 'result payload)))
		   (should (stringp (alist-get 'callback payload)))
		   (should (string-match-p "not-json"
					   (alist-get 'callback payload)))))

(mevedel-deftest mevedel-hooks-format-context
  (:doc "escapes event names and delimiter-looking body text")
  (should
   (equal
    (concat
     "<hook-context>\n"
     "<hook-event name=\"UserPromptSubmit\">\n"
     "literal &lt;/hook-event&gt; &amp; &lt;tag&gt; \"quoted\"\n"
     "</hook-event>\n"
     "<hook-event name=\"A&quot;B\">\n"
     "x &amp; y\n"
     "</hook-event>\n"
     "</hook-context>")
    (mevedel-hooks-format-context
     '((:event "UserPromptSubmit"
               :body "literal </hook-event> & <tag> \"quoted\"")
       (:event "A\"B" :body "x & y"))))))

(mevedel-deftest mevedel-hooks-consume-session-context ()
  ,test
  (test)
  :doc "consumes the captured prefix while preserving appended context"
  (let* ((session (mevedel-session--create :name "hooks"))
         (captured '((:event SessionStart :body "start")))
         (appended '((:event UserPromptSubmit :body "later"))))
    (setf (mevedel-session-hook-context-pending session)
          (append captured appended))
    (should (mevedel-hooks-consume-session-context session captured))
    (should (equal appended
                   (mevedel-session-hook-context-pending session))))

  :doc "leaves a changed prefix untouched"
  (let* ((session (mevedel-session--create :name "hooks"))
         (current '((:event SessionStart :body "replacement")))
         (captured '((:event SessionStart :body "start"))))
    (setf (mevedel-session-hook-context-pending session) current)
    (should-not
     (mevedel-hooks-consume-session-context session captured))
    (should (equal current
                   (mevedel-session-hook-context-pending session)))))


;;
;;; Decisions

(mevedel-deftest mevedel-hooks--context-contribution
  (:doc "keeps one handler's identity, reason, and ordered contexts")
  (should
   (equal
    '(:contexts ("a" "b")
      :source plugin
      :plugin-name "ponytail"
      :function inject
      :reason "active")
    (mevedel-hooks--context-contribution
     '(:source plugin :plugin-name "ponytail" :function inject)
     '(:additional-context ("a" "b") :system-message "active")))))

(mevedel-deftest mevedel-hooks-merge-decisions
		 (:doc "merges contexts and keeps restrictive permission precedence")
		 (let* ((first '(:permission-decision allow
						      :additional-context ("a")
						      :updated-result "one"))
			(second '(:permission-decision ask
						       :additional-context "b"))
			(third '(:permission-decision deny
						      :permission-reason "no")))
		   (let ((decision
			  (mevedel-hooks-merge-decisions
			   (mevedel-hooks-merge-decisions
			    (mevedel-hooks-merge-decisions nil first)
			    second)
			   third)))
		     (should (eq (plist-get decision :permission-decision) 'deny))
		     (should (equal (plist-get decision :additional-context)
				    '("a" "b")))
		     (should (equal (plist-get decision :updated-result) "one"))
		     (should (equal (plist-get decision :permission-reason) "no")))))

(mevedel-deftest mevedel-hooks-merge-decisions/malformed
  (:doc "ignores malformed base and next decisions")
  (let ((bad (intern "test-mevedel-view.el.")))
    (should-not (mevedel-hooks-merge-decisions bad bad))
    (should
     (equal (mevedel-hooks-merge-decisions
             bad '(:system-message "ok"))
            '(:system-message "ok")))))

(mevedel-deftest mevedel-hooks-decision-accessors
  (:doc "treat malformed decisions as nil")
  (let ((bad (intern "test-mevedel-view.el.")))
    (should-not (mevedel-hooks-terminal-decision-p
                 bad 'UserPromptSubmit))
    (should-not (mevedel-hooks-additional-context-string bad))
    (should-not (mevedel-hooks--decision-blocking-p bad))
    (should-not (mevedel-hooks-decision-reason bad))
    (should (equal (mevedel-hooks--apply-decision-to-event-plist
                    'UserPromptSubmit '(:prompt "old") bad)
                   '(:prompt "old")))))

(mevedel-deftest mevedel-hooks--parse-command-decision
		 (:doc "parses root and hookSpecificOutput JSON decisions")
		 ,test
		 (test)
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"continue\":false,\"stopReason\":\"halt\"}")
		   '(:continue nil :stop-reason "halt")))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"hookSpecificOutput\":{\"permissionDecision\":\"deny\",\
\"permissionDecisionReason\":\"no\"}}")
		   '(:permission-reason "no" :permission-decision deny)))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"permissionDecision\":\"deny\",\"permissionReason\":\"documented\"}")
		   '(:permission-reason "documented" :permission-decision deny)))
		 (should
		  (equal
		   (mevedel-hooks--parse-command-decision
		    "{\"updatedInput\":{\"file_path\":\"x\",\"old_string\":\"a\"}}")
		   '(:updated-input (:file_path "x" :old_string "a"))))
			 (should
			  (equal
			   (mevedel-hooks--parse-command-decision
			    "{\"suppressOutput\":true}")
			   '(:hook-error "Unsupported hook decision field: suppressOutput")))
			 (should-not
			  (mevedel-hooks--parse-command-decision
			   "{\"suppressOutput\":false}")))

(mevedel-deftest mevedel-hooks--command-timeout
		 (:doc "defaults malformed timeouts and clamps numeric timeouts")
		 (let ((mevedel-hooks-command-timeout 30)
		       (mevedel-hooks-command-timeout-max 120))
		   (should (= (mevedel-hooks--command-timeout
			       '(:type command :command "x" :timeout "30"))
			      30))
		   (should (= (mevedel-hooks--command-timeout
			       '(:type command :command "x" :timeout 500))
			      120))))


;;
;;; Execution

(mevedel-deftest mevedel-hooks-run-event
		 (:doc "runs matching Elisp hooks and ignores nonmatching groups")
		 (let* ((root (make-temp-file "mevedel-hooks-run" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn)))
			     (:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo old"))
				  cb session)))))
			 (should (equal decision
					'(:updated-input (:command "echo rewritten"))))
			 (should (= (length (mevedel-session-hook-log session)) 1)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/request-cancellation
  (:doc "request cancellation stops a command hook before delayed effects")
  (let* ((root (make-temp-file "mevedel-hooks-command-cancel" t))
         (ready-file (file-name-concat root "ready"))
         (late-file (file-name-concat root "late"))
         (session (mevedel-hooks-test--session root))
         (request (mevedel-request--create :session session))
         (mevedel-hooks-slow-threshold nil)
         (mevedel-hook-rules
          `((PreToolUse
             ((:matcher "Bash"
               :hooks
               ((:type command
                 :command
                 ,(format "printf ready > %s; sleep 0.2; printf late > %s"
                          (shell-quote-argument ready-file)
                          (shell-quote-argument late-file))
                 :timeout 5)))))))
         callback-called)
    (unwind-protect
        (progn
          (skip-unless (not (eq system-type 'windows-nt)))
          (mevedel-hooks-run-event
           'PreToolUse '(:tool-name "Bash")
           (lambda (_) (setq callback-called t))
           session nil request)
          (let ((deadline (+ (float-time) 2)))
            (while (and (not (file-exists-p ready-file))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should (file-exists-p ready-file))
          (mevedel-request-drain-cancellers request)
          (let ((deadline (+ (float-time) 0.5)))
            (while (< (float-time) deadline)
              (accept-process-output nil 0.01)))
          (should-not (file-exists-p late-file))
          (should-not callback-called))
      (mevedel-request-drain-cancellers request)
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-context-audit-records
  (:quiet t :doc "attributes merged context to handlers in execution order")
  (let* ((root (make-temp-file "mevedel-hooks-context-audit" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hook-rules nil))
    (setf (mevedel-session-hook-rules session)
          '((SubagentStart
             ((:matcher "explorer"
               :hooks ((:type elisp
                        :function mevedel-hooks-test--first-context-fn
                        :source plugin
                        :plugin-name "ponytail")
                       (:type elisp
                        :function mevedel-hooks-test--second-context-fn
                        :source project-file
                        :description "Inject project conventions")))))))
    (unwind-protect
        (let* ((decision
                (mevedel-hooks-test--await
                 (lambda (cb)
                   (mevedel-hooks-run-event
                    'SubagentStart '(:role "explorer") cb session))))
               (audits (mevedel-hooks-context-audit-records
                        decision 'SubagentStart 'subagent-context t))
               (handlers (plist-get (car audits) :handlers)))
          (should (equal '("first" "second-a" "second-b")
                         (plist-get decision :additional-context)))
          (should-not (plist-member decision :hook-context-handlers))
          (should (= 1 (length audits)))
          (should (= 2 (length handlers)))
          (should (equal "ponytail"
                         (plist-get (car handlers) :plugin-name)))
          (should (equal 'plugin
                         (plist-get (car handlers) :source)))
          (should (equal "first reason"
                         (plist-get (car handlers) :reason)))
          (should (equal "Inject project conventions"
                         (plist-get (cadr handlers) :description)))
          (should (equal 'project-file
                         (plist-get (cadr handlers) :source)))
          (should-not (plist-member (cadr handlers) :reason))
          (should-not (plist-member (car audits) :context)))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/session-start-log-source
  (:doc "logs whether SessionStart initializes or resumes a session")
  (let* ((root (make-temp-file "mevedel-hooks-session-source" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hook-rules
          '((SessionStart
             ((:matcher "*"
               :hooks ((:type elisp
                        :function mevedel-hooks-test--context-fn))))))))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session)
                (file-name-as-directory root))
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'SessionStart '(:source "resume") cb session)))
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'SessionStart '(:source "startup") cb session)))
          (should
           (equal '("resume" "startup")
                  (mapcar (lambda (entry)
                            (plist-get entry :event-source))
                          (mevedel-session-hook-log session))))
          (with-temp-buffer
            (insert-file-contents (file-name-concat root "hook-log.el"))
            (should (equal "resume" (plist-get (read (current-buffer))
                                               :event-source)))
            (should (equal "startup" (plist-get (read (current-buffer))
                                                :event-source)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/serial-mutation
		 (:doc "passes updated input from one hook to later hooks")
		 (let* ((root (make-temp-file "mevedel-hooks-serial" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hooks-test--seen-event nil)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn)
					       (:type elisp
						      :function mevedel-hooks-test--capture-fn))))))))
		   (unwind-protect
		       (progn
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse
			     '(:tool-name "Bash" :tool-input (:command "echo old"))
			     cb session)))
			 (should (equal (plist-get mevedel-hooks-test--seen-event
						   :tool-input)
					'(:command "echo rewritten"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/unified-elisp-engine
  (:doc "runs native then declarative Elisp with shared serial mutation")
  (let* ((root (make-temp-file "mevedel-hooks-unified" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hooks-test--execution-order nil)
         (mevedel-hooks-test--native-seen-event nil)
         (mevedel-hooks-test--seen-event nil)
         (mevedel-pre-tool-use-functions
          '(mevedel-hooks-test--native-rewrite-fn))
         (mevedel-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function
                        mevedel-hooks-test--declarative-capture-fn))))))))
    (unwind-protect
        (progn
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'PreToolUse
              '(:tool-name "Bash" :tool-input (:command "original"))
              cb session)))
          (should (equal '(native declarative)
                         mevedel-hooks-test--execution-order))
          (should (equal '(:command "native rewrite")
                         (plist-get mevedel-hooks-test--seen-event
                                    :tool-input)))
          (should-not (plist-member mevedel-hooks-test--native-seen-event
                                    :hook-handler))
          (should (plist-member mevedel-hooks-test--seen-event
                                :hook-handler))
          (should (= 2 (length (mevedel-session-hook-log session)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/unified-elisp-errors
  (:doc "normalizes native and declarative malformed decisions once each")
  (let* ((root (make-temp-file "mevedel-hooks-unified-error" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-pre-tool-use-functions
          '(mevedel-hooks-test--suppress-output-fn))
         (mevedel-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function mevedel-hooks-test--suppress-output-fn)
                       (:type elisp
                        :function mevedel-hooks-test--context-fn))))))))
    (unwind-protect
        (let ((decision
               (mevedel-hooks-test--await
                (lambda (cb)
                  (mevedel-hooks-run-event
                   'PreToolUse '(:tool-name "Bash") cb session)))))
          (should (equal '("later")
                         (plist-get decision :additional-context)))
          (should (equal '(error error ok)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (mevedel-session-hook-log session)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/unified-elisp-terminal
  (:quiet t :doc "a terminal native decision skips later declarative handlers")
  (let* ((root (make-temp-file "mevedel-hooks-unified-terminal" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-pre-tool-use-functions
          '(mevedel-hooks-test--stop-fn))
         (mevedel-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function mevedel-hooks-test--context-fn))))))))
    (unwind-protect
        (let ((decision
               (mevedel-hooks-test--await
                (lambda (cb)
                  (mevedel-hooks-run-event
                   'PreToolUse '(:tool-name "Bash") cb session)))))
          (should-not (plist-get decision :continue))
          (should-not (plist-get decision :additional-context))
          (should (= 1 (length (mevedel-session-hook-log session)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/stale-ambient-context
		 (:doc "ignores stale non-struct request context when collecting rules")
		 (let* ((root (make-temp-file "mevedel-hooks-stale" t))
			(session (mevedel-hooks-test--session root))
			(mevedel--current-request 'gs)
			(mevedel--agent-invocation 'gs))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hello")
				  cb session)))))
			 (should-not decision))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/malformed-decision
		 (:doc "does not expose malformed native hook return values to callers")
		 (let* ((root (make-temp-file "mevedel-hooks-malformed" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-user-prompt-submit-functions
			 '(mevedel-hooks-test--malformed-fn)))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hello")
				  cb session)))))
			 (should-not decision))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/malformed-symbol-decision
  (:doc "does not expose malformed non-keyword symbol decisions to callers")
  (let* ((root (make-temp-file "mevedel-hooks-malformed-symbol" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-user-prompt-submit-functions
          '(mevedel-hooks-test--malformed-file-symbol-fn)))
    (unwind-protect
        (let ((decision
               (mevedel-hooks-test--await
                (lambda (cb)
                  (mevedel-hooks-run-event
                   'UserPromptSubmit
                   '(:prompt "hello")
                   cb session)))))
          (should-not decision))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/session-reminders
  (:quiet t :doc "queues model-visible reminders for blocking outcomes only")
  (let* ((root (make-temp-file "mevedel-hooks-reminders" t))
         (session (mevedel-hooks-test--session root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setq-local mevedel--current-request
                      (mevedel-request--create :id "request-1"
                                               :session session))
          (let ((mevedel-pre-tool-use-functions
                 '(mevedel-hooks-test--deny-fn)))
            (mevedel-hooks-test--await
             (lambda (cb)
               (mevedel-hooks-run-event
                'PreToolUse
                '(:tool-name "Bash" :tool-input (:command "echo hi"))
                cb session)))
            (let ((body
                   (plist-get
                    (cdar (plist-get mevedel-reminders--turn-events :items))
                    :body)))
              (should (string-match-p "PreToolUse hook blocked" body))
              (should (string-match-p "blocked" body)))))
      (delete-directory root t)))
  (let* ((root (make-temp-file "mevedel-hooks-reminders" t))
         (session (mevedel-hooks-test--session root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setq-local mevedel--current-request
                      (mevedel-request--create :id "request-1"
                                               :session session))
          (let ((mevedel-post-tool-use-functions
                 '(mevedel-hooks-test--system-message-fn)))
            (mevedel-hooks-test--await
             (lambda (cb)
               (mevedel-hooks-run-event
                'PostToolUse
                '(:tool-name "Read" :tool-result "ok")
                cb session)))
            (should-not
             (plist-get mevedel-reminders--turn-events :items))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command
		 (:doc "runs command hooks, parses JSON stdout, and logs stderr privately")
		 (let* ((root (make-temp-file "mevedel-hooks-command" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 `((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command
						      :command
						      ,(mevedel-hooks-test--emacs-command
							"(princ \"{\\\"permissionDecision\\\":\\\"ask\\\",\\\"permissionDecisionReason\\\":\\\"review\\\"}\")")
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo hi"))
				  cb session)))))
			 (should (equal decision
					'(:permission-reason "review"
							     :permission-decision ask)))
			 (should (= (length (mevedel-session-hook-log session)) 1))
			 (should (equal (plist-get (car (mevedel-session-hook-log session))
						   :stdout-preview)
					"{\"permissionDecision\":\"ask\",\
\"permissionDecisionReason\":\"review\"}")))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command-output-cap
                         (:quiet t :doc "caps command hook output before returning block reasons")
                         (let* ((root (make-temp-file "mevedel-hooks-command" t))
                                (session (mevedel-hooks-test--session root))
                                (mevedel-hooks-command-output-max-chars 16)
                                (mevedel-hook-rules
                                 `((PreToolUse
                                    ((:matcher "Bash"
                                               :hooks ((:type command
                                                              :command
                                                              ,(mevedel-hooks-test--emacs-command
                                                                "(progn (princ (make-string 100 ?x)) (kill-emacs 2))")
                                                              :timeout 5))))))))
                           (unwind-protect
                               (let ((decision
                                      (mevedel-hooks-test--await
                                       (lambda (cb)
                                         (mevedel-hooks-run-event
                                          'PreToolUse
                                          '(:tool-name "Bash" :tool-input (:command "echo hi"))
                                          cb session)))))
                                 (should (equal 'deny
                                                (plist-get decision :permission-decision)))
                                 (should (string-match-p
                                          "Hook output truncated at 16 character limit"
                                          (plist-get decision :permission-reason))))
                             (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/nonblocking-events-continue
		 (:doc "does not let unsupported stop decisions skip observer hooks")
		 (let* ((root (make-temp-file "mevedel-hooks-observer" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--stop-fn)
					       (:type elisp
						      :function mevedel-hooks-test--context-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PostToolUse
				  '(:tool-name "Read" :result "ok")
				  cb session)))))
			 (should (equal (plist-get decision :additional-context)
					'("later"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/permission-request-terminal
		 (:quiet t :doc "stops later PermissionRequest hooks after fail-closed stop decisions")
		 (let* ((root (make-temp-file "mevedel-hooks-pr-terminal" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PermissionRequest
			    ((:matcher "Edit"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--stop-fn)
					       (:type elisp
						      :function mevedel-hooks-test--allow-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'PermissionRequest
				  '(:tool-name "Edit")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "stop"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/command-block
		 (:quiet t :doc "maps exit-code 2 to event-specific blocking decisions")
		 (let* ((root (make-temp-file "mevedel-hooks-block" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 `((UserPromptSubmit
			    ((:matcher "*"
				       :hooks ((:type command
						      :command
						      ,(mevedel-hooks-test--emacs-command
							"(progn (princ \"blocked\") (kill-emacs 2))")
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (mevedel-hooks-test--await
			       (lambda (cb)
				 (mevedel-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hi")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "blocked"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/unannotated-command-origin
  (:quiet t :doc "fails closed instead of inferring where an unannotated command runs")
  (let* ((root (make-temp-file "mevedel-hooks-unannotated" t))
         (marker (file-name-concat root "ran"))
         (session (mevedel-hooks-test--session root))
         (mevedel-hook-rules nil))
    (setf (mevedel-session-hook-rules session)
          `((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command
                        :command ,(format "touch %s"
                                          (shell-quote-argument marker)))))))))
    (unwind-protect
        (let ((decision
               (mevedel-hooks-test--await
                (lambda (cb)
                  (mevedel-hooks-run-event
                   'PreToolUse
                   '(:tool-name "Bash" :tool-input (:command "echo hi"))
                   cb session)))))
          (should (eq 'deny (plist-get decision :permission-decision)))
          (should (string-match-p
                   "execution origin"
                   (plist-get decision :permission-reason)))
          (should-not (file-exists-p marker)))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/project-command-cwd
		 (:doc "runs project-sourced command hooks from the workspace root")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-hooks-project-cwd" t)))
			(subdir (file-name-as-directory
				 (file-name-concat root "subdir")))
			session
			(mevedel-hook-rules nil))
		   (unwind-protect
		       (progn
			 (make-directory subdir t)
			 (setq session
			       (mevedel-session-create
				"hooks-test"
				(mevedel-hooks-test--workspace root)
				subdir))
			 (setf
			  (mevedel-session-hook-rules session)
			  `((PreToolUse
			     ((:matcher "Bash"
				:hooks ((:type command
					 :command
					 ,(mevedel-hooks-test--emacs-command
					   "(princ default-directory)")
					 :source project-file
					 :source-root ,root
					 :timeout 5)))))))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse
			     (mevedel-hooks-event-plist
			      'PreToolUse session (mevedel-session-workspace session)
			      :cwd subdir
			      :tool-name "Bash"
			      :tool-input '(:command "echo hi"))
			     cb session)))
			 (should
			  (file-equal-p
			   (directory-file-name
			    (string-trim
			     (plist-get (car (mevedel-session-hook-log session))
					:stdout-preview)))
			   (directory-file-name root))))
			     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/remote-command-origin
  (:doc "dispatches remote project and local user/plugin commands by origin")
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-hooks-remote-origin" t)))
         (subdir (file-name-as-directory (file-name-concat root "subdir")))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (remote-subdir (concat remote-root "subdir/"))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-local-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (project-plugin-local-root
          (file-name-as-directory
           (file-name-concat root ".mevedel" "plugins" "target")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (mevedel-hook-rules nil)
         (mevedel-hooks-persist-log nil)
         (mevedel-hooks-command-output-max-chars 16)
         (mevedel-hooks-require-project-trust nil)
         (process-environment (copy-sequence process-environment))
         workspace session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (make-directory subdir t)
          (make-directory (file-name-concat root ".mevedel") t)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (make-directory (file-name-concat project-plugin-local-root "hooks") t)
          (with-temp-file (file-name-concat user-dir "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s}]}]}}"
              (json-encode-string
               "pwd > user-pwd; cat > user-input.json"))))
          (with-temp-file (file-name-concat root ".mevedel" "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s},{\"type\":\"elisp\",\"function\":\"mevedel-hooks-test--capture-elisp-origin\"}]}]}}"
              (json-encode-string
               (concat "pwd > project-pwd; cat > project-input.json; "
                       "printf remote-stderr-with-extra-output >&2")))))
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s}]}]}}"
              (json-encode-string
               (concat
                "pwd > plugin-pwd; "
                "printf '%s|%s' \"$MEVEDEL_PLUGIN_ROOT\" "
                "\"${MEVEDEL_PLUGIN_DATA-unset}\" > plugin-env; "
                "cat > plugin-input.json")))))
          (with-temp-file
              (file-name-concat project-plugin-local-root "hooks" "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s}]}]}}"
              (json-encode-string
               (concat
                "pwd > project-plugin-pwd; "
                "printf '%s|%s' \"$MEVEDEL_PLUGIN_ROOT\" "
                "\"$MEVEDEL_PLUGIN_DATA\" > project-plugin-env; "
                "cat > project-plugin-input.json")))))
          (mevedel-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (mevedel-hooks-test--write-plugin-manifest
           project-plugin-local-root
           "{\"name\":\"target-demo\",\"hooks\":\"hooks/hooks.json\"}")
          (setq workspace (mevedel-hooks-test--workspace remote-root)
                session (mevedel-session-create
                         "remote-hooks" workspace remote-subdir)
                mevedel-hooks-test--elisp-origin nil)
          (let ((mevedel--session session))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (mevedel-plugins-enable "demo" workspace)
              (mevedel-plugins-enable "target-demo" workspace)))
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'PreToolUse
              (mevedel-hooks-event-plist
               'PreToolUse session workspace
               :tool-name "Bash"
               :tool-input '(:command "echo hi"))
              cb session workspace)))
          (cl-labels
              ((file-text (file)
                 (string-trim
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
               (read-input (file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (json-parse-buffer :object-type 'alist))))
            (should (file-equal-p root
                                  (file-text
                                   (file-name-concat root "project-pwd"))))
            (should (file-equal-p user-dir
                                  (file-text
                                   (file-name-concat user-dir "user-pwd"))))
            (should (file-equal-p plugin-root
                                  (file-text
                                   (file-name-concat plugin-root "plugin-pwd"))))
            (should
             (equal (concat plugin-root "|unset")
                    (file-text
                     (file-name-concat plugin-root "plugin-env"))))
            (should
             (file-equal-p
              project-plugin-local-root
              (file-text
               (file-name-concat project-plugin-local-root
                                 "project-plugin-pwd"))))
            (should
             (equal
              (concat project-plugin-local-root "|"
                      (file-name-concat root ".mevedel" "plugin-data"
                                        "target-demo"))
              (file-text
               (file-name-concat project-plugin-local-root
                                 "project-plugin-env"))))
            (dolist (input (list (read-input
                                  (file-name-concat root "project-input.json"))
                                 (read-input
                                  (file-name-concat user-dir "user-input.json"))
                                 (read-input
                                  (file-name-concat plugin-root
                                                    "plugin-input.json"))
                                 (read-input
                                  (file-name-concat
                                   project-plugin-local-root
                                   "project-plugin-input.json"))))
              (should (equal subdir (alist-get 'cwd input)))
              (should (equal root (alist-get 'workspace_root input)))
              (let ((target (alist-get 'execution_target input)))
                (should (equal "mevedelmock" (alist-get 'method target)))
                (should (equal (system-name) (alist-get 'host target)))))
            (should
             (equal remote-subdir
                    (plist-get
                     (plist-get mevedel-hooks-test--elisp-origin :event)
                     :cwd)))
            (should-not
             (file-remote-p
              (plist-get mevedel-hooks-test--elisp-origin
                         :default-directory)))
            (let ((entry
                   (cl-find-if
                    (lambda (item)
                      (eq 'project-file
                          (plist-get (plist-get item :handler) :source)))
                    (mevedel-session-hook-log session))))
              (should (string-match-p
                       "Hook output truncated at 16 character limit"
                       (plist-get entry :stderr-preview))))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks-run-event/foreign-command-target
  (:quiet t :doc "refuses a plugin command rooted on another remote target")
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-hooks-foreign" t)))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (foreign-root (format "/mevedelmock:foreign:%s" root))
         (marker (file-name-concat root "foreign-ran"))
         session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("foreign")
          (setq session
                (mevedel-session-create
                 "remote-hooks"
                 (mevedel-hooks-test--workspace remote-root)
                 remote-root))
          (setf
           (mevedel-session-hook-rules session)
           `((PreToolUse
              ((:matcher "Bash"
                :hooks ((:type command
                         :source plugin
                         :plugin-root ,foreign-root
                         :plugin-data ,(file-name-concat foreign-root "data")
                         :command ,(format "touch %s; cat >/dev/null"
                                           (shell-quote-argument marker)))))))))
          (let ((decision
                 (mevedel-hooks-test--await
                  (lambda (cb)
                    (mevedel-hooks-run-event
                     'PreToolUse
                     '(:tool-name "Bash" :tool-input (:command "echo hi"))
                     cb session)))))
            (should (eq 'deny (plist-get decision :permission-decision)))
            (should (string-match-p
                     "another execution target"
                     (plist-get decision :permission-reason)))
            (should-not (file-exists-p marker))))
      (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/plugin-command-env
  (:quiet t :doc "runs plugin command hooks with compatibility env and creates data dir")
  (let* ((root (make-temp-file "mevedel-hooks-plugin-env-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "mevedel-hooks-plugin-env-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (mevedel-user-dir user-dir)
         (mevedel-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (process-environment (copy-sequence process-environment))
         (session (mevedel-hooks-test--session root))
         (workspace (mevedel-session-workspace session))
         (data-dir (file-name-concat root ".mevedel"
                                     "plugin-data" "demo"))
         (env-command
          (mevedel-hooks-test--emacs-command
           "(princ (format \"{\\\"systemMessage\\\":\\\"%s|%s|%s|%s|%s|%s\\\"}\" (getenv \"PLUGIN_ROOT\") (getenv \"CLAUDE_PLUGIN_ROOT\") (getenv \"PLUGIN_DATA\") (getenv \"CLAUDE_PLUGIN_DATA\") (getenv \"MEVEDEL_PLUGIN_ROOT\") (getenv \"MEVEDEL_PLUGIN_DATA\")))")))
    (unwind-protect
        (progn
          (mevedel-hooks-test--clear-plugin-env)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":"
                    (json-encode-string env-command)
                    "}]}]}}"))
          (mevedel-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (mevedel-plugins-enable "demo" workspace))
          (let ((decision
                 (mevedel-hooks-test--await
                  (lambda (cb)
                    (mevedel-hooks-run-event
                     'PreToolUse
                     '(:tool-name "Bash" :tool-input (:command "echo hi"))
                     cb session)))))
            (should
             (equal (mapconcat #'identity
                               (list plugin-root
                                     plugin-root
                                     data-dir
                                     data-dir
                                     plugin-root
                                     data-dir)
                               "|")
                    (plist-get decision :system-message)))
            (should (file-directory-p data-dir)))
          (mevedel-plugins-disable "demo" workspace)
          (let* ((clean-command
                  (mevedel-hooks-test--emacs-command
                   "(princ (if (or (getenv \"PLUGIN_ROOT\") (getenv \"CLAUDE_PLUGIN_ROOT\") (getenv \"PLUGIN_DATA\") (getenv \"CLAUDE_PLUGIN_DATA\") (getenv \"MEVEDEL_PLUGIN_ROOT\") (getenv \"MEVEDEL_PLUGIN_DATA\")) \"{\\\"systemMessage\\\":\\\"leaked\\\"}\" \"{\\\"systemMessage\\\":\\\"clean\\\"}\"))"))
                 (mevedel-hook-rules
                  `((PreToolUse
                     ((:matcher "Bash"
                       :hooks ((:type command
                                :command ,clean-command))))))))
            (let ((decision
                   (mevedel-hooks-test--await
                    (lambda (cb)
                      (mevedel-hooks-run-event
                       'PreToolUse
                       '(:tool-name "Bash" :tool-input (:command "echo hi"))
                       cb session)))))
              (should (equal "clean"
                             (plist-get decision :system-message))))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-hooks-run-event/command-continuation-buffer
		 (:quiet t :doc "resumes later Elisp handlers in the original dispatch buffer")
		 (let* ((root (make-temp-file "mevedel-hooks-continuation" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hooks-test--seen-buffer nil)
			(mevedel-hook-rules
			 `((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type command
						      :command
						      ,(mevedel-hooks-test--emacs-command
							"(princ \"{\\\"systemMessage\\\":\\\"ok\\\"}\")")
						      :timeout 5)
					       (:type elisp
						      :function
						      mevedel-hooks-test--buffer-fn))))))))
		   (unwind-protect
		       (with-temp-buffer
			 (let ((dispatch-buffer (current-buffer)))
			   (mevedel-hooks-test--await
			    (lambda (cb)
			      (mevedel-hooks-run-event
			       'PostToolUse
			       '(:tool-name "Read" :result "ok")
			       cb session)))
			   (should (eq mevedel-hooks-test--seen-buffer
				       dispatch-buffer))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/fail-closed
		 (:quiet t :doc "command failures fail open by default and fail closed when requested")
		 (let* ((root (make-temp-file "mevedel-hooks-fail" t))
			(session (mevedel-hooks-test--session root)))
		   (unwind-protect
		       (let* ((open-decision
			       (let ((mevedel-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"))))))))
				 (mevedel-hooks-test--await
				  (lambda (cb)
				    (mevedel-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session)))))
			      (closed-decision
			       (let ((mevedel-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"
								   :fail-closed t))))))))
				 (mevedel-hooks-test--await
				  (lambda (cb)
				    (mevedel-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session))))))
			 (should-not open-decision)
			 (should (equal closed-decision
					'(:continue nil
						    :stop-reason "Hook exited with status 1"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-dry
		 (:doc "reports matching hooks without executing them")
		 (let* ((root (make-temp-file "mevedel-hooks-dry" t))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command :command "exit 99")
					       (:type elisp
						      :function mevedel-hooks-test--deny-fn)))
			     (:matcher "Read"
				       :hooks ((:type command :command "exit 99"))))))))
		   (unwind-protect
		       (let ((result
			      (mevedel-hooks-run-dry
			       'PreToolUse
			       '(:tool-name "Bash")
			       session)))
			 (should (eq (plist-get result :event) 'PreToolUse))
			 (should (equal (plist-get result :matcher-target) "Bash"))
			 (should (= (plist-get result :handler-count) 2))
			 (should-not (mevedel-session-hook-log session)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-log-persistence
		 (:doc "persists sanitized hook log entries under the session directory")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-hooks-log" t)))
			(session (mevedel-hooks-test--session root))
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (setf (mevedel-session-save-path session) root)
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse '(:tool-name "Bash") cb session)))
			 (let ((file (mevedel-hooks-log-path session)))
			   (should (file-readable-p file))
			   (with-temp-buffer
			     (insert-file-contents file)
			     (goto-char (point-min))
			     (let ((entry (read (current-buffer))))
			       (should (eq (plist-get entry :event) 'PreToolUse))
			       (should (eq (plist-get entry :status) 'ok))))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-surface-blocking-decision
		 (:doc "surfaces blocking hook decisions and system messages to the user")
		 (let* ((root (make-temp-file "mevedel-hooks-surface" t))
			(session (mevedel-hooks-test--session root))
			(messages nil)
			(mevedel-hooks-slow-threshold nil)
			(mevedel-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function mevedel-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PreToolUse '(:tool-name "Read") cb session)))
			 (should (member "mevedel: PreToolUse hook blocked: blocked"
					 messages)))
		     (delete-directory root t)))
		 (let* ((root (make-temp-file "mevedel-hooks-surface" t))
			(session (mevedel-hooks-test--session root))
			(messages nil)
			(mevedel-hooks-slow-threshold nil)
			(mevedel-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function
						      (lambda (_event)
							'(:system-message "formatted result"))))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (mevedel-hooks-test--await
			  (lambda (cb)
			    (mevedel-hooks-run-event
			     'PostToolUse '(:tool-name "Read") cb session)))
			 (should (member "mevedel: PostToolUse hook: formatted result"
					 messages)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-hooks-run-event/slow-progress (:quiet t)
  ,test
  (test)
  :doc "slow progress restores request status without disturbing composer or agent"
  (let* ((root (make-temp-file "mevedel-hooks-progress" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hooks-slow-threshold 1)
         (mevedel-view-spinner-animate nil)
         (mevedel-hook-rules
          '((PreToolUse
             ((:matcher "Read"
               :hooks ((:type elisp :function ignore)))))))
         slow-callback handler-finish hook-result)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/worker"
                            (mevedel-agent-record--create
                             :id "worker" :path "/root/worker"
                             :parent-path "/root" :activity 'running))))
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request
                        (mevedel-request--create
                         :session session :started-at (current-time))))
          (with-current-buffer view-buf
            (mevedel-view--start-spinner "Working...")
            (mevedel-view-test--insert-composer-draft
             "> quoted\nsecond line" 4))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq slow-callback
                             (lambda () (apply function args)))
                       nil))
                    ((symbol-function 'mevedel-hooks--run-handlers)
                     (lambda (_event _handlers _payload _session _request
                              _context callback _dispatch-buffer)
                       (setq handler-finish callback))))
            (with-current-buffer data-buf
              (mevedel-hooks-run-event
               'PreToolUse '(:tool-name "Read")
               (lambda (decision) (setq hook-result decision)) session))
            (should slow-callback)
            (funcall slow-callback)
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (mevedel-view--input-text)))
              (should (= (point) (+ (mevedel-view--input-start) 4)))
              (should (string-match-p
                       "Running PreToolUse hook"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              (should (string-match-p
                       "1 agent running"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))))
            (funcall handler-finish nil)
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (mevedel-view--input-text)))
              (should (= (point) (+ (mevedel-view--input-start) 4)))
              (should (equal "Working..." mevedel-view--spinner-status))
              (should (eq 'request mevedel-view--spinner-owner)))
            (should-not hook-result)))
      (delete-directory root t)))

  :doc "normal events without handlers create neither telemetry nor progress"
  (let* ((root (make-temp-file "mevedel-hooks-no-progress" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hooks-slow-threshold 1)
         (mevedel-hook-rules nil)
         telemetry result)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _args)
                     (ert-fail "Hook without handlers scheduled progress")))
                  ((symbol-function 'mevedel-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) telemetry)
                     'span))
                  ((symbol-function 'mevedel-telemetry-finish)
                   (lambda (_span &rest props)
                     (push (cons 'finish props) telemetry))))
          (mevedel-hooks-run-event
           'PreToolUse '(:tool-name "Read")
           (lambda (decision) (setq result decision)) session)
          (should-not result)
          (should-not telemetry))
      (delete-directory root t)))

  :doc "profiler events retain aggregate spans without handlers"
  (let* ((root (make-temp-file "mevedel-hooks-profiled-empty" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hook-rules nil)
         telemetry)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-telemetry-detailed-p)
                   (lambda (_session) t))
                  ((symbol-function 'mevedel-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) telemetry)
                     'span))
                  ((symbol-function 'mevedel-telemetry-finish)
                   (lambda (_span &rest props)
                     (push (cons 'finish props) telemetry))))
          (mevedel-hooks-run-event
           'PreToolUse '(:tool-name "Read") #'ignore session)
          (should (equal 0
                         (plist-get (cdr (assq 'hook-event telemetry))
                                    :handler-count)))
          (should (assq 'finish telemetry)))
      (delete-directory root t)))

  :doc "a concurrent hook cannot lease another hook's temporary status"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-test--with-buffers
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--update-spinner
         "Running PreToolUse hook..." (gensym "hook-progress-")))
      (with-current-buffer data-buf
        (should-not (mevedel-hooks--progress-snapshot))))))

(provide 'test-mevedel-hooks)
;;; test-mevedel-hooks.el ends here
