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

(defvar mevedel--agent-invocation)


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

(mevedel-deftest mevedel-hooks--telemetry-handler-id
  (:doc "is stable for one handler and changes with its executable identity")
  (let ((handler '(:type command :source project :command "one")))
    (should (equal (mevedel-hooks--telemetry-handler-id handler)
                   (mevedel-hooks--telemetry-handler-id handler)))
    (should-not
     (equal (mevedel-hooks--telemetry-handler-id handler)
            (mevedel-hooks--telemetry-handler-id
             '(:type command :source project :command "two"))))))

(mevedel-deftest mevedel-hooks--run-handlers
  (:doc "emits a paired handler lifecycle span with context size")
  (let* ((root (make-temp-file "mevedel-hook-handler-span-" t))
         (session (mevedel-hooks-test--session root))
         starts finishes result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-telemetry-start)
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
           nil session nil (lambda (decision) (setq result decision)))
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
		 (:doc "reads Lisp hook files with read evaluation disabled")
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
			 (should-not
			  (assq 'PermissionRequest
				(mevedel-hooks-effective-rules session workspace)))
			 (mevedel-hooks-trust-project workspace)
			 (let ((rules (mevedel-hooks-effective-rules session workspace)))
			   (should (assq 'PreToolUse rules))
			   (should (assq 'PostToolUse rules))
			   (should (assq 'PermissionRequest rules))
			   (should (assq 'PermissionDenied rules)))
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
			 (mevedel-hooks-trust-project workspace)
			 (should-not
			  (assq 'PermissionDenied
				(mevedel-hooks-effective-rules session workspace))))
		     (delete-directory root t)
		     (delete-directory user-dir t))))

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
              (mapcar (lambda (handler)
                        (plist-get handler :command))
                      (mevedel-hooks--matching-handlers
                       'PreToolUse
                       '(:tool-name "Bash")
                       (mevedel-hooks-effective-rules session workspace)))))
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
            (mevedel-hooks-trust-project workspace)
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
  (:doc "ignores malformed trusted hook entries")
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
          (mevedel-hooks-trust-project workspace)
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
         :function mevedel-hooks-test--declarative-capture-fn))))))

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
          mevedel-hooks-test--native-rewrite-fn)))))

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

(mevedel-deftest mevedel-hooks-context-audit-records
  (:doc "attributes merged context to handlers in execution order")
  (let* ((root (make-temp-file "mevedel-hooks-context-audit" t))
         (session (mevedel-hooks-test--session root))
         (mevedel-hook-rules
          '((SubagentStart
             ((:matcher "explorer"
               :hooks ((:type elisp
                        :function mevedel-hooks-test--first-context-fn
                        :source plugin
                        :plugin-name "ponytail")
                       (:type elisp
                        :function mevedel-hooks-test--second-context-fn
                        :source project-file
                        :description "Inject project conventions"))))))))
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
  (:doc "a terminal native decision skips later declarative handlers")
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
  (:doc "queues model-visible reminders for blocking outcomes only")
  (let* ((root (make-temp-file "mevedel-hooks-reminders" t))
         (session (mevedel-hooks-test--session root)))
    (unwind-protect
        (let ((mevedel-pre-tool-use-functions
               '(mevedel-hooks-test--deny-fn)))
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'PreToolUse
              '(:tool-name "Bash" :tool-input (:command "echo hi"))
              cb session)))
          (let ((body (car (mevedel-session-pending-reminders session))))
            (should (string-match-p "PreToolUse hook blocked" body))
            (should (string-match-p "blocked" body))))
      (delete-directory root t)))
  (let* ((root (make-temp-file "mevedel-hooks-reminders" t))
         (session (mevedel-hooks-test--session root)))
    (unwind-protect
        (let ((mevedel-post-tool-use-functions
               '(mevedel-hooks-test--system-message-fn)))
          (mevedel-hooks-test--await
           (lambda (cb)
             (mevedel-hooks-run-event
              'PostToolUse
              '(:tool-name "Read" :tool-result "ok")
              cb session)))
          (should-not (mevedel-session-pending-reminders session)))
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
                         (:doc "caps command hook output before returning block reasons")
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
		 (:doc "stops later PermissionRequest hooks after fail-closed stop decisions")
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
		 (:doc "maps exit-code 2 to event-specific blocking decisions")
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

(mevedel-deftest mevedel-hooks-run-event/project-command-cwd
		 (:doc "runs project-sourced command hooks from the workspace root")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-hooks-project-cwd" t)))
			(subdir (file-name-as-directory
				 (file-name-concat root "subdir")))
			session
			(mevedel-hook-rules
			 `((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command
						      :command
						      ,(mevedel-hooks-test--emacs-command
							"(princ default-directory)")
						      :source project-file
						      :timeout 5))))))))
		   (unwind-protect
		       (progn
			 (make-directory subdir t)
			 (setq session
			       (mevedel-session-create
				"hooks-test"
				(mevedel-hooks-test--workspace root)
				subdir))
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

(mevedel-deftest mevedel-hooks-run-event/plugin-command-env
  (:doc "runs plugin command hooks with compatibility env and creates data dir")
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
		 (:doc "resumes later Elisp handlers in the original dispatch buffer")
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
		 (:doc "command failures fail open by default and fail closed when requested")
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

(provide 'test-mevedel-hooks)
;;; test-mevedel-hooks.el ends here
