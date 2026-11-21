;;; test-niwa-hooks.el -- Tests for hook execution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'niwa-hooks)
(require 'niwa-plugins)
(require 'niwa-agent-control)
(require 'niwa-execution-target)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-telemetry)
(require 'niwa-tool-registry)
(require 'niwa-view)
(require 'niwa-view-stream)
(require 'niwa-workspace)
(require 'tramp)

(defvar niwa--agent-invocation)
(defvar niwa-hooks-test--elisp-origin nil)


;;
;;; Helpers

(defun niwa-hooks-test--await (starter)
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

(defun niwa-hooks-test--workspace (root)
  "Return a fresh workspace for ROOT."
  (niwa-workspace-get-or-create
   'project (format "hooks-test:%s" root) root "hooks-test"))

(defun niwa-hooks-test--emacs-command (form)
  "Return a shell command running this Emacs in batch and evaluating FORM."
  (mapconcat #'shell-quote-argument
             (list (expand-file-name invocation-name invocation-directory)
                   "--quick" "--batch" "--eval" form)
             " "))

(defun niwa-hooks-test--session (root)
  "Return a session rooted at ROOT."
  (niwa-session-create
   "hooks-test" (niwa-hooks-test--workspace root) root))

(defun niwa-hooks-test--write-plugin-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun niwa-hooks-test--clear-plugin-env ()
  "Remove plugin compatibility variables from local `process-environment'."
  (dolist (name '("PLUGIN_ROOT"
                  "CLAUDE_PLUGIN_ROOT"
                  "PLUGIN_DATA"
                  "CLAUDE_PLUGIN_DATA"
                  "NIWA_PLUGIN_ROOT"
                  "NIWA_PLUGIN_DATA"))
    (setenv name nil)))

(defun niwa-hooks-test--deny-fn (_event)
  "Return a deny decision for hook cases."
  '(:permission-decision deny :permission-reason "blocked"))

(defun niwa-hooks-test--rewrite-fn (_event)
  "Return updated input for hook cases."
  '(:updated-input (:command "echo rewritten")))

(defun niwa-hooks-test--capture-elisp-origin (event)
  "Capture EVENT and local `default-directory' for origin tests."
  (setq niwa-hooks-test--elisp-origin
        (list :event event :default-directory default-directory))
  nil)

(niwa-deftest niwa-hooks--annotate-handlers
  (:doc "annotates every ordered handler without mutating rules or metadata")
  (let* ((rules '((PreToolUse
                  (:matcher "Read" :hooks ((:command "first" :source old)
                                           (:command "second"))))
                 (Stop (:hooks ((:command "last"))))))
         (before (copy-tree rules))
         (properties '(:source nil :source-file "hooks.el"))
         (annotated (niwa-hooks--annotate-handlers rules properties)))
    (should (equal '((PreToolUse
                      (:matcher "Read"
                       :hooks ((:command "first" :source nil :source-file "hooks.el")
                               (:command "second" :source nil :source-file "hooks.el"))))
                     (Stop (:hooks ((:command "last" :source nil
                                     :source-file "hooks.el")))))
                   annotated))
    (should (equal rules before))
    (should (equal '(:source nil :source-file "hooks.el") properties))
    (should-not (niwa-hooks--annotate-handlers nil properties))))

(niwa-deftest niwa-hooks-annotate-rules-source
  (:doc "copies rule containers without replacing omitted provenance or callbacks")
  (let* ((callback (lambda (_event) nil))
         (rules `((PreToolUse
                   (:matcher "Read" :label "keep"
                    :hooks ((:type elisp :function ,callback :source old
                             :source-file "original.el" :source-root "/original/")))
                   (:matcher "empty" :hooks nil)
                   (:matcher "absent"))
                  (Stop)))
         (before (copy-tree rules))
         (annotated (niwa-hooks-annotate-rules-source rules 'user))
         (group (cadar annotated))
         (handler (car (plist-get group :hooks))))
    (should (equal rules before))
    (should (equal '(PreToolUse Stop) (mapcar #'car annotated)))
    (should (equal (cddar rules) (cddar annotated)))
    (should (eq callback (plist-get handler :function)))
    (should (eq 'user (plist-get handler :source)))
    (should (equal "original.el" (plist-get handler :source-file)))
    (should (equal "/original/" (plist-get handler :source-root)))
    (setf (plist-get group :label) "changed"
          (plist-get handler :source-root) "/changed/")
    (should (equal rules before))
    (let* ((replaced (niwa-hooks-annotate-rules-source
                      rules 'project-file "project.el" "/project/"))
           (handler (car (plist-get (cadar replaced) :hooks))))
      (should (equal "project.el" (plist-get handler :source-file)))
      (should (equal "/project/" (plist-get handler :source-root)))
      (should (equal rules before)))))

(niwa-deftest niwa-hooks--annotate-plugin-rules
  (:doc "replaces plugin authority while retaining manifest provenance")
  (let* ((root (make-temp-file "niwa-hooks-annotation" t))
         (workspace (niwa-hooks-test--workspace root))
         (plugin (niwa-plugin--create :name "demo" :root nil))
         (rules '((SessionStart
                   (:matcher "startup"
                    :hooks ((:type command :command "true"
                             :source user-file :source-file "hooks.json"
                             :source-root "/manifest/" :plugin-root "/old/"
                             :plugin-name "old" :plugin-data "/old/data/"))))))
         (before (copy-tree rules)))
    (unwind-protect
        (let* ((annotated (niwa-hooks--annotate-plugin-rules
                          rules plugin workspace))
               (handler (car (plist-get (cadar annotated) :hooks))))
          (should (equal rules before))
          (should (eq 'plugin (plist-get handler :source)))
          (should (equal "demo" (plist-get handler :plugin-name)))
          (should (plist-member handler :plugin-root))
          (should-not (plist-get handler :plugin-root))
          (should (equal (file-name-concat root ".niwa/state/plugin-data/demo")
                         (plist-get handler :plugin-data)))
          (should (equal "hooks.json" (plist-get handler :source-file)))
          (should (equal "/manifest/" (plist-get handler :source-root)))
          (setf (plist-get handler :command) "changed")
          (should (equal rules before)))
      (delete-directory root t))))

(niwa-deftest niwa-hooks--matcher-target
  (:doc "dispatch and inspection select the documented field for every event")
  (let* ((payload '(:tool-name "Read" :role "worker" :trigger "manual"
                   :source "resume" :reason "close" nil "not-a-target"))
         (root (make-temp-file "niwa-hooks-targets" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-context-frozen-p t))
    (unwind-protect
        (dolist (case '((PreToolUse :tool-name "Read")
                        (PermissionRequest :tool-name "Read")
                        (PermissionDenied :tool-name "Read")
                        (PostToolUse :tool-name "Read")
                        (PostToolUseFailure :tool-name "Read")
                        (SubagentStart :role "worker")
                        (SubagentStop :role "worker")
                        (PreCompact :trigger "manual")
                        (PostCompact :trigger "manual")
                        (SessionStart :source "resume")
                        (SessionEnd :reason "close")
                        (UserPromptSubmit nil nil)
                        (UserPromptExpansion nil nil)
                        (Stop nil nil) (StopFailure nil nil)))
          (pcase-let ((`(,event ,key ,target) case))
            (let (niwa-hooks-test--elisp-origin finished)
              (setf (niwa-session-hook-rules session)
                    `((,event
                       (:matcher ,(or target "*")
                        :hooks ((:type elisp
                                 :function
                                 niwa-hooks-test--capture-elisp-origin))))))
              (should (eq key (niwa-hooks--target-key-for-event event)))
              (should (equal target (niwa-hooks--matcher-target event payload)))
              (let ((dry (niwa-hooks-run-dry event payload session)))
                (should (= 1 (plist-get dry :handler-count)))
                (should (equal target (plist-get dry :matcher-target))))
              (should-not niwa-hooks-test--elisp-origin)
              (niwa-hooks-run-event
               event payload (lambda (_decision) (setq finished t)) session)
              (should (eq event (plist-get
                                 (plist-get niwa-hooks-test--elisp-origin :event)
                                 :hook-event-name)))
              (should finished))))
      (delete-directory root t))
    (should-not (niwa-hooks--matcher-target 'Unknown payload))))

(niwa-deftest niwa-hooks-tool-event-plist
  (:doc "carries nested tool identity and source into hook payloads")
  (let* ((tool (niwa-tool--create :name "Child"))
         (event
          (niwa-hooks-tool-event-plist
           'PreToolUse
           (list :tool tool :args '(:value "x")
                 :tool-use-id "parent/1"
                 :parent-tool-use-id "parent"
                 :call-source 'ptc))))
    (should (equal "parent/1" (plist-get event :tool-use-id)))
    (should (equal "parent" (plist-get event :parent-tool-use-id)))
    (should (eq 'ptc (plist-get event :call-source)))))

(niwa-deftest niwa-hooks--telemetry-handler-id
  (:doc "is stable for one handler and changes with its executable identity")
  (let ((handler '(:type command :source project :command "one")))
    (should (equal (niwa-hooks--telemetry-handler-id handler)
                   (niwa-hooks--telemetry-handler-id handler)))
    (should-not
     (equal (niwa-hooks--telemetry-handler-id handler)
            (niwa-hooks--telemetry-handler-id
             '(:type command :source project :command "two"))))))

(niwa-deftest niwa-hooks--command-event-plist
  (:doc "converts every command-hook filesystem fact to target-native paths")
  (let* ((target
          (niwa-execution-target-create "/ssh:hook-target:/workspace/"))
         (session
          (niwa-session--create :name "main" :execution-target target))
         (payload
          (niwa-hooks--command-event-plist
           '(:cwd "/ssh:hook-target:/workspace/src/"
             :workspace-root "/ssh:hook-target:/workspace/"
             :transcript-path
             "/ssh:hook-target:/workspace/.niwa/sessions/main/"
             :tool-input
             (:file_path "/ssh:hook-target:/workspace/src/a.el"
              :paths ("/ssh:hook-target:/workspace/a.el"
                      "/ssh:hook-target:/workspace/b.el")
              :command "echo /ssh:hook-target:/workspace/a.el"))
           session)))
    (should (equal "/workspace/src/" (plist-get payload :cwd)))
    (should (equal "/workspace/" (plist-get payload :workspace-root)))
    (should
     (equal "/workspace/.niwa/sessions/main/"
            (plist-get payload :transcript-path)))
    (should
     (equal '(:file_path "/workspace/src/a.el"
              :paths ("/workspace/a.el" "/workspace/b.el")
              :command "echo /ssh:hook-target:/workspace/a.el")
            (plist-get payload :tool-input)))))

(niwa-deftest niwa-hooks--command-default-directory
  (:doc "selects plugin conversation cwd without changing execution ownership")
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-hooks-plugin-cwd" t)))
         (subdir (file-name-concat root "conversation/"))
         (event-dir (file-name-concat root "event/"))
         (plugin-root (file-name-concat root "plugin/"))
         (handler (list :source 'plugin :plugin-root plugin-root))
         (session (niwa-session--create
                   :name "cwd" :working-directory subdir
                   :execution-target (niwa-execution-target-create root))))
    (unwind-protect
        (progn
          (should (equal event-dir
                         (niwa-hooks--command-default-directory
                          handler (list :cwd event-dir :workspace-root root)
                          session)))
          (should (equal subdir
                         (niwa-hooks--command-default-directory
                          handler (list :workspace-root root) session)))
          (should (equal root
                         (niwa-hooks--command-default-directory
                          handler (list :workspace-root root) nil)))
          (should (equal plugin-root
                         (niwa-hooks--command-default-directory
                          handler nil nil)))
          (should (equal event-dir
                         (niwa-hooks--command-default-directory
                          (list :source 'plugin :source-root plugin-root)
                          (list :cwd event-dir) session)))
          (let* ((remote-root (format "/niwamock:plugin-cwd:%s" root))
                 (remote-subdir (file-name-concat remote-root "conversation/"))
                 (remote-plugin (file-name-concat remote-root "plugin/"))
                 (remote-handler (list :source 'plugin :plugin-root remote-plugin))
                 (remote-session
                  (niwa-session--create
                   :name "remote-cwd" :working-directory remote-subdir
                   :execution-target
                   (niwa-execution-target-create remote-root))))
            (should (equal remote-subdir
                           (niwa-hooks--command-default-directory
                            remote-handler (list :cwd remote-subdir)
                            remote-session)))
            (should (equal remote-subdir
                           (niwa-hooks--command-default-directory
                            remote-handler nil remote-session)))
            (should (equal remote-subdir
                           (niwa-hooks--command-default-directory
                            remote-handler (list :cwd event-dir)
                            remote-session)))
            (should (equal plugin-root
                           (niwa-hooks--command-default-directory
                            handler (list :cwd remote-subdir) remote-session)))
            (should-error
             (niwa-hooks--command-default-directory
              remote-handler (list :cwd event-dir) session)
             :type 'niwa-execution-target-error)))
      (delete-directory root t))))

(niwa-deftest niwa-hooks--command-process-environment
  (:doc "keeps client variables local while adding deliberate target plugin variables")
  (let* ((process-environment
          (cons "NIWA_CLIENT_ONLY=secret"
                (copy-sequence process-environment)))
         (local-root (make-temp-file "niwa-hook-environment-" t))
         (remote-root (format "/niwamock:%s:%s/" (system-name) local-root))
         (plugin-root (file-name-concat remote-root "plugin/"))
         (plugin-data
          (file-name-concat remote-root ".niwa/state/plugin-data/demo/"))
         (target (niwa-execution-target-create remote-root))
         (session
          (niwa-session--create :name "main" :execution-target target)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (should-not
           (niwa-hooks--command-process-environment
            '(:type command :source project-file) session))
          (should
           (eq process-environment
               (niwa-hooks--command-process-environment
                '(:type command :source user-file) session)))
          (let ((environment
                 (niwa-hooks--command-process-environment
                  (list :type 'command :source 'plugin
                        :plugin-root plugin-root :plugin-data plugin-data)
                  session)))
            (should-not (member "NIWA_CLIENT_ONLY=secret" environment))
            (should
             (member (concat "NIWA_PLUGIN_ROOT=" local-root "/plugin/")
                     environment))
            (should
             (member
              (concat "NIWA_PLUGIN_DATA=" local-root
                      "/.niwa/state/plugin-data/demo/")
              environment))
            (should (file-directory-p plugin-data))))
      (delete-directory local-root t))))

(niwa-deftest niwa-hooks--persist-log-content
  (:doc "creates missing parents once and appends UTF-8 independent of the caller")
  (let* ((root (make-temp-file "niwa-hook-append-" t))
         (session (niwa-session--create
                   :save-path (file-name-concat root "nested" "session")))
         (niwa-hooks-persist-log t)
         (content "(:reason \"\u2603\")\n"))
    (unwind-protect
        (let ((coding-system-for-write 'utf-16le))
          (should (niwa-hooks--persist-log-content session content))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest _) (ert-fail "Recreated existing directory"))))
            (should (niwa-hooks--persist-log-content session content)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally (niwa-hooks-log-path session))
            (should (equal (encode-coding-string (concat content content) 'utf-8-unix)
                           (buffer-string)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks--log
  ()
  ,test
  (test)

  :doc "keeps detailed side logs transient and forwards value-free audit"
  (let* ((parent (niwa-session--create :name "parent"))
         (side (niwa-session--create
                :name "side" :audit-session parent))
         (entry '(:event PreToolUse
                  :handler (:type command :source project
                            :command "private-command")
                  :status allowed :elapsed 0.25 :exit-status 0
                  :stdout "private-output" :reason "private-reason"))
         calls)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (session event &rest props)
                 (push (list session event props) calls))))
      (niwa-hooks--log side entry))
    (should (equal (list entry) (niwa-session-hook-log side)))
    (should-not (niwa-session-hook-log parent))
    (pcase-let ((`((,session ,event ,props)) calls))
      (should (eq parent session))
      (should (eq 'hook-handler event))
      (should (eq 'btw (plist-get props :conversation-scope)))
      (should-not
       (string-match-p
        (regexp-opt '("private-command" "private-output" "private-reason"))
        (prin1-to-string props)))))
  :doc "a durable session repeats its hook log in telemetry only while profiled"
  (let* ((session (niwa-session--create :name "main"))
         (entry '(:event PostToolUse
                  :handler (:type command :source project :command "check")
                  :status allowed :elapsed 0.01 :exit-status 0))
         (niwa-hooks-persist-log t)
         (profiled nil)
         calls)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (_session event &rest _props) (push event calls)))
              ((symbol-function 'niwa-telemetry-detailed-p)
               (lambda (candidate) (and profiled (eq candidate session)))))
      (niwa-hooks--log session entry)
      (should-not calls)
      (setq profiled t)
      (niwa-hooks--log session entry)
      (setq profiled nil)
      ;; Without a persisted hook log, telemetry is the durable record.
      (let ((niwa-hooks-persist-log nil))
        (niwa-hooks--log session entry)))
    (should (equal '(hook-handler hook-handler) calls))
    (should (= 3 (length (niwa-session-hook-log session)))))
  :doc "defers a materialized remote append until session settlement"
  (let* ((root (make-temp-file "niwa-hook-log-remote-" t))
         (target
          (niwa-execution-target-create "/ssh:hook-host:/workspace/"))
         (session
          (niwa-session--create
           :name "main" :execution-target target :save-path root))
         (entry '(:event Stop :status completed))
         (later-entry '(:event SessionEnd :status completed))
         (niwa-hooks-persist-log t)
         calls)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function
                 'niwa-session-publication-append-diagnostic)
                (lambda (_session path content)
                  (push (list path content) calls)
                  t)))
            (niwa-hooks--log session entry)
            (niwa-hooks--log session later-entry)
            (should (equal (list entry later-entry)
                           (niwa-session-hook-log-pending session)))
            (should-not calls)
            (should-not (file-exists-p (niwa-hooks-log-path session)))
            (niwa-hooks-flush-log session))
          (should-not (niwa-session-hook-log-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should (equal (niwa-hooks-log-path session) path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (equal entry (read (current-buffer))))
              (should (equal later-entry (read (current-buffer)))))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-flush-log
  (:doc "retains a failed append and persists it on a later flush")
  (let* ((root (make-temp-file "niwa-hook-log-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (niwa-session--create :name "main"))
         (entry '(:event Stop :status completed))
         (later-entry '(:event SessionEnd :status completed))
         (niwa-hooks-persist-log t)
         warning)
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (niwa-session-save-path session) blocked)
          (niwa-test--with-captured-diagnostics warning
            (niwa-hooks--log session entry))
          (should (string-match-p "persistence failed" warning))
          (should (equal (list entry)
                         (niwa-session-hook-log-pending session)))
          (setf (niwa-session-save-path session) restored)
          (niwa-hooks--log session later-entry)
          (should (equal (list entry later-entry)
                         (niwa-session-hook-log-pending session)))
          (niwa-hooks-flush-log session)
          (should-not (niwa-session-hook-log-pending session))
          (with-temp-buffer
            (insert-file-contents (niwa-hooks-log-path session))
            (should (equal entry (read (current-buffer))))
            (should (equal later-entry (read (current-buffer))))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks--run-handlers
  (:doc "emits a paired handler lifecycle span with context size")
  (let* ((root (make-temp-file "niwa-hook-handler-span-" t))
         (session (niwa-hooks-test--session root))
         starts finishes result)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-telemetry-detailed-p)
                   (lambda (_session) t))
                  ((symbol-function 'niwa-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) starts)
                     '(:span handler)))
                  ((symbol-function 'niwa-telemetry-finish)
                   (lambda (_span &rest props) (push props finishes)))
                  ((symbol-function 'niwa-telemetry-record)
                   (lambda (&rest _))))
          (niwa-hooks--run-handlers
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

(defvar niwa-hooks-test--read-eval-ran nil)
(defvar niwa-hooks-test--native-seen-event nil)
(defvar niwa-hooks-test--seen-event nil)
(defvar niwa-hooks-test--seen-buffer nil)
(defvar niwa-hooks-test--execution-order nil)

(defun niwa-hooks-test--capture-fn (event)
  "Capture EVENT for serial hook cases."
  (setq niwa-hooks-test--seen-event event)
  nil)

(defun niwa-hooks-test--native-rewrite-fn (event)
  "Record native execution and rewrite input for unified-engine cases."
  (setq niwa-hooks-test--native-seen-event event)
  (setq niwa-hooks-test--execution-order
        (append niwa-hooks-test--execution-order '(native)))
  '(:updated-input (:command "native rewrite")))

(defun niwa-hooks-test--declarative-capture-fn (event)
  "Record declarative execution and capture EVENT for unified-engine cases."
  (setq niwa-hooks-test--execution-order
        (append niwa-hooks-test--execution-order '(declarative)))
  (setq niwa-hooks-test--seen-event event)
  nil)

(defun niwa-hooks-test--suppress-output-fn (_event)
  "Return a reserved decision field for native hook cases."
  '(:suppress-output t))

(defun niwa-hooks-test--stop-fn (_event)
  "Return a stop decision for terminal-behavior cases."
  '(:continue nil :stop-reason "stop"))

(defun niwa-hooks-test--malformed-fn (_event)
  "Return a malformed decision for hook boundary cases."
  :args)

(defun niwa-hooks-test--malformed-file-symbol-fn (_event)
  "Return a malformed non-keyword symbol decision."
  (intern "test-niwa-view.el."))

(defun niwa-hooks-test--context-fn (_event)
  "Return additional context for terminal-behavior cases."
  '(:additional-context ("later")))

(defun niwa-hooks-test--first-context-fn (_event)
  "Return the first attributed context contribution."
  '(:system-message "first reason"
    :additional-context ("first")))

(defun niwa-hooks-test--second-context-fn (_event)
  "Return the second attributed context contribution."
  '(:additional-context ("second-a" "second-b")))

(defun niwa-hooks-test--allow-fn (_event)
  "Return an allow decision for permission hook cases."
  '(:permission-decision allow))

(defun niwa-hooks-test--system-message-fn (_event)
  "Return a system-message decision for reminder cases."
  '(:system-message "remember the project rule"))

(defun niwa-hooks-test--buffer-fn (_event)
  "Capture the current buffer for async continuation cases."
  (setq niwa-hooks-test--seen-buffer (current-buffer))
  nil)


;;
;;; Config and matching

(niwa-deftest niwa-hooks-normalize-rules
  (:doc "normalizes Lisp hook rules and drops invalid handlers")
  (progn
    (should
     (equal
      (niwa-hooks-normalize-rules
       '((PreToolUse
          ((:matcher "Bash"
            :hooks ((:type command :command "echo ok")
                    (:type nope :command "ignored")
                    (:type elisp :function niwa-hooks-test--deny-fn)))))
         (NoSuchEvent
          ((:matcher "*" :hooks ((:type command :command "ignored")))))))
      '((PreToolUse
         (:matcher "Bash"
          :hooks ((:type command :command "echo ok")
                  (:type elisp :function niwa-hooks-test--deny-fn)))))))
    (should
     (equal
      (niwa-hooks-normalize-rules
       '((UserPromptSubmit
          ((:hooks ((:type command :command "echo wildcard")))))))
      '((UserPromptSubmit
         (:matcher "*"
          :hooks ((:type command :command "echo wildcard")))))))))

(niwa-deftest niwa-hooks-normalize-rules/malformed
  (:doc "drops malformed top-level rule values")
  (should-not (niwa-hooks-normalize-rules
               (intern "test-niwa-view.el."))))

(niwa-deftest niwa-hooks-normalize-rules/invalid-matcher
  (:doc "drops a group whose matcher is not a usable regexp")
  (let ((niwa--warn-once-table (make-hash-table :test #'equal))
        warning)
    (niwa-test--with-captured-diagnostics warning
      (should-not
       (niwa-hooks-normalize-rules
        '((UserPromptSubmit
           ((:matcher "["
             :hooks ((:type elisp
                      :function niwa-hooks-test--context-fn))))))))
      (should
       (equal
        (niwa-hooks-normalize-rules
         '((UserPromptSubmit
            ((:matcher "["
              :hooks ((:type elisp
                       :function niwa-hooks-test--context-fn)))
             (:matcher "*"
              :hooks ((:type elisp
                       :function niwa-hooks-test--context-fn)))))))
        '((UserPromptSubmit
           (:matcher "*"
            :hooks ((:type elisp
                     :function niwa-hooks-test--context-fn))))))))
    (should (string-match-p "matcher" warning))
    (should (string-match-p "UserPromptSubmit" warning))))

(niwa-deftest niwa-hooks-normalize-rules/scoped-stop
  (:doc "normalizes agent-scoped Stop to SubagentStop")
  (should
   (equal
    (niwa-hooks-normalize-rules
     '((Stop
        ((:matcher "*"
          :hooks ((:type elisp
                   :function niwa-hooks-test--context-fn))))))
     'agent)
    '((SubagentStop
       (:matcher "*"
        :hooks ((:type elisp
                 :function niwa-hooks-test--context-fn))))))))

(niwa-deftest niwa-hooks--read-json-file
		 (:doc "reads Claude/Codex-style JSON hook config")
		 (let ((file (make-temp-file "niwa-hooks" nil ".json")))
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
			   (niwa-hooks-read-config-file file)
			   '((PreToolUse
			      (:matcher "Bash"
					:hooks ((:type command
						       :command "echo ok"
						       :fail-closed t))))))))
		     (delete-file file))))

(niwa-deftest niwa-hooks--read-lisp-file
		 (:quiet t :doc "reads Lisp hook files with read evaluation disabled")
		 (let ((file (make-temp-file "niwa-hooks" nil ".el"))
		       (niwa-hooks-test--read-eval-ran nil))
		   (unwind-protect
		       (progn
			 (with-temp-file file
			   (insert "#.(progn "
				   "(setq niwa-hooks-test--read-eval-ran t) "
				   "nil)"))
			 (should-not (niwa-hooks-read-config-file file))
			 (should-not niwa-hooks-test--read-eval-ran))
		     (delete-file file))))

(niwa-deftest niwa-hooks-effective-rules
		 (:doc "merges defcustom, user el/json, and trusted project el/json layers")
		 (let* ((root (make-temp-file "niwa-hooks-ws" t))
			(user-dir (make-temp-file "niwa-hooks-user" t))
			(workspace (niwa-hooks-test--workspace root))
			(session (niwa-session-create "main" workspace root))
			(niwa-user-dir (file-name-as-directory user-dir))
			(niwa-hooks-require-project-trust t)
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-concat root ".niwa") t)
			 (make-directory user-dir t)
			 (with-temp-file (file-name-concat user-dir "hooks.el")
			   (prin1
			    '((PreToolUse
			       ((:matcher "Bash"
					  :hooks ((:type elisp
							 :function niwa-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat user-dir "hooks.json")
			   (insert "{\"hooks\":{\"PostToolUse\":[{\"matcher\":\"Bash\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo ok\"}]}]}}"))
			 (with-temp-file (file-name-concat root ".niwa" "hooks.el")
			   (prin1
			    '((PermissionRequest
			       ((:matcher "*"
					  :hooks ((:type elisp
							 :function niwa-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (with-temp-file (file-name-concat root ".niwa" "hooks.json")
			   (insert "{\"hooks\":{\"PermissionDenied\":[{\"matcher\":\"*\","
				   "\"hooks\":[{\"type\":\"command\",\"command\":\"echo deny\"}]}]}}"))
			 ;; Untrusted project configs are reported as they are
			 ;; skipped, which is the behavior under test here.
			 (let (diagnostics)
			   (niwa-test--with-captured-diagnostics diagnostics
			     (should-not
			      (assq 'PermissionRequest
				    (niwa-hooks-effective-rules
				     session workspace))))
			   (should (string-match-p "is not trusted" diagnostics)))
			 (niwa-test--with-captured-diagnostics nil
			   (niwa-hooks-trust-project workspace))
			 (let* ((rules (niwa-hooks-effective-rules session workspace))
				(defcustom-handler
				 (car (niwa-hooks--matching-handlers
				       'PreToolUse '(:tool-name "Read") rules)))
				(user-handler
				 (car (niwa-hooks--matching-handlers
				       'PostToolUse '(:tool-name "Bash") rules)))
				(project-handler
				 (car (niwa-hooks--matching-handlers
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
			    (equal (file-name-concat root ".niwa" "hooks.json")
				   (plist-get project-handler :source-file)))
			   (should
			    (equal (file-name-as-directory root)
				   (plist-get project-handler :source-root))))
			 (with-temp-file (file-name-concat root ".niwa" "hooks.el")
			   (prin1
			    '((SessionStart
			       ((:matcher "startup"
					  :hooks ((:type elisp
							 :function niwa-hooks-test--deny-fn))))))
			    (current-buffer)))
			 (should-not
			  (assq 'SessionStart
				(niwa-hooks-effective-rules session workspace)))
			 (delete-file (file-name-concat root ".niwa" "hooks.json"))
			 (niwa-test--with-captured-messages nil
			   (niwa-hooks-trust-project workspace))
			 (should-not
			  (assq 'PermissionDenied
				(niwa-hooks-effective-rules session workspace))))
		     (delete-directory root t)
		     (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-project-file-snapshot ()
  ,test
  (test)
  :doc "binds project skill trust to workspace, path, and exact content"
  (let* ((root (make-temp-file "niwa-hook-skill-root-" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hook-skill-user-" t)))
         (workspace (niwa-hooks-test--workspace root))
         (niwa-user-dir user-dir)
         (niwa-hooks-require-project-trust t)
         messages
         (file (file-name-concat root ".niwa" "skills" "demo"
                                 "SKILL.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "---\ndescription: Demo\n---\nBody"))
          (let ((snapshot
                 (niwa-hooks-project-file-snapshot workspace file)))
            (should-not (plist-get snapshot :trusted-p))
            (should (string-match-p "description: Demo"
                                    (plist-get snapshot :content))))
          (niwa-test--with-captured-messages messages
            (niwa-hooks-trust-project workspace))
          (should (equal "niwa: trusted 1 project file(s)\n" messages))
          (should
           (plist-get (niwa-hooks-project-file-snapshot workspace file)
                      :trusted-p))
          (with-temp-file file
            (insert "---\ndescription: Changed\n---\nBody"))
          (should-not
           (plist-get (niwa-hooks-project-file-snapshot workspace file)
                      :trusted-p)))
      (clrhash niwa-hooks--config-rules-cache)
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks--config-rules
  ()
  ,test
  (test)
  :doc "memoizes the configured layers until an explicit invalidation"
  (let* ((root (make-temp-file "niwa-hooks-cache-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-cache-user" t)))
         (workspace (niwa-hooks-test--workspace root))
         (niwa-user-dir user-dir)
         (niwa-hooks-require-project-trust t)
         (reads 0)
         (project-snapshots-function
          (symbol-function 'niwa-hooks--project-config-snapshots)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".niwa") t)
          (with-temp-file (file-name-concat root ".niwa" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "Bash"
                  :hooks ((:type command :command "echo project"))))))
             (current-buffer)))
          (niwa-test--with-captured-messages nil
            (niwa-hooks-trust-project workspace))
          (cl-letf (((symbol-function
                      'niwa-hooks--project-config-snapshots)
                     (lambda (ws)
                       (cl-incf reads)
                       (funcall project-snapshots-function ws))))
            (should (= 1 (length (niwa-hooks--matching-handlers
                                  'PreToolUse '(:tool-name "Bash")
                                  (niwa-hooks-effective-rules
                                   nil workspace)))))
            (niwa-hooks-effective-rules nil workspace)
            (should (= 1 reads))
            (niwa-test--with-captured-messages nil
              (niwa-hooks-reload))
            (niwa-hooks-effective-rules nil workspace)
            (should (= 2 reads))
            ;; Trusting the project invalidates too.
            (niwa-test--with-captured-messages nil
              (niwa-hooks-trust-project workspace))
            (niwa-hooks-effective-rules nil workspace)
            (should (= 3 reads))))
      (clrhash niwa-hooks--config-rules-cache)
      (delete-directory root t)
      (when (file-directory-p user-dir)
        (delete-directory user-dir t))))

  :doc "parses the exact project bytes whose hash was trusted"
  (let* ((root (make-temp-file "niwa-hooks-snapshot-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-snapshot-user" t)))
         (workspace (niwa-hooks-test--workspace root))
         (niwa-user-dir user-dir)
         (niwa-hooks-require-project-trust t)
         (file (file-name-concat root ".niwa" "hooks.el"))
         (replacement (file-name-concat root ".niwa" "replacement.el"))
         (project-snapshots-function
          (symbol-function 'niwa-hooks--project-config-snapshots)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert ";;; -*- coding: utf-8 -*-\n")
            (prin1
             '((PreToolUse
                ((:matcher "Bash"
                  :hooks ((:type command :command "echo tr\u00fcsted"))))))
             (current-buffer)))
          (niwa-test--with-captured-messages nil
            (niwa-hooks-trust-project workspace))
          (cl-letf (((symbol-function
                      'niwa-hooks--project-config-snapshots)
                     (lambda (ws)
                       (let ((snapshots
                              (funcall project-snapshots-function ws)))
                         (with-temp-file replacement
                           (prin1
                            '((PreToolUse
                               ((:matcher "Bash"
                                 :hooks
                                 ((:type command
                                   :command "echo replacement"))))))
                            (current-buffer)))
                         (rename-file replacement file t)
                         snapshots))))
            (should
             (equal
              '("echo tr\u00fcsted")
              (mapcar
               (lambda (handler) (plist-get handler :command))
               (niwa-hooks--matching-handlers
                'PreToolUse '(:tool-name "Bash")
                (niwa-hooks-effective-rules nil workspace)))))))
      (clrhash niwa-hooks--config-rules-cache)
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-effective-rules/agents-hook-roots
  (:doc "loads standalone agents hook roots in precedence order and trusts project agents files")
  (let* ((root (make-temp-file "niwa-hooks-agents-ws" t))
         (home (make-temp-file "niwa-hooks-agents-home" t))
         (user-dir (file-name-as-directory
                    (file-name-concat home ".niwa")))
         (workspace (niwa-hooks-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat home ".agents" "plugins"))
         (niwa-hooks-require-project-trust t)
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
              (niwa-test--with-captured-diagnostics nil
                (mapcar (lambda (handler)
                          (plist-get handler :command))
                        (niwa-hooks--matching-handlers
                         'PreToolUse
                         '(:tool-name "Bash")
                         (niwa-hooks-effective-rules
                          session workspace))))))
          (setenv "HOME" home)
          (let ((global-agents (file-name-concat home ".agents"))
                (global-niwa user-dir)
                (project-agents (file-name-concat root ".agents"))
                (project-niwa (file-name-concat root ".niwa"))
                (plugin-root (file-name-as-directory
                              (file-name-concat
                               niwa-plugin-install-directory "repo"))))
            (write-el global-agents "echo global-agents-el")
            (write-json global-agents "echo global-agents-json")
            (write-el global-niwa "echo global-niwa-el")
            (write-json global-niwa "echo global-niwa-json")
            (make-directory (file-name-concat plugin-root "hooks") t)
            (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo plugin\"}]}]}}"))
            (niwa-hooks-test--write-plugin-manifest
             plugin-root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (niwa-plugins-enable "demo" workspace))
            (write-el project-agents "echo project-agents-el")
            (write-json project-agents "echo project-agents-json")
            (write-el project-niwa "echo project-niwa-el")
            (write-json project-niwa "echo project-niwa-json")
            (should
             (equal '("echo global-agents-el"
                      "echo global-agents-json"
                      "echo global-niwa-el"
                      "echo global-niwa-json"
                      "echo plugin")
                    (commands)))
            (niwa-test--with-captured-messages nil
              (niwa-hooks-trust-project workspace))
            (should
             (equal '("echo global-agents-el"
                      "echo global-agents-json"
                      "echo global-niwa-el"
                      "echo global-niwa-json"
                      "echo plugin"
                      "echo project-agents-el"
                      "echo project-agents-json"
                      "echo project-niwa-el"
                      "echo project-niwa-json")
                    (commands)))))
      (delete-directory root t)
      (delete-directory home t))))

(niwa-deftest niwa-hooks-effective-rules/malformed-trust-db
  (:quiet t :doc "ignores malformed trusted hook entries")
  (let* ((root (make-temp-file "niwa-hooks-trust" t))
         (user-dir (make-temp-file "niwa-hooks-user" t))
         (workspace (niwa-hooks-test--workspace root))
         (niwa-user-dir (file-name-as-directory user-dir))
         (niwa-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".niwa") t)
          (make-directory user-dir t)
          (with-temp-file (file-name-concat root ".niwa" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "*"
                  :hooks ((:type elisp
                           :function niwa-hooks-test--deny-fn))))))
             (current-buffer)))
          (with-temp-file (file-name-concat user-dir "trusted-hooks.el")
            (prin1 (list (intern "test-niwa-view.el."))
                   (current-buffer)))
          (should-not (assq 'PreToolUse
                            (niwa-hooks-effective-rules nil workspace))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-effective-rules/plugin-hooks
  (:doc "loads enabled plugin hook files with metadata between user and project layers")
  (let* ((root (make-temp-file "niwa-hooks-plugin-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-plugin-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (workspace (niwa-hooks-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory user-dir t)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (make-directory (file-name-concat root ".niwa") t)
          (with-temp-file (file-name-concat user-dir "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":\"echo user\"}]}]}}"))
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":\"echo plugin\","
                    "\"timeout\":7,\"statusMessage\":\"plugin status\"}]}]}}"))
          (niwa-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (with-temp-file (file-name-concat root ".niwa" "hooks.el")
            (prin1
             '((PreToolUse
                ((:matcher "Bash"
                  :hooks ((:type command :command "echo project"))))))
             (current-buffer)))
          (niwa-test--with-captured-messages nil
            (niwa-hooks-trust-project workspace))
          (let ((handlers (niwa-hooks--matching-handlers
                           'PreToolUse
                           '(:tool-name "Bash")
                           (niwa-hooks-effective-rules
                            session workspace))))
            (should (equal '(user-file project-file)
                           (mapcar (lambda (handler)
                                     (plist-get handler :source))
                                   handlers))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (niwa-plugins-enable "demo" workspace))
          (let* ((handlers (niwa-hooks--matching-handlers
                            'PreToolUse
                            '(:tool-name "Bash")
                            (niwa-hooks-effective-rules
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
            (should (equal (file-name-concat root ".niwa"
                                             "state" "plugin-data" "demo")
                           (plist-get plugin-handler :plugin-data)))
            (should (= 7 (plist-get plugin-handler :timeout)))
            (should (equal "plugin status"
                           (plist-get plugin-handler :status-message))))
          (niwa-plugins-disable-hooks "demo" workspace)
          (let ((handlers (niwa-hooks--matching-handlers
                           'PreToolUse
                           '(:tool-name "Bash")
                           (niwa-hooks-effective-rules
                            session workspace))))
            (should (equal '(user-file project-file)
                           (mapcar (lambda (handler)
                                     (plist-get handler :source))
                                   handlers)))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-effective-rules/plugin-hook-manifest-shapes
  (:doc "loads default and explicit string plugin hook files")
  (let* ((root (make-temp-file "niwa-hooks-plugin-shapes-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-plugin-shapes-user" t)))
         (workspace (niwa-hooks-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins")))
    (unwind-protect
        (progn
          (let ((default-root (file-name-as-directory
                               (file-name-concat
                                niwa-plugin-install-directory "default"))))
            (make-directory (file-name-concat default-root "hooks") t)
            (with-temp-file (file-name-concat default-root "hooks" "hooks.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo default\"}]}]}}"))
            (niwa-hooks-test--write-plugin-manifest
             default-root "{\"name\":\"default\"}")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) t)))
              (niwa-plugins-enable "default" workspace)))
          (let ((path-root (file-name-as-directory
                            (file-name-concat
                             niwa-plugin-install-directory "path"))))
            (make-directory (file-name-concat path-root "hooks") t)
            (with-temp-file (file-name-concat path-root "hooks" "a.json")
              (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                      "\"hooks\":[{\"type\":\"command\","
                      "\"command\":\"echo path-a\"}]}]}}"))
            (niwa-hooks-test--write-plugin-manifest
             path-root
             "{\"name\":\"path\",\"hooks\":\"./hooks/a.json\"}")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) t)))
              (niwa-plugins-enable "path" workspace)))
          (let* ((handlers (niwa-hooks--matching-handlers
                            'PreToolUse
                            '(:tool-name "Bash")
                            (niwa-hooks-effective-rules
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

(niwa-deftest niwa-hooks-effective-rules/superpowers-manifest-hooks
  (:doc "uses manifest hooks for plugin named superpowers")
  (let* ((root (make-temp-file "niwa-hooks-superpowers-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-superpowers-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (workspace (niwa-hooks-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-hooks-require-project-trust t))
    (unwind-protect
        (progn
          (make-directory (file-name-concat plugin-root "hooks") t)
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"SessionStart\":[{\"matcher\":\"startup\","
                    "\"hooks\":[{\"type\":\"command\","
                    "\"command\":\"\\\"${PLUGIN_ROOT}/hooks/run-hook.cmd\\\" session-start-codex\"},"
                    "{\"type\":\"command\","
                    "\"command\":\"superpowers-extra\"}]}]}}"))
          (niwa-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"superpowers\",\"hooks\":\"hooks/hooks.json\"}")
          (let ((rules (niwa-hooks-effective-rules session workspace)))
            (should-not
             (niwa-hooks--matching-handlers
              'SessionStart '(:source "startup") rules)))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (niwa-plugins-enable "superpowers" workspace))
          (let* ((rules (niwa-hooks-effective-rules session workspace))
                 (handlers (niwa-hooks--matching-handlers
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

(ert-deftest niwa-hooks-matcher-matches-p ()
  "Match wildcard, exact alternatives, regex, and symbols."
  (dolist (case '((nil "Bash" t)
                  ("*" "Bash" t)
                  ("Bash|Read" "Read" t)
                  ("Bash|Read" "Edit" nil)
                  ("B.*" "Bash" t)
                  (Bash "Bash" t)
                  ;; An unusable regexp matches nothing rather than
                  ;; signalling out of the event that consulted it.
                  ("[" "Bash" nil)
                  ("Bash\\(" "Bash" nil)))
    (pcase-let ((`(,matcher ,target ,expected) case))
      (should (eq (not (null (niwa-hooks-matcher-matches-p
                              matcher target)))
	                  expected)))))

(niwa-deftest niwa-hooks--matching-handlers
  (:doc "ignores malformed normalized rule entries and groups")
  (let* ((handler '(:type elisp
                    :function niwa-hooks-test--deny-fn))
         (handlers
          (niwa-hooks--matching-handlers
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

(niwa-deftest niwa-hooks--handlers-for-event ()
  ,test
  (test)
  :doc "prepends native functions as executable Elisp handler records"
  (let ((niwa-pre-tool-use-functions
         '(niwa-hooks-test--native-rewrite-fn))
        (rules
         '((PreToolUse
            (:matcher "Bash"
             :hooks ((:type elisp
                      :function niwa-hooks-test--declarative-capture-fn)))))))
    (should
     (equal
      (niwa-hooks--handlers-for-event
       'PreToolUse '(:tool-name "Bash") rules)
      '((:type elisp
         :function niwa-hooks-test--native-rewrite-fn
         :source native)
        (:type elisp
         :function niwa-hooks-test--declarative-capture-fn)))))

  :doc "preserves buffer-local hook order and the global inheritance marker"
  (let ((niwa-pre-tool-use-functions
         '(niwa-hooks-test--native-rewrite-fn)))
    (with-temp-buffer
      (setq-local niwa-pre-tool-use-functions
                  '(niwa-hooks-test--declarative-capture-fn t))
      (should
       (equal
        (mapcar (lambda (handler) (plist-get handler :function))
                (niwa-hooks--handlers-for-event
                 'PreToolUse '(:tool-name "Bash") nil))
        '(niwa-hooks-test--declarative-capture-fn
          niwa-hooks-test--native-rewrite-fn))))))

(niwa-deftest niwa-hooks--event-json
		 (:doc "serializes Lisp booleans and nil optional fields as JSON values")
  ,test
  (test)
		 (let* ((payload (json-parse-string
				  (niwa-hooks--event-json
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
				    (niwa-hooks--event-json
				     (list :tool-name "Read" :tool-input table))
				    :object-type 'alist
				    :array-type 'list
				    :false-object :false))
			  (tool-input (alist-get 'tool_input payload)))
		     (should (eq (alist-get 'nested_false tool-input nil nil
					    #'equal)
				 :false)))))

(niwa-deftest niwa-hooks--event-json/raw-values
		 (:doc "normalizes raw byte strings and stringifies Lisp objects")
		 ,test
		 (test)
		 (let* ((raw (string (unibyte-char-to-multibyte #x80)))
			(payload (json-parse-string
				  (niwa-hooks--event-json
				   (list :result raw
					 :callback
					 (lambda () 'not-json)))
				  :object-type 'alist
				  :array-type 'list)))
		   (should (equal "\\x80" (alist-get 'result payload)))
		   (should (stringp (alist-get 'callback payload)))
		   (should (string-match-p "not-json"
					   (alist-get 'callback payload)))))

(niwa-deftest niwa-hooks-format-context
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
    (niwa-hooks-format-context
     '((:event "UserPromptSubmit"
               :body "literal </hook-event> & <tag> \"quoted\"")
       (:event "A\"B" :body "x & y"))))))

(niwa-deftest niwa-hooks-consume-session-context ()
  ,test
  (test)
  :doc "consumes the captured prefix while preserving appended context"
  (let* ((session (niwa-session--create :name "hooks"))
         (captured '((:event SessionStart :body "start")))
         (appended '((:event UserPromptSubmit :body "later"))))
    (setf (niwa-session-hook-context-pending session)
          (append captured appended))
    (should (niwa-hooks-consume-session-context session captured))
    (should (equal appended
                   (niwa-session-hook-context-pending session))))

  :doc "leaves a changed prefix untouched"
  (let* ((session (niwa-session--create :name "hooks"))
         (current '((:event SessionStart :body "replacement")))
         (captured '((:event SessionStart :body "start"))))
    (setf (niwa-session-hook-context-pending session) current)
    (should-not
     (niwa-hooks-consume-session-context session captured))
    (should (equal current
                   (niwa-session-hook-context-pending session)))))


;;
;;; Decisions

(niwa-deftest niwa-hooks--context-contribution
  (:doc "keeps one handler's identity, reason, and ordered contexts")
  (should
   (equal
    '(:contexts ("a" "b")
      :source plugin
      :plugin-name "ponytail"
      :function inject
      :reason "active")
    (niwa-hooks--context-contribution
     '(:source plugin :plugin-name "ponytail" :function inject)
     '(:additional-context ("a" "b") :system-message "active")))))

(niwa-deftest niwa-hooks-merge-decisions
		 (:doc "merges contexts and keeps restrictive permission and stop precedence")
		 (progn
		 (let* ((first '(:permission-decision allow
						      :additional-context ("a")
						      :updated-result "one"))
			(second '(:permission-decision ask
						       :additional-context "b"))
			(third '(:permission-decision deny
						      :permission-reason "no")))
		   (let ((decision
			  (niwa-hooks-merge-decisions
			   (niwa-hooks-merge-decisions
			    (niwa-hooks-merge-decisions nil first)
			    second)
			   third)))
		     (should (eq (plist-get decision :permission-decision) 'deny))
		     (should (equal (plist-get decision :additional-context)
				    '("a" "b")))
		     (should (equal (plist-get decision :updated-result) "one"))
		     (should (equal (plist-get decision :permission-reason) "no"))))
		 ;; A later `continue: true' cannot cancel an earlier stop.
		 (let ((decision
			(niwa-hooks-merge-decisions
			 (niwa-hooks-merge-decisions
			  (niwa-hooks-merge-decisions nil '(:continue t))
			  '(:continue nil :stop-reason "stop now"))
			 '(:continue t))))
		   (should (plist-member decision :continue))
		   (should-not (plist-get decision :continue))
		   (should (equal "stop now" (plist-get decision :stop-reason))))))

(niwa-deftest niwa-hooks-merge-decisions/malformed
  (:doc "ignores malformed base and next decisions")
  (let ((bad (intern "test-niwa-view.el.")))
    (should-not (niwa-hooks-merge-decisions bad bad))
    (should
     (equal (niwa-hooks-merge-decisions
             bad '(:system-message "ok"))
            '(:system-message "ok")))))

(niwa-deftest niwa-hooks-decision-accessors
  (:doc "treat malformed decisions as nil")
  (let ((bad (intern "test-niwa-view.el.")))
    (should-not (niwa-hooks-terminal-decision-p
                 bad 'UserPromptSubmit))
    (should-not (niwa-hooks-additional-context-string bad))
    (should-not (niwa-hooks--decision-blocking-p bad))
    (should-not (niwa-hooks-decision-reason bad))
    (should (equal (niwa-hooks--apply-decision-to-event-plist
                    'UserPromptSubmit '(:prompt "old") bad)
                   '(:prompt "old")))))

(niwa-deftest niwa-hooks--parse-command-decision
		 (:doc "parses root and hookSpecificOutput JSON decisions")
		 ,test
		 (test)
		 (should
		  (equal
		   (niwa-hooks--parse-command-decision
		    "{\"continue\":false,\"stopReason\":\"halt\"}")
		   '(:continue nil :stop-reason "halt")))
		 (should
		  (equal
		   (niwa-hooks--parse-command-decision
		    "{\"hookSpecificOutput\":{\"permissionDecision\":\"deny\",\
\"permissionDecisionReason\":\"no\"}}")
		   '(:permission-reason "no" :permission-decision deny)))
		 (should
		  (equal
		   (niwa-hooks--parse-command-decision
		    "{\"permissionDecision\":\"deny\",\"permissionReason\":\"documented\"}")
		   '(:permission-reason "documented" :permission-decision deny)))
		 (should
		  (equal
		   (niwa-hooks--parse-command-decision
		    "{\"updatedInput\":{\"file_path\":\"x\",\"old_string\":\"a\"}}")
		   '(:updated-input (:file_path "x" :old_string "a"))))
			 (should
			  (equal
			   (niwa-hooks--parse-command-decision
			    "{\"suppressOutput\":true}")
			   '(:hook-error "Unsupported hook decision field: suppressOutput")))
			 (should-not
			  (niwa-hooks--parse-command-decision
			   "{\"suppressOutput\":false}")))

(niwa-deftest niwa-hooks--command-timeout
		 (:doc "defaults malformed timeouts and clamps numeric timeouts")
		 (let ((niwa-hooks-command-timeout 30)
		       (niwa-hooks-command-timeout-max 120))
		   (should (= (niwa-hooks--command-timeout
			       '(:type command :command "x" :timeout "30"))
			      30))
		   (should (= (niwa-hooks--command-timeout
			       '(:type command :command "x" :timeout 500))
			      120))))


;;
;;; Execution

(niwa-deftest niwa-hooks-run-event
  ()
  ,test
  (test)
  :doc "runs matching Elisp hooks and ignores nonmatching groups"
		 (let* ((root (make-temp-file "niwa-hooks-run" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--rewrite-fn)))
			     (:matcher "Read"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo old"))
				  cb session)))))
			 (should (equal decision
					'(:updated-input (:command "echo rewritten"))))
			 (should (= (length (niwa-session-hook-log session)) 1)))
		     (delete-directory root t)))

  :doc "request terminal hooks survive teardown without attaching to the next request"
  (skip-unless (not (eq system-type 'windows-nt)))
  (dolist (event '(Stop StopFailure))
    (let* ((root (make-temp-file "niwa-hooks-terminal" t))
           (release-file (file-name-concat root "release"))
           (session (niwa-hooks-test--session root))
           (request
            (niwa-request--create
             :session session
             :hook-rules
             `((,event
                ((:hooks
                  ((:type command :source user :source-root ,root
                    :command ,(format
                               "while [ ! -e %s ]; do sleep 0.01; done; cat >/dev/null"
                               (shell-quote-argument release-file))
                    :timeout 2)
                   (:type command :source user :source-root ,root
                    :command "cat >/dev/null" :timeout 2))))))))
           (next-request (niwa-request--create :session session))
           (niwa-hook-rules nil)
           (niwa-hooks-slow-threshold nil)
           (callbacks 0)
           started)
      (unwind-protect
          (with-temp-buffer
            (setq-local niwa--current-request request)
            (niwa-hooks-test--await
             (lambda (callback)
               (setq started t)
               (niwa-hooks-run-event
                event nil
                (lambda (decision)
                  (cl-incf callbacks)
                  (funcall callback decision))
                session)
               ;; The real command cannot finish until released below.
               (should (= callbacks 0))
               (should-not (niwa-request-cancellers request))
               (niwa-request-drain-cancellers request)
               (setq-local niwa--current-request next-request)
               (niwa-request-drain-cancellers next-request)
               (with-temp-file release-file)))
            (should (= callbacks 1))
            (should-not (niwa-request-cancellers next-request))
            (let ((entries (niwa-session-hook-log session)))
              (should (= (length entries) 2))
              (dolist (entry entries)
                (should (eq (plist-get entry :status) 'ok)))))
        ;; Release and settle detached children even if an assertion fails.
        (with-temp-file release-file)
        (let ((deadline (+ (float-time) 3)))
          (while (and started (= callbacks 0) (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (niwa-request-drain-cancellers request)
        (niwa-request-drain-cancellers next-request)
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-hooks-run-event/remote-stdin-readiness
  (:quiet t)
  ,test
  (test)
  :doc "remote stdin waits for wrapper readiness, ownership, and the deadline"
  (dolist (stage '(normal early-filter early-buffer cancel-early cancel-wait
                         missing send-failure reentrant zero-cap no-timeout))
    (let* ((root (make-temp-file "niwa-hook-ready-" t))
           (session (niwa-hooks-test--session root))
           (request (niwa-request--create :session session))
           (niwa-hooks-slow-threshold nil)
           (niwa-hooks-command-timeout (unless (eq stage 'no-timeout) 1))
           (niwa-hooks-command-output-max-chars
            (if (eq stage 'zero-cap) 0 10000))
           (niwa-hook-rules
            '((PreToolUse
               ((:hooks ((:type command
                          :command "cat >/dev/null; printf '{\"system_message\":\"ready\"}'"
                          :fail-closed t)))))))
           child ready returned armed done decision (writes 0))
      (unwind-protect
          (cl-letf* ((make (symbol-function 'make-process))
                     (send (symbol-function 'process-send-string))
                     (timer (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (time &rest args)
                        (prog1 (apply timer time args)
                          (when (equal time 1) (setq armed t)))))
                     ((symbol-function 'niwa-hooks--command-default-directory)
                      (lambda (&rest _) "/ssh:hook-readiness.invalid:/workspace/"))
                     ((symbol-function 'make-process)
                      (lambda (&rest args)
                        ;; Replace only the transport: run the actual wrapper
                        ;; locally, with real pipes, filters and cancellation.
                        (let ((default-directory root)
                              (filter (plist-get args :filter)))
                          (setq args (plist-put args :file-handler nil))
                          (when (memq stage '(missing cancel-wait))
                            (setq args (plist-put args :command '("sleep" "5"))))
                          (setq args
                                (plist-put
                                 args :filter
                                 (unless (eq stage 'early-buffer)
                                   (lambda (proc text)
                                     (when (string-prefix-p (string 0) text)
                                       (setq ready t))
                                     (funcall filter proc text)))))
                          (setq child (apply make args))
                          (when (memq stage '(early-filter early-buffer cancel-early))
                            (let ((deadline (+ (float-time) 2)))
                              (while (and (not ready) (< (float-time) deadline))
                                (accept-process-output child 0.01)
                                (when (eq stage 'early-buffer)
                                  (setq ready
                                        (with-current-buffer (process-buffer child)
                                          (> (buffer-size) 0))))))
                            (should ready)
                            (should (= writes 0)))
                          (when (eq stage 'early-buffer)
                            (set-process-filter child filter))
                          (when (eq stage 'cancel-early)
                            (niwa-request-cancel request))
                          (setq returned t)
                          child)))
                     ((symbol-function 'process-send-string)
                      (lambda (proc text)
                        (should ready)
                        (should returned)
                        (should (or armed (eq stage 'no-timeout)))
                        (cl-incf writes)
                        (when (eq stage 'send-failure)
                          (signal 'file-error '("Readiness stdin failed")))
                        (when (eq stage 'reentrant)
                          (funcall (process-filter proc) proc " "))
                        (funcall send proc text))))
            (niwa-hooks-run-event
             'PreToolUse '(:tool-name "Read")
             (lambda (value) (setq done t decision value)) session nil request)
            (when (eq stage 'cancel-wait) (niwa-request-cancel request))
            (when (memq stage '(cancel-early cancel-wait))
              ;; A queued readiness notification must not resurrect stdin.
              (funcall (process-filter child) child (string 0)))
            (let ((deadline (+ (float-time) 3)))
              ;; Process exit precedes deferred output draining and the hook
              ;; result.  Wait for settlement, regardless of timer phase.
              (while (and (or (process-live-p child)
                              (null (niwa-session-hook-log session)))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should-not (process-live-p child))
            (let ((entries (niwa-session-hook-log session)))
              (should (= (length entries) 1))
              (should
               (eq (plist-get (car entries) :status)
                   (pcase stage
                     ((or 'cancel-early 'cancel-wait) 'cancelled)
                     ('missing 'timeout)
                     ('send-failure 'error)
                     ('zero-cap 'parse-error)
                     (_ 'ok)))))
            (if (memq stage '(cancel-early cancel-wait missing))
                (should (= writes 0))
              (should (= writes 1)))
            (unless (memq stage '(cancel-early cancel-wait missing send-failure zero-cap))
              (should done)
              (should (equal (plist-get decision :system-message) "ready"))))
        (niwa-request-cancel request)
        (when (and child (process-live-p child)) (delete-process child))
        (delete-directory root t)))))

(niwa-deftest niwa-hooks-run-event/invalid-matcher
  (:doc "an unusable matcher is reported and the event still runs")
  (let* ((root (make-temp-file "niwa-hooks-bad-matcher" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "["
               :hooks ((:type elisp
                        :function niwa-hooks-test--deny-fn)))
              (:matcher "Bash"
               :hooks ((:type elisp
                        :function niwa-hooks-test--rewrite-fn)))))))
         (niwa--warn-once-table (make-hash-table :test #'equal))
         warning)
    (unwind-protect
        (progn
          (niwa-test--with-captured-diagnostics warning
            ;; Twice, because the live defcustom layer is re-normalized on
            ;; every dispatch.
            (dotimes (_ 2)
              (let ((decision
                     (niwa-hooks-test--await
                      (lambda (cb)
                        (niwa-hooks-run-event
                         'PreToolUse
                         '(:tool-name "Bash"
                           :tool-input (:command "echo old"))
                         cb session)))))
                ;; The usable group still decides; the broken one is
                ;; skipped rather than aborting the event.
                (should (equal decision
                               '(:updated-input
                                 (:command "echo rewritten")))))))
          (should (string-match-p "matcher" warning))
          (should (string-match-p "niwa-hook-rules" warning))
          ;; The live defcustom layer is re-normalized per event, so the
          ;; first dispatch warns and the second is demoted to a quiet
          ;; *Messages* line: splitting on the message leaves exactly
          ;; three fragments (one warning plus one demoted repeat).
          (should (= 3 (length
                        (split-string warning "not a valid regexp")))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/request-cancellation
  (:doc "request cancellation stops a command hook before delayed effects")
  (let* ((root (make-temp-file "niwa-hooks-command-cancel" t))
         (ready-file (file-name-concat root "ready"))
         (release-file (file-name-concat root "release"))
         (late-file (file-name-concat root "late"))
         (session (niwa-hooks-test--session root))
         (request (niwa-request--create :session session))
         ;; Inspect cancellation and replay late delivery directly, rather
         ;; than racing a slow-notice deadline against child startup.
         (niwa-hooks-slow-threshold 60)
         (niwa-hook-rules
          `((PreToolUse
             ((:matcher "Bash"
               :hooks
               ((:type command
                 :command
                 ,(format "printf '%%s' \"$$\" > %s; while [ ! -e %s ]; do sleep 0.01; done; printf late > %s"
                          (shell-quote-argument ready-file)
                          (shell-quote-argument release-file)
                          (shell-quote-argument late-file))
                 :timeout 30)))))))
         callback-called messages slow-timer pid)
    (unwind-protect
        (progn
          (skip-unless (not (eq system-type 'windows-nt)))
          (niwa-test--with-captured-messages messages
            (cl-letf* ((schedule (symbol-function 'run-at-time))
                       ((symbol-function 'run-at-time)
                        (lambda (seconds repeat function &rest args)
                          (let ((timer (apply schedule seconds repeat function args)))
                            (when (eql seconds niwa-hooks-slow-threshold)
                              (setq slow-timer timer))
                            timer))))
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Bash")
               (lambda (_) (setq callback-called t))
               session nil request))
            (niwa-test--await 5 "hook child writes ready pid"
              (when-let* ((attributes (file-attributes ready-file)))
                (> (file-attribute-size attributes) 0)))
            (setq pid (string-to-number
                       (with-temp-buffer
                         (insert-file-contents ready-file)
                         (buffer-string))))
            (should (> pid 0))
            (should (memq slow-timer (default-toplevel-value 'timer-list)))
            (niwa-request-drain-cancellers request)
            (niwa-test--await 5 "cancelled hook child exits"
              (let ((attributes (process-attributes pid)))
                (or (null attributes) (equal "Z" (alist-get 'state attributes)))))
            (should-not (memq slow-timer (default-toplevel-value 'timer-list)))
            ;; Even a notice already dequeued when cancellation happened must
            ;; be fenced.  No wall-clock sleep is needed to deliver it.
            (apply (timer--function slow-timer) (timer--args slow-timer))
            (with-temp-file release-file))
          (should-not (file-exists-p late-file))
          ;; The caller's continuation belongs to the torn-down request
          ;; and never runs, but the handler still settles: one log entry
          ;; and no slow notice for a child that is already dead.
          (should-not callback-called)
          (should-not (string-match-p "still running" messages))
          (let ((entry (car (last (niwa-session-hook-log session)))))
            (should (eq 'cancelled (plist-get entry :status)))
            (should (eq 'cancelled (plist-get entry :exit-status)))
            (should (numberp (plist-get entry :elapsed)))))
      (niwa-request-drain-cancellers request)
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/startup-cancellation
  (:quiet t)
  ,test
  (test)
  :doc "cancellation fences preparation and releases handles returned after teardown"
  (dolist (stage '(before environment input pipe child))
    (let* ((root (make-temp-file "niwa-hook-startup-" t))
           (session (niwa-hooks-test--session root))
           (request (niwa-request--create :session session))
           (niwa-hooks-slow-threshold nil)
           (niwa-hook-rules
            '((PreToolUse ((:matcher "Bash"
                           :hooks ((:type command :command "sleep 5" :timeout 1)))))))
           children pipes timers prepared callback-called)
      (unwind-protect
          (progn
            (when (eq stage 'before) (niwa-request-cancel request))
            (cl-letf* ((environment (symbol-function 'niwa-hooks--command-process-environment))
                       (json (symbol-function 'niwa-hooks--event-json))
                       (pipe (symbol-function 'make-pipe-process))
                       (child (symbol-function 'make-process))
                       (timer (symbol-function 'run-at-time))
                       ((symbol-function 'niwa-hooks--command-process-environment)
                        (lambda (&rest args)
                          (setq prepared t)
                          (when (eq stage 'environment) (niwa-request-cancel request))
                          (apply environment args)))
                       ((symbol-function 'niwa-hooks--event-json)
                        (lambda (&rest args)
                          (when (eq stage 'input) (niwa-request-cancel request))
                          (apply json args)))
                       ((symbol-function 'make-pipe-process)
                        (lambda (&rest args)
                          (let ((proc (apply pipe args)))
                            (push proc pipes)
                            (when (eq stage 'pipe) (niwa-request-cancel request))
                            proc)))
                       ((symbol-function 'make-process)
                        (lambda (&rest args)
                          (let ((proc (apply child args)))
                            (push proc children)
                            (when (eq stage 'child) (niwa-request-cancel request))
                            proc)))
                       ((symbol-function 'run-at-time)
                        (lambda (&rest args)
                          (let ((value (apply timer args)))
                            (push value timers)
                            value))))
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Bash")
               (lambda (_) (setq callback-called t)) session nil request))
            (should-not callback-called)
            (when (eq stage 'before) (should-not prepared))
            (should (= (if (eq stage 'child) 1 0) (length children)))
            (should-not (cl-find-if #'process-live-p (append children pipes)))
            (dolist (timer timers)
              (should-not (memq timer timer-list)))
            (should (equal '(cancelled)
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (niwa-session-hook-log session)))))
        (niwa-request-cancel request)
        (dolist (proc (append children pipes))
          (when (process-live-p proc) (delete-process proc)))
        (dolist (timer timers) (cancel-timer timer))
        (delete-directory root t)))))

(niwa-deftest niwa-hooks-run-event/settled-child-release
  (:quiet t)
  ,test
  (test)
  :doc "settlement kills the child and arms the timeout before stdin"
  (let* ((order nil)
         (decisions 0)
         (children nil)
         (root (make-temp-file "niwa-hooks-child-release" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold nil)
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command :command "sleep 5" :timeout 30))))))))
    (unwind-protect
        (progn
          (skip-unless (not (eq system-type 'windows-nt)))
          (cl-letf* ((real-make-process (symbol-function 'make-process))
                     ((symbol-function 'make-process)
                      (lambda (&rest args)
                        (car (push (apply real-make-process args) children))))
                     (real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (&rest args)
                        (push 'timer order)
                        (apply real-run-at-time args)))
                     ;; The child is already running when stdin fails, and
                     ;; killing its buffer would collect it on its own --
                     ;; which is what settlement must not depend on.
                     ((symbol-function 'process-send-string)
                      (lambda (&rest _)
                        (push 'stdin order)
                        (error "Broken pipe")))
                     (real-kill-buffer (symbol-function 'kill-buffer))
                     ((symbol-function 'kill-buffer)
                      (lambda (&optional buffer-or-name)
                        (let ((buffer (get-buffer (or buffer-or-name
                                                      (current-buffer)))))
                          ;; Only the hook's own buffers survive, so
                          ;; unrelated kills keep working.
                          (if (and buffer
                                   (string-search "niwa-hook"
                                                  (buffer-name buffer)))
                              t
                            (funcall real-kill-buffer buffer-or-name))))))
            (niwa-hooks-run-event
             'PreToolUse '(:tool-name "Bash")
             (lambda (_) (cl-incf decisions)) session))
          (should children)
          (should-not (cl-find-if #'process-live-p children))
          ;; Other machinery may arm timers in this window, so the
          ;; invariant is that a timer preceded the write, not the exact
          ;; sequence.
          (should (eq 'timer (car (nreverse order))))
          (accept-process-output nil 0.05)
          (should (= 1 decisions)))
      (dolist (proc children)
        (when (process-live-p proc) (delete-process proc)))
      (dolist (buffer (buffer-list))
        (when (string-search "niwa-hook" (buffer-name buffer))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "an exit before stdin preserves the child's exit status"
  (let* ((root (make-temp-file "niwa-hooks-early-exit" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command :command "exit 1" :fail-closed t))))))))
    (unwind-protect
        (cl-letf* ((make (symbol-function 'make-process))
                   (send (symbol-function 'process-send-string))
                   ((symbol-function 'make-process)
                    (lambda (&rest arguments)
                      ;; A startup failure may exit even before the wrapper reads.
                      (apply make
                             (plist-put arguments :command '("sh" "-c" "exit 1")))))
                   ((symbol-function 'process-send-string)
                    (lambda (process text)
                      ;; Hold the sentinel until the write observes the exit.
                      ;; This is the ordering a sufficiently fast child takes.
                      (when (equal (process-name process) "niwa-hook")
                        (set-process-sentinel process #'ignore)
                        (with-timeout (5 (ert-fail "Hook did not exit"))
                          (while (process-live-p process)
                            (accept-process-output process 0.01))))
                      (funcall send process text))))
          (should
           (equal (niwa-hooks-test--await
                   (lambda (callback)
                     (niwa-hooks-run-event
                      'PreToolUse '(:tool-name "Bash") callback session)))
                  '(:continue nil :stop-reason "Hook exited with status 1"))))
      (delete-directory root t)))

  :doc "a timeout reports itself rather than the signal that stopped the child"
  ;; A blocking timeout legitimately reports to the echo area.
  (let* ((decision nil)
         (children nil)
         (root (make-temp-file "niwa-hooks-timeout-reason" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold nil)
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command :command "sleep 5" :timeout 1
                        :fail-closed t))))))))
    (unwind-protect
        (progn
          (skip-unless (not (eq system-type 'windows-nt)))
          (cl-letf* ((real-make-process (symbol-function 'make-process))
                     ((symbol-function 'make-process)
                      (lambda (&rest args)
                        (car (push (apply real-make-process args) children)))))
            (niwa-hooks-run-event
             'PreToolUse '(:tool-name "Bash")
             (lambda (value) (setq decision (or value 'settled))) session)
            (let ((deadline (+ (float-time) 5)))
              (while (and (null decision) (< (float-time) deadline))
                (accept-process-output nil 0.05))))
          (should decision)
          (should-not (cl-find-if #'process-live-p children))
          (should (string-match-p "timed out"
                                  (plist-get decision :stop-reason))))
      (dolist (proc children)
        (when (process-live-p proc) (delete-process proc)))
      (delete-directory root t)))

  :doc "transport-deferred completion stays cancellable before the next handler"
  (dolist (cancel-p '(nil t))
    (let* ((root (make-temp-file "niwa-hooks-deferred" t))
           (next-file (file-name-concat root "next"))
           (session (niwa-hooks-test--session root))
           (request (niwa-request--create :session session))
           (niwa-transport--enabled-p t)
           (niwa-hooks-slow-threshold nil)
           (niwa-hook-rules
            `((PreToolUse
               ((:matcher "Bash"
                 :hooks ((:type command :command "cat >/dev/null" :timeout 5)
                         (:type command :command
                          ,(format "cat >/dev/null; printf next > %s"
                                   (shell-quote-argument next-file)))))))))
           callback-called)
      (unwind-protect
          (progn
            (let ((niwa-transport--depth 1))
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Bash")
               (lambda (_) (setq callback-called t)) session nil request)
              (let ((deadline (+ (float-time) 2)))
                (while (and (not (cl-find 'hook-completion
                                         (hash-table-keys niwa-transport--pending)
                                         :key #'car-safe))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (cl-find 'hook-completion
                               (hash-table-keys niwa-transport--pending)
                               :key #'car-safe))
              (should-not callback-called)
              (should-not (file-exists-p next-file))
              (when cancel-p (niwa-request-drain-cancellers request)))
            (let ((deadline (+ (float-time) 2)))
              (while (and (not cancel-p) (not callback-called)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (eq (not cancel-p) callback-called))
            (should (eq (not cancel-p) (file-exists-p next-file)))
            (should-not (cl-find 'hook-completion
                                 (hash-table-keys niwa-transport--pending)
                                 :key #'car-safe))
            (should (equal (if cancel-p '(cancelled) '(ok ok))
                           (mapcar (lambda (entry) (plist-get entry :status))
                                   (niwa-session-hook-log session)))))
        (niwa-request-drain-cancellers request)
        (delete-directory root t)))))

(niwa-deftest niwa-hooks-run-event/teardown-failure
  (:doc "a signalling teardown still settles the event")
  (let* ((root (make-temp-file "niwa-hooks-teardown-fail" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold nil)
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command :command "sleep 5" :timeout 30)))))))
         (children nil)
         (decisions 0))
    (unwind-protect
        (progn
          (skip-unless (not (eq system-type 'windows-nt)))
          (cl-letf* ((real-make-process (symbol-function 'make-process))
                     ((symbol-function 'make-process)
                      (lambda (&rest args)
                        (car (push (apply real-make-process args) children))))
                     ;; Every deletion below fails, so the stderr pipe must be
                     ;; collected here too or it outlives the test.
                     (real-make-pipe-process (symbol-function 'make-pipe-process))
                     ((symbol-function 'make-pipe-process)
                      (lambda (&rest args)
                        (car (push (apply real-make-pipe-process args) children))))
                     ((symbol-function 'process-send-string)
                      (lambda (&rest _) (error "Broken pipe")))
                     (real-delete-process (symbol-function 'delete-process))
                     ((symbol-function 'delete-process)
                      (lambda (&rest _) (error "Sentinel exploded"))))
            (niwa-test--with-captured-diagnostics nil
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Bash")
               (lambda (_) (cl-incf decisions)) session))
            (should (= 1 decisions))
            ;; Restore before the teardown below needs it.
            (fset 'delete-process real-delete-process)))
      (dolist (proc children)
        (when (process-live-p proc) (delete-process proc)))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-context-audit-records
  (:quiet t :doc "attributes merged context to handlers in execution order")
  (let* ((root (make-temp-file "niwa-hooks-context-audit" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules nil))
    (setf (niwa-session-hook-rules session)
          `((SubagentStart
             ((:matcher "explorer"
               :hooks ((:type elisp
                        :function niwa-hooks-test--first-context-fn
                        :source plugin
                        :plugin-name "ponytail")
                       (:type elisp
                        :function niwa-hooks-test--second-context-fn
                        :source project-file
                        :source-file ,(file-name-concat root "hooks.el")
                        :description "Inject project conventions")))))))
    (unwind-protect
        (let* ((decision
                (niwa-hooks-test--await
                 (lambda (cb)
                   (niwa-hooks-run-event
                    'SubagentStart '(:role "explorer") cb session))))
               (audits (niwa-hooks-context-audit-records
                        decision 'SubagentStart 'subagent-context t))
               (handlers (plist-get (car audits) :handlers)))
          (should (equal '("first" "second-a" "second-b")
                         (plist-get decision :additional-context)))
          (let* ((entries (niwa-hooks-context-entries decision 'SubagentStart))
                 (rendered (niwa-hooks-format-context entries)))
            (should (equal "ponytail" (plist-get (car entries) :plugin-name)))
            (dolist (entry (cdr entries))
              (should (equal (file-name-concat root "hooks.el")
                             (plist-get entry :source-file))))
            (should (string-search "source=\"plugin\"" rendered))
            (should (string-search "plugin=\"ponytail\"" rendered))
            (should (string-search (file-name-concat root "hooks.el") rendered))
            (should (string-search "second-a" rendered))
            (should (string-search "second-b" rendered)))
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
          (should-not (plist-member (car audits) :context))
          (setcar (last (plist-get decision :additional-context)) "changed later")
          (dolist (entry (niwa-hooks-context-entries decision 'SubagentStart))
            (should-not (plist-get entry :source))
            (should-not (plist-get entry :source-file))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/session-start-log-source
  (:doc "logs whether SessionStart initializes or resumes a session")
  (let* ((root (make-temp-file "niwa-hooks-session-source" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules
          '((SessionStart
             ((:matcher "*"
               :hooks ((:type elisp
                        :function niwa-hooks-test--context-fn))))))))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session)
                (file-name-as-directory root))
          (niwa-hooks-test--await
           (lambda (cb)
             (niwa-hooks-run-event
              'SessionStart '(:source "resume") cb session)))
          (niwa-hooks-test--await
           (lambda (cb)
             (niwa-hooks-run-event
              'SessionStart '(:source "startup") cb session)))
          (should
           (equal '("resume" "startup")
                  (mapcar (lambda (entry)
                            (plist-get entry :event-source))
                          (niwa-session-hook-log session))))
          (with-temp-buffer
            (insert-file-contents (file-name-concat root "hook-log.el"))
            (should (equal "resume" (plist-get (read (current-buffer))
                                               :event-source)))
            (should (equal "startup" (plist-get (read (current-buffer))
                                                :event-source)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/serial-mutation
		 (:doc "passes updated input from one hook to later hooks")
		 (let* ((root (make-temp-file "niwa-hooks-serial" t))
			(session (niwa-hooks-test--session root))
			(niwa-hooks-test--seen-event nil)
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--rewrite-fn)
					       (:type elisp
						      :function niwa-hooks-test--capture-fn))))))))
		   (unwind-protect
		       (progn
			 (niwa-hooks-test--await
			  (lambda (cb)
			    (niwa-hooks-run-event
			     'PreToolUse
			     '(:tool-name "Bash" :tool-input (:command "echo old"))
			     cb session)))
			 (should (equal (plist-get niwa-hooks-test--seen-event
						   :tool-input)
					'(:command "echo rewritten"))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/unified-elisp-engine
  (:doc "runs native then declarative Elisp with shared serial mutation")
  (let* ((root (make-temp-file "niwa-hooks-unified" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-test--execution-order nil)
         (niwa-hooks-test--native-seen-event nil)
         (niwa-hooks-test--seen-event nil)
         (niwa-pre-tool-use-functions
          '(niwa-hooks-test--native-rewrite-fn))
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function
                        niwa-hooks-test--declarative-capture-fn))))))))
    (unwind-protect
        (progn
          (niwa-hooks-test--await
           (lambda (cb)
             (niwa-hooks-run-event
              'PreToolUse
              '(:tool-name "Bash" :tool-input (:command "original"))
              cb session)))
          (should (equal '(native declarative)
                         niwa-hooks-test--execution-order))
          (should (equal '(:command "native rewrite")
                         (plist-get niwa-hooks-test--seen-event
                                    :tool-input)))
          (should-not (plist-member niwa-hooks-test--native-seen-event
                                    :hook-handler))
          (should (plist-member niwa-hooks-test--seen-event
                                :hook-handler))
          (should (= 2 (length (niwa-session-hook-log session)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/unified-elisp-errors
  (:doc "normalizes native and declarative malformed decisions once each")
  (let* ((root (make-temp-file "niwa-hooks-unified-error" t))
         (session (niwa-hooks-test--session root))
         (niwa-pre-tool-use-functions
          '(niwa-hooks-test--suppress-output-fn))
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function niwa-hooks-test--suppress-output-fn)
                       (:type elisp
                        :function niwa-hooks-test--context-fn))))))))
    (unwind-protect
        (let ((decision
               (niwa-hooks-test--await
                (lambda (cb)
                  (niwa-hooks-run-event
                   'PreToolUse '(:tool-name "Bash") cb session)))))
          (should (equal '("later")
                         (plist-get decision :additional-context)))
          (should (equal '(error error ok)
                         (mapcar (lambda (entry) (plist-get entry :status))
                                 (niwa-session-hook-log session)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/unified-elisp-terminal
  (:quiet t :doc "a terminal native decision skips later declarative handlers")
  (let* ((root (make-temp-file "niwa-hooks-unified-terminal" t))
         (session (niwa-hooks-test--session root))
         (niwa-pre-tool-use-functions
          '(niwa-hooks-test--stop-fn))
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type elisp
                        :function niwa-hooks-test--context-fn))))))))
    (unwind-protect
        (let ((decision
               (niwa-hooks-test--await
                (lambda (cb)
                  (niwa-hooks-run-event
                   'PreToolUse '(:tool-name "Bash") cb session)))))
          (should-not (plist-get decision :continue))
          (should-not (plist-get decision :additional-context))
          (should (= 1 (length (niwa-session-hook-log session)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/stale-ambient-context
		 (:doc "ignores stale non-struct request context when collecting rules")
		 (let* ((root (make-temp-file "niwa-hooks-stale" t))
			(session (niwa-hooks-test--session root))
			(niwa--current-request 'gs)
			(niwa--agent-invocation 'gs))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hello")
				  cb session)))))
			 (should-not decision))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/malformed-decision
		 (:doc "does not expose malformed native hook return values to callers")
		 (let* ((root (make-temp-file "niwa-hooks-malformed" t))
			(session (niwa-hooks-test--session root))
			(niwa-user-prompt-submit-functions
			 '(niwa-hooks-test--malformed-fn)))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hello")
				  cb session)))))
			 (should-not decision))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/malformed-symbol-decision
  (:doc "does not expose malformed non-keyword symbol decisions to callers")
  (let* ((root (make-temp-file "niwa-hooks-malformed-symbol" t))
         (session (niwa-hooks-test--session root))
         (niwa-user-prompt-submit-functions
          '(niwa-hooks-test--malformed-file-symbol-fn)))
    (unwind-protect
        (let ((decision
               (niwa-hooks-test--await
                (lambda (cb)
                  (niwa-hooks-run-event
                   'UserPromptSubmit
                   '(:prompt "hello")
                   cb session)))))
          (should-not decision))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/session-reminders
  (:quiet t :doc "queues model-visible reminders for blocking outcomes only")
  ,test
  (test)
  (let* ((root (make-temp-file "niwa-hooks-reminders" t))
         (session (niwa-hooks-test--session root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (setq-local niwa--current-request
                      (niwa-request--create :id "request-1"
                                               :session session))
          (let ((niwa-pre-tool-use-functions
                 '(niwa-hooks-test--deny-fn)))
            (niwa-hooks-test--await
             (lambda (cb)
               (niwa-hooks-run-event
                'PreToolUse
                '(:tool-name "Bash" :tool-input (:command "echo hi"))
                cb session)))
            (let ((body
                   (plist-get
                    (cdar (plist-get niwa-reminders--turn-events :items))
                    :body)))
              (should (string-match-p "PreToolUse hook blocked" body))
              (should (string-match-p "blocked" body)))))
      (delete-directory root t)))
  (let* ((root (make-temp-file "niwa-hooks-reminders" t))
         (session (niwa-hooks-test--session root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (setq-local niwa--current-request
                      (niwa-request--create :id "request-1"
                                               :session session))
          (let ((niwa-post-tool-use-functions
                 '(niwa-hooks-test--system-message-fn)))
            (niwa-hooks-test--await
             (lambda (cb)
               (niwa-hooks-run-event
                'PostToolUse
                '(:tool-name "Read" :tool-result "ok")
                cb session)))
            (should-not
             (plist-get niwa-reminders--turn-events :items))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/command
		 (:doc "runs command hooks, parses JSON stdout, and logs stderr privately")
		 (let* ((root (make-temp-file "niwa-hooks-command" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 `((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command
						      :command
						      ,(niwa-hooks-test--emacs-command
							"(princ \"{\\\"permissionDecision\\\":\\\"ask\\\",\\\"permissionDecisionReason\\\":\\\"review\\\"}\")")
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'PreToolUse
				  '(:tool-name "Bash" :tool-input (:command "echo hi"))
				  cb session)))))
			 (should (equal decision
					'(:permission-reason "review"
							     :permission-decision ask)))
			 (should (= (length (niwa-session-hook-log session)) 1))
			 (should (equal (plist-get (car (niwa-session-hook-log session))
						   :stdout-preview)
					"{\"permissionDecision\":\"ask\",\
\"permissionDecisionReason\":\"review\"}")))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/command-output-cap
                         (:quiet t :doc "caps command hook output before returning block reasons")
                         (let* ((root (make-temp-file "niwa-hooks-command" t))
                                (session (niwa-hooks-test--session root))
                                (niwa-hooks-command-output-max-chars 16)
                                (niwa-hook-rules
                                 `((PreToolUse
                                    ((:matcher "Bash"
                                               :hooks ((:type command
                                                              :command
                                                              ,(niwa-hooks-test--emacs-command
                                                                "(progn (princ (make-string 100 ?x)) (kill-emacs 2))")
                                                              :timeout 5))))))))
                           (unwind-protect
                               (let ((decision
                                      (niwa-hooks-test--await
                                       (lambda (cb)
                                         (niwa-hooks-run-event
                                          'PreToolUse
                                          '(:tool-name "Bash" :tool-input (:command "echo hi"))
                                          cb session)))))
                                 (should (equal 'deny
                                                (plist-get decision :permission-decision)))
                                 (should (string-match-p
                                          "Hook output truncated at 16 character limit"
                                          (plist-get decision :permission-reason))))
                             (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/nonblocking-events-continue
		 (:doc "does not let unsupported stop decisions skip observer hooks")
		 (let* ((root (make-temp-file "niwa-hooks-observer" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--stop-fn)
					       (:type elisp
						      :function niwa-hooks-test--context-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'PostToolUse
				  '(:tool-name "Read" :result "ok")
				  cb session)))))
			 (should (equal (plist-get decision :additional-context)
					'("later"))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/permission-request-terminal
		 (:quiet t :doc "stops later PermissionRequest hooks after fail-closed stop decisions")
		 (let* ((root (make-temp-file "niwa-hooks-pr-terminal" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 '((PermissionRequest
			    ((:matcher "Edit"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--stop-fn)
					       (:type elisp
						      :function niwa-hooks-test--allow-fn))))))))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'PermissionRequest
				  '(:tool-name "Edit")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "stop"))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/command-block
		 (:quiet t :doc "maps exit-code 2 to event-specific blocking decisions")
		 (let* ((root (make-temp-file "niwa-hooks-block" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 `((UserPromptSubmit
			    ((:matcher "*"
				       :hooks ((:type command
						      :command
						      ,(niwa-hooks-test--emacs-command
							"(progn (princ \"blocked\") (kill-emacs 2))")
						      :timeout 5))))))))
		   (unwind-protect
		       (let ((decision
			      (niwa-hooks-test--await
			       (lambda (cb)
				 (niwa-hooks-run-event
				  'UserPromptSubmit
				  '(:prompt "hi")
				  cb session)))))
			 (should (equal decision
					'(:continue nil :stop-reason "blocked"))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/local-stderr-settlement
  (:quiet t :doc "drains local stderr before settling a fast hook")
  (dolist (case '((nil nil) (8 nil) (nil t)))
    (let* ((cap (car case))
           (surviving-writer-p (cadr case))
           (root (make-temp-file "niwa-hooks-stderr" t))
           (session (niwa-hooks-test--session root))
           (spawn (symbol-function 'make-process))
           (accept (symbol-function 'accept-process-output))
           (stderr-polls 0)
           (niwa-hooks-command-output-max-chars cap)
           (expected (if cap
                         "local-bl\n... Hook output truncated at 8 character limit."
                       "local-block-reason"))
           (niwa-hook-rules
            '((UserPromptSubmit
               ((:hooks ((:type command
                          :command "cat >/dev/null; printf local-block-reason >&2; exit 2"
                          :timeout 5)))))))
           child done decision)
      (unwind-protect
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq child (apply spawn args))))
                    ((symbol-function 'accept-process-output)
                     (lambda (&optional proc seconds millisec just-this-one)
                       (let ((result (funcall accept proc seconds millisec just-this-one)))
                         (if (and surviving-writer-p (processp proc)
                                  (string-prefix-p "niwa-hook-stderr" (process-name proc)))
                             (progn
                               ;; Preserve the real first read, then model a
                               ;; pipe continuously replenished by a descendant.
                               (cl-incf stderr-polls)
                               (should (< stderr-polls 25))
                               (sleep-for 0.01)
                               t)
                           result)))))
            (niwa-hooks-run-event
             'UserPromptSubmit '(:prompt "hi")
             (lambda (result) (setq decision result done t)) session)
            ;; Keep stderr pending, then deliver the real exit sentinel before
            ;; servicing its separate pipe, regardless of event-loop ordering.
            (with-timeout (5 (ert-fail "Timed out waiting for hook"))
              (while (process-live-p child)
                (accept-process-output child 0.05 nil t)))
            (unless done
              (funcall (process-sentinel child) child "finished\n"))
            (should done)
            (when surviving-writer-p
              (should (> stderr-polls 0)))
            (should (equal decision (list :continue nil :stop-reason expected)))
            (should (equal expected
                           (plist-get (car (niwa-session-hook-log session))
                                      :stderr-preview))))
        (delete-directory root t)))))

(niwa-deftest niwa-hooks-run-event/unannotated-command-origin
  (:quiet t :doc "fails closed instead of inferring where an unannotated command runs")
  (let* ((root (make-temp-file "niwa-hooks-unannotated" t))
         (marker (file-name-concat root "ran"))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules nil))
    (setf (niwa-session-hook-rules session)
          `((PreToolUse
             ((:matcher "Bash"
               :hooks ((:type command
                        :command ,(format "touch %s"
                                          (shell-quote-argument marker)))))))))
    (unwind-protect
        (let ((decision
               (niwa-hooks-test--await
                (lambda (cb)
                  (niwa-hooks-run-event
                   'PreToolUse
                   '(:tool-name "Bash" :tool-input (:command "echo hi"))
                   cb session)))))
          (should (eq 'deny (plist-get decision :permission-decision)))
          (should (string-match-p
                   "execution origin"
                   (plist-get decision :permission-reason)))
          (should-not (file-exists-p marker)))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/project-command-cwd
		 (:doc "runs project-sourced command hooks from the workspace root")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-hooks-project-cwd" t)))
			(subdir (file-name-as-directory
				 (file-name-concat root "subdir")))
			session
			(niwa-hook-rules nil))
		   (unwind-protect
		       (progn
			 (make-directory subdir t)
			 (setq session
			       (niwa-session-create
				"hooks-test"
				(niwa-hooks-test--workspace root)
				subdir))
			 (setf
			  (niwa-session-hook-rules session)
			  `((PreToolUse
			     ((:matcher "Bash"
				:hooks ((:type command
					 :command
					 ,(niwa-hooks-test--emacs-command
					   "(princ default-directory)")
					 :source project-file
					 :source-root ,root
					 :timeout 5)))))))
			 (niwa-hooks-test--await
			  (lambda (cb)
			    (niwa-hooks-run-event
			     'PreToolUse
			     (niwa-hooks-event-plist
			      'PreToolUse session (niwa-session-workspace session)
			      :cwd subdir
			      :tool-name "Bash"
			      :tool-input '(:command "echo hi"))
			     cb session)))
			 (should
			  (file-equal-p
			   (directory-file-name
			    (string-trim
			     (plist-get (car (niwa-session-hook-log session))
					:stdout-preview)))
			   (directory-file-name root))))
			     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/plugin-command-cwd
  (:quiet t :doc "loads plugin code by root and reads context from conversation cwd")
  (let* ((root (make-temp-file "niwa-hooks-plugin-context" t))
         (workspace-root (file-name-concat root "workspace"))
         (subdir (file-name-as-directory
                  (file-name-concat workspace-root "conversation")))
         (plugin-root (file-name-as-directory (file-name-concat root "plugin")))
         (script (file-name-concat plugin-root "context.el"))
         (niwa-hook-rules nil)
         (niwa-hooks-persist-log nil)
         (process-environment (copy-sequence process-environment))
         session)
    (unwind-protect
        (progn
          (make-directory subdir t)
          (make-directory plugin-root t)
          (with-temp-file (file-name-concat subdir "context.txt")
            (insert "workspace context"))
          (with-temp-file (file-name-concat plugin-root "context.txt")
            (insert "plugin context"))
          (with-temp-file script
            (insert
             "(require 'json)\n"
             "(princ (json-encode\n"
             " `((hookSpecificOutput .\n"
             "    ((hookEventName . \"SessionStart\")\n"
             "     (additionalContext . ,(with-temp-buffer\n"
             "                             (insert-file-contents \"context.txt\")\n"
             "                             (buffer-string))))))))\n"))
          (setq session (niwa-session-create
                         "plugin-cwd"
                         (niwa-hooks-test--workspace workspace-root) subdir))
          (setf (niwa-session-hook-rules session)
                `((SessionStart
                   ((:hooks
                     ((:type command :source plugin
                       :plugin-root ,plugin-root
                       :plugin-data ,(file-name-concat root "data")
                       :command
                       ,(niwa-hooks-test--emacs-command
                         "(load (expand-file-name \"context.el\" (getenv \"CLAUDE_PLUGIN_ROOT\")) nil t)"))))))))
          (let ((decision
                 (niwa-hooks-test--await
                  (lambda (cb)
                    (niwa-hooks-run-event
                     'SessionStart
                     (niwa-hooks-event-plist 'SessionStart session)
                     cb session)))))
            (should (equal '("workspace context")
                           (plist-get decision :additional-context)))
            (should (eq 'ok (plist-get (car (niwa-session-hook-log session))
                                      :status)))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/remote-command-origin
  (:doc "dispatches remote project and local user/plugin commands by origin")
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-hooks-remote-origin" t)))
         (subdir (file-name-as-directory (file-name-concat root "subdir")))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) root))
         (remote-subdir (concat remote-root "subdir/"))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-local-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (project-plugin-local-root
          (file-name-as-directory
           (file-name-concat root ".niwa" "plugins" "target")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (niwa-hook-rules nil)
         (niwa-hooks-persist-log nil)
         (niwa-hooks-command-output-max-chars 16)
         (niwa-hooks-require-project-trust nil)
         (process-environment (copy-sequence process-environment))
         (spawn (symbol-function 'make-process))
         workspace session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (make-directory subdir t)
          (make-directory (file-name-concat root ".niwa") t)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (make-directory (file-name-concat project-plugin-local-root "hooks") t)
          (with-temp-file (file-name-concat user-dir "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s}]}]}}"
              (json-encode-string
               "pwd > user-pwd; cat > user-input.json"))))
          (with-temp-file (file-name-concat root ".niwa" "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s},{\"type\":\"elisp\",\"function\":\"niwa-hooks-test--capture-elisp-origin\"}]}]}}"
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
                "printf '%s|%s' \"$NIWA_PLUGIN_ROOT\" "
                "\"${NIWA_PLUGIN_DATA-unset}\" > plugin-env; "
                "cat > plugin-input.json")))))
          (with-temp-file
              (file-name-concat project-plugin-local-root "hooks" "hooks.json")
            (insert
             (format
              "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":%s}]}]}}"
              (json-encode-string
               (concat
                "pwd > project-plugin-pwd; "
                "printf '%s|%s' \"$NIWA_PLUGIN_ROOT\" "
                "\"$NIWA_PLUGIN_DATA\" > project-plugin-env; "
                "cat > project-plugin-input.json")))))
          (niwa-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (niwa-hooks-test--write-plugin-manifest
           project-plugin-local-root
           "{\"name\":\"target-demo\",\"hooks\":\"hooks/hooks.json\"}")
          (setq workspace (niwa-hooks-test--workspace remote-root)
                session (niwa-session-create
                         "remote-hooks" workspace remote-subdir)
                niwa-hooks-test--elisp-origin nil)
          (let ((niwa--session session))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (niwa-plugins-enable "demo" workspace)
              (niwa-plugins-enable "target-demo" workspace)))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (let ((child (apply spawn args))
                             (stderr (plist-get args :stderr)))
                         (when (and (equal "niwa-hook" (plist-get args :name))
                                    (bufferp stderr)
                                    (string-match-p
                                     "remote-stderr-with-extra-output"
                                     (car (last (plist-get args :command)))))
                           ;; TRAMP drains stderr after the supplied sentinel.
                           ;; Force that ordering even if the pipe read was early.
                           (add-function
                            :around (process-sentinel child)
                            (lambda (sentinel proc event)
                              (when (buffer-live-p stderr)
                                (with-current-buffer stderr (erase-buffer)))
                              (funcall sentinel proc event)
                              (when (buffer-live-p stderr)
                                (with-current-buffer stderr
                                  (insert "remote-stderr-with-extra-output"))))))
                         child))))
            (niwa-hooks-test--await
             (lambda (cb)
               (niwa-hooks-run-event
                'PreToolUse
                (niwa-hooks-event-plist
                 'PreToolUse session workspace
                 :tool-name "Bash"
                 :tool-input '(:command "echo hi"))
                cb session workspace))))
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
              subdir
              (file-text
               (file-name-concat subdir "project-plugin-pwd"))))
            (should
             (equal
              (concat project-plugin-local-root "|"
                      (file-name-concat root ".niwa" "state" "plugin-data"
                                        "target-demo"))
              (file-text
               (file-name-concat subdir "project-plugin-env"))))
            (dolist (input (list (read-input
                                  (file-name-concat root "project-input.json"))
                                 (read-input
                                  (file-name-concat user-dir "user-input.json"))
                                 (read-input
                                  (file-name-concat plugin-root
                                                    "plugin-input.json"))
                                 (read-input
                                  (file-name-concat
                                   subdir
                                   "project-plugin-input.json"))))
              (should (equal subdir (alist-get 'cwd input)))
              (should (equal root (alist-get 'workspace_root input)))
              (let ((target (alist-get 'execution_target input)))
                (should (equal "niwamock" (alist-get 'method target)))
                (should (equal (system-name) (alist-get 'host target)))))
            (should
             (equal remote-subdir
                    (plist-get
                     (plist-get niwa-hooks-test--elisp-origin :event)
                     :cwd)))
            (should-not
             (file-remote-p
              (plist-get niwa-hooks-test--elisp-origin
                         :default-directory)))
            (let ((entry
                   (cl-find-if
                    (lambda (item)
                      (eq 'project-file
                          (plist-get (plist-get item :handler) :source)))
                    (niwa-session-hook-log session))))
              (should (string-match-p
                       "Hook output truncated at 16 character limit"
                       (plist-get entry :stderr-preview))))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-run-event/foreign-command-target
  (:quiet t :doc "refuses a plugin command rooted on another remote target")
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-hooks-foreign" t)))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) root))
         (foreign-root (format "/niwamock:foreign:%s" root))
         (marker (file-name-concat root "foreign-ran"))
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("foreign")
          (setq session
                (niwa-session-create
                 "remote-hooks"
                 (niwa-hooks-test--workspace remote-root)
                 remote-root))
          (setf
           (niwa-session-hook-rules session)
           `((PreToolUse
              ((:matcher "Bash"
                :hooks ((:type command
                         :source plugin
                         :plugin-root ,foreign-root
                         :plugin-data ,(file-name-concat foreign-root "data")
                         :command ,(format "touch %s; cat >/dev/null"
                                           (shell-quote-argument marker)))))))))
          (let ((decision
                 (niwa-hooks-test--await
                  (lambda (cb)
                    (niwa-hooks-run-event
                     'PreToolUse
                     (niwa-hooks-event-plist
                      'PreToolUse session nil
                      :tool-name "Bash" :tool-input '(:command "echo hi"))
                     cb session)))))
            (should (eq 'deny (plist-get decision :permission-decision)))
            (should (string-match-p
                     "another execution target"
                     (plist-get decision :permission-reason)))
            (should-not (file-exists-p marker))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/plugin-command-env
  (:quiet t :doc "runs plugin command hooks with compatibility env and creates data dir")
  (let* ((root (make-temp-file "niwa-hooks-plugin-env-ws" t))
         (user-dir (file-name-as-directory
                    (make-temp-file "niwa-hooks-plugin-env-user" t)))
         (plugin-root (file-name-as-directory
                       (file-name-concat user-dir ".agents" "plugins" "repo")))
         (niwa-user-dir user-dir)
         (niwa-plugin-install-directory
          (file-name-concat user-dir ".agents" "plugins"))
         (process-environment (copy-sequence process-environment))
         (session (niwa-hooks-test--session root))
         (workspace (niwa-session-workspace session))
         (data-dir (file-name-concat root ".niwa"
                                     "state" "plugin-data" "demo"))
         (env-command
          (niwa-hooks-test--emacs-command
           "(princ (format \"{\\\"systemMessage\\\":\\\"%s|%s|%s|%s|%s|%s\\\"}\" (getenv \"PLUGIN_ROOT\") (getenv \"CLAUDE_PLUGIN_ROOT\") (getenv \"PLUGIN_DATA\") (getenv \"CLAUDE_PLUGIN_DATA\") (getenv \"NIWA_PLUGIN_ROOT\") (getenv \"NIWA_PLUGIN_DATA\")))")))
    (unwind-protect
        (progn
          (niwa-hooks-test--clear-plugin-env)
          (make-directory (file-name-concat plugin-root "hooks") t)
          (with-temp-file (file-name-concat plugin-root "hooks" "hooks.json")
            (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                    "\"hooks\":[{\"type\":\"command\",\"command\":"
                    (json-encode-string env-command)
                    "}]}]}}"))
          (niwa-hooks-test--write-plugin-manifest
           plugin-root
           "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (niwa-plugins-enable "demo" workspace))
          (let ((decision
                 (niwa-hooks-test--await
                  (lambda (cb)
                    (niwa-hooks-run-event
                     'PreToolUse
                     '(:tool-name "Bash" :tool-input (:command "echo hi"))
                     cb session)))))
            ;; A failed or unparsed run carries its reason in the decision.
            (ert-info ((format "Decision: %S" decision))
              (should
               (equal (mapconcat #'identity
                                 (list plugin-root
                                       plugin-root
                                       data-dir
                                       data-dir
                                       plugin-root
                                       data-dir)
                                 "|")
                      (plist-get decision :system-message))))
            (should (file-directory-p data-dir)))
          (niwa-plugins-disable "demo" workspace)
          (let* ((clean-command
                  (niwa-hooks-test--emacs-command
                   "(princ (if (or (getenv \"PLUGIN_ROOT\") (getenv \"CLAUDE_PLUGIN_ROOT\") (getenv \"PLUGIN_DATA\") (getenv \"CLAUDE_PLUGIN_DATA\") (getenv \"NIWA_PLUGIN_ROOT\") (getenv \"NIWA_PLUGIN_DATA\")) \"{\\\"systemMessage\\\":\\\"leaked\\\"}\" \"{\\\"systemMessage\\\":\\\"clean\\\"}\"))"))
                 (niwa-hook-rules
                  `((PreToolUse
                     ((:matcher "Bash"
                       :hooks ((:type command
                                :command ,clean-command))))))))
            (let ((decision
                   (niwa-hooks-test--await
                    (lambda (cb)
                      (niwa-hooks-run-event
                       'PreToolUse
                       '(:tool-name "Bash" :tool-input (:command "echo hi"))
                       cb session)))))
              (should (equal "clean"
                             (plist-get decision :system-message))))))
      (delete-directory root t)
      (delete-directory user-dir t))))

(niwa-deftest niwa-hooks-run-event/command-continuation-buffer
		 (:quiet t :doc "resumes later Elisp handlers in the original dispatch buffer")
		 (let* ((root (make-temp-file "niwa-hooks-continuation" t))
			(session (niwa-hooks-test--session root))
			(niwa-hooks-test--seen-buffer nil)
			(niwa-hook-rules
			 `((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type command
						      :command
						      ,(niwa-hooks-test--emacs-command
							"(princ \"{\\\"systemMessage\\\":\\\"ok\\\"}\")")
						      :timeout 5)
					       (:type elisp
						      :function
						      niwa-hooks-test--buffer-fn))))))))
		   (unwind-protect
		       (with-temp-buffer
			 (let ((dispatch-buffer (current-buffer)))
			   (niwa-hooks-test--await
			    (lambda (cb)
			      (niwa-hooks-run-event
			       'PostToolUse
			       '(:tool-name "Read" :result "ok")
			       cb session)))
			   (should (eq niwa-hooks-test--seen-buffer
				       dispatch-buffer))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/early-closed-stdin
  (:quiet t :doc "a command closing stdin before input delivery cannot kill Emacs")
  (let* ((root (make-temp-file "niwa-hook-closed-input-" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules
          '((PreToolUse
             ((:hooks ((:type command :command "exec 0<&-; sleep .5; exit 1"
                        :fail-closed t))))))))
    (unwind-protect
        (cl-letf* ((send (symbol-function 'process-send-string))
                   ((symbol-function 'process-send-string)
                    (lambda (process text)
                      (when (equal (process-name process) "niwa-hook")
                        ;; Let a fast command close stdin before its event arrives.
                        (sleep-for .1))
                      (funcall send process text))))
          (should
           (equal (niwa-hooks-test--await
                   (lambda (callback)
                     (niwa-hooks-run-event
                      'PreToolUse '(:tool-name "Bash") callback session)))
                  '(:continue nil :stop-reason "Hook exited with status 1"))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/fail-closed
		 (:quiet t :doc "command failures fail open by default and fail closed when requested")
		 (let* ((root (make-temp-file "niwa-hooks-fail" t))
			(session (niwa-hooks-test--session root)))
		   (unwind-protect
		       (let* ((open-decision
			       (let ((niwa-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"))))))))
				 (niwa-hooks-test--await
				  (lambda (cb)
				    (niwa-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session)))))
			      (closed-decision
			       (let ((niwa-hook-rules
				      '((PreToolUse
					 ((:matcher "Bash"
						    :hooks ((:type command
								   :command "exit 1"
								   :fail-closed t))))))))
				 (niwa-hooks-test--await
				  (lambda (cb)
				    (niwa-hooks-run-event
				     'PreToolUse '(:tool-name "Bash") cb session))))))
			 (should-not open-decision)
			 (should (equal closed-decision
					'(:continue nil
						    :stop-reason "Hook exited with status 1"))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-dry
		 (:doc "reports matching hooks without executing them")
		 (let* ((root (make-temp-file "niwa-hooks-dry" t))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type command :command "exit 99")
					       (:type elisp
						      :function niwa-hooks-test--deny-fn)))
			     (:matcher "Read"
				       :hooks ((:type command :command "exit 99"))))))))
		   (unwind-protect
		       (let ((result
			      (niwa-hooks-run-dry
			       'PreToolUse
			       '(:tool-name "Bash")
			       session)))
			 (should (eq (plist-get result :event) 'PreToolUse))
			 (should (equal (plist-get result :matcher-target) "Bash"))
			 (should (= (plist-get result :handler-count) 2))
			 (should-not (niwa-session-hook-log session)))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-log-persistence
		 (:doc "persists sanitized hook log entries under the session directory")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-hooks-log" t)))
			(session (niwa-hooks-test--session root))
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Bash"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--rewrite-fn))))))))
		   (unwind-protect
		       (progn
			 (setf (niwa-session-save-path session) root)
			 (niwa-hooks-test--await
			  (lambda (cb)
			    (niwa-hooks-run-event
			     'PreToolUse '(:tool-name "Bash") cb session)))
			 (let ((file (niwa-hooks-log-path session)))
			   (should (file-readable-p file))
			   (with-temp-buffer
			     (insert-file-contents file)
			     (goto-char (point-min))
			     (let ((entry (read (current-buffer))))
			       (should (eq (plist-get entry :event) 'PreToolUse))
			       (should (eq (plist-get entry :status) 'ok))))))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-surface-blocking-decision
		 (:doc "surfaces blocking hook decisions and system messages to the user")
  ,test
  (test)
		 (let* ((root (make-temp-file "niwa-hooks-surface" t))
			(session (niwa-hooks-test--session root))
			(messages nil)
			(niwa-hooks-slow-threshold nil)
			(niwa-hook-rules
			 '((PreToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--deny-fn))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (niwa-hooks-test--await
			  (lambda (cb)
			    (niwa-hooks-run-event
			     'PreToolUse '(:tool-name "Read") cb session)))
			 (should (member (format "niwa [%s]: PreToolUse hook blocked: blocked" root)
					 messages)))
		     (delete-directory root t)))
		 (let* ((root (make-temp-file "niwa-hooks-surface" t))
			(session (niwa-hooks-test--session root))
			(messages nil)
			(niwa-hooks-slow-threshold nil)
			(niwa-hook-rules
			 '((PostToolUse
			    ((:matcher "Read"
				       :hooks ((:type elisp
						      :function niwa-hooks-test--notify))))))))
		   (unwind-protect
		       (cl-letf (((symbol-function 'niwa-hooks-test--notify)
                          (lambda (_event)
                            '(:system-message "formatted result")))
                         ((symbol-function 'message)
				  (lambda (fmt &rest args)
				    (push (apply #'format fmt args) messages))))
			 (niwa-hooks-test--await
			  (lambda (cb)
			    (niwa-hooks-run-event
			     'PostToolUse '(:tool-name "Read") cb session)))
			 (should (member "niwa: PostToolUse hook: formatted result"
					 messages)))
		     (delete-directory root t))))

(niwa-deftest niwa-hooks-run-event/slow-progress (:quiet t)
  ,test
  (test)
  :doc "slow progress restores request status without disturbing composer or agent"
  (let* ((root (make-temp-file "niwa-hooks-progress" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold 1)
         (niwa-view-spinner-animate nil)
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Read"
               :hooks ((:type elisp :function ignore)))))))
         slow-callback handler-finish hook-result)
    (unwind-protect
        (niwa-view-test--with-buffers
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/worker"
                            (niwa-agent-record--create
                             :id "worker" :path "/root/worker"
                             :parent-path "/root" :activity 'running))))
          (with-current-buffer data-buf
            (setq-local niwa--session session)
            (setq-local niwa--current-request
                        (niwa-request--create
                         :session session :started-at (current-time))))
          (with-current-buffer view-buf
            (niwa-view--start-spinner "Working...")
            (niwa-view-test--insert-composer-draft
             "> quoted\nsecond line" 4))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq slow-callback
                             (lambda () (apply function args)))
                       nil))
                    ((symbol-function 'niwa-hooks--run-handlers)
                     (lambda (_event _handlers _payload _session _request
                              _context callback _dispatch-buffer)
                       (setq handler-finish callback))))
            (with-current-buffer data-buf
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Read")
               (lambda (decision) (setq hook-result decision)) session))
            (should slow-callback)
            (funcall slow-callback)
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))
              (let ((rail (niwa-view-test--rail-text)))
                (should (string-match-p "Running PreToolUse hook" rail))
                ;; Agents are counted by the background-work panel, not the rail.
                (should-not (string-match-p "agent" rail))))
            (funcall handler-finish nil)
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))
              (should (equal "Working..." niwa-view--spinner-status))
              (should (eq 'request niwa-view--spinner-owner)))
            (should-not hook-result)))
      (delete-directory root t)))

  :doc "announces completion after the slow notice, and only then"
  (let* ((root (make-temp-file "niwa-hooks-slow-echo" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold 1)
         (niwa-hook-rules
          '((Stop ((:matcher "*"
                    :hooks ((:type elisp :function ignore)))))))
         surfaced slow-callback handler-finish)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest args)
                     (setq slow-callback
                           (lambda () (apply function args)))
                     nil))
                  ((symbol-function 'niwa-hooks--run-handlers)
                   (lambda (_event _handlers _payload _session _request
                            _context callback _dispatch-buffer)
                     (setq handler-finish callback)))
                  ((symbol-function 'niwa-hooks--surface)
                   (lambda (text) (push text surfaced))))
          ;; The slow notice is a bare echo; completion must overwrite it
          ;; so a lingering notice reliably means the hook never settled.
          (niwa-hooks-run-event
           'Stop '(:status "completed") #'ignore session)
          (funcall slow-callback)
          (should (cl-some (lambda (text)
                             (string-match-p "still running" text))
                           surfaced))
          (funcall handler-finish nil)
          (should (cl-some (lambda (text)
                             (string-match-p "hook finished" text))
                           surfaced))
          ;; A hook that settles before the threshold stays silent.
          (setq surfaced nil slow-callback nil handler-finish nil)
          (niwa-hooks-run-event
           'Stop '(:status "completed") #'ignore session)
          (funcall handler-finish nil)
          (should-not (cl-some (lambda (text)
                                 (string-match-p "hook finished" text))
                               surfaced)))
      (delete-directory root t)))

  :doc "normal events without handlers create neither telemetry nor progress"
  (let* ((root (make-temp-file "niwa-hooks-no-progress" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold 1)
         (niwa-hook-rules nil)
         telemetry result)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _args)
                     (ert-fail "Hook without handlers scheduled progress")))
                  ((symbol-function 'niwa-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) telemetry)
                     'span))
                  ((symbol-function 'niwa-telemetry-finish)
                   (lambda (_span &rest props)
                     (push (cons 'finish props) telemetry))))
          (niwa-hooks-run-event
           'PreToolUse '(:tool-name "Read")
           (lambda (decision) (setq result decision)) session)
          (should-not result)
          (should-not telemetry))
      (delete-directory root t)))

  :doc "profiler events retain aggregate spans without handlers"
  (let* ((root (make-temp-file "niwa-hooks-profiled-empty" t))
         (session (niwa-hooks-test--session root))
         (niwa-hook-rules nil)
         telemetry)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-telemetry-detailed-p)
                   (lambda (_session) t))
                  ((symbol-function 'niwa-telemetry-start)
                   (lambda (_session event &rest props)
                     (push (cons event props) telemetry)
                     'span))
                  ((symbol-function 'niwa-telemetry-finish)
                   (lambda (_span &rest props)
                     (push (cons 'finish props) telemetry))))
          (niwa-hooks-run-event
           'PreToolUse '(:tool-name "Read") #'ignore session)
          (should (equal 0
                         (plist-get (cdr (assq 'hook-event telemetry))
                                    :handler-count)))
          (should (assq 'finish telemetry)))
      (delete-directory root t)))

  :doc "a concurrent hook cannot lease another hook's temporary status"
  (let ((niwa-view-spinner-animate nil))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (niwa-view--start-spinner "Working...")
        (niwa-view--update-spinner
         "Running PreToolUse hook..." (gensym "hook-progress-")))
      (with-current-buffer data-buf
        (should-not (niwa-hooks--progress-snapshot))))))

(defun niwa-hooks-test--slow-events (prepare &optional before-slow)
  "Run a slow PreToolUse hook and return (EVENTS OWNER STATUS).
PREPARE runs in the view buffer before the event; BEFORE-SLOW, when
given, runs there after the event starts and before the slow notice.  EVENTS
is the ordered list of status-transition and slow-status telemetry."
  (let* ((root (make-temp-file "niwa-hooks-slow-events" t))
         (session (niwa-hooks-test--session root))
         (niwa-hooks-slow-threshold 1)
         (niwa-view-spinner-animate nil)
         (niwa-hook-rules
          '((PreToolUse
             ((:matcher "Read"
               :hooks ((:type elisp :function ignore)))))))
         slow-callback handler-finish records)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session))
          (with-current-buffer view-buf (funcall prepare))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq slow-callback
                             (lambda () (apply function args)))
                       nil))
                    ((symbol-function 'niwa-hooks--run-handlers)
                     (lambda (_event _handlers _payload _session _request
                              _context callback _dispatch-buffer)
                       (setq handler-finish callback)))
                    ((symbol-function 'niwa-telemetry-record)
                     (lambda (_session event &rest props)
                       (when (memq event '(status-transition
                                           hook-slow-status-acquired
                                           hook-slow-status-released))
                         (push (pcase event
                                 ('status-transition
                                  (list event
                                        (and (plist-get props :owner) t)
                                        (plist-get props :status)))
                                 ('hook-slow-status-released
                                  (list event (plist-get props :restored)))
                                 (_ (list event)))
                               records)))))
            (with-current-buffer data-buf
              (niwa-hooks-run-event
               'PreToolUse '(:tool-name "Read") #'ignore session))
            (when before-slow
              (with-current-buffer view-buf (funcall before-slow)))
            (funcall slow-callback)
            (funcall handler-finish nil)
            (with-current-buffer view-buf
              (list (nreverse records)
                    niwa-view--spinner-owner
                    niwa-view--spinner-status))))
      (delete-directory root t))))

(niwa-deftest niwa-hooks-slow-progress-sequence (:quiet t)
  ,test
  (test)
  :doc "request-owned progress is claimed, then restored, in telemetry order"
  (let ((result (niwa-hooks-test--slow-events
                 (lambda () (niwa-view--start-spinner "Working...")))))
    (should (equal '((status-transition t "Running PreToolUse hook...")
                     (hook-slow-status-acquired)
                     (status-transition t "Working...")
                     (hook-slow-status-released t))
                   (nth 0 result)))
    (should (equal "Working..." (nth 2 result)))
    (should (eq 'request (nth 1 result))))

  :doc "an idle view is never claimed"
  (should (equal '(nil nil nil)
                 (niwa-hooks-test--slow-events #'ignore)))

  :doc "a foreign owner's status is neither claimed nor restored"
  (let ((result (niwa-hooks-test--slow-events
                 (lambda ()
                   (niwa-view--start-spinner "Working...")
                   (niwa-view--update-spinner
                    "Other" (gensym "other-"))))))
    (should (equal "Other" (nth 2 result)))
    (should-not (assq 'hook-slow-status-acquired (nth 0 result))))

  :doc "a stale generation refuses the claim"
  (let ((result (niwa-hooks-test--slow-events
                 (lambda () (niwa-view--start-spinner "Working..."))
                 (lambda () (niwa-view--start-spinner "Newer")))))
    (should-not (assq 'hook-slow-status-acquired (nth 0 result)))
    (should (equal "Newer" (nth 2 result)))))

(provide 'test-niwa-hooks)
;;; test-niwa-hooks.el ends here
