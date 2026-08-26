;;; test-mevedel-presets.el --- Tests for mevedel-presets.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mevedel-permission-rules)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-system)
(require 'mevedel-reminders)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-code)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-repair)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-task)
(require 'mevedel-tool-introspect)
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'mevedel-compact)
(require 'mevedel-presets)

;; `gptel'
(defvar gptel--known-presets)
(defvar gptel-request--transitions)
(defvar gptel-test-gptel-setting nil)

;; Test variables
(defvar mevedel--test-private-setting nil)
(defvar mevedel-test-setting nil)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; FSM handler chain builder

(mevedel-deftest mevedel-preset--build-transitions ()
  ,test
  (test)

  :doc "continues a final response through WAIT only for pending steering"
  (let* ((transitions
          (mevedel-preset--build-transitions
           '((INIT . ((t . WAIT)))
             (WAIT . ((t . TYPE)))
             (TYPE . ((error-p . ERRS)
                      (tool-p . TPRE)
                      (t . DONE))))))
         (rules (cdr (assq 'TYPE transitions))))
    (should
     (equal '(error-p tool-p mevedel-tools--pending-steering-p t)
            (mapcar #'car rules)))
    (should (eq 'WAIT
                (cdr (assq #'mevedel-tools--pending-steering-p rules))))))

(mevedel-deftest mevedel-preset--build-handlers
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "adds deferred inject to WAIT and extra handlers to terminal states"
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((err . ERRS)
                     (tool . TOOL)
                     (t . DONE)))
            (TOOL . ((t . TRET)))
            (TRET . ((err . ERRS)
                     (tool-result . WAIT)
                     (t . DONE)))))
         (base-handlers '((WAIT handler-wait)
                           (TYPE handler-type)
                           (TPRE handler-tpre)
                           (DONE handler-done)
                           (ERRS handler-errs)))
         (result (mevedel-preset--build-handlers
                  (copy-tree base-handlers))))
    ;; WAIT entry should have deferred inject handler prepended
    (should (memq #'mevedel-tools--handle-deferred-inject
                  (cdr (assq 'WAIT result))))
    (should (memq #'mevedel-compact-estimation-record-token-baseline
                  (cdr (assq 'TPRE result))))
    ;; DONE exposes one canonical named transaction; failure cleanup stays
    ;; on ERRS and ABRT without installing the successful entry point.
    (let ((done-handlers (cdr (assq 'DONE result)))
          (errs-handlers (cdr (assq 'ERRS result)))
          (abrt-handlers (cdr (assq 'ABRT result))))
      (should (> (length done-handlers) 1))
      (should (> (length errs-handlers) 1))
      (should (eq (car (last done-handlers)) #'mevedel--complete-turn))
      (should (= 1 (cl-count #'mevedel--complete-turn done-handlers)))
      (should-not (memq #'mevedel--complete-turn errs-handlers))
      (should-not (memq #'mevedel--complete-turn abrt-handlers))))

  :doc "wraps gptel wait after request-scoped WAIT handlers"
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((t . DONE)))))
         (base-handlers `((WAIT ,#'gptel--handle-wait after-wait)
                          (DONE)))
         (result (mevedel-preset--build-handlers
                  (copy-tree base-handlers)))
         (wait-handlers (cdr (assq 'WAIT result)))
         (gate-pos (cl-position #'mevedel--compact-handle-wait
                                wait-handlers))
         (begin-pos (cl-position-if
                     (lambda (handler)
                       (and (functionp handler)
                            (not (symbolp handler))))
                     wait-handlers))
         (message-pos (cl-position #'mevedel-tools--handle-message-inject
                                   wait-handlers))
         (reminder-pos (cl-position #'mevedel-reminders--handle-inject
                                    wait-handlers))
         (roster-pos
          (cl-position #'mevedel-tools--handle-agent-roster-inject
                       wait-handlers))
         (deferred-pos (cl-position #'mevedel-tools--handle-deferred-inject
                                    wait-handlers)))
    (should-not (memq #'gptel--handle-wait wait-handlers))
    (should gate-pos)
    (should (equal (nth (1+ gate-pos) wait-handlers) 'after-wait))
    (should (< reminder-pos roster-pos message-pos))
    (dolist (pos (list begin-pos reminder-pos roster-pos message-pos
                       deferred-pos))
      (should pos)
      (should (< pos gate-pos))))

  :doc "installs immutable read-only denial rules on discussion requests"
  (mevedel-tools-register)
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "read-only" :root "/tmp"
                     :name "read-only"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *discussion-request-rules*"))
         (handlers
          (mevedel-preset--build-handlers
           `((WAIT ,#'gptel--handle-wait) (DONE) (ERRS) (ABRT))))
         (begin-handler (car (cdr (assq 'WAIT handlers))))
         (fsm (gptel-make-fsm :info (list :buffer buffer))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session
                      mevedel--current-directive-uuid nil
                      mevedel--directive-read-only-request-p t)
          (cl-letf (((symbol-function 'mevedel-goal-capture-request)
                     #'ignore)
                    ((symbol-function 'mevedel-skills--drain-pending-context)
                     #'ignore))
            (funcall begin-handler fsm))
          (let ((rules
                 (mevedel-request-skill-permission-rules
                  mevedel--current-request)))
            (should (eq 'deny
                        (mevedel-permission-rules-action
                         rules "ApplyPatch")))
            (should (eq 'deny
                        (mevedel-permission-rules-action rules "Agent")))
            (should-not
             (mevedel-permission-rules-action rules "Read"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (mevedel-request-end))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel--add-termination-handler
  (:doc "adds a terminal handler to graph terminals and explicit abort")
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . DONE)))))
         (result
          (mevedel--add-termination-handler
           #'ignore '((DONE done) (ABRT abort)))))
    (should (equal '(done ignore) (cdr (assq 'DONE result))))
    (should (equal '(abort ignore) (cdr (assq 'ABRT result))))))



;;
;;; Final patch handler

(mevedel-deftest mevedel-preset--final-patch-handler
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "skips patch generation when the request buffer has no workspace"
  (let ((chat-buf (generate-new-buffer " *mevedel-no-workspace*"))
        (generated 0))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-workspace)
                   (lambda (&optional _buffer) nil))
                  ((symbol-function 'mevedel--generate-final-patch)
                   (lambda (&optional _workspace)
                     (cl-incf generated)
                     "diff")))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer chat-buf))))
            (mevedel-preset--final-patch-handler fsm))
          (should (= 0 generated)))
      (kill-buffer chat-buf)))

  :doc "captures patch coverage on the FSM and displays a non-empty patch"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (chat-buf (generate-new-buffer " *mevedel-workspace*"))
         generated capture-request
         replaced)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--current-directive-uuid nil))
          (cl-letf (((symbol-function 'mevedel-workspace)
                     (lambda (&optional _buffer) ws))
                    ((symbol-function 'mevedel--generate-final-patch)
                     (lambda (workspace)
                       (setq generated workspace)
                       "diff --git a/file b/file\n"))
                    ((symbol-function 'mevedel--directive-capture)
                     (lambda (request)
                       (setq capture-request request)
                       '(:capture complete
                         :covered-files ("/tmp/p/file")
                         :gaps nil)))
                    ((symbol-function 'mevedel--replace-patch-buffer)
                     (lambda (patch)
                       (setq replaced patch))))
            (let* ((request (mevedel-request--create))
                   (fsm (gptel-make-fsm
                         :info (list :buffer chat-buf
                                     :mevedel-request request))))
              (mevedel-preset--final-patch-handler fsm)
              (should (equal "diff --git a/file b/file\n"
                             (plist-get (gptel-fsm-info fsm)
                                        :mevedel-directive-patch)))
              (should (eq 'complete
                          (plist-get (gptel-fsm-info fsm)
                                     :mevedel-directive-capture)))
              (should (equal '("/tmp/p/file")
                             (plist-get (gptel-fsm-info fsm)
                                        :mevedel-directive-covered-files)))
              (should (eq request capture-request))))
          (should (eq ws generated))
          (should (equal "diff --git a/file b/file\n" replaced)))
      (kill-buffer chat-buf))))


;;
;;; mevedel-define-preset macro

(mevedel-deftest mevedel--define-presets
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "keeps Bash lifecycle controls active in implementation presets"
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets nil))
    (mevedel-tools-register)
    (mevedel--define-presets)
    (dolist (preset '(mevedel-implement))
      (let* ((metadata (mevedel-preset--resolved-metadata preset))
             (resolved
              (mevedel-tool-resolve (plist-get metadata :tool-specs)))
             (active
              (mapcar #'mevedel-tool-name (plist-get resolved :active))))
        (dolist (name '("Bash" "WriteStdin" "ListExecutions"
                        "StopExecution"))
          (should (member name active))))))

  :doc "makes directive discussion a hard read-only capability"
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets nil))
    (mevedel-tools-register)
    (mevedel--define-presets)
    (let* ((metadata (mevedel-preset--resolved-metadata 'mevedel-discuss))
           (resolved
            (mevedel-tool-resolve (plist-get metadata :tool-specs)))
           (tools (plist-get resolved :active))
           (names (mapcar #'mevedel-tool-name tools)))
      (should (cl-every #'mevedel-tool-read-only-p tools))
      (dolist (name '("Agent" "FollowupAgent" "SendMessage"
                      "Bash" "Eval" "ApplyPatch"))
        (should-not (member name names)))
      (should-not (plist-get metadata :agents))
      (let ((rules (mevedel-preset--read-only-rules)))
        (dolist (name '("Agent" "FollowupAgent" "SendMessage"
                        "Bash" "Eval" "ApplyPatch"))
          (should (eq 'deny
                      (mevedel-permission-rules-action rules name))))
        (should-not
         (mevedel-permission-rules-action rules "Read")))))

  :doc "an explicit empty :agents declaration registers no agents"
  ;; `plist-get' on the declared-empty list is nil, and nil used to mean
  ;; "no filter": `mevedel-discuss' declared none and installed all.
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets nil))
    (mevedel-tools-register)
    (mevedel--define-presets)
    (with-temp-buffer
      (mevedel-agents--setup-for-request 'mevedel-discuss)
      (should-not (mevedel-agents-specs)))
    (with-temp-buffer
      (mevedel-agents--setup-for-request 'mevedel-implement)
      (should (mevedel-agents-specs))))

  :doc "makes every built-in role available to model-facing Agent calls"
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets nil))
    (mevedel-tools-register)
    (mevedel--define-presets)
    (with-temp-buffer
      (mevedel-agents--setup-for-request 'mevedel-implement)
      (let* ((tool (gptel-get-tool '("mevedel" "Agent")))
             (validation-tool (mevedel-tool-get "Agent"))
             (role-arg
              (cl-find-if
               (lambda (arg) (equal (plist-get arg :name) "role"))
               (gptel-tool-args tool)))
             (roles (append (plist-get role-arg :enum) nil)))
        (dolist (role '("worker" "explorer" "reviewer" "verifier"))
          (should (member role roles))
          (should-not
           (mevedel-tool-repair-validate
            validation-tool
            (list :task_name "role_probe"
                  :message "Inspect only."
                  :role role)))))))

  :doc "selects the main prompt with no revision preset"
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets nil))
    (mevedel-tools-register)
    (mevedel--define-presets)
    (let ((main (funcall (plist-get
                          (gptel-get-preset 'mevedel-discuss)
                          :system))))
      (should (string-match-p "Task execution protocol" main))
      (should-not (gptel-get-preset 'mevedel-revise)))))

(mevedel-deftest mevedel-preset--variable-for-key
  ()
  ,test
  (test)
  :doc "prefers public then private mevedel variables before gptel"
  (should (eq 'mevedel-test-setting
              (mevedel-preset--variable-for-key :test-setting)))
  (should (eq 'mevedel--test-private-setting
              (mevedel-preset--variable-for-key :test-private-setting)))
  :doc "returns nil when no matching variable exists"
  (should-not
   (mevedel-preset--variable-for-key :test-certainly-not-defined)))

(mevedel-deftest mevedel-define-preset
  (:before-each (mevedel-tool-clear-registry)
   :after-each (progn
                 (mevedel-tool-clear-registry)
                 (setq mevedel-preset--registry nil)
                 (dolist (name '(test-preset test-parent-a test-parent-b
                                 test-child test-session-a test-session-b))
                   (setq gptel--known-presets
                         (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)

  :doc "Registers in both gptel and mevedel registries"
  (progn
    (mevedel-define-tool
      :name "TestRead"
      :handler #'ignore
      :description "Read"
      :groups (testgrp))
    (mevedel-define-preset test-preset
      :description "A test preset"
      :tools (testgrp)
      :agents (explorer verifier)
      :system "Test system prompt")
    ;; Registered in gptel
    (should (assq 'test-preset gptel--known-presets))
    ;; Registered in mevedel registry
    (let ((meta (alist-get 'test-preset mevedel-preset--registry)))
      (should meta)
      (should (equal '(explorer verifier) (plist-get meta :agents)))
      (should (equal '(testgrp) (plist-get meta :tool-specs)))))

  :doc "Resolves tools to gptel-tool structs"
  (progn
    (mevedel-define-tool
      :name "TestA"
      :handler #'ignore
      :description "Tool A"
      :groups (mygroup))
    (mevedel-define-tool
      :name "TestB"
      :handler #'ignore
      :description "Tool B"
      :groups (mygroup))
    (mevedel-define-preset test-preset
      :description "Test"
      :tools (mygroup))
    (let* ((gptel-spec (alist-get 'test-preset gptel--known-presets))
           (tools (plist-get gptel-spec :tools)))
      ;; Should have 2 tools from the group
      (should (= 2 (length tools)))
      ;; Each should be a gptel-tool struct
      (should (cl-every #'gptel-tool-p tools))))

  :doc "Preset with no :system omits system from gptel spec"
  (progn
    (mevedel-define-tool
      :name "TestC"
      :handler #'ignore
      :description "C"
      :groups (grp))
    (mevedel-define-preset test-preset
      :description "No system"
      :tools (grp))
    (let ((gptel-spec (alist-get 'test-preset gptel--known-presets)))
      (should-not (plist-member gptel-spec :system))))

  :doc "uses exact names and preserves explicitly quoted parents"
  (progn
    (mevedel-define-preset test-parent-a :description "Parent")
    (mevedel-define-preset test-preset
      :parents '(test-parent-a))
    (should (equal '(test-parent-a)
                   (plist-get (gptel-get-preset 'test-preset) :parents))))

  :doc "routes ordinary keys to mevedel before gptel variables"
  (let ((mevedel-test-setting 'global-mevedel)
        (gptel-test-gptel-setting 'global-gptel))
    (mevedel-define-preset test-preset
      :test-setting 'session-mevedel
      :test-gptel-setting 'session-gptel)
    (let ((metadata (alist-get 'test-preset mevedel-preset--registry))
          (gptel-spec (gptel-get-preset 'test-preset)))
      (should (equal '((mevedel-test-setting . session-mevedel))
                     (plist-get metadata :settings)))
      (should (eq 'session-gptel
                  (plist-get gptel-spec :test-gptel-setting)))))

  :doc "inherits agents and tools from the later parent then the child"
  (progn
    (mevedel-define-tool
      :name "ParentA"
      :handler #'ignore
      :description "Parent A"
      :groups (parent-a))
    (mevedel-define-tool
      :name "ParentB"
      :handler #'ignore
      :description "Parent B"
      :groups (parent-b))
    (mevedel-define-preset test-parent-a
      :tools (parent-a)
      :agents (explorer))
    (mevedel-define-preset test-parent-b
      :tools (parent-b)
      :agents (verifier))
    (mevedel-define-preset test-child
      :parents (test-parent-a test-parent-b))
    (let ((metadata (mevedel-preset--resolved-metadata 'test-child)))
      (should (equal '(parent-b) (plist-get metadata :tool-specs)))
      (should (equal '(verifier) (plist-get metadata :agents))))))

(mevedel-deftest mevedel-preset-resolve-settings
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-parent-c test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "supports replacement, prepend, append, and function semantics"
  (let ((mevedel-test-setting '(base)))
    (mevedel-define-preset test-parent-a
      :test-setting '(parent))
    (mevedel-define-preset test-parent-b
      :test-setting '(:prepend (before)))
    (mevedel-define-preset test-parent-c
      :test-setting '(:append (after)))
    (mevedel-define-preset test-child
      :parents (test-parent-a test-parent-b test-parent-c)
      :test-setting '(:function (lambda (value) (append value '(function)))))
    (should (equal '(before parent after function)
                   (alist-get 'mevedel-test-setting
                              (mevedel-preset-resolve-settings
                               'test-child)))))
  :doc "merges preset-local tier and workload maps by entry"
  (let ((mevedel-model-tiers '((balanced)))
        (mevedel-model-workloads '((planning :tier balanced))))
    (mevedel-define-preset test-parent-a
      :model-tiers ((fast :provider "Fast:fast-model"))
      :model-workloads ((implementation :tier fast)
                        ($plugin:code-review :tier fast)))
    (mevedel-define-preset test-parent-b
      :model-tiers ((strong :provider "Strong:strong-model"))
      :model-workloads ((review :tier strong)))
    (mevedel-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :model-workloads ((planning :tier strong)))
    (let ((settings (mevedel-preset-resolve-settings 'test-child)))
      (should (equal '(:provider "Fast:fast-model")
                     (alist-get 'fast
                                (alist-get 'mevedel-model-tiers settings))))
      (should (equal '(:provider "Strong:strong-model")
                     (alist-get 'strong
                                (alist-get 'mevedel-model-tiers settings))))
      (should (equal '(:tier strong)
                     (alist-get 'planning
                                (alist-get 'mevedel-model-workloads
                                           settings))))
      (should (equal '(:tier fast)
                     (alist-get '$plugin:code-review
                                (alist-get 'mevedel-model-workloads
                                           settings)))))))

(mevedel-deftest mevedel-preset--resolved-metadata
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "uses later parent metadata and then child metadata"
  (progn
    (mevedel-define-preset test-parent-a :agents (explorer))
    (mevedel-define-preset test-parent-b :agents (verifier))
    (mevedel-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :agents (reviewer))
    (should (equal '(reviewer)
                   (plist-get
                    (mevedel-preset--resolved-metadata 'test-child)
                    :agents)))))

(mevedel-deftest mevedel-preset--setting-specs
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "returns parent settings in declaration order before child settings"
  (progn
    (mevedel-define-preset test-parent-a :test-setting 'a)
    (mevedel-define-preset test-parent-b :test-setting 'b)
    (mevedel-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :test-setting 'child)
    (should (equal '((mevedel-test-setting . a)
                     (mevedel-test-setting . b)
                     (mevedel-test-setting . child))
                   (mevedel-preset--setting-specs 'test-child)))))

(mevedel-deftest mevedel-preset--apply-settings
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (setq gptel--known-presets
           (assq-delete-all 'test-preset gptel--known-presets))))
  ,test
  (test)
  :doc "stores persistent settings in the current buffer and session"
  (with-temp-buffer
    (setq-local mevedel--session (mevedel-session--create :name "test"))
    (mevedel-define-preset test-preset :test-setting 'local)
    (mevedel-preset--apply-settings 'test-preset)
    (should (eq 'local mevedel-test-setting))
    (should (local-variable-p 'mevedel-test-setting))
    (should (eq 'test-preset
                (mevedel-session-preset-name mevedel--session)))))

(mevedel-deftest mevedel-preset--refresh-tools
  (:after-each
   (progn
     (remhash '("mevedel" "TestWait") mevedel-tool--registry)
     (setq mevedel-preset--registry nil)
     (setq gptel--known-presets
           (assq-delete-all 'test-preset gptel--known-presets))))
  ,test
  (test)
  :doc "replaces frozen preset tools with the latest registered schema"
  ;; The refresh writes `gptel-tools' buffer-locally.  Let-binding the global
  ;; instead makes Emacs itself complain from C, where neither a capture nor
  ;; the mute list can reach it, so the case owns a buffer to be refreshed in.
  (with-temp-buffer
    (setq-local gptel-tools nil)
    (mevedel-define-tool
      :name "TestWait"
      :description "Old wait schema"
      :handler #'ignore
      :args ((execution_id string :required "Execution ID")
             (chars string :optional "Input")))
    (mevedel-define-preset test-preset
      :tools ((:tool "TestWait")))
    (setq-local gptel-tools
                (list (mevedel-tool-gptel-tool
                       (mevedel-tool-get "TestWait"))))
    (mevedel-define-tool
      :name "TestWait"
      :description "Current wait schema"
      :handler #'ignore
      :args ((execution_id string :required "Execution ID")
             (chars string :optional "Input")
             (yield_time_ms integer :optional "Wait")))
    (let ((latest
           (mevedel-tool-gptel-tool
            (mevedel-tool-get "TestWait"))))
      (should-not (eq latest (car gptel-tools)))
      (mevedel-preset--refresh-tools 'test-preset)
      (should (eq latest (car gptel-tools)))
      (should
       (equal '("exec-1" "" 30000)
              (gptel--map-tool-args
               (car gptel-tools)
               '(:execution_id "exec-1" :chars "" :yield_time_ms 30000)))))))

(mevedel-deftest mevedel-preset--post
  ()
  ,test
  (test)
  :doc "composes the user post hook with required mevedel setup"
  (let (calls)
    (cl-letf (((symbol-function 'mevedel-preset--apply-settings)
               (lambda (_) (push 'settings calls)))
              ((symbol-function 'mevedel-agents--setup-for-request)
               (lambda (_) (push 'agents calls)))
              ((symbol-function 'mevedel-preset--setup-deferred)
               (lambda (_) (push 'deferred calls)))
              ((symbol-function 'mevedel-preset--refresh-tools)
               (lambda (_) (push 'tools calls)))
              ((symbol-function 'mevedel-preset--setup-extras)
               (lambda (_) (push 'extras calls))))
      (mevedel-preset--post 'test-preset
                            (lambda () (push 'user calls))))
    (should (equal '(user settings tools agents deferred extras)
                   (nreverse calls)))))

(mevedel-deftest mevedel-preset--parent-chain-to
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-preset test-preset-parent test-preset-gptel))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "returns the chain from the start preset up to the named one"
  (progn
    (mevedel-preset--define 'test-preset '(:parents nil))
    (mevedel-preset--define 'test-preset-parent '(:parents (test-preset)))
    (should (equal '(test-preset-parent test-preset)
                   (mevedel-preset--parent-chain-to
                    'test-preset 'test-preset-parent)))
    (should-not (mevedel-preset--parent-chain-to
                 'test-preset-parent 'test-preset)))

  :doc "terminates when the presets walked already hold a cycle"
  ;; gptel registers parents without a cycle check, so the walk can be
  ;; handed one that mevedel never had the chance to reject.
  (progn
    (gptel-make-preset 'test-preset-gptel :parents '(test-preset-parent))
    (gptel-make-preset 'test-preset-parent :parents '(test-preset-gptel))
    (should-not (mevedel-preset--parent-chain-to
                 'test-preset 'test-preset-gptel))))

(mevedel-deftest mevedel-preset--define
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-preset test-preset-parent test-preset-base
                                 test-preset-left test-preset-right
                                 test-preset-gptel))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "rejects a parent cycle at definition time, leaving both registries intact"
  ;; `:parents' is a documented key, and the built-in `mevedel-implement'
  ;; already inherits from `mevedel-discuss', so pointing discuss back at
  ;; implement closes a cycle from one line of user configuration.  Resolution
  ;; is lazy, so the failure surfaces later as `excessive-lisp-nesting', which
  ;; names neither preset.
  (progn
    (mevedel-preset--define 'test-preset-parent '(:parents nil))
    (mevedel-preset--define 'test-preset '(:parents (test-preset-parent)))
    (let ((before (alist-get 'test-preset-parent mevedel-preset--registry))
          (before-gptel (gptel-get-preset 'test-preset-parent)))
      (let ((err (should-error
                  (mevedel-preset--define 'test-preset-parent
                                          '(:parents (test-preset)))
                  :type 'user-error)))
        (should (string-match-p "test-preset-parent" (error-message-string err)))
        (should (string-match-p "test-preset" (error-message-string err))))
      ;; A rejected definition must not have half-registered.
      (should (equal before
                     (alist-get 'test-preset-parent mevedel-preset--registry)))
      (should (equal before-gptel (gptel-get-preset 'test-preset-parent)))))

  :doc "rejects a preset naming itself as its own parent"
  (should-error (mevedel-preset--define 'test-preset '(:parents (test-preset)))
                :type 'user-error)

  :doc "rejects a cycle closed through a preset only gptel knows"
  ;; gptel registers a preset without a cycle check of its own, and its
  ;; own parent walk is equally unguarded, so a parent that never went
  ;; through mevedel still has to be followed.
  (progn
    (mevedel-preset--define 'test-preset '(:parents nil))
    (gptel-make-preset 'test-preset-gptel :parents '(test-preset))
    (should-error (mevedel-preset--define
                   'test-preset '(:parents (test-preset-gptel)))
                  :type 'user-error))

  :doc "accepts a diamond, which is not a cycle"
  (progn
    (mevedel-preset--define 'test-preset-base
                            '(:test-setting base))
    (mevedel-preset--define 'test-preset-left '(:parents (test-preset-base)))
    (mevedel-preset--define 'test-preset-right '(:parents (test-preset-base)))
    (mevedel-preset--define 'test-preset
                            '(:parents (test-preset-left test-preset-right)))
    ;; A shared ancestor reached by two paths is not a cycle: the
    ;; definition is accepted and resolution still terminates.
    (should (alist-get 'test-preset mevedel-preset--registry))
    (should (equal '((mevedel-test-setting . base)
                     (mevedel-test-setting . base))
                   (mevedel-preset--setting-specs 'test-preset))))

  :doc "warns and ignores an ordinary key without a matching variable"
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _)
                 (setq warning (cons type message)))))
      (mevedel-preset--define
       'test-preset '(:test-certainly-not-defined value)))
    (should (equal '(mevedel presets) (car warning)))
    (should (string-match-p "not found" (cdr warning)))))

(mevedel-deftest mevedel-preset-apply
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (dolist (name '(test-parent-a test-session-a test-session-b test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "keeps selected preset settings isolated between sessions"
  (let ((buffer-a (generate-new-buffer " *preset-a*"))
        (buffer-b (generate-new-buffer " *preset-b*")))
    (unwind-protect
        (progn
          (mevedel-define-preset test-session-a :test-setting 'a)
          (mevedel-define-preset test-session-b :test-setting 'b)
          (dolist (pair `((,buffer-a . test-session-a)
                          (,buffer-b . test-session-b)))
            (with-current-buffer (car pair)
              (setq-local mevedel--session
                          (mevedel-session--create :name "test"))
              (cl-letf (((symbol-function 'mevedel-agents--setup-for-request)
                         #'ignore)
                        ((symbol-function 'mevedel-preset--setup-deferred)
                         #'ignore)
                        ((symbol-function 'mevedel-preset--setup-extras)
                         #'ignore))
                (mevedel-preset-apply (cdr pair)))))
          (should (eq 'a (buffer-local-value 'mevedel-test-setting buffer-a)))
          (should (eq 'b (buffer-local-value 'mevedel-test-setting buffer-b)))
          (should (eq 'test-session-a
                      (mevedel-session-preset-name
                       (buffer-local-value 'mevedel--session buffer-a)))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b)))
  :doc "composes inherited ordinary settings exactly once when applied"
  (let ((mevedel-test-setting '(base)))
    (mevedel-define-preset test-parent-a
      :test-setting '(:append (parent)))
    (mevedel-define-preset test-child
      :parents (test-parent-a)
      :test-setting '(:append (child)))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session--create :name "test"))
      (cl-letf (((symbol-function 'mevedel-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-deferred)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-extras)
                 #'ignore))
        (mevedel-preset-apply 'test-child))
      (should (equal '(base parent child) mevedel-test-setting))
      (should (eq 'test-child
                  (mevedel-session-preset-name mevedel--session)))))
  :doc "stores resolved preset-local model policy on the session"
  (let ((mevedel-model-tiers '((balanced)))
        (mevedel-model-workloads nil))
    (mevedel-define-preset test-child
      :model-tiers ((strong :provider "Strong:strong-model" :effort high))
      :model-workloads ((review :tier strong)))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session--create :name "test"))
      (cl-letf (((symbol-function 'mevedel-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-deferred)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-extras)
                 #'ignore))
        (mevedel-preset-apply 'test-child))
      (should (equal '(:provider "Strong:strong-model" :effort high)
                     (alist-get 'strong mevedel-model-tiers)))
      (should (equal '(:tier strong)
                     (alist-get 'review mevedel-model-workloads)))
      (should (eq 'test-child
                  (mevedel-session-preset-name mevedel--session))))))

(mevedel-deftest mevedel-preset-restore-session
  (:doc "reapplies trusted gptel and mevedel preset settings from its name")
  ,test
  (test)
  (let ((mevedel-preset--registry nil)
        (gptel--known-presets (copy-tree gptel--known-presets)))
    (mevedel-define-preset test-preset
      :system "Trusted system prompt"
      :temperature 0.25
      :test-setting 'registered)
    (with-temp-buffer
      (let ((session
             (mevedel-session--create
              :name "test"
              :preset-name 'test-preset)))
        (cl-letf (((symbol-function 'mevedel-agents--setup-for-request)
                   #'ignore)
                  ((symbol-function 'mevedel-preset--setup-deferred)
                   #'ignore)
                  ((symbol-function 'mevedel-preset--setup-extras)
                   #'ignore))
          (mevedel-preset-restore-session session))
        (should (local-variable-p 'mevedel-test-setting))
        (should (eq 'registered mevedel-test-setting))
        (should (eq 'test-preset gptel--preset))
        (should (equal "Trusted system prompt" gptel-system-prompt))
        (should (= 0.25 gptel-temperature))
        (should-not (local-variable-p 'kill-buffer-hook))))))

(mevedel-deftest mevedel-with-preset
  (:after-each
   (progn
     (setq mevedel-preset--registry nil)
     (setq gptel--known-presets
           (assq-delete-all 'test-preset gptel--known-presets))))
  ,test
  (test)
  :doc "applies settings for one request without changing session state"
  (let ((mevedel-test-setting 'outside))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session--create :name "test"))
      (mevedel-define-preset test-preset :test-setting 'inside)
      (cl-letf (((symbol-function 'mevedel-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-deferred)
                 #'ignore)
                ((symbol-function 'mevedel-preset--setup-extras)
                 #'ignore))
        (mevedel-with-preset 'test-preset
          (should (eq 'inside mevedel-test-setting))))
      (should (eq 'outside mevedel-test-setting))
      (should-not (mevedel-session-preset-name mevedel--session))))
  :doc "delegates raw gptel preset specs"
  (let ((gptel-system-prompt "outside"))
    (mevedel-with-preset '(:system "inside")
      (should (equal "inside" gptel-system-prompt)))
    (should (equal "outside" gptel-system-prompt))))


;;
;;; Built-in agent definitions

(mevedel-deftest mevedel-agents--builtin
  (:before-each (progn (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ptc--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-introspect--register)
                       ;; Other deftests wipe `mevedel-agent--registry'
                       ;; in their :after-each; re-load to restore the
                       ;; built-in agent definitions.
                       (load (locate-library "mevedel-agents") nil t)))
  ,test
  (test)

  :doc "verifier agent is registered when mevedel-agents loads"
  (let ((agent (mevedel-agent-get "verifier")))
    (should agent)
    (should (equal "verifier" (mevedel-agent-name agent)))
    (should (= 20 (mevedel-agent-max-turns agent)))
    ;; Reminders are NOT embedded at definition time to avoid a
    ;; require cycle with mevedel-reminders; they are attached at
    ;; invocation time instead.
    (should (null (mevedel-agent-reminders agent))))

  :doc "verifier invocation gets the read-only reminder attached"
  (let* ((agent (mevedel-agent-get "verifier"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should (memq 'verifier-read-only types)))

  :doc "reviewer invocation gets the read-only reminder attached"
  (let* ((agent (mevedel-agent-get "reviewer"))
         (inv (mevedel-agent-invocation-create agent))
         (types (mapcar #'mevedel-reminder-type
                        (mevedel-agent-invocation-reminders inv))))
    (should (memq 'reviewer-read-only types)))

  :doc "explorer agent is registered and read-only"
  (should (mevedel-agent-get "explorer"))

  :doc "task-capable agents include the standalone task note tool"
  (dolist (name '("worker" "explorer"))
    (let ((agent (mevedel-agent-get name)))
      (should (member '(:tool "TaskNote")
                      (mevedel-agent-tools agent)))))

  :doc "planner agent is no longer a first-class built-in agent"
  (should-not (mevedel-agent-get "planner")))


;;
;;; mevedel-define-agent macro

(mevedel-deftest mevedel-define-agent
  (:before-each (mevedel-test--capture-agent-registry)
   :after-each (mevedel-test--restore-agent-registry))
  ,test
  (test)

  :doc "Creates struct and registers in registry"
  (progn
    (mevedel-define-agent test-analyst
      :description "A test agent"
      :tools (read code)
      :system-components
      '((role :text "Test analyst") workspace-config environment)
      :max-turns 20)
    (let ((agent (mevedel-agent-get "test-analyst")))
      (should agent)
      (should (equal "test-analyst" (mevedel-agent-name agent)))
      (should (equal "A test agent" (mevedel-agent-description agent)))
      (should (equal '(read code) (mevedel-agent-tools agent)))
      (should (functionp (mevedel-agent-system-prompt agent)))
      (should (string-match-p
               "Test analyst"
               (funcall (mevedel-agent-system-prompt agent))))
      (should (= 20 (mevedel-agent-max-turns agent)))))

  :doc "mevedel-agent-get accepts symbol or string"
  (progn
    (mevedel-define-agent my-agent
      :description "test"
      :tools (read))
    (should (mevedel-agent-get 'my-agent))
    (should (mevedel-agent-get "my-agent"))
    (should (eq (mevedel-agent-get 'my-agent)
                (mevedel-agent-get "my-agent"))))

  :doc "mevedel-agent-to-gptel-spec produces valid plist"
  (progn
    (mevedel-define-agent spec-agent
      :description "For spec test"
      :tools (read)
      :system-components
      '((role :text "system") workspace-config environment))
    (let* ((agent (mevedel-agent-get "spec-agent"))
           (spec (mevedel-agent-to-gptel-spec agent)))
      (should (equal "spec-agent" (car spec)))
      (should (stringp (plist-get (cdr spec) :description)))
      ;; :tools should be a (:function ...) spec
      (should (eq :function (car (plist-get (cdr spec) :tools))))
      ;; :system should be a (:function ...) spec
      (should (eq :function (car (plist-get (cdr spec) :system))))))

  :doc "stores :reminders list on the agent struct"
  (let ((r (mevedel-reminder-create
            :type 'note
            :trigger (lambda (_) t)
            :content (lambda (_) "x"))))
    (mevedel-define-agent reminder-agent
      :description "With reminders"
      :tools (read)
      :reminders (list r))
    (let ((agent (mevedel-agent-get "reminder-agent")))
      (should (equal (list r) (mevedel-agent-reminders agent)))))

  :doc "defaults :reminders to nil"
  (progn
    (mevedel-define-agent no-reminder-agent
      :description "Without reminders"
      :tools (read))
    (let ((agent (mevedel-agent-get "no-reminder-agent")))
      (should (null (mevedel-agent-reminders agent)))))

  :doc "stores agent-scoped hooks and normalizes Stop"
  (progn
    (mevedel-define-agent hook-agent
      :description "With hooks"
      :tools (read)
      :hooks ((Stop
               ((:matcher "*"
                 :hooks ((:type elisp
                          :function ignore)))))))
    (let* ((agent (mevedel-agent-get "hook-agent"))
           (rules (mevedel-agent-hook-rules agent))
           (invocation (mevedel-agent-invocation-create agent)))
      (should (eq 'SubagentStop (caar rules)))
      (should (equal rules
                     (mevedel-agent-invocation-hook-rules invocation))))))


(provide 'test-mevedel-presets)
;;; test-mevedel-presets.el ends here
