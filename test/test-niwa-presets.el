;;; test-niwa-presets.el --- Tests for niwa-presets.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-tool-exec-registration)

(require 'gptel)
(require 'gptel-anthropic)
(require 'niwa-permission-rules)
(require 'niwa-tool-registry)
(require 'niwa-tools)
(require 'niwa-system)
(require 'niwa-reminders)
(require 'niwa-tool-fs)
(require 'niwa-tool-code)
(require 'niwa-tool-exec)
(require 'niwa-tool-repair)
(require 'niwa-tool-ui)
(require 'niwa-tool-task)
(require 'niwa-tool-introspect)
(require 'niwa-agents)
(require 'niwa-agent-conversation)
(require 'niwa-hooks)
(require 'niwa-view)
(require 'niwa-workspace)
(require 'niwa-compact)
(require 'niwa-presets)
(require 'niwa-chat)
(require 'niwa-execution)
(require 'niwa-sandbox)
(require 'niwa-transport)
(require 'niwa-turn)
(require 'niwa-utilities)

;; `gptel'
(defvar gptel--known-presets)
(defvar gptel-request--transitions)
(defvar gptel-test-gptel-setting nil)

;; Test variables
(defvar niwa--test-private-setting nil)
(defvar niwa-test-setting nil)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; FSM handler chain builder

(defun niwa-presets-test--payload-agent-roles ()
  "Return the Agent role enum a dry-run request from this buffer would send."
  (let* ((data (plist-get (gptel-fsm-info (gptel-request "probe" :dry-run t))
                          :data))
         (agent (cl-find "Agent" (append (plist-get data :tools) nil)
                         :key (lambda (tool)
                                (or (plist-get tool :name)
                                    (plist-get (plist-get tool :function) :name)))
                         :test #'equal))
         (parameters (or (plist-get agent :parameters)
                         (plist-get (plist-get agent :function) :parameters))))
    (should agent)
    ;; gptel names schema properties with uninterned keywords.
    (cl-loop for (key value) on (plist-get parameters :properties) by #'cddr
             when (equal (symbol-name key) ":role")
             return (plist-get value :enum))))

(defun niwa-presets-test--guarded-wait-handlers (handlers)
  "Inspect WAIT HANDLERS behind their single preparation gate."
  (let ((fsm (gptel-make-fsm))
        (group (cdr (assq 'WAIT handlers)))
        enclosed)
    (should (= 1 (length group)))
    (cl-letf (((symbol-function 'niwa-preset--guard-preparation)
               (lambda (machine wait-handlers)
                 (should (eq machine fsm))
                 (setq enclosed wait-handlers))))
      (funcall (car group) fsm))
    (should enclosed)
    enclosed))

(niwa-deftest niwa-preset--build-transitions ()
  ,test
  (test)

  :doc "continues a final response through WAIT only for pending steering"
  (let* ((transitions
          (niwa-preset--build-transitions
           '((INIT . ((t . WAIT)))
             (WAIT . ((t . TYPE)))
             (TYPE . ((error-p . ERRS)
                      (tool-p . TPRE)
                      (t . DONE))))))
         (rules (cdr (assq 'TYPE transitions))))
    (should
     (equal '(error-p tool-p niwa-tools--pending-steering-p t)
            (mapcar #'car rules)))
    (should (eq 'WAIT
                (cdr (assq #'niwa-tools--pending-steering-p rules)))))

  :doc "ends a turn at its tool results only when asked, after errors and steering"
  (let ((fsm (gptel-make-fsm
              :state 'TRET
              :table (niwa-preset--build-transitions
                      (copy-tree (default-value 'gptel-send--transitions)))
              :info (list :tool-result '(result)))))
    (should (eq 'WAIT (gptel--fsm-next fsm)))
    (plist-put (gptel-fsm-info fsm) :niwa-end-turn 'test)
    (should (eq 'DONE (gptel--fsm-next fsm)))
    ;; Queued niwa steering is delivered before the turn ends.
    (cl-letf (((symbol-function 'niwa-tools--pending-steering-p) #'always))
      (should (eq 'WAIT (gptel--fsm-next fsm))))
    (plist-put (gptel-fsm-info fsm) :steering-message "steer")
    (should (eq 'TSTR (gptel--fsm-next fsm)))
    (plist-put (gptel-fsm-info fsm) :error "failed")
    (should (eq 'ERRS (gptel--fsm-next fsm)))))

(niwa-deftest niwa-preset--build-handlers
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "adds pre-sample guidance and terminal handlers"
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
         (result (niwa-preset--build-handlers
                  (copy-tree base-handlers))))
    ;; WAIT entry should have deferred inject handler prepended
    (should (memq #'niwa-tools--handle-plan-tool-filter
                  (niwa-presets-test--guarded-wait-handlers result)))
    (should (memq #'niwa-compact-estimation-record-token-baseline
                  (cdr (assq 'TPRE result))))
    ;; One terminal orchestrator owns patch, callback, and settlement order.
    (should (= 2 (length (cdr (assq 'DONE result)))))
    (should (= 2 (length (cdr (assq 'ERRS result)))))
    (should (= 1 (length (cdr (assq 'ABRT result))))))

  :doc "defers callback and settlement until the final patch is ready"
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . DONE)))))
         (workspace (niwa-workspace--create
                     :type 'project :id "/niwamock:host:/srv/p/"
                     :root "/niwamock:host:/srv/p/" :name "p"))
         (buffer (generate-new-buffer " *ordered-terminal*"))
         (events nil)
         deferred-thunk)
    (unwind-protect
        (with-current-buffer buffer
          (let* ((handlers
                  (niwa-preset--build-handlers
                   '((WAIT) (DONE))))
                 (terminal (car (last (cdr (assq 'DONE handlers)))))
                 (fsm (gptel-make-fsm
                       :state 'DONE
                       :info
                       (list
                        :buffer buffer
                        :niwa-request-callback
                        (lambda (_status _machine)
                          (push (if (memq 'patch-ready events)
                                    'callback-after-patch
                                  'callback-before-patch)
                                events))))))
            (cl-letf (((symbol-function 'niwa-workspace)
                       (lambda (&optional _) workspace))
                      ((symbol-function 'niwa-transport-busy-p)
                       (lambda (&optional _) t))
                      ((symbol-function 'niwa-transport-run-when-idle)
                       (lambda (_key _path thunk &optional _on-cancel)
                         (setq deferred-thunk thunk)
                         t))
                      ((symbol-function 'niwa--generate-final-patch)
                       (lambda (_workspace _request callback)
                         (funcall callback "diff\n" nil)))
                      ((symbol-function 'niwa--replace-patch-buffer)
                       (lambda (_) (push 'patch-ready events)))
                      ((symbol-function 'niwa--complete-turn)
                       (lambda (_) (push 'settled events))))
              (funcall terminal fsm)
              (should-not events)
              (should (functionp deferred-thunk))
              (funcall deferred-thunk)
              (should (equal '(patch-ready callback-after-patch settled)
                             (nreverse events))))))
      (kill-buffer buffer)))

  :doc "wraps gptel wait after request-scoped WAIT handlers"
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((t . DONE)))))
         (base-handlers `((WAIT ,#'gptel--handle-wait after-wait)
                          (DONE)))
         (result (niwa-preset--build-handlers
                  (copy-tree base-handlers)))
         (wait-handlers (niwa-presets-test--guarded-wait-handlers result))
         (gate-pos (cl-position #'niwa--compact-handle-wait
                                wait-handlers))
         (begin-pos (cl-position-if
                     (lambda (handler)
                       (and (functionp handler)
                            (not (symbolp handler))))
                     wait-handlers))
         (message-pos (cl-position #'niwa-tools--handle-message-inject
                                   wait-handlers))
         (roster-pos
          (cl-position #'niwa-tools--handle-agent-roster-inject
                       wait-handlers))
         (deferred-pos (cl-position #'niwa-tools--handle-plan-tool-filter
                                    wait-handlers)))
    (should-not (memq #'gptel--handle-wait wait-handlers))
    (should gate-pos)
    (should (equal (nth (1+ gate-pos) wait-handlers) 'after-wait))
    (should-not (memq #'niwa-reminders--handle-inject wait-handlers))
    (dolist (pos (list begin-pos roster-pos message-pos
                       deferred-pos))
      (should pos)
      (should (< pos gate-pos)))))

(niwa-deftest niwa--add-termination-handler
  (:doc "appends once to graph terminals and abort without changing the input")
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((error-p . ERRS) (t . DONE)))))
         (handlers '((WAIT waiting) (DONE done) (ABRT abort)))
         (before (copy-tree handlers))
         (result (niwa--add-termination-handler #'ignore handlers))
         (repeated (niwa--add-termination-handler #'ignore result)))
    (should (equal '(done ignore) (cdr (assq 'DONE result))))
    (should (equal '(abort ignore) (cdr (assq 'ABRT result))))
    (should (equal '(ignore) (cdr (assq 'ERRS result))))
    (should (equal before handlers))
    (should (equal result repeated))
    (should (eq (assq 'WAIT handlers) (assq 'WAIT result)))
    (should (eq (assq 'DONE result) (assq 'DONE repeated)))))



;;
;;; Final patch handler

(niwa-deftest niwa-preset--final-patch-handler
  (:before-each (niwa-workspace-clear-registry)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)

  :doc "skips patch generation when the request buffer has no workspace"
  (let ((chat-buf (generate-new-buffer " *niwa-no-workspace*"))
        (generated 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-workspace)
                   (lambda (&optional _buffer) nil))
                  ((symbol-function 'niwa--generate-final-patch)
                   (lambda (_workspace _request callback)
                     (cl-incf generated)
                     (funcall callback "diff" nil))))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer chat-buf))))
            (niwa-preset--final-patch-handler fsm))
          (should (= 0 generated)))
      (kill-buffer chat-buf)))

  :doc "generates the originating request's patch and displays it"
  (let* ((ws (niwa-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (chat-buf (generate-new-buffer " *niwa-workspace*"))
         generated generated-request
         replaced)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-workspace)
                     (lambda (&optional _buffer) ws))
                    ((symbol-function 'niwa--generate-final-patch)
                     (lambda (workspace request callback)
                       (setq generated workspace generated-request request)
                       (funcall callback "diff --git a/file b/file\n" nil)))
                    ((symbol-function 'niwa--replace-patch-buffer)
                     (lambda (patch)
                       (setq replaced patch))))
            (let* ((request (niwa-request--create))
                   (fsm (gptel-make-fsm
                         :info (list :buffer chat-buf
                                     :niwa-request request))))
              (niwa-preset--final-patch-handler fsm)
              (should (eq request generated-request))))
          (should (eq ws generated))
          (should (equal "diff --git a/file b/file\n" replaced)))
      (kill-buffer chat-buf)))

  :doc "a deferred patch failure is contained and continues exactly once"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/niwamock:host:/srv/p/"
                     :root "/niwamock:host:/srv/p/" :name "p"))
         (buffer (generate-new-buffer " *failed-final-patch*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer)))
         deferred-thunk
         (continuations 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-workspace)
                   (lambda (&optional _) workspace))
                  ((symbol-function 'niwa-transport-busy-p)
                   (lambda (&optional _) t))
                  ((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (_key _path thunk &optional _on-cancel)
                     (setq deferred-thunk thunk)
                     t))
                  ((symbol-function 'niwa--generate-final-patch)
                   (lambda (&rest _) (error "Patch failed"))))
          (niwa-preset--final-patch-handler
           fsm (lambda (_) (setq continuations (1+ continuations))))
          (should (= 0 continuations))
          (should (functionp deferred-thunk))
          (niwa-test--with-captured-diagnostics nil
            (funcall deferred-thunk))
          (should (= 1 continuations)))
      (kill-buffer buffer))))


;;
;;; niwa-define-preset macro

(niwa-deftest niwa--define-presets
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "keeps Eval beside Bash in the implementation preset's native roster"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (dolist (preset '(niwa-implement))
      (let* ((metadata (niwa-preset--resolved-metadata preset))
             (resolved
              (niwa-tool-resolve (plist-get metadata :tool-specs)))
             (active
              (mapcar #'niwa-tool-name (plist-get resolved :active))))
        (should (equal '("ApplyPatch" "Bash" "Eval" "Glob" "Grep" "Monitor" "Notify" "Read"
                         "Schedule" "ToolCall" "ToolSearch" "WriteStdin")
                       (sort active #'string<))))))

  :doc "offers Schedule in the implementation preset's native roster"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (let* ((metadata (niwa-preset--resolved-metadata 'niwa-implement))
           (resolved (niwa-tool-resolve (plist-get metadata :tool-specs))))
      (should (member "Schedule"
                      (mapcar #'niwa-tool-name (plist-get resolved :active))))))

  :doc "defers task, agent, and shared editing tools in the implementation preset"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (let* ((metadata (niwa-preset--resolved-metadata 'niwa-implement))
           (resolved
            (niwa-tool-resolve (plist-get metadata :tool-specs)))
           (active (mapcar #'niwa-tool-name (plist-get resolved :active)))
           (deferred
            (mapcar #'niwa-tool-name (plist-get resolved :discoverable))))
      (dolist (name '("TaskWrite" "TaskList" "Agent" "ListAgents"
                      "InterruptAgent" "SendMessage" "WaitAgent"
                      "SharedCreate" "SharedEdit"))
        (should (member name deferred))
        (should-not (member name active)))
      ;; Eval and execution follow-up tools do not need specialist discovery.
      (dolist (name '("Eval" "WriteStdin" "Monitor"))
        (should (member name active))
        (should-not (member name deferred)))
      (should (member "ToolSearch" active))))

  :doc "an explicit empty :agents declaration registers no agents"
  ;; A declared-empty list must not be mistaken for an absent filter.
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (niwa-define-preset test-no-agents :agents ())
    (with-temp-buffer
      (niwa-agents--setup-for-request 'test-no-agents)
      (should-not (niwa-agents-specs)))
    (with-temp-buffer
      (niwa-agents--setup-for-request 'niwa-implement)
      (should (niwa-agents-specs))))

  :doc "makes every built-in role available to model-facing Agent calls"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (with-temp-buffer
      ;; A preset that offers Agent directly gets a schema for its roster.
      (setq-local gptel-tools (list (gptel-get-tool '("niwa" "Agent"))))
      (niwa-agents--setup-for-request 'niwa-implement)
      (let* ((tool (car gptel-tools))
             (validation-tool (niwa-tool-get "Agent"))
             (role-arg
              (cl-find-if
               (lambda (arg) (equal (plist-get arg :name) "role"))
               (gptel-tool-args tool)))
             (roles (append (plist-get role-arg :enum) nil)))
        (dolist (role '("worker" "explorer" "reviewer" "verifier"))
          (should (member role roles))
          (should-not
           (niwa-tool-repair-validate
            validation-tool
            (list :task_name "role_probe"
                  :message "Inspect only."
                  :role role)))))))

  :doc "selects the main prompt with no revision preset"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (let ((main (funcall (plist-get
                          (gptel-get-preset 'niwa-implement)
                          :system))))
      (should (string-match-p "Task execution protocol" main))
      (should-not (gptel-get-preset 'niwa-revise)))))

(niwa-deftest niwa-preset--variable-for-key
  ()
  ,test
  (test)
  :doc "prefers public then private niwa variables before gptel"
  (should (eq 'niwa-test-setting
              (niwa-preset--variable-for-key :test-setting)))
  (should (eq 'niwa--test-private-setting
              (niwa-preset--variable-for-key :test-private-setting)))
  :doc "returns nil when no matching variable exists"
  (should-not
   (niwa-preset--variable-for-key :test-certainly-not-defined)))

(niwa-deftest niwa-define-preset
  (:before-each (niwa-tool-clear-registry)
   :after-each (progn
                 (niwa-tool-clear-registry)
                 (setq niwa-preset--registry nil)
                 (dolist (name '(test-preset test-parent-a test-parent-b
                                 test-child test-session-a test-session-b))
                   (setq gptel--known-presets
                         (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)

  :doc "Registers in both gptel and niwa registries"
  (progn
    (niwa-define-tool
      :name "TestRead"
      :handler #'ignore
      :description "Read"
      :groups (testgrp))
    (niwa-define-preset test-preset
      :description "A test preset"
      :tools (testgrp)
      :agents (explorer verifier)
      :system "Test system prompt")
    ;; Registered in gptel
    (should (assq 'test-preset gptel--known-presets))
    ;; Registered in niwa registry
    (let ((meta (alist-get 'test-preset niwa-preset--registry)))
      (should meta)
      (should (equal '(explorer verifier) (plist-get meta :agents)))
      (should (equal '(testgrp) (plist-get meta :tool-specs)))))

  :doc "Resolves tools to gptel-tool structs"
  (progn
    (niwa-define-tool
      :name "TestA"
      :handler #'ignore
      :description "Tool A"
      :groups (mygroup))
    (niwa-define-tool
      :name "TestB"
      :handler #'ignore
      :description "Tool B"
      :groups (mygroup))
    (niwa-define-preset test-preset
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
    (niwa-define-tool
      :name "TestC"
      :handler #'ignore
      :description "C"
      :groups (grp))
    (niwa-define-preset test-preset
      :description "No system"
      :tools (grp))
    (let ((gptel-spec (alist-get 'test-preset gptel--known-presets)))
      (should-not (plist-member gptel-spec :system))))

  :doc "uses exact names and preserves explicitly quoted parents"
  (progn
    (niwa-define-preset test-parent-a :description "Parent")
    (niwa-define-preset test-preset
      :parents '(test-parent-a))
    (should (equal '(test-parent-a)
                   (plist-get (gptel-get-preset 'test-preset) :parents))))

  :doc "routes ordinary keys to niwa before gptel variables"
  (let ((niwa-test-setting 'global-niwa)
        (gptel-test-gptel-setting 'global-gptel))
    (niwa-define-preset test-preset
      :test-setting 'session-niwa
      :test-gptel-setting 'session-gptel)
    (let ((metadata (alist-get 'test-preset niwa-preset--registry))
          (gptel-spec (gptel-get-preset 'test-preset)))
      (should (equal '((niwa-test-setting . session-niwa))
                     (plist-get metadata :settings)))
      (should (eq 'session-gptel
                  (plist-get gptel-spec :test-gptel-setting)))))

  :doc "inherits agents and tools from the later parent then the child"
  (progn
    (niwa-define-tool
      :name "ParentA"
      :handler #'ignore
      :description "Parent A"
      :groups (parent-a))
    (niwa-define-tool
      :name "ParentB"
      :handler #'ignore
      :description "Parent B"
      :groups (parent-b))
    (niwa-define-preset test-parent-a
      :tools (parent-a)
      :agents (explorer))
    (niwa-define-preset test-parent-b
      :tools (parent-b)
      :agents (verifier))
    (niwa-define-preset test-child
      :parents (test-parent-a test-parent-b))
    (let ((metadata (niwa-preset--resolved-metadata 'test-child)))
      (should (equal '(parent-b) (plist-get metadata :tool-specs)))
      (should (equal '(verifier) (plist-get metadata :agents))))))

(niwa-deftest niwa-preset-resolve-settings
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-parent-c test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "supports replacement, prepend, append, and function semantics"
  (let ((niwa-test-setting '(base)))
    (niwa-define-preset test-parent-a
      :test-setting '(parent))
    (niwa-define-preset test-parent-b
      :test-setting '(:prepend (before)))
    (niwa-define-preset test-parent-c
      :test-setting '(:append (after)))
    (niwa-define-preset test-child
      :parents (test-parent-a test-parent-b test-parent-c)
      :test-setting '(:function (lambda (value) (append value '(function)))))
    (should (equal '(before parent after function)
                   (alist-get 'niwa-test-setting
                              (niwa-preset-resolve-settings
                               'test-child)))))
  :doc "merges preset-local tier and workload maps by entry"
  (let ((niwa-model-tiers '((balanced)))
        (niwa-model-workloads '((planning :tier balanced))))
    (niwa-define-preset test-parent-a
      :model-tiers ((fast :provider "Fast:fast-model"))
      :model-workloads ((implementation :tier fast)
                        ($plugin:code-review :tier fast)))
    (niwa-define-preset test-parent-b
      :model-tiers ((strong :provider "Strong:strong-model"))
      :model-workloads ((review :tier strong)
                        (journal :tier balanced)
                        (memory :tier balanced)))
    (niwa-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :model-workloads ((planning :tier strong)
                        (journal :tier fast)))
    (let ((settings (niwa-preset-resolve-settings 'test-child)))
      (should (equal '(:provider "Fast:fast-model")
                     (alist-get 'fast
                                (alist-get 'niwa-model-tiers settings))))
      (should (equal '(:provider "Strong:strong-model")
                     (alist-get 'strong
                                (alist-get 'niwa-model-tiers settings))))
      (should (equal '(:tier strong)
                     (alist-get 'planning
                                (alist-get 'niwa-model-workloads
                                           settings))))
      (should (equal '(:tier fast)
                     (alist-get '$plugin:code-review
                                (alist-get 'niwa-model-workloads
                                           settings))))
      (should (equal '(:tier fast)
                     (alist-get 'journal (alist-get 'niwa-model-workloads settings))))
      (should (equal '(:tier balanced)
                     (alist-get 'memory (alist-get 'niwa-model-workloads settings)))))))

(niwa-deftest niwa-preset--resolved-metadata
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "uses later parent metadata and then child metadata"
  (progn
    (niwa-define-preset test-parent-a :agents (explorer))
    (niwa-define-preset test-parent-b :agents (verifier))
    (niwa-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :agents (reviewer))
    (should (equal '(reviewer)
                   (plist-get
                    (niwa-preset--resolved-metadata 'test-child)
                    :agents)))))

(niwa-deftest niwa-preset--setting-specs
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (dolist (name '(test-parent-a test-parent-b test-child))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "returns parent settings in declaration order before child settings"
  (progn
    (niwa-define-preset test-parent-a :test-setting 'a)
    (niwa-define-preset test-parent-b :test-setting 'b)
    (niwa-define-preset test-child
      :parents (test-parent-a test-parent-b)
      :test-setting 'child)
    (should (equal '((niwa-test-setting . a)
                     (niwa-test-setting . b)
                     (niwa-test-setting . child))
                   (niwa-preset--setting-specs 'test-child)))))

(niwa-deftest niwa-preset--apply-settings
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (setq gptel--known-presets
           (assq-delete-all 'test-preset gptel--known-presets))))
  ,test
  (test)
  :doc "stores persistent settings in the current buffer and session"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "test"))
    (niwa-define-preset test-preset :test-setting 'local)
    (niwa-preset--apply-settings 'test-preset)
    (should (eq 'local niwa-test-setting))
    (should (local-variable-p 'niwa-test-setting))
    (should (eq 'test-preset
                (niwa-session-preset-name niwa--session)))))

(niwa-deftest niwa-preset--refresh-tools
  (:after-each
   (progn
     (remhash '("niwa" "TestWait") niwa-tool--registry)
     (setq niwa-preset--registry nil)
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
    (niwa-define-tool
      :name "TestWait"
      :description "Old wait schema"
      :handler #'ignore
      :args ((execution_id string :required "Execution ID")
             (chars string :optional "Input")))
    (niwa-define-preset test-preset
      :tools ((:tool "TestWait")))
    (setq-local gptel-tools
                (list (niwa-tool-gptel-tool
                       (niwa-tool-get "TestWait"))))
    (niwa-define-tool
      :name "TestWait"
      :description "Current wait schema"
      :handler #'ignore
      :args ((execution_id string :required "Execution ID")
             (chars string :optional "Input")
             (yield_time_ms integer :optional "Wait")))
    (let ((latest
           (niwa-tool-gptel-tool
            (niwa-tool-get "TestWait"))))
      (should-not (eq latest (car gptel-tools)))
      (niwa-preset--refresh-tools 'test-preset)
      (should (eq latest (car gptel-tools)))
      (should
       (equal '("exec-1" "" 30000)
              (gptel--map-tool-args
               (car gptel-tools)
               '(:execution_id "exec-1" :chars "" :yield_time_ms 30000)))))))

(niwa-deftest niwa-preset--post
  ()
  ,test
  (test)
  :doc "composes the user post hook with required niwa setup"
  (let (calls)
    (cl-letf (((symbol-function 'niwa-preset--apply-settings)
               (lambda (_) (push 'settings calls)))
              ((symbol-function 'niwa-agents--setup-for-request)
               (lambda (_) (push 'agents calls)))
              ((symbol-function 'niwa-preset--setup-catalog)
               (lambda (_) (push 'deferred calls)))
              ((symbol-function 'niwa-preset--refresh-tools)
               (lambda (_) (push 'tools calls)))
              ((symbol-function 'niwa-preset--setup-extras)
               (lambda (_) (push 'extras calls))))
      (niwa-preset--post 'test-preset
                            (lambda () (push 'user calls))))
    ;; The roster comes last so its Agent schema sees the final tools.
    (should (equal '(user settings tools deferred extras agents)
                   (nreverse calls)))))

(niwa-deftest niwa-preset--setup-catalog ()
  ,test
  (test)
  :doc "refreshes the catalog and extras without changing native tools during search"
  (let* ((niwa-preset--registry nil)
         (gptel--known-presets (copy-tree gptel--known-presets))
         (session (niwa-session--create :name "catalog-test"))
         (niwa-preset-extra-tool-specs
          '((catalog-test (:discoverable (:tool "Eval"))))))
    (niwa-tools-register)
    (niwa-preset--define 'catalog-test
                           '(:tools ((:tool "Read") (:discoverable (:tool "Imenu")))))
    (niwa-preset--define 'read-test '(:tools ((:tool "Read"))))
    (with-temp-buffer
      (setq-local niwa--session session)
      (dotimes (_ 2)
        (niwa-preset--refresh-tools 'catalog-test)
        (niwa-preset--setup-catalog 'catalog-test)
        (niwa-preset--setup-extras 'catalog-test)
        (let ((before (copy-sequence gptel-tools)) result)
          (niwa-tools--tool-search (lambda (value) (setq result value)) "Eval Imenu")
          (should (string-search "ToolCall(expression)" result))
          (should (equal before gptel-tools))
          (should (equal '("Read") (mapcar #'gptel-tool-name gptel-tools)))
          (should (equal '("Imenu" "Eval")
                         (mapcar #'cadar (niwa-session-tool-catalog session))))))
      (niwa-preset--setup-catalog 'read-test)
      (should-not (niwa-session-tool-catalog session))))

  :doc "discovers built-in capabilities through native ToolSearch without changing schemas"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "discovery"))
      (niwa-preset--refresh-tools 'niwa-implement)
      (niwa-preset--setup-catalog 'niwa-implement)
      (let* ((search (niwa-tool-gptel-tool (niwa-tool-get "ToolSearch")))
             (backend (gptel--make-anthropic :name "discovery-test"))
             (schemas (gptel--json-encode
                       (gptel--parse-tools backend gptel-tools)))
             (native (copy-sequence gptel-tools)))
        ;; The listing offers callable shapes, so known specialists need no
        ;; search; native execution tools are not repeated there.
        (let ((listing (niwa-tools-listing-section)))
          (dolist (shape '("- (TaskWrite [:tasks array]" "- (Imenu :file_path string)"
                           "- (EmacsDescribe :symbol string :aspect string [:kind string])"
                           "- (Agent :task_name string :message string"
                           "- (SendMessage [:target string] :message string [:working_directory string])"))
            (should (string-search shape listing)))
          (dolist (native '("(Eval" "(WriteStdin" "(Monitor" "(Bash" "(ToolCall"))
            (should-not (string-search native listing))))
        (should-not (string-search "Useful search keys"
                                   (gptel-tool-description search)))
        (dolist (case '(("emacs" "EmacsDescribe" "EmacsFind" "EmacsManual")
                       ("tasks" "TaskWrite" "TaskList" "Calling signature:")
                       ("plan" "TaskWrite" "Calling signature:")
                       ("checklist" "TaskWrite")
                       ("dependencies" "TaskWrite" "TaskList")
                       ("owners" "TaskWrite" "TaskList")
                       ("EmacsDescribe" "Calling signature:"
                        "(EmacsDescribe :symbol")))
          (let (result)
            (funcall (gptel-tool-function search)
                     (lambda (value) (setq result value)) (car case))
            (dolist (text (cdr case))
              (should (string-search text result)))))
        (should (equal native gptel-tools))
        ;; gptel creates fresh uninterned property keys on each parse; compare
        ;; the serialized payload rather than their Lisp symbol identities.
        (should (equal schemas (gptel--json-encode
                                (gptel--parse-tools backend gptel-tools))))
        ;; A capability hint must not add tools to a narrower role or request.
        (niwa-define-preset test-read-only :tools ((:tool "Read")))
        (niwa-preset--refresh-tools 'test-read-only)
        (niwa-preset--setup-catalog 'test-read-only)
        (should-not (niwa-tools-listing-section))
        (dolist (query '("emacs" "plan" "dependencies"))
          (let (result)
            (funcall (gptel-tool-function search)
                     (lambda (value) (setq result value)) query)
            (should (string-prefix-p "No matching tools found." result))))
        (niwa-preset--refresh-tools 'niwa-implement)
        (niwa-preset--setup-catalog 'niwa-implement)
        (setq-local niwa--current-request
                    (niwa-request--create :ptc-primitives '("TaskList")))
        (let (result)
          (funcall (gptel-tool-function search)
                   (lambda (value) (setq result value)) "dependencies")
          (should (string-search "TaskList" result))
          (should-not (string-search "TaskWrite" result)))))))

(niwa-deftest niwa-preset--parent-chain-to
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (dolist (name '(test-preset test-preset-parent test-preset-gptel))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "returns the chain from the start preset up to the named one"
  (progn
    (niwa-preset--define 'test-preset '(:parents nil))
    (niwa-preset--define 'test-preset-parent '(:parents (test-preset)))
    (should (equal '(test-preset-parent test-preset)
                   (niwa-preset--parent-chain-to
                    'test-preset 'test-preset-parent)))
    (should-not (niwa-preset--parent-chain-to
                 'test-preset-parent 'test-preset)))

  :doc "terminates when the presets walked already hold a cycle"
  ;; gptel registers parents without a cycle check, so the walk can be
  ;; handed one that niwa never had the chance to reject.
  (progn
    (gptel-make-preset 'test-preset-gptel :parents '(test-preset-parent))
    (gptel-make-preset 'test-preset-parent :parents '(test-preset-gptel))
    (should-not (niwa-preset--parent-chain-to
                 'test-preset 'test-preset-gptel))))

(niwa-deftest niwa-preset--define
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (dolist (name '(test-preset test-preset-parent test-preset-base
                                 test-preset-left test-preset-right
                                 test-preset-gptel))
       (setq gptel--known-presets
             (assq-delete-all name gptel--known-presets)))))
  ,test
  (test)
  :doc "rejects a parent cycle at definition time, leaving both registries intact"
  ;; Reject cycles before lazy resolution loses both names in a nesting error.
  (progn
    (niwa-preset--define 'test-preset-parent '(:parents nil))
    (niwa-preset--define 'test-preset '(:parents (test-preset-parent)))
    (let ((before (alist-get 'test-preset-parent niwa-preset--registry))
          (before-gptel (gptel-get-preset 'test-preset-parent)))
      (let ((err (should-error
                  (niwa-preset--define 'test-preset-parent
                                          '(:parents (test-preset)))
                  :type 'user-error)))
        (should (string-match-p "test-preset-parent" (error-message-string err)))
        (should (string-match-p "test-preset" (error-message-string err))))
      ;; A rejected definition must not have half-registered.
      (should (equal before
                     (alist-get 'test-preset-parent niwa-preset--registry)))
      (should (equal before-gptel (gptel-get-preset 'test-preset-parent)))))

  :doc "rejects a preset naming itself as its own parent"
  (should-error (niwa-preset--define 'test-preset '(:parents (test-preset)))
                :type 'user-error)

  :doc "rejects a cycle closed through a preset only gptel knows"
  ;; gptel registers a preset without a cycle check of its own, and its
  ;; own parent walk is equally unguarded, so a parent that never went
  ;; through niwa still has to be followed.
  (progn
    (niwa-preset--define 'test-preset '(:parents nil))
    (gptel-make-preset 'test-preset-gptel :parents '(test-preset))
    (should-error (niwa-preset--define
                   'test-preset '(:parents (test-preset-gptel)))
                  :type 'user-error))

  :doc "accepts a diamond, which is not a cycle"
  (progn
    (niwa-preset--define 'test-preset-base
                            '(:test-setting base))
    (niwa-preset--define 'test-preset-left '(:parents (test-preset-base)))
    (niwa-preset--define 'test-preset-right '(:parents (test-preset-base)))
    (niwa-preset--define 'test-preset
                            '(:parents (test-preset-left test-preset-right)))
    ;; A shared ancestor reached by two paths is not a cycle: the
    ;; definition is accepted and resolution still terminates.
    (should (alist-get 'test-preset niwa-preset--registry))
    (should (equal '((niwa-test-setting . base)
                     (niwa-test-setting . base))
                   (niwa-preset--setting-specs 'test-preset))))

  :doc "warns and ignores an ordinary key without a matching variable"
  (let (warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type message &rest _)
                 (setq warning (cons type message)))))
      (niwa-preset--define
       'test-preset '(:test-certainly-not-defined value)))
    (should (equal '(niwa presets) (car warning)))
    (should (string-match-p "not found" (cdr warning)))))

(niwa-deftest niwa-preset-apply
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
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
          (niwa-define-preset test-session-a :test-setting 'a)
          (niwa-define-preset test-session-b :test-setting 'b)
          (dolist (pair `((,buffer-a . test-session-a)
                          (,buffer-b . test-session-b)))
            (with-current-buffer (car pair)
              (setq-local niwa--session
                          (niwa-session--create :name "test"))
              (cl-letf (((symbol-function 'niwa-agents--setup-for-request)
                         #'ignore)
                        ((symbol-function 'niwa-preset--setup-catalog)
                         #'ignore)
                        ((symbol-function 'niwa-preset--setup-extras)
                         #'ignore))
                (niwa-preset-apply (cdr pair)))))
          (should (eq 'a (buffer-local-value 'niwa-test-setting buffer-a)))
          (should (eq 'b (buffer-local-value 'niwa-test-setting buffer-b)))
          (should (eq 'test-session-a
                      (niwa-session-preset-name
                       (buffer-local-value 'niwa--session buffer-a)))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b)))
  :doc "composes inherited ordinary settings exactly once when applied"
  (let ((niwa-test-setting '(base)))
    (niwa-define-preset test-parent-a
      :test-setting '(:append (parent)))
    (niwa-define-preset test-child
      :parents (test-parent-a)
      :test-setting '(:append (child)))
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "test"))
      (cl-letf (((symbol-function 'niwa-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-catalog)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-extras)
                 #'ignore))
        (niwa-preset-apply 'test-child))
      (should (equal '(base parent child) niwa-test-setting))
      (should (eq 'test-child
                  (niwa-session-preset-name niwa--session)))))
  :doc "stores resolved preset-local model policy on the session"
  (let ((niwa-model-tiers '((balanced)))
        (niwa-model-workloads nil))
    (niwa-define-preset test-child
      :model-tiers ((strong :provider "Strong:strong-model" :effort high))
      :model-workloads ((review :tier strong)))
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "test"))
      (cl-letf (((symbol-function 'niwa-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-catalog)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-extras)
                 #'ignore))
        (niwa-preset-apply 'test-child))
      (should (equal '(:provider "Strong:strong-model" :effort high)
                     (alist-get 'strong niwa-model-tiers)))
      (should (equal '(:tier strong)
                     (alist-get 'review niwa-model-workloads)))
      (should (eq 'test-child
                  (niwa-session-preset-name niwa--session)))))
  :doc "each session's direct Agent schema advertises its own roster"
  (let ((gptel--known-backends nil)
        (gptel-use-tools t)
        (niwa-agent--registry
         (mapcar (lambda (name)
                   (cons name (niwa-agent--create
                               :name name :description "Probe role."
                               :system-prompt "Probe.")))
                 '("alpha" "beta")))
        ;; A native Agent extra lands after the preset's own tools.
        (niwa-preset-extra-tool-specs
         '((test-session-a (:tool "Agent") (:tool "Glob"))))
        (buffer-a (generate-new-buffer " *preset-agent-a*"))
        (buffer-b (generate-new-buffer " *preset-agent-b*"))
        (agent-count (lambda ()
                       (cl-count (niwa-tool-get "Agent") gptel-tools
                                 :key #'niwa-tool-for-gptel))))
    (unwind-protect
        (let ((gptel-backend (gptel-make-openai "Preset probe" :key "unused"))
              (gptel-model 'gpt-4o-mini))
          (niwa-tools-register)
          (niwa-define-preset test-session-a
            :tools ((:tool "Read") (:tool "Agent"))
            :agents (alpha beta))
          (niwa-define-preset test-session-b
            :tools ((:tool "Read") (:tool "Agent"))
            :agents ())
          (niwa-preset-apply 'test-session-a buffer-a)
          (with-current-buffer buffer-a
            (should (= 1 (funcall agent-count)))
            (should (equal ["alpha" "beta"]
                           (niwa-presets-test--payload-agent-roles))))
          (niwa-preset-apply 'test-session-b buffer-b)
          (with-current-buffer buffer-b
            (should (equal [] (niwa-presets-test--payload-agent-roles))))
          (with-current-buffer buffer-a
            (should (equal ["alpha" "beta"]
                           (niwa-presets-test--payload-agent-roles)))
            ;; Reapplying the preset and repeating setup leave one Agent,
            ;; extras included, and the other tools as registered.
            (niwa-preset-apply 'test-session-a)
            (should (= 1 (funcall agent-count)))
            (niwa-agents--setup-for-request 'test-session-a)
            (should (= 1 (funcall agent-count)))
            (dolist (name '("Read" "Glob"))
              (should (memq (niwa-tool-gptel-tool (niwa-tool-get name))
                            gptel-tools)))
            (should (equal ["alpha" "beta"]
                           (niwa-presets-test--payload-agent-roles))))
          (should (equal [] (plist-get
                             (cl-find "role"
                                      (gptel-tool-args
                                       (niwa-tool-gptel-tool
                                        (niwa-tool-get "Agent")))
                                      :key (lambda (arg) (plist-get arg :name))
                                      :test #'equal)
                             :enum))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

(niwa-deftest niwa-preset-restore-session
  (:doc "reapplies trusted gptel and niwa preset settings from its name")
  ,test
  (test)
  (let ((niwa-preset--registry nil)
        (gptel--known-presets (copy-tree gptel--known-presets)))
    (niwa-define-preset test-preset
      :system "Trusted system prompt"
      :temperature 0.25
      :test-setting 'registered)
    (with-temp-buffer
      (let ((session
             (niwa-session--create
              :name "test"
              :preset-name 'test-preset)))
        (cl-letf (((symbol-function 'niwa-agents--setup-for-request)
                   #'ignore)
                  ((symbol-function 'niwa-preset--setup-catalog)
                   #'ignore)
                  ((symbol-function 'niwa-preset--setup-extras)
                   #'ignore))
          (niwa-preset-restore-session session))
        (should (local-variable-p 'niwa-test-setting))
        (should (eq 'registered niwa-test-setting))
        (should (eq 'test-preset gptel--preset))
        (should (equal "Trusted system prompt" gptel-system-prompt))
        (should (= 0.25 gptel-temperature))
        (should-not (local-variable-p 'kill-buffer-hook))))
    ;; A missing preset blocks until any preset is applied to the session.
    (with-temp-buffer
      (let ((session (niwa-session--create :name "missing" :preset-name 'gone)))
        (setq-local niwa--session session)
        (niwa-preset-restore-session session)
        (should (equal "preset" (plist-get (niwa-recovery-blocker session) :id)))
        (cl-letf (((symbol-function 'niwa-agents--setup-for-request) #'ignore)
                  ((symbol-function 'niwa-preset--setup-catalog) #'ignore)
                  ((symbol-function 'niwa-preset--setup-extras) #'ignore))
          (niwa-preset-apply 'test-preset))
        (should (eq 'test-preset (niwa-session-preset-name session)))
        (should-not (niwa-recovery-blocker session))))))

(niwa-deftest niwa-with-preset
  (:after-each
   (progn
     (setq niwa-preset--registry nil)
     (setq gptel--known-presets
           (assq-delete-all 'test-preset gptel--known-presets))))
  ,test
  (test)
  :doc "applies settings for one request without changing session state"
  (let ((niwa-test-setting 'outside))
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "test"))
      (niwa-define-preset test-preset :test-setting 'inside)
      (cl-letf (((symbol-function 'niwa-agents--setup-for-request)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-catalog)
                 #'ignore)
                ((symbol-function 'niwa-preset--setup-extras)
                 #'ignore))
        (niwa-with-preset 'test-preset
          (should (eq 'inside niwa-test-setting))))
      (should (eq 'outside niwa-test-setting))
      (should-not (niwa-session-preset-name niwa--session))))
  :doc "keeps its agent roster and tool catalog until the request ends"
  (let ((niwa-preset--registry nil)
        (gptel--known-presets nil))
    (niwa-tools-register)
    (niwa--define-presets)
    (niwa-define-preset test-preset :tools ((:tool "Read")) :agents ())
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "test"))
      (niwa-preset-apply 'niwa-implement)
      (let ((catalog (copy-tree (niwa-session-tool-catalog niwa--session)))
            (agents (niwa-agents-specs))
            (request (niwa-request--create)))
        (setq-local niwa--current-request request)
        (niwa-with-preset 'test-preset
          (should-not (niwa-agents-specs)))
        (should-not (niwa-agents-specs))
        (should-not (equal catalog (niwa-session-tool-catalog
                                    niwa--session)))
        (niwa-request-drain-cancellers request)
        (should (equal agents (niwa-agents-specs)))
        (should (equal catalog (niwa-session-tool-catalog niwa--session)))
        (should (eq 'niwa-implement
                    (niwa-session-preset-name niwa--session))))))
  :doc "delegates raw gptel preset specs"
  (let ((gptel-system-prompt "outside"))
    (niwa-with-preset '(:system "inside")
      (should (equal "inside" gptel-system-prompt)))
    (should (equal "outside" gptel-system-prompt))))


;;
;;; Built-in agent definitions

(niwa-deftest niwa-agents--builtin
  (:before-each (progn (niwa-tool-fs--register)
                       (niwa-tool-code--register)
                       (niwa-tool-exec--register)
                       (niwa-tool-ptc--register)
                       (niwa-tool-ui--register)
                       (niwa-tool-task--register)
                       (niwa-tool-note-register)
                       (niwa-tool-introspect--register)
                       ;; Other deftests wipe `niwa-agent--registry'
                       ;; in their :after-each; re-load to restore the
                       ;; built-in agent definitions.
                       (load (locate-library "niwa-agents") nil t)))
  ,test
  (test)

  :doc "verifier agent is registered when niwa-agents loads"
  (let ((agent (niwa-agent-get "verifier")))
    (should agent)
    (should (equal "verifier" (niwa-agent-name agent)))
    ;; Built-in agents have no turn cap; `:max-turns' is opt-in.
    (should-not (niwa-agent-max-turns agent))
    ;; The frozen role prompt owns its verification contract.
    (should (null (niwa-agent-reminders agent))))

  :doc "read-only role contracts survive fresh configuration without per-turn duplicates"
  (dolist (name '("verifier" "reviewer"))
    (let* ((agent (niwa-agent-freeze (niwa-agent-get name)))
           (prompt (niwa-agent-system-prompt agent))
           (inv (niwa-agent-invocation-create agent)))
      (should (string-search "read-only" prompt))
      (should (string-search (if (equal name "verifier")
                                "VERDICT: PASS" "overall_correctness")
                            prompt))
      (setf (niwa-agent-invocation-frozen-configuration inv)
            (niwa-agent-configuration--create
             :agent agent
             :request-locals `((gptel-system-prompt . ,prompt))))
      (with-temp-buffer
        ;; Role policy lives outside the transcript that compaction replaces.
        (dotimes (turn 2)
          (erase-buffer)
          (insert "Replacement conversation summary.")
          (setq-local gptel-system-prompt "stale request configuration")
          (niwa-agent-conversation-configure inv (current-buffer))
          (should (equal prompt gptel-system-prompt))
          (let ((staged (niwa-reminders--collect-from
                         (niwa-agent-invocation-reminders inv) turn inv)))
            (dolist (entry (plist-get staged :entries))
              (should-not
               (memq (plist-get entry :type)
                     '(verifier-read-only reviewer-read-only))))
            (mapc #'funcall (plist-get staged :commits)))))))

  :doc "explorer agent is registered and read-only"
  (should (niwa-agent-get "explorer"))

  :doc "task-capable agents include both task tools"
  (dolist (name '("worker" "explorer"))
    (let ((agent (niwa-agent-get name)))
      (dolist (tool '("TaskWrite" "TaskList"))
        (should (member `(:discoverable (:tool ,tool))
                        (niwa-agent-tools agent))))))

  :doc "planner agent is no longer a first-class built-in agent"
  (should-not (niwa-agent-get "planner")))


;;
;;; niwa-define-agent macro

(niwa-deftest niwa-define-agent
  (:before-each (niwa-test--capture-agent-registry)
   :after-each (niwa-test--restore-agent-registry))
  ,test
  (test)

  :doc "Creates struct and registers in registry"
  (progn
    (niwa-define-agent test-analyst
      :description "A test agent"
      :tools (read code)
      :system-components
      '((role :text "Test analyst") workspace-config environment)
      :max-turns 20)
    (let ((agent (niwa-agent-get "test-analyst")))
      (should agent)
      (should (equal "test-analyst" (niwa-agent-name agent)))
      (should (equal "A test agent" (niwa-agent-description agent)))
      (should (equal '(read code) (niwa-agent-tools agent)))
      (should (functionp (niwa-agent-system-prompt agent)))
      (should (string-match-p
               "Test analyst"
               (funcall (niwa-agent-system-prompt agent))))
      (should (= 20 (niwa-agent-max-turns agent)))))

  :doc "niwa-agent-get accepts symbol or string"
  (progn
    (niwa-define-agent my-agent
      :description "test"
      :tools (read))
    (should (niwa-agent-get 'my-agent))
    (should (niwa-agent-get "my-agent"))
    (should (eq (niwa-agent-get 'my-agent)
                (niwa-agent-get "my-agent"))))

  :doc "niwa-agent-to-gptel-spec produces valid plist"
  (progn
    (niwa-define-agent spec-agent
      :description "For spec test"
      :tools (read)
      :system-components
      '((role :text "system") workspace-config environment))
    (let* ((agent (niwa-agent-get "spec-agent"))
           (spec (niwa-agent-to-gptel-spec agent)))
      (should (equal "spec-agent" (car spec)))
      (should (stringp (plist-get (cdr spec) :description)))
      ;; :tools should be a (:function ...) spec
      (should (eq :function (car (plist-get (cdr spec) :tools))))
      ;; :system should be a (:function ...) spec
      (should (eq :function (car (plist-get (cdr spec) :system))))))

  :doc "stores :reminders list on the agent struct"
  (let ((r (niwa-reminder-create
            :type 'note
            :trigger (lambda (_) t)
            :content (lambda (_) "x"))))
    (niwa-define-agent reminder-agent
      :description "With reminders"
      :tools (read)
      :reminders (list r))
    (let ((agent (niwa-agent-get "reminder-agent")))
      (should (equal (list r) (niwa-agent-reminders agent)))))

  :doc "defaults :reminders to nil"
  (progn
    (niwa-define-agent no-reminder-agent
      :description "Without reminders"
      :tools (read))
    (let ((agent (niwa-agent-get "no-reminder-agent")))
      (should (null (niwa-agent-reminders agent)))))

  :doc "stores agent-scoped hooks and normalizes Stop"
  (progn
    (niwa-tools-register)
    (niwa-define-agent hook-agent
      :description "With hooks"
      :tools (read)
      :hooks ((Stop
               ((:matcher "*"
                 :hooks ((:type elisp
                          :function ignore)))))))
    (let* ((agent (niwa-agent-get "hook-agent"))
           (rules (niwa-agent-hook-rules agent))
           (invocation (niwa-agent-invocation-create agent)))
      (should (eq 'SubagentStop (caar rules)))
      (should (equal rules
                     (niwa-agent-invocation-hook-rules invocation))))))


(niwa-deftest niwa-preset--apply-final-patch ()
  ,test
  (test)

  :doc "an idle transport displays the patch inline"
  (let ((fsm (gptel-make-fsm))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/niwamock:host:/srv/p/"
                    :root "/niwamock:host:/srv/p/" :name "p"))
        shown)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _) nil))
                ((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (funcall callback "diff --git a/x b/x\n" nil)))
                ((symbol-function 'niwa--replace-patch-buffer)
                 (lambda (patch) (setq shown patch))))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request)
        (should (equal "diff --git a/x b/x\n" shown)))))

  :doc "the turn waits for the patch without blocking the handler"
  ;; The diffs run as helper processes.  Waiting for them inside this
  ;; handler re-entered the event loop from gptel's process callback, where
  ;; quitting is inhibited, so a helper that never settled wedged the turn.
  (let ((fsm (gptel-make-fsm))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/tmp/p/" :root "/tmp/p/" :name "p"))
        pending continued shown)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (setq pending callback)))
                ((symbol-function 'niwa--replace-patch-buffer)
                 (lambda (patch) (setq shown patch))))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request
         (lambda (_) (setq continued t)))
        (should (functionp pending))
        (should-not continued)
        (funcall pending "diff --git a/x b/x\n" nil)
        (should continued)
        (should (equal "diff --git a/x b/x\n" shown)))))

  :doc "aborted terminal diffs retain policy and request ownership during real discovery"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")
                      (plist-get (niwa-sandbox-probe) :available)))
    (dolist (change '(nil cancel permission sandbox request))
      (let* ((root (make-temp-file "niwa-terminal-diff-" t))
             (workspace (niwa-workspace--create :root root :id root :type 'project))
             (session (niwa-session--create
                       :workspace workspace :working-directory root
                       :permission-mode (if (eq change 'sandbox) 'ask 'edits)
                       :sandbox-mode 'required))
             (snapshots (make-hash-table :test #'equal))
             (request (niwa-request--create
                       :id "terminal-diff" :session session :cancelled-p (not (eq change 'cancel))
                       :file-snapshots snapshots))
             (file (file-name-concat root "changed.txt"))
             (bin (file-name-concat root "bin"))
             (entered (file-name-concat root "entered"))
             (gate (file-name-concat root "continue"))
             (find (executable-find "find"))
             (exec-path (cons bin exec-path))
             (process-environment (copy-sequence process-environment))
             (niwa-protected-paths '(("**/.git/**" . read-only)))
             (temporary-file-directory (file-name-as-directory root))
             (continued 0) warnings shown)
        (unwind-protect
            (with-temp-buffer
              (setq-local niwa--session session)
              (setq-local niwa--current-request request)
              (niwa-session-set-root-buffer session (current-buffer))
              (with-temp-file file (insert "after\n"))
              (puthash file "before\n" snapshots)
              (make-directory bin)
              (with-temp-file (file-name-concat bin "find")
                (insert "#!/bin/sh\ntouch " (shell-quote-argument entered)
                        "\ncount=0\nwhile [ ! -e " (shell-quote-argument gate)
                        " ]; do\n count=$((count+1)); [ $count -lt 500 ] || exit 1\n sleep .01\ndone\nexec "
                        (shell-quote-argument find) " \"$@\"\n"))
              (set-file-modes (file-name-concat bin "find") #o700)
              (setenv "PATH" (concat bin path-separator (getenv "PATH")))
              (let ((fsm (gptel-make-fsm
                          :state 'ABRT
                          :info (list :buffer (current-buffer)
                                      :niwa-request request
                                      :niwa-request-id "terminal-diff"))))
                (cl-letf (((symbol-function 'display-warning)
                           (lambda (_type text &rest _) (push text warnings)))
                          ((symbol-function 'niwa--replace-patch-buffer)
                           (lambda (patch) (setq shown patch))))
                  (niwa-preset--apply-final-patch
                   fsm (current-buffer) workspace request
                   (lambda (_) (cl-incf continued)))
                  (niwa-test--await 5 "terminal discovery entered" (file-exists-p entered))
                  (should (zerop continued))
                  (should (memq fsm niwa--turn-settlements-pending))
                  (pcase change
                    ('cancel (setf (niwa-request-cancelled-p request) t))
                    ('permission (setf (niwa-session-permission-mode session) 'full-auto))
                    ('sandbox (setf (niwa-session-sandbox-mode session) 'off))
                    ('request (setq niwa--current-request (niwa-request--create))))
                  (with-temp-file gate (insert "continue"))
                  (niwa-test--await 5 "terminal diff settlement" (> continued 0)))
                (should (= 1 continued))
                (should-not (memq fsm niwa--turn-settlements-pending))
                (ert-info ((format "terminal authority change: %S" change))
                  (if (memq change '(permission sandbox request))
                      (progn
                        (should-not shown)
                        (should (equal (list (format "niwa [%s]: Execution authority changed during preparation; submit the operation again (FSM handler #<function> failed)" root))
                                       warnings)))
                    (should-not warnings)
                    (should (string-search "-before\n+after\n" shown))))))
          (niwa-execution-teardown-all)
          (delete-directory root t)))))

  :doc "a failed diff warns and still continues the turn"
  (let ((fsm (gptel-make-fsm))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/tmp/p/" :root "/tmp/p/" :name "p"))
        continued warned)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (funcall callback nil '(error "Diff failed"))))
                ((symbol-function 'niwa--warn-once)
                 (lambda (&rest _) (setq warned t)))
                ((symbol-function 'niwa--replace-patch-buffer)
                 (lambda (&rest _) (ert-fail "No patch to show"))))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request
         (lambda (_) (setq continued t)))
        (should warned)
        (should continued))))

  :doc "a busy transport defers instead of nesting inside the sentinel"
  ;; Until this deferral the same call signalled `Forbidden reentrant call
  ;; of Tramp' and lost the patch, so a late patch is the better outcome.
  (let ((fsm (gptel-make-fsm))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/niwamock:host:/srv/p/"
                    :root "/niwamock:host:/srv/p/" :name "p"))
        deferred-thunk generated shown)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path thunk &optional _on-cancel)
                   (setq deferred-thunk thunk)
                   t))
                ((symbol-function 'niwa--generate-final-patch)
                 (lambda (_ws req callback)
                   ;; Settlement clears the buffer-local before the
                   ;; deferred thunk runs; the captured request is what
                   ;; must reach the generator.
                   (setq generated req)
                   (funcall callback "diff\n" nil)))
                ((symbol-function 'niwa--replace-patch-buffer)
                 (lambda (patch) (setq shown patch))))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request)
        (should-not generated)
        (should (functionp deferred-thunk))
        (setq niwa--current-request nil)
        (funcall deferred-thunk)
        (should (eq request generated))
        (should (equal "diff\n" shown)))))

  :doc "a local workspace never defers, busy transport or not"
  (let ((fsm (gptel-make-fsm))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/tmp/p/" :root "/tmp/p/" :name "p"))
        generated)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (&rest _) (error "Local workspace must not defer")))
                ((symbol-function 'niwa--generate-final-patch)
                 (lambda (_workspace _request callback)
                   (setq generated t)
                   (funcall callback "" nil)))
                ((symbol-function 'niwa--replace-patch-buffer) #'ignore))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request)
        (should generated))))

  :doc "cancelling deferred work reports abort before failure teardown"
  (let* ((events nil)
         (fsm (gptel-make-fsm
               :info (list :niwa-request-callback
                           (lambda (status _fsm)
                             (push (list 'callback status) events)))))
        (request (niwa-request--create))
        (workspace (niwa-workspace--create
                    :type 'project :id "/niwamock:host:/srv/p/"
                    :root "/niwamock:host:/srv/p/" :name "p"))
         on-cancel continued)
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path _thunk &optional cancel)
                   (setq on-cancel cancel)
                   t))
                ((symbol-function 'niwa--fail-turn)
                 (lambda (_fsm reason) (push (list 'failed reason) events))))
        (niwa-preset--apply-final-patch
         fsm (current-buffer) workspace request
         (lambda (_) (setq continued t)))
        (should (functionp on-cancel))
        (funcall on-cancel)
        (should (equal '((callback abort) (failed aborted))
                       (nreverse events)))
        (should-not continued)))))

(provide 'test-niwa-presets)
;;; test-niwa-presets.el ends here
