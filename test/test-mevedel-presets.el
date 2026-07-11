;;; test-mevedel-presets.el --- Tests for mevedel-presets.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-system)
(require 'mevedel-reminders)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-code)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-task)
(require 'mevedel-tool-introspect)
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-view)
(require 'mevedel-compact)
(require 'mevedel-presets)

(defvar gptel-request--transitions)
(defvar gptel--known-presets)
(defvar mevedel-session-persistence)
(declare-function mevedel-skills--apply-overrides-handler
                  "mevedel-skills" (fsm))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; FSM handler chain builder

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
    (should (memq #'mevedel-view--handle-queued-user-message-inject
                  (cdr (assq 'WAIT result))))
    (should (memq #'mevedel--compact-record-token-baseline
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

  :doc "wraps gptel wait after existing WAIT injectors"
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
         (skill-pos (cl-position #'mevedel-skills--apply-overrides-handler
                                 wait-handlers))
         (queued-pos
          (cl-position #'mevedel-view--handle-queued-user-message-inject
                       wait-handlers))
         (message-pos (cl-position #'mevedel-tools--handle-message-inject
                                   wait-handlers))
         (deferred-pos (cl-position #'mevedel-tools--handle-deferred-inject
                                    wait-handlers)))
    (should-not (memq #'gptel--handle-wait wait-handlers))
    (should gate-pos)
    (should (equal (nth (1+ gate-pos) wait-handlers) 'after-wait))
    (dolist (pos (list begin-pos skill-pos queued-pos
                       message-pos deferred-pos))
      (should pos)
      (should (< pos gate-pos)))))



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

  :doc "generates and displays a patch when a workspace is available"
  (let* ((ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (chat-buf (generate-new-buffer " *mevedel-workspace*"))
         generated
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
                    ((symbol-function 'mevedel--replace-patch-buffer)
                     (lambda (patch)
                       (setq replaced patch))))
            (let ((fsm (gptel-make-fsm
                        :info (list :buffer chat-buf))))
              (mevedel-preset--final-patch-handler fsm)))
          (should (eq ws generated))
          (should (equal "diff --git a/file b/file\n" replaced)))
      (kill-buffer chat-buf))))


;;
;;; mevedel-define-preset macro

(mevedel-deftest mevedel-define-preset
  (:before-each (mevedel-tool-clear-registry)
   :after-each (progn
                 (mevedel-tool-clear-registry)
                 (setq mevedel-preset--registry nil)
                 (setq gptel--known-presets
                       (assq-delete-all 'mevedel-test-preset
                                        gptel--known-presets))))
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
    (should (assq 'mevedel-test-preset gptel--known-presets))
    ;; Registered in mevedel registry
    (let ((meta (alist-get 'mevedel-test-preset mevedel-preset--registry)))
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
    (let* ((gptel-spec (alist-get 'mevedel-test-preset gptel--known-presets))
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
    (let ((gptel-spec (alist-get 'mevedel-test-preset gptel--known-presets)))
      (should-not (plist-member gptel-spec :system)))))


;;
;;; Built-in agent definitions

(mevedel-deftest mevedel-agents--builtin
  (:before-each (progn (mevedel-tool-fs--register)
                       (mevedel-tool-code--register)
                       (mevedel-tool-exec--register)
                       (mevedel-tool-ui--register)
                       (mevedel-tool-task--register)
                       (mevedel-tool-introspect--register)
                       ;; Other deftests wipe `mevedel-agent--registry'
                       ;; in their :after-each; re-load to restore the
                       ;; built-in agent definitions.
                       (load-file (locate-library "mevedel-agents"))))
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
  (dolist (name '("explorer" "coordinator"))
    (let ((agent (mevedel-agent-get name)))
      (should (member '(:tool "TaskNote")
                      (mevedel-agent-tools agent)))))

  :doc "planner agent is no longer a first-class built-in agent"
  (should-not (mevedel-agent-get "planner")))


;;
;;; mevedel-define-agent macro

(mevedel-deftest mevedel-define-agent
  (:after-each (setq mevedel-agent--registry nil))
  ,test
  (test)

  :doc "Creates struct and registers in registry"
  (progn
    (mevedel-define-agent test-analyst
      :description "A test agent"
      :tools (read code)
      :system-prompt #'ignore
      :max-turns 20)
    (let ((agent (mevedel-agent-get "test-analyst")))
      (should agent)
      (should (equal "test-analyst" (mevedel-agent-name agent)))
      (should (equal "A test agent" (mevedel-agent-description agent)))
      (should (equal '(read code) (mevedel-agent-tools agent)))
      (should (functionp (mevedel-agent-system-prompt agent)))
      (should-not (funcall (mevedel-agent-system-prompt agent)))
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
      :system-prompt (lambda () "system"))
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
