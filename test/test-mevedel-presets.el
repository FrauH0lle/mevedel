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
(require 'mevedel-tool-introspect)
(require 'mevedel-agents)
(require 'mevedel-presets)

(defvar gptel-request--transitions)
(defvar gptel--known-presets)
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
                           (DONE handler-done)
                           (ERRS handler-errs)))
         (result (mevedel-preset--build-handlers
                  (copy-tree base-handlers))))
    ;; WAIT entry should have deferred inject handler prepended
    (should (memq #'mevedel-tools--handle-deferred-inject
                  (cdr (assq 'WAIT result))))
    ;; Terminal states (DONE, ERRS) should have extra handlers
    ;; (patch generation, callback, cleanup, turn-count)
    (let ((done-handlers (cdr (assq 'DONE result)))
          (errs-handlers (cdr (assq 'ERRS result))))
      (should (> (length done-handlers) 1))
      (should (> (length errs-handlers) 1))
      ;; Both should have the same number of extra handlers
      (should (= (length done-handlers) (length errs-handlers)))))

  :doc "turn-count handler increments mevedel-session-turn-count on terminal states"
  (let* ((gptel-request--transitions
          '((INIT . ((t . WAIT)))
            (WAIT . ((t . TYPE)))
            (TYPE . ((err . ERRS)
                     (t . DONE)))))
         (base-handlers '((WAIT) (DONE) (ERRS)))
         (handlers (mevedel-preset--build-handlers (copy-tree base-handlers)))
         (ws (mevedel-workspace-get-or-create
              'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws))
         (chat-buf (generate-new-buffer " *mevedel-test-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq-local mevedel--session session))
          ;; The turn-count handler is the second-to-last terminal handler
          ;; (request-end is added last).  Invoke it directly to verify it
          ;; increments the counter.
          (let* ((fsm (gptel-make-fsm
                       :info (list :buffer chat-buf)))
                 (done-handlers (cdr (assq 'DONE handlers)))
                 (turn-handler (nth (- (length done-handlers) 2) done-handlers)))
            (should (functionp turn-handler))
            (funcall turn-handler fsm)
            (should (= 1 (mevedel-session-turn-count session)))
            (funcall turn-handler fsm)
            (should (= 2 (mevedel-session-turn-count session)))
            ;; ERRS terminal gets the same handler
            (let* ((errs-handlers (cdr (assq 'ERRS handlers)))
                   (errs-turn-handler
                    (nth (- (length errs-handlers) 2) errs-handlers)))
              (funcall errs-turn-handler fsm)
              (should (= 3 (mevedel-session-turn-count session))))))
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
      :agents (explore planner)
      :system "Test system prompt")
    ;; Registered in gptel
    (should (assq 'mevedel-test-preset gptel--known-presets))
    ;; Registered in mevedel registry
    (let ((meta (alist-get 'mevedel-test-preset mevedel-preset--registry)))
      (should meta)
      (should (equal '(explore planner) (plist-get meta :agents)))
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

  :doc "explore agent is registered and read-only"
  (should (mevedel-agent-get "explore"))

  :doc "planner agent is registered"
  (should (mevedel-agent-get "planner")))


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
      (should (eq #'ignore (mevedel-agent-system-prompt agent)))
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
      (should (null (mevedel-agent-reminders agent))))))


(provide 'test-mevedel-presets)
;;; test-mevedel-presets.el ends here
