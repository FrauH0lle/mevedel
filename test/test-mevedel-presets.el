;;; test-mevedel-presets.el --- Tests for mevedel-presets.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-system)
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

(mevedel-deftest mevedel-preset--build-handlers ()
  ;; Minimal handlers alist mimicking gptel-send--handlers structure.
  ;; DONE and ERRS are terminal states (no outgoing transitions).
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
    ;; (patch generation, callback, cleanup)
    (let ((done-handlers (cdr (assq 'DONE result)))
          (errs-handlers (cdr (assq 'ERRS result))))
      (should (> (length done-handlers) 1))
      (should (> (length errs-handlers) 1))
      ;; Both should have the same number of extra handlers
      (should (= (length done-handlers) (length errs-handlers))))))


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
      :agents (codebase-analyst researcher)
      :system "Test system prompt")
    ;; Registered in gptel
    (should (assq 'mevedel-test-preset gptel--known-presets))
    ;; Registered in mevedel registry
    (let ((meta (alist-get 'mevedel-test-preset mevedel-preset--registry)))
      (should meta)
      (should (equal '(codebase-analyst researcher) (plist-get meta :agents)))
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
      (should (eq :function (car (plist-get (cdr spec) :system)))))))


(provide 'test-mevedel-presets)
;;; test-mevedel-presets.el ends here
