;;; test-mevedel-tool-plan.el --- Tests for mevedel-tool-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-tool-plan)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Registration

(mevedel-deftest mevedel-tool-plan--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers PresentPlan with expected slots"
  (progn
    (mevedel-tool-plan--register)
    (let ((tool (mevedel-tool-get "PresentPlan" "mevedel")))
      (should tool)
      (should (equal "PresentPlan" (mevedel-tool-name tool)))
      (should (eq #'mevedel-tool-plan--present (mevedel-tool-handler tool)))
      (should (eq t (mevedel-tool-async-p tool)))
      ;; PresentPlan only renders a plan and waits for user feedback --
      ;; `:read-only-p' keeps the permission chain from gating it (in
      ;; particular, `plan' mode would otherwise deny it).
      (should (eq t (mevedel-tool-read-only-p tool)))
      (let ((args (mevedel-tool-args tool)))
        (should (= 1 (length args)))
        (should (eq 'plan (caar args))))))

  :doc "registers CreatePlan with expected slots"
  (progn
    (mevedel-tool-plan--register)
    (let ((tool (mevedel-tool-get "CreatePlan" "mevedel")))
      (should tool)
      (should (equal "CreatePlan" (mevedel-tool-name tool)))
      (should (eq #'mevedel-tool-plan--create (mevedel-tool-handler tool)))
      (should (eq t (mevedel-tool-async-p tool)))
      ;; CreatePlan dispatches the read-only planner subagent; no user
      ;; code is mutated.  `:read-only-p' keeps the permission chain
      ;; from gating it (in particular under `plan' mode).
      (should (eq t (mevedel-tool-read-only-p tool)))
      (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
        (should (memq 'description arg-names))
        (should (memq 'prompt arg-names)))))

  :doc "both tools get gptel-tool wrappers"
  (progn
    (mevedel-tool-plan--register)
    (should (mevedel-tool-gptel-tool (mevedel-tool-get "PresentPlan" "mevedel")))
    (should (mevedel-tool-gptel-tool (mevedel-tool-get "CreatePlan" "mevedel")))))


;;
;;; Handler arg validation

(mevedel-deftest mevedel-tool-plan--present
  (:doc "`mevedel-tool-plan--present' requires :plan")
  (should-error (mevedel-tool-plan--present (lambda (_) nil) '())
                :type 'error))

(mevedel-deftest mevedel-tool-plan--create
  (:doc "`mevedel-tool-plan--create' validates required args")
  ,test
  (test)
  :doc "errors when description missing"
  (should-error
   (mevedel-tool-plan--create (lambda (_) nil) '(:prompt "details"))
   :type 'error)

  :doc "errors when prompt missing"
  (should-error
   (mevedel-tool-plan--create (lambda (_) nil) '(:description "do it"))
   :type 'error))


;;
;;; Renderer

(mevedel-deftest mevedel-tool-plan--render-create ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-plan--render-create
                 "CreatePlan" '(:description "refactor foo") nil nil)))

  :doc "header shows the task description; body-mode is markdown-mode"
  (let* ((body "# Plan: Refactor foo\n## Summary\nStuff\n")
         (plist (mevedel-tool-plan--render-create
                 "CreatePlan" '(:description "refactor foo") body nil)))
    (should (string-match-p "\\`CreatePlan: refactor foo " (plist-get plist :header)))
    (should (equal body (plist-get plist :body)))
    (should (eq 'markdown-mode (plist-get plist :body-mode)))))

(provide 'test-mevedel-tool-plan)
;;; test-mevedel-tool-plan.el ends here
