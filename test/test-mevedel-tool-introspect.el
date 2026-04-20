;;; test-mevedel-tool-introspect.el --- Tests for wrapped introspector tools -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies that the 16 gptel-agent introspection tools are registered
;; under the mevedel-introspection category with the expected metadata
;; and that the upstream "introspection" registrations remain intact.

;;; Code:

(require 'mevedel-tool-registry)
(require 'gptel-request)
(require 'gptel-agent-tools-introspection)
(require 'mevedel-tool-introspect)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


(defconst test-mevedel-tool-introspect--expected-tools
  '("symbol_exists" "load_paths" "features"
    "manual_names" "manual_nodes" "manual_node_contents"
    "symbol_manual_section"
    "function_completions" "command_completions" "variable_completions"
    "function_source" "variable_source"
    "function_documentation" "variable_documentation"
    "library_source" "variable_value")
  "All 16 introspection tools that should be wrapped.")


;;
;;; Registration

(mevedel-deftest mevedel-tool-introspect--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "registers every introspection tool under mevedel-introspection"
  (progn
    (mevedel-tool-introspect--register)
    (dolist (name test-mevedel-tool-introspect--expected-tools)
      (let ((tool (mevedel-tool-get name "mevedel-introspection")))
        (should tool)
        (should (eq t (mevedel-tool-read-only-p tool)))
        (should (memq 'elisp (mevedel-tool-groups tool))))))

  :doc "(:deferred elisp) pulls in all 16 wrapped tools"
  (progn
    (mevedel-tool-introspect--register)
    (let* ((resolved (mevedel-tool-resolve '((:deferred elisp))))
           (deferred (plist-get resolved :deferred))
           (names (mapcar #'mevedel-tool-name deferred)))
      (dolist (expected test-mevedel-tool-introspect--expected-tools)
        (should (member expected names)))))

  :doc "upstream introspection entries remain untouched"
  (progn
    (mevedel-tool-introspect--register)
    (dolist (name test-mevedel-tool-introspect--expected-tools)
      (should (gptel-get-tool (list "introspection" name)))))

  :doc "library_source check-permission returns allow unconditionally"
  (progn
    (mevedel-tool-introspect--register)
    (let ((tool (mevedel-tool-get "library_source" "mevedel-introspection")))
      (should (eq 'allow
                  (funcall (mevedel-tool-check-permission tool)
                           tool (list :library "subr"))))))

  :doc "variable_value check-permission returns ask unconditionally"
  (progn
    (mevedel-tool-introspect--register)
    (let ((tool (mevedel-tool-get "variable_value" "mevedel-introspection")))
      (should (eq 'ask
                  (funcall (mevedel-tool-check-permission tool)
                           tool (list :variable "load-path"))))))

  :doc "per-tool max-result-size is honoured"
  (progn
    (mevedel-tool-introspect--register)
    (should (null (mevedel-tool-max-result-size
                   (mevedel-tool-get "symbol_exists" "mevedel-introspection"))))
    (should (= 20000
               (mevedel-tool-max-result-size
                (mevedel-tool-get "features" "mevedel-introspection"))))
    (should (= 50000
               (mevedel-tool-max-result-size
                (mevedel-tool-get "library_source" "mevedel-introspection"))))
    (should (= 50000
               (mevedel-tool-max-result-size
                (mevedel-tool-get "manual_node_contents"
                                  "mevedel-introspection"))))))

(provide 'test-mevedel-tool-introspect)
;;; test-mevedel-tool-introspect.el ends here
