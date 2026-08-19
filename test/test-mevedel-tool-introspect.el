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

  :doc "keeps the literal registration table aligned with the exact roster"
  (progn
    (should (equal test-mevedel-tool-introspect--expected-tools
                   (mapcar #'car
                           mevedel-tool-introspect--registrations)))
    (should (eq 'mevedel-tool-introspect--library-source-check
                (nth 3 (assoc "library_source"
                              mevedel-tool-introspect--registrations))))
    (should (eq 'mevedel-tool-introspect--variable-value-check
                (nth 3 (assoc "variable_value"
                              mevedel-tool-introspect--registrations)))))

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

(mevedel-deftest mevedel-tool-introspect--library-source-check
  (:doc "allows only simple library names resolved inside a local load path")
  (let* ((root (make-temp-file "mevedel-introspection-" t))
         (libraries (file-name-concat root "libraries"))
         (outside (file-name-concat root "outside.el"))
         (safe (file-name-concat libraries "safe.el"))
         (escape (file-name-concat libraries "escape.el")))
    (unwind-protect
        (progn
          (make-directory libraries)
          (write-region ";;; safe.el\n" nil safe nil 'silent)
          (write-region ";;; outside.el\n" nil outside nil 'silent)
          (make-symbolic-link outside escape)
          (let ((load-path (list libraries)))
            (should
             (eq 'allow
                 (mevedel-tool-introspect--library-source-check
                  nil '(:library "safe"))))
            (dolist (library (list outside "../outside" "escape"
                                   "/ssh:example.invalid:/etc/passwd"))
              (should
               (eq 'deny
                   (car-safe
                    (mevedel-tool-introspect--library-source-check
                     nil (list :library library))))))))
          (require 'tramp)
          (require 'tramp-cache)
          (let ((load-path (append
                            (list "/ssh:example.invalid:/libraries"
                                  libraries)
                            load-path)))
            (should
             (eq 'deny
                 (car-safe
                  (mevedel-tool-introspect--library-source-check
                   nil '(:library "safe"))))))
      (delete-directory root t))))

(provide 'test-mevedel-tool-introspect)
;;; test-mevedel-tool-introspect.el ends here
