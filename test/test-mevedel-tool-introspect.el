;;; test-mevedel-tool-introspect.el --- Tests for native introspector tools -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies that the 16 native introspection tools are registered
;; under the mevedel-introspection category with the expected metadata
;; and their permission boundaries and native handlers behave correctly.

;;; Code:

(require 'mevedel-tool-registry)
(require 'gptel-request)
(require 'mevedel-tool-introspect)
(require 'mevedel-tools)
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
  "All 16 introspection tools that should be native.")


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

  :doc "(:discoverable elisp) pulls in all 16 native tools"
  (progn
    (mevedel-tool-introspect--register)
    (let* ((resolved (mevedel-tool-resolve '((:discoverable elisp))))
           (deferred (plist-get resolved :discoverable))
           (names (mapcar #'mevedel-tool-name deferred)))
      (dolist (expected test-mevedel-tool-introspect--expected-tools)
        (should (member expected names)))))

  :doc "native registrations retain package-owned prompts"
  (progn
    (mevedel-tool-introspect--register)
    (dolist (name test-mevedel-tool-introspect--expected-tools)
      (let* ((tool (mevedel-tool-get name "mevedel-introspection"))
             (provenance (mevedel-tool-prompt-source tool)))
        (should-not (mevedel-tool-async-p tool))
        (should (eq 'file (plist-get provenance :kind)))
        (should (equal (mevedel-tool-prompt tool)
                       (with-temp-buffer
                         (insert-file-contents (plist-get provenance :path))
                         (buffer-string)))))))

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


(defun test-mevedel-tool-introspect--call (name &rest values)
  "Call native tool NAME through its gptel interface with VALUES."
  (let* ((tool (mevedel-tool-get name "mevedel-introspection"))
         (mevedel-permission-rules `((,name :action allow)))
         (mevedel-permission-mode 'ask)
         (mevedel-protected-paths nil)
         result)
    (apply (gptel-tool-function (mevedel-tool-gptel-tool tool))
           (lambda (value) (setq result value)) values)
    (setq result (mevedel-tool-render-data-strip (gptel--to-string result)))
    (should (stringp result))
    (should-not (string-prefix-p "Error:" result))
    result))

(mevedel-deftest mevedel-tool-introspect/native-handlers
  (:before-each (mevedel-tool-introspect--register))
  ,test
  (test)
  :doc "native symbol, value, documentation, completion and source tools execute"
  (let* ((directory (make-temp-file "mevedel-introspect-" t))
         (file (file-name-concat directory "retirement-fixture.el"))
         (load-path (cons directory load-path))
         (load-history (copy-tree load-history))
         (before (buffer-list))
         (source ";;; fixture.el -*- lexical-binding: t; -*-\n(defvar mevedel-retirement-fixture-value 42 \"Fixture value documentation.\")\n(defun mevedel-retirement-fixture-command () \"Fixture function documentation.\" (interactive) 42)\n(provide 'retirement-fixture)\n"))
    (unwind-protect
        (progn
          (write-region source nil file nil 'silent)
          (load file nil t)
          (mevedel-test--with-captured-diagnostics nil
            (should (equal "mevedel-retirement-fixture-command"
                           (test-mevedel-tool-introspect--call
                            "symbol_exists" "mevedel-retirement-fixture-command")))
            (should (string-match-p (regexp-quote directory)
                                    (test-mevedel-tool-introspect--call "load_paths")))
            (should (equal "retirement-fixture"
                           (test-mevedel-tool-introspect--call "features" "retirement-fixture")))
            (dolist (name '("function_completions" "command_completions"))
              (should (string-match-p "mevedel-retirement-fixture-command"
                                      (test-mevedel-tool-introspect--call name "retirement fixture command"))))
            (should (string-match-p "mevedel-retirement-fixture-value"
                                    (test-mevedel-tool-introspect--call
                                     "variable_completions" "retirement fixture value")))
            (should (string-match-p "Fixture function documentation"
                                    (test-mevedel-tool-introspect--call
                                     "function_documentation" "mevedel-retirement-fixture-command")))
            (should (string-match-p "Fixture value documentation"
                                    (test-mevedel-tool-introspect--call
                                     "variable_documentation" "mevedel-retirement-fixture-value")))
            (let ((find-file-hook (list (lambda () (ert-fail "Ran a file hook")))))
              (should (string-match-p "(defun mevedel-retirement-fixture-command"
                                      (test-mevedel-tool-introspect--call
                                       "function_source" "mevedel-retirement-fixture-command"))))
            (should (string-match-p "(defvar mevedel-retirement-fixture-value"
                                    (test-mevedel-tool-introspect--call
                                     "variable_source" "mevedel-retirement-fixture-value")))
            (should (equal source
                           (test-mevedel-tool-introspect--call "library_source" "retirement-fixture")))
            ;; Permission is separately asserted to always ask. Exercise the
            ;; actual handler boundary with a buffer-local shadow of the value.
            (with-temp-buffer
              (set (make-local-variable 'mevedel-retirement-fixture-value) 99)
              (let (result)
                (mevedel-pipeline--step-handler
                 (list :tool (mevedel-tool-get "variable_value" "mevedel-introspection")
                       :args '(:variable "mevedel-retirement-fixture-value"))
                 (lambda (value) (setq result value)) #'ert-fail)
                (should (equal 42 (plist-get result :result)))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer)))
      (setq features (delq 'retirement-fixture features))
      (dolist (symbol '(mevedel-retirement-fixture-value mevedel-retirement-fixture-command))
        (unintern symbol obarray))
      (delete-directory directory t)))

  :doc "source tools leave unknown names uninterned and return no source"
  (let ((name (make-temp-name "mevedel-introspect-missing-")))
    (should-not (intern-soft name))
    (mevedel-test--with-captured-diagnostics nil
      (dolist (tool '("function_source" "variable_source"))
        (should (equal (gptel--to-string nil)
                       (test-mevedel-tool-introspect--call tool name)))))
    (should-not (intern-soft name)))

  :doc "all four native Info tools return installed manual content"
  (let ((before (buffer-list))
        (Info-history nil)
        (Info-history-list nil)
        (info-lookup-cache (copy-tree info-lookup-cache)))
    (unwind-protect
        (mevedel-test--with-captured-diagnostics nil
          (should (string-match-p "elisp" (test-mevedel-tool-introspect--call "manual_names")))
          (should (string-match-p "Lists" (test-mevedel-tool-introspect--call "manual_nodes" "cl")))
          (should (string-match-p "Lisp" (test-mevedel-tool-introspect--call "manual_node_contents" "elisp" "Top")))
          (should (string-match-p "car" (test-mevedel-tool-introspect--call "symbol_manual_section" "car"))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))


(mevedel-deftest mevedel-tool-introspect/variable-value-permission
  (:before-each (mevedel-tool-introspect--register))
  ,test
  (test)
  :doc "variable_value waits for approval in Edits, then reads the global value"
  (let ((session (mevedel-session--create :name "introspection" :permission-mode 'edits))
        (mevedel-permission-rules nil)
        entry result)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local fill-column 13)
      (mevedel-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (queued &optional _session) (setq entry queued))))
          (funcall (gptel-tool-function
                    (mevedel-tool-gptel-tool
                     (mevedel-tool-get "variable_value" "mevedel-introspection")))
                   (lambda (value) (setq result value)) "fill-column"))
        (should entry)
        (should-not result)
        (funcall (plist-get entry :callback) 'allow-once)
        (should (equal (default-value 'fill-column) result))))))

(provide 'test-mevedel-tool-introspect)
;;; test-mevedel-tool-introspect.el ends here
