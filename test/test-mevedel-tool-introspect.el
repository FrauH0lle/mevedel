;;; test-mevedel-tool-introspect.el --- Tests for native introspector tools -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the three native introspection tools: registration metadata,
;; permission boundaries, and their native handlers.

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
  '("EmacsDescribe" "EmacsFind" "EmacsManual")
  "The native introspection tools.")


;;
;;; Registration

(mevedel-deftest mevedel-tool-introspect--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "registers read-only elisp tools with package-owned prompts"
  (progn
    (mevedel-tool-introspect--register)
    (dolist (name test-mevedel-tool-introspect--expected-tools)
      (let* ((tool (mevedel-tool-get name "mevedel"))
             (provenance (mevedel-tool-prompt-source tool)))
        (should tool)
        (should (eq t (mevedel-tool-read-only-p tool)))
        (should (memq 'elisp (mevedel-tool-groups tool)))
        (should (mevedel-tool-summary tool))
        (should-not (mevedel-tool-async-p tool))
        (should (eq 'file (plist-get provenance :kind)))
        (should (equal (mevedel-tool-prompt tool)
                       (with-temp-buffer
                         (insert-file-contents (plist-get provenance :path))
                         (buffer-string)))))))

  :doc "(:discoverable elisp) pulls in exactly the three tools"
  (progn
    (mevedel-tool-introspect--register)
    (should (equal (sort (copy-sequence test-mevedel-tool-introspect--expected-tools)
                         #'string<)
                   (sort (mapcar #'mevedel-tool-name
                                 (plist-get (mevedel-tool-resolve '((:discoverable elisp)))
                                            :discoverable))
                         #'string<))))

  :doc "built-in lookup registers the tools on first use"
  (progn
    (should (mevedel-tool-ensure "EmacsFind"))
    (should (mevedel-tool-get "EmacsManual" "mevedel")))

  :doc "per-tool max-result-size is honoured"
  (progn
    (mevedel-tool-introspect--register)
    (should (= 50000 (mevedel-tool-max-result-size (mevedel-tool-get "EmacsDescribe" "mevedel"))))
    (should (= 20000 (mevedel-tool-max-result-size (mevedel-tool-get "EmacsFind" "mevedel"))))
    (should (= 50000 (mevedel-tool-max-result-size (mevedel-tool-get "EmacsManual" "mevedel"))))))

(mevedel-deftest mevedel-tool-introspect--describe-check
  (:doc "values always ask, library source is confined, other reads defer to policy")
  (let* ((root (make-temp-file "mevedel-introspection-check-" t))
         (load-path (list root)))
    (unwind-protect
        (progn
          (write-region ";;; safe.el\n" nil (file-name-concat root "safe.el") nil 'silent)
          (should (eq 'ask (mevedel-tool-introspect--describe-check
                            nil '(:symbol "load-path" :aspect "value"))))
          (should (eq 'allow (mevedel-tool-introspect--describe-check
                              nil '(:symbol "safe" :aspect "source" :kind "library"))))
          ;; An unknown name read for source is inferred to be a library.
          (should (eq 'allow (mevedel-tool-introspect--describe-check
                              nil '(:symbol "safe" :aspect "source"))))
          (should (eq 'deny (car-safe (mevedel-tool-introspect--describe-check
                                       nil '(:symbol "../safe" :aspect "source"
                                             :kind "library")))))
          (dolist (input '((:symbol "car" :aspect "source")
                           (:symbol "car" :aspect "documentation")
                           (:symbol "car" :aspect "manual")
                           ;; Location reads no file, even for an unknown library.
                           (:symbol "missing-library" :aspect "location")))
            (should-not (mevedel-tool-introspect--describe-check nil input))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-introspect--describe-kind
  (:doc "explicit kinds win; otherwise function, variable, then library for source")
  (progn
    (should (eq 'variable (mevedel-tool-introspect--describe-kind
                           '(:symbol "car" :kind "variable"))))
    (should (eq 'function (mevedel-tool-introspect--describe-kind '(:symbol "car"))))
    (should (eq 'variable (mevedel-tool-introspect--describe-kind
                           '(:symbol "fill-column"))))
    (let ((name (make-temp-name "mevedel-introspect-unknown-")))
      (should (eq 'library (mevedel-tool-introspect--describe-kind
                            (list :symbol name :aspect "source"))))
      (should (eq 'library (mevedel-tool-introspect--describe-kind
                            (list :symbol name :aspect "location"))))
      (should-not (mevedel-tool-introspect--describe-kind
                   (list :symbol name :aspect "documentation")))
      (should-not (intern-soft name)))))

(mevedel-deftest mevedel-tool-introspect--local-library-file
  (:doc "finds libraries only by simple name in local load-path directories")
  (let* ((root (make-temp-file "mevedel-introspection-lib-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second")))
    (unwind-protect
        (progn
          (make-directory first)
          (make-directory second)
          (write-region "" nil (file-name-concat first "probe.elc") nil 'silent)
          (write-region "" nil (file-name-concat first "probe.el") nil 'silent)
          (write-region "" nil (file-name-concat second "probe.el") nil 'silent)
          (require 'tramp)
          (let ((load-path (list "/ssh:example.invalid:/lisp" first second)))
            ;; Source wins within a directory, and load-path order across them.
            (should (equal (file-name-concat first "probe.el")
                           (mevedel-tool-introspect--local-library-file "probe")))
            (should-not (mevedel-tool-introspect--local-library-file "absent"))
            (dolist (name (list "../first/probe" (file-name-concat first "probe")))
              (should-not (mevedel-tool-introspect--local-library-file name)))
            (should (equal '("probe")
                           (mevedel-tool-introspect--library-names)))))
      (delete-directory root t))))

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
                  nil '(:symbol "safe"))))
            (dolist (library (list outside "../outside" "escape"
                                   "/ssh:example.invalid:/etc/passwd"))
              (should
               (eq 'deny
                   (car-safe
                    (mevedel-tool-introspect--library-source-check
                     nil (list :symbol library))))))))
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
                   nil '(:symbol "safe"))))))
      (delete-directory root t))))


(defun test-mevedel-tool-introspect--call (name &rest args)
  "Run native tool NAME through its gptel interface with plist ARGS.
Return the delivered text; omitted optional arguments are nil."
  (let* ((tool (mevedel-tool-get name "mevedel"))
         (mevedel-permission-rules `((,name :action allow)))
         (mevedel-permission-mode 'ask)
         (mevedel-protected-paths nil)
         result)
    (apply (gptel-tool-function (mevedel-tool-gptel-tool tool))
           (lambda (value) (setq result value))
           (mapcar (lambda (spec) (plist-get args (intern (format ":%s" (car spec)))))
                   (mevedel-tool-args tool)))
    (setq result (mevedel-tool-render-data-strip (gptel--to-string result)))
    (should (stringp result))
    result))

(defun test-mevedel-tool-introspect--ok (name &rest args)
  "Return NAME's text for plist ARGS, asserting it is not an error."
  (let ((result (apply #'test-mevedel-tool-introspect--call name args)))
    (should-not (string-prefix-p "Error:" result))
    result))

(mevedel-deftest mevedel-tool-introspect/native-handlers
  (:before-each (mevedel-tool-introspect--register))
  ,test
  (test)
  :doc "describe and find read documentation, source, locations and names"
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
            (should (equal "retirement-fixture"
                           (test-mevedel-tool-introspect--ok
                            "EmacsFind" :kind "library" :pattern "retirement fixture")))
            (should (string-match-p
                     (concat (regexp-quote file) "; its feature is loaded")
                     (test-mevedel-tool-introspect--ok
                      "EmacsDescribe" :symbol "retirement-fixture" :aspect "location")))
            (should (string-match-p
                     (regexp-quote file)
                     (test-mevedel-tool-introspect--ok
                      "EmacsDescribe" :symbol "mevedel-retirement-fixture-command"
                      :aspect "location")))
            (dolist (kind '("function" "command"))
              (should (string-match-p "mevedel-retirement-fixture-command"
                                      (test-mevedel-tool-introspect--ok
                                       "EmacsFind" :kind kind
                                       :pattern "retirement fixture command"))))
            (should (string-match-p "mevedel-retirement-fixture-value"
                                    (test-mevedel-tool-introspect--ok
                                     "EmacsFind" :kind "variable"
                                     :pattern "retirement fixture value")))
            (should (string-match-p "Fixture function documentation"
                                    (test-mevedel-tool-introspect--ok
                                     "EmacsDescribe" :symbol "mevedel-retirement-fixture-command"
                                     :aspect "documentation")))
            (should (string-match-p "Fixture value documentation"
                                    (test-mevedel-tool-introspect--ok
                                     "EmacsDescribe" :symbol "mevedel-retirement-fixture-value"
                                     :aspect "documentation")))
            (let ((find-file-hook (list (lambda () (ert-fail "Ran a file hook")))))
              (should (string-match-p "(defun mevedel-retirement-fixture-command"
                                      (test-mevedel-tool-introspect--ok
                                       "EmacsDescribe" :symbol "mevedel-retirement-fixture-command"
                                       :aspect "source"))))
            (should (string-match-p "(defvar mevedel-retirement-fixture-value"
                                    (test-mevedel-tool-introspect--ok
                                     "EmacsDescribe" :symbol "mevedel-retirement-fixture-value"
                                     :aspect "source")))
            (should (equal source
                           (test-mevedel-tool-introspect--ok
                            "EmacsDescribe" :symbol "retirement-fixture" :aspect "source"
                            :kind "library")))
            ;; Permission is separately asserted to always ask.  Exercise the
            ;; handler boundary with a buffer-local shadow of the value.
            (with-temp-buffer
              (set (make-local-variable 'mevedel-retirement-fixture-value) 99)
              (let (result)
                (mevedel-pipeline--step-handler
                 (list :tool (mevedel-tool-get "EmacsDescribe" "mevedel")
                       :args '(:symbol "mevedel-retirement-fixture-value" :aspect "value"))
                 (lambda (value) (setq result value)) #'ert-fail)
                (should (equal 42 (plist-get result :result)))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer)))
      (setq features (delq 'retirement-fixture features))
      (dolist (symbol '(mevedel-retirement-fixture-value mevedel-retirement-fixture-command))
        (unintern symbol obarray))
      (delete-directory directory t)))

  :doc "unknown names report absence without interning and features report misses"
  (let ((name (make-temp-name "mevedel-introspect-missing-")))
    (should-not (intern-soft name))
    (mevedel-test--with-captured-diagnostics nil
      (should (string-match-p "No function or variable named"
                              (test-mevedel-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "documentation")))
      ;; A value read always asks, so exercise its handler directly.
      (should (string-match-p "No function or variable named"
                              (plist-get (mevedel-tool-introspect--describe
                                          (list :symbol name :aspect "value"))
                                         :result)))
      (should (string-match-p "No function named"
                              (test-mevedel-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "source"
                               :kind "function")))
      (should (string-match-p "not found in local load-path directories"
                              (test-mevedel-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "location"))))
    (should-not (intern-soft name)))

  :doc "invalid combinations fail with a tool error"
  (mevedel-test--with-captured-diagnostics nil
    (dolist (call '(("EmacsDescribe" :symbol "car" :aspect "value")
                    ("EmacsDescribe" :symbol "subr-x" :aspect "documentation"
                     :kind "library")
                    ("EmacsFind" :kind "function")
                    ("EmacsFind" :kind "feature" :pattern "subr-x")
                    ("EmacsManual" :node "Top")))
      (should (string-prefix-p "Error:"
                               (apply #'test-mevedel-tool-introspect--call call)))))

  :doc "manual listings, node contents and a symbol's section use installed Info"
  (let ((before (buffer-list))
        (Info-history nil)
        (Info-history-list nil)
        (info-lookup-cache (copy-tree info-lookup-cache)))
    (unwind-protect
        (mevedel-test--with-captured-diagnostics nil
          (should (string-match-p "elisp" (test-mevedel-tool-introspect--ok "EmacsManual")))
          (should (string-match-p "Lists" (test-mevedel-tool-introspect--ok
                                           "EmacsManual" :manual "cl")))
          (should (string-match-p "Lisp" (test-mevedel-tool-introspect--ok
                                          "EmacsManual" :manual "elisp" :node "Top")))
          (should (string-match-p "car" (test-mevedel-tool-introspect--ok
                                         "EmacsDescribe" :symbol "car" :aspect "manual"))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))


(mevedel-deftest mevedel-tool-introspect--source
  (:before-each (mevedel-tool-introspect--register))
  ,test
  (test)
  :doc "missing C sources deliver one tool error without prompting"
  (let ((find-function-C-source-directory nil)
        (mevedel-permission-mode 'full-auto)
        prompted)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _)
                 (setq prompted t)
                 (error "Unexpected source directory prompt"))))
      (dolist (symbol '("scroll-left" "fill-column"))
        (let ((tool (mevedel-tool-get "EmacsDescribe" "mevedel"))
              (deliveries 0)
              result)
          (mevedel-test--with-captured-diagnostics nil
            (funcall (gptel-tool-function (mevedel-tool-gptel-tool tool))
                     (lambda (value)
                       (cl-incf deliveries)
                       (setq result (gptel--to-string value)))
                     symbol "source" nil))
          (should (= 1 deliveries))
          (should (string-match-p "Emacs C sources unavailable" result))
          (should-not prompted)))))
  :doc "configured C sources remain readable for functions and variables"
  (let* ((directory (make-temp-file "mevedel-c-source-" t))
         (find-function-C-source-directory directory)
         (before (buffer-list)))
    (unwind-protect
        (progn
          (write-region
           "DEFUN (\"scroll-left\", Fscroll_left, Sscroll_left, 0, 2, 0,\n       doc: /* Fixture C function. */)\n  (Lisp_Object arg, Lisp_Object set_minimum)\n{\n  return Qnil;\n}\n"
           nil (file-name-concat directory "window.c") nil 'silent)
          (write-region
           "DEFVAR_INT (\"fill-column\", fill_column,\n            doc: /* Fixture C variable. */);\n"
           nil (file-name-concat directory "buffer.c") nil 'silent)
          (mevedel-test--with-captured-diagnostics nil
            (should (string-match-p
                     "Fixture C function"
                     (test-mevedel-tool-introspect--ok
                      "EmacsDescribe" :symbol "scroll-left" :aspect "source")))
            (should (string-match-p
                     "Fixture C variable"
                     (test-mevedel-tool-introspect--ok
                      "EmacsDescribe" :symbol "fill-column" :aspect "source")))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer)))
      (delete-directory directory t))))

(mevedel-deftest mevedel-tool-introspect/variable-value-permission
  (:before-each (mevedel-tool-introspect--register))
  ,test
  (test)
  :doc "a value read asks in Edits despite a tool allow rule, then reads the global value"
  (let ((session (mevedel-session--create :name "introspection" :permission-mode 'edits))
        (mevedel-permission-rules '(("EmacsDescribe" :action allow)))
        entry result)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local fill-column 13)
      (mevedel-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (queued &optional _session) (setq entry queued))))
          (funcall (gptel-tool-function
                    (mevedel-tool-gptel-tool (mevedel-tool-get "EmacsDescribe" "mevedel")))
                   (lambda (value) (setq result value)) "fill-column" "value" nil))
        (should entry)
        (should-not result)
        (funcall (plist-get entry :callback) 'allow-once)
        (should (equal (default-value 'fill-column) result))))))

(provide 'test-mevedel-tool-introspect)
;;; test-mevedel-tool-introspect.el ends here
