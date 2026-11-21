;;; test-niwa-tool-introspect.el --- Tests for native introspector tools -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the three native introspection tools: registration metadata,
;; permission boundaries, and their native handlers.

;;; Code:

(require 'niwa-tool-registry)
(require 'gptel-request)
(require 'niwa-tool-introspect)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


(defconst test-niwa-tool-introspect--expected-tools
  '("EmacsDescribe" "EmacsFind" "EmacsManual")
  "The native introspection tools.")


;;
;;; Registration

(niwa-deftest niwa-tool-introspect--register
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "registers read-only elisp tools with package-owned prompts"
  (progn
    (niwa-tool-introspect--register)
    (dolist (name test-niwa-tool-introspect--expected-tools)
      (let* ((tool (niwa-tool-get name "niwa"))
             (provenance (niwa-tool-prompt-source tool)))
        (should tool)
        (should (eq t (niwa-tool-read-only-p tool)))
        (should (memq 'elisp (niwa-tool-groups tool)))
        (should (niwa-tool-summary tool))
        (should-not (niwa-tool-async-p tool))
        (should (eq 'file (plist-get provenance :kind)))
        (should (equal (niwa-tool-prompt tool)
                       (with-temp-buffer
                         (insert-file-contents (plist-get provenance :path))
                         (buffer-string)))))))

  :doc "(:discoverable elisp) pulls in exactly the three tools"
  (progn
    (niwa-tool-introspect--register)
    (should (equal (sort (copy-sequence test-niwa-tool-introspect--expected-tools)
                         #'string<)
                   (sort (mapcar #'niwa-tool-name
                                 (plist-get (niwa-tool-resolve '((:discoverable elisp)))
                                            :discoverable))
                         #'string<))))

  :doc "built-in lookup registers the tools on first use"
  (progn
    (should (niwa-tool-ensure "EmacsFind"))
    (should (niwa-tool-get "EmacsManual" "niwa")))

  :doc "per-tool max-result-size is honoured"
  (progn
    (niwa-tool-introspect--register)
    (should (= 50000 (niwa-tool-max-result-size (niwa-tool-get "EmacsDescribe" "niwa"))))
    (should (= 20000 (niwa-tool-max-result-size (niwa-tool-get "EmacsFind" "niwa"))))
    (should (= 50000 (niwa-tool-max-result-size (niwa-tool-get "EmacsManual" "niwa"))))))

(niwa-deftest niwa-tool-introspect--describe-check
  (:doc "values always ask, library source is confined, other reads defer to policy")
  (let* ((root (make-temp-file "niwa-introspection-check-" t))
         (load-path (list root)))
    (unwind-protect
        (progn
          (write-region ";;; safe.el\n" nil (file-name-concat root "safe.el") nil 'silent)
          (should (eq 'ask (niwa-tool-introspect--describe-check
                            nil '(:symbol "load-path" :aspect "value"))))
          (should (eq 'allow (niwa-tool-introspect--describe-check
                              nil '(:symbol "safe" :aspect "source" :kind "library"))))
          ;; An unknown name read for source is inferred to be a library.
          (should (eq 'allow (niwa-tool-introspect--describe-check
                              nil '(:symbol "safe" :aspect "source"))))
          (should (eq 'deny (car-safe (niwa-tool-introspect--describe-check
                                       nil '(:symbol "../safe" :aspect "source"
                                             :kind "library")))))
          (dolist (input '((:symbol "car" :aspect "source")
                           (:symbol "car" :aspect "documentation")
                           (:symbol "car" :aspect "manual")
                           ;; Location reads no file, even for an unknown library.
                           (:symbol "missing-library" :aspect "location")))
            (should-not (niwa-tool-introspect--describe-check nil input))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-introspect--describe-kind
  (:doc "explicit kinds win; otherwise function, variable, then library for source")
  (progn
    (should (eq 'variable (niwa-tool-introspect--describe-kind
                           '(:symbol "car" :kind "variable"))))
    (should (eq 'function (niwa-tool-introspect--describe-kind '(:symbol "car"))))
    (should (eq 'variable (niwa-tool-introspect--describe-kind
                           '(:symbol "fill-column"))))
    (let ((name (make-temp-name "niwa-introspect-unknown-")))
      (should (eq 'library (niwa-tool-introspect--describe-kind
                            (list :symbol name :aspect "source"))))
      (should (eq 'library (niwa-tool-introspect--describe-kind
                            (list :symbol name :aspect "location"))))
      (should-not (niwa-tool-introspect--describe-kind
                   (list :symbol name :aspect "documentation")))
      (should-not (intern-soft name)))))

(niwa-deftest niwa-tool-introspect--local-library-file
  (:doc "finds libraries only by simple name in local load-path directories")
  (let* ((root (make-temp-file "niwa-introspection-lib-" t))
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
                           (niwa-tool-introspect--local-library-file "probe")))
            (should-not (niwa-tool-introspect--local-library-file "absent"))
            (dolist (name (list "../first/probe" (file-name-concat first "probe")))
              (should-not (niwa-tool-introspect--local-library-file name)))
            (should (equal '("probe")
                           (niwa-tool-introspect--library-names)))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-introspect--library-source-check
  (:doc "allows only simple library names resolved inside a local load path")
  (let* ((root (make-temp-file "niwa-introspection-" t))
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
                 (niwa-tool-introspect--library-source-check
                  nil '(:symbol "safe"))))
            (dolist (library (list outside "../outside" "escape"
                                   "/ssh:example.invalid:/etc/passwd"))
              (should
               (eq 'deny
                   (car-safe
                    (niwa-tool-introspect--library-source-check
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
                  (niwa-tool-introspect--library-source-check
                   nil '(:symbol "safe"))))))
      (delete-directory root t))))


(defun test-niwa-tool-introspect--call (name &rest args)
  "Run native tool NAME through its gptel interface with plist ARGS.
Return the delivered text; omitted optional arguments are nil."
  (let* ((tool (niwa-tool-get name "niwa"))
         (niwa-permission-rules `((,name :action allow)))
         (niwa-permission-mode 'ask)
         (niwa-protected-paths nil)
         result)
    (apply (gptel-tool-function (niwa-tool-gptel-tool tool))
           (lambda (value) (setq result value))
           (mapcar (lambda (spec) (plist-get args (intern (format ":%s" (car spec)))))
                   (niwa-tool-args tool)))
    (setq result (niwa-tool-render-data-strip (gptel--to-string result)))
    (should (stringp result))
    result))

(defun test-niwa-tool-introspect--ok (name &rest args)
  "Return NAME's text for plist ARGS, asserting it is not an error."
  (let ((result (apply #'test-niwa-tool-introspect--call name args)))
    (should-not (string-prefix-p "Error:" result))
    result))

(niwa-deftest niwa-tool-introspect/native-handlers
  (:before-each (niwa-tool-introspect--register))
  ,test
  (test)
  :doc "describe and find read documentation, source, locations and names"
  (let* ((directory (make-temp-file "niwa-introspect-" t))
         (file (file-name-concat directory "retirement-fixture.el"))
         (load-path (cons directory load-path))
         (load-history (copy-tree load-history))
         (before (buffer-list))
         (source ";;; fixture.el -*- lexical-binding: t; -*-\n(defvar niwa-retirement-fixture-value 42 \"Fixture value documentation.\")\n(defun niwa-retirement-fixture-command () \"Fixture function documentation.\" (interactive) 42)\n(provide 'retirement-fixture)\n"))
    (unwind-protect
        (progn
          (write-region source nil file nil 'silent)
          (load file nil t)
          (niwa-test--with-captured-diagnostics nil
            (should (equal "retirement-fixture"
                           (test-niwa-tool-introspect--ok
                            "EmacsFind" :kind "library" :pattern "retirement fixture")))
            (should (string-match-p
                     (concat (regexp-quote file) "; its feature is loaded")
                     (test-niwa-tool-introspect--ok
                      "EmacsDescribe" :symbol "retirement-fixture" :aspect "location")))
            (should (string-match-p
                     (regexp-quote file)
                     (test-niwa-tool-introspect--ok
                      "EmacsDescribe" :symbol "niwa-retirement-fixture-command"
                      :aspect "location")))
            (dolist (kind '("function" "command"))
              (should (string-match-p "niwa-retirement-fixture-command"
                                      (test-niwa-tool-introspect--ok
                                       "EmacsFind" :kind kind
                                       :pattern "retirement fixture command"))))
            (should (string-match-p "niwa-retirement-fixture-value"
                                    (test-niwa-tool-introspect--ok
                                     "EmacsFind" :kind "variable"
                                     :pattern "retirement fixture value")))
            (should (string-match-p "Fixture function documentation"
                                    (test-niwa-tool-introspect--ok
                                     "EmacsDescribe" :symbol "niwa-retirement-fixture-command"
                                     :aspect "documentation")))
            (should (string-match-p "Fixture value documentation"
                                    (test-niwa-tool-introspect--ok
                                     "EmacsDescribe" :symbol "niwa-retirement-fixture-value"
                                     :aspect "documentation")))
            (let ((find-file-hook (list (lambda () (ert-fail "Ran a file hook")))))
              (should (string-match-p "(defun niwa-retirement-fixture-command"
                                      (test-niwa-tool-introspect--ok
                                       "EmacsDescribe" :symbol "niwa-retirement-fixture-command"
                                       :aspect "source"))))
            (should (string-match-p "(defvar niwa-retirement-fixture-value"
                                    (test-niwa-tool-introspect--ok
                                     "EmacsDescribe" :symbol "niwa-retirement-fixture-value"
                                     :aspect "source")))
            (should (equal source
                           (test-niwa-tool-introspect--ok
                            "EmacsDescribe" :symbol "retirement-fixture" :aspect "source"
                            :kind "library")))
            ;; Permission is separately asserted to always ask.  Exercise the
            ;; handler boundary with a buffer-local shadow of the value.
            (with-temp-buffer
              (set (make-local-variable 'niwa-retirement-fixture-value) 99)
              (let (result)
                (niwa-pipeline--step-handler
                 (list :tool (niwa-tool-get "EmacsDescribe" "niwa")
                       :args '(:symbol "niwa-retirement-fixture-value" :aspect "value"))
                 (lambda (value) (setq result value)) #'ert-fail)
                (should (equal 42 (plist-get result :result)))))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer)))
      (setq features (delq 'retirement-fixture features))
      (dolist (symbol '(niwa-retirement-fixture-value niwa-retirement-fixture-command))
        (unintern symbol obarray))
      (delete-directory directory t)))

  :doc "unknown names report absence without interning and features report misses"
  (let ((name (make-temp-name "niwa-introspect-missing-")))
    (should-not (intern-soft name))
    (niwa-test--with-captured-diagnostics nil
      (should (string-match-p "No function or variable named"
                              (test-niwa-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "documentation")))
      ;; A value read always asks, so exercise its handler directly.
      (should (string-match-p "No function or variable named"
                              (plist-get (niwa-tool-introspect--describe
                                          (list :symbol name :aspect "value"))
                                         :result)))
      (should (string-match-p "No function named"
                              (test-niwa-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "source"
                               :kind "function")))
      (should (string-match-p "not found in local load-path directories"
                              (test-niwa-tool-introspect--ok
                               "EmacsDescribe" :symbol name :aspect "location"))))
    (should-not (intern-soft name)))

  :doc "invalid combinations fail with a tool error"
  (niwa-test--with-captured-diagnostics nil
    (dolist (call '(("EmacsDescribe" :symbol "car" :aspect "value")
                    ("EmacsDescribe" :symbol "subr-x" :aspect "documentation"
                     :kind "library")
                    ("EmacsFind" :kind "function")
                    ("EmacsFind" :kind "feature" :pattern "subr-x")
                    ("EmacsManual" :node "Top")))
      (should (string-prefix-p "Error:"
                               (apply #'test-niwa-tool-introspect--call call)))))

  :doc "manual listings, node contents and a symbol's section use installed Info"
  (let ((before (buffer-list))
        (Info-history nil)
        (Info-history-list nil)
        (info-lookup-cache (copy-tree info-lookup-cache)))
    (unwind-protect
        (niwa-test--with-captured-diagnostics nil
          (should (string-match-p "elisp" (test-niwa-tool-introspect--ok "EmacsManual")))
          (should (string-match-p "Lists" (test-niwa-tool-introspect--ok
                                           "EmacsManual" :manual "cl")))
          (should (string-match-p "Lisp" (test-niwa-tool-introspect--ok
                                          "EmacsManual" :manual "elisp" :node "Top")))
          (should (string-match-p "car" (test-niwa-tool-introspect--ok
                                         "EmacsDescribe" :symbol "car" :aspect "manual"))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))


(niwa-deftest niwa-tool-introspect/non-ascii
  (:before-each (niwa-tool-introspect--register))
  ,test
  (test)
  :doc "non-ASCII manual node names reach the pipeline result as multibyte text"
  (let (context)
    (cl-letf (((symbol-function 'Info-build-node-completions)
               (lambda (_manual) (list (cons "Café — node" nil)))))
      (niwa-pipeline--step-handler
       (list :tool (niwa-tool-get "EmacsManual" "niwa")
             :args '(:manual "elisp") :cancel-cell (list #'ignore))
       (lambda (ctx) (setq context ctx)) #'ignore))
    (let ((text (plist-get context :result)))
      (should (multibyte-string-p text))
      (should (string-match-p "Café — node" text)))))

(ert-deftest niwa-tool-introspect--manual-preserves-existing-info ()
  "Reading a node leaves an existing Info reader and the caller unchanged."
  (let ((before (buffer-list))
        (Info-history nil)
        (Info-history-list nil))
    (unwind-protect
        (save-window-excursion
          (info "(elisp)Lists")
          (let ((reader (current-buffer))
                (contents (buffer-string))
                (position (point))
                (node Info-current-node)
                (file Info-current-file)
                (window (selected-window))
                (windows (window-list)))
            (with-temp-buffer
              (let ((caller (current-buffer)))
                (insert "Caller contents")
                (goto-char 4)
                (should (string-match-p
                         "Node: Top"
                         (niwa-tool-introspect--manual-node-contents "elisp" "Top")))
                (should (eq (current-buffer) caller))
                (should (= (point) 4))
                (should (equal (buffer-string) "Caller contents"))))
            (should (eq (selected-window) window))
            (should (equal (window-list) windows))
            (should (eq (window-buffer window) reader))
            (with-current-buffer reader
              (should (equal contents (buffer-string)))
              (should (= position (point)))
              (should (equal node Info-current-node))
              (should (equal file Info-current-file)))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))

(niwa-deftest niwa-tool-introspect--source
  (:before-each (niwa-tool-introspect--register))
  ,test
  (test)
  :doc "missing C sources deliver one tool error without prompting"
  (let ((find-function-C-source-directory nil)
        (niwa-permission-mode 'full-auto)
        prompted)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _)
                 (setq prompted t)
                 (error "Unexpected source directory prompt"))))
      (dolist (symbol '("scroll-left" "fill-column"))
        (let ((tool (niwa-tool-get "EmacsDescribe" "niwa"))
              (deliveries 0)
              result)
          (niwa-test--with-captured-diagnostics nil
            (funcall (gptel-tool-function (niwa-tool-gptel-tool tool))
                     (lambda (value)
                       (cl-incf deliveries)
                       (setq result (gptel--to-string value)))
                     symbol "source" nil))
          (should (= 1 deliveries))
          (should (string-match-p "Emacs C sources unavailable" result))
          (should-not prompted)))))
  :doc "configured C sources remain readable for functions and variables"
  (let* ((directory (make-temp-file "niwa-c-source-" t))
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
          (niwa-test--with-captured-diagnostics nil
            (should (string-match-p
                     "Fixture C function"
                     (test-niwa-tool-introspect--ok
                      "EmacsDescribe" :symbol "scroll-left" :aspect "source")))
            (should (string-match-p
                     "Fixture C variable"
                     (test-niwa-tool-introspect--ok
                      "EmacsDescribe" :symbol "fill-column" :aspect "source")))))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer)))
      (delete-directory directory t))))

(niwa-deftest niwa-tool-introspect/variable-value-permission
  (:before-each (niwa-tool-introspect--register))
  ,test
  (test)
  :doc "a value read asks in Edits despite a tool allow rule, then reads the global value"
  (let ((session (niwa-session--create :name "introspection" :permission-mode 'edits))
        (niwa-permission-rules '(("EmacsDescribe" :action allow)))
        result)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local fill-column 13)
      (niwa-test--with-captured-diagnostics nil
        (niwa-pq-test--with-view-here view
          (funcall (gptel-tool-function
                    (niwa-tool-gptel-tool (niwa-tool-get "EmacsDescribe" "niwa")))
                   (lambda (value) (setq result value)) "fill-column" "value" nil)
          (should-not result)
          (niwa-pq-test--press (niwa-pq-test--card view) "a")
          (should (equal (default-value 'fill-column) result)))))))

(provide 'test-niwa-tool-introspect)
;;; test-niwa-tool-introspect.el ends here
