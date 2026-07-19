;;; test-mevedel-tool-wrap.el --- Tests for :wrap mode and extras -*- lexical-binding: t -*-

;;; Commentary:

;; Covers:
;;  - mevedel-tool--args-from-gptel (including :type normalisation)
;;  - mevedel-tool--call-wrapped-handler (sync/async, error handling,
;;    lifecycle)
;;  - mevedel-define-tool :wrap (derived fields, overrides, rejections,
;;    registry keying)
;;  - mevedel-preset-extra-tool-specs and
;;    mevedel-agent-extra-tool-specs merging

;;; Code:

(require 'cl-lib)
(require 'mevedel-tool-registry)
(require 'mevedel-pipeline)
(require 'mevedel-tools)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar test-mevedel-tool-wrap--counter 0
  "Monotonically increasing suffix for unique source-tool names.")
(defvar mevedel-preset--registry)
(defvar mevedel-preset-extra-tool-specs)
(declare-function mevedel-preset--setup-extras "mevedel-presets" (preset))

(defun test-mevedel-tool-wrap--unique (base)
  "Return a unique tool name derived from BASE for this test run."
  (format "%s_%d" base (cl-incf test-mevedel-tool-wrap--counter)))

(cl-defun test-mevedel-tool-wrap--make-source
    (&key (name "src_tool") function (args nil)
          (async nil) (category "test-src") (description "src")
          (include t))
  "Register a synthetic `gptel-tool' named NAME in CATEGORY.
FUNCTION, ARGS, ASYNC, DESCRIPTION, and INCLUDE configure the tool."
  (gptel-make-tool
   :name name
   :function (or function (lambda (&rest _) "ok"))
   :description description
   :args args
   :async async
   :include include
   :category category))

(defun test-mevedel-tool-wrap--remove-source (category name)
  "Unregister the `gptel-tool' at CATEGORY/NAME in `gptel--known-tools'."
  (when-let* ((cat-cell (assoc category gptel--known-tools)))
    (setf (cdr cat-cell)
          (assoc-delete-all name (cdr cat-cell) #'equal))
    (unless (cdr cat-cell)
      (setq gptel--known-tools
            (assoc-delete-all category gptel--known-tools #'equal)))))


;;
;;; Args conversion and :type normalisation

(mevedel-deftest mevedel-tool--normalize-type
  ()
  ,test
  (test)
  :doc "normalises symbol types to canonical symbol"
  (should (eq 'string (mevedel-tool--normalize-type 'string "t")))
  :doc "normalises string types to canonical symbol"
  (should (eq 'integer (mevedel-tool--normalize-type "integer" "t")))
  :doc "accepts quoted-symbol form"
  (should (eq 'boolean (mevedel-tool--normalize-type ''boolean "t")))
  :doc "errors on unknown types with tool name attached"
  (should-error (mevedel-tool--normalize-type 'wat "my_tool") :type 'error))

(mevedel-deftest mevedel-tool--args-from-gptel
  ()
  ,test
  (test)
  :doc "round-trips a canonical args spec"
  (let* ((gptel-args '((:name "path" :type string :description "the path")
                       (:name "limit" :type integer :description "limit"
                              :optional t)))
         (mevedel-args (mevedel-tool--args-from-gptel gptel-args "t")))
    (should (equal '((path string :required "the path")
                     (limit integer :optional "limit"))
                   mevedel-args)))

  :doc "normalises string :type inputs (MCP JSON Schema shape)"
  (let ((mevedel-args (mevedel-tool--args-from-gptel
                       '((:name "path" :type "string" :description "p"))
                       "t")))
    (should (eq 'string (nth 1 (car mevedel-args)))))

  :doc "preserves extra keys as trailing plist tail"
  (let ((mevedel-args (mevedel-tool--args-from-gptel
                       '((:name "tag" :type string :description "t"
                                :enum ["a" "b"]))
                       "t")))
    (should (equal '(tag string :required "t" :enum ["a" "b"])
                   (car mevedel-args))))

  :doc "accepts symbol :name inputs"
  (let ((mevedel-args (mevedel-tool--args-from-gptel
                       '((:name path :type string :description "p"))
                       "t")))
    (should (eq 'path (car (car mevedel-args))))))


;;
;;; Dispatcher: sync, async, error propagation, lifecycle

(mevedel-deftest mevedel-tool--call-wrapped-handler
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "sync path calls source function and forwards result"
  (let* ((name (test-mevedel-tool-wrap--unique "sync"))
         (_ (test-mevedel-tool-wrap--make-source
             :name name
             :function (lambda (a b) (format "%s+%s" a b))
             :args '((:name "a" :type string :description "a")
                     (:name "b" :type string :description "b"))))
         (handler (mevedel-tool--call-wrapped-handler
                   "test-src" name nil))
         (got nil))
    (funcall handler (lambda (r) (setq got r))
             (list :a "x" :b "y"))
    (should (equal '(:result "x+y") got))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "async path receives callback as first arg"
  (let* ((name (test-mevedel-tool-wrap--unique "async"))
         (_ (test-mevedel-tool-wrap--make-source
             :name name
             :async t
             :function (lambda (cb v) (funcall cb (concat "async:" v)))
             :args '((:name "v" :type string :description "v"))))
         (handler (mevedel-tool--call-wrapped-handler
                   "test-src" name t))
         (got nil))
    (funcall handler (lambda (r) (setq got r)) (list :v "hi"))
    (should (equal '(:result "async:hi") got))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "sync source error propagates to callback as Error: string"
  (let* ((name (test-mevedel-tool-wrap--unique "err"))
	         (_ (test-mevedel-tool-wrap--make-source
	             :name name
	             :function (lambda (_v) (error "Boom"))
             :args '((:name "v" :type string :description "v"))))
         (handler (mevedel-tool--call-wrapped-handler
                   "test-src" name nil))
         (got nil))
    (funcall handler (lambda (r) (setq got r)) (list :v "x"))
    (should (string-prefix-p "Error:" (plist-get got :result)))
    (should (string-match-p "boom" (plist-get got :result)))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "source replaced: dispatcher picks up fresh :function on next call"
  (let* ((name (test-mevedel-tool-wrap--unique "replace"))
         (_ (test-mevedel-tool-wrap--make-source
             :name name
             :function (lambda (_v) "one")
             :args '((:name "v" :type string :description "v"))))
         (handler (mevedel-tool--call-wrapped-handler
                   "test-src" name nil))
         (first nil)
         (second nil))
    (funcall handler (lambda (r) (setq first r)) (list :v "x"))
    (should (equal '(:result "one") first))
    (test-mevedel-tool-wrap--make-source
     :name name
     :function (lambda (_v) "two")
     :args '((:name "v" :type string :description "v")))
    (funcall handler (lambda (r) (setq second r)) (list :v "x"))
    (should (equal '(:result "two") second))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "source unregistered: dispatcher returns actionable error"
  (let* ((name (test-mevedel-tool-wrap--unique "gone"))
         (_ (test-mevedel-tool-wrap--make-source
             :name name
             :function (lambda () "ok")))
         (handler (mevedel-tool--call-wrapped-handler
                   "test-src" name nil))
         (got nil))
    (test-mevedel-tool-wrap--remove-source "test-src" name)
    (funcall handler (lambda (r) (setq got r)) nil)
    (should (string-prefix-p "Error:" (plist-get got :result)))
    (should (string-match-p "unregistered" (plist-get got :result)))
    (should (string-match-p name (plist-get got :result)))))


;;
;;; mevedel-define-tool :wrap integration

(mevedel-deftest mevedel-define-tool--wrap
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "wrap derives name/args, forces async-p=t, defaults category to mevedel-<src>"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_default"))
         (src (test-mevedel-tool-wrap--make-source
               :name name
               :description "src desc"
               :args '((:name "x" :type string :description "x"))
               :async nil)))
    (mevedel-define-tool
      :wrap src
      :groups (web)
      :read-only-p t
      :max-result-size 12345)
    (let ((tool (mevedel-tool-get name "mevedel-test-src")))
      (should tool)
      (should (equal name (mevedel-tool-name tool)))
      (should (eq t (mevedel-tool-read-only-p tool)))
      ;; Wrapped handlers always take (callback args), so the pipeline
      ;; must treat them as async regardless of the source's async flag.
      (should (eq t (mevedel-tool-async-p tool)))
      (should (= 12345 (mevedel-tool-max-result-size tool)))
      (should (equal "src desc" (mevedel-tool-description tool)))
      (should (memq 'web (mevedel-tool-groups tool)))
      (should (equal '(x) (mapcar #'car (mevedel-tool-args tool)))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "description override applies to copy but not source"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_desc"))
         (src (test-mevedel-tool-wrap--make-source
               :name name :description "src-side")))
    (mevedel-define-tool
      :wrap src
      :description "mevedel-side"
      :groups (web)
      :read-only-p t)
    (let ((tool (mevedel-tool-get name "mevedel-test-src")))
      (should (equal "mevedel-side" (mevedel-tool-description tool))))
    (should (equal "src-side"
                   (gptel-tool-description
                    (gptel-get-tool (list "test-src" name)))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "explicit :category override wins"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_cat"))
         (src (test-mevedel-tool-wrap--make-source :name name)))
    (mevedel-define-tool
      :wrap src
      :category "customcat"
      :groups (web)
      :read-only-p t)
    (should (mevedel-tool-get name "customcat"))
    (should-not (mevedel-tool-get name "mevedel-test-src"))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "rejects redeclared :name"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_reject_name"))
         (src (test-mevedel-tool-wrap--make-source :name name)))
    (should-error (macroexpand
                   `(mevedel-define-tool
                      :wrap ,src
                      :name "other"
                      :read-only-p t))
                  :type 'error)
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "rejects redeclared :args"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_reject_args"))
         (src (test-mevedel-tool-wrap--make-source :name name)))
    (should-error (macroexpand
                   `(mevedel-define-tool
                      :wrap ,src
                      :args ((x string :required "x"))
                      :read-only-p t))
                  :type 'error)
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "double-wrap at the same (category name) replaces the registry entry"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_double"))
         (src (test-mevedel-tool-wrap--make-source :name name)))
    (mevedel-define-tool
      :wrap src
      :groups (web)
      :read-only-p t
      :max-result-size 1)
    (let ((initial (mevedel-tool-get name "mevedel-test-src")))
      (mevedel-define-tool
        :wrap src
        :description "fresh"
        :groups (xtra)
        :max-result-size 77)
      (let ((refreshed (mevedel-tool-get name "mevedel-test-src")))
        (should refreshed)
        (should-not (eq initial refreshed))
        (should (equal "fresh" (mevedel-tool-description refreshed)))
        (should (equal '(xtra) (mevedel-tool-groups refreshed)))
        (should-not (mevedel-tool-read-only-p refreshed))
        (should (= 77 (mevedel-tool-max-result-size refreshed)))
        (should (= 1
                   (cl-count-if
                    (lambda (tool)
                      (and (equal name (mevedel-tool-name tool))
                           (equal "mevedel-test-src"
                                  (mevedel-tool-category tool))))
                    (mevedel-tool-all))))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "second wrap under a different :category is legal"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_sibling"))
         (src (test-mevedel-tool-wrap--make-source :name name)))
    (mevedel-define-tool :wrap src :groups (web) :read-only-p t)
    (mevedel-define-tool
      :wrap src :category "sibling" :groups (web) :read-only-p t)
    (should (mevedel-tool-get name "mevedel-test-src"))
    (should (mevedel-tool-get name "sibling"))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "source struct :function stays untouched after wrap"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_untouched"))
         (orig (lambda (_v) "source"))
         (src (test-mevedel-tool-wrap--make-source
               :name name
               :function orig
               :args '((:name "v" :type string :description "v")))))
    (mevedel-define-tool :wrap src :groups (web) :read-only-p t)
    (should (eq orig (gptel-tool-function
                      (gptel-get-tool (list "test-src" name)))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "wrapped provider and internal schemas do not alias the source"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_schema_isolated"))
         (src (test-mevedel-tool-wrap--make-source
               :name name
               :args '((:name "tasks" :type array :description "tasks"
                        :items (:type object))))))
    (mevedel-define-tool :wrap src :groups (web) :read-only-p t)
    (let* ((source-args (gptel-tool-args src))
           (wrapped (mevedel-tool-get name "mevedel-test-src"))
           (target-args (gptel-tool-args (mevedel-tool-gptel-tool wrapped))))
      (should-not (eq source-args target-args))
      (should-not (eq (plist-get (car source-args) :items)
                      (plist-get (car target-args) :items)))
      (should (eq 'object
                  (plist-get (plist-get (car (mevedel-tool-args wrapped))
                                        :items)
                             :type))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "wrapped gptel-tool preserves the source include slot"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_include"))
         (src (test-mevedel-tool-wrap--make-source
               :name name
               :include nil)))
    (mevedel-define-tool :wrap src :groups (web) :read-only-p t)
    (should-not (gptel-tool-include
                 (gptel-get-tool (list "mevedel-test-src" name))))
    (test-mevedel-tool-wrap--remove-source "test-src" name))

  :doc "sync source runs end-to-end through the mevedel pipeline"
  (let* ((name (test-mevedel-tool-wrap--unique "wrap_sync_e2e"))
         (src (test-mevedel-tool-wrap--make-source
               :name name
               :function (lambda (v) (format "got:%s" v))
               :args '((:name "v" :type string :description "v"))
               :async nil)))
    (mevedel-define-tool :wrap src :groups (web) :read-only-p t)
    (let* ((gtool (gptel-get-tool (list "mevedel-test-src" name)))
           (result nil))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               "hello")
      (should (equal "got:hello" result)))
    (test-mevedel-tool-wrap--remove-source "test-src" name)))


;;
;;; Preset extras merging

(mevedel-deftest mevedel-preset--setup-extras
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "deferred extras augment the session's deferred-set at :post time"
  (let* ((extra-name (test-mevedel-tool-wrap--unique "extra"))
         (src (test-mevedel-tool-wrap--make-source
               :name extra-name
               :description "extra desc")))
    (require 'mevedel-presets)
    (require 'mevedel-structs)
    (mevedel-define-tool :wrap src :groups (xtra) :read-only-p t)
    (let* ((session (mevedel-session--create
                     :workspace (mevedel-workspace--create :root "/tmp")))
           (mevedel--session session)
           (gptel-tools nil))
      (setf (alist-get 'extratest mevedel-preset--registry)
            (list :agents nil :tool-specs nil))
      (let ((mevedel-preset-extra-tool-specs
             '((extratest . ((:deferred xtra))))))
        (mevedel-preset--setup-extras 'extratest)
        (let ((set (mevedel-session-deferred-set session)))
          (should (cl-some
                   (lambda (entry)
                     (equal extra-name (cadr (car entry))))
                   set)))))
    (test-mevedel-tool-wrap--remove-source "test-src" extra-name)))


;;
;;; Agent extras merging

(mevedel-deftest mevedel-agent--effective-specs
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "effective specs append extras from defcustom"
  (require 'mevedel-agents)
  (let* ((agent (mevedel-agent--create
                 :name "e-agent"
                 :tools '(read (:tool "Bash"))))
         (mevedel-agent-extra-tool-specs
          '((e-agent . ((:deferred xtra))))))
    (let ((specs (mevedel-agent--effective-specs agent)))
      (should (member '(:deferred xtra) specs))
      (should (member 'read specs))))

  :doc "adds the common communication tools when no extras are registered"
  (let ((agent (mevedel-agent--create :name "other-agent" :tools '(read)))
        (mevedel-agent-extra-tool-specs nil))
    (should (equal '(read (:tool "SendMessage") (:tool "ListAgents"))
                   (mevedel-agent--effective-specs agent)))))

(provide 'test-mevedel-tool-wrap)
;;; test-mevedel-tool-wrap.el ends here
