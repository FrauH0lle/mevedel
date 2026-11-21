;;; test-niwa-tool-wrap.el --- Tests for :wrap mode and extras -*- lexical-binding: t -*-

;;; Commentary:

;; Covers:
;;  - niwa-tool--args-from-gptel (including :type normalisation)
;;  - niwa-tool--call-wrapped-handler (sync/async, error handling,
;;    lifecycle)
;;  - niwa-define-tool :wrap (derived fields, overrides, rejections,
;;    registry keying)
;;  - niwa-preset-extra-tool-specs and
;;    niwa-agent-extra-tool-specs merging

;;; Code:

(require 'cl-lib)
(require 'niwa-agents)
(require 'niwa-tool-registry)
(require 'niwa-pipeline)
(require 'niwa-presets)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar niwa-preset-extra-tool-specs)
(declare-function niwa-preset--setup-extras "niwa-presets" (preset))

(cl-defun test-niwa-tool-wrap--make-source
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

(defun test-niwa-tool-wrap--remove-source (category name)
  "Unregister the `gptel-tool' at CATEGORY/NAME in `gptel--known-tools'."
  (when-let* ((cat-cell (assoc category gptel--known-tools)))
    (setf (cdr cat-cell)
          (assoc-delete-all name (cdr cat-cell) #'equal))
    (unless (cdr cat-cell)
      (setq gptel--known-tools
            (assoc-delete-all category gptel--known-tools #'equal)))))


;;
;;; Args conversion and :type normalisation

(niwa-deftest niwa-tool--normalize-type
  ()
  ,test
  (test)
  :doc "normalises symbol types to canonical symbol"
  (should (eq 'string (niwa-tool--normalize-type 'string "t")))
  :doc "normalises string types to canonical symbol"
  (should (eq 'integer (niwa-tool--normalize-type "integer" "t")))
  :doc "accepts quoted-symbol form"
  (should (eq 'boolean (niwa-tool--normalize-type ''boolean "t")))
  :doc "errors on unknown types with tool name attached"
  (should-error (niwa-tool--normalize-type 'wat "my_tool") :type 'error))

(niwa-deftest niwa-tool--args-from-gptel
  ()
  ,test
  (test)
  :doc "round-trips a canonical args spec"
  (let* ((gptel-args '((:name "path" :type string :description "the path")
                       (:name "limit" :type integer :description "limit"
                              :optional t)))
         (niwa-args (niwa-tool--args-from-gptel gptel-args "t")))
    (should (equal '((path string :required "the path")
                     (limit integer :optional "limit"))
                   niwa-args)))

  :doc "normalises string :type inputs (MCP JSON Schema shape)"
  (let ((niwa-args (niwa-tool--args-from-gptel
                       '((:name "path" :type "string" :description "p"))
                       "t")))
    (should (eq 'string (nth 1 (car niwa-args)))))

  :doc "preserves extra keys as trailing plist tail"
  (let ((niwa-args (niwa-tool--args-from-gptel
                       '((:name "tag" :type string :description "t"
                                :enum ["a" "b"]))
                       "t")))
    (should (equal '(tag string :required "t" :enum ["a" "b"])
                   (car niwa-args))))

  :doc "accepts symbol :name inputs"
  (let ((niwa-args (niwa-tool--args-from-gptel
                       '((:name path :type string :description "p"))
                       "t")))
    (should (eq 'path (car (car niwa-args))))))


;;
;;; Dispatcher: sync, async, error propagation, lifecycle

(niwa-deftest niwa-tool--call-wrapped-handler
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "sync path calls source function and forwards result"
  (let* ((name "test-wrap-sync")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :function (lambda (a b) (format "%s+%s" a b))
                  :args '((:name "a" :type string :description "a")
                          (:name "b" :type string :description "b"))))
         (handler (niwa-tool--call-wrapped-handler source))
         (got nil))
    (funcall handler (lambda (r) (setq got r))
             (list :a "x" :b "y"))
    (should (equal '(:result "x+y") got)))

  :doc "async path receives callback as first arg"
  (let* ((name "test-wrap-async")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :async t
                  :function (lambda (cb v) (funcall cb (concat "async:" v)))
                  :args '((:name "v" :type string :description "v"))))
         (handler (niwa-tool--call-wrapped-handler source))
         (got nil))
    (funcall handler (lambda (r) (setq got r)) (list :v "hi"))
    (should (equal '(:result "async:hi") got)))

  :doc "sync source error propagates to callback as Error: string"
  (let* ((name "test-wrap-err")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :function (lambda (_v) (error "Boom"))
                  :args '((:name "v" :type string :description "v"))))
         (handler (niwa-tool--call-wrapped-handler source))
         (got nil))
    (funcall handler (lambda (r) (setq got r)) (list :v "x"))
    (should (string-prefix-p "Error:" (plist-get got :result)))
    (should (string-match-p "boom" (plist-get got :result))))

  :doc "source replaced: dispatcher picks up fresh :function on next call"
  (let* ((name "test-wrap-replace")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :function (lambda (_v) "one")
                  :args '((:name "v" :type string :description "v"))))
         (handler (niwa-tool--call-wrapped-handler source))
         (first nil)
         (second nil))
    (funcall handler (lambda (r) (setq first r)) (list :v "x"))
    (should (equal '(:result "one") first))
    (test-niwa-tool-wrap--make-source
     :name name
     :function (lambda (_v) "two")
     :args '((:name "v" :type string :description "v")))
    (funcall handler (lambda (r) (setq second r)) (list :v "x"))
    (should (equal '(:result "two") second)))

  :doc "in-place nested schema drift is rejected before dispatch"
  (let* ((name "test-wrap-nested_drift")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :function (lambda (_mode) "called")
                  :args '((:name "mode" :type string :description "mode"
                                  :enum ["one" "two"]))))
         (handler (niwa-tool--call-wrapped-handler source))
         got)
    (aset (plist-get (car (gptel-tool-args source)) :enum) 0 "changed")
    (funcall handler (lambda (result) (setq got result)) '(:mode "one"))
    (should (string-match-p "contract changed; re-wrap"
                            (plist-get got :result))))

  :doc "source unregistered: dispatcher returns actionable error"
  (let* ((name "test-wrap-gone")
         (source (test-niwa-tool-wrap--make-source
                  :name name
                  :function (lambda () "ok")))
         (handler (niwa-tool--call-wrapped-handler source))
         (got nil))
    (test-niwa-tool-wrap--remove-source "test-src" name)
    (funcall handler (lambda (r) (setq got r)) nil)
    (should (string-prefix-p "Error:" (plist-get got :result)))
    (should (string-match-p "unregistered" (plist-get got :result)))
    (should (string-match-p name (plist-get got :result)))))


;;
;;; niwa-define-tool :wrap integration

(niwa-deftest niwa-define-tool--wrap
  (:quiet t :before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "wrap derives name/args, forces async-p=t, defaults category to niwa-<src>"
  (let* ((name "test-wrap-wrap_default")
         (src (test-niwa-tool-wrap--make-source
               :name name
               :description "src desc"
               :args '((:name "x" :type string :description "x"))
               :async nil)))
    (niwa-define-tool
      :wrap src
      :groups (web)
      :read-only-p t
      :max-result-size 12345)
    (let ((tool (niwa-tool-get name "niwa-test-src")))
      (should tool)
      (should (equal name (niwa-tool-name tool)))
      (should (eq t (niwa-tool-read-only-p tool)))
      ;; Wrapped handlers always take (callback args), so the pipeline
      ;; must treat them as async regardless of the source's async flag.
      (should (eq t (niwa-tool-async-p tool)))
      (should (= 12345 (niwa-tool-max-result-size tool)))
      (should (equal "src desc" (niwa-tool-description tool)))
      (should (memq 'web (niwa-tool-groups tool)))
      (should (equal '(x) (mapcar #'car (niwa-tool-args tool))))))

  :doc "description override applies to copy but not source"
  (let* ((name "test-wrap-wrap_desc")
         (src (test-niwa-tool-wrap--make-source
               :name name :description "src-side")))
    (niwa-define-tool
      :wrap src
      :description "niwa-side"
      :groups (web)
      :read-only-p t)
    (let ((tool (niwa-tool-get name "niwa-test-src")))
      (should (equal "niwa-side" (niwa-tool-description tool))))
    (should (equal "src-side"
                   (gptel-tool-description
                    (gptel-get-tool (list "test-src" name))))))

  :doc "explicit :category override wins"
  (let* ((name "test-wrap-wrap_cat")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (niwa-define-tool
      :wrap src
      :category "customcat"
      :groups (web)
      :read-only-p t)
    (should (niwa-tool-get name "customcat"))
    (should-not (niwa-tool-get name "niwa-test-src")))

  :doc "rejects redeclared :name"
  (let* ((name "test-wrap-wrap_reject_name")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (should-error (macroexpand
                   `(niwa-define-tool
                      :wrap ,src
                      :name "other"
                      :read-only-p t))
                  :type 'error))

  :doc "rejects redeclared :args"
  (let* ((name "test-wrap-wrap_reject_args")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (should-error (macroexpand
                   `(niwa-define-tool
                      :wrap ,src
                      :args ((x string :required "x"))
                      :read-only-p t))
                  :type 'error))

  :doc "rejects :contextualize, which only native tools support"
  (let* ((name "test-wrap-wrap_reject_contextualize")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (should-error (macroexpand
                   `(niwa-define-tool
                      :wrap ,src
                      :contextualize ,#'ignore
                      :read-only-p t))
                  :type 'error))

  :doc "double-wrap at the same (category name) replaces the registry entry"
  (let* ((name "test-wrap-wrap_double")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (niwa-define-tool
      :wrap src
      :groups (web)
      :read-only-p t
      :max-result-size 1)
    (let ((initial (niwa-tool-get name "niwa-test-src")))
      (niwa-define-tool
        :wrap src
        :description "fresh"
        :groups (xtra)
        :max-result-size 77)
      (let ((refreshed (niwa-tool-get name "niwa-test-src")))
        (should refreshed)
        (should-not (eq initial refreshed))
        (should (equal "fresh" (niwa-tool-description refreshed)))
        (should (equal '(xtra) (niwa-tool-groups refreshed)))
        (should-not (niwa-tool-read-only-p refreshed))
        (should (= 77 (niwa-tool-max-result-size refreshed)))
        (should (= 1
                   (cl-count-if
                    (lambda (tool)
                      (and (equal name (niwa-tool-name tool))
                           (equal "niwa-test-src"
                                  (niwa-tool-category tool))))
                    (niwa-tool-all)))))))

  :doc "second wrap under a different :category is legal"
  (let* ((name "test-wrap-wrap_sibling")
         (src (test-niwa-tool-wrap--make-source :name name)))
    (niwa-define-tool :wrap src :groups (web) :read-only-p t)
    (niwa-define-tool
      :wrap src :category "sibling" :groups (web) :read-only-p t)
    (should (niwa-tool-get name "niwa-test-src"))
    (should (niwa-tool-get name "sibling")))

  :doc "source struct :function stays untouched after wrap"
  (let* ((name "test-wrap-wrap_untouched")
         (orig (lambda (_v) "source"))
         (src (test-niwa-tool-wrap--make-source
               :name name
               :function orig
               :args '((:name "v" :type string :description "v")))))
    (niwa-define-tool :wrap src :groups (web) :read-only-p t)
    (should (eq orig (gptel-tool-function
                      (gptel-get-tool (list "test-src" name))))))

  :doc "wrapped provider and internal schemas do not alias the source"
  (let* ((name "test-wrap-wrap_schema_isolated")
         (src (test-niwa-tool-wrap--make-source
               :name name
               :args '((:name "tasks" :type array :description "tasks"
                        :items (:type object))))))
    (niwa-define-tool :wrap src :groups (web) :read-only-p t)
    (let* ((source-args (gptel-tool-args src))
           (wrapped (niwa-tool-get name "niwa-test-src"))
           (target-args (gptel-tool-args (niwa-tool-gptel-tool wrapped))))
      (should-not (eq source-args target-args))
      (should-not (eq (plist-get (car source-args) :items)
                      (plist-get (car target-args) :items)))
      (should (eq 'object
                  (plist-get (plist-get (car (niwa-tool-args wrapped))
                                        :items)
                             :type)))))

  :doc "wrapped gptel-tool preserves the source include slot"
  (let* ((name "test-wrap-wrap_include")
         (src (test-niwa-tool-wrap--make-source
               :name name
               :include nil)))
    (niwa-define-tool :wrap src :groups (web) :read-only-p t)
    (should-not (gptel-tool-include
                 (gptel-get-tool (list "niwa-test-src" name)))))

  :doc "sync source runs end-to-end through the niwa pipeline"
  (let* ((name "test-wrap-wrap_sync_e2e")
         (src (test-niwa-tool-wrap--make-source
               :name name
               :function (lambda (v) (format "got:%s" v))
               :args '((:name "v" :type string :description "v"))
               :async nil)))
    (niwa-define-tool :wrap src :groups (web) :read-only-p t)
    (let* ((gtool (gptel-get-tool (list "niwa-test-src" name)))
           (result nil))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               "hello")
      (should (equal "got:hello" result)))))


;;
;;; Preset extras merging

(niwa-deftest niwa-preset--setup-extras
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "deferred extras augment the session's tool-catalog at :post time"
  (let* ((extra-name "test-wrap-extra")
         (src (test-niwa-tool-wrap--make-source
               :name extra-name
               :description "extra desc")))
    (niwa-define-tool :wrap src :groups (xtra) :read-only-p t)
    (let* ((session (niwa-session--create
                     :workspace (niwa-workspace--create :root "/tmp")))
           (niwa--session session)
           (gptel-tools nil))
      (let ((niwa-preset-extra-tool-specs
             '((extratest . ((:discoverable xtra))))))
        (niwa-preset--setup-extras 'extratest)
        (let ((set (niwa-session-tool-catalog session)))
          (should (cl-some
                   (lambda (entry)
                     (equal extra-name (cadr (car entry))))
                   set)))))))


;;
;;; Agent extras merging

(niwa-deftest niwa-agent--effective-specs
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "effective specs append extras from defcustom"
  (let* ((agent (niwa-agent--create
                 :name "e-agent"
                 :tools '(read (:tool "Bash"))))
         (niwa-agent-extra-tool-specs
          '((e-agent . ((:discoverable xtra))))))
    (let ((specs (niwa-agent--effective-specs agent)))
      (should (member '(:discoverable xtra) specs))
      (should (member 'read specs))))

  :doc "adds the common communication tools when no extras are registered"
  (let ((agent (niwa-agent--create :name "other-agent" :tools '(read)))
        (niwa-agent-extra-tool-specs nil))
    (should (equal '(read (:tool "ToolSearch") (:tool "ToolCall")
                          (:discoverable notes)
                          (:tool "SendMessage")
                          (:discoverable (:tool "ListAgents")))
                   (niwa-agent--effective-specs agent)))))

(provide 'test-niwa-tool-wrap)
;;; test-niwa-tool-wrap.el ends here
