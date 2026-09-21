;;; test-mevedel-tool-ptc.el -- Tests for the ToolCall tool adapter -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises ToolCall roster construction, registration, and rendering.

;;; Code:

(require 'mevedel-tool-ptc)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-code)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

(defun test-mevedel-tool-ptc--gptel-tools (&rest names)
  "Return the gptel tool structs registered for NAMES."
  (delq nil (mapcar (lambda (name)
                      (when-let* ((tool (ignore-errors (mevedel-tool-ensure name))))
                        (mevedel-tool-gptel-tool tool)))
                    names)))


;;
;;; Registration

(mevedel-deftest mevedel-tool-ptc--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "registers ToolCall as an async tool with one required expression argument"
  (progn
    (mevedel-tool-ptc--register)
    (let ((tool (mevedel-tool-get "ToolCall")))
      (should tool)
      (should (mevedel-tool-async-p tool))
      (should (equal '(expression) (mapcar #'car (mevedel-tool-args tool))))
      ;; Read-only: the envelope itself never modifies state.  Each nested
      ;; call carries its own authority, so read-only request rules deny a
      ;; mutating child individually and the denial aborts the script.
      (should (mevedel-tool-read-only-p tool)))))


;;
;;; Rendering

(mevedel-deftest mevedel-tool-ptc--render ()
  ,test
  (test)

  :doc "summaries retain child failures without processing the returned body"
  (let* ((value (make-string 100000 ?\n))
         (data '(:kind ptc :outcome completed :elapsed-seconds 1.25
                 :calls ((:id "1" :tool "Read" :status error :result "failed"))))
         (split (symbol-function 'split-string))
         summary)
    (let ((mevedel-tool-render-summary-only t))
      (cl-letf (((symbol-function 'split-string)
                 (lambda (string &rest args)
                   (should-not (eq string value))
                   (apply split string args))))
        (setq summary (mevedel-tool-ptc--render "ToolCall" nil value data))))
    (should-not (plist-get summary :body))
    (should-not (plist-get summary :child-calls))
    (should (eq 'warning (plist-get summary :status)))
    (should (string-search "1 failed" (plist-get summary :header)))
    (let ((full (mevedel-tool-ptc--render "ToolCall" nil value data)))
      (should (equal (plist-get summary :header) (plist-get full :header)))
      (should (eq 'warning (plist-get full :status)))
      (should (equal value (plist-get (car (last (plist-get full :child-calls))) :result)))))

  :doc "keeps the returned value in the body and child output in its own row"
  (with-temp-buffer
    (let* ((rendering
            (mevedel-tool-ptc--render
             "ToolCall" nil "final"
             '(:kind ptc :outcome completed :elapsed-seconds 1.25
               :calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a")
                        :result "first full child output")))))
           (children (plist-get rendering :child-calls)))
      (should (string-match-p "1 call" (plist-get rendering :header)))
      (should (string-match-p "1.2s" (plist-get rendering :header)))
      (should (string-match-p "final" (plist-get rendering :body)))
      ;; The envelope body carries only what the script returned; the child
      ;; result travels as its own row for the nested tool's renderer.
      (should-not (string-match-p "first full child output"
                                  (plist-get rendering :body)))
      (should (equal "first full child output"
                     (plist-get (car children) :result)))
      (should (equal '(:file_path "a") (plist-get (car children) :args)))))

  :doc "folds a long returned value into a trailing collapsed row"
  (with-temp-buffer
    (let* ((value (mapconcat (lambda (i) (format "line %d" i))
                             (number-sequence 1 12) "\n"))
           (rendering
            (mevedel-tool-ptc--render
             "ToolCall" nil value
             '(:kind ptc :outcome completed
               :calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a") :result "child output")))))
           (returned (car (last (plist-get rendering :child-calls)))))
      (should-not (plist-get rendering :body))
      (should (equal "returned" (plist-get returned :id)))
      (should (equal "Returned" (plist-get returned :tool)))
      (should (equal value (plist-get returned :result)))
      ;; A zero threshold keeps the value inline.
      (let* ((mevedel-tool-ptc-result-collapse-line-threshold 0)
             (inline (mevedel-tool-ptc--render
                      "ToolCall" nil value
                      '(:kind ptc :outcome completed :calls nil))))
        (should (string-match-p "line 12" (plist-get inline :body)))
        (should-not (plist-get inline :child-calls)))))

  :doc "a failed script keeps its returned value visible inline"
  (with-temp-buffer
    (let* ((value (mapconcat (lambda (i) (format "err %d" i))
                             (number-sequence 1 12) "\n"))
           (rendering
            (mevedel-tool-ptc--render
             "ToolCall" nil value
             '(:kind ptc :outcome error :calls nil))))
      (should (string-match-p "err 12" (plist-get rendering :body)))
      (should-not (plist-get rendering :child-calls))))

  :doc "shows failures and permission waits in live progress"
  (with-temp-buffer
    (let ((header
           (plist-get
            (mevedel-tool-ptc--render
             "ToolCall" nil nil
             '(:kind ptc :live-p t :completed-count 1 :known-total 3
               :active-tool "Read" :permission-waits ("Bash")
               :calls ((:tool "Grep" :status error :preview "failed"))))
            :header)))
      (should (string-match-p "1/3 completed" header))
      (should (string-match-p "1 failed" header))
      (should (string-match-p "awaiting permission for Bash" header))))

  :doc "carries media references to the child row without payload bytes"
  (with-temp-buffer
    (let* ((rendering
            (mevedel-tool-ptc--render
             "ToolCall" nil "final"
             '(:kind ptc :outcome completed
               :calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "image.png")
                        :result "image"
                        :media ((:mime "image/png" :kind image
                                 :path "image.png")))))))
           (child (car (plist-get rendering :child-calls))))
      (should (string-match-p "image.png"
                              (format "%S" (plist-get child :media))))
      (should-not (string-match-p "QUJD" (format "%S" rendering))))))


;;
;;; Roster

(mevedel-deftest mevedel-tool-ptc--roster
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry)
   :vars* ((buffer (generate-new-buffer " *mevedel-ptc-roster*"))
           (workspace (mevedel-workspace--create
                       :type 'test :id "ptc-roster" :root "/tmp/ptc-roster/"
                       :name "ptc-roster"))
           (session (mevedel-session--create
                     :name "ptc-roster" :workspace workspace
                     :touched-files (make-hash-table :test #'equal)))))
  (unwind-protect ,test (kill-buffer buffer))
  (test)

  :doc "offers all active tools independently of their composition policy"
  (progn
    (mevedel-tool-fs--register)
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-mevedel-tool-ptc--gptel-tools "Read" "Glob"))
      (let ((mevedel-ptc-composable-tools '("Read" "Bash")))
        ;; Read is allowlisted and active; Glob is active but not in this
        ;; allowlist; Bash is allowlisted but not active in the request.
        (should (equal '("Read" "Glob") (mevedel-tool-ptc--roster)))
        (setq-local gptel-tools (test-mevedel-tool-ptc--gptel-tools "Glob"))
        (should (equal '("Glob") (mevedel-tool-ptc--roster))))))

  :doc "offers a deferred tool, which the pipeline can execute regardless"
  (progn
    (mevedel-tool-fs--register)
    (mevedel-tool-code--register)
    (with-current-buffer buffer
      (setq-local gptel-tools (test-mevedel-tool-ptc--gptel-tools "Read")
                  mevedel--session session)
      (setf (mevedel-session-tool-catalog session)
            (list (cons (list "mevedel" "Treesitter") "tree-sitter info")))
      (let ((mevedel-ptc-composable-tools '("Read" "Treesitter" "Bash")))
        ;; Read is active, Treesitter is deferred but still callable, and
        ;; Bash is neither.
        (should (equal '("Read" "Treesitter") (mevedel-tool-ptc--roster))))))

  :doc "intersects the active roster with the owning skill restriction"
  (progn
    (mevedel-tool-fs--register)
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-mevedel-tool-ptc--gptel-tools "Read" "Glob")
                  mevedel--current-request
                  (mevedel-request--create :ptc-primitives '("Glob" "Bash")))
      (let ((mevedel-ptc-composable-tools '("Read" "Glob")))
        ;; Bash is named by the skill but absent from the global allowlist
        ;; and request roster, so the restriction cannot grant it.
        (should (equal '("Glob") (mevedel-tool-ptc--roster))))))

  :doc "keeps the registered description stable as the catalog changes"
  (progn
    (mevedel-tool-ptc--register)
    (with-current-buffer buffer
      (setq-local mevedel--session session)
      (let* ((tool (mevedel-tool-ensure "ToolCall"))
             (before (gptel-tool-description (mevedel-tool-gptel-tool tool))))
        (setf (mevedel-session-tool-catalog session)
              '((("mevedel" "Imenu") . "outline")))
        (should (equal before (gptel-tool-description (mevedel-tool-gptel-tool tool))))
        (should (string-search "mevedel://ptc-dialect.md" before))))))

(mevedel-deftest mevedel-tool-ptc--call-template ()
  ,test
  (test)
  :doc "supplies a parseable one-line expression with required placeholders"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "TemplateProbe" :category "mevedel"
                           :args '((path path :required "Path")
                                   (extra string nil "Optional"))))
    (should (equal "(TemplateProbe :path \"<path>\")"
                   (mevedel-tool-ptc--call-template "TemplateProbe")))))

(mevedel-deftest mevedel-tool-ptc--active-tool-names ()
  ,test
  (test)
  :doc "keeps invalid native extras outside the expression roster"
  (let ((mevedel-tool--registry (make-hash-table :test #'equal))
        (gptel-tools nil))
    (dolist (spec '(("Probe" "server/team") ("list" "mevedel") ("Good" "server")))
      (let* ((tool (mevedel-tool--create :name (car spec) :category (cadr spec)))
             (native (gptel-make-tool :name (car spec) :category (cadr spec))))
        (mevedel-tool-register tool)
        (push native gptel-tools)))
    (should (equal '("server/Good") (mevedel-tool-ptc--active-tool-names)))))

(mevedel-deftest mevedel-tool-ptc--handler ()
  ,test
  (test)
  :doc "forwards the expression and restricted roster with standalone tools"
  (with-temp-buffer
    (mevedel-tool-fs--register)
    (setq-local gptel-tools
                (test-mevedel-tool-ptc--gptel-tools "Read" "Glob" "Grep")
                mevedel--current-request
                (mevedel-request--create :ptc-primitives '("Read" "Glob")))
    (let ((mevedel-ptc-composable-tools '("Read")))
      (cl-letf (((symbol-function 'mevedel-ptc-driver-run)
                 (lambda (callback expression roster &optional standalone)
                   (funcall callback (list expression roster standalone)))))
        (should
         (equal '("(Glob :pattern \"*.el\")" ("Read" "Glob") ("Glob"))
                (mevedel-tool-ptc--handler
                 #'identity '(:expression "(Glob :pattern \"*.el\")"))))))))

(provide 'test-mevedel-tool-ptc)
;;; test-mevedel-tool-ptc.el ends here
