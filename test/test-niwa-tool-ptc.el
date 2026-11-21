;;; test-niwa-tool-ptc.el -- Tests for the ToolCall tool adapter -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises ToolCall roster construction, registration, and rendering.

;;; Code:

(require 'niwa-tool-ptc)
(require 'niwa-tool-fs)
(require 'niwa-tool-code)
(require 'niwa-tool-registry)
(require 'niwa-tools)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

(defun test-niwa-tool-ptc--gptel-tools (&rest names)
  "Return the gptel tool structs registered for NAMES."
  (delq nil (mapcar (lambda (name)
                      (when-let* ((tool (ignore-errors (niwa-tool-ensure name))))
                        (niwa-tool-gptel-tool tool)))
                    names)))


;;
;;; Registration

(niwa-deftest niwa-tool-ptc--register
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "registers ToolCall as an async tool with one required expression argument"
  (progn
    (niwa-tool-ptc--register)
    (let ((tool (niwa-tool-get "ToolCall")))
      (should tool)
      (should (niwa-tool-async-p tool))
      (should (equal '(expression) (mapcar #'car (niwa-tool-args tool))))
      ;; Read-only: the envelope itself never modifies state.  Each nested
      ;; call carries its own authority, so read-only request rules deny a
      ;; mutating child individually and the denial aborts the script.
      (should (niwa-tool-read-only-p tool)))))


;;
;;; Rendering

(niwa-deftest niwa-tool-ptc--render ()
  ,test
  (test)

  :doc "summaries retain child failures without processing the returned body"
  (let* ((value (make-string 100000 ?\n))
         (data '(:kind ptc :outcome completed :elapsed-seconds 1.25
                 :calls ((:id "1" :tool "Read" :status error :result "failed"))))
         (split (symbol-function 'split-string))
         summary)
    (let ((niwa-tool-render-summary-only t))
      (cl-letf (((symbol-function 'split-string)
                 (lambda (string &rest args)
                   (should-not (eq string value))
                   (apply split string args))))
        (setq summary (niwa-tool-ptc--render "ToolCall" nil value data))))
    (should-not (plist-get summary :body))
    (should-not (plist-get summary :child-calls))
    (should (eq 'warning (plist-get summary :status)))
    (should (string-search "1 failed" (plist-get summary :header)))
    (let ((full (niwa-tool-ptc--render "ToolCall" nil value data)))
      (should (equal (plist-get summary :header) (plist-get full :header)))
      (should (eq 'warning (plist-get full :status)))
      (should (equal value (plist-get (car (last (plist-get full :child-calls))) :result)))))

  :doc "collected command failure does not mark successful nested input failed"
  (let* ((child '(:id "ptc/1" :tool "WriteStdin" :status error
                  :result "<bash-execution exit_code=\"2\"/>"
                  :render-data (:execution-control input :control-succeeded-p t
                                :outcome failure)))
         (rendering (niwa-tool-ptc--render
                     "ToolCall" nil "handled"
                     (list :kind 'ptc :outcome 'completed :calls (list child)))))
    (should (eq 'error (plist-get child :status)))
    (should (eq 'success (plist-get rendering :status)))
    (should-not (string-search "failed" (plist-get rendering :header)))
    (should (eq 'error (plist-get (car (plist-get rendering :child-calls))
                                 :status))))

  :doc "keeps the returned value in the body and child output in its own row"
  (with-temp-buffer
    (let* ((rendering
            (niwa-tool-ptc--render
             "ToolCall" nil "final"
             '(:kind ptc :outcome completed :elapsed-seconds 1.25
               :calls ((:id "ptc/1" :tool "Read" :status success
                        :args (:file_path "a")
                        :result "first full child output")))))
           (children (plist-get rendering :child-calls)))
      (should (string-match-p "1 call" (plist-get rendering :header)))
      (should (string-match-p "· 1s" (plist-get rendering :header)))
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
            (niwa-tool-ptc--render
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
      (let* ((niwa-tool-ptc-result-collapse-line-threshold 0)
             (inline (niwa-tool-ptc--render
                      "ToolCall" nil value
                      '(:kind ptc :outcome completed :calls nil))))
        (should (string-match-p "line 12" (plist-get inline :body)))
        (should-not (plist-get inline :child-calls)))))

  :doc "a failed script keeps its returned value visible inline"
  (with-temp-buffer
    (let* ((value (mapconcat (lambda (i) (format "err %d" i))
                             (number-sequence 1 12) "\n"))
           (rendering
            (niwa-tool-ptc--render
             "ToolCall" nil value
             '(:kind ptc :outcome error :calls nil))))
      (should (string-match-p "err 12" (plist-get rendering :body)))
      (should-not (plist-get rendering :child-calls))))

  :doc "shows failures and permission waits in live progress"
  (with-temp-buffer
    (let ((header
           (plist-get
            (niwa-tool-ptc--render
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
            (niwa-tool-ptc--render
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

(niwa-deftest niwa-tool-ptc--roster
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry)
   :vars* ((buffer (generate-new-buffer " *niwa-ptc-roster*"))
           (workspace (niwa-workspace--create
                       :type 'test :id "ptc-roster" :root "/tmp/ptc-roster/"
                       :name "ptc-roster"))
           (session (niwa-session--create
                     :name "ptc-roster" :workspace workspace
                     :touched-files (make-hash-table :test #'equal)))))
  (unwind-protect ,test (kill-buffer buffer))
  (test)

  :doc "offers all active tools independently of their composition policy"
  (progn
    (niwa-tool-fs--register)
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-niwa-tool-ptc--gptel-tools "Read" "Glob"))
      (let ((niwa-ptc-composable-tools '("Read" "Bash")))
        ;; Read is allowlisted and active; Glob is active but not in this
        ;; allowlist; Bash is allowlisted but not active in the request.
        (should (equal '("Read" "Glob") (niwa-tool-ptc--roster)))
        (setq-local gptel-tools (test-niwa-tool-ptc--gptel-tools "Glob"))
        (should (equal '("Glob") (niwa-tool-ptc--roster))))))

  :doc "offers a deferred tool, which the pipeline can execute regardless"
  (progn
    (niwa-tool-fs--register)
    (niwa-tool-code--register)
    (with-current-buffer buffer
      (setq-local gptel-tools (test-niwa-tool-ptc--gptel-tools "Read")
                  niwa--session session)
      (setf (niwa-session-tool-catalog session)
            (list (cons (list "niwa" "Treesitter") "tree-sitter info")))
      (let ((niwa-ptc-composable-tools '("Read" "Treesitter" "Bash")))
        ;; Read is active, Treesitter is deferred but still callable, and
        ;; Bash is neither.
        (should (equal '("Read" "Treesitter") (niwa-tool-ptc--roster))))))

  :doc "intersects the active roster with the owning skill restriction"
  (progn
    (niwa-tool-fs--register)
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-niwa-tool-ptc--gptel-tools "Read" "Glob")
                  niwa--current-request
                  (niwa-request--create :ptc-primitives '("Glob" "Bash")))
      (let ((niwa-ptc-composable-tools '("Read" "Glob")))
        ;; Bash is named by the skill but absent from the global allowlist
        ;; and request roster, so the restriction cannot grant it.
        (should (equal '("Glob") (niwa-tool-ptc--roster))))))

  :doc "keeps the registered description stable as the catalog changes"
  (progn
    (niwa-tool-ptc--register)
    (with-current-buffer buffer
      (setq-local niwa--session session)
      (let* ((tool (niwa-tool-ensure "ToolCall"))
             (before (gptel-tool-description (niwa-tool-gptel-tool tool))))
        (setf (niwa-session-tool-catalog session)
              '((("niwa" "Imenu") . "outline")))
        (should (equal before (gptel-tool-description (niwa-tool-gptel-tool tool))))
        (should (string-search "niwa://ptc-dialect.md" before))))))

(niwa-deftest niwa-tool-ptc--roster/visibility
  (:before-each (niwa-tools-register))
  ,test
  (test)
  :doc "Plan and child visibility also bound deferred calls and their listing"
  (with-temp-buffer
    (let* ((names '("Eval" "SharedEdit" "ApplyPatch" "Monitor" "Imenu"))
           (catalog (mapcar (lambda (name) (cons (list "niwa" name) name)) names))
           (session (niwa-session--create :tool-catalog catalog :plan-mode t)))
      (setq-local niwa--session session
                  gptel-tools (test-niwa-tool-ptc--gptel-tools "ToolCall"))
      ;; At prompt construction there need not be a bound engine yet.
      (should (equal '("ApplyPatch" "Monitor" "Imenu") (niwa-tool-ptc--roster)))
      (let ((listing (niwa-tools-listing-section)))
        (should-not (string-search "(Eval " listing))
        (should-not (string-search "(SharedEdit " listing)))
      ;; The immutable child stamp survives leaving the parent's Plan mode.
      (setf (niwa-session-plan-mode session) nil)
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :parent-session session :buffer (current-buffer)
                   :runtime-context (list :buffer (current-buffer))
                   :plan-read-only t :tool-catalog catalog))
      (dolist (owner (list nil
                          (gptel-make-fsm :info (list :buffer (current-buffer)))
                          niwa--agent-invocation))
        (let ((niwa-tools--current-engine owner))
          (should (equal '("ApplyPatch" "Imenu") (niwa-tool-ptc--roster))))))))

(niwa-deftest niwa-tool-ptc--call-template ()
  ,test
  (test)
  :doc "supplies a parseable one-line expression with required placeholders"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry)))
    (niwa-tool-register
     (niwa-tool--create :name "TemplateProbe" :category "niwa"
                           :args '((path path :required "Path")
                                   (extra string nil "Optional"))))
    (should (equal "(TemplateProbe :path \"<path>\")"
                   (niwa-tool-ptc--call-template "TemplateProbe")))))

(niwa-deftest niwa-tool-ptc--active-tool-names ()
  ,test
  (test)
  :doc "keeps invalid native extras outside the expression roster"
  (let ((niwa-tool--registry (make-hash-table :test #'equal))
        (gptel-tools nil))
    (dolist (spec '(("Probe" "server/team") ("list" "niwa") ("Good" "server")))
      (let* ((tool (niwa-tool--create :name (car spec) :category (cadr spec)))
             (native (gptel-make-tool :name (car spec) :category (cadr spec))))
        (setf (niwa-tool-gptel-tool tool) native)
        (niwa-tool-register tool)
        (push native gptel-tools)))
    (should (equal '("server/Good") (niwa-tool-ptc--active-tool-names))))

  :doc "rejects foreign same-name implementations but accepts request-local schema copies"
  (let ((niwa-tool--registry (make-hash-table :test #'equal)))
    (niwa-tool-fs--register)
    (let* ((native (niwa-tool-gptel-tool (niwa-tool-get "Read" "niwa")))
           (copy (gptel--copy-tool native))
           (foreign (gptel--copy-tool native))
           (gptel-tools (list copy foreign)))
      (setf (gptel-tool-description copy) "Request-local description"
            (gptel-tool-function foreign) #'ignore)
      (should (equal '("Read") (niwa-tool-ptc--active-tool-names)))))

  :doc "an explicitly empty request roster does not inherit buffer tools"
  (let ((niwa-tool--registry (make-hash-table :test #'equal)))
    (niwa-tool-fs--register)
    (let ((gptel-tools (test-niwa-tool-ptc--gptel-tools "Read"))
          (niwa-tools--current-engine (gptel-make-fsm :info '(:tools nil))))
      (should-not (niwa-tool-ptc--active-tool-names)))))

(niwa-deftest niwa-tool-ptc--tool-declaration ()
  ,test
  (test)
  :doc "compact declarations expose the same standalone restriction as execution"
  (let ((niwa-tool--registry (make-hash-table :test #'equal)))
    (niwa-tool-ensure "Skill")
    (niwa-tool-ensure "Read")
    (should (string-search "[standalone only]"
                           (niwa-tool-ptc--tool-declaration "Skill")))
    (should-not (string-search "[standalone only]"
                               (niwa-tool-ptc--tool-declaration "Read")))))

(niwa-deftest niwa-tool-ptc--handler ()
  ,test
  (test)
  :doc "forwards the expression and restricted roster with standalone tools"
  (with-temp-buffer
    (niwa-tool-fs--register)
    (setq-local gptel-tools
                (test-niwa-tool-ptc--gptel-tools "Read" "Glob" "Grep")
                niwa--current-request
                (niwa-request--create :ptc-primitives '("Read" "Glob")))
    (let ((niwa-ptc-composable-tools '("Read")))
      (cl-letf (((symbol-function 'niwa-ptc-driver-run)
                 (lambda (callback expression roster &optional standalone)
                   (funcall callback (list expression roster standalone)))))
        (should
         (equal '("(Glob :pattern \"*.el\")" ("Read" "Glob") ("Glob"))
                (niwa-tool-ptc--handler
                 #'identity '(:expression "(Glob :pattern \"*.el\")"))))))))

(provide 'test-niwa-tool-ptc)
;;; test-niwa-tool-ptc.el ends here
