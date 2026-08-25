;;; test-mevedel-tool-ptc.el -- Tests for the ToolScript tool adapter -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises ToolScript roster construction, registration, and rendering.

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

  :doc "registers ToolScript as an async tool with one required script argument"
  (progn
    (mevedel-tool-ptc--register)
    (let ((tool (mevedel-tool-get "ToolScript")))
      (should tool)
      (should (mevedel-tool-async-p tool))
      (should (equal '(script) (mapcar #'car (mevedel-tool-args tool))))
      ;; Read-only: the envelope itself never modifies state.  Each nested
      ;; call carries its own authority, so read-only request rules deny a
      ;; mutating child individually and the denial aborts the script.
      (should (mevedel-tool-read-only-p tool)))))


;;
;;; Rendering

(mevedel-deftest mevedel-tool-ptc--render ()
  ,test
  (test)

  :doc "keeps the returned value in the body and child output in its own row"
  (with-temp-buffer
    (let* ((rendering
            (mevedel-tool-ptc--render
             "ToolScript" nil "final"
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

  :doc "shows failures and permission waits in live progress"
  (with-temp-buffer
    (let ((header
           (plist-get
            (mevedel-tool-ptc--render
             "ToolScript" nil nil
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
             "ToolScript" nil "final"
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

(mevedel-deftest mevedel-tool-ptc--pure-primitive-reference
  (:vars ((mevedel-tool-ptc--pure-primitive-reference-cache nil)))
  ,test
  (test)

  :doc "generates exact signatures from the closed primitive table"
  (let ((reference (mevedel-tool-ptc--pure-primitive-reference)))
    (dolist (signature
             '("(split-string string [separators] [omit-nulls] [trim])"
               "(string-prefix-p prefix string [ignore-case])"
               "(string-suffix-p suffix string [ignore-case])"
               "(string-search needle haystack [start-pos])"))
      (should (string-match-p (regexp-quote signature) reference)))))


(mevedel-deftest mevedel-tool-ptc--roster
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry)
   :vars ((mevedel-tool-ptc--pure-primitive-reference-cache nil))
   :vars* ((buffer (generate-new-buffer " *mevedel-ptc-roster*"))
           (workspace (mevedel-workspace--create
                       :type 'test :id "ptc-roster" :root "/tmp/ptc-roster/"
                       :name "ptc-roster"))
           (session (mevedel-session--create
                     :name "ptc-roster" :workspace workspace
                     :touched-files (make-hash-table :test #'equal)))))
  ,test
  (test)

  :doc "offers only tools that are both allowlisted and active"
  (unwind-protect
      (progn
        (mevedel-tool-fs--register)
        (with-current-buffer buffer
          (setq-local gptel-tools
                      (test-mevedel-tool-ptc--gptel-tools "Read" "Glob"))
          (let ((mevedel-ptc-primitive-tools '("Read" "Bash")))
            ;; Read is allowlisted and active; Glob is active but not in this
            ;; allowlist; Bash is allowlisted but not active in the request.
            (should (equal '("Read") (mevedel-tool-ptc--roster))))))
    (kill-buffer buffer))

  :doc "offers a deferred tool, which the pipeline can execute regardless"
  (unwind-protect
      (progn
        (mevedel-tool-fs--register)
        (mevedel-tool-code--register)
        (with-current-buffer buffer
          (setq-local gptel-tools (test-mevedel-tool-ptc--gptel-tools "Read")
                      mevedel--session session)
          (setf (mevedel-session-deferred-set session)
                (list (cons (list "mevedel" "Treesitter") "tree-sitter info")))
          (let ((mevedel-ptc-primitive-tools '("Read" "Treesitter" "Bash")))
            ;; Read is active, Treesitter is deferred but still callable, and
            ;; Bash is neither.
            (should (equal '("Read" "Treesitter") (mevedel-tool-ptc--roster))))))
    (kill-buffer buffer))

  :doc "offers nothing when no allowlisted tool is active"
  (unwind-protect
      (progn
        (mevedel-tool-fs--register)
        (with-current-buffer buffer
          (setq-local gptel-tools (test-mevedel-tool-ptc--gptel-tools "Glob"))
          (let ((mevedel-ptc-primitive-tools '("Read")))
            (should (null (mevedel-tool-ptc--roster))))))
    (kill-buffer buffer))

  :doc "intersects the active roster with the owning skill restriction"
  (unwind-protect
      (progn
        (mevedel-tool-fs--register)
        (with-current-buffer buffer
          (setq-local gptel-tools
                      (test-mevedel-tool-ptc--gptel-tools "Read" "Glob")
                      mevedel--current-request
                      (mevedel-request--create :ptc-primitives '("Glob" "Bash")))
          (let ((mevedel-ptc-primitive-tools '("Read" "Glob")))
            ;; Bash is named by the skill but absent from the global allowlist
            ;; and request roster, so the restriction cannot grant it.
            (should (equal '("Glob") (mevedel-tool-ptc--roster))))))
    (kill-buffer buffer))

  :doc "generates the request-local description from active and deferred tools"
  (unwind-protect
      (progn
        (mevedel-tool-fs--register)
        (mevedel-tool-code--register)
        (mevedel-tool-ptc--register)
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Imenu") . "outline")))
          (let* ((ptc (mevedel-tool-gptel-tool
                      (mevedel-tool-ensure "ToolScript")))
                 (read (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read")))
                 (original (gptel-tool-description ptc))
                 (backend (gptel-make-openai "ptc-test" :models '(ptc-test)
                                             :key "unused"))
                 (fsm (gptel-make-fsm
                       :info (list :buffer buffer :backend backend
                                   :tools (list ptc read)
                                   :data (list :tools nil)))))
            (mevedel-tool-ptc--handle-description fsm)
            (let* ((tools (plist-get (gptel-fsm-info fsm) :tools))
                   (request-ptc
                    (seq-find (lambda (tool)
                                (equal "ToolScript" (gptel-tool-name tool)))
                              tools))
                   (description (gptel-tool-description request-ptc))
                   (manual-path
                    (file-name-concat mevedel-tool-registry--source-dir
                                      "docs" "ptc-dialect.md")))
              (should-not (eq ptc request-ptc))
              (should (equal original (gptel-tool-description ptc)))
              (should (string-match-p
                       "(Read :file_path string \\[:offset integer\\]"
                       description))
              (should (string-match-p "(Imenu :file_path string)" description))
              (should (string-match-p
                       (regexp-quote
                        "(split-string string [separators] [omit-nulls] [trim])")
                       description))
              (should (file-readable-p manual-path))
              (should (string-match-p
                       (regexp-quote manual-path)
                       description))
              (should-not (string-match-p
                           "{{PTC_DIALECT_MANUAL_PATH}}" description))
              (should (string-match-p
                       "do not inspect\\(?:.\\|\n\\)*implementation"
                       description))
              (should-not (string-match-p "(Bash " description))))))
    (kill-buffer buffer)))


(provide 'test-mevedel-tool-ptc)
;;; test-mevedel-tool-ptc.el ends here
