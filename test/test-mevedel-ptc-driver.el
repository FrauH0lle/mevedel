;;; test-mevedel-ptc-driver.el -- Tests for the ToolScript driver -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises nested dispatch, retention, audit settlement, and cancellation.

;;; Code:

(require 'mevedel-ptc-driver)
(require 'mevedel-tool-ptc)
(require 'mevedel-ptc-checkpoint)
(require 'mevedel-tool-fs)
(require 'mevedel-execution-target)
(require 'cl-lib)
(require 'mevedel-hooks)
(require 'mevedel-permission-queue)
(require 'mevedel-telemetry)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-render-data)
(require 'mevedel-pipeline)
(require 'mevedel-turn)
(require 'mevedel-tools)
(require 'mevedel-agents)
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

(defun test-mevedel-ptc-driver--gptel-tools (&rest names)
  "Return the gptel tool structs registered for NAMES."
  (delq nil (mapcar (lambda (name)
                      (when-let* ((tool (ignore-errors (mevedel-tool-ensure name))))
                        (mevedel-tool-gptel-tool tool)))
                    names)))

(defun test-mevedel-ptc-driver--visible (result)
  "Return RESULT without the hidden render-data block the pipeline appends."
  (if (string-match "\n?<!-- mevedel-render-data -->" result)
      (substring result 0 (match-beginning 0))
    result))

(defun test-mevedel-ptc-driver--register-probe
    (handler &optional async-p check-permission-async)
  "Register a real pipeline tool using HANDLER for driver tests."
  (let* ((args '((value string :required "Value")))
         (tool (mevedel-tool--create
                :name "PTCProbe" :category "mevedel"
                :handler handler :description "ToolScript test probe"
                :prompt "ToolScript test probe" :args args
                :read-only-p t :async-p async-p
                :check-permission-async check-permission-async))
         (gptel-tool
          (gptel-make-tool
           :name "PTCProbe" :description "ToolScript test probe"
           :function #'ignore :args nil :async t :category "mevedel")))
    (setf (mevedel-tool-gptel-tool tool) gptel-tool)
    (mevedel-tool-register tool)))

(defun test-mevedel-ptc-driver--select-probe (buffer)
  "Make PTCProbe the nested roster in BUFFER."
  (with-current-buffer buffer
    (setq-local gptel-tools
                (test-mevedel-ptc-driver--gptel-tools "PTCProbe"))))

(defun test-mevedel-ptc-driver--run (buffer script)
  "Run SCRIPT through ToolScript in BUFFER and return its visible result."
  (let ((deadline (+ (float-time) 10.0))
        done result)
    (with-current-buffer buffer
      (mevedel-pipeline-run-tool
       (mevedel-tool-ensure "ToolScript")
       (lambda (value) (setq result value done t))
       (list :script script))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should done)
    (test-mevedel-ptc-driver--visible result)))

(defun test-mevedel-ptc-driver--telemetry (session)
  "Return persisted and pending telemetry entries for SESSION."
  (let ((path (file-name-concat (mevedel-session-save-path session)
                                mevedel-telemetry-file-name))
        entries)
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (condition-case nil
            (while t (push (read (current-buffer)) entries))
          (end-of-file))))
    (append (nreverse entries)
            (reverse (mevedel-session-telemetry-pending session)))))


;;
;;; Retained child data

(mevedel-deftest mevedel-ptc-driver--bound-strings ()
  ,test
  (test)
  :doc "truncates every nested string without changing non-string values"
  (should
   (equal '("abc..." (:nested "def..." 7))
          (mevedel-ptc-driver--bound-strings
           '("abcdef" (:nested "defghi" 7)) 3))))

(mevedel-deftest mevedel-ptc-driver--child-args ()
  ,test
  (test)
  :doc "bounds one retained argument string at the configured limit"
  (let* ((text (make-string (1+ mevedel-ptc-driver--arg-value-max) ?x))
         (bounded (plist-get (mevedel-ptc-driver--child-args
                              (list :value text))
                             :value)))
    (should (= (+ mevedel-ptc-driver--arg-value-max 3) (length bounded)))
    (should (string-suffix-p "..." bounded)))
  :doc "drops aggregate or deeply nested arguments before recursive truncation"
  (should-not
   (mevedel-ptc-driver--child-args
    (list :value (make-string (1+ mevedel-ptc-driver--arg-total-max) ?x))))
  (should-not
   (mevedel-ptc-driver--child-args
    (list :value (make-list (1+ mevedel-ptc-driver--arg-node-max) "x")))))

(mevedel-deftest mevedel-ptc-driver--child-render-data ()
  ,test
  (test)
  :doc "retains details at the value limit and drops oversized details"
  (let ((at-limit (make-string mevedel-ptc-driver--child-detail-max ?x))
        (over-limit (make-string (1+ mevedel-ptc-driver--child-detail-max) ?x)))
    (should (eq at-limit (mevedel-ptc-driver--child-render-data at-limit)))
    (should-not (mevedel-ptc-driver--child-render-data over-limit))
    (should-not (mevedel-ptc-driver--child-render-data nil)))
  :doc "drops guest closures from durable child render data"
  (let ((state (mevedel-ptc-start "(lambda () 1)" nil)))
    (unwind-protect
        (let ((closure (cadr (mevedel-ptc-step state))))
          (should-not (mevedel-ptc-driver--child-render-data closure)))
      (mevedel-ptc-close state))))



;;
;;; Argument conversion

(mevedel-deftest mevedel-ptc-driver--convert-args
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "interns declared guest keywords as host keywords"
  (progn
    (mevedel-tool-fs--register)
    (let* ((tool (mevedel-tool-get "Read"))
           (guest (list (intern ":file_path" (obarray-make)) "/tmp/x")))
      (should (equal '(:file_path "/tmp/x")
                     (mevedel-ptc-driver--convert-args tool guest)))))

  :doc "rejects an undeclared argument and names what the tool accepts"
  (progn
    (mevedel-tool-fs--register)
    (let* ((tool (mevedel-tool-get "Read"))
           (guest (list (intern ":nonesuch" (obarray-make)) 1)))
      ;; Emacs renders ` and ' as curly quotes in error text, so match the
      ;; stable parts rather than the quoting.
      (let ((message (cadr (should-error
                            (mevedel-ptc-driver--convert-args tool guest)
                            :type 'error))))
        (should (string-match-p "has no argument" message))
        (should (string-match-p "nonesuch" message))
        (should (string-match-p "file_path" message)))))

  :doc "rejects arguments that are not keyword and value pairs"
  (progn
    (mevedel-tool-fs--register)
    (let ((tool (mevedel-tool-get "Read")))
      (should-error (mevedel-ptc-driver--convert-args tool '("positional")))
      (should-error (mevedel-ptc-driver--convert-args
                     tool (list (intern ":file_path" (obarray-make))))))))


;;
;;; Driving a script through the pipeline

(mevedel-deftest mevedel-ptc-driver-run
  (:vars ((mevedel-ptc-driver--next-envelope-id 0))
   :vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-ptc-" t)))
           (save-path (make-temp-file "mevedel-ptc-save-" t))
           (workspace (mevedel-workspace--create
                       :type 'test :id root :root root :name "ptc"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create
                     :name "ptc" :workspace workspace
                     :save-path save-path :working-directory root
                     :permission-mode 'full-auto
                     :touched-files (make-hash-table :test #'equal)))
           (buffer (generate-new-buffer " *mevedel-ptc-driver*")))
   :before-each
   (progn
     (mevedel-tool-clear-registry)
     (mevedel-tool-fs--register)
     (mevedel-tool-ptc--register)
     (with-temp-file (file-name-concat root "one.txt") (insert "alpha\n"))
     (with-temp-file (file-name-concat root "two.txt") (insert "beta\n"))
     (with-current-buffer buffer
       (setq-local default-directory root
                   mevedel--workspace workspace
                   mevedel--session session
                   gptel-tools (test-mevedel-ptc-driver--gptel-tools "Read"))))
   :after-each
   (progn
     (mevedel-tool-clear-registry)
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when (file-directory-p root) (delete-directory root t))
     (when (file-directory-p save-path) (delete-directory save-path t))))
  ,test
  (test)

  :doc "runs a script with no tool calls and returns its final value"
  (should (equal "3" (test-mevedel-ptc-driver--run buffer "(+ 1 2)")))

  :doc "rejects a closure as the script's final value"
  (should
   (string-match-p
    "closures cannot cross"
    (test-mevedel-ptc-driver--run buffer "(lambda () 1)")))

  :doc "rejects a closure before passing it to a nested tool"
  (let (called)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args) (setq called t) '(:result "unexpected")))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should
       (string-match-p
        "closures cannot cross"
        (test-mevedel-ptc-driver--run
         buffer "(PTCProbe :value (lambda () 1))"))))
    (should-not called))

  :doc "settles errors raised after the timer-driven pump starts"
  (let ((mevedel-ptc-primitive-tools '("Read")))
    (cl-letf (((symbol-function 'mevedel-ptc-driver--child-args)
               (lambda (_args) (error "Audit retention failed"))))
      (should
       (string-match-p
        "Audit retention failed"
        (test-mevedel-ptc-driver--run
         buffer
         (format "(Read :file_path %S)"
                 (file-name-concat root "one.txt")))))))

  :doc "settles errors raised by an asynchronous child callback"
  (let ((mevedel-ptc-parallelism 1)
        done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (child-callback _args)
       (run-at-time 0 nil
                    (lambda ()
                      (funcall child-callback '(:result "done")))))
     t)
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (cl-letf (((symbol-function 'mevedel-ptc-driver--classify-outcome)
                 (lambda (&rest _) (error "Async completion failed"))))
        (with-current-buffer buffer
          (mevedel-pipeline-run-tool
           (mevedel-tool-ensure "ToolScript")
           (lambda (value) (setq result value done t))
           '(:script "(parallel (PTCProbe :value \"one\")
                                (PTCProbe :value \"two\"))")))
        (let ((deadline (+ (float-time) 2.0)))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should done)
        (should (string-match-p "Async completion failed" result))
        (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
               (calls (plist-get render-data :calls)))
          (should (= 2 (length calls)))
          (should (equal '(success interrupted)
                         (mapcar (lambda (call)
                                   (plist-get call :status))
                                 calls)))
          (should-not
           (seq-find
            (lambda (call)
              (memq (plist-get call :status) '(queued running)))
            calls))))))

  :doc "settles with an error when final checkpoint persistence fails"
  (let ((durable-session
         (mevedel-session--create
          :name "ptc" :workspace workspace :save-path save-path
          :working-directory root
          :execution-target (mevedel-execution-target-create root)
          :permission-mode 'full-auto
          :touched-files (make-hash-table :test #'equal))))
    (with-current-buffer buffer
      (setq-local mevedel--session durable-session))
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-ptc-checkpoint-update)
               (lambda (&rest _) nil)))
      (should
       (string-match-p
        "final audit could not be persisted"
        (test-mevedel-ptc-driver--run buffer "(+ 1 2)")))))

  :doc "never checkpoints a script run from a retained-agent buffer"
  (let ((durable-session
         (mevedel-session--create
          :name "ptc" :workspace workspace :save-path save-path
          :working-directory root
          :execution-target (mevedel-execution-target-create root)
          :permission-mode 'full-auto
          :touched-files (make-hash-table :test #'equal)))
        (started nil))
    (with-current-buffer buffer
      (setq-local mevedel--session durable-session)
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create :path "/root/worker")))
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-start)
               (lambda (&rest _) (setq started t) t)))
      (should (equal "3" (test-mevedel-ptc-driver--run buffer "(+ 1 2)")))
      (should-not started)))

  :doc "checks the logical serialized size of a shared final value"
  (let ((mevedel-ptc-max-value-bytes 100))
    (should
     (string-match-p
      "value byte budget"
      (test-mevedel-ptc-driver--run
       buffer
       "(let* ((x \"abcdefghij\")
               (ys (mapcar (lambda (_) x)
                           (list 1 2 3 4 5 6 7 8 9 10 11 12))))
          ys)"))))

  :doc "dispatches nested calls through the pipeline and aggregates them"
  (let ((result (test-mevedel-ptc-driver--run
                 buffer
                 (format "(let ((out nil))
                            (dolist (p (list %S %S))
                              (push (Read :file_path p) out))
                            (length out))"
                         (file-name-concat root "one.txt")
                         (file-name-concat root "two.txt")))))
    (should (equal "2" result)))

  :doc "returns a nested failure as a value the script can branch on"
  (let ((result (test-mevedel-ptc-driver--run
                 buffer
                 (format "(let ((r (Read :file_path %S)))
                            (if (plist-get r :error) :handled :unexpected))"
                         (file-name-concat root "missing.txt")))))
    (should (equal ":handled" result)))

  :doc "uses structured child status and assigns synthetic identity"
  (let (child-id)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args)
       (setq child-id (mevedel-pipeline-active-tool-use-id))
       '(:result "Error: harmless data" :status success)))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should
       (equal "Error: harmless data"
              (test-mevedel-ptc-driver--run
               buffer "(PTCProbe :value \"one\")"))))
    (should (string-suffix-p "/1" child-id)))

  :doc "aborts on structured denial with bounded completed-work details"
  (let ((decisions 0)
        result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value)))
     nil
     (lambda (_tool args cont)
       (setq decisions (1+ decisions))
       (funcall cont (if (equal "two" (plist-get args :value))
                         '(deny . "not allowed")
                       'allow))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (setq result
            (test-mevedel-ptc-driver--run
             buffer
             "(progn (PTCProbe :value \"one\")
                     (PTCProbe :value \"two\"))")))
    (should (= 2 decisions))
    (should (string-match-p "aborted.*denied" result))
    (should (string-match-p "Nested call audit" result))
    (should (string-match-p "/1 PTCProbe.*one.*success" result))
    (should (string-match-p "/2 PTCProbe.*two.*denied" result)))

  :doc "keeps a queued permission from settling the envelope"
  (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
    (test-mevedel-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value)))
     nil
     (lambda (_tool _args cont) (funcall cont 'ask)))
    (test-mevedel-ptc-driver--select-probe buffer)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-head)
               #'ignore))
      (dolist (case '((allow-once . "one")
                      (deny-once . "denied")
                      (aborted . "Error:")))
        (let ((deadline (+ (float-time) 2.0))
              done result)
          (with-current-buffer buffer
            (mevedel-pipeline-run-tool
             (mevedel-tool-ensure "ToolScript")
             (lambda (value) (setq result value done t))
             '(:script "(PTCProbe :value \"one\")")))
          (while (and (null (mevedel-session-permission-queue session))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (should (mevedel-session-permission-queue session))
          (should-not done)
          (mevedel-permission-queue--on-head-outcome
           (car (mevedel-session-permission-queue session)) (car case))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (should done)
          (should (string-match-p (cdr case)
                                  (test-mevedel-ptc-driver--visible result)))))))

  :doc "preserves the recorded malformed Grep calls as script errors"
  (progn
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-mevedel-ptc-driver--gptel-tools "Grep")))
    (dolist
        (script
         '("(split-string
              (Grep :pattern \"^[(]defcustom[ )]\"
                    :path \".\" :glob \"/*.el\"
                    :output_mode \"count\")
              \"\\n\" t)"
           "(split-string
              (Grep :pattern \"^\\\\\\\\(defcustom\\\\b\"
                    :path \".\" :glob \"*.el\"
                    :output_mode \"count\")
              \"\\n\" t)"))
      (should (string-match-p
               "Wrong type argument: stringp"
               (test-mevedel-ptc-driver--run buffer script)))))

  :doc "resumes off-stack, so nested calls do not grow the Lisp stack"
  ;; Most tool callbacks fire synchronously.  Resuming the interpreter
  ;; directly from one recurses per nested call and never returns to the
  ;; command loop, which froze Emacs for the length of the script.  Depth
  ;; must therefore stay flat as the call count rises, not merely finish.
  (let ((depths nil))
    (test-mevedel-ptc-driver--register-probe
     (lambda (args)
       (push (length (backtrace-frames)) depths)
       (list :result (plist-get args :value))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should (equal ":ok"
                     (test-mevedel-ptc-driver--run
                      buffer
                      (format "(progn (dolist (i (list 1 2 3 4 5 6 7 8 9 10))
                                        (PTCProbe :value %S))
                                      :ok)"
                              (file-name-concat root "one.txt"))))))
    (should (= 10 (length depths)))
    (should (< (- (apply #'max depths) (apply #'min depths)) 50)))

  :doc "runs bounded batches concurrently and preserves source result order"
  (let ((active 0)
        (max-active 0)
        (started nil)
        (mevedel-ptc-parallelism 2))
    (test-mevedel-ptc-driver--register-probe
     (lambda (child-callback child-args)
       (let* ((value (plist-get child-args :value))
              (delay (pcase value ("a" 0.03) ("b" 0.01) (_ 0))))
         (push value started)
         (setq active (1+ active)
               max-active (max max-active active))
         (run-at-time
          delay nil
          (lambda ()
            (setq active (1- active))
            (funcall child-callback (list :result value))))))
     t)
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (should (equal '("PTCProbe") (mevedel-tool-ptc--roster))))
      (should
       (equal "(\"a\" \"b\" \"c\")"
              (test-mevedel-ptc-driver--run
               buffer
               "(parallel (PTCProbe :value \"a\")
                          (PTCProbe :value \"b\")
                          (PTCProbe :value \"c\"))"))))
    (should (= 2 max-active))
    (should (equal '("a" "b" "c") (nreverse started))))

  :doc "rejects oversized child results before retaining their contents"
  (let ((mevedel-ptc-max-value-bytes 128))
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args) (list :result (make-string 200 ?x))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should
       (equal ":handled"
              (test-mevedel-ptc-driver--run
               buffer
               "(let ((r (PTCProbe :value \"large\")))
                  (if (plist-get r :error) :handled :unexpected))")))))

  :doc "charges the guest error wrapper around a nested failure"
  (let ((mevedel-ptc-max-retained-bytes 1))
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args) '(:result "x" :status error)))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should
       (string-match-p
        "retained value budget"
        (test-mevedel-ptc-driver--run
         buffer "(PTCProbe :value \"failure\")")))))

  :doc "bounds cumulative parallel results before audit retention"
  (let ((mevedel-ptc-max-retained-bytes 200)
        done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args) (list :result (make-string 120 ?x))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure "ToolScript")
         (lambda (value) (setq result value done t))
         '(:script "(parallel (PTCProbe :value \"a\")
                              (PTCProbe :value \"b\"))")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (= 2 (length calls)))
        (should (string-match-p
                 "retained value budget"
                 (plist-get (nth 1 calls) :result)))
        (should (< (length (plist-get (nth 1 calls) :result)) 200)))))

  :doc "reports only terminal children as completed live work"
  (let (events)
    (test-mevedel-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (cl-letf (((symbol-function
                  'mevedel-view-stream-handle-tool-progress)
                 (lambda (event)
                   (push (copy-tree (plist-get event :facts) t) events))))
        (should
         (equal "one"
                (test-mevedel-ptc-driver--run
                 buffer "(PTCProbe :value \"one\")")))))
    (let ((unfinished
           (seq-find
            (lambda (facts)
              (memq (plist-get (car (plist-get facts :calls)) :status)
                    '(queued running)))
            events)))
      (should unfinished)
      (should (= 0 (plist-get unfinished :completed-count)))))

  :doc "does not launch queued siblings after synchronous batch denial"
  (let ((decisions nil)
        (runs nil)
        (phases nil)
        (mevedel-ptc-parallelism 2)
        done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (args)
       (push (plist-get args :value) runs)
       (list :result "unexpected"))
     nil
     (lambda (_tool args cont)
       (push (plist-get args :value) decisions)
       (funcall cont '(deny . "not allowed"))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (cl-letf (((symbol-function 'mevedel-view-stream-handle-tool-progress)
                 (lambda (event)
                   (setq phases
                         (append phases (list (plist-get event :type)))))))
        (with-current-buffer buffer
          (mevedel-pipeline-run-tool
           (mevedel-tool-ensure "ToolScript")
           (lambda (value) (setq result value done t))
           '(:script "(parallel (PTCProbe :value \"a\")
                                (PTCProbe :value \"b\")
                                (PTCProbe :value \"c\"))")))
        (let ((deadline (+ (float-time) 2.0)))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))))
      (should done)
      (should (string-match-p "batch aborted.*denied" result))
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (equal '(denied cancelled cancelled)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls)))))
    (should (eq 'terminal (car (last phases))))
    (should (equal '("a") (nreverse decisions)))
    (should-not runs))

  :doc "records active siblings cancelled after a batch denial"
  (let ((mevedel-ptc-parallelism 2)
        done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_callback _args) nil)
     t
     (lambda (_tool args cont)
       (funcall cont (if (equal "deny" (plist-get args :value))
                         '(deny . "not allowed")
                       'allow))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure "ToolScript")
         (lambda (value) (setq result value done t))
         '(:script "(parallel (PTCProbe :value \"pending\")
                              (PTCProbe :value \"deny\"))")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (equal '(cancelled denied)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls))))))

  :doc "marks the calls of one concurrent join and leaves sequential ones bare"
  (let ((mevedel-ptc-parallelism 2)
        done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure "ToolScript")
         (lambda (value) (setq result value done t))
         '(:script "(progn (PTCProbe :value \"lead\")
                           (parallel (PTCProbe :value \"a\")
                                     (PTCProbe :value \"b\"))
                           (parallel (PTCProbe :value \"lone\")))")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (batches (mapcar (lambda (call) (plist-get call :batch))
                              (plist-get render-data :calls))))
        ;; The sequential lead call and the one-call join ran nothing
        ;; concurrently, so only the real batch is marked.
        (should (equal '(nil 1 1 nil) batches)))))

  :doc "preserves child media references without payload data"
  (let (done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_args)
       '(:result "image"
         :media ((:mime "image/png" :kind image :path "image.png"
                  :data "QUJD")))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure "ToolScript")
         (lambda (value) (setq result value done t))
         '(:script "(PTCProbe :value \"image\")")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (media (plist-get (car (plist-get render-data :calls)) :media)))
        (should (equal '((:mime "image/png" :kind image :path "image.png"))
                       media)))))

  :doc "records script settlement telemetry without script contents"
  (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
    (test-mevedel-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (should (equal "one"
                   (test-mevedel-ptc-driver--run
                    buffer "(PTCProbe :value \"one\")")))
    (let ((finish
           (seq-find
            (lambda (entry)
              (and (eq 'ptc-script (plist-get entry :event))
                   (eq 'finish (plist-get entry :stage))))
            (test-mevedel-ptc-driver--telemetry session))))
      (should finish)
      (should (eq 'final-value (plist-get finish :outcome)))
      (should (= 1 (plist-get finish :nested-call-count)))
      (should (>= (plist-get finish :duration-ms) 0))
      (should-not (string-match-p "PTCProbe\\|one"
                                  (format "%S" finish)))))

  :doc "categorizes budget failures in script telemetry"
  (let ((mevedel-ptc-max-steps 1))
    (let ((result (test-mevedel-ptc-driver--run buffer "(progn 1 2 3)")))
      (should (string-match-p "step budget" result))
      (should (string-match-p "after 2 steps" result)))
    (let ((finish
           (seq-find
            (lambda (entry)
              (and (eq 'ptc-script (plist-get entry :event))
                   (eq 'finish (plist-get entry :stage))))
            (test-mevedel-ptc-driver--telemetry session))))
      (should (eq 'script-error (plist-get finish :outcome)))
      (should (eq 'step (plist-get finish :budget-kind)))))

  :doc "audits the active child before request cancellation settles ToolScript"
  (let* ((request
          (mevedel-request--create
           :id "ptc-cancel" :session session
           :file-snapshots (make-hash-table :test #'equal)))
         (started nil)
         (mevedel-ptc-parallelism 1)
         done result)
    (test-mevedel-ptc-driver--register-probe
     (lambda (_callback _args) (setq started t)) t)
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (setq-local mevedel--current-request request)
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure "ToolScript")
         (lambda (value) (setq result value done t))
         '(:script "(parallel (PTCProbe :value \"active\")
                              (PTCProbe :value \"queued-1\")
                              (PTCProbe :value \"queued-2\"))")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not started) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should started)
      (should-not done)
      (mevedel-request-drain-cancellers request)
      (should done)
      (let* ((render-data (cdr (mevedel-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (eq 'interrupted (plist-get render-data :outcome)))
        (should (equal '(cancelled cancelled cancelled)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls))))))

  :doc "ignores its request canceller after successful settlement"
  (let* ((durable-session
          (mevedel-session--create
           :name "ptc" :workspace workspace :save-path save-path
           :working-directory root
           :execution-target (mevedel-execution-target-create root)
           :permission-mode 'full-auto
           :touched-files (make-hash-table :test #'equal)))
         (request
         (mevedel-request--create
          :id "ptc-settled" :session durable-session
          :file-snapshots (make-hash-table :test #'equal)))
        (mevedel-telemetry-enabled nil)
        (updates 0)
        last-update)
    (with-current-buffer buffer
      (setq-local mevedel--current-request request
                  mevedel--session durable-session))
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-ptc-checkpoint-update)
               (lambda (_session _buffer _id update)
                 (setq updates (1+ updates)
                       last-update (copy-tree update t))
                 t)))
      (should (equal "3" (test-mevedel-ptc-driver--run buffer "(+ 1 2)")))
      (let ((before (copy-tree last-update t)))
        (should (= 1 updates))
        (mevedel-request-drain-cancellers request)
        (should (= 1 updates))
        (should (equal before last-update)))))

  :doc "journals child audit in memory and writes durably only at settlement"
  (let* ((durable-session
          (mevedel-session--create
           :name "ptc" :workspace workspace :save-path save-path
           :working-directory root
           :execution-target (mevedel-execution-target-create root)
           :permission-mode 'full-auto
           :touched-files (make-hash-table :test #'equal)))
         (request
          (mevedel-request--create
           :id "ptc-journal" :session durable-session
           :file-snapshots (make-hash-table :test #'equal)))
         (mevedel-telemetry-enabled nil)
         (updates 0)
         (notes 0))
    (with-current-buffer buffer
      (setq-local mevedel--current-request request
                  mevedel--session durable-session))
    (cl-letf (((symbol-function 'mevedel-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-ptc-checkpoint-note)
               (lambda (&rest _) (setq notes (1+ notes)) t))
              ((symbol-function 'mevedel-ptc-checkpoint-update)
               (lambda (&rest _) (setq updates (1+ updates)) t)))
      (should (equal "2"
                     (test-mevedel-ptc-driver--run
                      buffer
                      (format "(length (list (Read :file_path %S) (Read :file_path %S)))"
                              (file-name-concat root "one.txt")
                              (file-name-concat root "two.txt")))))
      ;; Per-child audit progress stays in memory; the sidecar is written
      ;; once, at settlement.
      (should (= 1 updates))
      (should (>= notes 2))))

  :doc "uses child hooks and snapshots from the shared pipeline"
  (let* ((path (file-name-concat root "mutated.txt"))
         (request
          (mevedel-request--create
           :id "ptc-mutation" :session session
           :file-snapshots (make-hash-table :test #'equal)))
         (events nil)
         (original-run-event (symbol-function 'mevedel-hooks-run-event)))
    (with-temp-file path (insert "before"))
    (test-mevedel-ptc-driver--register-probe
     (lambda (args)
       (with-temp-file (plist-get args :value) (insert "after"))
       '(:result "changed")))
    (let ((probe (mevedel-tool-ensure "PTCProbe")))
      (setf (mevedel-tool-read-only-p probe) nil
            (mevedel-tool-snapshot-p probe) t
            (mevedel-tool-get-paths probe)
            (lambda (args) (list (plist-get args :value)))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (with-current-buffer buffer
        (setq-local mevedel--current-request request))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event event-plist callback &rest rest)
                   (when (equal "PTCProbe" (plist-get event-plist :tool-name))
                     (push event events))
                   (apply original-run-event
                          event event-plist callback rest))))
        (should (equal "changed"
                       (test-mevedel-ptc-driver--run
                        buffer (format "(PTCProbe :value %S)" path)))))
      (should (equal "before"
                     (gethash path (mevedel-request-file-snapshots request))))
      (should (memq 'PreToolUse events))
      (should (memq 'PostToolUse events))))

  :doc "yields between capped chunks of synchronous batch calls"
  (let ((calls 0)
        observed
        (mevedel-ptc-parallelism 2))
    (test-mevedel-ptc-driver--register-probe
     (lambda (args)
       (setq calls (1+ calls))
       ;; Queue the observer before the first synchronous completion queues
       ;; the next batch turn.  It should still see the whole first chunk.
       (when (= calls 1)
         (run-at-time 0 nil (lambda () (setq observed calls))))
       (list :result (plist-get args :value))))
    (test-mevedel-ptc-driver--select-probe buffer)
    (let ((mevedel-ptc-primitive-tools '("PTCProbe")))
      (should
       (equal '("a" "b" "c" "d" "e")
              (read
               (test-mevedel-ptc-driver--run
                buffer
                "(parallel (PTCProbe :value \"a\")
                           (PTCProbe :value \"b\")
                           (PTCProbe :value \"c\")
                           (PTCProbe :value \"d\")
                           (PTCProbe :value \"e\"))")))))
    (should (= 2 observed)))

  :doc "settles an over-budget script with an error naming the budget"
  ;; The user-visible failure mode: a script that runs away must come back
  ;; as a bounded error, not hang the request.
  (progn
    (let ((mevedel-ptc-max-steps 2000)
          (mevedel-ptc-step-slice 200))
      (should (string-match-p "step budget"
                              (test-mevedel-ptc-driver--run buffer "(while t 1)"))))
    (let ((mevedel-ptc-max-tool-calls 2))
      (let ((result
             (test-mevedel-ptc-driver--run
              buffer
              (format "(let ((n 0)) (while t (setq n (Read :file_path %S))))"
                      (file-name-concat root "one.txt")))))
        (should (string-match-p "tool calls" result))
        (should (string-match-p "before nested call Read at call 3" result)))))

  :doc "reports an unreadable script without running anything"
  (should (string-match-p "Error:" (test-mevedel-ptc-driver--run buffer "(list (a . b))")))

  :doc "reports a forbidden operator as a script error"
  (should (string-match-p "Unknown functions: getenv"
                          (test-mevedel-ptc-driver--run buffer "(getenv \"HOME\")")))

  :doc "a tool outside the allowlist is not callable from a script"
  (let ((mevedel-ptc-primitive-tools '("Glob")))
    (should (string-match-p
             "Unknown functions: Read"
             (test-mevedel-ptc-driver--run
              buffer (format "(Read :file_path %S)" (file-name-concat root "one.txt")))))))


(provide 'test-mevedel-ptc-driver)
;;; test-mevedel-ptc-driver.el ends here
