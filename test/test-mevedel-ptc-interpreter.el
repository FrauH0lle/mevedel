;;; test-mevedel-ptc-interpreter.el -- Tests for the ToolScript guest interpreter -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises the closed ToolScript guest language, budgets, and machine.

;;; Code:

(require 'mevedel-ptc-interpreter)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar test-mevedel-ptc--expander-ran nil
  "Set by a host macro whose expander must never run on guest text.")

(defmacro test-mevedel-ptc--effectful-macro ()
  "A host macro with an effectful expander, used as a regression probe."
  (setq test-mevedel-ptc--expander-ran t)
  99)

(defun test-mevedel-ptc--host (value)
  "Return VALUE with guest symbols replaced by host symbols of the same name.

Guest text is read into a private obarray, so a symbol a script produces
is deliberately not `eq' to the host symbol spelled the same way.  Cases
that assert on returned data compare through this; the boundary itself is
asserted separately."
  (cond
   ((consp value) (cons (test-mevedel-ptc--host (car value))
                        (test-mevedel-ptc--host (cdr value))))
   ((and value (symbolp value)) (intern (symbol-name value)))
   (t value)))

(defvar test-mevedel-ptc--pause-limit 20
  "Fuel pauses a runaway script may take before the harness gives up.")

(defun test-mevedel-ptc--run (script &optional roster tool-fn)
  "Drive SCRIPT to completion with ROSTER and canned TOOL-FN results.

Return a plist with `:outcome' (`done', `error', or `runaway'), `:value',
`:kind' for typed errors, `:calls' (the list of (NAME ARGS) actually
dispatched), and `:pauses'."
  (let ((state (mevedel-ptc-start script roster))
        (calls nil)
        (pauses 0)
        (resume nil)
        (result nil))
    (catch 'settled
      (while t
        (let ((step (mevedel-ptc-step state resume)))
          (setq resume nil)
          (pcase (car step)
            (:pause
             (setq pauses (1+ pauses))
             ;; A runaway script must pause rather than hang; stop well before
             ;; the step budget so the case stays fast.
             (when (> pauses test-mevedel-ptc--pause-limit)
               (throw 'settled (setq result (list :outcome 'runaway)))))
            (:tool
             (push (list (nth 1 step) (nth 2 step)) calls)
             (setq resume (if tool-fn
                              (funcall tool-fn (nth 1 step) (nth 2 step))
                            (format "<%s>" (nth 1 step)))))
            (:tools
             (setq resume
                   (mapcar
                    (lambda (call)
                      (push call calls)
                      (if tool-fn
                          (funcall tool-fn (car call) (cadr call))
                        (format "<%s>" (car call))))
                    (nth 1 step))))
            (:done (throw 'settled (setq result (list :outcome 'done
                                                      :value (nth 1 step)))))
            (:error (throw 'settled (setq result (list :outcome 'error
                                                       :kind (nth 1 step)
                                                       :value (nth 2 step)))))))))
    (append (if (plist-member result :value)
                (plist-put result :value
                           (test-mevedel-ptc--host (plist-get result :value)))
              result)
            (list :calls (test-mevedel-ptc--host (nreverse calls))
                  :pauses pauses))))

(defun test-mevedel-ptc--value-raw (script &optional roster)
  "Return the final value of SCRIPT without canonicalizing guest symbols."
  (let ((state (mevedel-ptc-start script roster))
        (resume nil))
    (catch 'settled
      (while t
        (let ((step (mevedel-ptc-step state resume)))
          (setq resume nil)
          (pcase (car step)
            (:done (throw 'settled (nth 1 step)))
            (:error (error "%s" (nth 2 step)))
            (_ nil)))))))

(defun test-mevedel-ptc--value (script &optional roster tool-fn)
  "Return the final value of SCRIPT, or signal if it did not complete."
  (let ((run (test-mevedel-ptc--run script roster tool-fn)))
    (should (eq 'done (plist-get run :outcome)))
    (plist-get run :value)))

(defun test-mevedel-ptc--error (script &optional roster)
  "Return the error message SCRIPT produced, whether at read or run time."
  (condition-case err
      (let ((run (test-mevedel-ptc--run script roster)))
        (should (eq 'error (plist-get run :outcome)))
        (plist-get run :value))
    (error (error-message-string err))))


;;
;;; Reading and validation

(mevedel-deftest mevedel-ptc-start ()
  ,test
  (test)

  :doc "rejects record syntax that would materialize a live host object"
  (should (string-match-p "Unsupported literal"
                          (test-mevedel-ptc--error "(list #s(hash-table))")))

  :doc "rejects improper lists"
  (should (string-match-p "Improper list"
                          (test-mevedel-ptc--error "(list (a . b))")))

  :doc "bounds nesting depth before evaluation begins"
  (should (string-match-p
           "depth limit"
           (test-mevedel-ptc--error
            (concat (make-string 200 ?\() "1" (make-string 200 ?\))))))

  :doc "bounds input size"
  (let ((mevedel-ptc-max-input-bytes 32))
    (should (string-match-p "byte limit"
                            (test-mevedel-ptc--error "(list 1 2 3 4 5 6 7 8 9 10 11 12)"))))

  :doc "bounds node count, which also rejects circular reader syntax"
  (let ((mevedel-ptc-max-nodes 8))
    (should (string-match-p "node limit"
                            (test-mevedel-ptc--error "(list 1 2 3 4 5 6 7 8 9 10)"))))

  :doc "rejects an empty script and a non-string script"
  (progn
    (should (string-match-p "empty" (test-mevedel-ptc--error "   ")))
    (should-error (mevedel-ptc-start 42 nil)))

  :doc "reads several top-level forms as one implicit progn"
  (should (equal 2 (test-mevedel-ptc--value "(list 1) 2")))

  :doc "rejects an incomplete trailing form instead of ignoring it"
  (should (string-match-p
           "End of file during parsing"
           (test-mevedel-ptc--error "(+ 1 2) (")))

  :doc "returns guest symbols that are not the host symbols of that name"
  (let ((value (test-mevedel-ptc--value-raw "(quote (when a b))")))
    (should (equal "(when a b)" (prin1-to-string value)))
    (should-not (eq (car value) 'when)))

  :doc "keeps guest identifiers out of the host obarray"
  (let ((name "test-mevedel-ptc-unique-guest-identifier"))
    (should-not (intern-soft name))
    (ignore-errors (mevedel-ptc-start (format "(let ((%s 1)) %s)" name name) nil))
    (should-not (intern-soft name))))


;;
;;; Evaluation

(mevedel-deftest mevedel-ptc-step
  (:vars ((test-mevedel-ptc--expander-ran nil)))
  ,test
  (test)

  :doc "rejects a host macro without ever running its expander"
  (progn
    (should (string-match-p
             "Void or forbidden function"
             (test-mevedel-ptc--error "(test-mevedel-ptc--effectful-macro)")))
    (should-not test-mevedel-ptc--expander-ran))

  :doc "rejects eval-when-compile as an unknown operator"
  (should (string-match-p "Void or forbidden function"
                          (test-mevedel-ptc--error "(eval-when-compile (+ 1 2))")))

  :doc "rejects host-state readers that carry the side-effect-free property"
  (progn
    (should (get 'getenv 'side-effect-free))
    (should (string-match-p "Void or forbidden function"
                            (test-mevedel-ptc--error "(getenv \"HOME\")"))))

  :doc "an unknown operator wrapping a tool call runs zero tool calls"
  (let ((run (test-mevedel-ptc--run "(eval-when-compile (Bash :command \"x\"))"
                                    '("Bash"))))
    (should (eq 'error (plist-get run :outcome)))
    (should (null (plist-get run :calls))))

  :doc "funcall and apply reject a forbidden target before evaluating arguments"
  (let ((run (test-mevedel-ptc--run
              "(funcall 'delete-directory (Bash :command \"x\"))" '("Bash"))))
    (should (eq 'error (plist-get run :outcome)))
    (should (null (plist-get run :calls))))

  :doc "a variable named like a transformer is a variable, not an operator"
  (progn
    (should (equal 1 (test-mevedel-ptc--value "(let ((when 1)) when)")))
    (should (equal 5 (test-mevedel-ptc--value
                      "(funcall (lambda (unless) unless) 5)"))))

  :doc "a transformer inside a lambda body applies when the lambda runs"
  (should (equal :yes (test-mevedel-ptc--value
                       "(funcall (lambda (x) (when x :yes)) 1)")))

  :doc "quoted forms are returned unchanged"
  (should (equal '(when a b) (test-mevedel-ptc--value "(quote (when a b))")))

  :doc "transformers validate their shape rather than degrading silently"
  (progn
    (should (string-match-p "push.*expects" (test-mevedel-ptc--error "(push 1)")))
    (should (string-match-p "push.*expects"
                            (test-mevedel-ptc--error "(let ((x nil)) (push 1 x y))")))
    (should (string-match-p "dolist.*expects"
                            (test-mevedel-ptc--error "(dolist (x (list 1) r) x)"))))

  :doc "supports the documented control flow and data forms"
  (progn
    (should (equal '(2 :u) (test-mevedel-ptc--value
                            "(list (when t 1 2) (unless nil :u))")))
    (should (equal '(3 2 1) (test-mevedel-ptc--value
                             "(let ((acc nil)) (dolist (x (list 1 2 3)) (push x acc)) acc)")))
    (should (equal 2 (test-mevedel-ptc--value "(let* ((a 1) (b (+ a 1))) b)")))
    (should (equal '(:yes 2 3) (test-mevedel-ptc--value
                                "(list (cond ((equal 1 2) :no) (t :yes)) (and 1 2) (or nil 3))")))
    (should (equal '(10 20 30) (test-mevedel-ptc--value
                                "(mapcar (lambda (x) (* x 10)) (list 1 2 3))")))
    (should (equal 6 (test-mevedel-ptc--value "(apply '+ 1 (list 2 3))"))))

  :doc "canonicalizes guest nil and t to the host constants"
  (should (equal '(t :t t)
                 (test-mevedel-ptc--value
                  "(list (null nil) (if t :t :f) (equal nil (list)))")))

  :doc "dispatches roster tools and resumes with their result"
  (let ((run (test-mevedel-ptc--run "(concat (Bash :command \"ls\") \"!\")"
                                    '("Bash"))))
    (should (equal "<Bash>!" (plist-get run :value)))
    (should (equal '(("Bash" (:command "ls"))) (plist-get run :calls))))

  :doc "dispatches a roster tool applied indirectly"
  (let ((run (test-mevedel-ptc--run "(mapcar 'Read (list \"a\" \"b\"))" '("Read"))))
    (should (equal '("<Read>" "<Read>") (plist-get run :value)))
    (should (equal 2 (length (plist-get run :calls)))))

  :doc "joins parallel calls in source order"
  (let ((run
         (test-mevedel-ptc--run
          "(parallel (Read :file_path \"a\") (Read :file_path \"b\"))"
          '("Read")
          (lambda (_name args) (cadr args)))))
    (should (equal '("a" "b") (plist-get run :value)))
    (should (equal '(("Read" (:file_path "a"))
                     ("Read" (:file_path "b")))
                   (plist-get run :calls))))

  :doc "fans out one direct tool-call lambda with parallel-map"
  (should
   (equal '("a!" "b!")
          (test-mevedel-ptc--value
           "(parallel-map (lambda (p) (Read :file_path (concat p \"!\")))
                          (list \"a\" \"b\"))"
           '("Read")
           (lambda (_name args) (cadr args)))))

  :doc "rejects nested tool effects while constructing a parallel call"
  (let ((run (test-mevedel-ptc--run
              "(parallel (Read :file_path (Read :file_path \"hidden\")))"
              '("Read"))))
    (should (eq 'error (plist-get run :outcome)))
    (should (string-match-p "may not call another tool"
                            (plist-get run :value)))
    (should (null (plist-get run :calls))))

  :doc "a tool absent from the roster is a forbidden function"
  (should (string-match-p "Void or forbidden function"
                          (test-mevedel-ptc--error "(Bash :command \"x\")" '("Read"))))

  :doc "a runaway script yields fuel pauses instead of blocking"
  ;; A short slice and a low pause limit keep the case instant; the property
  ;; is that the loop pauses at all, not how many times.
  (let* ((mevedel-ptc-step-slice 50)
         (test-mevedel-ptc--pause-limit 3)
         (run (test-mevedel-ptc--run "(while t 1)")))
    (should (eq 'runaway (plist-get run :outcome)))
    (should (> (plist-get run :pauses) 0)))

  :doc "guest nesting does not consume the host evaluation stack"
  (let ((script "1")
        (result nil))
    (dotimes (_ 50)
      (setq script (concat "(+ 1 " script ")")))
    (let ((state (mevedel-ptc-start script nil)))
      (unwind-protect
          (let ((max-lisp-eval-depth 80))
            (while (not result)
              (let ((step (mevedel-ptc-step state)))
                (when (memq (car step) '(:done :error))
                  (setq result step)))))
        (mevedel-ptc-close state)))
    (should (equal '(:done 51) result)))

  :doc "bounds one uninterrupted slice by wall clock, not only by steps"
  ;; Emacs is single threaded, so a slice blocks everything until it yields.
  ;; A step-only bound is not enough because a step's cost varies by three
  ;; orders of magnitude between interpreted and compiled code.
  (let ((mevedel-ptc-step-slice 100000)
        (mevedel-ptc-slice-seconds 0.01)
        (test-mevedel-ptc--pause-limit 3))
    (let ((run (test-mevedel-ptc--run "(while t 1)")))
      ;; The step bound alone would never have paused here.
      (should (eq 'runaway (plist-get run :outcome)))
      (should (> (plist-get run :pauses) 0))))

  :doc "bounds total evaluation steps"
  (let ((mevedel-ptc-max-steps 500)
        (mevedel-ptc-step-slice 100000))
    (let ((run (test-mevedel-ptc--run "(while t 1)")))
      (should (eq 'step (plist-get run :kind)))
      (should (string-match-p "step budget" (plist-get run :value)))))

  :doc "bounds the number of tool calls"
  (let ((mevedel-ptc-max-tool-calls 2))
    (let ((run (test-mevedel-ptc--run
                "(let ((n 0)) (while t (setq n (Bash :command \"x\"))))"
                '("Bash"))))
      (should (eq 'tool-call (plist-get run :kind)))
      (should (string-match-p "tool calls" (plist-get run :value)))))

  :doc "bounds atomic primitive operands and results"
  (let ((mevedel-ptc-max-value-bytes 8))
    (let ((run (test-mevedel-ptc--run "(concat \"12345\" \"67890\")")))
      (should (eq 'value (plist-get run :kind)))
      (should (string-match-p "value byte budget" (plist-get run :value)))))

  :doc "classifies every intentional interpreter budget without parsing prose"
  (progn
    (let ((mevedel-ptc-max-input-bytes 1))
      (condition-case err
          (progn (mevedel-ptc-start "(list 1)" nil) (ert-fail "accepted"))
        (mevedel-ptc-error (should (eq 'input (nth 1 err))))))
    (let ((mevedel-ptc-max-transformed-nodes 1))
      (should (eq 'expansion
                  (plist-get (test-mevedel-ptc--run "(when t 1)") :kind))))
    (let ((mevedel-ptc-max-seconds -1))
      (let ((run (test-mevedel-ptc--run "(while t 1)")))
        (should (eq 'time (plist-get run :kind)))
        (should (string-match-p "after [0-9]+ steps" (plist-get run :value)))))
    (let ((mevedel-ptc-max-retained-bytes 1))
      (should (eq 'retained-value
                  (plist-get (test-mevedel-ptc--run "(list \"xx\")") :kind)))
      (should (eq 'retained-value
                  (plist-get
                   (test-mevedel-ptc--run
                    "(mapcar (lambda (_x) nil) '(1 2))")
                   :kind)))))

  :doc "rejects host regexps that can backtrack inside one guest step"
  (let ((called nil)
        (state (mevedel-ptc--make-state)))
    (dolist (regexp '("\\(?:a+\\)+b"
                      "\\(a\\|aa\\)\\(a\\|aa\\)b"))
      (condition-case err
          (mevedel-ptc--apply-pure
           state "string-match-p"
           (lambda (&rest _) (setq called t))
           (list regexp (make-string 100 ?a)))
        (mevedel-ptc-error
         (should (eq 'atomic-work (nth 1 err))))))
    (should-not called))

  :doc "allows the documented concatenation-oriented regexp subset"
  (progn
    (should (test-mevedel-ptc--value
             "(string-match-p \"^(defcustom\" \"(defcustom x\")"))
    (should (test-mevedel-ptc--value
             "(string-match-p \"[+*?]\" \"+\")"))
    (should (test-mevedel-ptc--value
             "(string-match-p \"a\\\\+b\\\\?\" \"a+b?\")"))
    (should (equal '("a" "b")
                   (test-mevedel-ptc--value
                    "(split-string \"a\\nb\" \"\\n\")"))))

  :doc "preflights split-string amplification before host allocation"
  (let ((called nil)
        (state (mevedel-ptc--make-state))
        (mevedel-ptc-max-value-nodes 10))
    (condition-case err
        (mevedel-ptc--apply-pure
         state "split-string"
         (lambda (&rest _) (setq called t))
         (list "1234567890" ","))
      (mevedel-ptc-error
       (should (eq 'atomic-work (nth 1 err)))))
    (should-not called))

  :doc "checks every guest regexp and preserves host match data"
  (let ((before '(0 1))
        (state (mevedel-ptc--make-state)))
    (set-match-data before)
    (dolist (spec `(("string-trim" ,#'string-trim
                    ("aaa" "a+" nil))
                   ("split-string" ,#'mevedel-ptc--split-string
                    ("abc" "b" nil "a+"))))
      (condition-case err
          (progn
            (mevedel-ptc--apply-pure
             state (nth 0 spec) (nth 1 spec) (nth 2 spec))
            (ert-fail "Unsafe regexp accepted"))
        (mevedel-ptc-error
         (should (eq 'atomic-work (nth 1 err))))))
    (should (equal before (match-data))))

  :doc "uses a fixed split-string default instead of host configuration"
  (let ((split-string-default-separators "X"))
    (should (equal '("a" "b")
                   (test-mevedel-ptc--value
                    "(split-string \"a b\")"))))

  :doc "counts repeated references before an atomic primitive allocates"
  (let* ((shared (list 1 2 3 4))
         (args (list shared shared shared))
         (mevedel-ptc-max-value-bytes 12)
         (called nil)
         (state (mevedel-ptc--make-state)))
    (should-error
     (mevedel-ptc--apply-pure
      state "append" (lambda (&rest _) (setq called t)) args))
    (should-not called))

  :doc "bounds one nested tool result before returning it to the guest"
  (let* ((mevedel-ptc-max-value-bytes 8)
         (run (test-mevedel-ptc--run
               "(Read :file_path \"x\")" '("Read")
               (lambda (_name _args) "123456789"))))
    (should (eq 'error (plist-get run :outcome)))
    (should (string-match-p "value byte budget" (plist-get run :value))))

  :doc "removes numeric amplification that has no orchestration need"
  (should (string-match-p "Void or forbidden function"
                          (test-mevedel-ptc--error "(expt 2 1000000)")))

  :doc "preflights format precision before host allocation"
  (let ((mevedel-ptc-max-value-bytes 100))
    (should (string-match-p "field size"
                            (test-mevedel-ptc--error
                             "(format \"%.1000000s\" \"x\")"))))

  :doc "rejects a circular host value before the guest receives it"
  (let* ((circle (list "x"))
         (run nil))
    (setcdr circle circle)
    (setq run
          (test-mevedel-ptc--run
           "(Read :file_path \"x\")" '("Read")
           (lambda (_name _args) circle)))
    (should (eq 'error (plist-get run :outcome)))
    (should (string-match-p "Circular aggregate" (plist-get run :value)))))

(provide 'test-mevedel-ptc-interpreter)
;;; test-mevedel-ptc-interpreter.el ends here
