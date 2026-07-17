;;; test-mevedel-execution.el --- Tests for managed child execution -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the bounded one-shot process boundary used by Bash, batch Eval, and
;; native external helpers.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-execution--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "execution"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-mevedel-execution--process-gone-p (pid)
  "Return non-nil when PID no longer names a live process."
  (null (process-attributes pid)))

(defun test-mevedel-execution--read-pid (path)
  "Return the process id stored at PATH."
  (string-to-number
   (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))))

(defun test-mevedel-execution--session (root)
  "Return a materialized test session rooted below ROOT."
  (let* ((workspace (test-mevedel-execution--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (save-path (file-name-as-directory
                     (file-name-concat root "session"))))
    (make-directory save-path t)
    (setf (mevedel-session-save-path session) save-path)
    session))

(defun test-mevedel-execution--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, bounded by TIMEOUT seconds."
  (with-timeout ((or timeout 5) (error "Timed out"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(defun test-mevedel-execution--stop-all (session owner ids)
  "Stop IDS owned by OWNER in SESSION and wait for every settlement."
  (let ((remaining (length ids)))
    (dolist (id ids)
      (mevedel-execution-stop
       session owner id (lambda (_value) (setq remaining (1- remaining)))))
    (test-mevedel-execution--wait (lambda () (zerop remaining)))))

(cl-defun test-mevedel-execution--start-managed
    (session root command &key (owner "main") outcome-function tty timeout
             (yield-time-ms 10))
  "Start managed COMMAND for SESSION at ROOT and return its first observation."
  (let (observation)
    (mevedel-execution-start-bash
     (lambda (value) (setq observation value))
     :session session :owner owner :command command
     :workdir root :writable-roots (list root)
     :artifact-directory (file-name-concat root "artifacts")
     :outcome-function outcome-function
     :timeout timeout :tty tty :yield-time-ms yield-time-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))

(cl-defun test-mevedel-execution--observe
    (session execution-id &key chars (wait-ms 5000))
  "Observe EXECUTION-ID in SESSION and return the delivered observation."
  (let (observation)
    (mevedel-execution-observe
     session "main" execution-id
     (lambda (value) (setq observation value))
     :chars chars :wait-ms wait-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))


;;
;;; One-shot execution

(mevedel-deftest mevedel-execution-start-one-shot ()
  ,test
  (test)
  :doc "settles an exited child even when Emacs does not deliver its sentinel"
  (let ((original-make-process (symbol-function 'make-process))
        (mevedel-sandbox-mode 'off)
        done result)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (apply original-make-process
                        (plist-put args :sentinel #'ignore)))))
      (should-not
       (mevedel-execution-start-one-shot
        (lambda (child-result)
          (setq result child-result
                done t))
        :name "mevedel-test-missed-sentinel"
        :command '("sh" "-c" "printf recovered; exit 7")
        :workdir temporary-file-directory
        :writable-roots (list temporary-file-directory)))
      (with-timeout (2 (error "Missed sentinel was not recovered"))
        (while (not done)
          (accept-process-output nil 0.05)))
      (should (= 7 (plist-get result :exit-code)))
      (should (equal "recovered" (plist-get result :output))))))

(mevedel-deftest mevedel-execution-run-one-shot ()
  ,test
  (test)
  :doc "returns complete output and structured terminal facts"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-success"
            :command '("sh" "-c" "printf 'hello'; printf ' world' >&2")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (equal "hello world" (plist-get result :output)))
      (should-not (plist-get result :timed-out-p))
      (should-not (plist-get result :output-limit-p))
      (should (numberp (plist-get result :wall-time-seconds)))
      (should-not (plist-member result :process))))
  :doc "counts raw output bytes independently of decoded characters"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-byte-count"
            :command '("sh" "-c" "printf '\\303\\244'")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal (string #xe4) (plist-get result :output)))
      (should (= 2 (plist-get result :output-bytes)))))
  :doc "reports spawn failure without exposing a process"
  (let ((mevedel-sandbox-mode 'off))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-failed-spawn"
            :command '("/definitely/missing/mevedel-executable")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= -1 (plist-get result :exit-code)))
      (should (plist-get result :error))
      (should-not (plist-member result :process))))
  :doc "applies stable child defaults while allowing command overrides"
  (let ((mevedel-sandbox-mode 'off))
    (let ((defaults
           (mevedel-execution-run-one-shot
            :name "mevedel-test-environment"
            :command
            '("sh" "-c"
              "printf '%s' \"$NO_COLOR|$TERM|$LC_ALL|$LANG|$COLORTERM|$PAGER|$GIT_PAGER|$GH_PAGER|$MEVEDEL_EXECUTION\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory)))
          (override
           (mevedel-execution-run-one-shot
            :name "mevedel-test-environment-override"
            :command '("sh" "-c" "PAGER=less; printf '%s' \"$PAGER\"")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (equal "1|dumb|C.UTF-8|C.UTF-8||cat|cat|cat|1"
                     (plist-get defaults :output)))
      (should (equal "less" (plist-get override :output)))))
  :doc "terminates a timed-out command and its process group"
  (let* ((root (make-temp-file "mevedel-execution-timeout-" t))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         result pid)
    (unwind-protect
        (progn
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-test-timeout"
                 :command
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                       "mevedel-test-timeout" pid-file)
                 :workdir root
                 :writable-roots (list root)
                 :timeout 0.1))
          (should (plist-get result :timed-out-p))
          (should (file-readable-p pid-file))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (with-timeout (1 (error "Descendant process survived timeout"))
            (while (not (test-mevedel-execution--process-gone-p pid))
              (accept-process-output nil 0.02))))
      (delete-directory root t)))
  :doc "spools large output without retaining a process buffer"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit (* 2 1024 1024)))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-large-output"
            :command
            '("sh" "-c"
              "printf head; head -c 1048576 /dev/zero | tr '\\0' x; printf tail")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should (= (+ 8 1048576) (length (plist-get result :output))))
      (should (string-prefix-p "head" (plist-get result :output)))
      (should (string-suffix-p "tail" (plist-get result :output)))
      (should-not (get-buffer " *mevedel-test-large-output*"))))
  :doc "enforces the output spool cap and reports the limit"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit 4096)
        (mevedel-execution--child-kill-delay 0.05))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-output-cap"
            :command
            '("sh" "-c" "head -c 100000 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (plist-get result :output-limit-p))
      (should (= 4096 (string-bytes (plist-get result :output))))))
  :doc "allows output exactly equal to the configured spool cap"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-execution-output-limit 4096))
    (let ((result
           (mevedel-execution-run-one-shot
            :name "mevedel-test-exact-output-cap"
            :command '("sh" "-c" "head -c 4096 /dev/zero | tr '\\0' x")
            :workdir temporary-file-directory
            :writable-roots (list temporary-file-directory))))
      (should (= 0 (plist-get result :exit-code)))
      (should-not (plist-get result :output-limit-p))
      (should (= 4096 (plist-get result :output-bytes))))))


;;
;;; Managed Bash execution

(mevedel-deftest mevedel-execution--begin-stop ()
  ,test
  (test)
  :doc "latches the first terminal cause across competing stop events"
  (let* ((mevedel-execution--child-kill-delay 30)
         (record (mevedel-execution--record-create)))
    (unwind-protect
        (progn
          (should (mevedel-execution--begin-stop record 'stopped))
          (should-not (mevedel-execution--begin-stop record 'timed-out))
          (should (eq 'stopped
                      (mevedel-execution--record-termination record))))
      (setf (mevedel-execution--record-finished-p record) t)
      (mevedel-execution--release-runtime record))))

(mevedel-deftest mevedel-execution--group-live-p ()
  ,test
  (test)
  :doc "requires a successful process-group probe"
  (let ((record (mevedel-execution--record-create :group-id 42)))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (_group _signal) -1)))
      (should-not (mevedel-execution--group-live-p record)))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (_group _signal) 0)))
      (should (mevedel-execution--group-live-p record)))))

(mevedel-deftest mevedel-execution--utf8-prefix ()
  ,test
  (test)
  :doc "never splits a multibyte character at the byte budget"
  (let ((euro (string #x20ac)))
    (should (equal (concat "a" euro)
                   (mevedel-execution--utf8-prefix
                    (concat "a" euro "b") 4)))
    (should (equal "a" (mevedel-execution--utf8-prefix
                        (concat "a" euro "b") 3)))))

(mevedel-deftest mevedel-execution--retain-unread ()
  ,test
  (test)
  :doc "keeps bounded prefix and suffix text with the full character count"
  (let ((mevedel-execution-inline-output-limit 3)
        (record (mevedel-execution--record-create)))
    (mevedel-execution--retain-unread record "ab")
    (mevedel-execution--retain-unread record "cde")
    (should (= 5 (mevedel-execution--record-unread-chars record)))
    (should (equal "abc" (mevedel-execution--record-unread-head record)))
    (should (equal "cde" (mevedel-execution--record-unread-tail record)))))

(mevedel-deftest mevedel-execution--write-managed-output ()
  ,test
  (test)
  :doc "spools complete UTF-8 characters at the output limit"
  (let* ((spool (make-temp-file "mevedel-managed-write-"))
         (euro (string #x20ac))
         (mevedel-execution-output-limit 4)
         (mevedel-execution--child-kill-delay 30)
         (record
          (mevedel-execution--record-create
           :spool-path spool :newline-count 0)))
    (unwind-protect
        (progn
          (mevedel-execution--write-managed-output
           record (concat "a" euro "b"))
          (should (equal (concat "a" euro)
                         (with-temp-buffer
                           (insert-file-contents spool)
                           (buffer-string))))
          (should (mevedel-execution--record-output-limit-p record)))
      (setf (mevedel-execution--record-finished-p record) t)
      (mevedel-execution--release-runtime record)
      (delete-file spool))))

(mevedel-deftest mevedel-execution--unread-preview ()
  ,test
  (test)
  :doc "uses shared character preview semantics and reports omitted bytes"
  (let ((mevedel-execution-inline-output-limit 10)
        (record
         (mevedel-execution--record-create
          :unread-chars 20
          :unread-head "1234567890"
          :unread-tail "abcdefghij")))
    (let ((preview (mevedel-execution--unread-preview record 20)))
      (should (string-match-p "omitted 10 chars"
                              (plist-get preview :output)))
      (should (= 10 (plist-get preview :omitted))))))

(mevedel-deftest mevedel-execution--managed-append ()
  ,test
  (test)
  :doc "ignores process-filter output delivered after terminal settlement"
  (let ((spool (make-temp-file "mevedel-managed-finished-")))
    (unwind-protect
        (let ((record
               (mevedel-execution--record-create
                :finished-p t :spool-path spool)))
          (mevedel-execution--managed-append record "late output\n")
          (should (= 0 (file-attribute-size (file-attributes spool)))))
      (delete-file spool))))

(mevedel-deftest mevedel-execution--resolve-outcome ()
  ,test
  (test)
  :doc "accepts canonical outcomes and defaults without an adapter resolver"
  (should (eq 'success (mevedel-execution--resolve-outcome nil 0 'exited)))
  (should (eq 'failure (mevedel-execution--resolve-outcome nil 1 'exited)))
  (dolist (outcome '(success failure no-match different false))
    (should
     (eq outcome
         (mevedel-execution--resolve-outcome
          (lambda (_exit-code _termination) outcome)
          1 'exited))))
  :doc "contains throwing and invalid adapter resolvers"
  (dolist (resolver
           (list (lambda (_exit-code _termination) (error "Resolver failed"))
                 (lambda (_exit-code _termination) 'invalid)))
    (should (eq 'success
                (mevedel-execution--resolve-outcome resolver 0 'exited)))
    (should (eq 'failure
                (mevedel-execution--resolve-outcome resolver 1 'exited)))))

(mevedel-deftest mevedel-execution-start-bash ()
  ,test
  (test)
  :doc "requires a session owner before spawning managed Bash"
  (let ((mevedel-sandbox-mode 'off))
    (should-error
     (mevedel-execution-start-bash
      #'ignore :session nil :owner "main"
      :command '("sh" "-c" "printf unreachable")
      :workdir temporary-file-directory
      :writable-roots (list temporary-file-directory))
     :type 'mevedel-execution-error))
  :doc "returns terminal output before yielding and removes the temporary spool"
  (let* ((root (make-temp-file "mevedel-managed-terminal-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "printf done")
                 :yield-time-ms nil))
          (should (equal "done" (plist-get observation :output)))
          (should (eq 'completed
                      (plist-get (plist-get observation :facts) :state)))
          (should-not (plist-get (plist-get observation :facts)
                                 :execution-id))
          (should-not (plist-get (plist-get observation :facts)
                                 :output-path))
          (should-not
           (directory-files (file-name-concat root "artifacts")
                            nil "\\`execution-")))
      (delete-directory root t)))
  :doc "retains the adapter outcome resolver across yield and observation"
  (let* ((root (make-temp-file "mevedel-managed-outcome-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         initial final)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; exit 1")
                 :outcome-function
                 (lambda (exit-code termination)
                   (if (and (= exit-code 1) (eq termination 'exited))
                       'false
                     'failure))))
          (should (eq 'running
                      (plist-get (plist-get initial :facts) :state)))
          (setq final
                (test-mevedel-execution--observe
                 session
                 (plist-get (plist-get initial :facts) :execution-id)))
          (should (= 1 (plist-get (plist-get final :facts) :exit-code)))
          (should (eq 'false
                      (plist-get (plist-get final :facts) :outcome))))
      (delete-directory root t)))
  :doc "retains one oversized pre-yield artifact with a head-and-tail preview"
  (let* ((root (make-temp-file "mevedel-managed-oversized-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-inline-output-limit 10)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf 1234567890abcdefghij")
                 :yield-time-ms nil))
          (should (string-prefix-p "12345" (plist-get observation :output)))
          (should (string-suffix-p "fghij" (plist-get observation :output)))
          (should (string-match-p "omitted 10 chars"
                                  (plist-get observation :output)))
          (should (= 10 (plist-get (plist-get observation :facts)
                                   :omitted-output-bytes)))
          (let ((path (plist-get (plist-get observation :facts)
                                 :output-path)))
            (should (file-exists-p path))
            (should (equal "1234567890abcdefghij"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))))
      (delete-directory root t)))
  :doc "preserves split UTF-8 characters in pipe and PTY filter chunks"
  (let* ((root (make-temp-file "mevedel-managed-utf8-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (dolist (tty '(nil t))
          (let (initial final id)
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root
                   '("sh" "-c"
                     "printf '\\342'; sleep .05; printf '\\202'; sleep .05; printf '\\254'")
                   :tty tty))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (should (equal "" (plist-get initial :output)))
            (setq final (test-mevedel-execution--observe session id))
            (should (equal (string #x20ac) (plist-get final :output)))
            (should (eq (and tty t)
                        (plist-get (plist-get final :facts) :tty)))))
      (delete-directory root t)))
  :doc "line-buffered programs expose progress only through a PTY"
  (skip-unless (executable-find "python3"))
  (let* ((root (make-temp-file "mevedel-managed-buffering-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (dolist (tty '(nil t))
          (let (initial final id)
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root
                   '("python3" "-c"
                     "import time; print('ready'); time.sleep(1); print('done')")
                   :tty tty :yield-time-ms 100))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (if tty
                (should (string-match-p "ready" (plist-get initial :output)))
              (should (equal "" (plist-get initial :output))))
            (setq final (test-mevedel-execution--observe session id))
            (should (string-match-p "done" (plist-get final :output)))))
      (delete-directory root t)))
  :doc "yields a stable owner-scoped id and retained session artifact"
  (let* ((root (make-temp-file "mevedel-managed-yield-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         observation)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf first; sleep 2; printf second")
                 :owner "agent--one"))
          (let* ((facts (plist-get observation :facts))
                 (id (plist-get facts :execution-id))
                 (path (plist-get facts :output-path)))
            (should (stringp id))
            (should (eq 'running (plist-get facts :state)))
            (should (file-in-directory-p
                     path (file-name-concat root "artifacts")))
            (should (file-exists-p path))
            (should (equal id
                           (plist-get
                            (car (mevedel-execution-list
                                  session "agent--one"))
                            :execution-id)))
            (test-mevedel-execution--stop-all
             session "agent--one" (list id))))
      (delete-directory root t)))
  :doc "foreground request abort stops work while yielded work survives it"
  (let* ((root (make-temp-file "mevedel-managed-abort-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         (foreground-request (mevedel-request--create :session session))
         (yielded-request (mevedel-request--create :session session))
         foreground yielded foreground-id yielded-id)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq foreground value))
           :session session :owner "main" :request foreground-request
           :command '("sh" "-c" "sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 1000)
          (mevedel-request-drain-cancellers foreground-request)
          (test-mevedel-execution--wait (lambda () foreground))
          (setq foreground-id
                (plist-get (plist-get foreground :facts) :execution-id))
          (should-not foreground-id)
          (should (eq 'aborted
                      (plist-get (plist-get foreground :facts)
                                 :termination)))
          (mevedel-execution-start-bash
           (lambda (value) (setq yielded value))
           :session session :owner "main" :request yielded-request
           :command '("sh" "-c" "sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () yielded))
          (setq yielded-id
                (plist-get (plist-get yielded :facts) :execution-id))
          (mevedel-request-drain-cancellers yielded-request)
          (should (equal yielded-id
                         (plist-get
                          (car (mevedel-execution-list session "main"))
                          :execution-id)))
          (test-mevedel-execution--stop-all
           session "main" (list yielded-id)))
      (delete-directory root t)))
  :doc "timeout and output caps remain active after yielding"
  (let* ((root (make-temp-file "mevedel-managed-limits-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-output-limit 64)
         (mevedel-execution--child-kill-delay 0.05)
         initial final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf before; sleep .05; head -c 10000 /dev/zero | tr '\\0' x")
                 :timeout 5))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (setq final (test-mevedel-execution--observe session id))
          (should (eq 'output-limit
                      (plist-get (plist-get final :facts) :termination)))
          (should (= 64 (plist-get (plist-get final :facts)
                                   :output-bytes)))
          (should (file-exists-p
                   (plist-get (plist-get final :facts) :output-path)))
          (dolist (tty '(nil t))
            (let ((mevedel-execution-output-limit 4096)
                  timeout-initial timeout-final timeout-id)
              (setq timeout-initial
                    (test-mevedel-execution--start-managed
                     session root
                     '("sh" "-c" "printf before-timeout; sleep 30")
                     :timeout 0.1 :tty tty))
              (setq timeout-id
                    (plist-get
                     (plist-get timeout-initial :facts) :execution-id))
              (setq timeout-final
                    (test-mevedel-execution--observe session timeout-id))
              (should (eq 'timed-out
                          (plist-get (plist-get timeout-final :facts)
                                     :termination)))
              (should (eq (and tty t)
                          (plist-get (plist-get timeout-final :facts) :tty)))
              (should (string-match-p
                       "before-timeout" (plist-get timeout-initial :output)))
              (should (equal "" (plist-get timeout-final :output))))))
      (delete-directory root t)))
  :doc "cleans descendants when the managed shell exits"
  (let* ((root (make-temp-file "mevedel-managed-descendant-" t))
         (session (test-mevedel-execution--session root))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         observation pid)
    (unwind-protect
        (progn
          (setq observation
                (test-mevedel-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; exit 0"
                       "managed-descendant" pid-file)
                 :yield-time-ms nil))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid)))
          (should (= 0 (plist-get (plist-get observation :facts)
                                  :exit-code))))
      (delete-directory root t)))
  :doc "refuses a sixty-fifth live managed process without eviction"
  (let* ((root (make-temp-file "mevedel-managed-limit-" t))
         (session (test-mevedel-execution--session root))
         (artifacts (file-name-concat root "artifacts"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         ids)
    (unwind-protect
        (progn
          (dotimes (_ 64)
            (let ((observation
                   (test-mevedel-execution--start-managed
                    session root '("sh" "-c" "sleep 30"))))
              (push (plist-get (plist-get observation :facts) :execution-id)
                    ids)))
          (should-error
           (mevedel-execution-start-bash
            #'ignore :session session :owner "main"
            :command '("sh" "-c" "sleep 30")
            :workdir root :writable-roots (list root)
            :artifact-directory artifacts :yield-time-ms 10)
           :type 'mevedel-execution-limit)
          (should (= 64 (length (mevedel-execution-list session "main"))))
          (test-mevedel-execution--stop-all session "main" ids))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-observe ()
  ,test
  (test)
  :doc "polls only unread output and rejects ordinary pipe input"
  (let* ((root (make-temp-file "mevedel-managed-observe-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial first final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf one; sleep 1; printf two")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (equal "one" (plist-get initial :output)))
          (should-error
           (mevedel-execution-observe
            session "main" id #'ignore :chars "input" :wait-ms 250)
           :type 'mevedel-execution-input-error)
          (setq first
                (test-mevedel-execution--observe
                 session id :wait-ms 10))
          (should (equal "" (plist-get first :output)))
          (setq final (test-mevedel-execution--observe session id))
          (should (equal "two" (plist-get final :output)))
          (should (plist-get final :claimed-final-p))
          (should (eq 'completed
                      (plist-get (plist-get final :facts) :state)))
          (should-error
           (mevedel-execution-observe session "main" id #'ignore)
           :type 'mevedel-execution-not-found))
      (delete-directory root t)))
  :doc "writes repeated PTY input and returns only newly unread output"
  (let* ((root (make-temp-file "mevedel-managed-pty-input-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial first pending second final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c"
                   "test -t 0 && test -t 1 || exit 9; printf 'start> '; while IFS= read -r line; do printf 'got:%s\\nnext> ' \"$line\"; done; printf eof")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (string-match-p "start>" (plist-get initial :output)))
          (should (plist-get (plist-get initial :facts) :tty))
          (setq first
                (test-mevedel-execution--observe
                 session id :chars "one\n" :wait-ms 250))
          (should (string-match-p "got:one" (plist-get first :output)))
          (should-not (string-match-p "start>" (plist-get first :output)))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq pending value))
           :wait-ms 300000)
          (setq second
                (test-mevedel-execution--observe
                 session id :chars "two\n" :wait-ms 250))
          (should (eq 'running
                      (plist-get (plist-get pending :facts) :state)))
          (should (string-match-p "got:two" (plist-get second :output)))
          (should-not (string-match-p "got:one" (plist-get second :output)))
          (setq final
                (test-mevedel-execution--observe
                 session id :chars (string 4)))
          (should (string-match-p "eof" (plist-get final :output)))
          (should-not (string-match-p "got:two" (plist-get final :output)))
          (should (eq 'completed
                      (plist-get (plist-get final :facts) :state))))
      (delete-directory root t)))
  :doc "Ctrl-C interrupts the execution group in pipe and PTY modes"
  (dolist (tty '(nil t))
    (let* ((root (make-temp-file "mevedel-managed-interrupt-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (mevedel-execution--child-kill-delay 0.05)
           initial pending final id)
      (unwind-protect
          (progn
            (setq initial
                  (test-mevedel-execution--start-managed
                   session root '("sh" "-c" "printf ready; exec sleep 30")
                   :tty tty))
            (setq id (plist-get (plist-get initial :facts) :execution-id))
            (mevedel-execution-observe
             session "main" id (lambda (value) (setq pending value))
             :wait-ms 300000)
            (setq final
                  (test-mevedel-execution--observe
                   session id :chars (string 3)))
            (should (eq 'running
                        (plist-get (plist-get pending :facts) :state)))
            (should (= 2 (plist-get (plist-get final :facts) :exit-code)))
            (should (eq 'signaled
                        (plist-get (plist-get final :facts) :termination)))
            (should (eq 'completed
                        (plist-get (plist-get final :facts) :state)))
            (should (eq (and tty t)
                        (plist-get (plist-get final :facts) :tty))))
        (delete-directory root t))))
  :doc "keeps trapped and late Ctrl-C exit status distinct from signals"
  (dolist (command '(("sh" "-c"
                      "trap 'exit 2' INT; printf ready; while :; do sleep 1; done")
                     ("sh" "-c" "printf ready; sleep .05; exit 2")))
    (let* ((root (make-temp-file "mevedel-managed-exit-two-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (initial
            (test-mevedel-execution--start-managed session root command))
           (id (plist-get (plist-get initial :facts) :execution-id))
           final)
      (unwind-protect
          (progn
            (when (string-match-p "sleep .05" (car (last command)))
              (test-mevedel-execution--wait
               (lambda ()
                 (eq 'completed (plist-get
                                 (car (mevedel-execution-list session "main"))
                                 :state)))))
            (setq final
                  (test-mevedel-execution--observe
                   session id :chars (string 3)))
            (should (= 2 (plist-get (plist-get final :facts) :exit-code)))
            (should (eq 'exited
                        (plist-get (plist-get final :facts) :termination))))
        (delete-directory root t))))
  :doc "independent completion waits for a model observer instead of recalling Bash"
  (let* ((root (make-temp-file "mevedel-managed-independent-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         (calls 0) initial final id)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value)
             (setq calls (1+ calls)
                   initial value))
           :session session :owner "main"
           :command '("sh" "-c" "sleep .05; printf finished")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 10)
          (test-mevedel-execution--wait (lambda () initial))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait
           (lambda ()
             (eq 'completed
                 (plist-get (car (mevedel-execution-list session "main"))
                            :state))))
          (should (= 1 calls))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq final value)))
          (should (equal "finished" (plist-get final :output)))
          (should (plist-get final :claimed-final-p)))
      (delete-directory root t)))
  :doc "bad outcome resolvers cannot block final delivery or handle cleanup"
  (dolist (resolver
           (list (lambda (_exit-code _termination) (error "Resolver failed"))
                 (lambda (_exit-code _termination) 'invalid)))
    (let* ((root (make-temp-file "mevedel-managed-bad-outcome-" t))
           (session (test-mevedel-execution--session root))
           (mevedel-sandbox-mode 'off)
           (initial
            (test-mevedel-execution--start-managed
             session root '("sh" "-c" "sleep .05; exit 1")
             :outcome-function resolver))
           (id (plist-get (plist-get initial :facts) :execution-id))
           final)
      (unwind-protect
          (progn
            (test-mevedel-execution--wait
             (lambda ()
               (mevedel-execution--record-finished-p
                (mevedel-execution--owned-yielded-record
                 session "main" id))))
            (setq final (test-mevedel-execution--observe session id))
            (should (eq 'failure
                        (plist-get (plist-get final :facts) :outcome)))
            (should (plist-get final :claimed-final-p))
            (should-not (mevedel-execution-list session "main")))
        (delete-directory root t))))
  :doc "owner mismatches reveal no execution state"
  (let* ((root (make-temp-file "mevedel-managed-owner-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--one"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should-error
           (mevedel-execution-observe session "agent--two" id #'ignore)
           :type 'mevedel-execution-not-found)
          (test-mevedel-execution--stop-all
           session "agent--one" (list id)))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution--cancel-observer ()
  ,test
  (test)
  :doc "request abort detaches a waiting poll without consuming its output"
  (let* ((root (make-temp-file "mevedel-managed-poll-abort-" t))
         (session (test-mevedel-execution--session root))
         (request (mevedel-request--create :session session))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial abandoned final id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c" "printf one; sleep .1; printf two")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq abandoned value))
           :wait-ms 5000 :request request)
          (mevedel-request-drain-cancellers request)
          (should-not abandoned)
          (setq final (test-mevedel-execution--observe session id))
          (should (equal "two" (plist-get final :output))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-list ()
  ,test
  (test)
  :doc "lists yielded executions for the canonical owner only"
  (let* ((root (make-temp-file "mevedel-managed-list-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         one two)
    (unwind-protect
        (progn
          (setq one
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--one"))
          (setq two
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep 30")
                 :owner "agent--two"))
          (should (= 1 (length (mevedel-execution-list
                                session "agent--one"))))
          (should (= 1 (length (mevedel-execution-list
                                session "agent--two"))))
          (should-not (plist-member
                       (car (mevedel-execution-list session "agent--one"))
                       :process))
          (test-mevedel-execution--stop-all
           session "agent--one"
           (list (plist-get (plist-get one :facts) :execution-id)))
          (test-mevedel-execution--stop-all
           session "agent--two"
           (list (plist-get (plist-get two :facts) :execution-id))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-stop ()
  ,test
  (test)
  :doc "stops the process group and returns unread terminal output"
  (let* ((root (make-temp-file "mevedel-managed-stop-" t))
         (session (test-mevedel-execution--session root))
         (pid-file (file-name-concat root "child.pid"))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial polled stopped id pid)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 (list "sh" "-c"
                       "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; printf ready; wait"
                       "managed-stop" pid-file)))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (mevedel-execution-observe
           session "main" id (lambda (value) (setq polled value))
           :wait-ms 300000)
          (mevedel-execution-stop
           session "main" id (lambda (value) (setq stopped value)))
          (should (eq 'running
                      (plist-get (plist-get polled :facts) :state)))
          (test-mevedel-execution--wait (lambda () stopped))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (plist-get stopped :claimed-final-p))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid))))
      (delete-directory root t)))
  :doc "force-kills a PTY process group that ignores TERM"
  (let* ((root (make-temp-file "mevedel-managed-pty-kill-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         initial stopped id)
    (unwind-protect
        (progn
          (setq initial
                (test-mevedel-execution--start-managed
                 session root
                 '("sh" "-c"
                   "trap '' TERM; printf ready; while :; do sleep 1; done")
                 :tty t))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (let* ((state (mevedel-session-execution-state session))
                 (record
                  (gethash id (mevedel-execution--state-records state))))
            (should (mevedel-execution--begin-stop record 'stopped)))
          (should-error
           (mevedel-execution-observe
            session "main" id #'ignore :chars "late\n" :wait-ms 250)
           :type 'mevedel-execution-input-error)
          (setq stopped (test-mevedel-execution--observe session id))
          (should (eq 'stopped
                      (plist-get (plist-get stopped :facts) :termination)))
          (should (integerp
                   (plist-get (plist-get stopped :facts) :exit-code)))
          (should-not (zerop
                       (plist-get (plist-get stopped :facts) :exit-code)))
          (should (plist-get (plist-get stopped :facts) :tty)))
      (delete-directory root t))))
(provide 'test-mevedel-execution)
;;; test-mevedel-execution.el ends here
