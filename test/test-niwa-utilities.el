;;; tests/test-niwa-utilities.el -- Unit tests for niwa-utilities.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'saveplace)
(require 'niwa-execution-target)
(require 'niwa-session-control-fs)
(require 'niwa-structs)
(require 'niwa-tool-render-data)
(require 'niwa-transcript)
(require 'niwa-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(eval-when-compile (require 'tramp))

(defun test-niwa-utilities--with-proc (files function)
  "Call FUNCTION while reads of the `/proc' names in FILES use real fixtures.
FILES maps an absolute file name to the contents of a temporary fixture
file, or to `missing' to leave that fixture absent, as for a process that
has exited.  Only those names are redirected; the real reader produces
every byte and error.  Other files are read normally."
  (let ((directory (make-temp-file "niwa-proc-fixture-" t))
        (insert (symbol-function 'insert-file-contents))
        (index 0)
        redirects)
    (unwind-protect
        (progn
          (dolist (entry files)
            (let ((fixture (file-name-concat directory
                                             (number-to-string (cl-incf index)))))
              (unless (eq (cdr entry) 'missing)
                (write-region (cdr entry) nil fixture nil 'silent))
              (push (cons (car entry) fixture) redirects)))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (file &rest args)
                       (apply insert (or (cdr (assoc file redirects)) file)
                              args))))
            (funcall function)))
      (delete-directory directory t))))

(defun test-niwa-utilities--stat (command start)
  "Return a `/proc/4242/stat' line for COMMAND with starttime START."
  (concat "4242 (" command ") "
          (mapconcat #'identity
                     (append '("S")
                             (mapcar #'number-to-string
                                     (number-sequence 4 21))
                             (list start)
                             (mapcar #'number-to-string
                                     (number-sequence 23 52)))
                     " ")
          "\n"))

(niwa-deftest niwa--process-identity ()
  ,test
  (test)
  :doc "identifies live processes stably and distinctly, and not dead ones"
  (let ((self (niwa--process-identity (emacs-pid)))
        (child (make-process :name "session-identity-holder"
                             :command '("sleep" "60") :noquery t
                             :sentinel #'ignore)))
    (unwind-protect
        (progn
          (unless self (ert-skip "Process identity is unavailable on this host"))
          (should (string-match-p
                   niwa--process-identity-regexp self))
          (should (equal self (niwa--process-identity
                               (emacs-pid))))
          (let ((other (niwa--process-identity
                        (process-id child))))
            (should (stringp other))
            (should-not (equal self other))
            ;; Both run in this boot.
            (should (equal (car (split-string self ":"))
                           (car (split-string other ":")))))
          (let ((pid (process-id child)))
            (delete-process child)
            (let ((deadline (+ (float-time) 5)))
              (while (and (memq pid (list-system-processes))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should-not (memq pid (list-system-processes)))
            (should-not (niwa--process-identity pid))))
      (when (process-live-p child) (delete-process child))))
  :doc "reads only local /proc, even from a remote workspace"
  (let ((local (niwa--process-identity (emacs-pid))))
    (unless local (ert-skip "Process identity is unavailable on this host"))
    (let ((default-directory "/ssh:unreachable:/project/"))
      (should (equal local (niwa--process-identity
                            (emacs-pid))))))
  :doc "never reads anything for invalid PIDs"
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (&rest _) (ert-fail "Invalid PID read /proc"))))
    (dolist (pid '(nil 0 -1 1.5 "123"))
      (should-not (niwa--process-identity pid))))
  :doc "takes the start ticks after the last parenthesis of the command"
  (let ((boot "0f1e2d3c-4b5a-4968-8776-655443322110"))
    (dolist (command '("emacs" "a b" "x) (y" ") 1 2 3 ("))
      (ert-info ((format "command %S" command))
        (test-niwa-utilities--with-proc
         `(("/proc/sys/kernel/random/boot_id" . ,(concat boot "\n"))
           ("/proc/4242/stat"
            . ,(test-niwa-utilities--stat command "777")))
         (lambda ()
           (should (equal (concat boot ":777")
                          (niwa--process-identity 4242))))))))
  :doc "fails closed on unavailable or malformed /proc evidence"
  (let ((boot "0f1e2d3c-4b5a-4968-8776-655443322110")
        (stat (test-niwa-utilities--stat "emacs" "777")))
      (dolist (files
               `((("/proc/sys/kernel/random/boot_id" . missing)
                  ("/proc/4242/stat" . ,stat))
                 (("/proc/sys/kernel/random/boot_id" . ,(concat boot "\n"))
                  ("/proc/4242/stat" . missing))
                 (("/proc/sys/kernel/random/boot_id" . "not-a-boot-id\n")
                  ("/proc/4242/stat" . ,stat))
                 (("/proc/sys/kernel/random/boot_id" . "")
                  ("/proc/4242/stat" . ,stat))
                 (("/proc/sys/kernel/random/boot_id" . ,(upcase boot))
                  ("/proc/4242/stat" . ,stat))
                 (("/proc/sys/kernel/random/boot_id" . ,boot)
                  ("/proc/4242/stat" . "4242 (emacs) S 1 2 3\n"))
                 (("/proc/sys/kernel/random/boot_id" . ,boot)
                  ("/proc/4242/stat" . "garbage"))
                 (("/proc/sys/kernel/random/boot_id" . ,boot)
                  ("/proc/4242/stat"
                   . ,(test-niwa-utilities--stat "emacs" "-7")))
                 (("/proc/sys/kernel/random/boot_id" . ,boot)
                  ("/proc/4242/stat"
                   . ,(test-niwa-utilities--stat "emacs" "7x")))))
        (ert-info ((format "proc %S" files))
          (test-niwa-utilities--with-proc
           files
           (lambda ()
             (should-not
              (niwa--process-identity 4242))))))))

(niwa-deftest niwa--delete-process ()
  ,test
  (test)
  :doc "deletes an established process immediately"
  (let ((process (make-pipe-process :name "delete-established" :noquery t)))
    (unwind-protect
        (progn
          (niwa--delete-process process)
          (should (eq 'closed (process-status process))))
      (when (process-live-p process) (delete-process process))))

  :doc "detaches pending setup and collects every terminal outcome without old callbacks"
  (dolist (terminal '(open failed closed))
    (let* ((buffer (generate-new-buffer " *delete-pending*"))
           (process (make-pipe-process :name "delete-pending"
                                       :buffer buffer :noquery t))
           (state 'connect) callbacks deleted-pending)
      (unwind-protect
          (cl-letf* ((status (symbol-function 'process-status))
                     (delete (symbol-function 'delete-process))
                     ((symbol-function 'process-status)
                      (lambda (value)
                        (if (eq value process) state (funcall status value))))
                     ((symbol-function 'delete-process)
                      (lambda (value)
                        (if (and (eq value process) (eq state 'connect))
                            (setq deleted-pending t)
                          (funcall delete value)))))
            (set-process-filter process (lambda (&rest _) (push 'filter callbacks)))
            (set-process-sentinel process (lambda (&rest _) (push 'sentinel callbacks)))
            (niwa--delete-process process)
            (should-not deleted-pending)
            (should (buffer-live-p buffer))
            (should-not (process-buffer process))
            (should (process-live-p process))
            (funcall (process-filter process) process "late data")
            (funcall (process-sentinel process) process "still connecting")
            (should (process-live-p process))
            (setq state terminal)
            (funcall (process-sentinel process) process "setup settled")
            (should (eq 'closed (funcall status process)))
            (should-not callbacks))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  :doc "collects setup that settles while the new sentinel is installed"
  (let ((process (make-pipe-process :name "delete-settled" :noquery t))
        (state 'connect))
    (unwind-protect
        (cl-letf* ((status (symbol-function 'process-status))
                   (sentinel (symbol-function 'set-process-sentinel))
                   ((symbol-function 'process-status)
                    (lambda (value)
                      (if (eq value process) state (funcall status value))))
                   ((symbol-function 'set-process-sentinel)
                    (lambda (value function)
                      (prog1 (funcall sentinel value function)
                        (when (eq value process) (setq state 'open))))))
          (niwa--delete-process process)
          (should (eq 'closed (funcall status process))))
      (when (process-live-p process) (delete-process process)))))

(niwa-deftest niwa--run-periodic-timer
  (:doc "shares one host wakeup and dispatches all due callbacks once")
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (now (seconds-to-time 100.25))
        first second calls)
    (unwind-protect
        ;; Dispatch/rearm semantics, not elapsed wall time: an OS stall can
        ;; cross the next integral second, or exhaust a batch's 25 ms budget.
        (cl-letf* ((next (symbol-function 'timer-next-integral-multiple-of-time))
                   (less (symbol-function 'time-less-p))
                   ((symbol-function 'current-time) (lambda () now))
                   ((symbol-function 'time-less-p)
                    (lambda (a b) (funcall less (or a now) (or b now))))
                   ((symbol-function 'timer-next-integral-multiple-of-time)
                    (lambda (time period) (funcall next (or time now) period))))
          (setq first (niwa--run-periodic-timer 'global 1 (lambda () (push 'first calls)))
                second (niwa--run-periodic-timer 'global 1 (lambda () (push 'second calls))))
          (dolist (timer (list first second))
            (let ((due (float-time (timer--time timer))))
              (should (= due (floor due)))))
          (should-not (memq first timer-list))
          (should-not (memq second timer-list))
          (should (niwa--ui-timer-pending-p first))
          (should (memq niwa--coalesced-timer (default-toplevel-value 'timer-list)))
          (timer-set-time first (time-subtract now 1) 1)
          (timer-set-time second (time-subtract now 1) 1)
          (niwa--coalesced-timer-tick)
          (should (equal '(first second) calls))
          (dolist (timer (list first second))
            (should (time-equal-p (seconds-to-time 101) (timer--time timer))))
          ;; A later tick skips missed periods instead of catching up.
          (setq now (seconds-to-time 103.25) calls nil)
          (niwa--coalesced-timer-tick)
          (should (equal '(first second) calls))
          (dolist (timer (list first second))
            (should (time-equal-p (seconds-to-time 104) (timer--time timer)))))
      (niwa--ui-timer-cancel first)
      (niwa--ui-timer-cancel second))
    (should-not niwa--coalesced-timer)))

(niwa-deftest niwa--coalesced-timer-tick ()
  ,test
  (test)
  :doc "cancellation during dispatch prevents an already-due callback"
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        first second called)
    (unwind-protect
        (progn
          (setq second (niwa--run-periodic-timer 'global 1 (lambda () (setq called t)))
                first (niwa--run-periodic-timer
                       'global 1 (lambda () (niwa--ui-timer-cancel second))))
          (timer-set-time first (time-subtract nil 1) 1)
          (timer-set-time second (time-subtract nil 1) 1)
          (niwa--coalesced-timer-tick)
          (should-not called)
          (should-not (niwa--ui-timer-pending-p second)))
      (niwa--ui-timer-cancel first)
      (niwa--ui-timer-cancel second)))

  :doc "one-shot work can rearm itself while another callback fails"
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (debug-on-error nil)
        good bad calls notices)
    (unwind-protect
        (progn
          (setq good (timer-create)
                bad (niwa--run-periodic-timer "/one/" 1 (lambda () (error "Expected failure"))))
          (timer-set-time good (time-subtract nil 1))
          (timer-set-function good (lambda ()
                                     (push 'good calls)
                                     (timer-set-time good (time-add nil 5))
                                     (niwa--ui-timer-activate good t)))
          (niwa--ui-timer-activate good t)
          (timer-set-time bad (time-subtract nil 1) 1)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) notices))))
            (niwa--coalesced-timer-tick))
          (should (equal '(good) calls))
          (should (= 1 (length notices)))
          (should (equal "niwa [/one/]: Error running periodic callback: Expected failure" (car notices)))
          (should (niwa--ui-timer-pending-p good)))
      (niwa--ui-timer-cancel good)
      (niwa--ui-timer-cancel bad))
    (should-not niwa--coalesced-timer))

  :doc "yields between callbacks for pending input without dropping due work"
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (pending nil) (calls 0) first second)
    (unwind-protect
        (cl-letf (((symbol-function 'input-pending-p) (lambda () pending)))
          (setq second (niwa--run-periodic-timer 'global 1 (lambda () (cl-incf calls)))
                first (niwa--run-periodic-timer
                       'global 1 (lambda () (cl-incf calls) (setq pending t))))
          (timer-set-time first (time-subtract nil 1) 1)
          (timer-set-time second (time-subtract nil 1) 1)
          (niwa--coalesced-timer-tick)
          (should (= 1 calls))
          (should (niwa--ui-timer-pending-p second))
          (should (time-less-p (timer--time second) nil))
          (setq pending nil)
          (niwa--coalesced-timer-tick)
          (should (= 2 calls)))
      (niwa--ui-timer-cancel first)
      (niwa--ui-timer-cancel second)))

  :doc "a yielded batch lets an ordinary due timer run before its remainder"
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (suspended
         (mapcar (lambda (timer) (cons timer (timer--triggered timer)))
                 (default-toplevel-value 'timer-list)))
        log busy first second ordinary)
    (unwind-protect
        (progn
          ;; Measure one timer pass without foreign callbacks (such as Org's
          ;; lock refresh) allowing the next host to overtake the ordinary timer.
          (dolist (entry suspended)
            (setf (timer--triggered (car entry)) t))
          ;; Busy members: a wait would run the ordinary timer itself.
          (setq busy (lambda ()
                       (push 'member log)
                       (let ((until (time-add nil 0.03)))
                         (while (time-less-p nil until))))
                second (niwa--run-periodic-timer
                        'global 1 (lambda ()
                            (niwa--ui-timer-cancel second)
                            (funcall busy)))
                first (niwa--run-periodic-timer
                       'global 1 (lambda ()
                           (niwa--ui-timer-cancel first)
                           (funcall busy))))
          (timer-set-time first (time-subtract nil 2) 1)
          (timer-set-time second (time-subtract nil 2) 1)
          (niwa--coalesced-timer-arm)
          ;; Ripe, but sorted after the host timer in this timer pass.
          (setq ordinary (run-at-time (time-subtract nil 1) nil
                                      (lambda () (push 'ordinary log))))
          ;; Only callback ordering is measured; delivery has a watchdog.
          (let ((deadline (time-add nil 5)))
            (while (and (< (length log) 3) (time-less-p nil deadline))
              (accept-process-output nil 0.05)))
          (should (equal '(member ordinary member) (reverse log))))
      (cancel-timer ordinary)
      (niwa--ui-timer-cancel first)
      (niwa--ui-timer-cancel second)
      (dolist (entry suspended)
        (setf (timer--triggered (car entry)) (cdr entry)))))

  :doc "a callback waiting for process output lets due siblings run, never itself"
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (process (make-process :name "niwa-coalesced-wait"
                               :command '("cat") :noquery t
                               :connection-type 'pipe))
        (waiter-calls 0) (sibling-calls 0) sibling-ran waiter sibling)
    (unwind-protect
        (progn
          ;; Members run newest first; the waiter goes ahead of its sibling.
          (setq sibling (niwa--run-periodic-timer
                         'global 1 (lambda ()
                             (cl-incf sibling-calls)
                             (setq sibling-ran t)
                             (niwa--ui-timer-cancel sibling)))
                waiter
                (niwa--run-periodic-timer
                 'global 1 (lambda ()
                     (cl-incf waiter-calls)
                     ;; Due again during its own wait: must not re-enter.
                     (timer-set-time waiter (time-subtract nil 1) 1)
                     (let ((deadline (time-add nil 5)))
                       (while (and (not sibling-ran)
                                   (time-less-p nil deadline))
                         (accept-process-output process 0.01))))))
          (timer-set-time waiter (time-subtract nil 2) 1)
          (timer-set-time sibling (time-subtract nil 1) 1)
          (niwa--coalesced-timer-tick)
          (should sibling-ran)
          (should (= 1 waiter-calls))
          (should (= 1 sibling-calls))
          (should-not (niwa--ui-timer-pending-p sibling))
          ;; The waiter is still due and the host timer is armed for it.
          (should (niwa--ui-timer-pending-p niwa--coalesced-timer))
          (should (equal (timer--time waiter)
                         (timer--time niwa--coalesced-timer))))
      (delete-process process)
      (niwa--ui-timer-cancel waiter)
      (niwa--ui-timer-cancel sibling))
    (should-not niwa--coalesced-timer)))

(niwa-deftest niwa--coalesced-timer-tick/host-delivery
  (:doc "real host delivery permits siblings while a callback waits")
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (waiter-calls 0) sibling-ran waiter sibling finished)
    (unwind-protect
        (progn
          (setq sibling (niwa--run-periodic-timer
                         'global 1 (lambda ()
                             (setq sibling-ran t)
                             (niwa--ui-timer-cancel sibling)))
                waiter (niwa--run-periodic-timer
                        'global 1 (lambda ()
                            (cl-incf waiter-calls)
                            (timer-set-time waiter (time-subtract nil 1) 1)
                            (let ((deadline (time-add nil 5)))
                              (while (and (not sibling-ran)
                                          (time-less-p nil deadline))
                                (sleep-for 0.01)))
                            (niwa--ui-timer-cancel waiter)
                            (setq finished t))))
          (timer-set-time waiter (time-subtract nil 2) 1)
          (timer-set-time sibling (time-subtract nil 1) 1)
          (niwa--coalesced-timer-arm)
          (let ((deadline (time-add nil 10)))
            (while (and (not finished) (time-less-p nil deadline))
              (sleep-for 0.01)))
          (should finished)
          (should sibling-ran)
          (should (= waiter-calls 1)))
      (niwa--ui-timer-cancel waiter)
      (niwa--ui-timer-cancel sibling))
    (should-not niwa--coalesced-timer)))

(niwa-deftest niwa--coalesced-timer-arm
  (:doc "owns one persistent host timer across temporary transport bindings")
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        owned clock)
    (unwind-protect
        (let ((timer-list nil))
          (setq owned (niwa--run-periodic-timer 'global 1 #'ignore)
                clock niwa--coalesced-timer)
          (should (memq clock (default-toplevel-value 'timer-list)))
          (should (niwa--ui-timer-pending-p owned))
          (niwa--coalesced-timer-arm)
          (should (eq clock niwa--coalesced-timer))
          (niwa--ui-timer-cancel owned)
          (should-not (memq clock (default-toplevel-value 'timer-list)))
          (should-not niwa--coalesced-timer))
      (niwa--ui-timer-cancel owned))))

(niwa-deftest niwa--coalesced-timer-call
  (:doc "restores the caller's current buffer after a callback changes it")
  (with-temp-buffer
    (let ((caller (current-buffer))
          (other (generate-new-buffer " *niwa-timer-other*"))
          (timer (timer-create)))
      (unwind-protect
          (progn
            (timer-set-function timer (lambda () (set-buffer other)))
            (niwa--coalesced-timer-call timer)
            (should (eq caller (current-buffer))))
        (kill-buffer other)))))

(niwa-deftest niwa--ui-timer-activate/untimed
  (:doc "rejects a coalesced timer without a time instead of breaking the queue")
  (let ((niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (untimed (timer-create)))
    (timer-set-function untimed #'ignore)
    (should-error (niwa--ui-timer-activate untimed t))
    (should-not niwa--coalesced-timers)
    (should-not niwa--coalesced-timer)))

(niwa-deftest niwa--ui-timer-activate
  (:doc "schedules on the host list during TRAMP without disturbing foreign timers")
  (let ((earlier (run-at-time 60 nil #'ignore))
        (later (run-at-time 180 nil #'ignore))
        (owned (timer-create)))
    (unwind-protect
        (progn
          (timer-set-time owned (time-add (current-time) (seconds-to-time 120)))
          (timer-set-function owned #'ignore)
          (with-tramp-suspended-timers
            (niwa--ui-timer-activate owned)
            (should-not (memq owned timer-list))
            (should (equal (cl-remove-if-not
                            (lambda (timer) (memq timer (list earlier owned later)))
                            (default-toplevel-value 'timer-list))
                           (list earlier owned later))))
          (should (memq owned timer-list)))
      (niwa--ui-timer-cancel owned)
      (cancel-timer earlier)
      (cancel-timer later))))

(niwa-deftest niwa--ui-timer-pending-p
  (:doc "recognizes a timer hidden by nested TRAMP bindings")
  (let ((owned (run-at-time 60 nil #'ignore)))
    (unwind-protect
        (with-tramp-suspended-timers
          (let ((timer-list nil))
            (should-not (niwa--timer-pending-p owned))
            (should (niwa--ui-timer-pending-p owned))))
      (niwa--ui-timer-cancel owned))))

(niwa-deftest niwa--ui-timer-cancel
  (:doc "removes only the owned outer timer on stop inside suspension")
  (let ((owned (run-at-time 60 nil #'ignore))
        (foreign (run-at-time 120 nil #'ignore)))
    (unwind-protect
        (progn
          (with-tramp-suspended-timers
            (niwa--ui-timer-cancel owned)
            (should-not (memq owned (default-toplevel-value 'timer-list)))
            (should (memq foreign (default-toplevel-value 'timer-list))))
          (should-not (memq owned timer-list))
          (should (memq foreign timer-list)))
      (niwa--ui-timer-cancel owned)
      (cancel-timer foreign))))

(defun test-niwa-utilities--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-niwa-utilities--raw-byte-string-p (string)
  "Return non-nil for STRING with raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))

(defvar niwa-provenance-test-ran nil
  "Set non-nil if a descriptor's contents were evaluated.")

(niwa-deftest niwa-library-provenance
  ()
  (let* ((root (make-temp-file "niwa-provenance-" t))
         (niwa-provenance-test-ran nil)
         (melpa-commit "0123456789abcdef0123456789abcdef01234567"))
    (unwind-protect
        (cl-labels
            ((dir (&rest parts)
               (let ((path (apply #'file-name-concat root parts)))
                 (make-directory path t)
                 path))
             (library (directory version)
               (let ((file (file-name-concat directory "fixture.el")))
                 (with-temp-file file
                   (insert ";;; fixture.el --- Fixture -*- lexical-binding: t -*-\n")
                   (when version (insert ";; Version: " version "\n"))
                   (insert "(provide 'fixture)\n"))
                 file))
             (descriptor (directory text)
               (with-temp-file (file-name-concat directory "fixture-pkg.el")
                 (insert text)))
             (repository (directory)
               (dolist (args '(("init" "--quiet") ("add" ".")
                               ("-c" "user.name=Test"
                                "-c" "user.email=test@example.invalid"
                                "-c" "commit.gpgsign=false"
                                "-c" "core.hooksPath=/dev/null"
                                "commit" "--quiet" "-m" "fixture")))
                 (let ((default-directory (file-name-as-directory directory)))
                   (should (zerop (apply #'process-file "git" nil nil nil args)))))
               (let ((default-directory (file-name-as-directory directory)))
                 (string-trim
                  (with-temp-buffer
                    (process-file "git" nil t nil "rev-parse" "HEAD")
                    (buffer-string)))))
             (provenance (file) (niwa-library-provenance file)))
          ,test)
      (delete-directory root t)))
  (test)
  :doc "a checkout root gives its commit and its header version"
  (let* ((checkout (dir "checkout"))
         (file (library checkout "1.2"))
         (head (repository checkout)))
    (should (equal (list :commit head :version "1.2") (provenance file))))

  :doc "a straight or elpaca build directory follows its source symlink"
  (let* ((checkout (dir "repos" "fixture"))
         (source (library checkout "1.2"))
         (head (repository checkout))
         (build (dir "build" "fixture"))
         (linked (file-name-concat build "fixture.el"))
         (compiled (file-name-concat build "fixture.elc")))
    (make-symbolic-link source linked)
    (with-temp-file compiled (insert "compiled\n"))
    (should (equal (list :commit head :version "1.2") (provenance linked)))
    (should (equal (list :commit head :version "1.2") (provenance compiled))))

  :doc "a package.el install inside another repository reports its descriptor"
  (let* ((outer (dir "dotfiles"))
         (package (dir "dotfiles" "elpa" "fixture-20260101.1"))
         (file (library package "9.9")))
    (descriptor package
                (format "(define-package \"fixture\" \"20260101.1\"\n  \"Fixture.\"\n  '((emacs \"27.1\"))\n  :url \"https://example.invalid\"\n  :commit \"%s\")\n"
                        melpa-commit))
    (with-temp-file (file-name-concat outer "README") (insert "dotfiles\n"))
    (should-not (equal melpa-commit (repository outer)))
    (should (equal (list :commit melpa-commit :version "20260101.1")
                   (provenance file))))

  :doc "an ELPA descriptor without a commit gives a version only"
  (let* ((package (dir "elpa" "fixture-1.0"))
         (file (library package "9.9")))
    (descriptor package "(define-package \"fixture\" \"1.0\" \"Fixture.\" nil)\n")
    (should (equal (list :commit nil :version "1.0") (provenance file))))

  :doc "a library with neither Git nor descriptor reports its header version"
  (let ((file (library (dir "plain") "1.2")))
    (should (equal (list :commit nil :version "1.2") (provenance file))))

  :doc "a library saying nothing gives nil values, and a missing file does not signal"
  (progn
    (should (equal (list :commit nil :version nil)
                   (provenance (library (dir "bare") nil))))
    (should (equal (list :commit nil :version nil)
                   (provenance (file-name-concat root "missing" "fixture.el"))))
    (should (equal (list :commit nil :version nil) (provenance nil))))

  :doc "without Git a checkout falls back to its descriptor"
  (let* ((checkout (dir "checkout"))
         (file (library checkout "1.2"))
         (head (repository checkout)))
    (descriptor checkout
                (format "(define-package \"fixture\" \"3.0\" \"Fixture.\" nil :commit \"%s\")\n"
                        melpa-commit))
    (should (equal (list :commit head :version "3.0") (provenance file)))
    (let ((exec-path nil))
      (should (equal (list :commit melpa-commit :version "3.0") (provenance file)))))

  :doc "a hostile or malformed descriptor is never evaluated and never trusted"
  (let* ((package (dir "elpa" "fixture-1.0"))
         (file (library package "1.2")))
    (dolist (text (list "#.(setq niwa-provenance-test-ran t)\n"
                        "(setq niwa-provenance-test-ran t)\n"
                        "(define-package \"fixture\" 42 \"Fixture.\" nil)\n"
                        (format "(define-package \"fixture\" \"%s\" \"Fixture.\" nil)\n"
                                (make-string 80 ?9))
                        "(define-package \"fixture\" \"1.0\"\n"))
      (descriptor package text)
      (should (equal (list :commit nil :version "1.2") (provenance file))))
    (should-not niwa-provenance-test-ran)
    ;; A usable version survives a garbage commit, which is dropped.
    (descriptor package
                "(define-package \"fixture\" \"1.0\" \"Fixture.\" nil :commit \"not hex!\")\n")
    (should (equal (list :commit nil :version "1.0") (provenance file)))))

(niwa-deftest niwa-library-source-directory
  (:doc "resolves source siblings without mistaking compiled build files for roots")
  (let* ((root (make-temp-file "niwa-library-layout-" t))
         (source (file-name-concat root "source"))
         (build (file-name-concat root "build"))
         (el (file-name-concat source "fixture.el"))
         (linked (file-name-concat build "fixture.el"))
         (elc (file-name-concat build "fixture.elc")))
    (unwind-protect
        (progn
          (make-directory source)
          (make-directory build)
          (with-temp-file el (insert ";; Source.\n"))
          (with-temp-file elc (insert "compiled fixture\n"))
          (should (equal (file-name-as-directory source)
                         (niwa-library-source-directory el)))
          ;; A source-less installation keeps its own data root.
          (should (equal (file-name-as-directory build)
                         (niwa-library-source-directory elc)))
          (make-symbolic-link el linked)
          (should (equal (file-name-as-directory source)
                         (niwa-library-source-directory linked)))
          (should (equal (file-name-as-directory source)
                         (niwa-library-source-directory elc)))
          ;; A dangling source link cannot redirect a usable compiled install.
          (delete-file el)
          (should (equal (file-name-as-directory build)
                         (niwa-library-source-directory elc)))
          (delete-file linked)
          (with-temp-file linked (insert ";; Co-located source.\n"))
          (should (equal (file-name-as-directory build)
                         (niwa-library-source-directory elc))))
      (delete-directory root t))))

(niwa-deftest niwa--diagnostic-value
  (:doc "copies nested data and preserves diagnostic keyword normalization")
  (let ((value (list :duplicate 1 :duplicate 2 :tail)))
    (should (equal '(:duplicate 2 :tail nil)
                   (niwa--diagnostic-value value)))
    (should (equal '(:duplicate 1 :duplicate 2 :tail) value))))

(niwa-deftest niwa--diagnostic-entry-text
  (:doc "prints a complete readable line despite ambient print limits")
  (let ((print-length 1) (print-level 1) (print-quoted nil))
    (should (equal "(:nested (a b c) :quoted 'value)\n"
                   (niwa--diagnostic-entry-text
                    '(:nested (a b c) :quoted (quote value)))))))

(niwa-deftest niwa--plain-data-p ()
  ,test
  (test)
  :doc "accepts nested read-safe values and rejects runtime objects"
  (should (niwa--plain-data-p
           '(nil symbol car "text" 4 (dotted . pair) [1 "two"])))
  (should-not (niwa--plain-data-p (lambda () t)))
  (should-not (niwa--plain-data-p (make-hash-table))))

(niwa-deftest niwa--write-private-data-file ()
  ,test
  (test)
  :doc "writes a value readable only by this user, replacing a looser file"
  (let* ((directory (make-temp-file "niwa-data-file-" t))
         (path (file-name-concat directory "nested" "record.eld")))
    (unwind-protect
        (progn
          (niwa--write-private-data-file path '(:roots ("/a" "/b") :n 1))
          (should (= #o600 (file-modes path)))
          (should (equal '(:roots ("/a" "/b") :n 1) (niwa--read-data-file path)))
          (set-file-modes path #o644)
          (niwa--write-private-data-file path '("/c"))
          (should (= #o600 (file-modes path)))
          (should (equal '("/c") (niwa--read-data-file path))))
      (delete-directory directory t))))

(niwa-deftest niwa--read-data-file ()
  ,test
  (test)
  :doc "yields nil for a missing or malformed file"
  (let* ((directory (make-temp-file "niwa-data-file-" t))
         (path (file-name-concat directory "record.eld")))
    (unwind-protect
        (progn
          (should-not (niwa--read-data-file path))
          (write-region "(unbalanced" nil path nil 'silent)
          (should-not (niwa--read-data-file path)))
      (delete-directory directory t))))

(niwa-deftest niwa--ordered-completion-table ()
  ,test
  (test)
  :doc "completes over the displays in their given order under CATEGORY"
  (let* ((displays '("2h ago       new" "yesterday    old"))
         (table (niwa--ordered-completion-table displays 'niwa-session))
         (metadata (cdr (funcall table "" nil 'metadata))))
    (should (equal displays (all-completions "" table)))
    (should (eq 'niwa-session (alist-get 'category metadata)))
    (should (eq 'identity (alist-get 'display-sort-function metadata)))
    (should (eq 'identity (alist-get 'cycle-sort-function metadata)))))

(defvar gptel-include-tool-results)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar org-element-cache-persistent)

(niwa-deftest niwa--transcript-org-mode ()
  ,test
  (test)

  :doc "cold Org configuration survives without running on transcript storage"
  ;; A fresh child is needed: other tests may already have loaded Org.
  ;; It inherits Eask's isolated HOME and XDG roots.
  (let ((root (file-name-directory (locate-library "niwa-utilities")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q" "-L" root
              "--eval"
              (prin1-to-string
               '(progn
                  (require 'niwa-utilities)
                  (when (featurep 'org)
                    (error "Org must start unloaded"))
                  ;; Activate buffer-local binding machinery, as editor
                  ;; packages do before the first transcript is opened.
                  (with-temp-buffer
                    (make-local-variable 'after-change-major-mode-hook))
                  (let* ((ran nil)
                         (hook (lambda ()
                                 (setq ran t)
                                 (add-hook 'after-change-major-mode-hook
                                           #'ignore nil t))))
                    (with-eval-after-load 'org
                      (add-hook 'org-mode-hook hook))
                    (with-temp-buffer
                      (niwa--transcript-org-mode)
                      (unless (derived-mode-p 'org-mode)
                        (error "Transcript did not enter Org mode")))
                    (when ran
                      (error "Cold-loaded user hook ran on transcript storage"))
                    (unless (memq hook (default-value 'org-mode-hook))
                      (error "Cold-loaded user hook was lost"))))))))
        (let ((hook-localization-warning
               (string-match-p
                "Making after-change-major-mode-hook buffer-local while locally let-bound!"
                (buffer-string))))
          (should-not hook-localization-warning))
        (should (equal "" (buffer-string)))
        (should (equal 0 status)))))

  :doc "disabling transcript fontification does not run the user's mode hook"
  (let ((calls 0)
        (font-lock-mode-hook (list (lambda () (cl-incf calls)))))
    (with-temp-buffer (niwa--transcript-org-mode))
    (should (= 0 calls)))

  :doc "transcript storage does not run silent JIT fontification after an edit"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (should-not font-lock-mode)
    (should-not jit-lock-mode)
    (should-not (memq #'jit-lock-after-change after-change-functions))
    (require 'niwa-transcript-source)
    (insert "Prompt\nAnswer\n")
    (let ((proof (niwa-transcript-source-create (point-min))))
      (niwa-transcript-source-accept proof)
      (goto-char (point-max))
      (let ((start (point)))
        (insert "Tail\n")
        (should (= (1- start) (niwa-transcript-source-change proof)))
        (should (= (buffer-modified-tick)
                   (niwa-transcript-source-tick proof))))))

  :doc "transcript startup skips persistent Org cache without changing user settings"
  (progn
    (require 'org)
    (require 'org-element)
    (require 'org-persist)
    (let ((org-element-cache-persistent t) (reads 0))
      (cl-letf (((symbol-function 'org-persist-load)
                 (lambda (&rest _) (cl-incf reads))))
        (with-temp-buffer
          (insert "* Saved transcript\nAnswer\n")
          (niwa--transcript-org-mode)
          (should (derived-mode-p 'org-mode))
          (should-not org-element-cache-persistent)))
      (should (= 0 reads))
      (should org-element-cache-persistent)))

  :doc "owns gptel's transcript shape regardless of the user's chat settings"
  (let ((gptel-prompt-prefix-alist '((org-mode . "*** ")))
        (gptel-response-prefix-alist '((org-mode . "Assistant: ")))
        (gptel-include-tool-results 'auto))
    (with-temp-buffer
      (niwa--transcript-org-mode)
      (should (local-variable-p 'gptel-prompt-prefix-alist))
      (should-not gptel-prompt-prefix-alist)
      (should (local-variable-p 'gptel-response-prefix-alist))
      (should-not gptel-response-prefix-alist)
      (should (local-variable-p 'gptel-include-tool-results))
      (should (eq t gptel-include-tool-results)))
    (should (equal '((org-mode . "*** ")) gptel-prompt-prefix-alist))
    (should (eq 'auto gptel-include-tool-results)))

  :doc "gptel's fold of an inserted tool result neither folds nor runs user TAB hooks"
  (progn
    (require 'gptel)
    (require 'org)
    (let ((ran nil))
      (let ((org-cycle-tab-first-hook (list (lambda () (setq ran t) nil))))
        (with-temp-buffer
          (niwa--transcript-org-mode)
          (let* ((tool (gptel-make-tool :name "Read" :function #'ignore))
                 (info (list :buffer (current-buffer) :position (point-marker)
                             :callback #'gptel--insert-response :tools (list tool)
                             :tool-use '((:id "fold-call" :name "Read"))))
                 (this-command nil)
                 (cycles 0)
                 (cycle (symbol-function 'org-cycle)))
            ;; Exercise the producer as well as the hook: gptel suppresses
            ;; fold errors, so a failed fold alone would look like success.
            (cl-letf (((symbol-function 'org-cycle)
                       (lambda (&rest args)
                         (cl-incf cycles)
                         (apply cycle args))))
              (gptel--display-tool-results
               (list (list tool '(:file_path "a") "result line")) info))
            (should (= 1 cycles))
            (should (equal '(tool . "fold-call")
                           (get-text-property
                            (save-excursion
                              (goto-char (point-min))
                              (1- (search-forward "result line")))
                            'gptel))))
          (goto-char (point-max))
          (forward-line -1)
          (should-not (invisible-p (save-excursion (goto-char (point-min))
                                                   (search-forward "result"))))
          ;; The same block still folds from an interactive TAB.
          (let ((this-command 'org-cycle)) (org-cycle))
          (should (invisible-p (save-excursion (goto-char (point-min))
                                               (search-forward "result")))))
        (should-not ran))))

  :doc "suppresses org-indent-mode while transcript Org hooks run"
  (progn
    (require 'org)
    (with-temp-buffer
      (let ((org-mode-hook (list (lambda () (org-indent-mode +1))))
            (redraws 0))
        (cl-letf (((symbol-function 'redraw-display)
                   (lambda () (cl-incf redraws))))
          (niwa--transcript-org-mode))
        (should (derived-mode-p 'org-mode))
        (should-not (bound-and-true-p org-indent-mode))
        (should (= 0 redraws)))))

  :doc "runs no mode hook, so a user Org setup cannot attach to storage"
  ;; Every minor mode attached here would run on each model and tool
  ;; insertion, and several of them reach the target on a remote workspace.
  (progn
    (require 'org)
    (with-temp-buffer
      (let* ((ran nil)
             (note (lambda (hook) (lambda () (push hook ran))))
             (org-mode-hook (list (funcall note 'org)))
             (text-mode-hook (list (funcall note 'text)))
             (outline-mode-hook (list (funcall note 'outline)))
             (change-major-mode-after-body-hook (list (funcall note 'body)))
             (after-change-major-mode-hook (list (funcall note 'after))))
        (setq-local change-major-mode-hook
                    (list (funcall note 'change)))
        (niwa--transcript-org-mode)
        (should (derived-mode-p 'org-mode))
        (should-not ran)
        (should-not delayed-mode-hooks))))

  :doc "inhibits the Org startup block the mode body runs directly"
  ;; These are in the mode body, not a hook.  `org-inhibit-startup' covers
  ;; them.
  (progn
    (require 'org)
    (with-temp-buffer
      (let ((org-startup-truncated t)
            (org-startup-numerated t))
        (setq truncate-lines nil)
        (niwa--transcript-org-mode)
        (should (derived-mode-p 'org-mode))
        (should-not truncate-lines)
        (should-not (bound-and-true-p org-num-mode)))))

  :doc "a Local Variables block in stored model output is not honoured"
  ;; These buffers visit files whose contents are model output.
  (progn
    (require 'org)
    (let ((file (make-temp-file "niwa-transcript-" nil ".org"
                                "# Local Variables:\n# fill-column: 12\n# End:\n")))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents file)
            (setq buffer-file-name file)
            (unwind-protect
                (let ((fill-column 70))
                  (niwa--transcript-org-mode)
                  (should (derived-mode-p 'org-mode))
                  (should (= 70 fill-column)))
              (setq buffer-file-name nil)))
        (delete-file file)))))

(niwa-deftest niwa--head-tail-preview-parts ()
  ,test
  (test)

  :doc "returns short content unchanged"
  (let ((preview (niwa--head-tail-preview-parts "short" "short" 5 10)))
    (should (equal "short" (plist-get preview :text)))
    (should (= 0 (plist-get preview :omitted-chars))))

  :doc "uses bounded newline-aware parts and an exact character count"
  (let ((preview
         (niwa--head-tail-preview-parts
          "1234\n67890" "abc\ndefghi" 30 10)))
    (should (equal
             "1234\n[niwa: tool output truncated; omitted 20 chars]\nefghi"
             (plist-get preview :text)))
    (should (= 20 (plist-get preview :omitted-chars)))))

(niwa-deftest niwa--head-tail-preview-parts-artifact ()
  ,test
  (test)
  :doc "names the artifact and how to Read it, keeping the marker intact"
  (let* ((address "artifact://executions/x.log")
         (text (plist-get
                (niwa--head-tail-preview-parts
                 "1234\n67890" "abc\ndefghi" 30 10 address)
                :text)))
    (should (string-match-p
             "\\[niwa: tool output truncated; omitted 20 chars\\]" text))
    (should (string-match-p
             (regexp-quote
              "Read(file_path=\"artifact://executions/x.log\", offset=1, limit=2000)")
             text))))

(niwa-deftest niwa--clamped-integer ()
  ,test
  (test)

  :doc "keeps in-range integers and clamps out-of-range ones"
  (should (= 500 (niwa--clamped-integer 500 100 10 1000)))
  (should (= 10 (niwa--clamped-integer 3 100 10 1000)))
  (should (= 1000 (niwa--clamped-integer 4000 100 10 1000)))

  :doc "coerces floats and numeric strings"
  (should (= 500 (niwa--clamped-integer 499.6 100 10 1000)))
  (should (= 500 (niwa--clamped-integer "500" 100 10 1000)))
  (should (= 500 (niwa--clamped-integer " 500.0 " 100 10 1000)))

  :doc "falls back to the default for absent or malformed values"
  (should (= 100 (niwa--clamped-integer nil 100 10 1000)))
  (should (= 100 (niwa--clamped-integer "fast" 100 10 1000)))
  (should (= 100 (niwa--clamped-integer t 100 10 1000)))
  (should (= 10 (niwa--clamped-integer nil 3 10 1000))))

(niwa-deftest niwa--window-text-layout
  (:doc "Changes with wrapping, text scale and line spacing, not with text.")
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((window (selected-window))
             (layout (niwa--window-text-layout window)))
        (insert "text")
        (should (equal layout (niwa--window-text-layout window)))
        (dolist (change (list (lambda () (setq-local truncate-lines t))
                              (lambda () (setq-local word-wrap t))
                              (lambda () (setq-local line-spacing 2))
                              (lambda () (text-scale-set 1))
                              ;; Rescaling edits the remapping list in place.
                              (lambda () (text-scale-set 2))))
          (funcall change)
          (should-not (equal layout (niwa--window-text-layout window)))
          (setq layout (niwa--window-text-layout window)))))))

(niwa-deftest niwa--timer-pending-p ()
  ,test
  (test)

  :doc "recognizes an armed timer and its cancellation"
  (let ((timer (run-at-time 3600 nil #'ignore)))
    (unwind-protect
        (should (niwa--timer-pending-p timer))
      (cancel-timer timer))
    (should-not (niwa--timer-pending-p timer)))

  :doc "rejects a timer discarded with a let-bound `timer-list'"
  (let (lost)
    (let (timer-list)
      (setq lost (run-at-time 3600 nil #'ignore)))
    (should (timerp lost))
    (should-not (niwa--timer-pending-p lost)))

  :doc "recognizes a repeating timer from inside its own function"
  ;; Emacs marks it triggered while it runs; a caller that re-arms an
  ;; unscheduled timer from there must not start a duplicate.
  (let (timer inside)
    (setq timer (run-at-time 0 0.01 (lambda ()
                                      (setq inside (niwa--timer-pending-p timer))
                                      (cancel-timer timer))))
    (with-timeout (2 (cancel-timer timer) (ert-fail "Repeating timer never ran"))
      (while (memq timer timer-list) (accept-process-output nil 0.01)))
    (should inside))

  :doc "counts a timer an exclusive section suspended"
  (require 'niwa-transport)
  (let ((timer (run-at-time 3600 nil #'ignore)) inside)
    (unwind-protect
        (progn
          (niwa-transport-with-exclusive-connection
            (setq inside (niwa--timer-pending-p timer)))
          (should inside))
      (cancel-timer timer)))

  :doc "rejects values that are not timers"
  (progn
    (should-not (niwa--timer-pending-p nil))
    (should-not (niwa--timer-pending-p 'scheduled))))

(niwa-deftest niwa--invalid-message-char-p ()
  ,test
  (test)

  :doc "recognizes characters that are not Unicode scalar values"
  (should-not (niwa--invalid-message-char-p ?a))
  (should-not (niwa--invalid-message-char-p #x10ffff))
  (should (niwa--invalid-message-char-p
           (aref (test-niwa-utilities--raw-bytes #x80) 0)))
  (should (niwa--invalid-message-char-p #xd800))
  (should (niwa--invalid-message-char-p #xdfff))
  (should (niwa--invalid-message-char-p #x110000)))

(niwa-deftest niwa--escape-invalid-message-chars ()
  ,test
  (test)

  :doc "escapes each invalid character as its uppercase UTF-8 bytes"
  (should
   (equal
    "a\\x80\\xED\\xA0\\x80\\xF4\\x90\\x80\\x80z"
    (niwa--escape-invalid-message-chars
     (concat "a"
             (test-niwa-utilities--raw-bytes #x80)
             (string #xd800 #x110000)
             "z")))))

(niwa-deftest niwa--normalize-message-text ()
  ,test
  (test)

  :doc "returns ASCII without scanning or copying its text and properties"
  (let* ((text (propertize (make-string (* 1024 1024) ?x) 'face 'bold))
         (matcher (symbol-function 'string-match-p))
         (scans 0))
    (cl-letf (((symbol-function 'string-match-p)
               (lambda (regexp string &optional start)
                 (when (eq string text) (cl-incf scans))
                 (funcall matcher regexp string start))))
      (should (eq text (niwa--normalize-message-text text))))
    (should (zerop scans))
    (should (eq 'bold (get-text-property 0 'face text))))

  :doc "leaves valid large Unicode text and its properties without per-character Lisp work"
  (let* ((text (concat (make-string 100000 ?x)
                       (string 0 #x7f #x80 #xff #xd7ff #xe000 #x10ffff)))
         (predicate (symbol-function 'niwa--invalid-message-char-p))
         (calls 0))
    (put-text-property 10 20 'gptel 'response text)
    (cl-letf (((symbol-function 'niwa--invalid-message-char-p)
               (lambda (char) (cl-incf calls) (funcall predicate char))))
      (should (eq text (niwa--normalize-message-text text))))
    (should (eq 'response (get-text-property 15 'gptel text)))
    (should (< calls 10)))

  :doc "decodes raw UTF-8 bytes into normal Unicode"
  (let* ((raw (test-niwa-utilities--raw-bytes
               #xe2 #x80 #x9c ?x #xe2 #x80 #x9d))
         (normalized (niwa--normalize-message-text raw)))
    (should (equal "“x”" normalized))
    (should-not (test-niwa-utilities--raw-byte-string-p normalized)))

  :doc "preserves existing Unicode while decoding raw UTF-8 runs"
  (let* ((raw (concat "lambda λ "
                      (test-niwa-utilities--raw-bytes
                       #xe2 #x80 #x94)
                      " dash"))
         (normalized (niwa--normalize-message-text raw)))
    (should (equal "lambda λ — dash" normalized))
    (should-not (test-niwa-utilities--raw-byte-string-p normalized)))

  :doc "escapes invalid raw bytes visibly"
  (let* ((raw (concat "bad "
                      (test-niwa-utilities--raw-bytes #xff)
                      " byte"))
         (normalized (niwa--normalize-message-text raw)))
    (should (equal "bad \\xFF byte" normalized))
    (should-not (test-niwa-utilities--raw-byte-string-p normalized)))

  :doc "escapes UTF-8 decoded beyond Unicode's maximum"
  (let* ((text (decode-coding-string
                (unibyte-string #xf4 #x90 #x80 #x80) 'utf-8-unix t))
         (normalized (niwa--normalize-message-text text)))
    (should (equal "\\xF4\\x90\\x80\\x80" normalized))
    (should (json-serialize normalized)))

  :doc "escapes surrogate code points"
  (dolist (case '((#xd800 . "\\xED\\xA0\\x80")
                  (#xdbff . "\\xED\\xAF\\xBF")
                  (#xdc00 . "\\xED\\xB0\\x80")
                  (#xdfff . "\\xED\\xBF\\xBF")))
    (let ((normalized (niwa--normalize-message-text
                       (string (car case)))))
      (should (equal (cdr case) normalized))
      (should (json-serialize normalized)))))

(niwa-deftest niwa--path-alias-helpers ()
  ,test
  (test)

  :doc "same-file comparison accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (niwa--same-file-p
               (file-name-concat alias-root "source.el")
               (file-name-concat real-root "source.el")))))

  :doc "directory containment accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (niwa--file-in-directory-p
               (file-name-concat alias-root "source.el")
               (file-name-as-directory real-root)))))

  :doc "relative-name helper keeps aliased children relative"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (equal "source.el"
                     (niwa--file-relative-name-or-absolute
                      (file-name-concat alias-root "source.el")
                      (file-name-as-directory real-root))))))

  :doc "relative-name helper avoids plain relative paths across aliases"
  (let* ((alias-root (expand-file-name "/alias/root"))
         (real-root (expand-file-name "/real/root"))
         (alias-file (file-name-concat alias-root "source.el"))
         (original-file-in-directory-p
          (symbol-function 'file-in-directory-p)))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root))))))
              ((symbol-function 'file-in-directory-p)
               (lambda (file directory)
                 (or (and (equal (directory-file-name file) alias-file)
                          (equal (directory-file-name directory) real-root))
                     (funcall original-file-in-directory-p file directory)))))
      (should (equal "source.el"
                     (niwa--file-relative-name-or-absolute
                      alias-file
                      (file-name-as-directory real-root))))))

  :doc "macOS system volume var aliases stay inside var roots"
  (let ((actual-system-type system-type)
        (system-type 'darwin))
    (should (equal "/var/folders/k8/x/T/root/source.el"
                   (niwa--file-macos-var-alias
                    "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el")))
    (should
     (equal "/var/folders/k8/x/T/root/source.el"
            (niwa--file-macos-var-alias
             "/System/Volumes/Data/var/folders/k8/x/T/root/source.el")))
    (unless (eq actual-system-type 'windows-nt)
      (should
       (niwa--file-in-directory-p
        "/System/Volumes/Data/private/var/folders/k8/x/T/root/.worktrees/foo/"
        "/var/folders/k8/x/T/root/.worktrees/"))
      (should (equal "source.el"
                     (niwa--file-relative-name-or-absolute
                      "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el"
                      "/var/folders/k8/x/T/root/")))))

  :doc "Windows long-name aliases accept trailing directory arguments"
  (let* ((system-type 'windows-nt)
         (short-root (expand-file-name
                      "/runner/RUNNER~1/AppData/Local/Temp/root"))
         (long-root (expand-file-name
                     "/runner/runneradmin/AppData/Local/Temp/root")))
    (cl-letf (((symbol-function 'w32-long-file-name)
               (lambda (file)
                 (unless (string-suffix-p "/" file)
                   (let ((file (directory-file-name file)))
                     (cond
                      ((string-prefix-p short-root file)
                       (concat long-root
                               (substring file (length short-root))))
                      ((string-prefix-p long-root file)
                       file)))))))
      (should (equal "source.el"
                     (niwa--file-relative-name-or-absolute
                      (file-name-concat long-root "source.el")
                      (file-name-as-directory short-root))))
      (should
       (niwa--file-in-directory-p
        (file-name-concat long-root ".worktrees" "foo")
        (file-name-as-directory
         (file-name-concat short-root ".worktrees"))))))

  :doc "relative-name helper leaves outside files absolute"
  (let ((file (expand-file-name "/elsewhere/source.el")))
    (should (equal file
                   (niwa--file-relative-name-or-absolute
                    file "/real/root/")))))

(niwa-deftest niwa--tint ()
  ,test
  (test)
  :doc "resolves noninteractive default-face colors without returning white"
  (should (equal "#ff7f7f" (niwa--tint "unspecified-bg" "red" 0.5)))
  (should (equal "#7f7f7f" (niwa--tint "unspecified-fg" "white" 0.5))))

(niwa-deftest niwa--environment-info-string ()
  ,test
  (test)
  :doc "renders cached target readiness facts without launching a process"
  (let* ((process-environment
          (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment))
         (target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status ready
            :operating-system "Linux"
            :operating-system-version "6.8.0-target"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected executable lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected target process"))))
      (let ((result
             (niwa--environment-info-string
              (niwa-workspace--create
               :root "/ssh:user@host:/srv/project/")
              "/ssh:user@host:/srv/project/lib/"
              target)))
        (should (string-prefix-p "Execution target: ssh:user@host\n" result))
        (should (string-match-p "Working directory: /srv/project/lib/"
                                result))
        (should (string-match-p "Platform: linux" result))
        (should (string-match-p "OS Version: 6.8.0-target" result))
        (should-not (string-match-p "Virtualization:" result)))
      (setf (niwa-execution-target-readiness target)
            (append '(:virtualization "docker")
                    (niwa-execution-target-readiness target)))
      (should (string-match-p
               "OS Version: 6.8.0-target\nVirtualization: docker\nEmacs version: "
               (niwa--environment-info-string
                nil "/ssh:user@host:/srv/project/" target)))))

  :doc "identifies remote directories with or without a target and no readiness"
  (dolist (case '(("/ssh:user@host:/workspace/" . "ssh:user@host")
                  ("/docker:dev:/workspace/" . "docker:dev")
                  ("/podman:dev:/workspace/" . "podman:dev")
                  ("/ssh:jump|ssh:user@host:/workspace/" . "ssh:user@host")))
    (let ((target (niwa-execution-target-create (car case)))
          unexpected-io)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _)
                   (push 'executable-find unexpected-io)
                   (error "Unexpected executable lookup")))
                ((symbol-function 'process-file)
                 (lambda (&rest _)
                   (push 'process-file unexpected-io)
                   (error "Unexpected target process")))
                ((symbol-function 'tramp-send-command)
                 (lambda (&rest _)
                   (push 'tramp-send-command unexpected-io)
                   (error "Unexpected remote command")))
                ((symbol-function 'file-attributes)
                 (lambda (&rest _)
                   (push 'file-attributes unexpected-io)
                   (error "Unexpected filesystem probe"))))
        (dolist (supplied-target (list target nil))
          (let ((result (niwa--environment-info-string
                         nil (car case) supplied-target)))
            (should (string-prefix-p
                     (format "Execution target: %s\nWorking directory: /workspace/\n"
                             (cdr case))
                     result))
            (should (string-match-p "Platform: unknown" result))
            (should (string-match-p "OS Version: unknown" result))))
        (should-not unexpected-io))))

  :doc "identifies a local directory without a session target"
  (let ((result (niwa--environment-info-string nil temporary-file-directory)))
    (should (string-prefix-p "Execution target: local\nWorking directory: " result))
    (should (string-match-p (regexp-quote emacs-version) result))))

(niwa-deftest niwa--clear-user-turn-gptel-properties ()
  ,test
  (test)
  :doc "clears assistant metadata from inserted user transcript text"
  (with-temp-buffer
    (insert (propertize "Assistant answer.\n" 'gptel 'response))
    (let ((start (point)))
      (insert (propertize "\nUser follow-up\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (niwa--clear-user-turn-gptel-properties start (point))
      (should (eq 'response (get-text-property (point-min) 'gptel)))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "clears copied view/tool properties from user transcript text"
  (with-temp-buffer
    (let ((start (point)))
      (insert (propertize "Bash: git diff\n"
                          'gptel '(tool . "call_1")
                          'read-only t
                          'keymap (make-sparse-keymap)
                          'niwa-view-source '(1 . 42)
                          'niwa-view-type 'tool-summary
                          'font-lock-face 'niwa-view-tool-name))
      (niwa--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "preserves atomic mention bindings while clearing copied UI properties"
  (with-temp-buffer
    (let ((start (point))
          (binding '(:kind skill :token "$alpha"
                     :source-file "/tmp/alpha/SKILL.md")))
      (insert (propertize "$alpha"
                          'niwa-mention-binding binding
                          'gptel 'response
                          'read-only t))
      (niwa--clear-user-turn-gptel-properties start (point))
      (should (equal binding
                     (get-text-property start 'niwa-mention-binding)))
      (should-not (get-text-property start 'gptel))
      (should-not (get-text-property start 'read-only))))

  :doc "preserves generated render provenance but not literal marker text"
  (with-temp-buffer
    (let (start block-start block-end literal-start literal-end)
      (setq start (point))
      (insert (propertize "Expanded prompt\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (setq block-start (point))
      (insert (niwa-tool-render-data-format
               '(:kind inline-skill :name "demo")))
      (setq block-end (point))
      (setq literal-start (point))
      (insert (substring-no-properties
               (niwa-tool-render-data-format
                '(:kind inline-skill :name "literal"))))
      (setq literal-end (point))
      (set-text-properties literal-start literal-end nil)
      (niwa--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) block-start)
        (should-not (get-text-property (point) 'gptel))
        (forward-char 1))
      (goto-char block-start)
      (while (< (point) block-end)
        (should (eq t (get-text-property (point) 'niwa-render-data)))
        (should-not (eq 'response (get-text-property (point) 'gptel)))
        (should-not (get-text-property (point) 'response))
        (should-not (get-text-property (point) 'invisible))
        (should-not (get-text-property (point) 'front-sticky))
        (forward-char 1))
      (goto-char literal-start)
      (search-forward "<!-- niwa-render-data -->" literal-end)
      (should-not (text-properties-at (match-beginning 0)))
      (should (string-search
               ":name \"literal\""
               (niwa-tool-render-data-strip
                (buffer-string)))))))

(niwa-deftest niwa--hook-audit-helpers ()
  ,test
  (test)

  :doc "formats hook audit blocks with producer-specific provenance"
  (let* ((record
          `(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted ,(propertize
                                "new <!-- /niwa-hook-audit -->"
                                'face 'bold)
                  :nested (:original ,(propertize "old" 'face 'italic))))
         (block (niwa--format-hook-audit-record record))
         parsed)
    (should (eq 'niwa-hook-audit (get-text-property 0 'gptel block)))
    (should (eq t (get-text-property 0 'niwa-hook-audit block)))
    (should (get-text-property 0 'invisible block))
    (with-temp-buffer
      (insert block)
      (goto-char (point-min))
      (search-forward niwa--hook-audit-open)
      (let ((body-start (point)))
        (search-forward niwa--hook-audit-close)
        (let ((payload (buffer-substring-no-properties
                        body-start (match-beginning 0))))
          (should-not (string-match-p
                       "<!-- /niwa-hook-audit -->"
                       payload))
          (setq parsed (niwa--read-hook-audit-record payload)))))
    (should-not (text-properties-at 0 (plist-get parsed :submitted)))
    (should-not (text-properties-at
                 0 (plist-get (plist-get parsed :nested) :original))))

  :doc "strips generated hook audit blocks from model-visible text"
  (let ((block (niwa--format-hook-audit-record
                '(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted "<!-- /niwa-hook-audit --> tail"))))
    (should (equal "beforeafter"
                   (niwa--strip-hook-audit-blocks
                    (concat "before" block "after")))))

  :doc "does not authorize property-free copied hook audit blocks"
  (with-temp-buffer
    (insert "before"
            (substring-no-properties
             (niwa--format-hook-audit-record
              '(:type prompt-rewrite :event "UserPromptSubmit")))
            "after")
    (niwa-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward niwa--hook-audit-open)
    (should-not (get-text-property (match-beginning 0) 'gptel)))

  :doc "keeps trailing tool whitespace inside the ignored audit span"
  (with-temp-buffer
    (insert
     (propertize
      (concat
       "(:name \"Read\" :args nil)\n\nresult"
       (niwa--format-hook-audit-record
        '(:type tool-input-repair :state committed))
       "\n")
      'gptel '(tool . "call-1")))
    (insert (propertize "#+end_tool\nThe next response."
                        'gptel 'ignore))
    (niwa-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward niwa--hook-audit-close)
    (while (looking-at-p "[ \t\r\n]")
      (should (eq 'niwa-hook-audit
                  (get-text-property (point) 'gptel)))
      (should (eq t (get-text-property (point) 'niwa-hook-audit)))
      (forward-char 1)))

  :doc "builds prompt rewrite audit records only when the prompt changed"
  (should-not
   (niwa--hook-prompt-rewrite-audit-record
    'UserPromptSubmit "same" "same" "why"))
  (should
   (equal
    '(:type prompt-rewrite
            :event "UserPromptSubmit"
            :original "old"
            :submitted "new"
            :reason "why")
    (niwa--hook-prompt-rewrite-audit-record
     'UserPromptSubmit "old" "new" "why"))))

(niwa-deftest niwa--tag-query-prefix-from-infix ()
  ,test
  (test)
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'foo and not bar or baz'"
  (should (equal '(or (and foo (not bar)) baz)
                 (niwa--tag-query-prefix-from-infix '(foo and not bar or baz))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'john or not [jane]'"
  (should (equal '(or john (not [jane]))
                 (niwa--tag-query-prefix-from-infix '(john or not [jane]))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'alice and bob and charlie'"
  (should (equal '(and alice bob charlie)
                 (niwa--tag-query-prefix-from-infix '(alice and bob and charlie))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts single tag 'foo'"
  (should (equal 'foo
                 (niwa--tag-query-prefix-from-infix '(foo))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'foo bar baz not john'"
  (should (equal '(and foo bar baz (not john))
                 (niwa--tag-query-prefix-from-infix '(foo bar baz not john))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '((foo))'"
  (should (equal 'foo
                 (niwa--tag-query-prefix-from-infix '((foo)))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '(((foo)))'"
  (should (equal 'foo
                 (niwa--tag-query-prefix-from-infix '(((foo))))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '(((foo foo foo)))'"
  (should (equal '(and foo foo foo)
                 (niwa--tag-query-prefix-from-infix '(((foo foo foo))))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'not bar and baz'"
  (should (equal '(and (not bar) baz)
                 (niwa--tag-query-prefix-from-infix '(not bar and baz))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'bar or bar or baz'"
  (should (equal '(or bar bar baz)
                 (niwa--tag-query-prefix-from-infix '(bar or bar or baz))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'bar bar or baz'"
  (should (equal '(or (and bar bar) baz)
                 (niwa--tag-query-prefix-from-infix '(bar bar or baz))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts empty list to nil"
  (should (equal nil
                 (niwa--tag-query-prefix-from-infix '())))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '((()))' to nil"
  (should (equal nil
                 (niwa--tag-query-prefix-from-infix '(((()))))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts 'danny and (joey and boris)'"
  (should (equal '(and danny (and joey boris))
                 (niwa--tag-query-prefix-from-infix '(danny and (joey and boris)))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '((danny and (joey and boris)) and (foo or bar))'"
  (should (equal '(and (and danny (and joey boris)) (or foo bar))
                 (niwa--tag-query-prefix-from-infix '((danny and (joey and boris)) and (foo or bar)))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '((alice or bob) and (charlie or dave))'"
  (should (equal '(and (or alice bob) (or charlie dave))
                 (niwa--tag-query-prefix-from-infix '((alice or bob) and (charlie or dave)))))
  :doc "Valid infix to prefix conversions:
`niwa--tag-query-prefix-from-infix' converts '((alice and bob) or (charlie and dave))'"
  (should (equal '(or (and alice bob) (and charlie dave))
                 (niwa--tag-query-prefix-from-infix '((alice and bob) or (charlie and dave)))))
  :doc "Valid infix to prefix conversions:
mixed implicit and explicit conjunctions retain precedence"
  (should (equal '(or (and foo bar baz) (and qux quux))
                 (niwa--tag-query-prefix-from-infix
                  '(foo bar and baz or qux quux))))
  :doc "Valid infix to prefix conversions:
explicit grouping permits nested negation"
  (should (equal '(not (not foo))
                 (niwa--tag-query-prefix-from-infix '(not (not foo)))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(and)'"
  (should-error (niwa--tag-query-prefix-from-infix '(and)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(or)'"
  (should-error (niwa--tag-query-prefix-from-infix '(or)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(not)'"
  (should-error (niwa--tag-query-prefix-from-infix '(not)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(and foo)'"
  (should-error (niwa--tag-query-prefix-from-infix '(and foo)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(or foo)'"
  (should-error (niwa--tag-query-prefix-from-infix '(or foo)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(and foo or bar)'"
  (should-error (niwa--tag-query-prefix-from-infix '(and foo or bar)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(or and foo bar)'"
  (should-error (niwa--tag-query-prefix-from-infix '(or and foo bar)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(and (or foo) bar)'"
  (should-error (niwa--tag-query-prefix-from-infix '(and (or foo) bar)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(foo (or bar))'"
  (should-error (niwa--tag-query-prefix-from-infix '(foo (or bar))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(foo or (and bar))'"
  (should-error (niwa--tag-query-prefix-from-infix '(foo or (and bar))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(foo bar and (not))'"
  (should-error (niwa--tag-query-prefix-from-infix '(foo bar and (not))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '((or bar))'"
  (should-error (niwa--tag-query-prefix-from-infix '((or bar))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '((and foo))'"
  (should-error (niwa--tag-query-prefix-from-infix '((and foo))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(foo or (and))'"
  (should-error (niwa--tag-query-prefix-from-infix '(foo or (and))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(or ())'"
  (should-error (niwa--tag-query-prefix-from-infix '(or ())))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(foo or not)'"
  (should-error (niwa--tag-query-prefix-from-infix '(foo or not)))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(and (and foo bar))'"
  (should-error (niwa--tag-query-prefix-from-infix '(and (and foo bar))))
  :doc "Invalid infix queries:
`niwa--tag-query-prefix-from-infix' rejects '(or (or(foo and bar)))'"
  (should-error (niwa--tag-query-prefix-from-infix '(or (or(foo and bar)))))
  :doc "Invalid infix queries:
rejects a nested empty operand"
  (should-error (niwa--tag-query-prefix-from-infix '(foo and (()))))
  :doc "Invalid infix queries:
rejects consecutive negation without an explicit group"
  (should-error (niwa--tag-query-prefix-from-infix '(not not foo)))
  :doc "Invalid infix queries:
rejects trailing binary operators"
  (should-error (niwa--tag-query-prefix-from-infix '(foo and)))
  (should-error (niwa--tag-query-prefix-from-infix '(foo or))))

(niwa-deftest niwa--gc-hold ()
  ,test
  (test)
  :doc "keeps the busy threshold while a hold lives and restores it afterwards"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil))
    (unwind-protect
        (progn
          (niwa--gc-hold 'first #'always)
          (niwa--gc-hold 'second #'always)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (should (timerp niwa--gc-timer))
          ;; Idle tuning lowers it again; the next maintenance restores it.
          (setq gc-cons-threshold 800000)
          (niwa--gc-maintain)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (niwa--gc-release 'first)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (niwa--gc-release 'second)
          (should (= 800000 gc-cons-threshold))
          (should-not niwa--gc-timer))
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))))

  :doc "drops a dead hold and leaves a value someone else chose"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil)
        (alive t))
    (unwind-protect
        (progn
          (niwa--gc-hold 'request (lambda () alive))
          (setq gc-cons-threshold (* 128 1024 1024))
          (setq alive nil)
          (niwa--gc-maintain)
          (should (= 0 (hash-table-count niwa--gc-holds)))
          (should (= (* 128 1024 1024) gc-cons-threshold))
          (should-not niwa--gc-timer))
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))))

  :doc "defers collection while typing and lets it run once input pauses"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing (* 256 1024 1024))
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil)
        (idle nil))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time)
                   (lambda () (and idle (seconds-to-time idle)))))
          ;; A command is running: input is current.
          (niwa--gc-hold 'work #'always)
          (should (= (* 256 1024 1024) gc-cons-threshold))
          (should (memq #'niwa--gc-note-input pre-command-hook))
          ;; A pause returns the busy floor so the collection can run.
          (setq idle 2.0)
          (niwa--gc-maintain)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          ;; Recent input raises it again at the next command.
          (setq idle 0.2)
          (niwa--gc-note-input)
          (should (= (* 256 1024 1024) gc-cons-threshold))
          (niwa--gc-release 'work)
          (should (= 800000 gc-cons-threshold))
          (should-not (memq #'niwa--gc-note-input pre-command-hook)))
      (remove-hook 'pre-command-hook #'niwa--gc-note-input)
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))))

  :doc "keeps the typing floor when other tuning resets the threshold before commands"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing (* 256 1024 1024))
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil)
        ;; Like `gcmh-set-high-threshold', installed at the default depth.
        (pre-command-hook (list (lambda () (setq gc-cons-threshold (* 64 1024 1024)))))
        (maintained 0))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time) (lambda () nil))
                  ((symbol-function 'niwa--gc-live-bytes) (lambda () (* 100 1024 1024))))
          (niwa--gc-hold 'work #'always)
          (advice-add 'niwa--gc-maintain :before (lambda () (cl-incf maintained))
                      '((name . test-count)))
          (unwind-protect
              (dotimes (_ 3)
                (run-hooks 'pre-command-hook)
                ;; The reset threshold no longer decides: a collection waits
                ;; for the larger of it and the percentage of the live heap.
                (should (= (* 64 1024 1024) gc-cons-threshold))
                (should (>= (* gc-cons-percentage 100 1024 1024) (* 256 1024 1024))))
            (advice-remove 'niwa--gc-maintain 'test-count))
          ;; The floor is already applied; commands do not re-run maintenance.
          (should (= 0 maintained))
          (niwa--gc-release 'work)
          (should (= 0.1 gc-cons-percentage))
          (should-not (memq #'niwa--gc-note-input pre-command-hook)))
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))))

  :doc "raises only the threshold when the runtime reports no heap breakdown"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil))
    (unwind-protect
        ;; Neomacs answers 0 instead of a list.
        (cl-letf (((symbol-function 'garbage-collect-heapsize) (lambda () 0)))
          (niwa--gc-hold 'work #'always)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (should (= 0.1 gc-cons-percentage))
          (niwa--gc-release 'work)
          (should (= 800000 gc-cons-threshold)))
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))))

  :doc "changes nothing in a batch Emacs or when disabled"
  (let ((gc-cons-threshold 800000)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil))
    (let ((noninteractive t)
          (niwa-gc-cons-threshold-while-busy (* 64 1024 1024)))
      (niwa--gc-hold 'batch #'always)
      (should (= 800000 gc-cons-threshold)))
    (let ((noninteractive nil)
          (niwa-gc-cons-threshold-while-busy nil))
      (niwa--gc-hold 'disabled #'always)
      (should (= 800000 gc-cons-threshold))
      (should-not niwa--gc-timer)
      (niwa--gc-release 'disabled))))

(niwa-deftest niwa--with-gc-busy ()
  ,test
  (test)
  :doc "holds the busy threshold for its body and releases it on exit"
  (let ((gc-cons-threshold 800000)
        (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
        (niwa-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (niwa--gc-timer nil))
    (unwind-protect
        (progn
          (should (eq 'done (niwa--with-gc-busy
                              (should (= (* 64 1024 1024) gc-cons-threshold))
                              'done)))
          (should (= 800000 gc-cons-threshold))
          (should-error (niwa--with-gc-busy (error "Failed copy")))
          (should (= 800000 gc-cons-threshold))
          (should (= 0 (hash-table-count niwa--gc-holds)))
          (should-not niwa--gc-timer))
      (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer)))))

(niwa-deftest niwa--transcript-cycle-skip ()
  ,test
  (test)
  :doc "skips programmatic folds and lets an interactive TAB cycle"
  (progn
    (let ((this-command nil)) (should (niwa--transcript-cycle-skip)))
    (let ((this-command 'self-insert-command)) (should (niwa--transcript-cycle-skip)))
    (let ((this-command 'org-cycle)) (should-not (niwa--transcript-cycle-skip)))))

(niwa-deftest niwa--optimize-transcript-buffer ()
  ,test
  (test)
  :doc "keeps no undo history in generated transcript storage"
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "streamed chunk")
    (should (consp buffer-undo-list))
    (niwa--optimize-transcript-buffer)
    (should (eq t buffer-undo-list))
    (insert "tool result")
    (should (eq t buffer-undo-list))
    (should-not save-place-mode)))

(niwa-deftest niwa--forget-place ()
  ,test
  (test)
  :doc "keeps a persisted niwa buffer out of `save-place-alist'"
  (let ((save-place-loaded t)
        (save-place-alist nil)
        (default-directory temporary-file-directory))
    (save-place-mode +1)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name
                (file-name-concat temporary-file-directory
                                  "segment-0001.chat.org"))
          (should save-place-mode)
          (niwa--forget-place)
          (should-not save-place-mode)
          (insert "transcript\n")
          (save-place-to-alist)
          (should-not save-place-alist)
          (setq buffer-file-name nil))
      (save-place-mode -1))))

(defun test-niwa-utilities--stub-helper (result &optional capture)
  "Return a `niwa-execution-start-helper' stub settling with RESULT.
CAPTURE, when non-nil, is called with the stub's arguments."
  (lambda (callback &rest args)
    (when capture (funcall capture args))
    (funcall callback result)
    #'ignore))

(defun test-niwa-utilities--diff (original modified filepath &optional labels-real)
  "Return the settled (DIFF . ERROR) of `niwa-generate-diff'."
  (let (settled)
    (niwa-generate-diff original modified filepath
                           (lambda (diff error) (setq settled (cons diff error)))
                           labels-real)
    (with-timeout (10 (ert-fail "Diff never settled"))
      (while (not settled) (accept-process-output nil 0.02)))
    settled))

(niwa-deftest niwa-start-helper-capturing-output ()
  ,test
  (test)
  :doc "routes a structured command and declared paths through the helper layer"
  (let ((session (niwa-session--create))
        captured settled)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (test-niwa-utilities--stub-helper
                '(:exit-code 7 :output " helper output \n")
                (lambda (args) (setq captured args)))))
      (let ((niwa--session session))
        (should (functionp
                 (niwa-start-helper-capturing-output
                  (lambda (&rest values) (setq settled values))
                  "media-helper" '("helper" "--flag") '("/input")
                  '("/artifacts"))))))
    (should (equal '(7 " helper output \n" nil) settled))
    (should (equal '("media-helper" ("helper" "--flag") ("/input")
                     ("/artifacts") :session)
                   (seq-take captured 5)))
    (should (eq session (nth 5 captured)))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured))))

  :doc "settles once with the error when the helper cannot start"
  (let (settled)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (lambda (&rest _) (error "No helper"))))
      (should-not (niwa-start-helper-capturing-output
                   (lambda (&rest values) (push values settled))
                   "helper" '("helper") nil)))
    (should (= 1 (length settled)))
    (should (equal '(error "No helper") (nth 2 (car settled)))))

  :doc "settles with an error when the helper's owner is torn down"
  (let (settled)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (lambda (_callback &rest args)
                 (funcall (plist-get (nthcdr 4 args) :teardown-callback))
                 #'ignore)))
      (niwa-start-helper-capturing-output
       (lambda (&rest values) (push values settled))
       "helper" '("helper") nil))
    (should (= 1 (length settled)))
    (should (nth 2 (car settled))))

  :doc "lets an error raised by the callback propagate"
  (cl-letf (((symbol-function 'niwa-execution-start-helper)
             (test-niwa-utilities--stub-helper '(:exit-code 0 :output ""))))
    (should-error (niwa-start-helper-capturing-output
                   (lambda (&rest _) (error "Callback failed"))
                   "helper" '("helper") nil))))

(niwa-deftest niwa-generate-diff ()
  ,test
  (test)
  :doc "runs local snapshot diffing locally despite an ambient remote session"
  (let* ((target
          (niwa-execution-target-create
           "/ssh:builder@example.test:/srv/project/"))
         (session (niwa-session--create :execution-target target))
         captured)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (test-niwa-utilities--stub-helper
                '(:exit-code 1 :output "unified diff")
                (lambda (args) (setq captured args)))))
      (let ((niwa--session session))
        (should (equal '("unified diff\n")
                       (test-niwa-utilities--diff "old" "new" "file.el")))))
    (should (equal "diff" (car (nth 1 captured))))
    (should (= 2 (length (nth 2 captured))))
    (should-not (nth 5 captured))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured))))

  :doc "produces a real unified diff through the helper layer"
  (let ((settled (test-niwa-utilities--diff "a\nb\n" "a\nc\n" "file.txt")))
    (should-not (cdr settled))
    (should (string-match-p "^-b$" (car settled)))
    (should (string-match-p "^\\+c$" (car settled)))
    (should (equal '("") (test-niwa-utilities--diff "same\n" "same\n" "file.txt"))))

  :doc "reports a failed helper as an error and removes its spools"
  (let (spools)
    (cl-letf (((symbol-function 'niwa-execution-start-helper)
               (test-niwa-utilities--stub-helper
                '(:exit-code -1 :output "" :error (error "Helper failed"))
                (lambda (args) (setq spools (nth 2 args))))))
      (let ((settled (test-niwa-utilities--diff "old" "new" "file.el")))
        (should-not (car settled))
        (should (equal '(error "Helper failed") (cdr settled)))))
    (should (= 2 (length spools)))
    (dolist (path spools) (should-not (file-exists-p path))))

  :doc "labels a relative path a/ and b/ and an absolute path as itself"
  ;; The a/ and b/ prefixes belong to git-style patches over
  ;; repository-relative paths.  The edited-file reminder diffs absolute
  ;; paths, where prefixing spelled the label `a//home/user/file'.
  (dolist (case '(("src/file.el" "a/src/file.el" "b/src/file.el")
                  ("/home/user/file.el" "/home/user/file.el"
                   "/home/user/file.el")))
    (let (captured)
      (cl-letf (((symbol-function 'niwa-execution-start-helper)
                 (test-niwa-utilities--stub-helper
                  '(:exit-code 1 :output "unified diff")
                  (lambda (args) (setq captured args)))))
        (test-niwa-utilities--diff "old" "new" (car case)))
      (let ((command (nth 1 captured)))
        (should (equal (list "diff" "-u"
                             "--label" (nth 1 case)
                             "--label" (nth 2 case))
                       (seq-take command 6))))))

  :doc "spools Unicode and literal cache bytes without asking for a coding system"
  (let* ((old "Old \u03bb \u2192\n")
         (new "New \u03bb \u2014\n")
         (old-bytes (encode-coding-string old 'utf-8-unix))
         (new-bytes (encode-coding-string new 'utf-8-unix)))
    (dolist (inputs (list (list old new)
                          (list old-bytes new-bytes)
                          (list (string-to-multibyte old-bytes)
                                (string-to-multibyte new-bytes))
                          (list old (string-to-multibyte new-bytes))))
      (let ((coding-system-for-write nil)
            spools)
        (cl-letf (((symbol-function 'select-safe-coding-system-interactively)
                   (lambda (&rest _) (ert-fail "Unexpected encoding prompt")))
                  ((symbol-function 'niwa-execution-start-helper)
                   (lambda (callback &rest args)
                     (setq spools (nth 2 args))
                     (should
                      (equal (list old-bytes new-bytes)
                             (mapcar
                              (lambda (path)
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally path)
                                  (buffer-string)))
                              spools)))
                     (funcall callback '(:exit-code 1 :output "diff"))
                     #'ignore)))
          (let ((noninteractive nil))
            (should (equal '("diff\n")
                           (test-niwa-utilities--diff
                            (car inputs) (cadr inputs) "unicode.md")))))
        (should (= 2 (length spools)))
        (dolist (path spools) (should-not (file-exists-p path))))))

  :doc "preserves a trailing blank context line in unified output"
  (cl-letf (((symbol-function 'niwa-execution-start-helper)
             (test-niwa-utilities--stub-helper
              '(:exit-code 1 :output "@@ -1 +1 @@\n-old\n+new\n \n"))))
    (should
     (equal '("@@ -1 +1 @@\n-old\n+new\n \n")
            (test-niwa-utilities--diff "old\n\n" "new\n\n" "file.el")))))

(niwa-deftest niwa--write-file-atomically ()
  ,test
  (test)

  :doc "writes content with ordinary file modes, creating the parent"
  (let* ((root (make-temp-file "niwa-atomic-" t))
         (path (file-name-concat root "deep" "state.eld")))
    (unwind-protect
        (progn
          (niwa--write-file-atomically path "(:answer 42)\n")
          (should (equal "(:answer 42)\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should (= (file-modes path) (default-file-modes)))
          (should-not (directory-files (file-name-directory path) nil
                                       "niwa-write")))
      (delete-directory root t)))

  :doc "no-conversion writes literal bytes"
  (let* ((root (make-temp-file "niwa-atomic-" t))
         (path (file-name-concat root "blob"))
         (bytes (unibyte-string 0 255 10 128)))
    (unwind-protect
        (progn
          (niwa--write-file-atomically path bytes 'no-conversion)
          (should (equal bytes
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally path)
                           (buffer-string)))))
      (delete-directory root t)))

  :doc "a mode argument overrides the default"
  (let* ((root (make-temp-file "niwa-atomic-" t))
         (path (file-name-concat root "script.sh")))
    (unwind-protect
        (progn
          (niwa--write-file-atomically path "#!/bin/sh\n" nil #o755)
          (should (= (file-modes path) #o755)))
      (delete-directory root t)))

  :doc "a write that dies leaves the previous content and no staging file"
  (let* ((root (make-temp-file "niwa-atomic-" t))
         (path (file-name-concat root "state.eld")))
    (unwind-protect
        (progn
          (niwa--write-file-atomically path "previous\n")
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (error "Disk full"))))
            (should-error (niwa--write-file-atomically path "next\n")))
          (should (equal "previous\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should-not (directory-files root nil "niwa-write")))
      (delete-directory root t)))

  :doc "replaces a remote file in one target program with the requested mode"
  (let* ((host "atomic-remote-host")
         (root (file-name-as-directory (make-temp-file "niwa-atomic-remote-" t)))
         (local (file-name-concat root "script.sh"))
         (program (symbol-function 'niwa-session-control-fs-run-program))
         (programs 0))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((path (format "/niwamock:%s:%s" host local)))
            (write-region "old\n" nil local nil 'silent)
            ;; Prime TRAMP's attribute cache with the old file.
            (should (file-exists-p path))
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest args) (cl-incf programs) (apply program args)))
                      ((symbol-function 'rename-file)
                       (lambda (&rest _) (ert-fail "Fell back to TRAMP file operations"))))
              (niwa--write-file-atomically path "#!/bin/sh\n" nil #o755))
            (should (= 1 programs))
            (should (equal "#!/bin/sh\n"
                           (with-temp-buffer (insert-file-contents local) (buffer-string))))
            (should (= #o755 (file-modes local)))
            ;; TRAMP no longer answers from the old file's attributes.
            (should (= #o755 (file-modes path)))
            (should-not (directory-files root nil "niwa-control-fs"))))
      (delete-directory root t)))

  :doc "falls back to TRAMP file operations where the program refuses"
  (let* ((host "atomic-remote-fallback-host")
         (root (file-name-as-directory (make-temp-file "niwa-atomic-fallback-" t)))
         (target (file-name-concat root "target.txt"))
         (link (file-name-concat root "link.txt"))
         (nested (file-name-concat root "new" "dir" "file.txt")))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (write-region "target\n" nil target nil 'silent)
          (make-symbolic-link target link)
          ;; A symlinked leaf is replaced as a file, as before.
          (niwa--write-file-atomically
           (format "/niwamock:%s:%s" host link) "replaced\n")
          (should-not (file-symlink-p link))
          (should (equal "target\n"
                         (with-temp-buffer (insert-file-contents target) (buffer-string))))
          ;; Missing parents are still created.
          (niwa--write-file-atomically
           (format "/niwamock:%s:%s" host nested) "nested\n")
          (should (equal "nested\n"
                         (with-temp-buffer (insert-file-contents nested) (buffer-string)))))
      (delete-directory root t))))

(niwa-deftest niwa-diagnostic-format
    (:doc "attributes diagnostics without probing or abbreviating the root")
  (should (equal ,expected (niwa-diagnostic-format ,scope "Failed: %s" "100%")))
  (scope expected)
  "/projects/one/" "niwa [/projects/one/]: Failed: 100%"
  "/projects/two/" "niwa [/projects/two/]: Failed: 100%"
  "/ssh:host:/srv/project/" "niwa [/ssh:host:/srv/project/]: Failed: 100%"
  "/" "niwa [/]: Failed: 100%"
  "/path with %/" "niwa [/path with %/]: Failed: 100%"
  'global "niwa [global]: Failed: 100%"
  'unavailable "niwa [workspace unavailable]: Failed: 100%")

(niwa-deftest niwa-diagnostic-user-error ()
  ,test
  (test)
  :doc "interactive failures keep the user-error condition and one attribution"
  (let ((err (should-error
              (niwa-diagnostic-user-error
               "/projects/one/" "%s; retry"
               "niwa [/projects/one/]: Not ready")
              :type 'user-error)))
    (should (equal "niwa [/projects/one/]: Not ready; retry"
                   (error-message-string err)))))

(niwa-deftest niwa-with-diagnostic-errors ()
  ,test
  (test)
  :doc "attributes command refusals without changing shared guard conditions"
  (let ((raw (should-error (user-error "Session is read-only") :type 'user-error))
        (local (should-error
                (niwa-with-diagnostic-errors "/one/"
                  (user-error "Session is read-only"))
                :type 'user-error)))
    (should (equal "Session is read-only" (error-message-string raw)))
    (should (equal "niwa [/one/]: Session is read-only" (error-message-string local))))
  :doc "keeps a file failure's condition type and non-message fields"
  (let ((failure (should-error
                  (niwa-with-diagnostic-errors "/one/"
                    (signal 'file-missing '("Opening input file" "No such file" "/absent")))
                  :type 'file-missing)))
    (should (equal '(file-missing "niwa [/one/]: Opening input file" "No such file" "/absent")
                   failure)))
  :doc "does not replace non-message Lisp errors or turn quit into a failure"
  (should (equal '(args-out-of-range "abc" 4)
                 (should-error (niwa-with-diagnostic-errors "/one/" (aref "abc" 4))
                               :type 'args-out-of-range)))
  (should-error (niwa-with-diagnostic-errors "/one/" (signal 'arith-error nil)) :type 'arith-error)
  (should (eq 'quit (condition-case nil
                       (niwa-with-diagnostic-errors "/one/" (signal 'quit nil))
                     (quit 'quit)))))

(niwa-deftest niwa-diagnostic-scope ()
  ,test
  (test)
  :doc "captures a view's owner without consulting the selected workspace"
  (let* ((data (generate-new-buffer " *diagnostic owner*"))
         (view (generate-new-buffer " *diagnostic view*"))
         (workspace (niwa-workspace--create :root "/one/project/"))
         scope)
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq niwa--session (niwa-session--create :workspace workspace)))
          (with-current-buffer view (setq niwa--data-buffer data))
          (with-temp-buffer
            (setq niwa--workspace (niwa-workspace--create :root "/two/project/"))
            (setq scope (niwa-diagnostic-scope view))
            (should (equal "/one/project/" scope)))
          (kill-buffer data)
          (should (equal "niwa [/one/project/]: Failed"
                         (niwa-diagnostic-format scope "Failed")))
          (should (eq 'unavailable (niwa-diagnostic-scope data))))
      (when (buffer-live-p data) (kill-buffer data))
      (kill-buffer view))))

(niwa-deftest niwa-diagnostic-warning ()
  ,test
  (test)
  :doc "preserves category severity and destination without remote access"
  (let ((file-name-handler-alist
         (list (cons "/ssh:" (lambda (&rest _) (ert-fail "Reporting performed remote I/O")))))
        captured)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (setq captured args))))
      (niwa-diagnostic-warning "/ssh:host:/srv/" '(niwa test) "Failure" :error "*test warning*"))
    (should (equal '((niwa test) "niwa [/ssh:host:/srv/]: Failure" :error "*test warning*")
                   captured))))

(niwa-deftest niwa-diagnostic-message ()
  ,test
  (test)
  :doc "uses explicit global scope rather than the active workspace"
  (let (text)
    (with-temp-buffer
      (setq niwa--workspace (niwa-workspace--create :root "/one/"))
      (niwa-test--with-captured-messages text
        (niwa-diagnostic-message 'global "Failed: %s" "100%")))
    (should (equal "niwa [global]: Failed: 100%\n" text))))

(niwa-deftest niwa--warn-once ()
  ,test
  (test)
  :doc "warns on the first call per key and demotes repeats to messages"
  (let ((niwa--warn-once-table (make-hash-table :test #'equal))
        warnings messages)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type text &rest _) (push (cons type text) warnings)))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (niwa--warn-once "/one/" 'test-key "problem %d" 1)
      (niwa--warn-once "/one/" 'test-key "problem %d" 2)
      (niwa--warn-once "/one/" (list 'test-site "a") "subject a")
      (niwa--warn-once "/one/" (list 'test-site "b") "subject b"))
    (should (equal '((niwa . "niwa [/one/]: problem 1")
                     (niwa . "niwa [/one/]: subject a")
                     (niwa . "niwa [/one/]: subject b"))
                   (nreverse warnings)))
    (should (equal '("niwa [/one/]: problem 2") messages)))

  :doc "repeats bind `inhibit-message' so the echo area stays untouched"
  (let ((niwa--warn-once-table (make-hash-table :test #'equal))
        inhibited)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _) (setq inhibited inhibit-message))))
      (niwa--warn-once 'global 'test-key "problem")
      (niwa--warn-once 'global 'test-key "problem"))
    (should inhibited)))

(niwa-deftest niwa--warn-once-reset-site ()
  ,test
  (test)
  :doc "re-arms plain and composite keys only for the requested site"
  (let ((niwa--warn-once-table (make-hash-table :test #'equal))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings)))
              ((symbol-function 'message) #'ignore))
      (niwa--warn-once "/one/" 'test-site "site")
      (niwa--warn-once "/one/" (list 'test-site "a") "site a")
      (niwa--warn-once "/one/" (list 'test-site "b") "site b")
      (niwa--warn-once "/one/" (list 'other-site "c") "other c")
      (niwa--warn-once "/two/" 'test-site "site")
      (niwa--warn-once-reset-site "/one/" 'test-site)
      (niwa--warn-once "/one/" 'test-site "site")
      (niwa--warn-once "/one/" (list 'test-site "a") "site a")
      (niwa--warn-once "/one/" (list 'other-site "c") "other c")
      (niwa--warn-once "/two/" 'test-site "site"))
    (should (equal '("niwa [/one/]: site" "niwa [/one/]: site a"
                     "niwa [/one/]: site b" "niwa [/one/]: other c"
                     "niwa [/two/]: site" "niwa [/one/]: site"
                     "niwa [/one/]: site a")
                   (nreverse warnings)))))

(niwa-deftest niwa--with-gc-batched ()
  ,test
  (test)
  :doc "raises the GC threshold for the dynamic extent of the body"
  (let ((gc-cons-threshold 800000))
    (niwa--with-gc-batched
      (should (>= gc-cons-threshold (* 64 1024 1024))))
    (should (= gc-cons-threshold 800000)))

  :doc "never lowers an already higher threshold"
  (let ((gc-cons-threshold (* 128 1024 1024)))
    (niwa--with-gc-batched
      (should (= gc-cons-threshold (* 128 1024 1024)))))

  :doc "raises the percentage trigger and never lowers it"
  (let ((gc-cons-percentage 0.1))
    (niwa--with-gc-batched
      (should (>= gc-cons-percentage 0.5)))
    (should (= gc-cons-percentage 0.1)))
  (let ((gc-cons-percentage 0.8))
    (niwa--with-gc-batched
      (should (= gc-cons-percentage 0.8)))))

(niwa-deftest niwa--file-name-candidates ()
  ,test
  (test)

  :doc "a target path resolves no aliases, and so touches no target"
  ;; The alias forms are local concepts and cannot apply to another host;
  ;; the directory walks call this once per ancestor during dispatch.
  (let ((truenames 0))
    (niwa-test--with-captured-diagnostics nil
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (name &rest _) (setq truenames (1+ truenames)) name)))
        (should (equal '("/niwamock:host:/srv/project/main.py")
                       (niwa--file-name-candidates
                        "/niwamock:host:/srv/project/main.py")))
        (should (= 0 truenames)))))

  :doc "a local path still resolves its aliases"
  (let* ((dir (make-temp-file "niwa-cand-" t))
         (file (file-name-concat dir "f.el")))
    (unwind-protect
        (progn
          (write-region "" nil file nil 'silent)
          (should (member (directory-file-name (expand-file-name file))
                          (niwa--file-name-candidates file))))
      (delete-directory dir t))))

(niwa-deftest niwa--executable-find ()
  ,test
  (test)

  :doc "one lookup per (target, name), positive and negative alike"
  ;; Glob, Grep, Read and every spawn probe for a tool on the hot path,
  ;; from inside gptel's curl sentinel; an uncached remote probe walks the
  ;; whole PATH with a stat per entry.
  (let ((lookups 0))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &rest _)
                     (setq lookups (1+ lookups))
                     (and (equal name "rg") "/bin/rg"))))
          (clrhash niwa--executable-cache)
          (should (equal "/bin/rg"
                         (niwa--executable-find "rg" "/niwamock:host:")))
          (should-not (niwa--executable-find "nope" "/niwamock:host:"))
          (should (= 2 lookups))
          (should (equal "/bin/rg"
                         (niwa--executable-find "rg" "/niwamock:host:")))
          (should-not (niwa--executable-find "nope" "/niwamock:host:"))
          (should (= 2 lookups)))
      (clrhash niwa--executable-cache)))

  :doc "local and remote answers do not share a cache entry"
  (let (asked)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &optional remote)
                     (push remote asked)
                     (and remote "/remote/bin/rg"))))
          (clrhash niwa--executable-cache)
          (should-not (niwa--executable-find "rg"))
          (should (equal "/remote/bin/rg"
                         (niwa--executable-find "rg" "/niwamock:host:")))
          (should (equal '("/niwamock:host:" nil) asked)))
      (clrhash niwa--executable-cache))))

(niwa-deftest niwa--truncate-display ()
  ,test
  (test)

  :doc "returns a fitting string untouched and raises nothing"
  ;; `truncate-string-to-width' ends its scan by running `aref' off the
  ;; end and catching the `args-out-of-range' itself, so a label shorter
  ;; than WIDTH always raised one internally -- which TRAMP's signal hook
  ;; then logged to *Messages* on every remote render.
  (let ((raised 0))
    (advice-add 'signal :before
                (lambda (&rest _) (setq raised (1+ raised)))
                '((name . niwa-truncate-count)))
    (unwind-protect
        (progn
          (should (equal "git status"
                         (niwa--truncate-display "git status" 60 "...")))
          (should (= 0 raised)))
      (advice-remove 'signal 'niwa-truncate-count)))

  :doc "truncates and marks a string that does not fit"
  (should (equal "ex..." (niwa--truncate-display "exactly-ten" 5 "...")))

  :doc "an empty string and a non-string are handled"
  (should (equal "" (niwa--truncate-display "" 10 "...")))
  (should-not (niwa--truncate-display nil 10 "...")))

(niwa-deftest niwa--truncate-bytes ()
  ,test
  (test)
  :doc "returns fitting text untouched"
  (should (equal "héllo" (niwa--truncate-bytes "héllo" 6 "[..]")))
  :doc "truncates on a character boundary and appends the marker within the budget"
  (let ((bounded (niwa--truncate-bytes "héllo wörld" 9 "[..]")))
    (should (equal "héll[..]" bounded))
    (should (<= (string-bytes bounded) 9)))
  :doc "rejects a budget that cannot hold its marker"
  (should-error (niwa--truncate-bytes "text" 2 "[...]"))
  :doc "bounds large multibyte strings at character boundaries, including the marker"
  (let* ((source (concat (make-string 100000 #x754c) "end"))
         (result (niwa--truncate-bytes source 32768 "[omitted]")))
    (should (<= (string-bytes result) 32768))
    (should (string-prefix-p (make-string 100 #x754c) result))
    (should (string-suffix-p "[omitted]" result))
    (should (= (% (- (string-bytes result) (string-bytes "[omitted]")) 3) 0))
    (should-not (cl-some (lambda (char) (eq (char-charset char) 'eight-bit)) result))))

(niwa-deftest niwa--unified-diff ()
  ,test
  (test)
  :doc "returns nil for identical text and hunks for a change"
  (should-not (niwa--unified-diff "same\n" "same\n"))
  (let ((diff (niwa--unified-diff "one\ntwo\n" "one\nthree\n")))
    (should (string-prefix-p "@@" diff))
    (should (string-match-p "^-two$" diff))
    (should (string-match-p "^\\+three$" diff))
    (should-not (string-match-p "Diff finished" diff)))
  :doc "honours the requested context width"
  (let ((original (mapconcat #'number-to-string (number-sequence 1 20) "\n"))
        (changed (mapconcat (lambda (n) (if (= n 10) "x" (number-to-string n)))
                            (number-sequence 1 20) "\n")))
    (should (string-match-p "^ 4$" (niwa--unified-diff original changed 6)))
    (should-not (string-match-p "^ 4$" (niwa--unified-diff original changed)))))

(niwa-deftest niwa--duration-label
  (:doc "Uses whole seconds with safe zero and minute/hour boundaries.")
  (dolist (entry '((nil . "0s") (-1 . "0s") (0.99 . "0s")
                   (59.99 . "59s") (60 . "1m 00s") (185 . "3m 05s")
                   (3600 . "1h 00m") (3720 . "1h 02m")))
    (should (equal (niwa--duration-label (car entry)) (cdr entry)))))

(niwa-deftest niwa-collapse-leading-slashes ()
  ,test
  (test)
  :doc "collapses only a leading slash run, keeping remote prefixes"
  (should (equal "/run/user/1000" (niwa-collapse-leading-slashes "//run/user/1000")))
  (should (equal "/run" (niwa-collapse-leading-slashes "///run")))
  (should (equal "/a//b" (niwa-collapse-leading-slashes "/a//b")))
  (should (equal "/ssh:host:/run/user" (niwa-collapse-leading-slashes "/ssh:host://run/user")))
  (should (equal "/-::/run" (niwa-collapse-leading-slashes "/-::/run")))
  (should (equal "/-::/run/user/1000" (niwa-collapse-leading-slashes "/-:://run/user/1000")))
  (should (equal "/ssh::/run/user" (niwa-collapse-leading-slashes "/ssh:://run/user")))
  (should (equal "/" (niwa-expand-file-name "//")))
  (should (equal "/home/x" (niwa-expand-file-name "//home/x"))))

(provide 'test-niwa-utilities)
;;; test-niwa-utilities.el ends here
