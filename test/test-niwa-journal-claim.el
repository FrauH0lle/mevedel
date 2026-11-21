;;; test-niwa-journal-claim.el -- Journal ownership tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises competing claims and immutable settlement through real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-claim)
(require 'niwa-journal-expiry-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-expiry-test-support"))
(require 'niwa-session-control-fs)

(niwa-deftest niwa-journal-claim-acquire ()
  ,test
  (test)
  :doc "excludes contenders and fences old owners after durable settlement"
  (let ((directory (make-temp-file "niwa-journal-claim-" t)))
    (unwind-protect
        (let ((first (niwa-journal-claim-acquire directory 120)))
          (should first)
          (should-not (niwa-journal-claim-acquire directory 120))
          (should (niwa-journal-claim-settle first 'completed "frozen result"))
          (should (equal "frozen result"
                         (plist-get (niwa-journal-claim-outcome first) :payload)))
          (let ((second (niwa-journal-claim-acquire directory 120)))
            (should second)
            (should (> (plist-get second :generation) (plist-get first :generation)))
            (should-not (equal (plist-get second :owner) (plist-get first :owner)))
            (should-not (niwa-journal-claim-settle first 'completed "late result"))
            (should-not (niwa-journal-claim-outcome second))
            (should (equal "frozen result"
                           (plist-get (niwa-journal-claim-outcome first) :payload)))
            (should (niwa-journal-claim-settle second 'failed "provider unavailable"))))
      (delete-directory directory t)))

  :doc "admits in three programs and reads a settlement with its claim"
  (let ((directory (file-name-concat (make-temp-file "niwa-journal-cost-" t)
                                     "missing" "scope"))
        (original (symbol-function 'niwa-session-control-fs-run-program))
        (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply original args))))
          ;; Missing parents cost their creation and a second observation;
          ;; no claim exists yet to read.
          (let ((first (niwa-journal-claim-acquire directory 120)))
            (should first)
            (should (= calls 5))
            (should (equal (cons first nil)
                           (niwa-journal-claim-current-settlement directory)))
            (niwa-journal-claim-settle first 'completed "done")
            (setq calls 0)
            (let ((settlement (niwa-journal-claim-current-settlement directory)))
              (should (equal first (car settlement)))
              (should (equal "done" (plist-get (cdr settlement) :payload)))
              (should (= calls 2)))
            ;; Observation, claim with outcome, election.
            (setq calls 0)
            (should (niwa-journal-claim-acquire directory 120))
            (should (= calls 3))))
      (delete-directory (file-name-directory (directory-file-name
                                              (file-name-directory directory)))
                        t)))

  :doc "nested work uses the admission deadline without extending it"
  (let ((root (make-temp-file "niwa-journal-deadline-" t)))
    (unwind-protect
        (let* ((admission (niwa-journal-claim-acquire (file-name-concat root "admission") 120))
               (deadline (plist-get admission :expires-at))
               (job (niwa-journal-claim-acquire (file-name-concat root "job") 120 deadline)))
          (should job)
          (should (= deadline (plist-get job :expires-at)))
          (should-error (niwa-journal-claim-acquire
                         (file-name-concat root "expired") 120 1))
          (should-error (niwa-journal-claim-acquire
                         (file-name-concat root "extended") 120 (+ deadline 1000))))
      (delete-directory root t)))

  :doc "elects exactly one owner across independent Emacs processes"
  (dolist (defer-output '(nil t))
    (let* ((root (make-temp-file "niwa-journal-race-" t))
           (directory (file-name-concat root "scope"))
           (barrier (file-name-concat root "start"))
           processes buffers)
      (unwind-protect
          (progn
            (dotimes (_ 2)
              (let* ((buffer (generate-new-buffer " *journal-claim-contender*"))
                     (process
                      (make-process
                       :name "journal-claim-contender" :buffer buffer :sentinel #'ignore
                       :command
                       (list (expand-file-name invocation-name invocation-directory)
                             "-Q" "--batch" "-L" default-directory
                             "-l" "niwa-journal-claim" "--eval"
                             (format
                              "(let ((deadline (+ (float-time) 10))) (while (and (not (file-exists-p %S)) (< (float-time) deadline)) (sleep-for 0.01)) (unless (file-exists-p %S) (error \"Barrier unavailable\")) (prin1 (niwa-journal-claim-acquire %S 120)))"
                              barrier barrier directory)))))
                (push process processes)
                (push buffer buffers)))
            (write-region "go" nil barrier nil 'silent)
            (let ((deadline (+ (float-time) 15)))
              (when defer-output
                ;; Observe real exits without dispatching their buffered output.
                (while (and (cl-some #'process-live-p processes) (< (float-time) deadline)))
                (dolist (buffer buffers)
                  (should (zerop (buffer-size buffer)))))
              (while (and (cl-some #'process-live-p processes) (< (float-time) deadline))
                (accept-process-output nil 0.01))
              ;; Exit status can arrive before the connection's final output.
              (dolist (process processes)
                (while (and (< (float-time) deadline)
                            (accept-process-output process 0.01)))))
            (dolist (process processes)
              (ert-info ((format "Contender %S: status %S, exit %S\nCommand: %S\nOutput:\n%s"
                                 (process-name process) (process-status process)
                                 (process-exit-status process) (process-command process)
                                 (with-current-buffer (process-buffer process)
                                   (buffer-string))))
                (should-not (process-live-p process))
                (should (= 0 (process-exit-status process)))))
            (let ((results (mapcar (lambda (buffer)
                                     (with-current-buffer buffer
                                       (car (read-from-string (buffer-string)))))
                                   buffers)))
              (should (= 1 (length (delq nil results))))
              (should (equal (car (delq nil results))
                             (niwa-journal-claim-current directory)))))
        (dolist (process processes)
          (when (process-live-p process) (delete-process process)))
        (mapc #'kill-buffer buffers)
        (delete-directory root t)))))

(niwa-deftest niwa-journal-claim-settle ()
  ,test
  (test)
  :doc "fences an expired owner and preserves the successor after a late callback"
  (let ((directory (make-temp-file "niwa-journal-expiry-" t)))
    (unwind-protect
        (let ((first (niwa-journal-claim-acquire directory 1)))
          (should first)
          ;; Exercise the target clock without substituting the client clock.
          (let ((deadline (+ (float-time) 5)))
            (while (and (< (niwa-session-control-fs-target-time directory)
                           (plist-get first :expires-at))
                        (< (float-time) deadline))
              (sleep-for 0.02)))
          (let ((second (niwa-journal-claim-acquire directory 120)))
            (should second)
            (should (eq 'expired (plist-get (niwa-journal-claim-outcome first) :status)))
            (dolist (status '(completed failed cancelled))
              (should-not (niwa-journal-claim-settle first status "late callback")))
            (should-not (niwa-journal-claim-outcome second))
            (should (niwa-journal-claim-settle second 'completed "successor result"))
            (should (equal "successor result"
                           (plist-get (niwa-journal-claim-outcome second) :payload)))))
      (delete-directory directory t)))

  :doc "refuses forged ownership, invalid statuses, and duplicate settlement"
  (let ((directory (make-temp-file "niwa-journal-settlement-" t)))
    (unwind-protect
        (let ((claim (niwa-journal-claim-acquire directory 120)))
          (should-error (niwa-journal-claim-settle claim 'expired "not a callback status"))
          (should-error (niwa-journal-claim-settle claim 'completed '(not text)))
          (should-not
           (niwa-journal-claim-settle
            (plist-put (copy-tree claim) :owner (make-string 64 ?0))
            'completed "foreign result"))
          (should-not (niwa-journal-claim-outcome claim))
          (let ((original (symbol-function 'niwa-session-control-fs-run-program))
                (calls 0))
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest args)
                         (cl-incf calls)
                         (apply original args))))
              (should (niwa-journal-claim-settle claim 'cancelled "user cancelled")))
            ;; The election proves ownership and the deadline itself: its
            ;; `verify' of the claim bytes and its target-side deadline guard
            ;; share the create's program, so no earlier read or clock probe
            ;; adds protection.
            (should (= calls 1)))
          (should-not (niwa-journal-claim-settle claim 'completed "after cancel"))
          (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome claim) :status))))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-claim-prune ()
  ,test
  (test)
  :doc "prunes expired settled attempts while preserving referenced and newest claims"
  (let ((directory (make-temp-file "niwa-claim-prune-" t)))
    (unwind-protect
        (let* ((first (niwa-journal-claim-acquire directory 120))
               (_ (niwa-journal-claim-settle first 'completed ""))
               (second (niwa-journal-claim-acquire directory 120))
               (_ (niwa-journal-claim-settle second 'completed "retained proof"))
               (current (niwa-journal-claim-acquire directory 120)))
          (should (= 0 (iter-do (_ (niwa-journal-claim-prune directory nil 200)))))
          (niwa-test-journal-with-expired-claims (list first second)
            (should (= 1 (iter-do (_ (niwa-journal-claim-prune
                                      directory (list (plist-get second :generation)) 200))))))
          (should-not (niwa-journal-claim-settle first 'completed "late"))
          (should (equal "retained proof" (plist-get (niwa-journal-claim-outcome second) :payload)))
          (should (equal current (niwa-journal-claim-current directory)))
          (niwa-journal-claim-settle current 'completed "")
          (should (= 4 (plist-get (niwa-journal-claim-acquire directory 120) :generation))))
      (delete-directory directory t)))

  :doc "retained-only claim directories need one listing and no resumptions"
  (let ((directory (make-temp-file "niwa-claim-retained-" t)))
    (unwind-protect
        (let* ((first (niwa-journal-claim-acquire directory 120))
               (_ (niwa-journal-claim-settle first 'completed "retained"))
               (current (niwa-journal-claim-acquire directory 120))
               (run (symbol-function 'niwa-session-control-fs-run-program))
               (calls 0) (yields 0))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (&rest args) (cl-incf calls) (apply run args))))
            (should (= 0 (iter-do (_ (niwa-journal-claim-prune
                                      directory (list (plist-get first :generation)) 200))
                           (cl-incf yields)))))
          (should (= calls 1))
          (should (= yields 0))
          (should (equal current (niwa-journal-claim-current directory)))
          (should (equal "retained" (plist-get (niwa-journal-claim-outcome first) :payload))))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-claim-pruning-race ()
  ,test
  (test)
  :doc "an acquirer paused before its locked check cannot reuse a pruned generation"
  (let ((directory (make-temp-file "niwa-claim-delayed-" t)))
    (unwind-protect
        (let* ((first (niwa-journal-claim-acquire directory 120))
               (_ (niwa-journal-claim-settle first 'completed ""))
               (run (symbol-function 'niwa-session-control-fs-run-program))
               paused winner)
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &optional lock)
                       (when (and (not paused) (eq 'verify-latest (plist-get (car operations) :op)))
                         (setq paused t)
                         (let ((second (niwa-journal-claim-acquire directory 120)))
                           (niwa-journal-claim-settle second 'completed "")
                           (setq winner (niwa-journal-claim-acquire directory 120))
                           (should (= 0 (iter-do (_ (niwa-journal-claim-prune directory nil 200)))))
                           (niwa-test-journal-with-expired-claims (list first second)
                             (should (= 2 (iter-do (_ (niwa-journal-claim-prune directory nil 200))))))))
                       (funcall run operations lock))))
            (should-not (niwa-journal-claim-acquire directory 120)))
          (should paused)
          (should (equal winner (niwa-journal-claim-current directory)))
          (should (= 1 (length (directory-files directory nil "\\.claim\\'"))))
          (should (= 3 (plist-get winner :generation))))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-claim--decode
  (:doc "preserves exact claim schemas and rejects malformed ownership or outcome records")
  (let ((record (list :generation 1 :owner (make-string 64 ?a) :expires-at 100)))
    (should (equal record (niwa-journal-claim--decode (json-serialize record) nil)))
    (should (equal (append record (list :status 'completed :payload "result"))
                   (niwa-journal-claim--decode
                    (json-serialize (append record (list :status "completed" :payload "result"))) t)))
    (should-error (niwa-journal-claim--decode "{}" nil))
    (should-error (niwa-journal-claim--decode "not JSON" nil))
    (should-error (niwa-journal-claim--decode (json-serialize record) t))
    (should-error (niwa-journal-claim--decode
                   (json-serialize (append record (list :extra "field"))) nil))
    (should-error (niwa-journal-claim--decode
                   (json-serialize (plist-put (copy-tree record) :generation 0)) nil))))

(niwa-deftest niwa-journal-claim-owned-p ()
  ,test
  (test)
  :doc "observes current identity, absence of settlement and target time in one native program"
  (let ((directory (make-temp-file "niwa-journal-owned-" t)))
    (unwind-protect
        (let ((claim (niwa-journal-claim-acquire directory 120))
              (original (symbol-function 'niwa-session-control-fs-run-program))
              (calls 0))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (apply original args))))
            (should (niwa-journal-claim-owned-p claim)))
          (should (= calls 1))
          (should-not (niwa-journal-claim-owned-p
                       (plist-put (copy-tree claim) :owner (make-string 64 ?0))))
          (should (niwa-journal-claim-settle claim 'cancelled ""))
          (should-not (niwa-journal-claim-owned-p claim))
          (let ((next (niwa-journal-claim-acquire directory 120)))
            (should (niwa-journal-claim-owned-p next))
            (should-not (niwa-journal-claim-owned-p claim))))
      (delete-directory directory t)))

  :doc "checks several claims in one target program and rejects any fenced owner"
  (let ((root (make-temp-file "niwa-journal-owned-batch-" t)))
    (unwind-protect
        (let* ((first (niwa-journal-claim-acquire (file-name-concat root "first") 120))
               (second (niwa-journal-claim-acquire (file-name-concat root "second") 120))
               (native (symbol-function 'niwa-session-control-fs-run-program))
               (calls 0))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (&rest args) (cl-incf calls) (apply native args))))
            (should (niwa-journal-claim-owned-p first second)))
          (should (= calls 1))
          (niwa-journal-claim-settle second 'cancelled "fenced")
          (should-not (niwa-journal-claim-owned-p first second))
          (should-not (niwa-journal-claim-owned-p second first))
          (should (niwa-journal-claim-owned-p first)))
      (delete-directory root t)))

  :doc "refuses expired, missing and malformed ownership without cached proof"
  (let ((directory (make-temp-file "niwa-journal-owned-expiry-" t)))
    (unwind-protect
        (let* ((claim (niwa-journal-claim-acquire directory 120))
               (path (niwa-journal-claim--path claim nil)))
          (should (niwa-journal-claim-owned-p claim))
          ;; Native records retain the same identity, with a deterministic
          ;; deadline in the past; no timer or substituted target clock.
          (plist-put claim :expires-at 1)
          (niwa-session-control-fs-write-file
           path (json-serialize (niwa-journal-claim--record claim)))
          (should-not (niwa-journal-claim-owned-p claim))
          (niwa-session-control-fs-write-file path "{}")
          (should-error (niwa-journal-claim-owned-p claim))
          (delete-file path)
          (should-not (niwa-journal-claim-owned-p claim)))
      (delete-directory directory t)))

  :doc "a newer generation or redirected parent invalidates an earlier observation"
  (let* ((root (make-temp-file "niwa-journal-owned-parent-" t))
         (directory (file-name-concat root "scope"))
         (moved (file-name-concat root "moved")))
    (unwind-protect
        (let* ((claim (niwa-journal-claim-acquire directory 120))
               (next (plist-put (copy-tree claim) :generation 2)))
          (should (niwa-journal-claim-owned-p claim))
          (niwa-session-control-fs-create-file
           (niwa-journal-claim--path next nil)
           (json-serialize (niwa-journal-claim--record next)))
          (should-not (niwa-journal-claim-owned-p claim))
          (should (niwa-journal-claim-owned-p next))
          (rename-file directory moved)
          (make-symbolic-link moved directory)
          (should-not (niwa-journal-claim-owned-p next)))
      (delete-directory root t))))

(niwa-deftest niwa-journal-claim-prune/budget ()
  ,test
  (test)
  :doc "stops yielding after its deletion budget while preserving the newest claim"
  (let ((directory (make-temp-file "niwa-claim-budget-" t)))
    (unwind-protect
        (progn
          (dotimes (n 8)
            (let* ((token (list :directory directory :generation (1+ n)
                                :owner (make-string 64 ?a) :expires-at 1))
                   (record (niwa-journal-claim--record token)))
              (with-temp-file (niwa-journal-claim--path token nil) (insert (json-serialize record)))
              (with-temp-file (niwa-journal-claim--path token t)
                (insert (json-serialize (append record (list :status "completed" :payload "")))))))
          (let ((steps 0))
            (iter-do (_ (niwa-journal-claim-prune directory nil 2)) (cl-incf steps))
            (should (= 2 steps)))
          (should (= 8 (plist-get (niwa-journal-claim-current directory) :generation)))
          (should (= 12 (length (directory-files directory nil "^[0-9]")))))
      (delete-directory directory t))))

(niwa-deftest niwa-journal-claim-current
  (:doc "reports no claim without creating state and fails closed on corrupt state")
  (let* ((root (make-temp-file "niwa-journal-current-" t))
         (directory (file-name-concat root "scope")))
    (unwind-protect
        (progn
          (should-not (niwa-journal-claim-current directory))
          (should-not (file-exists-p directory))
          (should (niwa-journal-claim-acquire directory 120))
          (write-region "{}" nil (file-name-concat directory "00000000000000000001.claim")
                        nil 'silent)
          (should-error (niwa-journal-claim-current directory))
          (should-error (niwa-journal-claim-acquire directory 120)))
      (delete-directory root t))))

(niwa-deftest niwa-journal-claim-outcome
  (:doc "recovers the winning payload and refuses an outcome with another owner")
  (let ((directory (make-temp-file "niwa-journal-outcome-" t)))
    (unwind-protect
        (let ((claim (niwa-journal-claim-acquire directory 120)))
          (should-not (niwa-journal-claim-outcome claim))
          (should (niwa-journal-claim-settle claim 'completed "recoverable result"))
          (should (equal "recoverable result"
                         (plist-get (niwa-journal-claim-outcome
                                     (niwa-journal-claim-current directory)) :payload)))
          (should-error
           (niwa-journal-claim-outcome
            (plist-put (copy-tree claim) :owner (make-string 64 ?0)))))
      (delete-directory directory t))))


(defun niwa-test-claim-prune-fixture (directory)
  "Settle generations 1 and 2 in DIRECTORY, leave 3 current; return 1 and 2."
  (let* ((first (niwa-journal-claim-acquire directory 120))
         (_ (niwa-journal-claim-settle first 'completed "one"))
         (second (niwa-journal-claim-acquire directory 120))
         (_ (niwa-journal-claim-settle second 'completed "two")))
    (niwa-journal-claim-acquire directory 120)
    (list first second)))

(defun niwa-test-claim-files (directory)
  "Return DIRECTORY's claim and outcome file names."
  (directory-files directory nil "\\.\\(?:claim\\|outcome\\)\\'"))

(niwa-deftest niwa-journal-claim-prune/programs ()
  ,test
  (test)
  :doc "each pruned generation costs two control programs"
  (let ((directory (make-temp-file "niwa-claim-programs-" t)))
    (unwind-protect
        (let* ((expired (niwa-test-claim-prune-fixture directory))
               (run (symbol-function 'niwa-session-control-fs-run-program))
               (calls 0) (marks nil))
          (niwa-test-journal-with-expired-claims expired
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest args) (cl-incf calls) (apply run args))))
              ;; Each generation's work lies between its yield and the next.
              (should (= 2 (iter-do (_ (niwa-journal-claim-prune directory nil 200))
                             (push calls marks))))
              (push calls marks)))
          (setq marks (nreverse marks))
          (should (= 3 (length marks)))
          (should (equal '(2 2) (cl-mapcar #'- (cdr marks) marks))))
      (delete-directory directory t)))

  :doc "absent and malformed records keep their results and delete nothing unintended"
  (dolist (case '((:absent-outcome nil 1 ("1.claim" "3.claim"))
                  (:absent-claim nil 2 ("3.claim"))
                  (:malformed-outcome error 0 ("1.claim" "1.outcome" "2.claim" "2.outcome" "3.claim"))
                  (:malformed-claim error 0 ("1.claim" "1.outcome" "2.claim" "2.outcome" "3.claim"))))
    (let ((directory (make-temp-file "niwa-claim-edges-" t)))
      (unwind-protect
          (let* ((expired (niwa-test-claim-prune-fixture directory))
                 (claim (niwa-journal-claim--path (car expired) nil))
                 (outcome (niwa-journal-claim--path (car expired) t)))
            (pcase (car case)
              (:absent-outcome (delete-file outcome))
              (:absent-claim (delete-file claim))
              (:malformed-outcome (write-region "{}" nil outcome nil 'silent))
              (:malformed-claim (write-region "garbage" nil claim nil 'silent)))
            (niwa-test-journal-with-expired-claims expired
              (let ((run (lambda ()
                           (iter-do (_ (niwa-journal-claim-prune directory nil 200))))))
                (if (eq (nth 1 case) 'error)
                    (should-error (funcall run))
                  (should (= (nth 2 case) (funcall run))))))
            (should (equal (mapcar (lambda (name) (replace-regexp-in-string "\\`0+" "" name))
                                   (niwa-test-claim-files directory))
                           (nth 3 case))))
        (delete-directory directory t)))))

(provide 'test-niwa-journal-claim)
;;; test-niwa-journal-claim.el ends here
