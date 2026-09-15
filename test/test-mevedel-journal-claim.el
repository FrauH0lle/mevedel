;;; test-mevedel-journal-claim.el -- Journal ownership tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises competing claims and immutable settlement through real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-claim)

(mevedel-deftest mevedel-journal-claim-acquire ()
  ,test
  (test)
  :doc "excludes contenders and fences old owners after durable settlement"
  (let ((directory (make-temp-file "mevedel-journal-claim-" t)))
    (unwind-protect
        (let ((first (mevedel-journal-claim-acquire directory 120)))
          (should first)
          (should-not (mevedel-journal-claim-acquire directory 120))
          (should (mevedel-journal-claim-settle first 'completed "frozen result"))
          (should (equal "frozen result"
                         (plist-get (mevedel-journal-claim-outcome first) :payload)))
          (let ((second (mevedel-journal-claim-acquire directory 120)))
            (should second)
            (should (> (plist-get second :generation) (plist-get first :generation)))
            (should-not (equal (plist-get second :owner) (plist-get first :owner)))
            (should-not (mevedel-journal-claim-settle first 'completed "late result"))
            (should-not (mevedel-journal-claim-outcome second))
            (should (equal "frozen result"
                           (plist-get (mevedel-journal-claim-outcome first) :payload)))
            (should (mevedel-journal-claim-settle second 'failed "provider unavailable"))))
      (delete-directory directory t)))

  :doc "nested work uses the admission deadline without extending it"
  (let ((root (make-temp-file "mevedel-journal-deadline-" t)))
    (unwind-protect
        (let* ((admission (mevedel-journal-claim-acquire (file-name-concat root "admission") 120))
               (deadline (plist-get admission :expires-at))
               (job (mevedel-journal-claim-acquire (file-name-concat root "job") 120 deadline)))
          (should job)
          (should (= deadline (plist-get job :expires-at)))
          (should-error (mevedel-journal-claim-acquire
                         (file-name-concat root "expired") 120 1))
          (should-error (mevedel-journal-claim-acquire
                         (file-name-concat root "extended") 120 (+ deadline 1000))))
      (delete-directory root t)))

  :doc "elects exactly one owner across independent Emacs processes"
  (let* ((root (make-temp-file "mevedel-journal-race-" t))
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
                           "-l" "mevedel-journal-claim" "--eval"
                           (format
                            "(let ((deadline (+ (float-time) 10))) (while (and (not (file-exists-p %S)) (< (float-time) deadline)) (sleep-for 0.01)) (unless (file-exists-p %S) (error \"Barrier unavailable\")) (prin1 (mevedel-journal-claim-acquire %S 120)))"
                            barrier barrier directory)))))
              (push process processes)
              (push buffer buffers)))
          (write-region "go" nil barrier nil 'silent)
          (let ((deadline (+ (float-time) 15)))
            (while (and (cl-some #'process-live-p processes) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (dolist (process processes)
            (should-not (process-live-p process))
            (should (= 0 (process-exit-status process))))
          (let ((results (mapcar (lambda (buffer)
                                   (with-current-buffer buffer
                                     (car (read-from-string (buffer-string)))))
                                 buffers)))
            (should (= 1 (length (delq nil results))))
            (should (equal (car (delq nil results))
                           (mevedel-journal-claim-current directory)))))
      (dolist (process processes)
        (when (process-live-p process) (delete-process process)))
      (mapc #'kill-buffer buffers)
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-claim-settle ()
  ,test
  (test)
  :doc "fences an expired owner and preserves the successor after a late callback"
  (let ((directory (make-temp-file "mevedel-journal-expiry-" t)))
    (unwind-protect
        (let ((first (mevedel-journal-claim-acquire directory 1)))
          (should first)
          ;; Exercise the target clock without substituting the client clock.
          (let ((deadline (+ (float-time) 5)))
            (while (and (< (mevedel-session-control-fs-target-time directory)
                           (plist-get first :expires-at))
                        (< (float-time) deadline))
              (sleep-for 0.02)))
          (let ((second (mevedel-journal-claim-acquire directory 120)))
            (should second)
            (should (eq 'expired (plist-get (mevedel-journal-claim-outcome first) :status)))
            (dolist (status '(completed failed cancelled))
              (should-not (mevedel-journal-claim-settle first status "late callback")))
            (should-not (mevedel-journal-claim-outcome second))
            (should (mevedel-journal-claim-settle second 'completed "successor result"))
            (should (equal "successor result"
                           (plist-get (mevedel-journal-claim-outcome second) :payload)))))
      (delete-directory directory t)))

  :doc "refuses forged ownership, invalid statuses, and duplicate settlement"
  (let ((directory (make-temp-file "mevedel-journal-settlement-" t)))
    (unwind-protect
        (let ((claim (mevedel-journal-claim-acquire directory 120)))
          (should-error (mevedel-journal-claim-settle claim 'expired "not a callback status"))
          (should-error (mevedel-journal-claim-settle claim 'completed '(not text)))
          (should-not
           (mevedel-journal-claim-settle
            (plist-put (copy-tree claim) :owner (make-string 64 ?0))
            'completed "foreign result"))
          (should-not (mevedel-journal-claim-outcome claim))
          (let ((original (symbol-function 'mevedel-session-control-fs-run-program))
                (calls 0))
            (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                       (lambda (&rest args)
                         (cl-incf calls)
                         (apply original args))))
              (should (mevedel-journal-claim-settle claim 'cancelled "user cancelled")))
            ;; The final target-side deadline guard owns admission. A separate
            ;; earlier clock probe adds no protection against expiry here.
            (should (= calls 2)))
          (should-not (mevedel-journal-claim-settle claim 'completed "after cancel"))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome claim) :status))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-journal-claim-prune ()
  ,test
  (test)
  :doc "prunes expired settled attempts while preserving referenced and newest claims"
  (let ((directory (make-temp-file "mevedel-claim-prune-" t)))
    (unwind-protect
        (let* ((first (mevedel-journal-claim-acquire directory 3))
               (_ (mevedel-journal-claim-settle first 'completed ""))
               (second (mevedel-journal-claim-acquire directory 3))
               (_ (mevedel-journal-claim-settle second 'completed "retained proof"))
               (current (mevedel-journal-claim-acquire directory 120)))
          (while (< (mevedel-session-control-fs-target-time directory)
                    (plist-get second :expires-at))
            (sleep-for 0.02))
          (should (= 1 (mevedel-journal-claim-prune
                        directory (list (plist-get second :generation)) 200)))
          (should-not (mevedel-journal-claim-settle first 'completed "late"))
          (should (equal "retained proof" (plist-get (mevedel-journal-claim-outcome second) :payload)))
          (should (equal current (mevedel-journal-claim-current directory)))
          (mevedel-journal-claim-settle current 'completed "")
          (should (= 4 (plist-get (mevedel-journal-claim-acquire directory 120) :generation))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-journal-claim-pruning-race ()
  ,test
  (test)
  :doc "an acquirer paused before its locked check cannot reuse a pruned generation"
  (let ((directory (make-temp-file "mevedel-claim-delayed-" t)))
    (unwind-protect
        (let* ((first (mevedel-journal-claim-acquire directory 3))
               (_ (mevedel-journal-claim-settle first 'completed ""))
               (run (symbol-function 'mevedel-session-control-fs-run-program))
               paused winner)
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations &optional lock)
                       (when (and (not paused) (eq 'verify-latest (plist-get (car operations) :op)))
                         (setq paused t)
                         (let ((second (mevedel-journal-claim-acquire directory 3)))
                           (mevedel-journal-claim-settle second 'completed "")
                           (while (< (mevedel-session-control-fs-target-time directory) (plist-get second :expires-at))
                             (sleep-for 0.02))
                           (setq winner (mevedel-journal-claim-acquire directory 120))
                           (should (= 2 (mevedel-journal-claim-prune directory nil 200)))))
                       (funcall run operations lock))))
            (should-not (mevedel-journal-claim-acquire directory 120)))
          (should (equal winner (mevedel-journal-claim-current directory)))
          (should (= 1 (length (directory-files directory nil "\\.claim\\'"))))
          (should (= 3 (plist-get winner :generation))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-journal-claim--decode
  (:doc "preserves exact claim schemas and rejects malformed ownership or outcome records")
  (let ((record (list :generation 1 :owner (make-string 64 ?a) :expires-at 100)))
    (should (equal record (mevedel-journal-claim--decode (json-serialize record) nil)))
    (should (equal (append record (list :status 'completed :payload "result"))
                   (mevedel-journal-claim--decode
                    (json-serialize (append record (list :status "completed" :payload "result"))) t)))
    (should-error (mevedel-journal-claim--decode "{}" nil))
    (should-error (mevedel-journal-claim--decode "not JSON" nil))
    (should-error (mevedel-journal-claim--decode (json-serialize record) t))
    (should-error (mevedel-journal-claim--decode
                   (json-serialize (append record (list :extra "field"))) nil))
    (should-error (mevedel-journal-claim--decode
                   (json-serialize (plist-put (copy-tree record) :generation 0)) nil))))

(mevedel-deftest mevedel-journal-claim-owned-p ()
  ,test
  (test)
  :doc "observes current identity, absence of settlement and target time in one native program"
  (let ((directory (make-temp-file "mevedel-journal-owned-" t)))
    (unwind-protect
        (let ((claim (mevedel-journal-claim-acquire directory 120))
              (original (symbol-function 'mevedel-session-control-fs-run-program))
              (calls 0))
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (apply original args))))
            (should (mevedel-journal-claim-owned-p claim)))
          (should (= calls 1))
          (should-not (mevedel-journal-claim-owned-p
                       (plist-put (copy-tree claim) :owner (make-string 64 ?0))))
          (should (mevedel-journal-claim-settle claim 'cancelled ""))
          (should-not (mevedel-journal-claim-owned-p claim))
          (let ((next (mevedel-journal-claim-acquire directory 120)))
            (should (mevedel-journal-claim-owned-p next))
            (should-not (mevedel-journal-claim-owned-p claim))))
      (delete-directory directory t)))

  :doc "refuses expired, missing and malformed ownership without cached proof"
  (let ((directory (make-temp-file "mevedel-journal-owned-expiry-" t)))
    (unwind-protect
        (let* ((claim (mevedel-journal-claim-acquire directory 120))
               (path (mevedel-journal-claim--path claim nil)))
          (should (mevedel-journal-claim-owned-p claim))
          ;; Native records retain the same identity, with a deterministic
          ;; deadline in the past; no timer or substituted target clock.
          (plist-put claim :expires-at 1)
          (mevedel-session-control-fs-write-file
           path (json-serialize (mevedel-journal-claim--record claim)))
          (should-not (mevedel-journal-claim-owned-p claim))
          (mevedel-session-control-fs-write-file path "{}")
          (should-error (mevedel-journal-claim-owned-p claim))
          (delete-file path)
          (should-not (mevedel-journal-claim-owned-p claim)))
      (delete-directory directory t)))

  :doc "a newer generation or redirected parent invalidates an earlier observation"
  (let* ((root (make-temp-file "mevedel-journal-owned-parent-" t))
         (directory (file-name-concat root "scope"))
         (moved (file-name-concat root "moved")))
    (unwind-protect
        (let* ((claim (mevedel-journal-claim-acquire directory 120))
               (next (plist-put (copy-tree claim) :generation 2)))
          (should (mevedel-journal-claim-owned-p claim))
          (mevedel-session-control-fs-create-file
           (mevedel-journal-claim--path next nil)
           (json-serialize (mevedel-journal-claim--record next)))
          (should-not (mevedel-journal-claim-owned-p claim))
          (should (mevedel-journal-claim-owned-p next))
          (rename-file directory moved)
          (make-symbolic-link moved directory)
          (should-not (mevedel-journal-claim-owned-p next)))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-claim-current
  (:doc "reports no claim without creating state and fails closed on corrupt state")
  (let* ((root (make-temp-file "mevedel-journal-current-" t))
         (directory (file-name-concat root "scope")))
    (unwind-protect
        (progn
          (should-not (mevedel-journal-claim-current directory))
          (should-not (file-exists-p directory))
          (should (mevedel-journal-claim-acquire directory 120))
          (write-region "{}" nil (file-name-concat directory "00000000000000000001.claim")
                        nil 'silent)
          (should-error (mevedel-journal-claim-current directory))
          (should-error (mevedel-journal-claim-acquire directory 120)))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-claim-outcome
  (:doc "recovers the winning payload and refuses an outcome with another owner")
  (let ((directory (make-temp-file "mevedel-journal-outcome-" t)))
    (unwind-protect
        (let ((claim (mevedel-journal-claim-acquire directory 120)))
          (should-not (mevedel-journal-claim-outcome claim))
          (should (mevedel-journal-claim-settle claim 'completed "recoverable result"))
          (should (equal "recoverable result"
                         (plist-get (mevedel-journal-claim-outcome
                                     (mevedel-journal-claim-current directory)) :payload)))
          (should-error
           (mevedel-journal-claim-outcome
            (plist-put (copy-tree claim) :owner (make-string 64 ?0)))))
      (delete-directory directory t))))

(provide 'test-mevedel-journal-claim)
;;; test-mevedel-journal-claim.el ends here
