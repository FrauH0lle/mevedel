;;; test-mevedel-artifact-lease.el --- Store item lease tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests item leases between two simulated clients: acquisition, read-only
;; refusal with a hand-over request, renewal and hand-over, idle release,
;; confirmed takeover after expiry, fenced writes, and the target programs
;; each step costs.

;;; Code:

(require 'mevedel-artifact-lease)
(require 'mevedel-shared-editing)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defconst mevedel-artifact-lease-test--other (make-string 64 ?b)
  "The second simulated client's id.")

(defmacro mevedel-artifact-lease-test--with-workspace (&rest body)
  "Run BODY with WORKSPACE in a temp ROOT, as client `a', leaving no lease."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "mevedel-artifact-lease-" t)))
          (workspace (mevedel-workspace--create :type 'file :id "w" :root root :name "w"))
          (mevedel-session-durability--client-id (make-string 64 ?a))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                mevedel-artifact-lease--held)
       (delete-directory root t))))

(defmacro mevedel-artifact-lease-test--as-other (&rest body)
  "Run BODY as the second client, which holds nothing yet."
  (declare (indent 0) (debug t))
  `(let ((mevedel-session-durability--client-id mevedel-artifact-lease-test--other)
         (mevedel-artifact-lease--held (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                mevedel-artifact-lease--held))))

(defmacro mevedel-artifact-lease-test--counting (count &rest body)
  "Run BODY, setting COUNT to the number of target programs it ran."
  (declare (indent 1) (debug t))
  `(let ((run (symbol-function 'mevedel-session-control-fs-run-program)))
     (setq ,count 0)
     (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (&rest args) (cl-incf ,count) (apply run args))))
       ,@body)))

(mevedel-deftest mevedel-artifact-lease-acquire ()
  ,test
  (test)
  :doc "lets one client edit and asks the holder to hand over for another"
  (mevedel-artifact-lease-test--with-workspace
    (mevedel-artifact-lease-acquire workspace "board")
    (should (mevedel-artifact-lease-held workspace "board"))
    (should (mevedel-artifact-lease-ensure workspace "board"))
    (should (string-prefix-p (expand-file-name ".mevedel/leases/artifacts/board" root)
                             (mevedel-artifact-lease-directory workspace "board")))
    (mevedel-artifact-lease-test--as-other
      (should-error (mevedel-artifact-lease-acquire workspace "board"))
      (should-not (mevedel-artifact-lease-held workspace "board"))
      (should (file-exists-p (file-name-concat
                              (mevedel-artifact-lease-directory workspace "board")
                              "request")))))

  :doc "hands an idle item over to the requester on renewal"
  (mevedel-artifact-lease-test--with-workspace
    (let ((directory (mevedel-artifact-lease-directory workspace "board")))
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-test--as-other
        (ignore-errors (mevedel-artifact-lease-acquire workspace "board")))
      ;; Busy items are not handed over.
      (plist-put (gethash directory mevedel-artifact-lease--held) :touched 0)
      (cl-letf (((symbol-function 'mevedel-shared-editing-item-busy-p) (lambda (&rest _) t)))
        (mevedel-artifact-lease--renew directory))
      (should (gethash directory mevedel-artifact-lease--held))
      ;; Nor are items edited moments ago, though the queue is empty.
      (plist-put (gethash directory mevedel-artifact-lease--held) :touched (float-time))
      (mevedel-artifact-lease--renew directory)
      (should (gethash directory mevedel-artifact-lease--held))
      (plist-put (gethash directory mevedel-artifact-lease--held) :touched 0)
      (let (messages)
        (mevedel-test--with-captured-messages messages
          (mevedel-artifact-lease--renew directory))
        (should (string-match-p "handed board over" messages)))
      (should-not (gethash directory mevedel-artifact-lease--held))
      ;; Reserved for the requester; nobody else may claim it meanwhile.
      (should-error (mevedel-artifact-lease-acquire workspace "board"))
      (mevedel-artifact-lease-test--as-other
        (mevedel-artifact-lease-acquire workspace "board")
        (should (mevedel-artifact-lease-held workspace "board")))
      ;; The answered request does not linger to hand a later holder's
      ;; item away.
      (should-not (file-exists-p (file-name-concat directory "request")))))

  :doc "ignores a request nobody repeated within a lease period"
  (mevedel-artifact-lease-test--with-workspace
    (let ((directory (mevedel-artifact-lease-directory workspace "board")))
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-test--as-other
        (ignore-errors (mevedel-artifact-lease-acquire workspace "board")))
      (let ((request (mevedel-session-durability--read-plist
                      (mevedel-artifact-lease--request-path directory))))
        (should (numberp (plist-get request :at)))
        (mevedel-session-durability--write-plist
         (mevedel-artifact-lease--request-path directory)
         (plist-put request :at (- (plist-get request :at)
                                   mevedel-session-lease-seconds 1))))
      ;; Quiet long enough to hand over, not yet to release.
      (plist-put (gethash directory mevedel-artifact-lease--held) :touched
                 (- (float-time) 20))
      (mevedel-artifact-lease--renew directory)
      (should (gethash directory mevedel-artifact-lease--held))
      (should-not (file-exists-p (mevedel-artifact-lease--request-path directory)))))

  :doc "takes over an expired foreign lease at the next edit, saying so"
  (mevedel-artifact-lease-test--with-workspace
    (mevedel-artifact-lease-acquire workspace "board")
    (cl-letf (((symbol-function 'mevedel-artifact-lease--now)
               (lambda (_directory) 1e12)))
      (mevedel-artifact-lease-test--as-other
        (let (messages)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) (error "Asked"))))
            (mevedel-test--with-captured-messages messages
              (mevedel-artifact-lease-acquire workspace "board")))
          (should (string-match-p "took over editing board" messages)))
        (should (mevedel-artifact-lease-held workspace "board")))))

  :doc "costs few target programs: one observation feeds the claim"
  (mevedel-artifact-lease-test--with-workspace
    (let (count)
      (make-directory (file-name-concat root ".mevedel/leases/artifacts") t)
      ;; New item: observe, create the directory, observe, claim, settle.
      (mevedel-artifact-lease-test--counting count
        (mevedel-artifact-lease-acquire workspace "board"))
      (should (<= count 5))
      (mevedel-artifact-lease-release workspace "board")
      ;; Released item: observe, read the records, claim, settle.
      (mevedel-artifact-lease-test--counting count
        (mevedel-artifact-lease-acquire workspace "board"))
      (should (<= count 4)))))

(mevedel-deftest mevedel-artifact-lease-ensure ()
  ,test
  (test)
  :doc "reuses a recently renewed lease without reading the target clock"
  (mevedel-artifact-lease-test--with-workspace
    (let ((directory (mevedel-artifact-lease-directory workspace "board"))
          count)
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-test--counting count
        (should (mevedel-artifact-lease-ensure workspace "board")))
      (should (= 0 count))
      ;; Past the renewal margin, the target clock decides.
      (plist-put (gethash directory mevedel-artifact-lease--held) :renewed 0)
      (mevedel-artifact-lease-test--counting count
        (should (mevedel-artifact-lease-ensure workspace "board")))
      (should (= 1 count))
      (cl-letf (((symbol-function 'mevedel-artifact-lease--now)
                 (lambda (_directory) 1e12)))
        ;; Expired: reclaimed, being this client's own.
        (should (mevedel-artifact-lease-ensure workspace "board"))
        (should (> (plist-get (plist-get (gethash directory mevedel-artifact-lease--held)
                                         :record)
                              :generation)
                   1))))))

(mevedel-deftest mevedel-artifact-lease-write ()
  ,test
  (test)
  :doc "writes only while the lease is still this client's"
  (mevedel-artifact-lease-test--with-workspace
    (let ((path (file-name-concat root "state.json")))
      (should-error (mevedel-artifact-lease-write workspace "board" path "x"))
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-write workspace "board" path "one")
      (should (equal "one" (with-temp-buffer (insert-file-contents path) (buffer-string))))
      ;; Another client takes over behind this one's back.
      (cl-letf (((symbol-function 'mevedel-artifact-lease--now)
                 (lambda (_directory) 1e12)))
        (mevedel-artifact-lease-test--as-other
          (mevedel-artifact-lease-acquire workspace "board")))
      (should-error (mevedel-artifact-lease-write workspace "board" path "two"))
      (should (equal "one" (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(mevedel-deftest mevedel-artifact-lease-run ()
  ,test (test)
  :doc "A reentrant heartbeat cannot invalidate the active writer's proof"
  (mevedel-artifact-lease-test--with-workspace
    (let* ((directory (mevedel-artifact-lease-directory workspace "board"))
           (path (file-name-concat root "state.json"))
           (run (symbol-function 'mevedel-session-control-fs-run-program)))
      (mevedel-artifact-lease-acquire workspace "board")
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (operations &optional lock)
                   (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program) run))
                     (let ((mevedel-session-lease-seconds 120))
                       (mevedel-artifact-lease--renew directory)))
                   (funcall run operations lock))))
        (should (mevedel-artifact-lease-write workspace "board" path "saved")))
      (should (equal "saved" (with-temp-buffer (insert-file-contents path) (buffer-string))))))

  :doc "An expired generation cannot write even before a successor claims it"
  (mevedel-artifact-lease-test--with-workspace
    (let ((path (file-name-concat root "state.json")))
      (cl-letf (((symbol-function 'mevedel-artifact-lease--now)
                 (lambda (_directory) 1)))
        (mevedel-artifact-lease-acquire workspace "board"))
      (should-error (mevedel-artifact-lease-run
                     workspace "board" (list (list :op 'write :path path :content "late"))))
      (should-not (file-exists-p path))))

  :doc "A newer generation fences both writes and renewal before its predecessor is pruned"
  (mevedel-artifact-lease-test--with-workspace
    (let* ((directory (mevedel-artifact-lease-directory workspace "board"))
           (path (file-name-concat root "state.json"))
           (first (mevedel-artifact-lease-acquire workspace "board"))
           (next (copy-sequence first)))
      (setq next (plist-put next :generation (1+ (plist-get first :generation))))
      (setq next (plist-put next :client-id mevedel-artifact-lease-test--other))
      (mevedel-session-durability--write-generation directory next)
      (mevedel-test--with-captured-messages nil
        (mevedel-artifact-lease--renew directory))
      (should-not (gethash directory mevedel-artifact-lease--held))
      ;; Restore stale in-memory ownership to exercise the independent write proof.
      (mevedel-artifact-lease--hold workspace "board" directory first)
      (should-error (mevedel-artifact-lease-run
                     workspace "board" (list (list :op 'write :path path :content "late"))))
      (should-not (file-exists-p path)))))

(mevedel-deftest mevedel-artifact-lease--claim
  (:doc "A stale observation cannot replace a renewed generation")
  (mevedel-artifact-lease-test--with-workspace
    (let* ((directory (mevedel-artifact-lease-directory workspace "board"))
           (first (copy-sequence (mevedel-artifact-lease-acquire workspace "board"))))
      (let ((mevedel-session-lease-seconds 120))
        (mevedel-artifact-lease--renew directory))
      (mevedel-artifact-lease-test--as-other
        (should-not (mevedel-artifact-lease--claim directory first "board")))
      (should (mevedel-artifact-lease-held workspace "board")))))

(mevedel-deftest mevedel-artifact-lease-release ()
  ,test
  (test)
  :doc "releases on request and after a quiet period, and renews otherwise"
  (mevedel-artifact-lease-test--with-workspace
    (let ((directory (mevedel-artifact-lease-directory workspace "board")))
      (mevedel-artifact-lease-acquire workspace "board")
      (let ((before (plist-get (plist-get (gethash directory mevedel-artifact-lease--held)
                                          :record)
                               :expires-at)))
        (mevedel-artifact-lease--renew directory)
        (should (>= (plist-get (plist-get (gethash directory mevedel-artifact-lease--held)
                                          :record)
                               :expires-at)
                    before)))
      (plist-put (gethash directory mevedel-artifact-lease--held) :touched 0)
      (mevedel-artifact-lease--renew directory)
      (should-not (gethash directory mevedel-artifact-lease--held))
      ;; Released without a reservation: another client edits at once.
      (mevedel-artifact-lease-test--as-other
        (mevedel-artifact-lease-acquire workspace "board")
        (mevedel-artifact-lease-release-all))
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-release-all)
      (should-not (gethash directory mevedel-artifact-lease--held))
      (mevedel-artifact-lease-acquire workspace "board")
      (mevedel-artifact-lease-forget-item workspace "board")
      (should-not (file-exists-p directory))))

  :doc "a stale holder cannot report a successful handover or release its successor"
  (mevedel-artifact-lease-test--with-workspace
    (let* ((directory (mevedel-artifact-lease-directory workspace "board"))
           (first (mevedel-artifact-lease-acquire workspace "board"))
           (next (copy-sequence first)))
      (setq next (plist-put next :generation (1+ (plist-get first :generation))))
      (setq next (plist-put next :client-id mevedel-artifact-lease-test--other))
      (mevedel-session-durability--write-generation directory next)
      (should-not (mevedel-artifact-lease-release workspace "board"))
      (should (equal next (mevedel-session-durability--lease-head directory)))
      (should-not (gethash directory mevedel-artifact-lease--held)))))

(provide 'test-mevedel-artifact-lease)
;;; test-mevedel-artifact-lease.el ends here
