;;; test-niwa-artifact-lease.el --- Store item lease tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests item leases between two simulated clients: acquisition, read-only
;; refusal with a hand-over request, renewal and hand-over, idle release,
;; confirmed takeover after expiry, fenced writes, and the target programs
;; each step costs.

;;; Code:

(require 'niwa-artifact-lease)
(require 'niwa-shared-editing)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defconst niwa-artifact-lease-test--other (make-string 64 ?b)
  "The second simulated client's id.")

(defmacro niwa-artifact-lease-test--with-workspace (&rest body)
  "Run BODY with WORKSPACE in a temp ROOT, as client `a', leaving no lease."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "niwa-artifact-lease-" t)))
          (workspace (niwa-workspace--create :type 'file :id "w" :root root :name "w"))
          (niwa-session-durability--client-id (make-string 64 ?a))
          (niwa-artifact-lease--held (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                niwa-artifact-lease--held)
       (delete-directory root t))))

(defmacro niwa-artifact-lease-test--as-other (&rest body)
  "Run BODY as the second client, which holds nothing yet."
  (declare (indent 0) (debug t))
  `(let ((niwa-session-durability--client-id niwa-artifact-lease-test--other)
         (niwa-artifact-lease--held (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_directory held)
                  (when (timerp (plist-get held :timer))
                    (cancel-timer (plist-get held :timer))))
                niwa-artifact-lease--held))))

(defmacro niwa-artifact-lease-test--counting (count &rest body)
  "Run BODY, setting COUNT to the number of target programs it ran."
  (declare (indent 1) (debug t))
  `(let ((run (symbol-function 'niwa-session-control-fs-run-program)))
     (setq ,count 0)
     (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (&rest args) (cl-incf ,count) (apply run args))))
       ,@body)))

(niwa-deftest niwa-artifact-lease-acquire ()
  ,test
  (test)
  :doc "lets one client edit and asks the holder to hand over for another"
  (niwa-artifact-lease-test--with-workspace
    (niwa-artifact-lease-acquire workspace "board")
    (should (niwa-artifact-lease-held workspace "board"))
    (should (niwa-artifact-lease-ensure workspace "board"))
    (should (string-prefix-p (expand-file-name ".niwa/leases/artifacts/board" root)
                             (niwa-artifact-lease-directory workspace "board")))
    (niwa-artifact-lease-test--as-other
      (should-error (niwa-artifact-lease-acquire workspace "board"))
      (should-not (niwa-artifact-lease-held workspace "board"))
      (should (file-exists-p (file-name-concat
                              (niwa-artifact-lease-directory workspace "board")
                              "request")))))

  :doc "hands an idle item over to the requester on renewal"
  (niwa-artifact-lease-test--with-workspace
    (let ((directory (niwa-artifact-lease-directory workspace "board")))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-test--as-other
        (ignore-errors (niwa-artifact-lease-acquire workspace "board")))
      ;; Busy items are not handed over.
      (plist-put (gethash directory niwa-artifact-lease--held) :touched 0)
      (cl-letf (((symbol-function 'niwa-shared-editing-item-busy-p) (lambda (&rest _) t)))
        (niwa-artifact-lease--renew directory))
      (should (gethash directory niwa-artifact-lease--held))
      ;; Nor are items edited moments ago, though the queue is empty.
      (plist-put (gethash directory niwa-artifact-lease--held) :touched (float-time))
      (niwa-artifact-lease--renew directory)
      (should (gethash directory niwa-artifact-lease--held))
      (plist-put (gethash directory niwa-artifact-lease--held) :touched 0)
      (let (messages)
        (niwa-test--with-captured-messages messages
          (niwa-artifact-lease--renew directory))
        (should (string-match-p "handed board over" messages)))
      (should-not (gethash directory niwa-artifact-lease--held))
      ;; Reserved for the requester; nobody else may claim it meanwhile.
      (should-error (niwa-artifact-lease-acquire workspace "board"))
      (niwa-artifact-lease-test--as-other
        (niwa-artifact-lease-acquire workspace "board")
        (should (niwa-artifact-lease-held workspace "board")))
      ;; The answered request does not linger to hand a later holder's
      ;; item away.
      (should-not (file-exists-p (file-name-concat directory "request")))))

  :doc "ignores a request nobody repeated within a lease period"
  (niwa-artifact-lease-test--with-workspace
    (let ((directory (niwa-artifact-lease-directory workspace "board")))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-test--as-other
        (ignore-errors (niwa-artifact-lease-acquire workspace "board")))
      (let ((request (niwa-session-durability--read-plist
                      (niwa-artifact-lease--request-path directory))))
        (should (numberp (plist-get request :at)))
        (niwa-session-durability--write-plist
         (niwa-artifact-lease--request-path directory)
         (plist-put request :at (- (plist-get request :at)
                                   niwa-session-lease-seconds 1))))
      ;; Quiet long enough to hand over, not yet to release.
      (plist-put (gethash directory niwa-artifact-lease--held) :touched
                 (- (float-time) 20))
      (niwa-artifact-lease--renew directory)
      (should (gethash directory niwa-artifact-lease--held))
      (should-not (file-exists-p (niwa-artifact-lease--request-path directory)))))

  :doc "takes over an expired foreign lease at the next edit, saying so"
  (niwa-artifact-lease-test--with-workspace
    (niwa-artifact-lease-acquire workspace "board")
    (cl-letf (((symbol-function 'niwa-artifact-lease--now)
               (lambda (_directory) 1e12)))
      (niwa-artifact-lease-test--as-other
        (let (messages)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) (error "Asked"))))
            (niwa-test--with-captured-messages messages
              (niwa-artifact-lease-acquire workspace "board")))
          (should (string-match-p "took over editing board" messages)))
        (should (niwa-artifact-lease-held workspace "board")))))

  :doc "costs few target programs: one observation feeds the claim"
  (niwa-artifact-lease-test--with-workspace
    (let (count)
      (make-directory (file-name-concat root ".niwa/leases/artifacts") t)
      ;; New item: observe, create the directory, observe, claim, settle.
      (niwa-artifact-lease-test--counting count
        (niwa-artifact-lease-acquire workspace "board"))
      (should (<= count 5))
      (niwa-artifact-lease-release workspace "board")
      ;; Released item: observe, read the records, claim, settle.
      (niwa-artifact-lease-test--counting count
        (niwa-artifact-lease-acquire workspace "board"))
      (should (<= count 4)))))

(niwa-deftest niwa-artifact-lease-ensure ()
  ,test
  (test)
  :doc "reuses a recently renewed lease without reading the target clock"
  (niwa-artifact-lease-test--with-workspace
    (let ((directory (niwa-artifact-lease-directory workspace "board"))
          count)
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-test--counting count
        (should (niwa-artifact-lease-ensure workspace "board")))
      (should (= 0 count))
      ;; Past the renewal margin, the target clock decides.
      (plist-put (gethash directory niwa-artifact-lease--held) :renewed 0)
      (niwa-artifact-lease-test--counting count
        (should (niwa-artifact-lease-ensure workspace "board")))
      (should (= 1 count))
      (cl-letf (((symbol-function 'niwa-artifact-lease--now)
                 (lambda (_directory) 1e12)))
        ;; Expired: reclaimed, being this client's own.
        (should (niwa-artifact-lease-ensure workspace "board"))
        (should (> (plist-get (plist-get (gethash directory niwa-artifact-lease--held)
                                         :record)
                              :generation)
                   1))))))

(niwa-deftest niwa-artifact-lease-held ()
  ,test
  (test)
  :doc "lapses once the lease went a whole lease period without renewal"
  (niwa-artifact-lease-test--with-workspace
    (let ((directory (niwa-artifact-lease-directory workspace "board")))
      (should-not (niwa-artifact-lease-held workspace "board"))
      (niwa-artifact-lease-acquire workspace "board")
      (let ((token (niwa-artifact-lease-held workspace "board"))
            (held (gethash directory niwa-artifact-lease--held)))
        (should token)
        ;; As across a suspend: another Emacs may have taken it over.
        (plist-put held :renewed (- (float-time) niwa-session-lease-seconds 1))
        (should-not (niwa-artifact-lease-held workspace "board"))
        (plist-put held :renewed (float-time))
        (should (eq token (niwa-artifact-lease-held workspace "board")))))))

(niwa-deftest niwa-artifact-lease-write ()
  ,test
  (test)
  :doc "writes only while the lease is still this client's"
  (niwa-artifact-lease-test--with-workspace
    (let ((path (file-name-concat root "state.json")))
      (should-error (niwa-artifact-lease-write workspace "board" path "x"))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-write workspace "board" path "one")
      (should (equal "one" (with-temp-buffer (insert-file-contents path) (buffer-string))))
      ;; Another client takes over behind this one's back.
      (let (messages)
        (niwa-test--with-captured-messages messages
          (cl-letf (((symbol-function 'niwa-artifact-lease--now)
                     (lambda (_directory) 1e12)))
            (niwa-artifact-lease-test--as-other
              (niwa-artifact-lease-acquire workspace "board"))))
        (should (string-match-p "took over editing board" messages)))
      (should-error (niwa-artifact-lease-write workspace "board" path "two"))
      (should (equal "one" (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(niwa-deftest niwa-artifact-lease-run ()
  ,test (test)
  :doc "A reentrant heartbeat cannot invalidate the active writer's proof"
  (niwa-artifact-lease-test--with-workspace
    (let* ((directory (niwa-artifact-lease-directory workspace "board"))
           (path (file-name-concat root "state.json"))
           (run (symbol-function 'niwa-session-control-fs-run-program)))
      (niwa-artifact-lease-acquire workspace "board")
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (operations &optional lock)
                   (cl-letf (((symbol-function 'niwa-session-control-fs-run-program) run))
                     (let ((niwa-session-lease-seconds 120))
                       (niwa-artifact-lease--renew directory)))
                   (funcall run operations lock))))
        (should (niwa-artifact-lease-write workspace "board" path "saved")))
      (should (equal "saved" (with-temp-buffer (insert-file-contents path) (buffer-string))))))

  :doc "An expired generation cannot write even before a successor claims it"
  (niwa-artifact-lease-test--with-workspace
    (let ((path (file-name-concat root "state.json")))
      (cl-letf (((symbol-function 'niwa-artifact-lease--now)
                 (lambda (_directory) 1)))
        (niwa-artifact-lease-acquire workspace "board"))
      (should-error (niwa-artifact-lease-run
                     workspace "board" (list (list :op 'write :path path :content "late"))))
      (should-not (file-exists-p path))))

  :doc "A newer generation fences both writes and renewal before its predecessor is pruned"
  (niwa-artifact-lease-test--with-workspace
    (let* ((directory (niwa-artifact-lease-directory workspace "board"))
           (path (file-name-concat root "state.json"))
           (first (niwa-artifact-lease-acquire workspace "board"))
           (next (copy-sequence first)))
      (setq next (plist-put next :generation (1+ (plist-get first :generation))))
      (setq next (plist-put next :client-id niwa-artifact-lease-test--other))
      (niwa-session-durability--write-generation directory next)
      (niwa-test--with-captured-messages nil
        (niwa-artifact-lease--renew directory))
      (should-not (gethash directory niwa-artifact-lease--held))
      ;; Restore stale in-memory ownership to exercise the independent write proof.
      (niwa-artifact-lease--hold workspace "board" directory first)
      (should-error (niwa-artifact-lease-run
                     workspace "board" (list (list :op 'write :path path :content "late"))))
      (should-not (file-exists-p path)))))

(niwa-deftest niwa-artifact-lease--claim
  (:doc "A stale observation cannot replace a renewed generation")
  (niwa-artifact-lease-test--with-workspace
    (let* ((directory (niwa-artifact-lease-directory workspace "board"))
           (first (copy-sequence (niwa-artifact-lease-acquire workspace "board"))))
      (let ((niwa-session-lease-seconds 120))
        (niwa-artifact-lease--renew directory))
      (niwa-artifact-lease-test--as-other
        (should-not (niwa-artifact-lease--claim directory first "board")))
      (should (niwa-artifact-lease-held workspace "board")))))

(niwa-deftest niwa-artifact-lease-release ()
  ,test
  (test)
  :doc "releases on request and after a quiet period, and renews otherwise"
  (niwa-artifact-lease-test--with-workspace
    (let ((directory (niwa-artifact-lease-directory workspace "board")))
      (niwa-artifact-lease-acquire workspace "board")
      (let ((before (plist-get (plist-get (gethash directory niwa-artifact-lease--held)
                                          :record)
                               :expires-at)))
        (niwa-artifact-lease--renew directory)
        (should (>= (plist-get (plist-get (gethash directory niwa-artifact-lease--held)
                                          :record)
                               :expires-at)
                    before)))
      (plist-put (gethash directory niwa-artifact-lease--held) :touched 0)
      (niwa-artifact-lease--renew directory)
      (should-not (gethash directory niwa-artifact-lease--held))
      ;; Released without a reservation: another client edits at once.
      (niwa-artifact-lease-test--as-other
        (niwa-artifact-lease-acquire workspace "board")
        (niwa-artifact-lease-release-all))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-release-all)
      (should-not (gethash directory niwa-artifact-lease--held))
      (niwa-artifact-lease-acquire workspace "board")
      (niwa-artifact-lease-forget-item workspace "board")
      (should-not (file-exists-p directory))))

  :doc "a stale holder cannot report a successful handover or release its successor"
  (niwa-artifact-lease-test--with-workspace
    (let* ((directory (niwa-artifact-lease-directory workspace "board"))
           (first (niwa-artifact-lease-acquire workspace "board"))
           (next (copy-sequence first)))
      (setq next (plist-put next :generation (1+ (plist-get first :generation))))
      (setq next (plist-put next :client-id niwa-artifact-lease-test--other))
      (niwa-session-durability--write-generation directory next)
      (should-not (niwa-artifact-lease-release workspace "board"))
      (should (equal next (niwa-session-durability--lease-head directory)))
      (should-not (gethash directory niwa-artifact-lease--held)))))

(provide 'test-niwa-artifact-lease)
;;; test-niwa-artifact-lease.el ends here
