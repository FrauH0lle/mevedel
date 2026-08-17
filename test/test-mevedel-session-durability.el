;;; test-mevedel-session-durability.el --- Portable session durability -*- lexical-binding: t -*-

;;; Commentary:

;; Covers portable leases, serialized publication, and recovery boundaries.

;;; Code:

(require 'mevedel)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
(require 'mevedel-session-recovery)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication)
(require 'mevedel-execution-target)
(require 'mevedel-hooks)
(require 'mevedel-structs)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-session-durability--remote-session (host local-root)
  "Return a session rooted at LOCAL-ROOT through mock TRAMP HOST."
  (let* ((root (format "/mevedelmock:%s:%s/" host local-root))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "remote")))
    (let ((session (mevedel-session-create "main" workspace)))
      (mevedel-execution-target-seed-incarnation
       (mevedel-session-execution-target session)
       (format "mock-incarnation-%s" host))
      session)))

(defun test-mevedel-session-durability--local-session (root)
  "Return a session rooted at local directory ROOT."
  (let ((workspace
         (mevedel-workspace--create
          :type 'project :id root :root root :name "local")))
    (mevedel-session-create "main" workspace)))

(defun test-mevedel-session-durability--accept-storage (session)
  "Mark SESSION's target storage disclosure accepted for this test."
  (puthash
   (mevedel-execution-target-identity
   (mevedel-session-execution-target session))
   t mevedel-session-durability--disclosed-targets))

(mevedel-deftest mevedel-session-transfer--directory
  (:doc "treats a concurrent creator as success only when it made a directory")
  (let* ((root (make-temp-file "mevedel-transfer-directory-" t))
         (real-make-directory (symbol-function 'make-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'make-directory)
                   (lambda (path &optional parents)
                     (funcall real-make-directory path parents)
                     (signal 'file-already-exists (list path)))))
          (should
           (file-directory-p
            (mevedel-session-transfer--directory
             root "requests"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-transfer--path
  (:doc "names a transfer mailbox without creating it")
  (let ((root (make-temp-file "mevedel-transfer-path-" t)))
    (unwind-protect
        (let ((path (mevedel-session-transfer--path root "requests")))
          (should (equal (file-name-concat root "requests") path))
          ;; A poll must not mutate the target on behalf of an observer, and
          ;; an absent mailbox already reads as "no requests".
          (should-not (file-exists-p path))
          (should-not
           (mevedel-session-control-fs-list-directory path "\\`request-")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-transfer--valid-request-p
  (:doc "rejects hostile labels and non-finite, future, or overlong deadlines")
  (let* ((now (float-time))
         (record
          (list :protocol-version 1
                :request-id (make-string 32 ?a)
                :session-id "session"
                :generation 1
                :owner-client-id (make-string 64 ?b)
                :requester-client-id (make-string 64 ?c)
                :requester-label "Laptop"
                :created-at now
                :deadline (+ now 30))))
    (should (mevedel-session-transfer--valid-request-p record now))
    (dolist (value (list "Laptop\n[g]rant" (make-string 129 ?x)))
      (should-not
       (mevedel-session-transfer--valid-request-p
        (plist-put (copy-sequence record) :requester-label value)
        now)))
    (dolist (value (list -1 (read "0.0e+NaN") (read "1.0e+INF")))
      (should-not
       (mevedel-session-transfer--valid-request-p
        (plist-put (copy-sequence record) :deadline value)
        now)))
    (should-not
     (mevedel-session-transfer--valid-request-p
      (plist-put (copy-sequence record) :deadline (+ now 301))
      now))
    (should-not
     (mevedel-session-transfer--valid-request-p
      (let ((copy (copy-sequence record)))
        (plist-put copy :created-at (+ now 301))
        (plist-put copy :deadline (+ now 302)))
      now))))

(mevedel-deftest mevedel-session-durability-control-transfer ()
  ,test
  (test)
  :doc "atomically requests, decides, fences, and hands off one lease generation"
  (let* ((root (make-temp-file "mevedel-control-transfer-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "control"))
         (owner (mevedel-session-create "main" workspace))
         (session-id "control-session")
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b))
         (other-id (make-string 64 ?c))
         (now 0.0)
         (results nil)
         (mutex (make-mutex "control-transfer-race"))
         threads)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner) session-dir
          (mevedel-session-session-id owner) session-id)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&rest _) now)))
          (let ((mevedel-session-durability--client-id owner-id))
          (should
           (mevedel-session-durability-lease-acquire
            session-dir "owner" owner))
          (let ((mevedel-session-durability--client-id requester-id)
                (mevedel-session-transfer-client-label "Laptop\n[g]rant"))
            (should-error
             (mevedel-session-transfer-request
              owner)))
          (let ((mevedel-session-durability--client-id requester-id)
                (mevedel-session-transfer-prompt-timeout 301))
            (should-error
             (mevedel-session-transfer-request
              owner)))
          (dolist (client (list requester-id other-id))
            (let ((client-id client))
              (push
               (make-thread
                (lambda ()
                  (let ((mevedel-session-durability--client-id client-id))
                    (let ((request
                           (mevedel-session-transfer-request
                            owner)))
                      (with-mutex mutex
                        (push request results))))))
               threads)))
          (mapc #'thread-join threads)
          (should (= 1 (length (delq nil results))))
          (let* ((request (car (delq nil results)))
                 (winner (plist-get request :requester-client-id))
                 (directory (mevedel-session-durability--lease-path
                             session-dir))
                 (decision-path
                  (mevedel-session-transfer--decision-path
                   directory (plist-get request :generation))))
            (should (equal "Emacs" (plist-get request :requester-label)))
            (let ((mevedel-session-durability--client-id winner))
              (should-not
               (mevedel-session-durability-lease-acquire
                session-dir "before-release")))
            (should (eq 'requested
                        (plist-get
                         (mevedel-session-transfer-poll owner)
                         :state)))
            (let ((decision
                   (mevedel-session-transfer-decide
                    owner 'grant))
                  (before nil))
              (setq before
                    (with-temp-buffer
                      (insert-file-contents-literally decision-path)
                      (buffer-string)))
              (should-error
               (mevedel-session-transfer-decide
                owner 'reject))
              (should (equal before
                             (with-temp-buffer
                               (insert-file-contents-literally decision-path)
                               (buffer-string))))
              (should (eq 'grant (plist-get decision :decision))))
            (should (eq 'quiescing
                        (plist-get (mevedel-session-control-transfer owner)
                                   :state)))
            (let ((fence
                   (mevedel-session-transfer-release owner)))
              (should (numberp (plist-get fence :expires-at)))
              (should
               (equal fence
                      (mevedel-session-transfer-release
                       owner)))
              (let ((mevedel-session-durability--client-id
                     (if (equal winner requester-id) other-id requester-id)))
                (should-not
                 (mevedel-session-durability-lease-acquire
                  session-dir "fenced"))))
            (let ((mevedel-session-durability--client-id winner))
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "successor" owner))
              ;; A released generation without a live transfer fence retains
              ;; the ordinary one-client successor path.
              (mevedel-session-durability-lease-release session-dir owner)
              (should (mevedel-session-durability-lease-acquire
                       session-dir "ordinary-successor" owner))
              (let ((mevedel-session-durability--client-id
                     (if (equal winner requester-id)
                         other-id requester-id))
                    (mevedel-session-transfer-prompt-timeout 0.01))
                (should
                 (mevedel-session-transfer-request
                  owner)))
              (setq now 1.0)
              (let ((mevedel-session-transfer-prompt-timeout 0.01))
                (should
                 (eq 'quiescing
                     (plist-get
                      (mevedel-session-transfer-poll owner)
                      :state)))))))))
      (mevedel-session-durability--cancel-renewal owner)
      (when (file-directory-p root)
        (delete-directory root t)))
  :doc "Keep control rotates the generation so a later request can succeed"
  (let* ((root (make-temp-file "mevedel-control-keep-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "keep"))
         (owner (mevedel-session-create "main" workspace))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner) session-dir
          (mevedel-session-session-id owner) "keep-session")
    (unwind-protect
        (let ((mevedel-session-durability--client-id owner-id))
          (should
           (mevedel-session-durability-lease-acquire
            session-dir "owner" owner))
          (let ((mevedel-session-durability--client-id requester-id))
            (should
             (mevedel-session-transfer-request owner)))
          (should
           (eq 'requested
               (plist-get
                (mevedel-session-transfer-poll owner)
                :state)))
          (let ((rejected
                 (mevedel-session-transfer-decide
                  owner 'reject)))
            (should
             (equal rejected
                    (mevedel-session-transfer-decide
                     owner 'reject))))
          (let ((generation
                 (plist-get (mevedel-session-lease owner) :generation)))
            (should (> generation 1))
            (let ((mevedel-session-durability--client-id requester-id))
              (should
               (= generation
                  (plist-get
                   (mevedel-session-transfer-request owner)
                   :generation))))))
      (let ((mevedel-session-durability--client-id owner-id))
        (mevedel-session-durability-lease-release session-dir owner))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "a pending request survives the owner's own generation rotation"
  (let* ((root (make-temp-file "mevedel-control-drift-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "drift"))
         (owner (mevedel-session-create "main" workspace))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b))
         (now 0.0))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner) session-dir
          (mevedel-session-session-id owner) "drift-session")
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&rest _) now)))
          (let ((mevedel-session-durability--client-id owner-id)
                (mevedel-session-transfer-prompt-timeout 1))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "owner" owner))
            (let ((mevedel-session-durability--client-id requester-id))
              (should (mevedel-session-transfer-request owner)))
            ;; Settling the durable mutation latch rotates the owner's
            ;; lease generation past the pending request.
            (should
             (mevedel-session-durability-set-unsettled-mutation owner t))
            (should
             (mevedel-session-durability-set-unsettled-mutation owner nil))
            (should (eq 'requested
                        (plist-get (mevedel-session-transfer-poll owner)
                                   :state)))
            (setq now 2.0)
            (should (eq 'quiescing
                        (plist-get (mevedel-session-transfer-poll owner)
                                   :state)))))
      (mevedel-session-durability--cancel-renewal owner)
      (let ((mevedel-session-durability--client-id owner-id))
        (ignore-errors
          (mevedel-session-durability-lease-release session-dir owner)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "rejects portable lease and transfer APIs for file sessions"
  (let* ((root (make-temp-file "mevedel-file-transfer-" t))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "file"))
         (session (mevedel-session-create "file" workspace))
         (session-dir (file-name-concat root "session")))
    (setf (mevedel-session-save-path session) session-dir
          (mevedel-session-session-id session) "file-session")
    (unwind-protect
        (progn
          (should-error
           (mevedel-session-durability-lease-acquire
            session-dir "file" session))
          (should-error
           (mevedel-session-transfer-request session)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-control-transfer-validation
  (:doc "fails closed for malformed current-generation transfer records")
  (let* ((root (make-temp-file "mevedel-control-transfer-invalid-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "control"))
         (owner (mevedel-session-create "main" workspace))
         (session-id "invalid-session")
         (owner-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner) session-dir
          (mevedel-session-session-id owner) session-id)
    (unwind-protect
        (let ((mevedel-session-durability--client-id owner-id))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "owner" owner))
          (let* ((directory (mevedel-session-durability--lease-path
                             session-dir))
                 (request-path
                  (mevedel-session-transfer--request-path
                   directory 1)))
            (mevedel-session-durability--write-plist
             request-path '(:protocol-version 0))
            (should-error
             (mevedel-session-transfer-poll owner))))
      (mevedel-session-durability--cancel-renewal owner)
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability--claim-next ()
  ,test
  (test)
  :doc "one claim is bounded by three target programs"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-claim-cost-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".lease")))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (programs 0)
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program)))
    (make-directory directory t)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations)
                       (cl-incf programs)
                       (funcall run-program-function operations))))
            (should (mevedel-session-durability--claim-next
                     directory nil "*claim-cost*")))
          ;; Observation, fencing create with predecessor reads, and the
          ;; settling write with its prunes: one program each.
          (should (<= programs 3)))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-durability-lease-status ()
  ,test
  (test)
  :doc "answers state and holding host from one lease observation"
  (let* ((root (make-temp-file "mevedel-lease-status-" t))
         (session-dir (file-name-concat root "session"))
         (session (test-mevedel-session-durability--local-session root))
         (owner (make-string 64 ?a))
         (other (make-string 64 ?b)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should
           (equal '(:state available :host nil)
                  (mevedel-session-durability-lease-status session-dir)))
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "owner" session))
            (should
             (equal (list :state 'owned :host (system-name))
                    (mevedel-session-durability-lease-status session-dir))))
          ;; The client id is per-process and opaque, so the host is the
          ;; only part of a held lease another machine can name.
          (let ((mevedel-session-durability--client-id other))
            (should
             (equal (list :state 'foreign :host (system-name))
                    (mevedel-session-durability-lease-status session-dir))))
          (let ((mevedel-session-durability--client-id owner))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-lease-state ()
  ,test
  (test)
  :doc "classifies available, owned, foreign, and expired portable leases"
  (let* ((root (make-temp-file "mevedel-lease-state-" t))
         (session-dir (file-name-concat root "session"))
         (session (test-mevedel-session-durability--local-session root))
         (owner (make-string 64 ?a))
         (other (make-string 64 ?b))
         ;; The target-authoritative clock has whole-second resolution, so
         ;; expiry must be observed across real target seconds.  The lease
         ;; also has to outlive the control operations of one acquisition.
         (mevedel-session-lease-seconds 2))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should
           (eq 'available
               (mevedel-session-durability-lease-state session-dir)))
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "owner" session))
            (should
             (eq 'owned
                 (mevedel-session-durability-lease-state session-dir))))
          (let ((mevedel-session-durability--client-id other))
            (should
             (eq 'foreign
                 (mevedel-session-durability-lease-state session-dir))))
          (sleep-for 2.1)
          (let ((mevedel-session-durability--client-id other))
            (should
             (eq 'expired
                 (mevedel-session-durability-lease-state session-dir))))
          (let ((mevedel-session-durability--client-id owner))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-target-clock-skew
  (:doc "uses target time when clients have radically skewed wall clocks")
  (let* ((root (make-temp-file "mevedel-target-clock-skew-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (client-clock 0.0)
         (mevedel-session-lease-seconds 10))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) target-now))
                  ((symbol-function 'float-time)
                   (lambda (&rest _) client-clock))
                  ((symbol-function 'current-time)
                   (lambda () (seconds-to-time client-clock)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*owner*")))
          (setq target-now 105.0
                client-clock 1000000.0)
          (let ((mevedel-session-durability--client-id successor))
            (should (eq 'foreign
                        (mevedel-session-durability-lease-state session-dir))))
          (setq target-now 120.0
                client-clock -1000000.0)
          (let ((mevedel-session-durability--client-id successor))
            (should (eq 'expired
                        (mevedel-session-durability-lease-state session-dir)))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*successor*"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability--lease-record
  (:doc "refuses to build a lease record without target-authoritative time")
  (should-error (mevedel-session-durability--lease-record "*buffer*" 1)
                :type 'error))

(mevedel-deftest mevedel-session-durability--lease-head
  (:doc "keeps a live lease visible when a newer generation cannot be read")
  (let* ((root (make-temp-file "mevedel-lease-head-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (mevedel-session-lease-seconds 90))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) target-now)))
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire session-dir "*owner*")))
          ;; A newer generation name that cannot be read must not read as
          ;; "no lease": that would let the next claim pass its
          ;; compare-and-set against a live owner.
          (let* ((directory (mevedel-session-durability--lease-path
                             session-dir))
                 (phantom (file-name-concat
                           directory
                           (format "%020d.el" 6000))))
            (write-region "" nil phantom nil 'silent)
            (should (mevedel-session-durability--lease-head directory))
            (should (equal owner
                           (plist-get
                            (mevedel-session-durability--lease-head directory)
                            :client-id)))
            (let ((mevedel-session-durability--client-id successor))
              (should (eq 'foreign
                          (mevedel-session-durability-lease-state
                           session-dir)))
              (should-not
               (mevedel-session-durability-lease-acquire
                session-dir "*successor*")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-reclaim-expired-lease
  (:doc "reclaims this client's own expired lease but still asks for another's")
  (let* ((root (make-temp-file "mevedel-expired-reclaim-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (prompts 0)
         (mevedel-session-lease-seconds 10))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) target-now))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (setq prompts (1+ prompts)) nil)))
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire session-dir "*owner*"))
            ;; A long blocking target operation can outlast the lease while
            ;; the renewal timer cannot run.
            (setq target-now 200.0)
            (should (eq 'expired
                        (mevedel-session-durability-lease-state session-dir)))
            (should
             (mevedel-session-durability-lease-acquire session-dir "*owner*"))
            (should (= 0 prompts))
            (should (eq 'owned
                        (mevedel-session-durability-lease-state session-dir))))
          (setq target-now 300.0)
          (let ((mevedel-session-durability--client-id successor))
            (should-not
             (mevedel-session-durability-lease-acquire
              session-dir "*successor*"))
            (should (= 1 prompts))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-persistence-lock-acquire/remote ()
  ,test
  (test)
  :doc "remote acquisition writes a portable lease instead of a PID lock"
  (let* ((host "lease-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-lease-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-mevedel-session-durability--remote-session
                  host local-root)))
            (test-mevedel-session-durability--accept-storage session)
            (setf (mevedel-session-save-path session) session-dir)
            (should
             (mevedel-session-persistence-lock-acquire
              session-dir "*remote*" session))
            (should (file-directory-p
                     (file-name-concat session-dir ".lease")))
            (should-not (file-exists-p
                         (file-name-concat session-dir ".lock")))
            (let ((lease (mevedel-session-lease session)))
              (should (equal (make-string 64 ?a)
                             (plist-get lease :client-id)))
              (should (numberp (plist-get lease :expires-at)))
              (should (natnump (plist-get lease :generation)))
              (should (plist-member lease :publication-head))
              (should-not (plist-get lease :publication-head))
              (should (plist-member lease :unsettled-mutation))
              (should-not (plist-get lease :unsettled-mutation))
              (should-not (plist-member lease :pid)))
            (should (eq 'owned
                        (plist-get (mevedel-session-lease session) :state)))
            (should (timerp (mevedel-session-lease-renewal-timer session)))
            (mevedel-session-persistence-lock-release session-dir session)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "expired remote takeover requires confirmation and changes owner"
  (let* ((host "takeover-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-takeover-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (mevedel-session-durability--client-id (make-string 64 ?b))
         (session (mevedel-session--create :authority-mode 'portable))
         (confirmed nil)
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t)))
            (let ((mevedel-session-durability--client-id (make-string 64 ?a)))
              (should
               (mevedel-session-persistence-lock-acquire
                session-dir "*expired*" session)))
            (setq now 100.0)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq confirmed t)
                         t)))
              (should
               (mevedel-session-persistence-lock-acquire
                session-dir "*new*" session)))
            (should confirmed)
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*new*" session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "simultaneous expired takeovers elect exactly one owner"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-takeover-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner (make-string 64 ?a))
         (now 0.0)
         (claim-mutex (make-mutex "mevedel lease claims"))
         (claim-condition (make-condition-variable claim-mutex))
         (claim-count 0)
         (claim-generations nil)
         (result-mutex (make-mutex "mevedel lease results"))
         (results nil)
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program))
         threads)
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now)))
          (let ((mevedel-session-durability--client-id owner))
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*expired*")))
          (setq now 100.0)
          (cl-letf
              (((symbol-function 'y-or-n-p)
                (lambda (&rest _) t))
               ;; A claim states its fence as the exclusive `create'
               ;; opening its program; both contenders are held at that
               ;; exact point so the races collide for real.
               ((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (operations)
                  (when (eq 'create (plist-get (car operations) :op))
                    (with-mutex claim-mutex
                      (cl-incf claim-count)
                      (push (mevedel-session-durability--generation
                             (plist-get (car operations) :path))
                            claim-generations)
                      (condition-notify claim-condition t)
                      (while (< claim-count 2)
                        (condition-wait claim-condition))))
                  (funcall run-program-function operations))))
            (dolist (client (list (make-string 64 ?b) (make-string 64 ?c)))
              (push
               (make-thread
                (lambda ()
                  (let ((mevedel-session-durability--client-id client))
                    (let ((won
                           (mevedel-session-durability-lease-acquire
                            session-dir "*contender*")))
                      (with-mutex result-mutex
                        (push won results))))))
               threads))
            (mapc #'thread-join threads))
          (should (equal '(2 2) claim-generations))
          (should (= 1 (cl-count t results))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a newer claiming head invalidates acquisition with or without a session"
  (dolist (bind-session '(nil t))
    (let* ((local-root (file-name-as-directory
                        (make-temp-file "mevedel-claiming-head-" t)))
           (session-dir (file-name-as-directory
                         (file-name-concat local-root "session")))
           (session (test-mevedel-session-durability--local-session
                     local-root))
           (owner (make-string 64 ?a))
           (contender (make-string 64 ?b))
           (blocker (make-string 64 ?c))
           (now 0.0))
      (make-directory session-dir t)
      (setf (mevedel-session-save-path session) session-dir)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t)))
            (let ((mevedel-session-durability--client-id owner))
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*expired*")))
            (setq now 100.0)
            (let ((mevedel-session-durability--client-id contender))
              (let ((mevedel-session-durability--client-id blocker))
                (should
                 (mevedel-session-durability--create-generation
                  (file-name-concat session-dir ".lease")
                  (mevedel-session-durability--lease-record
                   "*paused-claim*" 2 'claiming nil nil
                   (mevedel-session-durability--target-time
                    (file-name-concat session-dir ".lease"))))))
              (should-not
               (mevedel-session-durability-lease-acquire
                session-dir "*contender*" (and bind-session session))))
            (when bind-session
              (should
               (eq 'foreign
                   (plist-get (mevedel-session-lease session) :state)))))
        (mevedel-session-durability--cancel-renewal session)
        (when (file-directory-p local-root)
          (delete-directory local-root t)))))

  :doc "an expired interrupted claim still requires confirmed takeover"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-stale-claim-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (mevedel-session-lease-seconds 10)
         (now 0.0)
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program))
         (fail-finalization t)
         (confirmed nil))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now))
                  ;; A claim finalizes through the program whose first
                  ;; operation is the settle write; the fencing create and
                  ;; the lease commits open with other verbs.
                  ((symbol-function 'mevedel-session-control-fs-run-program)
                   (lambda (operations)
                     (if (and fail-finalization
                              (eq 'write (plist-get (car operations) :op)))
                         (progn
                           (setq fail-finalization nil)
                           (error "Injected claim finalization crash"))
                       (funcall run-program-function operations)))))
          (should-error
           (mevedel-session-durability-lease-acquire
            session-dir "*interrupted*")
           :type 'error)
          (setq now 20.0)
          (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq confirmed t)
                         t)))
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*successor*")))
            (should confirmed)
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*successor*"))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-set-unsettled-mutation ()
  ,test
  (test)
  :doc "lease transitions and takeover preserve the unsettled mutation latch"
  (let* ((host "unsettled-mutation-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-unsettled-mutation-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session
          (test-mevedel-session-durability--remote-session host local-root))
         first-generation)
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (let ((mevedel-session-durability--client-id (make-string 64 ?a)))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*first-owner*" session))
            (setq first-generation
                  (plist-get (mevedel-session-lease session) :generation))
            (should
             (mevedel-session-durability-set-unsettled-mutation session t))
            (should (mevedel-session-durability-lease-renew session))
            (should
             (mevedel-session-durability-call-with-reserved-lease
              session (lambda () t)))
            (should
             (mevedel-session-durability-unsettled-mutation-p session))
            (mevedel-session-durability-lease-release session-dir session))
          (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*successor*" session))
            (should (> (plist-get (mevedel-session-lease session) :generation)
                       first-generation))
            (should
             (mevedel-session-durability-unsettled-mutation-p session))
            (should
             (mevedel-session-durability-set-unsettled-mutation session nil))
            (mevedel-session-durability-lease-release session-dir session))
          (let ((mevedel-session-durability--client-id (make-string 64 ?c)))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*observer*" session))
            (should-not
             (mevedel-session-durability-unsettled-mutation-p session))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale renewal cannot overwrite a reentrant mutation arm"
  (let* ((host "unsettled-renew-race-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-unsettled-renew-race-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session
          (test-mevedel-session-durability--remote-session host local-root))
         (client-id (make-string 64 ?a))
         run-program-function
         injected
         initial-generation)
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (let ((mevedel-session-durability--client-id client-id))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*owner*" session))
            (setq initial-generation
                  (plist-get (mevedel-session-lease session) :generation)
                  run-program-function
                  (symbol-function 'mevedel-session-control-fs-run-program))
            ;; The renewal's settling write is the commit program opened
            ;; by its verify; arming the latch immediately before it runs
            ;; is the reentrant race this case pins.
            (cl-letf
                (((symbol-function 'mevedel-session-control-fs-run-program)
                  (lambda (operations)
                    (when (and (not injected)
                               (eq 'verify (plist-get (car operations) :op)))
                      (setq injected t)
                      (should
                       (mevedel-session-durability-set-unsettled-mutation
                        session t)))
                    (funcall run-program-function operations))))
              (mevedel-session-durability-lease-renew session))
            (should injected)
            (should
             (mevedel-session-durability-unsettled-mutation-p session))
            (should
             (> (plist-get (mevedel-session-lease session) :generation)
                initial-generation))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-request-begin/remote-lease
  (:after-each (setq mevedel--current-request nil))
  ,test
  (test)
  :doc "a foreign unexpired lease blocks request admission"
  (let* ((host "foreign-lease-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-foreign-lease-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (mevedel-session-durability--client-id (make-string 64 ?b)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let* ((session
                  (test-mevedel-session-durability--remote-session
                   host local-root))
                 (target (mevedel-session-execution-target session)))
            (test-mevedel-session-durability--accept-storage session)
            (setf (mevedel-session-save-path session) session-dir
                  (mevedel-execution-target-readiness target)
                  '(:status ready))
            (let ((mevedel-session-durability--client-id (make-string 64 ?a)))
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*owner*")))
            (should-not
             (mevedel-session-persistence-lock-acquire
              session-dir "*inspector*" session))
            (should-error (mevedel-request-begin session)
                          :type 'user-error)
            (should-not mevedel--current-request)))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-lease-renew ()
  ,test
  (test)
  :doc "heartbeat renewal extends an owned portable lease without PID liveness"
  (let* ((host "renew-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-renew-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now)))
            (should (mevedel-session-persistence-lock-acquire
                     session-dir "*renew*" session))
            (let ((old-expiry
                   (plist-get (mevedel-session-lease session) :expires-at)))
              (setq now 30.0)
              (should (mevedel-session-durability-lease-renew session))
              (should (> (plist-get (mevedel-session-lease session)
                                    :expires-at)
                         old-expiry))))
          (mevedel-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a renewal under a bound transaction clock assumes instead of observing"
  (let* ((host "renew-assume-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-renew-assume-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?c))
         (mevedel-session-durability--transaction-clock (list nil))
         (mevedel-session-durability--asserted-directories (list nil))
         (programs nil)
         (program-function
          (symbol-function 'mevedel-session-control-fs-run-program)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*assume*" session))
          ;; The first renewal may still observe; its commit's trailing
          ;; clock operation is what arms the assumption for the next one.
          (should (mevedel-session-durability-lease-renew session))
          (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                     (lambda (operations)
                       (push (mapcar (lambda (op) (plist-get op :op))
                                     operations)
                             programs)
                       (funcall program-function operations))))
            (should (mevedel-session-durability-lease-renew session)))
          (should (equal '((verify write list-directory target-time))
                         programs))
          (mevedel-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale renewal cannot overwrite a newer takeover"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-renew-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--remote-session
                   "renew-race-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         ;; Pin the lease window this timeline assumes instead of tracking
         ;; the shipped default.
         (mevedel-session-lease-seconds 90)
         (now 0.0)
         (triggered nil)
         (program-function
          (symbol-function 'mevedel-session-control-fs-run-program)))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now)))
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*owner*" session))
            (setq now 80.0)
            (cl-letf
                (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ;; A renewal states its precondition as the `verify' opening
                 ;; its commit program.  Letting a successor take over
                 ;; immediately before that program runs is the last moment a
                 ;; stale renewal could still overwrite the newer owner.
                 ((symbol-function 'mevedel-session-control-fs-run-program)
                  (lambda (operations)
                    (when (and (not triggered)
                               (eq 'verify (plist-get (car operations) :op)))
                      (setq triggered t
                            now 100.0)
                      (let ((mevedel-session-durability--client-id successor))
                        (should
                         (mevedel-session-durability-lease-acquire
                          session-dir "*successor*"))))
                    (funcall program-function operations))))
              (should-not
               (mevedel-session-durability-lease-renew session))
              (should triggered))
            (let ((mevedel-session-durability--client-id successor))
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*successor*"))
              (mevedel-session-durability-lease-release session-dir))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a completed renewal makes an earlier stale takeover abort"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-renew-before-claim-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--remote-session
                   "renew-before-claim-host" local-root))
         (owner (make-string 64 ?a))
         (contender (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         ;; Pin the lease window this timeline assumes instead of tracking
         ;; the shipped default.
         (mevedel-session-lease-seconds 90)
         (initial t)
         (mutex (make-mutex "mevedel renew-before-claim"))
         (condition (make-condition-variable mutex))
         (renew-ready nil)
         (renew-allowed nil)
         (renew-done nil)
         (renew-result nil)
         (program-function
          (symbol-function 'mevedel-session-control-fs-run-program))
         renew-thread)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-durability--target-time)
              (lambda (&optional _)
                (cond
                 (initial 0.0)
                 ((equal mevedel-session-durability--client-id owner) 80.0)
                 (t 100.0))))
             ;; The renewal is held immediately before its commit program,
             ;; which is the operation that proves its precondition and writes
             ;; the new record.  That is the moment the contender must find
             ;; still outstanding.
             ((symbol-function 'mevedel-session-control-fs-run-program)
              (lambda (operations)
                (when (and (equal (thread-name (current-thread))
                                  "mevedel-owner-renew")
                           (eq 'verify (plist-get (car operations) :op))
                           (not renew-ready))
                  (with-mutex mutex
                    (setq renew-ready t)
                    (condition-notify condition t)
                    (while (not renew-allowed)
                      (condition-wait condition))))
                (funcall program-function operations))))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (setq initial nil
                renew-thread
                (make-thread
                 (lambda ()
                   (let ((mevedel-session-durability--client-id owner))
                     (setq renew-result
                           (mevedel-session-durability-lease-renew session))
                     (with-mutex mutex
                       (setq renew-done t)
                       (condition-notify condition t))))
                 "mevedel-owner-renew"))
          (with-mutex mutex
            (while (not renew-ready)
              (condition-wait condition)))
          (let ((mevedel-session-durability--client-id contender))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (with-mutex mutex
                           (setq renew-allowed t)
                           (condition-notify condition t)
                           (while (not renew-done)
                             (condition-wait condition)))
                         t)))
              (should-not
               (mevedel-session-durability-lease-acquire
                session-dir "*contender*"))))
          (thread-join renew-thread)
          (should renew-result)
          (let ((mevedel-session-durability--client-id owner))
            (should (mevedel-session-durability-lease-renew session))))
      (mevedel-session-durability--cancel-renewal session)
      (when (and renew-thread (thread-live-p renew-thread))
        (thread-signal renew-thread 'quit nil))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "renewal bypasses a stale remote generation listing"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-stale-lease-cache-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (lease-directory (file-name-concat session-dir ".lease"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (session (test-mevedel-session-durability--local-session local-root))
         (program-function
          (symbol-function 'mevedel-session-control-fs-run-program))
         (helper-called nil))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (let ((mevedel-session-durability--client-id successor))
            (should
             (mevedel-session-durability--create-generation
              lease-directory
              (mevedel-session-durability--lease-record
               "*successor*" 2 'active nil nil
               (mevedel-session-durability--target-time lease-directory)))))
          ;; The renewal observes the generation listing inside its own
          ;; target program, so that is where consulting it is proved.
          (cl-letf
              (((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (operations)
                  (when (cl-find-if
                         (lambda (operation)
                           (and (eq 'list-directory (plist-get operation :op))
                                (equal (mevedel-session-control-fs-physical-path
                                        (plist-get operation :path))
                                       (mevedel-session-control-fs-physical-path
                                        lease-directory))))
                         operations)
                    (setq helper-called t))
                  (funcall program-function operations))))
            (should-not
             (mevedel-session-durability-lease-renew session)))
          (should helper-called))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "renewal recovers an owned publishing generation after serialization"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publishing-renew-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-durability--renew-publication-lease session))
          (should (eq 'publishing
                      (plist-get (mevedel-session-lease session) :status)))
          (should (mevedel-session-durability-lease-renew session))
          (should (eq 'active
                      (plist-get (mevedel-session-lease session) :status))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-lease-release ()
  ,test
  (test)
  :doc "release settles the current same-client successor generation"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-release-successor-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (let* ((directory (file-name-concat session-dir ".lease"))
                 (first (mevedel-session-durability--lease-head directory))
                 (successor
                  (mevedel-session-durability--claim-next
                   directory first "*owner*")))
            (should successor)
            (should (= 1 (plist-get (mevedel-session-lease session)
                                    :generation)))
            (should (= 2 (plist-get successor :generation)))
            (mevedel-session-durability-lease-release session-dir session)
            (should
             (eq 'released
                 (plist-get
                  (mevedel-session-durability--lease-head directory)
                  :status)))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale release cannot delete a newer takeover"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-release-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session (test-mevedel-session-durability--remote-session
                         "release-race-host" local-root))
         (successor-session
          (test-mevedel-session-durability--remote-session
           "release-race-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (now 0.0)
         (triggered nil)
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (setq now 80.0)
          (cl-labels
              ((trigger-successor
                ()
                (unless triggered
                  (setq triggered t
                        now 100.0)
                  (let ((mevedel-session-durability--client-id successor))
                    (cl-letf (((symbol-function 'y-or-n-p)
                               (lambda (&rest _) t)))
                      (should
                       (mevedel-session-durability-lease-acquire
                        session-dir "*successor*" successor-session)))))))
            ;; The release's first mutating program -- its fencing create
            ;; or settling write -- is the last moment a takeover can land
            ;; ahead of it; the observation before it must not trigger.
            (cl-letf
                (((symbol-function 'mevedel-session-control-fs-run-program)
                  (lambda (operations)
                    (when (and (not triggered)
                               (memq (plist-get (car operations) :op)
                                     '(create write verify)))
                      (trigger-successor))
                    (funcall run-program-function operations))))
              (mevedel-session-durability-lease-release
               session-dir owner-session))
            (should triggered))
          (let ((mevedel-session-durability--client-id successor))
            (should
             (mevedel-session-durability-lease-renew successor-session))
            (mevedel-session-durability-lease-release
             session-dir successor-session)))
      (mevedel-session-durability--cancel-renewal owner-session)
      (mevedel-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "release settles an inactive publisher without interrupting a live one"
  (dolist (publication-active '(nil t))
    (let* ((local-root (file-name-as-directory
                        (make-temp-file "mevedel-publishing-release-" t)))
           (session-dir (file-name-as-directory
                         (file-name-concat local-root "session")))
           (session (test-mevedel-session-durability--local-session local-root))
           (mevedel-session-durability--client-id (make-string 64 ?a)))
      (make-directory session-dir t)
      (setf (mevedel-session-save-path session) session-dir)
      (unwind-protect
          (progn
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*publisher*" session))
            (should
             (mevedel-session-durability--renew-publication-lease session))
            (setf (mevedel-session-publication-active-p session)
                  publication-active)
            (mevedel-session-durability-lease-release session-dir session)
            (if publication-active
                (progn
                  (should (plist-get (mevedel-session-lease session)
                                     :release-pending))
                  (should (timerp
                           (mevedel-session-lease-renewal-timer session))))
              (should-not (mevedel-session-lease session)))
            (should
             (eq (if publication-active 'publishing 'released)
                 (plist-get
                  (mevedel-session-durability--lease-head
                   (file-name-concat session-dir ".lease"))
                  :status))))
        (mevedel-session-durability--cancel-renewal session)
        (when (file-directory-p local-root)
          (delete-directory local-root t)))))

  :doc "serialization completes a release requested by its live publisher"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-deferred-release-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-durability-call-with-reserved-lease
            session
            (lambda ()
              (mevedel-session-durability-lease-release
               session-dir session)
              t)))
          (should-not (mevedel-session-lease session))
          (should-not (mevedel-session-lease-renewal-timer session))
          (should
           (eq 'released
               (plist-get
                (mevedel-session-durability--lease-head
                 (file-name-concat session-dir ".lease"))
                :status))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "release retains failed critical publication until explicit abandonment"
  (let* ((host "release-pending-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-release-pending-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/mevedelmock:%s:%s/" host session-dir-local))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (target (file-name-concat session-dir "blocker" "state.el"))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((session (test-mevedel-session-durability--remote-session
                          host local-root)))
            (test-mevedel-session-durability--accept-storage session)
            (setf (mevedel-session-save-path session) session-dir)
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*publisher*" session))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-publish
                session (list (list :path target :content "critical")))
               :type 'file-error))
            (let* ((pending (mevedel-session-pending-publication session))
                   (batch (car (plist-get pending :batches)))
                   (recovery (plist-get batch :directory)))
              (should (file-directory-p recovery))
              (mevedel-test--with-captured-diagnostics nil
                (mevedel-session-durability-lease-release
                 session-dir session))
              (should (eq pending
                          (mevedel-session-pending-publication session)))
              (should (file-directory-p recovery))
              (dolist (retained (plist-get pending :batches))
                (mevedel-session-publication--delete-batch retained)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "deferred release retains a reentrantly queued critical publication"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-release-queued-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (mevedel-test--with-captured-diagnostics nil
            (should
             (mevedel-session-durability-call-with-reserved-lease
              session
              (lambda ()
                (should
                 (eq 'queued
                     (mevedel-session-publication-publish
                      session
                      (list (list :path target :content "critical")))))
                (mevedel-session-durability-lease-release
                 session-dir session)
                t))))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))
            (mevedel-session-publication--delete-batch batch)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-forget-removed-session
  (:doc "clears lease, renewal, publication, and recovery without target I/O")
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-forget-removed-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (recovery (file-name-as-directory
                    (make-temp-file "mevedel-forget-recovery-" t)))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*removed*" session))
          (should (timerp
                   (mevedel-session-lease-renewal-timer session)))
          (setf (mevedel-session-publication session) '(:head "published")
                (mevedel-session-publication-active-p session) t
                (mevedel-session-pending-publication session)
                (list :batches
                      (list (list :directory recovery :artifacts nil))))
          (delete-directory session-dir t)
          (mevedel-session-durability-forget-removed-session session)
          (should-not (file-exists-p session-dir))
          (should-not (file-directory-p recovery))
          (should-not (mevedel-session-lease session))
          (should-not (mevedel-session-lease-renewal-timer session))
          (should-not (mevedel-session-publication session))
          (should-not (mevedel-session-publication-active-p session))
          (should-not (mevedel-session-pending-publication session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-call-with-reserved-lease ()
  ,test
  (test)
  :doc "suppresses timer target I/O and verifies ownership after reserved work"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-reserved-lease-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (rename-file-function (symbol-function 'rename-file))
         (renames 0))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (file newname &optional ok-if-exists)
                       (cl-incf renames)
                       (funcall rename-file-function
                                file newname ok-if-exists))))
            (should
             (eq 'reserved
                 (mevedel-session-durability-call-with-reserved-lease
                  session
                  (lambda ()
                    (let ((before renames))
                      (should
                       (mevedel-session-durability-lease-renew session))
                      (should (= before renames)))
                    'reserved)))))
          (should-not (mevedel-session-publication-active-p session))
          (should (eq 'active
                      (plist-get (mevedel-session-lease session) :status))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "signals and marks authority lost when ownership changes during work"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-reserved-loss-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-mevedel-session-durability--local-session local-root))
         (successor-session
          (test-mevedel-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (mevedel-session-publication-lease-seconds 100)
         (now 0.0))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (should-error
           (mevedel-session-durability-call-with-reserved-lease
            owner-session
            (lambda ()
              (setq now 101.0)
              (let ((mevedel-session-durability--client-id successor))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) t)))
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*successor*" successor-session))))))
           :type 'user-error)
          (should (eq 'lost
                      (plist-get (mevedel-session-lease owner-session)
                                 :state))))
      (mevedel-session-durability--cancel-renewal owner-session)
      (let ((mevedel-session-durability--client-id successor))
        (mevedel-session-durability-lease-release
         session-dir successor-session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "drains a deferred release when reserved work exits nonlocally"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-reserved-throw-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (should
           (eq 'out
               (catch 'nonlocal
                 (mevedel-session-durability-call-with-reserved-lease
                  session
                  (lambda ()
                    (mevedel-session-durability-lease-release
                     session-dir session)
                    (should (plist-get (mevedel-session-lease session)
                                       :release-pending))
                    (throw 'nonlocal 'out))))))
          (should-not (mevedel-session-lease-renewal-timer session))
          (should
           (eq 'released
               (plist-get
                (mevedel-session-durability--lease-head
                 (file-name-concat session-dir ".lease"))
                :status))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-commit-publication-head ()
  ,test
  (test)
  :doc "validates and preserves a publication head through lease transitions"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-head-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-mevedel-session-durability--local-session local-root))
         (successor-session
          (test-mevedel-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (head ".publications/0001/manifest.el"))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (dolist (invalid
                   '(nil "" "/absolute" "../manifest" "a/../manifest"
                     "/ssh:host:/manifest" "~/manifest" "a//manifest"
                     "a/./manifest" "manifest/" "session.meta.el"
                     ".lease/manifest.el" ".publications/plain.el"))
            (should-error
             (mevedel-session-durability-commit-publication-head
              owner-session invalid)
             :type 'error))
          (should
           (mevedel-session-durability--renew-publication-lease
            owner-session))
          (should
           (mevedel-session-durability-commit-publication-head
            owner-session head))
          (should (equal head
                         (plist-get (mevedel-session-lease owner-session)
                                    :publication-head)))
          (should (mevedel-session-durability-lease-renew owner-session))
          (should (equal head
                         (plist-get (mevedel-session-lease owner-session)
                                    :publication-head)))
          (mevedel-session-durability-lease-release
           session-dir owner-session)
          (should (equal head
                         (mevedel-session-durability-publication-head
                          session-dir)))
          (let ((mevedel-session-durability--client-id successor))
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*successor*" successor-session))
            (should (equal head
                           (plist-get
                            (mevedel-session-lease successor-session)
                            :publication-head)))
            (mevedel-session-durability-lease-release
             session-dir successor-session)))
      (mevedel-session-durability--cancel-renewal owner-session)
      (mevedel-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale commit cannot replace a newer generation's inherited head"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-head-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-mevedel-session-durability--local-session local-root))
         (successor-session
          (test-mevedel-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (mevedel-session-publication-lease-seconds 100)
         (now 0.0)
         (triggered nil)
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (should
           (mevedel-session-durability--renew-publication-lease
            owner-session))
          (should
           (mevedel-session-durability-commit-publication-head
            owner-session ".publications/old/manifest.el"))
          (setq now 80.0)
          ;; The stale commit states its precondition as the `verify'
          ;; opening its program; a successor takeover immediately before
          ;; that program runs is the last moment the race can happen.
          (cl-letf
              (((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (operations)
                  (when (and (not triggered)
                             (eq 'verify (plist-get (car operations) :op)))
                    (setq triggered t
                          now 101.0)
                    (let ((mevedel-session-durability--client-id successor))
                      (cl-letf (((symbol-function 'y-or-n-p)
                                 (lambda (&rest _) t)))
                        (should
                         (mevedel-session-durability-lease-acquire
                          session-dir "*successor*" successor-session)))))
                  (funcall run-program-function operations))))
            (should-not
             (mevedel-session-durability-commit-publication-head
              owner-session ".publications/stale/manifest.el")))
          (should triggered)
          (should
           (equal ".publications/old/manifest.el"
                  (plist-get (mevedel-session-lease successor-session)
                             :publication-head)))
          (let ((mevedel-session-durability--client-id successor))
            (should
             (mevedel-session-durability--renew-publication-lease
              successor-session))
            (should
             (mevedel-session-durability-commit-publication-head
              successor-session ".publications/new/manifest.el"))
            (should
             (equal ".publications/new/manifest.el"
                    (mevedel-session-durability-publication-head session-dir)))
            (mevedel-session-durability-lease-release
             session-dir successor-session)))
      (mevedel-session-durability--cancel-renewal owner-session)
      (mevedel-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-publication-head ()
  ,test
  (test)
  :doc "reads the uncached current head without creating or acquiring a lease"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-head-read-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (lease-directory (file-name-concat session-dir ".lease"))
         (list-directory-function
          (symbol-function 'mevedel-session-control-fs-list-directory))
         (helper-called nil))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (should-not
           (mevedel-session-durability-publication-head session-dir))
          (should-not (file-exists-p lease-directory))
          (make-directory lease-directory)
          (let ((mevedel-session-durability--client-id (make-string 64 ?a)))
            (should
             (mevedel-session-durability--create-generation
              lease-directory
              (mevedel-session-durability--lease-record
               "*old*" 1 'active ".publications/old/manifest.el" nil
               (mevedel-session-durability--target-time lease-directory)))))
          (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
            (should
             (mevedel-session-durability--create-generation
              lease-directory
              (mevedel-session-durability--lease-record
               "*new*" 2 'active ".publications/new/manifest.el" nil
               (mevedel-session-durability--target-time lease-directory)))))
          (cl-letf
              (((symbol-function 'mevedel-session-control-fs-list-directory)
                (lambda (directory regexp)
                  (if (not (equal directory lease-directory))
                      (funcall list-directory-function directory regexp)
                    (setq helper-called t)
                    (funcall list-directory-function directory regexp)))))
            (should
             (equal ".publications/new/manifest.el"
                    (mevedel-session-durability-publication-head
                     session-dir))))
          (should helper-called))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication--publish-artifact ()
  ,test
  (test)
  :doc "builds a missing parent chain instead of failing the artifact"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-chain-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         ;; Two missing levels below the session directory: the
         ;; single-level optional ensure alone cannot create them.
         (artifact (file-name-concat session-dir "tool-results"
                                     "executions" "exec-000001.log"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should (mevedel-session-publication-publish
                   session
                   (list (list :path artifact :content "execution log"))))
          (should (equal "execution log"
                         (with-temp-buffer
                           (insert-file-contents artifact)
                           (buffer-string))))
          (mevedel-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication-read ()
  ,test
  (test)
  :doc "validates manifest and sidecar without downloading every artifact"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-read-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should-not
           (mevedel-session-publication-read session-dir))
          (should-not (file-exists-p (file-name-concat session-dir ".lease")))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (mevedel-session-publication-read session-dir))
                 (segment-entry
                  (cdr (assoc "segment.chat.org"
                              (plist-get publication :artifacts))))
                 (sidecar-path (plist-get publication :sidecar))
                 (manifest-path
                  (file-name-concat session-dir
                                    (plist-get publication :head))))
            (with-temp-file (plist-get segment-entry :published)
              (insert "corrupt but unselected"))
            (should
             (equal publication
                    (mevedel-session-publication-read session-dir)))
            (with-temp-file sidecar-path (insert "corrupt sidecar"))
            (should-error
             (mevedel-session-publication-read session-dir)
             :type 'error)
            (with-temp-file manifest-path
              (prin1
               (list :sidecar "session.meta.el"
                     :artifacts
                     (list
                      (list "session.meta.el"
                            :published ".lease/forged.el"
                            :sha256 (make-string 64 ?a))))
               (current-buffer)))
            (should-error
             (mevedel-session-publication-read session-dir)
             :type 'error)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a publication head symlink that escapes immutable storage"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-head-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (outside-dir (file-name-as-directory
                       (file-name-concat local-root "outside")))
         (session (test-mevedel-session-durability--local-session local-root))
         (escaped-head ".publications/escaped-head/manifest.el")
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (make-directory outside-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (let* ((publication
                  (mevedel-session-publication-publish
                   session
                   (list (list :path segment :content "segment")
                         (list :path sidecar :content "sidecar"
                               :commit-marker t))))
                 (manifest
                  (file-name-concat session-dir
                                    (plist-get publication :head))))
            (copy-file manifest
                       (file-name-concat outside-dir "manifest.el"))
            (make-symbolic-link
             outside-dir
             (file-name-concat session-dir ".publications" "escaped-head"))
            (should
             (mevedel-session-durability--renew-publication-lease session))
            (should
             (mevedel-session-durability-commit-publication-head
              session escaped-head))
            (should
             (mevedel-session-durability--finish-publication-lease session))
            (should-error
             (mevedel-session-publication-read session-dir)
             :type 'error)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a manifest artifact symlink that escapes immutable storage"
  (let* ((local-root
          (file-name-as-directory
           (make-temp-file "mevedel-publication-artifact-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (outside-dir (file-name-as-directory
                       (file-name-concat local-root "outside")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (make-directory outside-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (let* ((publication
                  (mevedel-session-publication-publish
                   session
                   (list (list :path segment :content "segment")
                         (list :path sidecar :content "sidecar"
                               :commit-marker t))))
                 (manifest-path
                  (file-name-concat session-dir
                                    (plist-get publication :head)))
                 (manifest
                  (mevedel-session-durability--read-plist manifest-path))
                 (sidecar-entry
                  (cdr (assoc "session.meta.el"
                              (plist-get manifest :artifacts)))))
            (copy-file
             (file-name-concat
              session-dir (plist-get sidecar-entry :published))
             (file-name-concat outside-dir "session.meta.el"))
            (make-symbolic-link
             outside-dir
             (file-name-concat
              session-dir ".publications" "escaped-artifact"))
            (setf (plist-get sidecar-entry :published)
                  ".publications/escaped-artifact/session.meta.el")
            (with-temp-file manifest-path
              (let ((print-length nil)
                    (print-level nil))
                (prin1 manifest (current-buffer))))
            (should-error
             (mevedel-session-publication-read session-dir)
             :type 'error)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a publication root symlink outside the physical session"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-root-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (publication-dir (file-name-concat session-dir ".publications"))
         (outside-dir (file-name-concat local-root "outside-publications"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (rename-file publication-dir outside-dir)
          (make-symbolic-link outside-dir publication-dir)
          (should-error
           (mevedel-session-publication-read session-dir)
           :type 'error))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication-logical-path-p ()
  ,test
  (test)
  :doc "accepts normalized session artifacts outside durability control paths"
  (dolist (path '("session.meta.el" "agents/worker.chat.org"
                  "instructions/current.el"))
    (should (mevedel-session-publication-logical-path-p path)))
  (dolist (path '(nil "" "/absolute" "../escape" "a/../escape"
                  ".lock" ".lease/0001.el" ".publications/generation/file"
                  ".recovery/recovery-0001/bytes"
                  "/ssh:other:/session.meta.el" "~/session.meta.el"))
    (should-not (mevedel-session-publication-logical-path-p path))))

(mevedel-deftest mevedel-session-publication-uncommitted-artifact ()
  ,test
  (test)
  :doc "returns the newest retained or queued local source only"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-uncommitted-artifact-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (external (file-name-concat local-root "external.el"))
         (logical "agents/worker.chat.org")
         (target (file-name-concat session-dir logical))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path target :content "retained"
                        :coding 'utf-8-unix :opaque 'kept)
                  (list :path external :content "external"))))
          (let ((artifact
                 (car
                  (plist-get
                   (car
                    (mevedel-session-publication-uncommitted-batches session))
                   :artifacts))))
            (should-not (plist-member artifact :content))
            (should (eq 'utf-8-unix (plist-get artifact :coding)))
            (should (eq 'kept (plist-get artifact :opaque))))
          (let ((source
                 (mevedel-session-publication-uncommitted-artifact
                  session logical)))
            (should (file-name-absolute-p source))
            (should-not (file-remote-p source))
            (should (equal "retained"
                           (with-temp-buffer
                             (insert-file-contents-literally source)
                             (buffer-string)))))
          (setf (mevedel-session-publication-active-p session) t)
          (should
           (eq 'queued
               (mevedel-session-publication-publish
                session (list (list :path target :content "queued")))))
          (should
           (equal "queued"
                  (with-temp-buffer
                    (insert-file-contents-literally
                     (mevedel-session-publication-uncommitted-artifact
                      session logical))
                    (buffer-string))))
          (should-not
           (mevedel-session-publication-uncommitted-artifact
            session "missing.el"))
          (should-not
           (mevedel-session-publication-uncommitted-artifact
            session "external.el"))
          (should-error
           (mevedel-session-publication-uncommitted-artifact
            session "../external.el")
           :type 'error))
      (setf (mevedel-session-publication-active-p session) nil)
      ;; The queued publication is deliberately never drained; releasing
      ;; over it reports the retained recovery, which this teardown owns.
      (mevedel-test--with-captured-diagnostics nil
        (mevedel-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication-discard-rolled-back ()
  ,test
  (test)
  :doc "removes only local failed recovery and leaves the committed head"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-rollback-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         old-head
         recovery)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path sidecar :content "old" :commit-marker t))))
          (setq old-head
                (mevedel-session-durability-publication-head session-dir))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-publish
                session
                (list (list :path sidecar :content "new" :commit-marker t)))
               :type 'user-error)))
          (setq recovery
                (mapcar
                 (lambda (batch) (plist-get batch :directory))
                 (plist-get (mevedel-session-pending-publication session)
                            :batches)))
          (should (cl-every #'file-directory-p recovery))
          (should
           (mevedel-session-publication-discard-rolled-back
            session))
          (should-not (mevedel-session-pending-publication session))
          (should (cl-every (lambda (path) (not (file-exists-p path)))
                            recovery))
          (should (equal old-head
                         (mevedel-session-durability-publication-head
                          session-dir))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-disclose ()
  ,test
  (test)
  :doc "remote disclosure precedes every target-side state write and occurs once"
  (let* ((host "disclosure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-disclosure-" t)))
         (session nil)
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         (prompts 0))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (cl-incf prompts)
                         nil)))
              (should-error
               (mevedel-session-persistence-assert-mutation-authority
                session)
               :type 'user-error))
            (should-not
             (file-exists-p (file-name-concat local-root ".mevedel")))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (cl-incf prompts)
                         t)))
              (should
               (mevedel-session-persistence-assert-mutation-authority
                session))
              (should
               (mevedel-session-durability-disclose session)))
            (should (= 2 prompts))
            (when-let ((save-path (mevedel-session-save-path session)))
              (mevedel-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication-publish ()
  ,test
  (test)
  :doc "marker deduplication is ordered and excludes external artifacts"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-overlay-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (duplicate (file-name-concat session-dir "duplicate.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (external (file-name-concat local-root "external.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path duplicate :content "first")
                  (list :path external :content "external")
                  (list :path duplicate :content "last")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (mevedel-session-publication-read session-dir))
                 (artifacts (plist-get publication :artifacts))
                 (entry (cdr (assoc "duplicate.el" artifacts))))
            (should (equal '("duplicate.el" "session.meta.el")
                           (mapcar #'car artifacts)))
            (should (equal "last"
                           (with-temp-buffer
                             (insert-file-contents
                              (plist-get entry :published))
                             (buffer-string))))
            (should (equal "external"
                           (with-temp-buffer
                             (insert-file-contents external)
                             (buffer-string))))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "remote marker creates its immutable directory through a file handler"
  (let* ((host "publication-manifest-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-manifest-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session nil)
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (run-program-function
          (symbol-function 'mevedel-session-control-fs-run-program))
         (immutable-directories 0))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          ;; The generation directories ride the write-generation program
          ;; as make-directory operations rather than wrapper calls.
          (cl-letf
              (((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (operations)
                  (dolist (operation operations)
                    (when (and (eq 'make-directory
                                   (plist-get operation :op))
                               (string-match-p
                                "/\\.publications"
                                (plist-get operation :path)))
                      (cl-incf immutable-directories)))
                  (funcall run-program-function operations))))
            (should
             (mevedel-session-publication-publish
              session
              (list
               (list :path (file-name-concat session-dir "session.meta.el")
                     :content "sidecar" :commit-marker t)))))
          (should (= 2 immutable-directories))
          (should (file-remote-p
                   (plist-get
                    (mevedel-session-publication-read session-dir)
                    :sidecar)))
          (mevedel-session-durability-lease-release session-dir session))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "replacement marker resets the prior logical snapshot"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-replace-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path first :content "first")
                  (list :path second :content "old second")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path second :content "new second")
                  (list :path sidecar :content "new sidecar"
                        :commit-marker t :replace t))))
          (let ((artifacts
                 (plist-get
                  (mevedel-session-publication-read session-dir)
                  :artifacts)))
            (should (equal '("second.el" "session.meta.el")
                           (mapcar #'car artifacts)))
            (should-not (assoc "first.el" artifacts)))
          (should (file-exists-p first)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "reentrant non-marker and marker batches commit in enqueue order"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-reentrant-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (publish-artifact
          (symbol-function 'mevedel-session-publication--publish-artifact))
         injected)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-publication--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact artifact)
                    (unless injected
                      (setq injected t)
                      (should
                       (eq 'queued
                           (mevedel-session-publication-publish
                            session
                            (list (list :path second :content "second")))))
                      (should
                       (eq 'queued
                           (mevedel-session-publication-publish
                            session
                            (list
                             (list :path sidecar :content "sidecar"
                                   :commit-marker t))))))))))
            (should
             (mevedel-session-publication-publish
              session (list (list :path first :content "first")))))
          (should
           (equal '("first.el" "second.el" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (mevedel-session-publication-read session-dir)
                    :artifacts))))
          (should-not (mevedel-session-publication-queue session))
          (should-not
           (mevedel-session-publication-uncommitted-batches session)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects invalid markers, control paths, and cross-target writes"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-invalid-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (dolist
              (artifacts
               (list
                (list (list :path (file-name-concat
                                   session-dir "nested/session.meta.el")
                            :content "nested" :commit-marker t))
                (list (list :path (file-name-concat
                                   local-root "session.meta.el")
                            :content "external" :commit-marker t))
                (list (list :path (file-name-concat
                                   session-dir ".lease/forged.el")
                            :content "lease"))
                (list (list :path (file-name-concat
                                   session-dir ".publications/forged.el")
                            :content "publication"))
                (list (list :path sidecar :content "replace" :replace t))
                (list (list :path sidecar :content "one" :commit-marker t)
                      (list :path sidecar :content "two" :commit-marker t))
                (list (list :path "/ssh:other:/tmp/cross-target.el"
                            :content "cross"))))
            (should-error
             (mevedel-session-publication-publish session artifacts)
             :type 'error))
          (should-not (mevedel-session-pending-publication session))
          (should-not (mevedel-session-durability-publication-head
                       session-dir)))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "fixed session caches remain invisible until a marker commits a snapshot"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-manifest-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment-0001.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path segment :content "old segment")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (let* ((old (mevedel-session-publication-read
                       session-dir))
                 (old-segment
                  (cdr (assoc "segment-0001.chat.org"
                              (plist-get old :artifacts)))))
            (should (string-prefix-p ".publications/"
                                     (plist-get old :head)))
            (should (equal "old sidecar"
                           (with-temp-buffer
                             (insert-file-contents
                              (plist-get old :sidecar))
                             (buffer-string))))
            (should (equal "old segment"
                           (with-temp-buffer
                             (insert-file-contents
                              (plist-get old-segment :published))
                             (buffer-string))))
            (should
             (equal (secure-hash 'sha256 "old segment")
                    (plist-get old-segment :sha256)))
            (should
             (mevedel-session-publication-publish
              session (list (list :path segment :content "new segment"))))
            (should (equal "new segment"
                           (with-temp-buffer
                             (insert-file-contents segment)
                             (buffer-string))))
            (should
             (equal old
                    (mevedel-session-publication-read session-dir)))
            (should
             (mevedel-session-publication-publish
              session
              (list (list :path sidecar :content "new sidecar"
                          :commit-marker t))))
            (let* ((new (mevedel-session-publication-read
                         session-dir))
                   (new-segment
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get new :artifacts)))))
              (should-not (equal (plist-get old :head)
                                 (plist-get new :head)))
              (should (equal "new segment"
                             (with-temp-buffer
                               (insert-file-contents
                                (plist-get new-segment :published))
                               (buffer-string))))
              (should (equal new (mevedel-session-publication session)))
              (should-not
               (mevedel-session-publication-uncommitted-batches session)))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "pre-CAS failure retains prior and current sources for convergent retry"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-pre-cas-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         recovery)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-publication-publish
            session (list (list :path segment :content "segment"))))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-publish
                session
                (list (list :path sidecar :content "sidecar"
                            :commit-marker t)))
               :type 'user-error)))
          (let ((batches
                 (plist-get (mevedel-session-pending-publication session)
                            :batches)))
            (should (= 2 (length batches)))
            (setq recovery
                  (mapcar (lambda (batch) (plist-get batch :directory))
                          batches)))
          (should-not
           (mevedel-session-publication-uncommitted-batches session))
          (should-not
           (mevedel-session-durability-publication-head session-dir))
          (should
           (mevedel-session-publication-retry session))
          (should-not (mevedel-session-pending-publication session))
          (should (cl-every (lambda (path) (not (file-exists-p path)))
                            recovery))
          (should
           (equal '("segment.chat.org" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (mevedel-session-publication-read session-dir)
                    :artifacts)))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "post-CAS normalization loss stays committed without retry recovery"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-post-cas-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (temporary-file-directory local-root)
         (before (directory-files temporary-file-directory nil
                                  "\\`mevedel-publication-")))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability--finish-publication-lease)
                (lambda (current)
                  (setf (plist-get (mevedel-session-lease current) :state)
                        'lost)
                  nil)))
            (should-error
             (mevedel-session-publication-publish
              session
              (list (list :path sidecar :content "committed"
                          :commit-marker t)))
             :type 'user-error))
          (let ((publication
                 (mevedel-session-publication-read session-dir)))
            (should publication)
            (should (equal publication
                           (mevedel-session-publication session))))
          (should-not (mevedel-session-pending-publication session))
          (should-not
           (mevedel-session-publication-uncommitted-batches session))
          (should (eq 'lost
                      (plist-get (mevedel-session-lease session) :state)))
          (should (equal before
                         (directory-files temporary-file-directory nil
                                          "\\`mevedel-publication-"))))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "non-marker publication retains local source until lease release"
  (let* ((host "publication-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-publication-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (target (file-name-concat session-dir "state.el"))
         (session nil)
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (temporary-file-directory local-root)
         (before (directory-files temporary-file-directory nil
                                  "\\`mevedel-publication-")))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (should (mevedel-session-publication-publish
                   session
                   (list (list :path target :content "new-state"))))
          (should (mevedel-session-durability-lease-owned-p session))
          (should (equal "new-state"
                         (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))))
          (should-not (mevedel-session-pending-publication session))
          (let ((recovery
                 (mapcar
                  (lambda (batch) (plist-get batch :directory))
                  (mevedel-session-publication-uncommitted-batches session))))
            (should (= 1 (length recovery)))
            (should (file-directory-p (car recovery)))
            (mevedel-session-persistence-lock-release session-dir session)
            (should-not (file-exists-p (car recovery))))
          (should (equal before
                         (directory-files temporary-file-directory nil
                                          "\\`mevedel-publication-"))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "mid-publication failure blocks mutation and retry consumes recovery"
  (let* ((host "publication-retry-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-retry-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (first (file-name-concat session-dir "first.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (second (file-name-concat session-dir "blocker" "second.el"))
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (mevedel-test--with-captured-diagnostics nil
            (should-error
             (mevedel-session-publication-publish
              session
              (list (list :path first :content "first")
                    (list :path second :content "second")
                    (list :path sidecar :content "sidecar"
                          :commit-marker t)))
             :type 'file-error))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches)))
                 (recovery (plist-get batch :directory)))
            (should (file-directory-p recovery))
            (should-error
             (mevedel-session-persistence-assert-mutation-authority session)
             :type 'user-error)
            (delete-file blocker-local)
            (make-directory blocker-local)
            (should
             (mevedel-session-publication-retry session))
            (should-not (file-exists-p recovery)))
          (should-not (mevedel-session-pending-publication session))
          (should (equal "second"
                         (with-temp-buffer
                           (insert-file-contents second)
                           (buffer-string))))
          (mevedel-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "explicit abandonment removes retained recovery and unblocks admission"
  (let* ((host "publication-abandon-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-abandon-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (target (file-name-concat session-dir "blocker" "state.el"))
         (session nil)
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (mevedel-test--with-captured-diagnostics nil
            (should-error
             (mevedel-session-publication-publish
              session (list (list :path target :content "state")))
             :type 'file-error))
          (let* ((pending (mevedel-session-pending-publication session))
                 (recovery
                  (plist-get (car (plist-get pending :batches)) :directory)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (mevedel-test--with-captured-diagnostics nil
                (should
                 (mevedel-session-publication-abandon session))))
            (should-not (file-exists-p recovery)))
          (should-not (mevedel-session-pending-publication session))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (should
             (mevedel-session-persistence-assert-mutation-authority session)))
          (mevedel-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "long publication fences takeover without reentrant timer I/O"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-lease-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (session (test-mevedel-session-durability--remote-session
                   "publication-lease-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (mevedel-session-lease-seconds 10)
         (mevedel-session-publication-lease-seconds 100)
         (now 0.0)
         (artifact-number 0)
         (takeover-prompts 0)
         (publish-artifact-function
          (symbol-function 'mevedel-session-publication--publish-artifact))
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (cl-incf takeover-prompts)
                     t)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-publication--publish-artifact)
                (lambda (artifact)
                  (cl-incf artifact-number)
                  (setq now (if (= artifact-number 1) 50.0 150.0))
                  (let ((mevedel-session-durability--client-id successor))
                    (should-not
                     (mevedel-session-durability-lease-acquire
                      session-dir "*successor*")))
                  (when (= artifact-number 1)
                    (let ((renames 0))
                      (cl-letf
                          (((symbol-function 'rename-file)
                            (lambda (file newname &optional ok-if-exists)
                              (cl-incf renames)
                              (funcall rename-file-function
                                       file newname ok-if-exists))))
                        (should
                         (mevedel-session-durability-lease-renew session)))
                      (should (= 0 renames))))
                  (prog1 (funcall publish-artifact-function artifact)
                    (when (= artifact-number 1)
                      (setq now 95.0))))))
            (should
             (mevedel-session-publication-publish
              session
              (list (list :path first :content "first")
                    (list :path second :content "second")))))
          (should (= 0 takeover-prompts))
          (should (mevedel-session-durability-lease-owned-p session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a pre-artifact lease failure retains the staged recovery batch"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-reserve-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability--renew-publication-lease)
                (lambda (&rest _) nil)))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-publish
                session (list (list :path target :content "state")))
               :type 'user-error)))
          (should-not (file-exists-p target))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))))
      (when-let ((pending (mevedel-session-pending-publication session)))
        (dolist (batch (plist-get pending :batches))
          (mevedel-session-publication--delete-batch batch)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "an expired final artifact fails closed and retains local recovery"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-expired-publication-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-mevedel-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (mevedel-session-durability--client-id owner)
         (mevedel-session-publication-lease-seconds 100)
         (now 0.0)
         (takeover-prompt nil)
         (publish-artifact-function
          (symbol-function 'mevedel-session-publication--publish-artifact)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-publication--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact-function artifact)
                    (setq now 101.0)
                    (let ((mevedel-session-durability--client-id successor))
                      (cl-letf
                          (((symbol-function 'y-or-n-p)
                            (lambda (prompt)
                              (setq takeover-prompt prompt)
                              t)))
                        (should
                         (mevedel-session-durability-lease-acquire
                          session-dir "*successor*"))))))))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-publish
                session (list (list :path target :content "state")))
               :type 'user-error)))
          (should (string-match-p "critical write may still be in flight"
                                  takeover-prompt))
          (should (string-match-p "prior client is stopped"
                                  takeover-prompt))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))))
      (when-let ((pending (mevedel-session-pending-publication session)))
        (dolist (batch (plist-get pending :batches))
          (mevedel-session-publication--delete-batch batch)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-publication-append-diagnostic
  ()
  ,test
  (test)
  :doc "retries a failed remote diagnostic atomically without blocking mutation"
  (let* ((host "diagnostic-retry-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-diagnostic-" t)))
         (session nil)
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "initial transcript")
            (mevedel-session-persistence-save session (current-buffer))
            (let* ((save-path (mevedel-session-save-path session))
                   (log-path (file-name-concat save-path "hook-log.el"))
                   (entry '(:event Stop :status completed))
                   (publish-artifact
                    (symbol-function
                     'mevedel-session-publication--publish-artifact))
                   (diagnostic-publications 0)
                   warning)
              (make-directory log-path)
              (mevedel-hooks--log session entry)
              (goto-char (point-max))
              (insert "\nsettled")
              (cl-letf
                  (((symbol-function 'display-warning)
                    (lambda (_type message &rest _)
                      (setq warning message))))
                (should (mevedel-session-persistence-save
                         session (current-buffer)))
                ;; The diagnostic flush is deferred off the save; drive
                ;; the timer until its failure warning lands.
                (let ((deadline (+ (float-time) 2)))
                  (while (and (not warning) (< (float-time) deadline))
                    (accept-process-output nil 0.02))))
              (should warning)
              (should (mevedel-session-hook-log-pending session))
              (should-not (mevedel-session-pending-publication session))
              (should
               (mevedel-session-persistence-assert-mutation-authority session))
              (delete-directory log-path)
              (cl-letf
                  (((symbol-function
                     'mevedel-session-publication--publish-artifact)
                    (lambda (artifact)
                      (when (equal log-path (plist-get artifact :path))
                        (cl-incf diagnostic-publications))
                      (funcall publish-artifact artifact))))
                (should (mevedel-session-persistence-save
                         session (current-buffer)))
                (let ((deadline (+ (float-time) 2)))
                  (while (and (zerop diagnostic-publications)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.02))))
              (should (= 1 diagnostic-publications))
              (should-not (mevedel-session-hook-log-pending session))
              (should-not (mevedel-session-pending-publication session))
              (with-temp-buffer
                (insert-file-contents log-path)
                (should (equal entry (read (current-buffer)))))
              (mevedel-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "quietly retains a diagnostic while the lease cannot carry it"
  ;; A renewal the transport cannot run right now is a retry, not a
  ;; failure: nothing reads a diagnostic live, and echoing an error per
  ;; attempt buries real failures in expected noise.
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-diagnostic-quiet-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (captured nil))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (mevedel-test--with-captured-diagnostics captured
            (cl-letf (((symbol-function
                        'mevedel-session-durability-lease-renew)
                       (lambda (_session) nil)))
              (should-not
               (mevedel-session-publication-append-diagnostic
                session
                (file-name-concat session-dir "telemetry-log.el")
                "(:event probe)\n"))))
          (should (string-empty-p captured))
          ;; The lease itself is untouched; the next attempt may publish.
          (should (mevedel-session-durability-lease-owned-p session)))
      (mevedel-session-durability--cancel-renewal session)
      (ignore-errors
        (mevedel-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "final lease loss retains queued critical bytes as retryable batches"
  (let* ((host "diagnostic-critical-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-diagnostic-critical-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s/" host session-dir-local))
         (diagnostic (file-name-concat session-dir "hook-log.el"))
         (critical (file-name-concat session-dir "segment.chat.org"))
         (session
          (test-mevedel-session-durability--remote-session host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (publish-artifact
          (symbol-function 'mevedel-session-publication--publish-artifact))
         (finish-publication
          (symbol-function
           'mevedel-session-durability--finish-publication-lease))
         injected
         fail-finish)
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*diagnostic*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-publication--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact artifact)
                    (unless injected
                      (setq injected t
                            fail-finish t)
                      (should
                       (eq 'queued
                           (mevedel-session-publication-publish
                            session
                            (list (list :path critical
                                        :content "critical")))))))))
               ((symbol-function
                 'mevedel-session-durability--finish-publication-lease)
                (lambda (current)
                  (if fail-finish
                      (progn
                        (setq fail-finish nil)
                        nil)
                    (funcall finish-publication current)))))
            (mevedel-test--with-captured-diagnostics nil
              (should-error
               (mevedel-session-publication-append-diagnostic
                session diagnostic "diagnostic")
               :type 'user-error))
            (let ((batches
                   (plist-get (mevedel-session-pending-publication session)
                              :batches)))
              (should (= 1 (length batches)))
              (should (file-directory-p (plist-get (car batches) :directory)))
              (should (mevedel-session-publication--batch-live-p
                       (car batches))))
            (should
             (eq 'published
                 (mevedel-session-publication-retry session)))
            (should-not (mevedel-session-pending-publication session))
            (let ((batches
                   (mevedel-session-publication-uncommitted-batches session)))
              (should (= 1 (length batches)))
              (should (mevedel-session-publication--batch-live-p
                       (car batches)))))
          (mevedel-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence-publish-agent-terminal-state
  (:doc "publishes transcript and final registry sidecar as one retryable batch")
  (let* ((host "agent-terminal-host")
         ;; The transcript buffer visits a mock-remote file; a lockfile
         ;; there survives into teardown and warns on removal.
         (create-lockfiles nil)
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-agent-terminal-" t)))
         (root (format "/mevedelmock:%s:%s/" host local-root))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/mevedelmock:%s:%s" host session-dir-local))
         (transcript-relative "agents/explorer.chat.org")
         (transcript (concat session-dir transcript-relative))
         (sidecar (concat session-dir "session.meta.el"))
         session
         target
         (parent (generate-new-buffer " *remote-agent-parent*"))
         (child (generate-new-buffer " *remote-agent-child*"))
         (agent
          (mevedel-agent--create
           :name "explorer" :description "Explore" :tools nil
           :system-prompt nil :max-turns nil :hook-rules nil :frozen-p t))
         (configuration
          (mevedel-agent-configuration--create
           :agent agent :request-locals nil))
         invocation
         record
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root)
                target (mevedel-session-execution-target session)
                invocation (mevedel-agent-invocation-create agent)
                record
                (mevedel-agent-record--create
                 :id "explorer--test" :path "/root/explorer"
                 :parent-path "/root" :role "explorer"
                 :configuration configuration :activity 'idle
                 :conversation-location transcript-relative))
          (test-mevedel-session-durability--accept-storage session)
          (mevedel-workspace-identity-ensure root)
          (mevedel-execution-target-seed-incarnation target "remote-host-a")
          (setf (mevedel-session-save-path session) session-dir
                (mevedel-session-session-id session) "agent-session"
                (mevedel-session-created-at session) "created"
                (mevedel-session-updated-at session) "updated"
                (mevedel-session-current-segment session) 1
                (mevedel-session-agent-registry session)
                (list (cons "/root/explorer" record))
                (mevedel-session-agent-transcripts session)
                `(("explorer--test"
                   :agent-type "explorer" :agent-path "/root/explorer"
                   :description "Explore" :path ,transcript-relative
                   :status completed :created-at "created"
                   :updated-at "updated" :parent-turn 0)))
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (setq-local mevedel-workspace-additional-roots nil))
          (with-current-buffer child
            (setq-local mevedel--session session)
            (setq-local create-lockfiles nil)
            (setq buffer-file-name transcript)
            (insert "terminal transcript"))
          (setf (mevedel-agent-invocation-agent-id invocation)
                "explorer--test"
                (mevedel-agent-invocation-path invocation) "/root/explorer"
                (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-parent-data-buffer invocation) parent
                (mevedel-agent-invocation-buffer invocation) child
                (mevedel-agent-invocation-transcript-relative-path invocation)
                transcript-relative
                (mevedel-agent-invocation-transcript-status invocation)
                'completed
                (mevedel-agent-invocation-sidecar-dirty invocation) t)
          (should
           (mevedel-session-persistence-lock-acquire
            session-dir "*agent-terminal*" session))
          (should
           (mevedel-session-publication-publish
            session
            (list (list :path sidecar :content "(:materialized t)"
                        :commit-marker t))))
          (let ((publish-artifact
                 (symbol-function
                  'mevedel-session-publication--publish-artifact))
                (count 0))
            (cl-letf
                (((symbol-function
                   'mevedel-session-publication--publish-artifact)
                  (lambda (artifact)
                    (cl-incf count)
                    (if (= count 2)
                        (signal 'file-error
                                '("Injected agent sidecar failure"))
                      (funcall publish-artifact artifact)))))
              (mevedel-test--with-captured-diagnostics nil
                (should-error
                 (mevedel-session-persistence-publish-agent-terminal-state
                  invocation)
                 :type 'file-error))))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batches (plist-get pending :batches))
                 (artifacts (plist-get (car batches) :artifacts)))
            (should (= 1 (length batches)))
            (should
             (equal (list transcript sidecar)
                    (mapcar (lambda (artifact)
                              (plist-get artifact :path))
                            artifacts))))
          (should-error
           (mevedel-session-persistence-assert-mutation-authority session)
           :type 'user-error)
          (should (mevedel-session-publication-retry session))
          (should-not (mevedel-session-pending-publication session))
          (should
           (equal "terminal transcript"
                  (with-temp-buffer
                    (insert-file-contents transcript)
                    (buffer-string))))
          (let* ((saved (mevedel-session-persistence-read sidecar))
                 (saved-record (car (plist-get saved :agent-registry))))
            (should (eq 'idle (plist-get saved-record :activity)))
            (should
             (eq 'completed
                 (plist-get
                  (cdr (assoc "explorer--test"
                              (plist-get saved :agent-transcripts)))
                  :status))))
          (mevedel-session-persistence-lock-release session-dir session))
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence-save/remote ()
  ,test
  (test)
  :doc "canonical remote save publishes segment, snapshots, and sidecar target-side"
  (let* ((host "canonical-save-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-save-" t)))
         (session nil)
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "remote transcript")
            (should (mevedel-session-persistence-save session (current-buffer)))
            (let* ((save-path (mevedel-session-save-path session))
                   (segment (file-name-concat
                             save-path "segment-0001.chat.org"))
                   (sidecar (file-name-concat save-path "session.meta.el"))
                   (current (file-name-concat
                             save-path "instructions" "current.el")))
              (should (file-exists-p segment))
              (should (file-exists-p sidecar))
              (should (file-exists-p current))
              (should (equal "remote transcript"
                             (with-temp-buffer
                               (insert-file-contents segment)
                               (buffer-string))))
              (should-not (buffer-modified-p))
              (should-not (file-exists-p
                           (file-name-concat save-path ".lock")))
              (should (file-directory-p
                       (file-name-concat save-path ".lease")))
              (let* ((saved (mevedel-session-persistence-read sidecar))
                     (workspace (plist-get saved :workspace)))
                (should-not
                 (file-remote-p (plist-get workspace :target-native-root))))
              (mevedel-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence-rotate-segment/remote ()
  ,test
  (test)
  :doc "remote rotation publishes old, new, instructions, and sidecar together"
  (let* ((host "rotate-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-rotate-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-mevedel-session-durability--remote-session
                  host local-root)))
            (test-mevedel-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "Original prompt\nOriginal reply\n")
              (mevedel-session-persistence-save session (current-buffer))
              (let* ((save-path (mevedel-session-save-path session))
                     (old (file-name-concat
                           save-path "segment-0001.chat.org"))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (should
                 (equal new
                        (mevedel-session-persistence-rotate-segment
                         session (current-buffer) "Remote handoff.")))
                (should (equal new buffer-file-name))
                (should (string-match-p "Remote handoff" (buffer-string)))
                (should
                 (string-match-p
                  "MEVEDEL_SEGMENT_FINALIZED_AT"
                  (with-temp-buffer
                    (insert-file-contents old)
                    (buffer-string))))
                (should
                 (= 2
                    (plist-get
                     (mevedel-session-persistence-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (should-not
                 (mevedel-session-pending-publication session))
                (mevedel-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "mid-rotation failure retains one complete retryable transition"
  (let* ((host "rotate-failure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-rotate-failure-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-mevedel-session-durability--remote-session
                  host local-root)))
            (test-mevedel-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "Before rotation\n")
              (mevedel-session-persistence-save session (current-buffer))
              (let* ((save-path (mevedel-session-save-path session))
                     (instructions
                      (file-name-concat save-path "instructions"))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (delete-directory instructions t)
                (with-temp-file instructions (insert "blocker"))
                (mevedel-test--with-captured-diagnostics nil
                  (should-error
                   (mevedel-session-persistence-rotate-segment
                    session (current-buffer) "Pending handoff.")
                   :type 'file-error))
                (should (mevedel-session-pending-publication session))
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal new buffer-file-name))
                (should (string-match-p "Pending handoff" (buffer-string)))
                (delete-file instructions)
                (make-directory instructions)
                (should
                 (mevedel-session-publication-retry session))
                (should-not
                 (mevedel-session-pending-publication session))
                (should
                 (= 2
                    (plist-get
                     (mevedel-session-persistence-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (mevedel-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence-start-fresh-segment/remote ()
  ,test
  (test)
  :doc "remote clear atomically publishes the transition but not its draft"
  (let* ((host "fresh-segment-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-fresh-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-mevedel-session-durability--remote-session
                  host local-root)))
            (test-mevedel-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "Old conversation\n")
              (mevedel-session-persistence-save session (current-buffer))
              (let* ((save-path (mevedel-session-save-path session))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (should
                 (equal new
                        (mevedel-session-persistence-start-fresh-segment
                         session (current-buffer)
                         :initial-text "Unsent draft")))
                (should (string-match-p "Unsent draft" (buffer-string)))
                (should-not
                 (string-match-p
                  "Unsent draft"
                  (with-temp-buffer
                    (insert-file-contents new)
                    (buffer-string))))
                (should
                 (= 2
                    (plist-get
                     (mevedel-session-persistence-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (mevedel-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence--publish-fork/remote ()
  ,test
  (test)
  :doc "fork staging owns its child lease first and releases it on failure"
  (let* ((host "fork-staging-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-fork-stage-" t)))
         (staging-local
          (file-name-as-directory (file-name-concat local-root "staging")))
         (staging
          (format "/mevedelmock:%s:%s/" host staging-local))
         (published
          (format "/mevedelmock:%s:%s/" host
                  (file-name-concat local-root "published")))
         (marker (file-name-concat staging "stage-started"))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         lease-before-stage)
    (make-directory staging-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let* ((child
                  (test-mevedel-session-durability--remote-session
                   host local-root))
                 (buffer (generate-new-buffer " *fork-source*"))
                 (staging-buffer (generate-new-buffer " *fork-stage*")))
            (unwind-protect
                (progn
                  (test-mevedel-session-durability--accept-storage child)
                  (setf (mevedel-session-save-path child) staging
                        (mevedel-session-session-id child) "child")
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-persistence--stage-fork)
                        (lambda (&rest _)
                          (setq lease-before-stage
                                (file-directory-p
                                 (file-name-concat staging ".lease")))
                          (with-temp-file marker (insert "started"))
                          (error "Staging failed"))))
                    (should-error
                     (mevedel-session-persistence--publish-fork
                      child buffer staging-buffer staging staging published
                      1 0 nil)
                     :type 'error))
                  (should lease-before-stage)
                  (should-not (mevedel-session-lease child))
                  (should-not
                   (mevedel-session-lease-renewal-timer child))
                  (should-not (file-directory-p staging))
                  (should-not (file-directory-p published)))
              (when (buffer-live-p buffer)
                (kill-buffer buffer))
              (when (buffer-live-p staging-buffer)
                (kill-buffer staging-buffer)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-recovery-record-failure ()
  ,test
  (test)
  :doc "an incomplete specialized transaction blocks until explicit recovery"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-specialized-failure-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "specialized"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (mevedel-test--with-captured-diagnostics nil
            (mevedel-session-recovery-record-failure
             session "rewind rollback incomplete" "/target/recovery"))
          (should (mevedel-session-pending-publication session))
          (should-error
           (mevedel-session-publication-retry session)
           :type 'user-error)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (mevedel-test--with-captured-diagnostics nil
              (should
               (mevedel-session-publication-abandon session))))
          (should-not (mevedel-session-pending-publication session)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-specialized-recovery/remote ()
  ,test
  (test)
  :doc "target recovery survives client loss and blocks takeover until abandonment"
  (let* ((host "specialized-target-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-specialized-target-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/mevedelmock:%s:%s/" host session-dir-local))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (now 0.0)
         (session nil)
         recovery)
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now)))
            (let ((mevedel-session-durability--client-id owner))
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*rewind-owner*" session))
              (setq recovery (make-temp-file "mevedel-rewind-recovery-" t))
              (make-directory (file-name-concat recovery "files") t)
              (with-temp-file (file-name-concat recovery "files" "before.el")
                (insert "before"))
              (mevedel-test--with-captured-diagnostics nil
                (mevedel-session-recovery-record-failure
                 session "rewind rollback incomplete" recovery))
              (should-not (file-directory-p recovery))
              (let* ((durable
                      (mevedel-session-recovery-read
                       session-dir))
                     (directory (plist-get durable :directory))
                     (marker (plist-get durable :marker)))
                (should durable)
                (should (file-directory-p directory))
                (should (file-regular-p marker))
                (should (equal "before"
                               (with-temp-buffer
                                 (insert-file-contents
                                  (file-name-concat directory "files" "before.el"))
                                 (buffer-string))))
                (let ((mevedel--data-buffer nil)
                      (mevedel--agent-invocation nil))
                  (with-temp-buffer
                    (setq-local mevedel--session session)
                    (should-error
                     (mevedel-session-persistence-assert-mutation-authority
                      session (current-buffer))
                     :type 'user-error))))
              ;; Simulate client loss rather than a clean release: the marker
              ;; must remain discoverable when a new client takes over.
              (mevedel-session-durability--cancel-renewal session)
              (setq now 100.0)
              (let ((mevedel-session-durability--client-id successor))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) t))
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t)))
                  (let ((restored
                         (test-mevedel-session-durability--remote-session
                          host local-root)))
                    (setf (mevedel-session-save-path restored) session-dir)
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*rewind-successor*" restored))
                    (mevedel-session-recovery-refresh
                     restored)
                    (should (plist-get
                             (mevedel-session-pending-publication restored)
                             :manual-recovery-marker))
                    (let ((mevedel--data-buffer nil)
                          (mevedel--agent-invocation nil))
                      (with-temp-buffer
                        (setq-local mevedel--session restored)
                        (should-error
                         (mevedel-session-persistence-assert-mutation-authority
                          restored (current-buffer))
                         :type 'user-error)))
                    (mevedel-test--with-captured-diagnostics nil
                      (should
                       (mevedel-session-publication-abandon restored)))
                    (should-not
                     (mevedel-session-recovery-read
                      session-dir))
                    (should-not
                     (file-directory-p
                      (file-name-concat session-dir ".recovery")))
                    (should-not
                     (mevedel-session-pending-publication restored))
                    (let ((mevedel--data-buffer nil)
                          (mevedel--agent-invocation nil))
                      (with-temp-buffer
                        (setq-local mevedel--session restored)
                        (should
                         (mevedel-session-persistence-assert-mutation-authority
                          restored (current-buffer)))))
                    (mevedel-session-durability-lease-release
                     session-dir restored)))))))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "failed target installation retains honest local recovery"
  (let* ((host "specialized-target-failure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-specialized-target-failure-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session nil)
         (recovery (make-temp-file "mevedel-rewind-local-recovery-" t))
         (mevedel-session-durability--client-id (make-string 64 ?c))
         diagnostics)
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should
           (mevedel-session-durability-lease-acquire
            session-dir "*rewind-failure*" session))
          (cl-letf (((symbol-function
                      'mevedel-session-recovery--copy-local-directory)
                     (lambda (&rest _)
                       (error "Injected target recovery copy failure"))))
            (mevedel-test--with-captured-diagnostics diagnostics
              (mevedel-session-recovery-record-failure
               session "rewind rollback incomplete" recovery)))
          (should (string-match-p "recovery could not be installed"
                                  diagnostics))
          (let ((pending (mevedel-session-pending-publication session)))
            (should (file-directory-p recovery))
            (should-not (plist-get pending :recovery-portable))
            (should (equal recovery
                           (plist-get pending :manual-recovery-local)))
            (should (string-match-p "Injected target recovery copy failure"
                                    (plist-get pending :reason)))
            (should-not
             (mevedel-session-recovery-read
              session-dir))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (mevedel-test--with-captured-diagnostics nil
                (should
                 (mevedel-session-publication-abandon session))))
            (should-not (file-directory-p recovery))
            (should-not (mevedel-session-pending-publication session)))
          (mevedel-session-durability-lease-release session-dir session))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "removes target marker and bytes when marker readback fails"
  (let* ((host "specialized-target-readback-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-specialized-target-readback-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/mevedelmock:%s:%s/" host session-dir-local))
         (session nil)
         (recovery (make-temp-file "mevedel-rewind-readback-recovery-" t))
         (mevedel-session-durability--client-id (make-string 64 ?d)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should
           (mevedel-session-durability-lease-acquire
            session-dir "*rewind-readback*" session))
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (cl-letf (((symbol-function
                      'mevedel-session-recovery--read-marker)
                     (lambda (&rest _)
                       (error "Injected marker readback failure"))))
            (mevedel-test--with-captured-diagnostics nil
              (mevedel-session-recovery-record-failure
               session "rewind rollback incomplete" recovery)))
          (let ((pending (mevedel-session-pending-publication session))
                (root (file-name-concat session-dir ".recovery")))
            (should (file-directory-p recovery))
            (should-not (plist-get pending :recovery-portable))
            (should (string-match-p "Injected marker readback failure"
                                    (plist-get pending :reason)))
            (should-not
             (mevedel-session-recovery-read
              session-dir))
            (when (file-directory-p root)
              (should-not (directory-files root nil "\\`recovery-"))
              (should-not (directory-files root nil "\\`[0-9a-f]+\\'"))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (mevedel-test--with-captured-diagnostics nil
              (should
               (mevedel-session-publication-abandon session))))
          (should-not (file-directory-p recovery))
          (let ((mevedel-session-durability--client-id
                 (make-string 64 ?d)))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "retains local recovery when a foreign lease blocks target installation"
  (let* ((host "specialized-target-lease-loss-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-specialized-target-lease-loss-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/mevedelmock:%s:%s/" host session-dir-local))
         (owner (make-string 64 ?e))
         (successor (make-string 64 ?f))
         (session nil)
         (recovery (make-temp-file "mevedel-rewind-lease-loss-recovery-" t)))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (setq session
                (test-mevedel-session-durability--remote-session
                 host local-root))
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (let ((mevedel-session-durability--client-id owner))
            (should
             (mevedel-session-durability-lease-acquire
              session-dir "*rewind-owner*" session)))
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (let ((mevedel-session-durability--client-id successor))
            (mevedel-test--with-captured-diagnostics nil
              (mevedel-session-recovery-record-failure
               session "rewind rollback incomplete" recovery))
            (let ((pending (mevedel-session-pending-publication session)))
              (should (file-directory-p recovery))
              (should-not (plist-get pending :recovery-portable))
              (should (equal recovery
                             (plist-get pending :manual-recovery-local)))
              (should (string-match-p "lease is unavailable"
                                      (plist-get pending :reason)))
              (should-not
               (mevedel-session-recovery-read
                session-dir)))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (mevedel-test--with-captured-diagnostics nil
                (should
                 (mevedel-session-publication-abandon session)))))
          (should-not (file-directory-p recovery))
          (let ((mevedel-session-durability--client-id owner))
            (mevedel-session-durability-lease-release session-dir session)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-mixed-controls ()
  ,test
  (test)
  :doc "retains local recovery and fails closed when target controls are mixed"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-mixed-recovery-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "mixed-recovery"))
         (session (mevedel-session-create "main" workspace))
         (session-dir (file-name-as-directory
                       (file-name-concat root "session")))
         (recovery (make-temp-file "mevedel-mixed-local-recovery-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat session-dir ".lease") t)
          (with-temp-file (file-name-concat session-dir ".lock")
            (insert "mixed controls"))
          (setf (mevedel-session-save-path session) session-dir)
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (mevedel-test--with-captured-diagnostics nil
            (mevedel-session-recovery-record-failure
             session "rewind rollback incomplete" recovery))
          (let ((pending (mevedel-session-pending-publication session)))
            (should (file-directory-p recovery))
            (should (equal recovery
                           (plist-get pending :manual-recovery-local)))
            (should (string-match-p "Portable session has a PID lock"
                                    (plist-get pending :reason)))
            (should-not
             (file-exists-p (file-name-concat session-dir ".recovery")))
            (let* ((id "0123456789abcdef0123456789abcdef")
                   (recovery-root (file-name-concat session-dir ".recovery"))
                   (recovery-dir (file-name-concat recovery-root id))
                   (marker (file-name-concat
                            recovery-root (concat "recovery-" id ".el"))))
              (make-directory recovery-dir t)
              (with-temp-file marker
                (prin1 (list :version 1
                              :kind 'rewind
                              :directory (file-name-concat ".recovery" id)
                              :reason "mixed controls"
                              :created-at "now")
                       (current-buffer)))
              ;; A valid recovery marker must still be rejected when the
              ;; obsolete PID lock is present beside portable controls.
              (should-error
               (mevedel-session-recovery-read
                session-dir)
               :type 'error)
              (delete-file (file-name-concat session-dir ".lock"))
              (should
               (equal recovery-dir
                      (plist-get
                       (mevedel-session-recovery-read
                        session-dir)
                       :directory))))))
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-publication-status ()
  ,test
  (test)
  :doc "status exposes lease, pending publication, and authoritative path"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-durability-status-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "status"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (setf (mevedel-session-lease session)
                '(:state foreign
                  :publication-head ".publications/current/manifest.el")
                (mevedel-session-pending-publication session)
                '(:reason "offline"))
          (let ((status (mevedel-session-publication-status session)))
            (should (eq 'foreign (plist-get status :lease-state)))
            (should (equal ".publications/current/manifest.el"
                           (plist-get status :publication-head)))
            (should (eq t (plist-get status :pending-publication)))
            (should (equal "offline" (plist-get status :pending-reason)))
            (should (equal (file-name-concat root ".mevedel/")
                           (plist-get status :authoritative-state-path))))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (let ((header (mevedel-session-persistence-header-segment)))
              (should (string-match-p "Publication pending" header))
              (should (string-match-p "Lease: foreign" header)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'test-mevedel-session-durability)
;;; test-mevedel-session-durability.el ends here
