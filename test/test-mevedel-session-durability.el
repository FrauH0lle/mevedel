;;; test-mevedel-session-durability.el --- Durable remote sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Covers remote leases, serialized publication, and recovery boundaries.

;;; Code:

(require 'mevedel)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
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
         (confirmed nil)
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _) now)))
            (let ((mevedel-session-durability--client-id (make-string 64 ?a)))
              (should
               (mevedel-session-persistence-lock-acquire
                session-dir "*expired*")))
            (setq now 100.0)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq confirmed t)
                         t)))
              (should
               (mevedel-session-persistence-lock-acquire
                session-dir "*new*")))
            (should confirmed)
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*new*"))))
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
         (create-generation-function
          (symbol-function
           'mevedel-session-durability--create-generation))
         threads)
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (let ((mevedel-session-durability--client-id owner))
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*expired*")))
          (setq now 100.0)
          (cl-letf
              (((symbol-function 'y-or-n-p)
                (lambda (&rest _) t))
               ((symbol-function
                 'mevedel-session-durability--create-generation)
                (lambda (directory record)
                  (with-mutex claim-mutex
                    (cl-incf claim-count)
                    (push (plist-get record :generation)
                          claim-generations)
                    (condition-notify claim-condition t)
                    (while (< claim-count 2)
                      (condition-wait claim-condition)))
                  (funcall create-generation-function directory record))))
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
           (now 0.0)
           (claim-next-function
            (symbol-function 'mevedel-session-durability--claim-next)))
      (make-directory session-dir t)
      (setf (mevedel-session-save-path session) session-dir)
      (unwind-protect
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _) now)))
            (let ((mevedel-session-durability--client-id owner))
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*expired*")))
            (setq now 100.0)
            (let ((mevedel-session-durability--client-id contender))
              (cl-letf
                  (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                   ((symbol-function
                     'mevedel-session-durability--claim-next)
                    (lambda (directory expected buffer-name)
                      (let ((candidate
                             (funcall claim-next-function
                                      directory expected buffer-name)))
                        (let ((mevedel-session-durability--client-id blocker))
                          (should
                           (mevedel-session-durability--create-generation
                            directory
                            (mevedel-session-durability--lease-record
                             "*paused-claim*"
                             (1+ (plist-get candidate :generation))
                             'claiming))))
                        candidate))))
                (should-not
                 (mevedel-session-durability-lease-acquire
                  session-dir "*contender*" (and bind-session session)))))
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
         (rename-file-function (symbol-function 'rename-file))
         (fail-finalization t)
         (confirmed nil))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now))
                  ((symbol-function 'rename-file)
                   (lambda (file newname &optional ok-if-exists)
                     (if fail-finalization
                         (progn
                           (setq fail-finalization nil)
                           (error "Injected claim finalization crash"))
                       (funcall rename-file-function
                                file newname ok-if-exists)))))
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
         write-generation
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
                  write-generation
                  (symbol-function
                   'mevedel-session-durability--write-generation))
            (cl-letf
                (((symbol-function
                   'mevedel-session-durability--write-generation)
                  (lambda (directory record)
                    (when (and (not injected)
                               (eq 'active (plist-get record :status))
                               (not (plist-get record :unsettled-mutation)))
                      (setq injected t)
                      (should
                       (mevedel-session-durability-set-unsettled-mutation
                        session t)))
                    (funcall write-generation directory record))))
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
          (cl-letf (((symbol-function 'float-time)
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
         (now 0.0)
         (triggered nil)
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional _) now)))
            (should (mevedel-session-durability-lease-acquire
                     session-dir "*owner*" session))
            (setq now 80.0)
            (cl-letf
                (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ((symbol-function 'rename-file)
                  (lambda (file newname &optional ok-if-exists)
                    (unless triggered
                      (setq triggered t
                            now 100.0)
                      (let ((mevedel-session-durability--client-id successor))
                        (should
                         (mevedel-session-durability-lease-acquire
                          session-dir "*successor*"))))
                    (funcall rename-file-function
                             file newname ok-if-exists))))
              (should-not
               (mevedel-session-durability-lease-renew session)))
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
         (initial t)
         (mutex (make-mutex "mevedel renew-before-claim"))
         (condition (make-condition-variable mutex))
         (renew-ready nil)
         (renew-allowed nil)
         (renew-done nil)
         (renew-result nil)
         (rename-file-function (symbol-function 'rename-file))
         renew-thread)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf
            (((symbol-function 'float-time)
              (lambda (&optional _)
                (cond
                 (initial 0.0)
                 ((equal mevedel-session-durability--client-id owner) 80.0)
                 (t 100.0))))
             ((symbol-function 'rename-file)
              (lambda (file newname &optional ok-if-exists)
                (when (and (equal (thread-name (current-thread))
                                  "mevedel-owner-renew")
                           (not renew-ready))
                  (with-mutex mutex
                    (setq renew-ready t)
                    (condition-notify condition t)
                    (while (not renew-allowed)
                      (condition-wait condition))))
                (funcall rename-file-function file newname ok-if-exists))))
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
         (directory-files-function (symbol-function 'directory-files))
         (cache-inhibited nil))
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
               "*successor*" 2 'active))))
          (cl-letf
              (((symbol-function 'directory-files)
                (lambda (directory &rest arguments)
                  (if (not (equal directory lease-directory))
                      (apply directory-files-function directory arguments)
                    (setq cache-inhibited
                          (or cache-inhibited
                              remote-file-name-inhibit-cache))
                    (if remote-file-name-inhibit-cache
                        (apply directory-files-function directory arguments)
                      (list
                       (mevedel-session-durability--generation-path
                        lease-directory 1)))))))
            (should-not
             (mevedel-session-durability-lease-renew session)))
          (should cache-inhibited))
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
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
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
            (cl-letf
                (((symbol-function 'rename-file)
                  (lambda (file newname &optional ok-if-exists)
                    (when (and (not triggered)
                               (string-prefix-p
                                (file-name-concat session-dir ".lease")
                                newname))
                      (trigger-successor))
                    (funcall rename-file-function
                             file newname ok-if-exists))))
              (mevedel-session-durability-lease-release
               session-dir owner-session)))
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
            (should-error
             (mevedel-session-durability-publish
              session (list (list :path target :content "critical")))
             :type 'file-error)
            (let* ((pending (mevedel-session-pending-publication session))
                   (batch (car (plist-get pending :batches)))
                   (recovery (plist-get batch :directory)))
              (should (file-directory-p recovery))
              (mevedel-session-durability-lease-release session-dir session)
              (should (eq pending
                          (mevedel-session-pending-publication session)))
              (should (file-directory-p recovery))
              (dolist (retained (plist-get pending :batches))
                (mevedel-session-durability--delete-batch retained)))))
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
          (should
           (mevedel-session-durability-call-with-reserved-lease
            session
            (lambda ()
              (should
               (eq 'queued
                   (mevedel-session-durability-publish
                    session (list (list :path target :content "critical")))))
              (mevedel-session-durability-lease-release session-dir session)
              t)))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))
            (mevedel-session-durability--delete-batch batch)))
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
        (cl-letf (((symbol-function 'float-time)
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
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path owner-session) session-dir
          (mevedel-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
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
          (cl-letf
              (((symbol-function 'rename-file)
                (lambda (file newname &optional ok-if-exists)
                  (unless triggered
                    (setq triggered t
                          now 101.0)
                    (let ((mevedel-session-durability--client-id successor))
                      (cl-letf (((symbol-function 'y-or-n-p)
                                 (lambda (&rest _) t)))
                        (should
                         (mevedel-session-durability-lease-acquire
                          session-dir "*successor*" successor-session)))))
                  (funcall rename-file-function
                           file newname ok-if-exists))))
            (should-not
             (mevedel-session-durability-commit-publication-head
              owner-session ".publications/stale/manifest.el")))
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
         (directory-files-function (symbol-function 'directory-files))
         (cache-inhibited nil))
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
               "*old*" 1 'active ".publications/old/manifest.el"))))
          (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
            (should
             (mevedel-session-durability--create-generation
              lease-directory
              (mevedel-session-durability--lease-record
               "*new*" 2 'active ".publications/new/manifest.el"))))
          (cl-letf
              (((symbol-function 'directory-files)
                (lambda (directory &rest arguments)
                  (if (not (equal directory lease-directory))
                      (apply directory-files-function directory arguments)
                    (setq cache-inhibited
                          (or cache-inhibited
                              remote-file-name-inhibit-cache))
                    (if remote-file-name-inhibit-cache
                        (apply directory-files-function directory arguments)
                      (list
                       (mevedel-session-durability--generation-path
                        lease-directory 1)))))))
            (should
             (equal ".publications/new/manifest.el"
                    (mevedel-session-durability-publication-head
                     session-dir))))
          (should cache-inhibited))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-read-publication ()
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
           (mevedel-session-durability-read-publication session-dir))
          (should-not (file-exists-p (file-name-concat session-dir ".lease")))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (mevedel-session-durability-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (mevedel-session-durability-read-publication session-dir))
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
                    (mevedel-session-durability-read-publication session-dir)))
            (with-temp-file sidecar-path (insert "corrupt sidecar"))
            (should-error
             (mevedel-session-durability-read-publication session-dir)
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
             (mevedel-session-durability-read-publication session-dir)
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
                  (mevedel-session-durability-publish
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
             (mevedel-session-durability-read-publication session-dir)
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
                  (mevedel-session-durability-publish
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
             (mevedel-session-durability-read-publication session-dir)
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
           (mevedel-session-durability-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (rename-file publication-dir outside-dir)
          (make-symbolic-link outside-dir publication-dir)
          (should-error
           (mevedel-session-durability-read-publication session-dir)
           :type 'error))
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-seed-publication-base ()
  ,test
  (test)
  :doc "one-shot base validates copied bytes but leaves the child unpublished"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "mevedel-publication-seed-" t)))
         (source-dir (file-name-as-directory
                      (file-name-concat local-root "source")))
         (child-dir (file-name-as-directory
                     (file-name-concat local-root "child")))
         (bad-dir (file-name-as-directory
                   (file-name-concat local-root "bad-child")))
         (fail-dir (file-name-as-directory
                    (file-name-concat local-root "fail-child")))
         (source (test-mevedel-session-durability--local-session local-root))
         (child (test-mevedel-session-durability--local-session local-root))
         (bad-child
          (test-mevedel-session-durability--local-session local-root))
         (fail-child
          (test-mevedel-session-durability--local-session local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         publication
         head)
    (make-directory source-dir t)
    (make-directory child-dir t)
    (make-directory bad-dir t)
    (make-directory fail-dir t)
    (setf (mevedel-session-save-path source) source-dir
          (mevedel-session-save-path child) child-dir
          (mevedel-session-save-path bad-child) bad-dir
          (mevedel-session-save-path fail-child) fail-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   source-dir "*source*" source))
          (should
           (mevedel-session-durability-publish
            source
            (list (list :path (file-name-concat source-dir "segment.chat.org")
                        :content "segment")
                  (list :path (file-name-concat source-dir "session.meta.el")
                        :content "sidecar" :commit-marker t))))
          (setq publication
                (mevedel-session-durability-read-publication source-dir)
                head (plist-get publication :head))
          (mevedel-session-durability-lease-release source-dir source)
          (copy-directory (file-name-concat source-dir ".publications")
                          (file-name-concat child-dir ".publications") t t)
          (copy-directory (file-name-concat source-dir ".publications")
                          (file-name-concat bad-dir ".publications") t t)
          (copy-directory (file-name-concat source-dir ".publications")
                          (file-name-concat fail-dir ".publications") t t)
          (should (mevedel-session-durability-lease-acquire
                   child-dir "*child*" child))
          (should
           (equal head
                  (plist-get
                   (mevedel-session-durability-seed-publication-base
                    child head)
                   :head)))
          (should-not
           (mevedel-session-durability-read-publication child-dir))
          (should
           (mevedel-session-durability-publish
            child
            (list (list :path (file-name-concat child-dir "session.meta.el")
                        :content "child sidecar" :commit-marker t))))
          (should
           (equal (mevedel-session-durability-read-publication child-dir)
                  (mevedel-session-publication child)))
          (should
           (equal '("segment.chat.org" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get (mevedel-session-publication child)
                              :artifacts))))
          (should
           (equal "child sidecar"
                  (with-temp-buffer
                    (insert-file-contents
                     (plist-get (mevedel-session-publication child) :sidecar))
                    (buffer-string))))
          (should-error
           (mevedel-session-durability-seed-publication-base child head)
           :type 'error)
          (let* ((segment
                  (cdr (assoc "segment.chat.org"
                              (plist-get publication :artifacts))))
                 (relative
                  (file-relative-name
                   (plist-get segment :published) source-dir))
                 (bad-path (file-name-concat bad-dir relative)))
            (with-temp-file bad-path (insert "corrupt")))
          (should (mevedel-session-durability-lease-acquire
                   bad-dir "*bad-child*" bad-child))
          (should-error
           (mevedel-session-durability-seed-publication-base
            bad-child head)
           :type 'error)
          (should-not
           (mevedel-session-durability-publication-head bad-dir))
          (should-not (mevedel-session-publication bad-child))
          (should-not (mevedel-session-pending-publication bad-child))
          (should (mevedel-session-durability-lease-acquire
                   fail-dir "*fail-child*" fail-child))
          (should
           (mevedel-session-durability-seed-publication-base
            fail-child head))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-durability-publish
              fail-child
              (list
               (list :path (file-name-concat fail-dir "session.meta.el")
                     :content "failed sidecar" :commit-marker t)))
             :type 'user-error))
          (should-not
           (mevedel-session-durability-publication-head fail-dir))
          (should (mevedel-session-pending-publication fail-child)))
      (mevedel-session-durability-lease-release child-dir child)
      (mevedel-session-durability-lease-release bad-dir bad-child)
      (mevedel-session-durability-lease-release fail-dir fail-child)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-logical-path-p ()
  ,test
  (test)
  :doc "accepts normalized session artifacts outside durability control paths"
  (dolist (path '("session.meta.el" "agents/worker.chat.org"
                  "instructions/current.el"))
    (should (mevedel-session-durability-logical-path-p path)))
  (dolist (path '(nil "" "/absolute" "../escape" "a/../escape"
                  ".lease/0001.el" ".publications/generation/file"
                  "/ssh:other:/session.meta.el" "~/session.meta.el"))
    (should-not (mevedel-session-durability-logical-path-p path))))

(mevedel-deftest mevedel-session-durability-uncommitted-artifact ()
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
           (mevedel-session-durability-publish
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
                 (mevedel-session-durability-uncommitted-artifact
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
               (mevedel-session-durability-publish
                session (list (list :path target :content "queued")))))
          (should
           (equal "queued"
                  (with-temp-buffer
                    (insert-file-contents-literally
                     (mevedel-session-durability-uncommitted-artifact
                      session logical))
                    (buffer-string))))
          (should-not
           (mevedel-session-durability-uncommitted-artifact
            session "missing.el"))
          (should-not
           (mevedel-session-durability-uncommitted-artifact
            session "external.el"))
          (should-error
           (mevedel-session-durability-uncommitted-artifact
            session "../external.el")
           :type 'error))
      (setf (mevedel-session-publication-active-p session) nil)
      (mevedel-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-discard-rolled-back-publication ()
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
           (mevedel-session-durability-publish
            session
            (list (list :path sidecar :content "old" :commit-marker t))))
          (setq old-head
                (mevedel-session-durability-publication-head session-dir))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-durability-publish
              session
              (list (list :path sidecar :content "new" :commit-marker t)))
             :type 'user-error))
          (setq recovery
                (mapcar
                 (lambda (batch) (plist-get batch :directory))
                 (plist-get (mevedel-session-pending-publication session)
                            :batches)))
          (should (cl-every #'file-directory-p recovery))
          (should
           (mevedel-session-durability-discard-rolled-back-publication
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
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         (prompts 0))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
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

(mevedel-deftest mevedel-session-durability-publish ()
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
           (mevedel-session-durability-publish
            session
            (list (list :path duplicate :content "first")
                  (list :path external :content "external")
                  (list :path duplicate :content "last")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (mevedel-session-durability-read-publication session-dir))
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
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (make-nearby (symbol-function 'make-nearby-temp-file))
         (immutable-directories 0))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'make-nearby-temp-file)
                (lambda (prefix &optional directory suffix)
                  (when (and directory
                             (string-match-p "/\\.publications/" prefix))
                    (cl-incf immutable-directories))
                  (funcall make-nearby prefix directory suffix))))
            (should
             (mevedel-session-durability-publish
              session
              (list
               (list :path (file-name-concat session-dir "session.meta.el")
                     :content "sidecar" :commit-marker t)))))
          (should (= 1 immutable-directories))
          (should (file-remote-p
                   (plist-get
                    (mevedel-session-durability-read-publication session-dir)
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
           (mevedel-session-durability-publish
            session
            (list (list :path first :content "first")
                  (list :path second :content "old second")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (should
           (mevedel-session-durability-publish
            session
            (list (list :path second :content "new second")
                  (list :path sidecar :content "new sidecar"
                        :commit-marker t :replace t))))
          (let ((artifacts
                 (plist-get
                  (mevedel-session-durability-read-publication session-dir)
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
          (symbol-function 'mevedel-session-durability--publish-artifact))
         injected)
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-durability--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact artifact)
                    (unless injected
                      (setq injected t)
                      (should
                       (eq 'queued
                           (mevedel-session-durability-publish
                            session
                            (list (list :path second :content "second")))))
                      (should
                       (eq 'queued
                           (mevedel-session-durability-publish
                            session
                            (list
                             (list :path sidecar :content "sidecar"
                                   :commit-marker t))))))))))
            (should
             (mevedel-session-durability-publish
              session (list (list :path first :content "first")))))
          (should
           (equal '("first.el" "second.el" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (mevedel-session-durability-read-publication session-dir)
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
             (mevedel-session-durability-publish session artifacts)
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
           (mevedel-session-durability-publish
            session
            (list (list :path segment :content "old segment")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (let* ((old (mevedel-session-durability-read-publication
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
             (mevedel-session-durability-publish
              session (list (list :path segment :content "new segment"))))
            (should (equal "new segment"
                           (with-temp-buffer
                             (insert-file-contents segment)
                             (buffer-string))))
            (should
             (equal old
                    (mevedel-session-durability-read-publication session-dir)))
            (should
             (mevedel-session-durability-publish
              session
              (list (list :path sidecar :content "new sidecar"
                          :commit-marker t))))
            (let* ((new (mevedel-session-durability-read-publication
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
           (mevedel-session-durability-publish
            session (list (list :path segment :content "segment"))))
          (cl-letf
              (((symbol-function
                 'mevedel-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-durability-publish
              session
              (list (list :path sidecar :content "sidecar"
                          :commit-marker t)))
             :type 'user-error))
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
           (mevedel-session-durability-retry-publication session))
          (should-not (mevedel-session-pending-publication session))
          (should (cl-every (lambda (path) (not (file-exists-p path)))
                            recovery))
          (should
           (equal '("segment.chat.org" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (mevedel-session-durability-read-publication session-dir)
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
             (mevedel-session-durability-publish
              session
              (list (list :path sidecar :content "committed"
                          :commit-marker t)))
             :type 'user-error))
          (let ((publication
                 (mevedel-session-durability-read-publication session-dir)))
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
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (before (directory-files temporary-file-directory nil
                                  "\\`mevedel-publication-")))
    (make-directory session-dir-local t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (should (mevedel-session-durability-publish
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
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (should-error
           (mevedel-session-durability-publish
            session
            (list (list :path first :content "first")
                  (list :path second :content "second")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t)))
           :type 'file-error)
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
             (mevedel-session-durability-retry-publication session))
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
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (test-mevedel-session-durability--accept-storage session)
          (setf (mevedel-session-save-path session) session-dir)
          (should (mevedel-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (should-error
           (mevedel-session-durability-publish
            session (list (list :path target :content "state")))
           :type 'file-error)
          (let* ((pending (mevedel-session-pending-publication session))
                 (recovery
                  (plist-get (car (plist-get pending :batches)) :directory)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (should
               (mevedel-session-durability-abandon-publication session)))
            (should-not (file-exists-p recovery)))
          (should-not (mevedel-session-pending-publication session))
          (should
           (mevedel-session-persistence-assert-mutation-authority session))
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
          (symbol-function 'mevedel-session-durability--publish-artifact))
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (cl-incf takeover-prompts)
                     t)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-durability--publish-artifact)
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
             (mevedel-session-durability-publish
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
            (should-error
             (mevedel-session-durability-publish
              session (list (list :path target :content "state")))
             :type 'user-error))
          (should-not (file-exists-p target))
          (let* ((pending (mevedel-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))))
      (when-let ((pending (mevedel-session-pending-publication session)))
        (dolist (batch (plist-get pending :batches))
          (mevedel-session-durability--delete-batch batch)))
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
          (symbol-function 'mevedel-session-durability--publish-artifact)))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (should (mevedel-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'mevedel-session-durability--publish-artifact)
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
            (should-error
             (mevedel-session-durability-publish
              session (list (list :path target :content "state")))
             :type 'user-error))
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
          (mevedel-session-durability--delete-batch batch)))
      (mevedel-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-durability-append-diagnostic
  ()
  ,test
  (test)
  :doc "retries a failed remote diagnostic atomically without blocking mutation"
  (let* ((host "diagnostic-retry-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-diagnostic-" t)))
         (session
          (test-mevedel-session-durability--remote-session host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
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
                     'mevedel-session-durability--publish-artifact))
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
                         session (current-buffer))))
              (should warning)
              (should (mevedel-session-hook-log-pending session))
              (should-not (mevedel-session-pending-publication session))
              (should
               (mevedel-session-persistence-assert-mutation-authority session))
              (delete-directory log-path)
              (cl-letf
                  (((symbol-function
                     'mevedel-session-durability--publish-artifact)
                    (lambda (artifact)
                      (when (equal log-path (plist-get artifact :path))
                        (cl-incf diagnostic-publications))
                      (funcall publish-artifact artifact))))
                (should (mevedel-session-persistence-save
                         session (current-buffer))))
              (should (= 1 diagnostic-publications))
              (should-not (mevedel-session-hook-log-pending session))
              (should-not (mevedel-session-pending-publication session))
              (with-temp-buffer
                (insert-file-contents log-path)
                (should (equal entry (read (current-buffer)))))
              (mevedel-session-persistence-lock-release save-path session))))
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
          (symbol-function 'mevedel-session-durability--publish-artifact))
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
              (((symbol-function 'mevedel-session-durability--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact artifact)
                    (unless injected
                      (setq injected t
                            fail-finish t)
                      (should
                       (eq 'queued
                           (mevedel-session-durability-publish
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
            (should-error
             (mevedel-session-durability-append-diagnostic
              session diagnostic "diagnostic")
             :type 'user-error)
            (let ((batches
                   (plist-get (mevedel-session-pending-publication session)
                              :batches)))
              (should (= 1 (length batches)))
              (should (file-directory-p (plist-get (car batches) :directory)))
              (should (mevedel-session-durability--batch-live-p
                       (car batches))))
            (should
             (eq 'published
                 (mevedel-session-durability-retry-publication session)))
            (should-not (mevedel-session-pending-publication session))
            (let ((batches
                   (mevedel-session-publication-uncommitted-batches session)))
              (should (= 1 (length batches)))
              (should (mevedel-session-durability--batch-live-p
                       (car batches)))))
          (mevedel-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(mevedel-deftest mevedel-session-persistence-publish-agent-terminal-state
  (:doc "publishes transcript and final registry sidecar as one retryable batch")
  (let* ((host "agent-terminal-host")
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
           (mevedel-session-durability-publish
            session
            (list (list :path sidecar :content "(:materialized t)"
                        :commit-marker t))))
          (let ((publish-artifact
                 (symbol-function
                  'mevedel-session-durability--publish-artifact))
                (count 0))
            (cl-letf
                (((symbol-function
                   'mevedel-session-durability--publish-artifact)
                  (lambda (artifact)
                    (cl-incf count)
                    (if (= count 2)
                        (signal 'file-error
                                '("Injected agent sidecar failure"))
                      (funcall publish-artifact artifact)))))
              (should-error
               (mevedel-session-persistence-publish-agent-terminal-state
                invocation)
               :type 'file-error)))
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
          (should (mevedel-session-durability-retry-publication session))
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
         (session (test-mevedel-session-durability--remote-session
                   host local-root))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
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
                (should-error
                 (mevedel-session-persistence-rotate-segment
                  session (current-buffer) "Pending handoff.")
                 :type 'file-error)
                (should (mevedel-session-pending-publication session))
                (should (= 2 (mevedel-session-current-segment session)))
                (should (equal new buffer-file-name))
                (should (string-match-p "Pending handoff" (buffer-string)))
                (delete-file instructions)
                (make-directory instructions)
                (should
                 (mevedel-session-durability-retry-publication session))
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

(mevedel-deftest mevedel-session-durability-record-specialized-failure ()
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
          (mevedel-session-durability-record-specialized-failure
           session "rewind rollback incomplete" "/target/recovery")
          (should (mevedel-session-pending-publication session))
          (should-error
           (mevedel-session-durability-retry-publication session)
           :type 'user-error)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (should
             (mevedel-session-durability-abandon-publication session)))
          (should-not (mevedel-session-pending-publication session)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-durability-status ()
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
          (let ((status (mevedel-session-durability-status session)))
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
