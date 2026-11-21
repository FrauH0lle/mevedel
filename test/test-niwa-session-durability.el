;;; test-niwa-session-durability.el --- Portable session durability -*- lexical-binding: t -*-

;;; Commentary:

;; Covers portable leases, serialized publication, and recovery boundaries.

;;; Code:

(require 'gptel-request)
(require 'niwa-agent-control)
(require 'niwa-session-persistence)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-session-recovery)
(require 'niwa-session-transfer)
(require 'niwa-session-publication)
(require 'niwa-session-fork)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-structs)
(require 'niwa-turn)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-niwa-session-durability--remote-session (host local-root)
  "Return a session rooted at LOCAL-ROOT through mock TRAMP HOST."
  (let* ((root (format "/niwamock:%s:%s/" host local-root))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "remote")))
    (let ((session (niwa-session-create "main" workspace)))
      (setf (niwa-execution-target-incarnation
             (niwa-session-execution-target session))
            (format "mock-incarnation-%s" host))
      session)))

(defun test-niwa-session-durability--local-session (root)
  "Return a session rooted at local directory ROOT."
  (let ((workspace
         (niwa-workspace--create
          :type 'project :id root :root root :name "local")))
    (niwa-session-create "main" workspace)))

(defun test-niwa-session-durability--accept-storage (session)
  "Mark SESSION's target storage disclosure accepted for this test."
  (puthash
   (niwa-execution-target-identity
   (niwa-session-execution-target session))
   t niwa-session-durability--disclosed-targets))

(niwa-deftest niwa-session-transfer--directory
  (:doc "treats a concurrent creator as success only when it made a directory")
  (let* ((root (make-temp-file "niwa-transfer-directory-" t))
         (real-make-directory (symbol-function 'make-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'make-directory)
                   (lambda (path &optional parents)
                     (funcall real-make-directory path parents)
                     (signal 'file-already-exists (list path)))))
          (should
           (file-directory-p
            (niwa-session-transfer--directory
             root "requests"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-transfer--path
  (:doc "names a transfer mailbox without creating it")
  (let ((root (make-temp-file "niwa-transfer-path-" t)))
    (unwind-protect
        (let ((path (niwa-session-transfer--path root "requests")))
          (should (equal (file-name-concat root "requests") path))
          ;; A poll must not mutate the target on behalf of an observer, and
          ;; an absent mailbox already reads as "no requests".
          (should-not (file-exists-p path))
          (should-not
           (niwa-session-control-fs-list-directory path "\\`request-")))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-transfer--valid-request-p
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
    (should (niwa-session-transfer--valid-request-p record now))
    (dolist (value (list "Laptop\n[g]rant" (make-string 129 ?x)))
      (should-not
       (niwa-session-transfer--valid-request-p
        (plist-put (copy-sequence record) :requester-label value)
        now)))
    (dolist (value (list -1 (read "0.0e+NaN") (read "1.0e+INF")))
      (should-not
       (niwa-session-transfer--valid-request-p
        (plist-put (copy-sequence record) :deadline value)
        now)))
    (should-not
     (niwa-session-transfer--valid-request-p
      (plist-put (copy-sequence record) :deadline (+ now 301))
      now))
    (should-not
     (niwa-session-transfer--valid-request-p
      (let ((copy (copy-sequence record)))
        (plist-put copy :created-at (+ now 301))
        (plist-put copy :deadline (+ now 302)))
      now))))

(niwa-deftest niwa-session-transfer-poll ()
  ,test
  (test)
  :doc "skips the target program while a local mailbox stays empty"
  (let* ((root (make-temp-file "niwa-transfer-poll-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "poll"))
         (owner (niwa-session-create "main" workspace))
         (requester (niwa-session-create "requester" workspace))
         (programs 0)
         (count (lambda (original &rest args)
                  (cl-incf programs)
                  (apply original args))))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) "poll-session"
          (niwa-session-save-path requester) session-dir
          (niwa-session-session-id requester) "poll-session")
    (unwind-protect
        (let ((niwa-session-durability--client-id (make-string 64 ?a)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "owner" owner))
          (advice-add 'niwa-session-control-fs-run-program :around count)
          ;; The first poll observes the target; the next ones do not.
          (should-not (plist-get (niwa-session-transfer-poll owner) :state))
          (should (> programs 0))
          (setq programs 0)
          (dotimes (_ 3) (niwa-session-transfer-poll owner))
          (should (= 0 programs))
          ;; A request changes the mailbox and is seen on the next poll.
          (let ((niwa-session-durability--client-id (make-string 64 ?b)))
            (should (niwa-session-control-transfer-request requester)))
          (should (eq 'requested
                      (plist-get (niwa-session-transfer-poll owner) :state)))
          ;; While a transfer is in flight every poll reads the target.
          (setq programs 0)
          (niwa-session-transfer-poll owner)
          (should (> programs 0))
          ;; A requester with stale local history cannot cause a native
          ;; conversation to be handed off by the automatic grant timer.
          (let ((lease (copy-tree (niwa-session-lease owner))))
            (setf (niwa-session-external-conversations owner)
                  (list (list "root" :engine 'claude-code :id "native"
                              :host (system-name) :directory root :state 'ready)))
            (should (eq 'rejected
                        (plist-get (niwa-session-transfer-poll owner) :state)))
            (should-error (niwa-session-transfer-decide owner 'grant) :type 'user-error)
            (should-error (niwa-session-transfer-release owner) :type 'user-error)
            (should (equal lease (niwa-session-lease owner)))))
      (advice-remove 'niwa-session-control-fs-run-program count)
      (niwa-session-durability--cancel-renewal owner)
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-control-transfer ()
  ,test
  (test)
  :doc "atomically requests, decides, fences, and hands off one lease generation"
  (let* ((root (make-temp-file "niwa-control-transfer-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "control"))
         (owner (niwa-session-create "main" workspace))
         (session-id "control-session")
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b))
         (other-id (make-string 64 ?c))
         (now 0.0)
         (results nil)
         (mutex (make-mutex "control-transfer-race"))
         threads)
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) session-id)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&rest _) now)))
          (let ((niwa-session-durability--client-id owner-id))
          (should
           (niwa-session-durability-lease-acquire
            session-dir "owner" owner))
          (let ((niwa-session-durability--client-id requester-id)
                (niwa-session-transfer-client-label "Laptop\n[g]rant"))
            (should-error
             (niwa-session-transfer-request
              owner)))
          (let ((niwa-session-durability--client-id requester-id)
                (niwa-session-transfer-prompt-timeout 301))
            (should-error
             (niwa-session-transfer-request
              owner)))
          (dolist (client (list requester-id other-id))
            (let ((client-id client))
              (push
               (make-thread
                (lambda ()
                  (let ((niwa-session-durability--client-id client-id))
                    (let ((request
                           (niwa-session-transfer-request
                            owner)))
                      (with-mutex mutex
                        (push request results))))))
               threads)))
          (mapc #'thread-join threads)
          (should (= 1 (length (delq nil results))))
          (let* ((request (car (delq nil results)))
                 (winner (plist-get request :requester-client-id))
                 (directory (niwa-session-durability--lease-path
                             session-dir))
                 (decision-path
                  (niwa-session-transfer--decision-path
                   directory (plist-get request :generation))))
            (should (equal "Emacs" (plist-get request :requester-label)))
            (let ((niwa-session-durability--client-id winner))
              (should-not
               (niwa-session-durability-lease-acquire
                session-dir "before-release")))
            (should (eq 'requested
                        (plist-get
                         (niwa-session-transfer-poll owner)
                         :state)))
            (let ((decision
                   (niwa-session-transfer-decide
                    owner 'grant))
                  (before nil))
              (setq before
                    (with-temp-buffer
                      (insert-file-contents-literally decision-path)
                      (buffer-string)))
              (should-error
               (niwa-session-transfer-decide
                owner 'reject))
              (should (equal before
                             (with-temp-buffer
                               (insert-file-contents-literally decision-path)
                               (buffer-string))))
              (should (eq 'grant (plist-get decision :decision))))
            (should (eq 'quiescing
                        (plist-get (niwa-session-control-transfer owner)
                                   :state)))
            (let ((fence
                   (niwa-session-transfer-release owner)))
              (should (numberp (plist-get fence :expires-at)))
              (should
               (equal fence
                      (niwa-session-transfer-release
                       owner)))
              (let ((niwa-session-durability--client-id
                     (if (equal winner requester-id) other-id requester-id)))
                (should-not
                 (niwa-session-durability-lease-acquire
                  session-dir "fenced"))))
            (let ((niwa-session-durability--client-id winner))
              (should
               (niwa-session-durability-lease-acquire
                session-dir "successor" owner))
              ;; A released generation without a live transfer fence retains
              ;; the ordinary one-client successor path.
              (niwa-session-durability-lease-release session-dir owner)
              (should (niwa-session-durability-lease-acquire
                       session-dir "ordinary-successor" owner))
              (let ((niwa-session-durability--client-id
                     (if (equal winner requester-id)
                         other-id requester-id))
                    (niwa-session-transfer-prompt-timeout 0.01))
                (should
                 (niwa-session-transfer-request
                  owner)))
              (setq now 1.0)
              (let ((niwa-session-transfer-prompt-timeout 0.01))
                ;; The first poll is the one that could display the request,
                ;; so the notice window starts there rather than at the
                ;; requester's clock.
                (should
                 (eq 'requested
                     (plist-get
                      (niwa-session-transfer-poll owner)
                      :state)))
                (setq now 2.0)
                (should
                 (eq 'quiescing
                     (plist-get
                      (niwa-session-transfer-poll owner)
                      :state)))))))))
      (niwa-session-durability--cancel-renewal owner)
      (when (file-directory-p root)
        (delete-directory root t)))
  :doc "Keep control rotates the generation so a later request can succeed"
  (let* ((root (make-temp-file "niwa-control-keep-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "keep"))
         (owner (niwa-session-create "main" workspace))
         (requester (niwa-session-create "requester" workspace))
         (buffer (generate-new-buffer " *niwa-control-requester*"))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) "keep-session"
          (niwa-session-save-path requester) session-dir
          (niwa-session-session-id requester) "keep-session")
    (unwind-protect
        (let ((niwa-session-durability--client-id owner-id))
          (should
           (niwa-session-durability-lease-acquire
            session-dir "owner" owner))
          (let ((niwa-session-durability--client-id requester-id))
            (should
             (niwa-session-control-transfer-request requester)))
          (should
           (eq 'requested
               (plist-get
                (niwa-session-transfer-poll owner)
                :state)))
          (let ((claim-next
                 (symbol-function
                  'niwa-session-durability--claim-next))
                block-rotation)
            (setq block-rotation t)
            (cl-letf
                (((symbol-function 'niwa-session-durability--claim-next)
                  (lambda (&rest args)
                    (unless block-rotation
                      (apply claim-next args)))))
              (should-error
               (niwa-session-transfer-decide owner 'reject))
              (let ((niwa-session-durability--client-id requester-id))
                (should-not
                 (niwa-session-control-transfer-request requester)))
              (setq block-rotation nil)
              (let ((rejected
                     (niwa-session-transfer-decide owner 'reject)))
                (should
                 (equal rejected
                        (niwa-session-transfer-decide
                         owner 'reject))))))
          (let ((generation
                 (plist-get (niwa-session-lease owner) :generation)))
            (should (> generation 1))
            (let ((niwa-session-durability--client-id requester-id))
              (with-current-buffer buffer
                (setq buffer-read-only t)
                (should
                 (niwa-session-control-transfer--poll-requester
                  requester buffer)))
              (should
               (eq 'rejected
                   (plist-get
                    (niwa-session-control-transfer requester) :state)))
              (should
               (= generation
                  (plist-get
                   (niwa-session-control-transfer-request requester)
                   :generation))))))
      (let ((niwa-session-durability--client-id owner-id))
        (niwa-session-durability-lease-release session-dir owner))
      (niwa-session-control-transfer-unregister-root-buffer
       requester buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))
  :doc "a pending request survives the owner's own generation rotation"
  (let* ((root (make-temp-file "niwa-control-drift-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "drift"))
         (owner (niwa-session-create "main" workspace))
         (owner-id (make-string 64 ?a))
         (requester-id (make-string 64 ?b))
         (other-id (make-string 64 ?c))
         (now 0.0))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) "drift-session")
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&rest _) now)))
          (let ((niwa-session-durability--client-id owner-id)
                (niwa-session-transfer-prompt-timeout 1)
                request)
            (should
             (niwa-session-durability-lease-acquire
              session-dir "owner" owner))
            (let ((niwa-session-durability--client-id requester-id))
              (setq request (niwa-session-transfer-request owner))
              (should request))
            ;; Settling the durable mutation latch rotates the owner's
            ;; lease generation past the pending request.
            (should
             (niwa-session-durability-set-unsettled-mutation owner t))
            (should
             (niwa-session-durability-set-unsettled-mutation owner nil))
            (let ((niwa-session-durability--client-id requester-id))
              (should
               (equal request (niwa-session-transfer-request owner))))
            (let ((niwa-session-durability--client-id other-id))
              (should-not (niwa-session-transfer-request owner)))
            (should (eq 'requested
                        (plist-get (niwa-session-transfer-poll owner)
                                   :state)))
            (should
             (equal (plist-get request :request-id)
                    (plist-get
                     (plist-get (niwa-session-control-transfer owner)
                                :request)
                     :request-id)))
            (setq now 2.0)
            (should (eq 'quiescing
                        (plist-get (niwa-session-transfer-poll owner)
                                   :state)))
            (let ((release
                   (symbol-function 'niwa-session-durability-lease-release))
                  failed)
              (cl-letf
                  (((symbol-function
                     'niwa-session-durability-lease-release)
                    (lambda (&rest args)
                      (if failed
                          (apply release args)
                        (setq failed t)
                        (error "Injected post-fence failure")))))
                (should-error (niwa-session-transfer-release owner))
                (let ((release-generation
                       (plist-get
                        (niwa-session-control-transfer owner)
                        :release-generation)))
                  (should (> release-generation
                             (plist-get request :generation)))
                  (let ((fence (niwa-session-transfer-release owner)))
                    (should (= release-generation
                               (plist-get fence :generation)))
                    (should
                     (equal fence
                            (niwa-session-transfer-release owner)))))))))
      (niwa-session-durability--cancel-renewal owner)
      (let ((niwa-session-durability--client-id owner-id))
        (ignore-errors
          (niwa-session-durability-lease-release session-dir owner)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "one request wins when lease rotation splits two contenders"
  (let* ((root (make-temp-file "niwa-control-rotation-race-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "rotation-race"))
         (owner (niwa-session-create "main" workspace))
         (owner-id (make-string 64 ?a))
         (first-id (make-string 64 ?b))
         (second-id (make-string 64 ?c))
         first-result second-result)
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) "rotation-race-session")
    (unwind-protect
        (let ((niwa-session-durability--client-id owner-id))
          (should
           (niwa-session-durability-lease-acquire
            session-dir "owner" owner))
          (let ((create
                 (symbol-function
                  'niwa-session-durability--create-plist))
                injected)
            (cl-letf
                (((symbol-function 'niwa-session-durability--create-plist)
                  (lambda (path record)
                    (when (and (not injected)
                               (string-match-p "/requests/request-" path))
                      (setq injected t)
                      (let ((niwa-session-durability--client-id owner-id))
                        (should
                         (niwa-session-durability-set-unsettled-mutation
                          owner t)))
                      (let ((niwa-session-durability--client-id second-id))
                        (setq second-result
                              (niwa-session-transfer-request owner))))
                    (funcall create path record))))
              (let ((niwa-session-durability--client-id first-id))
                (setq first-result
                      (niwa-session-transfer-request owner))))
            (should injected)
            (should-not first-result)
            (should (equal second-id
                           (plist-get second-result :requester-client-id)))
            (should
             (= (plist-get (niwa-session-lease owner) :transfer-generation)
                (plist-get second-result :generation)))
            (should (eq 'requested
                        (plist-get (niwa-session-transfer-poll owner)
                                   :state)))
            (should
             (equal (plist-get second-result :request-id)
                    (plist-get
                     (plist-get (niwa-session-control-transfer owner)
                                :request)
                     :request-id)))))
      (niwa-session-durability--cancel-renewal owner)
      (let ((niwa-session-durability--client-id owner-id))
        (ignore-errors
          (niwa-session-durability-lease-release session-dir owner)))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))
  :doc "rejects portable lease and transfer APIs for file sessions"
  (let* ((root (make-temp-file "niwa-file-transfer-" t))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "file"))
         (session (niwa-session-create "file" workspace))
         (session-dir (file-name-concat root "session")))
    (setf (niwa-session-save-path session) session-dir
          (niwa-session-session-id session) "file-session")
    (unwind-protect
        (progn
          (should-error
           (niwa-session-durability-lease-acquire
            session-dir "file" session))
          (should-error
           (niwa-session-transfer-request session)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-control-transfer-validation
  (:doc "fails closed for malformed current-generation transfer records")
  (let* ((root (make-temp-file "niwa-control-transfer-invalid-" t))
         (session-dir (file-name-concat root "session"))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "control"))
         (owner (niwa-session-create "main" workspace))
         (session-id "invalid-session")
         (owner-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner) session-dir
          (niwa-session-session-id owner) session-id)
    (unwind-protect
        (let ((niwa-session-durability--client-id owner-id))
          (should (niwa-session-durability-lease-acquire
                   session-dir "owner" owner))
          (let* ((directory (niwa-session-durability--lease-path
                             session-dir))
                 (request-path
                  (niwa-session-transfer--request-path
                   directory 1)))
            (niwa-session-transfer--directory directory "requests")
            (niwa-session-durability--write-plist
             request-path '(:protocol-version 0))
            (should-error
             (niwa-session-transfer-poll owner))))
      (niwa-session-durability--cancel-renewal owner)
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-run-owned-program-async ()
  ,test
  (test)
  :doc "runs operations behind the proof and strips bytes it disproves"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-owned-async-" t)))
         (session (test-niwa-session-durability--local-session root))
         (log (file-name-concat root "log"))
         (niwa-session-durability--client-id (make-string 64 ?a))
         settled)
    (setf (niwa-session-save-path session) root)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire root "*owner*" session))
          (should (plist-get (niwa-session-lease session) :bytes))
          (niwa-session-durability-run-owned-program-async
           session (list (list :op 'append :path log :content "a"))
           (lambda (results error) (setq settled (list results error))))
          (let ((steps 0))
            (while (and (null settled) (< (cl-incf steps) 500))
              (accept-process-output nil 0.01)))
          (should (equal 'ok (plist-get (car (car settled)) :status)))
          (should (equal "a" (niwa-session-control-fs-read-file log)))
          ;; Bytes the target no longer holds fail the proof and are
          ;; forgotten, so the next renewal observes instead of assuming.
          (setf (niwa-session-lease session)
                (plist-put (copy-sequence (niwa-session-lease session))
                           :bytes "(:stale t)"))
          (setq settled nil)
          (niwa-session-durability-run-owned-program-async
           session (list (list :op 'append :path log :content "b"))
           (lambda (results error) (setq settled (list results error))))
          (let ((steps 0))
            (while (and (null settled) (< (cl-incf steps) 500))
              (accept-process-output nil 0.01)))
          (should (equal '(nil nil) settled))
          (should-not (plist-get (niwa-session-lease session) :bytes))
          (should (equal "a" (niwa-session-control-fs-read-file log))))
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors (niwa-session-durability-lease-release root session))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability--claim-next ()
  ,test
  (test)
  :doc "one claim is bounded by three target programs"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-claim-cost-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".lease")))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (programs 0)
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory directory t)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &optional lock-directory)
                       (cl-incf programs)
                       (funcall run-program-function operations lock-directory))))
            (should (niwa-session-durability--claim-next
                     directory nil "*claim-cost*")))
          ;; Observation, fencing create with predecessor reads, and the
          ;; settling write with its prunes: one program each.
          (should (<= programs 3)))
      (delete-directory root t)))

  :doc "an observed live predecessor is proved by bytes in the claim's program"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-claim-proof-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".lease")))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory directory t)
    (unwind-protect
        (let* ((first (niwa-session-durability--claim-next
                       directory nil "*claim-proof*"))
               (observed (niwa-session-durability--observe-lease
                          directory (plist-get first :generation)))
               (programs 0))
          (should (equal first (plist-get observed :record)))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &rest args)
                       (cl-incf programs)
                       (apply run-program-function operations args))))
            (let ((second (niwa-session-durability--claim-next
                           directory first "*claim-proof*" nil nil nil nil
                           observed)))
              (should (eq 'active (plist-get second :status)))
              (should (= 2 (plist-get second :generation)))))
          (should (= 1 programs))
          ;; The predecessor was pruned by the same program.
          (should-not (file-exists-p
                       (niwa-session-durability--generation-path
                        directory (plist-get first :generation))))
          ;; A predecessor that changed after its observation fails the
          ;; proof and leaves the fenced claim aborted.
          (let* ((second (niwa-session-durability--lease-head directory))
                 (observed (niwa-session-durability--observe-lease
                            directory (plist-get second :generation)))
                 (path (niwa-session-durability--generation-path
                        directory (plist-get second :generation))))
            (with-temp-file path
              (insert (niwa-session-durability--record-bytes
                       (plist-put (copy-sequence second) :buffer "*changed*"))))
            (should-not (niwa-session-durability--claim-next
                         directory second "*claim-proof*" nil nil nil nil
                         observed))
            (should-not (file-exists-p
                         (niwa-session-durability--generation-path
                          directory (1+ (plist-get second :generation)))))
            (should (equal "*changed*"
                           (plist-get (niwa-session-durability--lease-head
                                       directory)
                                      :buffer)))))
      (delete-directory root t))))

(defun test-niwa-session-durability--with-holder (function)
  "Call FUNCTION with a live `sleep' process and its session directory.
FUNCTION receives the directory and the process.  The process's real
holder identity is recorded by an owner lease acquired before the call."
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-dead-holder-" t)))
         (process (make-process :name "niwa-holder" :command '("sleep" "60")
                                :noquery t :connection-type 'pipe)))
    (unwind-protect
        (progn
          (unless (niwa-session-durability--process-holder
                   (process-id process))
            (ert-skip "No provable process identity on this host"))
          (let ((niwa-session-durability--client-id (make-string 64 ?a))
                (holder (niwa-session-durability--process-holder
                         (process-id process))))
            ;; The real writer records the child as this lease's holder.
            (cl-letf (((symbol-function 'niwa-session-durability--holder)
                       (lambda () holder)))
              (should (niwa-session-durability-lease-acquire root "*owner*"))))
          (funcall function root process))
      (when (process-live-p process) (delete-process process))
      (delete-directory root t))))

(defun test-niwa-session-durability--kill-holder (process)
  "Kill PROCESS and wait until its PID is provably gone."
  (let ((pid (process-id process)))
    (delete-process process)
    (let ((deadline (+ (float-time) 5)))
      (while (and (niwa-session-durability--pid-alive-p pid)
                  (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    ;; The injection is reached: the holder's PID really is gone.
    (should-not (niwa-session-durability--pid-alive-p pid))))

(niwa-deftest niwa-session-durability-lease-acquire ()
  ,test
  (test)
  :doc "reclaims an unexpired lease at once when its same-host holder died"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (test-niwa-session-durability--kill-holder process)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (&rest _) (error "A dead holder needs no confirmation"))))
       (let ((niwa-session-durability--client-id (make-string 64 ?b)))
         (should (niwa-session-durability-lease-acquire root "*new*"))
         (should (equal (make-string 64 ?b)
                        (plist-get (niwa-session-durability--lease-head
                                    (niwa-session-durability--lease-path root))
                                   :client-id)))))))

  :doc "reclaims an expired dead holder without asking"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory)))
       ;; Age the real record: only its deadline changes.
       (niwa-session-durability--write-generation
        directory (plist-put (plist-put (copy-sequence head) :renewed-at 1)
                             :expires-at 2)))
     (test-niwa-session-durability--kill-holder process)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (&rest _) (error "A dead holder needs no confirmation"))))
       (let ((niwa-session-durability--client-id (make-string 64 ?b)))
         (should (niwa-session-durability-lease-acquire root "*new*"))))))

  :doc "keeps a live same-host holder's lease"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (should (process-live-p process))
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       (should-not (niwa-session-durability-lease-acquire root "*new*")))))

  :doc "reclaims a lease whose live PID now belongs to another process"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (should (process-live-p process))
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory))
            (holder (copy-sequence (plist-get head :holder))))
       ;; Well-formed start ticks that differ from the live PID's: reuse.
       (niwa-session-durability--write-generation
        directory (plist-put (copy-sequence head) :holder
                             (plist-put holder :process
                                        (concat (plist-get holder :process) "0")))))
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       (should (niwa-session-durability-lease-acquire root "*new*")))))

  :doc "keeps a dead holder whose PID is out of range or whose boot disagrees"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (test-niwa-session-durability--kill-holder process)
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory))
            (holder (plist-get head :holder)))
       (dolist (forged
                (list (plist-put (copy-sequence holder) :pid 4194305)
                      (plist-put (copy-sequence holder) :process
                                 (replace-regexp-in-string
                                  "\\`[0-9a-f]" (lambda (c) (if (equal c "0") "1" "0"))
                                  (plist-get holder :process)))))
         (niwa-session-durability--write-generation
          directory (plist-put (copy-sequence head) :holder forged))
         (let ((niwa-session-durability--client-id (make-string 64 ?b)))
           (should-not (niwa-session-durability-lease-acquire root "*new*"))))
       ;; Control: the unforged dead holder is reclaimable.
       (niwa-session-durability--write-generation directory head)
       (let ((niwa-session-durability--client-id (make-string 64 ?b)))
         (should (niwa-session-durability-lease-acquire root "*new*"))))))

  :doc "keeps a live holder whose recorded identity is malformed"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (should (process-live-p process))
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory)))
       (niwa-session-durability--write-generation
        directory (plist-put (copy-sequence head) :holder
                             (plist-put (copy-sequence (plist-get head :holder))
                                        :process ""))))
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       (should-not (niwa-session-durability-lease-acquire root "*new*")))))

  :doc "never reclaims a dead holder recorded by another host"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (test-niwa-session-durability--kill-holder process)
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       (cl-letf (((symbol-function 'niwa-session-durability--host)
                  (lambda () "other-boot/other-machine/pid:[1]")))
         (should-not (niwa-session-durability-lease-acquire root "*new*"))))))

  :doc "never reclaims a dead holder recorded by another user"
  ;; Hidden /proc entries (hidepid, ProtectProc) make other users' live
  ;; processes look absent, so only the holder's own user may judge it.
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (test-niwa-session-durability--kill-holder process)
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory)))
       (niwa-session-durability--write-generation
        directory (plist-put (copy-sequence head) :holder
                             (plist-put (copy-sequence (plist-get head :holder))
                                        :uid (1+ (user-uid))))))
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       (should-not (niwa-session-durability-lease-acquire root "*new*")))))

  :doc "never reclaims when the holder's liveness is uncertain"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (test-niwa-session-durability--kill-holder process)
     (let ((niwa-session-durability--client-id (make-string 64 ?b)))
       ;; An unavailable process list is not evidence of death.
       (cl-letf (((symbol-function 'list-system-processes) #'ignore))
         (should-not (niwa-session-durability-lease-acquire root "*new*")))
       ;; Neither is a record without holder evidence.
       (let* ((directory (niwa-session-durability--lease-path root))
              (head (niwa-session-durability--lease-head directory)))
         (niwa-session-durability--write-generation
          directory (plist-put (copy-sequence head) :holder nil))
         (should-not (niwa-session-durability-lease-acquire root "*new*")))))))

(niwa-deftest niwa-session-durability--holder
  (:doc "never reuses a holder computed by another process, such as a dump")
  (let ((niwa-session-durability--holder-cache
         (cons (1+ (emacs-pid)) (list :pid (1+ (emacs-pid)))))
        (niwa-session-durability--host-cache
         (cons (1+ (emacs-pid)) "dumping-boot/dumping-machine/pid:[1]")))
    (let ((holder (niwa-session-durability--holder)))
      (unless holder (ert-skip "No provable process identity on this host"))
      (should (eql (emacs-pid) (plist-get holder :pid)))
      (should (eql (user-uid) (plist-get holder :uid)))
      (should-not (equal "dumping-boot/dumping-machine/pid:[1]"
                         (plist-get holder :host))))))

(niwa-deftest niwa-session-durability--abandoned-head-p ()
  ,test
  (test)
  :doc "treats an unexpired head whose same-host holder died as abandoned"
  (test-niwa-session-durability--with-holder
   (lambda (root process)
     (let* ((directory (niwa-session-durability--lease-path root))
            (head (niwa-session-durability--lease-head directory))
            (now (niwa-session-durability--target-time directory)))
       (should-not (niwa-session-durability--abandoned-head-p directory head now))
       (test-niwa-session-durability--kill-holder process)
       (should (niwa-session-durability--abandoned-head-p directory head now))))))

(niwa-deftest niwa-session-durability-lease-status ()
  ,test
  (test)
  :doc "answers state and holding host from one lease observation"
  (let* ((root (make-temp-file "niwa-lease-status-" t))
         (session-dir (file-name-concat root "session"))
         (session (test-niwa-session-durability--local-session root))
         (owner (make-string 64 ?a))
         (other (make-string 64 ?b)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should
           (equal '(:state available :host nil)
                  (niwa-session-durability-lease-status session-dir)))
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "owner" session))
            (should
             (equal (list :state 'owned :host (system-name))
                    (niwa-session-durability-lease-status session-dir))))
          ;; The client id is per-process and opaque, so the host is the
          ;; only part of a held lease another machine can name.
          (let ((niwa-session-durability--client-id other))
            (should
             (equal (list :state 'foreign :host (system-name))
                    (niwa-session-durability-lease-status session-dir))))
          (let ((niwa-session-durability--client-id owner))
            (niwa-session-durability-lease-release session-dir session))
          (let ((fences (file-name-concat session-dir ".lease" "fences")))
            (should-not (file-exists-p fences))
            (should (eq 'available
                        (niwa-session-durability-lease-state session-dir)))
            (should-not (file-exists-p fences))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-transfer-observe-decision
  (:doc "an absent decision does not create its mailbox while observing")
  (let* ((root (make-temp-file "niwa-decision-observe-" t))
         (session (test-niwa-session-durability--local-session root))
         (directory (file-name-concat root ".lease"))
         (requests (file-name-concat directory "requests"))
         (request (list :session-id "decision-observe" :generation 1
                        :requester-client-id
                        niwa-session-durability--client-id)))
    (setf (niwa-session-save-path session) root
          (niwa-session-session-id session) "decision-observe")
    (make-directory directory)
    (unwind-protect
        (progn
          (should-not (file-exists-p requests))
          (should-not
           (niwa-session-transfer-observe-decision session request))
          (should-not (file-exists-p requests)))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability--bind-lease
  (:doc "keeps the heartbeat while another client's claim is still in flight")
  (let* ((root (make-temp-file "niwa-bind-lease-" t))
         (session (test-niwa-session-durability--local-session root))
         (ours (list :generation 1 :transfer-generation 1 :status 'active
                     :publication-head nil :unsettled-mutation nil
                     :client-id niwa-session-durability--client-id
                     :host "desktop" :renewed-at 100 :expires-at 190
                     :buffer "main"))
         (claiming (list :generation 2 :transfer-generation 2 :status 'claiming
                         :publication-head nil :unsettled-mutation nil
                         :client-id (make-string 64 ?b)
                         :host "laptop" :renewed-at 100 :expires-at 190
                         :buffer "main")))
    (unwind-protect
        (progn
          (niwa-session-durability--bind-lease session ours 'owned)
          (should (timerp (niwa-session-lease-renewal-timer session)))
          ;; A contender that only reached `claiming' has not won: its record
          ;; aborts if our write landed first, and our own record is head
          ;; again seconds later.  Cancelling the heartbeat here would let a
          ;; live lease expire for real.
          (niwa-session-durability--bind-lease session claiming 'contested)
          (should (timerp (niwa-session-lease-renewal-timer session)))
          ;; A record that actually took over does end the heartbeat.
          (niwa-session-durability--bind-lease
           session (plist-put (copy-sequence claiming) :status 'active) 'lost)
          (should-not (niwa-session-lease-renewal-timer session)))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t))))

(niwa-deftest niwa-session-durability--valid-lease-p
  (:doc "rejects timestamps that are not finite nonnegative numbers")
  (let ((record
         (list :generation 1
               :transfer-generation 1
               :status 'active
               :publication-head nil
               :unsettled-mutation nil
               :client-id (make-string 64 ?a)
               :host "desktop"
               :renewed-at 100
               :expires-at 190
               :buffer "main")))
    (should (niwa-session-durability--valid-lease-p record))
    ;; The record is untrusted file bytes another machine may have written,
    ;; and this predicate is the only gate before every authority decision.
    (dolist (key '(:renewed-at :expires-at))
      (dolist (value (list (read "0.0e+NaN")
                           (read "1.0e+INF")
                           (read "-1.0e+INF")
                           -1
                           (1+ most-positive-fixnum)))
        (should-not
         (niwa-session-durability--valid-lease-p
          (plist-put (copy-sequence record) key value)))))
    ;; An expiry before its own renewal is not a coherent record either.
    (should-not
     (niwa-session-durability--valid-lease-p
      (plist-put (copy-sequence record) :expires-at 99)))))

(niwa-deftest niwa-session-durability--lease-state-of
  (:doc "refuses authority from a record whose expiry cannot be compared")
  (let* ((root (make-temp-file "niwa-lease-invalid-" t))
         (session-dir (file-name-concat root "session"))
         (lease-dir (file-name-concat session-dir ".lease")))
    (unwind-protect
        (progn
          (make-directory lease-dir t)
          (dolist (expiry (list (read "1.0e+INF") (read "0.0e+NaN")))
            ;; Real record bytes, because the round trip through `read' is
            ;; part of what makes these values reachable at all.
            (with-temp-file (file-name-concat
                             lease-dir "00000000000000000001.el")
              (prin1 (list :generation 1 :transfer-generation 1
                           :status 'active :publication-head nil
                           :unsettled-mutation nil
                           :client-id (make-string 64 ?a)
                           :host "laptop" :renewed-at 100
                           :expires-at expiry :buffer "main")
                     (current-buffer)))
            (should-error
             (niwa-session-durability-lease-state session-dir))))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-lease-state ()
  ,test
  (test)
  :doc "classifies available, owned, foreign, and expired portable leases"
  (let* ((root (make-temp-file "niwa-lease-state-" t))
         (session-dir (file-name-concat root "session"))
         (session (test-niwa-session-durability--local-session root))
         (owner (make-string 64 ?a))
         (other (make-string 64 ?b))
         ;; The lease has to outlive the control operations of one
         ;; acquisition.
         (niwa-session-lease-seconds 2)
         ;; Whole target seconds added to every later target clock reading,
         ;; so expiry is observed without waiting for the real clock.
         (offset 0)
         (shifted 0)
         (program-value (symbol-function
                         'niwa-session-control-fs--program-value)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs--program-value)
                   (lambda (op payload)
                     (let ((value (funcall program-value op payload)))
                       (if (and (eq 'target-time (plist-get op :op))
                                (> offset 0))
                           (progn (cl-incf shifted) (+ value offset))
                         value)))))
          (should
           (eq 'available
               (niwa-session-durability-lease-state session-dir)))
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "owner" session))
            (should
             (eq 'owned
                 (niwa-session-durability-lease-state session-dir))))
          (let ((niwa-session-durability--client-id other))
            (should
             (eq 'foreign
                 (niwa-session-durability-lease-state session-dir))))
          (setq offset (1+ niwa-session-lease-seconds))
          (let ((niwa-session-durability--client-id other))
            (should
             (eq 'expired
                 (niwa-session-durability-lease-state session-dir))))
          ;; The expiry came from a real target reading moved forward.
          (should (> shifted 0))
          (let ((niwa-session-durability--client-id owner))
            (niwa-session-durability-lease-release session-dir session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability--target-time ()
  ,test
  (test)
  :doc "transaction readings stay directory-specific, expire, and never escape their scope"
  (let ((outer "/niwamock:outer:/session/.lease/")
        (inner "/niwamock:inner:/session/.lease/")
        (niwa-session-durability--transaction-clock nil)
        (niwa-session-durability--observed-time nil)
        (elapsed 0.0)
        reads)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) elapsed))
              ((symbol-function 'niwa-session-control-fs-target-time)
               (lambda (directory)
                 (push directory reads)
                 (if (equal directory outer) 1000 100000))))
      (niwa-session-durability-with-transaction
        (should (= 1000 (niwa-session-durability--target-time outer)))
        (should-not (niwa-session-durability--target-time-cached-p inner))
        (niwa-session-durability-with-transaction
          (should (= 100000 (niwa-session-durability--target-time inner))))
        (setq elapsed 0.5)
        (should (= 1000 (niwa-session-durability--target-time outer)))
        (should (= 100000 (niwa-session-durability--target-time inner)))
        (should (equal reads (list inner outer)))
        (setq elapsed 1.0)
        (should-not (niwa-session-durability--target-time-cached-p outer))
        (should (= 1000 (niwa-session-durability--target-time outer)))
        (should (equal reads (list outer inner outer))))
      (should-not niwa-session-durability--transaction-clock)
      (dotimes (_ 2)
        (should (= 1000 (niwa-session-durability--target-time outer)))))
    (should (= 5 (length reads)))))


(niwa-deftest niwa-session-durability-target-clock-skew
  (:doc "uses target time when clients have radically skewed wall clocks")
  (let* ((root (make-temp-file "niwa-target-clock-skew-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (client-clock 0.0)
         (niwa-session-lease-seconds 10))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) target-now))
                  ((symbol-function 'float-time)
                   (lambda (&rest _) client-clock))
                  ((symbol-function 'current-time)
                   (lambda () (seconds-to-time client-clock)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*owner*")))
          (setq target-now 105.0
                client-clock 1000000.0)
          (let ((niwa-session-durability--client-id successor))
            (should (eq 'foreign
                        (niwa-session-durability-lease-state session-dir))))
          (setq target-now 120.0
                client-clock -1000000.0)
          (let ((niwa-session-durability--client-id successor))
            (should (eq 'expired
                        (niwa-session-durability-lease-state session-dir)))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*successor*"))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability--lease-record
  (:doc "requires target time and records the open transfer generation")
  ,test
  (test)
  (should-error (niwa-session-durability--lease-record "*buffer*" 1)
                :type 'error)
  (let ((niwa-session-durability--client-id (make-string 64 ?a)))
    (should
     (= 3
        (plist-get
         (niwa-session-durability--lease-record
          "*buffer*" 3 'active nil nil 1.0)
         :transfer-generation)))
    (should
     (= 1
        (plist-get
         (niwa-session-durability--lease-record
          "*buffer*" 3 'active nil nil 1.0 1)
         :transfer-generation)))))

(niwa-deftest niwa-session-durability--lease-head
  (:doc "keeps a live lease visible when a newer generation cannot be read")
  (let* ((root (make-temp-file "niwa-lease-head-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (niwa-session-lease-seconds 90))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) target-now)))
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire session-dir "*owner*")))
          ;; A newer generation name that cannot be read must not read as
          ;; "no lease": that would let the next claim pass its
          ;; compare-and-set against a live owner.
          (let* ((directory (niwa-session-durability--lease-path
                             session-dir))
                 (phantom (file-name-concat
                           directory
                           (format "%020d.el" 6000))))
            (write-region "" nil phantom nil 'silent)
            (should (niwa-session-durability--lease-head directory))
            (should (equal owner
                           (plist-get
                            (niwa-session-durability--lease-head directory)
                            :client-id)))
            (let ((niwa-session-durability--client-id successor))
              (should (eq 'foreign
                          (niwa-session-durability-lease-state
                           session-dir)))
              (should-not
               (niwa-session-durability-lease-acquire
                session-dir "*successor*")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-reclaim-expired-lease
  (:doc "reclaims this client's own expired lease but still asks for another's")
  (let* ((root (make-temp-file "niwa-expired-reclaim-" t))
         (session-dir (file-name-concat root "session"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (target-now 100.0)
         (prompts 0)
         (niwa-session-lease-seconds 10))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) target-now))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (setq prompts (1+ prompts)) nil)))
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire session-dir "*owner*"))
            ;; A long blocking target operation can outlast the lease while
            ;; the renewal timer cannot run.
            (setq target-now 200.0)
            (should (eq 'expired
                        (niwa-session-durability-lease-state session-dir)))
            (should
             (niwa-session-durability-lease-acquire session-dir "*owner*"))
            (should (= 0 prompts))
            (should (eq 'owned
                        (niwa-session-durability-lease-state session-dir))))
          (setq target-now 300.0)
          (let ((niwa-session-durability--client-id successor))
            (should-not
             (niwa-session-durability-lease-acquire
              session-dir "*successor*"))
            (should (= 1 prompts))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-persistence-lock-acquire/remote ()
  ,test
  (test)
  :doc "remote acquisition writes a portable lease instead of a PID lock"
  (let* ((host "lease-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-lease-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-niwa-session-durability--remote-session
                  host local-root)))
            (test-niwa-session-durability--accept-storage session)
            (setf (niwa-session-save-path session) session-dir)
            (should
             (niwa-session-persistence-lock-acquire
              session-dir "*remote*" session))
            (should (file-directory-p
                     (file-name-concat session-dir ".lease")))
            (should-not (file-exists-p
                         (file-name-concat session-dir ".lock")))
            (let ((lease (niwa-session-lease session)))
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
                        (plist-get (niwa-session-lease session) :state)))
            (should (timerp (niwa-session-lease-renewal-timer session)))
            (niwa-session-persistence-lock-release session-dir session)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "expired remote takeover requires confirmation and changes owner"
  (let* ((host "takeover-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-takeover-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (niwa-session-durability--client-id (make-string 64 ?b))
         (session (niwa-session--create :authority-mode 'portable))
         (confirmed nil)
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t)))
            (let ((niwa-session-durability--client-id (make-string 64 ?a)))
              (should
               (niwa-session-persistence-lock-acquire
                session-dir "*expired*" session)))
            (setq now 100.0)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq confirmed t)
                         t)))
              (should
               (niwa-session-persistence-lock-acquire
                session-dir "*new*" session)))
            (should confirmed)
            (should (niwa-session-durability-lease-acquire
                     session-dir "*new*" session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "simultaneous expired takeovers elect exactly one owner"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-takeover-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner (make-string 64 ?a))
         (now 0.0)
         (claim-mutex (make-mutex "niwa lease claims"))
         (claim-condition (make-condition-variable claim-mutex))
         (claim-count 0)
         (claim-generations nil)
         (result-mutex (make-mutex "niwa lease results"))
         (results nil)
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program))
         threads)
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now)))
          (let ((niwa-session-durability--client-id owner))
            (should (niwa-session-durability-lease-acquire
                     session-dir "*expired*")))
          (setq now 100.0)
          (cl-letf
              (((symbol-function 'y-or-n-p)
                (lambda (&rest _) t))
               ;; A claim states its fence as the exclusive `create'
               ;; opening its program; both contenders are held at that
               ;; exact point so the races collide for real.
               ((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (operations &optional lock-directory)
                  (when (eq 'create (plist-get (car operations) :op))
                    (with-mutex claim-mutex
                      (cl-incf claim-count)
                      (push (niwa-session-durability--generation
                             (plist-get (car operations) :path))
                            claim-generations)
                      (condition-notify claim-condition t)
                      (while (< claim-count 2)
                        (condition-wait claim-condition))))
                  (funcall run-program-function operations lock-directory))))
            (dolist (client (list (make-string 64 ?b) (make-string 64 ?c)))
              (push
               (make-thread
                (lambda ()
                  (let ((niwa-session-durability--client-id client))
                    (let ((won
                           (niwa-session-durability-lease-acquire
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
                        (make-temp-file "niwa-claiming-head-" t)))
           (session-dir (file-name-as-directory
                         (file-name-concat local-root "session")))
           (session (test-niwa-session-durability--local-session
                     local-root))
           (owner (make-string 64 ?a))
           (contender (make-string 64 ?b))
           (blocker (make-string 64 ?c))
           (now 0.0))
      (make-directory session-dir t)
      (setf (niwa-session-save-path session) session-dir)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t)))
            (let ((niwa-session-durability--client-id owner))
              (should (niwa-session-durability-lease-acquire
                       session-dir "*expired*")))
            (setq now 100.0)
            (let ((niwa-session-durability--client-id contender))
              (let ((niwa-session-durability--client-id blocker))
                (should
                 (niwa-session-durability--create-plist
                  (niwa-session-durability--generation-path
                   (file-name-concat session-dir ".lease") 2)
                  (niwa-session-durability--lease-record
                   "*paused-claim*" 2 'claiming nil nil
                   (niwa-session-durability--target-time
                    (file-name-concat session-dir ".lease"))))))
              (should-not
               (niwa-session-durability-lease-acquire
                session-dir "*contender*" (and bind-session session))))
            (when bind-session
              (should
               (eq 'foreign
                   (plist-get (niwa-session-lease session) :state)))))
        (niwa-session-durability--cancel-renewal session)
        (when (file-directory-p local-root)
          (delete-directory local-root t)))))

  :doc "an expired interrupted claim recovers without confirming an inactive holder"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-stale-claim-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (niwa-session-lease-seconds 10)
         (now 0.0)
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program))
         (fail-finalization t)
         (confirmed nil))
    (make-directory session-dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now))
                  ;; A claim finalizes through the program whose second
                  ;; operation is the settle write; the fencing create and
                  ;; the lease commits open with other verbs.
                  ((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (operations &optional lock-directory)
                     (if (and fail-finalization
                              (eq 'verify (plist-get (car operations) :op))
                              (eq 'write (plist-get (cadr operations) :op)))
                         (progn
                           (setq fail-finalization nil)
                           (error "Injected claim finalization crash"))
                       (funcall run-program-function operations lock-directory)))))
          (should-error
           (niwa-session-durability-lease-acquire
            session-dir "*interrupted*")
           :type 'error)
          (let ((niwa-session-durability--client-id (make-string 64 ?b)))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (setq confirmed t)
                         t)))
              (should-not (niwa-session-durability-lease-acquire
                           session-dir "*too-early*"))
              (setq now 20.0)
              (should (niwa-session-durability-lease-acquire
                       session-dir "*successor*")))
            (should-not confirmed)
            (should (niwa-session-durability-lease-acquire
                     session-dir "*successor*"))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-adopt-owned-lease ()
  ,test
  (test)
  :doc "moves a verified child lease and renewal owner into the live session"
  (let* ((root (make-temp-file "niwa-adopt-lease-" t))
         (parent-path (file-name-as-directory
                       (file-name-concat root "parent")))
         (child-path (file-name-as-directory
                      (file-name-concat root "child")))
         (parent (test-niwa-session-durability--local-session root))
         (child (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory parent-path t)
    (make-directory child-path t)
    (setf (niwa-session-save-path parent) parent-path
          (niwa-session-save-path child) child-path)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   parent-path "parent" parent))
          (should (niwa-session-durability-lease-acquire
                   child-path "child" child))
          (let ((child-generation
                 (plist-get (niwa-session-lease child) :generation))
                (child-timer
                 (niwa-session-lease-renewal-timer child)))
            (should
             (niwa-session-durability-adopt-owned-lease parent child))
            (should (equal (niwa-session-save-path parent) child-path))
            (should (= (plist-get (niwa-session-lease parent) :generation)
                       child-generation))
            (should (timerp
                     (niwa-session-lease-renewal-timer parent)))
            (should-not (eq (niwa-session-lease-renewal-timer parent)
                            child-timer))
            (should-not (niwa-session-lease child))
            (should-not (niwa-session-lease-renewal-timer child))
            (should (niwa-session-durability-lease-owned-p parent))))
      (ignore-errors
        (niwa-session-durability-lease-release child-path parent))
      (ignore-errors
        (niwa-session-durability-lease-release parent-path))
      (niwa-session-durability--cancel-renewal parent)
      (niwa-session-durability--cancel-renewal child)
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "verification failure preserves both owned lease objects"
  (let* ((root (make-temp-file "niwa-adopt-verify-" t))
         (parent-path (file-name-as-directory
                       (file-name-concat root "parent")))
         (child-path (file-name-as-directory
                      (file-name-concat root "child")))
         (parent (test-niwa-session-durability--local-session root))
         (child (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory parent-path t)
    (make-directory child-path t)
    (setf (niwa-session-save-path parent) parent-path
          (niwa-session-save-path child) child-path)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   parent-path "parent" parent))
          (should (niwa-session-durability-lease-acquire
                   child-path "child" child))
          (let ((parent-lease (niwa-session-lease parent))
                (parent-timer (niwa-session-lease-renewal-timer parent))
                (child-lease (niwa-session-lease child))
                (child-timer (niwa-session-lease-renewal-timer child)))
            (cl-letf (((symbol-function
                        'niwa-session-durability-call-with-reserved-lease)
                       (lambda (&rest _)
                         (error "Injected adoption verification failure"))))
              (should-error
               (niwa-session-durability-adopt-owned-lease parent child)))
            (should (equal parent-path (niwa-session-save-path parent)))
            (should (eq parent-lease (niwa-session-lease parent)))
            (should (eq parent-timer
                        (niwa-session-lease-renewal-timer parent)))
            (should (equal child-path (niwa-session-save-path child)))
            (should (eq child-lease (niwa-session-lease child)))
            (should (eq child-timer
                        (niwa-session-lease-renewal-timer child)))
            (should (niwa-session-durability-lease-owned-p parent))
            (should (niwa-session-durability-lease-owned-p child))))
      (ignore-errors
        (niwa-session-durability-lease-release parent-path parent))
      (ignore-errors
        (niwa-session-durability-lease-release child-path child))
      (niwa-session-durability--cancel-renewal parent)
      (niwa-session-durability--cancel-renewal child)
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-set-unsettled-mutation ()
  ,test
  (test)
  :doc "lease transitions and takeover preserve the unsettled mutation latch"
  (let* ((host "unsettled-mutation-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-unsettled-mutation-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session
          (test-niwa-session-durability--remote-session host local-root))
         first-generation)
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (let ((niwa-session-durability--client-id (make-string 64 ?a)))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*first-owner*" session))
            (setq first-generation
                  (plist-get (niwa-session-lease session) :generation))
            (should
             (niwa-session-durability-set-unsettled-mutation session t))
            (should (niwa-session-durability-lease-renew session))
            (should
             (niwa-session-durability-call-with-reserved-lease
              session (lambda () t)))
            (should
             (niwa-session-durability-unsettled-mutation-p session))
            (niwa-session-durability-lease-release session-dir session))
          (let ((niwa-session-durability--client-id (make-string 64 ?b)))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*successor*" session))
            (should (> (plist-get (niwa-session-lease session) :generation)
                       first-generation))
            (should
             (niwa-session-durability-unsettled-mutation-p session))
            (should
             (niwa-session-durability-set-unsettled-mutation session nil))
            (niwa-session-durability-lease-release session-dir session))
          (let ((niwa-session-durability--client-id (make-string 64 ?c)))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*observer*" session))
            (should-not
             (niwa-session-durability-unsettled-mutation-p session))
            (niwa-session-durability-lease-release session-dir session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale renewal cannot overwrite a reentrant mutation arm"
  (let* ((host "unsettled-renew-race-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-unsettled-renew-race-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session
          (test-niwa-session-durability--remote-session host local-root))
         (client-id (make-string 64 ?a))
         run-program-function
         injected
         initial-generation)
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (let ((niwa-session-durability--client-id client-id))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*owner*" session))
            (setq initial-generation
                  (plist-get (niwa-session-lease session) :generation)
                  run-program-function
                  (symbol-function 'niwa-session-control-fs-run-program))
            ;; The renewal's settling write is the commit program opened
            ;; by its verify; arming the latch immediately before it runs
            ;; is the reentrant race this case pins.
            (cl-letf
                (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (operations &optional lock-directory)
                    (when (and (not injected)
                               (eq 'verify (plist-get (car operations) :op)))
                      (setq injected t)
                      (should
                       (niwa-session-durability-set-unsettled-mutation
                        session t)))
                    (funcall run-program-function operations lock-directory))))
              (niwa-session-durability-lease-renew session))
            (should injected)
            (should
             (niwa-session-durability-unsettled-mutation-p session))
            (should
             (> (plist-get (niwa-session-lease session) :generation)
                initial-generation))
            (niwa-session-durability-lease-release session-dir session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-request-begin/remote-lease
  (:after-each (setq niwa--current-request nil))
  ,test
  (test)
  :doc "a foreign unexpired lease blocks request admission"
  (let* ((host "foreign-lease-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-foreign-lease-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (niwa-session-durability--client-id (make-string 64 ?b)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((session
                  (test-niwa-session-durability--remote-session
                   host local-root))
                 (target (niwa-session-execution-target session)))
            (test-niwa-session-durability--accept-storage session)
            (setf (niwa-session-save-path session) session-dir
                  (niwa-execution-target-readiness target)
                  '(:status ready))
            (let ((niwa-session-durability--client-id (make-string 64 ?a)))
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*owner*")))
            (should-not
             (niwa-session-persistence-lock-acquire
              session-dir "*inspector*" session))
            (should-error (niwa-request-begin session)
                          :type 'user-error)
            (should-not niwa--current-request)))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-note-no-pid-lock ()
  ,test
  (test)
  :doc "outside a transaction there is no memo to seed"
  (let ((niwa-session-durability--asserted-directories nil))
    (niwa-session-durability-note-no-pid-lock temporary-file-directory)
    (should-not niwa-session-durability--asserted-directories))

  :doc "inside a transaction the physical directory is recorded once"
  (let* ((dir (make-temp-file "niwa-note-no-lock-" t))
         (physical (niwa-session-control-fs-physical-path dir))
         (niwa-session-durability--asserted-directories (list nil)))
    (unwind-protect
        (progn
          (niwa-session-durability-note-no-pid-lock dir)
          (niwa-session-durability-note-no-pid-lock dir)
          (should (equal (list physical)
                         (car niwa-session-durability--asserted-directories))))
      (delete-directory dir t))))


(niwa-deftest niwa-session-durability-legacy-lock-proof ()
  ,test
  (test)
  :doc "a batched absence proof is reused within the durable transaction"
  (let ((root (make-temp-file "niwa-legacy-proof-" t)))
    (unwind-protect
        (niwa-session-durability-with-transaction
          (let* ((operation (niwa-session-durability--pid-lock-operation root))
                 (result (car (niwa-session-control-fs-run-program
                               (list operation)))))
            (should (eq 'absent (plist-get result :status)))
            (niwa-session-durability--settle-pid-lock root result)
            (should-not (niwa-session-durability--pid-lock-operation root))
            (niwa-session-durability--assert-no-pid-lock root)))
      (delete-directory root t)))
  :doc "direct and batched legacy-lock checks refuse without modifying bytes"
  (let* ((root (make-temp-file "niwa-legacy-refusal-" t))
         (lock (file-name-concat root ".lock")))
    (unwind-protect
        (progn
          (with-temp-file lock (insert "unmigrated"))
          (should
           (string-match-p
            "Unsupported transcript format"
            (error-message-string
             (should-error (niwa-session-durability--assert-no-pid-lock root)
                           :type 'error))))
          (let* ((operation (niwa-session-durability--pid-lock-operation root))
                 (result (car (niwa-session-control-fs-run-program
                               (list operation)))))
            (should
             (string-match-p
              "Unsupported transcript format"
              (error-message-string
               (should-error
                (niwa-session-durability--settle-pid-lock root result)
                :type 'error)))))
          (should (equal "unmigrated"
                         (with-temp-buffer
                           (insert-file-contents lock)
                           (buffer-string)))))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-lease-renew ()
  ,test
  (test)
  :doc "heartbeat renewal extends an owned portable lease without PID liveness"
  (let* ((host "renew-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-renew-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session (test-niwa-session-durability--remote-session
                   host local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now)))
            (should (niwa-session-persistence-lock-acquire
                     session-dir "*renew*" session))
            (let ((old-expiry
                   (plist-get (niwa-session-lease session) :expires-at)))
              (setq now 30.0)
              (should (niwa-session-durability-lease-renew session))
              (should (> (plist-get (niwa-session-lease session)
                                    :expires-at)
                         old-expiry))))
          (niwa-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a standby past the lease TTL reclaims this client's own record"
  (let* ((host "renew-standby-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-standby-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session (test-niwa-session-durability--remote-session
                   host local-root))
         (niwa-session-durability--client-id (make-string 64 ?b))
         (now 0.0))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now)))
            (should (niwa-session-persistence-lock-acquire
                     session-dir "*standby*" session))
            (let ((generation
                   (plist-get (niwa-session-lease session) :generation)))
              ;; The machine slept: the renewal timer never fired, the target
              ;; clock moved past the record's expiry anyway.
              (setq now (+ niwa-session-lease-seconds 30.0))
              (should (niwa-session-durability-lease-renew session))
              (should (eq 'owned
                          (plist-get (niwa-session-lease session) :state)))
              (should (> (plist-get (niwa-session-lease session)
                                    :generation)
                         generation))
              (should (niwa-session-durability-lease-owned-p session))))
          (niwa-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a renewal under a bound transaction clock assumes instead of observing"
  (let* ((host "renew-assume-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-renew-assume-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session (test-niwa-session-durability--remote-session
                   host local-root))
         (niwa-session-durability--client-id (make-string 64 ?c))
         (niwa-session-durability--transaction-clock (list nil))
         (niwa-session-durability--asserted-directories (list nil))
         (programs nil)
         (program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-persistence-lock-acquire
                   session-dir "*assume*" session))
          ;; The first renewal may still observe; its commit's trailing
          ;; clock operation is what arms the assumption for the next one.
          (should (niwa-session-durability-lease-renew session))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &optional lock-directory)
                       (push (mapcar (lambda (op) (plist-get op :op))
                                     operations)
                             programs)
                       (funcall program-function operations lock-directory))))
            (should (niwa-session-durability-lease-renew session)))
          (should (member programs
                          '(((verify write list-directory target-time))
                            ((verify list-directory target-time))
                            ((verify write list-directory))
                            ((verify list-directory)))))
          (niwa-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale renewal cannot overwrite a newer takeover"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-renew-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--remote-session
                   "renew-race-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         ;; Pin the lease window this timeline assumes instead of tracking
         ;; the shipped default.
         (niwa-session-lease-seconds 90)
         (now 0.0)
         (triggered nil)
         (program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now)))
            (should (niwa-session-durability-lease-acquire
                     session-dir "*owner*" session))
            (setq now 80.0)
            (cl-letf
                (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ;; A renewal states its precondition as the `verify' opening
                 ;; its commit program.  Letting a successor take over
                 ;; immediately before that program runs is the last moment a
                 ;; stale renewal could still overwrite the newer owner.
                 ((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (operations &optional lock-directory)
                    (when (and (not triggered)
                               (eq 'verify (plist-get (car operations) :op)))
                      (setq triggered t
                            now 100.0)
                      (let ((niwa-session-durability--client-id successor))
                        (should
                         (niwa-session-durability-lease-acquire
                          session-dir "*successor*"))))
                    (funcall program-function operations lock-directory))))
              (should-not
               (niwa-session-durability-lease-renew session))
              (should triggered))
            (let ((niwa-session-durability--client-id successor))
              (should (niwa-session-durability-lease-acquire
                       session-dir "*successor*"))
              (niwa-session-durability-lease-release session-dir))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a completed renewal makes an earlier stale takeover abort"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-renew-before-claim-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--remote-session
                   "renew-before-claim-host" local-root))
         (owner (make-string 64 ?a))
         (contender (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         ;; Pin the lease window this timeline assumes instead of tracking
         ;; the shipped default.
         (niwa-session-lease-seconds 90)
         (initial t)
         (mutex (make-mutex "niwa renew-before-claim"))
         (condition (make-condition-variable mutex))
         (renew-ready nil)
         (renew-allowed nil)
         (renew-done nil)
         (renew-result nil)
         (program-function
          (symbol-function 'niwa-session-control-fs-run-program))
         renew-thread)
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-durability--target-time)
              (lambda (&optional _)
                (cond
                 (initial 0.0)
                 ((equal niwa-session-durability--client-id owner) 80.0)
                 (t 100.0))))
             ;; The renewal is held immediately before its commit program,
             ;; which is the operation that proves its precondition and writes
             ;; the new record.  That is the moment the contender must find
             ;; still outstanding.
             ((symbol-function 'niwa-session-control-fs-run-program)
              (lambda (operations &optional lock-directory)
                (when (and (equal (thread-name (current-thread))
                                  "niwa-owner-renew")
                           (eq 'verify (plist-get (car operations) :op))
                           (not renew-ready))
                  (with-mutex mutex
                    (setq renew-ready t)
                    (condition-notify condition t)
                    (while (not renew-allowed)
                      (condition-wait condition))))
                (funcall program-function operations lock-directory))))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (setq initial nil
                renew-thread
                (make-thread
                 (lambda ()
                   (let ((niwa-session-durability--client-id owner))
                     (setq renew-result
                           (niwa-session-durability-lease-renew session))
                     (with-mutex mutex
                       (setq renew-done t)
                       (condition-notify condition t))))
                 "niwa-owner-renew"))
          (with-mutex mutex
            (while (not renew-ready)
              (condition-wait condition)))
          (let ((niwa-session-durability--client-id contender))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (with-mutex mutex
                           (setq renew-allowed t)
                           (condition-notify condition t)
                           (while (not renew-done)
                             (condition-wait condition)))
                         t)))
              (should-not
               (niwa-session-durability-lease-acquire
                session-dir "*contender*"))))
          (thread-join renew-thread)
          (should renew-result)
          (let ((niwa-session-durability--client-id owner))
            (should (niwa-session-durability-lease-renew session))))
      (niwa-session-durability--cancel-renewal session)
      (when (and renew-thread (thread-live-p renew-thread))
        (thread-signal renew-thread 'quit nil))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "renewal bypasses a stale remote generation listing"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-stale-lease-cache-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (lease-directory (file-name-concat session-dir ".lease"))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (session (test-niwa-session-durability--local-session local-root))
         (program-function
          (symbol-function 'niwa-session-control-fs-run-program))
         (helper-called nil))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (let ((niwa-session-durability--client-id successor))
            (should
             (niwa-session-durability--create-plist
              (niwa-session-durability--generation-path lease-directory 2)
              (niwa-session-durability--lease-record
               "*successor*" 2 'active nil nil
               (niwa-session-durability--target-time lease-directory)))))
          ;; The renewal observes the generation listing inside its own
          ;; target program, so that is where consulting it is proved.
          (cl-letf
              (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (operations &optional lock-directory)
                  (when (cl-find-if
                         (lambda (operation)
                           (and (eq 'list-directory (plist-get operation :op))
                                (equal (niwa-session-control-fs-physical-path
                                        (plist-get operation :path))
                                       (niwa-session-control-fs-physical-path
                                        lease-directory))))
                         operations)
                    (setq helper-called t))
                  (funcall program-function operations lock-directory))))
            (should-not
             (niwa-session-durability-lease-renew session)))
          (should helper-called))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "renewal recovers an owned publishing generation after serialization"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publishing-renew-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-durability--renew-publication-lease session))
          (should (eq 'publishing
                      (plist-get (niwa-session-lease session) :status)))
          (should (niwa-session-durability-lease-renew session))
          (should (eq 'active
                      (plist-get (niwa-session-lease session) :status))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "a tick defers while a publication job owns the lease, and a token resumes"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-renew-job-" t)))
         (session (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (token (list 'preparation)))
    (setf (niwa-session-save-path session) root)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   root "*owner*" session))
          (niwa-session-durability--cancel-renewal session)
          (let ((before (copy-sequence (niwa-session-lease session))))
            (setf (niwa-session-publication-job session) 'job)
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest _) (ert-fail "Tick reached the target"))))
              (should (niwa-session-durability-lease-renew session))
              (should (equal before (niwa-session-lease session))))
            ;; The running preparation's own token is not a competing owner.
            (setf (niwa-session-publication-preparation-token session) token)
            (should (niwa-session-durability-lease-renew session token))))
      (setf (niwa-session-publication-job session) nil
            (niwa-session-publication-preparation-token session) nil)
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors (niwa-session-durability-lease-release root session))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-lease-release ()
  ,test
  (test)
  :doc "release settles the current same-client successor generation"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-release-successor-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (let* ((directory (file-name-concat session-dir ".lease"))
                 (first (niwa-session-durability--lease-head directory))
                 (successor
                  (niwa-session-durability--claim-next
                   directory first "*owner*")))
            (should successor)
            (should (= 1 (plist-get (niwa-session-lease session)
                                    :generation)))
            (should (= 2 (plist-get successor :generation)))
            (niwa-session-durability-lease-release session-dir session)
            (should
             (eq 'released
                 (plist-get
                  (niwa-session-durability--lease-head directory)
                  :status)))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale release cannot delete a newer takeover"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-release-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session (test-niwa-session-durability--remote-session
                         "release-race-host" local-root))
         (successor-session
          (test-niwa-session-durability--remote-session
           "release-race-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (now 0.0)
         (triggered nil)
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner-session) session-dir
          (niwa-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (setq now 80.0)
          (cl-labels
              ((trigger-successor
                ()
                (unless triggered
                  (setq triggered t
                        now 100.0)
                  (let ((niwa-session-durability--client-id successor))
                    (cl-letf (((symbol-function 'y-or-n-p)
                               (lambda (&rest _) t)))
                      (should
                       (niwa-session-durability-lease-acquire
                        session-dir "*successor*" successor-session)))))))
            ;; The release's first mutating program -- its fencing create
            ;; or settling write -- is the last moment a takeover can land
            ;; ahead of it; the observation before it must not trigger.
            (cl-letf
                (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (operations &optional lock-directory)
                    (when (and (not triggered)
                               (memq (plist-get (car operations) :op)
                                     '(create write verify)))
                      (trigger-successor))
                    (funcall run-program-function operations lock-directory))))
              (niwa-session-durability-lease-release
               session-dir owner-session))
            (should triggered))
          (let ((niwa-session-durability--client-id successor))
            (should
             (niwa-session-durability-lease-renew successor-session))
            (niwa-session-durability-lease-release
             session-dir successor-session)))
      (niwa-session-durability--cancel-renewal owner-session)
      (niwa-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "release settles an inactive publisher without interrupting a live one"
  (dolist (publication-active '(nil t))
    (let* ((local-root (file-name-as-directory
                        (make-temp-file "niwa-publishing-release-" t)))
           (session-dir (file-name-as-directory
                         (file-name-concat local-root "session")))
           (session (test-niwa-session-durability--local-session local-root))
           (niwa-session-durability--client-id (make-string 64 ?a)))
      (make-directory session-dir t)
      (setf (niwa-session-save-path session) session-dir)
      (unwind-protect
          (progn
            (should (niwa-session-durability-lease-acquire
                     session-dir "*publisher*" session))
            (should
             (niwa-session-durability--renew-publication-lease session))
            (setf (niwa-session-publication-active-p session)
                  publication-active)
            (niwa-session-durability-lease-release session-dir session)
            (if publication-active
                (progn
                  (should (plist-get (niwa-session-lease session)
                                     :release-pending))
                  (should (timerp
                           (niwa-session-lease-renewal-timer session))))
              (should-not (niwa-session-lease session)))
            (should
             (eq (if publication-active 'publishing 'released)
                 (plist-get
                  (niwa-session-durability--lease-head
                   (file-name-concat session-dir ".lease"))
                  :status))))
        (niwa-session-durability--cancel-renewal session)
        (when (file-directory-p local-root)
          (delete-directory local-root t)))))

  :doc "serialization completes a release requested by its live publisher"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-deferred-release-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-durability-call-with-reserved-lease
            session
            (lambda ()
              (niwa-session-durability-lease-release
               session-dir session)
              t)))
          (should-not (niwa-session-lease session))
          (should-not (niwa-session-lease-renewal-timer session))
          (should
           (eq 'released
               (plist-get
                (niwa-session-durability--lease-head
                 (file-name-concat session-dir ".lease"))
                :status))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "release retains failed critical publication until explicit abandonment"
  (let* ((host "release-pending-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-release-pending-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/niwamock:%s:%s/" host session-dir-local))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (target (file-name-concat session-dir "blocker" "state.el"))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((session (test-niwa-session-durability--remote-session
                          host local-root)))
            (test-niwa-session-durability--accept-storage session)
            (setf (niwa-session-save-path session) session-dir)
            (should (niwa-session-durability-lease-acquire
                     session-dir "*publisher*" session))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session (list (list :path target :content "critical")))
               :type 'file-error))
            (let* ((pending (niwa-session-pending-publication session))
                   (batch (car (plist-get pending :batches)))
                   (recovery (plist-get batch :directory)))
              (should (file-directory-p recovery))
              (niwa-test--with-captured-diagnostics nil
                (niwa-session-durability-lease-release
                 session-dir session))
              (should (eq pending
                          (niwa-session-pending-publication session)))
              (should (file-directory-p recovery))
              (dolist (retained (plist-get pending :batches))
                (niwa-session-publication--delete-batch retained)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "deferred release retains a reentrantly queued critical publication"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-release-queued-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (niwa-test--with-captured-diagnostics nil
            (should
             (niwa-session-durability-call-with-reserved-lease
              session
              (lambda ()
                (should
                 (eq 'queued
                     (niwa-session-publication-publish
                      session
                      (list (list :path target :content "critical")))))
                (niwa-session-durability-lease-release
                 session-dir session)
                t))))
          (let* ((pending (niwa-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))
            (niwa-session-publication--delete-batch batch)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-forget-removed-session
  (:doc "clears lease, renewal, publication, and recovery without target I/O")
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-forget-removed-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (recovery (file-name-as-directory
                    (make-temp-file "niwa-forget-recovery-" t)))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*removed*" session))
          (should (timerp
                   (niwa-session-lease-renewal-timer session)))
          (setf (niwa-session-publication session) '(:head "published")
                (niwa-session-publication-active-p session) t
                (niwa-session-pending-publication session)
                (list :batches
                      (list (list :directory recovery :artifacts nil))))
          (delete-directory session-dir t)
          (niwa-session-durability-forget-removed-session session)
          (should-not (file-exists-p session-dir))
          (should-not (file-directory-p recovery))
          (should-not (niwa-session-lease session))
          (should-not (niwa-session-lease-renewal-timer session))
          (should-not (niwa-session-publication session))
          (should-not (niwa-session-publication-active-p session))
          (should-not (niwa-session-pending-publication session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-run-owned-program ()
  ,test
  (test)
  :doc "runs operations behind a proof of the bytes this client committed"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-owned-program-" t)))
         (session-dir (file-name-as-directory (file-name-concat root "session")))
         (session (test-niwa-session-durability--local-session root))
         (log (file-name-concat session-dir "diagnostic.log"))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (should (plist-get (niwa-session-lease session) :bytes))
          (should (equal '((:status ok :value nil))
                         (mapcar (lambda (result)
                                   (list :status (plist-get result :status)
                                         :value (plist-get result :value)))
                                 (niwa-session-durability-run-owned-program
                                  session (list (list :op 'append :path log
                                                      :content "one\n"))))))
          (should (equal "one\n" (niwa-session-control-fs-read-file log))))
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors (niwa-session-durability-lease-release session-dir session))
      (delete-directory root t)))

  :doc "stops before its operations when the committed bytes changed"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-owned-program-" t)))
         (session-dir (file-name-as-directory (file-name-concat root "session")))
         (session (test-niwa-session-durability--local-session root))
         (log (file-name-concat session-dir "diagnostic.log"))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          ;; Another writer replaced this client's record behind its back.
          (let* ((lease (niwa-session-lease session))
                 (path (niwa-session-durability--generation-path
                        (niwa-session-durability--lease-path session-dir)
                        (plist-get lease :generation))))
            (niwa-session-control-fs-write-file
             path (concat (niwa-session-control-fs-read-file path) " ")))
          (should-not (niwa-session-durability-run-owned-program
                       session (list (list :op 'append :path log :content "x"))))
          (should-not (file-exists-p log))
          ;; The stale belief is dropped so the next renewal observes.
          (should-not (plist-get (niwa-session-lease session) :bytes)))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t)))

  :doc "stops assuming ownership when a newer live generation appears"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-owned-program-" t)))
         (session-dir (file-name-as-directory (file-name-concat root "session")))
         (session (test-niwa-session-durability--local-session root))
         (log (file-name-concat session-dir "diagnostic.log"))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (let* ((lease (niwa-session-lease session))
                 (directory (niwa-session-durability--lease-path session-dir))
                 (next (1+ (plist-get lease :generation))))
            ;; A foreign claim activated the next generation while this
            ;; client's record survived its best-effort pruning.
            (niwa-session-durability--write-plist
             (niwa-session-durability--generation-path directory next)
             (let ((niwa-session-durability--client-id (make-string 64 ?b)))
               (niwa-session-durability--lease-record
                "*foreign*" next 'active nil nil
                (niwa-session-durability--target-time directory)))))
          ;; The precondition held at the proof, so the append ran ...
          (should (niwa-session-durability-run-owned-program
                   session (list (list :op 'append :path log :content "x"))))
          ;; ... but this client no longer assumes it owns the lease.
          (should-not (plist-get (niwa-session-lease session) :bytes)))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-call-with-held-lease ()
  ,test
  (test)
  :doc "holds publication active without writing the lease"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-held-lease-" t)))
         (session-dir (file-name-as-directory (file-name-concat root "session")))
         (session (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (program (symbol-function 'niwa-session-control-fs-run-program))
         (programs 0)
         active)
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (&rest args) (cl-incf programs) (apply program args))))
            (should (eq 'done
                        (niwa-session-durability-call-with-held-lease
                         session
                         (lambda ()
                           (setq active (niwa-session-publication-active-p session))
                           'done)))))
          (should active)
          (should-not (niwa-session-publication-active-p session))
          (should (= 0 programs)))
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors (niwa-session-durability-lease-release session-dir session))
      (delete-directory root t)))

  :doc "releases a lease requested while held, and rethrows the function's error"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-held-lease-" t)))
         (session-dir (file-name-as-directory (file-name-concat root "session")))
         (session (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (should-error
           (niwa-session-durability-call-with-held-lease
            session
            (lambda ()
              ;; Release defers itself behind the active publication.
              (niwa-session-durability-lease-release session-dir session)
              (should (plist-get (niwa-session-lease session) :release-pending))
              (error "Injected failure"))))
          (should-not (niwa-session-publication-active-p session))
          (should-not (niwa-session-lease session))
          (should (eq 'available
                      (niwa-session-durability-lease-state session-dir))))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-call-with-reserved-lease ()
  ,test
  (test)
  :doc "suppresses timer target I/O and verifies ownership after reserved work"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-reserved-lease-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (rename-file-function (symbol-function 'rename-file))
         (renames 0))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (file newname &optional ok-if-exists)
                       (cl-incf renames)
                       (funcall rename-file-function
                                file newname ok-if-exists))))
            (should
             (eq 'reserved
                 (niwa-session-durability-call-with-reserved-lease
                  session
                  (lambda ()
                    (let ((before renames))
                      (should
                       (niwa-session-durability-lease-renew session))
                      (should (= before renames)))
                    'reserved)))))
          (should-not (niwa-session-publication-active-p session))
          (should (eq 'active
                      (plist-get (niwa-session-lease session) :status))))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "signals and marks authority lost when ownership changes during work"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-reserved-loss-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-niwa-session-durability--local-session local-root))
         (successor-session
          (test-niwa-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (niwa-session-publication-lease-seconds 100)
         (now 0.0))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner-session) session-dir
          (niwa-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (should-error
           (niwa-session-durability-call-with-reserved-lease
            owner-session
            (lambda ()
              (setq now 101.0)
              (let ((niwa-session-durability--client-id successor))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) t)))
                  (should
                   (niwa-session-durability-lease-acquire
                    session-dir "*successor*" successor-session))))))
           :type 'user-error)
          (should (eq 'lost
                      (plist-get (niwa-session-lease owner-session)
                                 :state))))
      (niwa-session-durability--cancel-renewal owner-session)
      (let ((niwa-session-durability--client-id successor))
        (niwa-session-durability-lease-release
         session-dir successor-session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "drains a deferred release when reserved work exits nonlocally"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-reserved-throw-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          (should
           (eq 'out
               (catch 'nonlocal
                 (niwa-session-durability-call-with-reserved-lease
                  session
                  (lambda ()
                    (niwa-session-durability-lease-release
                     session-dir session)
                    (should (plist-get (niwa-session-lease session)
                                       :release-pending))
                    (throw 'nonlocal 'out))))))
          (should-not (niwa-session-lease-renewal-timer session))
          (should
           (eq 'released
               (plist-get
                (niwa-session-durability--lease-head
                 (file-name-concat session-dir ".lease"))
                :status))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-commit-publication-head ()
  ,test
  (test)
  :doc "validates and preserves a publication head through lease transitions"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-head-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-niwa-session-durability--local-session local-root))
         (successor-session
          (test-niwa-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (head ".publications/0001/manifest.el"))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner-session) session-dir
          (niwa-session-save-path successor-session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (dolist (invalid
                   '(nil "" "/absolute" "../manifest" "a/../manifest"
                     "/ssh:host:/manifest" "~/manifest" "a//manifest"
                     "a/./manifest" "manifest/" "session.meta.el"
                     ".lease/manifest.el" ".publications/plain.el"))
            (should-error
             (niwa-session-durability-commit-publication-head
              owner-session invalid)
             :type 'error))
          (should
           (niwa-session-durability--renew-publication-lease
            owner-session))
          (should
           (niwa-session-durability-commit-publication-head
            owner-session head))
          (should (equal head
                         (plist-get (niwa-session-lease owner-session)
                                    :publication-head)))
          (should (niwa-session-durability-lease-renew owner-session))
          (should (equal head
                         (plist-get (niwa-session-lease owner-session)
                                    :publication-head)))
          (niwa-session-durability-lease-release
           session-dir owner-session)
          (should (equal head
                         (niwa-session-durability-publication-head
                          session-dir)))
          (let ((niwa-session-durability--client-id successor))
            (should (niwa-session-durability-lease-acquire
                     session-dir "*successor*" successor-session))
            (should (equal head
                           (plist-get
                            (niwa-session-lease successor-session)
                            :publication-head)))
            (niwa-session-durability-lease-release
             session-dir successor-session)))
      (niwa-session-durability--cancel-renewal owner-session)
      (niwa-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a stale commit cannot replace a newer generation's inherited head"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-head-race-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (owner-session
          (test-niwa-session-durability--local-session local-root))
         (successor-session
          (test-niwa-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (niwa-session-publication-lease-seconds 100)
         (now 0.0)
         (triggered nil)
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path owner-session) session-dir
          (niwa-session-save-path successor-session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" owner-session))
          (should
           (niwa-session-durability--renew-publication-lease
            owner-session))
          (should
           (niwa-session-durability-commit-publication-head
            owner-session ".publications/old/manifest.el"))
          (setq now 80.0)
          ;; The stale commit states its precondition as the `verify'
          ;; opening its program; a successor takeover immediately before
          ;; that program runs is the last moment the race can happen.
          (cl-letf
              (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (operations &optional lock-directory)
                  (when (and (not triggered)
                             (eq 'verify (plist-get (car operations) :op)))
                    (setq triggered t
                          now 101.0)
                    (let ((niwa-session-durability--client-id successor))
                      (cl-letf (((symbol-function 'y-or-n-p)
                                 (lambda (&rest _) t)))
                        (should
                         (niwa-session-durability-lease-acquire
                          session-dir "*successor*" successor-session)))))
                  (funcall run-program-function operations lock-directory))))
            (should (equal
                     (should-error
                      (niwa-session-durability-commit-publication-head
                       owner-session ".publications/stale/manifest.el")
                      :type 'user-error)
                     '(user-error "Portable session lease changed before this update committed"))))
          (should triggered)
          (should
           (equal ".publications/old/manifest.el"
                  (plist-get (niwa-session-lease successor-session)
                             :publication-head)))
          (let ((niwa-session-durability--client-id successor))
            (should
             (niwa-session-durability--renew-publication-lease
              successor-session))
            (should
             (niwa-session-durability-commit-publication-head
              successor-session ".publications/new/manifest.el"))
            (should
             (equal ".publications/new/manifest.el"
                    (niwa-session-durability-publication-head session-dir)))
            (niwa-session-durability-lease-release
             session-dir successor-session)))
      (niwa-session-durability--cancel-renewal owner-session)
      (niwa-session-durability--cancel-renewal successor-session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-publication-head ()
  ,test
  (test)
  :doc "reads the uncached current head without creating or acquiring a lease"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-head-read-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (lease-directory (file-name-concat session-dir ".lease"))
         (list-directory-function
          (symbol-function 'niwa-session-control-fs-list-directory))
         (helper-called nil))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (should-not
           (niwa-session-durability-publication-head session-dir))
          (should-not (file-exists-p lease-directory))
          (make-directory lease-directory)
          (let ((niwa-session-durability--client-id (make-string 64 ?a)))
            (should
             (niwa-session-durability--create-plist
              (niwa-session-durability--generation-path lease-directory 1)
              (niwa-session-durability--lease-record
               "*old*" 1 'active ".publications/old/manifest.el" nil
               (niwa-session-durability--target-time lease-directory)))))
          (let ((niwa-session-durability--client-id (make-string 64 ?b)))
            (should
             (niwa-session-durability--create-plist
              (niwa-session-durability--generation-path lease-directory 2)
              (niwa-session-durability--lease-record
               "*new*" 2 'active ".publications/new/manifest.el" nil
               (niwa-session-durability--target-time lease-directory)))))
          (cl-letf
              (((symbol-function 'niwa-session-control-fs-list-directory)
                (lambda (directory regexp)
                  (if (not (equal directory lease-directory))
                      (funcall list-directory-function directory regexp)
                    (setq helper-called t)
                    (funcall list-directory-function directory regexp)))))
            (should
             (equal ".publications/new/manifest.el"
                    (niwa-session-durability-publication-head
                     session-dir))))
          (should helper-called))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication--publish-artifact ()
  ,test
  (test)
  :doc "builds a missing parent chain instead of failing the artifact"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-chain-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         ;; Two missing levels below the session directory: the
         ;; single-level optional ensure alone cannot create them.
         (artifact (file-name-concat session-dir "tool-results"
                                     "executions" "exec-000001.log"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should (niwa-session-publication-publish
                   session
                   (list (list :path artifact :content "execution log"))))
          (should (equal "execution log"
                         (with-temp-buffer
                           (insert-file-contents artifact)
                           (buffer-string))))
          (niwa-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication-read ()
  ,test
  (test)
  :doc "validates manifest and sidecar without downloading every artifact"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-read-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should-not
           (niwa-session-publication-read session-dir))
          (should-not (file-exists-p (file-name-concat session-dir ".lease")))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (niwa-session-publication-read session-dir))
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
                    (niwa-session-publication-read session-dir)))
            (with-temp-file sidecar-path (insert "corrupt sidecar"))
            (should-error
             (niwa-session-publication-read session-dir)
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
             (niwa-session-publication-read session-dir)
             :type 'error)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a publication head symlink that escapes immutable storage"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-head-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (outside-dir (file-name-as-directory
                       (file-name-concat local-root "outside")))
         (session (test-niwa-session-durability--local-session local-root))
         (escaped-head ".publications/escaped-head/manifest.el")
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (make-directory outside-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (let* ((publication
                  (niwa-session-publication-publish
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
             (niwa-session-durability--renew-publication-lease session))
            (should
             (niwa-session-durability-commit-publication-head
              session escaped-head))
            (should
             (niwa-session-durability--finish-publication-lease session))
            (should-error
             (niwa-session-publication-read session-dir)
             :type 'error)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a manifest artifact symlink that escapes immutable storage"
  (let* ((local-root
          (file-name-as-directory
           (make-temp-file "niwa-publication-artifact-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (outside-dir (file-name-as-directory
                       (file-name-concat local-root "outside")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (make-directory outside-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (let* ((publication
                  (niwa-session-publication-publish
                   session
                   (list (list :path segment :content "segment")
                         (list :path sidecar :content "sidecar"
                               :commit-marker t))))
                 (manifest-path
                  (file-name-concat session-dir
                                    (plist-get publication :head)))
                 (manifest
                  (niwa-session-durability--read-plist manifest-path))
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
             (niwa-session-publication-read session-dir)
             :type 'error)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects a publication root symlink outside the physical session"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-root-escape-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (publication-dir (file-name-concat session-dir ".publications"))
         (outside-dir (file-name-concat local-root "outside-publications"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path segment :content "segment")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (rename-file publication-dir outside-dir)
          (make-symbolic-link outside-dir publication-dir)
          (should-error
           (niwa-session-publication-read session-dir)
           :type 'error))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication-logical-path-p ()
  ,test
  (test)
  :doc "accepts normalized session artifacts outside durability control paths"
  (dolist (path '("session.meta.el" "agents/worker.chat.org"
                  "instructions/current.el"))
    (should (niwa-session-publication-logical-path-p path)))
  (dolist (path '(nil "" "/absolute" "../escape" "a/../escape"
                  ".lock" ".lease/0001.el" ".publications/generation/file"
                  ".journal-pins" ".journal-pins/capture.json"
                  ".recovery/recovery-0001/bytes"
                  "/ssh:other:/session.meta.el" "~/session.meta.el"))
    (should-not (niwa-session-publication-logical-path-p path))))

(niwa-deftest niwa-session-publication-uncommitted-artifact ()
  ,test
  (test)
  :doc "returns the newest retained or queued local source only"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-uncommitted-artifact-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (external (file-name-concat local-root "external.el"))
         (logical "agents/worker.chat.org")
         (target (file-name-concat session-dir logical))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path target :content "retained"
                        :coding 'utf-8-unix :opaque 'kept)
                  (list :path external :content "external"))))
          (let ((artifact
                 (car
                  (plist-get
                   (car
                    (niwa-session-publication-uncommitted-batches session))
                   :artifacts))))
            (should-not (plist-member artifact :content))
            (should (eq 'utf-8-unix (plist-get artifact :coding)))
            (should (eq 'kept (plist-get artifact :opaque))))
          (let ((source
                 (niwa-session-publication-uncommitted-artifact
                  session logical)))
            (should (file-name-absolute-p source))
            (should-not (file-remote-p source))
            (should (equal "retained"
                           (with-temp-buffer
                             (insert-file-contents-literally source)
                             (buffer-string)))))
          (setf (niwa-session-publication-active-p session) t)
          (should
           (eq 'queued
               (niwa-session-publication-publish
                session (list (list :path target :content "queued")))))
          (should
           (equal "queued"
                  (with-temp-buffer
                    (insert-file-contents-literally
                     (niwa-session-publication-uncommitted-artifact
                      session logical))
                    (buffer-string))))
          (should-not
           (niwa-session-publication-uncommitted-artifact
            session "missing.el"))
          (should-not
           (niwa-session-publication-uncommitted-artifact
            session "external.el"))
          (should-error
           (niwa-session-publication-uncommitted-artifact
            session "../external.el")
           :type 'error))
      (setf (niwa-session-publication-active-p session) nil)
      ;; The queued publication is deliberately never drained; releasing
      ;; over it reports the retained recovery, which this teardown owns.
      (niwa-test--with-captured-diagnostics nil
        (niwa-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication-discard-rolled-back ()
  ,test
  (test)
  :doc "removes only local failed recovery and leaves the committed head"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-rollback-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         old-head
         recovery)
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path sidecar :content "old" :commit-marker t))))
          (setq old-head
                (niwa-session-durability-publication-head session-dir))
          (cl-letf
              (((symbol-function
                 'niwa-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session
                (list (list :path sidecar :content "new" :commit-marker t)))
               :type 'user-error)))
          (setq recovery
                (mapcar
                 (lambda (batch) (plist-get batch :directory))
                 (plist-get (niwa-session-pending-publication session)
                            :batches)))
          (should (cl-every #'file-directory-p recovery))
          (should
           (niwa-session-publication-discard-rolled-back
            session))
          (should-not (niwa-session-pending-publication session))
          (should (cl-every (lambda (path) (not (file-exists-p path)))
                            recovery))
          (should (equal old-head
                         (niwa-session-durability-publication-head
                          session-dir))))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-disclose ()
  ,test
  (test)
  :doc "remote disclosure precedes every target-side state write and occurs once"
  (let* ((host "disclosure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-disclosure-" t)))
         (session nil)
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         (prompts 0))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (cl-incf prompts)
                         nil)))
              (should-error
               (niwa-session-artifacts-assert-mutation-authority
                session)
               :type 'user-error))
            (should-not
             (file-exists-p (file-name-concat local-root ".niwa")))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (cl-incf prompts)
                         t)))
              (should
               (niwa-session-artifacts-assert-mutation-authority
                session))
              (should
               (niwa-session-durability-disclose session)))
            (should (= 2 prompts))
            (when-let* ((save-path (niwa-session-save-path session)))
              (niwa-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "an advance acceptance stores target-side state without asking"
  (let* ((host "accepted-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-accepted-" t)))
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         (niwa-session-durability-accept-target-storage t)
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session (test-niwa-session-durability--remote-session
                         host local-root))
          ;; A host nobody sits at, such as a daemon serving guests.
          (let ((inhibit-interaction t))
            (should (niwa-session-durability-disclose session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication-publish ()
  ,test
  (test)
  :doc "marker deduplication is ordered and excludes external artifacts"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-overlay-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (duplicate (file-name-concat session-dir "duplicate.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (external (file-name-concat local-root "external.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path duplicate :content "first")
                  (list :path external :content "external")
                  (list :path duplicate :content "last")
                  (list :path sidecar :content "sidecar"
                        :commit-marker t))))
          (let* ((publication
                  (niwa-session-publication-read session-dir))
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
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "remote marker creates its immutable directory through a file handler"
  (let* ((host "publication-manifest-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-manifest-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (session nil)
         (niwa-session-durability--client-id (make-string 64 ?a))
         (run-program-function
          (symbol-function 'niwa-session-control-fs-run-program))
         (immutable-directories 0))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          ;; The generation directories ride the write-generation program
          ;; as make-directory operations rather than wrapper calls.
          (cl-letf
              (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (operations &optional lock-directory)
                  (dolist (operation operations)
                    (when (and (eq 'make-directory
                                   (plist-get operation :op))
                               (string-match-p
                                "/\\.publications"
                                (plist-get operation :path)))
                      (cl-incf immutable-directories)))
                  (funcall run-program-function operations lock-directory))))
            (should
             (niwa-session-publication-publish
              session
              (list
               (list :path (file-name-concat session-dir "session.meta.el")
                     :content "sidecar" :commit-marker t)))))
          (should (= 2 immutable-directories))
          (should (file-remote-p
                   (plist-get
                    (niwa-session-publication-read session-dir)
                    :sidecar)))
          (niwa-session-durability-lease-release session-dir session))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "replacement marker resets the prior logical snapshot"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-replace-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (niwa-workspace-identity-ensure local-root)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path first :content "first")
                  (list :path second :content "old second")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path second :content "new second")
                  (list :path sidecar :content
                        (niwa-session-artifacts-printed-value
                         (niwa-session-codec-serialize session))
                        :commit-marker t :replace t))))
          (let ((artifacts
                 (plist-get
                  (niwa-session-publication-read session-dir)
                  :artifacts)))
            (should (equal '("second.el" "session.meta.el")
                           (mapcar #'car artifacts)))
            (should-not (assoc "first.el" artifacts)))
          (should (file-exists-p first)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "reentrant non-marker and marker batches commit in enqueue order"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-reentrant-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (required (file-name-concat session-dir "required.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (publish-artifact
          (symbol-function 'niwa-session-publication--publish-artifact))
         injected)
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'niwa-session-publication--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact artifact)
                    (unless injected
                      (setq injected t)
                      (should-error
                       (niwa-session-publication-publish
                        session
                        (list (list :path required :content "required"))
                        t)
                       :type 'user-error)
                      (should
                       (eq 'queued
                           (niwa-session-publication-publish
                            session
                            (list (list :path second :content "second")))))
                      (should
                       (eq 'queued
                           (niwa-session-publication-publish
                            session
                            (list
                             (list :path sidecar :content "sidecar"
                                   :commit-marker t))))))))))
            (should
             (niwa-session-publication-publish
              session (list (list :path first :content "first")))))
          (should
           (equal '("first.el" "second.el" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (niwa-session-publication-read session-dir)
                    :artifacts))))
          (should-not (niwa-session-publication-queue session))
          (should-not
           (niwa-session-publication-uncommitted-batches session)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "rejects invalid markers, control paths, and cross-target writes"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-invalid-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
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
             (niwa-session-publication-publish session artifacts)
             :type 'error))
          (should-not (niwa-session-pending-publication session))
          (should-not (niwa-session-durability-publication-head
                       session-dir)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "staged transcripts remain private until a marker commits a snapshot"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-manifest-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment-0001.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session
            (list (list :path segment :content "old segment")
                  (list :path sidecar :content "old sidecar"
                        :commit-marker t))))
          (let* ((old (niwa-session-publication-read
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
             (niwa-session-publication-publish
              session (list (list :path segment :content "new segment"))))
            (should (equal "new segment"
                           (with-temp-buffer
                             (insert (niwa-session-artifacts-read-artifact
                                      session "segment-0001.chat.org"))
                             (buffer-string))))
            (should
             (equal old
                    (niwa-session-publication-read session-dir)))
            (should
             (niwa-session-publication-publish
              session
              (list (list :path sidecar :content "new sidecar"
                          :commit-marker t))))
            (let* ((new (niwa-session-publication-read
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
              (should (equal new (niwa-session-publication session)))
              (should-not
               (niwa-session-publication-uncommitted-batches session)))))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "pre-CAS failure retains prior and current sources for convergent retry"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-pre-cas-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (segment (file-name-concat session-dir "segment.chat.org"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (temporary-file-directory local-root)
         recovery)
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (should
           (niwa-session-publication-publish
            session (list (list :path segment :content "segment"))))
          (cl-letf
              (((symbol-function
                 'niwa-session-durability-commit-publication-head)
                (lambda (&rest _) nil)))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session
                (list (list :path sidecar :content "sidecar"
                            :commit-marker t)))
               :type 'user-error)))
          (let ((batches
                 (plist-get (niwa-session-pending-publication session)
                            :batches)))
            (should (= 2 (length batches)))
            (setq recovery
                  (mapcar (lambda (batch) (plist-get batch :directory))
                          batches)))
          (should-not
           (niwa-session-publication-uncommitted-batches session))
          (should-not
           (niwa-session-durability-publication-head session-dir))
          ;; Another publisher can create a sibling between filesystem
          ;; reads.  Keep real path resolution while changing parent metadata.
          (let ((native-truename (symbol-function 'file-truename))
                (parent (directory-file-name local-root))
                (changes 0))
            (cl-letf (((symbol-function 'file-truename)
                       (lambda (file &rest args)
                         (prog1 (apply native-truename file args)
                           (when (equal (directory-file-name file) parent)
                             (make-temp-file
                              (file-name-concat parent "concurrent-") t)
                             (cl-incf changes))))))
              (should (niwa-session-publication-retry session)))
            (should (> changes 0)))
          (should-not (niwa-session-pending-publication session))
          (should (cl-every (lambda (path) (not (file-exists-p path)))
                            recovery))
          (should
           (equal '("segment.chat.org" "session.meta.el")
                  (mapcar
                   #'car
                   (plist-get
                    (niwa-session-publication-read session-dir)
                    :artifacts)))))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "required post-CAS cleanup failure succeeds; ordinary callers classify it"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-post-cas-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (temporary-file-directory local-root)
         (before (directory-files temporary-file-directory nil
                                  "\\`niwa-publication-")))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function
                 'niwa-session-durability--finish-publication-lease)
                (lambda (current)
                  (setf (plist-get (niwa-session-lease current) :state)
                        'lost)
                  nil)))
            (niwa-test--with-captured-diagnostics nil
              (should
               (niwa-session-publication-publish
                session
                (list (list :path sidecar :content "committed"
                            :commit-marker t))
                t))))
          (let ((publication
                 (niwa-session-publication-read session-dir)))
            (should publication)
            (should (equal publication
                           (niwa-session-publication session))))
          (should-not (niwa-session-pending-publication session))
          (should-not
           (niwa-session-publication-uncommitted-batches session))
          (should (eq 'lost
                      (plist-get (niwa-session-lease session) :state)))
          (should (equal before
                         (directory-files temporary-file-directory nil
                                          "\\`niwa-publication-")))
          (setf (plist-get (niwa-session-lease session) :state) 'active)
          (cl-letf
              (((symbol-function
                 'niwa-session-durability--finish-publication-lease)
                (lambda (current)
                  (setf (plist-get (niwa-session-lease current) :state)
                        'lost)
                  nil)))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session
                (list (list :path sidecar :content "classified"
                            :commit-marker t)))
               :type 'error)))
          (should (equal "classified"
                         (with-temp-buffer
                           (insert-file-contents
                            (plist-get
                             (niwa-session-publication-read session-dir)
                             :sidecar))
                           (buffer-string))))
          (should-not (niwa-session-pending-publication session))
          (should-not
           (niwa-session-publication-uncommitted-batches session)))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "non-marker publication retains local source until lease release"
  (let* ((host "publication-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-publication-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (target (file-name-concat session-dir "state.el"))
         (session nil)
         (niwa-session-durability--client-id (make-string 64 ?a))
         (temporary-file-directory local-root)
         (before (directory-files temporary-file-directory nil
                                  "\\`niwa-publication-")))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (should (niwa-session-publication-publish
                   session
                   (list (list :path target :content "new-state"))))
          (should (niwa-session-durability-lease-owned-p session))
          (should (equal "new-state"
                         (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))))
          (should-not (niwa-session-pending-publication session))
          (let ((recovery
                 (mapcar
                  (lambda (batch) (plist-get batch :directory))
                  (niwa-session-publication-uncommitted-batches session))))
            (should (= 1 (length recovery)))
            (should (file-directory-p (car recovery)))
            (niwa-session-persistence-lock-release session-dir session)
            (should-not (file-exists-p (car recovery))))
          (should (equal before
                         (directory-files temporary-file-directory nil
                                          "\\`niwa-publication-"))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "mid-publication failure blocks mutation and retry consumes recovery"
  (let* ((host "publication-retry-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-retry-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (first (file-name-concat session-dir "first.el"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (second (file-name-concat session-dir "blocker" "second.el"))
         (session (test-niwa-session-durability--remote-session
                   host local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (niwa-test--with-captured-diagnostics nil
            (should-error
             (niwa-session-publication-publish
              session
              (list (list :path first :content "first")
                    (list :path second :content "second")
                    (list :path sidecar :content "sidecar"
                          :commit-marker t)))
             :type 'file-error))
          (let* ((pending (niwa-session-pending-publication session))
                 (batch (car (plist-get pending :batches)))
                 (recovery (plist-get batch :directory)))
            (should (file-directory-p recovery))
            (should-error
             (niwa-session-artifacts-assert-mutation-authority session)
             :type 'user-error)
            (delete-file blocker-local)
            (make-directory blocker-local)
            (should
             (niwa-session-publication-retry session))
            (should-not (file-exists-p recovery)))
          (should-not (niwa-session-pending-publication session))
          (should (equal "second"
                         (with-temp-buffer
                           (insert-file-contents second)
                           (buffer-string))))
          (niwa-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "explicit abandonment removes retained recovery and unblocks admission"
  (let* ((host "publication-abandon-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-abandon-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (blocker-local (file-name-concat session-dir-local "blocker"))
         (target (file-name-concat session-dir "blocker" "state.el"))
         (session nil)
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (with-temp-file blocker-local (insert "not-a-directory"))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-persistence-lock-acquire
                   session-dir "*publisher*" session))
          (niwa-test--with-captured-diagnostics nil
            (should-error
             (niwa-session-publication-publish
              session (list (list :path target :content "state")))
             :type 'file-error))
          (let* ((pending (niwa-session-pending-publication session))
                 (recovery
                  (plist-get (car (plist-get pending :batches)) :directory)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (niwa-test--with-captured-diagnostics nil
                (should
                 (niwa-session-publication-abandon session))))
            (should-not (file-exists-p recovery)))
          (should-not (niwa-session-pending-publication session))
          (with-temp-buffer
            (setq-local niwa--session session)
            (should
             (niwa-session-artifacts-assert-mutation-authority session)))
          (niwa-session-persistence-lock-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "long publication fences takeover without reentrant timer I/O"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-lease-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (first (file-name-concat session-dir "first.el"))
         (second (file-name-concat session-dir "second.el"))
         (session (test-niwa-session-durability--remote-session
                   "publication-lease-host" local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (niwa-session-lease-seconds 10)
         (niwa-session-publication-lease-seconds 100)
         (now 0.0)
         (artifact-number 0)
         (takeover-prompts 0)
         (publish-artifact-function
          (symbol-function 'niwa-session-publication--publish-artifact))
         (rename-file-function (symbol-function 'rename-file)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (cl-incf takeover-prompts)
                     t)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'niwa-session-publication--publish-artifact)
                (lambda (artifact)
                  (cl-incf artifact-number)
                  (setq now (if (= artifact-number 1) 50.0 150.0))
                  (let ((niwa-session-durability--client-id successor))
                    (should-not
                     (niwa-session-durability-lease-acquire
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
                         (niwa-session-durability-lease-renew session)))
                      (should (= 0 renames))))
                  (prog1 (funcall publish-artifact-function artifact)
                    (when (= artifact-number 1)
                      (setq now 95.0))))))
            (should
             (niwa-session-publication-publish
              session
              (list (list :path first :content "first")
                    (list :path second :content "second")))))
          (should (= 0 takeover-prompts))
          (should (niwa-session-durability-lease-owned-p session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a pre-artifact lease failure retains the staged recovery batch"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-reserve-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function
                 'niwa-session-durability--renew-publication-lease)
                (lambda (&rest _) nil)))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session (list (list :path target :content "state")))
               :type 'user-error)))
          (should-not (file-exists-p target))
          (let* ((pending (niwa-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))))
      (when-let* ((pending (niwa-session-pending-publication session)))
        (dolist (batch (plist-get pending :batches))
          (niwa-session-publication--delete-batch batch)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "an expired final artifact fails closed and retains local recovery"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-expired-publication-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (target (file-name-concat session-dir "state.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (niwa-session-durability--client-id owner)
         (niwa-session-publication-lease-seconds 100)
         (now 0.0)
         (takeover-prompt nil)
         (publish-artifact-function
          (symbol-function 'niwa-session-publication--publish-artifact)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                   (lambda (&optional _) now)))
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (cl-letf
              (((symbol-function 'niwa-session-publication--publish-artifact)
                (lambda (artifact)
                  (prog1 (funcall publish-artifact-function artifact)
                    (setq now 101.0)
                    (let ((niwa-session-durability--client-id successor))
                      (cl-letf
                          (((symbol-function 'y-or-n-p)
                            (lambda (prompt)
                              (setq takeover-prompt prompt)
                              t)))
                        (should
                         (niwa-session-durability-lease-acquire
                          session-dir "*successor*"))))))))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-publish
                session (list (list :path target :content "state")))
               :type 'user-error)))
          (should (string-match-p "critical write may still be in flight"
                                  takeover-prompt))
          (should (string-match-p "prior client is stopped"
                                  takeover-prompt))
          (let* ((pending (niwa-session-pending-publication session))
                 (batch (car (plist-get pending :batches))))
            (should pending)
            (should (file-directory-p (plist-get batch :directory)))))
      (when-let* ((pending (niwa-session-pending-publication session)))
        (dolist (batch (plist-get pending :batches))
          (niwa-session-publication--delete-batch batch)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "a deletion tombstone removes an overlaid manifest entry atomically"
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-publication-delete-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (artifact (file-name-concat session-dir "artifacts" "mockup.html"))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*publisher*" session))
          (niwa-session-publication-publish
           session
           (list (list :path artifact :content "mockup")
                 (list :path sidecar :content "one" :commit-marker t)))
          (should (assoc "artifacts/mockup.html"
                         (plist-get (niwa-session-publication-read session-dir)
                                    :artifacts)))
          (niwa-session-publication-publish
           session
           (list (list :path artifact :delete t)
                 (list :path sidecar :content "two" :commit-marker t)))
          (should-not
           (assoc "artifacts/mockup.html"
                  (plist-get (niwa-session-publication-read session-dir)
                             :artifacts)))
          (should-error
           (niwa-session-publication-publish
            session
            (list (list :path (file-name-concat local-root "outside")
                        :delete t)))))
      (niwa-session-durability-lease-release session-dir session)
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-publication-append-diagnostic
  (:quiet t)
  ,test
  (test)
  :doc "retries a failed remote diagnostic append without blocking mutation"
  (let* ((host "diagnostic-retry-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-diagnostic-" t)))
         (session nil)
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "initial transcript")
            (niwa-session-artifacts-save session (current-buffer))
            (let* ((save-path (niwa-session-save-path session))
                   (log-path (file-name-concat save-path "hook-log.el"))
                   (entry '(:event Stop :status completed))
                   (run-program
                    (symbol-function
                     'niwa-session-control-fs-run-program))
                   (diagnostic-publications 0)
                   warning)
              (make-directory log-path)
              (niwa-hooks--log session entry)
              (goto-char (point-max))
              (insert "\nsettled")
              (cl-letf
                  (((symbol-function 'display-warning)
                    (lambda (_type message &rest _)
                      (setq warning message))))
                (should (niwa-session-artifacts-save
                         session (current-buffer)))
                ;; The diagnostic flush is deferred off the save; drive
                ;; the timer until its failure warning lands.
                (let ((deadline (+ (float-time) 2)))
                  (while (and (not warning) (< (float-time) deadline))
                    (accept-process-output nil 0.02))))
              (should warning)
              ;; The log handed its entry over; the target refused it, so
              ;; it waits in the session's diagnostic backlog.
              (should-not (niwa-session-hook-log-pending session))
              (should (niwa-session-publication-diagnostic-backlog-p session))
              (should-not (niwa-session-pending-publication session))
              (should
               (niwa-session-artifacts-assert-mutation-authority session))
              (delete-directory log-path)
              (cl-letf
                  (((symbol-function
                     'niwa-session-control-fs-run-program)
                    (lambda (operations &rest args)
                      (dolist (op operations)
                        (when (and (eq 'append (plist-get op :op))
                                   (equal (file-name-nondirectory log-path)
                                          (file-name-nondirectory
                                           (plist-get op :path))))
                          (cl-incf diagnostic-publications)))
                      (apply run-program operations args))))
                (should (niwa-session-artifacts-save
                         session (current-buffer)))
                (let ((deadline (+ (float-time) 2)))
                  (while (and (zerop diagnostic-publications)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.02))))
              (should (= 1 diagnostic-publications))
              (should-not (niwa-session-publication-diagnostic-backlog-p session))
              (should-not (niwa-session-pending-publication session))
              (with-temp-buffer
                (insert-file-contents log-path)
                (should (equal entry (read (current-buffer)))))
              (niwa-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "quietly retains a diagnostic while the lease cannot carry it"
  ;; A renewal the transport cannot run right now is a retry, not a
  ;; failure: nothing reads a diagnostic live, and echoing an error per
  ;; attempt buries real failures in expected noise.
  (let* ((local-root (file-name-as-directory
                      (make-temp-file "niwa-diagnostic-quiet-" t)))
         (session-dir (file-name-as-directory
                       (file-name-concat local-root "session")))
         (session (test-niwa-session-durability--local-session local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (captured nil))
    (make-directory session-dir t)
    (setf (niwa-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should (niwa-session-durability-lease-acquire
                   session-dir "*owner*" session))
          ;; Without the bytes it last committed, this client must renew
          ;; before an append can prove ownership in its own program.
          (niwa-session-durability--bind-lease
           session (niwa-session-durability--strip-assumption
                    (niwa-session-lease session))
           'owned)
          (niwa-test--with-captured-diagnostics captured
            (cl-letf (((symbol-function
                        'niwa-session-durability-lease-renew)
                       (lambda (_session) nil)))
              (should-not
               (niwa-session-publication-append-diagnostic
                session
                (file-name-concat session-dir "telemetry-log.el")
                "(:event probe)\n"))))
          (should (string-empty-p captured))
          ;; The lease itself is untouched; the next attempt may publish.
          (should (niwa-session-durability-lease-owned-p session)))
      (niwa-session-durability--cancel-renewal session)
      (ignore-errors
        (niwa-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "final lease loss retains queued critical bytes as retryable batches"
  (let* ((host "diagnostic-critical-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-diagnostic-critical-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s/" host session-dir-local))
         (diagnostic (file-name-concat session-dir "hook-log.el"))
         (critical (file-name-concat session-dir "segment.chat.org"))
         (session
          (test-niwa-session-durability--remote-session host local-root))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (run-program
          (symbol-function 'niwa-session-control-fs-run-program))
         (finish-publication
          (symbol-function
           'niwa-session-durability--finish-publication-lease))
         injected
         fail-finish)
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should (niwa-session-durability-lease-acquire
                   session-dir "*diagnostic*" session))
          (cl-letf
              (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (operations &rest args)
                  (prog1 (apply run-program operations args)
                    ;; A critical publisher reached from a sentinel while
                    ;; the diagnostic append was in flight.
                    (when (and (not injected)
                               (seq-some (lambda (op) (eq 'append (plist-get op :op)))
                                         operations))
                      (setq injected t
                            fail-finish t)
                      (should
                       (eq 'queued
                           (niwa-session-publication-publish
                            session
                            (list (list :path critical
                                        :content "critical")))))))))
               ((symbol-function
                 'niwa-session-durability--finish-publication-lease)
                (lambda (current)
                  (if fail-finish
                      (progn
                        (setq fail-finish nil)
                        nil)
                    (funcall finish-publication current)))))
            (niwa-test--with-captured-diagnostics nil
              (should-error
               (niwa-session-publication-append-diagnostic
                session diagnostic "diagnostic")
               :type 'user-error))
            (let ((batches
                   (plist-get (niwa-session-pending-publication session)
                              :batches)))
              (should (= 1 (length batches)))
              (should (file-directory-p (plist-get (car batches) :directory)))
              (should (niwa-session-publication--batch-live-p
                       (car batches))))
            (should
             (eq 'published
                 (niwa-session-publication-retry session)))
            (should-not (niwa-session-pending-publication session))
            (let ((batches
                   (niwa-session-publication-uncommitted-batches session)))
              (should (= 1 (length batches)))
              (should (niwa-session-publication--batch-live-p
                       (car batches)))))
          (niwa-session-durability-lease-release session-dir session))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-artifacts-publish-agent-terminal-state
  ()
  ,test
  (test)
  :doc "publishes transcript and final registry sidecar as one retryable batch"
  (let* ((host "agent-terminal-host")
         ;; The transcript buffer visits a mock-remote file; a lockfile
         ;; there survives into teardown and warns on removal.
         (create-lockfiles nil)
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-agent-terminal-" t)))
         (root (format "/niwamock:%s:%s/" host local-root))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir
          (format "/niwamock:%s:%s" host session-dir-local))
         (transcript-relative "agents/explorer.chat.org")
         (transcript (concat session-dir transcript-relative))
         (sidecar (concat session-dir "session.meta.el"))
         session
         target
         (parent (generate-new-buffer " *remote-agent-parent*"))
         (child (generate-new-buffer " *remote-agent-child*"))
         (agent
          (niwa-agent--create
           :name "explorer" :description "Explore" :tools nil
           :system-prompt nil :max-turns nil :hook-rules nil :frozen-p t))
         (backend (gptel--make-backend :name "Agent Terminal"))
         (configuration
          (niwa-agent-configuration--create
           :agent agent
           :request-locals
           (mapcar
            (lambda (symbol)
              (cons symbol
                    (pcase symbol
                      ('gptel-backend backend)
                      ((or 'gptel-context 'gptel-tools) nil)
                      (_ nil))))
            niwa-agent-request-local-symbols)))
         invocation
         record
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root)
                target (niwa-session-execution-target session)
                invocation (niwa-agent-invocation-create agent)
                record
                (niwa-agent-record--create
                 :id "explorer--test" :path "/root/explorer"
                 :parent-path "/root" :role "explorer"
                 :configuration configuration :activity 'idle
                 :conversation-location transcript-relative))
          (test-niwa-session-durability--accept-storage session)
          (niwa-workspace-identity-ensure root)
          (setf (niwa-execution-target-incarnation target) "remote-host-a")
          (setf (niwa-session-save-path session) session-dir
                (niwa-session-session-id session) "agent-session"
                (niwa-session-created-at session) "created"
                (niwa-session-updated-at session) "updated"
                (niwa-session-current-segment session) 1
                (niwa-session-agent-registry session)
                (list (cons "/root/explorer" record))
                (niwa-session-agent-transcripts session)
                `(("explorer--test"
                   :agent-type "explorer" :agent-path "/root/explorer"
                   :description "Explore" :path ,transcript-relative
                   :status completed :created-at "created"
                   :updated-at "updated" :parent-turn 0)))
          (with-current-buffer parent
            (setq-local niwa--session session)
            (setq-local niwa-workspace-additional-roots nil))
          (with-current-buffer child
            (setq-local niwa--session session)
            (setq-local create-lockfiles nil)
            (setq buffer-file-name transcript)
            (insert "terminal transcript"))
          (setf (niwa-agent-invocation-agent-id invocation)
                "explorer--test"
                (niwa-agent-invocation-path invocation) "/root/explorer"
                (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent
                (niwa-agent-invocation-buffer invocation) child
                (niwa-agent-invocation-transcript-relative-path invocation)
                transcript-relative
                (niwa-agent-invocation-transcript-status invocation)
                'completed
                (niwa-agent-invocation-sidecar-dirty invocation) t)
          (should
           (niwa-session-persistence-lock-acquire
            session-dir "*agent-terminal*" session))
          ;; Establish the real target identity before testing a terminal
          ;; transaction.  Replacement acknowledgement is a separate commit.
          (niwa-session-artifacts-check-target-incarnation session parent)
          (should
           (niwa-session-publication-publish
            session
            (list (list :path sidecar :content "(:materialized t)"
                        :commit-marker t))))
          (let ((write-generation
                 (symbol-function 'niwa-session-publication--prepare-generation)))
            (cl-letf
                (((symbol-function 'niwa-session-publication--prepare-generation)
                  (lambda (root artifacts &rest args)
                    (if (cl-find transcript-relative artifacts
                                 :key (lambda (artifact) (plist-get artifact :logical))
                                 :test #'equal)
                        (signal 'file-error '("Injected agent generation failure"))
                      (apply write-generation root artifacts args)))))
              (niwa-test--with-captured-diagnostics nil
                (should-error
                 (niwa-session-artifacts-publish-agent-terminal-state
                  invocation)
                 :type 'file-error))))
          (let* ((pending (niwa-session-pending-publication session))
                 (batches (plist-get pending :batches))
                 (artifacts (plist-get (car batches) :artifacts)))
            (should (= 1 (length batches)))
            (should
             (equal (list transcript sidecar)
                    (mapcar (lambda (artifact)
                              (plist-get artifact :path))
                            artifacts))))
          (should-error
           (niwa-session-artifacts-assert-mutation-authority session)
           :type 'user-error)
          (should (niwa-session-publication-retry session))
          (should-not (niwa-session-pending-publication session))
          (should
           (equal "terminal transcript"
                  (with-temp-buffer
                    (insert (niwa-session-artifacts-read-artifact
                             session transcript-relative t))
                    (buffer-string))))
          (let* ((saved (niwa-session-codec-read sidecar))
                 (saved-record (car (plist-get saved :agent-registry))))
            (should (eq 'idle (plist-get saved-record :activity)))
            (should
             (eq 'completed
                 (plist-get
                  (cdr (assoc "explorer--test"
                              (plist-get saved :agent-transcripts)))
                  :status))))
          (niwa-session-persistence-lock-release session-dir session))
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "keeps a committed terminal head successful when post-save cleanup fails"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "niwa-agent-terminal-state-" t)))
         (parent (generate-new-buffer " *agent-terminal-parent*"))
         (session
          (test-niwa-session-durability--local-session root))
         (transcript (file-name-concat root "agents" "test.chat.org"))
         child
         published)
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure root)
          (setf (niwa-session-authority-mode session) 'portable
                (niwa-session-save-path session) root
                (niwa-session-root-buffer session) parent)
          (make-directory (file-name-directory transcript) t)
          (write-region "old\n" nil transcript nil 'silent)
          (setq child (find-file-noselect transcript))
          (with-current-buffer parent
            (setq-local niwa--session session))
          (with-current-buffer child
            (goto-char (point-max))
            (insert "terminal\n")
            (setq-local after-save-hook
                        (list (lambda () (error "Post-save hook failed")))))
          (let ((invocation
                 (niwa-agent-invocation--create
                  :agent-id "test-agent"
                  :path "/root/test-agent"
                  :parent-session session
                  :parent-data-buffer parent
                  :buffer child
                  :transcript-relative-path "agents/test.chat.org"
                  :sidecar-dirty t)))
            (setf (niwa-session-agent-transcripts session)
                  '(("test-agent" :path "agents/test.chat.org")))
            (cl-letf
                (((symbol-function
                   'niwa-session-codec-portable-authority-p)
                  (lambda (_session) t))
                 ((symbol-function
                   'niwa-session-artifacts-artifact-present-p)
                  (lambda (&rest _) t))
                 ((symbol-function
                   'niwa-session-artifacts-assert-mutation-authority)
                  (lambda (&rest _) t))
                 ((symbol-function 'niwa-session-publication-publish)
                  (lambda (_session artifacts &optional require-commit prepared _token)
                    (should require-commit)
                    (should prepared)
                    (setq published artifacts)
                    t)))
              (niwa-test--with-captured-diagnostics nil
                (should
                 (niwa-session-artifacts-publish-agent-terminal-state
                  invocation))))
            (should (= 2 (length published)))
            (should-not
             (niwa-agent-invocation-sidecar-dirty invocation))))
      (dolist (buffer (list child parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-artifacts-save/remote ()
  ,test
  (test)
  :doc "canonical remote save publishes segment and sidecar target-side"
  (let* ((host "canonical-save-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-save-" t)))
         (session nil)
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "remote transcript")
            (should (niwa-session-artifacts-save session (current-buffer)))
            (let* ((save-path (niwa-session-save-path session))
                   (segment (file-name-concat
                             save-path "segment-0001.chat.org"))
                   (sidecar (file-name-concat save-path "session.meta.el")))
              (should-not (file-exists-p segment))
              (should (niwa-session-artifacts-artifact-present-p
                       session "segment-0001.chat.org" t))
              (should (file-exists-p sidecar))
              ;; The workspace store owns notes: no instruction snapshot is
              ;; published.
              (should-not (niwa-session-artifacts-artifact-present-p
                           session "instructions/current.el" t))
              (should (equal "remote transcript"
                             (with-temp-buffer
                               (insert (niwa-session-artifacts-read-artifact
                                      session "segment-0001.chat.org"))
                               (buffer-string))))
              (should-not (buffer-modified-p))
              (should-not (file-exists-p
                           (file-name-concat save-path ".lock")))
              (should (file-directory-p
                       (file-name-concat save-path ".lease")))
              (let* ((saved (niwa-session-codec-read sidecar))
                     (workspace (plist-get saved :workspace)))
                (should-not
                 (file-remote-p (plist-get workspace :target-native-root))))
              (niwa-session-persistence-lock-release save-path session))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-artifacts-rotate-segment/remote ()
  ,test
  (test)
  :doc "remote rotation publishes old, new, and sidecar together"
  (let* ((host "rotate-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-rotate-" t)))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-niwa-session-durability--remote-session
                  host local-root)))
            (test-niwa-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "Original prompt\nOriginal reply\n")
              (niwa-session-artifacts-save session (current-buffer))
              (let* ((save-path (niwa-session-save-path session))
                     (old (file-name-concat
                           save-path "segment-0001.chat.org"))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (should
                 (equal new
                        (niwa-session-artifacts-rotate-segment
                         session (current-buffer) "Remote handoff.")))
                (should (equal new buffer-file-name))
                (should (string-match-p "Remote handoff" (buffer-string)))
                (should
                 (string-match-p
                  "NIWA_SEGMENT_FINALIZED_AT"
                  (with-temp-buffer
                    (insert (niwa-session-artifacts-read-artifact
                             session (file-name-nondirectory old) t))
                    (buffer-string))))
                (should
                 (= 2
                    (plist-get
                     (niwa-session-codec-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (should-not
                 (niwa-session-pending-publication session))
                (niwa-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "mid-rotation failure retains one complete retryable transition"
  (let* ((host "rotate-failure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-rotate-failure-" t)))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-niwa-session-durability--remote-session
                  host local-root)))
            (test-niwa-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "Before rotation\n")
              (niwa-session-artifacts-save session (current-buffer))
              (let* ((save-path (niwa-session-save-path session))
                     ;; No new generation can be created while the
                     ;; publications root refuses writes.
                     (publications
                      (file-name-concat save-path ".publications"))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (set-file-modes publications #o555)
                (unwind-protect
                    (niwa-test--with-captured-diagnostics nil
                      (should-error
                       (niwa-session-artifacts-rotate-segment
                        session (current-buffer) "Pending handoff.")))
                  (set-file-modes publications #o755))
                (should (niwa-session-pending-publication session))
                (should (= 2 (niwa-session-current-segment session)))
                (should (equal new buffer-file-name))
                (should (string-match-p "Pending handoff" (buffer-string)))
                (should
                 (niwa-session-publication-retry session))
                (should-not
                 (niwa-session-pending-publication session))
                (should
                 (= 2
                    (plist-get
                     (niwa-session-codec-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (niwa-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-artifacts-start-fresh-segment/remote ()
  ,test
  (test)
  :doc "remote clear atomically publishes an empty new segment"
  (let* ((host "fresh-segment-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-fresh-" t)))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let ((session
                 (test-niwa-session-durability--remote-session
                  host local-root)))
            (test-niwa-session-durability--accept-storage session)
            (with-temp-buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "Old conversation\n")
              (niwa-session-artifacts-save session (current-buffer))
              (let* ((save-path (niwa-session-save-path session))
                     (new (file-name-concat
                           save-path "segment-0002.chat.org")))
                (should
                 (equal new
                        (niwa-session-artifacts-start-fresh-segment
                         session (current-buffer))))
                (should-not (string-match-p "Old conversation" (buffer-string)))
                (should-not
                 (string-match-p
                  "Old conversation"
                  (with-temp-buffer
                    (insert (niwa-session-artifacts-read-artifact
                             session (file-name-nondirectory new) t))
                    (buffer-string))))
                (should
                 (= 2
                    (plist-get
                     (niwa-session-codec-read
                      (file-name-concat save-path "session.meta.el"))
                     :current-segment)))
                (niwa-session-persistence-lock-release
                 save-path session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-fork--publish-fork/remote ()
  ,test
  (test)
  :doc "fork staging owns its child lease first and releases it on failure"
  (let* ((host "fork-staging-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-fork-stage-" t)))
         (staging-local
          (file-name-as-directory (file-name-concat local-root "staging")))
         (staging
          (format "/niwamock:%s:%s/" host staging-local))
         (published
          (format "/niwamock:%s:%s/" host
                  (file-name-concat local-root "published")))
         (marker (file-name-concat staging "stage-started"))
         (niwa-session-durability--client-id (make-string 64 ?a))
         lease-before-stage)
    (make-directory staging-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((child
                  (test-niwa-session-durability--remote-session
                   host local-root))
                 (buffer (generate-new-buffer " *fork-source*"))
                 (staging-buffer (generate-new-buffer " *fork-stage*")))
            (unwind-protect
                (progn
                  (test-niwa-session-durability--accept-storage child)
                  (setf (niwa-session-save-path child) staging
                        (niwa-session-session-id child) "child")
                  (cl-letf
                      (((symbol-function
                         'niwa-session-fork--stage-fork)
                        (lambda (&rest _)
                          (setq lease-before-stage
                                (file-directory-p
                                 (file-name-concat staging ".lease")))
                          (with-temp-file marker (insert "started"))
                          (error "Staging failed"))))
                    (should-error
                     (niwa-session-fork--publish-fork
                      child buffer staging-buffer staging staging published
                      1 0 nil)
                     :type 'error))
                  (should lease-before-stage)
                  (should-not (niwa-session-lease child))
                  (should-not
                   (niwa-session-lease-renewal-timer child))
                  (should-not (file-directory-p staging))
                  (should-not (file-directory-p published)))
              (when (buffer-live-p buffer)
                (kill-buffer buffer))
              (when (buffer-live-p staging-buffer)
                (kill-buffer staging-buffer)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-recovery-record-failure ()
  ,test
  (test)
  :doc "an incomplete specialized transaction blocks until explicit recovery"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-specialized-failure-" t)))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "specialized"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (niwa-test--with-captured-diagnostics nil
            (niwa-session-recovery-record-failure
             session "rewind rollback incomplete" "/target/recovery"))
          (should (niwa-session-pending-publication session))
          (should-error
           (niwa-session-publication-retry session)
           :type 'user-error)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (niwa-test--with-captured-diagnostics nil
              (should
               (niwa-session-publication-abandon session))))
          (should-not (niwa-session-pending-publication session)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-specialized-recovery/remote ()
  ,test
  (test)
  :doc "target recovery survives client loss and blocks takeover until abandonment"
  (let* ((host "specialized-target-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-specialized-target-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/niwamock:%s:%s/" host session-dir-local))
         (owner (make-string 64 ?a))
         (successor (make-string 64 ?b))
         (now 0.0)
         (session nil)
         recovery)
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now)))
            (let ((niwa-session-durability--client-id owner))
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*rewind-owner*" session))
              (setq recovery (make-temp-file "niwa-rewind-recovery-" t))
              (make-directory (file-name-concat recovery "files") t)
              (with-temp-file (file-name-concat recovery "files" "before.el")
                (insert "before"))
              (niwa-test--with-captured-diagnostics nil
                (niwa-session-recovery-record-failure
                 session "rewind rollback incomplete" recovery))
              (should-not (file-directory-p recovery))
              (let* ((durable
                      (niwa-session-recovery-read
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
                (let ((niwa--data-buffer nil)
                      (niwa--agent-invocation nil))
                  (with-temp-buffer
                    (setq-local niwa--session session)
                    (should-error
                     (niwa-session-artifacts-assert-mutation-authority
                      session (current-buffer))
                     :type 'user-error))))
              ;; Simulate client loss rather than a clean release: the marker
              ;; must remain discoverable when a new client takes over.
              (niwa-session-durability--cancel-renewal session)
              (setq now 100.0)
              (let ((niwa-session-durability--client-id successor))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) t))
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t)))
                  (let ((restored
                         (test-niwa-session-durability--remote-session
                          host local-root)))
                    (setf (niwa-session-save-path restored) session-dir)
                    (should
                     (niwa-session-durability-lease-acquire
                      session-dir "*rewind-successor*" restored))
                    (niwa-session-recovery-refresh
                     restored)
                    (should (plist-get
                             (niwa-session-pending-publication restored)
                             :manual-recovery-marker))
                    (let ((niwa--data-buffer nil)
                          (niwa--agent-invocation nil))
                      (with-temp-buffer
                        (setq-local niwa--session restored)
                        (should-error
                         (niwa-session-artifacts-assert-mutation-authority
                          restored (current-buffer))
                         :type 'user-error)))
                    (niwa-test--with-captured-diagnostics nil
                      (should
                       (niwa-session-publication-abandon restored)))
                    (should-not
                     (niwa-session-recovery-read
                      session-dir))
                    (should-not
                     (file-directory-p
                      (file-name-concat session-dir ".recovery")))
                    (should-not
                     (niwa-session-pending-publication restored))
                    (let ((niwa--data-buffer nil)
                          (niwa--agent-invocation nil))
                      (with-temp-buffer
                        (setq-local niwa--session restored)
                        (should
                         (niwa-session-artifacts-assert-mutation-authority
                          restored (current-buffer)))))
                    (niwa-session-durability-lease-release
                     session-dir restored)))))))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "failed target installation retains honest local recovery"
  (let* ((host "specialized-target-failure-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-specialized-target-failure-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/niwamock:%s:%s/" host session-dir-local))
         (session nil)
         (recovery (make-temp-file "niwa-rewind-local-recovery-" t))
         (niwa-session-durability--client-id (make-string 64 ?c))
         diagnostics)
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should
           (niwa-session-durability-lease-acquire
            session-dir "*rewind-failure*" session))
          (cl-letf (((symbol-function
                      'niwa-session-recovery--copy-local-directory)
                     (lambda (&rest _)
                       (error "Injected target recovery copy failure"))))
            (niwa-test--with-captured-diagnostics diagnostics
              (niwa-session-recovery-record-failure
               session "rewind rollback incomplete" recovery)))
          (should (string-match-p "recovery could not be installed"
                                  diagnostics))
          (let ((pending (niwa-session-pending-publication session)))
            (should (file-directory-p recovery))
            (should-not (plist-get pending :recovery-portable))
            (should (equal recovery
                           (plist-get pending :manual-recovery-local)))
            (should (string-match-p "Injected target recovery copy failure"
                                    (plist-get pending :reason)))
            (should-not
             (niwa-session-recovery-read
              session-dir))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (niwa-test--with-captured-diagnostics nil
                (should
                 (niwa-session-publication-abandon session))))
            (should-not (file-directory-p recovery))
            (should-not (niwa-session-pending-publication session)))
          (niwa-session-durability-lease-release session-dir session))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "removes target marker and bytes when marker readback fails"
  (let* ((host "specialized-target-readback-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-specialized-target-readback-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/niwamock:%s:%s/" host session-dir-local))
         (session nil)
         (recovery (make-temp-file "niwa-rewind-readback-recovery-" t))
         (niwa-session-durability--client-id (make-string 64 ?d)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (should
           (niwa-session-durability-lease-acquire
            session-dir "*rewind-readback*" session))
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (cl-letf (((symbol-function
                      'niwa-session-recovery--read-marker)
                     (lambda (&rest _)
                       (error "Injected marker readback failure"))))
            (niwa-test--with-captured-diagnostics nil
              (niwa-session-recovery-record-failure
               session "rewind rollback incomplete" recovery)))
          (let ((pending (niwa-session-pending-publication session))
                (root (file-name-concat session-dir ".recovery")))
            (should (file-directory-p recovery))
            (should-not (plist-get pending :recovery-portable))
            (should (string-match-p "Injected marker readback failure"
                                    (plist-get pending :reason)))
            (should-not
             (niwa-session-recovery-read
              session-dir))
            (when (file-directory-p root)
              (should-not (directory-files root nil "\\`recovery-"))
              (should-not (directory-files root nil "\\`[0-9a-f]+\\'"))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (niwa-test--with-captured-diagnostics nil
              (should
               (niwa-session-publication-abandon session))))
          (should-not (file-directory-p recovery))
          (let ((niwa-session-durability--client-id
                 (make-string 64 ?d)))
            (niwa-session-durability-lease-release session-dir session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "retains local recovery when a foreign lease blocks target installation"
  (let* ((host "specialized-target-lease-loss-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-specialized-target-lease-loss-" t)))
         (session-dir-local
          (file-name-as-directory (file-name-concat local-root "session")))
         (session-dir (format "/niwamock:%s:%s/" host session-dir-local))
         (owner (make-string 64 ?e))
         (successor (make-string 64 ?f))
         (session nil)
         (recovery (make-temp-file "niwa-rewind-lease-loss-recovery-" t)))
    (make-directory session-dir-local t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (setq session
                (test-niwa-session-durability--remote-session
                 host local-root))
          (test-niwa-session-durability--accept-storage session)
          (setf (niwa-session-save-path session) session-dir)
          (let ((niwa-session-durability--client-id owner))
            (should
             (niwa-session-durability-lease-acquire
              session-dir "*rewind-owner*" session)))
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (let ((niwa-session-durability--client-id successor))
            (niwa-test--with-captured-diagnostics nil
              (niwa-session-recovery-record-failure
               session "rewind rollback incomplete" recovery))
            (let ((pending (niwa-session-pending-publication session)))
              (should (file-directory-p recovery))
              (should-not (plist-get pending :recovery-portable))
              (should (equal recovery
                             (plist-get pending :manual-recovery-local)))
              (should (string-match-p "lease is unavailable"
                                      (plist-get pending :reason)))
              (should-not
               (niwa-session-recovery-read
                session-dir)))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (niwa-test--with-captured-diagnostics nil
                (should
                 (niwa-session-publication-abandon session)))))
          (should-not (file-directory-p recovery))
          (let ((niwa-session-durability--client-id owner))
            (niwa-session-durability-lease-release session-dir session)))
      (niwa-session-durability--cancel-renewal session)
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-session-durability-mixed-controls ()
  ,test
  (test)
  :doc "retains local recovery and fails closed when target controls are mixed"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-mixed-recovery-" t)))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "mixed-recovery"))
         (session (niwa-session-create "main" workspace))
         (session-dir (file-name-as-directory
                       (file-name-concat root "session")))
         (recovery (make-temp-file "niwa-mixed-local-recovery-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat session-dir ".lease") t)
          (with-temp-file (file-name-concat session-dir ".lock")
            (insert "mixed controls"))
          (setf (niwa-session-save-path session) session-dir)
          (with-temp-file (file-name-concat recovery "before.el")
            (insert "before"))
          (niwa-test--with-captured-diagnostics nil
            (niwa-session-recovery-record-failure
             session "rewind rollback incomplete" recovery))
          (let ((pending (niwa-session-pending-publication session)))
            (should (file-directory-p recovery))
            (should (equal recovery
                           (plist-get pending :manual-recovery-local)))
            (should (string-match-p "Unsupported transcript format"
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
               (niwa-session-recovery-read
                session-dir)
               :type 'error)
              (delete-file (file-name-concat session-dir ".lock"))
              (should
               (equal recovery-dir
                      (plist-get
                       (niwa-session-recovery-read
                        session-dir)
                       :directory))))))
      (when (file-directory-p recovery)
        (delete-directory recovery t))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-publication-status ()
  ,test
  (test)
  :doc "status exposes lease, pending publication, and authoritative path"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-durability-status-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "status"))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (progn
          (setf (niwa-session-lease session)
                '(:state foreign
                  :publication-head ".publications/current/manifest.el")
                (niwa-session-pending-publication session)
                '(:reason "offline"))
          (let ((status (niwa-session-publication-status session)))
            (should (eq 'foreign (plist-get status :lease-state)))
            (should (equal ".publications/current/manifest.el"
                           (plist-get status :publication-head)))
            (should (eq t (plist-get status :pending-publication)))
            (should (equal "offline" (plist-get status :pending-reason)))
            (should (equal (file-name-concat root ".niwa/")
                           (plist-get status :authoritative-state-path))))
          (with-temp-buffer
            (setq-local niwa--session session)
            (let ((header (niwa-session-persistence-header-segment)))
              (should (string-match-p "Publication pending" header))
              (should (string-match-p "Lease: foreign" header)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-call-with-abandoned-lease ()
  ,test
  (test)
  :doc "capture recovery fences expired ordinary owners but preserves live or unresolved owners"
  (dolist (case '((active -1000 nil t)
                  (claiming -1000 nil t)
                  (released -1000 nil t)
                  (active 0 nil nil)
                  (publishing -1000 nil nil)
                  (active -1000 t nil)))
    (let* ((root (make-temp-file "niwa-abandoned-lease-" t))
           (session (test-niwa-session-durability--local-session root))
           (directory (file-name-concat root "session"))
           (lease-dir (file-name-concat directory ".lease"))
           (foreign (make-string 64 ?f))
           (niwa-session-durability--client-id (make-string 64 ?a))
           called)
      (setf (niwa-session-save-path session) directory)
      (unwind-protect
          (progn
            (make-directory directory t)
            (niwa-session-durability--ensure-lease-directory lease-dir)
            (let* ((now (niwa-session-durability--target-time lease-dir))
                   (record
                    (let ((niwa-session-durability--client-id foreign))
                      (niwa-session-durability--lease-record
                       "old owner" 1 (nth 0 case) nil (nth 2 case)
                       (+ now (nth 1 case))))))
              (niwa-session-durability--write-generation lease-dir record)
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _) (error "Recovery must not prompt"))))
                (should (eq (nth 3 case)
                            (niwa-session-durability-call-with-abandoned-lease
                             session (lambda () (setq called t))))))
              (should (eq (nth 3 case) called))
              (let ((head (niwa-session-durability--lease-head lease-dir)))
                (if called
                    (progn
                      (should (= 2 (plist-get head :generation)))
                      (should (eq 'released (plist-get head :status)))
                      (should (equal niwa-session-durability--client-id (plist-get head :client-id))))
                  (should (equal record head)))))
            (should-not (niwa-session-lease-renewal-timer session)))
        (niwa-session-durability-lease-release directory session)
        (delete-directory root t))))

  :doc "a callback failure releases new authority and leaves no renewal timer"
  (let* ((root (make-temp-file "niwa-abandoned-lease-" t))
         (session (test-niwa-session-durability--local-session root))
         (directory (file-name-concat root "session"))
         called)
    (setf (niwa-session-save-path session) directory)
    (unwind-protect
        (progn
          (make-directory directory t)
          (should-error
           (niwa-session-durability-call-with-abandoned-lease
            session (lambda () (setq called t) (error "Injected recovery failure"))))
          (should called)
          (should (eq 'available (niwa-session-durability-lease-state directory)))
          (should-not (niwa-session-lease-renewal-timer session)))
      (niwa-session-durability-lease-release directory session)
      (delete-directory root t))))

(niwa-deftest niwa-session-durability-delete-abandoned ()
  (let* ((root (make-temp-file "niwa-delete-abandoned-" t))
         (directory (file-name-concat root "session"))
         (lease-dir (file-name-concat directory ".lease"))
         (foreign (make-string 64 ?f))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (error "Cleanup must not prompt"))))
          (make-directory directory t)
          (niwa-session-durability--ensure-lease-directory lease-dir)
          ,test)
      (delete-directory root t)))
  (test)
  :doc "deletes only sessions no client owned within the cap, without prompting"
  (dolist (case '((nil 0 nil t)
                  (released -2000 nil t)
                  (active -2000 nil t)
                  (claiming -2000 nil t)
                  (released -500 nil nil)
                  (active 0 nil nil)
                  (publishing -2000 nil nil)
                  (active -2000 t nil)))
    (make-directory lease-dir t)
    (let ((record
           (when (nth 0 case)
             (let ((niwa-session-durability--client-id foreign))
               (niwa-session-durability--lease-record
                "old owner" 1 (nth 0 case) nil (nth 2 case)
                (+ (niwa-session-durability--target-time lease-dir) (nth 1 case)))))))
      (when record (niwa-session-durability--write-generation lease-dir record))
      (should (eq (nth 3 case)
                  (and (niwa-session-durability-delete-abandoned directory 1000) t)))
      (should (eq (nth 3 case) (not (file-directory-p directory))))
      (unless (nth 3 case)
        (should (equal record (niwa-session-durability--lease-head lease-dir)))
        (delete-directory directory t))
      (make-directory directory t)))
  :doc "a head in another lease format is retained even when its expiry is old"
  (dolist (case '((-2000 nil) (-500 nil)))
    (let ((record (let ((niwa-session-durability--client-id foreign))
                    (niwa-session-durability--lease-record
                     "old owner" 1 'released nil nil
                     (+ (niwa-session-durability--target-time lease-dir) (car case))))))
      (cl-remf record :transfer-generation)
      (should-not (niwa-session-durability--valid-lease-p record))
      (niwa-session-durability--write-generation lease-dir record)
      (should (eq (cadr case)
                  (and (niwa-session-durability-delete-abandoned directory 1000) t)))
      (should (eq (cadr case) (not (file-directory-p directory))))
      (when (file-directory-p directory) (delete-directory directory t))
      (make-directory lease-dir t)))
  :doc "journal pins and recovery markers keep the session without acquiring a claim"
  (dolist (blocker (list (lambda ()
                           (make-directory (file-name-concat directory ".journal-pins"))
                           (write-region "" nil (file-name-concat directory ".journal-pins"
                                                                  (concat (make-string 64 ?c) ".json"))))
                         (lambda ()
                           (write-region "" nil (niwa-session-recovery--root directory)))))
    (delete-directory directory t)
    (make-directory lease-dir t)
    (funcall blocker)
    (should-not (niwa-session-durability-delete-abandoned directory 1000))
    (should (file-directory-p directory))
    (should-not (niwa-session-durability--lease-head lease-dir)))
  :doc "a competing generation after the claim keeps the session"
  (let ((claim (symbol-function 'niwa-session-durability--claim-next)))
    (cl-letf (((symbol-function 'niwa-session-durability--claim-next)
               (lambda (&rest args)
                 (let ((lease (apply claim args)))
                   (let ((niwa-session-durability--client-id foreign))
                     (niwa-session-durability--write-generation
                      lease-dir (niwa-session-durability--lease-record
                                 "*laptop*" (1+ (plist-get lease :generation)) 'active nil nil
                                 (niwa-session-durability--target-time lease-dir))))
                   lease))))
      (should-not (niwa-session-durability-delete-abandoned directory 1000)))
    (should (file-directory-p directory))
    (should (equal foreign (plist-get (niwa-session-durability--lease-head lease-dir) :client-id)))))

(niwa-deftest niwa-session-recovery--local-temporary-p
  (:doc "accepts a temporary child even when a racing directory stat disagrees")
  (let* ((child (make-temp-file "niwa-local-temporary-" t))
         (escape (expand-file-name "escape" child)))
    (unwind-protect
        ;; Concurrent writers make `file-in-directory-p' compare differing
        ;; stats of the temporary directory itself.
        (cl-letf (((symbol-function 'file-in-directory-p) #'ignore))
          (make-symbolic-link invocation-directory escape)
          (should (niwa-session-recovery--local-temporary-p child))
          (should-not (niwa-session-recovery--local-temporary-p
                       temporary-file-directory))
          (should-not (niwa-session-recovery--local-temporary-p
                       invocation-directory))
          ;; A temporary name resolving outside is not temporary.
          (should-not (niwa-session-recovery--local-temporary-p escape)))
      (delete-directory child t))))

(niwa-deftest niwa-session-durability-cleanup-assessment ()
  (let* ((root (make-temp-file "niwa-cleanup-assessment-" t))
         (directory (file-name-concat root "session"))
         (lease-dir (file-name-concat directory ".lease")))
    (make-directory directory)
    (unwind-protect (progn ,test) (delete-directory root t)))
  (test)
  :doc "absent authority is unknown without creating control directories"
  (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
    (should (eq 'unknown (plist-get assessment :state)))
    (should-not (plist-get assessment :head))
    (should (equal '(invalid-authority) (plist-get assessment :reasons)))
    (should-not (file-exists-p lease-dir)))
  :doc "cleanup preserves an old session with neither a PID lock nor a lease"
  (let ((sidecar (file-name-concat directory "session.meta.el"))
        (transcript (file-name-concat directory "segment-0001.chat.org"))
        (old (seconds-to-time 1)))
    (write-region "(:version 1 :name \"legacy\")\n" nil sidecar nil 'silent)
    (write-region "A pre-lease conversation\n" nil transcript nil 'silent)
    (dolist (path (list sidecar transcript directory)) (set-file-times path old))
    (should-not (niwa-session-durability-delete-abandoned directory 1000))
    (should (file-readable-p sidecar))
    (should (file-readable-p transcript))
    (should-not (file-exists-p lease-dir)))
  :doc "carrier crash scratch is not authority and does not prevent automatic cleanup"
  (dolist (kind '(clean staged shell))
    (make-directory lease-dir t)
    (let* ((staged (list nil))
           (shell (file-name-concat lease-dir ".niwa-control-fs-Ab09Zx")))
      (pcase kind
        ('staged
         (niwa-session-control-fs--stage-payloads
          (list (list :op 'write
                      :path (niwa-session-durability--generation-path lease-dir 1)
                      :content "crash payload")) staged)
         (should (car staged)))
        ('shell (write-region "crash payload" nil shell nil 'silent)))
      (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
        (should (eq 'available (plist-get assessment :state)))
        (should-not (plist-get assessment :head))
        (should-not (plist-get assessment :reasons)))
      (dolist (path (append (car staged) (when (eq kind 'shell) (list shell))))
        (should (equal "crash payload"
                       (with-temp-buffer (insert-file-contents path) (buffer-string)))))
      (should (niwa-session-durability-delete-abandoned directory 0))
      (should-not (file-directory-p directory))))
  :doc "scratch-like unknown names remain invalid authority"
  (let ((case-fold-search t))
    (make-directory lease-dir)
    (dolist (name '(".niwa-control-fs-Ab09Zx.extra"
                    ".niwa-control-fs-staged-too-long"
                    ".niwa-unrelated-Ab09Zx"
                    ".niwa-control-clock-Ab09Zx"
                    ".NIWA-CONTROL-FS-Ab09Zx"
                    ".niwa-control-fs-STAGED-Ab09Zx"))
      (let ((path (file-name-concat lease-dir name)))
        (write-region "unknown evidence" nil path nil 'silent)
        (should (equal (list 'error (format "Unknown portable lease entry: %s" name))
                       (should-error
                        (niwa-session-durability-lease-acquire directory "unknown")
                        :type 'error)))
        (should (eq 'unknown
                    (plist-get (niwa-session-durability-cleanup-assessment directory) :state)))
        (should-not (niwa-session-durability-delete-abandoned directory 0))
        (should (file-exists-p path))
        (delete-file path))))
  :doc "corrupt generations are unknown, never missing or recoverable authority"
  (progn
    (make-directory lease-dir)
    (dolist (name '("00000000000000000001.el" "0.el"))
      (write-region "(:generation" nil (file-name-concat lease-dir name) nil 'silent)
      (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
        (should (eq 'unknown (plist-get assessment :state)))
        (should (memq 'invalid-authority (plist-get assessment :reasons)))
        (should-not (plist-get assessment :recoverable)))
      (delete-file (file-name-concat lease-dir name))))
  :doc "lease state and crash-retention reasons share the same validated observation"
  (progn
    (make-directory lease-dir)
    (dolist (case '((publishing -4000 t (publishing unsettled-mutation) expired t)
                    (active 0 nil (live-owner) owned nil)
                    (active -4000 nil nil expired nil)
                    (released 0 t (unsettled-mutation) available t)))
      (let ((record (niwa-session-durability--lease-record
                     "owner" 1 (nth 0 case) nil (nth 2 case)
                     (+ (niwa-session-durability--target-time lease-dir) (nth 1 case)))))
        (niwa-session-durability--write-generation lease-dir record)
        (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
          (should (equal record (plist-get assessment :head)))
          (should (equal (plist-get record :host) (plist-get assessment :host)))
          (should (equal (nth 3 case) (plist-get assessment :reasons)))
          (should (eq (nth 4 case) (plist-get assessment :state)))
          (should (eq (nth 5 case) (and (plist-get assessment :recoverable) t)))))))
  :doc "unknown pin and recovery entries remain visible and protected"
  (progn
    (make-directory lease-dir)
    (make-directory (file-name-concat directory ".journal-pins"))
    (write-region "unsupported pin" nil
                  (file-name-concat directory ".journal-pins" "unknown") nil 'silent)
    (make-directory (file-name-concat directory ".recovery"))
    (write-region "unresolved bytes" nil
                  (file-name-concat directory ".recovery" "unknown") nil 'silent)
    (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
      (should (equal '(journal-pins recovery) (plist-get assessment :reasons)))
      (should (plist-get assessment :recoverable))))
  :doc "a reserved transfer is available to its requester but never discardable"
  (let* ((niwa-session-durability--client-id (make-string 64 ?a))
         (owner-id (make-string 64 ?f))
         (now (progn (make-directory lease-dir t)
                     (niwa-session-durability--target-time lease-dir)))
         (fence-path (niwa-session-transfer--fence-path lease-dir 1))
         (session (test-niwa-session-durability--local-session root)))
    (let ((niwa-session-durability--client-id owner-id))
      (niwa-session-durability--write-generation
       lease-dir (niwa-session-durability--lease-record
                  "old owner" 1 'released nil nil now)))
    (make-directory (file-name-directory fence-path) t)
    (write-region
     (prin1-to-string
      (list :protocol-version 1 :generation 1 :request-id (make-string 32 ?b)
            :session-id "reserved" :owner-client-id owner-id
            :requester-client-id niwa-session-durability--client-id
            :expires-at (+ now 60)))
     nil fence-path nil 'silent)
    (setf (niwa-session-save-path session) directory)
    (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
      (should (eq 'available (plist-get assessment :state)))
      (should (equal '(transfer-reserved) (plist-get assessment :reasons)))
      (should-not (plist-get assessment :recoverable)))
    (should-not (niwa-session-durability-delete-abandoned directory 0))
    (should-error (niwa-session-durability-discard-session session #'ignore))
    (let ((niwa-session-durability--client-id owner-id))
      (should (eq 'foreign (plist-get
                           (niwa-session-durability-cleanup-assessment directory)
                           :state))))
    (should (file-exists-p fence-path)))
  :doc "a valid record followed by unparsed bytes is invalid authority"
  (progn
    (make-directory lease-dir)
    (write-region
     (concat (niwa-session-durability--record-bytes
              (niwa-session-durability--lease-record
               "owner" 1 'released nil nil
               (niwa-session-durability--target-time lease-dir)))
             "\nmalformed trailing record")
     nil (file-name-concat lease-dir "00000000000000000001.el") nil 'silent)
    (let ((assessment (niwa-session-durability-cleanup-assessment directory)))
      (should (equal '(invalid-authority) (plist-get assessment :reasons)))
      (should (eq 'unknown (plist-get assessment :state))))))

(niwa-deftest niwa-session-durability-discard-session ()
  (let* ((root (make-temp-file "niwa-discard-session-" t))
         (directory (file-name-concat root "session"))
         (lease-dir (file-name-concat directory ".lease"))
         (session (test-niwa-session-durability--local-session root))
         (niwa-session-durability--client-id (make-string 64 ?a)))
    (make-directory lease-dir t)
    (setf (niwa-session-save-path session) directory)
    (unwind-protect (progn ,test)
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t)))
  (test)
  :doc "explicit discard acquires without restore or a second takeover prompt"
  (let ((prepared nil))
    (let ((niwa-session-durability--client-id (make-string 64 ?f)))
      (niwa-session-durability--write-generation
       lease-dir (niwa-session-durability--lease-record
                  "crashed publisher" 1 'publishing nil t
                  (- (niwa-session-durability--target-time lease-dir) 4000))))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Second prompt"))))
      (should
       (niwa-session-durability-discard-session
        session
        (lambda ()
          (should (niwa-session-durability-lease-owned-p session))
          (should (plist-get (niwa-session-lease session) :unsettled-mutation))
          (should (niwa-session-durability-set-unsettled-mutation session nil))
          (setq prepared t)))))
    (should prepared)
    (should-not (file-exists-p directory))
    (should-not (niwa-session-lease session))
    (should-not (niwa-session-lease-renewal-timer session)))
  :doc "failed preparation releases authority without erasing unresolved mutation"
  (progn
    (let ((niwa-session-durability--client-id (make-string 64 ?f)))
      (niwa-session-durability--write-generation
       lease-dir (niwa-session-durability--lease-record
                  "crashed" 1 'active nil t
                  (- (niwa-session-durability--target-time lease-dir) 4000))))
    (should-error (niwa-session-durability-discard-session
                   session (lambda () (error "Preparation interrupted"))))
    (should (file-directory-p directory))
    (let ((head (niwa-session-durability--lease-head lease-dir)))
      (should (eq 'released (plist-get head :status)))
      (should (plist-get head :unsettled-mutation)))
    (should-not (niwa-session-lease session)))
  :doc "a malformed newer generation after assessment prevents election and survives"
  (let ((run (symbol-function 'niwa-session-control-fs-run-program))
        (path (niwa-session-durability--generation-path lease-dir 2))
        injected prepared)
    (niwa-session-durability--write-generation
     lease-dir (niwa-session-durability--lease-record
                "old owner" 1 'released nil nil
                (niwa-session-durability--target-time lease-dir)))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock-directory)
                 (when (and (not injected)
                            (seq-some (lambda (op)
                                        (and (eq 'list-directory (plist-get op :op))
                                             (equal lease-dir (plist-get op :path))
                                             (not (plist-get op :optional))))
                                      operations))
                   (setq injected t)
                   (write-region "(:generation" nil path nil 'silent))
                 (funcall run operations lock-directory))))
      (should-error
       (niwa-session-durability-discard-session
        session (lambda () (setq prepared t)))))
    (should injected)
    (should-not prepared)
    (should (file-directory-p directory))
    (should (equal "(:generation"
                   (with-temp-buffer (insert-file-contents path) (buffer-string)))))
  :doc "live ownership and malformed authority never reach preparation"
  (dolist (record (list (niwa-session-durability--lease-record
                        "local owner" 1 'active nil nil
                        (niwa-session-durability--target-time lease-dir))
                       '(:generation 1 :status released :expires-at 0)))
    (niwa-session-durability--write-generation lease-dir record)
    (should-error
     (niwa-session-durability-discard-session
      session (lambda () (ert-fail "Refused session reached preparation")))))
  :doc "a new blocker between final observation and deletion fails closed"
  (let ((run (symbol-function 'niwa-session-control-fs-run-program))
        (injected nil))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock-directory)
                 (when (seq-some (lambda (op) (eq 'delete-directory (plist-get op :op)))
                                 operations)
                   (should (equal root lock-directory))
                   (setq injected t)
                   (make-directory (file-name-concat directory ".journal-pins"))
                   (write-region "unknown capture" nil
                                 (file-name-concat directory ".journal-pins" "malformed")
                                 nil 'silent))
                 (funcall run operations lock-directory))))
      (should-error (niwa-session-durability-discard-session session #'ignore)))
    (should injected)
    (should (file-directory-p directory))
    (should (eq 'released (plist-get (niwa-session-durability--lease-head lease-dir)
                                   :status))))
  :doc "delayed activation and abort never recreate a removed claiming record"
  (dolist (abort '(nil t))
    (let ((run (symbol-function 'niwa-session-control-fs-run-program))
          injected)
      (when abort
        (niwa-session-durability--write-generation
         lease-dir (niwa-session-durability--lease-record
                    "old owner" 1 'released nil nil
                    (niwa-session-durability--target-time lease-dir))))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (operations &optional lock-directory)
                   (if (not (eq 'create (plist-get (car operations) :op)))
                       (funcall run operations lock-directory)
                     (should (equal root lock-directory))
                     (when abort
                       (niwa-session-durability--write-generation
                        lease-dir
                        (plist-put (niwa-session-durability--lease-head lease-dir)
                                   :buffer "changed predecessor")))
                     (prog1 (funcall run operations lock-directory)
                       (setq injected t)
                       (delete-directory directory t)
                       (make-directory lease-dir t))))))
        (should-error (niwa-session-durability-discard-session session #'ignore)))
      (should injected)
      (should-not (directory-files lease-dir nil "\\`[0-9]"))))
  :doc "an unexpected lease record between final observation and deletion is protected"
  (let ((run (symbol-function 'niwa-session-control-fs-run-program)))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock-directory)
                 (when (seq-some (lambda (op) (eq 'delete-directory (plist-get op :op)))
                                 operations)
                   (write-region "malformed authority" nil
                                 (file-name-concat lease-dir "0.el") nil 'silent))
                 (funcall run operations lock-directory))))
      (should-error (niwa-session-durability-discard-session session #'ignore)))
    (should (file-directory-p directory)))
  :doc "expiry at the final target clock check retains the session"
  (let ((niwa-session-lease-seconds 1)
        (run (symbol-function 'niwa-session-control-fs-run-program)))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock-directory)
                 (when (seq-some (lambda (op) (eq 'delete-directory (plist-get op :op)))
                                 operations)
                   (sleep-for 1.1))
                 (funcall run operations lock-directory))))
      (should-error (niwa-session-durability-discard-session session #'ignore)))
    (should (file-directory-p directory))))

(niwa-deftest niwa-session-durability-lease-acquire/scratch
  (:doc "synchronous takeover preserves carrier scratch payloads without treating them as authority")
  (dolist (scratch '(nil t))
    (let* ((root (make-temp-file "niwa-takeover-scratch-" t))
           (directory (file-name-concat root ".lease"))
           (session (test-niwa-session-durability--local-session root))
           (staged (list nil))
           paths)
      (setf (niwa-session-save-path session) root)
      (make-directory directory)
      (unwind-protect
          (progn
            (let ((niwa-session-durability--client-id (make-string 64 ?f)))
              (niwa-session-durability--write-generation
               directory (niwa-session-durability--lease-record "old" 1 'active nil nil 0)))
            (when scratch
              (niwa-session-control-fs--stage-payloads
               (list (list :op 'write
                           :path (niwa-session-durability--generation-path directory 2)
                           :content "crash payload")) staged)
              (should (car staged))
              (setq paths (cons (file-name-concat directory ".niwa-control-fs-Ab09Zx")
                                (car staged)))
              (write-region "crash payload" nil (car paths) nil 'silent))
            (cl-letf (((symbol-function 'y-or-n-p) #'always))
              (should (niwa-session-durability-lease-acquire root "takeover" session)))
            (should (niwa-session-durability-lease-owned-p session))
            (should (> (plist-get (niwa-session-lease session) :generation) 1))
            (dolist (path paths)
              (should (equal "crash payload"
                             (with-temp-buffer (insert-file-contents path) (buffer-string))))))
        (niwa-session-durability--cancel-renewal session)
        (delete-directory root t)))))

(niwa-deftest niwa-session-durability-lease-acquire/clock-scratch ()
  (let* ((operation ,operation)
         (root (make-temp-file "niwa-clock-scratch-" t))
         (directory (file-name-concat root ".lease"))
         (bin (file-name-concat root "bin"))
         (ready (file-name-concat root "ready"))
         (continue (file-name-concat root "continue"))
         (captured (file-name-concat root "captured"))
         (stat (executable-find "stat"))
         (session (test-niwa-session-durability--local-session root))
         child settled marker)
    (unwind-protect
        (progn
          (make-directory directory)
          (make-directory bin)
          (setf (niwa-session-save-path session) root)
          ;; Only the asynchronous producer inherits this pause, not acquisition.
          (with-temp-file (file-name-concat bin "stat")
            (insert
             "#!/bin/sh\nset -eu\n"
             (format "printf '%%s/%%s' \"$(pwd -P)\" \"$4\" > %s\n"
                     (shell-quote-argument captured))
             (format ": > %s\n" (shell-quote-argument ready))
             "waited=0\n"
             (format "while test ! -e %s; do\n" (shell-quote-argument continue))
             "  sleep 0.01\n  waited=$((waited + 1))\n"
             "  test \"$waited\" -lt 3000 || exit 79\ndone\n"
             (format "exec %s \"$@\"\n" (shell-quote-argument stat))))
          (set-file-modes (file-name-concat bin "stat") #o700)
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "PATH" (concat bin path-separator (getenv "PATH")))
            (setq child
                  (niwa-session-control-fs-run-program-async
                   (list (list :op operation :path directory
                               :content (when (eq operation 'before-time)
                                          "9999999999")))
                   (lambda (results error) (setq settled (list results error))))))
          (should (processp child))
          (with-timeout (10 (ert-fail "Target clock never reached stat"))
            (while (not (file-exists-p ready))
              (accept-process-output nil 0.01)))
          (setq marker (with-temp-buffer
                         (insert-file-contents captured)
                         (buffer-string)))
          (should (equal directory (directory-file-name (file-name-directory marker))))
          (should (file-regular-p marker))
          (should (process-live-p child))
          (should-not settled)
          (should (niwa-session-durability-lease-acquire root "overlap" session))
          (should (niwa-session-durability-lease-owned-p session))
          (should (file-regular-p marker))
          (should (process-live-p child))
          (should-not settled)
          (write-region "" nil continue nil 'silent)
          (with-timeout (10 (ert-fail "Target clock never settled"))
            (while (not settled) (accept-process-output nil 0.01)))
          (should-not (cadr settled))
          (should (eq 'ok (plist-get (caar settled) :status)))
          (when (eq operation 'target-time)
            (should (integerp (plist-get (caar settled) :value)))
            (should (> (plist-get (caar settled) :value) 0)))
          (should-not (file-exists-p marker)))
      (write-region "" nil continue nil 'silent)
      (unless (niwa-session-control-fs-wait-async)
        (when (and child (process-live-p child)) (delete-process child))
        (niwa-session-control-fs-wait-async))
      (niwa-session-durability--cancel-renewal session)
      (delete-directory root t)))
  (operation)
  :doc "target-time scratch remains non-authoritative during real acquisition"
  'target-time
  :doc "before-time scratch remains non-authoritative during real acquisition"
  'before-time)

(provide 'test-niwa-session-durability)
;;; test-niwa-session-durability.el ends here
