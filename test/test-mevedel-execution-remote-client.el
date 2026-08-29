;;; test-mevedel-execution-remote-client.el --- real remote client -*- lexical-binding: t -*-

;;; Commentary:

;; This file is invoked by the real transport acceptance test as two separate
;; Eask/Emacs processes.  It intentionally has no default test case: without
;; MEVEDEL_REMOTE_CLIENT_ROLE it is skipped by the normal test discovery.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-durability)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication)
(require 'mevedel-session-persistence)
(require 'mevedel-session-recovery)
(require 'mevedel-workspace)
(require 'tramp)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)
(defvar gptel-backend)
(defvar gptel-model)

;; `gptel-request'
(declare-function gptel--make-backend "ext:gptel-request" (&rest slots))
(declare-function gptel-get-backend "ext:gptel-request" (name))

(defun test-mevedel-execution-remote-client--stage (format-string &rest args)
  "Write a local observable stage line for the parent acceptance process."
  (let ((line (apply #'format format-string args)))
    (message "mevedel: remote client %s" line)
    (princ (concat "remote-client: " line "\n"))))

(defun test-mevedel-execution-remote-client--wait (root name)
  "Wait for marker NAME below target ROOT."
  (let ((path (file-name-concat root ".mevedel" "accept-transfer" name)))
    (test-mevedel-execution-remote-client--stage "wait-start %s" name)
    (with-timeout (30 (ert-fail (format "Timed out waiting for %s" path)))
      (while (not (file-exists-p path))
        (accept-process-output nil 0.05)))
    (test-mevedel-execution-remote-client--stage "wait-complete %s" name)
    path))

(defun test-mevedel-execution-remote-client--wait-until (predicate label)
  "Wait until PREDICATE is true, labelling the observable wait LABEL."
  (test-mevedel-execution-remote-client--stage "wait-start %s" label)
  (with-timeout (30 (ert-fail (format "Timed out waiting for %s" label)))
    (while (not (funcall predicate))
      (accept-process-output nil 0.05)))
  (test-mevedel-execution-remote-client--stage "wait-complete %s" label))

(defun test-mevedel-execution-remote-client--mark (root name)
  "Create marker NAME below target ROOT."
  (let ((path (file-name-concat root ".mevedel" "accept-transfer" name)))
    (make-directory (file-name-directory path) t)
    (write-region "ok\n" nil path nil 'silent)
    path))

(defun test-mevedel-execution-remote-client--transfer-facts (session)
  "Return one bounded diagnostic line describing SESSION's transfer state."
  (condition-case err
      (let* ((bound (mevedel-session-lease session))
             (directory (mevedel-session-durability--lease-path
                         (mevedel-session-save-path session)))
             (head (mevedel-session-durability--lease-head directory))
             (now (mevedel-session-durability--target-time directory))
             (generation (plist-get head :generation)))
        (format
         (concat "bound-gen=%S bound-state=%S head-gen=%S head-status=%S "
                 "head-mine=%S head-expires=%S now=%S request-p=%S "
                 "decision-p=%S cached-state=%S")
         (plist-get bound :generation)
         (plist-get bound :state)
         generation
         (plist-get head :status)
         (equal mevedel-session-durability--client-id
                (plist-get head :client-id))
         (plist-get head :expires-at)
         now
         (and generation
              (mevedel-session-control-fs-path-exists-p
               (mevedel-session-transfer--request-path directory generation)))
         (and generation
              (mevedel-session-control-fs-path-exists-p
               (mevedel-session-transfer--decision-path directory generation)))
         (plist-get (mevedel-session-control-transfer session) :state)))
    (error (format "facts-error=%s" (error-message-string err)))))

(defun test-mevedel-execution-remote-client--target-process-live-p
    (root pid)
  "Return non-nil when PID is still live on target ROOT."
  (let ((default-directory root)
        (process-environment nil))
    (zerop
     (process-file "bash" nil nil nil "-c"
                   "kill -0 \"$1\" 2>/dev/null"
                   "mevedel-client-process-check"
                   (number-to-string pid)))))

(defun test-mevedel-execution-remote-client--start-long-mutation
    (session buffer root)
  "Start a target mutation that remains live until its owner stops it."
  (let* ((pid-file (file-name-concat root ".mevedel" "accept-long.pid"))
         result)
    (with-current-buffer buffer
      (setq default-directory root)
      (mevedel-execution-start-bash
       (lambda (value) (setq result value))
       :session session :owner "accept-owner"
       ;; The workload must outlive both clients' slow cold startups so
       ;; the owner provably observes `quiescing' while work is still
       ;; live; the owner stops it explicitly, and the crash scenarios
       ;; kill it by PID.
       :command
       (list "bash" "-c"
             "sleep 300 & child=$!; printf '%s\\n' \"$child\" > \"$1\"; wait \"$child\""
             "mevedel-long-owner" pid-file)
       :workdir root :writable-roots (list root) :yield-time-ms 250))
    (with-timeout (20 (ert-fail "Long owner mutation did not yield"))
      (while (null result)
        (accept-process-output nil 0.05)))
    (should-not (plist-get result :error))
    (let ((execution-id (plist-get (plist-get result :facts) :execution-id)))
      (should (stringp execution-id))
      (with-timeout (20 (ert-fail "Long owner mutation did not publish its PID"))
        (while (not (file-exists-p pid-file))
          (accept-process-output nil 0.05)))
      execution-id)))

(defun test-mevedel-execution-remote-client--stop-long-mutation
    (session execution-id)
  "Stop the yielded mutation EXECUTION-ID and wait for settlement."
  (let (result)
    (mevedel-execution-stop
     session "accept-owner" execution-id
     (lambda (value) (setq result value)))
    (with-timeout (20 (ert-fail "Long owner mutation did not stop"))
      (while (null result)
        (accept-process-output nil 0.05)))
    (should (plist-get result :claimed-final-p))))

(defun test-mevedel-execution-remote-client--acquire-after-expiry
    (session label)
  "Take over SESSION after expiry, confirming the explicit takeover prompt."
  (let (acquired)
    (with-timeout (30 (ert-fail (format "%s did not take over" label)))
      (while (not acquired)
        ;; Expired-lease takeover confirms through `y-or-n-p'; leaving it
        ;; live would block this batch client on stdin.
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (_prompt) t)))
          (setq acquired
                (mevedel-session-durability-lease-acquire
                 (mevedel-session-save-path session) label session)))
        (unless acquired
          (accept-process-output nil 0.05))))
    (should acquired)
    acquired))

(defun test-mevedel-execution-remote-client--configure-chat-buffer (buffer)
  "Install an inert registered gptel backend and model in BUFFER."
  (let* ((name "Remote Acceptance Test")
         (backend
          (condition-case nil
              (gptel-get-backend name)
            (user-error
             (let ((backend (gptel--make-backend :name name)))
               (setf (gptel-get-backend name) backend)
               backend)))))
    (with-current-buffer buffer
      (setq-local gptel-backend backend
                  gptel-model 'remote-acceptance-test))))

(defun test-mevedel-execution-remote-client--session
    (root session-id client-id)
  "Restore SESSION-ID from target ROOT and return (BUFFER . SESSION)."
  (when-let* ((config (getenv "MEVEDEL_TEST_SSH_CONFIG")))
    (setq tramp-use-connection-share t
          tramp-ssh-controlmaster-options
          (format "-F %s" (shell-quote-argument config))))
  ;; Restore and lease acquisition can ask for target disclosure before the
  ;; restored session object exists.  Disclose the exact target first.
  (puthash
   (mevedel-execution-target-identity
    (mevedel-execution-target-create root))
   t mevedel-session-durability--disclosed-targets)
  (let ((mevedel-session-durability--client-id client-id))
    (let* ((workspace
            (mevedel-workspace-get-or-create
             'project root root "remote-transfer"))
           (entry
            (cl-find session-id
                     (mevedel-session-persistence-list-sessions workspace)
                     :key (lambda (item)
                            (plist-get (plist-get item :summary)
                                       :session-id))
                     :test #'equal)))
      (unless entry
        (error "Remote acceptance session was not discoverable"))
      (let ((buffer
             (cl-letf (((symbol-function 'mevedel--chat-buffer-init-common)
                        #'ignore)
                       ;; Restoring a session whose previous owner crashed
                       ;; asks to take the expired lease over.  A batch
                       ;; client has no terminal to answer from.
                       ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                       ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                       ((symbol-function
                         'mevedel-agent-persistence-restore-tree)
                        (lambda (&rest _) 0))
                       ((symbol-function
                         'mevedel-session-artifacts-load-instructions)
                        #'ignore))
               (mevedel-session-persistence-restore
                (plist-get entry :save-path) nil nil workspace))))
        (test-mevedel-execution-remote-client--configure-chat-buffer buffer)
        (cons buffer (buffer-local-value 'mevedel--session buffer))))))

(mevedel-deftest mevedel-real-remote-transfer-client
  (:tags (external remote control-transfer))
  ,test
  (test)
  :doc "drives one independent real transport transfer client"
  (let ((role (getenv "MEVEDEL_REMOTE_CLIENT_ROLE"))
        (scenario (or (getenv "MEVEDEL_REMOTE_CLIENT_SCENARIO")
                      "transfer")))
    (if (or (not (member role '("owner" "requester")))
            (not (equal scenario "transfer")))
        (ert-skip "Not the cooperative-transfer client scenario")
      (let* ((root (getenv "MEVEDEL_REMOTE_CLIENT_ROOT"))
             (session-id (getenv "MEVEDEL_REMOTE_SESSION_ID"))
             (client-id (getenv "MEVEDEL_REMOTE_CLIENT_ID")))
        (unless (and root session-id client-id)
          (ert-fail "Remote transfer client environment is incomplete"))
      (let* ((mevedel-session-transfer-prompt-timeout 1)
             ;; TRAMP setup and marker writes can occupy the child well past
             ;; the normal short interactive lease window.
             (mevedel-session-lease-seconds 600)
             (_ (test-mevedel-execution-remote-client--stage
                 "%s-session-start" role))
             (pair (test-mevedel-execution-remote-client--session
                    root session-id client-id))
             (buffer (car pair))
             (session (cdr pair))
             (target (mevedel-session-execution-target session)))
        (test-mevedel-execution-remote-client--stage "%s-session-restored" role)
        (puthash (mevedel-execution-target-identity target) t
                 mevedel-session-durability--disclosed-targets)
        (unwind-protect
            (let ((mevedel-session-durability--client-id client-id))
              (if (equal role "owner")
                  (progn
                    (test-mevedel-execution-remote-client--stage
                     "owner-lease-start")
                    (should
                     (mevedel-session-durability-lease-acquire
                      (mevedel-session-save-path session) "accept-owner"
                      session))
                    (test-mevedel-execution-remote-client--stage
                     "owner-lease-acquired")
                    (test-mevedel-execution-remote-client--mark
                     root "a-owner")
                    (test-mevedel-execution-remote-client--stage
                     "owner-marker-a-owner")
                    (let ((execution-id
                           (test-mevedel-execution-remote-client--start-long-mutation
                            session buffer root)))
                      (test-mevedel-execution-remote-client--mark
                       root "a-long-running")
                      (test-mevedel-execution-remote-client--stage
                       "owner-marker-a-long-running")
                      (test-mevedel-execution-remote-client--wait
                       root "b-request-1")
                      (should
                       (mevedel-session-control-transfer-decide
                        session 'reject))
                      (test-mevedel-execution-remote-client--mark
                       root "a-rejected")
                      (test-mevedel-execution-remote-client--stage
                       "owner-marker-a-rejected")
                      (test-mevedel-execution-remote-client--wait
                       root "b-request-2")
                      ;; The unanswered request is granted once its
                      ;; whole-second target deadline passes, so poll until
                      ;; the owner observes it instead of assuming one
                      ;; round trip is enough.
                      (let ((state nil))
                        (with-timeout
                            (30 (ert-fail
                                 (format
                                  "Owner never began quiescing: %s"
                                  (test-mevedel-execution-remote-client--transfer-facts
                                   session))))
                          (while (not (eq state 'quiescing))
                            (accept-process-output nil 0.5)
                            (setq state
                                  (plist-get
                                   (mevedel-session-control-transfer-poll
                                    session buffer nil)
                                   :state))
                            (test-mevedel-execution-remote-client--stage
                             "owner-poll state=%S %s" state
                             (test-mevedel-execution-remote-client--transfer-facts
                              session))))
                        (should (eq 'quiescing state)))
                      (test-mevedel-execution-remote-client--mark
                       root "a-still-running")
                      (test-mevedel-execution-remote-client--stage
                       "owner-marker-a-still-running")
                      (test-mevedel-execution-remote-client--stop-long-mutation
                       session execution-id))
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (insert "Owner settled before transfer.\n"))
                    (let ((state nil))
                      (with-timeout
                          (30 (ert-fail "Owner never released control"))
                        (while (not (eq state 'released))
                          (setq state
                                (plist-get
                                 (mevedel-session-control-transfer-poll
                                  session buffer nil)
                                 :state))
                          (unless (eq state 'released)
                            (accept-process-output nil 0.5))))
                      (should (eq 'released state)))
                    (test-mevedel-execution-remote-client--mark
                     root "a-released")
                    (test-mevedel-execution-remote-client--stage
                     "owner-marker-a-released")
                    (test-mevedel-execution-remote-client--wait
                     root "b-acquired"))
                (progn
                  (test-mevedel-execution-remote-client--stage
                   "requester-lease-check")
                  (should-not
                   (mevedel-session-durability-lease-acquire
                    (mevedel-session-save-path session) "accept-requester"
                    session))
                  (test-mevedel-execution-remote-client--mark
                   root "b-read-only")
                  (test-mevedel-execution-remote-client--stage
                   "requester-marker-b-read-only")
                  (should
                     (mevedel-session-control-transfer-request
                      session))
                  (test-mevedel-execution-remote-client--mark
                   root "b-request-1")
                  (test-mevedel-execution-remote-client--stage
                   "requester-marker-b-request-1")
                  (test-mevedel-execution-remote-client--wait
                   root "a-rejected")
                  (should
                   (mevedel-session-control-transfer-request
                    session))
                  (test-mevedel-execution-remote-client--mark
                   root "b-request-2")
                  (test-mevedel-execution-remote-client--stage
                   "requester-marker-b-request-2")
                  (let (acquired)
                    (with-timeout (30 (ert-fail "Requester did not acquire"))
                      (while (not acquired)
                        (setq acquired
                              (mevedel-session-control-transfer-poll
                               session buffer t))
                        (unless acquired
                          (accept-process-output nil 0.05))))
                    ;; Acquisition proves the owner released: the fence is
                    ;; the only way in.  Its marker is a separate target
                    ;; write that the protocol does not order before the
                    ;; claim, so waiting is the only way to observe it
                    ;; without racing.  Failing here instead left the owner
                    ;; waiting on a `b-acquired' marker this client would
                    ;; never write, and the run hung rather than failed.
                    (test-mevedel-execution-remote-client--wait
                     root "a-released")
                    (with-current-buffer buffer
                      (should (string-match-p
                               (regexp-quote
                                "Owner settled before transfer.")
                               (buffer-string))))
                    (test-mevedel-execution-remote-client--mark
                     root "b-acquired")
                    (test-mevedel-execution-remote-client--stage
                     "requester-marker-b-acquired")))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (let ((mevedel-session-durability--client-id client-id))
            (when (and (mevedel-session-save-path session)
                       (mevedel-session-durability-lease-owned-p session))
              (ignore-errors
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session))))))))))

(mevedel-deftest mevedel-real-remote-crash-recovery-client
  (:tags (external remote control-transfer recovery))
  ,test
  (test)
  :doc "takes over a crashed owner and recovers target-side incomplete state"
  (let ((role (getenv "MEVEDEL_REMOTE_CLIENT_ROLE"))
        (scenario (getenv "MEVEDEL_REMOTE_CLIENT_SCENARIO")))
    (if (or (not (member role '("owner" "requester")))
            (not (member scenario '("crash-long" "recovery"))))
        (ert-skip "Not a crashed-client recovery scenario")
      (let* ((root (getenv "MEVEDEL_REMOTE_CLIENT_ROOT"))
             (session-id (getenv "MEVEDEL_REMOTE_SESSION_ID"))
             (client-id (getenv "MEVEDEL_REMOTE_CLIENT_ID"))
             ;; Short enough that the crashed owner's lease expires while
             ;; the requester waits, long enough that acquiring it survives
             ;; a real target round trip.
             (mevedel-session-lease-seconds 5)
             (pair (test-mevedel-execution-remote-client--session
                    root session-id client-id))
             (buffer (car pair))
             (session (cdr pair))
             (crashed-p nil))
        (unless (and root session-id client-id)
          (ert-fail "Remote recovery client environment is incomplete"))
        (test-mevedel-execution-remote-client--stage
         "%s-session-start" role)
        (test-mevedel-execution-remote-client--stage
         "%s-session-restored" role)
        (unwind-protect
            (let ((mevedel-session-durability--client-id client-id))
              (if (equal role "owner")
                  (if (equal scenario "crash-long")
                      (progn
                        (should
                         (mevedel-session-durability-lease-acquire
                          (mevedel-session-save-path session) "accept-owner"
                          session))
                        (test-mevedel-execution-remote-client--mark
                         root "a-owner")
                        (test-mevedel-execution-remote-client--stage
                         "owner-marker-a-owner")
                        (test-mevedel-execution-remote-client--start-long-mutation
                         session buffer root)
                        (test-mevedel-execution-remote-client--mark
                         root "a-long-running")
                        (test-mevedel-execution-remote-client--stage
                         "owner-marker-a-long-running")
                        (accept-process-output nil 1.2)
                        (test-mevedel-execution-remote-client--mark
                         root "a-crashed")
                        (test-mevedel-execution-remote-client--stage
                         "owner-marker-a-crashed")
                        (setq crashed-p t)
                        (error "Intentional lost-owner crash"))
                    (progn
                      (should
                       (mevedel-session-durability-lease-acquire
                        (mevedel-session-save-path session) "accept-owner"
                        session))
                      (test-mevedel-execution-remote-client--mark
                       root "a-owner")
                      (test-mevedel-execution-remote-client--stage
                       "owner-marker-a-owner")
                      (let ((recovery-dir
                             (make-temp-file
                              "mevedel-remote-client-recovery-" t)))
                        (unwind-protect
                            (progn
                              (write-region
                               "client crash recovery evidence\n" nil
                               (file-name-concat recovery-dir "evidence.txt")
                               nil 'silent)
                              (mevedel-session-recovery-record-failure
                               session "independent client incomplete rollback"
                               recovery-dir))
                          (when (file-directory-p recovery-dir)
                            (delete-directory recovery-dir t))))
                      (test-mevedel-execution-remote-client--mark
                       root "a-recovery-installed")
                      (test-mevedel-execution-remote-client--stage
                       "owner-marker-a-recovery-installed")
                      (accept-process-output nil 1.2)
                      (setq crashed-p t)
                      (error "Intentional recovery-owner crash")))
                (if (equal scenario "crash-long")
                    (progn
                      (test-mevedel-execution-remote-client--wait
                       root "a-crashed")
                      (test-mevedel-execution-remote-client--stage
                       "requester-taking-over-expired-owner")
                      (test-mevedel-execution-remote-client--acquire-after-expiry
                       session "accept-takeover")
                      (let ((pid-file
                             (file-name-concat root ".mevedel"
                                               "accept-long.pid")))
                        (should (file-exists-p pid-file))
                        (let ((pid
                               (with-temp-buffer
                                 (insert-file-contents pid-file)
                                 (string-to-number
                                  (string-trim (buffer-string))))))
                          (should (> pid 0))
                          (let ((default-directory root)
                                (process-environment nil))
                            (process-file "kill" nil nil nil
                                          "-KILL" (number-to-string pid)))
                          (test-mevedel-execution-remote-client--wait-until
                           (lambda ()
                             (not
                              (test-mevedel-execution-remote-client--target-process-live-p
                               root pid)))
                           "Lost-owner mutation survived takeover")))
                      (test-mevedel-execution-remote-client--mark
                       root "b-takeover")
                      (test-mevedel-execution-remote-client--stage
                       "requester-marker-b-takeover"))
                  (test-mevedel-execution-remote-client--wait
                   root "a-recovery-installed")
                  (test-mevedel-execution-remote-client--stage
                   "requester-taking-over-recovery")
                  (test-mevedel-execution-remote-client--acquire-after-expiry
                   session "accept-recovery")
                  (should (mevedel-session-recovery-refresh session))
                  (should
                   (plist-get
                    (mevedel-session-pending-publication session)
                    :manual-recovery-marker))
                  (let ((mevedel-session-lease-seconds 600))
                    (should (mevedel-session-durability-lease-renew session))
                    (cl-letf (((symbol-function 'yes-or-no-p)
                               (lambda (_prompt) t)))
                      (should
                       (mevedel-session-publication-abandon session))))
                  (should-not (mevedel-session-recovery-read
                               (mevedel-session-save-path session)))
                  (test-mevedel-execution-remote-client--mark
                   root "b-recovery-recovered")
                  (test-mevedel-execution-remote-client--stage
                   "requester-marker-b-recovery-recovered"))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (let ((mevedel-session-durability--client-id client-id))
            (when (and (not crashed-p)
                       (mevedel-session-save-path session)
                       (mevedel-session-durability-lease-owned-p session))
              (ignore-errors
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))))))

(provide 'test-mevedel-execution-remote-client)

;;; test-mevedel-execution-remote-client.el ends here
