;;; test-niwa-execution-remote-client.el --- real remote client -*- lexical-binding: t -*-

;;; Commentary:

;; This file is invoked by the real transport acceptance test as two separate
;; Eask/Emacs processes.  Without NIWA_REMOTE_CLIENT_ROLE its real client
;; scenarios are skipped; fixture isolation checks still run normally.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa)
;; Restore also autoloads chat's cache setup.  Load its definitions before
;; stubbing initialization, or that autoload replaces the inert fixture.
(require 'niwa-chat)
(require 'niwa-execution)
(require 'niwa-execution-target)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-session-transfer)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-session-recovery)
(require 'niwa-workspace)
(require 'tramp)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

;; `gptel'
(defvar gptel-backend)
(defvar gptel-model)

;; `gptel-request'
(declare-function gptel--make-backend "ext:gptel-request" (&rest slots))
(declare-function gptel-get-backend "ext:gptel-request" (name))

;; `tramp-sh'
(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)

(defun test-niwa-execution-remote-client--root ()
  "Validate and return this client's exact disposable target directory."
  (let* ((value (getenv "NIWA_REMOTE_CLIENT_ROOT"))
         (method (and value (file-remote-p value 'method 'never)))
         (variable (pcase method
                     ("ssh" "NIWA_TEST_SSH_ROOT")
                     ("podman" "NIWA_TEST_PODMAN_ROOT")
                     ("docker" "NIWA_TEST_DOCKER_ROOT"))))
    (unless variable
      (ert-fail "Remote client root must use SSH, Podman, or Docker"))
    (let* ((base (test-niwa-execution-remote--real-root
                  variable (intern method)))
           (root (file-name-as-directory value)))
      (unless (and (equal (file-remote-p base) (file-remote-p root))
                   (not (file-remote-p value 'hop 'never))
                   (not (equal base root))
                   (not (equal "/" (file-remote-p root 'localname))))
        (ert-fail "Remote client root must own a directory on the provisioned target"))
      (test-niwa-execution-remote--real-root
       "NIWA_REMOTE_CLIENT_ROOT" (intern method)))))

(defmacro test-niwa-execution-remote-client--with-root (&rest body)
  "Run BODY with only the validated remote client root writable."
  (declare (indent 0) (debug (body)))
  `(let* ((root (test-niwa-execution-remote-client--root))
          (niwa-test--write-roots (cons root niwa-test--write-roots)))
     (test-niwa-execution-remote--with-temporary-directory root
       ,@body)))

(defun test-niwa-execution-remote-client--stage (format-string &rest args)
  "Write a local observable stage line for the parent acceptance process."
  (let ((line (apply #'format format-string args)))
    (message "niwa: remote client %s" line)
    (princ (concat "remote-client: " line "\n"))))

(defun test-niwa-execution-remote-client--wait (root name)
  "Wait for marker NAME below target ROOT."
  (let ((path (file-name-concat root ".niwa" "accept-transfer" name)))
    (test-niwa-execution-remote-client--stage "wait-start %s" name)
    (with-timeout (30 (ert-fail (format "Timed out waiting for %s" path)))
      (while (not (file-exists-p path))
        (accept-process-output nil 0.05)))
    (test-niwa-execution-remote-client--stage "wait-complete %s" name)
    path))

(defun test-niwa-execution-remote-client--wait-until (predicate label)
  "Wait until PREDICATE is true, labelling the observable wait LABEL."
  (test-niwa-execution-remote-client--stage "wait-start %s" label)
  (with-timeout (30 (ert-fail (format "Timed out waiting for %s" label)))
    (while (not (funcall predicate))
      (accept-process-output nil 0.05)))
  (test-niwa-execution-remote-client--stage "wait-complete %s" label))

(defun test-niwa-execution-remote-client--mark (root name)
  "Create marker NAME below target ROOT."
  (let ((path (file-name-concat root ".niwa" "accept-transfer" name)))
    (make-directory (file-name-directory path) t)
    (write-region "ok\n" nil path nil 'silent)
    path))

(defun test-niwa-execution-remote-client--transfer-facts (session)
  "Return one bounded diagnostic line describing SESSION's transfer state."
  (condition-case err
      (let* ((bound (niwa-session-lease session))
             (directory (niwa-session-durability--lease-path
                         (niwa-session-save-path session)))
             (head (niwa-session-durability--lease-head directory))
             (now (niwa-session-durability--target-time directory))
             (generation (plist-get head :generation)))
        (format
         (concat "bound-gen=%S bound-state=%S head-gen=%S head-status=%S "
                 "head-mine=%S head-expires=%S now=%S request-p=%S "
                 "decision-p=%S cached-state=%S")
         (plist-get bound :generation)
         (plist-get bound :state)
         generation
         (plist-get head :status)
         (equal niwa-session-durability--client-id
                (plist-get head :client-id))
         (plist-get head :expires-at)
         now
         (and generation
              (niwa-session-control-fs-path-exists-p
               (niwa-session-transfer--request-path directory generation)))
         (and generation
              (niwa-session-control-fs-path-exists-p
               (niwa-session-transfer--decision-path directory generation)))
         (plist-get (niwa-session-control-transfer session) :state)))
    (error (format "facts-error=%s" (error-message-string err)))))

(defun test-niwa-execution-remote-client--target-process-live-p
    (root pid)
  "Return non-nil when PID is still live on target ROOT."
  (let ((default-directory root)
        (process-environment nil))
    (zerop
     (process-file "bash" nil nil nil "-c"
                   "kill -0 \"$1\" 2>/dev/null"
                   "niwa-client-process-check"
                   (number-to-string pid)))))

(defun test-niwa-execution-remote-client--start-long-mutation
    (session buffer root)
  "Start a target mutation that remains live until its owner stops it."
  (let* ((pid-file (file-name-concat root ".niwa" "accept-long.pid"))
         result)
    (with-current-buffer buffer
      (setq default-directory root)
      (niwa-execution-start-bash
       (lambda (value) (setq result value))
       :session session :owner "accept-owner"
       ;; The workload must outlive both clients' slow cold startups so
       ;; the owner provably observes `quiescing' while work is still
       ;; live; the owner stops it explicitly, and the crash scenarios
       ;; kill it by PID.
       :command
       (list "bash" "-c"
             "sleep 300 & child=$!; printf '%s\\n' \"$child\" > \"$1\"; wait \"$child\""
             "niwa-long-owner" pid-file)
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

(defun test-niwa-execution-remote-client--stop-long-mutation
    (session execution-id)
  "Stop the yielded mutation EXECUTION-ID and wait for settlement."
  (let ((settle (symbol-function 'niwa-execution--write-mutation-settlement))
        result)
    ;; A completion can arrive while another target operation is still in flight.
    (cl-letf (((symbol-function 'niwa-execution--write-mutation-settlement)
               (lambda (record authority)
                 (niwa-transport-call-as-remote-operation
                  (lambda () (funcall settle record authority))))))
      (niwa-execution-stop
       session "accept-owner" execution-id
       (lambda (value) (setq result value)))
      (with-timeout (20 (ert-fail "Long owner mutation did not stop"))
        (while (null result)
          (accept-process-output nil 0.05))))
    (should (plist-get result :claimed-final-p))))

(defun test-niwa-execution-remote-client--acquire-after-expiry
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
                (niwa-session-durability-lease-acquire
                 (niwa-session-save-path session) label session)))
        (unless acquired
          (accept-process-output nil 0.05))))
    (should acquired)
    acquired))

(defun test-niwa-execution-remote-client--configure-chat-buffer (buffer)
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

(defun test-niwa-execution-remote-client--session
    (root session-id client-id)
  "Restore SESSION-ID from target ROOT and return (BUFFER . SESSION)."
  (when-let* ((config (getenv "NIWA_TEST_SSH_CONFIG")))
    (setq tramp-use-connection-share t
          tramp-ssh-controlmaster-options
          (format "-F %s" (shell-quote-argument config))))
  ;; Restore and lease acquisition can ask for target disclosure before the
  ;; restored session object exists.  Disclose the exact target first.
  (puthash
   (niwa-execution-target-identity
    (niwa-execution-target-create root))
   t niwa-session-durability--disclosed-targets)
  (let ((niwa-session-durability--client-id client-id))
    (let* ((workspace
            (niwa-workspace-get-or-create
             'project root root "remote-transfer"))
           (entry
            (cl-find session-id
                     (niwa-session-persistence-list-sessions workspace)
                     :key (lambda (item)
                            (plist-get (plist-get item :summary)
                                       :session-id))
                     :test #'equal)))
      (unless entry
        ;; Retain the failed discovery itself before fixture cleanup removes
        ;; its evidence.  Do not retry: a second read can hide an interleaving.
        (error "Remote acceptance session %s was not discoverable: %S"
               session-id
               (niwa-session-persistence--enumerate-sessions workspace t)))
      (let ((buffer
             (cl-letf (((symbol-function 'niwa--chat-buffer-init-common)
                        #'ignore)
                       ;; Restoring a session whose previous owner crashed
                       ;; asks to take the expired lease over.  A batch
                       ;; client has no terminal to answer from.
                       ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                       ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                       ((symbol-function
                         'niwa-agent-persistence-restore-tree)
                        (lambda (&rest _) 0))
                       ((symbol-function
                         'niwa-session-artifacts-load-instructions)
                        #'ignore))
               (niwa-session-persistence-restore
                (plist-get entry :save-path) nil nil workspace))))
        (test-niwa-execution-remote-client--configure-chat-buffer buffer)
        (cons buffer (buffer-local-value 'niwa--session buffer))))))

(niwa-deftest niwa-real-remote-transfer-client
  (:tags (external remote control-transfer)
         :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "drives one independent real transport transfer client"
  (let ((role (getenv "NIWA_REMOTE_CLIENT_ROLE"))
        (scenario (or (getenv "NIWA_REMOTE_CLIENT_SCENARIO")
                      "transfer")))
    (if (or (not (member role '("owner" "requester")))
            (not (equal scenario "transfer")))
        (ert-skip "Not the cooperative-transfer client scenario")
      (test-niwa-execution-remote-client--with-root
        (let* ((session-id (getenv "NIWA_REMOTE_SESSION_ID"))
               (client-id (getenv "NIWA_REMOTE_CLIENT_ID")))
          (unless (and root session-id client-id)
            (ert-fail "Remote transfer client environment is incomplete"))
          (let* ((niwa-session-transfer-prompt-timeout 1)
                 ;; TRAMP setup and marker writes can occupy the child well past
                 ;; the normal short interactive lease window.
                 (niwa-session-lease-seconds 600)
                 (_ (test-niwa-execution-remote-client--stage
                     "%s-session-start" role))
                 (pair (test-niwa-execution-remote-client--session
                        root session-id client-id))
                 (buffer (car pair))
                 (session (cdr pair))
                 (target (niwa-session-execution-target session)))
            (test-niwa-execution-remote-client--stage "%s-session-restored" role)
            (puthash (niwa-execution-target-identity target) t
                     niwa-session-durability--disclosed-targets)
            (unwind-protect
                (let ((niwa-session-durability--client-id client-id))
                  (if (equal role "owner")
                      (progn
                        (test-niwa-execution-remote-client--stage
                         "owner-lease-start")
                        (should
                         (niwa-session-durability-lease-acquire
                          (niwa-session-save-path session) "accept-owner"
                          session))
                        (test-niwa-execution-remote-client--stage
                         "owner-lease-acquired")
                        (test-niwa-execution-remote-client--mark
                         root "a-owner")
                        (test-niwa-execution-remote-client--stage
                         "owner-marker-a-owner")
                        (let ((execution-id
                               (test-niwa-execution-remote-client--start-long-mutation
                                session buffer root)))
                          (test-niwa-execution-remote-client--mark
                           root "a-long-running")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-long-running")
                          (test-niwa-execution-remote-client--wait
                           root "b-request-1")
                          (should
                           (niwa-session-control-transfer-decide
                            session 'reject))
                          (test-niwa-execution-remote-client--mark
                           root "a-rejected")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-rejected")
                          (test-niwa-execution-remote-client--wait
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
                                      (test-niwa-execution-remote-client--transfer-facts
                                       session))))
                              (while (not (eq state 'quiescing))
                                (accept-process-output nil 0.5)
                                (setq state
                                      (plist-get
                                       (niwa-session-control-transfer-poll
                                        session buffer nil)
                                       :state))
                                (test-niwa-execution-remote-client--stage
                                 "owner-poll state=%S %s" state
                                 (test-niwa-execution-remote-client--transfer-facts
                                  session))))
                            (should (eq 'quiescing state)))
                          (test-niwa-execution-remote-client--mark
                           root "a-still-running")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-still-running")
                          ;; The live mutation is what holds the granted
                          ;; transfer back.  Once it stops, a transfer poll timer
                          ;; can save, release, and replace BUFFER inside any
                          ;; wait, so the owner's last words go in first.
                          (with-current-buffer buffer
                            (goto-char (point-max))
                            (insert "Owner settled before transfer.\n"))
                          (test-niwa-execution-remote-client--stop-long-mutation
                           session execution-id))
                        (let ((state nil))
                          (with-timeout
                              (30 (ert-fail "Owner never released control"))
                            (while (not (eq state 'released))
                              (setq state
                                    (plist-get
                                     (niwa-session-control-transfer-poll
                                      session buffer nil)
                                     :state))
                              (unless (eq state 'released)
                                (accept-process-output nil 0.5))))
                          (should (eq 'released state)))
                        (test-niwa-execution-remote-client--mark
                         root "a-released")
                        (test-niwa-execution-remote-client--stage
                         "owner-marker-a-released")
                        (test-niwa-execution-remote-client--wait
                         root "b-acquired"))
                    (progn
                      (test-niwa-execution-remote-client--stage
                       "requester-lease-check")
                      (should-not
                       (niwa-session-durability-lease-acquire
                        (niwa-session-save-path session) "accept-requester"
                        session))
                      (test-niwa-execution-remote-client--mark
                       root "b-read-only")
                      (test-niwa-execution-remote-client--stage
                       "requester-marker-b-read-only")
                      (should
                       (niwa-session-control-transfer-request
                        session))
                      (test-niwa-execution-remote-client--mark
                       root "b-request-1")
                      (test-niwa-execution-remote-client--stage
                       "requester-marker-b-request-1")
                      (test-niwa-execution-remote-client--wait
                       root "a-rejected")
                      (should
                       (niwa-session-control-transfer-request
                        session))
                      (test-niwa-execution-remote-client--mark
                       root "b-request-2")
                      (test-niwa-execution-remote-client--stage
                       "requester-marker-b-request-2")
                      (let (acquired)
                        (with-timeout (30 (ert-fail "Requester did not acquire"))
                          (while (not acquired)
                            (setq acquired
                                  (niwa-session-control-transfer-poll
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
                        (test-niwa-execution-remote-client--wait
                         root "a-released")
                        (with-current-buffer buffer
                          (should (string-match-p
                                   (regexp-quote
                                    "Owner settled before transfer.")
                                   (buffer-string))))
                        (test-niwa-execution-remote-client--mark
                         root "b-acquired")
                        (test-niwa-execution-remote-client--stage
                         "requester-marker-b-acquired")))))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (set-buffer-modified-p nil))
                (kill-buffer buffer))
              (let ((niwa-session-durability--client-id client-id))
                ;; Release checks the target's head itself.  One failed
                ;; renewal binds this session `lost' while its record stays
                ;; live there; skipping the release on that belief leaves the
                ;; journey waiting out a 600-second lease.
                (when (niwa-session-save-path session)
                  (ignore-errors
                    (niwa-session-durability-lease-release
                     (niwa-session-save-path session) session)))))))))))

(niwa-deftest niwa-real-remote-crash-recovery-client
  (:tags (external remote control-transfer recovery)
         :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "takes over a crashed owner and recovers target-side incomplete state"
  (let ((role (getenv "NIWA_REMOTE_CLIENT_ROLE"))
        (scenario (getenv "NIWA_REMOTE_CLIENT_SCENARIO")))
    (if (or (not (member role '("owner" "requester")))
            (not (member scenario '("crash-long" "recovery"))))
        (ert-skip "Not a crashed-client recovery scenario")
      (test-niwa-execution-remote-client--with-root
        (let* ((session-id (getenv "NIWA_REMOTE_SESSION_ID"))
               (client-id (getenv "NIWA_REMOTE_CLIENT_ID"))
               (_ (unless (and session-id client-id)
                    (ert-fail "Remote recovery client environment is incomplete")))
               ;; Short enough that the crashed owner's lease expires while
               ;; the requester waits, long enough that acquiring it survives
               ;; a real target round trip.
               (niwa-session-lease-seconds 5)
               (pair (test-niwa-execution-remote-client--session
                      root session-id client-id))
               (buffer (car pair))
               (session (cdr pair))
               (crashed-p nil))
          (unless (and root session-id client-id)
            (ert-fail "Remote recovery client environment is incomplete"))
          (test-niwa-execution-remote-client--stage
           "%s-session-start" role)
          (test-niwa-execution-remote-client--stage
           "%s-session-restored" role)
          (unwind-protect
              (let ((niwa-session-durability--client-id client-id))
                (if (equal role "owner")
                    (if (equal scenario "crash-long")
                        (progn
                          (should
                           (niwa-session-durability-lease-acquire
                            (niwa-session-save-path session) "accept-owner"
                            session))
                          (test-niwa-execution-remote-client--mark
                           root "a-owner")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-owner")
                          (test-niwa-execution-remote-client--start-long-mutation
                           session buffer root)
                          (test-niwa-execution-remote-client--mark
                           root "a-long-running")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-long-running")
                          (accept-process-output nil 1.2)
                          (test-niwa-execution-remote-client--mark
                           root "a-crashed")
                          (test-niwa-execution-remote-client--stage
                           "owner-marker-a-crashed")
                          (setq crashed-p t)
                          (error "Intentional lost-owner crash"))
                      (progn
                        (should
                         (niwa-session-durability-lease-acquire
                          (niwa-session-save-path session) "accept-owner"
                          session))
                        (test-niwa-execution-remote-client--mark
                         root "a-owner")
                        (test-niwa-execution-remote-client--stage
                         "owner-marker-a-owner")
                        (let ((recovery-dir
                               (make-temp-file
                                "niwa-remote-client-recovery-" t)))
                          (unwind-protect
                              (progn
                                (write-region
                                 "client crash recovery evidence\n" nil
                                 (file-name-concat recovery-dir "evidence.txt")
                                 nil 'silent)
                                (niwa-session-recovery-record-failure
                                 session "independent client incomplete rollback"
                                 recovery-dir))
                            (when (file-directory-p recovery-dir)
                              (delete-directory recovery-dir t))))
                        (test-niwa-execution-remote-client--mark
                         root "a-recovery-installed")
                        (test-niwa-execution-remote-client--stage
                         "owner-marker-a-recovery-installed")
                        (accept-process-output nil 1.2)
                        (setq crashed-p t)
                        (error "Intentional recovery-owner crash")))
                  (if (equal scenario "crash-long")
                      (progn
                        (test-niwa-execution-remote-client--wait
                         root "a-crashed")
                        (test-niwa-execution-remote-client--stage
                         "requester-taking-over-expired-owner")
                        (test-niwa-execution-remote-client--acquire-after-expiry
                         session "accept-takeover")
                        (let ((pid-file
                               (file-name-concat root ".niwa"
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
                            (test-niwa-execution-remote-client--wait-until
                             (lambda ()
                               (not
                                (test-niwa-execution-remote-client--target-process-live-p
                                 root pid)))
                             "Lost-owner mutation survived takeover")))
                        (test-niwa-execution-remote-client--mark
                         root "b-takeover")
                        (test-niwa-execution-remote-client--stage
                         "requester-marker-b-takeover"))
                    (test-niwa-execution-remote-client--wait
                     root "a-recovery-installed")
                    (test-niwa-execution-remote-client--stage
                     "requester-taking-over-recovery")
                    (test-niwa-execution-remote-client--acquire-after-expiry
                     session "accept-recovery")
                    (should (niwa-session-recovery-refresh session))
                    (should
                     (plist-get
                      (niwa-session-pending-publication session)
                      :manual-recovery-marker))
                    (let ((niwa-session-lease-seconds 600))
                      (should (niwa-session-durability-lease-renew session))
                      (cl-letf (((symbol-function 'yes-or-no-p)
                                 (lambda (_prompt) t)))
                        (should
                         (niwa-session-publication-abandon session))))
                    (should-not (niwa-session-recovery-read
                                 (niwa-session-save-path session)))
                    (test-niwa-execution-remote-client--mark
                     root "b-recovery-recovered")
                    (test-niwa-execution-remote-client--stage
                     "requester-marker-b-recovery-recovered"))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (set-buffer-modified-p nil))
              (kill-buffer buffer))
            (let ((niwa-session-durability--client-id client-id))
              (when (and (not crashed-p)
                         (niwa-session-save-path session))
                (ignore-errors
                  (niwa-session-durability-lease-release
                   (niwa-session-save-path session) session))))))))))

(niwa-deftest test-niwa-execution-remote-client--root ()
  ,test
  (test)
  :doc "validates the exact client directory before opening its connection"
  (let ((process-environment (copy-sequence process-environment))
        checked)
    (cl-letf (((symbol-function 'test-niwa-execution-remote--real-root)
               (lambda (variable method)
                 (push (list variable method) checked)
                 (if (equal variable "NIWA_TEST_SSH_ROOT")
                     "/ssh:client-fixture:/workspace/"
                   (file-name-as-directory (getenv variable))))))
      (setenv "NIWA_REMOTE_CLIENT_ROOT" "/ssh:client-fixture:/tmp/owned")
      (should (equal (test-niwa-execution-remote-client--root)
                     "/ssh:client-fixture:/tmp/owned/"))
      (should (equal checked '(("NIWA_REMOTE_CLIENT_ROOT" ssh)
                               ("NIWA_TEST_SSH_ROOT" ssh))))
      (dolist (root '("/ssh:another-target:/tmp/owned/"
                      "/ssh:client-fixture:/workspace/"
                      "/ssh:client-fixture:/"
                      "/ssh:jump|ssh:client-fixture:/tmp/owned/"
                      "/tmp/not-remote/"))
        (setq checked nil)
        (setenv "NIWA_REMOTE_CLIENT_ROOT" root)
        (ert-info ((format "Rejected client root: %S, environment: %S"
                           root (getenv "NIWA_REMOTE_CLIENT_ROOT")))
          (should-error (test-niwa-execution-remote-client--root)
                        :type 'ert-test-failed))
        (should-not (assoc "NIWA_REMOTE_CLIENT_ROOT" checked))))))

(niwa-deftest test-niwa-execution-remote-client--with-root ()
  ,test
  (test)
  :doc "writes client markers only inside the exact validated directory"
  (let* ((owned (file-name-as-directory (make-temp-file "client-owned-" t)))
         (outside (make-temp-file "client-sibling-" t))
         (original-roots niwa-test--write-roots))
    (unwind-protect
        (cl-letf (((symbol-function 'test-niwa-execution-remote-client--root)
                   (lambda () owned)))
          (let ((niwa-test--write-roots nil)
                (niwa-test--write-violations nil))
            (test-niwa-execution-remote-client--with-root
              (should (equal niwa-test--write-roots (list owned)))
              (should (equal (temporary-file-directory) owned))
              (let ((marker (test-niwa-execution-remote-client--mark
                             root "fixture-marker")))
                (should (equal marker (file-name-concat
                                       owned ".niwa" "accept-transfer"
                                       "fixture-marker")))
                (should (file-exists-p marker)))
              (should-error (test-niwa-execution-remote-client--mark
                             outside "not-owned")))
            (should-not niwa-test--write-roots)))
      (should (equal original-roots niwa-test--write-roots))
      (delete-directory owned t)
      (delete-directory outside t))))

(provide 'test-niwa-execution-remote-client)

;;; test-niwa-execution-remote-client.el ends here
