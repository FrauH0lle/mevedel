;;; test-niwa-turn-request.el -- Request lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests request admission, identity, cancellation, and teardown.

;;; Code:

(require 'niwa-agent-control)
(require 'niwa-agents)
(require 'niwa-artifact-backend)
(require 'niwa-execution)
(require 'niwa-execution-target)
(require 'niwa-permission-queue)
(require 'niwa-permissions)
(require 'niwa-plan-mode)
(require 'niwa-queue)
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-turn)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `niwa-agents'
(defvar niwa--agent-invocation)
(niwa-deftest niwa-request-note-untracked-effect
  (:doc "deduplicates capture gaps by their mutation source")
  (let ((request (niwa-request--create)))
    (niwa-request-note-untracked-effect request "Bash" "first")
    (niwa-request-note-untracked-effect request "Bash" "second")
    (niwa-request-note-untracked-effect request "Eval" "third")
    (should (equal '(("Eval" . "third") ("Bash" . "first"))
                   (niwa-request-untracked-effects request)))))



;;
;;; Request lifecycle

(defun test-niwa-request--begin-in-memory (session)
  "Admit SESSION for request-state tests that have no durable conversation."
  ;; Lease admission is exercised by the real-storage and remote-lease cases.
  (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
             #'ignore))
    (niwa-request-begin session)))

(niwa-deftest niwa-current-origin ()
  ,test
  (test)
  :doc "prefers request ownership, then agent path, then root"
  (let ((niwa--current-request
         (niwa-request--create :origin "/root/request"))
        (niwa--agent-invocation
         (niwa-agent-invocation--create :agent-id "agent-owner" :path "/root/agent_owner")))
    (should (equal "/root/request" (niwa-current-origin))))
  (with-temp-buffer
    (let ((niwa--current-request nil))
      (setq-local
       niwa--agent-invocation
       (niwa-agent-invocation--create
        :agent-id "agent-owner" :path "/root/agent_owner"))
      (should (equal "/root/agent_owner" (niwa-current-origin)))))
  (with-temp-buffer
    (let ((niwa--current-request nil))
      (should (equal "/root" (niwa-current-origin))))))

(niwa-deftest niwa-current-turn ()
  ,test
  (test)
  :doc "uses the active request reservation and otherwise the next turn"
  (with-temp-buffer
    (let* ((session (niwa-session--create :turn-count 4))
           (niwa--current-request nil))
      (should (= 5 (niwa-current-turn session)))
      (setq niwa--current-request
            (niwa-request--create :session session :turn 7))
      (should (= 7 (niwa-current-turn session))))))

(niwa-deftest niwa-request-active-p ()
  ,test
  (test)
  :doc "reports whether a buffer has an active request"
  (with-temp-buffer
    (should-not (niwa-request-active-p))
    (setq-local niwa--current-request t)
    (should (niwa-request-active-p))))

(niwa-deftest niwa-turn-busy-p ()
  ,test
  (test)
  :doc "includes both active requests and deferred settlement"
  (with-temp-buffer
    (should-not (niwa-turn-busy-p))
    (setq-local niwa--turn-settlements-pending '(settlement))
    (should (niwa-turn-busy-p))
    (setq-local niwa--turn-settlements-pending nil
                niwa--current-request t)
    (should (niwa-turn-busy-p))))

(niwa-deftest niwa-busy-p ()
  ,test
  (test)
  :doc "answers for every buffer in this Emacs"
  (with-temp-buffer
    (should-not (niwa-busy-p))
    (setq-local niwa--turn-settlements-pending '(settlement))
    (with-temp-buffer
      (should (eq t (niwa-busy-p))))
    (setq-local niwa--turn-settlements-pending nil)
    (should-not (niwa-busy-p)))

  :doc "retained agents keep the host busy through admission and settlement"
  (with-temp-buffer
    (let* ((record (niwa-agent-record--create :path "/root/worker" :activity 'running))
           (session (niwa-session--create :agent-registry (list (cons "/root/worker" record)))))
      (setq-local niwa--session session)
      (should-not (niwa-turn-busy-p))
      (should (niwa-busy-p))
      (setf (niwa-agent-record-activity record) 'idle)
      (should-not (niwa-busy-p))
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :transcript-status 'completed
                   :runtime-pending-response '(:response "Done")))
      (should (niwa-busy-p))
      (setf (niwa-agent-invocation-runtime-pending-response niwa--agent-invocation) nil)
      (should-not (niwa-busy-p))
      (setf (niwa-session-agent-reservations session) '(reservation))
      (should (niwa-busy-p)))))

(niwa-deftest niwa-restart-unsafe-p ()
  ,test
  (test)
  :doc "a running turn makes a restart unsafe"
  (with-temp-buffer
    (should-not (niwa-restart-unsafe-p))
    (setq-local niwa--turn-settlements-pending '(settlement))
    (should (eq t (niwa-restart-unsafe-p))))

  :doc "an idle session with an active Goal makes a restart unsafe"
  (with-temp-buffer
    (let ((goal (niwa-goal--create :status 'active)))
      (setq-local niwa--session (niwa-session--create :goal goal))
      (should-not (niwa-busy-p))
      (should (eq t (niwa-restart-unsafe-p)))
      (dolist (status '(paused blocked budget-limited complete))
        (setf (niwa-goal-status goal) status)
        (should-not (niwa-restart-unsafe-p)))))

  :doc "an idle session with a live execution, including a Monitor, makes a restart unsafe"
  (with-temp-buffer
    (let* ((session (niwa-session--create))
           live)
      (setq-local niwa--session session)
      ;; Execution liveness has its own coverage; this checks the restart guard asks it.
      (cl-letf (((symbol-function 'niwa-execution-session-live-p)
                 (lambda (s) (and live (eq s session)))))
        (should-not (niwa-restart-unsafe-p))
        (setq live t)
        (should-not (niwa-busy-p))
        (should (eq t (niwa-restart-unsafe-p)))))))

(niwa-deftest niwa-request-state-label ()
  ,test
  (test)
  :doc "reports idle, settling, and running request states"
  (with-temp-buffer
    (should (equal "idle" (niwa-request-state-label)))
    (setq-local niwa--turn-settlements-pending '(settlement))
    (should (equal "settling" (niwa-request-state-label)))
    (setq-local niwa--current-request t)
    (should (equal "running" (niwa-request-state-label)))))

(niwa-deftest niwa-request-begin
  (:before-each (niwa-workspace-clear-registry)
   :after-each
   (niwa-workspace-clear-registry)
   (setq niwa--current-request nil))
  ,test
  (test)
  :doc "creates request and sets buffer-local"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (req (test-niwa-request--begin-in-memory session)))
      (should (niwa-request-p req))
      (should (eq req niwa--current-request))
      (should (eq session (niwa-request-session req)))
      (should (eq 'running
                  (niwa-session-agent-root-activity session)))
      (should (hash-table-p (niwa-request-file-snapshots req)))))

  :doc "starts in-flight conversation checkpoints"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (started 0))
      (cl-letf (((symbol-function
                  'niwa-session-persistence-start-checkpoints)
                 (lambda () (cl-incf started))))
        (test-niwa-request--begin-in-memory session))
      (should (= 1 started))))

  :doc "asks to keep the machine awake while the turn runs"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (updates 0))
      (cl-letf (((symbol-function 'niwa-sleep-update)
                 (lambda (&rest _) (cl-incf updates))))
        (test-niwa-request--begin-in-memory session))
      (should (= 1 updates))))

  :doc "rejects admission while a lost turn is still settling"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "settling")))
      (setq-local niwa--turn-settlements-pending '(settlement))
      (should-error (niwa-request-begin session) :type 'user-error)
      (should-not niwa--current-request)))

  :doc "reserves the next turn without committing it"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (setf (niwa-session-turn-count session) 4)
      (let ((req (test-niwa-request--begin-in-memory session)))
        (should (= 5 (niwa-request-turn req)))
        (should (= 4 (niwa-session-turn-count session))))))

  :doc "a root request is the retry: it clears notices and an unjustified failure pause"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (niwa-recovery-report session "request" 'authentication "Failed" nil)
      (niwa-recovery-report session "agent:a" 'request "Agent a failed" nil)
      (niwa-session-set-pending-input-failure-paused session t)
      (test-niwa-request--begin-in-memory session)
      (should-not (niwa-session-recovery-issues session))
      (should-not (niwa-session-pending-input-failure-paused session))
      (niwa-request-end)
      ;; Failed input still awaiting review keeps delivery paused.
      (setf (niwa-session-pending-steering session)
            (list (list :id 1 :input "lost" :state 'failed-turn)))
      (niwa-session-set-pending-input-failure-paused session t)
      (test-niwa-request--begin-in-memory session)
      (should (niwa-session-pending-input-failure-paused session))
      (niwa-request-end)
      (setf (niwa-session-pending-steering session) nil)
      ;; Owned blocking issues refuse the request and survive it.
      (niwa-recovery-report session "preset" 'configuration "Choose a preset" t)
      (should-error (niwa-request-begin session) :type 'user-error)
      (should (niwa-recovery-blocker session))))

  :doc "a retained agent's admission leaves root recovery state alone"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create :path "/root/child"))
      (niwa-recovery-report session "authentication" 'authentication "Sign in" t)
      (niwa-recovery-report session "request" 'request "Root failed" nil)
      (test-niwa-request--begin-in-memory session)
      (should (= 2 (length (niwa-session-recovery-issues session))))))

  :doc "records agent origin when request begins in a sub-agent buffer"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (agent (niwa-agent--create :name "verifier"))
           (inv (niwa-agent-invocation-create agent)))
      (setf (niwa-agent-invocation-agent-id inv) "verifier--abc")
      (setf (niwa-agent-invocation-path inv) "/root/verifier")
      (setq-local niwa--agent-invocation inv)
      (let ((req (test-niwa-request--begin-in-memory session)))
        (should (equal "/root/verifier"
                       (niwa-request-origin req)))
        (should (eq 'idle
                    (niwa-session-agent-root-activity session))))))

  :doc "replaces stale request with warning"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (req1 (test-niwa-request--begin-in-memory session))
           diagnostics
           (req2 (niwa-test--with-captured-messages diagnostics
                   (test-niwa-request--begin-in-memory session))))
      (should (string-match-p "stale request found" diagnostics))
      (should (eq req2 niwa--current-request))
      (should-not (eq req1 req2))))

  :doc "replacing stale request drains queued interactions"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (outcomes nil)
           (request (test-niwa-request--begin-in-memory session)))
      (setf (niwa-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :origin "/root"
                        :request-id (niwa-request-id request)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'permission outcome) outcomes)))))
      (setf (niwa-session-pending-plan-approval session)
            (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes))))
      (niwa-test--with-captured-messages nil
        (test-niwa-request--begin-in-memory session))
      (should (null (niwa-session-permission-queue session)))
      (should (null (niwa-session-pending-plan-approval session)))
      (should (equal '((plan . aborted) (permission . aborted))
                     outcomes))))

  :doc "probes an unprobed remote target before admitting the request"
  (with-temp-buffer
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote"
                       :root "/ssh:user@host:/srv/project/" :name "remote"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/ssh:user@host:/srv/project/"))
           (probe-count 0))
      (cl-letf (((symbol-function 'niwa-execution-target-probe)
                 (lambda (probed &optional _refresh _sandbox-mode)
                   (cl-incf probe-count)
                   (setf (niwa-execution-target-readiness probed)
                         '(:status ready))))
                ((symbol-function
                  'niwa-session-artifacts-assert-mutation-authority)
                 (lambda (&rest _) t)))
        (should (niwa-request-p (niwa-request-begin session))))
      (should (= 1 probe-count))))

  :doc "rechecks a cached remote target and revokes grants after replacement"
  (with-temp-buffer
    (let* ((target (niwa-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/"))
           calls)
      (setf (niwa-execution-target-readiness target) '(:status ready)
            (niwa-execution-target-incarnation target) "old-incarnation"
            (niwa-execution-target-observed-incarnation target)
            "new-incarnation"
            (niwa-execution-target-incarnation-changed-p target) t)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-target-probe)
                 (lambda (&rest _args)
                   (push 'probe calls)
                   '(:status ready)))
                ((symbol-function
                  'niwa-session-artifacts-assert-mutation-authority)
                 (lambda (checked &optional buffer)
                   (push 'lease calls)
                   (niwa-session-artifacts-check-target-incarnation
                    checked (or buffer (current-buffer)))
                   t))
                ((symbol-function
                  'niwa-permission-invalidate-target-grants)
                 (lambda (_session) (push 'invalidate calls) t))
                ((symbol-function
                  'niwa-session-artifacts-publish-sidecar-state)
                 (lambda (_session _root-buffer) (push 'publish calls) t)))
        (should (niwa-request-p (niwa-request-begin session))))
      (should (equal '(probe lease invalidate)
                     (nreverse calls)))
      (should (equal "new-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should-not
       (niwa-execution-target-observed-incarnation target))
      (should-not
       (niwa-execution-target-incarnation-changed-p target))))

  :doc "commits a replacement incarnation only with revoked exact grants"
  (let* ((local-root
          (file-name-as-directory
           (make-temp-file "niwa-incarnation-ack-" t)))
         (host "incarnation-ack")
         (remote-root
          (format "/niwamock:%s:%s" host local-root))
         (niwa-session-durability--client-id (make-string 64 ?9))
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         session
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((workspace
                  (niwa-workspace-get-or-create
                   'project remote-root remote-root "incarnation-ack"))
                 (_identity
                  (niwa-workspace-identity-ensure remote-root))
                 (grant-path (file-name-concat remote-root "granted.el"))
                 (session-id "main-incarnation-ack")
                 (session-dir
                  (file-name-as-directory
                   (file-name-concat
                    remote-root ".niwa" "sessions" session-id)))
                 (segment
                  (file-name-concat session-dir "segment-0001.chat.org")))
            (setq session
                  (niwa-session-create "main" workspace remote-root)
                  buffer
                  (generate-new-buffer " *incarnation-ack-root*"))
            (let ((target (niwa-session-execution-target session))
                  (replacement-incarnation
                   (secure-hash
                    'sha256
                    (niwa-execution-target--incarnation-payload
                     "fixture" "fixture" "1" "fixture"))))
              (setf (niwa-execution-target-incarnation target)
                    "old-incarnation")
              (setf (niwa-execution-target-support-tier target) 'supported
                    (niwa-session-sandbox-mode session) 'off
                    (niwa-session-resource-grants session)
                    (list (list :path grant-path :access 'read))
                    (niwa-session-session-id session) session-id
                    (niwa-session-save-path session) session-dir
                    (niwa-session-created-at session) "created"
                    (niwa-session-updated-at session) "updated"
                    (niwa-session-current-segment session) 1)
              (puthash
               (niwa-execution-target-identity target)
               t niwa-session-durability--disclosed-targets)
              (with-current-buffer buffer
                (org-mode)
                (setq-local niwa--session session)
                (setq buffer-file-name segment)
                (insert "Published transcript\n")
                (set-buffer-modified-p nil))
              (make-directory session-dir t)
              (should
               (niwa-session-durability-lease-acquire
                session-dir (buffer-name buffer) session))
              (should
               (niwa-session-publication-publish
                session
                (list
                 (list :path segment :content "Published transcript\n")
                 (list
                  :path
                  (file-name-concat session-dir "session.meta.el")
                  :content
                  (let ((print-length nil)
                        (print-level nil)
                        (print-circle t)
                        (print-quoted t))
                    (prin1-to-string
                     (niwa-session-codec-serialize session)))
                  :commit-marker t))))
              (cl-letf (((symbol-function 'executable-find)
                         (lambda (name &optional _remote)
                           (concat "/usr/bin/" name)))
                        ((symbol-function 'process-file)
                         (lambda (program _in destination _display
                                          &rest args)
                           (with-current-buffer destination
                             (insert
                              (cond
                               ((equal program "env")
                                (concat "HOME=" local-root "\0"))
                               ((string-suffix-p "bash" program)
                                "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                               ((equal args '("-r")) "6.8.0-target\n")
                               (t "Linux\n"))))
                           0)))
                (let ((readiness
                       (niwa-execution-target-probe target t 'off)))
                  (should (eq 'ready (plist-get readiness :status)))))
              ;; Anything persisting reentrantly after the probe still sees
              ;; the old baseline paired with the old exact grant.  The
              ;; grant sits under the target's probed HOME, so the sidecar
              ;; carries it home-abbreviated.
              (let ((pre-ack
                     (niwa-session-codec-serialize session)))
                (should
                 (equal "old-incarnation"
                        (plist-get pre-ack :target-incarnation)))
                (should
                 (equal '((:path "~/granted.el" :access read))
                        (plist-get pre-ack :resource-grants))))
              (should (equal "old-incarnation"
                             (niwa-execution-target-incarnation target)))
              (should (equal replacement-incarnation
                             (niwa-execution-target-observed-incarnation
                              target)))
              (with-current-buffer buffer
                (should (niwa-request-p
                         (niwa-request-begin session))))
              (let* ((sidecar-text
                      (niwa-session-artifacts-read-artifact
                       session "session.meta.el" t))
                     (sidecar
                      (with-temp-buffer
                        (insert sidecar-text)
                        (goto-char (point-min))
                        (read (current-buffer))))
                     (reloaded
                      (plist-get
                       (niwa-session-codec-deserialize
                        sidecar workspace)
                       :session)))
                (should
                 (equal replacement-incarnation
                        (plist-get sidecar :target-incarnation)))
                (should-not (plist-get sidecar :resource-grants))
                (should
                 (equal replacement-incarnation
                        (niwa-execution-target-incarnation
                         (niwa-session-execution-target reloaded))))
                (should-not
                 (niwa-session-resource-grants reloaded)))
              (should-not
               (niwa-execution-target-incarnation-changed-p target))
              (should-not
               (niwa-execution-target-observed-incarnation target))
              ;; A replacement after request admission is fenced again at
              ;; the next mutating-tool boundary, not deferred to a new turn.
              (setf (niwa-session-resource-grants session)
                    (list (list :path grant-path :access 'read)))
              (niwa-execution-target--record-incarnation
               target "second-incarnation")
              (with-current-buffer buffer
                (niwa-session-artifacts-assert-new-mutation-authority
                 session))
              (should-not (niwa-session-resource-grants session))
              (should (equal "second-incarnation"
                             (niwa-execution-target-incarnation target)))
              (should-not
               (niwa-execution-target-incarnation-changed-p target)))))
      (when (and session (niwa-session-save-path session))
        ;; Releasing reaches the target, so it needs the mock method that
        ;; only resolves inside this helper.
        (ignore-errors
          (niwa-test--with-local-shell-tramp (list host)
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))

  :doc "keeps a failed replacement marker pending and retryable"
  (with-temp-buffer
    (let* ((target (niwa-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/"
                     :session-id "failed-replacement"
                     :save-path "/tmp/niwa-failed-replacement/"
                     :resource-grants
                     '((:path "/docker:dev:/outside.el" :access read))))
           (fail-publication t))
      (setq-local niwa--session session)
      (setf (niwa-execution-target-readiness target) '(:status ready)
            (niwa-execution-target-incarnation target) "old-incarnation"
            (niwa-execution-target-observed-incarnation target)
            "new-incarnation"
            (niwa-execution-target-incarnation-changed-p target) t)
      (cl-letf
          (((symbol-function 'niwa-execution-target-probe) #'ignore)
           ((symbol-function
             'niwa-session-artifacts-artifact-present-p)
            (lambda (&rest _) t))
           ((symbol-function
             'niwa-session-artifacts-assert-mutation-authority)
            (lambda (checked &optional _buffer)
              (when (niwa-session-pending-publication checked)
                (user-error "Session has pending publication"))
              (niwa-session-artifacts-check-target-incarnation
               checked (current-buffer))
              t))
           ((symbol-function 'niwa-permission-invalidate-target-grants)
            (lambda (checked)
              (setf (niwa-session-resource-grants checked) nil)
              t))
           ((symbol-function
             'niwa-session-artifacts-publish-sidecar-state)
            (lambda (checked _root-buffer)
              (if fail-publication
                  (progn
                    (setf (niwa-session-pending-publication checked)
                          '(:batches (staged-incarnation-marker)))
                    (user-error "Injected publication failure"))
                t))))
        (should-error (niwa-request-begin session) :type 'user-error)
        (should
         (niwa-execution-target-incarnation-changed-p target))
        (should (equal "new-incarnation"
                       (niwa-execution-target-incarnation target)))
        (should (equal "new-incarnation"
                       (niwa-execution-target-observed-incarnation target)))
        (should-not (niwa-session-resource-grants session))
        (should (niwa-session-pending-publication session))
        (should-error (niwa-request-begin session) :type 'user-error)
        (setf (niwa-session-pending-publication session) nil
              fail-publication nil)
        (should (niwa-request-p (niwa-request-begin session)))
        (should-not
         (niwa-execution-target-incarnation-changed-p target))
        (should-not
         (niwa-execution-target-observed-incarnation target)))))

  :doc "keeps replacement unacknowledged when grant invalidation fails"
  (with-temp-buffer
    (let* ((target (niwa-execution-target-create
                    "/docker:dev:/workspace/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote"
                       :root "/docker:dev:/workspace/" :name "remote"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/docker:dev:/workspace/")))
      (setf (niwa-execution-target-readiness target) '(:status ready)
            (niwa-execution-target-incarnation target) "old-incarnation"
            (niwa-execution-target-observed-incarnation target)
            "new-incarnation"
            (niwa-execution-target-incarnation-changed-p target) t)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-target-probe)
                 #'ignore)
                ((symbol-function
                  'niwa-session-artifacts-assert-mutation-authority)
                 (lambda (checked &optional buffer)
                   (niwa-session-artifacts-check-target-incarnation
                    checked (or buffer (current-buffer)))
                   t))
                ((symbol-function
                  'niwa-permission-invalidate-target-grants)
                 (lambda (_session) (user-error "Publication failed"))))
        (should-error (niwa-request-begin session) :type 'user-error))
      (should
       (niwa-execution-target-incarnation-changed-p target))
      (should (equal "old-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should (equal "new-incarnation"
                     (niwa-execution-target-observed-incarnation target)))
      (should-not niwa--current-request)))

  :doc "blocks a remote request with one aggregated readiness error"
  (with-temp-buffer
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote"
                       :root "/ssh:user@host:/srv/project/" :name "remote"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :execution-target target
                     :working-directory "/ssh:user@host:/srv/project/")))
      (setf (niwa-execution-target-readiness target)
            `(:status blocked :reason missing-dependencies
              :sandbox-mode ,(niwa-sandbox-mode-effective session)
              :missing-dependencies (rg bash)))
      (let ((message
             (condition-case err
                 (progn (niwa-request-begin session) nil)
               (user-error (error-message-string err)))))
        (should (string-match-p "rg, bash" message))
        (should-not niwa--current-request))))

  :doc "does not impose remote readiness probing on a local request"
  (with-temp-buffer
    (let* ((root (niwa-test--placeholder-root))
           (workspace (niwa-workspace-get-or-create
                       'project root root "p1"))
           (session (niwa-session-create "main" workspace)))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-execution-target-probe)
                 (lambda (&rest _)
                   (ert-fail "local request unexpectedly probed"))))
        (should (niwa-request-p (niwa-request-begin session))))))

  :doc "invalidates local project grants, publishes the replacement, and admits later requests"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-local-incarnation-" t)))
         (workspace (niwa-workspace-get-or-create
                     'project "local-incarnation" root "local-incarnation"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *local-incarnation-root*"))
         (grant-path (file-name-concat root "granted.el"))
         save-path)
    (unwind-protect
        (progn
          (with-temp-file grant-path
            (insert "granted"))
          (setf (niwa-execution-target-incarnation
                 (niwa-session-execution-target session))
                "old-incarnation"
                (niwa-session-resource-grants session)
                (list (list :path grant-path :access 'read)))
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "Initial local request\n")
            (cl-letf (((symbol-function
                        'niwa-execution-target--local-incarnation)
                       (lambda () "old-incarnation")))
              (niwa-session-artifacts-save session buffer))
            (setq save-path (niwa-session-save-path session))
            (let ((sidecar
                   (with-temp-buffer
                     (insert
                      (niwa-session-artifacts-read-artifact
                       session "session.meta.el" t))
                     (goto-char (point-min))
                     (read (current-buffer)))))
              (should (equal "old-incarnation"
                             (plist-get sidecar :target-incarnation)))
              (should (plist-get sidecar :resource-grants)))
            (cl-letf (((symbol-function
                        'niwa-execution-target--local-incarnation)
                       (lambda () "new-incarnation")))
              (should (niwa-request-p (niwa-request-begin session)))
              (should-not (niwa-session-resource-grants session))
              (should-not
               (niwa-execution-target-incarnation-changed-p
                (niwa-session-execution-target session)))
              (let ((sidecar
                     (with-temp-buffer
                       (insert
                        (niwa-session-artifacts-read-artifact
                         session "session.meta.el" t))
                       (goto-char (point-min))
                       (read (current-buffer)))))
                (should (equal "new-incarnation"
                               (plist-get sidecar :target-incarnation)))
                (should-not (plist-get sidecar :resource-grants)))
              ;; A later request must see the acknowledged replacement as
              ;; the current baseline and remain admissible.
              (niwa-request-end)
              (should (niwa-request-p (niwa-request-begin session)))
              (niwa-request-end))))
      (when niwa--current-request
        (niwa-request-end))
      (when (and save-path (niwa-session-save-path session))
        (ignore-errors
          (niwa-session-persistence-lock-release save-path session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-request-end
  (:before-each (niwa-workspace-clear-registry)
   :after-each
   (niwa-workspace-clear-registry)
   (setq niwa--current-request nil))
  ;; These cases isolate teardown from lease admission (covered above).
  (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
             #'ignore))
    ,test)
  (test)
  :doc "clears buffer-local"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (niwa-request-begin session)
      (should niwa--current-request)
      (niwa-request-end)
      (should (null niwa--current-request))
      (should (eq 'idle
                  (niwa-session-agent-root-activity session)))))

  :doc "saves a version of each store artifact the request edited"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           saved messages)
      (cl-letf (((symbol-function 'niwa-artifact-backend-save-version)
                 (lambda (workspace id session-id)
                   (when (equal id "gone") (error "This item no longer exists"))
                   (push (list workspace id session-id) saved))))
        (niwa-request-begin session)
        (setf (niwa-request-edited-artifacts niwa--current-request)
              (list (cons ws "board") (cons ws "gone")))
        (niwa-test--with-captured-messages messages
          (niwa-request-end)))
      (should (equal (list (list ws "board" (niwa-session-session-id session))) saved))
      (should (string-match-p "no version of gone" messages))))

  :doc "a request admitted during artifact versioning keeps its running activity"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create 'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           admitted)
      (niwa-request-begin session)
      (setf (niwa-request-edited-artifacts niwa--current-request)
            (list (cons ws "board")))
      (cl-letf (((symbol-function 'niwa-artifact-backend-save-version)
                 (lambda (&rest _)
                   (setq admitted (niwa-request-begin session)))))
        (niwa-request-end))
      (should (eq admitted niwa--current-request))
      (should (eq 'running (niwa-session-agent-root-activity session)))
      (niwa-request-end)))

  :doc "offers the workspace an expiry sweep when a root turn ends"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           offered)
      (cl-letf (((symbol-function
                  'niwa-session-persistence-schedule-cleanup)
                 (lambda (workspace) (push workspace offered))))
        (niwa-request-begin session)
        (niwa-request-end))
      (should (equal (list ws) offered))))

  :doc "a teardown step that signals still cancels and clears the request"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           cancelled)
      (niwa-request-begin session)
      (cl-letf (((symbol-function 'niwa-request-cancel)
                 (lambda (&rest _) (error "Cancel failed"))))
        (should-error (niwa-request-end)))
      (should-not niwa--current-request)
      (niwa-request-begin session)
      (cl-letf (((symbol-function 'niwa-telemetry-record)
                 (lambda (&rest _) (error "Telemetry failed")))
                ((symbol-function 'niwa-request-cancel)
                 (lambda (&rest _) (setq cancelled t))))
        (should-error (niwa-request-end)))
      (should cancelled)
      (should-not niwa--current-request)
      (niwa-request-begin session)
      (cl-letf (((symbol-function
                  'niwa-session-persistence-schedule-cleanup)
                 (lambda (_workspace) (error "Sweep failed"))))
        (should-error (niwa-request-end)))
      (should-not niwa--current-request)
      (should (eq 'idle (niwa-session-agent-root-activity session)))))

  :doc "an agent turn end offers no expiry sweep"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (agent (niwa-agent--create :name "verifier"))
           (inv (niwa-agent-invocation-create agent))
           offered)
      (setf (niwa-agent-invocation-agent-id inv) "verifier--abc")
      (setf (niwa-agent-invocation-path inv) "/root/verifier")
      (setq-local niwa--agent-invocation inv)
      (cl-letf (((symbol-function
                  'niwa-session-persistence-schedule-cleanup)
                 (lambda (workspace) (push workspace offered))))
        (niwa-request-begin session)
        (niwa-request-end))
      (should-not offered)))

  :doc "holds the busy collection threshold through the settlement tail"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (gc-cons-threshold 800000)
           (niwa-gc-cons-threshold-while-busy (* 64 1024 1024))
           (niwa-gc-cons-threshold-while-typing nil)
           (noninteractive nil)
           ;; An interactive root turn end would queue an expiry sweep.
           (niwa-session-max-age-days nil)
           (niwa--gc-holds (make-hash-table :test #'eq))
           (niwa--gc-restore nil)
           (niwa--gc-percentage-restore nil)
           (niwa--gc-floor-applied nil)
           (gc-cons-percentage 0.1)
           (niwa--gc-timer nil))
      (unwind-protect
          (progn
            (niwa-request-begin session)
            (should (= (* 64 1024 1024) gc-cons-threshold))
            (niwa-request-end)
            ;; Settlement work still runs at the busy threshold ...
            (should (= (* 64 1024 1024) gc-cons-threshold))
            ;; ... until its grace period has passed.
            (let ((niwa--gc-settlement-grace -1))
              (niwa-request-begin session)
              (niwa-request-end))
            (maphash (lambda (key _) (puthash key #'ignore niwa--gc-holds))
                     niwa--gc-holds)
            (niwa--gc-maintain)
            (should (= 800000 gc-cons-threshold))
            (should-not niwa--gc-timer))
        (when (timerp niwa--gc-timer) (niwa--ui-timer-cancel niwa--gc-timer))
        ;; An interactive request also starts telemetry's lag heartbeat.
        (when (fboundp 'niwa-telemetry--lag-stop)
          (niwa-telemetry--lag-stop)))))

  :doc "drains every registered canceller on end"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (let* ((fired nil)
             (req (niwa-request-begin session)))
        (niwa-request-push-canceller req (lambda () (push 'a fired)))
        (niwa-request-push-canceller req (lambda () (push 'b fired)))
        (niwa-request-end)
        (should (equal (sort (copy-sequence fired) (lambda (a b)
                                                     (string< (symbol-name a)
                                                              (symbol-name b))))
                       '(a b)))
        (should (null niwa--current-request)))))

  :doc "tolerates canceller errors"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (let* ((survivor-fired nil)
             (req (niwa-request-begin session)))
        (niwa-request-push-canceller req (lambda () (error "Boom")))
        (niwa-request-push-canceller
         req (lambda () (setq survivor-fired t)))
        (niwa-request-end)
        (should survivor-fired)
        (should (null niwa--current-request)))))

  :doc "does not re-invoke cancellers on second end"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (let* ((count 0)
             (req (niwa-request-begin session)))
        (niwa-request-push-canceller req (lambda () (cl-incf count)))
        ;; Drain once manually, then end; canceller already fired and
        ;; the list is empty, so end must not re-fire it.
        (niwa-request-drain-cancellers req)
        (should (= count 1))
        (niwa-request-end)
        (should (= count 1))
        (should (null niwa--current-request)))))

  :doc "no-op when no active request"
  (with-temp-buffer
    (should (null niwa--current-request))
    (niwa-request-end)
    (should (null niwa--current-request))))

(niwa-deftest niwa-request-cancel ()
  ,test
  (test)
  :doc "cancels only an explicit request's scoped permission entries"
  (let* ((session (niwa-session--create))
         (request
          (niwa-request--create
           :id "request-1"
           :session session
           :origin "/root/worker"))
         swept)
    (cl-letf (((symbol-function 'niwa-permission-queue-sweep-request)
               (lambda (request-id actual-session &optional _no-render)
                 (setq swept (list request-id actual-session)))))
      (niwa-request-cancel request))
    (should
     (equal
      "request-1"
      (car swept)))
    (should (eq session (cadr swept))))
  :doc "late and nested cancellation registrations settle without reopening the request"
  (let ((request (niwa-request--create)) fired)
    (niwa-request-push-canceller
     request (lambda ()
               (should (niwa-request-cancelled-p request))
               (push 'original fired)
               (niwa-request-push-canceller
                request (lambda () (push 'nested fired)))))
    (niwa-request-cancel request)
    (niwa-request-push-canceller request (lambda () (error "Late cleanup failed")))
    (niwa-request-push-canceller request (lambda () (push 'late fired)))
    (niwa-request-cancel request)
    (should (equal '(late nested original) fired))
    (should-not (niwa-request-cancellers request)))
  :doc "drains registered cancellers without changing the ambient request"
  (let* ((ambient (niwa-request--create))
         (request (niwa-request--create))
         fired)
    (let ((niwa--current-request ambient))
      (niwa-request-push-canceller
       request (lambda () (setq fired t)))
      (cl-letf (((symbol-function 'niwa-permission-queue-sweep-request)
                 #'ignore))
        (niwa-request-cancel request))
      (should fired)
      (should (eq ambient niwa--current-request)))))

(niwa-deftest niwa-request-end/queues
  (:before-each (niwa-workspace-clear-registry)
   :after-each
   (niwa-workspace-clear-registry)
   (setq niwa--current-request nil))
  ;; Queue ownership is independent of the persistence admission guard.
  (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
             #'ignore))
    ,test)
  (test)

  :doc "request end sweeps only request-owned permissions and keeps plan approvals"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (let* ((outcomes nil)
             (request (niwa-request-begin session)))
        (setf (niwa-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id (niwa-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id "other-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
        (setf (niwa-session-pending-plan-approval session)
              (list :body "# Plan"
                          :chat-buffer (current-buffer)
                          :session session
                          :callback
                          (lambda (outcome)
                            (push (cons 'plan outcome) outcomes))))
        (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                   #'ignore))
          (niwa-request-end))
        (should (= 1 (length (niwa-session-permission-queue session))))
        (should (equal "other-request"
                       (plist-get (car (niwa-session-permission-queue session))
                                  :request-id)))
        (should (niwa-session-pending-plan-approval session))
        (should (equal '((main-permission . aborted))
                       outcomes))
        (niwa-request-end)
        (should (equal '((main-permission . aborted))
                       outcomes)))))

  :doc "request end renders a surviving permission when the swept entry was visible"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws)))
      (let* ((outcomes nil)
             (rendered nil)
             (request (niwa-request-begin session)))
        (setf (niwa-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id (niwa-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root/verifier"
                        :request-id "other-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
        (cl-letf (((symbol-function 'niwa-permission-queue--render-entry)
                   (lambda (entry)
                     (push (plist-get entry :origin) rendered))))
          (niwa-request-end))
        (should (equal '("/root/verifier") rendered))
        (should (equal '((main-permission . aborted))
                       outcomes)))))

  :doc "agent request end sweeps only that agent's permission entries"
  (with-temp-buffer
    (let* ((ws (niwa-workspace-get-or-create
                'file "/tmp/p1/" "/tmp/p1/" "p1"))
           (session (niwa-session-create "main" ws))
           (agent (niwa-agent--create :name "verifier"))
           (inv (niwa-agent-invocation-create agent))
           (outcomes nil)
           request)
      (setf (niwa-agent-invocation-agent-id inv) "verifier--abc")
      (setf (niwa-agent-invocation-path inv) "/root/verifier")
      (setq-local niwa--agent-invocation inv)
      (setq request (niwa-request-begin session))
      (setf (niwa-session-permission-queue session)
            (list (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root"
                        :request-id "root-request"
                        :callback
                        (lambda (outcome)
                          (push (cons 'main-permission outcome)
                                outcomes)))
                  (list :kind 'generic
                        :tool-name "Read"
                        :session session
                        :origin "/root/verifier"
                        :request-id (niwa-request-id request)
                        :callback
                        (lambda (outcome)
                          (push (cons 'agent-permission outcome)
                                outcomes)))))
      (niwa-request-end)
      (should (= 1 (length (niwa-session-permission-queue session))))
      (should (equal "/root"
                     (plist-get (car (niwa-session-permission-queue session))
                                :origin)))
      (should (equal '((agent-permission . aborted))
                     outcomes)))))


(provide 'test-niwa-turn-request)
;;; test-niwa-turn-request.el ends here
