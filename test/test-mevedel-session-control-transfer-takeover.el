;;; test-mevedel-session-control-transfer-takeover.el -- Forced takeover tests -*- lexical-binding: t -*-

;;; Commentary:

;; Forced takeover of a live portable lease, driven through real lease records
;; written by distinct client ids in one Emacs: the confirmation, the
;; generation fence against the old holder, its demotion to a read-only
;; follower, two clients forcing at once, a remote target, and the view key.

;;; Code:

(require 'cl-lib)
(require 'mevedel-agent-control)
(require 'mevedel-agent-runtime)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-turn)
(require 'mevedel-view)
(require 'mevedel-view-control-transfer)
(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-execution-test-helpers"))

(defvar test-mevedel-takeover--answer t
  "Answer each takeover confirmation gets, or a function returning it.")

(defvar test-mevedel-takeover--prompts nil
  "Takeover confirmation prompts shown, newest first.")

(defmacro test-mevedel-takeover--as (host &rest body)
  "Run BODY as the client on HOST, one of `desktop', `laptop' or `tablet'.
Each client has its own id and display host.  Confirmations go through
`yes-or-no-p'; a `y-or-n-p' fails the test, because forcing discards the
other client's unpublished work."
  (declare (indent 1))
  `(let ((mevedel-session-durability--client-id
          (make-string 64 (pcase ,host ('desktop ?a) ('laptop ?b) (_ ?c)))))
     (cl-letf (((symbol-function 'mevedel-session-durability--client-host)
                (lambda () (symbol-name ,host)))
               ((symbol-function 'yes-or-no-p)
                (lambda (prompt)
                  (push prompt test-mevedel-takeover--prompts)
                  (if (functionp test-mevedel-takeover--answer)
                      (funcall test-mevedel-takeover--answer)
                    test-mevedel-takeover--answer)))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt) (ert-fail (list 'y-or-n-p prompt)))))
       ,@body)))

(defun test-mevedel-takeover--with-session (kind body)
  "Call BODY with a warm portable SESSION of workspace KIND and its BUFFER.
KIND is `project' or `file'.  The holder renews once as host desktop."
  (cl-letf (((symbol-function 'test-mevedel-session-persistence--make-workspace)
             (if (eq kind 'project)
                 (symbol-function
                  'test-mevedel-session-persistence--make-workspace)
               (lambda (root)
                 (prog1 (test-mevedel-session-persistence--make-file-workspace
                         root)
                   (mevedel-workspace-identity-ensure root))))))
    (test-mevedel-publication-async--fixture
     (lambda (session buffer)
       (should (eq kind (mevedel-workspace-type
                         (mevedel-session-workspace session))))
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (cl-letf (((symbol-function 'mevedel-session-durability--client-host)
                  (lambda () "desktop")))
         (should (mevedel-session-durability-lease-renew session)))
       (should (equal "desktop"
                      (plist-get (test-mevedel-takeover--head session) :host)))
       (let ((test-mevedel-takeover--answer t)
             (test-mevedel-takeover--prompts nil))
         (funcall body session buffer))))))

(defun test-mevedel-takeover--head (session)
  "Return the lease head record of SESSION's directory."
  (mevedel-session-durability--lease-head
   (mevedel-session-durability--lease-path (mevedel-session-save-path session))))

(defun test-mevedel-takeover--follower (owner)
  "Return (SESSION . BUFFER): another client's read-only copy of OWNER."
  (let ((session (mevedel-session-create (mevedel-session-name owner)
                                         (mevedel-session-workspace owner)))
        (buffer (generate-new-buffer " *mevedel-takeover-follower*")))
    (setf (mevedel-session-session-id session) (mevedel-session-session-id owner)
          (mevedel-session-save-path session) (mevedel-session-save-path owner)
          (mevedel-session-working-directory session)
          (mevedel-session-working-directory owner)
          (mevedel-session-publication session)
          (copy-sequence (mevedel-session-publication owner)))
    (with-current-buffer buffer
      (org-mode)
      (setq-local mevedel--session session)
      (setq buffer-read-only t)
      (setq-local mevedel-session--read-only-mode t))
    (cons session buffer)))

(defun test-mevedel-takeover--publish (session buffer text)
  "Append TEXT to BUFFER and publish it synchronously for SESSION."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))
    (let ((dir (mevedel-session-save-path session)))
      (test-mevedel-session-persistence--publish-generation
       session dir
       (mevedel-session-artifacts-segment-path
        dir (mevedel-session-current-segment session))
       (buffer-string)
       (1+ (mevedel-session-turn-count session))))))

(defun test-mevedel-takeover--time (seconds)
  "Return SECONDS as the prompts render a lease time."
  (format-time-string "%FT%T%z" (seconds-to-time seconds)))

(defun test-mevedel-takeover--busy-agent (owner)
  "Register a running agent turn on OWNER; return a cell set when it is cancelled.
The turn's native canceller stands in for a provider request; interruption
itself runs the real runtime path, including its publication gate."
  (let* ((cell (list nil))
         (invocation (mevedel-agent-invocation--create
                      :agent-id "busy" :path "/root/busy" :parent-session owner
                      ;; `deferred': this test owns no settlement to finish.
                      :runtime-cancel (lambda () (setcar cell t) 'deferred))))
    (setf (mevedel-session-agent-registry owner)
          (list (cons "/root/busy"
                      (mevedel-agent-record--create
                       :id "busy" :path "/root/busy" :parent-path "/root"
                       :activity 'running :invocation invocation))))
    cell))

(mevedel-deftest mevedel-session-durability-lease-acquire/force (:quiet t)
  ,test
  (test)
  :doc "forcing a live foreign lease confirms with its holder, else changes nothing"
  (dolist (kind '(project file))
    (test-mevedel-takeover--with-session
     kind
     (lambda (owner _buffer)
       (let ((dir (mevedel-session-save-path owner))
             (holder (test-mevedel-takeover--head owner))
             (successor (car (test-mevedel-takeover--follower owner))))
         (test-mevedel-takeover--as 'laptop
           ;; Control: the same call without FORCE refuses, unasked.
           (should-not (mevedel-session-durability-lease-acquire
                        dir "*laptop*" successor))
           (should-not test-mevedel-takeover--prompts)
           (setq test-mevedel-takeover--answer nil)
           (should-not (mevedel-session-durability-lease-acquire
                        dir "*laptop*" successor nil t)))
         (should (= 1 (length test-mevedel-takeover--prompts)))
         (should (equal holder (test-mevedel-takeover--head owner)))
         (should (mevedel-session-durability-lease-owned-p owner))
         (setq test-mevedel-takeover--answer t)
         (test-mevedel-takeover--as 'laptop
           (should (mevedel-session-durability-lease-acquire
                    dir "*laptop*" successor nil t))
           (should (mevedel-session-durability-lease-owned-p successor)))
         (let ((prompt (car test-mevedel-takeover--prompts))
               (head (test-mevedel-takeover--head owner)))
           (dolist (part (list "desktop" (plist-get holder :buffer) "active"
                               (test-mevedel-takeover--time
                                (plist-get holder :renewed-at))))
             (should (string-search part prompt)))
           (should-not (string-search "critical write" prompt))
           (should (= (1+ (plist-get holder :generation))
                      (plist-get head :generation)))
           (should (equal (make-string 64 ?b) (plist-get head :client-id)))
           (should (equal "laptop" (plist-get head :host)))
           (should (eq 'active (plist-get head :status)))
           ;; The claim carries the committed publication forward.
           (should (equal (plist-get holder :publication-head)
                          (plist-get head :publication-head))))))))

  :doc "a remote force prompts with the holder's target-clock renewal and claims there"
  (let* ((host "takeover-force-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-takeover-remote-" t)))
         (root (format "/mevedelmock:%s:%s" host local-root))
         (session-dir (file-name-concat root "session/"))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "remote"))
         (owner (mevedel-session-create "main" workspace))
         (successor (mevedel-session-create "main" workspace))
         (now 1000.0))
    (make-directory (file-name-concat local-root "session") t)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (dolist (session (list owner successor))
            (setf (mevedel-execution-target-incarnation
                   (mevedel-session-execution-target session))
                  "mock-incarnation"
                  (mevedel-session-save-path session) session-dir)
            (puthash (mevedel-execution-target-identity
                      (mevedel-session-execution-target session))
                     t mevedel-session-durability--disclosed-targets))
          (cl-letf (((symbol-function 'mevedel-session-durability--target-time)
                     (lambda (&optional _) now)))
            (test-mevedel-takeover--as 'desktop
              (should (mevedel-session-durability-lease-acquire
                       session-dir "*desktop*" owner)))
            (setq now 1010.0)
            (let ((test-mevedel-takeover--prompts nil))
              (test-mevedel-takeover--as 'laptop
                (should-not (mevedel-session-durability-lease-acquire
                             session-dir "*laptop*" successor))
                (should (mevedel-session-durability-lease-acquire
                         session-dir "*laptop*" successor nil t)))
              (should (= 1 (length test-mevedel-takeover--prompts)))
              (should (string-search (test-mevedel-takeover--time 1000.0)
                                     (car test-mevedel-takeover--prompts)))
              (should-not (string-search "expired"
                                         (car test-mevedel-takeover--prompts))))
            (let ((head (test-mevedel-takeover--head successor)))
              (should (equal (make-string 64 ?b) (plist-get head :client-id)))
              (should (= 1010.0 (plist-get head :renewed-at)))
              (should (file-exists-p
                       (mevedel-session-durability--generation-path
                        (mevedel-session-durability--lease-path session-dir)
                        (plist-get head :generation)))))))
      (dolist (session (list owner successor))
        (mevedel-session-durability--cancel-renewal session))
      (delete-directory local-root t))))

(mevedel-deftest mevedel-session-control-transfer-taken-over (:quiet t)
  ,test
  (test)
  :doc "the old holder is fenced, stops its work, turns read-only and follows"
  (dolist (kind '(project file))
    (test-mevedel-takeover--with-session
     kind
     (lambda (owner buffer)
       (pcase-let* ((`(,successor . ,view) (test-mevedel-takeover--follower owner))
                    (canceller-ran nil)
                    (request (mevedel-request--create
                              :id "turn" :session owner
                              :cancellers
                              (list (lambda ()
                                      (setq canceller-ran
                                            (list 'ran mevedel-session-durability--final-commit))))))
                    (agent nil)
                    (own nil)
                    (messages nil))
         (setf (mevedel-session-sandbox-mode owner) 'off)
         (test-mevedel-execution--start-managed
          owner (mevedel-session-working-directory owner) '("sleep" "30"))
         (should (mevedel-execution-session-live-p owner))
         (with-current-buffer buffer
           (setq-local mevedel--current-request request)
           (set-buffer-modified-p t))
         ;; Controls: renewal succeeds, and losing this client's own record
         ;; is not a takeover.
         (should (mevedel-session-durability-lease-renew owner))
         (setq own (mevedel-session-lease owner))
         (mevedel-session-durability--bind-lease owner own 'lost)
         (should-not (buffer-local-value 'mevedel-session--read-only-mode buffer))
         (mevedel-session-durability--bind-lease
          owner own 'owned (plist-get own :bytes))
         (should (mevedel-session-durability-lease-renew owner))
         (setq agent (test-mevedel-takeover--busy-agent owner))
         (test-mevedel-takeover--as 'laptop
           (should (mevedel-session-control-transfer-take-over successor view)))
         (should-not (buffer-local-value 'mevedel-session--read-only-mode view))
         (should (string-search "Original response"
                                (with-current-buffer view (buffer-string))))
         (mevedel-test--with-captured-messages messages
           ;; Cancellers run outside the detecting caller's transaction.
           (let ((mevedel-session-durability--final-commit t))
             (should-not (mevedel-session-durability-lease-renew owner))))
         (test-mevedel-publication-async--wait
          (lambda () (not (buffer-local-value 'mevedel--current-request buffer))))
         (test-mevedel-publication-async--wait (lambda () (car agent)))
         (should (buffer-local-value 'mevedel-session--read-only-mode buffer))
         (should (buffer-local-value 'buffer-read-only buffer))
         (should (string-search "control taken over by laptop" messages))
         (should (string-search "unpublished changes here were not kept"
                                messages))
         (should-not (mevedel-session-lease-renewal-timer owner))
         (should (mevedel-request-cancelled-p request))
         (should (equal '(ran nil) canceller-ran))
         (test-mevedel-execution--wait
          (lambda () (not (mevedel-execution-session-live-p owner))))
         (should-not (buffer-modified-p buffer))
         ;; Its later commit declines; the new owner's head stands.
         (let ((head (test-mevedel-takeover--head owner)))
           (should-error (test-mevedel-takeover--publish owner buffer "Stale\n"))
           (should (equal head (test-mevedel-takeover--head owner))))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         ;; Following: nothing new until the new owner publishes.
         (should-not (mevedel-session-control-transfer--follow-published
                      owner buffer))
         (test-mevedel-takeover--as 'laptop
           (test-mevedel-takeover--publish successor view "Laptop turn\n"))
         (should (mevedel-session-control-transfer--follow-published owner buffer))
         (should (string-search "Laptop turn"
                                (with-current-buffer buffer (buffer-string))))))))

  :doc "a contested claim that errors, then activates, is a takeover; aborting is not"
  (dolist (ending '(aborted active))
    (test-mevedel-takeover--with-session
     'project
     (lambda (owner buffer)
       (let* ((directory (mevedel-session-durability--lease-path
                          (mevedel-session-save-path owner)))
              (claim (test-mevedel-takeover--as 'laptop
                       (mevedel-session-durability--claim-next
                        directory (test-mevedel-takeover--head owner)
                        "*laptop*" 'claiming))))
         (should (eq 'claiming (plist-get claim :status)))
         ;; Controls: an unactivated claim is contested, and losing it to a
         ;; renewal error is not yet a takeover.
         (mevedel-session-durability-lease-renew owner)
         (should (eq 'contested (plist-get (mevedel-session-lease owner) :state)))
         (cl-letf (((symbol-function 'mevedel-session-durability--update-owned-lease)
                    (lambda (&rest _) (error "Injected transport failure"))))
           (should-not (mevedel-session-durability-lease-renew owner)))
         (should (eq 'lost (plist-get (mevedel-session-lease owner) :state)))
         (should-not (buffer-local-value 'mevedel-session--read-only-mode buffer))
         (test-mevedel-takeover--as 'laptop
           (mevedel-session-durability--write-generation
            directory (plist-put (copy-sequence claim) :status ending)))
         (mevedel-session-durability-lease-renew owner)
         (should (eq (eq ending 'active)
                     (buffer-local-value 'mevedel-session--read-only-mode buffer)))
         (should (eq (eq ending 'active)
                     (and (memq (plist-get (mevedel-session-control-transfer owner)
                                           :state)
                                '(taken-over following))
                          t)))))))

  :doc "a takeover during the holder's publication fences its commit"
  (test-mevedel-takeover--with-session
   'project
   (lambda (owner buffer)
     (let* ((dir (mevedel-session-save-path owner))
            (successor (car (test-mevedel-takeover--follower owner)))
            (commit (symbol-function
                     'mevedel-session-durability-commit-publication-head))
            forced before)
       ;; Control: undisturbed, the same publication commits.
       (test-mevedel-takeover--publish owner buffer "Committed turn\n")
       (setq before (plist-get (mevedel-session-publication-read dir) :head))
       (should (equal before (plist-get (test-mevedel-takeover--head owner)
                                        :publication-head)))
       (cl-letf (((symbol-function
                   'mevedel-session-durability-commit-publication-head)
                  (lambda (session head)
                    (unless forced
                      (should (eq 'publishing
                                  (plist-get (test-mevedel-takeover--head owner)
                                             :status)))
                      (setq forced
                            (test-mevedel-takeover--as 'laptop
                              (mevedel-session-durability-lease-acquire
                               dir "*laptop*" successor nil t))))
                    (funcall commit session head))))
         (mevedel-test--with-captured-diagnostics nil
           (should-error
            (test-mevedel-takeover--publish owner buffer "Fenced turn\n"))))
       (should forced)
       (let ((prompt (car test-mevedel-takeover--prompts))
             (head (test-mevedel-takeover--head owner)))
         (should (string-search "critical write may still be in flight" prompt))
         (should (string-search "publishing" prompt))
         (should (equal (make-string 64 ?b) (plist-get head :client-id)))
         (should (equal before (plist-get head :publication-head))))
       ;; Readers see the committed head, never the fenced generation.
       (let ((publication (mevedel-session-publication-read dir)))
         (should (equal before (plist-get publication :head)))))))

  :doc "a committed async publication inherited by a successor is superseded, not rolled back"
  (dolist (client '(same laptop observe-error))
    (test-mevedel-takeover--with-session
     'project
     (lambda (owner buffer)
       (pcase-let* ((`(,successor . ,view) (test-mevedel-takeover--follower owner))
                    (commit (symbol-function
                             'mevedel-session-durability-commit-publication-head-async))
                    (finish (symbol-function
                             'mevedel-session-durability-finish-publication-lease-async))
                    (failed (symbol-function 'mevedel-session-publication--job-failed))
                    (id (secure-hash 'sha256 "/root:in-flight"))
                    (rollbacks 0)
                    (observe (symbol-function
                              'mevedel-session-durability-observe-publication-head-async))
                    (observed nil)
                    (agent nil)
                    (timer (mevedel-session-lease-renewal-timer owner))
                    (acks nil)
                    (interleaved nil))
         (cl-letf (((symbol-function
                     'mevedel-session-durability-commit-publication-head-async)
                    (lambda (session head callback)
                      (funcall
                       commit session head
                       (lambda (success error)
                         ;; The head committed; a successor adopts and
                         ;; republishes it before the result arrives, which
                         ;; then looks like an uncertain transport outcome.
                         (setq interleaved
                               (condition-case err
                                   (progn
                                     (should (and success (not error)))
                                     (if (eq client 'same)
                                         (progn
                                           (mevedel-session-control-transfer-acquire
                                            successor view)
                                           (test-mevedel-takeover--publish
                                            successor view "Successor turn\n"))
                                       (test-mevedel-takeover--as 'laptop
                                         (mevedel-session-control-transfer-take-over
                                          successor view)
                                         (test-mevedel-takeover--publish
                                          successor view "Successor turn\n")))
                                     'done)
                                 (error err)))
                         ;; The old holder learns of the loss and its stop
                         ;; step runs before the outcome reaches reconcile.
                         (unless (eq client 'same)
                           (setq agent (test-mevedel-takeover--busy-agent owner))
                           (mevedel-session-durability--bind-lease
                            owner (test-mevedel-takeover--head owner) 'lost)
                           ;; Control: the session timer cannot stop through
                           ;; a busy publication, even with no view open.
                           (apply (timer--function timer) (timer--args timer))
                           (should-not (car agent)))
                         (run-at-time
                          0.05 nil callback nil
                          '(error "Uncertain transport outcome"))))))
                   ((symbol-function
                     'mevedel-session-durability-finish-publication-lease-async)
                    (lambda (&rest args) (cl-incf rollbacks) (apply finish args)))
                   ((symbol-function 'mevedel-session-publication--job-failed)
                    (lambda (&rest args) (cl-incf rollbacks) (apply failed args)))
                   ;; The holder already bound the foreign lease; then the
                   ;; reconcile read itself fails.
                   ((symbol-function
                     'mevedel-session-durability-observe-publication-head-async)
                    (lambda (session callback)
                      (if (or observed (not (eq client 'observe-error)))
                          (funcall observe session callback)
                        (setq observed t)
                        (mevedel-session-durability--bind-lease
                         session (test-mevedel-takeover--head session) 'lost)
                        (funcall callback nil '(error "Observation failed"))))))
           (with-current-buffer buffer
             (setq-local mevedel-session--checkpoint-pending 'in-flight))
           (should (eq 'pending (test-mevedel-publication-async--send
                                 owner "in-flight"
                                 (lambda (ok error) (push (list ok error) acks)))))
           (test-mevedel-publication-async--wait
            (lambda ()
              (let ((job (mevedel-session-publication-job owner)))
                (or (null job)
                    (eq 'unresolved
                        (mevedel-session-publication--job-phase job)))))))
         (should (eq 'done interleaved))
         ;; The successor's head holds the in-flight mail exactly once.
         (should (= 1 (cl-count id (test-mevedel-publication-async--ids
                                    (test-mevedel-publication-async--restored-mail
                                     owner))
                                :test #'equal)))
         (should-not acks)
         (should (= 0 rollbacks))
         (when (eq client 'observe-error)
           ;; The session's timer, without a view poll, settles supersession.
           (should observed)
           (should (eq 'unresolved (mevedel-session-publication--job-phase
                                    (mevedel-session-publication-job owner))))
           (should (memq timer timer-list))
           (apply (timer--function timer) (timer--args timer))
           (test-mevedel-publication-async--wait
            (lambda () (not (mevedel-session-publication-job owner)))))
         (if (eq client 'same)
             ;; Control: a newer head from this same client proves nothing
             ;; about the mail, so the job stays unresolved and active.
             (progn
               (should (mevedel-session-publication-job owner))
               (should (mevedel-session-publication-active-p owner))
               (should (eq 'in-flight (buffer-local-value
                                       'mevedel-session--checkpoint-pending buffer))))
           (should-not (mevedel-session-publication-job owner))
           (should-not (mevedel-session-publication-active-p owner))
           (should (buffer-local-value 'mevedel-session--read-only-mode buffer))
           (should-not (mevedel-session-pending-publication owner))
           ;; No view has polled: the retained heartbeat stops the agent.
           (test-mevedel-publication-async--wait
            (lambda ()
              (or (car agent)
                  (progn
                    (should (memq timer timer-list))
                    (apply (timer--function timer) (timer--args timer))
                    (car agent)))))
           (should-not (mevedel-session-lease-renewal-timer owner))
           ;; A poll reports only the tick that advanced, so latch it; the
           ;; settling ticks above may already have advanced.
           (let (advanced)
             (test-mevedel-publication-async--wait
              (lambda ()
                (or advanced
                    (string-search "Successor turn"
                                   (with-current-buffer buffer (buffer-string)))
                    (setq advanced (mevedel-session-control-transfer-poll
                                    owner buffer t))))))
           (should-not (buffer-local-value 'mevedel-session--checkpoint-pending
                                           buffer))
           (should (string-search "Successor turn"
                                  (with-current-buffer buffer
                                    (buffer-string)))))))))

  :doc "a pending stop respects adoption inhibition, renewed ownership and root death"
  (test-mevedel-takeover--with-session
   'project
   (lambda (owner buffer)
     (let* ((successor (car (test-mevedel-takeover--follower owner)))
            (instructions (symbol-function 'mevedel-session-artifacts-load-instructions)))
       (test-mevedel-takeover--as 'laptop
         (mevedel-session-durability-lease-acquire
          (mevedel-session-save-path owner) "*laptop*" successor nil t))
       (setf (mevedel-session-publication-active-p owner) t)
       (mevedel-session-durability--bind-lease
        owner (test-mevedel-takeover--head owner) 'lost)
       (setf (mevedel-session-publication-active-p owner) nil)
       (let ((mevedel-session-durability--heartbeat-inhibit t))
         (mevedel-session-durability-heartbeat owner))
       (should (eq 'taken-over (plist-get (mevedel-session-control-transfer owner) :state)))
       (should (mevedel-session-lease-renewal-timer owner))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                 ((symbol-function 'mevedel-session-artifacts-load-instructions)
                  (lambda (session)
                    (let ((mevedel-session-durability--heartbeat-inhibit nil))
                      (mevedel-session-durability-heartbeat owner))
                    (mevedel-session-control-transfer-taken-over
                     owner (mevedel-session-lease owner))
                    (funcall instructions session))))
         (mevedel-session-control-transfer-take-over owner buffer))
       (should (mevedel-session-durability-lease-owned-p owner))
       (should (mevedel-session-lease-renewal-timer owner))
       (test-mevedel-takeover--as 'laptop
         (mevedel-session-durability-lease-acquire
          (mevedel-session-save-path owner) "*laptop*" successor nil t))
       (setf (mevedel-session-publication-active-p owner) t)
       (mevedel-session-durability--bind-lease
        owner (test-mevedel-takeover--head owner) 'lost)
       (should (mevedel-session-lease-renewal-timer owner))
       (kill-buffer buffer)
       (setf (mevedel-session-publication-active-p owner) nil)
       (mevedel-session-durability-heartbeat owner)
       (should-not (mevedel-session-lease-renewal-timer owner))))))

(mevedel-deftest mevedel-session-control-transfer-take-over (:quiet t)
  ,test
  (test)
  :doc "two clients forcing over the same holder elect exactly one"
  (test-mevedel-takeover--with-session
   'project
   (lambda (owner _buffer)
     (pcase-let* ((holder (test-mevedel-takeover--head owner))
                  (`(,laptop . ,laptop-view) (test-mevedel-takeover--follower owner))
                  (`(,tablet . ,tablet-view) (test-mevedel-takeover--follower owner))
                  (tablet-won nil)
                  (test-mevedel-takeover--answer
                   (lambda ()
                     ;; Tablet confirms and claims while laptop's prompt is open.
                     (let ((test-mevedel-takeover--answer t))
                       (test-mevedel-takeover--as 'tablet
                         (setq tablet-won
                               (mevedel-session-control-transfer-take-over
                                tablet tablet-view))))
                     t))
                  (failure nil))
       (test-mevedel-takeover--as 'laptop
         (setq failure
               (should-error (mevedel-session-control-transfer-take-over
                              laptop laptop-view)
                             :type 'user-error)))
       (should tablet-won)
       ;; `user-error' curls the apostrophe.
       (should (string-search "took this session" (cadr failure)))
       ;; Both confirmations named the same predecessor.
       (should (= 2 (length test-mevedel-takeover--prompts)))
       (dolist (prompt test-mevedel-takeover--prompts)
         (should (string-search "desktop" prompt)))
       (let ((head (test-mevedel-takeover--head owner)))
         (should (equal (make-string 64 ?c) (plist-get head :client-id)))
         (should (= (1+ (plist-get holder :generation))
                    (plist-get head :generation))))
       (should (buffer-local-value 'mevedel-session--read-only-mode laptop-view))
       ;; The loser is an ordinary follower: polling keeps it foreign, not
       ;; a taken-over owner.
       (test-mevedel-takeover--as 'laptop
         (mevedel-session-control-transfer-poll laptop laptop-view t))
       (should (eq 'foreign (plist-get (mevedel-session-lease laptop) :state)))
       (should-not (eq 'taken-over (plist-get (mevedel-session-control-transfer
                                               laptop)
                                              :state)))
       (should-not (buffer-local-value 'mevedel-session--read-only-mode
                                       tablet-view))))))

(mevedel-deftest mevedel-view-control-transfer-take-over (:quiet t)
  ,test
  (test)
  :doc "the follower's b key forces takeover; declining keeps it read-only"
  (should (equal "take over"
                 (cdr (assoc "b" (plist-get
                                  (mevedel-session-control-transfer-descriptor
                                   (mevedel-session--create :name "follower") t)
                                  :keys)))))
  (should-not (lookup-key mevedel-view--control-transfer-map (kbd "b")))
  (should-not (lookup-key mevedel-view--control-transfer-status-map (kbd "b")))
  (test-mevedel-takeover--with-session
   'file
   (lambda (owner buffer)
     (pcase-let ((`(,successor . ,data) (test-mevedel-takeover--follower owner))
                 (view (generate-new-buffer " *mevedel-takeover-view*"))
                 (owner-view (generate-new-buffer " *mevedel-takeover-owner-view*"))
                 (command (lookup-key mevedel-view--control-transfer-request-map
                                      (kbd "b"))))
       (with-current-buffer owner-view
         (setq-local mevedel--data-buffer buffer
                     mevedel-view--control-transfer-rebuild-function #'ignore))
       (with-current-buffer view
         (setq-local mevedel--data-buffer data
                     mevedel-view--control-transfer-rebuild-function #'ignore)
         ;; A follower waiting on a frozen owner can still force; the
         ;; owner's own quiescing row offers no takeover.
         (dolist (state '(requested quiescing))
           (mevedel-session-set-control-transfer successor (list :state state))
           (should (eq command
                       (lookup-key (plist-get
                                    (mevedel-view-control-transfer-current-descriptor)
                                    :keymap)
                                   (kbd "b")))))
         (with-current-buffer data (setq-local mevedel-session--read-only-mode nil))
         (should-not (lookup-key (plist-get
                                  (mevedel-view-control-transfer-current-descriptor)
                                  :keymap)
                                 (kbd "b")))
         (with-current-buffer data (setq-local mevedel-session--read-only-mode t))
         (mevedel-session-set-control-transfer successor nil)
         ;; The bare view has no rendered layout to redraw.
         (cl-letf (((symbol-function 'mevedel-view-rerender) #'ignore))
           ;; Control: the holder's own view offers no takeover.
           (mevedel-view--control-transfer-refresh owner-view)
           (should-not (assoc "b" (plist-get (buffer-local-value
                                              'mevedel-view--control-transfer-descriptor
                                              owner-view)
                                             :keys)))
           (test-mevedel-takeover--as 'laptop
             (setq test-mevedel-takeover--answer nil)
             (ignore-errors (call-interactively command))
             (should (buffer-local-value 'mevedel-session--read-only-mode data)))
           (should (mevedel-session-durability-lease-owned-p owner))
           (setq test-mevedel-takeover--answer t)
           (test-mevedel-takeover--as 'laptop
             (call-interactively command)
             (should (mevedel-session-durability-lease-owned-p successor)))
           ;; Once the holder learns of the takeover, one refresh redraws its
           ;; row as a read-only follower's.
           (should-not (mevedel-session-durability-lease-renew owner))
           (mevedel-view--control-transfer-refresh owner-view)
           (should (buffer-local-value 'mevedel-session--read-only-mode buffer))
           (should (assoc "b" (plist-get (buffer-local-value
                                          'mevedel-view--control-transfer-descriptor
                                          owner-view)
                                         :keys))))
         (should (= 2 (length test-mevedel-takeover--prompts)))
         (should-not (buffer-local-value 'mevedel-session--read-only-mode
                                         data)))))))

(provide 'test-mevedel-session-control-transfer-takeover)

;;; test-mevedel-session-control-transfer-takeover.el ends here
