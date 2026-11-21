;;; test-niwa-session-control-transfer-takeover.el -- Forced takeover tests -*- lexical-binding: t -*-

;;; Commentary:

;; Forced takeover of a live portable lease, driven through real lease records
;; written by distinct client ids in one Emacs: the confirmation, the
;; generation fence against the old holder, its demotion to a read-only
;; follower, two clients forcing at once, a remote target, and the view key.

;;; Code:

(require 'cl-lib)
(require 'niwa-agent-control)
(require 'niwa-agent-runtime)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-fs)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-turn)
(require 'niwa-view)
(require 'niwa-view-control-transfer)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-execution-test-helpers"))

(defvar test-niwa-takeover--answer t
  "Answer each takeover confirmation gets, or a function returning it.")

(defvar test-niwa-takeover--prompts nil
  "Takeover confirmation prompts shown, newest first.")

(defmacro test-niwa-takeover--as (host &rest body)
  "Run BODY as the client on HOST, one of `desktop', `laptop' or `tablet'.
Each client has its own id and display host.  Confirmations go through
`yes-or-no-p'; a `y-or-n-p' fails the test, because forcing discards the
other client's unpublished work."
  (declare (indent 1))
  `(let ((niwa-session-durability--client-id
          (make-string 64 (pcase ,host ('desktop ?a) ('laptop ?b) (_ ?c)))))
     (cl-letf (((symbol-function 'niwa-session-durability--client-host)
                (lambda () (symbol-name ,host)))
               ((symbol-function 'yes-or-no-p)
                (lambda (prompt)
                  (push prompt test-niwa-takeover--prompts)
                  (if (functionp test-niwa-takeover--answer)
                      (funcall test-niwa-takeover--answer)
                    test-niwa-takeover--answer)))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt) (ert-fail (list 'y-or-n-p prompt)))))
       ,@body)))

(defun test-niwa-takeover--with-session (kind body)
  "Call BODY with a warm portable SESSION of workspace KIND and its BUFFER.
KIND is `project' or `file'.  The holder renews once as host desktop."
  (cl-letf (((symbol-function 'test-niwa-session-persistence--make-workspace)
             (if (eq kind 'project)
                 (symbol-function
                  'test-niwa-session-persistence--make-workspace)
               (lambda (root)
                 (prog1 (test-niwa-session-persistence--make-file-workspace
                         root)
                   (niwa-workspace-identity-ensure root))))))
    (test-niwa-publication-async--fixture
     (lambda (session buffer)
       (should (eq kind (niwa-workspace-type
                         (niwa-session-workspace session))))
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (cl-letf (((symbol-function 'niwa-session-durability--client-host)
                  (lambda () "desktop")))
         (should (niwa-session-durability-lease-renew session)))
       (should (equal "desktop"
                      (plist-get (test-niwa-takeover--head session) :host)))
       (let ((test-niwa-takeover--answer t)
             (test-niwa-takeover--prompts nil))
         (funcall body session buffer))))))

(defun test-niwa-takeover--head (session)
  "Return the lease head record of SESSION's directory."
  (niwa-session-durability--lease-head
   (niwa-session-durability--lease-path (niwa-session-save-path session))))

(defun test-niwa-takeover--follower (owner)
  "Return (SESSION . BUFFER): another client's read-only copy of OWNER."
  (let ((session (niwa-session-create (niwa-session-name owner)
                                         (niwa-session-workspace owner)))
        (buffer (generate-new-buffer " *niwa-takeover-follower*")))
    (setf (niwa-session-session-id session) (niwa-session-session-id owner)
          (niwa-session-save-path session) (niwa-session-save-path owner)
          (niwa-session-working-directory session)
          (niwa-session-working-directory owner)
          (niwa-session-publication session)
          (copy-sequence (niwa-session-publication owner)))
    (with-current-buffer buffer
      (org-mode)
      (setq-local niwa--session session)
      (setq buffer-read-only t)
      (setq-local niwa-session--read-only-mode t))
    (cons session buffer)))

(defun test-niwa-takeover--publish (session buffer text)
  "Append TEXT to BUFFER and publish it synchronously for SESSION."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))
    (let ((dir (niwa-session-save-path session)))
      (test-niwa-session-persistence--publish-generation
       session dir
       (niwa-session-artifacts-segment-path
        dir (niwa-session-current-segment session))
       (buffer-string)
       (1+ (niwa-session-turn-count session))))))

(defun test-niwa-takeover--time (seconds)
  "Return SECONDS as the prompts render a lease time."
  (format-time-string "%FT%T%z" (seconds-to-time seconds)))

(defun test-niwa-takeover--busy-agent (owner &optional settle)
  "Register a running agent turn on OWNER; return a cell set when it is cancelled.
The turn's native canceller stands in for a provider request; interruption
itself runs the real runtime path, including its publication gate.
With SETTLE, run the real terminal settlement callback as well."
  (let* ((cell (list nil))
         (invocation (niwa-agent-invocation--create
                      :agent-id "busy" :path "/root/busy" :parent-session owner
                      :ownership-epoch (niwa-session-ownership-epoch owner)
                      :runtime-cancel (lambda ()
                                        (setcar cell t)
                                        (unless settle 'deferred))))
         (record (niwa-agent-record--create
                  :id "busy" :path "/root/busy" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (when settle
      (setf (niwa-agent-invocation-runtime-settle-callback invocation)
            (apply-partially #'niwa-agent-control--settle owner record)))
    (setf (niwa-session-agent-registry owner)
          (list (cons "/root/busy" record)))
    cell))

(niwa-deftest niwa-session-durability-lease-acquire/force (:quiet t)
  ,test
  (test)
  :doc "forcing a live foreign lease confirms with its holder, else changes nothing"
  (dolist (kind '(project file))
    (test-niwa-takeover--with-session
     kind
     (lambda (owner _buffer)
       (let ((dir (niwa-session-save-path owner))
             (holder (test-niwa-takeover--head owner))
             (successor (car (test-niwa-takeover--follower owner))))
         (test-niwa-takeover--as 'laptop
           ;; Control: the same call without FORCE refuses, unasked.
           (should-not (niwa-session-durability-lease-acquire
                        dir "*laptop*" successor))
           (should-not test-niwa-takeover--prompts)
           (setq test-niwa-takeover--answer nil)
           (should-not (niwa-session-durability-lease-acquire
                        dir "*laptop*" successor nil t)))
         (should (= 1 (length test-niwa-takeover--prompts)))
         (should (equal holder (test-niwa-takeover--head owner)))
         (should (niwa-session-durability-lease-owned-p owner))
         (setq test-niwa-takeover--answer t)
         (test-niwa-takeover--as 'laptop
           (should (niwa-session-durability-lease-acquire
                    dir "*laptop*" successor nil t))
           (should (niwa-session-durability-lease-owned-p successor)))
         (let ((prompt (car test-niwa-takeover--prompts))
               (head (test-niwa-takeover--head owner)))
           (dolist (part (list "desktop" (plist-get holder :buffer) "active"
                               (test-niwa-takeover--time
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

  :doc "target holder labels are plain bounded text in confirmation and loss messages"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner _buffer)
     (let* ((successor (car (test-niwa-takeover--follower owner)))
            (directory (niwa-session-durability--lease-path
                        (niwa-session-save-path owner)))
            (host (propertize (concat "host\n\r\t\u202e" (make-string 100 ?H))
                              'display "SPOOF"))
            (label (propertize (concat "buffer\n" (make-string 100 ?B))
                               'face 'error))
            (record (test-niwa-takeover--head owner)) messages)
       (setq record (plist-put record :host host)
             record (plist-put record :buffer label))
       (niwa-session-durability--write-generation directory record)
       (should (get-text-property 0 'display
                                  (plist-get (test-niwa-takeover--head owner) :host)))
       (test-niwa-takeover--as 'laptop
         (should (niwa-session-durability-lease-acquire
                  (niwa-session-save-path owner) "*laptop*" successor nil t)))
       (let ((prompt (car test-niwa-takeover--prompts)))
         (should-not (string-match-p "[[:cntrl:]]" prompt))
         (should (equal-including-properties prompt (substring-no-properties prompt)))
         (should (string-search (concat "host" (make-string 76 ?H) ", buffer ") prompt))
         (should (string-search (concat "buffer" (make-string 74 ?B) ", last renewal") prompt)))
       (niwa-session-durability--write-generation
        directory (plist-put (test-niwa-takeover--head owner) :host host))
       (niwa-test--with-captured-messages messages
         (should-not (niwa-session-durability-lease-renew owner)))
       ;; The capture helper appends a newline, independently of the label.
       (setq messages (string-remove-suffix "\n" messages))
       (should-not (string-match-p "[[:cntrl:]]" messages))
       (should (equal-including-properties messages (substring-no-properties messages)))
       (should (string-search (concat "host" (make-string 76 ?H) " at ") messages)))))

  :doc "a remote force prompts with the holder's target-clock renewal and claims there"
  (let* ((host "takeover-force-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-takeover-remote-" t)))
         (root (format "/niwamock:%s:%s" host local-root))
         (session-dir (file-name-concat root "session/"))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "remote"))
         (owner (niwa-session-create "main" workspace))
         (successor (niwa-session-create "main" workspace))
         (now 1000.0))
    (make-directory (file-name-concat local-root "session") t)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (dolist (session (list owner successor))
            (setf (niwa-execution-target-incarnation
                   (niwa-session-execution-target session))
                  "mock-incarnation"
                  (niwa-session-save-path session) session-dir)
            (puthash (niwa-execution-target-identity
                      (niwa-session-execution-target session))
                     t niwa-session-durability--disclosed-targets))
          (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                     (lambda (&optional _) now)))
            (test-niwa-takeover--as 'desktop
              (should (niwa-session-durability-lease-acquire
                       session-dir "*desktop*" owner)))
            (setq now 1010.0)
            (let ((test-niwa-takeover--prompts nil))
              (test-niwa-takeover--as 'laptop
                (should-not (niwa-session-durability-lease-acquire
                             session-dir "*laptop*" successor))
                (should (niwa-session-durability-lease-acquire
                         session-dir "*laptop*" successor nil t)))
              (should (= 1 (length test-niwa-takeover--prompts)))
              (should (string-search (test-niwa-takeover--time 1000.0)
                                     (car test-niwa-takeover--prompts)))
              (should-not (string-search "expired"
                                         (car test-niwa-takeover--prompts))))
            (let ((head (test-niwa-takeover--head successor)))
              (should (equal (make-string 64 ?b) (plist-get head :client-id)))
              (should (= 1010.0 (plist-get head :renewed-at)))
              (should (file-exists-p
                       (niwa-session-durability--generation-path
                        (niwa-session-durability--lease-path session-dir)
                        (plist-get head :generation)))))))
      (dolist (session (list owner successor))
        (niwa-session-durability--cancel-renewal session))
      (delete-directory local-root t))))

(niwa-deftest niwa-session-control-transfer-taken-over (:quiet t)
  ,test
  (test)
  :doc "the old holder is fenced, stops its work, turns read-only and follows"
  (dolist (kind '(project file))
    (test-niwa-takeover--with-session
     kind
     (lambda (owner buffer)
       (pcase-let* ((`(,successor . ,view) (test-niwa-takeover--follower owner))
                    (canceller-ran nil)
                    (request (niwa-request--create
                              :id "turn" :session owner
                              :cancellers
                              (list (lambda ()
                                      (setq canceller-ran
                                            (list 'ran niwa-session-durability--final-commit))))))
                    (agent nil)
                    (own nil)
                    (messages nil))
         (setf (niwa-session-sandbox-mode owner) 'off)
         (test-niwa-execution--start-managed
          owner (niwa-session-working-directory owner) '("sleep" "30"))
         (should (niwa-execution-session-live-p owner))
         (with-current-buffer buffer
           (setq-local niwa--current-request request)
           (set-buffer-modified-p t))
         ;; Controls: renewal succeeds, and losing this client's own record
         ;; is not a takeover.
         (should (niwa-session-durability-lease-renew owner))
         (setq own (niwa-session-lease owner))
         (niwa-session-durability--bind-lease owner own 'lost)
         (should-not (buffer-local-value 'niwa-session--read-only-mode buffer))
         (niwa-session-durability--bind-lease
          owner own 'owned (plist-get own :bytes))
         (should (niwa-session-durability-lease-renew owner))
         (setq agent (test-niwa-takeover--busy-agent owner t))
         (test-niwa-takeover--as 'laptop
           (should (niwa-session-control-transfer-take-over successor view)))
         (should-not (buffer-local-value 'niwa-session--read-only-mode view))
         (should (string-search "Original response"
                                (with-current-buffer view (buffer-string))))
         (niwa-test--with-captured-messages messages
           ;; Cancellers run outside the detecting caller's transaction.
           (let ((niwa-session-durability--final-commit t))
             (should-not (niwa-session-durability-lease-renew owner))))
         (test-niwa-publication-async--wait
          (lambda () (not (buffer-local-value 'niwa--current-request buffer))))
         (test-niwa-publication-async--wait (lambda () (car agent)))
         (should (buffer-local-value 'niwa-session--read-only-mode buffer))
         (should (buffer-local-value 'buffer-read-only buffer))
         (should (string-search "control taken over by laptop" messages))
         (should (string-search "unpublished changes here were not kept"
                                messages))
         (should-not (niwa-session-lease-renewal-timer owner))
         (should (niwa-request-cancelled-p request))
         (should (equal '(ran nil) canceller-ran))
         (test-niwa-execution--wait
          (lambda () (not (niwa-execution-session-live-p owner))))
         (should-not (buffer-modified-p buffer))
         ;; Its later commit declines; the new owner's head stands.
         (let ((head (test-niwa-takeover--head owner)))
           (should-error (test-niwa-takeover--publish owner buffer "Stale\n"))
           (should (equal head (test-niwa-takeover--head owner))))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         ;; Following: nothing new until the new owner publishes.
         (should-not (niwa-session-control-transfer--follow-published
                      owner buffer))
         (test-niwa-takeover--as 'laptop
           (test-niwa-takeover--publish successor view "Laptop turn\n"))
         (should (niwa-session-control-transfer--follow-published owner buffer))
         (should (string-search "Laptop turn"
                                (with-current-buffer buffer (buffer-string))))))))

  :doc "a contested claim that errors, then activates, is a takeover; aborting is not"
  (dolist (ending '(aborted active))
    (test-niwa-takeover--with-session
     'project
     (lambda (owner buffer)
       (let* ((directory (niwa-session-durability--lease-path
                          (niwa-session-save-path owner)))
              (claim (test-niwa-takeover--as 'laptop
                       (niwa-session-durability--claim-next
                        directory (test-niwa-takeover--head owner)
                        "*laptop*" 'claiming))))
         (should (eq 'claiming (plist-get claim :status)))
         ;; Controls: an unactivated claim is contested, and losing it to a
         ;; renewal error is not yet a takeover.
         (niwa-session-durability-lease-renew owner)
         (should (eq 'contested (plist-get (niwa-session-lease owner) :state)))
         (cl-letf (((symbol-function 'niwa-session-durability--update-owned-lease)
                    (lambda (&rest _) (error "Injected transport failure"))))
           (should-not (niwa-session-durability-lease-renew owner)))
         (should (eq 'lost (plist-get (niwa-session-lease owner) :state)))
         (should-not (buffer-local-value 'niwa-session--read-only-mode buffer))
         (test-niwa-takeover--as 'laptop
           (niwa-session-durability--write-generation
            directory (plist-put (copy-sequence claim) :status ending)))
         (niwa-session-durability-lease-renew owner)
         (should (eq (eq ending 'active)
                     (buffer-local-value 'niwa-session--read-only-mode buffer)))
         (should (eq (eq ending 'active)
                     (and (memq (plist-get (niwa-session-control-transfer owner)
                                           :state)
                                '(taken-over following))
                          t)))))))

  :doc "a takeover during the holder's publication fences its commit"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner buffer)
     (let* ((dir (niwa-session-save-path owner))
            (successor (car (test-niwa-takeover--follower owner)))
            (commit (symbol-function
                     'niwa-session-durability-commit-publication-head))
            forced before)
       ;; Control: undisturbed, the same publication commits.
       (test-niwa-takeover--publish owner buffer "Committed turn\n")
       (setq before (plist-get (niwa-session-publication-read dir) :head))
       (should (equal before (plist-get (test-niwa-takeover--head owner)
                                        :publication-head)))
       (cl-letf (((symbol-function
                   'niwa-session-durability-commit-publication-head)
                  (lambda (session head)
                    (unless forced
                      (should (eq 'publishing
                                  (plist-get (test-niwa-takeover--head owner)
                                             :status)))
                      (setq forced
                            (test-niwa-takeover--as 'laptop
                              (niwa-session-durability-lease-acquire
                               dir "*laptop*" successor nil t))))
                    (funcall commit session head))))
         (niwa-test--with-captured-diagnostics nil
           (should-error
            (test-niwa-takeover--publish owner buffer "Fenced turn\n"))))
       (should forced)
       (let ((prompt (car test-niwa-takeover--prompts))
             (head (test-niwa-takeover--head owner)))
         (should (string-search "critical write may still be in flight" prompt))
         (should (string-search "publishing" prompt))
         (should (equal (make-string 64 ?b) (plist-get head :client-id)))
         (should (equal before (plist-get head :publication-head))))
       ;; Readers see the committed head, never the fenced generation.
       (let ((publication (niwa-session-publication-read dir)))
         (should (equal before (plist-get publication :head)))))))

  :doc "a committed async publication inherited by a successor is superseded, not rolled back"
  (dolist (client '(same laptop observe-error))
    (test-niwa-takeover--with-session
     'project
     (lambda (owner buffer)
       (pcase-let* ((`(,successor . ,view) (test-niwa-takeover--follower owner))
                    (commit (symbol-function
                             'niwa-session-durability-commit-publication-head-async))
                    (finish (symbol-function
                             'niwa-session-durability-finish-publication-lease-async))
                    (failed (symbol-function 'niwa-session-publication--job-failed))
                    (id (secure-hash 'sha256 "/root:in-flight"))
                    (rollbacks 0)
                    (observe (symbol-function
                              'niwa-session-durability-observe-publication-head-async))
                    (observed nil)
                    (agent nil)
                    (timer (niwa-session-lease-renewal-timer owner))
                    (queued-acks nil)
                    (mail-acks nil)
                    (acks nil)
                    (interleaved nil))
         (cl-letf (((symbol-function
                     'niwa-session-durability-commit-publication-head-async)
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
                                           (niwa-session-control-transfer-acquire
                                            successor view)
                                           (test-niwa-takeover--publish
                                            successor view "Successor turn\n"))
                                       (test-niwa-takeover--as 'laptop
                                         (niwa-session-control-transfer-take-over
                                          successor view)
                                         (test-niwa-takeover--publish
                                          successor view "Successor turn\n")))
                                     'done)
                                 (error err)))
                         ;; The old holder learns of the loss and its stop
                         ;; step runs before the outcome reaches reconcile.
                         (unless (eq client 'same)
                           (setq agent (test-niwa-takeover--busy-agent owner))
                           (niwa-session-durability--bind-lease
                            owner (test-niwa-takeover--head owner) 'lost)
                           ;; Control: the session timer cannot stop through
                           ;; a busy publication, even with no view open.
                           (apply (timer--function timer) (timer--args timer))
                           (should-not (car agent)))
                         (run-at-time
                          0.05 nil callback nil
                          '(error "Uncertain transport outcome"))))))
                   ((symbol-function
                     'niwa-session-durability-finish-publication-lease-async)
                    (lambda (&rest args) (cl-incf rollbacks) (apply finish args)))
                   ((symbol-function 'niwa-session-publication--job-failed)
                    (lambda (&rest args) (cl-incf rollbacks) (apply failed args)))
                   ;; The holder already bound the foreign lease; then the
                   ;; reconcile read itself fails.
                   ((symbol-function
                     'niwa-session-durability-observe-publication-head-async)
                    (lambda (session callback)
                      (if (or observed (not (eq client 'observe-error)))
                          (funcall observe session callback)
                        (setq observed t)
                        (niwa-session-durability--bind-lease
                         session (test-niwa-takeover--head session) 'lost)
                        (funcall callback nil '(error "Observation failed"))))))
           (with-current-buffer buffer
             (setq-local niwa-session--checkpoint-pending 'in-flight))
           (should (eq 'pending (test-niwa-publication-async--send
                                 owner "in-flight"
                                 (lambda (ok error) (push (list ok error) acks)))))
           (niwa-session-publication-submit
            owner (lambda () (ert-fail "Superseded queued preparation ran"))
            (lambda (ok err) (push (list ok err) queued-acks)))
           (niwa-agent-control--batch-mail
            owner "/root" (list (list :id "queued-mail" :text "not prepared")) nil
            (lambda (ok err) (push (list ok err) mail-acks)))
           (test-niwa-publication-async--wait
            (lambda ()
              (let ((job (niwa-session-publication-job owner)))
                (or (null job)
                    (eq 'unresolved
                        (niwa-session-publication--job-phase job)))))))
         (should (eq 'done interleaved))
         ;; The successor's head holds the in-flight mail exactly once.
         (should (= 1 (cl-count id (test-niwa-publication-async--ids
                                    (test-niwa-publication-async--restored-mail
                                     owner))
                                :test #'equal)))
         (should-not acks)
         (should (= 0 rollbacks))
         (when (eq client 'observe-error)
           ;; The session's timer, without a view poll, settles supersession.
           (should observed)
           (should (eq 'unresolved (niwa-session-publication--job-phase
                                    (niwa-session-publication-job owner))))
           (should (memq timer timer-list))
           (apply (timer--function timer) (timer--args timer))
           (test-niwa-publication-async--wait
            (lambda () (not (niwa-session-publication-job owner)))))
         (if (eq client 'same)
             ;; Control: a newer head from this same client proves nothing
             ;; about the mail, so the job stays unresolved and active.
             (progn
               (should (niwa-session-publication-job owner))
               (should (niwa-session-publication-active-p owner))
               (should-not queued-acks)
               (should-not mail-acks)
               (should (niwa-session-mail-batch owner))
               (should (eq 'in-flight (buffer-local-value
                                       'niwa-session--checkpoint-pending buffer))))
           (should-not (niwa-session-publication-job owner))
           (should-not (niwa-session-publication-active-p owner))
           (should (buffer-local-value 'niwa-session--read-only-mode buffer))
           (should (= 1 (length queued-acks)))
           (should (= 1 (length mail-acks)))
           (dolist (ack (append queued-acks mail-acks))
             (should-not (car ack))
             (should (string-search "lease taken over" (error-message-string (cadr ack)))))
           (should-not (niwa-session-mail-batch owner))
           (should-not (niwa-session-pending-publication owner))
           ;; No view has polled: the retained heartbeat stops the agent.
           (test-niwa-publication-async--wait
            (lambda ()
              (or (car agent)
                  (progn
                    (should (memq timer timer-list))
                    (apply (timer--function timer) (timer--args timer))
                    (car agent)))))
           (should-not (niwa-session-lease-renewal-timer owner))
           ;; A poll reports only the tick that advanced, so latch it; the
           ;; settling ticks above may already have advanced.
           (let (advanced)
             (test-niwa-publication-async--wait
              (lambda ()
                (or advanced
                    (string-search "Successor turn"
                                   (with-current-buffer buffer (buffer-string)))
                    (setq advanced (niwa-session-control-transfer-poll
                                    owner buffer t))))))
           (should-not (buffer-local-value 'niwa-session--checkpoint-pending
                                           buffer))
           (should (string-search "Successor turn"
                                  (with-current-buffer buffer
                                    (buffer-string)))))))))

  :doc "a pending stop respects adoption inhibition, renewed ownership and root death"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner buffer)
     (let* ((successor (car (test-niwa-takeover--follower owner)))
            (instructions (symbol-function 'niwa-session-artifacts-load-instructions)))
       (test-niwa-takeover--as 'laptop
         (niwa-session-durability-lease-acquire
          (niwa-session-save-path owner) "*laptop*" successor nil t))
       (setf (niwa-session-publication-active-p owner) t)
       (niwa-session-durability--bind-lease
        owner (test-niwa-takeover--head owner) 'lost)
       (setf (niwa-session-publication-active-p owner) nil)
       (let ((niwa-session-durability--heartbeat-inhibit t))
         (niwa-session-durability-heartbeat owner))
       (should (eq 'taken-over (plist-get (niwa-session-control-transfer owner) :state)))
       (should (niwa-session-lease-renewal-timer owner))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                 ((symbol-function 'niwa-session-artifacts-load-instructions)
                  (lambda (session)
                    (let ((niwa-session-durability--heartbeat-inhibit nil))
                      (niwa-session-durability-heartbeat owner))
                    (niwa-session-control-transfer-taken-over
                     owner (niwa-session-lease owner))
                    (funcall instructions session))))
         (niwa-session-control-transfer-take-over owner buffer))
       (should (niwa-session-durability-lease-owned-p owner))
       (should (niwa-session-lease-renewal-timer owner))
       (test-niwa-takeover--as 'laptop
         (niwa-session-durability-lease-acquire
          (niwa-session-save-path owner) "*laptop*" successor nil t))
       (setf (niwa-session-publication-active-p owner) t)
       (niwa-session-durability--bind-lease
        owner (test-niwa-takeover--head owner) 'lost)
       (should (niwa-session-lease-renewal-timer owner))
       (kill-buffer buffer)
       (setf (niwa-session-publication-active-p owner) nil)
       (niwa-session-durability-heartbeat owner)
       (should-not (niwa-session-lease-renewal-timer owner))
       ;; Let the cancelled renewal's real sentinel retire its control job.
       (should (niwa-session-control-fs-wait-async 5))
       (should-not (niwa-session-lease-renewal-job owner))))))

(niwa-deftest niwa-session-control-transfer-take-over (:quiet t)
  ,test
  (test)
  :doc "two clients forcing over the same holder elect exactly one"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner _buffer)
     (pcase-let* ((holder (test-niwa-takeover--head owner))
                  (`(,laptop . ,laptop-view) (test-niwa-takeover--follower owner))
                  (`(,tablet . ,tablet-view) (test-niwa-takeover--follower owner))
                  (tablet-won nil)
                  (test-niwa-takeover--answer
                   (lambda ()
                     ;; Tablet confirms and claims while laptop's prompt is open.
                     (let ((test-niwa-takeover--answer t))
                       (test-niwa-takeover--as 'tablet
                         (setq tablet-won
                               (niwa-session-control-transfer-take-over
                                tablet tablet-view))))
                     t))
                  (failure nil))
       (test-niwa-takeover--as 'laptop
         (setq failure
               (should-error (niwa-session-control-transfer-take-over
                              laptop laptop-view)
                             :type 'user-error)))
       (should tablet-won)
       ;; `user-error' curls the apostrophe.
       (should (string-search "took this session" (cadr failure)))
       ;; Both confirmations named the same predecessor.
       (should (= 2 (length test-niwa-takeover--prompts)))
       (dolist (prompt test-niwa-takeover--prompts)
         (should (string-search "desktop" prompt)))
       (let ((head (test-niwa-takeover--head owner)))
         (should (equal (make-string 64 ?c) (plist-get head :client-id)))
         (should (= (1+ (plist-get holder :generation))
                    (plist-get head :generation))))
       (should (buffer-local-value 'niwa-session--read-only-mode laptop-view))
       ;; The loser is an ordinary follower: polling keeps it foreign, not
       ;; a taken-over owner.
       (test-niwa-takeover--as 'laptop
         (niwa-session-control-transfer-poll laptop laptop-view t))
       (should (eq 'foreign (plist-get (niwa-session-lease laptop) :state)))
       (should-not (eq 'taken-over (plist-get (niwa-session-control-transfer
                                               laptop)
                                              :state)))
       (should-not (buffer-local-value 'niwa-session--read-only-mode
                                       tablet-view))))))

(niwa-deftest niwa-view-control-transfer-take-over (:quiet t)
  ,test
  (test)
  :doc "the follower's b key forces takeover; declining keeps it read-only"
  (should (equal "take over"
                 (cdr (assoc "b" (plist-get
                                  (niwa-session-control-transfer-descriptor
                                   (niwa-session--create :name "follower") t)
                                  :keys)))))
  (should-not (lookup-key niwa-view--control-transfer-map (kbd "b")))
  (should-not (lookup-key niwa-view--control-transfer-status-map (kbd "b")))
  (test-niwa-takeover--with-session
   'file
   (lambda (owner buffer)
     (pcase-let ((`(,successor . ,data) (test-niwa-takeover--follower owner))
                 (view (generate-new-buffer " *niwa-takeover-view*"))
                 (owner-view (generate-new-buffer " *niwa-takeover-owner-view*"))
                 (command (lookup-key niwa-view--control-transfer-request-map
                                      (kbd "b"))))
       (with-current-buffer owner-view
         (setq-local niwa--data-buffer buffer
                     niwa-view--control-transfer-rebuild-function #'ignore))
       (with-current-buffer view
         (setq-local niwa--data-buffer data
                     niwa-view--control-transfer-rebuild-function #'ignore)
         ;; A follower waiting on a frozen owner can still force; the
         ;; owner's own quiescing row offers no takeover.
         (dolist (state '(requested quiescing))
           (niwa-session-set-control-transfer successor (list :state state))
           (should (eq command
                       (lookup-key (plist-get
                                    (niwa-view-control-transfer-current-descriptor)
                                    :keymap)
                                   (kbd "b")))))
         (with-current-buffer data (setq-local niwa-session--read-only-mode nil))
         (should-not (lookup-key (plist-get
                                  (niwa-view-control-transfer-current-descriptor)
                                  :keymap)
                                 (kbd "b")))
         (with-current-buffer data (setq-local niwa-session--read-only-mode t))
         (niwa-session-set-control-transfer successor nil)
         ;; The bare view has no rendered layout to redraw.
         (cl-letf (((symbol-function 'niwa-view-rerender) #'ignore))
           ;; Control: the holder's own view offers no takeover.
           (niwa-view--control-transfer-refresh owner-view)
           (should-not (assoc "b" (plist-get (buffer-local-value
                                              'niwa-view--control-transfer-descriptor
                                              owner-view)
                                             :keys)))
           (test-niwa-takeover--as 'laptop
             (setq test-niwa-takeover--answer nil)
             (ignore-errors (call-interactively command))
             (should (buffer-local-value 'niwa-session--read-only-mode data)))
           (should (niwa-session-durability-lease-owned-p owner))
           (setq test-niwa-takeover--answer t)
           (test-niwa-takeover--as 'laptop
             (call-interactively command)
             (should (niwa-session-durability-lease-owned-p successor)))
           ;; Once the holder learns of the takeover, one refresh redraws its
           ;; row as a read-only follower's.
           (should-not (niwa-session-durability-lease-renew owner))
           (niwa-view--control-transfer-refresh owner-view)
           (should (buffer-local-value 'niwa-session--read-only-mode buffer))
           (should (assoc "b" (plist-get (buffer-local-value
                                          'niwa-view--control-transfer-descriptor
                                          owner-view)
                                         :keys))))
         (should (= 2 (length test-niwa-takeover--prompts)))
         (should-not (buffer-local-value 'niwa-session--read-only-mode
                                         data)))))))

(niwa-deftest niwa-session-control-transfer-taken-over/safety (:quiet t)
  ,test
  (test)
  :doc "an agent submitting after loss cannot keep the stop gate busy"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner _buffer)
     (let ((successor (car (test-niwa-takeover--follower owner)))
           (agent (test-niwa-takeover--busy-agent owner)) acks)
       (test-niwa-takeover--as 'laptop
                                  (niwa-session-durability-lease-acquire
                                   (niwa-session-save-path owner) "*laptop*" successor nil t))
       (setf (niwa-session-publication-active-p owner) t)
       (niwa-session-durability--bind-lease
        owner (test-niwa-takeover--head owner) 'lost)
       (should-not (car agent))
       (setf (niwa-session-publication-active-p owner) nil)
       (dotimes (_ 2)
         (unless (car agent)
           (niwa-session-publication-submit
            owner (lambda () (ert-fail "Fenced agent preparation ran"))
            (lambda (ok err) (push (list ok err) acks)))
           (niwa-session-durability-heartbeat owner)))
       (should (car agent))
       (should acks)
       (should-not (car (car acks)))
       (should (string-search "lease taken over"
                              (error-message-string (cadar acks))))
       (should-not (niwa-session-publication-job owner))
       (should-not (niwa-session-publication-preparations owner)))))

  :doc "taking control back settles old children before adopting; a busy stop refuses"
  (dolist (gated '(nil t))
    (test-niwa-takeover--with-session
     'project
     (lambda (owner buffer)
       (let* ((successor (car (test-niwa-takeover--follower owner)))
              (root (niwa-session-working-directory owner))
              (pid-file (file-name-concat root "takeback-child.pid"))
              (initial (progn
                         (setf (niwa-session-sandbox-mode owner) 'off)
                         (test-niwa-execution--start-managed
                          owner root (list "sh" "-c" "echo $$ > \"$1\"; exec sleep 30"
                                           "takeback" pid-file))))
              (record (gethash (plist-get (plist-get initial :facts) :execution-id)
                               (niwa-execution--state-records
                                (niwa-session-execution-state owner)))))
         (unwind-protect
             (progn
               (test-niwa-execution--wait (lambda () (file-readable-p pid-file)))
               (let ((pid (test-niwa-execution--read-pid pid-file)))
                 (test-niwa-takeover--as 'laptop
                                            (niwa-session-durability-lease-acquire
                                             (niwa-session-save-path owner) "*laptop*" successor nil t))
                 (setf (niwa-session-publication-active-p owner) t)
                 (niwa-session-durability--bind-lease
                  owner (test-niwa-takeover--head owner) 'lost)
                 (when gated
                   (let* ((head (test-niwa-takeover--head owner))
                          (failure (should-error
                                    (niwa-session-control-transfer-take-over owner buffer)
                                    :type 'user-error)))
                     (should (string-search "Still stopping" (error-message-string failure)))
                     (should (equal head (test-niwa-takeover--head owner)))
                     (should-not (test-niwa-execution--process-gone-p pid))))
                 (setf (niwa-session-publication-active-p owner) nil)
                 (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
                   (niwa-session-control-transfer-take-over owner buffer))
                 (should (niwa-execution--record-finished-p record))
                 (should (eq 'session-ended (niwa-execution--record-termination record)))
                 (should (test-niwa-execution--process-gone-p pid))
                 (should (niwa-session-durability-lease-owned-p owner))))
           (setf (niwa-session-publication-active-p owner) nil)
           (unless (niwa-execution--record-finished-p record)
             (niwa-execution--discard-record record 'session-ended))))))))

(niwa-deftest niwa-session-publication-submit/foreign-admission (:quiet t)
  ,test
  (test)
  :doc "an admission binding the successor as foreign refuses submissions and stops"
  (dolist (takeover '(nil t))
    (test-niwa-takeover--with-session
     'project
     (lambda (owner _buffer)
       (let ((agent (and takeover (test-niwa-takeover--busy-agent owner)))
             (preparations 0) acks)
         (when takeover
           (let ((successor (car (test-niwa-takeover--follower owner))))
             (test-niwa-takeover--as 'laptop
                                        (should (niwa-session-durability-lease-acquire
                                                 (niwa-session-save-path owner) "*laptop*" successor nil t)))))
         (should (eq (not takeover)
                     (niwa-session-durability-lease-acquire
                      (niwa-session-save-path owner) "*owner*" owner)))
         (should (eq (if takeover 'foreign 'owned)
                     (plist-get (niwa-session-lease owner) :state)))
         (niwa-session-publication-submit
          owner (lambda () (cl-incf preparations) nil)
          (lambda (ok err) (push (list ok err) acks)))
         (test-niwa-publication-async--wait (lambda () acks))
         (should acks)
         (if takeover
             (progn
               (should (= 0 preparations))
               (should-not (caar acks))
               (should (string-search "lease taken over" (error-message-string (cadar acks))))
               (should (car agent))
               (should (eq 'following (plist-get (niwa-session-control-transfer owner) :state))))
           (should (= 1 preparations))
           (should (caar acks)))
         (should-not (niwa-session-publication-preparations owner))
         (should (niwa-session-control-fs-wait-async 5)))))))

(niwa-deftest niwa-session-publication-enqueue/renewal-loss (:quiet t)
  ,test
  (test)
  :doc "own expired authority keeps preparations queued until reclamation commits them"
  (test-niwa-takeover--with-session
   'project
   (lambda (owner buffer)
     (let* ((directory (niwa-session-durability--lease-path
                        (niwa-session-save-path owner)))
            (record (test-niwa-takeover--head owner))
            (generation (plist-get record :generation))
            failures acknowledgements)
       (dolist (key '(:renewed-at :expires-at))
         (plist-put record key (- (plist-get record key) 120)))
       (niwa-session-durability--write-generation directory record)
       (should (eq 'lost (niwa-session-durability--bind-latest
                          owner record directory (float-time))))
       (should-not (memq (plist-get (niwa-session-control-transfer owner) :state)
                         '(taken-over following)))
       (setf (niwa-session-publication-active-p owner) t)
       (unwind-protect
           (progn
             (niwa-session-publication-enqueue
              owner
              (lambda (token)
                (should (niwa-session-durability-lease-acquire
                         (niwa-session-save-path owner) (buffer-name buffer) owner token)))
              (lambda (err) (push err failures)))
             (niwa-agent-control--batch-mail
              owner "/root"
              (list (list :id "renewal-loss" :type 'EXECUTION :sender "/root"
                          :recipient "/root" :payload "retained" :timestamp (current-time))) nil
              (lambda (ok err) (push (list ok err) acknowledgements)))
             (should (= 2 (length (niwa-session-publication-preparations owner))))
             (should-not acknowledgements)
             (setf (niwa-session-publication-active-p owner) nil)
             (test-niwa-publication-async--wait (lambda () acknowledgements))
             (should-not failures)
             (should (equal '((t nil)) acknowledgements))
             (should (> (plist-get (test-niwa-takeover--head owner) :generation) generation))
             (should (equal '("renewal-loss")
                            (test-niwa-publication-async--ids
                             (test-niwa-publication-async--restored-mail owner)))))
         (setf (niwa-session-publication-active-p owner) nil))))))

(provide 'test-niwa-session-control-transfer-takeover)

;;; test-niwa-session-control-transfer-takeover.el ends here
