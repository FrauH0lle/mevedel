;;; test-niwa-execution-acknowledgement.el --- Acknowledged mailbox delivery tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the acknowledgement contract between execution settlement and the
;; mailbox sink: exactly-once acknowledgement, retained observation and
;; identity until success, and retry after failure.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'niwa-transport)
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
(require 'niwa-session-publication-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-publication-test-helpers"))

(defun test-niwa-execution-ack--start-yielded (session root output)
  "Start a managed command for SESSION at ROOT that prints OUTPUT after yielding.
The command waits for a gate file created only after the first observation
yielded, so a delayed yield timer cannot let it finish in the foreground,
which would never produce the mailbox delivery these cases hold."
  (let ((initial (test-niwa-execution--start-managed
                  session root
                  (list "sh" "-c"
                        (concat "while [ ! -e go ]; do sleep 0.01; done; printf "
                                output)))))
    (should (plist-get (plist-get initial :facts) :execution-id))
    (write-region "" nil (file-name-concat root "go") nil 'silent)
    initial))

(defun test-niwa-execution-ack--record ()
  "Return a bare record whose owner context is the symbol `context'."
  (niwa-execution--record-create
   :origin (niwa-execution--origin-create :owner-context 'context)))

(niwa-deftest niwa-execution--deliver-mailbox-event ()
  ,test
  (test)
  :doc "a synchronous sink acknowledges once and no acknowledgement is outstanding"
  (let ((record (test-niwa-execution-ack--record))
        (niwa-execution-mailbox-delivery-function
         (lambda (_event context ack)
           (should (eq 'context context))
           (funcall ack t nil)
           ;; A repeated acknowledgement is ignored.
           (funcall ack nil "late")
           t))
        acks)
    (should-not
     (niwa-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((t nil)) acks)))
  :doc "a pending sink leaves the acknowledgement outstanding until it runs"
  (let* ((record (test-niwa-execution-ack--record))
         held acks
         (niwa-execution-mailbox-delivery-function
          (lambda (_event _context ack) (setq held ack) 'pending)))
    (should (eq 'pending
                (niwa-execution--deliver-mailbox-event
                 record '(:type terminal)
                 (lambda (success error) (push (list success error) acks)))))
    (should-not acks)
    (funcall held t nil)
    (funcall held nil "again")
    (should (equal '((t nil)) acks)))
  :doc "a sink receives an isolated copy of the event"
  (let ((record (test-niwa-execution-ack--record))
        (event (list :type 'terminal :output (copy-sequence "abc")))
        (niwa-execution-mailbox-delivery-function
         (lambda (event _context ack)
           (aset (plist-get event :output) 0 ?X)
           (funcall ack t nil)
           t)))
    (niwa-execution--deliver-mailbox-event record event #'ignore)
    (should (equal "abc" (plist-get event :output))))
  :doc "a sink that signals acknowledges failure with its message"
  (let ((record (test-niwa-execution-ack--record))
        (niwa-execution-mailbox-delivery-function
         (lambda (&rest _) (error "Disk full")))
        acks)
    (niwa-test--with-captured-diagnostics nil
      (should-not
       (niwa-execution--deliver-mailbox-event
        record '(:type terminal)
        (lambda (success error) (push (list success error) acks)))))
    (should (equal '((nil "Disk full")) acks)))
  :doc "a sink that returns without acknowledging acknowledges failure"
  (let ((record (test-niwa-execution-ack--record))
        (niwa-execution-mailbox-delivery-function
         (lambda (&rest _) t))
        acks)
    (should-not
     (niwa-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "Mailbox sink returned without acknowledging"))
                   acks)))
  :doc "a missing sink acknowledges failure"
  (let ((record (test-niwa-execution-ack--record))
        (niwa-execution-mailbox-delivery-function nil)
        acks)
    (should-not
     (niwa-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "No mailbox consumer")) acks)))
  :doc "an error in the acknowledgement continuation never reaches the sink"
  (let ((record (test-niwa-execution-ack--record))
        (niwa-execution-mailbox-delivery-function
         (lambda (_event _context ack)
           (funcall ack t nil)
           t))
        acks)
    (niwa-test--with-captured-diagnostics nil
      (niwa-execution--deliver-mailbox-event
       record '(:type terminal)
       (lambda (success error)
         (push (list success error) acks)
         (error "Continuation failed"))))
    (should (equal '((t nil)) acks))))

(niwa-deftest niwa-execution--deliver-independent ()
  ,test
  (test)
  :doc "keeps output, identity and the claim until success, then serves polls"
  (let* ((root (make-temp-file "niwa-ack-complete-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         events held calls initial id record pending-error)
    (unwind-protect
        (let ((niwa-execution-event-functions
               (list (lambda (event) (push event events))))
              (niwa-execution-mailbox-delivery-function
               (lambda (event _context ack)
                 (push event calls)
                 (setq held ack)
                 'pending)))
          (setq initial
                (test-niwa-execution-ack--start-yielded session root "done"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-niwa-execution--wait (lambda () held))
          (setq record (niwa-execution--owned-yielded-record
                        session "main" id))
          ;; Pending: still live work, still unclaimed by any poll.
          (should (niwa-execution--record-delivery-pending-p record))
          (should (niwa-execution-session-live-p session))
          (should (niwa-execution--mailbox-pending-p record))
          (should-not (cl-find-if
                       (lambda (event) (eq (plist-get event :type) 'terminal))
                       events))
          (condition-case err
              (test-niwa-execution--observe session id :wait-ms 0)
            (niwa-execution-not-found (setq pending-error err)))
          (should (string-match-p "pending" (error-message-string pending-error)))
          (should (niwa-execution--record-delivery-pending-p record))
          (should (eq 'mailbox (niwa-execution--record-delivery-state record)))
          (should (equal "done"
                         (plist-get
                          (plist-get (car calls) :observation) :output)))
          (funcall held t nil)
          (should-not (niwa-execution--record-delivery-pending-p record))
          (should-not (niwa-execution-session-live-p session))
          (should-not (niwa-execution--mailbox-pending-p record))
          (should (= 1 (length calls)))
          (should (= 1 (cl-count-if
                        (lambda (event) (eq (plist-get event :type) 'terminal))
                        events)))
          (should (equal "done" (plist-get
                                 (test-niwa-execution--observe session id)
                                 :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a failed acknowledgement restores the record and a retry reuses its identity"
  (let* ((root (make-temp-file "niwa-ack-retry-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         held ids initial id record)
    (unwind-protect
        (let ((niwa-execution-mailbox-delivery-function
               (lambda (event _context ack)
                 (push (cons (plist-get (plist-get event :facts) :execution-id)
                             (plist-get (plist-get event :observation) :output))
                       ids)
                 (setq held ack)
                 'pending)))
          (setq initial
                (test-niwa-execution-ack--start-yielded session root "kept"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-niwa-execution--wait (lambda () held))
          (setq record (niwa-execution--owned-yielded-record
                        session "main" id))
          (niwa-test--with-captured-diagnostics nil
            (funcall held nil "Disk full"))
          (should-not (niwa-execution--record-delivery-state record))
          (should-not (niwa-execution--record-terminal-observation record))
          (should-not (niwa-execution--record-delivery-pending-p record))
          ;; The identical record and output are delivered again.
          (setq held nil)
          (should (eq 'pending (niwa-execution--deliver-independent record)))
          (funcall held t nil)
          (should (equal (list (cons id "kept") (cons id "kept")) ids))
          (should (equal "kept" (plist-get
                                 (test-niwa-execution--observe session id)
                                 :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a failed delivery leaves the output claimable by its owner"
  (let* ((root (make-temp-file "niwa-ack-fail-poll-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         held initial id)
    (unwind-protect
        (let ((niwa-execution-mailbox-delivery-function
               (lambda (_event _context ack) (setq held ack) 'pending)))
          (setq initial
                (test-niwa-execution-ack--start-yielded session root "unread"))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-niwa-execution--wait (lambda () held))
          (niwa-test--with-captured-diagnostics nil
            (funcall held nil "Disk full"))
          (should (equal "unread"
                         (plist-get
                          (test-niwa-execution--observe session id)
                          :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "an already claimed record settles without delivering"
  (let ((record (test-niwa-execution-ack--record))
        settled)
    (setf (niwa-execution--record-delivery-state record) 'model)
    (should-not (niwa-execution--deliver-independent
                 record (lambda (success) (push success settled))))
    (should (equal '(nil) settled))))

(niwa-deftest niwa-execution--monitor-batch-settled ()
  ,test
  (test)
  :doc "success drops only the delivered lines and keeps later arrivals"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :finished-p t
                 :origin (niwa-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("c" "b" "a")
                 :monitor-bytes 6 :monitor-omitted 3))
        emitted)
    (cl-letf (((symbol-function 'niwa-execution--emit-event)
               (lambda (event) (push event emitted)))
              ((symbol-function 'niwa-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'niwa-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0)))
      (niwa-execution--monitor-batch-settled record '(:event) 2 1 t))
    (should-not (niwa-execution--record-monitor-inflight record))
    (should (equal '("c") (niwa-execution--record-monitor-lines record)))
    (should (= 2 (niwa-execution--record-monitor-bytes record)))
    (should (= 2 (niwa-execution--record-monitor-omitted record)))
    (should (equal '((:event)) emitted)))
  :doc "failure keeps every line, publishes nothing and re-sends nothing itself"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :origin (niwa-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("b" "a") :monitor-bytes 4))
        emitted)
    (cl-letf (((symbol-function 'niwa-execution--emit-event)
               (lambda (event) (push event emitted))))
      (niwa-execution--monitor-batch-settled record '(:event) 2 0 nil))
    (should-not (niwa-execution--record-monitor-inflight record))
    (should (equal '("b" "a") (niwa-execution--record-monitor-lines record)))
    (should-not emitted)
    (should-not (niwa-execution--record-monitor-timer record)))
  :doc "success rearms the flush for lines that arrived while pending"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :origin (niwa-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("c" "b" "a") :monitor-bytes 6))
        armed)
    (cl-letf (((symbol-function 'niwa-execution--emit-event) #'ignore)
              ((symbol-function 'niwa-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'niwa-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0))
              ((symbol-function 'niwa-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (setq armed (list seconds function args))
                 'timer)))
      (niwa-execution--monitor-batch-settled record '(:event) 2 0 t))
    (should (equal (list 0.2 #'niwa-execution--monitor-flush (list record))
                   armed)))
  :doc "a waiting terminal delivery resumes once the batch settles"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :origin (niwa-execution--origin-create)
                 :monitor-inflight t))
        resumed)
    (should (eq 'pending
                (niwa-execution--deliver-independent record #'ignore)))
    (should (niwa-execution--record-monitor-after record))
    (should-not (niwa-execution--record-delivery-state record))
    (cl-letf (((symbol-function 'niwa-execution--deliver-independent)
               (lambda (value &optional settled)
                 (push (list value settled) resumed))))
      (niwa-execution--monitor-batch-settled record '(:event) 0 0 nil))
    (should (equal (list (list record #'ignore)) resumed))))

(niwa-deftest niwa-execution--monitor-flush ()
  ,test
  (test)
  :doc "only one batch is in flight; later lines wait for its acknowledgement"
  (let ((record (niwa-execution--record-create
                 :kind 'monitor :origin (niwa-execution--origin-create)))
        held sent)
    (cl-letf (((symbol-function 'niwa-execution--event)
               (lambda (_ type &rest properties) (cons :type (cons type properties))))
              ((symbol-function 'niwa-execution--deliver-mailbox-event)
               (lambda (_ event ack)
                 (push event sent)
                 (setq held ack)
                 'pending))
              ((symbol-function 'niwa-execution--emit-event) #'ignore)
              ((symbol-function 'niwa-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'niwa-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0))
              ((symbol-function 'niwa-transport-run-at-time)
               (lambda (&rest _) 'timer)))
      (niwa-execution--monitor-frame record "a\nb\n")
      (niwa-execution--monitor-flush record)
      (should (= 1 (length sent)))
      (should (equal '("a" "b") (plist-get (car sent) :monitor-lines)))
      ;; Lines survive in the record while pending and new ones queue up.
      (should (equal '("b" "a") (niwa-execution--record-monitor-lines record)))
      (niwa-execution--monitor-frame record "c\n")
      (niwa-execution--monitor-flush record)
      (should (= 1 (length sent)))
      (funcall held t nil)
      (should (equal '("c") (niwa-execution--record-monitor-lines record)))
      (should (equal "c\n" (niwa-execution--monitor-body record))))))

(defun test-niwa-execution-ack--pending-record (session root output)
  "Start a managed command printing OUTPUT and hold its mailbox delivery.
Return (RECORD ID HELD-ACK-CELL) once the terminal event awaits its ack."
  (let (held)
    (setq niwa-execution-mailbox-delivery-function
          (lambda (_event _context ack) (setq held ack) 'pending))
    (let* ((initial (test-niwa-execution-ack--start-yielded session root output))
           (id (plist-get (plist-get initial :facts) :execution-id)))
      (test-niwa-execution--wait (lambda () held))
      (list (niwa-execution--owned-yielded-record session "main" id)
            id (lambda () held)))))

(niwa-deftest niwa-execution--discard-record/mailbox-pending (:quiet t)
  ,test
  (test)
  :doc "an owner stop keeps a finished pending delivery and success retires it"
  (let* ((root (make-temp-file "niwa-ack-owner-stop-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function nil))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-niwa-execution-ack--pending-record
                      session root "kept")))
          (should (= 1 (niwa-execution-stop-owner session "main")))
          (should (eq record (gethash id (niwa-execution--state-records
                                          (niwa-session-execution-state
                                           session)))))
          (should (niwa-execution--mailbox-pending-p record))
          (should (niwa-execution--record-delivery-pending-p record))
          (funcall (funcall held) t nil)
          (should-not (niwa-execution--record-delivery-pending-p record))
          (should (niwa-execution--record-terminal-observation record))
          (should (equal "kept" (plist-get
                                 (test-niwa-execution--observe session id)
                                 :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "an owner stop keeps the output unread when the delivery then fails"
  (let* ((root (make-temp-file "niwa-ack-owner-stop-fail-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function nil))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-niwa-execution-ack--pending-record
                      session root "unread")))
          (niwa-execution-stop-owner session "main")
          (niwa-test--with-captured-diagnostics nil
            (funcall (funcall held) nil "Disk full"))
          (should-not (niwa-execution--record-delivery-state record))
          (should (eq record (gethash id (niwa-execution--state-records
                                          (niwa-session-execution-state
                                           session)))))
          (should (equal "unread" (plist-get
                                   (test-niwa-execution--observe session id)
                                   :output))))
      (niwa-execution-teardown-session session)
      (delete-directory root t)))
  :doc "ending the session discards a pending delivery and its late ack is inert"
  (let* ((root (make-temp-file "niwa-ack-session-end-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off)
         (niwa-execution-mailbox-delivery-function nil)
         (events nil)
         (niwa-execution-event-functions
          (list (lambda (event) (push (plist-get event :type) events)))))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-niwa-execution-ack--pending-record
                      session root "gone")))
          (setq events nil)
          (niwa-execution-teardown-session session)
          (should-not (gethash id (niwa-execution--state-records
                                   (niwa-session-execution-state session))))
          (should (eq 'discarded (niwa-execution--record-delivery-state record)))
          ;; The late acknowledgement neither resurrects nor republishes it.
          (funcall (funcall held) t nil)
          (should-not (gethash id (niwa-execution--state-records
                                   (niwa-session-execution-state session))))
          (should-not (niwa-execution--record-terminal-observation record))
          (should-not (niwa-execution--record-delivery-pending-p record))
          (should-not (memq 'terminal events)))
      (delete-directory root t))))

(defvar test-niwa-execution-ack--remote-fiction nil
  "Non-nil until the arming check consumes its one fictional remote answer.")

(defun test-niwa-execution-ack--call-as-remote (function &rest arguments)
  "Call FUNCTION with ARGUMENTS; its first remote-target check answers yes.
Only that single predicate answer is fictional, so the process, latch,
lease and disk the arming reaches stay real and local."
  (let ((test-niwa-execution-ack--remote-fiction t))
    (apply function arguments)))

(defun test-niwa-execution-ack--disk-latch (session)
  "Return the unsettled-mutation flag of SESSION's lease head on disk."
  (and (plist-get
        (niwa-session-durability--lease-head
         (niwa-session-durability--lease-path
          (niwa-session-save-path session)))
        :unsettled-mutation)
       t))

(defun test-niwa-execution-ack--hold-job (session id acknowledgements)
  "Start delivery job ID for SESSION held at lease reservation.
Return a function that releases it after a short timer.  Its acknowledgement
is pushed onto the list variable named by ACKNOWLEDGEMENTS' cell."
  (let ((reserve (symbol-function
                  'niwa-session-durability-reserve-publication-lease-async))
        continuation)
    (cl-letf (((symbol-function
                'niwa-session-durability-reserve-publication-lease-async)
               (lambda (owner callback)
                 (funcall reserve owner
                          (lambda (ok err)
                            (setq continuation
                                  (lambda () (funcall callback ok err))))))))
      (test-niwa-publication-async--send
       session id (lambda (ok err)
                    (push (list id ok err) (car acknowledgements))))
      (test-niwa-publication-async--wait (lambda () continuation)))
    (lambda () (run-at-time 0.01 nil continuation))))

(niwa-deftest niwa-execution--settle-mutation/publication-job (:quiet t)
  ,test
  (test)
  :doc "arming and settling a remote mutation order behind an in-flight job"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let* ((root (niwa-workspace-root (niwa-session-workspace session)))
            (go (file-name-concat root "go"))
            (niwa-sandbox-mode 'off)
            (acknowledgements (list nil))
            (events nil)
            (latch (symbol-function 'niwa-session-durability-set-unsettled-mutation))
            (arm (symbol-function 'niwa-execution--arm-mutation))
            (niwa-session-durability--disclosed-targets
             (make-hash-table :test #'equal))
            initial id record)
       (puthash (niwa-execution-target-identity
                 (niwa-session-execution-target session))
                t niwa-session-durability--disclosed-targets)
       (cl-letf (((symbol-function 'niwa-execution-target-remote-p)
                  (lambda (_target)
                    (prog1 test-niwa-execution-ack--remote-fiction
                      (setq test-niwa-execution-ack--remote-fiction nil))))
                 ((symbol-function 'niwa-execution--arm-mutation)
                  (lambda (&rest arguments)
                    (apply #'test-niwa-execution-ack--call-as-remote
                           arm arguments)))
                 ((symbol-function 'niwa-session-durability-set-unsettled-mutation)
                  (lambda (target value &rest arguments)
                    (push (list 'latch value) events)
                    (prog1 (apply latch target value arguments)
                      (push (list 'latched value (length (car acknowledgements)))
                            events)))))
         ;; Arm: the mutating child's start waits for the held job.
         (let ((release (test-niwa-execution-ack--hold-job
                         session "arm-job" acknowledgements)))
           (funcall release)
           (setq initial
                 (test-niwa-execution--start-managed
                  session root
                  (list "sh" "-c" "while [ ! -e \"$1\" ]; do sleep 0.02; done"
                        "wait" go)
                  :owner "main")))
         (should (eq 'running (plist-get (plist-get initial :facts) :state)))
         (setq id (plist-get (plist-get initial :facts) :execution-id)
               record (niwa-execution--owned-yielded-record session "main" id))
         (should (equal '(("arm-job" t nil)) (car acknowledgements)))
         (should (niwa-execution--record-mutation-armed-p record))
         (should (niwa-session-durability-unsettled-mutation-p session))
         (should (test-niwa-execution-ack--disk-latch session))
         ;; The latch committed only after the in-flight job's acknowledgement.
         (let ((order (reverse events)))
           (should (equal '((latch t) (latched t 1)) order)))
         (should (= 1 (length (test-niwa-publication-async--restored-mail
                               session))))
         ;; Settle: the child ends while a second job holds the gate.
         (setq events nil)
         (let ((release (test-niwa-execution-ack--hold-job
                         session "settle-job" acknowledgements)))
           (write-region "" nil go nil 'silent)
           (funcall release)
           (test-niwa-execution--wait
            (lambda () (niwa-execution--record-finished-p record)))
           (test-niwa-execution--wait
            (lambda () (not (niwa-session-publication-busy-p session)))))
         (should (equal '(("settle-job" t nil) ("arm-job" t nil))
                        (car acknowledgements)))
         (should (equal '((latch nil) (latched nil 2)) (reverse events)))
         (should-not (niwa-execution--record-mutation-armed-p record))
         (should-not (niwa-execution-unknown-outcome session))
         (should-not (niwa-session-durability-unsettled-mutation-p session))
         (should-not (test-niwa-execution-ack--disk-latch session))
         (should (= 2 (length (test-niwa-publication-async--restored-mail
                               session)))))))))

(provide 'test-niwa-execution-acknowledgement)
;;; test-niwa-execution-acknowledgement.el ends here
