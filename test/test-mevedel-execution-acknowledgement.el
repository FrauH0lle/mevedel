;;; test-mevedel-execution-acknowledgement.el --- Acknowledged mailbox delivery tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the acknowledgement contract between execution settlement and the
;; mailbox sink: exactly-once acknowledgement, retained observation and
;; identity until success, and retry after failure.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-execution-process)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-transport)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))
(require 'mevedel-session-publication-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-publication-test-helpers"))

(defun test-mevedel-execution-ack--record ()
  "Return a bare record whose owner context is the symbol `context'."
  (mevedel-execution--record-create
   :origin (mevedel-execution--origin-create :owner-context 'context)))

(mevedel-deftest mevedel-execution--deliver-mailbox-event ()
  ,test
  (test)
  :doc "a synchronous sink acknowledges once and no acknowledgement is outstanding"
  (let ((record (test-mevedel-execution-ack--record))
        (mevedel-execution-mailbox-delivery-function
         (lambda (_event context ack)
           (should (eq 'context context))
           (funcall ack t nil)
           ;; A repeated acknowledgement is ignored.
           (funcall ack nil "late")
           t))
        acks)
    (should-not
     (mevedel-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((t nil)) acks)))
  :doc "a pending sink leaves the acknowledgement outstanding until it runs"
  (let* ((record (test-mevedel-execution-ack--record))
         held acks
         (mevedel-execution-mailbox-delivery-function
          (lambda (_event _context ack) (setq held ack) 'pending)))
    (should (eq 'pending
                (mevedel-execution--deliver-mailbox-event
                 record '(:type terminal)
                 (lambda (success error) (push (list success error) acks)))))
    (should-not acks)
    (funcall held t nil)
    (funcall held nil "again")
    (should (equal '((t nil)) acks)))
  :doc "a sink receives an isolated copy of the event"
  (let ((record (test-mevedel-execution-ack--record))
        (event (list :type 'terminal :output (copy-sequence "abc")))
        (mevedel-execution-mailbox-delivery-function
         (lambda (event _context ack)
           (aset (plist-get event :output) 0 ?X)
           (funcall ack t nil)
           t)))
    (mevedel-execution--deliver-mailbox-event record event #'ignore)
    (should (equal "abc" (plist-get event :output))))
  :doc "a sink that signals acknowledges failure with its message"
  (let ((record (test-mevedel-execution-ack--record))
        (mevedel-execution-mailbox-delivery-function
         (lambda (&rest _) (error "Disk full")))
        acks)
    (mevedel-test--with-captured-diagnostics nil
      (should-not
       (mevedel-execution--deliver-mailbox-event
        record '(:type terminal)
        (lambda (success error) (push (list success error) acks)))))
    (should (equal '((nil "Disk full")) acks)))
  :doc "a sink that returns without acknowledging acknowledges failure"
  (let ((record (test-mevedel-execution-ack--record))
        (mevedel-execution-mailbox-delivery-function
         (lambda (&rest _) t))
        acks)
    (should-not
     (mevedel-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "Mailbox sink returned without acknowledging"))
                   acks)))
  :doc "a missing sink acknowledges failure"
  (let ((record (test-mevedel-execution-ack--record))
        (mevedel-execution-mailbox-delivery-function nil)
        acks)
    (should-not
     (mevedel-execution--deliver-mailbox-event
      record '(:type terminal)
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "No mailbox consumer")) acks)))
  :doc "an error in the acknowledgement continuation never reaches the sink"
  (let ((record (test-mevedel-execution-ack--record))
        (mevedel-execution-mailbox-delivery-function
         (lambda (_event _context ack)
           (funcall ack t nil)
           t))
        acks)
    (mevedel-test--with-captured-diagnostics nil
      (mevedel-execution--deliver-mailbox-event
       record '(:type terminal)
       (lambda (success error)
         (push (list success error) acks)
         (error "Continuation failed"))))
    (should (equal '((t nil)) acks))))

(mevedel-deftest mevedel-execution--deliver-independent ()
  ,test
  (test)
  :doc "keeps output, identity and the claim until success, then serves polls"
  (let* ((root (make-temp-file "mevedel-ack-complete-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         events held calls initial id record pending-error)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (event _context ack)
                 (push event calls)
                 (setq held ack)
                 'pending)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf done")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait (lambda () held))
          (setq record (mevedel-execution--owned-yielded-record
                        session "main" id))
          ;; Pending: still live work, still unclaimed by any poll.
          (should (mevedel-execution--record-delivery-pending-p record))
          (should (mevedel-execution-session-live-p session))
          (should (mevedel-execution--mailbox-pending-p record))
          (should-not (cl-find-if
                       (lambda (event) (eq (plist-get event :type) 'terminal))
                       events))
          (condition-case err
              (test-mevedel-execution--observe session id :wait-ms 0)
            (mevedel-execution-not-found (setq pending-error err)))
          (should (string-match-p "pending" (error-message-string pending-error)))
          (should (mevedel-execution--record-delivery-pending-p record))
          (should (eq 'mailbox (mevedel-execution--record-delivery-state record)))
          (should (equal "done"
                         (plist-get
                          (plist-get (car calls) :observation) :output)))
          (funcall held t nil)
          (should-not (mevedel-execution--record-delivery-pending-p record))
          (should-not (mevedel-execution-session-live-p session))
          (should-not (mevedel-execution--mailbox-pending-p record))
          (should (= 1 (length calls)))
          (should (= 1 (cl-count-if
                        (lambda (event) (eq (plist-get event :type) 'terminal))
                        events)))
          (should (equal "done" (plist-get
                                 (test-mevedel-execution--observe session id)
                                 :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a failed acknowledgement restores the record and a retry reuses its identity"
  (let* ((root (make-temp-file "mevedel-ack-retry-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         held ids initial id record)
    (unwind-protect
        (let ((mevedel-execution-mailbox-delivery-function
               (lambda (event _context ack)
                 (push (cons (plist-get (plist-get event :facts) :execution-id)
                             (plist-get (plist-get event :observation) :output))
                       ids)
                 (setq held ack)
                 'pending)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf kept")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait (lambda () held))
          (setq record (mevedel-execution--owned-yielded-record
                        session "main" id))
          (mevedel-test--with-captured-diagnostics nil
            (funcall held nil "Disk full"))
          (should-not (mevedel-execution--record-delivery-state record))
          (should-not (mevedel-execution--record-terminal-observation record))
          (should-not (mevedel-execution--record-delivery-pending-p record))
          ;; The identical record and output are delivered again.
          (setq held nil)
          (should (eq 'pending (mevedel-execution--deliver-independent record)))
          (funcall held t nil)
          (should (equal (list (cons id "kept") (cons id "kept")) ids))
          (should (equal "kept" (plist-get
                                 (test-mevedel-execution--observe session id)
                                 :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "a failed delivery leaves the output claimable by its owner"
  (let* ((root (make-temp-file "mevedel-ack-fail-poll-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         held initial id)
    (unwind-protect
        (let ((mevedel-execution-mailbox-delivery-function
               (lambda (_event _context ack) (setq held ack) 'pending)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "sleep .05; printf unread")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (test-mevedel-execution--wait (lambda () held))
          (mevedel-test--with-captured-diagnostics nil
            (funcall held nil "Disk full"))
          (should (equal "unread"
                         (plist-get
                          (test-mevedel-execution--observe session id)
                          :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "an already claimed record settles without delivering"
  (let ((record (test-mevedel-execution-ack--record))
        settled)
    (setf (mevedel-execution--record-delivery-state record) 'model)
    (should-not (mevedel-execution--deliver-independent
                 record (lambda (success) (push success settled))))
    (should (equal '(nil) settled))))

(mevedel-deftest mevedel-execution--monitor-batch-settled ()
  ,test
  (test)
  :doc "success drops only the delivered lines and keeps later arrivals"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :finished-p t
                 :origin (mevedel-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("c" "b" "a")
                 :monitor-bytes 6 :monitor-omitted 3))
        emitted)
    (cl-letf (((symbol-function 'mevedel-execution--emit-event)
               (lambda (event) (push event emitted)))
              ((symbol-function 'mevedel-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'mevedel-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0)))
      (mevedel-execution--monitor-batch-settled record '(:event) 2 1 t))
    (should-not (mevedel-execution--record-monitor-inflight record))
    (should (equal '("c") (mevedel-execution--record-monitor-lines record)))
    (should (= 2 (mevedel-execution--record-monitor-bytes record)))
    (should (= 2 (mevedel-execution--record-monitor-omitted record)))
    (should (equal '((:event)) emitted)))
  :doc "failure keeps every line, publishes nothing and re-sends nothing itself"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :origin (mevedel-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("b" "a") :monitor-bytes 4))
        emitted)
    (cl-letf (((symbol-function 'mevedel-execution--emit-event)
               (lambda (event) (push event emitted))))
      (mevedel-execution--monitor-batch-settled record '(:event) 2 0 nil))
    (should-not (mevedel-execution--record-monitor-inflight record))
    (should (equal '("b" "a") (mevedel-execution--record-monitor-lines record)))
    (should-not emitted)
    (should-not (mevedel-execution--record-monitor-timer record)))
  :doc "success rearms the flush for lines that arrived while pending"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :origin (mevedel-execution--origin-create)
                 :monitor-inflight t
                 :monitor-lines '("c" "b" "a") :monitor-bytes 6))
        armed)
    (cl-letf (((symbol-function 'mevedel-execution--emit-event) #'ignore)
              ((symbol-function 'mevedel-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'mevedel-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0))
              ((symbol-function 'mevedel-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (setq armed (list seconds function args))
                 'timer)))
      (mevedel-execution--monitor-batch-settled record '(:event) 2 0 t))
    (should (equal (list 0.2 #'mevedel-execution--monitor-flush (list record))
                   armed)))
  :doc "a waiting terminal delivery resumes once the batch settles"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :origin (mevedel-execution--origin-create)
                 :monitor-inflight t))
        resumed)
    (should (eq 'pending
                (mevedel-execution--deliver-independent record #'ignore)))
    (should (mevedel-execution--record-monitor-after record))
    (should-not (mevedel-execution--record-delivery-state record))
    (cl-letf (((symbol-function 'mevedel-execution--deliver-independent)
               (lambda (value &optional settled)
                 (push (list value settled) resumed))))
      (mevedel-execution--monitor-batch-settled record '(:event) 0 0 nil))
    (should (equal (list (list record #'ignore)) resumed))))

(mevedel-deftest mevedel-execution--monitor-flush ()
  ,test
  (test)
  :doc "only one batch is in flight; later lines wait for its acknowledgement"
  (let ((record (mevedel-execution--record-create
                 :kind 'monitor :origin (mevedel-execution--origin-create)))
        held sent)
    (cl-letf (((symbol-function 'mevedel-execution--event)
               (lambda (_ type &rest properties) (cons :type (cons type properties))))
              ((symbol-function 'mevedel-execution--deliver-mailbox-event)
               (lambda (_ event ack)
                 (push event sent)
                 (setq held ack)
                 'pending))
              ((symbol-function 'mevedel-execution--emit-event) #'ignore)
              ((symbol-function 'mevedel-execution--notify-state-change)
               #'ignore)
              ((symbol-function 'mevedel-agent-control-monitor-unread-bytes)
               (lambda (&rest _) 0))
              ((symbol-function 'mevedel-transport-run-at-time)
               (lambda (&rest _) 'timer)))
      (mevedel-execution--monitor-frame record "a\nb\n")
      (mevedel-execution--monitor-flush record)
      (should (= 1 (length sent)))
      (should (equal '("a" "b") (plist-get (car sent) :monitor-lines)))
      ;; Lines survive in the record while pending and new ones queue up.
      (should (equal '("b" "a") (mevedel-execution--record-monitor-lines record)))
      (mevedel-execution--monitor-frame record "c\n")
      (mevedel-execution--monitor-flush record)
      (should (= 1 (length sent)))
      (funcall held t nil)
      (should (equal '("c") (mevedel-execution--record-monitor-lines record)))
      (should (equal "c\n" (mevedel-execution--monitor-body record))))))

(defun test-mevedel-execution-ack--pending-record (session root output)
  "Start a managed command printing OUTPUT and hold its mailbox delivery.
Return (RECORD ID HELD-ACK-CELL) once the terminal event awaits its ack."
  (let (held)
    (setq mevedel-execution-mailbox-delivery-function
          (lambda (_event _context ack) (setq held ack) 'pending))
    (let* ((initial (test-mevedel-execution--start-managed
                     session root
                     (list "sh" "-c" (concat "sleep .05; printf " output))))
           (id (plist-get (plist-get initial :facts) :execution-id)))
      (test-mevedel-execution--wait (lambda () held))
      (list (mevedel-execution--owned-yielded-record session "main" id)
            id (lambda () held)))))

(mevedel-deftest mevedel-execution--discard-record/mailbox-pending (:quiet t)
  ,test
  (test)
  :doc "an owner stop keeps a finished pending delivery and success retires it"
  (let* ((root (make-temp-file "mevedel-ack-owner-stop-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-mailbox-delivery-function nil))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-mevedel-execution-ack--pending-record
                      session root "kept")))
          (should (= 1 (mevedel-execution-stop-owner session "main")))
          (should (eq record (gethash id (mevedel-execution--state-records
                                          (mevedel-session-execution-state
                                           session)))))
          (should (mevedel-execution--mailbox-pending-p record))
          (should (mevedel-execution--record-delivery-pending-p record))
          (funcall (funcall held) t nil)
          (should-not (mevedel-execution--record-delivery-pending-p record))
          (should (mevedel-execution--record-terminal-observation record))
          (should (equal "kept" (plist-get
                                 (test-mevedel-execution--observe session id)
                                 :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "an owner stop keeps the output unread when the delivery then fails"
  (let* ((root (make-temp-file "mevedel-ack-owner-stop-fail-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-mailbox-delivery-function nil))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-mevedel-execution-ack--pending-record
                      session root "unread")))
          (mevedel-execution-stop-owner session "main")
          (mevedel-test--with-captured-diagnostics nil
            (funcall (funcall held) nil "Disk full"))
          (should-not (mevedel-execution--record-delivery-state record))
          (should (eq record (gethash id (mevedel-execution--state-records
                                          (mevedel-session-execution-state
                                           session)))))
          (should (equal "unread" (plist-get
                                   (test-mevedel-execution--observe session id)
                                   :output))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t)))
  :doc "ending the session discards a pending delivery and its late ack is inert"
  (let* ((root (make-temp-file "mevedel-ack-session-end-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution-mailbox-delivery-function nil)
         (events nil)
         (mevedel-execution-event-functions
          (list (lambda (event) (push (plist-get event :type) events)))))
    (unwind-protect
        (pcase-let ((`(,record ,id ,held)
                     (test-mevedel-execution-ack--pending-record
                      session root "gone")))
          (setq events nil)
          (mevedel-execution-teardown-session session)
          (should-not (gethash id (mevedel-execution--state-records
                                   (mevedel-session-execution-state session))))
          (should (eq 'discarded (mevedel-execution--record-delivery-state record)))
          ;; The late acknowledgement neither resurrects nor republishes it.
          (funcall (funcall held) t nil)
          (should-not (gethash id (mevedel-execution--state-records
                                   (mevedel-session-execution-state session))))
          (should-not (mevedel-execution--record-terminal-observation record))
          (should-not (mevedel-execution--record-delivery-pending-p record))
          (should-not (memq 'terminal events)))
      (delete-directory root t))))

(defvar test-mevedel-execution-ack--remote-fiction nil
  "Non-nil until the arming check consumes its one fictional remote answer.")

(defun test-mevedel-execution-ack--call-as-remote (function &rest arguments)
  "Call FUNCTION with ARGUMENTS; its first remote-target check answers yes.
Only that single predicate answer is fictional, so the process, latch,
lease and disk the arming reaches stay real and local."
  (let ((test-mevedel-execution-ack--remote-fiction t))
    (apply function arguments)))

(defun test-mevedel-execution-ack--disk-latch (session)
  "Return the unsettled-mutation flag of SESSION's lease head on disk."
  (and (plist-get
        (mevedel-session-durability--lease-head
         (mevedel-session-durability--lease-path
          (mevedel-session-save-path session)))
        :unsettled-mutation)
       t))

(defun test-mevedel-execution-ack--hold-job (session id acknowledgements)
  "Start delivery job ID for SESSION held at lease reservation.
Return a function that releases it after a short timer.  Its acknowledgement
is pushed onto the list variable named by ACKNOWLEDGEMENTS' cell."
  (let ((reserve (symbol-function
                  'mevedel-session-durability-reserve-publication-lease-async))
        continuation)
    (cl-letf (((symbol-function
                'mevedel-session-durability-reserve-publication-lease-async)
               (lambda (owner callback)
                 (funcall reserve owner
                          (lambda (ok err)
                            (setq continuation
                                  (lambda () (funcall callback ok err))))))))
      (test-mevedel-publication-async--send
       session id (lambda (ok err)
                    (push (list id ok err) (car acknowledgements))))
      (test-mevedel-publication-async--wait (lambda () continuation)))
    (lambda () (run-at-time 0.01 nil continuation))))

(mevedel-deftest mevedel-execution--settle-mutation/publication-job (:quiet t)
  ,test
  (test)
  :doc "arming and settling a remote mutation order behind an in-flight job"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let* ((root (mevedel-workspace-root (mevedel-session-workspace session)))
            (go (file-name-concat root "go"))
            (mevedel-sandbox-mode 'off)
            (acknowledgements (list nil))
            (events nil)
            (latch (symbol-function 'mevedel-session-durability-set-unsettled-mutation))
            (arm (symbol-function 'mevedel-execution--arm-mutation))
            (mevedel-session-durability--disclosed-targets
             (make-hash-table :test #'equal))
            initial id record)
       (puthash (mevedel-execution-target-identity
                 (mevedel-session-execution-target session))
                t mevedel-session-durability--disclosed-targets)
       (cl-letf (((symbol-function 'mevedel-execution-target-remote-p)
                  (lambda (_target)
                    (prog1 test-mevedel-execution-ack--remote-fiction
                      (setq test-mevedel-execution-ack--remote-fiction nil))))
                 ((symbol-function 'mevedel-execution--arm-mutation)
                  (lambda (&rest arguments)
                    (apply #'test-mevedel-execution-ack--call-as-remote
                           arm arguments)))
                 ((symbol-function 'mevedel-session-durability-set-unsettled-mutation)
                  (lambda (target value &rest arguments)
                    (push (list 'latch value) events)
                    (prog1 (apply latch target value arguments)
                      (push (list 'latched value (length (car acknowledgements)))
                            events)))))
         ;; Arm: the mutating child's start waits for the held job.
         (let ((release (test-mevedel-execution-ack--hold-job
                         session "arm-job" acknowledgements)))
           (funcall release)
           (setq initial
                 (test-mevedel-execution--start-managed
                  session root
                  (list "sh" "-c" "while [ ! -e \"$1\" ]; do sleep 0.02; done"
                        "wait" go)
                  :owner "main")))
         (should (eq 'running (plist-get (plist-get initial :facts) :state)))
         (setq id (plist-get (plist-get initial :facts) :execution-id)
               record (mevedel-execution--owned-yielded-record session "main" id))
         (should (equal '(("arm-job" t nil)) (car acknowledgements)))
         (should (mevedel-execution--record-mutation-armed-p record))
         (should (mevedel-session-durability-unsettled-mutation-p session))
         (should (test-mevedel-execution-ack--disk-latch session))
         ;; The latch committed only after the in-flight job's acknowledgement.
         (let ((order (reverse events)))
           (should (equal '((latch t) (latched t 1)) order)))
         (should (= 1 (length (test-mevedel-publication-async--restored-mail
                               session))))
         ;; Settle: the child ends while a second job holds the gate.
         (setq events nil)
         (let ((release (test-mevedel-execution-ack--hold-job
                         session "settle-job" acknowledgements)))
           (write-region "" nil go nil 'silent)
           (funcall release)
           (test-mevedel-execution--wait
            (lambda () (mevedel-execution--record-finished-p record)))
           (test-mevedel-execution--wait
            (lambda () (not (mevedel-session-publication-busy-p session)))))
         (should (equal '(("settle-job" t nil) ("arm-job" t nil))
                        (car acknowledgements)))
         (should (equal '((latch nil) (latched nil 2)) (reverse events)))
         (should-not (mevedel-execution--record-mutation-armed-p record))
         (should-not (mevedel-execution-unknown-outcome session))
         (should-not (mevedel-session-durability-unsettled-mutation-p session))
         (should-not (test-mevedel-execution-ack--disk-latch session))
         (should (= 2 (length (test-mevedel-publication-async--restored-mail
                               session)))))))))

(provide 'test-mevedel-execution-acknowledgement)
;;; test-mevedel-execution-acknowledgement.el ends here
