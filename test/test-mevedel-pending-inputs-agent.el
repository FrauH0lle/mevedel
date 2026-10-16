;;; test-mevedel-pending-inputs-agent.el --- Late agent report wakes -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise root settlement, generated turns and mailbox injection together.
;; Only transport scheduling, persistence, presentation and provider I/O are
;; replaced; a wake must insert a real turn and consume its real mailbox.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-chat)
(require 'mevedel-pending-inputs)
(require 'mevedel-tools)
(require 'mevedel-turn)
(require 'mevedel-view)


(defmacro mevedel-pending-agent-test--with-root (&rest body)
  "Run BODY with a root FSM, session and deterministic idle callbacks.
SENT contains actual generated request FSMs.  DRAIN runs scheduled work;
ON-SAVE can deliver a report inside the real settlement transaction."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((workspace (mevedel-workspace--create
                        :type 'file :id "late-agent-mail"
                        :root mevedel-user-dir :name "late-agent-mail"))
            (session (mevedel-session--create
                      :name "late-agent-mail" :workspace workspace
                      :root-buffer data-buf :turn-count 0
                      :agent-root-activity 'running))
            (request (mevedel-request--create
                      :id "original" :session session :origin "/root"
                      :turn 1 :started-at (current-time)))
            (fsm (gptel-make-fsm
                  :info (list :buffer data-buf :mevedel-request-id "original"
                              :backend nil :data (list :messages []))))
            (mevedel-turn-settled-functions nil)
            (mevedel-agent-control-suppress-persistence nil)
            (real-timer (symbol-function 'run-at-time))
            callbacks timers sent on-save)
       (with-current-buffer data-buf
         (setq-local mevedel--session session
                     mevedel--workspace workspace
                     mevedel--current-request request))
       (with-current-buffer view-buf
         (setq-local mevedel--session session))
       (cl-labels
           ((drain ()
              (let ((steps 0))
                (while callbacks
                  (should (< (cl-incf steps) 100))
                  (funcall (pop callbacks))))))
         (cl-letf
             (((symbol-function 'mevedel-transport-run-when-idle)
               (lambda (_key _path thunk &rest _)
                 (setq callbacks (nconc callbacks (list thunk))) t))
              ((symbol-function 'run-at-time)
               (lambda (time repeat function &rest args)
                 (if (eq function #'mevedel-pending-inputs--offer-idle-work-now)
                     (progn
                       (setq callbacks
                             (nconc callbacks
                                    (list (lambda () (apply function args)))))
                       nil)
                   (let ((timer (apply real-timer time repeat function args)))
                     (push timer timers) timer))))
              ((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest _) (when on-save (funcall on-save)) t))
              ((symbol-function 'mevedel--turn-checkpoint) #'ignore)
              ((symbol-function 'mevedel--run-turn-terminal-hook) #'ignore)
              ((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
               #'ignore)
              ((symbol-function 'mevedel-session-artifacts-assert-new-mutation-authority)
               #'ignore)
              ((symbol-function 'mevedel-session-persistence-start-checkpoints)
               #'ignore)
              ((symbol-function 'mevedel-session-persistence-schedule-cleanup)
               #'ignore)
              ((symbol-function 'mevedel-session-persistence-save-agent-state-soon)
               #'ignore)
              ((symbol-function 'mevedel--gc-hold) #'ignore)
              ((symbol-function 'mevedel-telemetry-record) #'ignore)
              ((symbol-function 'mevedel-view--begin-external-turn) #'ignore)
              ((symbol-function 'mevedel--send-request)
               (lambda (&optional _model-input)
                 (let* ((new (mevedel-request-begin session))
                        (machine
                         (gptel-make-fsm
                          :info (list :buffer data-buf :backend nil
                                      :position (copy-marker (point-max) t)
                                      :mevedel-request-id (mevedel-request-id new)
                                      :data (list :messages [])))))
                   (push machine sent)
                   (mevedel-tools--handle-message-inject machine)))))
           (unwind-protect
               (progn ,@body)
             (setq callbacks nil)
             (mevedel-agent-control-cancel-wait session "/root")
             (with-current-buffer data-buf (mevedel-request-end))
             (dolist (timer timers) (cancel-timer timer))))))))

(defun mevedel-pending-agent-test--report (session type payload)
  "Publish an ordinary agent report of TYPE with PAYLOAD to SESSION."
  (let ((record (list :type type :sender "/root/worker" :recipient "/root"
                      :outcome 'completed :payload payload)))
    (if (eq type 'RESULT)
        (mevedel-agent-control--publish-result
         session (mevedel-agent-record--create
                  :path "/root/worker" :parent-path "/root") record)
      (mevedel-agent-control--enqueue session "/root" record))
    record))

(defun mevedel-pending-agent-test--assert-delivered (session buffer sent reports)
  "Assert one real turn in BUFFER consumed REPORTS from SESSION via SENT."
  (should (= 1 (length sent)))
  (let* ((machine (car sent))
         (messages (plist-get (plist-get (gptel-fsm-info machine) :data) :messages)))
    (should (= (length reports) (length messages)))
    (cl-loop for report in reports for message across messages do
             (should (equal "user" (plist-get message :role)))
             (should (equal (mevedel-tools--message-delivery-block report)
                            (plist-get message :content))))
    ;; A second injection boundary must not deliver a report twice.
    (mevedel-tools--handle-message-inject machine)
    (should (equal messages
                   (plist-get (plist-get (gptel-fsm-info machine) :data) :messages))))
  (should-not (mevedel-agent-control-context-mailbox session))
  (should-not (mevedel-agent-control-wake-pending-p session))
  (with-current-buffer buffer
    (should (= 2 (mevedel-request-turn mevedel--current-request)))
    (should (= 1 (mevedel-view-test--count-substring
                  mevedel-pending-inputs--event-trigger (buffer-string))))
    (dolist (report reports)
      (should (= 1 (mevedel-view-test--count-substring
                    (mevedel-tools--message-delivery-block report)
                    (buffer-string)))))))


(mevedel-deftest mevedel-pending-agent-late-report ()
  (mevedel-pending-agent-test--with-root
    (let (report)
      ;; This is the last injection boundary of the original request.
      (mevedel-tools--handle-message-inject fsm)
      (pcase ',phase
        ('before
         (setq report (mevedel-pending-agent-test--report session ',type "late report"))
         (drain)
         (should-not sent))
        ('during
         (setq on-save
               (lambda ()
                 (should (mevedel-turn-busy-p data-buf))
                 (should (buffer-local-value 'mevedel--turn-settlements-pending data-buf))
                 (setq report (mevedel-pending-agent-test--report
                               session ',type "late report"))
                 (drain)
                 (should-not sent)))))
      (mevedel--complete-turn fsm)
      (drain)
      (when (eq ',phase 'after)
        (should-not (mevedel-turn-busy-p data-buf))
        (should-not sent)
        (setq report (mevedel-pending-agent-test--report session ',type "late report"))
        (drain))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (= 0 (length (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                      :messages))))
      (mevedel-pending-agent-test--assert-delivered session data-buf sent (list report))))
  (phase type)
  :doc "MAIL after the last injection wakes after successful settlement"
  before MAIL
  :doc "RESULT after the last injection wakes after successful settlement"
  before RESULT
  :doc "MAIL arriving inside publication waits for settlement"
  during MAIL
  :doc "RESULT arriving inside publication waits for settlement"
  during RESULT
  :doc "MAIL arriving after settlement starts a background turn"
  after MAIL
  :doc "RESULT arriving after settlement starts a background turn"
  after RESULT)

(mevedel-deftest mevedel-pending-agent-coalesce ()
  ,test
  (test)
  :doc "multiple late reports and duplicate completion callbacks produce one turn"
  (mevedel-pending-agent-test--with-root
    (mevedel--complete-turn fsm)
    (drain)
    (let ((reports (list (mevedel-pending-agent-test--report session 'MAIL "interim")
                         (mevedel-pending-agent-test--report session 'RESULT "terminal"))))
      (mevedel--complete-turn fsm)
      (drain)
      (mevedel-pending-agent-test--assert-delivered session data-buf sent reports))))

(mevedel-deftest mevedel-pending-agent-failed-root ()
  (mevedel-pending-agent-test--with-root
    (when ,wake
      (mevedel--complete-turn fsm)
      (drain)
      (let ((report (mevedel-pending-agent-test--report session 'RESULT "start wake")))
        (drain)
        (mevedel-pending-agent-test--assert-delivered session data-buf sent (list report)))
      (setq fsm (car sent)))
    (mevedel-tools--handle-message-inject fsm)
    (let ((report (mevedel-pending-agent-test--report session 'MAIL "unread report")))
      (mevedel--fail-turn fsm ',status)
      (drain)
      (should-not (mevedel-turn-busy-p data-buf))
      (should (= ,(if wake 2 1) (mevedel-session-turn-count session)))
      (should (equal ',status (mevedel-session-last-outcome session)))
      (should (equal (list report) (mevedel-agent-control-context-mailbox session)))
      (should (= ,(if wake 1 0) (length sent)))
      (should-not callbacks)
      ;; Repeated terminal notification cannot restart a failed wake either.
      (mevedel--fail-turn fsm ',status)
      (drain)
      (should (= ,(if wake 1 0) (length sent)))))
  (status wake)
  :doc "failed root leaves late mail unread without an automatic retry loop"
  error nil
  :doc "aborted root leaves late mail unread without an automatic retry loop"
  aborted nil
  :doc "a failed background-event turn cannot loop on its own late mail"
  error t
  :doc "an aborted background-event turn cannot loop on its own late mail"
  aborted t)

(mevedel-deftest mevedel-pending-agent-active-wait ()
  ,test
  (test)
  :doc "WaitAgent consumes mail in its active request without a parallel turn"
  (mevedel-pending-agent-test--with-root
    (let (wakes)
      (with-current-buffer data-buf
        (mevedel-agent-control-wait
         session (lambda (reason)
                   (push reason wakes)
                   (mevedel-tools--handle-message-inject fsm)) 10000))
      (should (mevedel-agent-control-root-waiting-p session))
      (let ((report (mevedel-pending-agent-test--report session 'MAIL "wait report")))
        (drain)
        (should (equal '(mailbox) wakes))
        (should-not (mevedel-agent-control-root-waiting-p session))
        (should-not sent)
        (should-not (mevedel-agent-control-context-mailbox session))
        (should (equal (mevedel-tools--message-delivery-block report)
                       (plist-get (aref (plist-get
                                         (plist-get (gptel-fsm-info fsm) :data)
                                         :messages) 0) :content)))
        (mevedel--complete-turn fsm)
        (drain)
        (should-not sent)
        (with-current-buffer data-buf
          (should-not (string-search mevedel-pending-inputs--event-trigger
                                     (buffer-string))))))))

(mevedel-deftest mevedel-pending-agent-negative-controls ()
  ,test
  (test)
  :doc "without the runtime wake mark, successful settlement strands late mail"
  (mevedel-pending-agent-test--with-root
    (let (report)
      ;; Reproduce the old notification-only behavior without editing product
      ;; sources.  The same mailbox, settlement and provider fixture stays live.
      (cl-letf (((symbol-function 'mevedel-agent-control--deliver-mail)
                 (lambda (owner recipient _record &optional reason)
                   (mevedel-agent-control--wake owner recipient (or reason 'mailbox)))))
        (setq report (mevedel-pending-agent-test--report session 'MAIL "unmarked")))
      (mevedel--complete-turn fsm)
      (drain)
      (should-not sent)
      (should-not (mevedel-agent-control-wake-pending-p session))
      (should (equal (list report) (mevedel-agent-control-context-mailbox session)))
      ;; Restoring the real delivery path makes that exact report consumable.
      (mevedel-agent-control--deliver-mail session "/root" report)
      (drain)
      (mevedel-pending-agent-test--assert-delivered session data-buf sent (list report))))
  :doc "without the idle offer, marked post-settlement RESULT remains stranded"
  (mevedel-pending-agent-test--with-root
    (mevedel--complete-turn fsm)
    (drain)
    (let (report)
      (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event) #'ignore))
        (setq report (mevedel-pending-agent-test--report session 'RESULT "unoffered")))
      (drain)
      (should-not sent)
      (should (mevedel-agent-control-wake-pending-p session))
      (should (equal (list report) (mevedel-agent-control-context-mailbox session)))
      (mevedel-pending-inputs-offer-root-event session)
      (drain)
      (mevedel-pending-agent-test--assert-delivered session data-buf sent (list report)))))

(provide 'test-mevedel-pending-inputs-agent)
;;; test-mevedel-pending-inputs-agent.el ends here
