;;; test-niwa-pending-inputs-agent.el --- Late agent report wakes -*- lexical-binding: t -*-

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
(require 'niwa-agent-control)
(require 'niwa-chat)
(require 'niwa-pending-inputs)
(require 'niwa-tools)
(require 'niwa-turn)
(require 'niwa-view)


(defmacro niwa-pending-agent-test--with-root (&rest body)
  "Run BODY with a root FSM, session and deterministic idle callbacks.
SENT contains actual generated request FSMs.  DRAIN runs scheduled work;
ON-SAVE can deliver a report inside the real settlement transaction."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let* ((workspace (niwa-workspace--create
                        :type 'file :id "late-agent-mail"
                        :root niwa-user-dir :name "late-agent-mail"))
            (session (niwa-session--create
                      :name "late-agent-mail" :workspace workspace
                      :root-buffer data-buf :turn-count 0
                      :agent-root-activity 'running))
            (request (niwa-request--create
                      :id "original" :session session :origin "/root"
                      :turn 1 :started-at (current-time)))
            (fsm (gptel-make-fsm
                  :info (list :buffer data-buf :niwa-request-id "original"
                              :backend nil :data (list :messages []))))
            (niwa-turn-settled-functions nil)
            (niwa-agent-control-suppress-persistence nil)
            (real-timer (symbol-function 'run-at-time))
            callbacks timers sent on-save)
       (with-current-buffer data-buf
         (setq-local niwa--session session
                     niwa--workspace workspace
                     niwa--current-request request))
       (with-current-buffer view-buf
         (setq-local niwa--session session))
       (cl-labels
           ((drain ()
              (let ((steps 0))
                (while callbacks
                  (should (< (cl-incf steps) 100))
                  (funcall (pop callbacks))))))
         (cl-letf
             (((symbol-function 'niwa-transport-run-when-idle)
               (lambda (_key _path thunk &rest _)
                 (setq callbacks (nconc callbacks (list thunk))) t))
              ((symbol-function 'run-at-time)
               (lambda (time repeat function &rest args)
                 (if (eq function #'niwa-pending-inputs--offer-idle-work-now)
                     (progn
                       (setq callbacks
                             (nconc callbacks
                                    (list (lambda () (apply function args)))))
                       nil)
                   (let ((timer (apply real-timer time repeat function args)))
                     (push timer timers) timer))))
              ((symbol-function 'niwa-session-artifacts-save)
               (lambda (&rest _) (when on-save (funcall on-save)) t))
              ((symbol-function 'niwa--turn-checkpoint) #'ignore)
              ((symbol-function 'niwa--run-turn-terminal-hook) #'ignore)
              ((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
               #'ignore)
              ((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
               #'ignore)
              ((symbol-function 'niwa-session-persistence-start-checkpoints)
               #'ignore)
              ((symbol-function 'niwa-session-persistence-schedule-cleanup)
               #'ignore)
              ((symbol-function 'niwa-session-persistence-save-agent-state-soon)
               #'ignore)
              ((symbol-function 'niwa--gc-hold) #'ignore)
              ((symbol-function 'niwa-telemetry-record) #'ignore)
              ((symbol-function 'niwa-view--begin-external-turn) #'ignore)
              ((symbol-function 'niwa--send-request)
               (lambda (&optional _model-input _trigger)
                 (let* ((new (niwa-request-begin session))
                        (machine
                         (gptel-make-fsm
                          :info (list :buffer data-buf :backend nil
                                      :position (copy-marker (point-max) t)
                                      :niwa-request-id (niwa-request-id new)
                                      :data (list :messages [])))))
                   (push machine sent)
                   (niwa-tools--handle-message-inject machine)))))
           (unwind-protect
               (progn ,@body)
             (setq callbacks nil)
             (niwa-agent-control-cancel-wait session "/root")
             (with-current-buffer data-buf (niwa-request-end))
             (dolist (timer timers) (cancel-timer timer))))))))

(defun niwa-pending-agent-test--report (session type payload)
  "Publish an ordinary agent report of TYPE with PAYLOAD to SESSION."
  (let ((record (list :type type :sender "/root/worker" :recipient "/root"
                      :outcome 'completed :payload payload)))
    (if (eq type 'RESULT)
        (niwa-agent-control--publish-result
         session (niwa-agent-record--create
                  :path "/root/worker" :parent-path "/root") record)
      (niwa-agent-control--enqueue session "/root" record))
    record))

(defun niwa-pending-agent-test--assert-delivered (session buffer sent reports)
  "Assert one real turn in BUFFER consumed REPORTS from SESSION via SENT."
  (should (= 1 (length sent)))
  (let* ((machine (car sent))
         (messages (plist-get (plist-get (gptel-fsm-info machine) :data) :messages)))
    (should (= (length reports) (length messages)))
    (cl-loop for report in reports for message across messages do
             (should (equal "user" (plist-get message :role)))
             (should (equal (niwa-tools--message-delivery-block report)
                            (plist-get message :content))))
    ;; A second injection boundary must not deliver a report twice.
    (niwa-tools--handle-message-inject machine)
    (should (equal messages
                   (plist-get (plist-get (gptel-fsm-info machine) :data) :messages))))
  (should-not (niwa-agent-control-context-mailbox session))
  (should-not (niwa-agent-control-wake-pending-p session))
  (with-current-buffer buffer
    (should (= 2 (niwa-request-turn niwa--current-request)))
    (should (= 1 (niwa-view-test--count-substring
                  niwa-pending-inputs--event-trigger (buffer-string))))
    (dolist (report reports)
      (should (= 1 (niwa-view-test--count-substring
                    (niwa-tools--message-delivery-block report)
                    (buffer-string)))))))


(niwa-deftest niwa-pending-agent-late-report ()
  (niwa-pending-agent-test--with-root
    (let (report)
      ;; This is the last injection boundary of the original request.
      (niwa-tools--handle-message-inject fsm)
      (pcase ',phase
        ('before
         (setq report (niwa-pending-agent-test--report session ',type "late report"))
         (drain)
         (should-not sent))
        ('during
         (setq on-save
               (lambda ()
                 (should (niwa-turn-busy-p data-buf))
                 (should (buffer-local-value 'niwa--turn-settlements-pending data-buf))
                 (setq report (niwa-pending-agent-test--report
                               session ',type "late report"))
                 (drain)
                 (should-not sent)))))
      (niwa--complete-turn fsm)
      (drain)
      (when (eq ',phase 'after)
        (should-not (niwa-turn-busy-p data-buf))
        (should-not sent)
        (setq report (niwa-pending-agent-test--report session ',type "late report"))
        (drain))
      (should (= 1 (niwa-session-turn-count session)))
      (should (= 0 (length (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                      :messages))))
      (niwa-pending-agent-test--assert-delivered session data-buf sent (list report))))
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

(niwa-deftest niwa-pending-agent-coalesce ()
  ,test
  (test)
  :doc "multiple late reports and duplicate completion callbacks produce one turn"
  (niwa-pending-agent-test--with-root
    (niwa--complete-turn fsm)
    (drain)
    (let ((reports (list (niwa-pending-agent-test--report session 'MAIL "interim")
                         (niwa-pending-agent-test--report session 'RESULT "terminal"))))
      (niwa--complete-turn fsm)
      (drain)
      (niwa-pending-agent-test--assert-delivered session data-buf sent reports))))

(niwa-deftest niwa-pending-agent-failed-root ()
  (niwa-pending-agent-test--with-root
    (when ,wake
      (niwa--complete-turn fsm)
      (drain)
      (let ((report (niwa-pending-agent-test--report session 'RESULT "start wake")))
        (drain)
        (niwa-pending-agent-test--assert-delivered session data-buf sent (list report)))
      (setq fsm (car sent)))
    (niwa-tools--handle-message-inject fsm)
    (let ((report (niwa-pending-agent-test--report session 'MAIL "unread report")))
      (niwa--fail-turn fsm ',status)
      (drain)
      (should-not (niwa-turn-busy-p data-buf))
      (should (= ,(if wake 2 1) (niwa-session-turn-count session)))
      (should (equal ',status (niwa-session-last-outcome session)))
      (should (equal (list report) (niwa-agent-control-context-mailbox session)))
      (should (= ,(if wake 1 0) (length sent)))
      (should-not callbacks)
      ;; Repeated terminal notification cannot restart a failed wake either.
      (niwa--fail-turn fsm ',status)
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

(niwa-deftest niwa-pending-agent-active-wait ()
  ,test
  (test)
  :doc "WaitAgent consumes mail in its active request without a parallel turn"
  (niwa-pending-agent-test--with-root
    (let (wakes)
      (with-current-buffer data-buf
        (niwa-agent-control-wait
         session (lambda (reason)
                   (push reason wakes)
                   (niwa-tools--handle-message-inject fsm)) 10000))
      (should (niwa-agent-control-root-waiting-p session))
      (let ((report (niwa-pending-agent-test--report session 'MAIL "wait report")))
        (drain)
        (should (equal '(mailbox) wakes))
        (should-not (niwa-agent-control-root-waiting-p session))
        (should-not sent)
        (should-not (niwa-agent-control-context-mailbox session))
        (should (equal (niwa-tools--message-delivery-block report)
                       (plist-get (aref (plist-get
                                         (plist-get (gptel-fsm-info fsm) :data)
                                         :messages) 0) :content)))
        (niwa--complete-turn fsm)
        (drain)
        (should-not sent)
        (with-current-buffer data-buf
          (should-not (string-search niwa-pending-inputs--event-trigger
                                     (buffer-string))))))))

(niwa-deftest niwa-pending-agent-negative-controls ()
  ,test
  (test)
  :doc "without the runtime wake mark, successful settlement strands late mail"
  (niwa-pending-agent-test--with-root
    (let (report)
      ;; Reproduce the old notification-only behavior without editing product
      ;; sources.  The same mailbox, settlement and provider fixture stays live.
      (cl-letf (((symbol-function 'niwa-agent-control--deliver-mail)
                 (lambda (owner recipient _record &optional reason)
                   (niwa-agent-control--wake owner recipient (or reason 'mailbox)))))
        (setq report (niwa-pending-agent-test--report session 'MAIL "unmarked")))
      (niwa--complete-turn fsm)
      (drain)
      (should-not sent)
      (should-not (niwa-agent-control-wake-pending-p session))
      (should (equal (list report) (niwa-agent-control-context-mailbox session)))
      ;; Restoring the real delivery path makes that exact report consumable.
      (niwa-agent-control--deliver-mail session "/root" report)
      (drain)
      (niwa-pending-agent-test--assert-delivered session data-buf sent (list report))))
  :doc "without the idle offer, marked post-settlement RESULT remains stranded"
  (niwa-pending-agent-test--with-root
    (niwa--complete-turn fsm)
    (drain)
    (let (report)
      (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore))
        (setq report (niwa-pending-agent-test--report session 'RESULT "unoffered")))
      (drain)
      (should-not sent)
      (should (niwa-agent-control-wake-pending-p session))
      (should (equal (list report) (niwa-agent-control-context-mailbox session)))
      (niwa-pending-inputs-offer-root-event session)
      (drain)
      (niwa-pending-agent-test--assert-delivered session data-buf sent (list report)))))

(provide 'test-niwa-pending-inputs-agent)
;;; test-niwa-pending-inputs-agent.el ends here
