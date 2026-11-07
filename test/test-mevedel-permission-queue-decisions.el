;;; test-mevedel-permission-queue-decisions.el --- Permission queue decision table -*- lexical-binding: t -*-

;;; Commentary:

;; Characterization table for the permission queue's request path: the real
;; entry point, the real rendered card and the real prompt keys.  It pins what
;; is allowed or denied and when a prompt, review or hold happens, so the
;; request path can change shape without changing a decision.
;;
;; Rows: permission mode (ask, full-auto), reviewer (user, auto), cancelled
;; request, held session, no session, answer keys, settle-once for the same
;; request, coalescing on session and always outcomes, validation refusal,
;; wrong-session and ambient-buffer answers, card replacement, and prompt,
;; review and hold timing.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-permission-queue-test-support"))
(require 'mevedel-permissions)
(require 'mevedel-permission-log)
(require 'mevedel-permission-review)
(require 'mevedel-permission-mode)
(require 'mevedel-sandbox)
(require 'mevedel-tool-exec)
(require 'mevedel-tools)

(defun pqd--entry (tool &rest props)
  "Return a root generic entry for TOOL with PROPS, plus its outcome cell.
The cell is a cons whose car lists the outcomes the callback received."
  (let* ((cell (cons nil nil))
         (entry (append props
                        (list :kind 'generic :tool-name tool :origin "/root"
                              :specifier-value "/x.el" :resource-access 'read
                              :callback (lambda (outcome)
                                          (push outcome (car cell)))))))
    (cons entry cell)))

(defun pqd--outcomes (made)
  "Return the outcomes the callback of MADE received, oldest first."
  (reverse (car (cdr made))))

(defun pqd--events (session)
  "Return SESSION's permission log events as a list of symbols."
  (mapcar (lambda (event) (plist-get event :event))
          (mevedel-session-permission-log-pending session)))

(defmacro pqd--with-reviewer (reviewer &rest body)
  "Run BODY with REVIEWER selected and the review model replaced.
The model is the only seam: it counts reviews in `reviews', keeps the
provider callback in `answer' and the review timeout in `expire'.  The user
turn in the data buffer is the real intent the review requires."
  (declare (indent 1) (debug t))
  `(let ((mevedel-permission-reviewer ,reviewer)
         (mevedel-permission-rules nil)
         (reviews 0) answer expire)
     (ignore answer expire)
     (cl-letf (((symbol-function 'mevedel-permission-review--model)
                (lambda (_evidence callback &optional _stream)
                  (cl-incf reviews)
                  (setq answer callback)
                  #'ignore))
               ((symbol-function 'run-at-time)
                (lambda (_time _repeat function &rest args)
                  (setq expire (lambda () (apply function args)))
                  (timer-create))))
       (with-current-buffer data
         (insert "Read the file /x.el for me.\n"))
       ,@body)))

(mevedel-deftest mevedel-permission-queue/decisions
  (:quiet t :doc "decision table of the permission queue's request path")
  ,test
  (test)

  :doc "ask mode shows one card and each key settles its request once"
  (dolist (row '(("a" . allow-once) ("s" . allow-session)
                 ("d" . deny-once) ("D" . deny-session)
                 ("q" . deny-once) ("C-g" . deny-once)))
    (mevedel-pq-test--with-queue (session _data view)
      (let ((made (pqd--entry "Read" :include-always t)))
        (mevedel-pq-test--request (car made))
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (let ((card (mevedel-pq-test--card view)))
          (should-not (pqd--outcomes made))
          (mevedel-pq-test--press card (car row)))
        (should (equal (list (cdr row)) (pqd--outcomes made)))
        (should-not (mevedel-session-permission-queue session))
        (should-not (mevedel-pq-test--cards view)))))

  :doc "always-allow settles only on a card that offers it"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((made (pqd--entry "Read" :include-always nil)))
      (mevedel-pq-test--request (car made))
      ;; The card does not bind the key, so it falls to the read-only text.
      (should-error (mevedel-pq-test--press (mevedel-pq-test--card view) "A")
                    :type 'text-read-only)
      (should-not (pqd--outcomes made))
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (mevedel-pq-test--card view))))
  (mevedel-pq-test--with-queue (session _data view)
    (let ((made (pqd--entry "Read" :include-always t)))
      (mevedel-pq-test--request (car made))
      (mevedel-pq-test--press (mevedel-pq-test--card view) "A")
      (should (equal '(always-allow) (pqd--outcomes made)))
      (should-not (mevedel-session-permission-queue session))))

  :doc "full-auto mode settles by policy without a card"
  ;; A card kind the policy cannot decide (a sandbox card for another tool)
  ;; is aborted.
  (dolist (row '((none . allow-once) (deny . deny-once) (undecidable . aborted)))
    (mevedel-pq-test--with-queue (session _data view)
      :mode 'full-auto
      :rules (and (eq (car row) 'deny)
                  (list '("Read" :path "/x.el" :action deny)))
      (let ((made (if (eq (car row) 'undecidable)
                      (pqd--entry "Probe" :kind 'sandbox :detail "touch x")
                    (pqd--entry "Read"))))
        (mevedel-pq-test--request (car made))
        (should (equal (list (cdr row)) (pqd--outcomes made)))
        (should-not (mevedel-session-permission-queue session))
        (should-not (mevedel-pq-test--cards view)))))

  :doc "the auto reviewer settles without a card and only the first answer counts"
  (dolist (row '(("allow-once" . allow-once)
                 ("deny" . (deny . "Not asked for"))))
    (mevedel-pq-test--with-queue (session data view)
      (pqd--with-reviewer 'auto
        (let ((made (pqd--entry "Read" :data-buffer data)))
          (mevedel-pq-test--request (car made))
          (should (= 1 reviews))
          ;; Under review nothing is settled and no card exists yet.
          (should-not (pqd--outcomes made))
          (should-not (mevedel-session-permission-queue session))
          (should-not (mevedel-pq-test--cards view))
          (funcall answer (format "{\"decision\":\"%s\",\"reason\":\"Not asked for\"}"
                                  (car row)))
          (funcall answer "{\"decision\":\"deny\",\"reason\":\"Late\"}")
          (should (equal (list (cdr row)) (pqd--outcomes made)))
          (should-not (mevedel-session-permission-queue session))
          (should-not (mevedel-pq-test--cards view))
          (should (equal '(permission-review-completed) (pqd--events session)))))))

  :doc "an auto review that cannot decide leaves the card to the user"
  (dolist (reply '("{\"decision\":\"ask\",\"reason\":\"Unsure\"}" "not json"))
    (mevedel-pq-test--with-queue (session data view)
      (pqd--with-reviewer 'auto
        (let ((made (pqd--entry "Read" :data-buffer data)))
          (mevedel-pq-test--request (car made))
          (should-not (mevedel-pq-test--cards view))
          (funcall answer reply)
          (should-not (pqd--outcomes made))
          (let ((card (mevedel-pq-test--card view)))
            (should (plist-get (plist-get card :entry) :review-fallback))
            (funcall answer "{\"decision\":\"allow-once\",\"reason\":\"Late\"}")
            (should-not (pqd--outcomes made))
            (mevedel-pq-test--press card "d"))
          (should (equal '(deny-once) (pqd--outcomes made)))
          (should (equal '(permission-review-completed permission-enqueued
                           permission-displayed permission-resolved)
                         (pqd--events session)))))))

  :doc "review timing: the review timeout shows the card once and drops the review"
  (mevedel-pq-test--with-queue (session data view)
    (pqd--with-reviewer 'auto
      (let ((made (pqd--entry "Read" :data-buffer data)))
        (mevedel-pq-test--request (car made))
        (should-not (mevedel-pq-test--cards view))
        (funcall expire)
        (funcall expire)
        (should (= 1 (length (mevedel-pq-test--cards view))))
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (funcall answer "{\"decision\":\"allow-once\",\"reason\":\"Late\"}")
        (should-not (pqd--outcomes made))
        (mevedel-pq-test--press (mevedel-pq-test--card view) "a")
        (should (equal '(allow-once) (pqd--outcomes made))))))

  :doc "the reviewer never runs in full-auto mode, without a session or when user"
  (mevedel-pq-test--with-queue (session data view)
    :mode 'full-auto
    (pqd--with-reviewer 'auto
      (let ((made (pqd--entry "Read" :data-buffer data)))
        (mevedel-pq-test--request (car made))
        (should (= 0 reviews))
        (should (equal '(allow-once) (pqd--outcomes made))))))
  (with-temp-buffer
    (let ((mevedel--session nil)
          (mevedel-permission-reviewer 'auto)
          (made (pqd--entry "Read")))
      (cl-letf (((symbol-function 'mevedel-permission-review--model)
                 (lambda (&rest _) (ert-fail "reviewed a request without a session"))))
        (mevedel-pq-test--request (car made)))
      (should (equal '(aborted) (pqd--outcomes made)))))
  (mevedel-pq-test--with-queue (session data view)
    (pqd--with-reviewer 'user
      (let ((made (pqd--entry "Read" :data-buffer data)))
        (mevedel-pq-test--request (car made))
        (should (= 0 reviews))
        (should (mevedel-pq-test--card view))
        (mevedel-pq-test--press (mevedel-pq-test--card view) "a")
        (should (equal '(allow-once) (pqd--outcomes made))))))

  :doc "a cancelled request aborts without a card"
  (mevedel-pq-test--with-queue (session _data view)
    (let* ((request (mevedel-request--create :id "cancelled" :session session))
           (made (pqd--entry "Read" :request request)))
      (setf (mevedel-request-cancelled-p request) t)
      (mevedel-pq-test--request (car made))
      (should (equal '(aborted) (pqd--outcomes made)))
      (should-not (mevedel-session-permission-queue session))
      (should-not (mevedel-pq-test--cards view))))

  :doc "no session aborts without a card or queue"
  (with-temp-buffer
    (let ((mevedel--session nil)
          (made (pqd--entry "Read")))
      (mevedel-pq-test--request (car made))
      (should (equal '(aborted) (pqd--outcomes made)))))

  :doc "a held session denies with the hold reason and shows no card"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((mevedel-permission-queue-timeout 60)
          (made (pqd--entry "Read")))
      (setf (mevedel-session-permission-absence session)
            (list :expired-at (float-time) :count 1))
      (mevedel-pq-test--request (car made))
      (should (equal (list (cons 'deny mevedel-permission-queue--held-reason))
                     (pqd--outcomes made)))
      (should-not (mevedel-session-permission-queue session))
      (should-not (mevedel-pq-test--cards view))))

  :doc "a stale repeated answer to the first request never settles the next"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((first (pqd--entry "Read" :specifier-value "/first.el"))
          (second (pqd--entry "Read" :specifier-value "/second.el")))
      (mevedel-pq-test--request (car first))
      (mevedel-pq-test--request (car second))
      (let ((card (mevedel-pq-test--card view)))
        (should (equal "/first.el" (plist-get (plist-get card :entry)
                                              :specifier-value)))
        (should (= 2 (plist-get card :count)))
        (mevedel-pq-test--press card "a")
        (should (equal '(allow-once) (pqd--outcomes first)))
        (mevedel-pq-test--answer-captured card 'allow-once)
        (mevedel-pq-test--answer-captured card 'always-allow)
        (should (equal '(allow-once) (pqd--outcomes first)))
        (should-not (pqd--outcomes second))
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (let ((next (mevedel-pq-test--card view)))
          (should (equal "/second.el" (plist-get (plist-get next :entry)
                                                 :specifier-value)))
          (should (= 1 (plist-get next :count)))
          (mevedel-pq-test--press next "d")))
      (should (equal '(deny-once) (pqd--outcomes second)))
      (should (equal '(allow-once) (pqd--outcomes first)))
      (should-not (mevedel-session-permission-queue session))))

  :doc "session and always outcomes coalesce matching siblings only"
  (dolist (key '("s" "A"))
    (mevedel-pq-test--with-queue (session _data view)
      (let* ((head (pqd--entry "Read" :include-always t))
             (twin (pqd--entry "Read"))
             (other (pqd--entry "Read" :specifier-value "/other.el")))
        ;; The caller stores the authority the answer grants, as the pipeline
        ;; does, before the queue re-evaluates its siblings.
        (setq head
              (cons (plist-put
                     (car head) :callback
                     (let ((inner (plist-get (car head) :callback)))
                       (lambda (outcome)
                         (funcall inner outcome)
                         (mevedel-permission-add-session-resource-grant
                          session "/x.el" 'read))))
                    (cdr head)))
        (mevedel-pq-test--request (car head))
        (mevedel-pq-test--request (car twin))
        (mevedel-pq-test--request (car other))
        (mevedel-pq-test--press (mevedel-pq-test--card view) key)
        (should (equal (list (if (equal key "s") 'allow-session 'always-allow))
                       (pqd--outcomes head)))
        (should (equal '(allow) (pqd--outcomes twin)))
        (should-not (pqd--outcomes other))
        (should (= 1 (length (mevedel-session-permission-queue session))))
        (let ((card (mevedel-pq-test--card view)))
          (should (equal "/other.el" (plist-get (plist-get card :entry)
                                                :specifier-value)))
          (should (= 1 (plist-get card :count)))))))
  (mevedel-pq-test--with-queue (session _data view)
    (let* ((head (pqd--entry "Read"))
           (twin (pqd--entry "Read")))
      (setq head
            (cons (plist-put
                   (car head) :callback
                   (let ((inner (plist-get (car head) :callback)))
                     (lambda (outcome)
                       (funcall inner outcome)
                       (push '("Read" :path "/x.el" :action deny)
                             (mevedel-session-permission-rules session)))))
                  (cdr head)))
      (mevedel-pq-test--request (car head))
      (mevedel-pq-test--request (car twin))
      (mevedel-pq-test--press (mevedel-pq-test--card view) "D")
      (should (equal '(deny-session) (pqd--outcomes head)))
      (should (equal '(deny) (pqd--outcomes twin)))
      (should-not (mevedel-session-permission-queue session))))

  :doc "approval refused by validation leaves the card and queue unsettled"
  (let ((root (make-temp-file "mevedel-pq-validation-" t)))
    (unwind-protect
        (mevedel-pq-test--with-queue (session _data view)
          (setf (mevedel-session-sandbox-mode session) 'required)
          (let* ((selection (list (list (list :path root :access 'write))))
                 (made (pqd--entry "Bash" :kind 'sandbox
                                   :resource-selection-cell selection
                                   :detail "touch example")))
            (mevedel-pq-test--request (car made))
            ;; Simulate a stale or externally supplied unusable selection.
            (setcar selection (list (list :path root :access 'write)))
            (let ((card (mevedel-pq-test--card view)))
              (should (equal '(user-error text-read-only text-read-only)
                             (mapcar (lambda (key)
                                       (car (should-error
                                             (mevedel-pq-test--press card key))))
                                     '("a" "s" "A"))))
              (should-not (pqd--outcomes made))
              (should (= 1 (length (mevedel-session-permission-queue session))))
              (should (mevedel-pq-test--card view))
              (mevedel-pq-test--press card "d"))
            (should (equal '(deny-once) (pqd--outcomes made)))))
      (delete-directory root t)))

  :doc "a request from a buffer without the session's view aborts unseen"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((other (mevedel-pq-test--make-session))
          (made (pqd--entry "Read")))
      (with-temp-buffer
        (setq-local mevedel--session other)
        (mevedel-pq-test--request (car made) session))
      (should (equal '(aborted) (pqd--outcomes made)))
      (should-not (mevedel-session-permission-queue session))
      (should-not (mevedel-session-permission-queue other))
      (should-not (mevedel-pq-test--cards view))))

  :doc "the entry's session settles its request whatever buffer answers"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((other (mevedel-pq-test--make-session))
          (made (pqd--entry "Read")))
      (mevedel-pq-test--request (car made))
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (eq session (plist-get (car (mevedel-session-permission-queue
                                           session))
                                     :session)))
      (should-not (mevedel-session-permission-queue other))
      (let ((card (mevedel-pq-test--card view)))
        (with-temp-buffer
          (setq-local mevedel--session other)
          (mevedel-pq-test--answer-captured card 'deny-once)
          (mevedel-pq-test--answer-captured card 'deny-once)))
      (should (equal '(deny-once) (pqd--outcomes made)))
      (should-not (mevedel-session-permission-queue session))
      (should-not (mevedel-session-permission-queue other))))

  :doc "a sibling's arrival replaces the card without a second settlement"
  (mevedel-pq-test--with-queue (session _data view)
    (let ((first (pqd--entry "Read" :specifier-value "/first.el"))
          (second (pqd--entry "Read" :specifier-value "/second.el"))
          old)
      (mevedel-pq-test--request (car first))
      (setq old (mevedel-pq-test--card view))
      (should (= 1 (plist-get old :count)))
      (mevedel-pq-test--request (car second))
      (let ((new (mevedel-pq-test--card view)))
        (should (= 2 (plist-get new :count)))
        (should (equal (plist-get (plist-get old :entry) :permission-id)
                       (plist-get (plist-get new :entry) :permission-id)))
        (mevedel-pq-test--answer-captured old 'allow-once)
        (mevedel-pq-test--answer-captured new 'deny-once))
      (should (equal '(allow-once) (pqd--outcomes first)))
      (should-not (pqd--outcomes second))
      (should (= 1 (length (mevedel-session-permission-queue session))))
      (should (equal "/second.el"
                     (plist-get (plist-get (mevedel-pq-test--card view) :entry)
                                :specifier-value)))))

  :doc "prompt timing: a card appears at admission and the next after settlement"
  (mevedel-pq-test--with-queue (session _data view)
    (let* ((notified nil)
           (mevedel-permission-notify-function
            (lambda (entry) (push (plist-get entry :specifier-value) notified)))
           (first (pqd--entry "Read" :specifier-value "/first.el"))
           (second (pqd--entry "Read" :specifier-value "/second.el")))
      (mevedel-pq-test--request (car first))
      (should (equal '("/first.el") notified))
      (should (equal '(permission-enqueued permission-displayed)
                     (pqd--events session)))
      (mevedel-pq-test--request (car second))
      (should (equal '("/second.el" "/first.el") notified))
      (should (equal '(permission-enqueued permission-displayed
                       permission-enqueued)
                     (pqd--events session)))
      (mevedel-pq-test--press (mevedel-pq-test--card view) "a")
      (should (equal '(permission-enqueued permission-displayed
                       permission-enqueued permission-resolved
                       permission-displayed)
                     (pqd--events session)))
      (mevedel-pq-test--press (mevedel-pq-test--card view) "d")
      (should (equal '(permission-resolved)
                     (last (pqd--events session))))
      (should (= 6 (length (pqd--events session))))))

  :doc "hold timing: expiry denies every card, holds, and a message resumes"
  (mevedel-pq-test--with-queue (session _data view)
    (let* ((mevedel-permission-queue-timeout 60)
           (first (pqd--entry "Read" :specifier-value "/first.el"))
           (second (pqd--entry "Read" :specifier-value "/second.el"))
           (late (pqd--entry "Read" :specifier-value "/late.el"))
           (resumed (pqd--entry "Read" :specifier-value "/resumed.el")))
      (mevedel-pq-test--request (car first))
      (mevedel-pq-test--request (car second))
      (let ((timer (plist-get (mevedel-session-permission-absence session)
                              :timer)))
        (should (timerp timer))
        (should (plist-get (mevedel-session-permission-absence session)
                           :deadline))
        (timer-event-handler timer))
      (let ((denial (list (cons 'deny mevedel-permission-queue--expired-reason))))
        (should (equal denial (pqd--outcomes first)))
        (should (equal denial (pqd--outcomes second))))
      (should-not (mevedel-session-permission-queue session))
      (should-not (mevedel-pq-test--cards view))
      (should (mevedel-permission-absence-held-p session))
      (mevedel-pq-test--request (car late))
      (should (equal (list (cons 'deny mevedel-permission-queue--held-reason))
                     (pqd--outcomes late)))
      (should-not (mevedel-pq-test--cards view))
      (should (mevedel-permission-absence-clear session))
      (mevedel-pq-test--request (car resumed))
      (should (mevedel-pq-test--card view))
      (should-not (pqd--outcomes resumed))
      (mevedel-pq-test--press (mevedel-pq-test--card view) "a")
      (should (equal '(allow-once) (pqd--outcomes resumed)))))

  :doc "an expired request's late answer settles nothing"
  (mevedel-pq-test--with-queue (session _data view)
    (let* ((mevedel-permission-queue-timeout 60)
           (made (pqd--entry "Read"))
           captured)
      (mevedel-pq-test--request (car made))
      (setq captured (mevedel-pq-test--card view))
      (timer-event-handler
       (plist-get (mevedel-session-permission-absence session) :timer))
      (should (= 1 (length (pqd--outcomes made))))
      (mevedel-pq-test--answer-captured captured 'allow-once)
      (should (= 1 (length (pqd--outcomes made))))
      (should-not (memq 'allow-once (pqd--outcomes made)))))
  )

(provide 'test-mevedel-permission-queue-decisions)
;;; test-mevedel-permission-queue-decisions.el ends here
