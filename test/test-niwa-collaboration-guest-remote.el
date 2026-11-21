;;; test-niwa-collaboration-guest-remote.el --- Remote guest interaction tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests remote interaction publication and settlement for collaboration guests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-pending-inputs)
(require 'niwa-prompt-submission)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-transcript-audit)
(require 'niwa-chat)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-input-files)
(require 'niwa-view-render)
(require 'niwa-workspace)
(require 'niwa-skills-invoke)


;;
;;; Remote interactions

(niwa-deftest niwa-collaboration--on-prompt-created
  (:doc "presents remote-capable prompts to writable guests only, gated by the defcustom")
  (with-temp-buffer
    (insert "prompt text")
    (let* ((guests (make-hash-table :test #'eql))
           (push-guests (make-hash-table :test #'equal))
           (requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :guests guests
                       :data-buffer (current-buffer)
                       :ui-requests requests
                       :push-guests push-guests))
           (overlay (make-overlay 1 5))
           sent controls)
      (overlay-put overlay 'niwa--remote
                   '(:body "Run rm -rf /tmp/x?"
                     :options ((allow-once . "Allow once")
                               (deny-once . "Deny"))
                     :feedback t))
      (overlay-put overlay 'niwa-view-interaction-id 'patch-review)
      (puthash 1 (list :name "phone" :writable t :ready t
                       :guest-id "phone-guest-id")
               guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      ;; Keep the notification routing fact even when a sleeping browser's
      ;; WebSocket is no longer among the current peers.
      (puthash "phone-guest-id"
               '(:endpoint "https://push.example/subscription" :writable t)
               push-guests)
      (let ((niwa-collaboration--rooms (niwa-test-room-registry room))
            (niwa-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'niwa-collaboration--transport-control)
                   (lambda (_transport control)
                     (push control controls)
                     t)))
          ;; An overlay without a remote descriptor is never broadcast.
          (niwa-collaboration--on-prompt-created (make-overlay 1 2))
          (should-not sent)
          (niwa-collaboration--on-prompt-created overlay)
          (should (= 1 (length sent)))
          (should (equal '(:t "push" :guestIds ["phone-guest-id"])
                         (car controls)))
          ;; Writable guest only; the read-only guest sees nothing.
          (should (= 1 (car (car sent))))
          (let ((frame (cdr (car sent))))
            (should (equal "ui-request" (plist-get frame :t)))
            (should (equal "Run rm -rf /tmp/x?" (plist-get frame :body)))
            (should (equal '("Allow once" "Deny")
                           (mapcar (lambda (option)
                                     (cdr (assoc "label" option)))
                                   (append (plist-get frame :options) nil))))
            (should (eq t (plist-get frame :allowFeedback)))
            (should (= 1 (hash-table-count requests)))
            ;; An in-flight body-only update reuses the request and removes
            ;; every decision from the guest card.
            (setq sent nil)
            (setq controls nil)
            (overlay-put overlay 'niwa--remote
                         '(:body "Applying patch" :body-kind "text"))
            (niwa-collaboration--on-prompt-created overlay)
            (let ((update (cdr (car sent))))
              (should (equal (plist-get frame :reqId)
                             (plist-get update :reqId)))
              (should (equal "Applying patch" (plist-get update :body)))
              (should (= 0 (length (plist-get update :options))))
              (should (eq :json-false
                          (plist-get update :allowFeedback))))
            (should-not controls)
            ;; A late-joining writable guest receives the active request.
            (setq sent nil)
            (niwa-collaboration--send-ui-requests room 7)
            (should (equal (plist-get frame :reqId)
                           (plist-get (cdr (car sent)) :reqId)))
            ;; A narrowed one is withheld on reconnect as well: a guest
            ;; must not collect a decision it may not see by rejoining.
            (setq sent nil)
            (overlay-put overlay 'niwa--remote
                         '(:body "Approve?" :audience owner))
            (niwa-collaboration--send-ui-requests room 1)
            (should-not sent)
            (puthash 8 (list :name "owner" :writable t :owner t :ready t)
                     guests)
            (niwa-collaboration--send-ui-requests room 8)
            (should (= 1 (length sent))))
          ;; The defcustom gates the whole surface.
          (let ((niwa-collaboration-remote-interactions nil))
            (setq sent nil)
            (niwa-collaboration--on-prompt-created overlay)
            (should-not sent)))))))

(niwa-deftest niwa-collaboration-retire-permission-requests
  (:doc "retires the whole batch before sending, with one notice per affected guest")
  (with-temp-buffer
    (insert "prompt text")
    (let* ((session (niwa-session--create))
           (other-session (niwa-session--create))
           (guests (make-hash-table :test #'eql))
           (requests (make-hash-table :test #'eql))
           (other-requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :session session :guests guests
                       :data-buffer (current-buffer) :ui-requests requests))
           (other-room (list :transport 'other :session other-session
                             :guests guests :ui-requests other-requests))
           (first (list :kind 'generic))
           (second (list :kind 'bash))
           (unshown (list :kind 'eval))
           (unrelated (make-overlay 1 2))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           sent)
      (puthash 'other-room other-room niwa-collaboration--rooms)
      (puthash 1 '(:name "full" :writable t) guests)
      (puthash 2 '(:name "readonly" :writable nil) guests)
      (puthash 3 '(:name "owner" :writable t :owner t) guests)
      (dolist (pair (list (cons 41 first) (cons 42 second)))
        (let ((overlay (make-overlay 1 5)))
          (overlay-put overlay 'niwa-view-interaction-entry (cdr pair))
          (overlay-put overlay 'niwa--callback
                       (lambda (_) (ert-fail "Retirement invoked an answer callback")))
          (when (= (car pair) 42)
            (overlay-put overlay 'niwa--remote '(:audience owner)))
          (puthash (car pair) overlay requests)))
      (puthash 43 unrelated requests)
      (puthash 41 (gethash 41 requests) other-requests)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (transport peer frame)
                   ;; No transport callback may see a partially retired batch.
                   (should-not (gethash 41 requests))
                   (should-not (gethash 42 requests))
                   (should (eq transport 'transport))
                   (push (cons peer frame) sent)
                   t)))
        (niwa-collaboration-retire-permission-requests
         session (list first second unshown))
        (should (= 1 (hash-table-count requests)))
        (should (eq unrelated (gethash 43 requests)))
        (should (= 1 (hash-table-count other-requests)))
        (should (= 5 (length sent)))
        (should-not (assq 2 sent))
        (dolist (peer '(1 3))
          (let* ((frames (mapcar #'cdr (seq-filter (lambda (item) (= peer (car item))) sent)))
                 (notices (seq-filter (lambda (frame) (equal "notice" (plist-get frame :t))) frames))
                 (ends (seq-filter (lambda (frame) (equal "ui-request-end" (plist-get frame :t))) frames)))
            (should (= 1 (length notices)))
            (should (equal "3 permission requests expired; nothing was approved."
                           (plist-get (car notices) :message)))
            (should (equal (if (= peer 1) '(41) '(41 42))
                           (sort (mapcar (lambda (frame) (plist-get frame :reqId)) ends) #'<)))))
        ;; Repeated retirement is inert and cannot dismiss an unrelated Ask.
        (setq sent nil)
        (niwa-collaboration-retire-permission-requests session (list first second))
        (should-not sent)
        ;; A single expired request is announced in the singular.
        (let ((overlay (make-overlay 1 5))
              (single (list :kind 'generic)))
          (overlay-put overlay 'niwa-view-interaction-entry single)
          (puthash 44 overlay requests)
          (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent)
                       t)))
            (niwa-collaboration-retire-permission-requests
             session (list single)))
          (should (equal "1 permission request expired; nothing was approved."
                         (plist-get
                          (cdr (seq-find (lambda (item)
                                           (equal "notice" (plist-get (cdr item) :t)))
                                         sent))
                          :message))))))))

(niwa-deftest niwa-collaboration--handle-ui-response
  ()
  ,test
  (test)
  :doc "settles once with the mapped outcome and ignores unauthorized answers"
  (with-temp-buffer
    (insert "prompt text")
    (let* ((guests (make-hash-table :test #'eql))
           (requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :guests guests
                       :data-buffer (current-buffer)
                       :ui-requests requests))
           (overlay (make-overlay 1 5))
           (accepted nil)
           settled sent)
      (overlay-put overlay 'niwa--remote
                   (list :options
                         (list '(allow-once . "Allow once")
                               (cons (lambda () (setq accepted t)) "Accept"))
                         :feedback t))
      (puthash 1 (list :name "phone" :writable t :ready t) guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      (puthash 41 overlay requests)
      (let ((niwa-collaboration--rooms (niwa-test-room-registry room))
            (niwa-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'niwa--prompt--settle)
                   (lambda (_overlay outcome) (setq settled outcome)))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          ;; A read-only guest is ignored; an unknown id gets a notice.
          (niwa-collaboration--handle-ui-response
           room 2 (list :reqId 41 :option 0))
          (should-not sent)
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 999 :option 0))
          (should-not settled)
          (should (equal '(1 . (:t "notice" :message
                                  "Request is no longer pending; nothing was approved."))
                         (car sent)))
          ;; Seeing a narrowed interaction and answering it are the same
          ;; authority: a guest outside the audience is refused even
          ;; though a request id is easy to guess.
          (overlay-put overlay 'niwa--remote
                       (append (overlay-get overlay 'niwa--remote)
                               (list :audience 'owner)))
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should-not settled)
          (overlay-put overlay 'niwa--remote
                       (list :options
                             (list '(allow-once . "Allow once")
                                   (cons (lambda () (setq accepted t))
                                         "Accept"))
                             :feedback t))
          ;; A symbol option settles through the shared settle.
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option -1))
          (should-not settled)
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should (eq 'allow-once settled))
          ;; A function option runs instead of settling.
          (setq settled nil)
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 1))
          (should accepted)
          (should-not settled)
          ;; Feedback maps to the standard feedback outcome.
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :feedback "  needs a dry run  "))
          (should (equal '(feedback . "needs a dry run") settled))
          ;; A questionnaire answer set reaches the answer handler
          ;; atomically and trimmed; blank means no preference.
          (let ((received nil))
            (overlay-put overlay 'niwa--remote
                         (append (overlay-get overlay 'niwa--remote)
                                 (list :answer
                                       (lambda (answers)
                                         (setq received answers)))))
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '(" MVP first " "Yes")))
            (should (equal '("MVP first" "Yes") received))
            (setq received nil)
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" "   ")))
            (should (equal '("MVP first" "") received))
            (setq received nil)
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" 42)))
            (should-not received)
            ;; A guest is untrusted, and an answer reaches the model and the
            ;; transcript exactly as a guest prompt does, so it carries the
            ;; same byte budget.
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (1+ niwa-collaboration--max-prompt-bytes)
                                 ?x)
                                "Yes")))
            (should-not received)
            ;; Answers that each clear the budget but together exceed it
            ;; still arrive in one tool result.
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (/ niwa-collaboration--max-prompt-bytes 2)
                                 ?x)
                                (make-string
                                 niwa-collaboration--max-prompt-bytes ?y))))
            (should-not received))
          (setq settled nil)
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41
                        :feedback (make-string
                                   (1+ niwa-collaboration--max-prompt-bytes)
                                   ?x)))
          (should-not settled)
          ;; A cancel response runs the remote cancel handler when the
          ;; interaction offers one; without a handler, or from a
          ;; read-only guest, it is ignored.
          (let ((cancelled nil))
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (overlay-put overlay 'niwa--remote
                         (append (overlay-get overlay 'niwa--remote)
                                 (list :cancel
                                       (lambda () (setq cancelled t)))))
            (niwa-collaboration--handle-ui-response
             room 2 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (niwa-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should cancelled))
          ;; A questionnaire overlay's frame carries the questions and
          ;; advertises its cancel affordance.
          (overlay-put overlay 'niwa--remote
                       (append (overlay-get overlay 'niwa--remote)
                               (list :questions
                                     (lambda ()
                                       '((("question" . "Which?")
                                          ("options" . [(("label" . "A"))])))))))
          (let ((frame (niwa-collaboration--ui-request-frame 41 overlay)))
            (should (equal "Which?"
                           (cdr (assoc "question"
                                       (aref (plist-get frame :questions)
                                             0)))))
            (should (eq t (plist-get frame :allowCancel))))
          ;; Settlement dismisses the request everywhere writable.
          (niwa-collaboration--on-prompt-settled overlay)
          (should (= 0 (hash-table-count requests)))
          (should (equal '(1 . (:t "ui-request-end" :reqId 41))
                         (car sent)))
          ;; A late answer after dismissal gets a nonterminal notice.
          (setq settled nil)
          (niwa-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should-not settled)
          (should (equal '(1 . (:t "notice" :message
                                  "Request is no longer pending; nothing was approved."))
                         (car sent)))))))

  :doc "actionable response failures reach the normal owner-safe frame boundary"
  (with-temp-buffer
    (let* ((overlay (make-overlay (point-min) (point-min)))
           (guests (make-hash-table :test #'eql))
           (requests (make-hash-table :test #'eql))
           (room (list :transport 'transport :guests guests
                       :data-buffer (current-buffer) :ui-requests requests))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (niwa-collaboration-remote-interactions t)
           sent)
      (overlay-put overlay 'niwa--remote
                   (list :options
                         (list (cons (lambda () (user-error "Review changed patch"))
                                     "Apply patch"))))
      (puthash 41 overlay requests)
      (puthash 1 '(:writable t :ready t :name "writer") guests)
      (puthash 2 '(:writable t :owner t :ready t :name "owner") guests)
      (puthash 3 '(:writable nil :ready t :name "reader") guests)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport peer frame) (push (cons peer frame) sent))))
        (niwa-test--with-captured-messages nil
          (niwa-collaboration--on-frame
           (current-buffer) 3 '(:t "ui-response" :reqId 41 :option 0))
          (should-not sent)
          (niwa-collaboration--on-frame
           (current-buffer) 1 '(:t "ui-response" :reqId 41 :option 0))
          (should (equal (caar sent) 1))
          (should (equal (plist-get (cdar sent) :t) "notice"))
          (should-not (string-match-p "Review changed patch"
                                      (plist-get (cdar sent) :message)))
          (niwa-collaboration--on-frame
           (current-buffer) 2 '(:t "ui-response" :reqId 41 :option 0))
          (should (equal (caar sent) 2))
          (should (equal (plist-get (cdar sent) :message) "Review changed patch"))
          (should (eq (gethash 41 requests) overlay)))))))

(niwa-deftest niwa-collaboration--questionnaire-answers
  ()
  ,test
  (test)
  :doc "normalizes mixed single/multi answers and empty selections atomically"
  (progn
    (should (equal (niwa-collaboration--questionnaire-answers
                    '(" yes " (" a " "b") nil ""))
                   '("yes" ("a" "b") nil "")))
    (dolist (answers '(42 ("valid" 3) ("valid" ("a" 3)) ("valid" (("nested")))))
      (should-not (niwa-collaboration--questionnaire-answers answers))))

  :doc "enforces a shared UTF-8 budget across nested selections"
  (let ((niwa-collaboration--max-prompt-bytes 8))
    (should (equal (niwa-collaboration--questionnaire-answers
                    '("1234" ("5678"))) '("1234" ("5678"))))
    (should-not (niwa-collaboration--questionnaire-answers
                 '("1234" ("56789"))))
    (should-not (niwa-collaboration--questionnaire-answers
                 '(("123456789"))))))

(niwa-deftest niwa-collaboration--ui-request-frame
  (:doc "projects current patch warnings alongside its selected diff")
  (with-temp-buffer
    (let ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'niwa--remote
                   '(:body "+selected change" :body-kind "diff"
                     :notice "Conflict: file changed."))
      (let ((frame (niwa-collaboration--ui-request-frame 42 overlay)))
        (should (equal (plist-get frame :notice) "Conflict: file changed."))
        (should (equal (plist-get frame :body) "+selected change"))
        (should (equal (plist-get frame :bodyKind) "diff"))))))

(provide 'test-niwa-collaboration-guest-remote)
;;; test-niwa-collaboration-guest-remote.el ends here
