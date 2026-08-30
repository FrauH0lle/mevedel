;;; test-mevedel-collaboration-guest-remote.el --- Remote guest interaction tests -*- lexical-binding: t; -*-

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
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'mevedel-skills-invoke)


;;
;;; Remote interactions

(mevedel-deftest mevedel-collaboration--on-prompt-created
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
      (overlay-put overlay 'mevedel--remote
                   '(:body "Run rm -rf /tmp/x?"
                     :options ((allow-once . "Allow once")
                               (deny-once . "Deny"))
                     :feedback t))
      (overlay-put overlay 'mevedel-view-interaction-id 'patch-review)
      (puthash 1 (list :name "phone" :writable t :ready t
                       :guest-id "phone-guest-id")
               guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      ;; Keep the notification routing fact even when a sleeping browser's
      ;; WebSocket is no longer among the current peers.
      (puthash "phone-guest-id"
               '(:endpoint "https://push.example/subscription" :writable t)
               push-guests)
      (let ((mevedel-collaboration--rooms (mevedel-test-room-registry room))
            (mevedel-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'mevedel-collaboration--transport-control)
                   (lambda (_transport control)
                     (push control controls)
                     t)))
          ;; An overlay without a remote descriptor is never broadcast.
          (mevedel-collaboration--on-prompt-created (make-overlay 1 2))
          (should-not sent)
          (mevedel-collaboration--on-prompt-created overlay)
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
            (overlay-put overlay 'mevedel--remote
                         '(:body "Applying patch" :body-kind "text"))
            (mevedel-collaboration--on-prompt-created overlay)
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
            (mevedel-collaboration--send-ui-requests room 7)
            (should (equal (plist-get frame :reqId)
                           (plist-get (cdr (car sent)) :reqId))))
          ;; The defcustom gates the whole surface.
          (let ((mevedel-collaboration-remote-interactions nil))
            (setq sent nil)
            (mevedel-collaboration--on-prompt-created overlay)
            (should-not sent)))))))

(mevedel-deftest mevedel-collaboration--handle-ui-response
  (:doc "settles once with the mapped outcome and ignores unauthorized answers")
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
      (overlay-put overlay 'mevedel--remote
                   (list :options
                         (list '(allow-once . "Allow once")
                               (cons (lambda () (setq accepted t)) "Accept"))
                         :feedback t))
      (puthash 1 (list :name "phone" :writable t :ready t) guests)
      (puthash 2 (list :name "laptop" :writable nil :ready t) guests)
      (puthash 41 overlay requests)
      (let ((mevedel-collaboration--rooms (mevedel-test-room-registry room))
            (mevedel-collaboration-remote-interactions t))
        (cl-letf (((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay outcome) (setq settled outcome)))
                  ((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          ;; A read-only guest and an unknown request id are ignored.
          (mevedel-collaboration--handle-ui-response
           room 2 (list :reqId 41 :option 0))
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 999 :option 0))
          (should-not settled)
          ;; A symbol option settles through the shared settle.
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should (eq 'allow-once settled))
          ;; A function option runs instead of settling.
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 1))
          (should accepted)
          (should-not settled)
          ;; Feedback maps to the standard feedback outcome.
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :feedback "  needs a dry run  "))
          (should (equal '(feedback . "needs a dry run") settled))
          ;; A questionnaire answer set reaches the answer handler
          ;; atomically and trimmed; blank means no preference.
          (let ((received nil))
            (overlay-put overlay 'mevedel--remote
                         (append (overlay-get overlay 'mevedel--remote)
                                 (list :answer
                                       (lambda (answers)
                                         (setq received answers)))))
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '(" MVP first " "Yes")))
            (should (equal '("MVP first" "Yes") received))
            (setq received nil)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" "   ")))
            (should (equal '("MVP first" "") received))
            (setq received nil)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :answers '("MVP first" 42)))
            (should-not received)
            ;; A guest is untrusted, and an answer reaches the model and the
            ;; transcript exactly as a guest prompt does, so it carries the
            ;; same byte budget.
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (1+ mevedel-collaboration--max-prompt-bytes)
                                 ?x)
                                "Yes")))
            (should-not received)
            ;; Answers that each clear the budget but together exceed it
            ;; still arrive in one tool result.
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41
                          :answers
                          (list (make-string
                                 (/ mevedel-collaboration--max-prompt-bytes 2)
                                 ?x)
                                (make-string
                                 mevedel-collaboration--max-prompt-bytes ?y))))
            (should-not received))
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41
                        :feedback (make-string
                                   (1+ mevedel-collaboration--max-prompt-bytes)
                                   ?x)))
          (should-not settled)
          ;; A cancel response runs the remote cancel handler when the
          ;; interaction offers one; without a handler, or from a
          ;; read-only guest, it is ignored.
          (let ((cancelled nil))
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (overlay-put overlay 'mevedel--remote
                         (append (overlay-get overlay 'mevedel--remote)
                                 (list :cancel
                                       (lambda () (setq cancelled t)))))
            (mevedel-collaboration--handle-ui-response
             room 2 (list :reqId 41 :cancel t))
            (should-not cancelled)
            (mevedel-collaboration--handle-ui-response
             room 1 (list :reqId 41 :cancel t))
            (should cancelled))
          ;; A questionnaire overlay's frame carries the questions and
          ;; advertises its cancel affordance.
          (overlay-put overlay 'mevedel--remote
                       (append (overlay-get overlay 'mevedel--remote)
                               (list :questions
                                     (lambda ()
                                       '((("question" . "Which?")
                                          ("options" . [(("label" . "A"))])))))))
          (let ((frame (mevedel-collaboration--ui-request-frame 41 overlay)))
            (should (equal "Which?"
                           (cdr (assoc "question"
                                       (aref (plist-get frame :questions)
                                             0)))))
            (should (eq t (plist-get frame :allowCancel))))
          ;; Settlement dismisses the request everywhere writable.
          (mevedel-collaboration--on-prompt-settled overlay)
          (should (= 0 (hash-table-count requests)))
          (should (equal '(1 . (:t "ui-request-end" :reqId 41))
                         (car sent)))
          ;; A late answer after dismissal is ignored silently.
          (setq settled nil)
          (mevedel-collaboration--handle-ui-response
           room 1 (list :reqId 41 :option 0))
          (should-not settled))))))

(provide 'test-mevedel-collaboration-guest-remote)
;;; test-mevedel-collaboration-guest-remote.el ends here
