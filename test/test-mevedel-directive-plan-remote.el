;;; test-mevedel-directive-plan-remote.el --- Browser directive feedback -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise directive Plan feedback through its rendered remote interaction.

;;; Code:

(require 'mevedel)
(require 'mevedel-directive-plan)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-directive-plan--present
  (:quiet t)
  ,test
  (test)
  :doc "rendered browser feedback resumes directive planning for full and owner, not view"
  (dolist (tier '(view full owner))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create :name "main"))
             (attempt (gensym "attempt-"))
             (selection (list :location 'here :context 'current :execution 'direct
                              :mode 'edits :model-provider "OpenAI:gpt-5"))
             (record (mevedel-directive--create
                      :id "directive" :request "Request"
                      :plan (list :status 'proposed :action 'implement
                                  :attempt attempt :chat-buffer data-buf
                                  :selection selection :proposal "# Current plan"
                                  :implementation-prompt "Exact request")))
             (directive (make-overlay 1 1 data-buf))
             (guests (make-hash-table :test #'equal))
             (room (list :guests guests :ui-requests (make-hash-table)))
             (mevedel-collaboration-remote-interactions t)
             (feedback "Cover recovery; literal $skill and /clear")
             overlay frame prompt options)
        (puthash "peer" (list :name "Reviewer" :writable (not (eq tier 'view))
                              :owner (eq tier 'owner)) guests)
        (with-current-buffer data-buf (setq-local mevedel--session session))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel--directive-record)
                       (lambda (_) record))
                      ((symbol-function 'mevedel-directive-plan--planning-model-policy)
                       #'ignore)
                      ((symbol-function 'mevedel--prompt-announce)
                       (lambda (value)
                         (setq overlay value
                               frame (mevedel-collaboration--ui-request-frame 1 value))
                         (puthash 1 value (plist-get room :ui-requests))))
                      ((symbol-function 'mevedel--process-directive)
                       (lambda (owner _preset prompt-fn _callback &optional value)
                         (should (eq owner directive))
                         (setq prompt (funcall prompt-fn "ignored") options value))))
              (mevedel-directive-plan--present
               directive record session data-buf nil)
              (should (overlayp overlay))
              (should (eq t (plist-get frame :allowFeedback)))
              (mevedel-collaboration--handle-ui-response
               room "peer" (list :reqId 1 :feedback feedback))
              (if (eq tier 'view)
                  (progn
                    (should-not prompt)
                    (should (mevedel-session-pending-plan-approval session)))
                (should (string-search feedback prompt))
                (should (string-search "Exact request" prompt))
                (should (string-search "# Current plan" prompt))
                (should (eq t (plist-get options :plan-continuation)))
                (should (equal feedback (plist-get options :message)))
                (should-not (mevedel-session-pending-plan-approval session))
                (should (eq 'planning
                            (plist-get (mevedel-directive-plan record) :status)))))
          (setf (mevedel-session-pending-plan-approval session) nil)
          (delete-overlay directive))))))

(provide 'test-mevedel-directive-plan-remote)
;;; test-mevedel-directive-plan-remote.el ends here
