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

(mevedel-deftest mevedel-directive-plan-continue/remote-retry
  (:quiet t)
  (mevedel-view-test--with-buffers
    (let* ((failure ',failure)
           (session (mevedel-session--create :name "main"))
           (attempt (gensym "attempt-"))
           (workflow (list :directive-id "directive" :action 'implement
                           :phase 'approval :attempt attempt :callback #'ignore))
           (record (mevedel-directive--create
                    :id "directive" :request "Request"
                    :plan (list :status 'proposed :action 'implement
                                :attempt attempt :chat-buffer data-buf
                                :selection (list :location 'here :context 'current
                                                 :execution 'direct :mode 'edits
                                                 :model-provider "OpenAI:gpt-5")
                                :proposal "# Current plan"
                                :implementation-prompt "Exact request")))
           (directive (make-overlay 1 1 data-buf))
           (guests (make-hash-table :test #'equal))
           (room (list :guests guests :ui-requests (make-hash-table)))
           (mevedel-collaboration-remote-interactions t)
           (feedback "Retry literal $skill and /clear")
           (fail t) (injected 0) (dispatches 0) (request-id 0)
           entry prior warnings prompt options)
      (puthash "peer" (list :name "Reviewer" :writable t) guests)
      (with-current-buffer data-buf (setq-local mevedel--session session))
      (setf (mevedel-session-directive-planning session) workflow)
      (overlay-put directive 'mevedel-directive-action 'plan)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel--directive-record)
                     (lambda (_) record))
                    ((symbol-function 'mevedel-directive-plan--planning-model-policy)
                     (lambda (_)
                       (when (and fail (eq failure 'policy))
                         (cl-incf injected)
                         (error "Injected planning policy failure"))))
                    ((symbol-function 'display-warning)
                     (lambda (_type text &rest _) (push text warnings)))
                    ((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                    ((symbol-function 'mevedel--prompt-announce)
                     (lambda (overlay)
                       (puthash (cl-incf request-id) overlay
                                (plist-get room :ui-requests))))
                    ((symbol-function 'mevedel--process-directive)
                     (lambda (_owner _preset prompt-fn _callback &optional value)
                       (when fail
                         (cl-incf injected)
                         ;; Startup cleanup releases its planning workflow.
                         (setf (mevedel-session-directive-planning session) nil)
                         (error "Injected planning dispatch failure"))
                       (cl-incf dispatches)
                       (setq prompt (funcall prompt-fn "ignored") options value)
                       (setf (mevedel-session-directive-planning session)
                             (list :directive-id "directive" :phase 'planning)))))
            (mevedel-directive-plan--present directive record session data-buf nil)
            (setq entry (mevedel-session-pending-plan-approval session)
                  prior (copy-tree (mevedel-directive-plan record)))
            (mevedel-collaboration--handle-ui-response
             room "peer" (list :reqId request-id :feedback feedback))
            (should (= injected 1))
            (should (= dispatches 0))
            (should (= (length warnings) 1))
            (should (string-search
                     (format "Injected planning %s failure" failure) (car warnings)))
            (should (eq entry (mevedel-session-pending-plan-approval session)))
            (should (> request-id 1))
            ;; The redisplayed approval still captures the original attempt.
            (should (equal prior (mevedel-directive-plan record)))
            (should (equal workflow (mevedel-session-directive-planning session)))
            (setq fail nil)
            (mevedel-collaboration--handle-ui-response
             room "peer" (list :reqId request-id :feedback feedback))
            (should (= dispatches 1))
            (should-not (mevedel-session-pending-plan-approval session))
            (should (string-search feedback prompt))
            (should (string-search "# Current plan" prompt))
            (should (string-search "Exact request" prompt))
            (should (equal feedback (plist-get options :message)))
            (should (plist-get options :plan-continuation))
            (should (eq 'planning (plist-get (mevedel-directive-plan record) :status)))
            (should (eq (plist-get (mevedel-directive-plan record) :attempt)
                        (plist-get (mevedel-session-directive-planning session) :attempt))))
        (setf (mevedel-session-pending-plan-approval session) nil)
        (delete-overlay directive))))
  (failure)
  :doc "policy failure retains the original approval and permits browser feedback retry"
  policy
  :doc "dispatch failure restores the proposal and workflow for browser feedback retry"
  dispatch)

(mevedel-deftest mevedel-directive-plan-continue/late-startup-error
  (:quiet t)
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "main"))
           (record (mevedel-directive--create
                    :id "directive"
                    :plan (list :status 'proposed :attempt 'old
                                :chat-buffer (current-buffer))))
           (directive (make-overlay (point-min) (point-min)))
           (replacement (list :status 'proposed :attempt 'replacement))
           retained)
      (setq-local mevedel--session session)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel--directive-record) (lambda (_) record))
                    ((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                    ((symbol-function 'mevedel-directive-plan--planning-model-policy) #'ignore)
                    ((symbol-function 'mevedel--process-directive)
                     (lambda (_owner _preset _prompt callback &optional _options)
                       (if ',settle
                           (funcall callback "Planning settled synchronously" nil)
                         (setf (mevedel-directive-plan record) replacement
                               (mevedel-session-directive-planning session)
                               (list :directive-id "directive" :attempt 'replacement)))
                       (setq retained (copy-tree (mevedel-directive-plan record)))
                       (error "Late dispatcher error"))))
            (should-error (mevedel-directive-plan-continue directive "Feedback"))
            (should (equal retained (mevedel-directive-plan record)))
            (if ',settle
                (progn
                  (should (eq 'draft (plist-get retained :status)))
                  (should (plist-get retained :cancelled))
                  (should-not (mevedel-session-directive-planning session)))
              (should (eq 'replacement
                          (plist-get (mevedel-session-directive-planning session)
                                     :attempt)))))
        (delete-overlay directive))))
  (settle)
  :doc "an error after synchronous settlement must not resurrect the old proposal"
  t
  :doc "an error after replacement must not roll back the replacement attempt"
  nil)

(provide 'test-mevedel-directive-plan-remote)
;;; test-mevedel-directive-plan-remote.el ends here
