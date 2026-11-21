;;; test-niwa-utilities-response.el --- Response utility tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'niwa-agents)
(require 'niwa-gptel-stream-bridge)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-utilities)
(require 'org)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa--live-buffer-marker-p ()
  ,test
  (test)
  :doc "accepts only live markers in the requested buffer"
  (let ((other (generate-new-buffer " *niwa-marker-other*")))
    (unwind-protect
        (with-temp-buffer
          (let ((marker (point-marker)))
            (should (niwa--live-buffer-marker-p marker (current-buffer)))
            (should-not (niwa--live-buffer-marker-p marker other))
            (set-marker marker nil)
            (should-not
             (niwa--live-buffer-marker-p marker (current-buffer)))))
      (kill-buffer other))))

(niwa-deftest niwa--active-response-marker ()
  ,test
  (test)
  :doc "prefers a live tracking marker and falls back to position"
  (with-temp-buffer
    (let* ((position (point-marker))
           (tracking (copy-marker position))
           (info (list :position position :tracking-marker tracking)))
      (should (eq tracking
                  (niwa--active-response-marker info (current-buffer))))
      (set-marker tracking nil)
      (should (eq position
                  (niwa--active-response-marker info (current-buffer))))
      (set-marker position nil)
      (should-not (niwa--active-response-marker info (current-buffer))))))

(niwa-deftest niwa--insert-at-active-response ()
  ,test
  (test)
  :doc "inserts at a running gptel response, before text streamed later"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker))))
      (setq-local niwa--current-request
                  (niwa-request--create :fsm (gptel-make-fsm :info info)))
      (gptel-curl--stream-insert-response "before" info)
      (let ((start (niwa--insert-at-active-response
                    (current-buffer) "RECORD")))
        (should (equal "RECORD" (buffer-substring start (+ start 6))))
        (should (= (+ start 6) (plist-get info :tracking-marker))))
      (gptel-curl--stream-insert-response " after" info)
      (should (equal "beforeRECORD after\n" (buffer-string)))))
  :doc "leaves an idle transcript unchanged"
  (with-temp-buffer
    (insert "settled\n")
    (setq-local niwa--current-request nil)
    (should-not (niwa--insert-at-active-response (current-buffer) "RECORD"))
    (should (equal "settled\n" (buffer-string))))
  :doc "leaves a running request without a live response marker unchanged"
  (with-temp-buffer
    (insert "starting\n")
    (setq-local niwa--current-request
                (niwa-request--create
                 :fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
    (should-not (niwa--insert-at-active-response (current-buffer) "RECORD"))
    (should (equal "starting\n" (buffer-string))))
  :doc "uses an external engine context that has no native state machine"
  (with-temp-buffer
    (insert "head")
    (let ((position (point-marker)))
      (insert "\ntail")
      (setq-local niwa--current-request
                  (niwa-request--create
                   :context (list :buffer (current-buffer) :position position)))
      (niwa--insert-at-active-response (current-buffer) "RECORD")
      (should (equal "headRECORD\ntail" (buffer-string)))
      (should (= 11 position))))
  :doc "uses a running agent invocation and ignores a settled one"
  (with-temp-buffer
    (insert "agent")
    (let* ((position (point-marker))
           (invocation
            (niwa-agent-invocation--create
             :runtime-context (list :buffer (current-buffer)
                                    :position position))))
      (insert "\n")
      (setq-local niwa--current-request nil)
      (setq-local niwa--agent-invocation invocation)
      (niwa--insert-at-active-response (current-buffer) "RECORD")
      (should (equal "agentRECORD\n" (buffer-string)))
      (setf (niwa-agent-invocation-runtime-settled-p invocation) t)
      (should-not
       (niwa--insert-at-active-response (current-buffer) "LATE"))
      (should (equal "agentRECORD\n" (buffer-string)))))
  :doc "closes an open gptel reasoning block before the record"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :include-reasoning 'ignore :reasoning-block 'in)))
      (setq-local niwa--current-request
                  (niwa-request--create :fsm (gptel-make-fsm :info info)))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((start (niwa--insert-at-active-response
                    (current-buffer) "RECORD")))
        (goto-char (point-min))
        (should (< (search-forward "#+end_reasoning") start))
        (should-not (plist-get info :reasoning-block)))))
  :doc "closes an open external reasoning block before the record"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :include-reasoning 'ignore :reasoning-block 'in
                      :niwa-acp-reasoning t)))
      (setq-local niwa--current-request
                  (niwa-request--create :context info))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((start (niwa--insert-at-active-response
                    (current-buffer) "RECORD")))
        (goto-char (point-min))
        (should (< (search-forward "#+end_reasoning") start))
        (should (= 1 (niwa-view-test--count-substring
                      "#+end_reasoning" (buffer-string))))
        (should-not (plist-get info :niwa-acp-reasoning)))))
  :doc "flushes batched stream text before the record"
  (with-temp-buffer
    (org-mode)
    (setq-local niwa--session 'test)
    (let* ((niwa-gptel-stream-bridge-insert-batch-delay 60)
           (info (list :buffer (current-buffer) :position (point-marker)))
           (advice
            #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
           (installed
            (advice-member-p advice 'gptel-curl--stream-insert-response)))
      (setq-local niwa--current-request
                  (niwa-request--create :fsm (gptel-make-fsm :info info)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response :around advice))
            (gptel-curl--stream-insert-response "first" info)
            (gptel-curl--stream-insert-response " batch" info)
            (niwa--insert-at-active-response (current-buffer) "RECORD")
            (should (string-prefix-p "first batchRECORD" (buffer-string))))
        (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
        (unless installed
          (advice-remove 'gptel-curl--stream-insert-response advice))))))

(niwa-deftest niwa--split-open-reasoning-before-user-input ()
  ,test
  (test)

  :doc "closes and resets an active streamed reasoning block"
  (with-temp-buffer
    (org-mode)
    (let* ((gptel-mode t)
           (info
            (list :buffer (current-buffer)
                  :position (point-marker)
                  :include-reasoning 'ignore
                  :reasoning-block 'in)))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((old-marker (plist-get info :reasoning-marker)))
        (niwa--split-open-reasoning-before-user-input info)
        (should (string-match-p
                 (regexp-quote "#+end_reasoning")
                 (buffer-string)))
        (should-not (marker-position old-marker))
        (should-not (plist-get info :reasoning-marker))
        (should-not (plist-get info :reasoning-block)))))

  :doc "locks the tracking marker when a closing-fence stream hook signals"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :include-reasoning 'ignore :reasoning-block 'in)))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((gptel-post-stream-hook
             (list (lambda () (error "Injected stream hook failure")))))
        (should-error (niwa--split-open-reasoning-before-user-input info)
                      :type 'error))
      (should-not
       (marker-insertion-type (plist-get info :tracking-marker)))
      (should (eq (plist-get info :reasoning-block) 'in))))

  :doc "direct response after the split does not add another reasoning close"
  (with-temp-buffer
    (org-mode)
    (let* ((gptel-mode t)
           (info
            (list :buffer (current-buffer)
                  :position (point-marker)
                  :include-reasoning 'ignore
                  :reasoning-block 'in)))
      (gptel--display-reasoning-stream "thinking" info)
      (niwa--split-open-reasoning-before-user-input info)
      (niwa--insert-user-role-block-at-marker
       "steer directly" (plist-get info :tracking-marker))
      (gptel-curl--stream-insert-response "answer" info)
      (niwa-transcript-normalize-properties)
      (let ((text (buffer-string)))
        (should
         (= 1
            (niwa-view-test--count-substring
             "#+begin_reasoning" text)
            (niwa-view-test--count-substring
             "#+end_reasoning" text))))
      (goto-char (point-min))
      (search-forward "steer directly")
      (should-not
       (get-text-property (match-beginning 0) 'gptel))))

  :doc "batched reasoning close stays before injected user text"
  (with-temp-buffer
    (org-mode)
    (setq-local niwa--session 'test)
    (let* ((gptel-mode t)
           (niwa-gptel-stream-bridge-insert-batch-delay 60)
           (info
            (list :buffer (current-buffer)
                  :position (point-marker)
                  :include-reasoning 'ignore
                  :reasoning-block 'in))
           (advice
            #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice)
           (installed
            (advice-member-p advice 'gptel-curl--stream-insert-response)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response
                          :around advice))
            (gptel--display-reasoning-stream "thinking" info)
            (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch
             info)
            (plist-put info :reasoning-marker
                       (copy-marker (plist-get info :tracking-marker) nil))
            (niwa--split-open-reasoning-before-user-input info)
            (niwa--insert-user-role-block-at-marker
             "batched steer" (plist-get info :tracking-marker))
            (niwa-gptel-stream-bridge--flush-gptel-stream-insert-batch
             info)
            (niwa-transcript-normalize-properties)
            (goto-char (point-min))
            (let ((reasoning-end
                   (search-forward "#+end_reasoning" nil t))
                  (steering
                   (progn
                     (goto-char (point-min))
                     (search-forward "batched steer" nil t))))
              (should reasoning-end)
              (should steering)
              (should (< reasoning-end steering))
              (should-not
               (get-text-property (- steering (length "batched steer"))
                                  'gptel))))
        (unless installed
          (advice-remove 'gptel-curl--stream-insert-response advice))))))

(provide 'test-niwa-utilities-response)
;;; test-niwa-utilities-response.el ends here
