;;; test-mevedel-utilities-response.el --- Response utility tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mevedel-agents)
(require 'mevedel-gptel-stream-bridge)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-utilities)
(require 'org)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--live-buffer-marker-p ()
  ,test
  (test)
  :doc "accepts only live markers in the requested buffer"
  (let ((other (generate-new-buffer " *mevedel-marker-other*")))
    (unwind-protect
        (with-temp-buffer
          (let ((marker (point-marker)))
            (should (mevedel--live-buffer-marker-p marker (current-buffer)))
            (should-not (mevedel--live-buffer-marker-p marker other))
            (set-marker marker nil)
            (should-not
             (mevedel--live-buffer-marker-p marker (current-buffer)))))
      (kill-buffer other))))

(mevedel-deftest mevedel--active-response-marker ()
  ,test
  (test)
  :doc "prefers a live tracking marker and falls back to position"
  (with-temp-buffer
    (let* ((position (point-marker))
           (tracking (copy-marker position))
           (info (list :position position :tracking-marker tracking)))
      (should (eq tracking
                  (mevedel--active-response-marker info (current-buffer))))
      (set-marker tracking nil)
      (should (eq position
                  (mevedel--active-response-marker info (current-buffer))))
      (set-marker position nil)
      (should-not (mevedel--active-response-marker info (current-buffer))))))

(mevedel-deftest mevedel--insert-at-active-response ()
  ,test
  (test)
  :doc "inserts at a running gptel response, before text streamed later"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker))))
      (setq-local mevedel--current-request
                  (mevedel-request--create :fsm (gptel-make-fsm :info info)))
      (gptel-curl--stream-insert-response "before" info)
      (let ((start (mevedel--insert-at-active-response
                    (current-buffer) "RECORD")))
        (should (equal "RECORD" (buffer-substring start (+ start 6))))
        (should (= (+ start 6) (plist-get info :tracking-marker))))
      (gptel-curl--stream-insert-response " after" info)
      (should (equal "beforeRECORD after\n" (buffer-string)))))
  :doc "leaves an idle transcript unchanged"
  (with-temp-buffer
    (insert "settled\n")
    (setq-local mevedel--current-request nil)
    (should-not (mevedel--insert-at-active-response (current-buffer) "RECORD"))
    (should (equal "settled\n" (buffer-string))))
  :doc "leaves a running request without a live response marker unchanged"
  (with-temp-buffer
    (insert "starting\n")
    (setq-local mevedel--current-request
                (mevedel-request--create
                 :fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
    (should-not (mevedel--insert-at-active-response (current-buffer) "RECORD"))
    (should (equal "starting\n" (buffer-string))))
  :doc "uses an external engine context that has no native state machine"
  (with-temp-buffer
    (insert "head")
    (let ((position (point-marker)))
      (insert "\ntail")
      (setq-local mevedel--current-request
                  (mevedel-request--create
                   :context (list :buffer (current-buffer) :position position)))
      (mevedel--insert-at-active-response (current-buffer) "RECORD")
      (should (equal "headRECORD\ntail" (buffer-string)))
      (should (= 11 position))))
  :doc "uses a running agent invocation and ignores a settled one"
  (with-temp-buffer
    (insert "agent")
    (let* ((position (point-marker))
           (invocation
            (mevedel-agent-invocation--create
             :runtime-context (list :buffer (current-buffer)
                                    :position position))))
      (insert "\n")
      (setq-local mevedel--current-request nil)
      (setq-local mevedel--agent-invocation invocation)
      (mevedel--insert-at-active-response (current-buffer) "RECORD")
      (should (equal "agentRECORD\n" (buffer-string)))
      (setf (mevedel-agent-invocation-runtime-settled-p invocation) t)
      (should-not
       (mevedel--insert-at-active-response (current-buffer) "LATE"))
      (should (equal "agentRECORD\n" (buffer-string)))))
  :doc "closes an open gptel reasoning block before the record"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :include-reasoning 'ignore :reasoning-block 'in)))
      (setq-local mevedel--current-request
                  (mevedel-request--create :fsm (gptel-make-fsm :info info)))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((start (mevedel--insert-at-active-response
                    (current-buffer) "RECORD")))
        (goto-char (point-min))
        (should (< (search-forward "#+end_reasoning") start))
        (should-not (plist-get info :reasoning-block)))))
  :doc "closes an open external reasoning block before the record"
  (with-temp-buffer
    (org-mode)
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :include-reasoning 'ignore :reasoning-block 'in
                      :mevedel-acp-reasoning t)))
      (setq-local mevedel--current-request
                  (mevedel-request--create :context info))
      (gptel--display-reasoning-stream "thinking" info)
      (let ((start (mevedel--insert-at-active-response
                    (current-buffer) "RECORD")))
        (goto-char (point-min))
        (should (< (search-forward "#+end_reasoning") start))
        (should (= 1 (mevedel-view-test--count-substring
                      "#+end_reasoning" (buffer-string))))
        (should-not (plist-get info :mevedel-acp-reasoning)))))
  :doc "flushes batched stream text before the record"
  (with-temp-buffer
    (org-mode)
    (setq-local mevedel--session 'test)
    (let* ((mevedel-gptel-stream-bridge-insert-batch-delay 60)
           (info (list :buffer (current-buffer) :position (point-marker)))
           (advice
            #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice)
           (installed
            (advice-member-p advice 'gptel-curl--stream-insert-response)))
      (setq-local mevedel--current-request
                  (mevedel-request--create :fsm (gptel-make-fsm :info info)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response :around advice))
            (gptel-curl--stream-insert-response "first" info)
            (gptel-curl--stream-insert-response " batch" info)
            (mevedel--insert-at-active-response (current-buffer) "RECORD")
            (should (string-prefix-p "first batchRECORD" (buffer-string))))
        (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch info)
        (unless installed
          (advice-remove 'gptel-curl--stream-insert-response advice))))))

(mevedel-deftest mevedel--split-open-reasoning-before-user-input ()
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
        (mevedel--split-open-reasoning-before-user-input info)
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
        (should-error (mevedel--split-open-reasoning-before-user-input info)
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
      (mevedel--split-open-reasoning-before-user-input info)
      (mevedel--insert-user-role-block-at-marker
       "steer directly" (plist-get info :tracking-marker))
      (gptel-curl--stream-insert-response "answer" info)
      (mevedel-transcript-normalize-properties)
      (let ((text (buffer-string)))
        (should
         (= 1
            (mevedel-view-test--count-substring
             "#+begin_reasoning" text)
            (mevedel-view-test--count-substring
             "#+end_reasoning" text))))
      (goto-char (point-min))
      (search-forward "steer directly")
      (should-not
       (get-text-property (match-beginning 0) 'gptel))))

  :doc "batched reasoning close stays before injected user text"
  (with-temp-buffer
    (org-mode)
    (setq-local mevedel--session 'test)
    (let* ((gptel-mode t)
           (mevedel-gptel-stream-bridge-insert-batch-delay 60)
           (info
            (list :buffer (current-buffer)
                  :position (point-marker)
                  :include-reasoning 'ignore
                  :reasoning-block 'in))
           (advice
            #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice)
           (installed
            (advice-member-p advice 'gptel-curl--stream-insert-response)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response
                          :around advice))
            (gptel--display-reasoning-stream "thinking" info)
            (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch
             info)
            (plist-put info :reasoning-marker
                       (copy-marker (plist-get info :tracking-marker) nil))
            (mevedel--split-open-reasoning-before-user-input info)
            (mevedel--insert-user-role-block-at-marker
             "batched steer" (plist-get info :tracking-marker))
            (mevedel-gptel-stream-bridge--flush-gptel-stream-insert-batch
             info)
            (mevedel-transcript-normalize-properties)
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

(provide 'test-mevedel-utilities-response)
;;; test-mevedel-utilities-response.el ends here
