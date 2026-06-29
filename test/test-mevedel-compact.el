;;; test-mevedel-compact.el --- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-compact)
(require 'mevedel-models)
(require 'mevedel-hooks)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-model)
(defvar gptel-max-tokens)
(defvar gptel-backend)
(defvar gptel--request-params)

(mevedel-deftest mevedel--compact-previous-summary ()
  ,test
  (test)
  :doc "strips the persisted handoff prefix before reusing a summary"
  (with-temp-buffer
    (insert "#+begin_summary\n"
            mevedel-session-persistence--summary-handoff-prefix
            "## Goal\n- Continue\n"
            "#+end_summary\n")
    (should (equal "## Goal\n- Continue"
                   (mevedel--compact-previous-summary)))))

(mevedel-deftest mevedel--file-local-variables-start ()
  ,test
  (test)
  :doc "returns nil when no file-local variables"
  (with-temp-buffer
    (insert "Hello world\n")
    (should (null (mevedel--file-local-variables-start))))

  :doc "detects elisp-style file-local variables"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "detects markdown-style file-local variables"
  (with-temp-buffer
    (insert "# Markdown content\n\n")
    (let ((start (point)))
      (insert "<!-- Local Variables: -->\n")
      (insert "<!-- gptel-model: \"test\" -->\n")
      (insert "<!-- End: -->\n")
      (should (= (mevedel--file-local-variables-start) start))))

  :doc "finds first Local Variables block when multiple exist"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((first-start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n\n")
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test2\"\n")
      (insert ";; End:\n")
      (should (= (mevedel--file-local-variables-start) first-start)))))

(mevedel-deftest mevedel--estimate-tokens ()
  ,test
  (test)
  :doc "counts tokens without file-local variables"
  (with-temp-buffer
    (insert "Hello world")  ; 11 chars / 4 = 2 tokens
    (should (= (mevedel--estimate-tokens) 2)))

  :doc "excludes elisp-style file-local variables from count"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"claude-sonnet-4\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))

  :doc "excludes markdown-style file-local variables from count"
  (with-temp-buffer
    (insert "# Title\n\nContent\n\n")  ; 18 chars / 4 = 4 tokens
    (insert "<!-- Local Variables: -->\n")
    (insert "<!-- gptel-model: \"test\" -->\n")
    (insert "<!-- End: -->\n")
    (should (= (mevedel--estimate-tokens) 4)))

  :doc "respects gptel ignore property"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world")
    (should (= (mevedel--estimate-tokens) 2)))  ; "Hello world" = 11 chars / 4 = 2 tokens

  :doc "combines gptel ignore property and file-local variables exclusion"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3)))  ; "Hello world\n\n" = 13 chars / 4 = 3 tokens

  :doc "excludes all file-local variables when multiple blocks exist"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test2\"\n")
    (insert ";; End:\n")
    (should (= (mevedel--estimate-tokens) 3))))

(mevedel-deftest mevedel--compact-estimate-data-tokens ()
  ,test
  (test)
  :doc "keeps chars/4 behavior for plain realized data"
  (should (= (mevedel--compact-estimate-data-tokens
              (list :messages
                    (vector (list :role "user"
                                  :content "abcdefgh"))))
             3))

  :doc "counts OpenAI Responses input_image data URLs as image tokens"
  (let* ((mevedel-compact-image-token-estimate 123)
         (prefix "data:image/png;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :type "input_image" :image_url url))
         (expected (+ mevedel-compact-image-token-estimate
                      (/ (+ (length "input_image") (length prefix)) 4))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               expected)))

  :doc "counts OpenAI chat image_url data URLs as image tokens"
  (let* ((mevedel-compact-image-token-estimate 123)
         (prefix "data:image/jpeg;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :type "image_url"
                     :image_url (list :url url)))
         (expected (+ mevedel-compact-image-token-estimate
                      (/ (+ (length "image_url") (length prefix)) 4))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               expected)))

  :doc "counts Anthropic base64 image blocks as image tokens"
  (let* ((mevedel-compact-image-token-estimate 123)
         (data (list :type "image"
                     :source (list :type "base64"
                                   :media_type "image/png"
                                   :data (make-string 4000 ?A))))
         (expected (+ mevedel-compact-image-token-estimate
                      (/ (+ (length "image")
                            (length "base64")
                            (length "image/png"))
                         4))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               expected)))

  :doc "counts Bedrock image bytes blocks as image tokens"
  (let* ((mevedel-compact-image-token-estimate 123)
         (data (list :image (list :format "png"
                                  :source (list :bytes
                                                (make-string 4000 ?A)))))
         (expected (+ mevedel-compact-image-token-estimate
                      (/ (length "png") 4))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               expected)))

  :doc "adds image estimates for multiple images"
  (let* ((mevedel-compact-image-token-estimate 123)
         (prefix "data:image/png;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :content
                     (vector "abcd"
                             (list :type "input_image" :image_url url)
                             (list :type "input_image" :image_url url))))
         (expected (+ (* 2 mevedel-compact-image-token-estimate)
                      (/ (+ (length "abcd")
                            (* 2 (+ (length "input_image")
                                    (length prefix))))
                         4))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               expected)))

  :doc "leaves non-image data URLs as text"
  (let* ((url (concat "data:text/plain;base64,"
                      (make-string 400 ?A)))
         (data (list :url url)))
    (should (= (mevedel--compact-estimate-data-tokens data)
               (/ (length url) 4))))

  :doc "leaves quoted image data URLs as text"
  (let* ((url (concat "data:image/png;base64,"
                      (make-string 400 ?A)))
         (data (list :content url)))
    (should (= (mevedel--compact-estimate-data-tokens data)
               (/ (length url) 4))))

  :doc "leaves unrelated base64 strings as text"
  (let ((payload (make-string 400 ?A)))
    (should (= (mevedel--compact-estimate-data-tokens
                (list :data payload))
               (/ (length payload) 4))))

  :doc "falls back to text counting for malformed image blocks"
  (let ((data (list :type "image"
                    :source (list :type "base64"
                                  :media_type "image/png"))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               (/ (+ (length "image")
                     (length "base64")
                     (length "image/png"))
                  4))))

  :doc "falls back to text counting for malformed Bedrock image blocks"
  (let* ((payload (make-string 400 ?A))
         (data (list :image (list :source (list :bytes payload)))))
    (should (= (mevedel--compact-estimate-data-tokens data)
               (/ (length payload) 4)))))

(mevedel-deftest mevedel--compact-record-token-baseline ()
  ,test
  (test)
  :doc "records gptel-reported tokens as estimate baseline"
  (with-temp-buffer
    (insert "abcd")
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens '(:input 10
                                       :cached 5
                                       :output 7)
                             :tokens-full '(:input 10
                                            :cached 5
                                            :output 7)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (= (plist-get mevedel--known-token-baseline :tokens) 22))
      (should (= (plist-get mevedel--known-token-baseline :input-tokens) 15))
      (should (= (plist-get mevedel--known-token-baseline :output-tokens) 7))
      (goto-char (point-max))
      (insert "abcdefgh")
      (should (= (mevedel--estimate-tokens) 24))))

  :doc "prefers latest request tokens over cumulative tokens-full"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens '(:input 10 :output 5)
                             :tokens-full '(:input 1000 :output 500)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (= (plist-get mevedel--known-token-baseline :tokens) 15))
      (should (= (plist-get mevedel--known-token-baseline :input-tokens) 10))
      (should (= (plist-get mevedel--known-token-baseline :output-tokens) 5))))

  :doc "falls back to tokens-full when latest request tokens are absent"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens-full '(:input 10 :output 7)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (= (plist-get mevedel--known-token-baseline :tokens) 17))))

  :doc "ignores compaction request token usage"
  (with-temp-buffer
    (insert "abcd")
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :context '(:mevedel-compaction t)
                             :tokens-full '(:input 10 :output 7)))))
      (mevedel--compact-record-token-baseline fsm)
      (should (null mevedel--known-token-baseline)))))

(mevedel-deftest mevedel--compact-queue-file-reference-reminder
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "queues touched file references whose turns fall outside the preserved tail"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/p/" "/tmp/p/" "p"))
         (session (mevedel-session-create "main" ws)))
    (let ((mevedel-compact-tail-turns 2))
      (setf (mevedel-session-turn-count session) 10)
      (puthash "/tmp/p/old.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/old.el" :read-turn 4)
               (mevedel-session-touched-files session))
      (puthash "/tmp/p/boundary.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/boundary.el" :read-turn 8)
               (mevedel-session-touched-files session))
      (puthash "/tmp/p/recent.el"
               (mevedel-file-interaction--create
                :path "/tmp/p/recent.el" :read-turn 9)
               (mevedel-session-touched-files session))
      (mevedel--compact-queue-file-reference-reminder session 2)
      (let ((body (car (mevedel-session-pending-reminders session))))
        (should (string-match-p "/tmp/p/old.el" body))
        (should-not (string-match-p "/tmp/p/boundary.el" body))
        (should-not (string-match-p "/tmp/p/recent.el" body)))))

  :doc "aggressive compaction queues even recent touched file references"
  (let* ((ws (mevedel-workspace-get-or-create 'project "/tmp/q/" "/tmp/q/" "q"))
         (session (mevedel-session-create "main" ws)))
    (setf (mevedel-session-turn-count session) 10)
    (puthash "/tmp/q/recent.el"
             (mevedel-file-interaction--create
              :path "/tmp/q/recent.el" :read-turn 9)
             (mevedel-session-touched-files session))
    (mevedel--compact-queue-file-reference-reminder session 0)
    (let ((body (car (mevedel-session-pending-reminders session))))
      (should (string-match-p "/tmp/q/recent.el" body)))))

(mevedel-deftest mevedel--compact-transform-auto ()
  ,test
  (test)
  :doc "successful auto-compaction preserves later prompt-buffer transforms"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (setq-local mevedel--view-buffer nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending prompt\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                       (lambda (&optional _token-estimate) t))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (with-current-buffer prompt-buf
                           (goto-char (point-min))
                           (insert "Late prefix\n")
                           (when-let* ((start (mevedel--compact-find-boundary)))
                             (goto-char start)
                             (insert "Late context\n\n")))
                         (with-current-buffer source-buf
                           (setq-local mevedel--compact-current-request-reminder
                                       "Re-read /tmp/old.el")
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "#+begin_summary\nSummary\n#+end_summary\n")
                             (insert "Tail prompt\n")
                             (insert (propertize "Tail response\n"
                                                 'gptel 'response))
                             (insert "Pending prompt\n")))
                         (funcall (plist-get args :callback) nil))))
              (with-current-buffer prompt-buf
                (let ((gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (with-current-buffer prompt-buf
            (let ((text (buffer-string)))
              (should continued)
              (should (string-match-p "Summary" text))
              (should (string-match-p "Late prefix" text))
              (should (string-match-p "Late context" text))
              (should (string-match-p "<system-reminder>\nRe-read /tmp/old.el"
                                      text))
              (should-not (string-match-p "Old prompt" text))
              (should (string-match-p "Pending prompt" text))))
          (with-current-buffer source-buf
            (should-not mevedel--compact-current-request-reminder)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(mevedel-deftest mevedel--compact-transform-auto-threshold ()
  ,test
  (test)
  :doc "auto threshold uses source-buffer API baseline"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (ran nil)
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Small pending\n")
            (setq-local mevedel--known-token-baseline
                        (list :tokens 100
                              :position (copy-marker (point-max)))))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert "tiny\n"))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (setq ran t)
                         (funcall (plist-get args :callback) :skip))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-token-threshold 50)
                      (gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (should ran)
          (should continued))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "auto threshold uses transformed prompt buffer size"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (ran nil)
        (continued nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Small pending\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf)
            (insert (make-string 400 ?x)))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (setq ran t)
                         (funcall (plist-get args :callback) :skip))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-token-threshold 50)
                      (gptel-backend nil)
                      (gptel-model nil)
                      (gptel-max-tokens nil)
                      (gptel--request-params nil))
                  (mevedel--compact-transform-auto
                   (lambda () (setq continued t))
                   fsm)))))
          (should ran)
          (should continued))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf)))))

(mevedel-deftest mevedel--compact-rebuild-info-data-from-buffer ()
  ,test
  (test)
  :doc "injects current-request reminder before realizing continuation data"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-rebuild*"))
        captured)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (org-mode)
            (setq-local mevedel--compact-current-request-reminder
                        "Re-read /tmp/old.el")
            (insert "Prompt\n")
            (let ((response-start (point)))
              (insert "Response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "Tool result\n"))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer chat-buf
                                  :data 'old-data))))
            (cl-letf (((symbol-function 'gptel--create-prompt-buffer)
                       (lambda (&optional _prompt-end)
                         (let ((buf (generate-new-buffer
                                     " *mevedel-compact-realize*")))
                           (with-current-buffer buf
                             (org-mode)
                             (insert-buffer-substring chat-buf))
                           buf)))
                      ((symbol-function 'gptel--realize-query)
                       (lambda (realize-fsm)
                         (let ((prompt-buffer
                                (plist-get (gptel-fsm-info realize-fsm)
                                           :data)))
                           (with-current-buffer prompt-buffer
                             (setq captured (buffer-string)))
                           (plist-put (gptel-fsm-info realize-fsm)
                                      :data 'realized)))))
              (mevedel--compact-rebuild-info-data-from-buffer fsm chat-buf)
              (should (eq (plist-get (gptel-fsm-info fsm) :data) 'realized))
              (should (string-match-p
                       "<system-reminder>\nRe-read /tmp/old.el"
                       captured))
              (with-current-buffer chat-buf
                (should-not mevedel--compact-current-request-reminder)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(mevedel-deftest mevedel--compact-handle-wait ()
  ,test
  (test)
  :doc "does not run continuation gate on initial INIT to WAIT"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (checked nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(INIT)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq checked t)
                       t)))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 1))
          (should-not checked))
      (kill-buffer chat-buf)))

  :doc "sends continuation directly when realized data is below threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content "small")))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest _args) (setq ran t))))
            (let ((mevedel-compact-token-threshold 50))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "does not compact continuation when image payload is below media-aware threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let* ((prefix "data:image/png;base64,")
               (url (concat prefix (make-string 400 ?A)))
               (fsm (gptel-make-fsm
                     :info (list :buffer chat-buf
                                 :history '(TRET)
                                 :data (list :messages
                                             (vector
                                              (list :role "user"
                                                    :content
                                                    (vector
                                                     (list :type "input_image"
                                                           :image_url
                                                           url)))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest _args) (setq ran t))))
            (let ((mevedel-compact-token-threshold 50)
                  (mevedel-compact-image-token-estimate 10))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "runs continuation gate on TRET to WAIT over threshold"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-auto-eligible-p)
                     (lambda () t))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest args)
                       (setq ran t)
                       (funcall (plist-get args :callback) :skip))))
            (let ((mevedel-compact-token-threshold 50))
              (mevedel--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "suppresses original wait handler until compaction succeeds"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (rebuilt nil)
        callback)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens) t))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest args)
                       (setq callback (plist-get args :callback))))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (_fsm _chat-buffer)
                       (setq rebuilt t))))
            (mevedel--compact-handle-wait fsm)
            (should (= sent 0))
            (should callback)
            (funcall callback nil))
          (should rebuilt)
          (should (= sent 1)))
      (kill-buffer chat-buf)))

  :doc "does not send continuation when compaction fails"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        warning)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data (list :messages
                                            (vector
                                             (list :role "user"
                                                   :content
                                                   (make-string 400 ?x))))))))
          (with-current-buffer chat-buf
            (insert "Prompt\n")
            (insert (propertize "Response\n" 'gptel 'response))
            (insert "Tool result\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens) t))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest args)
                       (funcall (plist-get args :callback) "boom")))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer)
                       (setq warning message)))
                    ((symbol-function 'gptel--update-status)
                     #'ignore))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p
                   "Auto-compaction failed; request not sent: boom"
                   warning)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--compact-run ()
  ,test
  (test)
  :doc "manual compaction failure stops the view spinner"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-chat*"))
        (view-buf (generate-new-buffer " *mevedel-compact-view*"))
        (updated-status nil)
        (stopped nil))
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--view-buffer view-buf)
          (setq-local mevedel--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'mevedel-view--update-spinner)
                     (lambda (status)
                       (setq updated-status status)))
                    ((symbol-function 'mevedel-view--stop-spinner)
                     (lambda ()
                       (setq stopped t)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test")))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest args)
                       (apply function args)
                       nil))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (funcall (plist-get args :callback) 'abort nil))))
            (mevedel--compact-run :aggressive t :pending-start (point-max)))
	  (should (equal updated-status "Compacting..."))
	  (should stopped)
	  (should-not mevedel--compaction-in-flight))
      (when (buffer-live-p chat-buf)
	(kill-buffer chat-buf))
      (when (buffer-live-p view-buf)
	(kill-buffer view-buf)))))

  :doc "passes gptel-stream through to the compaction request"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-stream*"))
        captured-stream)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (setq-local gptel-stream t)
          (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :stream nil)))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (setq captured-stream (plist-get args :stream))
                       (funcall (plist-get args :callback) 'abort nil))))
            (mevedel--compact-run :aggressive t :pending-start (point-max)))
          (should (eq captured-stream t))
          (should-not mevedel--compaction-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "streaming compaction applies the accumulated summary at completion"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-stream-callback*"))
        applied-summary
        observations)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (setq-local gptel-stream t)
          (let ((mevedel-compact-warn-on-completion nil))
            (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                       (lambda (&rest _)
                         "system prompt"))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test")))
                      ((symbol-function 'mevedel--compact-apply)
                       (lambda (_boundary summary &rest _)
                         (setq applied-summary summary)))
                      ((symbol-function 'mevedel-hooks-run-event)
                       (lambda (_event _plist callback &rest _)
                         (funcall callback nil)))
                      ((symbol-function 'message)
                       #'ignore)
                      ((symbol-function 'display-warning)
                       #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (let ((callback (plist-get args :callback))
                               (info '(:stream t)))
                           (funcall callback "summary " info)
                           (push (cons 'after-first applied-summary)
                                 observations)
                           (funcall callback "text" info)
                           (push (cons 'after-second applied-summary)
                                 observations)
                           (funcall callback t info)))))
              (mevedel--compact-run :aggressive t :pending-start (point-max))))
          (should (null (alist-get 'after-first observations)))
          (should (null (alist-get 'after-second observations)))
          (should (equal applied-summary "summary text"))
          (should-not mevedel--compaction-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "uses compaction workload tier for the gptel request"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-workload*"))
        (captured-workload nil)
        (captured-selector nil)
        (captured-noerror nil)
        (captured-backend nil)
        (captured-model nil))
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (setq-local gptel-backend 'current-backend)
          (setq-local gptel-model 'current-model)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test")))
                    ((symbol-function 'mevedel-model-workload-default-selector)
                     (lambda (workload)
                       (setq captured-workload workload)
                       '(:tier balanced)))
                    ((symbol-function 'mevedel-model-resolve-selector)
                     (lambda (selector &optional noerror)
                       (setq captured-selector selector
                             captured-noerror noerror)
                       '(:backend workload-backend :model workload-model)))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (setq captured-backend gptel-backend
                             captured-model gptel-model)
                       (funcall (plist-get args :callback) 'abort nil))))
            (mevedel--compact-run :aggressive t :pending-start (point-max)))
          (should (eq captured-workload 'compaction))
          (should (equal captured-selector '(:tier balanced)))
          (should captured-noerror)
          (should (eq captured-backend 'workload-backend))
          (should (eq captured-model 'workload-model))
          (should-not mevedel--compaction-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "async PreCompact hook marks compaction in flight before request"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-prehook*"))
        hook-callback
        request-called)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'gptel-request)
                     (lambda (&rest _)
                       (setq request-called t)))
                    ((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _plist callback &rest _)
                       (setq hook-callback callback)))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore))
            (mevedel--compact-run :aggressive t :pending-start (point-max))
            (should mevedel--compaction-in-flight)
            (should hook-callback)
            (should-not request-called)
            (should-error
             (mevedel--compact-run :aggressive t :pending-start (point-max))
             :type 'user-error)
            (funcall hook-callback
                     '(:continue nil :stop-reason "blocked"))
            (should-not mevedel--compaction-in-flight)))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

(mevedel-deftest mevedel--compact-run-auto-file-reference-reminder ()
  ,test
  (test)
  :doc "auto compaction keeps file-reference reminder out of pending FIFO"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-auto-reminder*"))
        queued)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (let ((mevedel-compact-warn-on-completion nil))
            (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                       (lambda (&rest _)
                         "system prompt"))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test")))
                      ((symbol-function 'mevedel--compact-apply)
                       #'ignore)
                      ((symbol-function 'mevedel--compact-file-reference-reminder-body)
                       (lambda (&rest _)
                         "Re-read /tmp/old.el"))
                      ((symbol-function 'mevedel-session-enqueue-pending-reminder)
                       (lambda (_session body)
                         (setq queued body)))
                      ((symbol-function 'mevedel-hooks-run-event)
                       (lambda (_event _plist callback &rest _)
                         (funcall callback nil)))
                      ((symbol-function 'message)
                       #'ignore)
                      ((symbol-function 'display-warning)
                       #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (funcall (plist-get args :callback)
                                  "summary" nil))))
              (mevedel--compact-run
               :aggressive t
               :pending-start (point-max)
               :auto t)))
          (should (equal mevedel--compact-current-request-reminder
                         "Re-read /tmp/old.el"))
          (should-not queued))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(mevedel-deftest mevedel--compact-context-limit ()
  ,test
  (test)
  :doc "prefers explicit override"
  (let ((mevedel-compact-context-limit 12345)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 200)
    (should (= (mevedel--compact-context-limit) 12345)))

  :doc "uses gptel model context-window in thousands of tokens"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 8.192)
    (should (= (mevedel--compact-context-limit) 8192)))

  :doc "falls back when model has no context-window"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model-no-context))
    (put 'mevedel-test-model-no-context :context-window nil)
    (should (= (mevedel--compact-context-limit) 200000))))

(mevedel-deftest mevedel--compact-current-persisted-p ()
  ,test
  (test)
  :doc "requires current buffer to be the session's active segment"
  (let* ((tempdir (make-temp-file "mevedel-compact-persisted-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "compact-persisted" tempdir "compact-persisted"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (setf (mevedel-session-current-segment session) 2)
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-persistence--segment-path tempdir 2))
            (setq-local mevedel--session session)
            (should (mevedel--compact-current-persisted-p)))
          (with-temp-buffer
            (setq buffer-file-name
                  (mevedel-session-persistence--segment-path tempdir 1))
            (setq-local mevedel--session session)
            (should-not (mevedel--compact-current-persisted-p))))
      (mevedel-workspace-clear-registry)
      (delete-directory tempdir t))))

(mevedel-deftest mevedel--compact-threshold-tokens ()
  ,test
  (test)
  :doc "integer threshold is absolute"
  (let ((mevedel-compact-token-threshold 150000))
    (should (= (mevedel--compact-threshold-tokens) 150000)))

  :doc "float threshold applies to usable context"
  (let ((mevedel-compact-context-limit 200000)
        (mevedel-compact-reserve-tokens 20000)
        (mevedel-compact-token-threshold 0.8)
        (gptel-max-tokens nil))
    (should (= (mevedel--compact-threshold-tokens) 144000)))

  :doc "reserve is capped on small context windows"
  (let ((mevedel-compact-context-limit 8000)
        (mevedel-compact-reserve-tokens 20000)
        (mevedel-compact-token-threshold 0.8)
        (gptel-max-tokens nil))
    (should (= (mevedel--compact-usable-tokens) 4000))
    (should (= (mevedel--compact-threshold-tokens) 3200))))

(mevedel-deftest mevedel--compact-turn-starts-before ()
  ,test
  (test)
  :doc "ignores leading org metadata when finding turn starts"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:foo: bar\n:END:\n")
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (let ((u2-start (point)))
        (insert "u2\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel--compact-turn-starts-before (point-max))
                       (list u1-start u2-start))))))

  :doc "does not count prompt text after limit in a widened user span"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (insert "\n  ")
      (let ((limit (point)))
        (insert "u2\n")
        (should (equal (mevedel--compact-turn-starts-before limit)
                       (list u1-start))))))
  :doc "skips unpropertized reasoning and tool scaffolding before prompt"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (insert "#+begin_reasoning\nThinking text.\n")
      (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
      (let ((tool-start (point)))
        (insert "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\n"
                "body\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call_1")))
      (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
      (let ((u2-start (point)))
        (insert "u2\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel--compact-turn-starts-before (point-max))
                       (list u1-start u2-start))))))
  :doc "keeps user-authored org block marker as turn start"
  (with-temp-buffer
    (let ((u1-start (point)))
      (insert "u1\n")
      (let ((a1-start (point)))
        (insert "a1\n")
        (put-text-property a1-start (point) 'gptel 'response))
      (let ((u2-start (point)))
        (insert "#+begin_src emacs-lisp\n")
        (insert "(message \"hello\")\n")
        (insert "#+end_src\n")
        (let ((a2-start (point)))
          (insert "a2\n")
          (put-text-property a2-start (point) 'gptel 'response))
        (should (equal (mevedel--compact-turn-starts-before (point-max))
                       (list u1-start u2-start)))))))

(mevedel-deftest mevedel--compact-tail-start ()
  ,test
  (test)
  :doc "keeps configured recent response turns when budget allows"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((after-a1 (point)))
      (insert "u2\n")
      (let ((a2-start (point)))
        (insert "a2\n")
        (put-text-property a2-start (point) 'gptel 'response))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert "a3\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-compact-context-limit 200000)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.25))
        (should (= (mevedel--compact-tail-start (point-max) nil)
                   after-a1)))))

  :doc "drops older preserved turns when tail budget would be exceeded"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (insert "u2\n")
    (let ((a2-start (point)))
      (insert (make-string 40 ?a) "\n")
      (put-text-property a2-start (point) 'gptel 'response))
    (let ((after-a2 (point)))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert (make-string 40 ?b) "\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-compact-context-limit 100)
            (mevedel-compact-reserve-tokens 20)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.01))
        (should (= (mevedel--compact-tail-start (point-max) nil)
                   after-a2)))))

  :doc "drops older turn even when session has only target turn count"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert (make-string 40 ?a) "\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((after-a1 (point)))
      (insert "u2\n")
      (let ((a2-start (point)))
        (insert (make-string 40 ?b) "\n")
        (put-text-property a2-start (point) 'gptel 'response))
	  (let ((mevedel-compact-context-limit 100)
	        (mevedel-compact-reserve-tokens 20)
	        (mevedel-compact-tail-turns 2)
	        (mevedel-compact-tail-budget 0.01))
	    (should (= (mevedel--compact-tail-start (point-max) nil)
	               after-a1)))))

  :doc "keeps tool-using response chunks inside the same turn"
  (with-temp-buffer
    (insert "u1\n")
    (let ((a1-start (point)))
      (insert "a1\n")
      (put-text-property a1-start (point) 'gptel 'response))
    (let ((u2-start (point)))
      (insert "u2\n")
      (let ((a2a-start (point)))
        (insert "a2 part 1\n")
        (put-text-property a2a-start (point) 'gptel 'response))
      (let ((tool-start (point)))
        (insert "tool result\n")
        (put-text-property tool-start (point) 'gptel '(tool . result)))
      (let ((a2b-start (point)))
        (insert "a2 part 2\n")
        (put-text-property a2b-start (point) 'gptel 'response))
      (insert "u3\n")
      (let ((a3-start (point)))
        (insert "a3\n")
        (put-text-property a3-start (point) 'gptel 'response))
      (let ((mevedel-compact-context-limit 200000)
            (mevedel-compact-tail-turns 2)
            (mevedel-compact-tail-budget 0.25))
        (should (= (mevedel--compact-tail-start (point-max) nil)
                   u2-start))))))

(mevedel-deftest mevedel--compact-pending-text-from-prompt-buffer ()
  ,test
  (test)
  :doc "uses prompt-buffer response boundary after inserted reminders"
  (with-temp-buffer
    (insert "old user\n")
    (let ((response-start (point)))
      (insert "old response\n")
      (put-text-property response-start (point) 'gptel 'response))
    (insert "<system-reminder>\nexpanded reminder\n</system-reminder>\n")
    (insert "new user prompt\n")
    (let ((text (mevedel--compact-pending-text-from-prompt-buffer)))
      (should (string-prefix-p "<system-reminder>" text))
      (should (string-match-p "expanded reminder" text))
      (should (string-match-p "new user prompt" text)))))

(mevedel-deftest mevedel--compact-region-with-tool-output-cap ()
  ,test
  (test)
  :doc "caps tool output spans while preserving surrounding text"
  (with-temp-buffer
    (insert "before\n")
    (let ((tool-start (point)))
      (insert "abcdef")
      (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
    (insert "\nafter\n")
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 3 t)))
      (should (string-match-p "before" text))
      (should (string-match-p "abc" text))
      (should-not (string-match-p "def" text))
      (should (string-match-p "omitted 3 chars" text))
      (should (string-match-p "after" text))))

  :doc "restores tool property only on parseable org tool sexp and result"
  (with-temp-buffer
    (insert "#+begin_tool (Bash :command \"date\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Bash\" :args (:command \"date\"))\n\nresult\n")
      (let ((tool-end (point)))
        (insert "#+end_tool\n")
        (put-text-property tool-start tool-end 'gptel '(tool . "call-date"))))
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (let* ((sexp-start (progn
                             (goto-char (point-min))
                             (search-forward "(:name")
                             (match-beginning 0)))
               (suffix-start (progn
                               (goto-char (point-min))
                               (search-forward "#+end_tool")
                               (match-beginning 0))))
          (should-not (eq (car-safe (get-text-property (point-min) 'gptel))
                          'tool))
          (should (equal (get-text-property sexp-start 'gptel)
                         '(tool . "call-date")))
          (should-not (eq (car-safe (get-text-property suffix-start 'gptel))
                          'tool))
          (goto-char sexp-start)
          (should (equal "Bash" (plist-get (read (current-buffer)) :name)))))))

  :doc "does not restore tool property over unparseable org tool scaffolding"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\nnot a sexp\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (should-not (eq (car-safe (get-text-property (point) 'gptel))
                          'tool))
          (goto-char (next-single-property-change (point) 'gptel nil
                                                  (point-max)))))))

  :doc "does not restore tool property over unparseable Lisp-looking spans"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "(:name \"Bash\" :args")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (should-not (eq (car-safe (get-text-property (point-min) 'gptel))
                        'tool)))))

  :doc "ignores stray readable sexps inside malformed org tool result text"
  (with-temp-buffer
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\n"
              "result mentions (:name \"Fake\")\n"
              "#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-bad")))
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 1000 nil)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (should-not (eq (car-safe (get-text-property (point) 'gptel))
                          'tool))
          (goto-char (next-single-property-change (point) 'gptel nil
                                                  (point-max)))))))

  :doc "keeps large Edit tool arguments readable when truncation lands inside args"
  (with-temp-buffer
    (let ((large-arg (make-string 2000 ?x)))
      (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
      (let ((tool-start (point)))
        (insert (prin1-to-string
                 (list :name "Edit"
                       :args (list :file_path "mevedel-chat.el"
                                   :old_string large-arg
                                   :new_string large-arg))))
        (insert "\n\nEdited mevedel-chat.el (+1 -1)\n#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call-edit"))))
    (let* ((text (mevedel--compact-region-with-tool-output-cap
                  (point-min) (point-max) 200 t))
           (sexp-start (string-match "(:name" text))
           (read-result (read-from-string text sexp-start))
           (sexp (car read-result)))
      (should (equal "Edit" (plist-get sexp :name)))
      (should (equal "mevedel-chat.el" (plist-get (plist-get sexp :args)
                                                  :file_path)))
      (should (string-match-p "string argument truncated" text))
      (should (string-match-p "^#\\+end_tool" text))))

  :doc "keeps the org tool close marker after truncating a large result body"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"big.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"big.txt\"))\n\n")
      (insert (make-string 500 ?r))
      (insert "\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let ((text (mevedel--compact-region-with-tool-output-cap
                 (point-min) (point-max) 80 t)))
      (should (string-match-p "tool output truncated" text))
      (should (string-match-p "\n#\\+end_tool\n\\'" text))))

  :doc "shortens large args in unpropertied org tool headers"
  (with-temp-buffer
    (let* ((large-arg (make-string 2000 ?x))
           (header-form (list 'Edit :file_path "mevedel-chat.el"
                              :old_string large-arg)))
      (insert "#+begin_tool " (prin1-to-string header-form) "\n")
      (let ((tool-start (point)))
        (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\"))\n\n")
        (insert "Edited mevedel-chat.el (+1 -1)\n#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call-edit")))
      (let* ((text (mevedel--compact-region-with-tool-output-cap
                    (point-min) (point-max) 200 t))
             (header-start (string-match "#\\+begin_tool " text))
             (header (car (read-from-string text (match-end 0)))))
        (should header-start)
        (should (eq 'Edit (car header)))
        (should (equal "mevedel-chat.el" (plist-get (cdr header) :file_path)))
        (should (string-match-p "string argument truncated"
                                (plist-get (cdr header) :old_string))))))

  :doc "escapes nested-looking tool markers in truncated result bodies"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"outer.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"outer.txt\"))\n\n")
      (insert "outer before\n")
      (insert "#+begin_tool (Bash :command \"echo nested\")\n")
      (insert "(:name \"Bash\" :args (:command \"echo nested\"))\n")
      (insert "nested result\n#+end_tool\nouter after\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let* ((text (mevedel--compact-region-with-tool-output-cap
                  (point-min) (point-max) 80 t))
           (begin-count (cl-loop with pos = 0
                                 while (string-match "^#\\+begin_tool" text pos)
                                 count t
                                 do (setq pos (match-end 0))))
           (end-count (cl-loop with pos = 0
                               while (string-match "^#\\+end_tool" text pos)
                               count t
                               do (setq pos (match-end 0)))))
      (should (= 1 begin-count))
      (should (= 1 end-count))
      (should (string-match-p "# [+]begin_tool" text))
      (should (string-match-p "tool output truncated" text))))

  :doc "escapes nested-looking tool markers in retained result bodies"
  (with-temp-buffer
    (insert "#+begin_tool (Read :file_path \"outer.txt\")\n")
    (let ((tool-start (point)))
      (insert "(:name \"Read\" :args (:file_path \"outer.txt\"))\n\n")
      (insert "short before\n#+begin_tool (Bash :command \"nested\")\nshort after\n")
      (insert "#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-read")))
    (let* ((text (mevedel--compact-region-with-tool-output-cap
                  (point-min) (point-max) 10000 t))
           (begin-count (cl-loop with pos = 0
                                 while (string-match "^#\\+begin_tool" text pos)
                                 count t
                                 do (setq pos (match-end 0))))
           (end-count (cl-loop with pos = 0
                               while (string-match "^#\\+end_tool" text pos)
                               count t
                               do (setq pos (match-end 0)))))
      (should (= 1 begin-count))
      (should (= 1 end-count))
      (should (string-match-p "# [+]begin_tool" text))
      (should-not (string-match-p "tool output truncated" text)))))

(mevedel-deftest mevedel--compact-prompt ()
  ,test
  (test)
  :doc "creates anchored summary prompt with required sections"
  (let ((prompt (mevedel--compact-prompt nil nil nil)))
    (should (string-match-p "Create a new anchored summary" prompt))
    (should (string-match-p "## Skills Invoked" prompt))
    (should (string-match-p "- (none)" prompt))
    (should (string-match-p "Do NOT call any tools" prompt)))

  :doc "updates with previous summary and manual instructions"
  (let ((prompt (mevedel--compact-prompt "old summary" "focus tests" nil)))
    (should (string-match-p "Update the anchored summary" prompt))
    (should (string-match-p "previous summary is authoritative retained context" prompt))
    (should (string-match-p "Do NOT replace it with only the recent conversation" prompt))
    (should (string-match-p "Do not discard previous-summary details" prompt))
    (should (string-match-p "<previous-summary>" prompt))
    (should (string-match-p "old summary" prompt))
    (should (string-match-p "## Additional Instructions" prompt))
    (should (string-match-p "focus tests" prompt))))

(mevedel-deftest mevedel--compact-skills-section ()
  ,test
  (test)
  :doc "returns none marker when no session"
  (should (equal (mevedel--compact-skills-section nil) "- (none)"))

  :doc "returns none marker when session has no invoked-skills records"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c1" :root "/tmp/c1" :name "c1"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (should (equal (mevedel--compact-skills-section session) "- (none)")))

  :doc "lists invoked skills with name, args, trigger, turn"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c2" :root "/tmp/c2" :name "c2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (rec1 (mevedel-skill-invocation-record--create
                :name "grill-me" :args "spec 22"
                :trigger 'user-slash :turn 3
                :source-path "/skills/grill-me/SKILL.md"
                :prepared-body "Body 1"))
         (rec2 (mevedel-skill-invocation-record--create
                :name "review-spec" :args nil
                :trigger 'model-skill :turn 7
                :source-path "/skills/review-spec/SKILL.md"
                :prepared-body "Body 2")))
    (setf (mevedel-session-invoked-skills session) (list rec1 rec2))
    (let ((section (mevedel--compact-skills-section session)))
      (should (string-match-p "/grill-me spec 22" section))
      (should (string-match-p "user-slash" section))
      (should (string-match-p "turn: 3" section))
      (should (string-match-p "/review-spec" section))
      (should (string-match-p "model-skill" section)))))

(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
