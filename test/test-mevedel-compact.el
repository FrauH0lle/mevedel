;;; test-mevedel-compact.el --- Tests for mevedel-compact.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-compact)
(require 'mevedel-models)
(require 'mevedel-hooks)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-system)
(require 'mevedel-utilities)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
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
(defvar gptel-reasoning-effort)
(defvar gptel--request-params)

(defun test-mevedel-compact--failing-hook (_event)
  "Signal the hook failure used by fail-closed compaction tests."
  (error "Hook failed"))

(cl-defmacro test-mevedel-compact--with-persisted-buffer
    ((buffer session) &rest body)
  "Run BODY in persisted BUFFER owned by SESSION."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "mevedel-compact-test-" t))
          (workspace
           (mevedel-workspace-get-or-create
            'project "compact-test" tempdir "compact-test"))
          (,session (mevedel-session-create "main" workspace))
          (,buffer (generate-new-buffer " *mevedel-compact-test*")))
     (unwind-protect
         (with-current-buffer ,buffer
           (org-mode)
           (setq-local mevedel--session ,session)
           (mevedel-session-persistence-ensure-files ,session ,buffer)
           ,@body)
       (mevedel-session-persistence-lock-release tempdir)
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           (set-buffer-modified-p nil))
         (kill-buffer ,buffer))
       (mevedel-workspace-clear-registry)
       (delete-directory tempdir t))))

(cl-defmacro test-mevedel-compact--with-persisted-agent
    ((buffer invocation session canonical-path parent-buffer) &rest body)
  "Run BODY in persisted agent BUFFER with INVOCATION and SESSION."
  (declare
   (indent 1)
   (debug ((symbolp symbolp symbolp symbolp symbolp) body)))
  `(let* ((tempdir (make-temp-file "mevedel-compact-agent-test-" t))
          (workspace
           (mevedel-workspace-get-or-create
            'project "compact-agent-test" tempdir "compact-agent-test"))
          (,session (mevedel-session-create "main" workspace))
          (,parent-buffer
           (generate-new-buffer " *mevedel-compact-parent*"))
          (agent (mevedel-agent--create :name "explorer"))
          (,invocation (mevedel-agent-invocation-create agent))
          (,buffer (generate-new-buffer " *mevedel-compact-agent*"))
          (,canonical-path nil))
     (unwind-protect
         (progn
           (with-current-buffer ,parent-buffer
             (org-mode)
             (setq-local mevedel--session ,session)
             (mevedel-session-persistence-ensure-files
              ,session parent-buffer))
           (let ((relative-path "agents/explorer-test.chat.org"))
             (setq ,canonical-path
                   (expand-file-name relative-path
                                     (mevedel-session-save-path ,session)))
             (make-directory (file-name-directory ,canonical-path) t)
             (setf (mevedel-agent-invocation-agent-id ,invocation)
                   "explorer--test")
             (setf (mevedel-agent-invocation-path ,invocation)
                   "/root/explorer")
             (setf (mevedel-agent-invocation-buffer ,invocation) ,buffer)
             (setf (mevedel-agent-invocation-parent-data-buffer ,invocation)
                   ,parent-buffer)
             (setf (mevedel-agent-invocation-parent-session ,invocation)
                   ,session)
             (setf
              (mevedel-agent-invocation-transcript-relative-path ,invocation)
              relative-path)
             (setf (mevedel-agent-invocation-transcript-status ,invocation)
                   'running)
             (with-current-buffer ,buffer
               (org-mode)
               (setq-local mevedel--agent-invocation ,invocation)
               (set-visited-file-name ,canonical-path t t)
               ,@body)))
       (mevedel-session-persistence-lock-release tempdir)
       (dolist (candidate (list ,buffer ,parent-buffer))
         (when (buffer-live-p candidate)
           (with-current-buffer candidate
             (set-buffer-modified-p nil))
           (kill-buffer candidate)))
       (mevedel-workspace-clear-registry)
       (delete-directory tempdir t))))

(defun test-mevedel-compact--insert-agent-task
    (invocation description prompt)
  "Insert INVOCATION's persisted task heading, DESCRIPTION, and PROMPT."
  (let ((text
         (format "* Agent Task: %s\n:PROPERTIES:\n:%s: %s\n:END:\n\n%s\n"
                 description
                 mevedel-agent-task-path-property
                 (mevedel-agent-invocation-require-path invocation)
                 prompt)))
    (insert text)
    text))

(mevedel-deftest mevedel--compact-previous-summary ()
  ,test
  (test)
  :doc "strips the persisted handoff prefix before reusing a summary"
  (with-temp-buffer
    (insert "#+begin_summary\n"
            mevedel-session-persistence--summary-handoff-prefix
            "## Goal\n- Continue\n"
            (mevedel--format-hook-audit-record
             '(:type compact-context
               :event "PreCompact"
               :context "private audit"))
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
                       (lambda (&optional _token-estimate)
                         '(:summary-policy nil :target-pressure nil)))
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
                           (setq-local
                            mevedel--compact-current-request-hook-context
                            "<hook-context>\n<hook-event name=\"SessionStart\">\ncompact context\n</hook-event>\n</hook-context>")
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
              (let ((first (string-match "compact context" text)))
                (should first)
                (should-not (string-match "compact context" text (1+ first))))
              (should (string-match-p "<system-reminder>\nRe-read /tmp/old.el"
                                      text))
              (should-not (string-match-p "Old prompt" text))
              (should (string-match-p "Pending prompt" text))))
          (with-current-buffer source-buf
            (should-not mevedel--compact-current-request-reminder)
            (should-not mevedel--compact-current-request-hook-context)))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p prompt-buf)
        (kill-buffer prompt-buf))))

  :doc "blocks target-pressure prompt without a transcript boundary"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        (continued 0) failure)
    (unwind-protect
        (progn
          (with-current-buffer source-buf
            (org-mode)
            (insert "No response boundary\n"))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                       (lambda (&optional _tokens)
                         '(:summary-policy nil :target-pressure t)))
                      ((symbol-function 'mevedel--compact-auto-failure)
                       (lambda (_buffer err) (setq failure err))))
              (with-current-buffer prompt-buf
                (mevedel--compact-transform-auto
                 (lambda () (cl-incf continued)) fsm))))
          (should (= continued 0))
          (should (string-match-p "No compactable history" failure)))
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
                (let ((mevedel-compact-token-threshold 0.5)
                      (mevedel-compact-context-limit 200)
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
                (let ((mevedel-compact-token-threshold 0.5)
                      (mevedel-compact-context-limit 200)
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

  :doc "summarizer pressure triggers before target-model pressure"
  (let ((source-buf (generate-new-buffer " *mevedel-compact-source*"))
        (prompt-buf (generate-new-buffer " *mevedel-compact-prompt*"))
        captured-args captured-policy)
    (unwind-protect
        (progn
          (put 'mevedel-target-model :context-window 2)
          (put 'mevedel-summary-model :context-window 0.2)
          (with-current-buffer source-buf
            (org-mode)
            (setq-local mevedel--compaction-in-flight nil)
            (insert "Old prompt\n")
            (insert (propertize "Old response\n" 'gptel 'response))
            (insert "Pending\n")
            (setq-local mevedel--known-token-baseline
                        (list :tokens 200
                              :position (copy-marker (point-max)))))
          (with-current-buffer prompt-buf
            (org-mode)
            (insert-buffer-substring source-buf))
          (let ((fsm (gptel-make-fsm :info (list :buffer source-buf))))
            (cl-letf (((symbol-function 'mevedel--compact-auto-eligible-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model mevedel-summary-model)))
                      ((symbol-function 'mevedel--compact-run)
                       (lambda (&rest args)
                         (setq captured-args args
                               captured-policy
                               (plist-get
                                (gptel-fsm-info fsm)
                                :mevedel-compaction-target-policy)))))
              (with-current-buffer prompt-buf
                (let ((mevedel-compact-token-threshold 0.8)
                      (mevedel-compact-reserve-tokens 0)
                      (gptel-backend nil)
                      (gptel-model 'mevedel-target-model)
                      (gptel-max-tokens 150)
                      (gptel--request-params '(:temperature 0.5)))
                  (mevedel--compact-transform-auto #'ignore fsm)))))
          (should captured-args)
          (should
           (equal captured-policy
                  '(:backend nil :model mevedel-target-model
                    :effort nil
                    :max-tokens 150
                    :request-params (:temperature 0.5))))
          (should-not (plist-get (plist-get captured-args :admission)
                                 :target-pressure))
          (should (eq (plist-get
                       (plist-get (plist-get captured-args :admission)
                                  :summary-policy)
                       :model)
                      'mevedel-summary-model)))
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
            (let ((mevedel-compact-token-threshold 0.5)
                  (mevedel-compact-context-limit 200))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "preserves the realized reasoning effort during continuation admission"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        captured-effort
        sent-effort)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list
                           :buffer chat-buf
                           :history '(TRET)
                           :mevedel-compaction-target-policy
                           '(:backend target-backend :model target-model
                             :effort max :max-tokens nil
                             :request-params nil)
                           :data '(:messages [])))))
          (with-current-buffer chat-buf
            (setq-local gptel-reasoning-effort 'medium))
          (cl-letf (((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       (setq captured-effort gptel-reasoning-effort)
                       nil))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm)
                       (setq sent-effort gptel-reasoning-effort))))
            (mevedel--compact-handle-wait fsm))
          (should (eq captured-effort 'max))
          (should (eq sent-effort 'max)))
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
            (let ((mevedel-compact-token-threshold 0.5)
                  (mevedel-compact-context-limit 200)
                  (mevedel-compact-image-token-estimate 10))
              (mevedel--compact-handle-wait fsm)))
          (should (= sent 1))
          (should-not ran))
      (kill-buffer chat-buf)))

  :doc "uses the realized request reserve instead of chat-buffer settings"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0)
        (ran nil))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info
                    (list
                     :buffer chat-buf
                     :history '(TRET)
                     :mevedel-compaction-target-policy
                     '(:backend nil :model nil :max-tokens 100
                       :request-params nil)
                     :data
                     (list :messages
                           (vector
                            (list :role "user"
                                  :content (make-string 240 ?x))))))))
          (with-current-buffer chat-buf
            (setq-local gptel-max-tokens nil)
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
            (let ((mevedel-compact-token-threshold 0.5)
                  (mevedel-compact-context-limit 200)
                  (mevedel-compact-reserve-tokens 0))
              (mevedel--compact-handle-wait fsm)))
          (should ran)
          (should (= sent 1)))
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
            (let ((mevedel-compact-token-threshold 0.5)
                  (mevedel-compact-context-limit 200))
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
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
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
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil)))
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
      (kill-buffer chat-buf)))

  :doc "blocks target-pressure continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0) failure)
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure t)))
                    ((symbol-function 'mevedel--compact-auto-failure)
                     (lambda (_buffer err) (setq failure err))))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 0))
          (should (string-match-p "No compactable history" failure)))
      (kill-buffer chat-buf)))

  :doc "allows summarizer-only continuation without a transcript boundary"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-wait*"))
        (sent 0))
    (unwind-protect
        (let ((fsm (gptel-make-fsm
                    :info (list :buffer chat-buf
                                :history '(TRET)
                                :data '(:messages [])))))
          (with-current-buffer chat-buf
            (insert "No response boundary\n"))
          (cl-letf (((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf sent)))
                    ((symbol-function 'mevedel--compact-should-compact-p)
                     (lambda (&optional _tokens)
                       '(:summary-policy nil :target-pressure nil))))
            (mevedel--compact-handle-wait fsm))
          (should (= sent 1)))
      (kill-buffer chat-buf))))

(mevedel-deftest mevedel--compact-agent-task-heading ()
  ,test
  (test)
  :doc "finds the first initial-task marker for the exact agent path"
  (with-temp-buffer
    (org-mode)
    (let ((other
           (mevedel-agent-invocation--create :path "/root/other"))
          (invocation
           (mevedel-agent-invocation--create :path "/root/inspect")))
      (test-mevedel-compact--insert-agent-task
       other "other" "Other task.")
      (insert "* Agent Task: unmarked\n\nUnmarked task.\n")
      (let ((initial-heading (point)))
        (test-mevedel-compact--insert-agent-task
         invocation "inspect" "Initial task.")
        (test-mevedel-compact--insert-agent-task
         invocation "follow-up" "Later task.")
        (should (= initial-heading
                   (mevedel--compact-agent-task-heading invocation))))))

  :doc "returns nil when no task marker matches the agent path"
  (with-temp-buffer
    (org-mode)
    (let ((other
           (mevedel-agent-invocation--create :path "/root/other"))
          (invocation
           (mevedel-agent-invocation--create :path "/root/missing")))
      (test-mevedel-compact--insert-agent-task
       other "other" "Other task.")
      (insert "* Agent Task: unmarked\n\nUnmarked task.\n")
      (should-not (mevedel--compact-agent-task-heading invocation)))))

(mevedel-deftest mevedel--compact-agent-summary-bounds ()
  ,test
  (test)
  :doc "finds the anchored summary body in an agent transcript"
  (with-temp-buffer
    (org-mode)
    (let ((invocation
           (mevedel-agent-invocation--create :path "/root/inspect")))
      (setq-local mevedel--agent-invocation invocation)
      (test-mevedel-compact--insert-agent-task
       invocation "inspect" "Keep this task.")
      (insert "#+begin_summary\n## Goal\n- Continue\n#+end_summary\n"
              "Recent tail.\n")
      (let ((bounds (mevedel--compact-agent-summary-bounds invocation)))
      (should (equal "#+begin_summary\n## Goal\n- Continue\n#+end_summary"
                     (buffer-substring-no-properties
                      (plist-get bounds :begin) (plist-get bounds :end))))
      (should (equal "## Goal\n- Continue\n"
                     (buffer-substring-no-properties
                      (plist-get bounds :body-begin)
                        (plist-get bounds :body-end)))))))

  :doc "uses the path marker to ignore inherited task headings and summaries"
  (with-temp-buffer
    (org-mode)
    (let ((parent
           (mevedel-agent-invocation--create :path "/root/parent"))
          (child
           (mevedel-agent-invocation--create :path "/root/parent/child")))
      (test-mevedel-compact--insert-agent-task
       parent "parent" "Parent task.")
      (insert "#+begin_summary\nParent summary.\n#+end_summary\n")
      (setq-local mevedel--agent-invocation child)
      (test-mevedel-compact--insert-agent-task
       child "child" "Child task.")
      (insert "#+begin_summary\nAgent summary.\n#+end_summary\n")
      (let ((bounds (mevedel--compact-agent-summary-bounds child)))
        (should (equal "Agent summary.\n"
                       (buffer-substring-no-properties
                        (plist-get bounds :body-begin)
                        (plist-get bounds :body-end))))))))

(mevedel-deftest mevedel--compact-agent-target ()
  ,test
  (test)
  :doc "builds a complete adapter only for the live canonical transcript"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Agent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let ((target (mevedel--compact-agent-target invocation)))
      (should (eq agent-buffer (plist-get target :buffer)))
      (should (eq invocation (plist-get target :invocation)))
      (should (eq session (plist-get target :session)))
      (should (equal canonical-path (plist-get target :transcript-path)))
      (dolist (operation '(:apply :start :complete :resume :fail))
        (should (functionp (plist-get target operation)))))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/other.chat.org")
    (should-not (mevedel--compact-agent-target invocation)))

  :doc "anchors the marked child task after inherited nested-agent context"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((parent
           (mevedel-agent-invocation--create :path "/root/parent")))
      (test-mevedel-compact--insert-agent-task
       parent "parent" "Inherited prompt."))
    (let ((start (point)))
      (insert "Inherited response.\n")
      (put-text-property start (point) 'gptel 'response))
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Agent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let ((target (mevedel--compact-agent-target invocation)))
      (should target)
      (should (string-match-p "Inherited prompt"
                              (buffer-substring-no-properties
                               (caar (plist-get
                                      target :history-prefix-regions))
                               (cdar (plist-get
                                      target :history-prefix-regions)))))
      (should-not (string-match-p "Inherited prompt"
                                  (plist-get target :anchor-text)))
      (should (string-match-p "Keep this task"
                              (plist-get target :anchor-text))))))

(mevedel-deftest mevedel--compact-agent-archive-path ()
  ,test
  (test)
  :doc "selects the first unused numbered sibling archive"
  (let* ((tempdir (make-temp-file "mevedel-compact-archive-test-" t))
         (canonical (expand-file-name "agent.chat.org" tempdir))
         (first (expand-file-name "agent.compact-0001.chat.org" tempdir)))
    (unwind-protect
        (progn
          (write-region "canonical" nil canonical nil 'silent)
          (should (equal first
                         (mevedel--compact-agent-archive-path canonical)))
          (write-region "archive" nil first nil 'silent)
          (should
           (equal (expand-file-name "agent.compact-0002.chat.org" tempdir)
                  (mevedel--compact-agent-archive-path canonical))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel--compact-archived-tool-use-ids ()
  ,test
  (test)
  :doc "collects each concrete tool row removed by the compacted prefix"
  (with-temp-buffer
    (insert (propertize "first" 'gptel '(tool . "call-1")))
    (insert " plain ")
    (insert (propertize "again" 'gptel '(tool . "call-1")))
    (insert (propertize "second" 'gptel '(tool . "call-2")))
    (insert
     (mevedel--format-hook-audit-record
      '(:type execution-archive :tool-use-id "call-archived"
        :render-data (:execution-id "exec-archived" :state running
                      :live-execution-p t))))
    (should
     (equal '("call-1" "call-2" "call-archived")
            (mevedel--compact-archived-tool-use-ids
             (point-min) (point-max))))))

(mevedel-deftest mevedel--compact-agent-apply ()
  ,test
  (test)
  :doc "archives the full canonical transcript before rewriting it"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Old response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let* ((record
            (mevedel-agent-record--create
             :id (mevedel-agent-invocation-agent-id invocation)
             :path "/root/explorer"
             :parent-path "/root"
             :activity 'running
             :conversation-buffer agent-buffer
             :conversation-location
             (mevedel-agent-invocation-transcript-relative-path invocation)
             :invocation invocation))
           (_ (setf (mevedel-session-agent-registry session)
                    (list (cons "/root/explorer" record))))
           (original (buffer-string))
           (target (mevedel--compact-agent-target invocation))
           (archive
            (mevedel--compact-agent-apply
             target "## Goal\n- Continue" "Recent tail.\n"
             "Pending result.\n" nil)))
      (should (file-exists-p archive))
      (should (equal original
                     (with-temp-buffer
                       (insert-file-contents archive)
                       (buffer-string))))
      (should (string-match-p "Keep this task" (buffer-string)))
      (should (string-match-p "Continue" (buffer-string)))
      (should (string-match-p "Recent tail" (buffer-string)))
      (should (string-match-p "Pending result" (buffer-string)))
      (should (eq record
                  (cdr (assoc "/root/explorer"
                              (mevedel-session-agent-registry session)))))
      (should (eq agent-buffer
                  (mevedel-agent-record-conversation-buffer record)))
      (should (equal (buffer-string)
                     (with-temp-buffer
                       (insert-file-contents canonical-path)
                       (buffer-string))))))

  :doc "leaves live and canonical transcripts unchanged on archive failure"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Old response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let* ((target (mevedel--compact-agent-target invocation))
           (directory (file-name-directory canonical-path))
           (original-live (buffer-string))
           (original-canonical
            (with-temp-buffer
              (insert-file-contents canonical-path)
              (buffer-string))))
      (unwind-protect
          (progn
            (set-file-modes directory #o500)
            (should-error
             (mevedel--compact-agent-apply
              target "## Goal\n- Continue" nil nil nil)
             :type 'file-error))
        (set-file-modes directory #o700))
      (should (equal original-live (buffer-string)))
      (should (equal original-canonical
                     (with-temp-buffer
                       (insert-file-contents canonical-path)
                       (buffer-string))))
      (should-not
       (directory-files directory nil "\\.compact-[0-9]+\\.chat\\.org\\'"))))

  :doc "retains the full archive when canonical application later fails"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "Old response.\n")
      (put-text-property start (point) 'gptel 'response))
    (basic-save-buffer)
    (let* ((original (buffer-string))
           (target (mevedel--compact-agent-target invocation)))
      (add-hook
       'before-change-functions
       (lambda (&rest _)
         (error "Stop compacted transcript application"))
       nil t)
      (should-error
       (mevedel--compact-agent-apply
        target "## Goal\n- Continue" nil nil nil)
       :type 'error)
      (let ((archives
             (directory-files
              (file-name-directory canonical-path) t
              "\\.compact-[0-9]+\\.chat\\.org\\'")))
        (should (= 1 (length archives)))
        (should (equal original
                       (with-temp-buffer
                         (insert-file-contents (car archives))
                         (buffer-string))))))))

(mevedel-deftest mevedel--compact-main-apply ()
  ,test
  (test)
  :doc "delegates rewriting and routes reminders by compaction mode"
  (let ((session 'session)
        applied queued)
    (cl-letf (((symbol-function 'mevedel--compact-apply)
               (lambda (&rest args) (setq applied args)))
              ((symbol-function
                'mevedel--compact-file-reference-reminder-body)
               (lambda (_session _turns) "remember files"))
              ((symbol-function 'mevedel-session-enqueue-pending-reminder)
               (lambda (_session reminder) (push reminder queued))))
      (let ((mevedel--compact-current-request-reminder nil))
        (mevedel--compact-main-apply
         (list :session session) "summary" "tail" "pending" nil t 2)
        (should (equal '("summary" "tail" "pending" nil nil) applied))
        (should (equal "remember files"
                       mevedel--compact-current-request-reminder))
        (should-not queued)
        (setq mevedel--compact-current-request-reminder nil)
        (mevedel--compact-main-apply
         (list :session session) "summary" "tail" nil nil nil 2)
        (should (equal '("remember files") queued))
        (should-not mevedel--compact-current-request-reminder)))))

(mevedel-deftest mevedel--compact-main-start ()
  ,test
  (test)
  :doc "shows main-session compaction progress in the view"
  (let ((view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        spinner)
    (unwind-protect
        (let ((mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--update-spinner)
                     (lambda (text) (setq spinner text))))
            (mevedel--compact-main-start nil))
          (should (equal "Compacting..." spinner)))
      (kill-buffer view-buffer))))

(mevedel-deftest mevedel--compact-agent-start ()
  ,test
  (test)
  :doc "records and displays agent compaction progress"
  (let ((invocation (mevedel-agent-invocation-create
                     (mevedel-agent--create :name "explorer")))
        activity status)
    (cl-letf (((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation value) (setq activity value)))
              ((symbol-function 'gptel--update-status)
               (lambda (value &optional _face) (setq status value))))
      (mevedel--compact-agent-start (list :invocation invocation)))
    (should (equal '(:type status :summary "Compacting...") activity))
    (should (equal " Compacting..." status))))

(mevedel-deftest mevedel--compact-begin-root-context-epoch ()
  ,test
  (test)
  :doc "manual compaction leaves compact-start context for the next input"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "compact-epoch" :root "/tmp"
                     :name "compact-epoch"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *mevedel-compact-epoch*"))
         source)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Transcript\n"))
          (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (value)
                       (setq source value)
                       (mevedel-hooks-record-session-context
                        session '(:additional-context ("fresh context"))
                        'SessionStart))))
            (mevedel--compact-begin-root-context-epoch
             (list :buffer buffer :session session) nil))
          (should (equal "compact" source))
          (should (mevedel-session-hook-context-pending session))
          (with-current-buffer buffer
            (should-not mevedel--compact-current-request-hook-context)
            (should-not (string-match-p "fresh context" (buffer-string)))))
      (kill-buffer buffer)))

  :doc "automatic compaction consumes compact-start context into its request"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "compact-auto-epoch" :root "/tmp"
                     :name "compact-auto-epoch"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *mevedel-compact-auto-epoch*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Pending prompt\n"))
          (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (_source)
                       (mevedel-hooks-record-session-context
                        session '(:additional-context ("fresh context"))
                        'SessionStart))))
            (mevedel--compact-begin-root-context-epoch
             (list :buffer buffer :session session) t))
          (should-not (mevedel-session-hook-context-pending session))
          (with-current-buffer buffer
            (should (string-match-p
                     "fresh context"
                     mevedel--compact-current-request-hook-context))
            (should (string-match-p "fresh context" (buffer-string)))))
      (kill-buffer buffer)))

  :doc "retained-agent compaction does not begin a root context epoch"
  (let ((buffer (generate-new-buffer " *mevedel-agent-compact-epoch*"))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--run-session-start-hooks)
                   (lambda (_source) (setq called t))))
          (mevedel--compact-begin-root-context-epoch
           (list :buffer buffer :session 'session :invocation 'agent) t)
          (should-not called))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--compact-main-complete ()
  ,test
  (test)
  :doc "rerenders the main view and stops manual request progress"
  (let ((view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        (renders 0) (stops 0))
    (unwind-protect
        (let ((mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                     (lambda () (cl-incf renders)))
                    ((symbol-function 'mevedel-view--stop-request-progress)
                     (lambda () (cl-incf stops))))
            (mevedel--compact-main-complete nil nil)
            (mevedel--compact-main-complete nil t))
          (should (= 2 renders))
          (should (= 1 stops)))
      (kill-buffer view-buffer)))

  :doc "real compaction redraw preserves a multiline leading-> draft and point"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line")
          (point-offset 4))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft point-offset))
      (with-current-buffer data-buf
        (mevedel--compact-main-complete nil t))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point)
                   (+ (mevedel-view--input-start) point-offset)))))))

(mevedel-deftest mevedel--compact-agent-complete ()
  ,test
  (test)
  :doc "restores ordinary agent continuation status"
  (let ((invocation (mevedel-agent-invocation-create
                     (mevedel-agent--create :name "explorer")))
        activity status)
    (cl-letf (((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation value) (setq activity value)))
              ((symbol-function 'gptel--update-status)
               (lambda (value &optional _face) (setq status value))))
      (mevedel--compact-agent-complete (list :invocation invocation) t))
    (should (equal '(:type status :summary "waiting") activity))
    (should (equal " Calling Agent..." status))))

(mevedel-deftest mevedel--compact-main-target ()
  ,test
  (test)
  :doc "builds the complete adapter for the active persisted segment"
  (test-mevedel-compact--with-persisted-buffer (buffer session)
    (insert "Prompt.\n")
    (let ((start (point)))
      (insert "Response.\n")
      (put-text-property start (point) 'gptel 'response))
    (let ((target (mevedel--compact-main-target)))
      (should (eq buffer (plist-get target :buffer)))
      (should (eq session (plist-get target :session)))
      (should (plist-get target :eligible-p))
      (dolist (operation '(:apply :start :complete :resume :fail))
        (should (functionp (plist-get target operation)))))))

(mevedel-deftest mevedel--compact-target-call ()
  ,test
  (test)
  :doc "passes the target and arguments to its selected operation"
  (let* ((target (list :apply (lambda (self one two)
                                (list self one two))))
         (result (mevedel--compact-target-call target :apply 1 2)))
    (should (eq target (car result)))
    (should (equal '(1 2) (cdr result)))
    (should-error (mevedel--compact-target-call target :fail)
                  :type 'error)))

(mevedel-deftest mevedel--compact-main-resume-status ()
  ,test
  (test)
  :doc "restores main request progress after continuation compaction"
  (let ((chat-buffer (generate-new-buffer " *mevedel-compact-chat*"))
        (view-buffer (generate-new-buffer " *mevedel-compact-view*"))
        spinner)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--update-spinner)
                     (lambda (text) (setq spinner text))))
            (mevedel--compact-main-resume-status
             (list :buffer chat-buffer)))
          (should (equal "Thinking..." spinner)))
      (kill-buffer chat-buffer)
      (kill-buffer view-buffer))))

(mevedel-deftest mevedel--compact-target-resume ()
  ,test
  (test)
  :doc "rebuilds a compacted target and resumes its FSM once"
  (let* ((buffer (generate-new-buffer " *mevedel-compact-resume*"))
         (marker (with-current-buffer buffer (copy-marker (point-min))))
         (fsm (gptel-make-fsm :info (list :position marker)))
         rebuilt status (waits 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "compacted"))
          (cl-letf (((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (actual-fsm actual-buffer)
                       (setq rebuilt (list actual-fsm actual-buffer))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf waits))))
            (mevedel--compact-target-resume
             (list :buffer buffer
                   :resume-status
                   (lambda (_target) (setq status t)))
             fsm))
          (should (equal (list fsm buffer) rebuilt))
          (should status)
          (should (= 1 waits))
          (should (= (with-current-buffer buffer (point-max))
                     (marker-position marker))))
      (set-marker marker nil)
      (kill-buffer buffer))))

(mevedel-deftest mevedel--compact-agent-terminal-failure ()
  ,test
  (test)
  :doc "clears activity, records a structured error, and enters ERRS"
  (let* ((invocation
          (mevedel-agent-invocation-create
           (mevedel-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :info (list :buffer nil
                       :mevedel-agent-invocation invocation)))
         activity status transition)
    (cl-letf (((symbol-function 'gptel--fsm-transition)
               (lambda (_fsm state) (setq transition state)))
              ((symbol-function 'mevedel-agent-conversation-record-activity)
               (lambda (_invocation item) (setq activity item)))
              ((symbol-function 'gptel--update-status)
               (lambda (text &optional _face) (setq status text))))
      (mevedel--compact-agent-terminal-failure nil fsm "boom"))
    (should (equal '(:type status :summary "error") activity))
    (should (equal " Agent failed" status))
    (should (eq 'ERRS transition))
    (should (equal "Compaction failed: boom"
                   (plist-get (gptel-fsm-info fsm) :status)))
    (should (equal '(:type "compaction_error" :message "boom")
                   (plist-get (gptel-fsm-info fsm) :error)))))

(mevedel-deftest mevedel--compact-main-failure ()
  ,test
  (test)
  :doc "routes target failure through main automatic failure reporting"
  (let ((buffer (generate-new-buffer " *mevedel-compact-failure*"))
        captured)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--compact-auto-failure)
                   (lambda (actual-buffer err)
                     (setq captured (list actual-buffer err)))))
          (mevedel--compact-main-failure
           (list :buffer buffer) nil "boom")
          (should (equal (list buffer "boom") captured)))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--compact-handle-target-wait ()
  ,test
  (test)
  :doc "passes through when admission does not request compaction"
  (let ((fsm (gptel-make-fsm :info nil)) (waits 0))
    (cl-letf (((symbol-function 'gptel--handle-wait)
               (lambda (_fsm) (cl-incf waits))))
      (mevedel--compact-handle-target-wait fsm nil nil))
    (should (= 1 waits)))

  :doc "settles no-boundary continuations according to target pressure"
  (dolist (pressure '(nil t))
    (let ((fsm (gptel-make-fsm :info nil)) (waits 0) failure)
      (cl-letf (((symbol-function 'mevedel--compact-find-boundary)
                 (lambda () nil))
                ((symbol-function 'gptel--handle-wait)
                 (lambda (_fsm) (cl-incf waits))))
        (mevedel--compact-handle-target-wait
         fsm
         (list :fail (lambda (_target _fsm err) (setq failure err)))
         (list :target-pressure pressure)))
      (if pressure
          (progn
            (should (= 0 waits))
            (should (string-match-p "No compactable history" failure)))
        (should (= 1 waits)))))

  :doc "resumes success and routes run or rebuild failures once"
  (dolist (outcome '(success run-error rebuild-error))
    (let ((fsm (gptel-make-fsm :info nil)) resumed failure)
      (cl-letf (((symbol-function 'mevedel--compact-find-boundary)
                 (lambda () 10))
                ((symbol-function 'mevedel--compact-run)
                 (lambda (&rest args)
                   (funcall (plist-get args :callback)
                            (and (eq outcome 'run-error) "run failed")))))
        (mevedel--compact-handle-target-wait
         fsm
         (list :resume
               (lambda (_target _fsm)
                 (if (eq outcome 'rebuild-error)
                     (error "rebuild failed")
                   (setq resumed t)))
               :fail
               (lambda (_target _fsm err) (setq failure err)))
         '(:target-pressure t)))
      (pcase outcome
        ('success (should resumed) (should-not failure))
        ('run-error (should (equal "run failed" failure)))
        ('rebuild-error (should (equal "rebuild failed" failure)))))))

(mevedel-deftest mevedel--compact-handle-agent-wait ()
  ,test
  (test)
  :doc "archives, compacts, rebuilds, and resumes one persisted continuation"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-mevedel-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          pre-event post-event captured-system captured-body
          resumed-data statuses messages)
      (setf (mevedel-session-goal session) "PARENT-GOAL-ONLY")
      (setf (mevedel-session-invoked-skills session)
            (list
             (mevedel-skill-invocation-record--create
              :name "parent-only-skill"
              :role 'command
              :origin 'user
              :turn 1)))
      (setf (mevedel-session-touched-files session)
            '(("/tmp/PARENT-FILE-ONLY" . 1)))
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending tool result.\n")
      (basic-save-buffer)
      (let* ((original (buffer-string))
             (data (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x)))))
             (fsm
              (gptel-make-fsm
               :info
               (list :buffer agent-buffer
                     :history '(TRET)
                     :data data
                     :backend nil
                     :model 'mevedel-agent-target-model
                     :mevedel-agent-invocation invocation
                     :mevedel-compaction-target-policy
                     '(:backend nil :model mevedel-agent-target-model
                       :max-tokens 0 :request-params nil)))))
        (put 'mevedel-agent-target-model :context-window 0.4)
        (put 'mevedel-agent-summary-model :context-window 2)
        (let ((mevedel-compact-token-threshold 0.5)
              (mevedel-compact-reserve-tokens 0)
              (mevedel-compact-tail-turns 1)
              (mevedel-compact-tail-budget 1.0)
              (mevedel-compact-warn-on-completion t)
              (gptel-backend nil)
              (gptel-model 'mevedel-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil)
              (mevedel-pre-compact-functions
               (list
                (lambda (event)
                  (setq pre-event event)
                  '(:additional-context ("agent hook context")))))
              (mevedel-post-compact-functions
               (list (lambda (event) (setq post-event event)))))
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model mevedel-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (setq captured-body body
                             captured-system (plist-get args :system))
                       (funcall (plist-get args :callback)
                                "## Goal\n- Continue the agent task." nil)))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm buffer)
                       (plist-put
                        (gptel-fsm-info rebuild-fsm) :data
                        (with-current-buffer buffer (buffer-string)))))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (wait-fsm)
                       (push (plist-get (gptel-fsm-info wait-fsm) :data)
                             resumed-data)))
                    ((symbol-function 'gptel--update-status)
                     (lambda (status &optional _face) (push status statuses)))
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (when format-string
                         (let ((rendered
                                (apply #'format format-string args)))
                           (push rendered messages)
                           rendered)))))
            (mevedel--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archives
                (directory-files
                 (file-name-directory canonical-path) t
                 "\\.compact-[0-9]+\\.chat\\.org\\'")))
          (should (= 1 (length archives)))
          (should (equal original
                         (with-temp-buffer
                           (insert-file-contents (car archives))
                           (buffer-string))))
          (should (equal canonical-path buffer-file-name))
          (should (string-prefix-p task canonical))
          (should (string-match-p "#\\+begin_summary" canonical))
          (should (string-match-p "Continue the agent task" canonical))
          (should (string-match-p "Recent user four" canonical))
          (should (string-match-p "Pending tool result" canonical))
          (should-not (string-match-p "Old user two" canonical))
          (should (string-match-p "mevedel-hook-audit" canonical))
          (should-not (plist-get (gptel-fsm-info fsm) :error))
          (should (= 1 (length resumed-data)))
          (should (string-match-p "Continue the agent task"
                                  (car resumed-data)))
          (should (equal (plist-get pre-event :origin) "/root/explorer"))
          (should (equal (plist-get pre-event :transcript-path)
                         canonical-path))
          (should (equal (plist-get post-event :origin) "/root/explorer"))
          (should (string-match-p "agent hook context" captured-system))
          (should (string-match-p "Old user two" captured-body))
          (should-not (string-match-p "Recent user four" captured-body))
          (dolist (parent-only
                   '("PARENT-GOAL-ONLY" "parent-only-skill"
                     "PARENT-FILE-ONLY"))
            (should-not (string-match-p parent-only captured-system))
            (should-not (string-match-p parent-only captured-body))
            (should-not (string-match-p parent-only canonical)))
          (should (member " Compacting..." statuses))
          (should-not
           (cl-some (lambda (entry) (string-match-p "long threads" entry))
                    messages))
          (should-not mevedel--compact-current-request-reminder)))))

  :doc "updates the anchored summary and archives each persisted continuation"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (let ((task (test-mevedel-compact--insert-agent-task
                 invocation "inspect" "Keep this exact task."))
          (request-data
           (list :messages
                 (vector
                  (list :role "user" :content (make-string 1000 ?x)))))
          systems bodies (resumed 0)
          first-canonical before-second)
      (dolist (turn '(("Old response one.\n" . "Old user two.\n")
                      ("Old response two.\n" . "Old user three.\n")
                      ("Old response three.\n" . "Recent user four.\n")))
        (let ((start (point)))
          (insert (car turn))
          (put-text-property start (point) 'gptel 'response))
        (insert (cdr turn)))
      (let ((start (point)))
        (insert "Recent response four.\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "Pending result four.\n")
      (basic-save-buffer)
      (let ((original (buffer-string))
            (fsm
             (gptel-make-fsm
              :info
              (list :buffer agent-buffer
                    :history '(TRET)
                    :data request-data
                    :backend nil
                    :model 'mevedel-agent-target-model
                    :mevedel-agent-invocation invocation
                    :mevedel-compaction-target-policy
                    '(:backend nil :model mevedel-agent-target-model
                      :max-tokens 0 :request-params nil)))))
        (put 'mevedel-agent-target-model :context-window 0.4)
        (put 'mevedel-agent-summary-model :context-window 2)
        (let ((mevedel-compact-token-threshold 0.5)
              (mevedel-compact-reserve-tokens 0)
              (mevedel-compact-tail-turns 1)
              (mevedel-compact-tail-budget 1.0)
              (gptel-backend nil)
              (gptel-model 'mevedel-agent-target-model)
              (gptel-max-tokens 0)
              (gptel--request-params nil)
              (gptel-stream nil))
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model mevedel-agent-summary-model)))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'gptel-request)
                     (lambda (body &rest args)
                       (push body bodies)
                       (push (plist-get args :system) systems)
                       (funcall
                        (plist-get args :callback)
                        (if (= (length bodies) 1)
                            "## Goal\n- First retained fact."
                          "## Goal\n- Updated retained fact.")
                        nil)))
                    ((symbol-function
                      'mevedel--compact-rebuild-info-data-from-buffer)
                     (lambda (rebuild-fsm _buffer)
                       (plist-put (gptel-fsm-info rebuild-fsm)
                                  :data request-data)))
                    ((symbol-function 'gptel--handle-wait)
                     (lambda (_fsm) (cl-incf resumed)))
                    ((symbol-function 'gptel--update-status) #'ignore)
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (mevedel--compact-handle-agent-wait fsm)
            (setq first-canonical (buffer-string))
            (goto-char (point-max))
            (insert "New user five.\n")
            (let ((start (point)))
              (insert "New response five.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "New user six.\n")
            (let ((start (point)))
              (insert "New response six.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Latest user seven.\n")
            (let ((start (point)))
              (insert "Latest response seven.\n")
              (put-text-property start (point) 'gptel 'response))
            (insert "Pending result seven.\n")
            (basic-save-buffer)
            (setq before-second (buffer-string))
            (plist-put (gptel-fsm-info fsm) :data request-data)
            (mevedel--compact-handle-agent-wait fsm)))
        (let* ((canonical (buffer-string))
               (archive-directory (file-name-directory canonical-path))
               (first-archive
                (expand-file-name
                 "explorer-test.compact-0001.chat.org"
                 archive-directory))
               (second-archive
                (expand-file-name
                 "explorer-test.compact-0002.chat.org"
                 archive-directory)))
          (should (= 2 resumed))
          (should (= 2 (length bodies)))
          (should (file-exists-p first-archive))
          (should (file-exists-p second-archive))
          (should
           (equal original
                  (with-temp-buffer
                    (insert-file-contents first-archive)
                    (buffer-string))))
          (should
           (equal before-second
                  (with-temp-buffer
                    (insert-file-contents second-archive)
                    (buffer-string))))
          (should (string-prefix-p task first-canonical))
          (should (string-prefix-p task canonical))
          (should (= 1 (how-many "^#\\+begin_summary" (point-min)
                                 (point-max))))
          (should (string-match-p "Updated retained fact" canonical))
          (should (string-match-p "Latest user seven" canonical))
          (should (string-match-p "Pending result seven" canonical))
          (should-not (string-match-p "New user five" canonical))
          (should (string-match-p "<previous-summary>" (car systems)))
          (should (string-match-p "First retained fact" (car systems)))
          (should (string-match-p "New user five" (car bodies)))
          (should-not (string-match-p "Latest user seven" (car bodies)))
          (should (equal canonical-path buffer-file-name))))))

  :doc "does not inspect or compact an agent's initial WAIT"
  (let* ((agent-buffer (generate-new-buffer " *mevedel-agent-initial-wait*"))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent--create :name "explorer")))
         (fsm (gptel-make-fsm
               :info (list :buffer agent-buffer
                           :history '(INIT)
                           :mevedel-agent-invocation invocation)))
         (waits 0)
         compacted)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'mevedel--compact-run)
                   (lambda (&rest _) (setq compacted t))))
          (mevedel--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should-not compacted))
      (kill-buffer agent-buffer)))

  :doc "lets ephemeral agents ignore summarizer-only pressure but not target pressure"
  (let* ((agent-buffer (generate-new-buffer " *mevedel-ephemeral-agent*"))
         (invocation
          (mevedel-agent-invocation-create
           (mevedel-agent--create :name "explorer")))
         (fsm
          (gptel-make-fsm
           :table gptel-send--transitions
           :handlers (copy-tree mevedel-agent-exec--handlers)
           :state 'WAIT
           :info
           (list :buffer agent-buffer
                 :history '(TRET)
                 :data
                 (list :messages
                       (vector
                        (list :role "user" :content (make-string 1000 ?x))))
                 :mevedel-agent-invocation invocation
                 :mevedel-compaction-target-policy
                 '(:backend nil :model mevedel-large-agent-model
                   :max-tokens 0 :request-params nil))))
         (waits 0) (terminal-events 0))
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-buffer invocation) agent-buffer)
          (setf (mevedel-agent-invocation-agent-id invocation)
                "explorer--ephemeral")
          (setf (mevedel-agent-invocation-transcript-status invocation)
                'running)
          (plist-put
           (gptel-fsm-info fsm) :mevedel-agent-terminal-callback
           (lambda (_event)
             (cl-incf terminal-events)
             (setf (mevedel-agent-invocation-transcript-status invocation)
                   'error)))
          (put 'mevedel-large-agent-model :context-window 2)
          (put 'mevedel-small-agent-model :context-window 0.4)
          (put 'mevedel-agent-summary-model :context-window 0.4)
          (let ((mevedel-compact-token-threshold 0.5)
                (mevedel-compact-reserve-tokens 0))
            (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         (error "Summarizer policy must not be resolved")))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test" :max-tokens nil
                           :request-params nil)))
                      ((symbol-function 'gptel--handle-wait)
                       (lambda (_fsm) (cl-incf waits)))
                      ((symbol-function 'gptel--handle-error) #'ignore)
                      ((symbol-function 'gptel--update-status) #'ignore))
              (with-current-buffer agent-buffer
                (insert "Ephemeral transcript stays intact.\n"))
              (mevedel--compact-handle-agent-wait fsm)
              (should (= 1 waits))
              (should (= 0 terminal-events))
              (plist-put
               (gptel-fsm-info fsm) :mevedel-compaction-target-policy
               '(:backend nil :model mevedel-small-agent-model
                 :max-tokens 0 :request-params nil))
              (mevedel--compact-handle-agent-wait fsm)))
          (should (= 1 waits))
          (should (= 1 terminal-events))
          (should (eq 'ERRS (gptel-fsm-state fsm)))
          (should (eq 'error
                      (mevedel-agent-invocation-transcript-status invocation)))
          (with-current-buffer agent-buffer
            (should-not buffer-file-name)
            (should (equal "Ephemeral transcript stays intact.\n"
                           (buffer-string)))))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "lets a prefixless persisted agent proceed only below target pressure"
  (test-mevedel-compact--with-persisted-agent
      (agent-buffer invocation session canonical-path parent-buffer)
    (test-mevedel-compact--insert-agent-task
     invocation "inspect" "Keep this task.")
    (let ((start (point)))
      (insert "First recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Only recent user turn.\n")
    (let ((start (point)))
      (insert "Second recent response.\n")
      (put-text-property start (point) 'gptel 'response))
    (insert "Pending tool result.\n")
    (basic-save-buffer)
    (let* ((original (buffer-string))
           (fsm
            (gptel-make-fsm
             :table gptel-send--transitions
             :handlers (copy-tree mevedel-agent-exec--handlers)
             :state 'WAIT
             :info
             (list :buffer agent-buffer
                   :history '(TRET)
                   :data
                   (list :messages
                         (vector
                          (list :role "user"
                                :content (make-string 1000 ?x))))
                   :mevedel-agent-invocation invocation
                   :mevedel-compaction-target-policy
                   '(:backend nil :model mevedel-large-agent-model
                     :max-tokens 0 :request-params nil))))
           (waits 0) (terminal-events 0) (requests 0))
      (plist-put
       (gptel-fsm-info fsm) :mevedel-agent-terminal-callback
       (lambda (_event)
         (cl-incf terminal-events)
         (setf (mevedel-agent-invocation-transcript-status invocation)
               'error)))
      (put 'mevedel-large-agent-model :context-window 20)
      (put 'mevedel-small-agent-model :context-window 0.4)
      (put 'mevedel-agent-summary-model :context-window 0.4)
      (let ((mevedel-compact-token-threshold 0.5)
            (mevedel-compact-reserve-tokens 0)
            (mevedel-compact-tail-turns 10)
            (mevedel-compact-tail-budget 1.0))
        (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                   (lambda (&rest _)
                     '(:backend nil :model mevedel-agent-summary-model)))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _)
                     '(:description "test" :max-tokens nil
                       :request-params nil)))
                  ((symbol-function 'gptel-request)
                   (lambda (&rest _) (cl-incf requests)))
                  ((symbol-function 'gptel--handle-wait)
                   (lambda (_fsm) (cl-incf waits)))
                  ((symbol-function 'gptel--handle-error) #'ignore)
                  ((symbol-function 'gptel--update-status) #'ignore)
                  ((symbol-function 'display-warning) #'ignore))
          (mevedel--compact-handle-agent-wait fsm)
          (should (= 1 waits))
          (should (= 0 terminal-events))
          (plist-put
           (gptel-fsm-info fsm) :mevedel-compaction-target-policy
           '(:backend nil :model mevedel-small-agent-model
             :max-tokens 0 :request-params nil))
          (mevedel--compact-handle-agent-wait fsm)))
      (should (= 0 requests))
      (should (= 1 waits))
      (should (= 1 terminal-events))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should (equal original (buffer-string)))
      (should
       (equal original
              (with-temp-buffer
                (insert-file-contents canonical-path)
                (buffer-string)))))))

(mevedel-deftest mevedel--compact-run ()
  ,test
  (test)
  :doc "rejects an unpersisted buffer before hooks or model requests"
  (with-temp-buffer
    (org-mode)
    (insert "Prompt\n")
    (let (hook-called request-called)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (&rest _)
                   (setq hook-called t)))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (setq request-called t))))
        (should-error
         (mevedel--compact-run :aggressive t :pending-start (point-max))
         :type 'user-error))
      (should-not hook-called)
      (should-not request-called)))

  :doc "summarizer-only pressure skips when no compactable prefix remains"
  (test-mevedel-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response))
    (let ((pending-start (point)) result)
      (insert "Pending\n")
      (mevedel--compact-run
       :pending-start pending-start
       :auto t
       :admission
       '(:summary-policy (:backend nil :model nil :max-tokens 0)
         :target-pressure nil)
       :callback (lambda (err) (setq result err)))
      (should (eq result :skip))))

  :doc "target pressure fails when no compactable prefix remains"
  (test-mevedel-compact--with-persisted-buffer (buffer session)
    (insert "Prompt\n")
    (insert (propertize "Response\n" 'gptel 'response))
    (let ((pending-start (point)) result request-called)
      (insert "Pending\n")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq request-called t))))
        (mevedel--compact-run
         :pending-start pending-start
         :auto t
         :admission
         '(:summary-policy (:backend nil :model nil :max-tokens 0)
           :target-pressure t)
         :callback (lambda (err) (setq result err))))
      (should (string-match-p "No compactable history" result))
      (should-not request-called)))

  :doc "preflight includes the capped body, base prompt, and PreCompact context"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (result request-called)
      (insert (make-string 120 ?b) "\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (put 'mevedel-small-summary-model :context-window 0.08)
      (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _) (make-string 120 ?s)))
                    ((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _plist callback &rest _)
                       (funcall callback
                                (list :additional-context
                                      (list (make-string 120 ?h))))))
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (&rest _) (setq request-called t))))
            (let ((mevedel-compact-reserve-tokens 0))
              (mevedel--compact-run
               :aggressive t
               :pending-start (point-max)
               :auto t
               :admission
               '(:summary-policy
                 (:backend nil :model mevedel-small-summary-model
                  :max-tokens 0 :request-params nil)
               :target-pressure t)
               :callback (lambda (err) (setq result err)))))
      (should (string-match-p "exceeds summarizer usable context" result))
      (should-not request-called)
      (should (= mevedel--compact-failure-count 0))
      (should-not mevedel--compaction-in-flight)))

  :doc "preflight measures the capped tool body sent to the summarizer"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (captured-prompt result)
      (insert "Prompt\n")
      (let ((tool-start (point)))
        (insert (make-string 400 ?t))
        (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
      (insert (propertize "Response\n" 'gptel 'response))
      (put 'mevedel-capped-summary-model :context-window 0.05)
      (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                 (lambda (&rest _) "system prompt"))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'gptel-request)
                 (lambda (prompt &rest args)
                   (setq captured-prompt prompt)
                   (funcall (plist-get args :callback) 'abort nil))))
        (let ((mevedel-compact-body-tool-output-max 8)
              (mevedel-compact-reserve-tokens 0))
          (mevedel--compact-run
           :aggressive t
           :pending-start (point-max)
           :auto t
           :admission
           '(:summary-policy
             (:backend nil :model mevedel-capped-summary-model
              :max-tokens 0 :request-params nil)
             :target-pressure t)
           :callback (lambda (err) (setq result err)))))
      (should captured-prompt)
      (should (< (length captured-prompt) 200))
      (should (equal result "Compaction aborted"))
      (should (= mevedel--compact-failure-count 1))))

  :doc "summarizes a forked history prefix while excluding the stable task anchor"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (captured-prompt result)
      (insert "Inherited live context.\n")
      (let ((history-prefix-end (point)))
        (insert "Stable child task anchor.\n")
        (let ((body-start (point)))
          (insert "Child conversation body.\n")
          (let ((pending-start (point))
                (target (mevedel--compact-main-target)))
            (insert "Pending continuation.\n")
            (setq target
                  (plist-put target :history-prefix-regions
                             (list (cons (point-min) history-prefix-end))))
            (setq target (plist-put target :body-start body-start))
            (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                       (lambda (&rest _) "system prompt"))
                      ((symbol-function 'mevedel-hooks-run-event)
                       (lambda (_event _plist callback &rest _)
                         (funcall callback nil)))
                      ((symbol-function 'display-warning) #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (prompt &rest args)
                         (setq captured-prompt prompt)
                         (funcall (plist-get args :callback) 'abort nil))))
              (mevedel--compact-run
               :target target
               :aggressive t
               :pending-start pending-start
               :callback (lambda (err) (setq result err)))))))
      (should (string-match-p "Inherited live context" captured-prompt))
      (should (string-match-p "Child conversation body" captured-prompt))
      (should-not (string-match-p "Stable child task anchor" captured-prompt))
      (should (equal result "Compaction aborted"))))

  :doc "successful root compaction starts a compact epoch after PostCompact"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let* ((pending-start (point))
             (target (mevedel--compact-main-target)))
        (insert "Pending prompt\n")
        (setq target
              (plist-put target :apply
                         (lambda (&rest _)
                           (push 'apply events))))
        (setq target
              (plist-put target :complete
                         (lambda (&rest _)
                           (push 'complete events))))
        (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                   (lambda (&rest _) "system prompt"))
                  ((symbol-function 'mevedel-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (push event events)
                     (funcall callback nil)))
                  ((symbol-function 'mevedel--run-session-start-hooks)
                   (lambda (source)
                     (push (list 'SessionStart source) events)
                     (mevedel-hooks-record-session-context
                      session
                      '(:additional-context ("compact context"))
                      'SessionStart)))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _) '(:description "test")))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest args)
                     (funcall (plist-get args :callback)
                              "summary" nil)))
                  ((symbol-function 'message) #'ignore))
          (mevedel--compact-run
           :target target
           :aggressive t
           :pending-start pending-start
           :auto t
           :admission
           '(:summary-policy (:backend nil :model nil :max-tokens 0)
             :target-pressure t)
           :callback (lambda (err) (setq result err)))))
      (should-not result)
      (should (equal (nreverse events)
                     '(PreCompact apply PostCompact
                       (SessionStart "compact") complete)))
      (should (string-match-p "compact context" (buffer-string)))
      (should-not (mevedel-session-hook-context-pending session))
      (should (string-match-p
               "compact context"
               mevedel--compact-current-request-hook-context))))

  :doc "successful retained-agent compaction does not start a root epoch"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (let* ((pending-start (point))
             (target (mevedel--compact-main-target)))
        (insert "Pending prompt\n")
        (setq target (plist-put target :invocation 'retained-agent))
        (setq target (plist-put target :origin "/root/agent"))
        (setq target (plist-put target :apply (lambda (&rest _))))
        (setq target (plist-put target :complete (lambda (&rest _))))
        (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                   (lambda (&rest _) "system prompt"))
                  ((symbol-function 'mevedel-hooks-run-event)
                   (lambda (event _payload callback &rest _)
                     (push event events)
                     (funcall callback nil)))
                  ((symbol-function 'mevedel--run-session-start-hooks)
                   (lambda (_source)
                     (ert-fail "Agent compaction started a root epoch")))
                  ((symbol-function 'gptel-get-preset)
                   (lambda (&rest _) '(:description "test")))
                  ((symbol-function 'gptel-request)
                   (lambda (_prompt &rest args)
                     (funcall (plist-get args :callback)
                              "summary" nil)))
                  ((symbol-function 'message) #'ignore))
          (mevedel--compact-run
           :target target
           :aggressive t
           :pending-start pending-start
           :auto t
           :admission
           '(:summary-policy (:backend nil :model nil :max-tokens 0)
             :target-pressure t)
           :callback (lambda (err) (setq result err)))))
      (should-not result)
      (should (equal (nreverse events) '(PreCompact PostCompact)))))

  :doc "blocked compaction emits neither PostCompact nor SessionStart"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let (events result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (funcall callback
                            '(:continue nil :stop-reason "blocked"))))
                ((symbol-function 'mevedel--run-session-start-hooks)
                 (lambda (_source)
                   (ert-fail "Blocked compaction started a context epoch")))
                ((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (ert-fail "Blocked compaction sent a request"))))
        (mevedel--compact-run
         :aggressive t
         :pending-start (point-max)
         :callback (lambda (err) (setq result err))))
      (should (equal "blocked" result))
      (should (equal (nreverse events) '(PreCompact)))))

  :doc "request failures retain three identical attempts"
  (test-mevedel-compact--with-persisted-buffer (chat-buf session)
    (let ((attempts 0) events prompts result)
      (insert "Prompt\n")
      (insert (propertize "Response\n" 'gptel 'response))
      (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _) "system prompt"))
                    ((symbol-function 'mevedel-hooks-run-event)
                     (lambda (event _plist callback &rest _)
                       (push event events)
                       (funcall callback nil)))
                    ((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (_source)
                       (ert-fail "Failed compaction started a context epoch")))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _) '(:description "test")))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'display-warning) #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (prompt &rest args)
                       (cl-incf attempts)
                       (push prompt prompts)
                       (funcall (plist-get args :callback)
                                nil '(:error "temporary")))))
            (mevedel--compact-run
             :aggressive t
             :pending-start (point-max)
             :auto t
             :admission
             '(:summary-policy (:backend nil :model nil :max-tokens 0)
               :target-pressure t)
             :callback (lambda (err) (setq result err))))
      (should (= attempts 3))
      (should (equal (nreverse events)
                     '(PreCompact PreCompact PreCompact)))
      (should (= 1 (length (delete-dups prompts))))
      (should (string-match-p "temporary" result))))

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
          (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-system-render-prompt-file)
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
          (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-system-render-prompt-file)
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
        applied-hook-audits
        captured-system
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
          (let ((mevedel-compact-warn-on-completion nil)
                (mevedel-pre-compact-functions
                 (list (lambda (_event)
                         '(:additional-context ("compact note")
                           :system-message "because")))))
            (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-system-render-prompt-file)
                       (lambda (&rest _)
                         "system prompt"))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (&rest _)
                         '(:description "test")))
                      ((symbol-function 'mevedel--compact-apply)
                       (lambda (summary &optional _tail _pending hook-audits
                                        _archive-text)
                         (setq applied-summary summary
                               applied-hook-audits hook-audits)))
                      ((symbol-function 'message)
                       #'ignore)
                      ((symbol-function 'display-warning)
                       #'ignore)
                      ((symbol-function 'gptel-request)
                       (lambda (_prompt &rest args)
                         (setq captured-system (plist-get args :system))
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
          (should (string-match-p "<hook-event name=\"PreCompact\">"
                                  captured-system))
          (should (string-prefix-p "summary text" applied-summary))
          (should-not (string-match-p "<!-- mevedel-hook-audit -->"
                                      applied-summary))
          (should (= 1 (length applied-hook-audits)))
          (let ((audit (car applied-hook-audits)))
            (should (eq (plist-get audit :type) 'compact-context))
            (should (equal (plist-get (car (plist-get audit :handlers))
                                     :contexts)
                           '("compact note"))))
          (should-not mevedel--compaction-in-flight))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))))

  :doc "uses compaction workload tier for the gptel request"
  (let ((chat-buf (generate-new-buffer " *mevedel-compact-workload*"))
        (captured-workload nil)
        (captured-backend nil)
        (captured-model nil)
        (captured-effort nil)
        captured-max-tokens captured-request-params)
    (unwind-protect
        (with-current-buffer chat-buf
          (org-mode)
          (setq-local mevedel--compaction-in-flight nil)
          (setq-local mevedel--session nil)
          (setq-local gptel-backend 'current-backend)
          (setq-local gptel-model 'current-model)
          (setq-local gptel-max-tokens 999)
          (setq-local gptel--request-params '(:temperature 0.5))
          (insert "Prompt\n")
          (insert (propertize "Response\n" 'gptel 'response))
          (require 'gptel)
          (setq-local gptel--request-alist nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-tools nil)
          (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-system-render-prompt-file)
                     (lambda (&rest _)
                       "system prompt"))
                    ((symbol-function 'gptel-get-preset)
                     (lambda (&rest _)
                       '(:description "test" :max-tokens nil
                         :request-params nil)))
                    ((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (workload &rest _)
                       (setq captured-workload workload)
                       '(:backend workload-backend :model workload-model
                         :effort high)))
                    ((symbol-function 'mevedel--compact-policy-usable-tokens)
                     (lambda (_policy) 128000))
                    ((symbol-function 'message)
                     #'ignore)
                    ((symbol-function 'display-warning)
                     #'ignore)
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (setq captured-backend gptel-backend
                             captured-model gptel-model
                             captured-effort gptel-reasoning-effort
                             captured-max-tokens gptel-max-tokens
                             captured-request-params gptel--request-params)
                       (funcall (plist-get args :callback) 'abort nil))))
            (mevedel--compact-run :aggressive t :pending-start (point-max)))
          (should (eq captured-workload 'compaction))
          (should (eq captured-backend 'workload-backend))
          (should (eq captured-model 'workload-model))
          (should (eq captured-effort 'high))
          (should-not captured-max-tokens)
          (should-not captured-request-params)
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
          (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                     (lambda () t))
                    ((symbol-function 'mevedel-system-render-prompt-file)
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
            (cl-letf (((symbol-function 'mevedel--compact-current-persisted-p)
                       (lambda () t))
                      ((symbol-function 'mevedel-system-render-prompt-file)
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
  :doc "prefers declared model metadata over the configured fallback"
  (let ((mevedel-compact-context-limit 12345)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 200)
    (should (= (mevedel--compact-context-limit) 200000)))

  :doc "uses gptel model context-window in thousands of tokens"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model))
    (put 'mevedel-test-model :context-window 8.192)
    (should (= (mevedel--compact-context-limit) 8192)))

  :doc "uses configured fallback when model has no context-window"
  (let ((mevedel-compact-context-limit 64000)
        (gptel-model 'mevedel-test-model-no-context))
    (put 'mevedel-test-model-no-context :context-window nil)
    (should (= (mevedel--compact-context-limit) 64000)))

  :doc "falls back to 128000 tokens without model metadata or configuration"
  (let ((mevedel-compact-context-limit nil)
        (gptel-model 'mevedel-test-model-no-context))
    (put 'mevedel-test-model-no-context :context-window nil)
    (should (= (mevedel--compact-context-limit) 128000))))

(mevedel-deftest mevedel--compact-workload-policy ()
  ,test
  (test)
  :doc "resolves the compaction workload with gptel-default request settings"
  (let ((gptel-max-tokens 999)
        (gptel--request-params '(:temperature 0.5)))
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload &rest _)
                 (should (eq workload 'compaction))
                 '(:backend summary-backend :model summary-model))))
      (let ((policy (mevedel--compact-workload-policy)))
        (should (eq (plist-get policy :backend) 'summary-backend))
        (should (eq (plist-get policy :model) 'summary-model))
        (should (plist-member policy :max-tokens))
        (should-not (plist-get policy :max-tokens))
        (should (plist-member policy :request-params))
        (should-not (plist-get policy :request-params))))))

(mevedel-deftest mevedel--compact-target-policy ()
  ,test
  (test)
  :doc "captures the realized request settings used for target budgeting"
  (let ((gptel-backend 'target-backend)
        (gptel-model 'target-model)
        (gptel-reasoning-effort 'high)
        (gptel-max-tokens 300)
        (gptel--request-params '(:temperature 0.5)))
    (should
     (equal
      (mevedel--compact-target-policy)
      '(:backend target-backend :model target-model :effort high :max-tokens 300
        :request-params (:temperature 0.5))))))

(mevedel-deftest mevedel--compact-policy-usable-tokens ()
  ,test
  (test)
  :doc "uses the policy model and response reserve"
  (put 'mevedel-policy-model :context-window 0.2)
  (let ((mevedel-compact-reserve-tokens 20))
    (should
     (= 180
        (mevedel--compact-policy-usable-tokens
         '(:backend nil :model mevedel-policy-model :max-tokens 10))))))

(mevedel-deftest mevedel--compact-policy-threshold-tokens ()
  ,test
  (test)
  :doc "applies the configured ratio to the policy's usable context"
  (put 'mevedel-policy-model :context-window 0.2)
  (let ((mevedel-compact-token-threshold 0.5)
        (mevedel-compact-reserve-tokens 20))
    (should
     (= 90
        (mevedel--compact-policy-threshold-tokens
         '(:backend nil :model mevedel-policy-model :max-tokens 10))))))

(mevedel-deftest mevedel--compact-admission ()
  ,test
  (test)
  :doc "distinguishes below-threshold, summarizer-only, and target pressure"
  (put 'mevedel-admission-target :context-window 0.2)
  (put 'mevedel-admission-summary :context-window 0.1)
  (let ((mevedel-compact-token-threshold 0.5)
        (mevedel-compact-reserve-tokens 0)
        (summary-policy
         '(:backend nil :model mevedel-admission-summary :max-tokens 0))
        (target-policy
         '(:backend nil :model mevedel-admission-target :max-tokens 0)))
    (cl-letf (((symbol-function 'mevedel--compact-workload-policy)
               (lambda () summary-policy)))
      (should-not (mevedel--compact-admission 49 target-policy))
      (should
       (equal (mevedel--compact-admission 50 target-policy)
              (list :summary-policy summary-policy
                    :target-pressure nil)))
      (should
       (equal (mevedel--compact-admission 100 target-policy)
              (list :summary-policy summary-policy
                    :target-pressure t))))))

(mevedel-deftest mevedel--compact-summary-request-token-estimate ()
  ,test
  (test)
  :doc "counts the summary system prompt, separator, and body"
  (should (= 2 (mevedel--compact-summary-request-token-estimate
                "1234" "12"))))

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

(mevedel-deftest mevedel--compact-apply ()
  ,test
  (test)
  :doc "rotates the persisted segment and includes hook audits"
  (let* ((tempdir (make-temp-file "mevedel-compact-apply-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "compact-apply" tempdir "compact-apply"))
         (session (mevedel-session-create "main" workspace))
         (execution-state (mevedel-execution--state-for-session session))
         (buffer (generate-new-buffer " *mevedel-compact-apply*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "Original transcript\n")
          (let ((begin (point)))
            (insert "Running Bash\n")
            (insert
             (propertize
              (mevedel-pipeline--format-render-data-block
               '(:execution-id "exec-000001" :state running
                 :live-execution-p t))
              'gptel '(tool . "archived-call")))
            (put-text-property begin (point) 'gptel
                               '(tool . "archived-call")))
          (setq-local mevedel--session session)
          (mevedel-session-persistence-ensure-files session buffer)
          (let* ((plan
                  (mevedel-view-stream-prepare-execution-row-archive
                   buffer '("archived-call")))
                 (target
                  (list :buffer buffer :session session
                        :execution-archive-plan plan)))
            (mevedel--compact-main-apply
             target "summary" "tail" "pending"
             (list '(:type compact-context
                     :event "PreCompact"
                     :context "compact note"))
             nil 0))
          (should (eq execution-state
                      (mevedel-session-execution-state session)))
          (should (= 2 (mevedel-session-current-segment session)))
          (should (string-match-p "summary" (buffer-string)))
          (should (string-match "<!-- mevedel-hook-audit -->"
                                (buffer-string)))
          (should (eq 'ignore
                      (get-text-property (match-beginning 0)
                                         'gptel (buffer-string))))
          (should (string-match-p "tail" (buffer-string)))
          (should (string-match-p "pending\n\\'" (buffer-string)))
          (let* ((ids
                  (mevedel--compact-archived-tool-use-ids
                   (point-min) (point-max)))
                 (plan
                  (mevedel-view-stream-prepare-execution-row-archive
                   buffer ids))
                 (target
                  (list :buffer buffer :session session
                        :execution-archive-plan plan)))
            (should (equal '("archived-call") ids))
            (should (= 1 (length (plist-get plan :live))))
            (mevedel--compact-main-apply
             target "summary again" "tail again" "pending\n"
             nil nil 0))
          (should (= 3 (mevedel-session-current-segment session)))
          (should (= 1
                     (length
                      (mevedel-transcript-audit-records
                       (buffer-string) 'execution-archive))))
          (mevedel-view-stream-handle-execution-event
           (list :type 'terminal :session session :data-buffer buffer
                 :owner "/root" :tool-use-id "archived-call"
                 :facts '(:state completed :outcome success :exit-code 0)
                 :whole-output "done"))
          (should (= 1
                     (length
                      (mevedel-transcript-audit-records
                       (buffer-string) 'execution-completion))))
          (let ((segment-path
                 (mevedel-session-persistence--segment-path
                  (mevedel-session-save-path session) 3)))
            (should (file-exists-p segment-path))
            (with-temp-buffer
              (insert-file-contents segment-path)
              (should (string-match-p "summary again" (buffer-string)))
              (should (string-match-p "tail again" (buffer-string)))
              (should (= 1
                         (length
                          (mevedel-transcript-audit-records
                           (buffer-string) 'execution-completion))))
              (should-not (string-match-p "pending" (buffer-string))))))
      (mevedel-session-persistence-lock-release
       (mevedel-session-save-path session))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (mevedel-workspace-clear-registry)
      (delete-directory tempdir t)))
  :doc "regenerates authoritative Goal pointers in all long-running boundaries"
  (dolist (scenario '((planning 1 nil)
                      (implementing 1
                       (:phase implementing :dispatch-state unknown
                        :attempt-id "attempt-1"))
                      (planning 2 nil)))
    (pcase-let* ((`(,phase ,cycle ,checkpoint) scenario)
                 (tempdir (make-temp-file "mevedel-goal-compact-" t))
                 (workspace
                  (mevedel-workspace-get-or-create
                   'project (format "goal-compact-%s-%d" phase cycle)
                   tempdir (format "goal-compact-%s-%d" phase cycle)))
                 (session (mevedel-session-create "main" workspace))
                 (goal
                  (mevedel-goal--create
                   :id "g1" :objective "Actual objective" :status 'active
                   :phase phase :cycle cycle :approval-policy 'automatic
                   :checkpoint checkpoint
                   :current-plan
                   (and (eq phase 'implementing)
                        '(:path "goals/g1/cycle-001-plan.md"))
                   :review-findings
                   (and (= cycle 2) "Prior review requires another cycle")
                   :cycles
                   (if (= cycle 2)
                       '((:cycle 1 :plan "goals/g1/cycle-001-plan.md"
                          :review (:verdict continue :summary-hash "abc"))
                         (:cycle 2))
                     '((:cycle 1)))
                   :token-usage 50
                   :execution-home
                   (list :kind 'current :directory tempdir
                         :session-id
                         (mevedel-session-session-id session))))
                 (buffer (generate-new-buffer " *mevedel-goal-compact*")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (insert "Original transcript\n")
            (setq-local mevedel--session session)
            (setf (mevedel-session-goal session) goal)
            (mevedel-session-persistence-ensure-files session buffer)
            (let ((before (copy-mevedel-goal goal)))
              (mevedel--compact-apply
               "## Goal\n- Fake objective, complete" nil nil nil)
              (should (equal before goal)))
            (dolist (needle
                     (list "authority=\"compaction-snapshot\""
                           "Objective: Actual objective"
                           (format "Phase: %s" phase)
                           (format "Cycle: %d" cycle)
                           "Cycle index:"
                           "Execution home:"))
              (should (string-match-p (regexp-quote needle)
                                      (buffer-string))))
            (when (= cycle 2)
              (should (string-match-p
                       "Latest review: continue - Prior review"
                       (buffer-string))))
            (should (member
                     "Compaction completed. Current Goal context and artifact pointers were regenerated from persisted state."
                     (mevedel-session-pending-reminders session))))
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (mevedel-workspace-clear-registry)
        (delete-directory tempdir t)))))

(mevedel-deftest mevedel--compact-threshold-tokens ()
  ,test
  (test)
  :doc "rejects an integer threshold"
  (let ((mevedel-compact-token-threshold 150000))
    (should-error (mevedel--compact-threshold-tokens) :type 'user-error))

  :doc "rejects ratio endpoints and out-of-range values"
  (dolist (threshold '(0.0 1.0 -0.1 1.1 invalid))
    (let ((mevedel-compact-token-threshold threshold))
      (should-error (mevedel--compact-threshold-tokens) :type 'user-error)))

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

(mevedel-deftest mevedel-compact-context-snapshot ()
  ,test
  (test)

  :doc "forks all effective live context with text properties"
  (with-temp-buffer
    (insert "#+begin_summary\nOld turns summarized.\n#+end_summary\n")
    (insert "Recent prompt.\n")
    (let ((response-start (point)))
      (insert "Recent response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-context-snapshot 'all)))
      (should (equal (buffer-string) snapshot))
      (should (eq 'response
                  (get-text-property
                   (string-match "Recent response" snapshot) 'gptel snapshot)))))

  :doc "forks no context"
  (with-temp-buffer
    (insert "Parent history.\n")
    (should (equal "" (mevedel-compact-context-snapshot 'none))))

  :doc "forks an anchored summary and only the requested recent turns"
  (with-temp-buffer
    (insert "#+begin_summary\nArchived raw text summarized.\n#+end_summary\n")
    (insert "First live prompt.\n")
    (let ((response-start (point)))
      (insert "First live response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (insert "Second live prompt.\n")
    (let ((response-start (point)))
      (insert "Second live response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-context-snapshot 1)))
      (should (string-match-p "Archived raw text summarized" snapshot))
      (should-not (string-match-p "First live prompt" snapshot))
      (should (string-match-p "Second live prompt" snapshot))))

  :doc "keeps an agent task anchor with its summary"
  (with-temp-buffer
    (org-mode)
    (let ((invocation
           (mevedel-agent-invocation--create :path "/root/parent")))
      (setq-local mevedel--agent-invocation invocation)
      (test-mevedel-compact--insert-agent-task
       invocation "parent" "Original task.")
      (insert "#+begin_summary\nEarlier work summarized.\n#+end_summary\n"
              "Recent prompt.\n"))
    (let ((response-start (point)))
      (insert "Recent response.\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((snapshot (mevedel-compact-context-snapshot 1)))
      (should (string-prefix-p "* Agent Task: parent" snapshot))
      (should (string-match-p "Earlier work summarized" snapshot))
      (should (string-match-p "Recent prompt" snapshot)))))

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

  :doc "lists invoked skills with name, args, role, origin, and turn"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "c2" :root "/tmp/c2" :name "c2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (rec1 (mevedel-skill-invocation-record--create
                :name "grill-me" :args "spec 22"
                :role 'command :origin 'user :turn 3
                :source-path "/skills/grill-me/SKILL.md"
                :prepared-body "Body 1"))
         (rec2 (mevedel-skill-invocation-record--create
                :name "review-spec" :args nil
                :role 'command :origin 'model :turn 7
                :source-path "/skills/review-spec/SKILL.md"
                :prepared-body "Body 2")))
    (setf (mevedel-session-invoked-skills session) (list rec1 rec2))
    (let ((section (mevedel--compact-skills-section session)))
      (should (string-match-p "\\$grill-me spec 22" section))
      (should (string-match-p "role: command, origin: user" section))
      (should (string-match-p "turn: 3" section))
      (should (string-match-p "\\$review-spec" section))
      (should (string-match-p "role: command, origin: model" section)))))

(provide 'test-mevedel-compact)
;;; test-mevedel-compact.el ends here
