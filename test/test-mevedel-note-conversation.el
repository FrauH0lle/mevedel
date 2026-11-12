;;; test-mevedel-note-conversation.el -- Note send flows -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise note commands in real files with the normal session/view pipeline.
;; Only gptel's network transport is replaced with canned answers.

;;; Code:

(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-claude-code)
(require 'mevedel-notes)
(require 'mevedel-session-persistence)
(require 'mevedel-transport)
(require 'mevedel-journal-process)
(require 'gptel-openai)
(require 'mevedel-file-state)
(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-note-conversation-test--with-source (&rest body)
  "Run BODY in a real source file, replacing only the model transport."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mevedel-note-send-" t))
          (file (file-name-concat root "source.el"))
          (mevedel-sessions-directory (file-name-concat root "sessions"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (mevedel-journal-enabled nil)
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel--known-presets (copy-tree gptel--known-presets))
          (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
          (transport-enabled mevedel-transport--enabled-p)
          (gptel--known-backends nil)
          (gptel-backend (gptel-make-openai "Notes" :key "test" :models '(test-model)))
          (gptel-model 'test-model)
          (gptel-use-curl t)
          (gptel-stream nil)
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          source workspace sent)
     (unwind-protect
         (save-window-excursion
           (make-directory (file-name-concat root ".mevedel"))
           (write-region "outside-before\nselected code\noutside-after\n" nil file nil 'silent)
           (write-region "Project-only instruction" nil
                         (file-name-concat root "AGENTS.md") nil 'silent)
           (setq workspace (mevedel-workspace-get-or-create 'file root root "notes")
                 source (find-file-noselect file))
           (mevedel-install)
           (cl-letf (((symbol-function 'gptel-curl-get-response)
                      (lambda (fsm) (push fsm sent))))
             (switch-to-buffer source)
             (setq-local mevedel--workspace workspace)
             (goto-char (point-min))
             (forward-line 1)
             ,@body))
       (dolist (buf (cl-set-difference (buffer-list) before))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (set-buffer-modified-p nil)
             (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
       (dolist (timer (append timer-list timer-idle-list nil))
         (unless (memq timer timers) (cancel-timer timer)))
       (mevedel-workspace-clear-registry)
       (setq mevedel-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
       (mevedel-uninstall)
       (when transport-enabled (mevedel-transport-install))
       (delete-directory root t))))

(defun mevedel-note-conversation-test--answer (fsm text)
  "Deliver canned TEXT through FSM's normal insertion and settlement."
  (let ((info (gptel-fsm-info fsm)))
    (plist-put info :http-status "200")
    (plist-put info :status "HTTP/1.1 200 OK")
    (gptel--fsm-transition fsm)
    (gptel--insert-response text info)
    (gptel--fsm-transition fsm)
    (mevedel-test--await 5 "Note answer did not settle"
      (not (mevedel-turn-busy-p (plist-get info :buffer))))))

(mevedel-deftest mevedel-note-send
  (:quiet t)
  ,test
  (test)
  :doc "send focuses a conversation with a read-only answer and editable prompt"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Why is this selected?"))
           (view (mevedel-note-send note)))
      (should (eq view (window-buffer (selected-window))))
      (should (window-parameter (selected-window) 'window-side))
      (should (= 1 (length sent)))
      (mevedel-note-conversation-test--answer (car sent) "It is the selected code.")
      (with-current-buffer view
        (goto-char (point-min))
        (should (search-forward "It is the selected code." nil t))
        (should-error (insert "not editable") :type 'text-read-only)
        (goto-char (point-max))
        (insert "A reply without the minibuffer"))
      (with-current-buffer source
        (should (equal "outside-before\nselected code\noutside-after\n" (buffer-string))))))
  :doc "initial provider request and sent inspector exclude unrelated workspace context"
  (mevedel-note-conversation-test--with-source
    (make-directory (file-name-concat root ".mevedel" "memory") t)
    (write-region "UNRELATED-MEMORY-SECRET" nil
                  (file-name-concat root ".mevedel" "memory" "MEMORY.md") nil 'silent)
    (mevedel-note-create "UNRELATED-NOTE-SECRET")
    (goto-char (point-min))
    (mevedel--create-reference-in source (point) (line-end-position))
    (let ((main (mevedel--chat-buffer "main" t workspace)))
      (with-current-buffer main (insert "UNRELATED-CHAT-SECRET")))
    (forward-line 1)
    (let* ((note (mevedel-note-create "Explain this code?"))
           (view (mevedel-note-send note))
           (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
      (dolist (expected '("Explain this code?" "selected code" "Project-only instruction"))
        (should (string-search expected payload)))
      (dolist (excluded '("UNRELATED-MEMORY-SECRET" "UNRELATED-NOTE-SECRET"
                          "UNRELATED-CHAT-SECRET" "outside-before" "outside-after"))
        (should-not (string-search excluded payload)))
      (with-current-buffer view
        (let ((report (call-interactively (key-binding (kbd "C-c C-v")))))
          (with-current-buffer report
            (should (equal payload (buffer-substring-no-properties (point-min) (point-max)))))))
      (mevedel-note-conversation-test--answer (car sent) "An explanation, not an edit.")))
  :doc "close preserves the conversation and send reopens without another request"
  (mevedel-note-conversation-test--with-source
    (let* ((origin (point))
           (note (mevedel-note-create "Explain, please"))
           (view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "A retained answer.")
      (with-current-buffer view
        (goto-char (point-max))
        (insert "Draft still here")
        (call-interactively (key-binding (kbd "C-c C-q"))))
      (should (buffer-live-p view))
      (should (eq source (window-buffer (selected-window))))
      (should (= origin (window-point (selected-window))))
      (with-current-buffer source
        (should (eq view (mevedel-note-send note))))
      (should (= 1 (length sent)))
      (with-current-buffer view
        (should (string-search "A retained answer." (buffer-string)))
        (should (string-suffix-p "Draft still here" (buffer-string))))))
  :doc "source menu sends a note without installing plain-letter source bindings"
  (mevedel-note-conversation-test--with-source
    (mevedel-note-create "Menu question?")
    (should (eq #'self-insert-command (key-binding "s")))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (assq ?s choices))))
      (call-interactively #'mevedel-note-menu))
    (should (= 1 (length sent)))
    (mevedel-note-conversation-test--answer (car sent) "Sent through the menu.")))

(mevedel-deftest mevedel-note-send/chat-selection-isolation
  (:quiet t)
  ,test
  (test)
  :doc "ordinary chat entry never adopts a sent note's dedicated conversation"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "A dedicated question"))
           (note-view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "Only this note's answer")
      (with-current-buffer note-view (mevedel-note-conversation-close))
      (with-current-buffer source
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Note session offered in ordinary chat chooser"))))
          (call-interactively #'mevedel)))
      (should-not (eq note-view (window-buffer (selected-window))))
      (with-current-buffer source
        (should (eq note-view (mevedel-note-send note)))))))

(mevedel-deftest mevedel-note-send/edited-file-isolation
  (:quiet t)
  ,test
  (test)
  :doc "send does not inject unrelated changed files remembered by another chat"
  (mevedel-note-conversation-test--with-source
    (let ((other (file-name-concat root "other.txt")))
      (write-region "old unrelated content" nil other nil 'silent)
      (mevedel-file-cache-put (mevedel-workspace-file-cache workspace)
                              (mevedel-file-state-from-file other))
      (write-region "UNRELATED-EDIT-SECRET" nil other nil 'silent)
      (set-file-times other (time-add (current-time) 2))
      (mevedel-note-send (mevedel-note-create "Only this selected code"))
      (mevedel-test--await 5 "Note request did not reach transport" sent)
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should-not (string-search "UNRELATED-EDIT-SECRET" payload))
        (should-not (string-search "other.txt" payload)))
      (mevedel-note-conversation-test--answer (car sent) "Only this note."))))

(mevedel-deftest mevedel-note-send-batch
  (:quiet t)
  ,test
  (test)
  :doc "batch sends two notes into one durable conversation and either note reopens it"
  (mevedel-note-conversation-test--with-source
    (let ((first (mevedel-note-create "First question")))
      (forward-line 1)
      (let* ((second (mevedel-note-create "Related question"))
             (view (mevedel-note-send-batch (list first second)))
             (payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (= 1 (length sent)))
        (dolist (text '("First question" "Related question" "selected code" "outside-after"))
          (should (string-search text payload)))
        (mevedel-note-conversation-test--answer (car sent) "One answer for both.")
        (with-current-buffer source
          (should (eq view (mevedel-note-send first)))
          (should (eq view (mevedel-note-send second)))
          (should (eq view (mevedel-note-send-batch (list first second)))))
        (should (= 1 (length sent))))))
  :doc "batch refuses mixing existing conversations without losing either thread"
  (mevedel-note-conversation-test--with-source
    (let* ((first (mevedel-note-create "First"))
           (first-view (mevedel-note-send first))
           second second-view)
      (mevedel-note-conversation-test--answer (car sent) "First answer")
      (with-current-buffer source
        (forward-line 1)
        (setq second (mevedel-note-create "Second")
              second-view (mevedel-note-send second)))
      (mevedel-note-conversation-test--answer (car sent) "Second answer")
      (with-current-buffer source
        (should-error (mevedel-note-send-batch (list first second)) :type 'user-error)
        (should (eq first-view (mevedel-note-send first)))
        (should (eq second-view (mevedel-note-send second)))))))

(mevedel-deftest mevedel-note-conversation-reply
  (:quiet t)
  ,test
  (test)
  :doc "one-key reply uses its own history and background answer preserves the next draft"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Explain this?"))
           (view (mevedel-note-send note)))
      (mevedel-note-conversation-test--answer (car sent) "First answer.")
      (with-current-buffer view
        (goto-char (point-max))
        (insert "And why?")
        (call-interactively (key-binding (kbd "C-c C-c"))))
      (should (= 2 (length sent)))
      (let ((payload (gptel--json-encode (plist-get (gptel-fsm-info (car sent)) :data))))
        (should (string-search "First answer." payload))
        (should (string-search "And why?" payload)))
      (with-current-buffer view
        (goto-char (point-max))
        (insert "> multiline draft\nsecond line")
        (let ((position (- (point) (mevedel-view--input-start)))
              (start (window-start))
              (window (selected-window)))
          (mevedel-note-conversation-test--answer (car sent) "Second answer.")
          (should (eq window (selected-window)))
          (should (= position (- (point) (mevedel-view--input-start))))
          (should (= start (window-start)))
          (should (string-suffix-p "> multiline draft\nsecond line" (buffer-string))))))))

(mevedel-deftest mevedel-note-conversation-restart
  (:quiet t)
  ,test
  (test)
  :doc "a fresh Emacs reopens the saved note's session and original sent input"
  (mevedel-note-conversation-test--with-source
    (let* ((note (mevedel-note-create "Question before restart?"))
           (view (mevedel-note-send note))
           (data (buffer-local-value 'mevedel--data-buffer view))
           (script (file-name-concat root "restart.el")))
      (mevedel-note-conversation-test--answer (car sent) "Answer survives restart.")
      (with-current-buffer data (mevedel-session-persistence-release-on-kill))
      (mevedel-chat-discard-buffers data t)
      (write-region
       (concat ";;; restart.el -*- lexical-binding: t -*-\n" (prin1-to-string
        `(progn
           (setq load-path ',load-path)
           (require 'mevedel)
           (require 'gptel-openai)
           (require 'mevedel-persistence)
           (setq gptel-backend (gptel-make-openai "Notes" :key "test" :models '(test-model))
                 gptel-model 'test-model
                 mevedel-sessions-directory ,mevedel-sessions-directory)
           (mevedel-install)
           (let* ((workspace (mevedel-workspace-get-or-create 'file ,root ,root "notes"))
                  (source (find-file-noselect ,file)))
             (switch-to-buffer source)
             (setq-local mevedel--workspace workspace)
             (mevedel-instruction-store-observe workspace)
             (goto-char (point-min))
             (forward-line 1)
             (let ((view (mevedel-note-send)))
               (with-current-buffer view
                 (let ((deadline (+ (float-time) 5)))
                   (while (and (not (string-search "Answer survives restart." (buffer-string)))
                               (< (float-time) deadline))
                     (accept-process-output nil 0.01)))
                 (unless (string-search "Answer survives restart." (buffer-string))
                   (error "Lost note answer"))
                 (let ((report (call-interactively (key-binding (kbd "C-c C-v")))))
                   (with-current-buffer report
                     (unless (string-search "Question before restart?" (buffer-string))
                       (error "Lost sent input")))))
               (princ "NOTE-RESTART-PASSED")))
           (kill-emacs 0))))
       nil script nil 'silent)
      (with-temp-buffer
        (let ((default-directory root))
          (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                      nil (current-buffer) nil "-Q" "--batch" "-l" script)))
            (ert-info ((buffer-string)) (should (equal 0 status)))))
        (should (string-search "NOTE-RESTART-PASSED" (buffer-string)))))))

(mevedel-deftest mevedel-note-send/native
  (:quiet t)
  ,test
  (test)
  :doc "native note session retains its exact initial context without memory recall"
  (mevedel-note-conversation-test--with-source
    (let ((mevedel-claude-code-directory (file-name-concat root "claude"))
          (acp--shutdown-grace-period 0)
          system prompts view data)
      (make-directory (file-name-concat root ".mevedel" "memory") t)
      (write-region "NATIVE-MEMORY-SECRET" nil
                    (file-name-concat root ".mevedel" "memory" "MEMORY.md") nil 'silent)
      (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason) #'ignore)
                ((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (prompt &rest _)
                    (setq system prompt)
                    (list :meta '((echoAllText . t)))))))
        (mevedel-claude-code-register)
        (let ((gptel-backend (gptel-get-backend "Claude Code"))
              (gptel-model 'sonnet))
          (advice-add 'mevedel-acp-prompt :before
                      (lambda (_client input &rest _) (push input prompts))
                      '((name . mevedel-note-native-test)))
          (unwind-protect
              (progn
                (setq view (mevedel-note-send (mevedel-note-create "Native question?"))
                      data (buffer-local-value 'mevedel--data-buffer view))
                (mevedel-test--await 10 "Native note did not settle"
                  (not (mevedel-turn-busy-p data)))
                (should prompts)
                (should (string-search "Native question?" (format "%S" prompts)))
                (should-not (string-search "NATIVE-MEMORY-SECRET" (format "%S" prompts)))
                (with-current-buffer view
                  (let* ((report (call-interactively (key-binding (kbd "C-c C-v"))))
                         (payload (with-current-buffer report
                                    (json-parse-string (buffer-string) :object-type 'alist))))
                    (should (equal system (alist-get 'system payload)))
                    (should (equal (car prompts) (alist-get 'input payload))))))
            (advice-remove 'mevedel-acp-prompt 'mevedel-note-native-test)))))))

(provide 'test-mevedel-note-conversation)
;;; test-mevedel-note-conversation.el ends here
