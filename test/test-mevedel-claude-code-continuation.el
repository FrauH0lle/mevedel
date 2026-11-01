;;; test-mevedel-claude-code-continuation.el --- Full native context continuation -*- lexical-binding: t -*-

;;; Commentary:
;; A native hook's size limit must not require a new user turn for context.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-claude-code)
(require 'mevedel-view-stream)

(mevedel-deftest mevedel--send-request/claude-large-context (:quiet t)
                 (dolist (mode '(normal missing pause cancel restore))
                   (mevedel-engine-test--with-claude-session
                    (let* ((mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
                           (mevedel-system-retained-components '(memory))
                           (state "Initial memory")
                           (changed (concat "FULL-UPDATE-" (make-string 5100 ?😀) "-END"))
                           (file (file-name-concat root "evidence.txt"))
                           (mevedel-goal-token-budget 48)
                           (goal (mevedel-goal-create "Finish this bounded native turn" :session session))
                           (calls 0) prompts decisions readings steered
                           (mevedel-post-tool-use-functions
                            (list (lambda (_event)
                                    (cl-incf calls)
                                    (unless (eq mode 'restore) (setq state changed))
                                    nil))))
                      (setf (alist-get 'memory mevedel-system--prompt-components)
                            (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state)))
                      (write-region "Evidence" nil file nil 'silent)
                      (setq-local gptel-system-prompt "Continuation fixture"
                                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
                      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                 (mevedel-engine-test--claude-launch
                                  (lambda (_system mcp _model _effort &optional id hook)
                                    (list :observe (lambda (owner event)
                                                     (when (and (eq mode 'restore)
                                                                (equal "agent_message_chunk" (map-nested-elt event '(params update sessionUpdate))))
                                                       (setq state changed))
                                                     (let ((message (alist-get 'message (alist-get 'params event))))
                                                       (when (equal "user" (alist-get 'type message))
                                                         (cl-pushnew (map-nested-elt message '(message content)) prompts :test #'equal)))
                                                     (mevedel-claude-code--observe owner event))
                                          :control (lambda (owner event)
                                                     (unless (equal "PreToolUse" (plist-get event :hook_event_name))
                                                       (push (copy-sequence (plist-get (mevedel-engine-info owner) :tokens-full)) readings))
                                                     (let ((decision (mevedel-claude-code--control owner event)))
                                                       (when (eq :json-false (plist-get decision :continue))
                                                         (when (and (eq mode 'restore) (not steered))
                                                           (setq steered t)
                                                           (mevedel-session-enqueue-pending-input
                                                            session 'steering
                                                            (list :input "RESTORATION-STEER-4801"
                                                                  :request-id (mevedel-request-id request))))
                                                         (pcase mode
                                                           ('pause (mevedel-goal-pause))
                                                           ('cancel (mevedel-abort buffer))))
                                                       (unless (equal "PreToolUse" (plist-get event :hook_event_name))
                                                         (push decision decisions)) decision))
                                          :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                                  (preToolHook . t)
                                                  (compactBeforeBatch . ,(when (eq mode 'restore) 1))
                                                  (sdkBeforeBatches . [[((type . "assistant")
                                                                         (message . ((id . "sample-A")
                                                                                     (usage . ((input_tokens . 13) (cache_creation_input_tokens . 0) (output_tokens . 7))))))]])
                                                  (toolBatches . [[((name . "Read") (id . "before-full-context")
                                                                    (args . ((file_path . ,file))))]
                                                                  [((name . "Read") (id . "must-not-run-before-full-context")
                                                                    (args . ((file_path . ,file))))]])
                                                  (promptResponse . ((stopReason . "end_turn")
                                                                     (usage . ((inputTokens . 13) (cachedWriteTokens . 0) (outputTokens . 7)))))
                                                  (continuationPrompts . [((promptAcknowledgement . ,(if (eq mode 'missing) :false t))
                                                                           (sdkBeforeBatches . [[((type . "assistant")
                                                                                                  (message . ((id . "sample-A")
                                                                                                              (usage . ((input_tokens . 13) (cache_creation_input_tokens . 0) (output_tokens . 7))))))
                                                                                                 ((type . "assistant")
                                                                                                  (message . ((id . "sample-B")
                                                                                                              (usage . ((input_tokens . 17) (cache_creation_input_tokens . 0) (output_tokens . 11))))))]])
                                                                           (toolBatches . [[((name . "Read") (id . "after-full-context")
                                                                                             (args . ((file_path . ,file))))]])
                                                                           (promptResponse . ((stopReason . "end_turn")
                                                                                              (usage . ((inputTokens . 17) (cachedWriteTokens . 0) (outputTokens . 11))))))])))))))
                        (mevedel--insert-user-turn "Read with the latest memory")
                        (mevedel--send-request "Read with the latest memory")
                        (mevedel-test--await 8 "Full-context continuation did not settle"
                          (not (mevedel-turn-busy-p buffer))))
                      (ert-info ((format "mode=%s error=%S" mode (plist-get (mevedel-engine-info request) :error)))
                                (should (eq (pcase mode ('missing 'error) ('cancel 'aborted) (_ 'success))
                                            (plist-get (mevedel-engine-info request) :mevedel-acp-outcome))))
                      (should (= (if (memq mode '(normal restore)) 2 1) calls))
                      (ert-info ((format "Prompts: %S; decisions: %S"
                                         (mapcar (lambda (parts)
                                                   (mapcar (lambda (part) (truncate-string-to-width (alist-get 'text part) 160)) parts)) prompts)
                                         decisions))
                                (should (= (if (memq mode '(normal restore)) 2 1) (length prompts))))
                      (when (memq mode '(normal restore))
                        (should (string-search changed (prin1-to-string (car prompts))))
                        (should (eq :json-false (plist-get (cadr decisions) :continue))))
                      (when (eq mode 'normal)
                        (should (equal
                                 "Continue the current task after applying the complete context updates below. The prior native prompt stopped only to deliver this context in full. Earlier tool effects remain applied; do not replay them."
                                 (alist-get 'text (seq-elt (car prompts) 0)))))
                      (when (eq mode 'restore)
                        (should (equal "RESTORATION-STEER-4801"
                                       (alist-get 'text (seq-elt (car prompts) (1- (length (car prompts)))))))
                        (should-not (string-search "Continue the current task" (prin1-to-string (car prompts))))
                        (should (= 1 (how-many "RESTORATION-STEER-4801" (point-min) (point-max)))))
                      (should (equal (append (when (memq mode '(normal restore)) '((:input 30 :output 18 :cache 0)))
                                             '((:input 13 :output 7 :cache 0))
                                             (when (eq mode 'restore) '((:input 13 :output 7 :cache 0)))) readings))
                      (should (= (if (memq mode '(normal restore)) 1 0) (mevedel-engine-test--count-evidence "FULL-UPDATE-")))
                      (should (= 1 (mevedel-session-turn-count session)))
                      (should (= 1 (mevedel-goal-turns-run goal)))
                      (if (eq mode 'missing)
                          ;; MCP refusal may race the later SDK usage notification on stdio.
                          (should (memq (mevedel-goal-tokens-used goal) '(20 48)))
                        (should (= (if (memq mode '(pause cancel)) 20 48) (mevedel-goal-tokens-used goal))))))))

(mevedel-deftest mevedel--send-request/claude-steering-boundary (:quiet t)
  (dolist (mode '(normal end-turn cancel-before cancel-after))
    (mevedel-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (draft "> Keep this draft\nwith its second line")
             (calls 0) (terminals 0) steered stopped received prompts view
             (gptel-post-response-functions
              (cons (lambda (&rest _) (cl-incf terminals)) gptel-post-response-functions))
             (mevedel-post-tool-use-functions
              (list (lambda (_event)
                      (cl-incf calls)
                      (unless steered
                        (setq steered t)
                        (mevedel-session-enqueue-pending-input
                         session 'steering (list :input "BOUNDARY-USER-8841"
                                                 :request-id (mevedel-request-id request))))
                      nil))))
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Steering boundary fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (unwind-protect
            (save-window-excursion
              (setq view (mevedel-view--ensure buffer))
              (set-window-buffer (selected-window) view)
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (mevedel-engine-test--claude-launch
                          (lambda (_system _mcp _model _effort &optional _id hook)
                            (list :control
                                  (lambda (owner event)
                                    (let ((decision (mevedel-claude-code--control owner event)))
                                      (when (and (eq :json-false (plist-get decision :continue)) (not stopped))
                                        (setq stopped t)
                                        (pcase mode
                                          ('end-turn
                                           ;; Keep the requeued follow-up inspectable after the stop.
                                           (mevedel-session-set-pending-input-paused session t)
                                           (setf (mevedel-engine-info owner)
                                                 (plist-put (mevedel-engine-info owner) :mevedel-end-turn 'user)))
                                          ('cancel-before (mevedel-abort buffer))))
                                      decision))
                                  :observe
                                  (lambda (owner event)
                                    (let ((message (map-nested-elt event '(params message))))
                                      (when (equal "user" (alist-get 'type message))
                                        (push (copy-tree (map-nested-elt message '(message content))) prompts))
                                      (mevedel-claude-code--observe owner event)
                                      (when (and (eq mode 'cancel-after) (not received)
                                                 (seq-some (lambda (part) (equal "BOUNDARY-USER-8841" (alist-get 'text part)))
                                                           (map-nested-elt message '(message content))))
                                        (setq received t)
                                        (mevedel-abort buffer))))
                                  :meta
                                  `((hookCommand . ,hook) (suppressResponseOnStop . t) (preToolHook . t)
                                    (toolBatches . [[((name . "Read") (id . "before-steering") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "must-not-run") (args . ((file_path . ,file))))]])
                                    (promptResponse . ((stopReason . "end_turn")
                                                       (usage . ((inputTokens . 13) (cachedWriteTokens . 0) (outputTokens . 7)))))
                                    (continuationPrompts
                                     . [((toolBatches . [[((name . "Read") (id . "after-steering") (args . ((file_path . ,file))))]])
                                         (promptResponse . ((stopReason . "end_turn")
                                                            (usage . ((inputTokens . 17) (cachedWriteTokens . 0) (outputTokens . 11))))))])))))))
                (mevedel--insert-user-turn "Read the evidence")
                (mevedel--send-request "Read the evidence")
                (with-current-buffer view
                  (goto-char (point-max))
                  (insert draft))
                (let ((window (selected-window))
                      (start (window-start))
                      (offset (with-current-buffer view (- (point) (mevedel-view--input-start)))))
                  (mevedel-test--await 8 "Steering boundary did not settle"
                    (not (mevedel-turn-busy-p buffer)))
                  (with-current-buffer view
                    (mevedel-view--render-stream-update buffer)
                    (should (equal draft (mevedel-view--input-text)))
                    (should (= offset (- (point) (mevedel-view--input-start)))))
                  (should (eq window (selected-window)))
                  (should (= start (window-start window)))))
              (ert-info ((format "steering boundary=%S" mode))
                (should stopped)
                (should (= 1 terminals))
                (should (= 1 (mevedel-session-turn-count session)))
                (should (= (if (eq mode 'normal) 2 1) calls))
                (should (= (if (memq mode '(normal cancel-after)) 1 0)
                           (how-many "BOUNDARY-USER-8841" (point-min) (point-max))))
                (pcase mode
                  ('normal
                   (should (= 2 (length (seq-uniq prompts #'equal))))
                   (should (equal '(:input 30 :output 18 :cache 0)
                                  (plist-get (mevedel-engine-info request) :tokens-full)))
                   (should-not (mevedel-session-pending-steering session)))
                  ('end-turn
                   (should (= 1 (length (seq-uniq prompts #'equal))))
                   (should-not (mevedel-session-pending-steering session))
                   (should (equal "BOUNDARY-USER-8841" (plist-get (car (mevedel-session-pending-follow-ups session)) :input))))
                  ('cancel-before
                   (should (equal "BOUNDARY-USER-8841" (plist-get (car (mevedel-session-pending-steering session)) :input)))
                   (should-not (plist-get (car (mevedel-session-pending-steering session)) :delivering)))
                  ('cancel-after (should received) (should-not (mevedel-session-pending-steering session))))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-large-context (:quiet t)
                 (mevedel-engine-test--with-claude-session
                  (let* ((mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
                         (state "Initial child memory")
                         (file (file-name-concat root "evidence.txt"))
                         (calls 0) record invocation
                         (mevedel-post-tool-use-functions
                          (list (lambda (_event)
                                  (cl-incf calls)
                                  (setq state (concat "CHILD-UPDATE-" (make-string 11000 ?x) "-END")) nil))))
                    (setf (alist-get 'memory mevedel-system--prompt-components)
                          (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state)))
                    (write-region "Evidence" nil file nil 'silent)
                    (mevedel-tool-ensure "Read")
                    (unwind-protect
                        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                   (mevedel-engine-test--claude-launch
                                    (lambda (_system mcp _model _effort &optional id hook)
                                      (list :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                                    (toolBatches . [[((name . "Read") (id . "child-first") (args . ((file_path . ,file))))]])
                                                    (promptResponse . ((stopReason . "end_turn")
                                                                       (usage . ((inputTokens . 13) (cachedWriteTokens . 0) (outputTokens . 7)))))
                                                    (continuationPrompts . [((toolBatches . [[((name . "Read") (id . "child-second") (args . ((file_path . ,file))))]
                                                                                             [((name . "Read") (id . "child-must-not-run") (args . ((file_path . ,file))))]])
                                                                             (promptResponse . ((stopReason . "end_turn")
                                                                                                (usage . ((inputTokens . 17) (cachedWriteTokens . 0) (outputTokens . 11))))))])))))))
                          (mevedel-agent-control-spawn
                           session "reader" "Read with current memory, within two samples."
                           (lambda (value) (setq record (plist-get value :record)))
                           :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                                         :tools '(Read) :max-turns 2 :context-components '(memory)
                                                         :system-prompt "Child continuation fixture")
                           :on-invocation (lambda (value) (setq invocation value)))
                          (mevedel-test--await 8 "Child full-context continuation did not settle"
                            (mevedel-agent-invocation-runtime-settled-p invocation))
                          (should (= 2 calls))
                          (should (= 2 (mevedel-agent-invocation-turn-count invocation)))
                          (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                          (should (string-search "2-turn limit was reached" (mevedel-agent-record-settled-result record)))
                          (should (equal '(:input 30 :output 18 :cache 0) (plist-get (mevedel-engine-info invocation) :tokens-full)))
                          (should (= 0 (mevedel-session-turn-count session)))
                          (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                            (should (= 1 (mevedel-engine-test--count-evidence "CHILD-UPDATE-")))))
                      (mevedel-agent-control-teardown-session session)))))

(mevedel-deftest mevedel-agent-control-spawn/claude-restoration-limit (:quiet t)
                 (dolist (scenario '((2 . t) (3 . t) (2) (3)))
                   (mevedel-engine-test--with-claude-session
                    (let* ((limit (car scenario))
                           (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
                           (state "Initial memory")
                           (file (file-name-concat root "evidence.txt"))
                           (calls 0) record invocation
                           (mevedel-post-tool-use-functions (list (lambda (_) (cl-incf calls) nil))))
                      (setf (alist-get 'memory mevedel-system--prompt-components)
                            (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state)))
                      (write-region "Evidence" nil file nil 'silent)
                      (mevedel-tool-ensure "Read")
                      (unwind-protect
                          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                     (mevedel-engine-test--claude-launch
                                      (lambda (_system mcp _model _effort &optional id hook)
                                        (list :control (lambda (owner event)
                                                         (when (equal "SessionStart" (plist-get event :hook_event_name))
                                                           (setq state (concat "RESTORED-MEMORY-" (make-string 11000 ?x))))
                                                         (mevedel-claude-code--control owner event))
                                              :meta `((hookCommand . ,hook) (preToolHook . t)
                                                      (suppressResponseOnStop . t) (compactBeforeBatch . 1)
                                                      (toolBatches . ,(vector
                                                                       `[((name . "Read") (id . "initial") (args . ((file_path . ,file))))]
                                                                       (if (cdr scenario)
                                                                           `[((name . "Read") (id . "denied") (args . ((file_path . ,file))))]
                                                                         [])))
                                                      (continuationPrompts . [((toolBatches . [[((name . "Read") (id . "restored") (args . ((file_path . ,file))))]]))])))))))
                            (mevedel-agent-control-spawn
                             session "reader" "Read using current memory."
                             (lambda (value) (setq record (plist-get value :record)))
                             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                                           :tools '(Read) :max-turns limit :context-components '(memory)
                                                           :system-prompt "Restoration sample limit fixture")
                             :on-invocation (lambda (value) (setq invocation value)))
                            (mevedel-test--await 8 "Restored child did not settle"
                              (mevedel-agent-invocation-runtime-settled-p invocation))
                            (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                            (should (= limit (mevedel-agent-invocation-turn-count invocation)))
                            (should (= (1- limit) calls))
                            (should (string-search (format "%d-turn limit was reached" limit)
                                                   (mevedel-agent-record-settled-result record)))
                            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                              (should (= 1 (mevedel-engine-test--count-evidence "This is your final turn")))
                              (should (= (- limit 2) (mevedel-engine-test--count-evidence "RESTORED-MEMORY-")))))
                        (mevedel-agent-control-teardown-session session))))))

(provide 'test-mevedel-claude-code-continuation)
;;; test-mevedel-claude-code-continuation.el ends here
