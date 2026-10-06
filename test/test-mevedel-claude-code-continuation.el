;;; test-mevedel-claude-code-continuation.el --- Full native context continuation -*- lexical-binding: t -*-

;;; Commentary:
;; A native hook's size limit must not require a new user turn for context.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-continuation-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel--send-request/claude-large-context (:quiet t)
                 (dolist (mode '(normal missing pause cancel restore))
                   (mevedel-engine-test--with-session
                    (let* ((gptel--known-backends nil)
                           (mevedel-claude-code-directory (file-name-concat root "claude"))
                           (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
                           (mevedel-system-retained-components '(memory))
                           (state "Initial memory")
                           (changed (concat "FULL-UPDATE-" (make-string 5100 ?😀) "-END"))
                           (file (file-name-concat root "evidence.txt"))
                           (mevedel-goal-token-budget 48)
                           (goal (mevedel-goal-create "Finish this bounded native turn" session))
                           (calls 0) prompts decisions readings
                           (mevedel-post-tool-use-functions
                            (list (lambda (_event)
                                    (cl-incf calls)
                                    (unless (eq mode 'restore) (setq state changed))
                                    nil))))
                      (setf (alist-get 'memory mevedel-system--prompt-components)
                            (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state)))
                      (write-region "Evidence" nil file nil 'silent)
                      (mevedel-claude-code-register)
                      (mevedel-model-set-session-provider
                       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
                      (setq-local gptel-system-prompt "Continuation fixture"
                                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
                      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                 (lambda (_system mcp _model _effort &optional id hook)
                                   (list :command (executable-find "python3")
                                         :args (list mevedel-claude-code-continuation-test--peer)
                                         :cwd root :mcp mcp :session-id id
                                         :tool-id-field :claudecode/toolUseId
                                         :observe (lambda (owner event)
                                                    (when (and (eq mode 'restore)
                                                               (equal "agent_message_chunk" (map-nested-elt event '(params update sessionUpdate))))
                                                      (setq state changed))
                                                    (let ((message (alist-get 'message (alist-get 'params event))))
                                                      (when (equal "user" (alist-get 'type message))
                                                        (cl-pushnew (map-nested-elt message '(message content)) prompts :test #'equal)))
                                                    (mevedel-claude-code--observe owner event))
                                         :check-context #'mevedel-claude-code-context-check
                                         :normalize-outcome #'mevedel-claude-code--outcome
                                         :complete-prompt #'mevedel-claude-code--complete-prompt
                                         :control (lambda (owner event)
                                                    (unless (equal "PreToolUse" (plist-get event :hook_event_name))
                                                      (push (copy-sequence (plist-get (mevedel-engine-info owner) :tokens-full)) readings))
                                                    (let ((decision (mevedel-claude-code--control owner event)))
                                                      (when (eq :json-false (plist-get decision :continue))
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
                                                                                             (usage . ((inputTokens . 17) (cachedWriteTokens . 0) (outputTokens . 11))))))]))))))
                        (mevedel--insert-user-turn "Read with the latest memory")
                        (mevedel--send-request "Read with the latest memory")
                        (with-timeout (8 (ert-fail "Full-context continuation did not settle"))
                          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
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
                      (should (equal (append (when (memq mode '(normal restore)) '((:input 30 :output 18 :cache 0)))
                                             '((:input 13 :output 7 :cache 0))
                                             (when (eq mode 'restore) '((:input 13 :output 7 :cache 0)))) readings))
                      (should (= (if (memq mode '(normal restore)) 1 0) (how-many "FULL-UPDATE-" (point-min) (point-max))))
                      (should (= 1 (mevedel-session-turn-count session)))
                      (should (= 1 (mevedel-goal-turns-run goal)))
                      (if (eq mode 'missing)
                          ;; MCP refusal may race the later SDK usage notification on stdio.
                          (should (memq (mevedel-goal-tokens-used goal) '(20 48)))
                        (should (= (if (memq mode '(pause cancel)) 20 48) (mevedel-goal-tokens-used goal))))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-large-context (:quiet t)
                 (mevedel-engine-test--with-session
                  (let* ((gptel--known-backends nil)
                         (mevedel-claude-code-directory (file-name-concat root "claude"))
                         (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
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
                    (mevedel-claude-code-register)
                    (mevedel-model-set-session-provider
                     session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
                    (unwind-protect
                        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                   (lambda (_system mcp _model _effort &optional id hook)
                                     (list :command (executable-find "python3")
                                           :args (list mevedel-claude-code-continuation-test--peer)
                                           :cwd root :mcp mcp :session-id id
                                           :tool-id-field :claudecode/toolUseId
                                           :observe #'mevedel-claude-code--observe
                                           :check-context #'mevedel-claude-code-context-check
                                           :normalize-outcome #'mevedel-claude-code--outcome
                                           :complete-prompt #'mevedel-claude-code--complete-prompt
                                           :control #'mevedel-claude-code--control
                                           :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                                   (toolBatches . [[((name . "Read") (id . "child-first") (args . ((file_path . ,file))))]])
                                                   (promptResponse . ((stopReason . "end_turn")
                                                                      (usage . ((inputTokens . 13) (cachedWriteTokens . 0) (outputTokens . 7)))))
                                                   (continuationPrompts . [((toolBatches . [[((name . "Read") (id . "child-second") (args . ((file_path . ,file))))]
                                                                                            [((name . "Read") (id . "child-must-not-run") (args . ((file_path . ,file))))]])
                                                                            (promptResponse . ((stopReason . "end_turn")
                                                                                               (usage . ((inputTokens . 17) (cachedWriteTokens . 0) (outputTokens . 11))))))]))))))
                          (mevedel-agent-control-spawn
                           session "reader" "Read with current memory, within two samples."
                           (lambda (value) (setq record (plist-get value :record)))
                           :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                                         :tools '(Read) :max-turns 2 :context-components '(memory)
                                                         :system-prompt "Child continuation fixture")
                           :on-invocation (lambda (value) (setq invocation value)))
                          (with-timeout (8 (ert-fail "Child full-context continuation did not settle"))
                            (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                              (accept-process-output nil 0.01)))
                          (should (= 2 calls))
                          (should (= 2 (mevedel-agent-invocation-turn-count invocation)))
                          (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                          (should (string-search "2-turn limit was reached" (mevedel-agent-record-settled-result record)))
                          (should (equal '(:input 30 :output 18 :cache 0) (plist-get (mevedel-engine-info invocation) :tokens-full)))
                          (should (= 0 (mevedel-session-turn-count session)))
                          (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                            (should (= 1 (how-many "CHILD-UPDATE-" (point-min) (point-max))))))
                      (mevedel-agent-control-teardown-session session)))))

(mevedel-deftest mevedel-agent-control-spawn/claude-restoration-limit (:quiet t)
                 (dolist (scenario '((2 . t) (3 . t) (2) (3)))
                   (mevedel-engine-test--with-session
                    (let* ((limit (car scenario))
                           (gptel--known-backends nil)
                           (mevedel-claude-code-directory (file-name-concat root "claude"))
                           (mevedel-system--prompt-components (copy-tree mevedel-system--prompt-components))
                           (state "Initial memory")
                           (file (file-name-concat root "evidence.txt"))
                           (calls 0) record invocation
                           (mevedel-post-tool-use-functions (list (lambda (_) (cl-incf calls) nil))))
                      (setf (alist-get 'memory mevedel-system--prompt-components)
                            (mevedel-system-prompt-component--create :name 'memory :producer (lambda (_) state)))
                      (write-region "Evidence" nil file nil 'silent)
                      (mevedel-tool-ensure "Read")
                      (mevedel-claude-code-register)
                      (mevedel-model-set-session-provider
                       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
                      (unwind-protect
                          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                     (lambda (_system mcp _model _effort &optional id hook)
                                       (list :command (executable-find "python3")
                                             :args (list mevedel-claude-code-continuation-test--peer)
                                             :cwd root :mcp mcp :session-id id
                                             :tool-id-field :claudecode/toolUseId
                                             :observe #'mevedel-claude-code--observe
                                             :check-context #'mevedel-claude-code-context-check
                                             :complete-prompt #'mevedel-claude-code--complete-prompt
                                             :control (lambda (owner event)
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
                                                     (continuationPrompts . [((toolBatches . [[((name . "Read") (id . "restored") (args . ((file_path . ,file))))]]))]))))))
                            (mevedel-agent-control-spawn
                             session "reader" "Read using current memory."
                             (lambda (value) (setq record (plist-get value :record)))
                             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                                           :tools '(Read) :max-turns limit :context-components '(memory)
                                                           :system-prompt "Restoration sample limit fixture")
                             :on-invocation (lambda (value) (setq invocation value)))
                            (with-timeout (8 (ert-fail "Restored child did not settle"))
                              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                                (accept-process-output nil 0.01)))
                            (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                            (should (= limit (mevedel-agent-invocation-turn-count invocation)))
                            (should (= (1- limit) calls))
                            (should (string-search (format "%d-turn limit was reached" limit)
                                                   (mevedel-agent-record-settled-result record)))
                            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                              (should (= 1 (mevedel-engine-test--count-evidence "This is your final turn")))
                              (should (= (- limit 2) (how-many "RESTORED-MEMORY-" (point-min) (point-max))))))
                        (mevedel-agent-control-teardown-session session))))))

(provide 'test-mevedel-claude-code-continuation)
;;; test-mevedel-claude-code-continuation.el ends here
