;;; test-niwa-context-delivery.el --- Retained context tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real context changes and the delivery transaction without a network.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-context-delivery)
(require 'niwa-reminders)
(require 'niwa-agents)
(require 'niwa-goal)
(require 'niwa-skills-prompt)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'niwa-journal-index)
(require 'gptel-openai)

(niwa-deftest niwa-context-delivery--record-edit ()
  ,test
  (test)
  :doc "records the character generation observed by normal edit hooks"
  (with-temp-buffer
    (insert "history")
    (niwa-context-delivery--record-edit)
    (should (= niwa-context-delivery--edit-tick (buffer-chars-modified-tick)))))

(niwa-deftest niwa-context-delivery--invalidate ()
  ,test
  (test)
  :doc "append preserves the cursor; an earlier edit discards derived observations"
  (with-temp-buffer
    (insert "history")
    (setq niwa-context-delivery--cache (list (point-marker)))
    (niwa-context-delivery--record-edit)
    (niwa-context-delivery--invalidate (point-max) (point-max))
    (should niwa-context-delivery--cache)
    (niwa-context-delivery--invalidate (point-min) (point-max))
    (should-not niwa-context-delivery--cache)))

(niwa-deftest niwa-context-delivery--previous ()
  ,test
  (test)
  :doc "uses newest trusted deliveries, rejecting user-authored lookalikes"
  (with-temp-buffer
    (let ((old (niwa--format-hook-audit-record
                '(:type injected-reminders :items ((:type context-memory :body "old")))))
          (new (niwa--format-hook-audit-record
                '(:type injected-reminders :items ((:type context-memory :body "new"))))))
      (insert "A summary quotes `" niwa--hook-audit-open "`.\n"
              old (substring-no-properties new))
      (should (equal '((context-memory . "old"))
                     (niwa-context-delivery--previous '(context-memory))))
      (insert new)
      (should (equal '((context-memory . "new"))
                     (niwa-context-delivery--previous '(context-memory))))
      ;; Rewind and control rollback replace text with edit hooks inhibited.
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert (make-string 2000 ?x)))
      (insert "An ordinary edit follows the silent replacement.")
      (should-not (niwa-context-delivery--previous '(context-memory)))
      (insert old)
      (should (equal '((context-memory . "old"))
                     (niwa-context-delivery--previous '(context-memory)))))))

(niwa-deftest niwa-context-delivery-native ()
  ,test
  (test)
  :doc "derives latest receipts for one native conversation, never isolated or foreign turns"
  (with-temp-buffer
    (cl-flet ((record (native &rest items)
                (insert (niwa--format-hook-audit-record
                         (append (list :type 'injected-reminders :phase 'turn-start :items items)
                                 (when native (list :native native)))))))
      (should-not (niwa-context-delivery-native "X"))
      (record "X" '(:type context-memory :body "M1")
              '(:type context-journal :body "J1")
              '(:type context-journal :body "J2"))
      ;; An isolated turn and another conversation never reached X.
      (record nil '(:type context-memory :body "ISOLATED"))
      (record "Y" '(:type context-memory :body "OTHER"))
      (record "X" '(:type context-memory :body "M2"))
      (should (equal "M2" (alist-get 'context-memory (niwa-context-delivery-native "X"))))
      (should (equal "J2" (alist-get 'context-journal (niwa-context-delivery-native "X"))))
      (should (= 2 (length (niwa-context-delivery-native "X"))))
      (should-not (niwa-context-delivery-native nil))
      ;; Compaction/rewind derives only what the retained segment contains.
      (erase-buffer)
      (should-not (niwa-context-delivery-native "X"))
      (record "X" '(:type context-memory :body "M3"))
      (should (equal '((context-memory . "M3")) (niwa-context-delivery-native "X"))))))

(niwa-deftest niwa-context-delivery--message-text ()
  ,test
  (test)
  :doc "accepts native user text containers but rejects tool results and assistant prose"
  (dolist (message '((:role "user" :content "state")
                     (:role "user" :content [(:type "input_text" :text "state")])
                     (:role "user" :parts [(:text "state")])))
    (should (equal "state" (niwa-context-delivery--message-text message))))
  (should-not (niwa-context-delivery--message-text
               '(:role "assistant" :content "state")))
  (should-not (niwa-context-delivery--message-text
               '(:role "user" :content [(:type "tool_result" :content "state")]))))

(niwa-deftest niwa-context-delivery--previous/selection ()
  ,test
  (test)
  :doc "acknowledges actual selected order rather than full-source state or substrings"
  (with-temp-buffer
    (dolist (body '("A" "B" "A"))
      (insert (niwa--format-hook-audit-record
               (list :type 'injected-reminders
                     :items (list (list :type 'context-memory :body body))))))
    (let* ((a "<system-reminder>\nA\n</system-reminder>")
           (b "<system-reminder>\nB\n</system-reminder>")
           (data (list :messages (vector (list :role "user" :content a)
                                         (list :role "user" :content b)))))
      (should (equal '((context-memory . "B"))
                     (niwa-context-delivery--previous '(context-memory) data)))
      (should-not (niwa-context-delivery--previous
                   '(context-memory) '(:messages [(:role "user" :content "new task")])))
      (should-not (niwa-context-delivery--previous
                   '(context-memory)
                   (list :messages (vector (list :role "user" :content (concat "Quoted: " a)))))))))

(niwa-deftest niwa-context-delivery--observations (:quiet t)
  ,test
  (test)
  :doc "an empty selection creates no retained context"
  (should-not (niwa-context-delivery--observations nil nil))

  :doc "instruction-only selections do not create fact updates"
  (let* ((root (make-temp-file "niwa-instruction-context-" t))
         (workspace (niwa-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "rules"))
         (session (niwa-session-create "rules" workspace)))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file (file-name-concat root "AGENTS.md")
            (insert "# Repository rules\nInclude RULE-MARKER in reports.\n"))
          (let ((observations
                 (niwa-context-delivery--observations
                  '(workspace-config)
                  (niwa-system--make-context workspace root session (current-buffer)))))
            (should (equal '(context-workspace-config) (mapcar #'car observations)))
            (should (string-search "RULE-MARKER" (cdar observations)))
            (should-not (string-search "Current session state." (cdar observations)))))
      (delete-directory root t))))

(niwa-deftest niwa-context-delivery-stage (:quiet t)
  ,test
  (test)
  :doc "retains changes, retries abandoned staging, and rehydrates after context loss"
  (let* ((niwa-journal-max-age-days nil)
         (root (make-temp-file "niwa-context-" t))
         (default-directory (file-name-as-directory root))
         (workspace (niwa-workspace--create :root default-directory :id root
                                               :type 'project :name "context"))
         (session (niwa-session-create "context" workspace))
         (niwa-memory-dirs '(".agents/memory/"))
         (niwa-skills-include-bundled nil)
         (niwa-skill-dirs '(".agents/skills/"))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "context-test" :key "test" :host "example.test" :models '(test))))
    (unwind-protect
        (with-temp-buffer
          (make-directory (file-name-concat root ".agents/memory") t)
          (make-directory (file-name-concat root ".agents/skills/fixture") t)
          (with-temp-file (file-name-concat root ".agents/skills/fixture/SKILL.md")
            (insert "---\nname: fixture\ndescription: Inspect fixture state.\n---\nFixture guidance.\n"))
          (setf (niwa-session-skills session)
                (niwa-skills-scan root nil workspace))
          (setq-local niwa--session session)
          (insert "Current user task\n")
          (let* ((digest (niwa-journal-store-publish-digest
                          root
                          (list :capture-id (make-string 64 ?a) :session "context-session"
                                :session-name "Context" :workspace (make-string 64 ?b)
                                :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                                :turns '(1) :turn-ids (list (make-string 64 ?d))
                                :created "2026-09-07T12:00:00Z" :model "provider:model")
                          "## Done\n- none\n## Learned\n- Observed: journal lesson.\n## Surprised\n- none\n## Unfinished\n- none"))
                 (data (list :messages [(:role "user" :content "Current user task")]))
                 (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                                  :backend backend :data data
                                                  :position (point-marker))))
                 (system (niwa-system-build-prompt 'main :retained t
                                                      :session session)))
            (niwa-context-delivery-stage fsm)
            (should (= 9 (length (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
            ;; Retrying undelivered staging neither acknowledges nor duplicates it.
            (niwa-context-delivery-stage fsm)
            (should (= 9 (length (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
            (niwa-reminders--handle-inject fsm)
            (let ((body (plist-get (car (last (append (plist-get data :messages) nil)))
                                   :content)))
              (dolist (text '("## Environment\n" "## Active Goal\n" "## Tools\n" "## Skills\n"
                              "## Memory\n" "## Journal\n" "## Resources\n" "<env>"
                              "Unreviewed digests: 1" "journal lesson"
                              "Inspect fixture state." "### Available indexes"))
                (should (string-search text body)))
              (should-not (string-match-p "## [^\n]+\n+## " body))
              (should-not (string-search "## Memory context" body)))
            (let ((prefix (copy-tree (plist-get data :messages) t)))
              (niwa-context-delivery-stage fsm)
              (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
              (make-directory (expand-file-name ".agents/memory" root) t)
              (with-temp-file (expand-file-name ".agents/memory/MEMORY.md" root)
                (insert "- [Fact](fact.md) - new remembered context\n"))
              (niwa-context-delivery-stage fsm)
              ;; A change before delivery replaces the pending section.
              (with-temp-file (file-name-concat root ".agents/memory/MEMORY.md")
                (insert "- [Fact](fact.md) - latest pending memory state\n"))
              (niwa-context-delivery-stage fsm)
              (let ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
                (should (equal '(context-memory)
                               (mapcar (lambda (entry) (plist-get entry :type)) entries))))
              (niwa-reminders--handle-inject fsm)
              (let ((body (plist-get (car (last (append (plist-get data :messages) nil)))
                                     :content)))
                (dolist (text '("## Memory\n" "This updates only Memory;"
                                "all other previously supplied state remains applicable."
                                "[Fact]" "latest pending memory state"))
                  (should (string-search text body)))
                (dolist (text '("## Environment\n" "## Active Goal\n"
                                "## Skills\n" "## Resources\n"
                                "new remembered context" "## Memory context"))
                  (should-not (string-search text body)))
                (should (string-search "### Available indexes" body)))
              (should (equal prefix (cl-subseq (plist-get data :messages) 0 (length prefix))))
              (should (equal system (niwa-system-build-prompt 'main :retained t
                                                               :session session))))
            ;; Filtering only the latest memory delivery rearms that section,
            ;; even though its newer audit record remains in the source buffer.
            (let ((messages (plist-get data :messages)))
              (setf (plist-get data :messages)
                    (cl-subseq messages 0 (1- (length messages)))))
            (niwa-context-delivery-stage fsm)
            (should (equal '(context-memory)
                           (mapcar (lambda (entry) (plist-get entry :type))
                                   (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
            (niwa-reminders--handle-inject fsm)
            ;; Removing an index reports its absence while preserving prior context.
            (delete-file (expand-file-name ".agents/memory/MEMORY.md" root))
            (niwa-context-delivery-stage fsm)
            (let ((entry (cl-find 'context-memory
                                  (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)
                                  :key (lambda (item) (plist-get item :type)))))
              (should entry)
              (should-not (string-search "[Fact]" (plist-get entry :body))))
            (niwa-reminders--handle-inject fsm)
            ;; Repository changes neither rewrite history nor repeat current facts.
            (with-temp-file (file-name-concat root "AGENTS.md")
              (insert "# Repository rules\nInclude UPDATED-RULE in reports.\n"))
            (niwa-context-delivery-stage fsm)
            (let ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
              (should (equal '(context-workspace-config)
                             (mapcar (lambda (entry) (plist-get entry :type)) entries)))
              (should (string-search "UPDATED-RULE" (plist-get (car entries) :body))))
            (niwa-reminders--handle-inject fsm)
            ;; Coverage changes update only journal, without rewriting the system.
            (niwa-journal-store-publish-review
             root (list :pass-id (make-string 64 ?e) :workspace (make-string 64 ?b)
                        :created "2026-09-08T12:00:00Z" :model "provider:model"
                        :focus "" :digests (list (plist-get digest :id))
                        :proposals nil :references nil))
            (setf (niwa-workspace-journal-observation workspace) nil)
            (niwa-context-delivery-stage fsm)
            (let ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
              (should (equal '(context-journal)
                             (mapcar (lambda (entry) (plist-get entry :type)) entries)))
              (should (string-search "Unreviewed digests: 0" (plist-get (car entries) :body)))
              (should (string-search "journal lesson" (plist-get (car entries) :body))))
            (niwa-reminders--handle-inject fsm)
            (niwa-context-delivery-stage fsm)
            (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
            (should (equal system (niwa-system-build-prompt 'main :retained t
                                                             :session session)))
            ;; Loss of the map explicitly clears the previous journal observation.
            (delete-file (file-name-concat (niwa-journal-store-directory root)
                                          (plist-get digest :file)))
            (setf (niwa-workspace-journal-observation workspace) nil)
            (niwa-context-delivery-stage fsm)
            (let ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
              (should (equal '(context-journal)
                             (mapcar (lambda (entry) (plist-get entry :type)) entries)))
              (should (string-search "None currently available or active."
                                     (plist-get (car entries) :body))))
            (niwa-reminders--handle-inject fsm)
            ;; Completing a delivered Goal explicitly retires its facts and policy.
            (setf (niwa-session-goal session)
                  (niwa-goal--create :id "context-goal" :objective "Inspect the fixture"
                                       :status 'active :tokens-used 100 :turns-run 1))
            (niwa-context-delivery-stage fsm)
            (niwa-reminders--handle-inject fsm)
            ;; Usage and the turn count move with every charge or turn and
            ;; re-send nothing.
            (cl-incf (niwa-goal-tokens-used (niwa-session-goal session)))
            (niwa-context-delivery-stage fsm)
            (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
            (cl-incf (niwa-goal-turns-run (niwa-session-goal session)))
            (niwa-context-delivery-stage fsm)
            (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
            (setf (niwa-goal-status (niwa-session-goal session)) 'complete)
            (niwa-context-delivery-stage fsm)
            (dolist (type '(context-active-goal context-goal-policy))
              (let ((entry (cl-find type
                                    (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)
                                    :key (lambda (item) (plist-get item :type)))))
                (should entry)
                (should (string-search
                         "None currently available or active."
                         (plist-get entry :body)))
                (should-not (string-search "Inspect the fixture" (plist-get entry :body)))))
            (niwa-reminders--handle-inject fsm)
            ;; A restored transcript needs no process-local delivery state.
            (let ((text (buffer-string)))
              (with-temp-buffer
                (insert text)
                (should (= 9 (length (niwa-context-delivery--previous
                                      '(context-workspace-config context-goal-policy context-environment
                                        context-active-goal context-tools context-skills context-memory
                                        context-journal context-resources)))))))
            ;; Filtering payload history rearms delivery even if source is intact.
            (setf (plist-get data :messages) [(:role "user" :content "Selected new task")])
            (niwa-context-delivery-stage fsm)
            (should (= 9 (length (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
            (niwa-reminders--handle-inject fsm)
            ;; Removing the retained observations (compaction or rewind) rearms delivery.
            (erase-buffer)
            (set-marker (plist-get (gptel-fsm-info fsm) :position) (point-max))
            (niwa-context-delivery-stage fsm)
            (should (= 9 (length (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))))
      (delete-directory root t)))

  :doc "expiry replaces a delivered journal observation even with a warm journal cache"
  (let* ((root (make-temp-file "niwa-context-expiry-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :working-directory root))
         (niwa-journal-max-age-days 14)
         (niwa-system-retained-components '(journal))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "context-expiry-test" :key "test"
                                     :host "example.test" :models '(test)))
         (deadline (+ (float-time (date-to-time "2026-09-01T12:00:00Z")) (* 14 86400)))
         (now (1- deadline))
         (real-time (symbol-function 'float-time)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (insert "Current task\n")
          (let* ((entry (niwa-journal-store-publish-digest
                         root
                         (list :capture-id (make-string 64 ?a) :session "expiry-session"
                               :session-name "Expiry" :workspace (niwa-workspace-identity-ensure root)
                               :trigger 'session-end :segment 1 :source-revision (make-string 64 ?b)
                               :turns '(1) :turn-ids (list (make-string 64 ?c))
                               :created "2026-09-01T12:00:00Z" :model "test:model")
                         "## Done\n- none\n## Learned\n- Observed: RETAINED-JOURNAL-CANARY.\n## Surprised\n- none\n## Unfinished\n- none"))
                 (data (list :messages [(:role "user" :content "Current task")]))
                 (fsm (gptel-make-fsm :info (list :buffer (current-buffer) :backend backend
                                                  :data data :position (point-marker)))))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional time) (if time (funcall real-time time) now))))
              (niwa-context-delivery-stage fsm)
              (should (string-search "RETAINED-JOURNAL-CANARY"
                                     (plist-get (car (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)) :body)))
              (niwa-reminders--handle-inject fsm)
              (niwa-context-delivery-stage fsm)
              (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
              (let ((observation (niwa-workspace-journal-observation workspace)))
                (setq now deadline)
                (niwa-context-delivery-stage fsm)
                (should (eq observation (niwa-workspace-journal-observation workspace)))
                (let ((updates (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
                  (should (equal '(context-journal) (mapcar (lambda (row) (plist-get row :type)) updates)))
                  (should (string-search "replaces earlier Journal state" (plist-get (car updates) :body)))
                  (should-not (string-search "RETAINED-JOURNAL-CANARY" (plist-get (car updates) :body)))))
              (niwa-reminders--handle-inject fsm)
              (niwa-context-delivery-stage fsm)
              (should-not (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
              (should (equal entry (niwa-journal-store-read root (plist-get entry :file)))))))
      (delete-directory root t)))

  :doc "agent delivery respects frozen component selection"
  (let* ((root (make-temp-file "niwa-agent-context-" t))
         (workspace (niwa-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "scope"))
         (session (niwa-session-create "scope" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setf (niwa-session-goal session)
                (niwa-goal--create :id "root-goal" :objective "ROOT-ONLY-OBJECTIVE"
                                     :status 'active :tokens-used 100 :turns-run 1))
          (setq-local niwa--session session
                      niwa--agent-invocation
                      (niwa-agent-invocation--create
                       :agent (niwa-agent--create :name "readonly"
                                                     :context-components '(environment))))
          (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
            (niwa-context-delivery-stage fsm)
            (should (equal '(context-environment)
                           (mapcar (lambda (entry) (plist-get entry :type))
                                   (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
            (let ((body (plist-get (car (plist-get (gptel-fsm-info fsm)
                                                  :niwa-reminder-entries)) :body)))
              (should (string-search "## Environment\n" body))
              (dolist (excluded '("## Active Goal\n" "ROOT-ONLY-OBJECTIVE"
                                  "## Skills\n" "## Memory\n" "## Resources\n"))
                (should-not (string-search excluded body))))))
      (delete-directory root t)))

  :doc "a default agent inherits root context, minus Goal and journal, with its own tools"
  (let* ((root (make-temp-file "niwa-agent-tools-" t))
         (workspace (niwa-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "tools"))
         (session (niwa-session-create "tools" workspace)))
    (unwind-protect
        (with-temp-buffer
          (niwa-tools-register)
          (write-region "PROJECT-RULE-7731" nil (file-name-concat root "AGENTS.md") nil 'silent)
          (setf (niwa-session-goal session)
                (niwa-goal--create :id "root-goal" :objective "ROOT-ONLY-OBJECTIVE"
                                     :status 'active :tokens-used 100 :turns-run 1))
          (setf (niwa-session-tool-catalog session)
                '((("niwa" "TaskWrite") . "Track a plan")))
          (setq-local niwa--session session)
          ;; Freezing in the root's buffer resolves the delegator, as dispatch does.
          (let ((agent (niwa-agent-freeze (niwa-agent-default))))
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create
                         :agent agent
                         :tool-catalog '((("niwa" "Imenu") . "Outline")))))
          (setq-local gptel-tools
                      (list (niwa-tool-gptel-tool (niwa-tool-get "ToolCall"))))
          (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                                :tools gptel-tools))))
            (niwa-context-delivery-stage fsm)
            (let* ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))
                   (types (mapcar (lambda (entry) (plist-get entry :type)) entries))
                   (body (mapconcat (lambda (entry) (plist-get entry :body)) entries "\n"))
                   (tools (plist-get (cl-find 'context-tools entries
                                              :key (lambda (entry) (plist-get entry :type)))
                                     :body)))
              (dolist (type '(context-workspace-config context-environment
                              context-tools context-resources))
                (should (memq type types)))
              (dolist (type '(context-active-goal context-goal-policy context-journal))
                (should-not (memq type types)))
              (should (string-search "PROJECT-RULE-7731" body))
              (should (string-search "<env>" body))
              (should-not (string-search "ROOT-ONLY-OBJECTIVE" body))
              (should (string-search "- (Imenu :file_path string)" tools))
              (should-not (string-search "TaskWrite" tools)))))
      (delete-directory root t)))

  :doc "unacknowledged root path instructions reach the root request"
  (let* ((root (make-temp-file "niwa-root-instructions-" t))
         (workspace (niwa-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "scope"))
         (session (niwa-session-create "scope" workspace))
         (file (file-name-concat root "lib" "AGENTS.md")))
    (unwind-protect
        (with-temp-buffer
          (make-directory (file-name-directory file))
          (write-region "ROOT-PATH-6631" nil file nil 'silent)
          (setf (niwa-session-workspace-instruction-hashes session)
                (list (cons (list "/root" file) nil)))
          (setq-local niwa--session session)
          (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
            (niwa-context-delivery-stage fsm)
            (should (seq-find (lambda (entry)
                                (equal (cons 'workspace-instructions file)
                                       (plist-get entry :type)))
                              (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))))
      (delete-directory root t))))

(provide 'test-niwa-context-delivery)
;;; test-niwa-context-delivery.el ends here
