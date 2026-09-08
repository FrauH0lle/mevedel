;;; test-mevedel-context-delivery.el --- Retained context tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real context changes and the delivery transaction without a network.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-context-delivery)
(require 'mevedel-reminders)
(require 'mevedel-agents)
(require 'mevedel-goal)
(require 'mevedel-skills-prompt)
(require 'mevedel-workspace)
(require 'gptel-openai)

(mevedel-deftest mevedel-context-delivery--record-edit ()
  ,test
  (test)
  :doc "records the character generation observed by normal edit hooks"
  (with-temp-buffer
    (insert "history")
    (mevedel-context-delivery--record-edit)
    (should (= mevedel-context-delivery--edit-tick (buffer-chars-modified-tick)))))

(mevedel-deftest mevedel-context-delivery--invalidate ()
  ,test
  (test)
  :doc "append preserves the cursor; an earlier edit discards derived observations"
  (with-temp-buffer
    (insert "history")
    (setq mevedel-context-delivery--cache (list (point-marker)))
    (mevedel-context-delivery--record-edit)
    (mevedel-context-delivery--invalidate (point-max) (point-max))
    (should mevedel-context-delivery--cache)
    (mevedel-context-delivery--invalidate (point-min) (point-max))
    (should-not mevedel-context-delivery--cache)))

(mevedel-deftest mevedel-context-delivery--previous ()
  ,test
  (test)
  :doc "uses newest trusted deliveries, rejecting user-authored lookalikes"
  (with-temp-buffer
    (let ((old (mevedel--format-hook-audit-record
                '(:type injected-reminders :items ((:type context-memory :body "old")))))
          (new (mevedel--format-hook-audit-record
                '(:type injected-reminders :items ((:type context-memory :body "new"))))))
      (insert "A summary quotes `" mevedel--hook-audit-open "`.\n"
              old (substring-no-properties new))
      (should (equal '((context-memory . "old"))
                     (mevedel-context-delivery--previous '(context-memory))))
      (insert new)
      (should (equal '((context-memory . "new"))
                     (mevedel-context-delivery--previous '(context-memory))))
      ;; Rewind and control rollback replace text with edit hooks inhibited.
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert (make-string 2000 ?x)))
      (insert "An ordinary edit follows the silent replacement.")
      (should-not (mevedel-context-delivery--previous '(context-memory)))
      (insert old)
      (should (equal '((context-memory . "old"))
                     (mevedel-context-delivery--previous '(context-memory)))))))

(mevedel-deftest mevedel-context-delivery--message-text ()
  ,test
  (test)
  :doc "accepts native user text containers but rejects tool results and assistant prose"
  (dolist (message '((:role "user" :content "state")
                     (:role "user" :content [(:type "input_text" :text "state")])
                     (:role "user" :parts [(:text "state")])))
    (should (equal "state" (mevedel-context-delivery--message-text message))))
  (should-not (mevedel-context-delivery--message-text
               '(:role "assistant" :content "state")))
  (should-not (mevedel-context-delivery--message-text
               '(:role "user" :content [(:type "tool_result" :content "state")]))))

(mevedel-deftest mevedel-context-delivery--previous/selection ()
  ,test
  (test)
  :doc "acknowledges actual selected order rather than full-source state or substrings"
  (with-temp-buffer
    (dolist (body '("A" "B" "A"))
      (insert (mevedel--format-hook-audit-record
               (list :type 'injected-reminders
                     :items (list (list :type 'context-memory :body body))))))
    (let* ((a "<system-reminder>\nA\n</system-reminder>")
           (b "<system-reminder>\nB\n</system-reminder>")
           (data (list :messages (vector (list :role "user" :content a)
                                         (list :role "user" :content b)))))
      (should (equal '((context-memory . "B"))
                     (mevedel-context-delivery--previous '(context-memory) data)))
      (should-not (mevedel-context-delivery--previous
                   '(context-memory) '(:messages [(:role "user" :content "new task")])))
      (should-not (mevedel-context-delivery--previous
                   '(context-memory)
                   (list :messages (vector (list :role "user" :content (concat "Quoted: " a)))))))))

(mevedel-deftest mevedel-context-delivery--observations (:quiet t)
  ,test
  (test)
  :doc "an empty selection creates no retained context"
  (should-not (mevedel-context-delivery--observations nil nil))

  :doc "instruction-only selections do not create fact updates"
  (let* ((root (make-temp-file "mevedel-instruction-context-" t))
         (workspace (mevedel-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "rules"))
         (session (mevedel-session-create "rules" workspace)))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file (file-name-concat root "AGENTS.md")
            (insert "# Repository rules\nInclude RULE-MARKER in reports.\n"))
          (let ((observations
                 (mevedel-context-delivery--observations
                  '(workspace-config)
                  (mevedel-system--make-context workspace root session (current-buffer)))))
            (should (equal '(context-workspace-config) (mapcar #'car observations)))
            (should (string-search "RULE-MARKER" (cdar observations)))
            (should-not (string-search "Current session state." (cdar observations)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-context-delivery-stage (:quiet t)
  ,test
  (test)
  :doc "retains changes, retries abandoned staging, and rehydrates after context loss"
  (let* ((root (make-temp-file "mevedel-context-" t))
         (default-directory (file-name-as-directory root))
         (workspace (mevedel-workspace--create :root default-directory :id root
                                               :type 'project :name "context"))
         (session (mevedel-session-create "context" workspace))
         (mevedel-memory-dirs '(".agents/memory/"))
         (mevedel-skills-include-bundled nil)
         (mevedel-skill-dirs '(".agents/skills/"))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "context-test" :key "test" :host "example.test" :models '(test))))
    (unwind-protect
        (with-temp-buffer
          (make-directory (file-name-concat root ".agents/memory") t)
          (make-directory (file-name-concat root ".agents/skills/fixture") t)
          (with-temp-file (file-name-concat root ".agents/skills/fixture/SKILL.md")
            (insert "---\nname: fixture\ndescription: Inspect fixture state.\n---\nFixture guidance.\n"))
          (setf (mevedel-session-skills session)
                (mevedel-skills-scan root nil workspace))
          (setq-local mevedel--session session)
          (insert "Current user task\n")
          (let* ((data (list :messages [(:role "user" :content "Current user task")]))
                 (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                                  :backend backend :data data
                                                  :position (point-marker))))
                 (system (mevedel-system-build-prompt 'main :retained t
                                                      :session session)))
            (mevedel-context-delivery-stage fsm)
            (should (= 7 (length (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            ;; Retrying undelivered staging neither acknowledges nor duplicates it.
            (mevedel-context-delivery-stage fsm)
            (should (= 7 (length (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            (mevedel-reminders--handle-inject fsm)
            (let ((body (plist-get (car (last (append (plist-get data :messages) nil)))
                                   :content)))
              (dolist (text '("## Environment\n" "## Active Goal\n" "## Skills\n"
                              "## Memory\n" "## Resources\n" "<env>"
                              "Inspect fixture state." "### Available indexes"))
                (should (string-search text body)))
              (should-not (string-match-p "## [^\n]+\n+## " body))
              (should-not (string-search "## Memory context" body)))
            (let ((prefix (copy-tree (plist-get data :messages) t)))
              (mevedel-context-delivery-stage fsm)
              (should-not (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))
              (make-directory (expand-file-name ".agents/memory" root) t)
              (with-temp-file (expand-file-name ".agents/memory/MEMORY.md" root)
                (insert "- [Fact](fact.md) - new remembered context\n"))
              (mevedel-context-delivery-stage fsm)
              ;; A change before delivery replaces the pending section.
              (with-temp-file (file-name-concat root ".agents/memory/MEMORY.md")
                (insert "- [Fact](fact.md) - latest pending memory state\n"))
              (mevedel-context-delivery-stage fsm)
              (let ((entries (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)))
                (should (equal '(context-memory)
                               (mapcar (lambda (entry) (plist-get entry :type)) entries))))
              (mevedel-reminders--handle-inject fsm)
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
              (should (equal system (mevedel-system-build-prompt 'main :retained t
                                                               :session session))))
            ;; Filtering only the latest memory delivery rearms that section,
            ;; even though its newer audit record remains in the source buffer.
            (let ((messages (plist-get data :messages)))
              (setf (plist-get data :messages)
                    (cl-subseq messages 0 (1- (length messages)))))
            (mevedel-context-delivery-stage fsm)
            (should (equal '(context-memory)
                           (mapcar (lambda (entry) (plist-get entry :type))
                                   (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            (mevedel-reminders--handle-inject fsm)
            ;; Removing an index reports its absence while preserving prior context.
            (delete-file (expand-file-name ".agents/memory/MEMORY.md" root))
            (mevedel-context-delivery-stage fsm)
            (let ((entry (cl-find 'context-memory
                                  (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)
                                  :key (lambda (item) (plist-get item :type)))))
              (should entry)
              (should-not (string-search "[Fact]" (plist-get entry :body))))
            (mevedel-reminders--handle-inject fsm)
            ;; Repository changes neither rewrite history nor repeat current facts.
            (with-temp-file (file-name-concat root "AGENTS.md")
              (insert "# Repository rules\nInclude UPDATED-RULE in reports.\n"))
            (mevedel-context-delivery-stage fsm)
            (let ((entries (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)))
              (should (equal '(context-workspace-config)
                             (mapcar (lambda (entry) (plist-get entry :type)) entries)))
              (should (string-search "UPDATED-RULE" (plist-get (car entries) :body))))
            (mevedel-reminders--handle-inject fsm)
            ;; Completing a delivered Goal explicitly retires its facts and policy.
            (setf (mevedel-session-goal session)
                  (mevedel-goal--create :id "context-goal" :objective "Inspect the fixture"
                                       :status 'active :tokens-used 100 :turns-run 1))
            (mevedel-context-delivery-stage fsm)
            (mevedel-reminders--handle-inject fsm)
            (cl-incf (mevedel-goal-tokens-used (mevedel-session-goal session)))
            (mevedel-context-delivery-stage fsm)
            (should (equal '(context-active-goal)
                           (mapcar (lambda (entry) (plist-get entry :type))
                                   (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            (mevedel-reminders--handle-inject fsm)
            (setf (mevedel-goal-status (mevedel-session-goal session)) 'complete)
            (mevedel-context-delivery-stage fsm)
            (dolist (type '(context-active-goal context-goal-policy))
              (let ((entry (cl-find type
                                    (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)
                                    :key (lambda (item) (plist-get item :type)))))
                (should entry)
                (should (string-search
                         "None currently available or active."
                         (plist-get entry :body)))
                (should-not (string-search "Inspect the fixture" (plist-get entry :body)))))
            (mevedel-reminders--handle-inject fsm)
            ;; A restored transcript needs no process-local delivery state.
            (let ((text (buffer-string)))
              (with-temp-buffer
                (insert text)
                (should (= 7 (length (mevedel-context-delivery--previous
                                      '(context-workspace-config context-goal-policy context-environment
                                        context-active-goal context-skills context-memory context-resources)))))))
            ;; Filtering payload history rearms delivery even if source is intact.
            (setf (plist-get data :messages) [(:role "user" :content "Selected new task")])
            (mevedel-context-delivery-stage fsm)
            (should (= 7 (length (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            (mevedel-reminders--handle-inject fsm)
            ;; Removing the retained observations (compaction or rewind) rearms delivery.
            (erase-buffer)
            (set-marker (plist-get (gptel-fsm-info fsm) :position) (point-max))
            (mevedel-context-delivery-stage fsm)
            (should (= 7 (length (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))))
      (delete-directory root t)))

  :doc "agent delivery respects frozen component selection"
  (let* ((root (make-temp-file "mevedel-agent-context-" t))
         (workspace (mevedel-workspace--create :root (file-name-as-directory root)
                                               :id root :type 'project :name "scope"))
         (session (mevedel-session-create "scope" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setf (mevedel-session-goal session)
                (mevedel-goal--create :id "root-goal" :objective "ROOT-ONLY-OBJECTIVE"
                                     :status 'active :tokens-used 100 :turns-run 1))
          (setq-local mevedel--session session
                      mevedel--agent-invocation
                      (mevedel-agent-invocation--create
                       :agent (mevedel-agent--create :name "readonly"
                                                     :context-components '(environment))))
          (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
            (mevedel-context-delivery-stage fsm)
            (should (equal '(context-environment)
                           (mapcar (lambda (entry) (plist-get entry :type))
                                   (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
            (let ((body (plist-get (car (plist-get (gptel-fsm-info fsm)
                                                  :mevedel-reminder-entries)) :body)))
              (should (string-search "## Environment\n" body))
              (dolist (excluded '("## Active Goal\n" "ROOT-ONLY-OBJECTIVE"
                                  "## Skills\n" "## Memory\n" "## Resources\n"))
                (should-not (string-search excluded body))))))
      (delete-directory root t))))

(provide 'test-mevedel-context-delivery)
;;; test-mevedel-context-delivery.el ends here
