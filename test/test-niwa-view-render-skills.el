;;; test-niwa-view-render-skills.el --- Live attached skills -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise skill disclosures through accepted handoffs and composer sends.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-plan-handoff)
(require 'niwa-pipeline)
(require 'niwa-reminders)
(require 'niwa-resource)
(require 'niwa-skills-invoke)
(require 'niwa-skills-plan)
(require 'niwa-tool-registry)
(require 'niwa-tool-render-data)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-disclosure)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(niwa-deftest niwa-plan-handoff-start/live-skills (:quiet t)
  ,test
  (test)
  :doc "an accepted plan immediately discloses prepared skills and sends them to ACP"
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let* ((view (generate-new-buffer " *plan-skill-view*"))
           (niwa-skills-include-bundled nil)
           (niwa-skills-check-for-modifications nil)
           (skill-root (file-name-concat root "skills"))
           (niwa-skill-dirs (list skill-root))
           (source (file-name-concat skill-root "alpha" "SKILL.md"))
           (plan "# Accepted disclosure plan\nKeep the attachment visible.\n")
           (draft "> unfinished\ncomposer draft")
           launched owner launch-policy)
      (unwind-protect
          (progn
            (dolist (entry '(("base" "BASE ATTACHMENT BODY")
                             ("alpha" "!$base\nALPHA ATTACHMENT BODY")))
              (let ((dir (file-name-concat skill-root (car entry))))
                (make-directory dir t)
                (write-region
                 (format "---\nname: %s\ndescription: Fixture skill\ncontext: inline\nmodel: \"Claude Code:opus\"\neffort: xhigh\n---\n%s\n"
                         (car entry) (cadr entry))
                 nil (file-name-concat dir "SKILL.md") nil 'silent)))
            (niwa-model-set-session-provider
             session (niwa-model-resolve-provider "Claude Code:haiku") buffer)
            (niwa-permission-mode-set-raw 'ask)
            ;; This lightweight session omits ordinary installation defaults.
            ;; Request-scoped policy does not run a session-mode transition to
            ;; install the separate full-auto lifecycle reminder.
            (niwa-session-ensure-reminder
             session (niwa-reminders-make-mode-constraints))
            (setq-local gptel-system-prompt "Plan skill fixture" gptel-tools nil)
            (niwa-skills-install session buffer)
            (niwa-chat-install-request-hooks)
            (niwa-view--setup view buffer)
            (with-current-buffer view
              (niwa-view-composer-insert draft))
            (let* ((accepted (niwa-plan-archive-accepted
                              (niwa-plan-write-current plan session buffer) session))
                   (selection
                    (list :location 'here :context 'current :execution 'direct
                          :mode 'full-auto :model-provider "Claude Code:sonnet"
                          :reasoning-effort 'high
                          :skills
                          (list (list :name "alpha" :source-file source))
                          :instructions "Also use $alpha.")))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _)
                           (ert-fail "Accepted native handoff fell back to an API request")))
                        ((symbol-function 'niwa-claude-code-launch)
                         (niwa-engine-test--claude-launch
                          (lambda (_system _mcp model effort &optional _id _hook)
                            (setq launched t owner niwa--current-request
                                  launch-policy (list model effort))
                            (list :meta '((echoAllText . t)))))))
                (niwa-plan-handoff-start session buffer selection accepted)
                (should-not
                 (plist-get (plist-get (niwa-session-plan-metadata session)
                                       :implementation-retry) :failure))
                (niwa-test--await 5
                    (format "Plan preparation did not launch: %s" niwa-test--captured)
                  launched)
                ;; Selected attachments are instructions: their opus/xhigh
                ;; policies must not replace the accepted request settings.
                (should (equal '("sonnet" high) launch-policy))
                (should
                 (equal '(:model-provider "Claude Code:sonnet"
                          :reasoning-effort high :permission-mode full-auto)
                        (niwa-request-implementation-policy owner)))
                (should (eq 'full-auto (niwa-request-permission-mode-override owner)))
                (should (eq 'full-auto
                            (niwa-permission-mode-effective session buffer nil owner)))
                (should (equal "Claude Code:haiku" (niwa-session-model-provider session)))
                (should-not (niwa-session-reasoning-effort session))
                (should (eq 'ask (niwa-session-permission-mode session)))
                (should (equal "haiku" (gptel--model-name gptel-model)))
                (should-not gptel-reasoning-effort)
                (should (eq 'ask niwa-permission-mode))
                ;; Before polling the fixture, the request's user turn must
                ;; already expose the attachments in the live view.
                (with-current-buffer view
                  (should (string-search "Implement accepted plan" (buffer-string)))
                  (should (string-search "2 attached skills (base, alpha)" (buffer-string)))
                  (should-not (string-search "ALPHA ATTACHMENT BODY" (buffer-string)))
                  (should (equal draft (niwa-view-composer-draft-text nil t)))
                  (goto-char (point-min))
                  (search-forward "Prompt")
                  (niwa-view-toggle-section)
                  (should (string-search "Accepted disclosure plan" (buffer-string)))
                  (should-not (string-search "ALPHA ATTACHMENT BODY" (buffer-string)))
                  (goto-char (point-min))
                  (search-forward "attached skills")
                  (niwa-view-toggle-section)
                  (should (string-search "ALPHA ATTACHMENT BODY" (buffer-string))))
                (niwa-test--await 5 "Plan skill fixture did not settle"
                  (and launched (not (niwa-turn-busy-p buffer))))
                (let ((received
                       (mapconcat
                        (lambda (segment)
                          (if (eq (car segment) 'response)
                              (buffer-substring-no-properties (cadr segment) (caddr segment))
                            ""))
                        (niwa-transcript-segments (point-min) (point-max)) "")))
                  (should (string-search plan received))
                  (should (string-search "ALPHA ATTACHMENT BODY" received))
                  (should (string-search "BASE ATTACHMENT BODY" received))
                  (should (string-search "Permission mode: `full-auto'" received))
                  (should-not (string-search "niwa-render-data" received)))
                (should (eq 'success
                            (plist-get (niwa-engine-info owner) :niwa-acp-outcome)))
                (should-not niwa--pending-implementation-policy)
                (should-not niwa--current-request)
                (should-not (niwa-session-goal session))
                (should (equal "Claude Code:haiku" (niwa-session-model-provider session)))
                (should-not (niwa-session-reasoning-effort session))
                (should (eq 'ask (niwa-session-permission-mode session)))
                (should (equal "haiku" (gptel--model-name gptel-model)))
                (should-not gptel-reasoning-effort)
                (should (eq 'ask niwa-permission-mode))
                (with-current-buffer view
                  (should (= 1 (how-many "2 attached skills (base, alpha)"
                                         (point-min) (niwa-view-transcript-tail))))
                  ;; The prompt record lists the native system prompt's changed
                  ;; observation sections beside the trailing mode constraint.
                  (should (string-search
                           (concat "10 system reminders (context-workspace-config, "
                                   "context-goal-policy, context-environment, "
                                   "context-active-goal, context-tools, "
                                   "context-skills, context-memory, "
                                   "context-journal, context-resources, "
                                   "mode-constraints)")
                           (buffer-string)))
                  (should (string-search "ALPHA ATTACHMENT BODY" (buffer-string)))
                  (should (string-search "BASE ATTACHMENT BODY" (buffer-string)))
                  (niwa-view--full-rerender)
                  (should (string-search "You\nImplement accepted plan\n" (buffer-string)))
                  (should (= 1 (how-many "2 attached skills (base, alpha)"
                                         (point-min) (niwa-view-transcript-tail))))
                  (should (string-search "ALPHA ATTACHMENT BODY" (buffer-string)))
                  (goto-char (point-min))
                  (search-forward "attached skills")
                  (niwa-view-toggle-section)
                  ;; Prompt remains expanded after collapsing the skill row.
                  (goto-char (point-min))
                  (search-forward "Prompt")
                  (should-not (get-text-property (1- (point)) 'niwa-view-collapsed))
                  (should (equal draft (niwa-view-composer-draft-text nil t)))))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-view-send/live-skills (:quiet t)
  ,test
  (test)
  :doc "ordinary prepared submissions disclose skills before provider startup"
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let* ((view (generate-new-buffer " *composer-skill-view*"))
           (niwa-skills-include-bundled nil)
           (niwa-skills-check-for-modifications nil)
           (skill-root (file-name-concat root "skills"))
           (niwa-skill-dirs (list skill-root))
           launched)
      (unwind-protect
          (progn
            (make-directory (file-name-concat skill-root "alpha") t)
            (write-region
             "---\nname: alpha\ndescription: Fixture skill\ncontext: inline\n---\nCOMPOSER SKILL BODY\n"
             nil (file-name-concat skill-root "alpha" "SKILL.md") nil 'silent)
            (setq-local gptel-system-prompt "Composer fixture" gptel-tools nil)
            (niwa-skills-install session buffer)
            (niwa-chat-install-request-hooks)
            (niwa-view--setup view buffer)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (&rest _args)
                          ;; The provider has not even started its process yet.
                          (with-current-buffer view
                            (should (string-search "1 attached skill (alpha)" (buffer-string)))
                            (should-not (string-search "COMPOSER SKILL BODY" (buffer-string)))
                            (goto-char (point-min))
                            (search-forward "attached skill")
                            (niwa-view-toggle-section)
                            (should (string-search "COMPOSER SKILL BODY" (buffer-string))))
                          (setq launched t)
                          (list :meta '((responseText . "Completed")))))))
              (with-current-buffer view
                (niwa-view-composer-insert "Use $alpha")
                (niwa-view-send))
              (niwa-test--await 5 "Composer skill fixture did not settle"
                (and launched (not (niwa-turn-busy-p buffer))))
              (with-current-buffer view
                (should (= 1 (how-many "1 attached skill (alpha)"
                                       (point-min) (niwa-view-transcript-tail))))
                (should (string-search "COMPOSER SKILL BODY" (buffer-string)))
                (niwa-view--full-rerender)
                (should (= 1 (how-many "1 attached skill (alpha)"
                                       (point-min) (niwa-view-transcript-tail))))
                (should (string-search "COMPOSER SKILL BODY" (buffer-string))))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-plan-handoff-start/unavailable-skills (:quiet t)
  ,test
  (test)
  :doc "missing and disabled selected sources launch nothing and can be retried"
  (dolist (unavailable '(missing disabled))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let* ((view (generate-new-buffer " *retry-skill-view*"))
             (niwa-skills-include-bundled nil)
             (niwa-skills-check-for-modifications nil)
             (skill-root (file-name-concat root "skills"))
             (niwa-skill-dirs (list skill-root))
             (source (file-name-concat skill-root "alpha" "SKILL.md"))
             (body "---\nname: alpha\ndescription: Fixture skill\ncontext: inline\n---\nRETRY SKILL BODY\n")
             launched)
        (unwind-protect
            (progn
              (make-directory (file-name-directory source) t)
              (write-region body nil source nil 'silent)
              (setq-local gptel-system-prompt "Retry fixture" gptel-tools nil)
              (niwa-skills-install session buffer)
              (niwa-chat-install-request-hooks)
              (niwa-view--setup view buffer)
              (with-current-buffer view
                (niwa-view-composer-insert "> retry\ndraft"))
              (let* ((accepted (niwa-plan-archive-accepted
                                (niwa-plan-write-current "# Retry plan" session buffer) session))
                     (selection
                      (list :location 'here :context 'current :execution 'direct
                            :mode 'full-auto :model-provider "Claude Code:sonnet"
                            :skills (list (list :name "alpha" :source-file source)))))
                (if (eq unavailable 'missing)
                    (delete-file source)
                  (write-region
                   "---\nname: alpha\ndescription: Fixture skill\nuser-invocable: false\n---\nDisabled body\n"
                   nil source nil 'silent))
                (cl-letf (((symbol-function 'niwa-claude-code-launch)
                           (niwa-engine-test--claude-launch
                            (lambda (&rest _args)
                              (setq launched t)
                              (list :meta '((responseText . "Completed")))))))
                  (niwa-plan-handoff-start session buffer selection accepted)
                  (should-not launched)
                  (should-not (niwa-turn-busy-p buffer))
                  (let ((retry (plist-get (niwa-session-plan-metadata session)
                                          :implementation-retry)))
                    (should (plist-get retry :failure))
                    (should (equal accepted (plist-get retry :accepted))))
                  (with-current-buffer view
                    (should (equal "> retry\ndraft" (niwa-view-composer-draft-text nil t)))
                    (should-not (string-search "attached skill" (buffer-string))))
                  (write-region body nil source nil 'silent)
                  (niwa-retry-plan-implementation session buffer)
                  (niwa-test--await 5 "Restored skill did not dispatch on retry"
                    (and launched (not (niwa-turn-busy-p buffer))))
                  (should-not (plist-get (niwa-session-plan-metadata session)
                                         :implementation-retry))
                  (with-current-buffer view
                    (should (string-search "1 attached skill (alpha)" (buffer-string)))
                    (should (equal "> retry\ndraft" (niwa-view-composer-draft-text nil t)))))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa--insert-local-user-turn/without-skills (:quiet t)
  ,test
  (test)
  :doc "generated attachment-free turns retain their compact label and draft"
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let ((view (generate-new-buffer " *no-skill-view*")))
      (unwind-protect
          (progn
            (niwa-view--setup view buffer)
            (with-current-buffer view
              (niwa-view-composer-insert "> no skill\ndraft"))
            (niwa--insert-local-user-turn "Full generated prompt" "Implement accepted plan as Goal"
                                             nil nil t)
            (with-current-buffer view
              (should (string-search "You\nImplement accepted plan as Goal\n" (buffer-string)))
              (should-not (string-search "attached skill" (buffer-string)))
              (should-not (string-search "Prompt" (buffer-string)))
              (niwa-view--full-rerender)
              (should (string-search "You\nImplement accepted plan as Goal\n" (buffer-string)))
              (should (equal "> no skill\ndraft" (niwa-view-composer-draft-text nil t)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-view--insert-skill-disclosures (:quiet t)
  ,test
  (test)
  :doc "live and reconstructed skills stay separate from hook context and Prompt"
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let* ((view (generate-new-buffer " *skill-disclosure-view*"))
           (hook (niwa-hooks-format-context
                  '((:event "UserPromptSubmit" :body "HOOK EVIDENCE"))))
           (metadata (niwa-tool-render-data-format
                      '(:kind inline-skill :display-text "Original instruction"
                        :attachments ((:name "alpha" :body "EXACT SKILL BODY"))
                        :prompt "EXACT PREPARED PROMPT"))))
      (unwind-protect
          (progn
            (niwa-view--setup view buffer)
            (with-current-buffer view
              (niwa-view-composer-insert "> keep\ndraft"))
            (niwa--insert-local-user-turn
             (concat "Original instruction\n" hook metadata)
             "Implement accepted plan as Goal" nil hook t)
            (with-current-buffer view
              (should (string-search "hook context added" (buffer-string)))
              (should (string-search "1 attached skill (alpha)" (buffer-string)))
              (should (string-search "Prompt" (buffer-string)))
              (should-not (string-search "EXACT SKILL BODY" (buffer-string)))
              (should-not (string-search "EXACT PREPARED PROMPT" (buffer-string)))
              (should-not (string-search "niwa-render-data" (buffer-string)))
              (goto-char (point-min))
              (search-forward "attached skill")
              (let ((skill-source (get-text-property (1- (point)) 'niwa-view-source)))
                (niwa-view-toggle-section)
                (should (string-search "EXACT SKILL BODY" (buffer-string)))
                (should-not (string-search "EXACT PREPARED PROMPT" (buffer-string)))
                (goto-char (point-min))
                (search-forward "Prompt")
                (should-not (eq skill-source
                                (get-text-property (1- (point)) 'niwa-view-source)))
                (niwa-view-toggle-section))
              (should (string-search "EXACT PREPARED PROMPT" (buffer-string)))
              (should-not (string-search "HOOK EVIDENCE" (buffer-string)))
              (niwa-view--full-rerender)
              (should (string-search "You\nImplement accepted plan as Goal\n" (buffer-string)))
              (should (string-search "EXACT SKILL BODY" (buffer-string)))
              (should (string-search "EXACT PREPARED PROMPT" (buffer-string)))
              (should (equal "> keep\ndraft" (niwa-view-composer-draft-text nil t)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-view--invoke-renderer/skill-source ()
  ,test
  (test)
  :doc "ordinary Skill results deliver exact sources but render only raw body and line count"
  (let* ((root (make-temp-file "skill-source-view-" t))
         (source (file-name-concat root "SKILL.md"))
         (body "FIRST PREPARED LINE\nSECOND PREPARED LINE")
         (skill (niwa-skill--create
                 :name "plain" :body body :source-file source :source-dir root
                 :context 'inline :model-invocable-p t :active-p t))
         (workspace (niwa-workspace--create :type 'test :root root :id root))
         (session (niwa-session--create :workspace workspace :skills (list skill)))
         (tool (niwa-tool-ensure "Skill"))
         result)
    (unwind-protect
        (with-temp-buffer
          (with-temp-file source
            (insert "---\nname: plain\ndescription: Plain\n---\n" body))
          (setq-local niwa--session session
                      niwa--current-request
                      (niwa-request--create
                       :session session :file-snapshots (make-hash-table :test #'equal)))
          (niwa-pipeline-run-tool
           tool (lambda (value) (setq result value)) '(:name "plain"))
          (let* ((parts (niwa-tool-render-data-extract result session))
                 (visible (car parts))
                 (data (cdr parts))
                 (rendering (niwa-view--invoke-renderer
                             tool data '(:name "plain") visible)))
            (should (string-search (niwa-resource--skill-address skill) visible))
            (should (equal body (plist-get data :prompt)))
            (should (equal "Skill: plain (2 lines)" (plist-get rendering :header)))
            (should (equal body (plist-get rendering :body)))
            (should (plist-get rendering :initially-collapsed-p))
            (with-temp-file source (insert "Changed after delivery"))
            (should (equal body (plist-get
                                (niwa-view--invoke-renderer
                                 tool data '(:name "plain") visible)
                                :body)))))
      (delete-directory root t)))

  :doc "source-less Skill delivery remains raw with no invented address or render header"
  (let* ((body "Synthetic body\nSecond line")
         (skill (niwa-skill--create :name "synthetic" :body body :context 'inline))
         (session (niwa-session--create :skills (list skill)))
         envelope)
    (with-temp-buffer
      (setq-local niwa--session session)
      (niwa-skills--invoke-handler
       (lambda (value) (setq envelope value)) '(:name "synthetic")))
    (should (equal body (plist-get envelope :result)))
    (should-not (plist-get envelope :render-data))
    (let ((rendering (niwa-skills--render-skill-tool
                      "Skill" '(:name "synthetic") (plist-get envelope :result) nil)))
      (should (equal body (plist-get rendering :body)))
      (should (equal "Skill: synthetic (2 lines)" (plist-get rendering :header))))))

(niwa-deftest niwa-view--inline-skill-info ()
  ,test
  (test)
  :doc "anchors the decoded record without reparsing its attachment bodies"
  (with-temp-buffer
    (insert "User text\n")
    (let* ((start (point))
           (body (concat "Quoted markers: " niwa-tool-render-data-open
                         "\n" niwa-tool-render-data-close))
           (record (list :kind 'inline-skill :display-text "Use $alpha"
                         :attachments (list (list :name "alpha" :body body))
                         :prompt "Prepared prompt"))
           (decode (symbol-function 'niwa-tool-render-data--read-block))
           (decodes 0))
      (insert (niwa-tool-render-data-format record))
      (let ((end (point)))
        (insert "\nFollowing separator\n")
        (let ((info
               (cl-letf (((symbol-function 'niwa-tool-render-data--read-block)
                          (lambda (&rest args)
                            (cl-incf decodes)
                            (apply decode args))))
                 (niwa-view--inline-skill-info
                  (list (list 'user (point-min) (point-max))) (current-buffer)))))
          (should (equal body (plist-get (car (plist-get info :attachments)) :body)))
          (should (= start (car (plist-get info :source))))
          (should (= end (cdr (plist-get info :source))))
          (should (= 1 decodes))))))

  :doc "rejects untrusted and tool-owned records before accepting a trusted attachment"
  (with-temp-buffer
    (let ((record '(:kind inline-skill :display-text "Use $alpha"
                    :attachments ((:name "alpha" :body "Trusted body"))
                    :prompt "Prepared prompt")))
      (insert (substring-no-properties (niwa-tool-render-data-format record)))
      (insert (niwa-tool-render-data-format record "tool-owner"))
      (should-not
       (niwa-view--inline-skill-info
        (list (list 'user (point-min) (point-max))) (current-buffer)))
      (let ((start (point)))
        (insert (niwa-tool-render-data-format record))
        (let ((info (niwa-view--inline-skill-info
                     (list (list 'user (point-min) (point-max))) (current-buffer))))
          (should (equal "Prepared prompt" (plist-get info :prompt)))
          (should (= start (car (plist-get info :source)))))))))

(provide 'test-niwa-view-render-skills)
;;; test-niwa-view-render-skills.el ends here
