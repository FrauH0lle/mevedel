;;; test-mevedel-skills-prompt.el --- Skill roster prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests request-time skill rosters and event-shaped skill reminders.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact)
(require 'mevedel-file-state)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-prompt)
(require 'mevedel-structs)
(require 'mevedel-tool-patch)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-skills-prompt-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-skills-prompt-section
                    mevedel-skills--format-listing-result
                    mevedel-skills--post-tool-activate
                    mevedel-skills-install-activation-hook
                    mevedel-skills--short-purpose))
    (should (equal "mevedel-skills-prompt"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Prompt roster and conditional activation

(mevedel-deftest mevedel-skills--short-purpose ()
  ,test
  (test)
  :doc "retains a purpose while omitting later procedures and bounding long prose"
  (should (equal "" (mevedel-skills--short-purpose
                     (mevedel-skill--create :name "empty" :description ""))))
  (dolist (description '("Review changes. Then run the full review procedure."
                         "Review changes.\nSecond line with triggers."))
    (let ((skill (mevedel-skill--create :name "review" :description description)))
      (should (equal "Review changes." (mevedel-skills--short-purpose skill)))
      (should (equal description (mevedel-skill-description skill)))))
  (should (<= (length (mevedel-skills--short-purpose
                       (mevedel-skill--create :name "long"
                                              :description (make-string 500 ?x))))
              160)))

(mevedel-deftest mevedel-skills--listing-budget-chars ()
  ,test
  (test)
  :doc "follows the active model context window"
  ;; `mevedel-model-context-limit' is the fallback for a model that
  ;; declares no window, so reading it alone pinned the roster to a
  ;; constant: 2% of a small local window is a much larger share of it.
  (let ((model (make-symbol "mevedel-test-model")))
    (put model :context-window 8)
    (let ((gptel-model model)
          (mevedel-model-context-limit 200000)
          (mevedel-skills-listing-budget 0.02))
      (should (= 640 (mevedel-skills--listing-budget-chars)))))

  :doc "budgets against a pending skill model override, not the buffer model"
  ;; gptel funcalls the system prompt before the transforms apply a
  ;; leading skill's :model, so reading the buffer's `gptel-model'
  ;; sized the roster for the wrong window.
  (let ((buffer-model (make-symbol "mevedel-test-big"))
        (override-model (make-symbol "mevedel-test-small")))
    (put buffer-model :context-window 200)
    (put override-model :context-window 8)
    (with-temp-buffer
      (setq-local gptel-model buffer-model)
      (setq-local mevedel-skills--pending-request-context
                  (list :model override-model))
      (cl-letf (((symbol-function 'mevedel-model-resolve-selector)
                 (lambda (selector)
                   (list :backend 'stub :model selector))))
        (let ((mevedel-model-context-limit 200000)
              (mevedel-skills-listing-budget 0.02))
          (should (= 640 (mevedel-skills--listing-budget-chars))))))))

(mevedel-deftest mevedel-skills--format-listing ()
  ,test
  (test)
  :doc "includes roster header and one line per skill"
  (let* ((skills (list (mevedel-skill--create :name "s1" :description "d1")
                       (mevedel-skill--create :name "s2" :description "d2")))
         (listing (mevedel-skills--format-listing skills)))
    (should (string-match-p "### Available skills" listing))
    (should (string-match-p "^- s1: d1$" listing))
    (should (string-match-p "^- s2: d2$" listing)))

  :doc "budget shortens descriptions before omitting skill names"
  (let* ((mevedel-model-context-limit 25)
         (mevedel-skills-listing-budget 1.0)
         (mevedel-skills-listing-max-entry-chars 250)
         (long "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
         (skills (list (mevedel-skill--create :name "s1" :description long)
                       (mevedel-skill--create :name "s2" :description long)
                       (mevedel-skill--create :name "s3" :description long)))
         (result (mevedel-skills--format-listing-result skills))
         (listing (plist-get result :text)))
    (should (string-match-p "^- s1: " listing))
    (should (string-match-p "^- s2: " listing))
    (should (string-match-p "^- s3: " listing))
    (should (string-match-p "descriptions were shortened" listing))
    (should (eq 'truncated (plist-get result :status)))
    (should (<= (length listing) (mevedel-skills--listing-budget-chars))))

  :doc "omits whole entries only when name-only roster does not fit"
  (let* ((mevedel-model-context-limit 25)
         (mevedel-skills-listing-budget 1.0)
         (skills (mapcar
                  (lambda (name)
                    (mevedel-skill--create :name name :description "d"))
                  '("skill-0001" "skill-0002" "skill-0003" "skill-0004"
                    "skill-0005" "skill-0006" "skill-0007" "skill-0008")))
         (result (mevedel-skills--format-listing-result skills))
         (listing (plist-get result :text)))
    (should (string-match-p "skill-0001" listing))
    (should (string-match-p "skills omitted" listing))
    (should (eq 'omitted (plist-get result :status)))
    (should (<= (length listing) (mevedel-skills--listing-budget-chars)))))

(mevedel-deftest mevedel-skills--system-roster-candidates ()
  ,test
  (test)
  :doc "keeps path-scoped discovery out of the baseline across activation"
  (let* ((session (mevedel-skills-test--make-session))
         (ordinary (mevedel-skill--create :name "ordinary" :active-p t))
         (scoped (mevedel-skill--create :name "scoped" :path-patterns '("*.el")))
         (hidden (mevedel-skill--create :name "hidden" :active-p t
                                        :model-invocable-p nil)))
    (setf (mevedel-session-skills session) (list ordinary scoped hidden))
    (should (equal (list ordinary) (mevedel-skills--system-roster-candidates session)))
    (mevedel-skills-maybe-activate session "sample.el")
    (should (mevedel-skill-active-p scoped))
    (should (memq scoped (mevedel-skills--listing-candidates session)))
    (should (equal (list ordinary) (mevedel-skills--system-roster-candidates session)))))

(mevedel-deftest mevedel-skills-prompt-section ()
  ,test
  (test)
  :doc "renders canonical active model-invocable skills and concise contract"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "r" :root "/tmp/r" :name "r"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (section nil))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "simplify"
                 :display-name "Pretty Simplifier"
                 :description "Review code"
                 :source-file "/tmp/r/.mevedel/skills/simplify/SKILL.md"
                 :active-p t
                 :model-invocable-p t)
                (mevedel-skill--create
                 :name "plugin:flow"
                 :description "Plugin flow"
                 :active-p t
                 :model-invocable-p t)
                (mevedel-skill--create
                 :name "hidden"
                 :description "Hidden"
                 :active-p t
                 :model-invocable-p nil)
                (mevedel-skill--create
                 :name "dormant"
                 :description "Dormant"
                 :active-p nil
                 :model-invocable-p t
                 :path-patterns '("*.el"))))
    (setq section (mevedel-skills-prompt-section session))
    (should (string-match-p "## Skills" section))
    (should (string-match-p
             "^- simplify \\[Read skill://simplify@[[:xdigit:]]+\\]: Review code$"
             section))
    (should (string-match-p "^- plugin:flow: Plugin flow$" section))
    (should (string-match-p "\\$SkillName" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "(Skill :name" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "(ListSkills :query" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "optional" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "skill://" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "Quoted, escaped, or Markdown-code" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should-not (string-match-p "Pretty Simplifier" section))
    (should-not (string-match-p "SKILL\\.md" section))
    (should-not (string-match-p "hidden" section))
    (should-not (string-match-p "^- dormant:" section)))

  :doc "keeps skill discovery reachable when no roster names fit"
  (let ((session (mevedel-skills-test--make-session))
        (mevedel-skills-listing-budget 0))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create :name "a-long-name" :active-p t
                                       :model-invocable-p t)))
    (let ((section (mevedel-skills-prompt-section session)))
      (should-not (string-match-p "^- a-long-name:" section))
      (should (string-search "ListSkills by purpose" section))
      (should (string-search "omitted from this budgeted roster" (mevedel-system-render-prompt-file "prompts/system/skill-policy.md")))))

  :doc "path discovery changes neither baseline text nor its acknowledged snapshot"
  (let* ((session (mevedel-skills-test--make-session))
         (scoped (mevedel-skill--create :name "scoped" :description "Elisp guidance"
                                        :path-patterns '("*.el") :active-p nil))
         (ordinary (mevedel-skill--create :name "ordinary" :description "General guidance"
                                          :active-p t)))
    (dolist (skills (list (list scoped) (list ordinary scoped)))
      (setf (mevedel-session-skills session) skills
            (mevedel-skill-active-p scoped) nil)
      (let ((section (mevedel-skills-prompt-section session))
            (snapshot (mevedel-skills-prompt-section session)))
        (mevedel-skills-maybe-activate session "sample.el")
        (should (mevedel-skill-active-p scoped))
        (should (equal section (mevedel-skills-prompt-section session)))
        (should (equal snapshot (mevedel-skills-prompt-section session)))
        ;; Rediscovery after restart resets catalogue path activity.
        (setf (mevedel-skill-active-p scoped) nil)
        (should (equal section (mevedel-skills-prompt-section session)))))
    ;; An authored change must still replace stale baseline guidance.
    (setf (mevedel-skill-description ordinary) "Updated guidance")
    (should (string-search "Updated guidance" (mevedel-skills-prompt-section session))))

  :doc "omits section when no active model-invocable skills exist"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "r2" :root "/tmp/r2" :name "r2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "disabled" :description "d"
                 :model-invocable-p nil :active-p t)))
    (should-not (mevedel-skills-prompt-section session))))

(mevedel-deftest mevedel-skills--post-tool-activate ()
  ,test
  (test)
  :doc "root and agents independently discover a shared path skill and cancellation does not acknowledge it"
  (let* ((root (make-temp-file "mevedel-path-discovery-" t))
         (mevedel-user-dir (file-name-as-directory root))
         (session (mevedel-skills-test--make-session))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "path-discovery" :key "test" :models '(test)))
         (skill (mevedel-skill--create :name "path-helper" :description "Inspect Elisp"
                                      :path-patterns '("*.el") :active-p nil))
         (tool (mevedel-tool--create :name "PathRead" :get-path
                                     (lambda (args) (plist-get args :path))))
         (call '(:tool-name "PathRead" :tool-input (:path "sample.el")))
         (buffers (mapcar (lambda (name) (generate-new-buffer name))
                          '(" *path-root*" " *path-a*" " *path-b*"))))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (cl-letf (((symbol-function 'mevedel-tool-get)
                     (lambda (&rest _) tool)))
            (dolist (buffer buffers)
              (with-current-buffer buffer
                (setq-local mevedel--session session
                            gptel-tools (list (gptel-make-tool :name "Skill")))
                (if (eq buffer (car buffers))
                    (setq-local mevedel--current-request
                                (mevedel-request--create :id "root-1" :session session))
                  (setq-local mevedel--agent-invocation
                              (mevedel-agent-invocation--create :parent-session session)))
                (mevedel-skills--post-tool-activate call)
                (let ((staged (mevedel-reminders--stage-turn-events buffer))
                      (root-snapshot (mevedel-skills-prompt-section session)))
                  (should (string-search
                           "path-helper" (plist-get (car (plist-get staged :entries)) :body)))
                  (should (equal root-snapshot (mevedel-skills-prompt-section session)))
                  ;; Abandon this delivery and retry the observation.  An agent
                  ;; may already have activated the shared catalogue entry.
                  (setq mevedel-reminders--turn-events nil)
                  (mevedel-skills--post-tool-activate call)
                  (setq staged (mevedel-reminders--stage-turn-events buffer))
                  (should (plist-get staged :entries))
                  (let* ((data (list :messages [(:role "user" :content "task")]))
                         (fsm (gptel-make-fsm
                               :info (list :buffer buffer :backend backend :data data
                                           :position (with-current-buffer buffer (point-marker))))))
                    (mevedel-reminders--handle-inject fsm)
                    (should (string-search "path-helper" (json-encode data)))
                    (should (equal root-snapshot
                                   (mevedel-skills-prompt-section session))))
                  (mevedel-skills--post-tool-activate call)
                  (should-not (plist-get (mevedel-reminders--stage-turn-events buffer)
                                         :entries)))
                (setf (mevedel-skill-description skill)
                      (concat (mevedel-skill-description skill) " changed"))
                (setq gptel-tools nil)
                (mevedel-skills--post-tool-activate call)
                (should-not (plist-get mevedel-reminders--turn-events :items))
                (setq gptel-tools (list (gptel-make-tool :name "Skill")))
                (mevedel-skills--post-tool-activate
                 '(:tool-name "PathRead" :tool-input (:path "unrelated.txt")))
                (should-not (plist-get mevedel-reminders--turn-events :items))
                (mevedel-skills--post-tool-activate call)
                (should (plist-get mevedel-reminders--turn-events :items))))))
      (mapc #'kill-buffer buffers)
      (delete-directory root t)))

  :doc "keeps path extraction scoped to the actual wrapped tool category"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
        (session (mevedel-skills-test--make-session))
        (skill (mevedel-skill--create :name "path-helper" :description "Elisp"
                                     :path-patterns '("*.el") :active-p nil)))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-tool-register (mevedel-tool--create :name "Same" :category "server-a"
                                                 :get-path (lambda (args) (plist-get args :first))))
    (mevedel-tool-register (mevedel-tool--create :name "Same" :category "server-b"
                                                 :get-path (lambda (args) (plist-get args :second))))
    (with-temp-buffer
      (setq-local mevedel--session session gptel-tools nil)
      (mevedel-skills--post-tool-activate
       '(:tool-name "Same" :tool-category "server-a"
         :tool-input (:first "example.el" :second "unrelated.txt"))))
    (should (mevedel-skill-active-p skill)))

  :doc "activates on every path a file-writing tool declares at once"
  ;; ApplyPatch is the only tool that writes a file, and it declares its
  ;; paths together rather than one at a time, so a path-scoped skill
  ;; never woke up on an edit -- only on the reads around it.
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (root (make-temp-file "mevedel-skills-patch-" t))
         (ws (mevedel-workspace--create
              :type 'file :id "p" :root root :name "p"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil
                 :model-invocable-p t))
         (other-skill (mevedel-skill--create
                       :name "docs" :path-patterns '("*.md") :active-p nil
                       :model-invocable-p t))
         (fake-tool (mevedel-tool--create
                     :name "ApplyPatch"
                     :handler #'ignore
                     :get-paths #'mevedel-tool-patch--get-paths)))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill other-skill))
          (cl-letf (((symbol-function 'mevedel-tool-get)
                     (lambda (_name &optional _cat) fake-tool)))
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory root))
              (setq-local mevedel--session session
                          gptel-tools (list (gptel-make-tool :name "Skill")))
              (setq-local mevedel--current-request
                          (mevedel-request--create :id "request-1"
                                                   :session session))
              (mevedel-skills--post-tool-activate
               (list :tool-name "ApplyPatch"
                     :tool-input (list :patch
                                 (concat "*** Begin Patch\n"
                                         "*** Add File: lib/foo.el\n"
                                         "+x\n"
                                         "*** Add File: README.md\n"
                                         "+docs\n"
                                         "*** End Patch"))))
              (should (mevedel-skill-active-p skill))
              (should (mevedel-skill-active-p other-skill))
              (should (= 2 (length (plist-get mevedel-reminders--turn-events :items))))
              (should (string-match-p
                       "foo\\.el"
                       (plist-get
                        (cdar (plist-get mevedel-reminders--turn-events
                                         :items))
                        :body))))))
      (delete-directory user-dir t)
      (delete-directory root t)))

  :doc "activates conditional skills using the tool's get-path slot"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (ws (mevedel-workspace--create
              :type 'file :id "p" :root "/tmp/p" :name "p"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil
                 :model-invocable-p t))
         (user-only (mevedel-skill--create
                     :name "user-only" :path-patterns '("*.el")
                     :active-p nil :model-invocable-p nil))
         (disabled (mevedel-skills-test--stateful-skill
                    :name "disabled" :path-patterns '("*.el")
                    :active-p nil :model-invocable-p t
                    :workspace ws))
         (fake-tool (mevedel-tool--create
                     :name "Read"
                     :handler #'ignore
                     :get-path (lambda (args) (plist-get args :path)))))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (list skill user-only disabled))
          (mevedel-skills-set-enabled disabled nil)
          (cl-letf (((symbol-function 'mevedel-tool-get)
                     (lambda (_name &optional _cat) fake-tool)))
            (with-temp-buffer
              (setq-local mevedel--session session
                          gptel-tools (list (gptel-make-tool :name "Skill")))
              (setq-local mevedel--current-request
                          (mevedel-request--create :id "request-1"
                                                   :session session))
              (mevedel-skills--post-tool-activate
               (list :tool-name "Read" :tool-input '(:path "lib/foo.el")))
              (should (mevedel-skill-active-p skill))
              (should (mevedel-skill-active-p user-only))
              (should-not (mevedel-skill-active-p disabled))
              (should (= 1 (length
                            (plist-get mevedel-reminders--turn-events
                                       :items))))
              (let ((reminder
                     (plist-get
                      (cdar (plist-get mevedel-reminders--turn-events :items))
                      :body)))
                (should (string-match-p "lib/foo\\.el" reminder))
                (should (string-match-p "elisp" reminder))
                (should-not (string-match-p "user-only" reminder))
                (should-not (string-match-p "disabled" reminder)))
              (should-not (mevedel-skills-prompt-section session)))))
      (delete-directory user-dir t))))

(provide 'test-mevedel-skills-prompt)

;;; test-mevedel-skills-prompt.el ends here
