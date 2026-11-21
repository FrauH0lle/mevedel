;;; test-niwa-skills-prompt.el --- Skill roster prompt tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests request-time skill rosters and event-shaped skill reminders.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-compact)
(require 'niwa-file-state)
(require 'niwa-reminders)
(require 'niwa-skills-core)
(require 'niwa-skills-invoke)
(require 'niwa-skills-prompt)
(require 'niwa-structs)
(require 'niwa-tool-patch)
(require 'niwa-tool-registry)
(require 'niwa-workspace)

(niwa-deftest niwa-skills-prompt-ownership ()
  ,test
  (test)
  (dolist (symbol '(niwa-skills-prompt-section
                    niwa-skills--format-listing-result
                    niwa-skills--post-tool-activate
                    niwa-skills-install-activation-hook
                    niwa-skills--short-purpose))
    (should (equal "niwa-skills-prompt"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Prompt roster and conditional activation

(niwa-deftest niwa-skills--short-purpose ()
  ,test
  (test)
  :doc "retains a purpose while omitting later procedures and bounding long prose"
  (should (equal "" (niwa-skills--short-purpose
                     (niwa-skill--create :name "empty" :description ""))))
  (dolist (description '("Review changes. Then run the full review procedure."
                         "Review changes.\nSecond line with triggers."))
    (let ((skill (niwa-skill--create :name "review" :description description)))
      (should (equal "Review changes." (niwa-skills--short-purpose skill)))
      (should (equal description (niwa-skill-description skill)))))
  (should (<= (length (niwa-skills--short-purpose
                       (niwa-skill--create :name "long"
                                              :description (make-string 500 ?x))))
              160)))

(niwa-deftest niwa-skills--listing-budget-chars ()
  ,test
  (test)
  :doc "follows the active model context window"
  ;; `niwa-model-context-limit' is the fallback for a model that
  ;; declares no window, so reading it alone pinned the roster to a
  ;; constant: 2% of a small local window is a much larger share of it.
  (let ((model (make-symbol "niwa-test-model")))
    (put model :context-window 8)
    (let ((gptel-model model)
          (niwa-model-context-limit 200000)
          (niwa-skills-listing-budget 0.02))
      (should (= 640 (niwa-skills--listing-budget-chars)))))

  :doc "budgets against a pending skill model override, not the buffer model"
  ;; gptel funcalls the system prompt before the transforms apply a
  ;; leading skill's :model, so reading the buffer's `gptel-model'
  ;; sized the roster for the wrong window.
  (let ((buffer-model (make-symbol "niwa-test-big"))
        (override-model (make-symbol "niwa-test-small")))
    (put buffer-model :context-window 200)
    (put override-model :context-window 8)
    (with-temp-buffer
      (setq-local gptel-model buffer-model)
      (setq-local niwa-skills--pending-request-context
                  (list :model override-model))
      (cl-letf (((symbol-function 'niwa-model-resolve-selector)
                 (lambda (selector)
                   (list :backend 'stub :model selector))))
        (let ((niwa-model-context-limit 200000)
              (niwa-skills-listing-budget 0.02))
          (should (= 640 (niwa-skills--listing-budget-chars))))))))

(niwa-deftest niwa-skills--format-listing-result ()
  ,test
  (test)
  :doc "includes roster header and one line per skill"
  (let* ((skills (list (niwa-skill--create :name "s1" :description "d1")
                       (niwa-skill--create :name "s2" :description "d2")))
         (listing (plist-get (niwa-skills--format-listing-result skills) :text)))
    (should (string-match-p "### Available skills" listing))
    (should (string-match-p "^- s1: d1$" listing))
    (should (string-match-p "^- s2: d2$" listing)))

  :doc "does not resolve unused name-only fallback prefixes when the roster fits"
  (let ((skill (niwa-skill--create :name "s1" :description "Purpose"))
        (original (symbol-function 'niwa-skills--entry-base))
        (calls 0))
    (cl-letf (((symbol-function 'niwa-skills--entry-base)
               (lambda (&rest args)
                 (cl-incf calls)
                 (apply original args))))
      (let ((result (niwa-skills--format-listing-result (list skill))))
        (should-not (plist-get result :status))
        (should (string-search "s1: Purpose" (plist-get result :text)))
        ;; The full entry and its description limit each need a prefix;
        ;; a name-only fallback must not resolve a third one.
        (should (= 2 calls)))))

  :doc "budget shortens descriptions before omitting skill names"
  (let* ((niwa-model-context-limit 25)
         (niwa-skills-listing-budget 1.0)
         (niwa-skills-listing-max-entry-chars 250)
         (long "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
         (skills (list (niwa-skill--create :name "s1" :description long)
                       (niwa-skill--create :name "s2" :description long)
                       (niwa-skill--create :name "s3" :description long)))
         (result (niwa-skills--format-listing-result skills))
         (listing (plist-get result :text)))
    (should (string-match-p "^- s1: " listing))
    (should (string-match-p "^- s2: " listing))
    (should (string-match-p "^- s3: " listing))
    (should (string-match-p "descriptions were shortened" listing))
    (should (eq 'truncated (plist-get result :status)))
    (should (<= (length listing) (niwa-skills--listing-budget-chars))))

  :doc "omits whole entries only when name-only roster does not fit"
  (let* ((niwa-model-context-limit 25)
         (niwa-skills-listing-budget 1.0)
         (skills (mapcar
                  (lambda (name)
                    (niwa-skill--create :name name :description "d"))
                  '("skill-0001" "skill-0002" "skill-0003" "skill-0004"
                    "skill-0005" "skill-0006" "skill-0007" "skill-0008")))
         (result (niwa-skills--format-listing-result skills))
         (listing (plist-get result :text)))
    (should (string-match-p "skill-0001" listing))
    (should (string-match-p "skills omitted" listing))
    (should (eq 'omitted (plist-get result :status)))
    (should (<= (length listing) (niwa-skills--listing-budget-chars)))))

(niwa-deftest niwa-skills--system-roster-candidates ()
  ,test
  (test)
  :doc "keeps path-scoped discovery out of the baseline across activation"
  (let* ((session (niwa-skills-test--make-session))
         (ordinary (niwa-skill--create :name "ordinary" :active-p t))
         (scoped (niwa-skill--create :name "scoped" :path-patterns '("*.el")))
         (hidden (niwa-skill--create :name "hidden" :active-p t
                                        :model-invocable-p nil)))
    (setf (niwa-session-skills session) (list ordinary scoped hidden))
    (should (equal (list ordinary) (niwa-skills--system-roster-candidates session)))
    (niwa-skills-maybe-activate session "sample.el")
    (should (niwa-skill-active-p scoped))
    (should (memq scoped (niwa-skills--listing-candidates session)))
    (should (equal (list ordinary) (niwa-skills--system-roster-candidates session)))))

(niwa-deftest niwa-skills-prompt-section ()
  ,test
  (test)
  :doc "renders canonical active model-invocable skills and concise contract"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "r" :root "/tmp/r" :name "r"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (section nil))
    (setf (niwa-session-skills session)
          (list (niwa-skill--create
                 :name "simplify"
                 :display-name "Pretty Simplifier"
                 :description "Review code"
                 :source-file "/tmp/r/.niwa/skills/simplify/SKILL.md"
                 :active-p t
                 :model-invocable-p t)
                (niwa-skill--create
                 :name "plugin:flow"
                 :description "Plugin flow"
                 :active-p t
                 :model-invocable-p t)
                (niwa-skill--create
                 :name "hidden"
                 :description "Hidden"
                 :active-p t
                 :model-invocable-p nil)
                (niwa-skill--create
                 :name "dormant"
                 :description "Dormant"
                 :active-p nil
                 :model-invocable-p t
                 :path-patterns '("*.el"))))
    (setq section (niwa-skills-prompt-section session))
    (should (string-match-p "## Skills" section))
    (should (string-match-p
             "^- simplify \\[Read skill://simplify@[[:xdigit:]]+\\]: Review code$"
             section))
    (should (string-match-p "^- plugin:flow: Plugin flow$" section))
    (should (string-match-p "\\$SkillName" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "(Skill :name" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "(ListSkills :query" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "optional" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "listed registered resource address" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should (string-match-p "Quoted, escaped, or Markdown-code" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))
    (should-not (string-match-p "Pretty Simplifier" section))
    (should-not (string-match-p "SKILL\\.md" section))
    (should-not (string-match-p "hidden" section))
    (should-not (string-match-p "^- dormant:" section)))

  :doc "records roster rendering only during detailed profiling"
  (let ((session (niwa-skills-test--make-session))
        recorded detailed)
    (setf (niwa-session-skills session)
          (list (niwa-skill--create :name "roster" :description "Guidance"
                                       :active-p t :model-invocable-p t)))
    (cl-letf (((symbol-function 'niwa-telemetry-detailed-p)
               (lambda (_session) detailed))
              ((symbol-function 'niwa-telemetry-record)
               (lambda (_session event &rest props)
                 (push (cons event props) recorded))))
      (let ((section (niwa-skills-prompt-section session)))
        (should (string-search "Guidance" section))
        (should-not recorded)
        (setq detailed t)
        (should (equal section (niwa-skills-prompt-section session)))
        (should (= 1 (length recorded)))
        (should (eq 'skills-roster-advertised (caar recorded)))
        (should (= 1 (plist-get (cdar recorded) :skill-count)))
        (should (equal '("roster") (plist-get (cdar recorded) :skill-names))))))

  :doc "keeps skill discovery reachable when no roster names fit"
  (let ((session (niwa-skills-test--make-session))
        (niwa-skills-listing-budget 0))
    (setf (niwa-session-skills session)
          (list (niwa-skill--create :name "a-long-name" :active-p t
                                       :model-invocable-p t)))
    (let ((section (niwa-skills-prompt-section session)))
      (should-not (string-match-p "^- a-long-name:" section))
      (should (string-search "ListSkills by purpose" section))
      (should (string-search "omitted from this budgeted roster" (niwa-system-render-prompt-file "prompts/system/skill-policy.md")))))

  :doc "path discovery leaves the roster unchanged across activation resets"
  (let* ((session (niwa-skills-test--make-session))
         (scoped (niwa-skill--create :name "scoped" :description "Elisp guidance"
                                        :path-patterns '("*.el") :active-p nil))
         (ordinary (niwa-skill--create :name "ordinary" :description "General guidance"
                                          :active-p t)))
    (dolist (skills (list (list scoped) (list ordinary scoped)))
      (setf (niwa-session-skills session) skills
            (niwa-skill-active-p scoped) nil)
      (let ((section (niwa-skills-prompt-section session)))
        (niwa-skills-maybe-activate session "sample.el")
        (should (niwa-skill-active-p scoped))
        (should (equal section (niwa-skills-prompt-section session)))
        ;; Rediscovery after restart resets catalogue path activity.
        (setf (niwa-skill-active-p scoped) nil)
        (should (equal section (niwa-skills-prompt-section session)))))
    ;; An authored change must still replace stale baseline guidance.
    (setf (niwa-skill-description ordinary) "Updated guidance")
    (should (string-search "Updated guidance" (niwa-skills-prompt-section session))))

  :doc "omits section when no active model-invocable skills exist"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "r2" :root "/tmp/r2" :name "r2"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws)))
    (setf (niwa-session-skills session)
          (list (niwa-skill--create
                 :name "disabled" :description "d"
                 :model-invocable-p nil :active-p t)))
    (should-not (niwa-skills-prompt-section session))))

(niwa-deftest niwa-skills--post-tool-activate ()
  ,test
  (test)
  :doc "root and agents independently discover a shared path skill and cancellation does not acknowledge it"
  (let* ((root (make-temp-file "niwa-path-discovery-" t))
         (niwa-user-dir (file-name-as-directory root))
         (session (niwa-skills-test--make-session))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "path-discovery" :key "test" :models '(test)))
         (skill (niwa-skill--create :name "path-helper" :description "Inspect Elisp"
                                      :path-patterns '("*.el") :active-p nil))
         (tool (niwa-tool--create :name "PathRead" :get-path
                                     (lambda (args) (plist-get args :path))))
         (call '(:tool-name "PathRead" :tool-input (:path "sample.el")))
         (buffers (mapcar (lambda (name) (generate-new-buffer name))
                          '(" *path-root*" " *path-a*" " *path-b*"))))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (cl-letf (((symbol-function 'niwa-tool-get)
                     (lambda (&rest _) tool)))
            (dolist (buffer buffers)
              (with-current-buffer buffer
                (setq-local niwa--session session
                            gptel-tools (list (gptel-make-tool :name "Skill")))
                (if (eq buffer (car buffers))
                    (setq-local niwa--current-request
                                (niwa-request--create :id "root-1" :session session))
                  (setq-local niwa--agent-invocation
                              (niwa-agent-invocation--create :parent-session session)))
                (niwa-skills--post-tool-activate call)
                (let ((staged (niwa-reminders--stage-turn-events buffer))
                      (root-snapshot (niwa-skills-prompt-section session)))
                  (should (string-search
                           "path-helper" (plist-get (car (plist-get staged :entries)) :body)))
                  (should (equal root-snapshot (niwa-skills-prompt-section session)))
                  ;; Abandon this delivery and retry the observation.  An agent
                  ;; may already have activated the shared catalogue entry.
                  (setq niwa-reminders--turn-events nil)
                  (niwa-skills--post-tool-activate call)
                  (setq staged (niwa-reminders--stage-turn-events buffer))
                  (should (plist-get staged :entries))
                  (let* ((data (list :messages [(:role "user" :content "task")]))
                         (fsm (gptel-make-fsm
                               :info (list :buffer buffer :backend backend :data data
                                           :position (with-current-buffer buffer (point-marker))))))
                    (niwa-reminders--handle-inject fsm)
                    (should (string-search "path-helper" (json-encode data)))
                    (should (equal root-snapshot
                                   (niwa-skills-prompt-section session))))
                  (niwa-skills--post-tool-activate call)
                  (should-not (plist-get (niwa-reminders--stage-turn-events buffer)
                                         :entries)))
                (setf (niwa-skill-description skill)
                      (concat (niwa-skill-description skill) " changed"))
                (setq gptel-tools nil)
                (niwa-skills--post-tool-activate call)
                (should-not (plist-get niwa-reminders--turn-events :items))
                (setq gptel-tools (list (gptel-make-tool :name "Skill")))
                (niwa-skills--post-tool-activate
                 '(:tool-name "PathRead" :tool-input (:path "unrelated.txt")))
                (should-not (plist-get niwa-reminders--turn-events :items))
                (niwa-skills--post-tool-activate call)
                (should (plist-get niwa-reminders--turn-events :items))))))
      (mapc #'kill-buffer buffers)
      (delete-directory root t)))

  :doc "keeps path extraction scoped to the actual wrapped tool category"
  (let ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
        (session (niwa-skills-test--make-session))
        (skill (niwa-skill--create :name "path-helper" :description "Elisp"
                                     :path-patterns '("*.el") :active-p nil)))
    (setf (niwa-session-skills session) (list skill))
    (niwa-tool-register (niwa-tool--create :name "Same" :category "server-a"
                                                 :get-path (lambda (args) (plist-get args :first))))
    (niwa-tool-register (niwa-tool--create :name "Same" :category "server-b"
                                                 :get-path (lambda (args) (plist-get args :second))))
    (with-temp-buffer
      (setq-local niwa--session session gptel-tools nil)
      (niwa-skills--post-tool-activate
       '(:tool-name "Same" :tool-category "server-a"
         :tool-input (:first "example.el" :second "unrelated.txt"))))
    (should (niwa-skill-active-p skill)))

  :doc "activates on every path a file-writing tool declares at once"
  ;; ApplyPatch is the only tool that writes a file, and it declares its
  ;; paths together rather than one at a time, so a path-scoped skill
  ;; never woke up on an edit -- only on the reads around it.
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (root (make-temp-file "niwa-skills-patch-" t))
         (ws (niwa-workspace--create
              :type 'file :id "p" :root root :name "p"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil
                 :model-invocable-p t))
         (other-skill (niwa-skill--create
                       :name "docs" :path-patterns '("*.md") :active-p nil
                       :model-invocable-p t))
         (fake-tool (niwa-tool--create
                     :name "ApplyPatch"
                     :handler #'ignore
                     :get-paths #'niwa-tool-patch--get-paths)))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill other-skill))
          (cl-letf (((symbol-function 'niwa-tool-get)
                     (lambda (_name &optional _cat) fake-tool)))
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory root))
              (setq-local niwa--session session
                          gptel-tools (list (gptel-make-tool :name "Skill")))
              (setq-local niwa--current-request
                          (niwa-request--create :id "request-1"
                                                   :session session))
              (niwa-skills--post-tool-activate
               (list :tool-name "ApplyPatch"
                     :tool-input (list :patch
                                 (concat "*** Begin Patch\n"
                                         "*** Add File: lib/foo.el\n"
                                         "+x\n"
                                         "*** Add File: README.md\n"
                                         "+docs\n"
                                         "*** End Patch"))))
              (should (niwa-skill-active-p skill))
              (should (niwa-skill-active-p other-skill))
              (should (= 2 (length (plist-get niwa-reminders--turn-events :items))))
              (should (string-match-p
                       "foo\\.el"
                       (plist-get
                        (cdar (plist-get niwa-reminders--turn-events
                                         :items))
                        :body))))))
      (delete-directory user-dir t)
      (delete-directory root t)))

  :doc "activates conditional skills using the tool's get-path slot"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (ws (niwa-workspace--create
              :type 'file :id "p"
              :root (niwa-test--placeholder-root) :name "p"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "elisp" :path-patterns '("*.el") :active-p nil
                 :model-invocable-p t))
         (user-only (niwa-skill--create
                     :name "user-only" :path-patterns '("*.el")
                     :active-p nil :model-invocable-p nil))
         (disabled (niwa-skills-test--stateful-skill
                    :name "disabled" :path-patterns '("*.el")
                    :active-p nil :model-invocable-p t
                    :workspace ws))
         (fake-tool (niwa-tool--create
                     :name "Read"
                     :handler #'ignore
                     :get-path (lambda (args) (plist-get args :path)))))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session)
                (list skill user-only disabled))
          (niwa-skills-set-enabled disabled nil)
          (cl-letf (((symbol-function 'niwa-tool-get)
                     (lambda (_name &optional _cat) fake-tool)))
            (with-temp-buffer
              (setq-local niwa--session session
                          gptel-tools (list (gptel-make-tool :name "Skill")))
              (setq-local niwa--current-request
                          (niwa-request--create :id "request-1"
                                                   :session session))
              (niwa-skills--post-tool-activate
               (list :tool-name "Read" :tool-input '(:path "lib/foo.el")))
              (should (niwa-skill-active-p skill))
              (should (niwa-skill-active-p user-only))
              (should-not (niwa-skill-active-p disabled))
              (should (= 1 (length
                            (plist-get niwa-reminders--turn-events
                                       :items))))
              (let ((reminder
                     (plist-get
                      (cdar (plist-get niwa-reminders--turn-events :items))
                      :body)))
                (should (string-match-p "lib/foo\\.el" reminder))
                (should (string-match-p "elisp" reminder))
                (should-not (string-match-p "user-only" reminder))
                (should-not (string-match-p "disabled" reminder)))
              (should-not (niwa-skills-prompt-section session)))))
      (delete-directory user-dir t))))

(provide 'test-niwa-skills-prompt)

;;; test-niwa-skills-prompt.el ends here
