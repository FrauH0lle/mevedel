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
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-skills-prompt-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-skills-prompt-section
                    mevedel-skills--format-listing-result
                    mevedel-reminders-make-skills-delta
                    mevedel-reminders-make-skills-roster-budget
                    mevedel-skills--post-tool-activate
                    mevedel-skills-install-activation-hook
                    mevedel-skills-install-reminder))
    (should (equal "mevedel-skills-prompt"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))


;;
;;; Prompt roster and conditional activation

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
  (let* ((mevedel-compact-context-limit 25)
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
  (let* ((mevedel-compact-context-limit 25)
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

(mevedel-deftest mevedel-reminders-make-skills-roster-budget ()
  ,test
  (test)
  :doc "fires once when roster descriptions are shortened"
  (let* ((mevedel-compact-context-limit 25)
         (mevedel-skills-listing-budget 1.0)
         (mevedel-skills-listing-max-entry-chars 250)
         (session (mevedel-skills-test--make-session))
         (long "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
         (reminder (mevedel-reminders-make-skills-roster-budget)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "s1" :description long
                 :active-p t :model-invocable-p t)
                (mevedel-skill--create
                 :name "s2" :description long
                 :active-p t :model-invocable-p t)))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p "shortened some descriptions" body))
      (should (string-match-p "ListSkills(query)" body)))
    (should-not (funcall (mevedel-reminder-trigger reminder) session)))

  :doc "reports omitted entries when names alone do not fit"
  (let* ((mevedel-compact-context-limit 25)
         (mevedel-skills-listing-budget 1.0)
         (session (mevedel-skills-test--make-session))
         (reminder (mevedel-reminders-make-skills-roster-budget)))
    (setf (mevedel-session-skills session)
          (mapcar
           (lambda (name)
             (mevedel-skill--create
              :name name :description "d"
              :active-p t :model-invocable-p t))
           '("skill-0001" "skill-0002" "skill-0003" "skill-0004"
             "skill-0005" "skill-0006" "skill-0007" "skill-0008")))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (should (string-match-p
             "omitted some active skills"
             (funcall (mevedel-reminder-content reminder) session)))))

(mevedel-deftest mevedel-skills-prompt-section ()
  ,test
  (test)
  :doc "renders canonical active model-invocable skills and concise contract"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r" :root "/tmp/r" :name "r"
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
    (should (string-match-p "^- simplify: Review code$" section))
    (should (string-match-p "^- plugin:flow: Plugin flow$" section))
    (should (string-match-p "\\$SkillName" section))
    (should (string-match-p "Skill(name=\\.\\.\\.)" section))
    (should (string-match-p "ListSkills(query)" section))
    (should (string-match-p "minimal applicable skill set" section))
    (should (string-match-p "Quoted, escaped, or Markdown-code" section))
    (should-not (string-match-p "Pretty Simplifier" section))
    (should-not (string-match-p "SKILL\\.md" section))
    (should-not (string-match-p "hidden" section))
    (should-not (string-match-p "dormant" section)))

  :doc "omits section when no active model-invocable skills exist"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r2" :root "/tmp/r2" :name "r2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "disabled" :description "d"
                 :model-invocable-p nil :active-p t)))
    (should-not (mevedel-skills-prompt-section session))))

(mevedel-deftest mevedel-reminders-make-skills-delta ()
  ,test
  (test)
  :doc "initial snapshot is silent, later additions are reported once"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r3" :root "/tmp/r3" :name "r3"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-delta)))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "alpha" :description "Alpha"
                 :active-p t :model-invocable-p t)))
    (should-not (funcall (mevedel-reminder-trigger reminder) session))
    (should (equal '(("alpha" . "Alpha"))
                   (mevedel-session-skills-snapshot session)))
    (setf (mevedel-session-skills session)
          (append (mevedel-session-skills session)
                  (list (mevedel-skill--create
                         :name "beta" :description "Beta"
                         :active-p t :model-invocable-p t))))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p "Available skills changed" body))
      (should (string-match-p "Added skills:" body))
      (should (string-match-p "beta: Beta" body)))
    (should-not (funcall (mevedel-reminder-trigger reminder) session)))

  :doc "removed skills are listed by name only"
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "r4" :root "/tmp/r4" :name "r4"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (reminder (mevedel-reminders-make-skills-delta)))
    (setf (mevedel-session-skills-snapshot session)
          '(("gone" . "Old description")))
    (should (funcall (mevedel-reminder-trigger reminder) session))
    (let ((body (funcall (mevedel-reminder-content reminder) session)))
      (should (string-match-p "Removed skills:" body))
      (should (string-match-p "  - gone" body))
      (should-not (string-match-p "Old description" body))))

  :doc "overflow uses compact ListSkills suffix"
  (let* ((added (mapcar (lambda (index)
                          (cons (format "added-%02d" index) "A"))
                        (number-sequence 1 12)))
         (removed (mapcar (lambda (index)
                            (cons (format "removed-%02d" index) "R"))
                          (number-sequence 1 11)))
         (body (mevedel-skills--format-delta added removed)))
    (should (string-match-p "and 2 more; use ListSkills(query)" body))
    (should (string-match-p "and 1 more; use ListSkills(query)" body))
    (should-not (string-match-p "omitted; use ListSkills" body))))

(mevedel-deftest mevedel-reminders-make-skills-delta/hot-reload
  (:before-each (mevedel-skills-test--reset-watchers)
   :after-each (mevedel-skills-test--reset-watchers))
  ,test
  (test)
  :doc "trigger refreshes dirty skills from the chat buffer, not prompt buffer"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-skills-reminder-hot-" t))
         (mevedel-user-dir (file-name-as-directory
                            (file-name-concat root "user")))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (buf (generate-new-buffer " *mevedel-test-reminder-hot*"))
         (reminder (mevedel-reminders-make-skills-delta)))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session buf))
          (should (= 1 (length (mevedel-session-skills session))))
          (setf (mevedel-session-skills-snapshot session)
                (mevedel-skills--skill-snapshot session))
          (mevedel-skills-test--write-skill
           root "beta" "name: beta\ndescription: B\n")
          (mevedel-skills--mark-buffer-dirty buf)
          ;; Reminder triggers run from gptel's temporary prompt buffer.
          (let ((mevedel-reminders--current-chat-buffer buf))
            (with-temp-buffer
              (should (funcall (mevedel-reminder-trigger reminder) session))))
          (should (= 2 (length (mevedel-session-skills session))))
          (should-not (gethash buf mevedel-skills--dirty-buffers))
          (should (cl-find "beta" (mevedel-session-skills session)
                           :key #'mevedel-skill-name :test #'equal)))
      (kill-buffer buf)
      (delete-directory root t))))

(mevedel-deftest mevedel-skills--post-tool-activate ()
  ,test
  (test)
  :doc "activates conditional skills using the tool's get-path slot"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (ws (mevedel-workspace--create
              :type 'test :id "p" :root "/tmp/p" :name "p"
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
                    :active-p nil :model-invocable-p t))
         (fake-tool (mevedel-tool--create
                     :name "Read"
                     :handler #'ignore
                     :get-path (lambda (args) (plist-get args :path)))))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (list skill user-only disabled))
          (mevedel-skills--set-enabled disabled nil)
          (cl-letf (((symbol-function 'mevedel-tool-get)
                     (lambda (_name &optional _cat) fake-tool)))
            (with-temp-buffer
              (setq mevedel--session session)
              (mevedel-skills--post-tool-activate
               (list :name "Read" :args '(:path "lib/foo.el")))
              (should (mevedel-skill-active-p skill))
              (should (mevedel-skill-active-p user-only))
              (should-not (mevedel-skill-active-p disabled))
              (should (= 1 (length (mevedel-session-pending-reminders
                                    session))))
              (let ((reminder (car (mevedel-session-pending-reminders
                                    session))))
                (should (string-match-p "lib/foo\\.el" reminder))
                (should (string-match-p "elisp" reminder))
                (should-not (string-match-p "user-only" reminder))
                (should-not (string-match-p "disabled" reminder)))
              (should (equal '(("elisp" . ""))
                             (mevedel-session-skills-snapshot session))))))
      (delete-directory user-dir t))))

(provide 'test-mevedel-skills-prompt)

;;; test-mevedel-skills-prompt.el ends here
