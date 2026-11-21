;;; test-niwa-resource-skill.el --- Skill resource provider tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for skill resource listing, aliases, and exact source resolution.

;;; Code:

(require 'niwa-resource)
(require 'niwa-skills-core)
(require 'niwa-structs)
(require 'niwa-pipeline)
(require 'niwa-permissions)
(require 'niwa-permission-queue)
(require 'niwa-skills-prompt)
(require 'niwa-tool-fs)
(require 'niwa-tool-skills)
(require 'niwa-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-skills--entry-base
  (:quiet t :doc "listed external skill address reads through the pipeline without host grants")
  (let* ((root (make-temp-file "niwa-skill-roster-workspace-" t))
         (external (make-temp-file "niwa-skill-roster-source-" t))
         (file (file-name-concat external "SKILL.md"))
         (workspace (niwa-workspace--create :root root :id root :type 'project))
         (skill (niwa-skill--create
                 :name "demo" :description "Known skill"
                 :source-file file :source-dir external
                 :model-invocable-p t :active-p t))
         (session (niwa-session--create
                   :workspace workspace :working-directory root
                   :permission-mode 'full-auto :skills (list skill))))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local niwa--session session)
          (setq-local temporary-file-directory root)
          (with-temp-file file
            (insert "---\nname: demo\n---\nRegistered skill contents.\n"))
          (niwa-tool-fs--register)
          (niwa-tool-skills--register)
          (let ((niwa-permission-rules nil)
                listing address result)
            (niwa-pipeline-run-tool
             (niwa-tool-get "ListSkills")
             (lambda (value) (setq listing value)) nil)
            (should (string-match "skill://demo@[[:xdigit:]]+" listing))
            (setq address (match-string 0 listing))
            (should-not (string-match-p (regexp-quote external) listing))
            (should (string-match-p
                     (regexp-quote address) (niwa-skills-prompt-section session)))
            (niwa-pipeline-run-tool
             (niwa-tool-get "Read") (lambda (value) (setq result value))
             (list :file_path address))
            (should (string-match-p "Registered skill contents" result))
            (should-not (niwa-session-resource-grants session))
            (should-not (niwa-session-permission-queue session))
            (setf (niwa-skill-enabled-p skill) nil)
            (setq result nil)
            (niwa-pipeline-run-tool
             (niwa-tool-get "Read") (lambda (value) (setq result value))
             (list :file_path address))
            (should (string-prefix-p "Error:" result))
            (should-not (string-match-p "Registered skill contents" result))
            (should-not (niwa-session-permission-queue session))))
      (niwa-permission-queue-abort-all session)
      (delete-directory external t)
      (delete-directory root t))))

(niwa-deftest niwa-resource-skill-provider ()
  ,test
  (test)
  :doc "lists and reads an exact discovered skill source"
  (let* ((root (make-temp-file "niwa-resource-skill-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (workspace (niwa-workspace--create
                     :type 'test :id root :root root :name "resource-skill"))
         (skill (niwa-skill--create
                 :name "demo" :description "A test skill"
                 :source-file skill-file :source-dir skill-dir))
         (session (niwa-session--create
                   :workspace workspace :skills (list skill)))
         (digest (niwa-resource-skill-digest skill-file))
         (address (format "skill://demo@%s" digest)))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\nname: demo\n---\nUse the demo skill.\n"))
          (make-directory (file-name-concat skill-dir "templates") t)
          (with-temp-file (file-name-concat skill-dir "templates" "prompt.tmpl")
            (insert "template needle\n"))
          (dolist (operation '(glob grep))
            (let ((failure (should-error
                            (niwa-resource-prepare operation "skill://"
                                                      (list :session session)))))
              (should (string-search "Read skill://" (error-message-string failure)))
              (should (string-search "ListSkills" (error-message-string failure)))))
          (let* ((listing
                  (niwa-resource-execute
                   (niwa-resource-prepare
                    'read "skill://" (list :session session))))
                 (attempt (niwa-resource-prepare
                           'read address (list :session session)))
                 (body
                  (niwa-resource-execute
                   attempt
                   (lambda (path _address)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))))
            (should (string-match-p (regexp-quote address)
                                    (plist-get listing :result)))
            (should (string-match-p "Use the demo skill" body)))
          (let (glob-path grep-path)
            (niwa-resource-execute
             (niwa-resource-prepare 'glob address
                                       (list :session session))
             (lambda (path authored)
               (setq glob-path (list path authored))))
            (niwa-resource-execute
             (niwa-resource-prepare 'grep address
                                       (list :session session))
             (lambda (path authored)
               (setq grep-path (list path authored))))
            (should (equal (list skill-dir address) glob-path))
            (should (equal (list skill-dir address) grep-path))))
      (delete-directory root t)))

  :doc "disabled skills are absent from listings and reject exact resolution"
  (let* ((root (make-temp-file "niwa-resource-disabled-skill-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (workspace (niwa-workspace--create
                     :type 'test :id root :root root
                     :name "resource-disabled-skill"))
         (skill (niwa-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir :enabled-p nil))
         (session (niwa-session--create
                   :workspace workspace :skills (list skill)))
         (address (format "skill://demo@%s"
                          (niwa-resource-skill-digest skill-file))))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file (insert "disabled\n"))
          (let ((listing
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "skill://" (list :session session)))))
            (should-not (string-match-p
                         (regexp-quote address)
                         (plist-get listing :result))))
          (should-error
           (niwa-resource-execute
            (niwa-resource-prepare
             'read address (list :session session)))
           :type 'niwa-resource-unavailable)
          (let ((err
                 (should-error
                  (niwa-resource-prepare
                   'read "skill://local-agents/demo"
                   (list :session session))
                  :type 'niwa-resource-error)))
            (should (string-match-p
                     "Skill alias does not name an enabled skill"
                     (error-message-string err)))))
      (delete-directory root t)))

  :doc "resolves readable aliases once while preserving the authored address"
  (let* ((root (make-temp-file "niwa-resource-skill-alias-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (replacement-dir (file-name-concat root "replacement"))
         (replacement-file (file-name-concat replacement-dir "SKILL.md"))
         (workspace (niwa-workspace--create
                     :type 'test :id root :root root
                     :name "resource-skill-alias"))
         (skill (niwa-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir :description "A project skill"))
         (replacement (niwa-skill--create
                       :name "demo" :raw-name "demo" :source 'project
                       :source-family 'agents :source-file replacement-file
                       :source-dir replacement-dir))
         (session (niwa-session--create
                   :workspace workspace :skills (list skill)))
         (alias "skill://local-agents/demo")
         (exact (format "skill://demo@%s"
                        (niwa-resource-skill-digest skill-file))))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (make-directory replacement-dir t)
          (make-directory (file-name-concat skill-dir "templates") t)
          (with-temp-file skill-file (insert "original\n"))
          (with-temp-file (file-name-concat skill-dir "templates" "prompt.tmpl")
            (insert "prompt\n"))
          (with-temp-file replacement-file (insert "replacement\n"))
          (let* ((listing
                  (niwa-resource-execute
                   (niwa-resource-prepare
                    'read "skill://" (list :session session))))
                 (attempt
                  (niwa-resource-prepare
                   'read alias (list :session session)))
                 (data (gethash attempt niwa-resource--attempt-table))
                 seen)
            (should (string-match-p (regexp-quote alias)
                                    (plist-get listing :result)))
            (should (string-match-p (regexp-quote exact)
                                    (plist-get listing :result)))
            (should (equal exact (plist-get data :exact-address)))
            (niwa-resource-execute
             attempt
             (lambda (path authored)
               (setq seen (list path authored))))
            (should (equal (list skill-file alias) seen)))
          (let* ((descendant (concat alias "/templates/prompt.tmpl"))
                 (attempt (niwa-resource-prepare
                           'read descendant (list :session session)))
                 (data (gethash attempt niwa-resource--attempt-table)))
            (should (equal (concat exact "/templates/prompt.tmpl")
                           (plist-get data :exact-address)))
            (niwa-resource-execute attempt (lambda (_path _authored) t)))
          (let ((attempt
                 (niwa-resource-prepare
                  'read alias (list :session session))))
            (delete-file skill-file)
            (should-error (niwa-resource-execute attempt)
                          :type 'niwa-resource-unavailable)
            (with-temp-file skill-file (insert "original\n")))
          (let ((attempt
                 (niwa-resource-prepare
                  'read alias (list :session session))))
            (setf (niwa-session-skills session) (list replacement))
            (should-error (niwa-resource-execute attempt)
                          :type 'niwa-resource-unavailable)))
      (delete-directory root t)))

  :doc "rejects unknown and ambiguous readable aliases during preparation"
  (let* ((first (niwa-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file "/tmp/first/SKILL.md"
                 :source-dir "/tmp/first"))
         (second (niwa-skill--create
                  :name "agents:demo" :raw-name "demo" :source 'project
                  :source-family 'agents :source-file "/tmp/second/SKILL.md"
                  :source-dir "/tmp/second"))
         (session (niwa-session--create :skills (list first second))))
    (should-error
     (niwa-resource-prepare
      'read "skill://local-agents/missing" (list :session session))
     :type 'niwa-resource-error)
    (should-error
     (niwa-resource-prepare
      'read "skill://local-agents/demo" (list :session session))
     :type 'niwa-resource-error))

  :doc "resolves every supported readable origin alias to its own source"
  (let* ((root (make-temp-file "niwa-resource-skill-origins-" t))
         (specs '(("local-niwa" project niwa nil)
                  ("local-agents" project agents nil)
                  ("global-niwa" user niwa nil)
                  ("global-agents" user agents nil)
                  ("bundled" bundled nil nil)
                  ("managed" managed nil nil)
                  ("plugin/demo-plugin" plugin nil "demo-plugin")))
         skills)
    (unwind-protect
        (progn
          (dolist (spec specs)
            (let* ((label (car spec))
                   (dir (file-name-concat root label))
                   (source-file (file-name-concat dir "SKILL.md")))
              (make-directory dir t)
              (with-temp-file source-file (insert label "\n"))
              (push (niwa-skill--create
                     :name (concat label ":demo")
                     :raw-name "demo"
                     :plugin-name (nth 3 spec)
                     :source-file source-file
                     :source-dir dir
                     :source (nth 1 spec)
                     :source-family (nth 2 spec))
                    skills)))
          (let ((session (niwa-session--create :skills skills)))
            (dolist (spec specs)
              (let* ((alias (format "skill://%s/demo" (car spec)))
                     (expected (file-name-concat root (car spec) "SKILL.md"))
                     seen)
                (niwa-resource-execute
                 (niwa-resource-prepare
                  'read alias (list :session session))
                 (lambda (path authored)
                   (setq seen (list path authored))))
                (should (equal (list expected alias) seen))))))
      (delete-directory root t))))

(provide 'test-niwa-resource-skill)
;;; test-niwa-resource-skill.el ends here
