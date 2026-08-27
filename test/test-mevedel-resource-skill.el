;;; test-mevedel-resource-skill.el --- Skill resource provider tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for skill resource listing, aliases, and exact source resolution.

;;; Code:

(require 'mevedel-resource)
(require 'mevedel-skills-core)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-resource-skill-provider ()
  ,test
  (test)
  :doc "lists and reads an exact discovered skill source"
  (let* ((root (make-temp-file "mevedel-resource-skill-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root :name "resource-skill"))
         (skill (mevedel-skill--create
                 :name "demo" :description "A test skill"
                 :source-file skill-file :source-dir skill-dir))
         (session (mevedel-session--create
                   :workspace workspace :skills (list skill)))
         (digest (mevedel-resource-skill-digest skill-file))
         (address (format "skill://demo@%s" digest)))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file
            (insert "---\nname: demo\n---\nUse the demo skill.\n"))
          (make-directory (file-name-concat skill-dir "templates") t)
          (with-temp-file (file-name-concat skill-dir "templates" "prompt.tmpl")
            (insert "template needle\n"))
          (should-error
           (mevedel-resource-prepare 'glob "skill://"
                                     (list :session session)))
          (let* ((listing
                  (mevedel-resource-execute
                   (mevedel-resource-prepare
                    'read "skill://" (list :session session))))
                 (attempt (mevedel-resource-prepare
                           'read address (list :session session)))
                 (body
                  (mevedel-resource-execute
                   attempt
                   (lambda (path _address)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))))
            (should (string-match-p (regexp-quote address)
                                    (plist-get listing :result)))
            (should (string-match-p "Use the demo skill" body))))
          (let (glob-path grep-path)
            (mevedel-resource-execute
             (mevedel-resource-prepare 'glob address
                                       (list :session session))
             (lambda (path authored)
               (setq glob-path (list path authored))))
            (mevedel-resource-execute
             (mevedel-resource-prepare 'grep address
                                       (list :session session))
             (lambda (path authored)
               (setq grep-path (list path authored))))
            (should (equal (list skill-dir address) glob-path))
            (should (equal (list skill-dir address) grep-path))))
      (delete-directory root t)))

  :doc "disabled skills are absent from listings and reject exact resolution"
  (let* ((root (make-temp-file "mevedel-resource-disabled-skill-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root
                     :name "resource-disabled-skill"))
         (skill (mevedel-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir :enabled-p nil))
         (session (mevedel-session--create
                   :workspace workspace :skills (list skill)))
         (address (format "skill://demo@%s"
                          (mevedel-resource-skill-digest skill-file))))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file (insert "disabled\n"))
          (let ((listing
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "skill://" (list :session session)))))
            (should-not (string-match-p
                         (regexp-quote address)
                         (plist-get listing :result))))
          (should-error
           (mevedel-resource-execute
            (mevedel-resource-prepare
             'read address (list :session session)))
           :type 'mevedel-resource-unavailable)
          (let ((err
                 (should-error
                  (mevedel-resource-prepare
                   'read "skill://local-agents/demo"
                   (list :session session))
                  :type 'mevedel-resource-error)))
            (should (string-match-p
                     "Skill alias does not name an enabled skill"
                     (error-message-string err)))))
      (delete-directory root t)))

  :doc "resolves readable aliases once while preserving the authored address"
  (let* ((root (make-temp-file "mevedel-resource-skill-alias-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (replacement-dir (file-name-concat root "replacement"))
         (replacement-file (file-name-concat replacement-dir "SKILL.md"))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root
                     :name "resource-skill-alias"))
         (skill (mevedel-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir :description "A project skill"))
         (replacement (mevedel-skill--create
                       :name "demo" :raw-name "demo" :source 'project
                       :source-family 'agents :source-file replacement-file
                       :source-dir replacement-dir))
         (session (mevedel-session--create
                   :workspace workspace :skills (list skill)))
         (alias "skill://local-agents/demo")
         (exact (format "skill://demo@%s"
                        (mevedel-resource-skill-digest skill-file))))
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
                  (mevedel-resource-execute
                   (mevedel-resource-prepare
                    'read "skill://" (list :session session))))
                 (attempt
                  (mevedel-resource-prepare
                   'read alias (list :session session)))
                 (data (gethash attempt mevedel-resource--attempt-table))
                 seen)
            (should (string-match-p (regexp-quote alias)
                                    (plist-get listing :result)))
            (should (string-match-p (regexp-quote exact)
                                    (plist-get listing :result)))
            (should (equal exact (plist-get data :exact-address)))
            (mevedel-resource-execute
             attempt
             (lambda (path authored)
               (setq seen (list path authored))))
            (should (equal (list skill-file alias) seen)))
          (let* ((descendant (concat alias "/templates/prompt.tmpl"))
                 (attempt (mevedel-resource-prepare
                           'read descendant (list :session session)))
                 (data (gethash attempt mevedel-resource--attempt-table)))
            (should (equal (concat exact "/templates/prompt.tmpl")
                           (plist-get data :exact-address)))
            (mevedel-resource-execute attempt (lambda (_path _authored) t)))
          (let ((attempt
                 (mevedel-resource-prepare
                  'read alias (list :session session))))
            (delete-file skill-file)
            (should-error (mevedel-resource-execute attempt)
                          :type 'mevedel-resource-unavailable)
            (with-temp-file skill-file (insert "original\n")))
          (let ((attempt
                 (mevedel-resource-prepare
                  'read alias (list :session session))))
            (setf (mevedel-session-skills session) (list replacement))
            (should-error (mevedel-resource-execute attempt)
                          :type 'mevedel-resource-unavailable)))
      (delete-directory root t)))

  :doc "rejects unknown and ambiguous readable aliases during preparation"
  (let* ((first (mevedel-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file "/tmp/first/SKILL.md"
                 :source-dir "/tmp/first"))
         (second (mevedel-skill--create
                  :name "agents:demo" :raw-name "demo" :source 'project
                  :source-family 'agents :source-file "/tmp/second/SKILL.md"
                  :source-dir "/tmp/second"))
         (session (mevedel-session--create :skills (list first second))))
    (should-error
     (mevedel-resource-prepare
      'read "skill://local-agents/missing" (list :session session))
     :type 'mevedel-resource-error)
    (should-error
     (mevedel-resource-prepare
      'read "skill://local-agents/demo" (list :session session))
     :type 'mevedel-resource-error))

  :doc "resolves every supported readable origin alias to its own source"
  (let* ((root (make-temp-file "mevedel-resource-skill-origins-" t))
         (specs '(("local-mevedel" project mevedel nil)
                  ("local-agents" project agents nil)
                  ("global-mevedel" user mevedel nil)
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
              (push (mevedel-skill--create
                     :name (concat label ":demo")
                     :raw-name "demo"
                     :plugin-name (nth 3 spec)
                     :source-file source-file
                     :source-dir dir
                     :source (nth 1 spec)
                     :source-family (nth 2 spec))
                    skills)))
          (let ((session (mevedel-session--create :skills skills)))
            (dolist (spec specs)
              (let* ((alias (format "skill://%s/demo" (car spec)))
                     (expected (file-name-concat root (car spec) "SKILL.md"))
                     seen)
                (mevedel-resource-execute
                 (mevedel-resource-prepare
                  'read alias (list :session session))
                 (lambda (path authored)
                   (setq seen (list path authored))))
                (should (equal (list expected alias) seen))))))
      (delete-directory root t)))

(provide 'test-mevedel-resource-skill)
;;; test-mevedel-resource-skill.el ends here
