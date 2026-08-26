;;; test-mevedel-resource-capf.el --- Resource address completion tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-agent-control)
(require 'mevedel-resource)
(require 'mevedel-resource-capf)
(require 'mevedel-skills-core)
(require 'mevedel-system)
(require 'mcp-hub)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun mevedel-resource-capf-test--candidates (result)
  "Return plain candidates from CAPF RESULT."
  (when result
    (all-completions "" (nth 2 result))))

(defun mevedel-resource-capf-test--annotation (result candidate)
  "Return RESULT's annotation for CANDIDATE."
  (funcall (plist-get (nthcdr 3 result) :annotation-function) candidate))

(defun mevedel-resource-capf-test--session (save-path)
  "Return a throwaway session rooted at SAVE-PATH."
  (mevedel-session--create
   :authority-mode 'pid-lock
   :name "resource-capf"
   :save-path save-path
   :agent-registry nil
   :skills nil))


;;
;;; Completion

(mevedel-deftest mevedel-resource-capf-prefixes
  (:doc "offers the eight canonical scheme prefixes without a session")
  (with-temp-buffer
    (let ((mevedel--session nil))
      (insert "")
      (let ((result (mevedel-resource-capf)))
        (should (equal
                 '("agent://" "artifact://" "history://" "local://"
                   "mcp://" "memory://" "mevedel://" "skill://")
                 (sort (mevedel-resource-capf-test--candidates result)
                       #'string-lessp)))
        (dolist (candidate (mevedel-resource-capf-test--candidates result))
          (should-not (get-text-property 0 'mevedel-mention-binding
                                         candidate)))))))

(mevedel-deftest mevedel-resource-capf-paths
  (:doc "completes one bounded local or artifact directory level")
  (let* ((save-path (make-temp-file "mevedel-resource-capf-" t))
         (local-root (file-name-concat save-path "local"))
         (artifact-root (file-name-concat save-path "tool-results"))
         (session (mevedel-resource-capf-test--session save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat local-root "nested") t)
          (make-directory (file-name-concat artifact-root "batch") t)
          (with-temp-file (file-name-concat local-root "space name.md")
            (insert "local"))
          (with-temp-file (file-name-concat local-root "nested" "note.md")
            (insert "nested"))
          (with-temp-file (file-name-concat artifact-root "batch" "result.txt")
            (insert "artifact"))
          (make-directory (file-name-concat local-root "plans") t)
          (with-temp-file
              (file-name-concat local-root "plans" "current.md")
            (insert "# Managed plan"))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert "local://")
            (let ((candidates
                   (mevedel-resource-capf-test--candidates
                    (mevedel-resource-capf))))
              (should (member "local://space%20name.md" candidates))
              (should (member "local://nested" candidates))
              (should-not (member "local://nested/note.md" candidates))))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert "local://plans/")
            (should
             (member "local://plans/current.md"
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf)))))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert "local://nested/")
            (should
             (member "local://nested/note.md"
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf)))))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert "artifact://batch/")
            (should
             (member "artifact://batch/result.txt"
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf)))))
          (let ((remote-session
                 (mevedel-resource-capf-test--session
                  "/ssh:example.invalid:/tmp/resource-capf")))
            (cl-letf (((symbol-function 'file-directory-p)
                       (lambda (path)
                         (when (file-remote-p path)
                           (error "Remote root was probed"))
                         nil))
                      ((symbol-function 'directory-files)
                       (lambda (&rest _)
                         (error "Remote directory was listed"))))
              (with-temp-buffer
                (setq mevedel--session remote-session)
                (insert "local://")
                (should-not (mevedel-resource-capf))))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource-capf-skills
  (:doc "stages readable skill aliases before exact bounded candidates")
  (let* ((root (make-temp-file "mevedel-resource-capf-skill-" t))
         (skill-dir (file-name-concat root "alpha"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (plugin-dir (file-name-concat root "beta"))
         (plugin-file (file-name-concat plugin-dir "SKILL.md"))
         (session (mevedel-resource-capf-test--session nil))
         (skill (mevedel-skill--create
                 :name "alpha" :raw-name "alpha"
                 :source-file skill-file :source-dir skill-dir
                 :source 'project :source-family 'mevedel
                 :description "Alpha skill"))
         (plugin-skill (mevedel-skill--create
                        :name "demo:beta" :raw-name "beta"
                        :plugin-name "demo" :source-file plugin-file
                        :source-dir plugin-dir :source 'plugin
                        :description "Beta skill")))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (make-directory plugin-dir t)
          (with-temp-file skill-file (insert "skill"))
          (with-temp-file (file-name-concat skill-dir "template.txt")
            (insert "template"))
          (make-directory (file-name-concat skill-dir "nested"))
          (with-temp-file (file-name-concat skill-dir "nested" "deep.txt")
            (insert "deep"))
          (with-temp-file plugin-file (insert "skill"))
          (with-temp-file (file-name-concat plugin-dir "prompt.md")
            (insert "prompt"))
          (setf (mevedel-session-skills session) (list skill plugin-skill))
          (let ((exact (format "skill://alpha@%s"
                               (mevedel-resource-skill-digest skill-file)))
                (plugin-exact
                 (format "skill://demo%%3Abeta@%s"
                         (mevedel-resource-skill-digest plugin-file))))
            (cl-letf (((symbol-function 'mevedel-skill-load-body)
                       (lambda (&rest _)
                         (error "Skill body must not be loaded")))
                      ((symbol-function 'mevedel-mention-bindings-set)
                       (lambda (&rest _)
                         (error "Mention binding must not be created"))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://")
                (let* ((result (mevedel-resource-capf))
                       (candidates
                        (mevedel-resource-capf-test--candidates result)))
                  (should (member "skill://local-mevedel/" candidates))
                  (should (member "skill://plugin/" candidates))
                  ;; A unique readable alias covers the skill, so the
                  ;; hashed locator is not offered beside it.
                  (should-not (member exact candidates))
                  (should-not (member plugin-exact candidates))
                  (dolist (candidate candidates)
                    (should-not (get-text-property
                                 0 'mevedel-mention-binding candidate)))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://alpha@")
                (let ((result (mevedel-resource-capf)))
                  (should (member exact
                                  (mevedel-resource-capf-test--candidates
                                   result)))
                  (should (string-match-p
                           "\\[exact\\]"
                           (mevedel-resource-capf-test--annotation
                            result exact)))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://local-mevedel/")
                (let ((result (mevedel-resource-capf)))
                  (should
                   (member "skill://local-mevedel/alpha"
                           (mevedel-resource-capf-test--candidates result)))
                  (should
                   (string-match-p
                    "\\[local-mevedel\\].*Alpha skill"
                    (mevedel-resource-capf-test--annotation
                     result "skill://local-mevedel/alpha")))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://plugin/")
                (should
                 (equal '("skill://plugin/demo/")
                        (mevedel-resource-capf-test--candidates
                         (mevedel-resource-capf)))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://plugin/demo/")
                (should
                 (equal '("skill://plugin/demo/beta")
                        (mevedel-resource-capf-test--candidates
                         (mevedel-resource-capf)))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://local-mevedel/alpha/")
                (let* ((result (mevedel-resource-capf))
                       (candidates
                        (mevedel-resource-capf-test--candidates result)))
                  (should
                   (member "skill://local-mevedel/alpha/template.txt"
                           candidates))
                  (should
                   (string-match-p
                    "\\[local-mevedel\\].*Alpha skill"
                    (mevedel-resource-capf-test--annotation
                     result "skill://local-mevedel/alpha/template.txt")))
                  (should (member "skill://local-mevedel/alpha/nested"
                                  candidates))
                  (should-not
                   (member "skill://local-mevedel/alpha/nested/deep.txt"
                           candidates))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert "skill://plugin/demo/beta/")
                (should
                 (member "skill://plugin/demo/beta/prompt.md"
                         (mevedel-resource-capf-test--candidates
                          (mevedel-resource-capf)))))
              (with-temp-buffer
                (setq mevedel--session session)
                (insert (concat exact "/"))
                (should
                 (member (concat exact "/template.txt")
                         (mevedel-resource-capf-test--candidates
                          (mevedel-resource-capf))))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-capf-mevedel
  (:doc "completes documentation one level using directories and Markdown")
  (let ((root (make-temp-file "mevedel-resource-capf-docs-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "nested"))
          (with-temp-file (file-name-concat root "guide.md")
            (insert "guide"))
          (with-temp-file (file-name-concat root "notes.txt")
            (insert "notes"))
          (with-temp-file (file-name-concat root "image.png")
            (insert "image"))
          (with-temp-file (file-name-concat root "upper.MD")
            (insert "upper"))
          (with-temp-file (file-name-concat root "nested" "deep.md")
            (insert "deep"))
          (cl-letf (((symbol-function 'mevedel-resource--root)
                     (lambda (scheme _session)
                       (and (eq scheme 'mevedel) root))))
            (with-temp-buffer
              (insert "mevedel://")
              (let ((candidates
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf))))
                (should (member "mevedel://guide.md" candidates))
                (should (member "mevedel://nested" candidates))
                (should-not (member "mevedel://notes.txt" candidates))
                (should-not (member "mevedel://image.png" candidates))
                (should-not (member "mevedel://upper.MD" candidates))
                (should-not (member "mevedel://nested/deep.md" candidates))))
            (with-temp-buffer
              (insert "mevedel://nested/")
              (should
               (equal '("mevedel://nested/deep.md")
                      (mevedel-resource-capf-test--candidates
                       (mevedel-resource-capf)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-capf-agents
  (:doc "completes agents while limiting history to retained conversations")
  (let* ((session (mevedel-resource-capf-test--session nil))
         (record
          (mevedel-agent-record--create
           :path "/root/reviewer" :role "reviewer" :activity 'idle
           :conversation-location "agents/reviewer.chat.org")))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/reviewer" record)))
    (with-temp-buffer
      (setq mevedel--session session)
      (insert "agent://")
      (let ((candidates
             (mevedel-resource-capf-test--candidates
              (mevedel-resource-capf))))
        (should-not (member "agent://root" candidates))
        (should (member "agent://root/reviewer" candidates))))
    (with-temp-buffer
      (setq mevedel--session session)
      (insert "history://")
      (should (equal '("history://root/reviewer")
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf)))))))

(mevedel-deftest mevedel-resource-capf-memory
  (:doc "completes configured memory topics without recursive scans")
  (let* ((root (make-temp-file "mevedel-resource-capf-memory-" t))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root :name "memory-capf"))
         (session (mevedel-resource-capf-test--session nil))
         (remote-root "/ssh:example.invalid:/tmp/resource-memory")
         (mevedel-memory-dirs (list root remote-root))
         (key (mevedel-resource-memory-root-key
               (list :dir (file-name-as-directory root))))
         (file-truename-function (symbol-function 'file-truename)))
    (setf (mevedel-session-workspace session) workspace)
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "nested") t)
          (with-temp-file (file-name-concat root "topic.md")
            (insert "topic"))
          (with-temp-file (file-name-concat root "nested" "deep.md")
            (insert "deep"))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert "memory://")
            (cl-letf (((symbol-function 'file-truename)
                       (lambda (path &rest args)
                         (when (file-remote-p path)
                           (error "Remote memory root was canonicalized"))
                         (apply file-truename-function path args))))
              (let ((candidates
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf))))
                (should (member "memory://root" candidates))
                (should (member (format "memory://%s/topic.md" key)
                                candidates))
                (should (member (format "memory://%s/nested" key)
                                candidates))
                (should-not
                 (member (format "memory://%s/nested/deep.md" key)
                         candidates)))))
          (with-temp-buffer
            (setq mevedel--session session)
            (insert (format "memory://%s/nested/" key))
            (should
             (member (format "memory://%s/nested/deep.md" key)
                     (mevedel-resource-capf-test--candidates
                      (mevedel-resource-capf)))))
          ;; A configured `.mevedel' root has a unique readable key, which
          ;; replaces its digest in completion.
          (let* ((relative (file-name-concat ".mevedel" "memory"))
                 (memory-dir (file-name-concat root relative))
                 (mevedel-memory-dirs (list relative)))
            (make-directory memory-dir t)
            (with-temp-file (file-name-concat memory-dir "MEMORY.md")
              (insert "index"))
            (with-temp-buffer
              (setq mevedel--session session)
              (insert "memory://local-mevedel/")
              (should
               (member "memory://local-mevedel/MEMORY.md"
                       (mevedel-resource-capf-test--candidates
                        (mevedel-resource-capf)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-capf-mcp
  (:doc "uses configured MCP metadata without reading resources")
  (let ((servers
         '((:name "docs" :status connected
                  :resources ((:uri "file:///guide" :name "Guide")))))
        (session (mevedel-resource-capf-test--session nil)))
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () servers))
              ((symbol-function 'mcp-read-resource)
               (lambda (&rest _) (error "MCP content must not be read"))))
      (with-temp-buffer
        (setq mevedel--session session)
        (insert "mcp://")
        (should
         (member "mcp://docs"
                 (mevedel-resource-capf-test--candidates
                  (mevedel-resource-capf))))
        (erase-buffer)
        (insert "mcp://docs/")
        (should
         (member "mcp://docs/file%3A%2F%2F%2Fguide"
                 (mevedel-resource-capf-test--candidates
                  (mevedel-resource-capf))))))))

(mevedel-deftest mevedel-resource-capf-no-session
  (:doc "does not materialize a session while completing local addresses")
  (let ((mevedel--session nil)
        (session-create-called nil))
    (cl-letf (((symbol-function 'mevedel-session-create)
               (lambda (&rest _)
                 (setq session-create-called t)
                 (error "resource completion must not create a session"))))
      (with-temp-buffer
        (insert "local://")
        (should-not (mevedel-resource-capf))
        (should-not session-create-called)
        (should (equal "local://" (buffer-string)))))))

(provide 'test-mevedel-resource-capf)
;;; test-mevedel-resource-capf.el ends here
