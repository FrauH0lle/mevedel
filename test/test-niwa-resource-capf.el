;;; test-niwa-resource-capf.el --- Resource address completion tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'niwa-structs)
(require 'niwa-agent-control)
(require 'niwa-resource)
(require 'niwa-resource-capf)
(require 'niwa-skills-core)
(require 'niwa-system)
(require 'mcp-hub)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun niwa-resource-capf-test--candidates (result)
  "Return plain candidates from CAPF RESULT."
  (when result
    (all-completions "" (nth 2 result))))

(defun niwa-resource-capf-test--annotation (result candidate)
  "Return RESULT's annotation for CANDIDATE."
  (funcall (plist-get (nthcdr 3 result) :annotation-function) candidate))

(defun niwa-resource-capf-test--session (save-path)
  "Return a throwaway session rooted at SAVE-PATH."
  (niwa-session--create
   :name "resource-capf"
   :save-path save-path
   :agent-registry nil
   :skills nil))


;;
;;; Completion

(niwa-deftest niwa-resource-capf-prefixes
  (:doc "offers the canonical scheme prefixes without a session")
  (with-temp-buffer
    (let ((niwa--session nil))
      (let ((result (niwa-resource-capf)))
        (should (equal
                 '("agent://" "artifact://" "history://"
                   "mcp://" "memory://" "niwa://" "notes://" "shared://" "skill://" "work://")
                 (sort (niwa-resource-capf-test--candidates result)
                       #'string-lessp)))
        (dolist (candidate (niwa-resource-capf-test--candidates result))
          (should-not (get-text-property 0 'niwa-mention-binding
                                         candidate)))))))

(niwa-deftest niwa-resource-capf-paths
  (:doc "completes one bounded local or artifact directory level")
  (let* ((save-path (make-temp-file "niwa-resource-capf-" t))
         (local-root (file-name-concat save-path "local"))
         (artifact-root (file-name-concat save-path "tool-results"))
         (session (niwa-resource-capf-test--session save-path)))
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
            (setq niwa--session session)
            (insert "work://")
            (let ((candidates
                   (niwa-resource-capf-test--candidates
                    (niwa-resource-capf))))
              (should (equal '("work://nested" "work://plans"
                               "work://space%20name.md")
                             candidates))))
          (with-temp-buffer
            (setq niwa--session session)
            (insert "work://plans/")
            (should
             (member "work://plans/current.md"
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf)))))
          (with-temp-buffer
            (setq niwa--session session)
            (insert "work://nested/")
            (should
             (member "work://nested/note.md"
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf)))))
          (with-temp-buffer
            (setq niwa--session session)
            (insert "artifact://batch/")
            (should
             (member "artifact://batch/result.txt"
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf)))))
          (let ((remote-session
                 (niwa-resource-capf-test--session
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
                (setq niwa--session remote-session)
                (insert "work://")
                (should-not (niwa-resource-capf))))))
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-capf-skills
  (:doc "stages readable skill aliases before exact bounded candidates")
  (let* ((root (make-temp-file "niwa-resource-capf-skill-" t))
         (skill-dir (file-name-concat root "alpha"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (plugin-dir (file-name-concat root "beta"))
         (plugin-file (file-name-concat plugin-dir "SKILL.md"))
         (session (niwa-resource-capf-test--session nil))
         (skill (niwa-skill--create
                 :name "alpha" :raw-name "alpha"
                 :source-file skill-file :source-dir skill-dir
                 :source 'project :source-family 'niwa
                 :description "Alpha skill"))
         (plugin-skill (niwa-skill--create
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
          (setf (niwa-session-skills session) (list skill plugin-skill))
          (let ((exact (format "skill://alpha@%s"
                               (niwa-resource-skill-digest skill-file)))
                (plugin-exact
                 (format "skill://demo%%3Abeta@%s"
                         (niwa-resource-skill-digest plugin-file))))
            (cl-letf (((symbol-function 'niwa-skill-load-body)
                       (lambda (&rest _)
                         (error "Skill body must not be loaded")))
                      ((symbol-function 'niwa-mention-bindings-set)
                       (lambda (&rest _)
                         (error "Mention binding must not be created"))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://")
                (let* ((result (niwa-resource-capf))
                       (candidates
                        (niwa-resource-capf-test--candidates result)))
                  (should (member "skill://local-niwa/" candidates))
                  (should (member "skill://plugin/" candidates))
                  ;; A unique readable alias covers the skill, so the
                  ;; hashed locator is not offered beside it.
                  (should-not (member exact candidates))
                  (should-not (member plugin-exact candidates))
                  (dolist (candidate candidates)
                    (should-not (get-text-property
                                 0 'niwa-mention-binding candidate)))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://alpha@")
                (let ((result (niwa-resource-capf)))
                  (should (member exact
                                  (niwa-resource-capf-test--candidates
                                   result)))
                  (should (string-match-p
                           "\\[exact\\]"
                           (niwa-resource-capf-test--annotation
                            result exact)))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://local-niwa/")
                (let ((result (niwa-resource-capf)))
                  (should
                   (member "skill://local-niwa/alpha"
                           (niwa-resource-capf-test--candidates result)))
                  (should
                   (string-match-p
                    "\\[local-niwa\\].*Alpha skill"
                    (niwa-resource-capf-test--annotation
                     result "skill://local-niwa/alpha")))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://plugin/")
                (should
                 (equal '("skill://plugin/demo/")
                        (niwa-resource-capf-test--candidates
                         (niwa-resource-capf)))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://plugin/demo/")
                (should
                 (equal '("skill://plugin/demo/beta")
                        (niwa-resource-capf-test--candidates
                         (niwa-resource-capf)))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://local-niwa/alpha/")
                (let* ((result (niwa-resource-capf))
                       (candidates
                        (niwa-resource-capf-test--candidates result)))
                  (should
                   (member "skill://local-niwa/alpha/template.txt"
                           candidates))
                  (should
                   (string-match-p
                    "\\[local-niwa\\].*Alpha skill"
                    (niwa-resource-capf-test--annotation
                     result "skill://local-niwa/alpha/template.txt")))
                  (should (member "skill://local-niwa/alpha/nested"
                                  candidates))
                  (should-not
                   (member "skill://local-niwa/alpha/nested/deep.txt"
                           candidates))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert "skill://plugin/demo/beta/")
                (should
                 (member "skill://plugin/demo/beta/prompt.md"
                         (niwa-resource-capf-test--candidates
                          (niwa-resource-capf)))))
              (with-temp-buffer
                (setq niwa--session session)
                (insert (concat exact "/"))
                (should
                 (member (concat exact "/template.txt")
                         (niwa-resource-capf-test--candidates
                          (niwa-resource-capf))))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-capf-niwa
  (:doc "completes documentation one level using directories and Markdown")
  (let ((root (make-temp-file "niwa-resource-capf-docs-" t)))
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
          (cl-letf (((symbol-function 'niwa-resource--root)
                     (lambda (scheme _session)
                       (and (eq scheme 'niwa) root))))
            (with-temp-buffer
              (insert "niwa://")
              (let ((candidates
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf))))
                (should (member "niwa://guide.md" candidates))
                (should (member "niwa://nested" candidates))
                (should-not (member "niwa://notes.txt" candidates))
                (should-not (member "niwa://image.png" candidates))
                (should-not (member "niwa://upper.MD" candidates))
                (should-not (member "niwa://nested/deep.md" candidates))))
            (with-temp-buffer
              (insert "niwa://nested/")
              (should
               (equal '("niwa://nested/deep.md")
                      (niwa-resource-capf-test--candidates
                       (niwa-resource-capf)))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-capf-agents
  ()
  ,test
  (test)
  :doc "completes agents while limiting history to available conversations"
  (let* ((session (niwa-resource-capf-test--session nil))
         (record
          (niwa-agent-record--create
           :path "/root/reviewer" :role "reviewer" :activity 'idle
           :conversation-location "agents/reviewer.chat.org")))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/reviewer" record)))
    (with-temp-buffer
      (setq niwa--session session)
      (insert "agent://")
      (let ((candidates
             (niwa-resource-capf-test--candidates
              (niwa-resource-capf))))
        (should-not (member "agent://root" candidates))
        (should (member "agent://root/reviewer" candidates))))
    (with-temp-buffer
      (setq niwa--session session)
      (insert "history://")
      (should (equal '("history://root/reviewer")
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf))))))
  :doc "offers root history without children or reading the root transcript"
  (let ((session (niwa-resource-capf-test--session nil)))
    (with-temp-buffer
      (let ((root (current-buffer)))
        (insert "Root transcript stays untouched\n")
        (niwa-session-set-root-buffer session root)
        (with-temp-buffer
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-agent-conversation-project-history)
                     (lambda (&rest _) (ert-fail "Completion read history"))))
            (insert "history://")
            (let ((result (niwa-resource-capf)))
              (should (equal '("history://root")
                             (niwa-resource-capf-test--candidates result)))
              (should (string-match-p
                       "ready"
                       (niwa-resource-capf-test--annotation
                        result "history://root"))))
            (erase-buffer)
            (insert "history://ro")
            (should (equal '("history://root")
                           (niwa-resource-capf-test--candidates
                            (niwa-resource-capf)))))
          (erase-buffer)
          (insert "agent://")
          (should-not (member "agent://root"
                              (niwa-resource-capf-test--candidates
                               (niwa-resource-capf)))))))
    (with-temp-buffer
      (setq-local niwa--session session)
      (insert "history://")
      (should-not (niwa-resource-capf-test--candidates
                   (niwa-resource-capf))))
    (should-not (niwa-session-save-path session)))
  :doc "offers saved history from workspace metadata without filesystem discovery"
  (let* ((root (make-temp-file "niwa-history-capf-" t))
         (session (niwa-resource-capf-test--session nil))
         (workspace (niwa-workspace--create :type 'project :root root)))
    (unwind-protect
        (progn
          (setf (niwa-session-workspace session) workspace)
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "history://sa")
            (cl-letf (((symbol-function 'directory-files)
                       (lambda (&rest _) (ert-fail "Completion enumerated saved storage")))
                      ((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest _) (ert-fail "Completion read saved storage"))))
              (should (equal '("history://saved")
                             (niwa-resource-capf-test--candidates
                              (niwa-resource-capf)))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-capf-memory
  (:doc "completes configured memory topics without recursive scans")
  (let* ((root (make-temp-file "niwa-resource-capf-memory-" t))
         (workspace (niwa-workspace--create
                     :type 'test :id root :root root :name "memory-capf"))
         (session (niwa-resource-capf-test--session nil))
         (remote-root "/ssh:example.invalid:/tmp/resource-memory")
         (niwa-memory-dirs (list root remote-root))
         (key (niwa-resource-memory-root-key
               (list :dir (file-name-as-directory root))))
         (file-truename-function (symbol-function 'file-truename)))
    (setf (niwa-session-workspace session) workspace)
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "nested") t)
          (with-temp-file (file-name-concat root "topic.md")
            (insert "topic"))
          (with-temp-file (file-name-concat root "nested" "deep.md")
            (insert "deep"))
          (with-temp-buffer
            (setq niwa--session session)
            (insert "memory://")
            (cl-letf (((symbol-function 'file-truename)
                       (lambda (path &rest args)
                         (when (file-remote-p path)
                           (error "Remote memory root was canonicalized"))
                         (apply file-truename-function path args))))
              (let ((candidates
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf))))
                (should (member "memory://root" candidates))
                (should (member (format "memory://%s/topic.md" key)
                                candidates))
                (should (member (format "memory://%s/nested" key)
                                candidates))
                (should-not
                 (member (format "memory://%s/nested/deep.md" key)
                         candidates)))))
          (with-temp-buffer
            (setq niwa--session session)
            (insert (format "memory://%s/nested/" key))
            (should
             (member (format "memory://%s/nested/deep.md" key)
                     (niwa-resource-capf-test--candidates
                      (niwa-resource-capf)))))
          ;; A configured `.niwa' root has a unique readable key, which
          ;; replaces its digest in completion.
          (let* ((relative (file-name-concat ".niwa" "memory"))
                 (memory-dir (file-name-concat root relative))
                 (niwa-memory-dirs (list relative)))
            (make-directory memory-dir t)
            (with-temp-file (file-name-concat memory-dir "MEMORY.md")
              (insert "index"))
            (with-temp-buffer
              (setq niwa--session session)
              (insert "memory://local-niwa/")
              (should
               (member "memory://local-niwa/MEMORY.md"
                       (niwa-resource-capf-test--candidates
                        (niwa-resource-capf)))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-capf-mcp
  (:doc "uses configured MCP metadata without reading resources")
  (let ((servers
         '((:name "docs" :status connected
                  :resources ((:uri "file:///guide" :name "Guide")))))
        (session (niwa-resource-capf-test--session nil)))
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () servers))
              ((symbol-function 'mcp-read-resource)
               (lambda (&rest _) (error "MCP content must not be read"))))
      (with-temp-buffer
        (setq niwa--session session)
        (insert "mcp://")
        (should
         (member "mcp://docs"
                 (niwa-resource-capf-test--candidates
                  (niwa-resource-capf))))
        (erase-buffer)
        (insert "mcp://docs/")
        (let ((result (niwa-resource-capf))
              (address "mcp://docs/file%3A%2F%2F%2Fguide"))
          (should (member address (niwa-resource-capf-test--candidates result)))
          (should (equal " [mcp] Guide (connected)"
                         (niwa-resource-capf-test--annotation result address))))))))

(niwa-deftest niwa-resource-capf-no-session
  (:doc "does not materialize a session while completing local addresses")
  (let ((niwa--session nil)
        (session-create-called nil))
    (cl-letf (((symbol-function 'niwa-session-create)
               (lambda (&rest _)
                 (setq session-create-called t)
                 (error "resource completion must not create a session"))))
      (with-temp-buffer
        (insert "work://")
        (should-not (niwa-resource-capf))
        (should-not session-create-called)
        (should (equal "work://" (buffer-string)))))))

(provide 'test-niwa-resource-capf)
;;; test-niwa-resource-capf.el ends here
