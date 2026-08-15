;;; test-mevedel-resource.el --- Tests for resource addresses -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mcp)
(require 'mcp-hub)
(require 'mevedel-structs)
(require 'mevedel-plan)
(require 'mevedel-resource)
(require 'mevedel-agent-control)
(require 'mevedel-agent-persistence)
(require 'mevedel-agents)
(require 'mevedel-tools)
(require 'mevedel-skills-core)
(require 'mevedel-system)
(require 'mevedel-mentions)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-resource-parse-address ()
  ,test
  (test)
  :doc "parses a canonical local address as a session-relative locator"
  (let ((parsed (mevedel-resource-parse-address "local://notes%20one.md")))
    (should (eq 'local (plist-get parsed :scheme)))
    (should (equal '("notes one.md")
                   (plist-get parsed :components)))
    (should (equal "local://notes%20one.md"
                   (plist-get parsed :canonical)))
    (should (eq 'session-relative (plist-get parsed :locator-class)))
    (should-not (plist-get parsed :dynamic-p)))
  :doc "classifies a bare local address as dynamic discovery"
  (let ((parsed (mevedel-resource-parse-address "local://")))
    (should (eq 'local (plist-get parsed :scheme)))
    (should-not (plist-get parsed :components))
    (should (equal "local://" (plist-get parsed :canonical)))
    (should (eq 'dynamic (plist-get parsed :locator-class)))
    (should (plist-get parsed :dynamic-p)))
  :doc "rejects malformed and noncanonical path components"
  (dolist (address '("local://a//b" "local://a/../b" "local://a/./b"
                     "local://a%2fb" "local://a%2Fb" "local://a%2eb"
                     "local://a%2Eb" "local://a%ZZ" "local:///a"
                     "local://a#fragment"))
    (should-error (mevedel-resource-parse-address address)))
  :doc "rejects unknown scheme URLs instead of treating them as paths"
  (should-error (mevedel-resource-parse-address "https://example.test/a")))

(mevedel-deftest mevedel-resource-encode-component ()
  ,test
  (test)
  :doc "encodes UTF-8 bytes with uppercase RFC 3986 escapes"
  (should (equal "space%20and%2F%C3%A4%25"
                 (mevedel-resource-encode-component "space and/ä%")))
  :doc "leaves only unreserved bytes literal"
  (should (equal "AZaz09-._~"
                 (mevedel-resource-encode-component "AZaz09-._~"))))

(mevedel-deftest mevedel-resource-locator-class ()
  ,test
  (test)
  :doc "recognizes supported schemes and ordinary native paths"
  (dolist (scheme '(local artifact skill agent history memory mcp))
    (should (mevedel-resource-supported-scheme-p scheme)))
  (should-not (mevedel-resource-address-p "ordinary/path:with-colon"))
  (should (mevedel-resource-address-p "artifact://result.txt")))

(mevedel-deftest mevedel-resource-scheme-grammar ()
  ,test
  (test)
  :doc "requires scheme-specific roots and canonical identities"
  (let ((digest
         "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
    (should (equal 'dynamic
                   (plist-get (mevedel-resource-parse-address "mcp://")
                              :locator-class)))
    (should (equal 'dynamic
                   (plist-get (mevedel-resource-parse-address "memory://root")
                              :locator-class)))
    (should
     (equal "agent://root/reviewer#/findings/0/path"
            (plist-get
             (mevedel-resource-parse-address
              "agent://root/reviewer#/findings/0/path")
             :canonical)))
    (should (equal '("findings" "0" "path")
                   (plist-get
                    (mevedel-resource-parse-address
                     "agent://root/reviewer#/findings/0/path")
                    :pointer)))
    (dolist (address
             (list "memory://"
                   "memory://root/topic.md"
                   "agent://reviewer"
                   "agent://root"
                   "history://reviewer"
                   "history://root"
                   "agent://root/Reviewer"
                   "history://root/reviewer-name"
                   "skill://name@ABC"
                   (concat "skill://name@" (upcase digest))
                   "mcp://server/uri/extra"
                   "mcp://server/"
                   "agent://root/reviewer?query=1"
                   "local://notes?query=1"
                   "agent://root/reviewer#not-a-pointer"
                   "agent://root/reviewer#/%7E0"
                   "agent://root/reviewer#/bad~2escape"))
      (should-error (mevedel-resource-parse-address address))))
  :doc "decodes one encoded MCP URI component without splitting it"
  (let ((parsed (mevedel-resource-parse-address "mcp://server/a%2Fb%3Fx")))
    (should (equal '("server" "a/b?x")
                   (plist-get parsed :components)))
    (should (equal "mcp://server/a%2Fb%3Fx"
                   (plist-get parsed :canonical))))
  :doc "keeps empty agent JSON pointer distinct from no fragment"
  (let ((without (mevedel-resource-parse-address "agent://root/reviewer"))
        (empty (mevedel-resource-parse-address "agent://root/reviewer#")))
    (should-not (plist-get without :fragment-p))
    (should (plist-get empty :fragment-p))
    (should (equal "" (plist-get empty :fragment)))
    (should-not (plist-get empty :pointer))))

(mevedel-deftest mevedel-resource-prepare ()
  ,test
  (test)
  :doc "returns an opaque attempt whose execution keeps the authored address"
  (let* ((save-path (make-temp-file "mevedel-resource-session-" t))
         (local (file-name-concat save-path "local"))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (address "local://notes.md")
         path seen-address)
    (unwind-protect
        (progn
          (make-directory local t)
          (with-temp-file (file-name-concat local "notes.md")
            (insert "note"))
          (let ((attempt (mevedel-resource-prepare
                          'read address (list :session session))))
            (should (symbolp attempt))
            (should (equal address (mevedel-resource-attempt-address attempt)))
            (should
             (equal "note"
                    (mevedel-resource-execute
                     attempt
                     (lambda (physical authored)
                       (setq path physical
                             seen-address authored)
                       (with-temp-buffer
                         (insert-file-contents physical)
                         (buffer-string)))))))
          (should (equal address seen-address))
          (should (equal (file-name-concat local "notes.md") path)))
      (delete-directory save-path t)))
  :doc "rejects symlink escapes before the handler receives an attempt"
  (let* ((save-path (make-temp-file "mevedel-resource-session-" t))
         (outside (make-temp-file "mevedel-resource-outside-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (make-symbolic-link outside
                              (file-name-concat save-path "local" "escape"))
          (should-error
           (mevedel-resource-prepare
            'read "local://escape/missing.txt" (list :session session))))
      (delete-directory save-path t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-resource-artifact-address ()
  ,test
  (test)
  :doc "encodes artifact path components while retaining separators"
  (let* ((save-path (make-temp-file "mevedel-resource-artifact-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (directory (file-name-concat save-path "tool-results" "part one"))
         (path (file-name-concat directory "result.txt")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file path (insert "result"))
          (should (equal "artifact://part%20one/result.txt"
                         (mevedel-resource-artifact-address path session))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource-apply-patch-preparation ()
  ,test
  (test)
  :doc "prepares a new local ApplyPatch target without materializing its root"
  (let* ((save-path (make-temp-file "mevedel-resource-patch-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (address "local://notes/new.txt")
         (local-root (file-name-concat save-path "local"))
         (expected (file-name-concat local-root "notes" "new.txt")))
    (unwind-protect
        (let ((attempt (mevedel-resource-prepare
                        'apply-patch address (list :session session))))
          (should (symbolp attempt))
          (should-not (file-directory-p local-root))
          (should
           (equal expected
                  (mevedel-resource-execute
                   attempt
                   (lambda (path authored)
                     (should (equal address authored))
                     path)))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource-validation-errors ()
  ,test
  (test)
  :doc "includes the authored address without exposing session storage"
  (let* ((save-path (make-temp-file "mevedel-resource-validation-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (address "local://notes/../bad.txt")
         message)
    (unwind-protect
        (condition-case err
            (progn
              (mevedel-resource-prepare
               'read address (list :session session))
              (error "Expected resource validation to fail"))
          (mevedel-resource-error
           (setq message (error-message-string err))))
      (delete-directory save-path t))
    (should (string-match-p (regexp-quote address) message))
    (should-not (string-match-p (regexp-quote save-path) message))))

(mevedel-deftest mevedel-resource-containment-and-lifecycle
  (:doc "keeps symlink escapes and pending execution spools out of resources")
  ,test
  (test)
  (let* ((save-path (make-temp-file "mevedel-resource-lifecycle-" t))
         (outside (make-temp-file "mevedel-resource-outside-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (local-root (file-name-concat save-path "local"))
         (artifact-root (file-name-concat save-path "tool-results"))
         (pending (file-name-concat artifact-root
                                    ".mevedel-pending-executions"))
         (address "local://note.md")
         physical
         renamed-save)
    (unwind-protect
        (progn
          (make-directory local-root t)
          (make-directory pending t)
          (with-temp-file (file-name-concat local-root "note.md")
            (insert "inside"))
          (with-temp-file (file-name-concat artifact-root "published.log")
            (insert "published"))
          (with-temp-file (file-name-concat pending "hidden.log")
            (insert "hidden"))
          (make-symbolic-link outside (file-name-concat local-root "escape"))
          (let ((listing
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "local://" (list :session session))))
                (artifacts
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "artifact://" (list :session session)))))
            (should (string-match-p "local://note.md"
                                    (plist-get listing :result)))
            (should-not (string-match-p "escape" (plist-get listing :result)))
            (should (string-match-p "artifact://published.log"
                                    (plist-get artifacts :result)))
            (should-not (string-match-p "hidden.log"
                                        (plist-get artifacts :result))))
          (should-error
           (mevedel-resource-execute
            (mevedel-resource-prepare
             'read "artifact://.mevedel-pending-executions/hidden.log"
             (list :session session)))))
          (let ((attempt (mevedel-resource-prepare
                          'read address (list :session session))))
            (mevedel-resource-execute
             attempt
             (lambda (path _authored)
               (setq physical path)))
            (should (equal (file-name-concat local-root "note.md") physical))
            (should-not (mevedel-resource-attempt-address attempt)))
          (setq renamed-save
                (make-temp-file "mevedel-resource-renamed-" t))
          (make-directory (file-name-concat renamed-save "local") t)
          (with-temp-file (file-name-concat renamed-save "local" "note.md")
            (insert "renamed"))
          (setf (mevedel-session-save-path session) renamed-save)
          (let ((attempt (mevedel-resource-prepare
                          'read address (list :session session))))
            (mevedel-resource-execute
             attempt
             (lambda (path _authored)
               (setq physical path)))
            (should (equal (file-name-concat renamed-save "local" "note.md")
                           physical)))
          (setf (mevedel-session-save-path session) save-path)
          (let ((attempt (mevedel-resource-prepare
                          'read address (list :session session))))
            (delete-file (file-name-concat local-root "note.md"))
            (make-symbolic-link outside (file-name-concat local-root "note.md"))
            (should-error
             (mevedel-resource-execute
              attempt
              (lambda (&rest _)
                (error "Resource freshness check was bypassed"))))))
      (delete-directory save-path t)
      (when renamed-save
        (delete-directory renamed-save t))
      (delete-directory outside t)))

(mevedel-deftest mevedel-resource-local-artifact-provider ()
  ,test
  (test)
  :doc "lists local and artifact roots through logical addresses"
  (let* ((save-path (make-temp-file "mevedel-resource-provider-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (local-root (file-name-concat save-path "local"))
         (artifact-root (file-name-concat save-path "tool-results")))
    (unwind-protect
        (progn
          (make-directory local-root t)
          (make-directory artifact-root t)
          (with-temp-file (file-name-concat local-root "notes.md")
            (insert "needle"))
          (with-temp-file (file-name-concat artifact-root "answer.txt")
            (insert "artifact"))
          (with-temp-buffer
            (mevedel-plan-write-current
             "# Managed plan" session (current-buffer)))
          (let ((local-read
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "local://" (list :session session))))
                (artifact-read
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "artifact://" (list :session session))))
                (grep-path nil))
            (should (string-match-p "local://notes.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "local://plans/current.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "artifact://answer.txt"
                                    (plist-get artifact-read :result)))
            (mevedel-resource-execute
             (mevedel-resource-prepare 'grep "local://"
                                        (list :session session))
             (lambda (path authored)
               (setq grep-path (list path authored))))
            (should (equal (list local-root "local://") grep-path))
            ;; A bare listing address is never a patch endpoint.
            (should-error
             (mevedel-resource-prepare
              'apply-patch "local://" (list :session session))
             :type 'mevedel-resource-error)))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource--session ()
  ,test
  (test)
  :doc "resolves one shared local root for the parent and its retained agents"
  (let* ((save-path (make-temp-file "mevedel-resource-shared-" t))
         (local-root (file-name-concat save-path "local"))
         (parent-session (mevedel-session--create :name "parent"
                                                  :save-path save-path))
         (agent-buffer (generate-new-buffer " *mevedel-resource-agent*"))
         parent-path agent-path)
    (unwind-protect
        (progn
          (make-directory local-root t)
          (with-temp-file (file-name-concat local-root "shared.md")
            (insert "shared note"))
          (mevedel-resource-execute
           (mevedel-resource-prepare
            'read "local://shared.md" (list :session parent-session))
           (lambda (physical _authored) (setq parent-path physical)))
          ;; A retained agent conversation buffer owns the parent session, so
          ;; its context resolves the same physical root.
          (with-current-buffer agent-buffer
            (setq-local mevedel--session parent-session)
            (mevedel-resource-execute
             (mevedel-resource-prepare 'read "local://shared.md" nil)
             (lambda (physical _authored) (setq agent-path physical))))
          (should (equal (file-name-concat local-root "shared.md")
                         parent-path))
          (should (equal parent-path agent-path)))
      (kill-buffer agent-buffer)
      (delete-directory save-path t))))

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

(mevedel-deftest mevedel-resource-agent-provider ()
  ,test
  (test)
  :doc "reads settled agent output and applies RFC 6901 extraction"
  (let* ((payload "{\"b\":2,\"findings\":[{\"path\":\"a/b\",\"ok\":null}],\"a\":1}")
         (record (mevedel-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result payload :settled-outcome 'completed))
         (session (mevedel-session--create)))
    (mevedel-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (let* ((exact
            (mevedel-resource-execute
             (mevedel-resource-prepare
              'read "agent://root/reviewer" (list :session session))))
           (selected
            (mevedel-resource-execute
             (mevedel-resource-prepare
              'read "agent://root/reviewer#/findings/0/path"
              (list :session session))))
           (null-value
            (mevedel-resource-execute
             (mevedel-resource-prepare
              'read "agent://root/reviewer#/findings/0/ok"
              (list :session session)))))
      (should (equal payload (plist-get exact :result)))
      (should (equal "a/b" (plist-get selected :result)))
      (should (equal "null" (plist-get null-value :result))))
    (should-error
     (mevedel-resource-execute
      (mevedel-resource-prepare
       'read "agent://root/reviewer#/findings/1/path"
       (list :session session)))))
  :doc "lists retained agent paths with readiness"
  (let* ((record (mevedel-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "done" :settled-outcome 'completed))
         (session (mevedel-session--create)))
    (mevedel-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (let ((result
           (mevedel-resource-execute
            (mevedel-resource-prepare
             'read "agent://" (list :session session)))))
      (should (string-match-p "agent://root/reviewer"
                              (plist-get result :result)))
      (should-not (string-match-p "\\`agent://root\t"
                                  (plist-get result :result)))
      (should (string-match-p "ready" (plist-get result :result))))))
  :doc "refreshes an unavailable agent when its record appears before execution"
  (let* ((session (mevedel-session--create))
         (attempt (mevedel-resource-prepare
                   'read "agent://root/reviewer" (list :session session)))
         (record (mevedel-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "now available" :settled-outcome 'completed)))
    (should (plist-get (gethash attempt mevedel-resource--attempt-table)
                       :unavailable-p))
    (mevedel-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (should (equal "now available"
                   (plist-get (mevedel-resource-execute attempt) :result))))

(mevedel-deftest mevedel-resource-history-provider ()
  ,test
  (test)
  :doc "lists and projects a live retained agent conversation"
  (let ((buffer (generate-new-buffer " *mevedel-resource-history*")))
    (unwind-protect
        (let* ((record (mevedel-agent-record--create
                        :path "/root/reviewer" :role "reviewer"
                        :activity 'idle :conversation-buffer buffer))
               (session (mevedel-session--create)))
          (with-current-buffer buffer
            (org-mode)
            (insert "A user request\n")
            (let ((start (point)))
              (insert "An assistant decision\n"
                      "[media: image; MIME image/png; path /private/raw.png]\n")
              (put-text-property start (point) 'gptel 'response)))
          (mevedel-session--set-agent-registry
           session (list (cons "/root/reviewer" record)))
          (let ((listing
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "history://" (list :session session))))
                (history
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "history://root/reviewer"
                   (list :session session)))))
            (should (string-match-p "history://root/reviewer"
                                    (plist-get listing :result)))
            (should (string-match-p "An assistant decision"
                                    (plist-get history :result)))
            (should-not (string-match-p "path /private/raw.png"
                                        (plist-get history :result)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-resource-history-provider-cold-hydration ()
  ,test
  (test)
  :doc "hydrates a persisted retained conversation when no live buffer exists"
  (let* ((root (make-temp-file "mevedel-resource-cold-history-" t))
         (agents-dir (file-name-concat root "agents"))
         (conversation (file-name-concat agents-dir "reviewer.chat.org"))
         (agent (mevedel-agent--create
                 :name "default" :description "Cold test agent"
                 :tools nil :system-prompt "Frozen instructions"
                 :max-turns nil :hook-rules nil :frozen-p t))
         (configuration
          (mevedel-agent-configuration--create
           :agent agent :request-locals nil))
         (record (mevedel-agent-record--create
                  :id "cold-reviewer" :path "/root/reviewer"
                  :parent-path "/root" :role "reviewer"
                  :configuration configuration :activity 'idle
                  :conversation-location "agents/reviewer.chat.org"))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path root))
         (root-buffer (generate-new-buffer " *mevedel-resource-cold-root*")))
    (unwind-protect
        (progn
          (make-directory agents-dir t)
          (with-temp-file conversation
            (insert "Cold retained decision\n"))
          (mevedel-session--set-agent-registry
           session (list (cons "/root/reviewer" record)))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local mevedel--session session))
          (let ((result
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "history://root/reviewer"
                   (list :session session)))))
            (should (string-match-p "Cold retained decision"
                                    (plist-get result :result)))
            (should (buffer-live-p
                     (mevedel-agent-record-conversation-buffer record)))))
      (mevedel-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-memory-provider ()
  ,test
  (test)
  :doc "reads the system memory union and searches root-bound topics"
  (let* ((workspace-root (make-temp-file "mevedel-resource-memory-" t))
         (workspace (mevedel-workspace--create
                     :type 'test :id workspace-root :root workspace-root
                     :name "resource-memory"))
         (memory-dir (file-name-concat workspace-root ".mevedel" "memory"))
         (context (list :workspace workspace)))
    (unwind-protect
        (progn
          (make-directory memory-dir t)
          (with-temp-file (file-name-concat memory-dir "MEMORY.md")
            (insert "Index entry"))
          (with-temp-file (file-name-concat memory-dir "topic.md")
            (insert "remember this fact"))
          (let* ((mevedel-memory-dirs
                  (list (file-name-concat ".mevedel" "memory")))
                 (root (car (mevedel-system--memory-roots workspace)))
                 (key (mevedel-resource-memory-root-key root))
                 (topic (format "memory://%s/topic.md" key))
                 (index
                  (mevedel-resource-execute
                   (mevedel-resource-prepare
                    'read "memory://root" context)))
                 (glob
                  (mevedel-resource-execute
                   (mevedel-resource-prepare
                    'glob "memory://root" context)
                   nil '(:pattern "*.md")))
                 (read-topic
                  (mevedel-resource-execute
                   (mevedel-resource-prepare 'read topic context)
                   (lambda (path _address)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))))
            (should (string-match-p "Index entry" (plist-get index :result)))
            (should (equal
                     (concat "memory://" key)
                     (plist-get
                      (car (plist-get glob :resource-search-roots))
                      :address-prefix)))
            (should (equal "remember this fact" read-topic))))
      (delete-directory workspace-root t))))

(mevedel-deftest mevedel-resource-mcp-provider ()
  ,test
  (test)
  :doc "lists MCP metadata and reads fresh text through the mention seam"
  (let* ((server "srv/name")
         (uri "file:///a b?x#frag")
         (connection (make-symbol "mcp-connection"))
         (connections (make-hash-table :test #'equal))
         (servers (list (list :name server :status 'connected
                              :resources
                              (list (list :uri uri :name "Example")))))
         (calls 0)
         (address (format "mcp://%s/%s"
                          (mevedel-resource-encode-component server)
                          (mevedel-resource-encode-component uri))))
    (puthash server connection connections)
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () servers))
              ((symbol-function 'mcp-read-resource)
               (lambda (_connection _uri)
                 (cl-incf calls)
                 (list :contents
                       (vector (list :type "text" :text "one")
                               (list :type "text" :text "two"))))))
      (let ((mcp-server-connections connections))
        (let* ((listing
                (mevedel-resource-execute
                 (mevedel-resource-prepare 'read "mcp://" nil)))
               (resources
                (mevedel-resource-execute
                 (mevedel-resource-prepare
                  'read (format "mcp://%s"
                                (mevedel-resource-encode-component server))
                  nil)))
               (attempt (mevedel-resource-prepare 'read address nil)))
          (should (string-match-p "mcp://srv%2Fname"
                                  (plist-get listing :result)))
          (should (string-match-p (regexp-quote address)
                                  (plist-get resources :result)))
          (should (equal "one\ntwo"
                         (plist-get
                          (mevedel-resource-execute attempt) :result)))
          (let ((fresh (mevedel-resource-prepare 'read address nil)))
            (should (equal "one\ntwo"
                           (plist-get
                            (mevedel-resource-execute fresh) :result))))
          (should (= 2 calls))))))
  :doc "rejects unknown MCP servers without falling back to all-server listing"
  (cl-letf (((symbol-function 'mcp-hub-get-servers) (lambda () nil)))
    (should-error
     (mevedel-resource-execute
      (mevedel-resource-prepare 'read "mcp://unknown" nil)))))

(provide 'test-mevedel-resource)
;;; test-mevedel-resource.el ends here
