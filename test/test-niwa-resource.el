;;; test-niwa-resource.el --- Tests for resource addresses -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))
(require 'ert)
(require 'cl-lib)
(require 'mcp)
(require 'mcp-hub)
(require 'niwa-structs)
(require 'niwa-artifact-backend)
(require 'niwa-plan)
(require 'niwa-resource)
(require 'niwa-agent-control)
(require 'niwa-agent-persistence)
(require 'niwa-agents)
(require 'niwa-tools)
(require 'niwa-tool-fs)
(require 'niwa-tool-fs-read)
(require 'niwa-skills-core)
(require 'niwa-system)
(require 'niwa-mentions)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-resource-parse-address ()
  ,test
  (test)
  :doc "parses a canonical local address as a session-relative locator"
  (let ((parsed (niwa-resource-parse-address "work://notes%20one.md")))
    (should (eq 'work (plist-get parsed :scheme)))
    (should (equal '("notes one.md")
                   (plist-get parsed :components)))
    (should (equal "work://notes%20one.md"
                   (plist-get parsed :canonical)))
    (should (eq 'session-relative (plist-get parsed :locator-class)))
    (should-not (plist-get parsed :dynamic-p)))
  :doc "classifies a bare local address as dynamic discovery"
  (let ((parsed (niwa-resource-parse-address "work://")))
    (should (eq 'work (plist-get parsed :scheme)))
    (should-not (plist-get parsed :components))
    (should (equal "work://" (plist-get parsed :canonical)))
    (should (eq 'dynamic (plist-get parsed :locator-class)))
    (should (plist-get parsed :dynamic-p)))
  :doc "rejects malformed and unsafe path components"
  (dolist (address '("work://a//b" "work://a/../b" "work://a/./b"
                     "work://a%2fb" "work://a%2Fb" "work://a/%2e%2E/b"
                     "work://a%ZZ" "work:///a" "work://a#fragment"
                     "work://a//" "work:///" "work://a%0Ab"))
    (should-error (niwa-resource-parse-address address)))
  :doc "normalizes literal characters, escape case, and one trailing slash"
  (dolist (entry '(("work://shared/" . "work://shared")
                   ("work://notes one/" . "work://notes%20one")
                   ("work://a%2eb" . "work://a.b")
                   ("work://a%c3%a4" . "work://a%C3%A4")
                   ("work://\u00e4 b.md" . "work://%C3%A4%20b.md")
                   ("memory://journal" . "memory://journal/")
                   ("memory://journal/" . "memory://journal/")
                   ("history://root/" . "history://root")
                   ("history://%72oot" . "history://root")
                   ("history://%73aved/session" . "history://saved/session")
                   ("skill://local%2dniwa/review"
                    . "skill://local-niwa/review")
                   ("mcp://server/" . "mcp://server")
                   ("shared://library/System Design/sheet.png"
                    . "shared://library/System%20Design/sheet.png")))
    (should (equal (cdr entry)
                   (plist-get (niwa-resource-parse-address (car entry))
                              :canonical))))
  :doc "rejects invalid UTF-8 bytes in names and JSON pointer fragments"
  (dolist (bytes '("%FF" "%80" "%C0%AF" "%C3" "%ED%A0%80"
                   "%F4%90%80%80"))
    (dolist (prefix '("work://" "mcp://server/" "agent://root/review#/"))
      (should-error (niwa-resource-parse-address (concat prefix bytes))
                    :type 'niwa-resource-error)))
  :doc "rejects unknown scheme URLs instead of treating them as paths"
  (should-error (niwa-resource-parse-address "https://example.test/a"))
  :doc "rejects an unknown scheme without interning its name"
  ;; A scheme name arrives in model tool arguments and is parsed before the
  ;; permission step runs, so an unknown one must leave no symbol behind:
  ;; Emacs never collects an interned symbol.
  (let* ((name "niwaunknownscheme")
         (address (concat name "://a")))
    (unwind-protect
        (progn
          ;; It must still look address-like, or an unknown scheme would be
          ;; expanded as a relative filesystem path instead of rejected.
          (should (niwa-resource-address-like-p address))
          (should-error (niwa-resource-parse-address address))
          (should-not (intern-soft name)))
      (unintern name obarray)))
  :doc "rejects a scheme named after a falsy symbol"
  ;; `nil' interns to a symbol that is itself false, so interning the prefix
  ;; made this address look like no address at all, and it was expanded as a
  ;; relative path instead of rejected.
  (progn
    (should (niwa-resource-address-like-p "nil://a"))
    (should-error (niwa-resource-parse-address "nil://a"))))

(niwa-deftest niwa-resource-encode-component ()
  ,test
  (test)
  :doc "encodes UTF-8 bytes with uppercase RFC 3986 escapes"
  (should (equal "space%20and%2F%C3%A4%25"
                 (niwa-resource-encode-component "space and/ä%")))
  :doc "leaves only unreserved bytes literal"
  (should (equal "AZaz09-._~"
                 (niwa-resource-encode-component "AZaz09-._~"))))

(niwa-deftest niwa-resource-supported-scheme-p
  (:doc "answers a scheme name with the scheme it names")
  (progn
    (should (eq 'work (niwa-resource-supported-scheme-p "WORK")))
    (should (eq 'work (niwa-resource-supported-scheme-p 'work)))
    (should-not (niwa-resource-supported-scheme-p "https"))))

(niwa-deftest niwa-resource-locator-class ()
  ,test
  (test)
  :doc "recognizes supported schemes and ordinary native paths"
  (dolist (scheme '(work artifact skill agent history memory mcp niwa))
    (should (niwa-resource-supported-scheme-p scheme)))
  (should-not (niwa-resource-address-p "ordinary/path:with-colon"))
  (should (niwa-resource-address-p "artifact://result.txt")))

(niwa-deftest niwa-resource-scheme-grammar ()
  ,test
  (test)
  :doc "requires scheme-specific roots and canonical identities"
  (let ((digest
         "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
    (should (equal 'dynamic
                   (plist-get (niwa-resource-parse-address "mcp://")
                              :locator-class)))
    (should (equal 'dynamic
                   (plist-get (niwa-resource-parse-address "memory://root")
                              :locator-class)))
    (should
     (equal "agent://root/reviewer#/findings/0/path"
            (plist-get
             (niwa-resource-parse-address
              "agent://root/reviewer#/findings/0/path")
             :canonical)))
    (should (equal '("findings" "0" "path")
                   (plist-get
                    (niwa-resource-parse-address
                     "agent://root/reviewer#/findings/0/path")
                    :pointer)))
    (dolist (address
             (list "memory://"
                   "memory://root/topic.md"
                   "agent://reviewer"
                   "agent://root"
                   "history://reviewer"
                   "history://root#"
                   "history://root/../other"
                   "agent://root/Reviewer"
                   "history://root/reviewer.name"
                   "skill://name@ABC"
                   (concat "skill://name@" (upcase digest))
                   "mcp://server/uri/extra"
                   "agent://root/reviewer?query=1"
                   "work://notes?query=1"
                   "agent://root/reviewer#not-a-pointer"
                   "agent://root/reviewer#/%7E0"
                   "agent://root/reviewer#/bad~2escape"))
      (should-error (niwa-resource-parse-address address)))
    (dolist (fragment '("" "/" "/a~1b/~0/%C3%A4%25"))
      (let* ((address (concat "agent://root/reviewer#" fragment))
             (parsed (niwa-resource-parse-address address)))
        (should (equal address (plist-get parsed :canonical)))
        (should (= (length "root/reviewer") (plist-get parsed :fragment-p))))))
  :doc "accepts the root history as a session-relative read-only address"
  (let ((parsed (niwa-resource-parse-address "history://root")))
    (should (equal '("root") (plist-get parsed :components)))
    (should (equal "history://root" (plist-get parsed :canonical)))
    (should (eq 'session-relative (plist-get parsed :locator-class)))
    (should-not (plist-get parsed :dynamic-p))
    (dolist (operation '(glob apply-patch))
      (should-error (niwa-resource-prepare operation "history://root" nil)
                    :type 'niwa-resource-error)))
  :doc "decodes one encoded MCP URI component without splitting it"
  (let ((parsed (niwa-resource-parse-address "mcp://server/a%2Fb%3Fx")))
    (should (equal '("server" "a/b?x")
                   (plist-get parsed :components)))
    (should (equal "mcp://server/a%2Fb%3Fx"
                   (plist-get parsed :canonical))))
  :doc "keeps empty agent JSON pointer distinct from no fragment"
  (let ((without (niwa-resource-parse-address "agent://root/reviewer"))
        (empty (niwa-resource-parse-address "agent://root/reviewer#")))
    (should-not (plist-get without :fragment-p))
    (should (plist-get empty :fragment-p))
    (should (equal "" (plist-get empty :fragment)))
    (should-not (plist-get empty :pointer)))
  :doc "parses readable standard and plugin skill aliases"
  (let ((ordinary
         (niwa-resource-parse-address
          "skill://local-agents/demo/templates/prompt.tmpl"))
        (plugin
         (niwa-resource-parse-address
          "skill://plugin/superpowers/brainstorming/references/guide.md")))
    (should (eq 'alias (plist-get ordinary :locator-class)))
    (should (eq 'local-agents (plist-get ordinary :alias-source)))
    (should (equal "demo" (plist-get ordinary :raw-name)))
    (should (equal '("templates" "prompt.tmpl")
                   (plist-get ordinary :components)))
    (should (eq 'alias (plist-get plugin :locator-class)))
    (should (eq 'plugin (plist-get plugin :alias-source)))
    (should (equal "superpowers" (plist-get plugin :plugin-name)))
    (should (equal "brainstorming" (plist-get plugin :raw-name)))
    (should (equal '("references" "guide.md")
                   (plist-get plugin :components)))))

(niwa-deftest niwa-resource-completion-metadata ()
  ,test
  (test)
  :doc "loads only the provider selected by each completion scheme"
  (let* ((workspace (niwa-workspace--create
                     :type 'test :id "completion" :root default-directory
                     :name "completion"))
         (session (niwa-session--create
                   :save-path default-directory
                   :workspace workspace))
         (resource-root-function
          (symbol-function 'niwa-resource--root)))
    (dolist (scheme '(work artifact skill agent history memory mcp niwa))
      (cl-letf (((symbol-function 'niwa-resource--root)
                 (lambda (root-scheme owner)
                   (unless (eq root-scheme scheme)
                     (error "Unrelated path root ran"))
                   (funcall resource-root-function root-scheme owner)))
                ((symbol-function 'niwa-resource--skill-list)
                 (lambda (&rest _)
                   (error "Skill discovery ran during completion")))
                ((symbol-function 'niwa-agent-control-list-agents)
                 (lambda (&rest _)
                   (unless (memq scheme '(agent history))
                     (error "Unrelated agent provider ran"))))
                ((symbol-function 'niwa-resource--memory-roots)
                 (lambda (&rest _)
                   (unless (eq scheme 'memory)
                     (error "Unrelated memory provider ran"))))
                ((symbol-function 'mcp-hub-get-servers)
                 (lambda (&rest _)
                   (unless (eq scheme 'mcp)
                     (error "Unrelated MCP provider ran")))))
        (let ((metadata
               (niwa-resource-completion-metadata
                (list :session session) scheme)))
          (should
           (equal (mapcar #'car (plist-get metadata :roots))
                  (pcase scheme
                    ('work '(work))
                    ('artifact '(artifact))
                    ('niwa '(niwa)))))))))
  :doc "drops remote skill and memory roots before identity lookup"
  (let* ((remote "/ssh:example.invalid:/tmp/resource")
         (niwa-memory-dirs (list remote))
         (workspace (niwa-workspace--create
                     :type 'test :id "remote" :root remote
                     :name "remote"))
         (skill (niwa-skill--create
                 :name "remote" :source-file (concat remote "/SKILL.md")
                 :source-dir remote))
         (session (niwa-session--create
                   :skills (list skill)
                   :workspace workspace)))
    (cl-letf (((symbol-function 'niwa-skills-skill-enabled-p)
               (lambda (&rest _)
                 (error "Remote skill identity was inspected")))
              ((symbol-function 'niwa-skills-scan)
               (lambda (&rest _)
                 (error "Remote workspace skills were scanned")))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (error "Remote root was canonicalized"))))
      (should-not
       (plist-get
        (niwa-resource-completion-metadata
         (list :session session) 'skill)
        :skills))
      (setf (niwa-session-skills session) nil)
      (should-not
       (plist-get
        (niwa-resource-completion-metadata
         (list :session session) 'skill)
        :skills))
      (should-not
       (plist-get
        (niwa-resource-completion-metadata
         (list :session session) 'memory)
        :memory-roots)))))

(niwa-deftest niwa-resource-prepare ()
  ,test
  (test)
  :doc "returns an opaque attempt whose execution keeps the authored address"
  (let* ((save-path (make-temp-file "niwa-resource-session-" t))
         (local (file-name-concat save-path "local"))
         (session (niwa-session--create :save-path save-path))
         (address "work://notes.md")
         path seen-address)
    (unwind-protect
        (progn
          (make-directory local t)
          (with-temp-file (file-name-concat local "notes.md")
            (insert "note"))
          (let ((attempt (niwa-resource-prepare
                          'read address (list :session session))))
            (should (symbolp attempt))
            (should (equal address (niwa-resource-attempt-address attempt)))
            (should
             (equal "note"
                    (niwa-resource-execute
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
  (let* ((save-path (make-temp-file "niwa-resource-session-" t))
         (outside (make-temp-file "niwa-resource-outside-" t))
         (session (niwa-session--create :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local") t)
          (make-symbolic-link outside
                              (file-name-concat save-path "local" "escape"))
          (should-error
           (niwa-resource-prepare
            'read "work://escape/missing.txt" (list :session session))))
      (delete-directory save-path t)
      (delete-directory outside t))))

(niwa-deftest niwa-resource-visit-path ()
  ,test
  (test)
  :doc "returns a file-backed address's file, nothing for a listing, and leaves no attempt"
  (let* ((save-path (make-temp-file "niwa-resource-visit-" t))
         (local (file-name-concat save-path "local"))
         (session (niwa-session--create :save-path save-path))
         (before (hash-table-count niwa-resource--attempt-table)))
    (unwind-protect
        (progn
          (make-directory local t)
          (with-temp-file (file-name-concat local "notes.md") (insert "note"))
          (should (equal (file-name-concat local "notes.md")
                         (niwa-resource-visit-path "work://notes.md" (list :session session))))
          (should-not (niwa-resource-visit-path "work://" (list :session session)))
          (should-not (niwa-resource-visit-path "agent://" (list :session session)))
          (should (= before (hash-table-count niwa-resource--attempt-table))))
      (delete-directory save-path t))))

(niwa-deftest niwa-resource--shared-shape-p ()
  ,test
  (test)
  :doc "names items, their parts and element libraries, and nothing else"
  (dolist (components '(nil ("library") ("library" "Weather") ("library" "Weather" "sheet.png")
                        ("library" "Weather" "cloud" "view.png")
                        ("board" "library") ("board" "library" "Weather")
                        ("board" "library" "Weather" "sheet.png")
                        ("board" "library" "Weather" "cloud" "view.png")
                        ("board") ("board" "view.png") ("board" "discussions") ("board" "history")
                        ("board" "elements" "api") ("board" "images" "img-0123456789ab")))
    (should (niwa-resource--shared-shape-p components)))
  (dolist (components '(("board" "nope") ("board" "elements") ("board" "elements" "a" "b")
                        ("library" "Weather" "cloud" "nope.png")
                        ("board" "library" "Weather" "cloud" "sheet.png")
                        ("bad id") ("board" "elements" "bad.id")))
    (should-not (niwa-resource--shared-shape-p components))))

(niwa-deftest niwa-resource-prepare-shared ()
  ,test
  (test)
  :doc "reads items and libraries, greps text, and refuses missing items and other operations"
  (let* ((save-path (make-temp-file "niwa-resource-shared-" t))
         (session (niwa-session--create :save-path save-path))
         (context (list :session session))
         (execute (lambda (operation address)
                    (niwa-resource-execute
                     (niwa-resource-prepare operation address context)))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-artifact-backend-present-p)
                   (lambda (_ id) (equal id "board")))
                  ((symbol-function 'niwa-artifact-backend-catalog)
                   (lambda (_ detail)
                     (should (eq 'items detail))
                     (list (list :id "board" :kind "whiteboard" :title "Plan")))))
          (should (equal "shared://board\twhiteboard \"Plan\"\nshared://library\tWhiteboard element libraries for SharedEdit insert"
                         (plist-get (funcall execute 'read "shared://") :result)))
          (should (equal '("board" "elements" "api")
                         (plist-get (funcall execute 'read "shared://board/elements/api") :shared-view)))
          (should (equal '("board")
                         (plist-get (funcall execute 'grep "shared://board") :shared-view)))
          (should (equal '("library")
                         (plist-get (funcall execute 'read "shared://library") :shared-view)))
          (should (eq 'session-relative
                      (plist-get (niwa-resource-parse-address "shared://board") :locator-class)))
          (should (string-search "Shared item not found"
                                 (cadr (should-error (funcall execute 'read "shared://gone")))))
          (should (string-search "Unknown shared:// address"
                                 (cadr (should-error (niwa-resource-prepare 'read "shared://board/nope" context)))))
          (should (string-search "Glob does not support shared://"
                                 (cadr (should-error (niwa-resource-prepare 'glob "shared://board" context)))))
          (should (string-search "ApplyPatch does not support shared://"
                                 (cadr (should-error (niwa-resource-prepare 'apply-patch "shared://board" context))))))
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-artifact-address ()
  ,test
  (test)
  :doc "encodes artifact path components while retaining separators"
  (let* ((save-path (make-temp-file "niwa-resource-artifact-" t))
         (session (niwa-session--create :save-path save-path))
         (directory (file-name-concat save-path "tool-results" "part one"))
         (path (file-name-concat directory "result.txt")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file path (insert "result"))
          (should (equal "artifact://part%20one/result.txt"
                         (niwa-resource-artifact-address path session))))
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-apply-patch-preparation ()
  ,test
  (test)
  :doc "prepares a new local ApplyPatch target without materializing its root"
  (let* ((save-path (make-temp-file "niwa-resource-patch-" t))
         (session (niwa-session--create :save-path save-path))
         (address "work://notes/new.txt")
         (local-root (file-name-concat save-path "local"))
         (expected (file-name-concat local-root "notes" "new.txt")))
    (unwind-protect
        (let ((attempt (niwa-resource-prepare
                        'apply-patch address (list :session session))))
          (should (symbolp attempt))
          (should-not (file-directory-p local-root))
          (should
           (equal expected
                  (niwa-resource-execute
                   attempt
                   (lambda (path authored)
                     (should (equal address authored))
                     path)))))
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-validation-errors ()
  ,test
  (test)
  :doc "includes the authored address without exposing session storage"
  (let* ((save-path (make-temp-file "niwa-resource-validation-" t))
         (session (niwa-session--create :save-path save-path))
         (address "work://notes/../bad.txt")
         message)
    (unwind-protect
        (condition-case err
            (progn
              (niwa-resource-prepare
               'read address (list :session session))
              (error "Expected resource validation to fail"))
          (niwa-resource-error
           (setq message (error-message-string err))))
      (delete-directory save-path t))
    (should (string-match-p (regexp-quote address) message))
    (should-not (string-match-p (regexp-quote save-path) message))))

(niwa-deftest niwa-resource-within-root-p ()
  ,test
  (test)
  :doc "concurrent root timestamp changes do not invalidate containment"
  (let* ((root (make-temp-file "niwa-resource-containment-" t))
         (path (file-name-concat root "child"))
         (attributes (symbol-function 'file-attributes)))
    (unwind-protect
        (progn
          (make-directory path)
          (cl-letf (((symbol-function 'file-attributes)
                     (lambda (file &rest args)
                       (let ((result (apply attributes file args)))
                         (when (equal (directory-file-name file) root)
                           ;; A sibling writer can update the directory between
                           ;; the two stats used by `file-equal-p'.
                           (set-file-times root
                                           (time-add (file-attribute-modification-time result) 1)))
                         result))))
            (should (niwa-resource-within-root-p path root))))
      (delete-directory root t)))
  :doc "canonical root equality and child boundaries work for remote names"
  (let ((native-comp-enable-subr-trampolines nil)
        (file-name-handler-alist nil)
        (root "/ssh:example:/workspace/"))
    (cl-letf (((symbol-function 'file-symlink-p) (lambda (_) nil))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'file-truename) #'identity))
      (should (niwa-resource-within-root-p root root))
      (should (niwa-resource-within-root-p (concat root "child") root))
      (should-not (niwa-resource-within-root-p "/ssh:example:/workspace-other/child" root)))))

(niwa-deftest niwa-resource-containment-and-lifecycle
  (:doc "keeps symlink escapes and pending execution spools out of resources")
  ,test
  (test)
  (let* ((save-path (make-temp-file "niwa-resource-lifecycle-" t))
         (outside (make-temp-file "niwa-resource-outside-" t))
         (session (niwa-session--create :save-path save-path))
         (local-root (file-name-concat save-path "local"))
         (artifact-root (file-name-concat save-path "tool-results"))
         (pending (file-name-concat artifact-root
                                    ".niwa-pending-executions"))
         (address "work://note.md")
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
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "work://" (list :session session))))
                (artifacts
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "artifact://" (list :session session)))))
            (should (string-match-p "work://note.md"
                                    (plist-get listing :result)))
            (should-not (string-match-p "escape" (plist-get listing :result)))
            (should (string-match-p "artifact://published.log"
                                    (plist-get artifacts :result)))
            (should-not (string-match-p "hidden.log"
                                        (plist-get artifacts :result))))
          (should-error
           (niwa-resource-execute
            (niwa-resource-prepare
             'read "artifact://.niwa-pending-executions/hidden.log"
             (list :session session)))))
          (let ((attempt (niwa-resource-prepare
                          'read address (list :session session))))
            (niwa-resource-execute
             attempt
             (lambda (path _authored)
               (setq physical path)))
            (should (equal (file-name-concat local-root "note.md") physical))
            (should-not (niwa-resource-attempt-address attempt)))
          (setq renamed-save
                (make-temp-file "niwa-resource-renamed-" t))
          (make-directory (file-name-concat renamed-save "local") t)
          (with-temp-file (file-name-concat renamed-save "local" "note.md")
            (insert "renamed"))
          (setf (niwa-session-save-path session) renamed-save)
          (let ((attempt (niwa-resource-prepare
                          'read address (list :session session))))
            (niwa-resource-execute
             attempt
             (lambda (path _authored)
               (setq physical path)))
            (should (equal (file-name-concat renamed-save "local" "note.md")
                           physical)))
          (setf (niwa-session-save-path session) save-path)
          (let ((attempt (niwa-resource-prepare
                          'read address (list :session session))))
            (delete-file (file-name-concat local-root "note.md"))
            (make-symbolic-link outside (file-name-concat local-root "note.md"))
            (should-error
             (niwa-resource-execute
              attempt
              (lambda (&rest _)
                (error "Resource freshness check was bypassed"))))))
      (delete-directory save-path t)
      (when renamed-save
        (delete-directory renamed-save t))
      (delete-directory outside t)))

(niwa-deftest niwa-resource-local-artifact-provider ()
  ,test
  (test)
  :doc "lists local and artifact roots through logical addresses"
  (let* ((save-path (make-temp-file "niwa-resource-provider-" t))
         (session (test-niwa-session-persistence--leased-session save-path))
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
            (niwa-plan-write-current
             "# Managed plan" session (current-buffer)))
          (let ((local-read
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "work://" (list :session session))))
                (artifact-read
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "artifact://" (list :session session))))
                (grep-path nil))
            (should (string-match-p "work://notes.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "work://plans/current.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "artifact://answer.txt"
                                    (plist-get artifact-read :result)))
            (niwa-resource-execute
             (niwa-resource-prepare 'grep "work://"
                                        (list :session session))
             (lambda (path authored)
               (setq grep-path (list path authored))))
            (should (equal (list (list :path local-root :address "work://"))
                           (plist-get (car grep-path) :resource-search-roots)))
            ;; A bare listing address is never a patch endpoint.
            (should-error
             (niwa-resource-prepare
              'apply-patch "work://" (list :session session))
             :type 'niwa-resource-error)))
      (niwa-session-persistence-lock-release save-path session)
      (niwa-workspace-clear-registry)
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-niwa-provider ()
  ,test
  (test)
  :doc "lists current installed Markdown docs without a session or source paths"
  (let* ((root (make-temp-file "niwa-resource-installed-" t))
         (docs (file-name-concat root "docs"))
         (nested (file-name-concat docs "nested"))
         (private (file-name-concat root "private.md"))
         (niwa-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory nested t)
          (with-temp-file (file-name-concat root "niwa-resource.el")
            (insert ";; Package source must not be addressable.\n"))
          (with-temp-file (file-name-concat docs "z.md")
            (insert "zeta\n"))
          (with-temp-file (file-name-concat nested "a.md")
            (insert "alpha\n"))
          (with-temp-file (file-name-concat docs "ignored.txt")
            (insert "not Markdown\n"))
          (with-temp-file private
            (insert "private package content\n"))
          (let ((attempt (niwa-resource-prepare 'read "niwa://" nil)))
            (with-temp-file (file-name-concat docs "current.md")
              (insert "created after preparation\n"))
            (make-symbolic-link private (file-name-concat docs "private.md"))
            (let ((result (plist-get (niwa-resource-execute attempt) :result)))
              (should
               (equal (string-join '("niwa://current.md"
                                     "niwa://nested/a.md"
                                     "niwa://z.md")
                                   "\n")
                      result))
              (should-not (string-match-p (regexp-quote root) result))
              (should-not (string-match-p "niwa-resource\\.el" result))))
          (let ((parsed (niwa-resource-parse-address
                         "niwa://nested/a.md")))
            (should (eq 'exact (plist-get parsed :locator-class)))
            (should (equal '("nested" "a.md")
                           (plist-get parsed :components))))
          (should-error
           (niwa-resource-prepare 'read "niwa://private.md" nil)
           :type 'niwa-resource-error)
          (dolist (address '("niwa://ignored.txt"
                             "niwa://missing.md"))
            (should-error
             (niwa-resource-execute
              (niwa-resource-prepare 'read address nil)
              (lambda (_path _authored) t))
             :type 'niwa-resource-unavailable))
          (should-error
           (niwa-resource-prepare 'apply-patch
                                     "niwa://nested/a.md" nil)
           :type 'niwa-resource-error)
          (delete-directory docs t)
          (should-error
           (niwa-resource-execute
            (niwa-resource-prepare 'read "niwa://" nil))
           :type 'niwa-resource-unavailable))
      (delete-directory root t))))

(niwa-deftest niwa-resource--session ()
  ,test
  (test)
  :doc "resolves one shared local root for the parent and its retained agents"
  (let* ((save-path (make-temp-file "niwa-resource-shared-" t))
         (local-root (file-name-concat save-path "local"))
         (parent-session (niwa-session--create :name "parent"
                                                  :save-path save-path))
         (agent-buffer (generate-new-buffer " *niwa-resource-agent*"))
         parent-path agent-path)
    (unwind-protect
        (progn
          (make-directory local-root t)
          (with-temp-file (file-name-concat local-root "shared.md")
            (insert "shared note"))
          (niwa-resource-execute
           (niwa-resource-prepare
            'read "work://shared.md" (list :session parent-session))
           (lambda (physical _authored) (setq parent-path physical)))
          ;; A retained agent conversation buffer owns the parent session, so
          ;; its context resolves the same physical root.
          (with-current-buffer agent-buffer
            (setq-local niwa--session parent-session)
            (niwa-resource-execute
             (niwa-resource-prepare 'read "work://shared.md" nil)
             (lambda (physical _authored) (setq agent-path physical))))
          (should (equal (file-name-concat local-root "shared.md")
                         parent-path))
          (should (equal parent-path agent-path)))
      (kill-buffer agent-buffer)
      (delete-directory save-path t))))

(niwa-deftest niwa-resource-agent-provider ()
  ,test
  (test)
  :doc "reads settled agent output and applies RFC 6901 extraction"
  (let* ((payload "{\"b\":2,\"findings\":[{\"path\":\"a/b\",\"ok\":null}],\"a\":1}")
         (record (niwa-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result payload :settled-outcome 'completed))
         (session (niwa-session--create)))
    (niwa-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (let* ((exact
            (niwa-resource-execute
             (niwa-resource-prepare
              'read "agent://root/reviewer" (list :session session))))
           (selected
            (niwa-resource-execute
             (niwa-resource-prepare
              'read "agent://root/reviewer#/findings/0/path"
              (list :session session))))
           (null-value
            (niwa-resource-execute
             (niwa-resource-prepare
              'read "agent://root/reviewer#/findings/0/ok"
              (list :session session)))))
      (should (equal payload (plist-get exact :result)))
      (should (equal "a/b" (plist-get selected :result)))
      (should (equal "null" (plist-get null-value :result))))
    (should-error
     (niwa-resource-execute
      (niwa-resource-prepare
       'read "agent://root/reviewer#/findings/1/path"
       (list :session session)))))
  :doc "renders structured JSON selections without losing keys or string quoting"
  (let* ((record (niwa-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-outcome 'completed))
         (session (niwa-session--create)))
    (niwa-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (dolist (entry '(("{\"z\":2,\"a\":\"hello\"}" "" "{\"a\":\"hello\",\"z\":2}")
                     ("[\"hello\",\"a\\\"b\",\"line\\nend\"]" ""
                      "[\"hello\",\"a\\\"b\",\"line\\nend\"]")
                     ("{\"nested\":{\"z\":[false,null,true,0,{},[]],\"a\":\"hello\"}}"
                      "/nested" "{\"a\":\"hello\",\"z\":[false,null,true,0,{},[]]}")
                     ("{\"nested\":[{\"z\":\"last\",\"a\":\"first\"}]}"
                      "/nested" "[{\"a\":\"first\",\"z\":\"last\"}]")
                     ("{\"z\":0,\"\":\"empty key\",\"a\\\"b\":1}" ""
                      "{\"\":\"empty key\",\"a\\\"b\":1,\"z\":0}")))
      (setf (niwa-agent-record-settled-result record) (car entry))
      (ert-info ((format "JSON selection %S" entry))
        (should (equal (nth 2 entry)
                       (plist-get
                        (niwa-resource-execute
                         (niwa-resource-prepare
                          'read (concat "agent://root/reviewer#" (nth 1 entry))
                          (list :session session)))
                        :result))))))
  :doc "keeps selected scalars readable and null distinct from missing"
  (let* ((record (niwa-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "[\"hello\\nworld\",\"\",null,false,true,0,3.5,{},[]]"
                  :settled-outcome 'completed))
         (session (niwa-session--create)))
    (niwa-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (cl-loop for expected in '("hello\nworld" "" "null" "false" "true" "0" "3.5" "{}" "[]")
             for index from 0 do
             (should (equal expected
                            (plist-get
                             (niwa-resource-execute
                              (niwa-resource-prepare
                               'read (format "agent://root/reviewer#/%d" index)
                               (list :session session)))
                             :result))))
    (should-error
     (niwa-resource-execute
      (niwa-resource-prepare
       'read "agent://root/reviewer#/9" (list :session session)))
     :type 'niwa-resource-unavailable))
  :doc "lists retained agent paths with readiness"
  (let* ((record (niwa-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "done" :settled-outcome 'completed))
         (session (niwa-session--create)))
    (niwa-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (let ((result
           (niwa-resource-execute
            (niwa-resource-prepare
             'read "agent://" (list :session session)))))
      (should (string-match-p "agent://root/reviewer"
                              (plist-get result :result)))
      (should-not (string-match-p "\\`agent://root\t"
                                  (plist-get result :result)))
      (should (string-match-p "ready" (plist-get result :result)))))
  :doc "refreshes an unavailable agent when its record appears before execution"
  (let* ((session (niwa-session--create))
         (attempt (niwa-resource-prepare
                   'read "agent://root/reviewer" (list :session session)))
         (record (niwa-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "now available" :settled-outcome 'completed)))
    (should (plist-get (gethash attempt niwa-resource--attempt-table)
                       :unavailable-p))
    (niwa-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (should (equal "now available"
                   (plist-get (niwa-resource-execute attempt) :result)))))

(niwa-deftest niwa-resource-agent-pending-read
  (:doc "active agent Read returns status rather than stale payload or a pagination error")
  (with-temp-buffer
    (org-mode)
    (let* ((record (niwa-agent-record--create
                    :path "/root/reviewer" :role "reviewer" :activity 'running
                    :conversation-buffer (current-buffer)
                    :settled-result "stale secret result" :settled-outcome 'completed))
           (session (niwa-session--create :permission-mode 'full-auto)))
      (setq-local niwa--session session)
      (niwa-session--set-agent-registry session (list (cons "/root/reviewer" record)))
      (niwa-tool-fs--register)
      (dolist (activity '(starting running waiting permission-blocked interaction-blocked))
        (setf (niwa-agent-record-activity record) activity)
        (dolist (address '("agent://root/reviewer" "agent://root/reviewer#/findings/0/path"))
          (let (result)
            (niwa-pipeline-run-tool
             (niwa-tool-get "Read") (lambda (value) (setq result value))
             (list :file_path address :offset 1000000 :limit 1))
            (should (stringp result))
            (should (string-search "NOT READY" result))
            (should (string-search "history://root/reviewer" result))
            (should-not (string-prefix-p "Error:" result))
            (should-not (string-search "stale secret result" result))
            (should-not (string-search "#/findings" result))))))))

(niwa-deftest niwa-resource-agent-state-boundaries
  (:doc "pending status preserves option validation and settled or unavailable state boundaries")
  (with-temp-buffer
    (org-mode)
    (let* ((record (niwa-agent-record--create :path "/root/reviewer" :activity 'starting))
           (session (niwa-session--create :permission-mode 'full-auto)))
      (setq-local niwa--session session)
      (niwa-session--set-agent-registry session (list (cons "/root/reviewer" record)))
      (niwa-tool-fs--register)
      (cl-labels ((read-agent (options)
                    (let (result)
                      (niwa-pipeline-run-tool
                       (niwa-tool-get "Read") (lambda (value) (setq result value))
                       (append (list :file_path "agent://root/reviewer") options))
                      result)))
        (let ((status (read-agent nil)))
          (should (string-search "NOT READY" status))
          (should (string-search "history is not yet available" status))
          (should-not (string-search "history://root/reviewer" status)))
        (dolist (options '((:offset -1) (:limit -1) (:pages "1") (:max_width 10)))
          (should (string-prefix-p "Error:" (read-agent options))))
        (setf (niwa-agent-record-activity record) 'idle)
        (should (string-prefix-p "Error:" (read-agent nil)))
        (setf (niwa-agent-record-settled-result record) "invalid outcome payload"
              (niwa-agent-record-settled-outcome record) 'running)
        (should (string-prefix-p "Error:" (read-agent nil)))
        (setf (niwa-agent-record-settled-result record) ""
              (niwa-agent-record-settled-outcome record) 'completed)
        (should (equal "" (read-agent nil)))
        ;; Execution must observe settlement after preparation, not return
        ;; the status captured when the permission attempt was prepared.
        (setf (niwa-agent-record-activity record) 'running)
        (let ((attempt (niwa-resource-prepare
                        'read "agent://root/reviewer#/answer" (list :session session))))
          (setf (niwa-agent-record-activity record) 'idle
                (niwa-agent-record-settled-result record) "{\"answer\":\"settled now\"}")
          (should (equal "settled now" (plist-get (niwa-resource-execute attempt) :result))))
        (niwa-session--set-agent-registry session nil)
        (should (string-prefix-p "Error:" (read-agent nil)))))))

(niwa-deftest niwa-resource-history-provider ()
  ,test
  (test)
  :doc "lists and projects a live retained agent conversation"
  (let ((buffer (generate-new-buffer " *niwa-resource-history*")))
    (unwind-protect
        (let* ((record (niwa-agent-record--create
                        :path "/root/reviewer" :role "reviewer"
                        :activity 'idle :conversation-buffer buffer))
               (session (niwa-session--create)))
          (with-current-buffer buffer
            (org-mode)
            (insert "A user request\n")
            (let ((start (point)))
              (insert "An assistant decision\n"
                      "[media: image; MIME image/png; path /private/raw.png]\n")
              (put-text-property start (point) 'gptel 'response)))
          (niwa-session--set-agent-registry
           session (list (cons "/root/reviewer" record)))
          (let ((listing
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "history://" (list :session session))))
                (history
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "history://root/reviewer"
                   (list :session session)))))
            (should (string-match-p "history://root/reviewer"
                                    (plist-get listing :result)))
            (should (string-match-p "An assistant decision"
                                    (plist-get history :result)))
            (should-not (string-match-p "path /private/raw.png"
                                        (plist-get history :result)))))
      (kill-buffer buffer)))
  :doc "projects and paginates the owning root like a retained conversation"
  (with-temp-buffer
    (org-mode)
    (let* ((root (current-buffer))
           (session (niwa-session--create))
           (record (niwa-agent-record--create
                    :path "/root/reviewer" :role "reviewer" :activity 'idle
                    :conversation-buffer root)))
      (setq-local niwa--session session)
      (niwa-session-set-root-buffer session root)
      (insert ":PROPERTIES:\n:GPTEL_SYSTEM: hidden provider prompt\n:END:\n\n"
              "Root user request\n")
      (let ((start (point)))
        (insert "Root assistant answer\nsecond answer line\n"
                "[media: image; MIME image/png; path /private/raw.png]\n")
        (put-text-property start (point) 'gptel 'response))
      (insert "#+begin_tool (Read :file_path \"src.el\")\n")
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"src.el\"))\n\n"
                "visible tool evidence\n")
        (put-text-property start (point) 'gptel '(tool . "history-read")))
      (insert "#+end_tool\n"
              (niwa-tool-render-data-format '(:secret "hidden render data")))
      (insert "<system-reminder>\nhidden reminder\n</system-reminder>\n"
              "#+begin_reasoning\nhidden reasoning\n#+end_reasoning\n")
      (let ((before (buffer-string))
            (modified (buffer-modified-p))
            (position (point)))
        (with-temp-buffer
          (org-mode)
          (setq-local niwa--session (niwa-session--create))
          (insert "Unrelated caller conversation\n")
          (cl-labels
              ((read-history (address)
                 (plist-get
                  (niwa-resource-execute
                   (niwa-resource-prepare
                    'read address (list :session session)))
                  :result)))
            (should (equal "history://root\tdefault\tready"
                           (read-history "history://")))
            (let ((root-history (read-history "history://root")))
              (should (string-match-p "Root user request" root-history))
              (should (string-match-p "Root assistant answer" root-history))
              (should (string-match-p "visible tool evidence" root-history))
              (dolist (hidden '("hidden provider" "hidden reminder"
                                "hidden reasoning" "hidden render data"
                                "#+begin_tool" "/private/raw.png"
                                "Unrelated caller"))
                (should-not (string-match-p (regexp-quote hidden) root-history)))
              (niwa-session--set-agent-registry
               session (list (cons "/root/reviewer" record)))
              (should (equal root-history
                             (read-history "history://root/reviewer")))
              (dolist (address '("history://root" "history://root/reviewer"))
                (let ((search (niwa-resource-execute
                               (niwa-resource-prepare
                                'grep address (list :session session)))))
                  (should (equal root-history
                                 (cdar (plist-get search :resource-search-documents))))))
              (let (pages)
                (dolist (address '("history://root" "history://root/reviewer"))
                  (let* ((attempt (niwa-resource-prepare
                                   'read address (list :session session)))
                         (niwa-resource-current-attempts
                          (list (cons address attempt)))
                         (page (plist-get
                                (niwa-test--read
                                 (list :file_path address :offset 2 :limit 1))
                                :result)))
                    (should (string-match-p "Root user request" page))
                    (should-not (string-match-p "Root assistant answer" page))
                    (push (replace-regexp-in-string
                           (regexp-quote address) "HISTORY" page t t)
                          pages)))
                (should (equal (car pages) (cadr pages)))))))
        (should (equal before (buffer-string)))
        (should (eq modified (buffer-modified-p)))
        (should (= position (point)))
        (should-not (niwa-session-save-path session)))))
  :doc "refreshes root availability without reading another ambient session"
  (let* ((session (niwa-session--create))
         (attempt (niwa-resource-prepare
                   'read "history://root" (list :session session))))
    (with-temp-buffer
      (org-mode)
      (insert "Root appeared while permission was pending\n")
      (niwa-session-set-root-buffer session (current-buffer))
      (should (string-match-p
               "Root appeared"
               (plist-get (niwa-resource-execute attempt) :result)))
      (setq attempt (niwa-resource-prepare
                     'read "history://root" (list :session session))))
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create))
      (niwa-session-set-root-buffer niwa--session (current-buffer))
      (insert "Unrelated live root\n")
      (should-error (niwa-resource-execute attempt)
                    :type 'niwa-resource-unavailable)
      (should-error
       (niwa-resource-execute
        (niwa-resource-prepare
         'read "history://root" (list :session session)))
       :type 'niwa-resource-unavailable))))

(niwa-deftest niwa-resource-history-provider-cold-hydration ()
  ,test
  (test)
  :doc "hydrates a persisted retained conversation when no live buffer exists"
  (let* ((_ (niwa-tool-fs--register))
         (root (make-temp-file "niwa-resource-cold-history-" t))
         (agents-dir (file-name-concat root "agents"))
         (conversation (file-name-concat agents-dir "reviewer.chat.org"))
         (agent (niwa-agent--create
                 :name "default" :description "Cold test agent"
                 :tools nil :system-prompt "Frozen instructions"
                 :max-turns nil :hook-rules nil :frozen-p t))
         (configuration
          (niwa-agent-configuration--create
           :agent agent
           :request-locals
           (niwa-agent-configuration-request-locals
            (test-niwa-session-persistence--agent-configuration))))
         (record (niwa-agent-record--create
                  :id "cold-reviewer" :path "/root/reviewer"
                  :parent-path "/root" :role "reviewer"
                  :configuration configuration :activity 'idle
                  :conversation-location "agents/reviewer.chat.org"))
         (session (test-niwa-session-persistence--leased-session root))
         (root-buffer (generate-new-buffer " *niwa-resource-cold-root*")))
    (unwind-protect
        (progn
          (make-directory agents-dir t)
          (niwa-session--set-agent-registry
           session (list (cons "/root/reviewer" record)))
          (niwa-session-publication-publish
           session
           (list (list :path conversation :content "Cold retained decision\n")
                 (list :path (niwa-session-artifacts-segment-path root 1)
                       :content "")
                 (list :path (niwa-session-artifacts-sidecar-path root)
                       :content (niwa-session-artifacts-printed-value
                                 (niwa-session-codec-serialize session))
                       :commit-marker t)) t)
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local niwa--session session))
          (let ((result
                 (niwa-resource-execute
                  (niwa-resource-prepare
                   'read "history://root/reviewer"
                   (list :session session)))))
            (should (string-match-p "Cold retained decision"
                                    (plist-get result :result)))
            (should (buffer-live-p
                     (niwa-agent-record-conversation-buffer record)))))
      (niwa-agent-control-teardown-session session)
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (niwa-session-persistence-lock-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-resource-memory-provider ()
  ,test
  (test)
  :doc "reads the system memory union and searches root-bound topics"
  (let* ((workspace-root (make-temp-file "niwa-resource-memory-" t))
         (workspace (niwa-workspace--create
                     :type 'test :id workspace-root :root workspace-root
                     :name "resource-memory"))
         (memory-dir (file-name-concat workspace-root ".niwa" "memory"))
         (context (list :workspace workspace)))
    (unwind-protect
        (progn
          (make-directory memory-dir t)
          (with-temp-file (file-name-concat memory-dir "MEMORY.md")
            (insert "Index entry"))
          (with-temp-file (file-name-concat memory-dir "topic.md")
            (insert "remember this fact"))
          (let* ((niwa-memory-dirs
                  (list (file-name-concat ".niwa" "memory")
                        (file-name-concat ".agents" "memory")))
                 (root (car (niwa-system--memory-roots workspace)))
                 (key (niwa-resource-memory-root-key root))
                 (topic (format "memory://%s/topic.md" key))
                 (alias-topic "memory://local-niwa/topic.md")
                 (index
                  (niwa-resource-execute
                   (niwa-resource-prepare
                    'read "memory://root" context)))
                 (glob
                  (niwa-resource-execute
                   (niwa-resource-prepare
                    'glob "memory://root" context)
                   nil '(:pattern "*.md")))
                 (read-topic
                  (niwa-resource-execute
                   (niwa-resource-prepare 'read topic context)
                   (lambda (path _address)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))))
                 (read-alias-topic
                  (niwa-resource-execute
                   (niwa-resource-prepare 'read alias-topic context)
                   (lambda (path _address)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))))
            (should (string-match-p "Index entry" (plist-get index :result)))
            ;; The missing `.agents/memory' root is excluded from search.
            (should (= 1 (length (plist-get glob :resource-search-roots))))
            ;; A unique readable root key replaces the digest in
            ;; disclosed addresses, and both forms resolve the same root.
            (should (equal
                     "memory://local-niwa"
                     (plist-get
                      (car (plist-get glob :resource-search-roots))
                      :address-prefix)))
            (should (equal "remember this fact" read-topic))
            (should (equal "remember this fact" read-alias-topic))
            (should (plist-get
                     (gethash
                      (niwa-resource-prepare
                       'read "memory://global-agents/topic.md" context)
                      niwa-resource--attempt-table)
                     :unavailable-p))))
      (delete-directory workspace-root t))))

(provide 'test-niwa-resource)
;;; test-niwa-resource.el ends here
