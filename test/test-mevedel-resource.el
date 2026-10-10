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
(require 'mevedel-tool-fs-read)
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
  (let ((parsed (mevedel-resource-parse-address "work://notes%20one.md")))
    (should (eq 'work (plist-get parsed :scheme)))
    (should (equal '("notes one.md")
                   (plist-get parsed :components)))
    (should (equal "work://notes%20one.md"
                   (plist-get parsed :canonical)))
    (should (eq 'session-relative (plist-get parsed :locator-class)))
    (should-not (plist-get parsed :dynamic-p)))
  :doc "classifies a bare local address as dynamic discovery"
  (let ((parsed (mevedel-resource-parse-address "work://")))
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
    (should-error (mevedel-resource-parse-address address)))
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
                   ("skill://local%2dmevedel/review"
                    . "skill://local-mevedel/review")
                   ("mcp://server/" . "mcp://server")
                   ("shared://library/System Design/sheet.png"
                    . "shared://library/System%20Design/sheet.png")))
    (should (equal (cdr entry)
                   (plist-get (mevedel-resource-parse-address (car entry))
                              :canonical))))
  :doc "rejects invalid UTF-8 bytes in names and JSON pointer fragments"
  (dolist (bytes '("%FF" "%80" "%C0%AF" "%C3" "%ED%A0%80"
                   "%F4%90%80%80"))
    (dolist (prefix '("work://" "mcp://server/" "agent://root/review#/"))
      (should-error (mevedel-resource-parse-address (concat prefix bytes))
                    :type 'mevedel-resource-error)))
  :doc "rejects unknown scheme URLs instead of treating them as paths"
  (should-error (mevedel-resource-parse-address "https://example.test/a"))
  :doc "rejects an unknown scheme without interning its name"
  ;; A scheme name arrives in model tool arguments and is parsed before the
  ;; permission step runs, so an unknown one must leave no symbol behind:
  ;; Emacs never collects an interned symbol.
  (let* ((name "mevedelunknownscheme")
         (address (concat name "://a")))
    (unwind-protect
        (progn
          ;; It must still look address-like, or an unknown scheme would be
          ;; expanded as a relative filesystem path instead of rejected.
          (should (mevedel-resource-address-like-p address))
          (should-error (mevedel-resource-parse-address address))
          (should-not (intern-soft name)))
      (unintern name obarray)))
  :doc "rejects a scheme named after a falsy symbol"
  ;; `nil' interns to a symbol that is itself false, so interning the prefix
  ;; made this address look like no address at all, and it was expanded as a
  ;; relative path instead of rejected.
  (progn
    (should (mevedel-resource-address-like-p "nil://a"))
    (should-error (mevedel-resource-parse-address "nil://a"))))

(mevedel-deftest mevedel-resource-encode-component ()
  ,test
  (test)
  :doc "encodes UTF-8 bytes with uppercase RFC 3986 escapes"
  (should (equal "space%20and%2F%C3%A4%25"
                 (mevedel-resource-encode-component "space and/ä%")))
  :doc "leaves only unreserved bytes literal"
  (should (equal "AZaz09-._~"
                 (mevedel-resource-encode-component "AZaz09-._~"))))

(mevedel-deftest mevedel-resource-supported-scheme-p
  (:doc "answers a scheme name with the scheme it names")
  (progn
    (should (eq 'work (mevedel-resource-supported-scheme-p "WORK")))
    (should (eq 'work (mevedel-resource-supported-scheme-p 'work)))
    (should-not (mevedel-resource-supported-scheme-p "https"))))

(mevedel-deftest mevedel-resource-locator-class ()
  ,test
  (test)
  :doc "recognizes supported schemes and ordinary native paths"
  (dolist (scheme '(work artifact skill agent history memory mcp mevedel))
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
                   "history://root#"
                   "history://root/../other"
                   "agent://root/Reviewer"
                   "history://root/reviewer-name"
                   "skill://name@ABC"
                   (concat "skill://name@" (upcase digest))
                   "mcp://server/uri/extra"
                   "agent://root/reviewer?query=1"
                   "work://notes?query=1"
                   "agent://root/reviewer#not-a-pointer"
                   "agent://root/reviewer#/%7E0"
                   "agent://root/reviewer#/bad~2escape"))
      (should-error (mevedel-resource-parse-address address)))
    (dolist (fragment '("" "/" "/a~1b/~0/%C3%A4%25"))
      (let* ((address (concat "agent://root/reviewer#" fragment))
             (parsed (mevedel-resource-parse-address address)))
        (should (equal address (plist-get parsed :canonical)))
        (should (= (length "root/reviewer") (plist-get parsed :fragment-p))))))
  :doc "accepts the root history as a session-relative read-only address"
  (let ((parsed (mevedel-resource-parse-address "history://root")))
    (should (equal '("root") (plist-get parsed :components)))
    (should (equal "history://root" (plist-get parsed :canonical)))
    (should (eq 'session-relative (plist-get parsed :locator-class)))
    (should-not (plist-get parsed :dynamic-p))
    (dolist (operation '(glob apply-patch))
      (should-error (mevedel-resource-prepare operation "history://root" nil)
                    :type 'mevedel-resource-error)))
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
    (should-not (plist-get empty :pointer)))
  :doc "parses readable standard and plugin skill aliases"
  (let ((ordinary
         (mevedel-resource-parse-address
          "skill://local-agents/demo/templates/prompt.tmpl"))
        (plugin
         (mevedel-resource-parse-address
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

(mevedel-deftest mevedel-resource-completion-metadata ()
  ,test
  (test)
  :doc "loads only the provider selected by each completion scheme"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "completion" :root default-directory
                     :name "completion"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :save-path default-directory
                   :workspace workspace))
         (resource-root-function
          (symbol-function 'mevedel-resource--root)))
    (dolist (scheme '(work artifact skill agent history memory mcp mevedel))
      (cl-letf (((symbol-function 'mevedel-resource--root)
                 (lambda (root-scheme owner)
                   (unless (eq root-scheme scheme)
                     (error "Unrelated path root ran"))
                   (funcall resource-root-function root-scheme owner)))
                ((symbol-function 'mevedel-resource--skill-list)
                 (lambda (&rest _)
                   (error "Skill discovery ran during completion")))
                ((symbol-function 'mevedel-agent-control-list-agents)
                 (lambda (&rest _)
                   (unless (memq scheme '(agent history))
                     (error "Unrelated agent provider ran"))))
                ((symbol-function 'mevedel-resource--memory-roots)
                 (lambda (&rest _)
                   (unless (eq scheme 'memory)
                     (error "Unrelated memory provider ran"))))
                ((symbol-function 'mcp-hub-get-servers)
                 (lambda (&rest _)
                   (unless (eq scheme 'mcp)
                     (error "Unrelated MCP provider ran")))))
        (let ((metadata
               (mevedel-resource-completion-metadata
                (list :session session) scheme)))
          (should
           (equal (mapcar #'car (plist-get metadata :roots))
                  (pcase scheme
                    ('work '(work))
                    ('artifact '(artifact))
                    ('mevedel '(mevedel)))))))))
  :doc "drops remote skill and memory roots before identity lookup"
  (let* ((remote "/ssh:example.invalid:/tmp/resource")
         (mevedel-memory-dirs (list remote))
         (workspace (mevedel-workspace--create
                     :type 'test :id "remote" :root remote
                     :name "remote"))
         (skill (mevedel-skill--create
                 :name "remote" :source-file (concat remote "/SKILL.md")
                 :source-dir remote))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :skills (list skill)
                   :workspace workspace)))
    (cl-letf (((symbol-function 'mevedel-skills-skill-enabled-p)
               (lambda (&rest _)
                 (error "Remote skill identity was inspected")))
              ((symbol-function 'mevedel-skills-scan)
               (lambda (&rest _)
                 (error "Remote workspace skills were scanned")))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (error "Remote root was canonicalized"))))
      (should-not
       (plist-get
        (mevedel-resource-completion-metadata
         (list :session session) 'skill)
        :skills))
      (setf (mevedel-session-skills session) nil)
      (should-not
       (plist-get
        (mevedel-resource-completion-metadata
         (list :session session) 'skill)
        :skills))
      (should-not
       (plist-get
        (mevedel-resource-completion-metadata
         (list :session session) 'memory)
        :memory-roots)))))

(mevedel-deftest mevedel-resource-prepare ()
  ,test
  (test)
  :doc "returns an opaque attempt whose execution keeps the authored address"
  (let* ((save-path (make-temp-file "mevedel-resource-session-" t))
         (local (file-name-concat save-path "local"))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (address "work://notes.md")
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
            'read "work://escape/missing.txt" (list :session session))))
      (delete-directory save-path t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-resource-visit-path ()
  ,test
  (test)
  :doc "returns a file-backed address's file, nothing for a listing, and leaves no attempt"
  (let* ((save-path (make-temp-file "mevedel-resource-visit-" t))
         (local (file-name-concat save-path "local"))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (before (hash-table-count mevedel-resource--attempt-table)))
    (unwind-protect
        (progn
          (make-directory local t)
          (with-temp-file (file-name-concat local "notes.md") (insert "note"))
          (should (equal (file-name-concat local "notes.md")
                         (mevedel-resource-visit-path "work://notes.md" (list :session session))))
          (should-not (mevedel-resource-visit-path "work://" (list :session session)))
          (should-not (mevedel-resource-visit-path "agent://" (list :session session)))
          (should (= before (hash-table-count mevedel-resource--attempt-table))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource--shared-shape-p ()
  ,test
  (test)
  :doc "names items, their parts and element libraries, and nothing else"
  (dolist (components '(nil ("library") ("library" "Weather") ("library" "Weather" "sheet.png")
                        ("board") ("board" "view.png") ("board" "comments") ("board" "history")
                        ("board" "elements" "api") ("board" "images" "img-0123456789ab")))
    (should (mevedel-resource--shared-shape-p components)))
  (dolist (components '(("board" "nope") ("board" "elements") ("board" "elements" "a" "b")
                        ("bad id") ("board" "elements" "bad.id")))
    (should-not (mevedel-resource--shared-shape-p components))))

(mevedel-deftest mevedel-resource-prepare-shared ()
  ,test
  (test)
  :doc "reads items and libraries, greps text, and refuses missing items and other operations"
  (let* ((save-path (make-temp-file "mevedel-resource-shared-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (context (list :session session))
         (execute (lambda (operation address)
                    (mevedel-resource-execute
                     (mevedel-resource-prepare operation address context)))))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-shared-editing-ids) (lambda (_) '("board")))
                  ((symbol-function 'mevedel-shared-editing-list)
                   (lambda (_) (list (list :id "board" :kind "whiteboard" :title "Plan" :revision 4)))))
          (should (equal "shared://board\twhiteboard \"Plan\" · revision 4\nshared://library\tWhiteboard element libraries for SharedEdit insert"
                         (plist-get (funcall execute 'read "shared://") :result)))
          (should (equal '("board" "elements" "api")
                         (plist-get (funcall execute 'read "shared://board/elements/api") :shared-view)))
          (should (equal '("board")
                         (plist-get (funcall execute 'grep "shared://board") :shared-view)))
          (should (equal '("library")
                         (plist-get (funcall execute 'read "shared://library") :shared-view)))
          (should (eq 'session-relative
                      (plist-get (mevedel-resource-parse-address "shared://board") :locator-class)))
          (should (string-search "Shared item not found"
                                 (cadr (should-error (funcall execute 'read "shared://gone")))))
          (should (string-search "Unknown shared:// address"
                                 (cadr (should-error (mevedel-resource-prepare 'read "shared://board/nope" context)))))
          (should (string-search "Glob does not support shared://"
                                 (cadr (should-error (mevedel-resource-prepare 'glob "shared://board" context)))))
          (should (string-search "ApplyPatch does not support shared://"
                                 (cadr (should-error (mevedel-resource-prepare 'apply-patch "shared://board" context))))))
      (delete-directory save-path t))))

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
         (address "work://notes/new.txt")
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
         (address "work://notes/../bad.txt")
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

(mevedel-deftest mevedel-resource-within-root-p ()
  ,test
  (test)
  :doc "concurrent root timestamp changes do not invalidate containment"
  (let* ((root (make-temp-file "mevedel-resource-containment-" t))
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
            (should (mevedel-resource-within-root-p path root))))
      (delete-directory root t)))
  :doc "canonical root equality and child boundaries work for remote names"
  (let ((native-comp-enable-subr-trampolines nil)
        (file-name-handler-alist nil)
        (root "/ssh:example:/workspace/"))
    (cl-letf (((symbol-function 'file-symlink-p) (lambda (_) nil))
              ((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'file-truename) #'identity))
      (should (mevedel-resource-within-root-p root root))
      (should (mevedel-resource-within-root-p (concat root "child") root))
      (should-not (mevedel-resource-within-root-p "/ssh:example:/workspace-other/child" root)))))

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
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "work://" (list :session session))))
                (artifacts
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "artifact://" (list :session session)))))
            (should (string-match-p "work://note.md"
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
                   'read "work://" (list :session session))))
                (artifact-read
                 (mevedel-resource-execute
                  (mevedel-resource-prepare
                   'read "artifact://" (list :session session))))
                (grep-path nil))
            (should (string-match-p "work://notes.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "work://plans/current.md"
                                    (plist-get local-read :result)))
            (should (string-match-p "artifact://answer.txt"
                                    (plist-get artifact-read :result)))
            (mevedel-resource-execute
             (mevedel-resource-prepare 'grep "work://"
                                        (list :session session))
             (lambda (path authored)
               (setq grep-path (list path authored))))
            (should (equal (list (list :path local-root :address "work://"))
                           (plist-get (car grep-path) :resource-search-roots)))
            ;; A bare listing address is never a patch endpoint.
            (should-error
             (mevedel-resource-prepare
              'apply-patch "work://" (list :session session))
             :type 'mevedel-resource-error)))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-resource-mevedel-provider ()
  ,test
  (test)
  :doc "lists current installed Markdown docs without a session or source paths"
  (let* ((root (make-temp-file "mevedel-resource-installed-" t))
         (docs (file-name-concat root "docs"))
         (nested (file-name-concat docs "nested"))
         (private (file-name-concat root "private.md"))
         (mevedel-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory nested t)
          (with-temp-file (file-name-concat root "mevedel-resource.el")
            (insert ";; Package source must not be addressable.\n"))
          (with-temp-file (file-name-concat docs "z.md")
            (insert "zeta\n"))
          (with-temp-file (file-name-concat nested "a.md")
            (insert "alpha\n"))
          (with-temp-file (file-name-concat docs "ignored.txt")
            (insert "not Markdown\n"))
          (with-temp-file private
            (insert "private package content\n"))
          (let ((attempt (mevedel-resource-prepare 'read "mevedel://" nil)))
            (with-temp-file (file-name-concat docs "current.md")
              (insert "created after preparation\n"))
            (make-symbolic-link private (file-name-concat docs "private.md"))
            (let ((result (plist-get (mevedel-resource-execute attempt) :result)))
              (should
               (equal (string-join '("mevedel://current.md"
                                     "mevedel://nested/a.md"
                                     "mevedel://z.md")
                                   "\n")
                      result))
              (should-not (string-match-p (regexp-quote root) result))
              (should-not (string-match-p "mevedel-resource\\.el" result))))
          (let ((parsed (mevedel-resource-parse-address
                         "mevedel://nested/a.md")))
            (should (eq 'exact (plist-get parsed :locator-class)))
            (should (equal '("nested" "a.md")
                           (plist-get parsed :components))))
          (should-error
           (mevedel-resource-prepare 'read "mevedel://private.md" nil)
           :type 'mevedel-resource-error)
          (dolist (address '("mevedel://ignored.txt"
                             "mevedel://missing.md"))
            (should-error
             (mevedel-resource-execute
              (mevedel-resource-prepare 'read address nil)
              (lambda (_path _authored) t))
             :type 'mevedel-resource-unavailable))
          (should-error
           (mevedel-resource-prepare 'apply-patch
                                     "mevedel://nested/a.md" nil)
           :type 'mevedel-resource-error)
          (delete-directory docs t)
          (should-error
           (mevedel-resource-execute
            (mevedel-resource-prepare 'read "mevedel://" nil))
           :type 'mevedel-resource-unavailable))
      (delete-directory root t))))

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
            'read "work://shared.md" (list :session parent-session))
           (lambda (physical _authored) (setq parent-path physical)))
          ;; A retained agent conversation buffer owns the parent session, so
          ;; its context resolves the same physical root.
          (with-current-buffer agent-buffer
            (setq-local mevedel--session parent-session)
            (mevedel-resource-execute
             (mevedel-resource-prepare 'read "work://shared.md" nil)
             (lambda (physical _authored) (setq agent-path physical))))
          (should (equal (file-name-concat local-root "shared.md")
                         parent-path))
          (should (equal parent-path agent-path)))
      (kill-buffer agent-buffer)
      (delete-directory save-path t))))

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
  :doc "renders structured JSON selections without losing keys or string quoting"
  (let* ((record (mevedel-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-outcome 'completed))
         (session (mevedel-session--create)))
    (mevedel-session--set-agent-registry
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
      (setf (mevedel-agent-record-settled-result record) (car entry))
      (ert-info ((format "JSON selection %S" entry))
        (should (equal (nth 2 entry)
                       (plist-get
                        (mevedel-resource-execute
                         (mevedel-resource-prepare
                          'read (concat "agent://root/reviewer#" (nth 1 entry))
                          (list :session session)))
                        :result))))))
  :doc "keeps selected scalars readable and null distinct from missing"
  (let* ((record (mevedel-agent-record--create
                  :path "/root/reviewer" :role "reviewer" :activity 'idle
                  :settled-result "[\"hello\\nworld\",\"\",null,false,true,0,3.5,{},[]]"
                  :settled-outcome 'completed))
         (session (mevedel-session--create)))
    (mevedel-session--set-agent-registry
     session (list (cons "/root/reviewer" record)))
    (cl-loop for expected in '("hello\nworld" "" "null" "false" "true" "0" "3.5" "{}" "[]")
             for index from 0 do
             (should (equal expected
                            (plist-get
                             (mevedel-resource-execute
                              (mevedel-resource-prepare
                               'read (format "agent://root/reviewer#/%d" index)
                               (list :session session)))
                             :result))))
    (should-error
     (mevedel-resource-execute
      (mevedel-resource-prepare
       'read "agent://root/reviewer#/9" (list :session session)))
     :type 'mevedel-resource-unavailable))
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
      (should (string-match-p "ready" (plist-get result :result)))))
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
                   (plist-get (mevedel-resource-execute attempt) :result)))))

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
      (kill-buffer buffer)))
  :doc "projects and paginates the owning root like a retained conversation"
  (with-temp-buffer
    (org-mode)
    (let* ((root (current-buffer))
           (session (mevedel-session--create))
           (record (mevedel-agent-record--create
                    :path "/root/reviewer" :role "reviewer" :activity 'idle
                    :conversation-buffer root)))
      (setq-local mevedel--session session)
      (mevedel-session-set-root-buffer session root)
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
              (mevedel-tool-render-data-format '(:secret "hidden render data")))
      (insert "<system-reminder>\nhidden reminder\n</system-reminder>\n"
              "#+begin_reasoning\nhidden reasoning\n#+end_reasoning\n")
      (let ((before (buffer-string))
            (modified (buffer-modified-p))
            (position (point)))
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session (mevedel-session--create))
          (insert "Unrelated caller conversation\n")
          (cl-labels
              ((read-history (address)
                 (plist-get
                  (mevedel-resource-execute
                   (mevedel-resource-prepare
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
              (mevedel-session--set-agent-registry
               session (list (cons "/root/reviewer" record)))
              (should (equal root-history
                             (read-history "history://root/reviewer")))
              (dolist (address '("history://root" "history://root/reviewer"))
                (let ((search (mevedel-resource-execute
                               (mevedel-resource-prepare
                                'grep address (list :session session)))))
                  (should (equal root-history
                                 (cdar (plist-get search :resource-search-documents))))))
              (let (pages)
                (dolist (address '("history://root" "history://root/reviewer"))
                  (let* ((attempt (mevedel-resource-prepare
                                   'read address (list :session session)))
                         (mevedel-resource-current-attempts
                          (list (cons address attempt)))
                         (page (plist-get
                                (mevedel-test--read
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
        (should-not (mevedel-session-save-path session)))))
  :doc "refreshes root availability without reading another ambient session"
  (let* ((session (mevedel-session--create))
         (attempt (mevedel-resource-prepare
                   'read "history://root" (list :session session))))
    (with-temp-buffer
      (org-mode)
      (insert "Root appeared while permission was pending\n")
      (mevedel-session-set-root-buffer session (current-buffer))
      (should (string-match-p
               "Root appeared"
               (plist-get (mevedel-resource-execute attempt) :result)))
      (setq attempt (mevedel-resource-prepare
                     'read "history://root" (list :session session))))
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session--create))
      (mevedel-session-set-root-buffer mevedel--session (current-buffer))
      (insert "Unrelated live root\n")
      (should-error (mevedel-resource-execute attempt)
                    :type 'mevedel-resource-unavailable)
      (should-error
       (mevedel-resource-execute
        (mevedel-resource-prepare
         'read "history://root" (list :session session)))
       :type 'mevedel-resource-unavailable))))

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
                  (list (file-name-concat ".mevedel" "memory")
                        (file-name-concat ".agents" "memory")))
                 (root (car (mevedel-system--memory-roots workspace)))
                 (key (mevedel-resource-memory-root-key root))
                 (topic (format "memory://%s/topic.md" key))
                 (alias-topic "memory://local-mevedel/topic.md")
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
                       (buffer-string)))))
                 (read-alias-topic
                  (mevedel-resource-execute
                   (mevedel-resource-prepare 'read alias-topic context)
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
                     "memory://local-mevedel"
                     (plist-get
                      (car (plist-get glob :resource-search-roots))
                      :address-prefix)))
            (should (equal "remember this fact" read-topic))
            (should (equal "remember this fact" read-alias-topic))
            (should (plist-get
                     (gethash
                      (mevedel-resource-prepare
                       'read "memory://global-agents/topic.md" context)
                      mevedel-resource--attempt-table)
                     :unavailable-p))))
      (delete-directory workspace-root t))))

(provide 'test-mevedel-resource)
;;; test-mevedel-resource.el ends here
