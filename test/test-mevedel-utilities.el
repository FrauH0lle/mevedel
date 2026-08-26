;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-execution-target)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript)
(require 'mevedel-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun test-mevedel-utilities--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-mevedel-utilities--raw-byte-string-p (string)
  "Return non-nil for STRING with raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))

(mevedel-deftest mevedel--plain-data-p ()
  ,test
  (test)
  :doc "accepts nested read-safe values and rejects runtime objects"
  (should (mevedel--plain-data-p
           '(nil symbol car "text" 4 (dotted . pair) [1 "two"])))
  (should-not (mevedel--plain-data-p (lambda () t)))
  (should-not (mevedel--plain-data-p (make-hash-table))))

(mevedel-deftest mevedel--transcript-org-mode ()
  ,test
  (test)
  :doc "suppresses org-indent-mode while transcript Org hooks run"
  (progn
    (require 'org)
    (with-temp-buffer
      (let ((org-mode-hook (list (lambda () (org-indent-mode +1))))
            (redraws 0))
        (cl-letf (((symbol-function 'redraw-display)
                   (lambda () (cl-incf redraws))))
          (mevedel--transcript-org-mode))
        (should (derived-mode-p 'org-mode))
        (should-not (bound-and-true-p org-indent-mode))
        (should (= 0 redraws))))))

(mevedel-deftest mevedel--head-tail-preview-parts ()
  ,test
  (test)

  :doc "returns short content unchanged"
  (let ((preview (mevedel--head-tail-preview-parts "short" "short" 5 10)))
    (should (equal "short" (plist-get preview :text)))
    (should (= 0 (plist-get preview :omitted-chars))))

  :doc "uses bounded newline-aware parts and an exact character count"
  (let ((preview
         (mevedel--head-tail-preview-parts
          "1234\n67890" "abc\ndefghi" 30 10)))
    (should (equal
             "1234\n[mevedel: tool output truncated; omitted 20 chars]\nefghi"
             (plist-get preview :text)))
    (should (= 20 (plist-get preview :omitted-chars)))))

(mevedel-deftest mevedel--clamped-integer ()
  ,test
  (test)

  :doc "keeps in-range integers and clamps out-of-range ones"
  (should (= 500 (mevedel--clamped-integer 500 100 10 1000)))
  (should (= 10 (mevedel--clamped-integer 3 100 10 1000)))
  (should (= 1000 (mevedel--clamped-integer 4000 100 10 1000)))

  :doc "coerces floats and numeric strings"
  (should (= 500 (mevedel--clamped-integer 499.6 100 10 1000)))
  (should (= 500 (mevedel--clamped-integer "500" 100 10 1000)))
  (should (= 500 (mevedel--clamped-integer " 500.0 " 100 10 1000)))

  :doc "falls back to the default for absent or malformed values"
  (should (= 100 (mevedel--clamped-integer nil 100 10 1000)))
  (should (= 100 (mevedel--clamped-integer "fast" 100 10 1000)))
  (should (= 100 (mevedel--clamped-integer t 100 10 1000)))
  (should (= 10 (mevedel--clamped-integer nil 3 10 1000))))

(mevedel-deftest mevedel--normalize-message-text ()
  ,test
  (test)

  :doc "decodes raw UTF-8 bytes into normal Unicode"
  (let* ((raw (test-mevedel-utilities--raw-bytes
               #xe2 #x80 #x9c ?x #xe2 #x80 #x9d))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "“x”" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized)))

  :doc "preserves existing Unicode while decoding raw UTF-8 runs"
  (let* ((raw (concat "lambda λ "
                      (test-mevedel-utilities--raw-bytes
                       #xe2 #x80 #x94)
                      " dash"))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "lambda λ — dash" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized)))

  :doc "escapes invalid raw bytes visibly"
  (let* ((raw (concat "bad "
                      (test-mevedel-utilities--raw-bytes #xff)
                      " byte"))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "bad \\xFF byte" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized))))

(mevedel-deftest mevedel--path-alias-helpers ()
  ,test
  (test)

  :doc "same-file comparison accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (mevedel--same-file-p
               (file-name-concat alias-root "source.el")
               (file-name-concat real-root "source.el")))))

  :doc "directory containment accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (mevedel--file-in-directory-p
               (file-name-concat alias-root "source.el")
               (file-name-as-directory real-root)))))

  :doc "relative-name helper keeps aliased children relative"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      (file-name-concat alias-root "source.el")
                      (file-name-as-directory real-root))))))

  :doc "relative-name helper avoids plain relative paths across aliases"
  (let* ((alias-root (expand-file-name "/alias/root"))
         (real-root (expand-file-name "/real/root"))
         (alias-file (file-name-concat alias-root "source.el"))
         (original-file-in-directory-p
          (symbol-function 'file-in-directory-p)))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root))))))
              ((symbol-function 'file-in-directory-p)
               (lambda (file directory)
                 (or (and (equal (directory-file-name file) alias-file)
                          (equal (directory-file-name directory) real-root))
                     (funcall original-file-in-directory-p file directory)))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      alias-file
                      (file-name-as-directory real-root))))))

  :doc "macOS system volume var aliases stay inside var roots"
  (let ((actual-system-type system-type)
        (system-type 'darwin))
    (should (equal "/var/folders/k8/x/T/root/source.el"
                   (mevedel--file-macos-var-alias
                    "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el")))
    (should
     (equal "/var/folders/k8/x/T/root/source.el"
            (mevedel--file-macos-var-alias
             "/System/Volumes/Data/var/folders/k8/x/T/root/source.el")))
    (unless (eq actual-system-type 'windows-nt)
      (should
       (mevedel--file-in-directory-p
        "/System/Volumes/Data/private/var/folders/k8/x/T/root/.worktrees/foo/"
        "/var/folders/k8/x/T/root/.worktrees/"))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el"
                      "/var/folders/k8/x/T/root/")))))

  :doc "Windows long-name aliases accept trailing directory arguments"
  (let* ((system-type 'windows-nt)
         (short-root (expand-file-name
                      "/runner/RUNNER~1/AppData/Local/Temp/root"))
         (long-root (expand-file-name
                     "/runner/runneradmin/AppData/Local/Temp/root")))
    (cl-letf (((symbol-function 'w32-long-file-name)
               (lambda (file)
                 (unless (string-suffix-p "/" file)
                   (let ((file (directory-file-name file)))
                     (cond
                      ((string-prefix-p short-root file)
                       (concat long-root
                               (substring file (length short-root))))
                      ((string-prefix-p long-root file)
                       file)))))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      (file-name-concat long-root "source.el")
                      (file-name-as-directory short-root))))
      (should
       (mevedel--file-in-directory-p
        (file-name-concat long-root ".worktrees" "foo")
        (file-name-as-directory
         (file-name-concat short-root ".worktrees"))))))

  :doc "relative-name helper leaves outside files absolute"
  (let ((file (expand-file-name "/elsewhere/source.el")))
    (should (equal file
                   (mevedel--file-relative-name-or-absolute
                    file "/real/root/")))))

(mevedel-deftest mevedel--tint ()
  ,test
  (test)
  :doc "resolves noninteractive default-face colors without returning white"
  (should (equal "#ff7f7f" (mevedel--tint "unspecified-bg" "red" 0.5)))
  (should (equal "#7f7f7f" (mevedel--tint "unspecified-fg" "white" 0.5))))

(mevedel-deftest mevedel--environment-info-string ()
  ,test
  (test)
  :doc "renders cached target readiness facts without launching a process"
  (let* ((process-environment
          (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment))
         (target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status ready
            :operating-system "Linux"
            :operating-system-version "6.8.0-target"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected executable lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected target process"))))
      (let ((result
             (mevedel--environment-info-string
              (mevedel-workspace--create
               :root "/ssh:user@host:/srv/project/")
              "/ssh:user@host:/srv/project/lib/"
              target)))
        (should (string-match-p "Working directory: /srv/project/lib/"
                                result))
        (should (string-match-p "Platform: linux" result))
        (should (string-match-p "OS Version: 6.8.0-target" result))))))

(mevedel-deftest mevedel--clear-user-turn-gptel-properties ()
  ,test
  (test)
  :doc "clears assistant metadata from inserted user transcript text"
  (with-temp-buffer
    (insert (propertize "Assistant answer.\n" 'gptel 'response))
    (let ((start (point)))
      (insert (propertize "\nUser follow-up\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (should (eq 'response (get-text-property (point-min) 'gptel)))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "clears copied view/tool properties from user transcript text"
  (with-temp-buffer
    (let ((start (point)))
      (insert (propertize "Bash: git diff\n"
                          'gptel '(tool . "call_1")
                          'read-only t
                          'keymap (make-sparse-keymap)
                          'mevedel-view-source '(1 . 42)
                          'mevedel-view-type 'tool-summary
                          'font-lock-face 'mevedel-view-tool-name))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "preserves atomic mention bindings while clearing copied UI properties"
  (with-temp-buffer
    (let ((start (point))
          (binding '(:kind skill :token "$alpha"
                     :source-file "/tmp/alpha/SKILL.md")))
      (insert (propertize "$alpha"
                          'mevedel-mention-binding binding
                          'gptel 'response
                          'read-only t))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (should (equal binding
                     (get-text-property start 'mevedel-mention-binding)))
      (should-not (get-text-property start 'gptel))
      (should-not (get-text-property start 'read-only))))

  :doc "preserves generated render provenance but not literal marker text"
  (with-temp-buffer
    (let (start block-start block-end literal-start literal-end)
      (setq start (point))
      (insert (propertize "Expanded prompt\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (setq block-start (point))
      (insert (mevedel-tool-render-data-format
               '(:kind inline-skill :name "demo")))
      (setq block-end (point))
      (setq literal-start (point))
      (insert (substring-no-properties
               (mevedel-tool-render-data-format
                '(:kind inline-skill :name "literal"))))
      (setq literal-end (point))
      (set-text-properties literal-start literal-end nil)
      (mevedel--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) block-start)
        (should-not (get-text-property (point) 'gptel))
        (forward-char 1))
      (goto-char block-start)
      (while (< (point) block-end)
        (should (eq t (get-text-property (point) 'mevedel-render-data)))
        (should-not (eq 'response (get-text-property (point) 'gptel)))
        (should-not (get-text-property (point) 'response))
        (should-not (get-text-property (point) 'invisible))
        (should-not (get-text-property (point) 'front-sticky))
        (forward-char 1))
      (goto-char literal-start)
      (search-forward "<!-- mevedel-render-data -->" literal-end)
      (should-not (text-properties-at (match-beginning 0)))
      (should (string-search
               ":name \"literal\""
               (mevedel-tool-render-data-strip
                (buffer-string)))))))

(mevedel-deftest mevedel--hook-audit-helpers ()
  ,test
  (test)

  :doc "formats hook audit blocks with producer-specific provenance"
  (let* ((record
          `(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted ,(propertize
                                "new <!-- /mevedel-hook-audit -->"
                                'face 'bold)
                  :nested (:original ,(propertize "old" 'face 'italic))))
         (block (mevedel--format-hook-audit-record record))
         parsed)
    (should (eq 'mevedel-hook-audit (get-text-property 0 'gptel block)))
    (should (eq t (get-text-property 0 'mevedel-hook-audit block)))
    (should (get-text-property 0 'invisible block))
    (with-temp-buffer
      (insert block)
      (goto-char (point-min))
      (search-forward mevedel--hook-audit-open)
      (let ((body-start (point)))
        (search-forward mevedel--hook-audit-close)
        (let ((payload (buffer-substring-no-properties
                        body-start (match-beginning 0))))
          (should-not (string-match-p
                       "<!-- /mevedel-hook-audit -->"
                       payload))
          (setq parsed (mevedel--read-hook-audit-record payload)))))
    (should-not (text-properties-at 0 (plist-get parsed :submitted)))
    (should-not (text-properties-at
                 0 (plist-get (plist-get parsed :nested) :original))))

  :doc "strips generated hook audit blocks from model-visible text"
  (let ((block (mevedel--format-hook-audit-record
                '(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted "<!-- /mevedel-hook-audit --> tail"))))
    (should (equal "beforeafter"
                   (mevedel--strip-hook-audit-blocks
                    (concat "before" block "after")))))

  :doc "does not authorize property-free copied hook audit blocks"
  (with-temp-buffer
    (insert "before"
            (substring-no-properties
             (mevedel--format-hook-audit-record
              '(:type prompt-rewrite :event "UserPromptSubmit")))
            "after")
    (mevedel-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward mevedel--hook-audit-open)
    (should-not (get-text-property (match-beginning 0) 'gptel)))

  :doc "keeps trailing tool whitespace inside the ignored audit span"
  (with-temp-buffer
    (insert
     (propertize
      (concat
       "(:name \"Read\" :args nil)\n\nresult"
       (mevedel--format-hook-audit-record
        '(:type tool-input-repair :state committed))
       "\n")
      'gptel '(tool . "call-1")))
    (insert (propertize "#+end_tool\nThe next response."
                        'gptel 'ignore))
    (mevedel-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward mevedel--hook-audit-close)
    (while (looking-at-p "[ \t\r\n]")
      (should (eq 'mevedel-hook-audit
                  (get-text-property (point) 'gptel)))
      (should (eq t (get-text-property (point) 'mevedel-hook-audit)))
      (forward-char 1)))

  :doc "builds prompt rewrite audit records only when the prompt changed"
  (should-not
   (mevedel--hook-prompt-rewrite-audit-record
    'UserPromptSubmit "same" "same" "why"))
  (should
   (equal
    '(:type prompt-rewrite
            :event "UserPromptSubmit"
            :original "old"
            :submitted "new"
            :reason "why")
    (mevedel--hook-prompt-rewrite-audit-record
     'UserPromptSubmit "old" "new" "why"))))

(mevedel-deftest mevedel--tag-query-prefix-from-infix ()
  ,test
  (test)
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'foo and not bar or baz'"
  (should (equal '(or (and foo (not bar)) baz)
                 (mevedel--tag-query-prefix-from-infix '(foo and not bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'john or not [jane]'"
  (should (equal '(or john (not [jane]))
                 (mevedel--tag-query-prefix-from-infix '(john or not [jane]))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'alice and bob and charlie'"
  (should (equal '(and alice bob charlie)
                 (mevedel--tag-query-prefix-from-infix '(alice and bob and charlie))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts single tag 'foo'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '(foo))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'foo bar baz not john'"
  (should (equal '(and foo bar baz (not john))
                 (mevedel--tag-query-prefix-from-infix '(foo bar baz not john))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((foo))'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '((foo)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '(((foo)))'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '(((foo))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '(((foo foo foo)))'"
  (should (equal '(and foo foo foo)
                 (mevedel--tag-query-prefix-from-infix '(((foo foo foo))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'not bar and baz'"
  (should (equal '(and (not bar) baz)
                 (mevedel--tag-query-prefix-from-infix '(not bar and baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'bar or bar or baz'"
  (should (equal '(or bar bar baz)
                 (mevedel--tag-query-prefix-from-infix '(bar or bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'bar bar or baz'"
  (should (equal '(or (and bar bar) baz)
                 (mevedel--tag-query-prefix-from-infix '(bar bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts empty list to nil"
  (should (equal nil
                 (mevedel--tag-query-prefix-from-infix '())))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((()))' to nil"
  (should (equal nil
                 (mevedel--tag-query-prefix-from-infix '(((()))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'danny and (joey and boris)'"
  (should (equal '(and danny (and joey boris))
                 (mevedel--tag-query-prefix-from-infix '(danny and (joey and boris)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((danny and (joey and boris)) and (foo or bar))'"
  (should (equal '(and (and danny (and joey boris)) (or foo bar))
                 (mevedel--tag-query-prefix-from-infix '((danny and (joey and boris)) and (foo or bar)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((alice or bob) and (charlie or dave))'"
  (should (equal '(and (or alice bob) (or charlie dave))
                 (mevedel--tag-query-prefix-from-infix '((alice or bob) and (charlie or dave)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((alice and bob) or (charlie and dave))'"
  (should (equal '(or (and alice bob) (and charlie dave))
                 (mevedel--tag-query-prefix-from-infix '((alice and bob) or (charlie and dave)))))
  :doc "Valid infix to prefix conversions:
mixed implicit and explicit conjunctions retain precedence"
  (should (equal '(or (and foo bar baz) (and qux quux))
                 (mevedel--tag-query-prefix-from-infix
                  '(foo bar and baz or qux quux))))
  :doc "Valid infix to prefix conversions:
explicit grouping permits nested negation"
  (should (equal '(not (not foo))
                 (mevedel--tag-query-prefix-from-infix '(not (not foo)))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(not)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(not)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and foo)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and foo)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or foo)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or foo)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and foo or bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and foo or bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or and foo bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or and foo bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and (or foo) bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and (or foo) bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo (or bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo (or bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or (and bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or (and bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo bar and (not))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo bar and (not))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '((or bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '((or bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '((and foo))'"
  (should-error (mevedel--tag-query-prefix-from-infix '((and foo))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or (and))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or (and))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or ())'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or ())))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or not)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or not)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and (and foo bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and (and foo bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or (or(foo and bar)))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar)))))
  :doc "Invalid infix queries:
rejects a nested empty operand"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo and (()))))
  :doc "Invalid infix queries:
rejects consecutive negation without an explicit group"
  (should-error (mevedel--tag-query-prefix-from-infix '(not not foo)))
  :doc "Invalid infix queries:
rejects trailing binary operators"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo and)))
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or))))

(mevedel-deftest mevedel--forget-place ()
  ,test
  (test)
  :doc "keeps a persisted mevedel buffer out of `save-place-alist'"
  (require 'saveplace)
  (let ((save-place-loaded t)
        (save-place-alist nil)
        (default-directory temporary-file-directory))
    (save-place-mode +1)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name
                (file-name-concat temporary-file-directory
                                  "segment-0001.chat.org"))
          (should save-place-mode)
          (mevedel--forget-place)
          (should-not save-place-mode)
          (insert "transcript\n")
          (save-place-to-alist)
          (should-not save-place-alist)
          (setq buffer-file-name nil))
      (save-place-mode -1))))

(mevedel-deftest mevedel-run-helper-capturing-output ()
  ,test
  (test)
  :doc "routes a structured command and declared paths through the helper layer"
  (let ((session (mevedel-session--create))
        captured)
    (cl-letf (((symbol-function 'mevedel-execution-run-helper)
               (lambda (&rest args)
                 (setq captured args)
                 '(:exit-code 7 :output " helper output \n"))))
      (let ((mevedel--session session))
        (should
         (equal '(7 . " helper output \n")
                (mevedel-run-helper-capturing-output
                 "media-helper" '("helper" "--flag") '("/input")
                 '("/artifacts"))))))
    (should (equal '("media-helper" ("helper" "--flag") ("/input")
                     ("/artifacts") :session)
                   (seq-take captured 5)))
    (should (eq session (nth 5 captured)))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured)))))

(mevedel-deftest mevedel-generate-diff ()
  ,test
  (test)
  :doc "runs local snapshot diffing locally despite an ambient remote session"
  (let* ((target
          (mevedel-execution-target-create
           "/ssh:builder@example.test:/srv/project/"))
         (session (mevedel-session--create :execution-target target))
        captured)
    (cl-letf (((symbol-function 'mevedel-execution-run-helper)
               (lambda (&rest args)
                 (setq captured args)
                 '(:exit-code 1 :output "unified diff"))))
      (let ((mevedel--session session))
        (should (equal "unified diff\n"
                       (mevedel-generate-diff
                        "old" "new" "file.el")))))
    (should (equal "diff" (car (nth 1 captured))))
    (should (= 2 (length (nth 2 captured))))
    (should-not (nth 5 captured))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured))))
  :doc "preserves a trailing blank context line in unified output"
  (cl-letf (((symbol-function 'mevedel-execution-run-helper)
             (lambda (&rest _)
               '(:exit-code 1 :output "@@ -1 +1 @@\n-old\n+new\n \n"))))
    (should
     (equal "@@ -1 +1 @@\n-old\n+new\n \n"
            (mevedel-generate-diff "old\n\n" "new\n\n" "file.el")))))

(mevedel-deftest mevedel--write-file-atomically ()
  ,test
  (test)

  :doc "writes content with ordinary file modes, creating the parent"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "deep" "state.eld")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "(:answer 42)\n")
          (should (equal "(:answer 42)\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should (= (file-modes path) (default-file-modes)))
          (should-not (directory-files (file-name-directory path) nil
                                       "mevedel-write")))
      (delete-directory root t)))

  :doc "no-conversion writes literal bytes"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "blob"))
         (bytes (unibyte-string 0 255 10 128)))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path bytes 'no-conversion)
          (should (equal bytes
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally path)
                           (buffer-string)))))
      (delete-directory root t)))

  :doc "a mode argument overrides the default"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "script.sh")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "#!/bin/sh\n" nil #o755)
          (should (= (file-modes path) #o755)))
      (delete-directory root t)))

  :doc "a write that dies leaves the previous content and no staging file"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "state.eld")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "previous\n")
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (error "Disk full"))))
            (should-error (mevedel--write-file-atomically path "next\n")))
          (should (equal "previous\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should-not (directory-files root nil "mevedel-write")))
      (delete-directory root t))))

(mevedel-deftest mevedel--warn-once ()
  ,test
  (test)
  :doc "warns on the first call per key and demotes repeats to messages"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        warnings messages)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type text &rest _) (push (cons type text) warnings)))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (mevedel--warn-once 'test-key "problem %d" 1)
      (mevedel--warn-once 'test-key "problem %d" 2)
      (mevedel--warn-once (list 'test-site "a") "subject a")
      (mevedel--warn-once (list 'test-site "b") "subject b"))
    (should (equal '((mevedel . "problem 1")
                     (mevedel . "subject a")
                     (mevedel . "subject b"))
                   (nreverse warnings)))
    (should (equal '("mevedel: problem 2") messages)))

  :doc "repeats bind `inhibit-message' so the echo area stays untouched"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        inhibited)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _) (setq inhibited inhibit-message))))
      (mevedel--warn-once 'test-key "problem")
      (mevedel--warn-once 'test-key "problem"))
    (should inhibited)))

(mevedel-deftest mevedel--warn-once-reset-site ()
  ,test
  (test)
  :doc "re-arms plain and composite keys only for the requested site"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings)))
              ((symbol-function 'message) #'ignore))
      (mevedel--warn-once 'test-site "site")
      (mevedel--warn-once (list 'test-site "a") "site a")
      (mevedel--warn-once (list 'test-site "b") "site b")
      (mevedel--warn-once (list 'other-site "c") "other c")
      (mevedel--warn-once-reset-site 'test-site)
      (mevedel--warn-once 'test-site "site")
      (mevedel--warn-once (list 'test-site "a") "site a")
      (mevedel--warn-once (list 'other-site "c") "other c"))
    (should (equal '("site" "site a" "site b" "other c" "site" "site a")
                   (nreverse warnings)))))

(mevedel-deftest mevedel--with-gc-batched ()
  ,test
  (test)
  :doc "raises the GC threshold for the dynamic extent of the body"
  (let ((gc-cons-threshold 800000))
    (mevedel--with-gc-batched
      (should (>= gc-cons-threshold (* 64 1024 1024))))
    (should (= gc-cons-threshold 800000)))

  :doc "never lowers an already higher threshold"
  (let ((gc-cons-threshold (* 128 1024 1024)))
    (mevedel--with-gc-batched
      (should (= gc-cons-threshold (* 128 1024 1024))))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
