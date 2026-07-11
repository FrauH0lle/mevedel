;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
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

  :doc "keeps internal render-data blocks ignored inside user turns"
  (with-temp-buffer
    (let (start block-start block-end)
      (setq start (point))
      (insert (propertize "Expanded prompt\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (setq block-start (point))
      (insert (propertize "<!-- mevedel-render-data -->\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (insert (propertize "(:kind inline-skill :name \"demo\")\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (insert (propertize "<!-- /mevedel-render-data -->\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (setq block-end (point))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) block-start)
        (should-not (get-text-property (point) 'gptel))
        (forward-char 1))
      (goto-char block-start)
      (while (< (point) block-end)
        (should (eq 'ignore (get-text-property (point) 'gptel)))
        (should-not (get-text-property (point) 'response))
        (should-not (get-text-property (point) 'invisible))
        (should-not (get-text-property (point) 'front-sticky))
        (forward-char 1)))))

(mevedel-deftest mevedel--hook-audit-helpers ()
  ,test
  (test)

  :doc "formats hook audit blocks as ignored hidden side channels"
  (let* ((record
          `(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted ,(propertize
                                "new <!-- /mevedel-hook-audit -->"
                                'face 'bold)
                  :nested (:original ,(propertize "old" 'face 'italic))))
         (block (mevedel--format-hook-audit-record record))
         parsed)
    (should (eq 'ignore (get-text-property 0 'gptel block)))
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

  :doc "restores ignored properties on copied hook audit blocks"
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
    (should (eq 'ignore
                (get-text-property (match-beginning 0) 'gptel))))

  :doc "keeps trailing tool whitespace inside the ignored audit span"
  (with-temp-buffer
    (insert
     (propertize
      (concat
       "(:name \"Read\" :args nil)\n\nresult"
       (substring-no-properties
        (mevedel--format-hook-audit-record
         '(:type tool-input-repair :state committed)))
       "\n")
      'gptel '(tool . "call-1")))
    (insert (propertize "#+end_tool\nThe next response."
                        'gptel 'ignore))
    (mevedel-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward mevedel--hook-audit-close)
    (while (looking-at-p "[ \t\r\n]")
      (should (eq 'ignore (get-text-property (point) 'gptel)))
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
  (should-error (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar))))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
