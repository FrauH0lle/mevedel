;;; test-mevedel-view-fontify.el --- View fontification tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests quiet generic and reusable Markdown fontification for view text.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-render)


;;
;;; Helpers

(defun mevedel-view-test--late-fontify-mode ()
  "Return `mevedel-view-test-late-mode\=' as the Markdown fontification mode."
  'mevedel-view-test-late-mode)

(define-derived-mode mevedel-view-test-late-mode fundamental-mode "LateFL"
  "Mode that wires `font-lock-defaults\=' only after font-lock has locked in.
Mirrors `markdown-ts-mode\=', whose `outline-minor-mode\=' call lets a user
hook run `font-lock-set-defaults\=' before `treesit-major-mode-setup\='."
  (font-lock-set-defaults)
  (setq-local font-lock-defaults '((("late" . font-lock-keyword-face)))))

(defun mevedel-view-test--context-match (limit)
  "Match a fence, list or table continuation before LIMIT.
The match depends on earlier lines in the same complete buffer."
  (catch 'found
    (while (re-search-forward "^\\(.+\\)$" limit t)
      (let* ((start (match-beginning 0))
             (line (match-string-no-properties 0))
             (context
              (save-match-data
                (save-excursion
                  (goto-char start)
                  (forward-line -1)
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))))
        (when (or (and (string= line "fenced body")
                       (string-prefix-p "```" context))
                  (and (string= line "  list continuation")
                       (string-prefix-p "- item" context))
                  (and (string= line "| table continuation |")
                       (string-prefix-p "| ---" context)))
          (throw 'found t))))))

(define-derived-mode mevedel-view-test-context-mode fundamental-mode "ContextFL"
  "Line fontifier that reads Markdown-like context across region boundaries."
  (setq-local font-lock-defaults
              '(((mevedel-view-test--context-match . font-lock-keyword-face)))))

(defun mevedel-view-test--same-font-lock-faces (actual expected)
  "Assert ACTUAL and EXPECTED have identical text and font-lock faces."
  (should (equal (substring-no-properties actual)
                 (substring-no-properties expected)))
  (dotimes (index (length expected))
    (should (equal (get-text-property index 'font-lock-face actual)
                   (get-text-property index 'font-lock-face expected)))))


;;
;;; Generic fontification

(mevedel-deftest mevedel-view--with-render-temp-buffer ()
  ,test
  (test)

  :doc "suppresses arbitrary major-mode hooks in render temp buffers"
  (let* ((called nil)
         (hook (lambda ()
                 (setq called t))))
    (unwind-protect
        (progn
          (add-hook 'emacs-lisp-mode-hook hook)
          (mevedel-view--with-render-temp-buffer
            (emacs-lisp-mode))
          (should-not called))
      (remove-hook 'emacs-lisp-mode-hook hook)))

  :doc "leaves `hack-local-variables-hook' unbound for mode setup"
  ;; Nothing here visits a file, so the hook never runs, while `sh-mode' and
  ;; friends register on it buffer-locally from their mode body.  Binding it
  ;; made every such call print "Making hack-local-variables-hook
  ;; buffer-local while locally let-bound!" into the messages log.
  (let ((hack-local-variables-hook '(mevedel-view-test--sentinel)))
    (mevedel-view--with-render-temp-buffer
      (should (equal hack-local-variables-hook
                     '(mevedel-view-test--sentinel))))))

(mevedel-deftest mevedel-view--fontify-as
  (:before-each (mevedel-view--release-markdown-fontify-buffer)
   :after-each (mevedel-view--release-markdown-fontify-buffer))
  ,test
  (test)

  :doc "fontifies final diff lines without adding characters to the result"
  (dolist (diff '("@@ -0,0 +1 @@\n+added"
                  "@@ -1 +0,0 @@\n-removed"
                  "@@ -1 +1 @@\n-old\n+new\n"))
    (let ((text (mevedel-view--fontify-as diff 'diff-mode)))
      (should (equal diff (substring-no-properties text)))
      (should (eq (if (string-search "-removed" diff) 'diff-removed 'diff-added)
                  (get-text-property (1- (length text)) 'font-lock-face text)))))

  :doc "the reusable Markdown target fontifies its final unterminated line"
  ;; Diff mode supplies a newline-sensitive fontifier even without TS grammars.
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'diff-mode)))
    (dolist (source '("@@ -0,0 +1 @@\n+added" "@@ -0,0 +1 @@\n+added\n" ""))
      (let ((text (mevedel-view--fontify-as source 'markdown-mode)))
        (should (equal source (substring-no-properties text)))
        (unless (string-empty-p source)
          (should (eq 'diff-added
                      (get-text-property (1- (length text))
                                         'font-lock-face text)))))))

  :doc "fontifies a mode that installs font-lock defaults late"
  ;; `markdown-ts-mode' enables `outline-minor-mode' before
  ;; `treesit-major-mode-setup'; a user hook adding keywords there calls
  ;; `font-lock-set-defaults' against the parent's defaults and the real
  ;; ones never take.
  (let ((text (mevedel-view--fontify-as
               "late\n" 'mevedel-view-test-late-mode)))
    (should (eq 'font-lock-keyword-face
                (get-text-property 0 'font-lock-face text))))

  :doc "leaves Markdown text verbatim when no mode is available"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () nil)))
    (mevedel-view--release-markdown-fontify-buffer)
    (let ((text (mevedel-view--fontify-as "a **b** c" 'markdown-mode)))
      (should (equal "a **b** c" (substring-no-properties text)))
      (should-not (get-text-property 2 'font-lock-face text)))))

(mevedel-deftest mevedel-view--with-quiet-mode-setup ()
  ,test
  (test)

  :doc "suppresses user font-lock hooks while retaining the mode's local hook"
  (let* ((called nil)
         (hook (lambda () (setq called t))))
    (cl-letf (((default-value 'font-lock-mode-hook) (list hook)))
      (with-temp-buffer
        (mevedel-view--with-quiet-mode-setup
          (diff-mode)
          (font-lock-mode -1)
          (should-not called))
        (should (memq 'diff--font-lock-cleanup font-lock-mode-hook)))
      (should (equal (default-value 'font-lock-mode-hook) (list hook)))))

  :doc "repeated diff rendering does not log local hook binding warnings"
  ;; Emacs only warns after the hook has previously been made buffer-local.
  ;; Its C-level message bypasses Lisp `message' interception.
  (let ((message-log-max t)
        (messages (with-current-buffer (messages-buffer) (buffer-string))))
    (unwind-protect
        (progn
          (dotimes (_ 2)
            (mevedel-view--fontify-as "@@ -0,0 +1 @@\n+added" 'diff-mode))
          (with-current-buffer (messages-buffer)
            (should (equal messages (buffer-string)))))
      (with-current-buffer (messages-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert messages))))))


;;
;;; Markdown fontification

(mevedel-deftest mevedel-view--markdown-fontify-job-step
  (:before-each (mevedel-view--release-markdown-fontify-buffer)
   :after-each (mevedel-view--release-markdown-fontify-buffer))
  ,test
  (test)

  :doc "keeps full context and matches synchronous faces across chunk boundaries"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-context-mode)))
    (let* ((text (concat "introduction\n```elisp\nfenced body\n```\n"
                         "- item\n  list continuation\n"
                         "| a | b |\n| --- | --- |\n| table continuation |\n"
                         "last line"))
           (expected (mevedel-view--fontify-as text 'markdown-mode))
           (job (mevedel-view--markdown-fontify-job-start text))
           (ensure (symbol-function 'font-lock-ensure))
           (steps nil))
      (unwind-protect
          (progn
            (should-not (mevedel-view--markdown-fontify-job-result job))
            (cl-letf (((symbol-function 'font-lock-ensure)
                       (lambda (start end)
                         (push (cons start end) steps)
                         (funcall ensure start end))))
              (while (not (mevedel-view--markdown-fontify-job-step job 8))
                (should-not (mevedel-view--markdown-fontify-job-result job))))
            (should (> (length steps) 3))
            (should (cl-every (lambda (bounds)
                                (<= (- (cdr bounds) (car bounds)) 32))
                              steps))
            (mevedel-view-test--same-font-lock-faces
             (mevedel-view--markdown-fontify-job-result job) expected)
            (dolist (fragment '("fenced body" "list continuation"
                                "table continuation"))
              (should (eq 'font-lock-keyword-face
                          (get-text-property
                           (string-match fragment expected)
                           'font-lock-face expected))))
            (should (mevedel-view--markdown-fontify-job-step job 8)))
        (mevedel-view--markdown-fontify-job-cancel job))))

  :doc "interleaved synchronous render cannot overwrite a pending job"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-late-mode)))
    (let* ((text (mapconcat #'identity (make-list 30 "late\n") ""))
           (job (mevedel-view--markdown-fontify-job-start text)))
      (unwind-protect
          (progn
            (should-not (mevedel-view--markdown-fontify-job-step job 5))
            (should (eq 'font-lock-keyword-face
                        (get-text-property
                         0 'font-lock-face
                         (mevedel-view--fontify-as "late" 'markdown-mode))))
            (while (not (mevedel-view--markdown-fontify-job-step job 5)))
            (mevedel-view-test--same-font-lock-faces
             (mevedel-view--markdown-fontify-job-result job)
             (mevedel-view--fontify-as text 'markdown-mode)))
        (mevedel-view--markdown-fontify-job-cancel job))))

  :doc "matches the real Markdown fontifier when both grammars are installed"
  (when (mevedel-view--markdown-fontify-mode)
    (let* ((text (concat "# Header\n\n```elisp\n(+ 1 2)\n```\n\n"
                         "- list\n  - nested\n\n"
                         "| One | Two |\n| --- | --- |\n| A | B |\n"
                         "A **bold** ending"))
           (expected (mevedel-view--fontify-as text 'markdown-mode))
           (job (mevedel-view--markdown-fontify-job-start text)))
      (unwind-protect
          (progn
            (while (not (mevedel-view--markdown-fontify-job-step job 9)))
            (mevedel-view-test--same-font-lock-faces
             (mevedel-view--markdown-fontify-job-result job) expected))
        (mevedel-view--markdown-fontify-job-cancel job))))

  :doc "falls back to unchanged text if fontification raises an error"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-late-mode)))
    (let* ((job (mevedel-view--markdown-fontify-job-start "late\nlate"))
           (buffer (mevedel-view--markdown-fontify-job-buffer job)))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _) (error "Injected fontification failure"))))
        (should (mevedel-view--markdown-fontify-job-step job 3)))
      (should (equal "late\nlate" (mevedel-view--markdown-fontify-job-result job)))
      (should-not (buffer-live-p buffer))
      (mevedel-view--markdown-fontify-job-cancel job)))

  :doc "a nonlocal fontification exit cancels the job and its buffer"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-late-mode)))
    (let* ((job (mevedel-view--markdown-fontify-job-start "late\nlate"))
           (buffer (mevedel-view--markdown-fontify-job-buffer job)))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _) (throw 'stop-fontify t))))
        (should (catch 'stop-fontify
                  (mevedel-view--markdown-fontify-job-step job 3))))
      (should-not (buffer-live-p buffer))
      (should-not (mevedel-view--markdown-fontify-job-result job))
      (should-error (mevedel-view--markdown-fontify-job-step job 3))))

  :doc "a missing Markdown mode completes immediately with original text"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () nil)))
    (let ((job (mevedel-view--markdown-fontify-job-start "**plain**")))
      (should (mevedel-view--markdown-fontify-job-step job 16))
      (should (equal "**plain**" (mevedel-view--markdown-fontify-job-result job)))
      (should-not (mevedel-view--markdown-fontify-job-buffer job))
      (mevedel-view--markdown-fontify-job-cancel job))))

(mevedel-deftest mevedel-view--markdown-fontify-job-start ()
  ,test
  (test)

  :doc "a failed mode setup falls back and releases the private buffer"
  (let (created)
    (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
               (lambda () 'mevedel-view-test-context-mode))
              ((symbol-function 'mevedel-view-test-context-mode)
               (lambda ()
                 (setq created (current-buffer))
                 (error "Injected mode setup failure"))))
      (let ((job (mevedel-view--markdown-fontify-job-start "**plain**")))
        (should (mevedel-view--markdown-fontify-job-step job 8))
        (should (equal "**plain**" (mevedel-view--markdown-fontify-job-result job)))
        (mevedel-view--markdown-fontify-job-cancel job)))
    (should-not (buffer-live-p created)))

  :doc "a nonlocal setup exit also kills its private buffer"
  (let (created)
    (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
               (lambda () 'mevedel-view-test-context-mode))
              ((symbol-function 'mevedel-view-test-context-mode)
               (lambda ()
                 (setq created (current-buffer))
                 (throw 'stop-fontify t))))
      (should (catch 'stop-fontify
                (mevedel-view--markdown-fontify-job-start "text"))))
    (should-not (buffer-live-p created))))

(mevedel-deftest mevedel-view--markdown-fontify-job-cancel ()
  ,test
  (test)

  :doc "cancellation midway releases the buffer and never returns partial text"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-late-mode)))
    (let* ((job (mevedel-view--markdown-fontify-job-start
                 (mapconcat #'identity (make-list 40 "late\n") "")))
           (buffer (mevedel-view--markdown-fontify-job-buffer job)))
      (should-not (mevedel-view--markdown-fontify-job-step job 5))
      (mevedel-view--markdown-fontify-job-cancel job)
      (mevedel-view--markdown-fontify-job-cancel job)
      (should-not (buffer-live-p buffer))
      (should-not (mevedel-view--markdown-fontify-job-result job))
      (should-error (mevedel-view--markdown-fontify-job-step job 5))))

  :doc "cancelling a completed job before extraction discards the result"
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             (lambda () 'mevedel-view-test-late-mode)))
    (let* ((job (mevedel-view--markdown-fontify-job-start "late"))
           (buffer (mevedel-view--markdown-fontify-job-buffer job)))
      (should (mevedel-view--markdown-fontify-job-step job 100))
      (should (buffer-live-p buffer))
      (mevedel-view--markdown-fontify-job-cancel job)
      (should-not (buffer-live-p buffer))
      (should-not (mevedel-view--markdown-fontify-job-result job)))))

(mevedel-deftest mevedel-view--markdown-fontify-mode ()
  ,test
  (test)

  :doc "refuses markdown-ts-mode while a grammar is missing"
  ;; The mode calls `treesit-ensure-installed', which offers to clone and
  ;; compile.  A render must never raise that prompt.
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _) (eq lang 'markdown))))
    (should-not (mevedel-view--markdown-grammars-ready-p))
    (should-not (mevedel-view--markdown-fontify-mode)))

  :doc "selects markdown-ts-mode once both grammars are installed"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (&rest _) t)))
    (should (mevedel-view--markdown-grammars-ready-p))
    (should (eq (and (fboundp 'markdown-ts-mode) 'markdown-ts-mode)
                (mevedel-view--markdown-fontify-mode)))))

(mevedel-deftest mevedel-view--markdown-fontify-target
  (;; The grammars are absent in a test environment, so the pooling itself
   ;; is exercised against a deterministic stand-in mode.  What is under
   ;; test is mevedel's reuse of one buffer, not tree-sitter.
   :before-each
   (progn
     (mevedel-view--release-markdown-fontify-buffer)
     (advice-add 'mevedel-view--markdown-fontify-mode :override
                 #'mevedel-view-test--late-fontify-mode))
   :after-each
   (progn
     (advice-remove 'mevedel-view--markdown-fontify-mode
                    #'mevedel-view-test--late-fontify-mode)
     (mevedel-view--release-markdown-fontify-buffer)))
  ,test
  (test)

  :doc "reuses one buffer across calls instead of rebuilding the mode"
  (let ((first (mevedel-view--markdown-fontify-target)))
    (should (buffer-live-p first))
    (should (eq first (mevedel-view--markdown-fontify-target)))
    (should (eq 'mevedel-view-test-late-mode
                (buffer-local-value 'major-mode first))))

  :doc "rebuilds the buffer after it is killed"
  (let ((first (mevedel-view--markdown-fontify-target)))
    (kill-buffer first)
    (let ((second (mevedel-view--markdown-fontify-target)))
      (should (buffer-live-p second))
      (should-not (eq first second))))

  :doc "fontifies Markdown bodies through the reused buffer"
  (let ((text (mevedel-view--fontify-as "late\n" 'markdown-mode)))
    (should (eq 'font-lock-keyword-face
                (get-text-property 0 'font-lock-face text))))

  :doc "carries no fontification between two texts in the reused buffer"
  ;; The reused buffer is the point of the pool and also its only real
  ;; risk: leftover content or font-lock state would fontify the next
  ;; caller's text against the previous one.
  (progn
    (mevedel-view--fontify-as
     (mapconcat #'identity (make-list 40 "late late late\n") "")
     'markdown-mode)
    (let ((text (mevedel-view--fontify-as "quiet late\n" 'markdown-mode)))
      (should (equal "quiet late\n" (substring-no-properties text)))
      (should-not (get-text-property 0 'font-lock-face text))
      (should (eq 'font-lock-keyword-face
                  (get-text-property 6 'font-lock-face text)))))

  :doc "isolates nested fontification from another view and cleans its buffer"
  (let ((ensure (symbol-function 'font-lock-ensure))
        outer-buffer nested-buffer nested-text entered)
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest args)
                 (if entered
                     (setq nested-buffer (current-buffer))
                   (setq entered t outer-buffer (current-buffer))
                   (with-temp-buffer
                     (setq nested-text
                           (mevedel-view--fontify-as "inner late\n" 'markdown-mode))))
                 (apply ensure args))))
      (let ((text (with-temp-buffer
                    (mevedel-view--fontify-as "late outer\n" 'markdown-mode))))
        (should (equal "late outer\n" (substring-no-properties text)))
        (should (eq 'font-lock-keyword-face
                    (get-text-property 0 'font-lock-face text)))
        (should (equal "inner late\n" (substring-no-properties nested-text)))
        (should (eq 'font-lock-keyword-face
                    (get-text-property 6 'font-lock-face nested-text)))))
    (should-not (eq outer-buffer nested-buffer))
    (should-not (buffer-live-p nested-buffer))
    (should (eq outer-buffer (mevedel-view--markdown-fontify-target))))

  :doc "nested fontification errors leave the outer buffer intact and reusable"
  (let ((ensure (symbol-function 'font-lock-ensure))
        nested-buffer nested-text entered)
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest args)
                 (if entered
                     (progn
                       (setq nested-buffer (current-buffer))
                       (error "Injected nested fontification failure"))
                   (setq entered t)
                   (setq nested-text
                         (mevedel-view--fontify-as "inner late\n" 'markdown-mode))
                   (apply ensure args)))))
      (let ((text (mevedel-view--fontify-as "late outer\n" 'markdown-mode)))
        (should (equal "late outer\n" (substring-no-properties text)))
        (should (eq 'font-lock-keyword-face
                    (get-text-property 0 'font-lock-face text)))
        (should (equal "inner late\n" nested-text))))
    (should-not (buffer-live-p nested-buffer))
    (should (equal "late again\n"
                   (substring-no-properties
                    (mevedel-view--fontify-as "late again\n" 'markdown-mode)))))

  :doc "release during fontification retires the buffer after its owner returns"
  (let ((ensure (symbol-function 'font-lock-ensure)) retired)
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest args)
                 (setq retired (current-buffer))
                 (mevedel-view--release-markdown-fontify-buffer)
                 (apply ensure args))))
      (let ((text (mevedel-view--fontify-as "late outer\n" 'markdown-mode)))
        (should (equal "late outer\n" (substring-no-properties text)))
        (should (eq 'font-lock-keyword-face
                    (get-text-property 0 'font-lock-face text)))))
    (should-not (buffer-live-p retired))
    (should-not mevedel-view--markdown-fontify-buffer)
    (should (buffer-live-p (mevedel-view--markdown-fontify-target))))

  :doc "nested nonlocal exits release all active ownership and temporary buffers"
  (let ((ensure (symbol-function 'font-lock-ensure))
        nested-buffer outer-buffer entered)
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest args)
                 (if entered
                     (progn
                       (setq nested-buffer (current-buffer))
                       (throw 'fontify-stop 'stopped))
                   (setq entered t outer-buffer (current-buffer))
                   (mevedel-view--fontify-as "inner late\n" 'markdown-mode)
                   (apply ensure args)))))
      (should (eq 'stopped
                  (catch 'fontify-stop
                    (mevedel-view--fontify-as "late outer\n" 'markdown-mode)))))
    (should-not (buffer-live-p nested-buffer))
    (should (eq outer-buffer (mevedel-view--markdown-fontify-target)))
    (should (equal "late again\n"
                   (substring-no-properties
                    (mevedel-view--fontify-as "late again\n" 'markdown-mode)))))

  :doc "failed Markdown mode setup does not retain a partial buffer"
  (let ((mode (symbol-function 'mevedel-view-test-late-mode)) failed)
    (cl-letf (((symbol-function 'mevedel-view-test-late-mode)
               (lambda ()
                 (setq failed (current-buffer))
                 (error "Injected mode setup failure"))))
      (should (equal "late\n" (mevedel-view--fontify-as "late\n" 'markdown-mode))))
    (should-not (buffer-live-p failed))
    (should-not mevedel-view--markdown-fontify-buffer)
    (should (eq mode (symbol-function 'mevedel-view-test-late-mode)))
    (should (buffer-live-p (mevedel-view--markdown-fontify-target))))

  :doc "nested mode setup never reuses the partially initialized outer buffer"
  (let ((mode (symbol-function 'mevedel-view-test-late-mode))
        nested-text buffers entered)
    (cl-letf (((symbol-function 'mevedel-view-test-late-mode)
               (lambda ()
                 (push (current-buffer) buffers)
                 (unless entered
                   (setq entered t)
                   (setq nested-text
                         (mevedel-view--fontify-as "inner late\n" 'markdown-mode)))
                 (funcall mode))))
      (let ((text (mevedel-view--fontify-as "late outer\n" 'markdown-mode)))
        (should (equal "late outer\n" (substring-no-properties text)))
        (should (eq 'font-lock-keyword-face
                    (get-text-property 0 'font-lock-face text)))))
    (should (= 2 (length buffers)))
    (should-not (buffer-live-p (car buffers)))
    (should (eq (cadr buffers) (mevedel-view--markdown-fontify-target)))
    (should (equal "inner late\n" (substring-no-properties nested-text))))

  :doc "leaves no buffer behind once released"
  (progn
    (should (buffer-live-p (mevedel-view--markdown-fontify-target)))
    (mevedel-view--release-markdown-fontify-buffer)
    (should-not mevedel-view--markdown-fontify-buffer)
    (should-not (get-buffer " *mevedel-markdown-fontify*"))))

(mevedel-deftest mevedel-view-hide-markdown-markup
  (:before-each (mevedel-view--release-markdown-fontify-buffer)
   :after-each (mevedel-view--release-markdown-fontify-buffer))
  ,test
  (test)

  :doc "the setting reaches the buffer the fontifier runs in"
  ;; What mevedel owns is handing the setting to the mode; the hiding
  ;; itself is `markdown-ts-mode's, and needs its grammars.
  (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
             #'mevedel-view-test--late-fontify-mode))
    (dolist (hide '(t nil))
      (mevedel-view--release-markdown-fontify-buffer)
      (let ((mevedel-view-hide-markdown-markup hide))
        (should (eq hide (buffer-local-value
                          'markdown-ts-hide-markup
                          (mevedel-view--markdown-fontify-target)))))))

  :doc "hidden markup rides along as an invisible property, not a deletion"
  ;; The view hides markup by making it invisible, so every position the
  ;; renderer maps back to the data buffer has to survive untouched.
  (when (mevedel-view--markdown-fontify-mode)
    (let* ((src "# Head\n\nSome **bold** text.\n")
           (mevedel-view-hide-markdown-markup t)
           (out (mevedel-view--fontify-as src 'markdown-mode)))
      (should (equal src (substring-no-properties out)))
      (should (eq 'markdown-ts--markup (get-text-property 0 'invisible out)))
      (should-not (get-text-property (string-match "bold" out)
                                     'invisible out))))

  :doc "shown markup carries no invisible property"
  (when (mevedel-view--markdown-fontify-mode)
    (let* ((src "# Head\n\nSome **bold** text.\n")
           (mevedel-view-hide-markdown-markup nil)
           (out (mevedel-view--fontify-as src 'markdown-mode)))
      (should (equal src (substring-no-properties out)))
      (should-not (text-property-not-all 0 (length out) 'invisible nil out)))))

(provide 'test-mevedel-view-fontify)
;;; test-mevedel-view-fontify.el ends here
