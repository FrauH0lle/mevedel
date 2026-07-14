;;; test-mevedel-view-history.el --- Tests for view input history -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-structs)
(require 'mevedel-mention-bindings)
(require 'mevedel-session-persistence)


;;
;;; Test helpers

(defun mevedel-view-history-test--workspace (dir)
  "Return a minimal workspace rooted at DIR."
  (mevedel-workspace--create
   :type 'project
   :id dir
   :root dir
   :name "test"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil
                :total-bytes 0)))

(defun mevedel-view-history-test--session (workspace-dir &optional save-dir)
  "Return a test session with WORKSPACE-DIR and optional SAVE-DIR."
  (mevedel-session--create
   :name "main"
   :workspace (mevedel-view-history-test--workspace workspace-dir)
   :save-path (or save-dir workspace-dir)))

(defun mevedel-view-history-test--path (workspace-dir)
  "Return the workspace input history path for WORKSPACE-DIR."
  (file-name-concat workspace-dir ".mevedel/input-history.el"))

(defun mevedel-view-history-test--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun mevedel-view-history-test--bound-input (text name source-file)
  "Return TEXT with its `$NAME' token bound to SOURCE-FILE."
  (let* ((result (copy-sequence text))
         (token (concat "$" name))
         (start (string-match (regexp-quote token) result)))
    (should start)
    (mevedel-mention-bindings-set
     start (+ start (length token))
     (list :kind 'skill :token token :source-file source-file)
     result)
    result))

(defmacro mevedel-view-history-test--with-view (&rest body)
  "Run BODY with a minimal data/view buffer pair.
Binds `data-buf' and `view-buf'."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(defmacro mevedel-view-history-test--with-temp-dir (dir &rest body)
  "Bind DIR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory
                (make-temp-file "mevedel-view-history-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,dir)
         (delete-directory ,dir t)))))


;;
;;; Ring and commands

(mevedel-deftest mevedel-view-history--ring
  (:doc "adds newest-first entries, trims input, and keeps the newest logical duplicate")
  (let ((mevedel-view-input-history-size 3))
    (with-temp-buffer
      (mevedel-view-history-add "  first  ")
      (mevedel-view-history-add "first")
      (mevedel-view-history-add "")
      (mevedel-view-history-add "second")
      (mevedel-view-history-add "third")
      (mevedel-view-history-add "fourth")
      (should (equal '("fourth" "third" "second")
                     (mevedel-view-history--entries)))
      (let* ((old (mevedel-view-history-test--bound-input
                   "  use $alpha  " "alpha" "/tmp/old/SKILL.md"))
             (new (mevedel-view-history-test--bound-input
                   "  use $alpha  " "alpha" "/tmp/new/SKILL.md")))
        (put-text-property 0 (length old) 'face 'error old)
        (mevedel-view-history-add old)
        (mevedel-view-history-add new)
        (let* ((entries (mevedel-view-history--entries))
               (entry (car entries))
               (token-start (string-match "\\$alpha" entry)))
          (should (= 3 (length entries)))
          (should (equal "use $alpha" entry))
          (should (equal "/tmp/new/SKILL.md"
                         (plist-get
                          (get-text-property
                           token-start 'mevedel-mention-binding entry)
                          :source-file)))
          (should-not (text-property-not-all
                       0 (length entry) 'face nil entry)))))))

(mevedel-deftest mevedel-view-history--navigation
  (:doc "M-p / M-n preserve bindings in recalled and incomplete input")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert (mevedel-view-history-test--bound-input
               "draft with $alpha" "alpha" "/tmp/draft/SKILL.md"))
      (mevedel-view-history-add "first")
      (mevedel-view-history-add
       (mevedel-view-history-test--bound-input
        "second with $alpha" "alpha" "/tmp/sent/SKILL.md"))
      (mevedel-view-history-previous)
      (let* ((input (mevedel-view-history--input-text))
             (start (string-match "\\$alpha" input)))
        (should (equal "second with $alpha" input))
        (should (equal "/tmp/sent/SKILL.md"
                       (plist-get
                        (get-text-property
                         start 'mevedel-mention-binding input)
                        :source-file))))
      (mevedel-view-history-previous)
      (should (equal "first" (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (should (equal "second with $alpha"
                     (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (let* ((input (mevedel-view-history--input-text))
             (start (string-match "\\$alpha" input)))
        (should (equal "draft with $alpha" input))
        (should (equal "/tmp/draft/SKILL.md"
                       (plist-get
                        (get-text-property
                         start 'mevedel-mention-binding input)
                        :source-file)))))))

(mevedel-deftest mevedel-view-history--outside-input-fallback
  (:doc "M-p / M-n outside the input region delegate to global bindings")
  (mevedel-view-history-test--with-view
    (let ((prev nil)
          (next nil)
          (old-prev (lookup-key global-map (kbd "M-p")))
          (old-next (lookup-key global-map (kbd "M-n"))))
      (unwind-protect
          (progn
            (define-key global-map (kbd "M-p")
                        (lambda () (interactive) (setq prev t)))
            (define-key global-map (kbd "M-n")
                        (lambda () (interactive) (setq next t)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (mevedel-view-history-previous)
              (mevedel-view-history-next))
            (should prev)
            (should next))
        (define-key global-map (kbd "M-p") old-prev)
        (define-key global-map (kbd "M-n") old-next)))))

(mevedel-deftest mevedel-view-history--restore-incomplete
  (:doc "M-n restores typed-but-unsent input after history browsing")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "draft")
      (mevedel-view-history-add "sent")
      (mevedel-view-history-previous)
      (should (equal "sent" (mevedel-view-history--input-text)))
      (mevedel-view-history-next)
      (should (equal "draft" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--bol
  (:doc "C-a moves to input start on the prompt line")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "abc")
      (mevedel-view-history-beginning-of-line)
      (should (= (point) (mevedel-view--input-start))))))

(mevedel-deftest mevedel-view-history--clear-input
  (:doc "C-c C-u clears input without touching history")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "abc")
      (mevedel-view-history-clear-input)
      (should (equal "" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--isearch
  (:doc "M-r incrementally searches, cycles matches, and restores on C-g")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "draft")
      (mevedel-view-history-add "match older")
      (mevedel-view-history-add "match newer")
      (let ((unread-command-events
             (append (list ?m ?a ?\M-r ?\r) unread-command-events)))
        (mevedel-view-history-search))
      (should (equal "match older" (mevedel-view-history--input-text)))
      (mevedel-view-history--replace-input "draft")
      (let ((unread-command-events
             (append (list ?m ?\C-g) unread-command-events)))
        (should (condition-case nil
                    (progn
                      (mevedel-view-history-search)
                      nil)
                  (quit t))))
      (should (equal "draft" (mevedel-view-history--input-text)))
      (let ((aborted nil)
            (unread-command-events
             (append (list ?m ?\C-c ?\C-k) unread-command-events)))
        (cl-letf (((symbol-function 'mevedel-view-abort)
                   (lambda () (interactive) (setq aborted t))))
          (mevedel-view-history-search))
        (should aborted))
      (should (equal "draft" (mevedel-view-history--input-text))))))

(mevedel-deftest mevedel-view-history--browse-picker
  (:doc "C-c C-l restores the stored binding behind a plain picker choice")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (mevedel-view-history-add
       (mevedel-view-history-test--bound-input
        "use $alpha" "alpha" "/tmp/browse/SKILL.md"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "use $alpha")))
        (mevedel-view-history-browse))
      (let* ((input (mevedel-view-history--input-text))
             (start (string-match "\\$alpha" input)))
        (should (equal "use $alpha" input))
        (should (equal "/tmp/browse/SKILL.md"
                       (plist-get
                        (get-text-property
                         start 'mevedel-mention-binding input)
                        :source-file)))))))

(mevedel-deftest mevedel-view-history--ret-dispatch
  (:doc "plain RET inserts input newline; display and descriptors keep RET actions")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (should-not (lookup-key mevedel-view-mode-map (kbd "RET")))
      (goto-char (point-min))
      (should (eq (lookup-key (get-text-property (point) 'keymap)
                              (kbd "RET"))
                  #'mevedel-view-activate-at-point))
      (let ((map (make-sparse-keymap))
            (activated nil))
        (define-key map (kbd "RET")
                    (lambda () (interactive) (setq activated t)))
        (mevedel-view--interaction-register
         (list :id "test-ret"
               :kind 'ask
               :body "Descriptor\n"
               :keymap map))
        (goto-char (point-min))
        (search-forward "Descriptor")
        (call-interactively (key-binding (kbd "RET")))
        (should activated)))))


;;
;;; Persistence

(mevedel-deftest mevedel-view-history--persistence
  (:doc "round-trips visible input and its exact skill binding"
   :doc "a write failure retains the live bound entry and disables later saves")
  (mevedel-view-history-test--with-temp-dir workspace-dir
    (mevedel-view-history-test--with-temp-dir session-dir
      (let* ((session (mevedel-view-history-test--session
                       workspace-dir session-dir))
             (path (mevedel-view-history-test--path workspace-dir))
             (session-path (file-name-concat session-dir "input-history.el"))
             (skill-dir (file-name-concat workspace-dir
                                          ".agents/skills/alpha"))
             (skill-file (file-name-concat skill-dir "SKILL.md"))
             (body-marker "ALPHA BODY MUST NOT BE PERSISTED")
             bound-input ref-input)
        (make-directory skill-dir t)
        (with-temp-file skill-file
          (insert "---\nname: alpha\ndescription: Test alpha\n---\n\n# Alpha\n\n"
                  body-marker "\n"))
        (setq bound-input
              (mevedel-view-history-test--bound-input
               "use $alpha." "alpha" skill-file))
        (setq ref-input (copy-sequence "inspect @ref:7"))
        (let ((start (string-match "@ref:7" ref-input)))
          (mevedel-mention-bindings-set
           start (+ start (length "@ref:7"))
           '(:kind ref :token "@ref:7" :reference-uuid "uuid-7")
           ref-input))
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-view-history-add "first")
          (mevedel-view-history-add "second")
          (mevedel-view-history-add "multi\nline \"quoted\"")
          (mevedel-view-history-add
           (concat "raw "
                   (mevedel-view-history-test--raw-bytes
                    #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)))
          (mevedel-view-history-add (concat "unicode " (string #x03bb)))
          (mevedel-view-history-add ref-input)
          (mevedel-view-history-add bound-input)
          (mevedel-view-history-save (current-buffer)))
        (should (file-exists-p path))
        (should-not (file-exists-p session-path))
        (let ((persisted (mevedel-session-persistence-read path)))
          (should (equal 2 (plist-get persisted :version)))
          (should-not
           (string-match-p
            (regexp-quote body-marker)
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string)))))
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-view-history-load session)
          (let* ((entries (mevedel-view-history--entries))
                 (restored (car entries))
                 (token-start (string-match "\\$alpha" restored))
                 (restored-ref (cadr entries))
                 (ref-start (string-match "@ref:7" restored-ref)))
            (should (equal (list "use $alpha."
                                 "inspect @ref:7"
                                 (concat "unicode " (string #x03bb))
                                 "raw “x”"
                                 "multi\nline \"quoted\""
                                 "second"
                                 "first")
                           entries))
            (should (equal (list :kind 'skill
                                 :token "$alpha"
                                 :source-file skill-file)
                           (get-text-property
                            token-start 'mevedel-mention-binding
                            restored)))
            (should
             (equal '(:kind ref :token "@ref:7"
                      :reference-uuid "uuid-7")
                    (get-text-property
                     ref-start 'mevedel-mention-binding restored-ref))))))))
  (mevedel-view-history-test--with-temp-dir workspace-dir
    (let* ((session (mevedel-view-history-test--session workspace-dir))
           (skill-file (file-name-concat workspace-dir "alpha/SKILL.md"))
           (bound-input nil))
      (make-directory (file-name-directory skill-file) t)
      (with-temp-file skill-file
        (insert "---\nname: alpha\ndescription: Alpha\n---\n"))
      (setq bound-input
            (mevedel-view-history-test--bound-input
             "use $alpha" "alpha" skill-file))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add bound-input)
        (cl-letf (((symbol-function 'mevedel-session-persistence-write)
                   (lambda (&rest _) (error "Disk unavailable"))))
          (should-not (mevedel-view-history-save (current-buffer))))
        (should mevedel-view-history--save-failed)
        (should (equal-including-properties
                 bound-input (car (mevedel-view-history--entries))))))))

(mevedel-deftest mevedel-view-history--persistence-merge
  (:doc "saving one view merges existing workspace history from other views"
   :doc "saving from a stale full ring keeps another view's new entry")
  (let ((cases '((:initial nil
                  :expected ("from session two" "from session one"))
                 (:initial ("old-1" "old-2" "old-3" "old-4" "old-5")
                  :one ("from session one")
                  :two "from session two"
                  :expected ("from session two" "from session one"
                             "old-1" "old-2" "old-3"))
                 (:initial ("old-1" "old-2" "old-3" "old-4" "old-5")
                  :one ("one-1" "one-2" "one-3" "one-4" "one-5")
                  :two "two-1"
                  :expected ("two-1" "one-5" "one-4" "one-3" "one-2")))))
    (dolist (case cases)
      (mevedel-view-history-test--with-temp-dir workspace-dir
        (let* ((workspace (mevedel-view-history-test--workspace workspace-dir))
               (session-one (mevedel-session--create
                             :name "one" :workspace workspace
                             :save-path workspace-dir))
               (session-two (mevedel-session--create
                             :name "two" :workspace workspace
                             :save-path workspace-dir))
               (path (mevedel-view-history-test--path workspace-dir))
               (initial (plist-get case :initial))
               (one-entries (or (plist-get case :one)
                                '("from session one")))
               (two-entry (or (plist-get case :two)
                              "from session two"))
               (expected (plist-get case :expected))
               (mevedel-view-input-history-size 5)
               (buf-one (generate-new-buffer " *history-one*"))
               (buf-two (generate-new-buffer " *history-two*")))
          (unwind-protect
              (progn
                (when initial
                  (make-directory (file-name-directory path) t)
                  (mevedel-session-persistence-write
                   path (list :version 2 :entries initial)))
                (with-current-buffer buf-one
                  (setq-local mevedel--session session-one)
                  (mevedel-view-history-load session-one))
                (with-current-buffer buf-two
                  (setq-local mevedel--session session-two)
                  (mevedel-view-history-load session-two))
                (with-current-buffer buf-one
                  (dolist (entry one-entries)
                    (mevedel-view-history-add entry))
                  (mevedel-view-history-save (current-buffer)))
                (with-current-buffer buf-two
                  (mevedel-view-history-add two-entry)
                  (mevedel-view-history-save (current-buffer))
                  (should (equal expected
                                 (mevedel-view-history--entries))))
                (should
                 (equal (with-temp-buffer
                          (insert-file-contents path)
                          (read (current-buffer)))
                        (list :version 2 :entries expected))))
            (when (buffer-live-p buf-one) (kill-buffer buf-one))
            (when (buffer-live-p buf-two) (kill-buffer buf-two))))))))

(mevedel-deftest mevedel-view-history--persistence-corrupt
  (:doc "rejects malformed binding persistence through the corrupt-history path")
  (mevedel-view-history-test--with-temp-dir dir
    (let* ((path (mevedel-view-history-test--path dir))
           (session (mevedel-view-history-test--session dir))
           (malformed-value (copy-sequence "use $alpha"))
           (partial-token (copy-sequence "use $alpha"))
           (extra-character (copy-sequence "use $alpha!"))
           (unsupported-kind (copy-sequence "use $alpha"))
           (unknown-field (copy-sequence "use $alpha"))
           (duplicate-key (copy-sequence "use $alpha"))
           (odd-plist (copy-sequence "use $alpha"))
           (improper-plist (copy-sequence "use $alpha")))
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind skill :name "alpha") malformed-value)
      (put-text-property
       4 9 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       partial-token)
      (put-text-property
       4 11 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       extra-character)
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind file :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       unsupported-kind)
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md"
         :extra t)
       unknown-field)
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md"
         :name "alpha")
       duplicate-key)
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file)
       odd-plist)
      (put-text-property
       4 10 'mevedel-mention-binding
       '(:kind skill :name "alpha" :source-file . "/tmp/alpha/SKILL.md")
       improper-plist)
      (make-directory (file-name-directory path) t)
      (dolist (contents
               (list 'raw-reader-failure
                     '(:version 1 :entries ("old unbound history"))
                     (list :version 2 :entries (list malformed-value))
                     (list :version 2 :entries (list partial-token))
                     (list :version 2 :entries (list extra-character))
                     (list :version 2 :entries (list unsupported-kind))
                     (list :version 2 :entries (list unknown-field))
                     (list :version 2 :entries (list duplicate-key))
                     (list :version 2 :entries (list odd-plist))
                     (list :version 2 :entries (list improper-plist))))
        (if (eq contents 'raw-reader-failure)
            (with-temp-file path
              (insert "not a plist"))
          (mevedel-session-persistence-write path contents))
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-view-history-load session)
          (should (equal nil (mevedel-view-history--entries))))
        (should (file-exists-p (concat path ".bad")))))))

(mevedel-deftest mevedel-view-history--no-workspace
  (:doc "keeps history in memory only when no workspace is available")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-session--create
                    :name "main"
                    :save-path dir)))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (mevedel-view-history-add "first")
        (mevedel-view-history-save (current-buffer)))
      (should-not (file-exists-p
                   (file-name-concat dir ".mevedel/input-history.el")))
      (should-not (file-exists-p
                   (file-name-concat dir "input-history.el"))))))

(mevedel-deftest mevedel-view-history--read-only-attach
  (:doc "does not write input-history.el when attached data buffer is read-only")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (path (mevedel-view-history-test--path dir)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory path) t)
            (with-temp-file path
              (prin1 '(:version 2 :entries ("old")) (current-buffer)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-session--read-only-mode t))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--data-buffer data-buf)
              (mevedel-view-history-load session)
              (should (equal '("old") (mevedel-view-history--entries)))
              (mevedel-view-history-add "first")
              (mevedel-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (read (current-buffer)))
                           '(:version 2 :entries ("old")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-view-history--agent-transcript-no-save
  (:doc "agent transcript views never overwrite the parent input history")
  (mevedel-view-history-test--with-temp-dir dir
    (let ((session (mevedel-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (path (mevedel-view-history-test--path dir)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory path) t)
            (with-temp-file path
              (prin1 '(:version 2 :entries ("parent")) (current-buffer)))
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local gptel-response-separator "\n\n")
              (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
            (mevedel-view--setup
             view-buf data-buf
             (list :agent-transcript-p t
                   :agent-id "agent--1"
                   :transcript-info nil
                   :parent-view nil))
            (with-current-buffer view-buf
              (mevedel-view-history-add "transcript")
              (mevedel-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (read (current-buffer)))
                           '(:version 2 :entries ("parent")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-view-history--rewind-keeps-ring
  (:doc "history ring is buffer-local state and survives view text rewrites")
  (mevedel-view-history-test--with-view
    (with-current-buffer view-buf
      (mevedel-view-history-add "before rewind")
      (let ((inhibit-read-only t))
        (delete-region (point-min) (mevedel-view--input-start))
        (goto-char (point-min))
        (insert "rewound transcript\n"))
      (should (equal '("before rewind")
                     (mevedel-view-history--entries))))))

(provide 'test-mevedel-view-history)
;;; test-mevedel-view-history.el ends here
