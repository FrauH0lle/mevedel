;;; test-niwa-view-history.el --- Tests for view input history -*- lexical-binding: t -*-

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
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-history)
(require 'niwa-structs)
(require 'niwa-mention-bindings)
(require 'niwa-session-persistence)


;;
;;; Test helpers

(defun niwa-view-history-test--workspace (dir)
  "Return a minimal workspace rooted at DIR."
  (niwa-workspace--create
   :type 'project
   :id dir
   :root dir
   :name "test"
   :file-cache (niwa-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil
                :total-bytes 0)))

(defun niwa-view-history-test--session (workspace-dir &optional save-dir)
  "Return a test session with WORKSPACE-DIR and optional SAVE-DIR."
  (niwa-session--create
   :name "main"
   :workspace (niwa-view-history-test--workspace workspace-dir)
   :save-path (or save-dir workspace-dir)))

(defun niwa-view-history-test--path (workspace-dir)
  "Return the workspace input history path for WORKSPACE-DIR."
  (file-name-concat workspace-dir ".niwa/input-history.el"))

(defun niwa-view-history-test--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun niwa-view-history-test--bound-input (text name source-file)
  "Return TEXT with its `$NAME' token bound to SOURCE-FILE."
  (let* ((result (copy-sequence text))
         (token (concat "$" name))
         (start (string-match (regexp-quote token) result)))
    (should start)
    (niwa-mention-bindings-set
     start (+ start (length token))
     (list :kind 'skill :token token :source-file source-file)
     result)
    result))

(defmacro niwa-view-history-test--with-view (&rest body)
  "Run BODY with a minimal data/view buffer pair.
Binds `data-buf' and `view-buf'."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (niwa--transcript-org-mode)
             (setq-local gptel-response-separator "\n\n"))
           (niwa-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(defmacro niwa-view-history-test--with-temp-dir (dir &rest body)
  "Bind DIR to a temporary directory while running BODY."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory
                (make-temp-file "niwa-view-history-" t))))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,dir)
         (delete-directory ,dir t)))))


;;
;;; Ring and commands

(niwa-deftest niwa-view-history--ring
  (:doc "adds newest-first entries, trims input, and keeps the newest logical duplicate")
  (let ((niwa-view-input-history-size 3))
    (with-temp-buffer
      (niwa-view-history-add "  first  ")
      (niwa-view-history-add "first")
      (niwa-view-history-add "")
      (niwa-view-history-add "second")
      (niwa-view-history-add "third")
      (niwa-view-history-add "fourth")
      (should (equal '("fourth" "third" "second")
                     (niwa-view-history--entries)))
      (let* ((old (niwa-view-history-test--bound-input
                   "  use $alpha  " "alpha" "/tmp/old/SKILL.md"))
             (new (niwa-view-history-test--bound-input
                   "  use $alpha  " "alpha" "/tmp/new/SKILL.md")))
        (put-text-property 0 (length old) 'face 'error old)
        (niwa-view-history-add old)
        (niwa-view-history-add new)
        (let* ((entries (niwa-view-history--entries))
               (entry (car entries))
               (token-start (string-match "\\$alpha" entry)))
          (should (= 3 (length entries)))
          (should (equal "use $alpha" entry))
          (should (equal "/tmp/new/SKILL.md"
                         (plist-get
                          (get-text-property
                           token-start 'niwa-mention-binding entry)
                          :source-file)))
          (should-not (text-property-not-all
                       0 (length entry) 'face nil entry)))))))

(niwa-deftest niwa-view-history--navigation
  (:doc "M-p / M-n preserve bindings in recalled and incomplete input")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf
      (niwa-view-history-add "first")
      (niwa-view-history-add
       (niwa-view-history-test--bound-input
        "second with $alpha" "alpha" "/tmp/sent/SKILL.md")))
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      (niwa-view-composer-insert
       (niwa-view-history-test--bound-input
        "draft with $alpha" "alpha" "/tmp/draft/SKILL.md")
       nil view-buf)
      (niwa-view-history-previous)
      (should-not niwa-view-history--ring)
      (should (eql 0 (buffer-local-value 'niwa-view-history--index view-buf)))
      (let* ((input (niwa-view-history--input-text))
             (start (string-match "\\$alpha" input)))
        (should (equal "second with $alpha" input))
        (should (equal "/tmp/sent/SKILL.md"
                       (plist-get
                        (get-text-property
                         start 'niwa-mention-binding input)
                        :source-file))))
      (niwa-view-history-previous)
      (should (equal "first" (niwa-view-history--input-text)))
      (niwa-view-history-next)
      (should (equal "second with $alpha"
                     (niwa-view-history--input-text)))
      (niwa-view-history-next)
      (let* ((input (niwa-view-history--input-text))
             (start (string-match "\\$alpha" input)))
        (should (equal "draft with $alpha" input))
        (should (equal "/tmp/draft/SKILL.md"
                       (plist-get
                        (get-text-property
                         start 'niwa-mention-binding input)
                        :source-file)))))))

(niwa-deftest niwa-view-history--outside-input-fallback
  (:doc "M-p / M-n outside the input region delegate to global bindings")
  (niwa-view-history-test--with-view
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
              (niwa-view-history-previous)
              (niwa-view-history-next))
            (should prev)
            (should next))
        (define-key global-map (kbd "M-p") old-prev)
        (define-key global-map (kbd "M-n") old-next)))))

(niwa-deftest niwa-view-history--restore-incomplete
  (:doc "M-n restores typed-but-unsent input after history browsing")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf (niwa-view-history-add "sent"))
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      (niwa-view-composer-insert "draft" nil view-buf)
      (niwa-view-history-previous)
      (should (equal "sent" (niwa-view-history--input-text)))
      (niwa-view-history-next)
      (should (equal "draft" (niwa-view-history--input-text))))))

(niwa-deftest niwa-view-history--bol
  (:doc "C-a moves to input start on the prompt line")
  (niwa-view-history-test--with-view
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      (niwa-view-composer-insert "abc" nil view-buf)
      (niwa-view-history-beginning-of-line)
      (should (= (point) (niwa-view-composer-draft-start view-buf))))))

(niwa-deftest niwa-view-history--clear-input
  (:doc "C-c C-u clears input without touching history")
  (niwa-view-history-test--with-view
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      (niwa-view-composer-insert "abc" nil view-buf)
      (niwa-view-history-clear-input)
      (should (equal "" (niwa-view-history--input-text))))))

(niwa-deftest niwa-view-history--isearch
  (:doc "M-r incrementally searches, cycles matches, and restores on C-g")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf
      (niwa-view-history-add "match older")
      (niwa-view-history-add "match newer"))
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      ;; `read-key' updates this global even for simulated input.
      (let ((last-nonmenu-event last-nonmenu-event))
        (niwa-view-composer-insert "draft" nil view-buf)
        (let ((unread-command-events
               (append (list ?m ?a ?\M-r ?\r) unread-command-events)))
          (niwa-view-history-search))
        (should (equal "match older" (niwa-view-history--input-text)))
        (niwa-view-history--replace-input "draft")
        (let ((unread-command-events
               (append (list ?m ?\C-g) unread-command-events)))
          (should (condition-case nil
                      (progn
                        (niwa-view-history-search)
                        nil)
                    (quit t))))
        (should (equal "draft" (niwa-view-history--input-text)))
        (let ((aborted nil)
              (unread-command-events
               (append (list ?m ?\C-c ?\C-k) unread-command-events)))
          (cl-letf (((symbol-function 'niwa-view-abort)
                     (lambda () (interactive) (setq aborted t))))
            (niwa-view-history-search))
          (should aborted))
        (should (equal "draft" (niwa-view-history--input-text)))))))

(niwa-deftest niwa-view-history--browse-picker
  (:doc "C-c C-l recalls a view entry into the native draft only")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf
      (niwa-view-history-add
       (niwa-view-history-test--bound-input
        "use $alpha" "alpha" "/tmp/browse/SKILL.md"))
      (goto-char (point-min))
      (let ((transcript (buffer-string))
            (reader-point (point)))
        (with-current-buffer (niwa-view-composer-buffer)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "use $alpha")))
            (niwa-view-history-browse))
          (let ((input (niwa-view-composer-draft-text)))
            (should (equal "use $alpha" input))
            (should (equal "/tmp/browse/SKILL.md"
                           (plist-get (get-text-property
                                       4 'niwa-mention-binding input)
                                      :source-file)))))
        (should (= reader-point (point)))
        (should (equal transcript (buffer-string)))))))

(niwa-deftest niwa-view-history--ret-dispatch
  (:doc "plain RET inserts input newline; display and descriptors keep RET actions")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf
      (should-not (lookup-key niwa-view-mode-map (kbd "RET")))
      (goto-char (point-min))
      (should (eq (lookup-key (get-text-property (point) 'keymap)
                              (kbd "RET"))
                  #'niwa-view-activate-at-point))
      (let ((map (make-sparse-keymap))
            (activated nil))
        (define-key map (kbd "RET")
                    (lambda () (interactive) (setq activated t)))
        (niwa-view--interaction-register
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

(niwa-deftest niwa-view-history--persistence
  (:doc "persists visible input and exact mention-binding state")
  ,test
  (test)
  :doc "round-trips visible input and its exact mixed mention bindings"
  (niwa-view-history-test--with-temp-dir workspace-dir
    (niwa-view-history-test--with-temp-dir session-dir
      (let* ((session (niwa-view-history-test--session
                       workspace-dir session-dir))
             (path (niwa-view-history-test--path workspace-dir))
             (session-path (file-name-concat session-dir "input-history.el"))
             (skill-dir (file-name-concat workspace-dir
                                          ".agents/skills/alpha"))
             (skill-file (file-name-concat skill-dir "SKILL.md"))
             (body-marker "ALPHA BODY MUST NOT BE PERSISTED")
             bound-input ref-input file-input mcp-input mixed-input)
        (make-directory skill-dir t)
        (with-temp-file skill-file
          (insert "---\nname: alpha\ndescription: Test alpha\n---\n\n# Alpha\n\n"
                  body-marker "\n"))
        (setq bound-input
              (niwa-view-history-test--bound-input
               "use $alpha." "alpha" skill-file))
        (setq ref-input (copy-sequence "inspect @ref:7"))
        (let ((start (string-match "@ref:7" ref-input)))
          (niwa-mention-bindings-set
           start (+ start (length "@ref:7"))
           '(:kind ref :token "@ref:7" :reference-uuid "uuid-7")
           ref-input))
        (setq file-input (copy-sequence "read @file:notes.txt#L2"))
        (let ((start (string-match "@file:" file-input)))
          (niwa-mention-bindings-set
           start (length file-input)
           (list :kind 'file
                 :token "@file:notes.txt#L2"
                 :path (file-name-concat workspace-dir "notes.txt"))
           file-input))
        (setq mcp-input (copy-sequence "consult @mcp:docs:file:///guide"))
        (let ((start (string-match "@mcp:" mcp-input)))
          (niwa-mention-bindings-set
           start (length mcp-input)
           '(:kind mcp :token "@mcp:docs:file:///guide"
             :server "docs" :uri "file:///guide")
           mcp-input))
        (setq mixed-input
              (concat bound-input " " ref-input " " file-input " " mcp-input))
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-view-history-add "first")
          (niwa-view-history-add "second")
          (niwa-view-history-add "multi\nline \"quoted\"")
          (niwa-view-history-add
           (concat "raw "
                   (niwa-view-history-test--raw-bytes
                    #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)))
          (niwa-view-history-add (concat "unicode " (string #x03bb)))
          (niwa-view-history-add ref-input)
          (niwa-view-history-add file-input)
          (niwa-view-history-add mcp-input)
          (niwa-view-history-add bound-input)
          (niwa-view-history-add mixed-input)
          (niwa-view-history-save (current-buffer)))
        (should (file-exists-p path))
        (should-not (file-exists-p session-path))
        (let ((persisted (niwa-session-codec-read path)))
          (should (equal 2 (plist-get persisted :version)))
          (should-not
           (string-match-p
            (regexp-quote body-marker)
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string)))))
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-view-history-load session)
          (let* ((entries (niwa-view-history--entries))
                 (restored-mixed (car entries))
                 (restored (cadr entries))
                 (token-start (string-match "\\$alpha" restored))
                 (restored-mcp (nth 2 entries))
                 (mcp-start (string-match "@mcp:" restored-mcp))
                 (restored-file (nth 3 entries))
                 (file-start (string-match "@file:" restored-file))
                 (restored-ref (nth 4 entries))
                 (ref-start (string-match "@ref:7" restored-ref)))
            (should (equal (list (substring-no-properties mixed-input)
                                 "use $alpha."
                                 "consult @mcp:docs:file:///guide"
                                 "read @file:notes.txt#L2"
                                 "inspect @ref:7"
                                 (concat "unicode " (string #x03bb))
                                 "raw “x”"
                                 "multi\nline \"quoted\""
                                 "second"
                                 "first")
                           entries))
            (should
             (equal (list (list :kind 'skill :token "$alpha"
                                :source-file skill-file)
                          '(:kind ref :token "@ref:7"
                            :reference-uuid "uuid-7")
                          (list :kind 'file :token "@file:notes.txt#L2"
                                :path (file-name-concat
                                       workspace-dir "notes.txt"))
                          '(:kind mcp :token "@mcp:docs:file:///guide"
                            :server "docs" :uri "file:///guide"))
                    (mapcar (lambda (range)
                              (plist-get range :binding))
                            (niwa-mention-bindings-ranges
                             restored-mixed))))
            (should (equal (list :kind 'skill
                                 :token "$alpha"
                                 :source-file skill-file)
                           (get-text-property
                            token-start 'niwa-mention-binding
                            restored)))
            (should
             (equal '(:kind ref :token "@ref:7"
                      :reference-uuid "uuid-7")
                    (get-text-property
                     ref-start 'niwa-mention-binding restored-ref)))
            (should
             (equal '(:kind mcp :token "@mcp:docs:file:///guide"
                      :server "docs" :uri "file:///guide")
                    (get-text-property
                     mcp-start 'niwa-mention-binding restored-mcp)))
            (should
             (equal (list :kind 'file
                          :token "@file:notes.txt#L2"
                          :path (file-name-concat workspace-dir "notes.txt"))
                    (get-text-property
                     file-start 'niwa-mention-binding restored-file))))))))
  :doc "a write failure retains the live bound entry and retries later saves"
  (niwa-view-history-test--with-temp-dir workspace-dir
    (let* ((session (niwa-view-history-test--session workspace-dir))
           (skill-file (file-name-concat workspace-dir "alpha/SKILL.md"))
           (bound-input nil))
      (make-directory (file-name-directory skill-file) t)
      (with-temp-file skill-file
        (insert "---\nname: alpha\ndescription: Alpha\n---\n"))
      (setq bound-input
            (niwa-view-history-test--bound-input
             "use $alpha" "alpha" skill-file))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-view-history-add bound-input)
        (let (diagnostics)
          (cl-letf (((symbol-function 'niwa-session-codec-write)
                     (lambda (&rest _) (error "Disk unavailable"))))
            (niwa-test--with-captured-diagnostics diagnostics
              (niwa-view-history-save (current-buffer))))
          (should (string-match-p "Disk unavailable" diagnostics)))
        (should niwa-view-history--save-failed)
        (should (equal-including-properties
                 bound-input (car (niwa-view-history--entries))))
        (niwa-view-history-save (current-buffer))
        (should-not niwa-view-history--save-failed)
        (should (file-exists-p
                 (niwa-view-history-test--path workspace-dir))))))
  :doc "same visible input persists its newest mention binding"
  (niwa-view-history-test--with-temp-dir workspace-dir
    (let* ((session (niwa-view-history-test--session workspace-dir))
           (old-file (file-name-concat workspace-dir "old/SKILL.md"))
           (new-file (file-name-concat workspace-dir "new/SKILL.md"))
           (old-input (niwa-view-history-test--bound-input
                       "use $alpha" "alpha" old-file))
           (new-input (niwa-view-history-test--bound-input
                       "use $alpha" "alpha" new-file)))
      (dolist (file (list old-file new-file))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert "---\nname: alpha\ndescription: Alpha\n---\n")))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-view-history-add old-input)
        (niwa-view-history-save (current-buffer)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-view-history-load session)
        (niwa-view-history-add new-input)
        (niwa-view-history-save (current-buffer)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-view-history-load session)
        (let* ((entry (car (niwa-view-history--entries)))
               (start (string-match "\\$alpha" entry)))
          (should (equal new-file
                         (plist-get
                          (get-text-property
                           start 'niwa-mention-binding entry)
                          :source-file))))))))

(niwa-deftest niwa-view-history--persistence-merge
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
      (niwa-view-history-test--with-temp-dir workspace-dir
        (let* ((workspace (niwa-view-history-test--workspace workspace-dir))
               (session-one (niwa-session--create
                             :name "one" :workspace workspace
                             :save-path workspace-dir))
               (session-two (niwa-session--create
                             :name "two" :workspace workspace
                             :save-path workspace-dir))
               (path (niwa-view-history-test--path workspace-dir))
               (initial (plist-get case :initial))
               (one-entries (or (plist-get case :one)
                                '("from session one")))
               (two-entry (or (plist-get case :two)
                              "from session two"))
               (expected (plist-get case :expected))
               (niwa-view-input-history-size 5)
               (buf-one (generate-new-buffer " *history-one*"))
               (buf-two (generate-new-buffer " *history-two*")))
          (unwind-protect
              (progn
                (when initial
                  (make-directory (file-name-directory path) t)
                  (niwa-session-codec-write
                   path (list :version 2 :entries initial)))
                (with-current-buffer buf-one
                  (setq-local niwa--session session-one)
                  (niwa-view-history-load session-one))
                (with-current-buffer buf-two
                  (setq-local niwa--session session-two)
                  (niwa-view-history-load session-two))
                (with-current-buffer buf-one
                  (dolist (entry one-entries)
                    (niwa-view-history-add entry))
                  (niwa-view-history-save (current-buffer)))
                (with-current-buffer buf-two
                  (niwa-view-history-add two-entry)
                  (niwa-view-history-save (current-buffer))
                  (should (equal expected
                                 (niwa-view-history--entries))))
                (should
                 (equal (with-temp-buffer
                          (insert-file-contents path)
                          (read (current-buffer)))
                        (list :version 2 :entries expected))))
            (when (buffer-live-p buf-one) (kill-buffer buf-one))
            (when (buffer-live-p buf-two) (kill-buffer buf-two))))))))

(niwa-deftest niwa-view-history-load
  (:quiet t :doc "loads valid history and isolates confirmed corruption")
  ,test
  (test)
  :doc "rejects malformed binding persistence through the corrupt-history path"
  (niwa-view-history-test--with-temp-dir dir
    (let* ((path (niwa-view-history-test--path dir))
           (session (niwa-view-history-test--session dir))
           (malformed-value (copy-sequence "use $alpha"))
           (partial-token (copy-sequence "use $alpha"))
           (extra-character (copy-sequence "use $alpha!"))
           (unsupported-kind (copy-sequence "use $alpha"))
           (unknown-field (copy-sequence "use $alpha"))
           (duplicate-key (copy-sequence "use $alpha"))
           (odd-plist (copy-sequence "use $alpha"))
           (improper-plist (copy-sequence "use $alpha")))
      (put-text-property
       4 10 'niwa-mention-binding
       '(:kind skill :name "alpha") malformed-value)
      (put-text-property
       4 9 'niwa-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       partial-token)
      (put-text-property
       4 11 'niwa-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       extra-character)
      (put-text-property
       4 10 'niwa-mention-binding
       '(:kind file :name "alpha" :source-file "/tmp/alpha/SKILL.md")
       unsupported-kind)
      (put-text-property
       4 10 'niwa-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md"
         :extra t)
       unknown-field)
      (put-text-property
       4 10 'niwa-mention-binding
       '(:kind skill :name "alpha" :source-file "/tmp/alpha/SKILL.md"
         :name "alpha")
       duplicate-key)
      (put-text-property
       4 10 'niwa-mention-binding
       '(:kind skill :name "alpha" :source-file)
       odd-plist)
      (put-text-property
       4 10 'niwa-mention-binding
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
          (niwa-session-codec-write path contents))
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-view-history-load session)
          (should (equal nil (niwa-view-history--entries))))
        (should (file-exists-p (concat path ".bad"))))))
  :doc "keeps valid history outside parse and schema failures"
  (niwa-view-history-test--with-temp-dir dir
    (let* ((path (niwa-view-history-test--path dir))
           (session (niwa-view-history-test--session dir)))
      (make-directory (file-name-directory path) t)
      (dolist (failure '(probe read install))
        (niwa-session-codec-write
         path '(:version 2 :entries ("kept")))
        (with-temp-buffer
          (setq-local niwa--session session)
          (pcase failure
            ('probe
             (let ((exists-p (symbol-function 'file-exists-p)))
               (cl-letf (((symbol-function 'file-exists-p)
                          (lambda (candidate)
                            (if (equal candidate path)
                                (error "Target unavailable")
                              (funcall exists-p candidate)))))
                 (niwa-view-history-load session))))
            ('read
             (cl-letf (((symbol-function
                         'niwa-session-control-fs-read-file)
                        (lambda (&rest _) (error "Target unavailable"))))
               (niwa-view-history-load session)))
            ('install
             (cl-letf (((symbol-function 'niwa-view-history--set-entries)
                        (lambda (&rest _) (error "View unavailable"))))
               (should-error (niwa-view-history-load session)))))
          (should (equal nil (niwa-view-history--entries))))
        (should (file-exists-p path))
        (should-not (file-exists-p (concat path ".bad")))
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-view-history-load session)
          (should (equal '("kept")
                         (niwa-view-history--entries))))))))

(niwa-deftest niwa-view-history--no-workspace
  (:doc "keeps history in memory only when no workspace is available")
  (niwa-view-history-test--with-temp-dir dir
    (let ((session (niwa-session--create
                    :name "main"
                    :save-path dir)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-view-history-add "first")
        (niwa-view-history-save (current-buffer)))
      (should-not (file-exists-p
                   (file-name-concat dir ".niwa/input-history.el")))
      (should-not (file-exists-p
                   (file-name-concat dir "input-history.el"))))))

(niwa-deftest niwa-view-history--read-only-attach
  (:doc "does not write input-history.el when attached data buffer is read-only")
  (niwa-view-history-test--with-temp-dir dir
    (let ((session (niwa-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (path (niwa-view-history-test--path dir)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory path) t)
            (with-temp-file path
              (prin1 '(:version 2 :entries ("old")) (current-buffer)))
            (with-current-buffer data-buf
              (setq-local niwa--session session)
              (setq-local niwa-session--read-only-mode t))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (setq-local niwa--data-buffer data-buf)
              (niwa-view-history-load session)
              (should (equal '("old") (niwa-view-history--entries)))
              (niwa-view-history-add "first")
              (niwa-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (read (current-buffer)))
                           '(:version 2 :entries ("old")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(niwa-deftest niwa-view-history--agent-transcript-no-save
  (:doc "agent transcript views never overwrite the parent input history")
  (niwa-view-history-test--with-temp-dir dir
    (let ((session (niwa-view-history-test--session dir))
          (data-buf (generate-new-buffer " *test-data*"))
          (view-buf (generate-new-buffer " *test-view*"))
          (path (niwa-view-history-test--path dir)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory path) t)
            (with-temp-file path
              (prin1 '(:version 2 :entries ("parent")) (current-buffer)))
            (with-current-buffer data-buf
              (niwa--transcript-org-mode)
              (setq-local niwa--session session)
              (setq-local gptel-response-separator "\n\n"))
            (niwa-view--setup
             view-buf data-buf
             (list :agent-transcript-p t
                   :agent-id "agent--1"
                   :transcript-info nil
                   :parent-view nil))
            (with-current-buffer view-buf
              (niwa-view-history-add "transcript")
              (niwa-view-history-save (current-buffer)))
            (should (equal (with-temp-buffer
                             (insert-file-contents path)
                             (read (current-buffer)))
                           '(:version 2 :entries ("parent")))))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(niwa-deftest niwa-view-history--rewind-keeps-ring
  (:doc "history ring is buffer-local state and survives view text rewrites")
  (niwa-view-history-test--with-view
    (with-current-buffer view-buf
      (niwa-view-history-add "before rewind")
      (let ((inhibit-read-only t))
        (delete-region (point-min) (niwa-view-transcript-tail))
        (goto-char (point-min))
        (insert "rewound transcript\n"))
      (should (equal '("before rewind")
                     (niwa-view-history--entries))))))

(provide 'test-niwa-view-history)
;;; test-niwa-view-history.el ends here
