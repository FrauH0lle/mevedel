;;; test-mevedel-mentions.el --- Tests for mevedel-mentions.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mcp)
(require 'mcp-hub)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-file-state)
(require 'mevedel-overlays)
(require 'mevedel-permissions)
(require 'mevedel-persistence)
(require 'mevedel-mentions)
(require 'mevedel-transcript)
(require 'mevedel-tool-fs)
(require 'mevedel-workspace)

;; Declared in gptel; declared here so `let' binds it dynamically in
;; tests that do not load gptel.
(defvar gptel-default-mode)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helper

(mevedel-deftest mevedel-mentions-replace-with-placeholder ()
  ,test
  (test)
  :doc "preserves the replaced region's gptel property"
  (with-temp-buffer
    (insert (propertize "Use @ref:1 here" 'gptel 'prompt))
    (goto-char (point-min))
    (search-forward "@ref:1")
    (mevedel-mentions-replace-with-placeholder
     (match-beginning 0) (match-end 0) "[ref:1]")
    (goto-char (point-min))
    (search-forward "[ref:1]")
    (should (eq 'prompt
                (get-text-property (match-beginning 0) 'gptel)))))

(defun mevedel-test--make-ref-buffer (content text &optional workspace)
  "Create a live file-visiting buffer with CONTENT and a reference on TEXT.
When WORKSPACE is non-nil, associate the buffer with it.
Returns (buffer . overlay)."
  (let* ((tmp (make-temp-file "mevedel-mention-" nil ".txt" content))
         (buf (find-file-noselect tmp))
         ov)
    (with-current-buffer buf
      (fundamental-mode)
      (when workspace
        (setq-local mevedel--workspace workspace))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (re-search-forward (regexp-quote text))
      (setq ov (mevedel--create-reference-in buf
                                             (match-beginning 0)
                                             (match-end 0))))
    (cons buf ov)))

(defun mevedel-test--reset-instructions ()
  "Reset global and workspace-scoped instruction state for mentions."
  (let (buffers)
    (maphash
     (lambda (_key state)
       (dolist (entry (plist-get state :instructions))
         (when (bufferp (car entry))
           (push (car entry) buffers))))
     mevedel--instruction-states)
    (dolist (buffer (delete-dups buffers))
      (when (buffer-live-p buffer)
        (let ((file (buffer-file-name buffer)))
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil)
            (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (when (and file
                     (file-exists-p file)
                     (file-in-directory-p file temporary-file-directory))
            (delete-file file))))))
  (setq mevedel--instruction-states (make-hash-table :test #'equal))
  (setq mevedel--instruction-current-state-key :global))


;;
;;; Resolution

(mevedel-deftest mevedel--resolve-ref-by-id
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions))
  ,test
  (test)
  :doc "returns nil for unknown id"
  (should (null (mevedel--resolve-ref-by-id 999999)))

  :doc "returns overlay for known reference id"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov)))
    (unwind-protect
        (should (eq ov (mevedel--resolve-ref-by-id id)))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file))))))

(mevedel-deftest mevedel--resolve-ref-by-uuid
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions))
  ,test
  (test)
  :doc "returns only the reference carrying the exact UUID"
  (let* ((cell (mevedel-test--make-ref-buffer "alpha\n" "alpha"))
         (buf (car cell))
         (ref (cdr cell))
         (uuid (overlay-get ref 'mevedel-uuid)))
    (unwind-protect
        (progn
          (should (eq ref (mevedel--resolve-ref-by-uuid uuid)))
          (should-not (mevedel--resolve-ref-by-uuid "missing-uuid")))
      (let ((file (buffer-file-name buf)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (when (file-exists-p file) (delete-file file)))))

  :doc "dispatch restores a stashed reference and reads its current contents"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "restore-ref" :root "/tmp"
                     :name "restore-ref"))
         (cell (mevedel-test--make-ref-buffer
                "alpha body\n" "alpha body" workspace))
         (buffer (car cell))
         (ref (cdr cell))
         (uuid (overlay-get ref 'mevedel-uuid))
         (id (mevedel--instruction-id ref))
         (token (format "@ref:%d" id))
         (session (mevedel-session-create "restore-ref" workspace))
         (file (buffer-file-name buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)
          (let ((result
                 (mevedel--handle-ref-mention
                  (list :match-text token
                        :capture (number-to-string id)
                        :captures (list token (number-to-string id) nil)
                        :binding
                        (list :kind 'ref :token token
                              :reference-uuid uuid)
                        :session session))))
            (should (string-match-p "alpha body"
                                    (plist-get result :reminder)))))
      (mevedel-test--reset-instructions)
      (when (file-exists-p file) (delete-file file)))))

(mevedel-deftest mevedel--resolve-refs-by-tag-query
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions))
  ,test
  (test)
  :doc "limits a live query to its explicit workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "query-a" :root "/tmp" :name "query-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "query-b" :root "/tmp" :name "query-b"))
         (first (mevedel-test--make-ref-buffer
                 "first query body\n" "first query body" workspace-a))
         (second (mevedel-test--make-ref-buffer
                  "second query body\n" "second query body" workspace-b)))
    (overlay-put (cdr first) 'mevedel-reference-tags '(shared))
    (overlay-put (cdr second) 'mevedel-reference-tags '(shared))
    (should (equal (list (cdr first))
                   (mevedel--resolve-refs-by-tag-query
                    "shared" workspace-a)))
    (should (equal (list (cdr second))
                   (mevedel--resolve-refs-by-tag-query
                    "shared" workspace-b)))))

(mevedel-deftest mevedel-mentions-prepare-user-input
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions))
  ,test
  (test)
  :doc "binds known direct references while leaving queries and misses live"
  (let* ((cell (mevedel-test--make-ref-buffer "alpha\n" "alpha"))
         (buf (car cell))
         (ref (cdr cell))
         (id (mevedel--instruction-id ref))
         (uuid (overlay-get ref 'mevedel-uuid))
         (input (format "Use @ref:%d, @ref:{tag}, and @ref:99999" id))
         (prepared (mevedel-mentions-prepare-user-input input))
         (start (string-match (format "@ref:%d" id) prepared))
         (binding (get-text-property start 'mevedel-mention-binding prepared)))
    (unwind-protect
        (progn
          (should (equal 'ref (plist-get binding :kind)))
          (should (equal (format "@ref:%d" id) (plist-get binding :token)))
          (should (equal uuid (plist-get binding :reference-uuid)))
          (should-not (get-text-property
                       (string-match "@ref:{" prepared)
                       'mevedel-mention-binding prepared))
          (should-not (get-text-property
                       (string-match "@ref:99999" prepared)
                       'mevedel-mention-binding prepared))
          (should-not (text-property-not-all
                       0 (length input) 'mevedel-mention-binding nil input)))
      (let ((file (buffer-file-name buf)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (when (file-exists-p file) (delete-file file)))))

  :doc "binds from the session workspace rather than the current buffer"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "prepare-a" :root "/tmp"
                       :name "prepare-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "prepare-b" :root "/tmp"
                       :name "prepare-b"))
         (first (mevedel-test--make-ref-buffer
                 "first body\n" "first body" workspace-a))
         (second (mevedel-test--make-ref-buffer
                  "second body\n" "second body" workspace-b))
         (first-reference (cdr first))
         (second-reference (cdr second))
         (id (mevedel--instruction-id first-reference))
         (session (mevedel-session-create "prepare-a" workspace-a)))
    (should (= id (mevedel--instruction-id second-reference)))
    (with-current-buffer (car second)
      (let* ((prepared (mevedel-mentions-prepare-user-input
                        (format "Use @ref:%d" id) session))
             (binding (get-text-property
                       (string-match "@ref:" prepared)
                       'mevedel-mention-binding prepared)))
        (should (equal (overlay-get first-reference 'mevedel-uuid)
                       (plist-get binding :reference-uuid))))))

  :doc "rejects malformed existing mention bindings"
  (let ((input (copy-sequence "Use @ref:1")))
    (put-text-property 4 10 'mevedel-mention-binding
                       '(:kind ref :token "@ref:1") input)
    (should-error (mevedel-mentions-prepare-user-input input)
                  :type 'user-error)))


;;
;;; Font-lock matchers

(mevedel-deftest mevedel--fontify-ref-id-keyword
  (:doc "`mevedel--fontify-ref-id-keyword' matches @ref:ID at BOB or after whitespace")
  ,test
  (test)
  :doc "matches mention at beginning of buffer"
  (with-temp-buffer
    (insert "@ref:7 text")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-id-keyword (point-max)))
    (should (equal "7" (match-string 1))))

  :doc "matches mention after whitespace"
  (with-temp-buffer
    (insert "some text @ref:42 here")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-id-keyword (point-max)))
    (should (equal "42" (match-string 1)))))

(mevedel-deftest mevedel--fontify-ref-tag-keyword
  (:doc "`mevedel--fontify-ref-tag-keyword' matches @ref:{query}")
  ,test
  (test)
  :doc "matches tag query at beginning of buffer"
  (with-temp-buffer
    (insert "@ref:{core} extra")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-tag-keyword (point-max)))
    (should (equal "core" (match-string 1))))

  :doc "matches tag query after whitespace"
  (with-temp-buffer
    (insert "pre @ref:{core or utils} post")
    (goto-char (point-min))
    (should (mevedel--fontify-ref-tag-keyword (point-max)))
    (should (equal "core or utils" (match-string 1)))))

(mevedel-deftest mevedel--fontify-file-keyword
  (:doc "`mevedel--fontify-file-keyword' matches @file:path tokens")
  ,test
  (test)
  :doc "matches file mention with slashes"
  (with-temp-buffer
    (insert "see @file:/tmp/foo.el here")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "/tmp/foo.el" (match-string 1))))

  :doc "captures path after @file:"
  (with-temp-buffer
    (insert "before @file:lib/core.el after")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "lib/core.el" (match-string 1))))

  :doc "matches @file:path#L<start>-<end> line range"
  (with-temp-buffer
    (insert "see @file:/tmp/foo.el#L10-20 here")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "/tmp/foo.el" (match-string 1))))

  :doc "matches @file:path#L<n> single-line range"
  (with-temp-buffer
    (insert "see @file:/tmp/foo.el#L42 here")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "/tmp/foo.el" (match-string 1))))

  :doc "matches braced @file:{path with spaces} form"
  (with-temp-buffer
    (insert "see @file:{/tmp/foo bar.el} here")
    (goto-char (point-min))
    (should (mevedel--fontify-file-keyword (point-max)))
    (should (equal "{/tmp/foo bar.el}" (match-string 1)))
    (should (equal "/tmp/foo bar.el"
                   (mevedel-mentions--unescape-braced-file-path
                    (match-string 1))))))

(mevedel-deftest mevedel--fontify-agent-keyword
  (:doc "`mevedel--fontify-agent-keyword' matches @agent:name tokens")
  ,test
  (test)
  :doc "matches agent mention at beginning of buffer"
  (with-temp-buffer
    (insert "@agent:explorer please")
    (goto-char (point-min))
    (should (mevedel--fontify-agent-keyword (point-max)))
    (should (equal "explorer" (match-string 1))))

  :doc "matches agent mention after whitespace"
  (with-temp-buffer
    (insert "ask @agent:verifier to help")
    (goto-char (point-min))
    (should (mevedel--fontify-agent-keyword (point-max)))
    (should (equal "verifier" (match-string 1))))

  :doc "matches unknown agent name (highlighting decides face)"
  (with-temp-buffer
    (insert "invoke @agent:nosuch_agent now")
    (goto-char (point-min))
    (should (mevedel--fontify-agent-keyword (point-max)))
    (should (equal "nosuch_agent" (match-string 1)))))

(mevedel-deftest mevedel--fontify-mcp-keyword
  (:doc "`mevedel--fontify-mcp-keyword' matches @mcp:server:uri tokens")
  ,test
  (test)
  :doc "matches mcp mention after whitespace"
  (with-temp-buffer
    (insert "ask @mcp:foo:bar/baz for data")
    (goto-char (point-min))
    (should (mevedel--fontify-mcp-keyword (point-max)))
    (should (equal "foo" (match-string 1)))
    (should (equal "bar/baz" (match-string 2))))

  :doc "captures uri with internal colons (file:// style)"
  (with-temp-buffer
    (insert "see @mcp:srv:file:///etc/hosts here")
    (goto-char (point-min))
    (should (mevedel--fontify-mcp-keyword (point-max)))
    (should (equal "srv" (match-string 1)))
    (should (equal "file:///etc/hosts" (match-string 2)))))


;;
;;; Mention handlers

(mevedel-deftest mevedel--handle-ref-mention
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "ID form: returns placeholder + reminder + content-hash for known ref"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (uuid (overlay-get ov 'mevedel-uuid))
         (id-str (number-to-string id))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (let ((result (mevedel--handle-ref-mention
                       (list :match-text (format "@ref:%d" id)
                             :capture id-str
                             :captures (list (format "@ref:%d" id) id-str nil)))))
          (should (equal (format "[ref:%d -- contents attached above]" id)
                         (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (string-match-p (format "Reference #%d" id)
                                  (plist-get result :reminder)))
          (should (equal (cons 'ref uuid) (plist-get result :key)))
          (should (stringp (plist-get result :hash))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "ID form: missing ref yields graceful placeholder, nil hash"
  (let ((result (mevedel--handle-ref-mention
                 (list :match-text "@ref:99999"
                       :capture "99999"
                       :captures '("@ref:99999" "99999" nil)))))
    (should (string-match-p "\\[ref:99999 -- removed" (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "system annotation" (plist-get result :reminder)))
    (should (equal (cons 'ref 99999) (plist-get result :key)))
    (should (null (plist-get result :hash))))

  :doc "unbound ID form never resolves from another session workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "dispatch-a" :root "/tmp"
                       :name "dispatch-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "dispatch-b" :root "/tmp"
                       :name "dispatch-b"))
         (other (mevedel-test--make-ref-buffer
                 "other workspace body\n" "other workspace body" workspace-b))
         (id (mevedel--instruction-id (cdr other)))
         (token (format "@ref:%d" id))
         (session (mevedel-session-create "dispatch-a" workspace-a))
         (result (mevedel--handle-ref-mention
                  (list :match-text token
                        :capture (number-to-string id)
                        :captures (list token (number-to-string id) nil)
                        :session session))))
    (should (string-match-p "removed" (plist-get result :placeholder)))
    (should-not (string-match-p "other workspace body"
                                (plist-get result :reminder))))

  :doc "bound ID form: deletion plus number reuse never retargets"
  (let* ((first (mevedel-test--make-ref-buffer "alpha body\n" "alpha body"))
         (first-buf (car first))
         (first-ref (cdr first))
         (id (mevedel--instruction-id first-ref))
         (uuid (overlay-get first-ref 'mevedel-uuid))
         (token (format "@ref:%d" id))
         (binding (list :kind 'ref :token token :reference-uuid uuid))
         second)
    (unwind-protect
        (progn
          (mevedel--delete-instruction first-ref first-buf)
          (setq second
                (mevedel-test--make-ref-buffer "replacement body\n"
                                               "replacement body"))
          (should (= id (mevedel--instruction-id (cdr second))))
          (let ((bound-result
                 (mevedel--handle-ref-mention
                  (list :match-text token
                        :capture (number-to-string id)
                        :captures (list token (number-to-string id) nil)
                        :binding binding)))
                (live-result
                 (mevedel--handle-ref-mention
                  (list :match-text token
                        :capture (number-to-string id)
                        :captures (list token (number-to-string id) nil)))))
            (should (equal (cons 'ref uuid) (plist-get bound-result :key)))
            (should (string-match-p "unavailable"
                                    (plist-get bound-result :placeholder)))
            (should (stringp (plist-get bound-result :warning)))
            (should-not (string-match-p "replacement body"
                                        (plist-get bound-result :reminder)))
            (should (string-match-p "replacement body"
                                    (plist-get live-result :reminder)))))
      (dolist (buf (delq nil (list first-buf (car-safe second))))
        (when (buffer-live-p buf)
          (let ((file (buffer-file-name buf)))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)
            (when (and file (file-exists-p file))
              (delete-file file)))))))

  :doc "tag form: no-match query yields graceful placeholder, nil hash"
  (let ((result (mevedel--handle-ref-mention
                 (list :match-text "@ref:{nosuchtag}"
                       :capture nil
                       :captures '("@ref:{nosuchtag}" nil "nosuchtag")))))
    (should (equal "[ref:{nosuchtag} -- no matches]"
                   (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "system annotation" (plist-get result :reminder)))
    (should (equal (cons 'ref-tag "nosuchtag") (plist-get result :key)))
    (should (null (plist-get result :hash)))))

(mevedel-deftest mevedel--handle-file-mention
  (:doc "`mevedel--handle-file-mention' handles files, dirs, missing, and readable")
  ,test
  (test)
  :doc "missing file yields graceful placeholder with explanatory reminder"
  (let* ((path "/nonexistent/path/to/file.txt")
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" path)
                        :capture path))))
    (should (string-match-p "does not exist" (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "system annotation" (plist-get result :reminder)))
    (should (null (plist-get result :hash))))

  :doc "directory yields listing placeholder and reminder with entries"
  (let* ((dir (make-temp-file "mevedel-dir-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "alpha.txt" dir) (insert "a"))
          (with-temp-file (expand-file-name "beta.txt" dir) (insert "b"))
          (let ((result (mevedel--handle-file-mention
                         (list :match-text (concat "@file:" dir)
                               :capture dir
                               :workspace-root temporary-file-directory))))
            (should (string-match-p "directory listing attached"
                                    (plist-get result :placeholder)))
            (should (stringp (plist-get result :reminder)))
            (should (string-match-p "alpha.txt" (plist-get result :reminder)))
            (should (string-match-p "beta.txt" (plist-get result :reminder)))
            (should (equal (cons 'dir dir) (plist-get result :key)))))
      (delete-directory dir t)))

  :doc "directory listing is truncated when it exceeds the max cap"
  (let* ((dir (make-temp-file "mevedel-dir-" t))
         (mevedel-file-mention-directory-max-entries 3))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (expand-file-name (format "f%d.txt" i) dir)
              (insert "x")))
          (let ((result (mevedel--handle-file-mention
                         (list :match-text (concat "@file:" dir)
                               :capture dir
                               :workspace-root temporary-file-directory))))
            (should (string-match-p "truncated at 3"
                                    (plist-get result :reminder)))))
      (delete-directory dir t)))

  :doc "directory listing respects permission denial"
  (let* ((ws-root (make-temp-file "mevedel-ws-" t))
         (outside-dir (make-temp-file "mevedel-outside-" t))
         (ws (mevedel-workspace--create :type 'project :id "dir-deny"
                                        :root ws-root :name "dir-deny"))
         (session (mevedel-session-create "main" ws))
         (mevedel-permission-rules nil))
    (unwind-protect
        (let ((result (mevedel--handle-file-mention
                       (list :match-text (concat "@file:" outside-dir)
                             :capture outside-dir
                             :session session
                             :workspace-root ws-root))))
          (should (string-match-p "permission denied"
                                  (plist-get result :placeholder))))
      (delete-directory outside-dir t)
      (delete-directory ws-root t)))

  :doc "readable file yields placeholder, reminder, and content hash"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt" "hello world\n"))
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" tmp)
                        :capture tmp
                        :workspace-root temporary-file-directory))))
    (unwind-protect
        (progn
          (should (equal (format "[file:%s -- contents attached above]" tmp)
                         (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (string-match-p "hello world" (plist-get result :reminder)))
          (should (string-match-p "Do not call Read"
                                  (plist-get result :reminder)))
          (should (equal (cons 'file (cons tmp "")) (plist-get result :key)))
          (should (stringp (plist-get result :hash))))
      (delete-file tmp)))

  :doc "media file on unsupported model yields graceful placeholder"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".png" "not really png\n"))
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" tmp)
                        :capture tmp
                        :workspace-root temporary-file-directory))))
    (unwind-protect
        (progn
          (should (string-match-p "model does not support image/png media"
                                  (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (null (plist-get result :hash))))
      (delete-file tmp)))

  :doc "large PDF media mention emits bounded-page reminder"
  (let ((tmp (make-temp-file "mevedel-file-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (cap &optional _model) (eq cap 'media)))
                  ((symbol-function 'gptel--model-mime-capable-p)
                   (lambda (mime &optional _model)
                     (equal mime "application/pdf")))
                  ((symbol-function 'mevedel-tool-fs--large-pdf-p)
                   (lambda (_path) t))
                  ((symbol-function 'mevedel-tool-fs--format-large-pdf-reminder)
                   (lambda (_path) "large PDF guidance")))
          (let ((result (mevedel--handle-file-mention
                         (list :match-text (concat "@file:" tmp)
                               :capture tmp
                               :workspace-root temporary-file-directory))))
            (should (string-match-p "media attached"
                                    (plist-get result :placeholder)))
            (should (equal (list tmp "application/pdf")
                           (plist-get result :media-context)))
            (should (string-match-p "large PDF guidance"
                                    (plist-get result :reminder)))))
      (delete-file tmp)))

  :doc "oversized PDF media mention includes bounded-page guidance"
  (let ((tmp (make-temp-file "mevedel-file-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((mevedel-tool-fs--media-max-bytes 1))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "application/pdf")))
                    ((symbol-function
                      'mevedel-tool-fs--format-large-pdf-reminder)
                     (lambda (_path) "large PDF guidance")))
            (let ((result (mevedel--handle-file-mention
                           (list :match-text (concat "@file:" tmp)
                                 :capture tmp
                                 :workspace-root temporary-file-directory))))
              (should (string-match-p "media file is too large"
                                      (plist-get result :placeholder)))
              (should (string-match-p "large PDF guidance"
                                      (plist-get result :reminder)))
              (should (null (plist-get result :hash))))))
      (delete-file tmp)))

  :doc "braced path form reads files whose names contain spaces"
  (let* ((dir (make-temp-file "mevedel-file-dir-" t))
         (tmp (expand-file-name "space name.txt" dir))
         (token (format "{%s}" tmp))
         result)
    (with-temp-file tmp (insert "hello spaced path\n"))
    (setq result
          (mevedel--handle-file-mention
           (list :match-text (concat "@file:" token)
                 :capture token
                 :captures (list (concat "@file:" token) token nil nil)
                 :workspace-root temporary-file-directory)))
    (unwind-protect
        (progn
          (should (string-match-p "contents attached"
                                  (plist-get result :placeholder)))
          (should (string-match-p "hello spaced path"
                                  (plist-get result :reminder))))
      (delete-directory dir t)))

  :doc "records file read on session so Read tool can dedupe"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create :type 'project :id "mention-dedup"
                                        :root temporary-file-directory
                                        :name "mention-dedup"))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (progn
          (mevedel--handle-file-mention
           (list :match-text (concat "@file:" tmp)
                 :capture tmp
                 :session session
                 :workspace-root temporary-file-directory))
          (let ((entry (gethash (expand-file-name tmp)
                                (mevedel-session-touched-files session))))
            (should entry)
            (should (mevedel-file-interaction-read-turn entry))))
      (delete-file tmp)))

  :doc "permission deny yields denial placeholder"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create :type 'project :id "mention-deny"
                                        :root temporary-file-directory
                                        :name "mention-deny"))
         (session (mevedel-session-create "main" ws))
         ;; Session deny rule matching the exact path
         (mevedel-permission-rules nil))
    (mevedel-permission--add-session-rule session "Read" 'deny tmp)
    (unwind-protect
        (let ((result (mevedel--handle-file-mention
                       (list :match-text (concat "@file:" tmp)
                             :capture tmp
                             :session session
                             :workspace-root temporary-file-directory))))
          (should (string-match-p "permission denied"
                                  (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (string-match-p "system annotation"
                                  (plist-get result :reminder)))
          (should (null (plist-get result :hash))))
      (delete-file tmp)))

  :doc "path outside workspace without covering rule is rejected"
  (let* ((ws-root (make-temp-file "mevedel-ws-" t))
         (outside-dir (make-temp-file "mevedel-outside-" t))
         (tmp (expand-file-name "outside.txt" outside-dir))
         (ws (mevedel-workspace--create :type 'project :id "mention-outside"
                                        :root ws-root
                                        :name "mention-outside"))
         (session (mevedel-session-create "main" ws))
         (mevedel-permission-rules nil))
    (with-temp-file tmp (insert "secret\n"))
    (unwind-protect
        (let ((result (mevedel--handle-file-mention
                       (list :match-text (concat "@file:" tmp)
                             :capture tmp
                             :session session
                             :workspace-root ws-root))))
          (should (string-match-p "permission denied"
                                  (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (null (plist-get result :hash))))
      (delete-file tmp)
      (delete-directory outside-dir)
      (delete-directory ws-root)))

  :doc "active dropped-file grant allows exact outside-workspace file"
  (let* ((ws-root (make-temp-file "mevedel-ws-" t))
         (outside-dir (make-temp-file "mevedel-outside-" t))
         (tmp (expand-file-name "outside.txt" outside-dir))
         (ws (mevedel-workspace--create :type 'project :id "mention-drop"
                                        :root ws-root
                                        :name "mention-drop"))
         (session (mevedel-session-create "main" ws))
         (mevedel-permission-rules nil))
    (with-temp-file tmp (insert "dropped secret\n"))
    (mevedel-session-activate-dropped-file-grants session (list tmp))
    (unwind-protect
        (let ((result (mevedel--handle-file-mention
                       (list :match-text (concat "@file:" tmp)
                             :capture tmp
                             :session session
                             :workspace-root ws-root))))
          (should (string-match-p "contents attached"
                                  (plist-get result :placeholder)))
          (should (string-match-p "dropped secret"
                                  (plist-get result :reminder))))
      (delete-file tmp)
      (delete-directory outside-dir)
      (delete-directory ws-root)))

  :doc "active dropped-file grant does not override explicit deny rule"
  (let* ((ws-root (make-temp-file "mevedel-ws-" t))
         (outside-dir (make-temp-file "mevedel-outside-" t))
         (tmp (expand-file-name "outside.txt" outside-dir))
         (ws (mevedel-workspace--create :type 'project :id "mention-drop-deny"
                                        :root ws-root
                                        :name "mention-drop-deny"))
         (session (mevedel-session-create "main" ws))
         (mevedel-permission-rules nil))
    (with-temp-file tmp (insert "dropped secret\n"))
    (mevedel-session-activate-dropped-file-grants session (list tmp))
    (mevedel-permission--add-session-rule session "Read" 'deny tmp)
    (unwind-protect
        (let ((result (mevedel--handle-file-mention
                       (list :match-text (concat "@file:" tmp)
                             :capture tmp
                             :session session
                             :workspace-root ws-root))))
          (should (string-match-p "permission denied"
                                  (plist-get result :placeholder)))
          (should (null (plist-get result :hash))))
      (delete-file tmp)
      (delete-directory outside-dir)
      (delete-directory ws-root)))

  :doc "line-range suffix reads only the requested lines"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt"
                              "line1\nline2\nline3\nline4\nline5\n"))
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" tmp "#L2-3")
                        :capture tmp
                        :captures (list (concat "@file:" tmp "#L2-3")
                                        tmp "2" "3")
                        :workspace-root temporary-file-directory))))
    (unwind-protect
        (let ((reminder (plist-get result :reminder))
              (placeholder (plist-get result :placeholder)))
          (should (string-match-p (format "\\[file:%s#L2-3 -- contents attached above\\]"
                                          (regexp-quote tmp))
                                  placeholder))
          (should (stringp reminder))
          (should (string-match-p "line2" reminder))
          (should (string-match-p "line3" reminder))
          (should-not (string-match-p "line1" reminder))
          (should-not (string-match-p "line5" reminder))
          (should (string-match-p "Lines 2-3 of" reminder)))
      (delete-file tmp)))

  :doc "single-line suffix reads exactly one line"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt"
                              "line1\nline2\nline3\n"))
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" tmp "#L2")
                        :capture tmp
                        :captures (list (concat "@file:" tmp "#L2")
                                        tmp "2" nil)
                        :workspace-root temporary-file-directory))))
    (unwind-protect
        (let ((reminder (plist-get result :reminder)))
          (should (string-match-p "line2" reminder))
          (should-not (string-match-p "line1" reminder))
          (should-not (string-match-p "line3" reminder))
          (should (string-match-p "Line 2 of" reminder)))
      (delete-file tmp)))

  :doc "whole-file and range reads have independent dedup keys"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".txt" "line1\nline2\n"))
         (whole (mevedel--handle-file-mention
                 (list :match-text (concat "@file:" tmp)
                       :capture tmp
                       :captures (list (concat "@file:" tmp) tmp nil nil)
                       :workspace-root temporary-file-directory)))
         (range (mevedel--handle-file-mention
                 (list :match-text (concat "@file:" tmp "#L1")
                       :capture tmp
                       :captures (list (concat "@file:" tmp "#L1")
                                       tmp "1" nil)
                       :workspace-root temporary-file-directory))))
    (unwind-protect
        (should-not (equal (plist-get whole :key) (plist-get range :key)))
      (delete-file tmp))))

(mevedel-deftest mevedel--handle-agent-mention
  (:doc "`mevedel--handle-agent-mention' delegates to registered agents")
  ,test
  (test)
  :doc "known agent yields delegation placeholder and invocation hint"
  (let ((result (mevedel--handle-agent-mention
                 (list :match-text "@agent:explorer"
                       :capture "explorer"))))
    (should (equal "[agent:explorer -- delegation requested]"
                   (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "subagent_type=\"explorer\""
                            (plist-get result :reminder)))
    (should (string-match-p "Do not mention this reminder"
                            (plist-get result :reminder)))
    (should (equal (cons 'agent "explorer") (plist-get result :key)))
    (should (stringp (plist-get result :hash))))

  :doc "unknown agent yields graceful placeholder, nil hash"
  (let ((result (mevedel--handle-agent-mention
                 (list :match-text "@agent:definitely_not_an_agent"
                       :capture "definitely_not_an_agent"))))
    (should (string-match-p "no such agent"
                            (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "no agent by that name"
                            (plist-get result :reminder)))
    (should (string-match-p "system annotation"
                            (plist-get result :reminder)))
    (should (null (plist-get result :hash)))))

(mevedel-deftest mevedel-agent-capf
  (:doc "`mevedel-agent-capf' completes @agent: against registered agents")
  ,test
  (test)
  :doc "returns registered agent names as candidates at @agent: point"
  (with-temp-buffer
    (insert "@agent:")
    (goto-char (point-max))
    (let ((result (mevedel-agent-capf)))
      (should result)
      (let ((candidates (nth 2 result)))
        (should (member "explorer" candidates))
        (should (member "verifier" candidates)))))

  :doc "returns nil when not at an @agent: prefix"
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-max))
    (should (null (mevedel-agent-capf)))))

(mevedel-deftest mevedel--handle-mcp-mention
  (:doc "`mevedel--handle-mcp-mention' reads MCP resources when server is connected")
  ,test
  (test)
  :doc "mcp.el unavailable yields graceful placeholder"
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat &rest _) (not (eq feat 'mcp)))))
    (let ((result (mevedel--handle-mcp-mention
                   (list :match-text "@mcp:srv:uri/x"
                         :captures '("@mcp:srv:uri/x" "srv" "uri/x")))))
      (should (string-match-p "mcp.el not available"
                              (plist-get result :placeholder)))
      (should (null (plist-get result :hash)))))

  :doc "unknown server yields graceful placeholder"
  (cl-letf (((symbol-function 'mcp-hub-get-servers) (lambda () nil)))
    (let ((result (mevedel--handle-mcp-mention
                   (list :match-text "@mcp:nosuch:uri"
                         :captures '("@mcp:nosuch:uri" "nosuch" "uri")))))
      (should (string-match-p "unknown server"
                              (plist-get result :placeholder)))
      (should (null (plist-get result :hash)))))

  :doc "disconnected server yields graceful placeholder"
  (cl-letf (((symbol-function 'mcp-hub-get-servers)
             (lambda () (list (list :name "srv" :status 'stop)))))
    (let ((result (mevedel--handle-mcp-mention
                   (list :match-text "@mcp:srv:uri"
                         :captures '("@mcp:srv:uri" "srv" "uri")))))
      (should (string-match-p "not connected"
                              (plist-get result :placeholder)))
      (should (null (plist-get result :hash)))))

  :doc "successful read attaches content and computes hash"
  (let ((fake-conn (make-hash-table))
        (table (make-hash-table :test #'equal)))
    (puthash "srv" fake-conn table)
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () (list (list :name "srv" :status 'connected))))
              ((symbol-function 'mcp-read-resource)
               (lambda (_conn _uri)
                 (list :contents (vector (list :type "text"
                                               :text "hello resource"))))))
      (let* ((mcp-server-connections table)
             (result (mevedel--handle-mcp-mention
                      (list :match-text "@mcp:srv:uri/x"
                            :captures '("@mcp:srv:uri/x" "srv" "uri/x")))))
        (should (equal "[mcp:srv:uri/x -- contents attached above]"
                       (plist-get result :placeholder)))
        (should (string-match-p "hello resource"
                                (plist-get result :reminder)))
        (should (equal (cons 'mcp (cons "srv" "uri/x"))
                       (plist-get result :key)))
        (should (stringp (plist-get result :hash))))))

  :doc "read failure yields graceful placeholder"
  (let ((fake-conn (make-hash-table))
        (table (make-hash-table :test #'equal)))
    (puthash "srv" fake-conn table)
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () (list (list :name "srv" :status 'connected))))
              ((symbol-function 'mcp-read-resource)
               (lambda (_conn _uri) (signal 'error '("boom")))))
      (let* ((mcp-server-connections table)
             (result (mevedel--handle-mcp-mention
                      (list :match-text "@mcp:srv:bad"
                            :captures '("@mcp:srv:bad" "srv" "bad")))))
        (should (string-match-p "read failed: boom"
                                (plist-get result :placeholder)))
        (should (null (plist-get result :hash)))))))

(mevedel-deftest mevedel-mentions--ref-completion-exit-function
  (:doc "`mevedel-mentions--ref-completion-exit-function' binds a UUID")
  ,test
  (test)
  :doc "binds the completed visible token to the candidate UUID"
  (with-temp-buffer
    (insert "@ref:7")
    (let ((candidate (propertize "7" 'mevedel-reference-uuid "uuid-7")))
      (mevedel-mentions--ref-completion-exit-function
       (list candidate) "7" 'finished)
      (should
       (equal '(:kind ref :token "@ref:7" :reference-uuid "uuid-7")
              (get-text-property (point-min) 'mevedel-mention-binding))))))

(mevedel-deftest mevedel-ref-capf
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (mevedel-test--reset-instructions))
  ,test
  (test)
  :doc "direct reference completion binds the selected UUID immediately"
  (let* ((cell (mevedel-test--make-ref-buffer "alpha\n" "alpha"))
         (buf (car cell))
         (ref (cdr cell))
         (id-string (number-to-string (mevedel--instruction-id ref)))
         (uuid (overlay-get ref 'mevedel-uuid))
         (state-key
          (mevedel--instruction-workspace-key
           (mevedel--instruction-buffer-workspace buf))))
    (unwind-protect
        (with-temp-buffer
          (insert "@ref:")
          (let* ((mevedel--instruction-state-key-override state-key)
                 (capf (mevedel-ref-capf))
                 (start (nth 0 capf))
                 (end (nth 1 capf))
                 (exit (plist-get (nthcdr 3 capf) :exit-function)))
            (should (functionp exit))
            (delete-region start end)
            (goto-char start)
            (insert id-string)
            (funcall exit id-string 'finished)
            (let ((binding (get-text-property
                            (point-min) 'mevedel-mention-binding)))
              (should (equal 'ref (plist-get binding :kind)))
              (should (equal (concat "@ref:" id-string)
                             (plist-get binding :token)))
              (should (equal uuid (plist-get binding :reference-uuid))))))
      (let ((file (buffer-file-name buf)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (when (and file (file-exists-p file)) (delete-file file)))))

  :doc "reference query completion remains unbound"
  (let* ((cell (mevedel-test--make-ref-buffer "alpha\n" "alpha"))
         (buf (car cell))
         (ref (cdr cell))
         (state-key
          (mevedel--instruction-workspace-key
           (mevedel--instruction-buffer-workspace buf))))
    (overlay-put ref 'mevedel-tags '(alpha))
    (unwind-protect
        (with-temp-buffer
          (insert "@ref:{a")
          (let* ((mevedel--instruction-state-key-override state-key)
                 (capf (mevedel-ref-capf)))
            (should capf)
            (should-not (plist-get (nthcdr 3 capf) :exit-function))
            (should-not (text-property-not-all
                         (point-min) (point-max)
                         'mevedel-mention-binding nil))))
      (let ((file (buffer-file-name buf)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)
        (when (and file (file-exists-p file)) (delete-file file))))))

(mevedel-deftest mevedel-mention-capf
  (:doc "`mevedel-mention-capf' offers mention prefixes at bare @")
  ,test
  (test)
  :doc "returns all mention-type candidates at bare @"
  (with-temp-buffer
    (insert "@")
    (goto-char (point-max))
    (let ((result (mevedel-mention-capf)))
      (should result)
      (let ((candidates (nth 2 result)))
        (should (member "@ref:" candidates))
        (should (member "@ref:{}" candidates))
        (should (member "@file:" candidates))
        (should (member "@agent:" candidates))
        (should (member "@mcp:" candidates)))))

  :doc "returns candidates after partial alpha letters"
  (with-temp-buffer
    (insert "@fil")
    (goto-char (point-max))
    (let ((result (mevedel-mention-capf)))
      (should result)
      (let ((start (nth 0 result))
            (end (nth 1 result)))
        (should (= start 1))
        (should (= end 5)))))

  :doc "returns nil once prefix is complete (colon seen)"
  (with-temp-buffer
    (insert "@ref:")
    (goto-char (point-max))
    (should (null (mevedel-mention-capf))))

  :doc "returns nil for plain text without @"
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-max))
    (should (null (mevedel-mention-capf))))

  :doc "exit-function places point between braces after @ref:{} completion"
  (with-temp-buffer
    (insert "@")
    (goto-char (point-max))
    (let* ((result (mevedel-mention-capf))
           (start (nth 0 result))
           (end (nth 1 result))
           (exit-fn (plist-get (nthcdr 3 result) :exit-function)))
      (should (functionp exit-fn))
      ;; Simulate completion inserting "@ref:{}" and moving point after it.
      (delete-region start end)
      (goto-char start)
      (insert "@ref:{}")
      (funcall exit-fn "@ref:{}" 'finished)
      (should (equal "@ref:{}" (buffer-string)))
      ;; Point must land between the braces.
      (should (= (point) (1- (point-max))))
      (should (eq (char-after) ?}))
      (should (eq (char-before) ?{)))))

(mevedel-deftest mevedel-mcp-capf
  (:doc "`mevedel-mcp-capf' completes servers and resources from mcp.el")
  ,test
  (test)
  :doc "returns server name candidates at @mcp: prefix"
  (cl-letf (((symbol-function 'mcp-hub-get-servers)
             (lambda ()
               (list (list :name "alpha" :status 'connected)
                     (list :name "beta"  :status 'stop)))))
    (with-temp-buffer
      (insert "@mcp:")
      (goto-char (point-max))
      (let ((result (mevedel-mcp-capf)))
        (should result)
        (let ((candidates (nth 2 result)))
          (should (member "alpha" candidates))
          (should (member "beta" candidates))))))

  :doc "returns resource uri candidates at @mcp:server: prefix"
  (cl-letf (((symbol-function 'mcp-hub-get-servers)
             (lambda ()
               (list (list :name "alpha"
                           :status 'connected
                           :resources
                           (list (list :uri "file:///a.txt" :name "A")
                                 (list :uri "file:///b.txt" :name "B")))))))
    (with-temp-buffer
      (insert "@mcp:alpha:")
      (goto-char (point-max))
      (let ((result (mevedel-mcp-capf)))
        (should result)
        (let ((candidates (nth 2 result)))
          (should (member "file:///a.txt" candidates))
          (should (member "file:///b.txt" candidates))))))

  :doc "returns nil when not at an @mcp: prefix"
  (cl-letf (((symbol-function 'mcp-hub-get-servers) (lambda () nil)))
    (with-temp-buffer
      (insert "plain text")
      (goto-char (point-max))
      (should (null (mevedel-mcp-capf))))))


;;
;;; Transform

(mevedel-deftest mevedel--transform-expand-mentions
  (:before-each
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry)
   :after-each
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "replaces mentions with placeholders and injects reminder block"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (with-temp-buffer
          (insert (propertize (format "check @ref:%d please" id)
                              'gptel 'prompt))
          (mevedel--transform-expand-mentions nil)
          (let ((content (buffer-string)))
            (should (string-match-p (format "\\[ref:%d -- contents attached above\\]" id) content))
            (should-not (string-match-p (format "@ref:%d" id) content))
            (should (string-match-p "<system-reminder>" content))
            (should (string-match-p (format "Reference #%d" id) content)))
          (goto-char (point-min))
          (should (search-forward "<system-reminder>" nil t))
          (let ((start (match-beginning 0)))
            (search-forward "</system-reminder>")
            (dolist (prop '(gptel front-sticky))
              (let ((pos start)
                    found)
                (while (and (< pos (point)) (not found))
                  (when (get-text-property pos prop)
                    (setq found t))
                  (setq pos (or (next-single-property-change
                                 pos prop nil (point))
                                (point))))
                (should-not found)))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "no reminder emitted when session dedup already has same hash"
  (let* ((ws (mevedel-workspace--create :type 'project :id "test1"
                                        :root "/tmp" :name "test1"))
         (cell (mevedel-test--make-ref-buffer "hello world\n" "hello" ws))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode)
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-test-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session))
          ;; Priming call records the hash.
          (with-temp-buffer
            (insert (propertize (format "check @ref:%d" id) 'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm))
          ;; Second send with unchanged content should skip the reminder.
          (with-temp-buffer
            (insert (propertize (format "again @ref:%d" id) 'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm)
            (should (string-match-p (format "\\[ref:%d -- contents attached above\\]" id) (buffer-string)))
            (should-not (string-match-p "<system-reminder>" (buffer-string)))))
      (kill-buffer chat)
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "reminder re-emitted when content hash changes"
  (let* ((ws (mevedel-workspace--create :type 'project :id "test2"
                                        :root "/tmp" :name "test2"))
         (cell (mevedel-test--make-ref-buffer "hello world\n" "hello" ws))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode)
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-test-chat2*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session))
          (with-temp-buffer
            (insert (propertize (format "first @ref:%d" id) 'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm))
          ;; Mutate the referenced region so its content hash changes.
          (with-current-buffer buf
            (save-excursion
              (goto-char (1+ (overlay-start ov)))
              (insert "X")))
          (with-temp-buffer
            (insert (propertize (format "second @ref:%d" id) 'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm)
            (should (string-match-p "<system-reminder>" (buffer-string)))))
      (kill-buffer chat)
      (let ((file (buffer-file-name buf)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "reference queries expand only from the session workspace"
  (let* ((workspace-a (mevedel-workspace--create
                       :type 'test :id "transform-query-a" :root "/tmp"
                       :name "transform-query-a"))
         (workspace-b (mevedel-workspace--create
                       :type 'test :id "transform-query-b" :root "/tmp"
                       :name "transform-query-b"))
         (first (mevedel-test--make-ref-buffer
                 "first query body\n" "first query body" workspace-a))
         (second (mevedel-test--make-ref-buffer
                  "second query body\n" "second query body" workspace-b))
         (session (mevedel-session-create "query-a" workspace-a))
         (chat (generate-new-buffer " *mevedel-query-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (overlay-put (cdr first) 'mevedel-reference-tags '(shared))
    (overlay-put (cdr second) 'mevedel-reference-tags '(shared))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session))
          (with-temp-buffer
            (insert (propertize "inspect @ref:{shared}" 'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm)
            (should (string-match-p "first query body" (buffer-string)))
            (should-not (string-match-p "second query body" (buffer-string)))))
      (kill-buffer chat)))

  :doc "unavailable bound reference warns without mutating stored input"
  (let* ((first (mevedel-test--make-ref-buffer "alpha body\n" "alpha body"))
         (first-buf (car first))
         (first-ref (cdr first))
         (id (mevedel--instruction-id first-ref))
         (uuid (overlay-get first-ref 'mevedel-uuid))
         (token (format "@ref:%d" id))
         (input (copy-sequence (format "check %s and continue" token)))
         (start (string-match (regexp-quote token) input))
         second warning transformed)
    (mevedel-mention-bindings-set
     start (+ start (length token))
     (list :kind 'ref :token token :reference-uuid uuid)
     input)
    (unwind-protect
        (progn
          (mevedel--delete-instruction first-ref first-buf)
          (setq second
                (mevedel-test--make-ref-buffer "replacement body\n"
                                               "replacement body"))
          (with-temp-buffer
            (insert (propertize input 'gptel 'prompt))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq warning (apply #'format format-string args)))))
              (mevedel--transform-expand-mentions nil))
            (setq transformed (buffer-string)))
          (should (string-match-p "unavailable" transformed))
          (should-not (string-match-p "replacement body" transformed))
          (should (string-match-p "mevedel: reference .* unavailable" warning))
          (should (equal (format "check %s and continue" token) input))
          (should (equal uuid
                         (plist-get
                          (get-text-property
                           start 'mevedel-mention-binding input)
                          :reference-uuid))))
      (dolist (buf (delq nil (list first-buf (car-safe second))))
        (when (buffer-live-p buf)
          (let ((file (buffer-file-name buf)))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)
            (when (and file (file-exists-p file))
              (delete-file file))))))))

(mevedel-deftest mevedel--transform-expand-mentions-media
  (:doc "`mevedel--transform-expand-mentions' adds gptel media context")
  ,test
  (test)
  (let* ((tmp (make-temp-file "mevedel-media-" nil ".png" "fake media\n"))
         (model (make-symbol "mevedel-media-model"))
         (ws (mevedel-workspace--create :type 'project :id "media"
                                        :root temporary-file-directory
                                        :name "media"))
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-test-media-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (put model :capabilities '(media))
    (put model :mime-types '("image/png"))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session))
          (with-temp-buffer
            (let ((gptel-model model)
                  (gptel-context nil)
                  (gptel-use-context nil))
              (insert (propertize (format "look @file:%s" tmp)
                                  'gptel 'prompt))
              (mevedel--transform-expand-mentions fsm)
              (should (string-match-p "media attached" (buffer-string)))
              (should-not (string-match-p "<system-reminder>"
                                          (buffer-string)))
              (should (equal (list (list tmp :mime "image/png"))
                             gptel-context))
              (should (eq gptel-use-context 'system))))
          (with-temp-buffer
            (let ((gptel-model model)
                  (gptel-context nil)
                  (gptel-use-context nil))
              (insert (propertize (format "again @file:%s" tmp)
                                  'gptel 'prompt))
              (mevedel--transform-expand-mentions fsm)
              (should (string-match-p "media attached" (buffer-string)))
              (should-not (string-match-p "<system-reminder>"
                                          (buffer-string)))
              (should (equal (list (list tmp :mime "image/png"))
                             gptel-context))
              (should (eq gptel-use-context 'system)))))
      (kill-buffer chat)
      (delete-file tmp))))

(mevedel-deftest mevedel-mentions--valid-mention-context-p
  ()
  ,test
  (test)
  :doc "accepts mention at start of buffer"
  (with-temp-buffer
    (insert "@file:foo")
    (should (mevedel-mentions--valid-mention-context-p (point-min))))

  :doc "accepts mention preceded by whitespace"
  (with-temp-buffer
    (insert "see @file:foo")
    (should (mevedel-mentions--valid-mention-context-p
             (save-excursion (search-backward "@"))))
    ;; Parenthetical prose: `(see @file:foo)' — char-before `@' is a space.
    (erase-buffer)
    (insert "(see @file:foo)")
    (should (mevedel-mentions--valid-mention-context-p
             (save-excursion (search-backward "@")))))

  :doc "rejects mention adjacent to a quoting char"
  (dolist (prefix '("`" "'" "\"" "{" "["))
    (with-temp-buffer
      (insert prefix "@file:foo")
      (should-not (mevedel-mentions--valid-mention-context-p
                   (save-excursion (search-backward "@")))))))

(mevedel-deftest mevedel--transform-expand-mentions-boundary
  (:before-each
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry)
   :after-each
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "leaves quoted @ref untouched and emits no reminder"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (with-temp-buffer
          (insert (propertize (format "`@ref:%d`" id) 'gptel 'prompt))
          (mevedel--transform-expand-mentions nil)
          (let ((content (buffer-string)))
            (should (string-match-p (format "@ref:%d" id) content))
            (should-not (string-match-p "<system-reminder>" content))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "expands parenthetical (see @ref:N) because char-before @ is a space"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (with-temp-buffer
          (insert (propertize (format "(see @ref:%d)" id) 'gptel 'prompt))
          (mevedel--transform-expand-mentions nil)
          (let ((content (buffer-string)))
            (should (string-match-p
                     (format "\\[ref:%d -- contents attached above\\]"
                             id)
                     content))
            (should (string-match-p "<system-reminder>" content))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "skips mentions inside a gptel-marked (prior response) region"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode))
    (unwind-protect
        (with-temp-buffer
          ;; Simulate a prior assistant reply that literally contains
          ;; `@ref:N' in its text — the transform must not touch it.
          (insert (propertize (format "prior response mentions @ref:%d here"
                                      id)
                              'gptel 'response))
          (insert (propertize "\nnow the user speaks"
                              'gptel 'prompt))
          (mevedel--transform-expand-mentions nil)
          (let ((content (buffer-string)))
            (should (string-match-p (format "@ref:%d" id) content))
            (should-not (string-match-p "<system-reminder>" content))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file))))))

(provide 'test-mevedel-mentions)
;;; test-mevedel-mentions.el ends here
