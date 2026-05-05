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

(defun mevedel-test--make-ref-buffer (content text)
  "Create a live file-visiting buffer with CONTENT and a reference on TEXT.
Returns (buffer . overlay)."
  (let* ((tmp (make-temp-file "mevedel-mention-" nil ".txt" content))
         (buf (find-file-noselect tmp))
         ov)
    (with-current-buffer buf
      (fundamental-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (re-search-forward (regexp-quote text))
      (setq ov (mevedel--create-reference-in buf
                                             (match-beginning 0)
                                             (match-end 0))))
    (cons buf ov)))

(defun mevedel-test--reset-instructions ()
  "Reset global and workspace-scoped instruction state for mention tests."
  (setq mevedel--instructions nil)
  (setq mevedel--id-counter 0)
  (setq mevedel--id-usage-map (make-hash-table))
  (setq mevedel--retired-ids nil)
  (setq mevedel--instruction-states (make-hash-table :test #'equal))
  (setq mevedel--instruction-current-state-key :global))


;;
;;; Resolution

(mevedel-deftest mevedel--resolve-ref-by-id
  (:before-each
   (mevedel-test--reset-instructions)
   :after-each
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (let ((file (buffer-file-name (car entry))))
         (with-current-buffer (car entry)
           (set-buffer-modified-p nil))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
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
    (should (equal "/tmp/foo.el" (match-string 1)))))

(mevedel-deftest mevedel--fontify-agent-keyword
  (:doc "`mevedel--fontify-agent-keyword' matches @agent:name tokens")
  ,test
  (test)
  :doc "matches agent mention at beginning of buffer"
  (with-temp-buffer
    (insert "@agent:explore please")
    (goto-char (point-min))
    (should (mevedel--fontify-agent-keyword (point-max)))
    (should (equal "explore" (match-string 1))))

  :doc "matches agent mention after whitespace"
  (with-temp-buffer
    (insert "ask @agent:planner to help")
    (goto-char (point-min))
    (should (mevedel--fontify-agent-keyword (point-max)))
    (should (equal "planner" (match-string 1))))

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
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (let ((file (buffer-file-name (car entry))))
         (with-current-buffer (car entry)
           (set-buffer-modified-p nil))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
   (mevedel-test--reset-instructions)
   (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "ID form: returns placeholder + reminder + content-hash for known ref"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
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
          (should (equal (cons 'ref id) (plist-get result :key)))
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

  :doc "binary file extension yields graceful placeholder, explanatory reminder"
  (let* ((tmp (make-temp-file "mevedel-file-" nil ".png" "not really png\n"))
         (result (mevedel--handle-file-mention
                  (list :match-text (concat "@file:" tmp)
                        :capture tmp
                        :workspace-root temporary-file-directory))))
    (unwind-protect
        (progn
          (should (string-match-p "binary" (plist-get result :placeholder)))
          (should (stringp (plist-get result :reminder)))
          (should (null (plist-get result :hash))))
      (delete-file tmp)))

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
                 (list :match-text "@agent:explore"
                       :capture "explore"))))
    (should (equal "[agent:explore -- delegation requested]"
                   (plist-get result :placeholder)))
    (should (stringp (plist-get result :reminder)))
    (should (string-match-p "subagent_type=\"explore\""
                            (plist-get result :reminder)))
    (should (string-match-p "Do not mention this reminder"
                            (plist-get result :reminder)))
    (should (equal (cons 'agent "explore") (plist-get result :key)))
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
        (should (member "explore" candidates))
        (should (member "planner" candidates)))))

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
               (lambda (_conn _uri) (error "boom"))))
      (let* ((mcp-server-connections table)
             (result (mevedel--handle-mcp-mention
                      (list :match-text "@mcp:srv:bad"
                            :captures '("@mcp:srv:bad" "srv" "bad")))))
        (should (string-match-p "read failed: boom"
                                (plist-get result :placeholder)))
        (should (null (plist-get result :hash)))))))

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
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (let ((file (buffer-file-name (car entry))))
         (with-current-buffer (car entry)
           (set-buffer-modified-p nil))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
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
            (should (string-match-p (format "Reference #%d" id) content))))
      (let ((file (buffer-file-name buf)))
        (kill-buffer buf)
        (when (and file (file-exists-p file))
          (delete-file file)))))

  :doc "no reminder emitted when session dedup already has same hash"
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode)
         (ws (mevedel-workspace--create :type 'project :id "test1"
                                        :root "/tmp" :name "test1"))
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
  (let* ((cell (mevedel-test--make-ref-buffer "hello world\n" "hello"))
         (buf (car cell))
         (ov (cdr cell))
         (id (mevedel--instruction-id ov))
         (gptel-default-mode 'text-mode)
         (ws (mevedel-workspace--create :type 'project :id "test2"
                                        :root "/tmp" :name "test2"))
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
          (delete-file file))))))

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
   (dolist (entry mevedel--instructions)
     (when (buffer-live-p (car entry))
       (let ((file (buffer-file-name (car entry))))
         (with-current-buffer (car entry)
           (set-buffer-modified-p nil))
         (kill-buffer (car entry))
         (when (and file (file-exists-p file))
           (delete-file file)))))
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
