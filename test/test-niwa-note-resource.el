;;; test-niwa-note-resource.el -- Model-facing note reads -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise Read, Glob and Grep over real workspace notes and source files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa)
(require 'niwa-agents)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-permissions)
(require 'niwa-resource)
(require 'niwa-system)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-fs-search)
(require 'niwa-workspace)

(defmacro niwa-resource-notes-test--workspace (&rest body)
  "Run BODY with real notes in an isolated workspace."
  (declare (indent 0))
  `(let* ((root (make-temp-file "resource-notes-" t))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa-workspace--registry (make-hash-table :test #'equal))
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          (workspace (niwa-workspace-get-or-create 'file root root "notes"))
          (session (niwa-session--create :workspace workspace :working-directory root))
          (file (file-name-concat root "source.el"))
          note address)
     (unwind-protect
         (progn
           (make-directory (file-name-concat root ".niwa"))
           (write-region "before\n(marked-code)\nafter\n" nil file nil 'silent)
           (with-current-buffer (find-file-noselect file)
             (setq-local niwa--workspace workspace)
             (goto-char (point-min))
             (forward-line 1)
             (setq note (niwa-note-create "Review this\nsecond thought"))
             (niwa-note-set-tags '(review parser) note)
             (setq address (concat "notes://" (overlay-get note 'niwa-uuid) ".md")))
           ,@body)
       (when-let* ((buffer (find-buffer-visiting file)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (setq niwa-instruction-store--dirty nil)
       (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
       (niwa-execution-teardown-all)
       (delete-directory root t))))

(defun niwa-resource-notes-test--tool (session operation args)
  "Run model file-tool OPERATION with ARGS in SESSION."
  (let* ((address (plist-get args (if (eq operation 'read) :file_path :path)))
         (attempt (niwa-resource-prepare operation address (list :session session :args args)))
         (niwa-resource-current-attempts (list (cons address attempt)))
         (niwa--session session)
         done result)
    (unwind-protect
        (progn
          (if (eq operation 'read)
              (setq result (niwa-test--read args) done t)
            (funcall (if (eq operation 'glob) #'niwa-tool-fs-search-glob
                       #'niwa-tool-fs-search-grep)
                     (lambda (value) (setq result value done t)) args))
          (with-timeout (10 (ert-fail "Note tool did not settle"))
            (while (not done) (accept-process-output nil 0.01)))
          result)
      (niwa-resource-discard-attempts (list attempt)))))

(niwa-deftest niwa-tool-fs-read/notes (:quiet t)
  ,test
  (test)
  :doc "lists notes and reads text, tags, author, state, location and current code"
  (niwa-resource-notes-test--workspace
    (let ((listing (niwa-resource-notes-test--tool session 'read '(:file_path "notes://"))))
      (should (string-search address (plist-get listing :result))))
    (with-current-buffer (overlay-buffer note)
      (goto-char (overlay-start note))
      (insert "current-"))
    (let ((text (plist-get (niwa-resource-notes-test--tool
                           session 'read (list :file_path address)) :result)))
      (dolist (expected '("Review this" "second thought" "review" "parser"
                          "user" "unsent" "source.el:2" "current-(marked-code)"))
        (should (string-search expected text)))
      (should-not (string-search root text))))
  :doc "reads re-anchored current source after reload without visiting source buffers"
  (niwa-resource-notes-test--workspace
    (niwa-instruction-store-flush t)
    (kill-buffer (overlay-buffer note))
    (niwa--clear-instruction-state workspace)
    (write-region "new-before\nbefore\n(marked-code)\nafter\n" nil file nil 'silent)
    (let ((text (plist-get (niwa-resource-notes-test--tool
                           session 'read (list :file_path address)) :result)))
      (should (string-search "source.el:3" text))
      (should (string-search "(marked-code)" text))
      (should-not (find-buffer-visiting file))))
  :doc "closed notes remain readable with their number, UUID and close reason"
  (niwa-resource-notes-test--workspace
    (niwa-note-done note)
    (niwa-instruction-store-flush t)
    (kill-buffer (find-buffer-visiting file))
    (niwa--clear-instruction-state workspace)
    (let ((listing (plist-get (niwa-resource-notes-test--tool
                              session 'read '(:file_path "notes://")) :result))
          (text (plist-get (niwa-resource-notes-test--tool
                           session 'read (list :file_path address)) :result)))
      (dolist (value (list listing text))
        (should (string-search address value))
        (should (string-search "#1" value))
        (should (string-search "closed" value))
        (should (string-search "done" value)))
      (should (string-search "(marked-code)" text))
      (should-not (find-buffer-visiting file))))
  :doc "denied source reads preserve note metadata but never expose code"
  (niwa-resource-notes-test--workspace
    (niwa-permission--add-session-rule session (list "Read" :path file :action 'deny))
    (let ((text (plist-get (niwa-resource-notes-test--tool
                           session 'read (list :file_path address)) :result)))
      (should (string-search "Review this" text))
      (should (string-search "Read permission denied" text))
      (should-not (string-search "marked-code" text)))
    (let ((result (niwa-resource-notes-test--tool
                   session 'grep '(:path "notes://" :pattern "marked-code" :output_mode "content"))))
      (should-not (string-search "marked-code" (plist-get result :result)))))
  :doc "missing or changed unopened sources do not return old anchor text as current code"
  (niwa-resource-notes-test--workspace
    (niwa-instruction-store-flush t)
    (kill-buffer (overlay-buffer note))
    (dolist (state '(changed missing))
      (if (eq state 'changed)
          (write-region "unrelated replacement\n" nil file nil 'silent)
        (delete-file file))
      (let ((text (plist-get (niwa-resource-notes-test--tool
                             session 'read (list :file_path address)) :result)))
        (should (string-search "Review this" text))
        (should (string-search "Code unavailable" text))
        (should-not (string-search "marked-code" text)))))
  :doc "prepared reads see later note edits instead of preparation-time content"
  (niwa-resource-notes-test--workspace
    (let* ((attempt (niwa-resource-prepare 'read address (list :session session)))
           (niwa-resource-current-attempts (list (cons address attempt))))
      (unwind-protect
          (progn
            (niwa-note-edit "Updated after preparation" note)
            (let ((text (plist-get (niwa-test--read (list :file_path address)) :result)))
              (should (string-search "Updated after preparation" text))
              (should-not (string-search "Review this" text))))
        (niwa-resource-discard-attempts (list attempt)))))
  :doc "an unsaved-buffer note exposes metadata without inventing source authority"
  (niwa-resource-notes-test--workspace
    (with-current-buffer (overlay-buffer note)
      (set-visited-file-name nil t)
      (unwind-protect
          (let ((text (plist-get (niwa-resource-notes-test--tool
                                 session 'read (list :file_path address)) :result)))
            (should (string-search "Review this" text))
            (should (string-search "unsaved buffer" text))
            (should (string-search "Code unavailable" text))
            (should-not (string-search "(marked-code)" text)))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))))
  :doc "request and agent invocation Read denies also protect note source code"
  (niwa-resource-notes-test--workspace
    (dolist (bucket '(request invocation))
      (let* ((rules (list (list "Read" :path file :action 'deny)))
             (niwa--current-request
              (and (eq bucket 'request)
                   (niwa-request--create :skill-permission-rules rules)))
             (niwa--agent-invocation
              (and (eq bucket 'invocation)
                   (niwa-agent-invocation--create :skill-permission-rules rules)))
             (text (plist-get (niwa-resource-notes-test--tool
                               session 'read (list :file_path address)) :result)))
        (should (string-search "Read permission denied" text))
        (should-not (string-search "(marked-code)" text)))))
  :doc "unreadable store errors name the logical address without private storage paths"
  (niwa-resource-notes-test--workspace
    (write-region "not an instruction store" nil
                  (niwa-instruction-store-path workspace) nil 'silent)
    (let* ((attempt (niwa-resource-prepare 'read "notes://" (list :session session)))
           (failure (should-error (niwa-resource-execute attempt)))
           (message (error-message-string failure)))
      (should (string-search "notes://" message))
      (should (string-search "unreadable" message))
      (should-not (string-search root message)))))

(niwa-deftest niwa-tool-fs-search-grep/notes (:quiet t)
  ,test
  (test)
  :doc "searches note text and source with followable addresses and Read line numbers"
  (niwa-resource-notes-test--workspace
    (dolist (path (list "notes://" address))
      (dolist (mode '("files_with_matches" "content" "count"))
        (let* ((result (niwa-resource-notes-test--tool
                        session 'grep (list :path path :pattern "marked-code" :output_mode mode)))
               (text (plist-get result :result)))
          (should-not (eq 'error (plist-get result :status)))
          (should (string-search address text))
          (should-not (string-search root text))
          (pcase mode
            ("content"
             (should (string-match (concat (regexp-quote address) ":\\([0-9]+\\):") text))
             (let* ((line (string-to-number (match-string 1 text)))
                    (read (niwa-resource-notes-test--tool
                           session 'read (list :file_path address :offset line :limit 1))))
               (should (string-search "(marked-code)" (plist-get read :result)))))
            ("count" (should (string-search (concat address ":1") text)))
            (_ (should (equal address (string-trim text)))))))))
  :doc "search reads each unopened source once even when it has several notes"
  (niwa-resource-notes-test--workspace
    (with-current-buffer (overlay-buffer note)
      (goto-char (point-min))
      (niwa-note-create "Another note"))
    (niwa-instruction-store-flush t)
    (kill-buffer (find-buffer-visiting file))
    (let* ((reads 0)
          (file-name-handler-alist
           (cons (cons (concat "\\`" (regexp-quote file) "\\'")
                       (lambda (operation &rest args)
                         (when (eq operation 'insert-file-contents) (cl-incf reads))
                         (let ((file-name-handler-alist nil)) (apply operation args))))
                 file-name-handler-alist)))
      (niwa-resource-notes-test--tool
       session 'grep '(:path "notes://" :pattern "note" :output_mode "content"))
      (should (= 1 reads))
      (should-not (find-buffer-visiting file)))))

(niwa-deftest niwa-tool-fs-search-glob/notes (:quiet t)
  ,test
  (test)
  :doc "lists only matching note addresses without reading source content"
  (niwa-resource-notes-test--workspace
    (dolist (path (list "notes://" address))
      (let ((result (niwa-resource-notes-test--tool
                     session 'glob (list :path path :pattern "*.md"))))
        (should-not (eq 'error (plist-get result :status)))
        (should (equal address (string-trim (plist-get result :result))))))
    (let ((result (niwa-resource-notes-test--tool
                   session 'glob '(:path "notes://" :pattern "not-a-note*"))))
      (should-not (string-search address (plist-get result :result))))))

(niwa-deftest niwa-resource-prepare/notes (:quiet t)
  ,test
  (test)
  :doc "rejects patches and malformed selections before execution"
  (dolist (address '("notes://" "notes://00000000-0000-0000-0000-000000000000.md"))
    (should-error (niwa-resource-prepare 'apply-patch address nil)
                  :type 'niwa-resource-error))
  (dolist (address '("notes://1" "notes://other/file.md" "notes://../secret"
                     "notes://%2Fsecret" "notes://?workspace=other" "notes://#fragment"))
    (should-error (niwa-resource-prepare 'read address nil)
                  :type 'niwa-resource-error))
  :doc "requires the session's workspace, not an explicit alternative owner"
  (niwa-resource-notes-test--workspace
    (dolist (owner (list nil (niwa-session--create)))
      (let ((attempt (niwa-resource-prepare 'read "notes://"
                                             (list :session owner :workspace workspace))))
        (should-error (niwa-resource-execute attempt)
                      :type 'niwa-resource-unavailable))))
  :doc "a UUID from another workspace never selects the same display number"
  (niwa-resource-notes-test--workspace
    (let* ((other-root (make-temp-file "other-note-workspace-" t))
           (other (niwa-workspace-get-or-create 'file other-root other-root "other"))
           (other-session (niwa-session--create :workspace other)))
      (unwind-protect
          (progn
            (should (string-search "No notes" (plist-get (niwa-resource-notes-test--tool
                                                          other-session 'read '(:file_path "notes://")) :result)))
            (let ((attempt (niwa-resource-prepare 'read address
                                                   (list :session other-session :workspace workspace))))
              (should-error (niwa-resource-execute attempt)
                            :type 'niwa-resource-unavailable)))
        (delete-directory other-root t)))))

(niwa-deftest niwa-system--resource-roster/notes (:quiet t)
  ,test
  (test)
  :doc "advertises workspace note discovery without attaching note text or code"
  (niwa-resource-notes-test--workspace
    (let* ((context (niwa-system-context--create :session session :workspace workspace
                                                   :working-directory root))
           (roster (niwa-system--resource-roster context)))
      (should (string-search "notes://" roster))
      (should (string-search "read-only" roster))
      (should-not (string-search "Review this" roster))
      (should-not (string-search "(marked-code)" roster)))))

(provide 'test-niwa-note-resource)
;;; test-niwa-note-resource.el ends here
