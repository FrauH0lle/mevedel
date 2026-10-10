;;; test-mevedel-artifact-store.el --- Workspace artifact store tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the store layout, metadata, versions with their caps, duplicate,
;; delete, attachment, and the bookkeeping of settled ApplyPatch writes.

;;; Code:

(require 'mevedel-artifact-store)
(require 'mevedel-chat)
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-artifact-store-test--with-workspace (&rest body)
  "Run BODY with `workspace' rooted in a fresh temp directory and `store'."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "mevedel-artifact-store-" t)))
          (workspace (mevedel-workspace--create :type 'project :id "w"
                                                :root root :name "w"))
          (store (mevedel-artifact-store-directory workspace)))
     (ignore store)
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun mevedel-artifact-store-test--write (store relative content)
  "Write CONTENT to RELATIVE below STORE and return the absolute path."
  (let ((path (file-name-concat store relative)))
    (make-directory (file-name-directory path) t)
    (write-region content nil path nil 'silent)
    path))

(defun mevedel-artifact-store-test--note (session path)
  "Report a settled ApplyPatch write of PATH for SESSION."
  (mevedel-artifact-store-note-writes
   session (list (list :action 'write :path path))))

(mevedel-deftest mevedel-artifact-store-directory ()
  ,test
  (test)
  :doc "lives in the workspace's .mevedel directory"
  (mevedel-artifact-store-test--with-workspace
    (should (equal (file-name-concat root ".mevedel/artifacts/") store))))

(mevedel-deftest mevedel-artifact-store-ids ()
  ,test
  (test)
  :doc "lists visible directories only, sorted, and nil without a store"
  (mevedel-artifact-store-test--with-workspace
    (should-not (mevedel-artifact-store-ids workspace))
    (mevedel-artifact-store-test--write store "b/index.html" "b")
    (mevedel-artifact-store-test--write store "a/index.html" "a")
    (mevedel-artifact-store-test--write store "loose.html" "x")
    (make-directory (file-name-concat store ".hidden") t)
    (should (equal '("a" "b") (mevedel-artifact-store-ids workspace)))))

(mevedel-deftest mevedel-artifact-store-note-writes ()
  ,test
  (test)
  :doc "a first write creates the metadata, a version, and attaches the session"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace :session-id "s1")))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/index.html" "<p>1</p>"))
      ;; Titled by its id, which says what it shows; the file is index.html.
      (should (equal '(:kind html :title "flow" :file "index.html")
                     (cl-subseq (mevedel-artifact-store-meta workspace "flow") 0 6)))
      (should (equal '("flow") (mevedel-session-attached-artifacts session)))
      (let ((versions (mevedel-artifact-store-versions workspace "flow")))
        (should (= 1 (length versions)))
        (should (equal "s1" (plist-get (car versions) :session)))
        (should (equal "<p>1</p>"
                       (with-temp-buffer
                         (insert-file-contents
                          (mevedel-artifact-store-version-path workspace "flow" 1))
                         (buffer-string)))))))

  :doc "later writes add versions; secondary, bookkeeping, and loose files do not"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace)))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/index.html" "1"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/index.html" "2"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/logo.png" "png"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/meta.el" "()"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "loose.html" "x"))
      (mevedel-artifact-store-note-writes
       session (list (list :action 'delete
                           :path (file-name-concat store "flow/index.html"))))
      (should (= 2 (length (mevedel-artifact-store-versions workspace "flow"))))
      (should (equal '("flow") (mevedel-session-attached-artifacts session)))))

  :doc "during a turn, notes the artifact for one version when it settles"
  (mevedel-artifact-store-test--with-workspace
    (let* ((session (mevedel-session--create :workspace workspace :session-id "s1"))
           (request (mevedel-request--create)))
      (dolist (content '("1" "2" "3"))
        (mevedel-artifact-store-note-writes
         session
         (list (list :action 'write
                     :path (mevedel-artifact-store-test--write store "flow/index.html" content)))
         request))
      (should-not (mevedel-artifact-store-versions workspace "flow"))
      (should (equal (list (cons workspace "flow")) (mevedel-request-edited-artifacts request)))
      (should (= 1 (mevedel-artifact-store-save-version workspace "flow" "s1")))))

  :doc "a failed metadata read is not taken for a new artifact"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace)))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/index.html" "1"))
      (let ((before (mevedel-artifact-store-meta workspace "flow")))
        (cl-letf* ((insert (symbol-function 'insert-file-contents))
                   ((symbol-function 'insert-file-contents)
                    (lambda (file &rest args)
                      (if (string-suffix-p "meta.el" file)
                          (signal 'file-error (list "Connection lost" file))
                        (apply insert file args)))))
          (should-error (mevedel-artifact-store-note-writes
                         session (list (list :action 'write
                                             :path (mevedel-artifact-store-test--write
                                                    store "flow/logo.png" "png"))))))
        (should (equal before (mevedel-artifact-store-meta workspace "flow"))))))

  :doc "follows writes reported through the workspace root's true name"
  (let* ((real (file-name-as-directory (make-temp-file "mevedel-artifact-real-" t)))
         (link (concat (directory-file-name real) "-link/"))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root link :name "w"))
         (session (mevedel-session--create :workspace workspace)))
    (unwind-protect
        (progn
          (make-symbolic-link (directory-file-name real) (directory-file-name link))
          (mevedel-artifact-store-test--note
           session (mevedel-artifact-store-test--write
                    (file-name-concat real ".mevedel/artifacts/") "flow/index.html" "1"))
          (should (mevedel-artifact-store-meta workspace "flow"))
          (should (equal '("flow") (mevedel-session-attached-artifacts session))))
      (delete-file (directory-file-name link))
      (delete-directory real t)))

  :doc "writes outside the store are ignored"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace))
          (path (file-name-concat root "src/app.html")))
      (make-directory (file-name-directory path) t)
      (write-region "x" nil path nil 'silent)
      (mevedel-artifact-store-test--note session path)
      (should-not (mevedel-session-attached-artifacts session))
      (should-not (file-exists-p store)))))

(mevedel-deftest mevedel-artifact-store-meta ()
  ,test
  (test)
  :doc "refuses metadata naming a file outside the artifact"
  (mevedel-artifact-store-test--with-workspace
    (dolist (file '("../../.bashrc" "/etc/passwd" ""))
      (make-directory (file-name-concat store "evil") t)
      (mevedel-artifact-store--write (file-name-concat store "evil" "meta.el")
                                     (list :kind 'html :file file))
      (should-error (mevedel-artifact-store-meta workspace "evil")))))

(mevedel-deftest mevedel-artifact-store-record-version ()
  ,test
  (test)
  :doc "drops the oldest beyond the count cap, with their files"
  (mevedel-artifact-store-test--with-workspace
    (let ((mevedel-artifact-store-max-versions 2)
          (session (mevedel-session--create :workspace workspace)))
      (dolist (content '("1" "2" "3"))
        (mevedel-artifact-store-test--note
         session (mevedel-artifact-store-test--write store "a/x.md" content)))
      (should (equal '(2 3) (mapcar (lambda (row) (plist-get row :n))
                                    (mevedel-artifact-store-versions workspace "a"))))
      (should-not (file-exists-p (file-name-concat store "a/versions/000001.md")))))

  :doc "drops the oldest beyond the byte cap but always keeps the latest"
  (mevedel-artifact-store-test--with-workspace
    (let ((mevedel-artifact-store-max-version-bytes 5)
          (session (mevedel-session--create :workspace workspace)))
      (dolist (content '("abc" "def" "too large"))
        (mevedel-artifact-store-test--note
         session (mevedel-artifact-store-test--write store "a/x.md" content)))
      (should (equal '(3) (mapcar (lambda (row) (plist-get row :n))
                                  (mevedel-artifact-store-versions workspace "a")))))))

(mevedel-deftest mevedel-artifact-store-save-version ()
  ,test
  (test)
  :doc "versions a file at once and a whiteboard through its editing queue"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-test--write store "page/index.html" "x")
    (mevedel-artifact-store-create-meta workspace "page" "index.html")
    (make-directory (file-name-concat store "board") t)
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (let (queued)
      (cl-letf (((symbol-function 'mevedel-shared-editing-save-version-later)
                 (lambda (&rest args) (push args queued))))
        (should (= 1 (mevedel-artifact-store-save-version workspace "page" "s1")))
        (should-not (mevedel-artifact-store-save-version workspace "board" "s1")))
      (should (equal (list (list workspace "board" "s1")) queued)))))

(mevedel-deftest mevedel-artifact-store-restore-version ()
  ,test
  (test)
  :doc "copies the old version back as a new version"
  (mevedel-artifact-store-test--with-workspace
    (let* ((session (mevedel-session--create :workspace workspace))
           (path (mevedel-artifact-store-test--write store "a/x.md" "old")))
      (mevedel-artifact-store-test--note session path)
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "a/x.md" "new"))
      (should (= 3 (mevedel-artifact-store-restore-version workspace "a" 1 "s2")))
      (should (equal "old" (with-temp-buffer (insert-file-contents path)
                                             (buffer-string))))
      (should (equal "s2" (plist-get (car (last (mevedel-artifact-store-versions
                                                  workspace "a")))
                                     :session)))
      (should-error (mevedel-artifact-store-restore-version workspace "a" 9))))

  :doc "restores a whiteboard as ACTOR's edit, then versions it and reports"
  (mevedel-artifact-store-test--with-workspace
    (make-directory (file-name-concat store "board") t)
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (let (edits saved outcomes)
      (cl-letf (((symbol-function 'mevedel-shared-editing-restore)
                 (lambda (_workspace id n actor callback)
                   (push (list id n actor) edits)
                   (funcall callback (if (= n 1) '(:result t) '(:error "Busy")))))
                ((symbol-function 'mevedel-shared-editing-save-version)
                 (lambda (_workspace id session-id) (push (list id session-id) saved) 2)))
        (should-not (mevedel-artifact-store-restore-version
                     workspace "board" 1 "s2" "Guest: Ann"
                     (lambda (failure) (push failure outcomes))))
        (mevedel-artifact-store-restore-version
         workspace "board" 2 "s2" nil (lambda (failure) (push failure outcomes)))
        ;; Restored, but its version failed: not reported as unrestored.
        (cl-letf (((symbol-function 'mevedel-shared-editing-save-version)
                   (lambda (&rest _) (error "Disk full"))))
          (mevedel-artifact-store-restore-version
           workspace "board" 1 "s2" nil (lambda (failure) (push failure outcomes)))))
      (should (equal '(("board" 1 "Host") ("board" 2 "Host") ("board" 1 "Guest: Ann")) edits))
      (should (equal '(("board" "s2")) saved))
      (should (equal '("Restored; no version saved: Disk full" "Busy" nil) outcomes)))))

(mevedel-deftest mevedel-artifact-store-duplicate ()
  ,test
  (test)
  :doc "copies the artifact with fresh metadata and a single version"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace)))
      (dolist (content '("1" "2"))
        (mevedel-artifact-store-test--note
         session (mevedel-artifact-store-test--write store "a/x.md" content)))
      (mevedel-artifact-store-test--write store "a/img.png" "png")
      (should (equal "b" (mevedel-artifact-store-duplicate workspace "a" "b")))
      (should (file-exists-p (file-name-concat store "b/img.png")))
      (should (equal "x.md" (plist-get (mevedel-artifact-store-meta workspace "b") :file)))
      (should (= 1 (length (mevedel-artifact-store-versions workspace "b"))))
      (should (= 2 (length (mevedel-artifact-store-versions workspace "a"))))
      (should-error (mevedel-artifact-store-duplicate workspace "a" "b"))
      ;; Comments stay with the original.
      (mevedel-artifact-store-test--write store "a/comments.json" "{}")
      (mevedel-artifact-store-duplicate workspace "a" "c")
      (should-not (file-exists-p (file-name-concat store "c/comments.json")))
      (should-error (mevedel-artifact-store-duplicate workspace "a" "../c")))))

(mevedel-deftest mevedel-artifact-store-delete ()
  ,test
  (test)
  :doc "removes the artifact directory and refuses ids that escape the store"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-test--write store "a/x.md" "1")
    (mevedel-artifact-store-delete workspace "a")
    (should-not (file-exists-p (file-name-concat store "a")))
    (should-error (mevedel-artifact-store-delete workspace ".."))
    (should-error (mevedel-artifact-store-delete workspace "a/b"))
    (should (file-directory-p root))))

(mevedel-deftest mevedel-artifact-store-list ()
  ,test
  (test)
  :doc "lists artifacts with metadata, stats, and missing files"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace)))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "a/x.md" "123"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "b/y.png" "1"))
      (delete-file (file-name-concat store "b/y.png"))
      ;; A directory without metadata is not an artifact yet.
      (mevedel-artifact-store-test--write store "c/z.md" "1")
      (let ((rows (mevedel-artifact-store-list workspace)))
        (should (equal '("a" "b") (mapcar (lambda (row) (plist-get row :id)) rows)))
        (should (= 3 (plist-get (car rows) :size)))
        (should (eq 'markdown (plist-get (car rows) :kind)))
        (should (= 1 (plist-get (car rows) :versions)))
        (should (plist-get (cadr rows) :missing))))))

(mevedel-deftest mevedel-artifact-store-attach ()
  ,test
  (test)
  :doc "attaches once, keeps order, writes the sidecar only with a buffer, and announces it"
  (let* ((session (mevedel-session--create :workspace 'workspace))
         changed written
         (mevedel-artifact-store-changed-functions
          (list (lambda (workspace) (push workspace changed)))))
    (cl-letf (((symbol-function 'mevedel-session-persistence-write-sidecar-now)
               (lambda (&rest args) (push args written))))
      (mevedel-artifact-store-attach session "a")
      (mevedel-artifact-store-attach session "b" 'buffer)
      (mevedel-artifact-store-attach session "a" 'buffer))
    (should (equal '("a" "b") (mevedel-session-attached-artifacts session)))
    (should (equal (list (list session 'buffer)) written))
    (should (equal '(workspace workspace) changed))))

(mevedel-deftest mevedel-artifact-store--changed ()
  ,test
  (test)
  :doc "announces versions, new artifacts, metadata changes and deletions"
  (mevedel-artifact-store-test--with-workspace
    (let* (changed
           (mevedel-artifact-store-changed-functions
            (list (lambda (seen) (push seen changed)))))
      (mevedel-artifact-store-test--note
       (mevedel-session--create :workspace workspace)
       (mevedel-artifact-store-test--write store "a/x.md" "1"))
      (should (>= (length changed) 2))
      (setq changed nil)
      (mevedel-artifact-store-update-meta workspace "a" :title "Renamed")
      (should (equal (list workspace) changed))
      (setq changed nil)
      (mevedel-artifact-store-delete workspace "a")
      (should (equal (list workspace) changed)))))

;; ponytail: one real lifecycle case; the chat setup is too slow for several.
(mevedel-deftest mevedel-artifact-store-conversation (:quiet t)
  ,test
  (test)
  :doc "creates a saved, attached, hidden session once, resumes it, and closes and deletes it"
  (pcase-let* ((`(,workspace . ,root)
                (test-mevedel-session-persistence--make-tempdir-workspace))
               (store (mevedel-artifact-store-directory workspace))
               (buffer nil))
    (unwind-protect
        (progn
          (mevedel-artifact-store-test--write store "flow/index.html" "x")
          (mevedel-artifact-store-create-meta workspace "flow" "index.html")
          ;; Presets are defined by the package setup the test omits.
          (cl-letf (((symbol-function 'mevedel--ensure-chat-preset) #'ignore))
            (setq buffer (mevedel-artifact-store-conversation workspace "flow")))
          (let* ((session (buffer-local-value 'mevedel--session buffer))
                 (id (mevedel-session-session-id session)))
            (should (equal "Artifact flow" (mevedel-session-name session)))
            (should (equal '("flow") (mevedel-session-attached-artifacts session)))
            (should (equal "flow" (mevedel-session-dedicated-artifact session)))
            (should (mevedel-artifact-store-dedicated-p workspace id "flow"))
            (should (file-exists-p (mevedel-session-artifacts-sidecar-path
                                    (mevedel-session-save-path session))))
            (should (eq buffer (mevedel-artifact-store-conversation workspace "flow")))
            ;; A copy made outside mevedel names the same conversation;
            ;; deleting it leaves that conversation to its own artifact.
            (copy-directory (file-name-concat store "flow") (file-name-concat store "copy"))
            (mevedel-artifact-store-delete workspace "copy")
            (should-not (file-exists-p (file-name-concat store "copy")))
            (should (buffer-live-p buffer))
            (should (mevedel-artifact-store-dedicated-p workspace id "flow"))
            ;; A running turn keeps its artifact.
            (let (failure)
              (cl-letf (((symbol-function 'mevedel-turn-busy-p) (lambda (&rest _) t)))
                (mevedel-artifact-store-delete workspace "flow" nil
                                               (lambda (why) (setq failure why))))
              (should (string-match-p "still working" failure))
              (should (file-exists-p (file-name-concat store "flow"))))
            (test-mevedel-session-persistence--release-and-kill buffer session)
            (setq buffer (mevedel-artifact-store-conversation workspace "flow"))
            (should (equal id (mevedel-session-session-id
                               (buffer-local-value 'mevedel--session buffer))))
            ;; An idle open conversation, as a room opens it from the lobby,
            ;; closes with its artifact.
            (mevedel-artifact-store-delete workspace "flow")
            (should-not (buffer-live-p buffer))
            (should-not (file-exists-p (file-name-concat store "flow")))
            (should-not (file-exists-p (file-name-concat
                                        (mevedel-session-artifacts-sessions-dir workspace)
                                        id)))))
      (when (buffer-live-p buffer)
        (test-mevedel-session-persistence--release-and-kill
         buffer (buffer-local-value 'mevedel--session buffer)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-artifact-store)
;;; test-mevedel-artifact-store.el ends here
