;;; test-mevedel-artifact-store.el --- Workspace artifact store tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the store layout, metadata, versions with their caps, duplicate,
;; delete, attachment, and the bookkeeping of settled ApplyPatch writes.

;;; Code:

(require 'mevedel-artifact-store)
(require 'mevedel-artifact-lease)
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

(mevedel-deftest mevedel-artifact-store--with-changes ()
  ,test
  (test)
  :doc "nested mutations notify once after completion, including partial failure"
  (let (seen
        (mevedel-artifact-store--pending-changes nil))
    (let ((mevedel-artifact-store-changed-functions
           (list (lambda (workspace) (push workspace seen)))))
      (should-error
       (mevedel-artifact-store--with-changes
         (mevedel-artifact-store--changed 'one)
         (mevedel-artifact-store--with-changes
           (mevedel-artifact-store--changed 'one)
           (mevedel-artifact-store--changed 'two))
         (should-not seen)
         (error "Partial mutation")))
      (should (equal '(one two) seen)))))

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
    (should (equal '("a" "b") (mevedel-artifact-store-ids workspace))))

  :doc "keeps shared items discoverable after Git omits their empty authored directories"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Board")
    (mevedel-artifact-store-test--write store ".state/board/state.json" "{}")
    (delete-directory (mevedel-artifact-store-artifact-directory workspace "board"))
    (should (equal '("board") (mevedel-artifact-store-ids workspace)))
    (should (equal "board" (plist-get (car (mevedel-artifact-store-list workspace)) :id)))
    (unwind-protect
        (progn
          (mevedel-artifact-lease-acquire workspace "board")
          (mevedel-artifact-store-delete workspace "board")
          (should-not (mevedel-artifact-store-ids workspace)))
      (mevedel-artifact-lease--forget (mevedel-artifact-lease-directory workspace "board")))))

(mevedel-deftest mevedel-artifact-store-note-writes ()
  ,test
  (test)
  :doc "a first write creates the metadata, a version, and attaches the session"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace :session-id "s1")))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "flow/index.html" "<p>1</p>"))
      (should (equal '(:kind html :title "index.html" :file "index.html")
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

  :doc "a settled multi-file patch publishes one complete store snapshot"
  (mevedel-artifact-store-test--with-workspace
    (let* ((session (mevedel-session--create :workspace workspace))
           snapshots
           (mevedel-artifact-store-changed-functions
            (list (lambda (changed)
                    (push (list (length (mevedel-artifact-store-list changed))
                                (copy-sequence
                                 (mevedel-session-attached-artifacts session)))
                          snapshots)))))
      (mevedel-artifact-store-note-writes
       session
       (mapcar (lambda (id)
                 (list :action 'write
                       :path (mevedel-artifact-store-test--write
                              store (concat id "/index.html") "content")))
               '("one" "two")))
      (should (equal '((2 ("one" "two"))) snapshots))))

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
       session (mevedel-artifact-store-test--write store ".state/ignored/meta.el" "()"))
      (mevedel-artifact-store-test--note
       session (mevedel-artifact-store-test--write store "loose.html" "x"))
      (mevedel-artifact-store-note-writes
       session (list (list :action 'delete
                           :path (file-name-concat store "flow/index.html"))))
      (should (= 2 (length (mevedel-artifact-store-versions workspace "flow"))))
      (should (equal '("flow") (mevedel-session-attached-artifacts session)))))

  :doc "versions the settled patch bytes even if another writer changed the primary file"
  (mevedel-artifact-store-test--with-workspace
    (let* ((session (mevedel-session--create :workspace workspace))
           (path (mevedel-artifact-store-test--write store "a/x.md" "newer")))
      (mevedel-artifact-store-note-writes
       session (list (list :action 'write :path path :content "this patch")))
      (should (equal "this patch"
                     (mevedel-artifact-store--read-bytes
                      (mevedel-artifact-store-version-path workspace "a" 1))))))

  :doc "writes outside the store are ignored"
  (mevedel-artifact-store-test--with-workspace
    (let ((session (mevedel-session--create :workspace workspace))
          (path (file-name-concat root "src/app.html")))
      (make-directory (file-name-directory path) t)
      (write-region "x" nil path nil 'silent)
      (mevedel-artifact-store-test--note session path)
      (should-not (mevedel-session-attached-artifacts session))
      (should-not (file-exists-p store)))))

(mevedel-deftest mevedel-artifact-store-record-version ()
  ,test
  (test)
  :doc "refuses a late authored-source link before copying external bytes into protected versions"
  (mevedel-artifact-store-test--with-workspace
    (let* ((outside (make-temp-file "artifact-version-outside-" t))
           (victim (mevedel-artifact-store-test--write outside "secret" "secret"))
           (path (mevedel-artifact-store-test--write store "a/x.md" "authored"))
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           swapped)
      (unwind-protect
          (progn
            (mevedel-artifact-store-create-meta workspace "a" "x.md")
            (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                       (lambda (operations &optional lock)
                         (when (and (not swapped)
                                    (cl-find-if (lambda (op)
                                                  (and (eq (plist-get op :op) 'read)
                                                       (equal (plist-get op :path) path))) operations))
                           (setq swapped t)
                           (delete-file path)
                           (make-symbolic-link victim path))
                         (funcall run operations lock))))
              (should-error (mevedel-artifact-store-record-version workspace "a")))
            (should swapped)
            (should-not (mevedel-artifact-store-versions workspace "a")))
        (delete-directory outside t))))

  :doc "drops the oldest beyond the count cap, with their files"
  (mevedel-artifact-store-test--with-workspace
    (let ((mevedel-artifact-store-max-versions 2)
          (session (mevedel-session--create :workspace workspace)))
      (dolist (content '("1" "2" "3"))
        (mevedel-artifact-store-test--note
         session (mevedel-artifact-store-test--write store "a/x.md" content)))
      (should (equal '(2 3) (mapcar (lambda (row) (plist-get row :n))
                                    (mevedel-artifact-store-versions workspace "a"))))
      (should-not (file-exists-p (file-name-concat store ".state/a/versions/000001.md")))))

  :doc "drops the oldest beyond the byte cap but always keeps the latest"
  (mevedel-artifact-store-test--with-workspace
    (let ((mevedel-artifact-store-max-version-bytes 5)
          (session (mevedel-session--create :workspace workspace)))
      (dolist (content '("abc" "def" "too large"))
        (mevedel-artifact-store-test--note
         session (mevedel-artifact-store-test--write store "a/x.md" content)))
      (should (equal '(3) (mapcar (lambda (row) (plist-get row :n))
                                  (mevedel-artifact-store-versions workspace "a")))))))

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

  :doc "restore refuses a late primary symlink without overwriting its external target"
  (mevedel-artifact-store-test--with-workspace
    (let* ((outside (make-temp-file "artifact-restore-outside-" t))
           (victim (mevedel-artifact-store-test--write outside "victim" "innocent"))
           (path (mevedel-artifact-store-test--write store "a/x.md" (make-string 70000 ?x)))
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           swapped)
      (unwind-protect
          (progn
            (mevedel-artifact-store-create-meta workspace "a" "x.md")
            (mevedel-artifact-store-record-version workspace "a")
            (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                       (lambda (operations &optional lock)
                         (when (and (not swapped)
                                    (cl-find-if (lambda (op)
                                                  (and (eq (plist-get op :op) 'write-mode)
                                                       (equal (plist-get op :path) path))) operations))
                           (setq swapped t)
                           (delete-file path)
                           (make-symbolic-link victim path))
                         (funcall run operations lock))))
              (should-error (mevedel-artifact-store-restore-version workspace "a" 1)))
            (should swapped)
            (should (equal "innocent" (mevedel-artifact-store--read-bytes victim)))
            (should (= 1 (length (mevedel-artifact-store-versions workspace "a")))))
        (delete-directory outside t))))

  :doc "restore versions its captured bytes even if an authored writer immediately changes the file"
  (mevedel-artifact-store-test--with-workspace
    (let* ((path (mevedel-artifact-store-test--write store "a/x.md" "old"))
           (run (symbol-function 'mevedel-session-control-fs-run-program))
           changed)
      (mevedel-artifact-store-create-meta workspace "a" "x.md")
      (mevedel-artifact-store-record-version workspace "a")
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (operations &optional lock)
                   (prog1 (funcall run operations lock)
                     (when (cl-find-if (lambda (op)
                                         (and (eq (plist-get op :op) 'write-mode)
                                              (equal (plist-get op :path) path))) operations)
                       (setq changed t)
                       (write-region "raced" nil path nil 'silent))))))
        (should (= 2 (mevedel-artifact-store-restore-version workspace "a" 1))))
      (should changed)
      (should (equal "old" (mevedel-artifact-store--read-bytes
                             (mevedel-artifact-store-version-path workspace "a" 2))))))

  :doc "queued restore acknowledges only after its version is durable"
  (let (finish result saved)
    (cl-letf (((symbol-function 'mevedel-artifact-store-meta)
               (lambda (&rest _) '(:kind document)))
              ((symbol-function 'mevedel-shared-editing-restore)
               (lambda (_workspace _id _n _actor callback) (setq finish callback)))
              ((symbol-function 'mevedel-shared-editing-save-version)
               (lambda (&rest _) (setq saved t) 4)))
      (should-not (mevedel-artifact-store-restore-version
                   'workspace "doc" 1 "session"
                   (lambda (reply) (setq result reply))))
      (should-not result)
      (funcall finish '(:ok t))
      (should saved)
      (should (equal '(:n 4) result))
      (setq result nil saved nil)
      (funcall finish '(:error "Rejected"))
      (should-not saved)
      (should (equal '(:error "Rejected") result))
      (cl-letf (((symbol-function 'mevedel-shared-editing-save-version)
                 (lambda (&rest _) (error "Version save failed"))))
        (funcall finish '(:ok t))
        (should (equal '(:error "Version save failed") result))))))

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
      (set-file-modes (mevedel-artifact-store-test--write store "a/assets/run.sh" "#!/bin/sh\n") #o755)
      (set-file-modes (file-name-concat store "a/assets") #o750)
      (make-directory (file-name-concat store "a/empty"))
      (dotimes (i 40)
        (mevedel-artifact-store-test--write store (format "a/asset-%02d" i) (format "%d" i)))
      (should (equal "b" (mevedel-artifact-store-duplicate workspace "a" "b")))
      (should (file-exists-p (file-name-concat store "b/img.png")))
      (should (equal "#!/bin/sh\n" (mevedel-artifact-store--read-bytes
                                      (file-name-concat store "b/assets/run.sh"))))
      (should (= #o755 (file-modes (file-name-concat store "b/assets/run.sh"))))
      (should (= #o750 (file-modes (file-name-concat store "b/assets"))))
      (dotimes (i 40)
        (should (equal (format "%d" i) (mevedel-artifact-store--read-bytes
                                         (file-name-concat store (format "b/asset-%02d" i))))))
      (should (file-directory-p (file-name-concat store "b/empty")))
      (should (equal "x.md" (plist-get (mevedel-artifact-store-meta workspace "b") :file)))
      (should (= 1 (length (mevedel-artifact-store-versions workspace "b"))))
      (should (= 2 (length (mevedel-artifact-store-versions workspace "a"))))
      (should-error (mevedel-artifact-store-duplicate workspace "a" "b"))
      ;; Comments stay with the original.
      (mevedel-artifact-store-test--write store ".state/a/comments.json" "{}")
      (mevedel-artifact-store-duplicate workspace "a" "c")
      (should-not (file-exists-p (file-name-concat store ".state/c/comments.json")))
      (should-error (mevedel-artifact-store-duplicate workspace "a" "../c"))))

  :doc "duplicate restores read-only directory modes after writing their children"
  (mevedel-artifact-store-test--with-workspace
    (let ((source (file-name-concat store "a/assets"))
          (target (file-name-concat store "b/assets")))
      (unwind-protect
          (progn
            (mevedel-artifact-store-test--write store "a/assets/x.md" "read-only")
            (mevedel-artifact-store-create-meta workspace "a" "assets/x.md")
            (set-file-modes source #o555)
            (should (equal "b" (mevedel-artifact-store-duplicate workspace "a" "b")))
            (should (= #o555 (file-modes target))))
        (set-file-modes source #o755)
        (when (file-directory-p target) (set-file-modes target #o755)))))

  :doc "duplicate pins both authored source reads and target writes against late parent swaps"
  (dolist (swap-source '(nil t))
    (mevedel-artifact-store-test--with-workspace
      (let* ((outside (make-temp-file "artifact-duplicate-outside-" t))
             (victim (mevedel-artifact-store-test--write outside "x.md" "secret"))
             (source (mevedel-artifact-store-test--write store "a/x.md" (make-string 70000 ?x)))
             (target (file-name-concat store "b/x.md"))
             (run (symbol-function 'mevedel-session-control-fs-run-program))
             swapped)
        (unwind-protect
            (progn
              (mevedel-artifact-store-create-meta workspace "a" "x.md")
              (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                         (lambda (operations &optional lock)
                           (when (and (not swapped)
                                      (cl-find-if
                                       (lambda (op)
                                         (and (eq (plist-get op :op) (if swap-source 'read 'write-mode))
                                              (equal (plist-get op :path) (if swap-source source target))))
                                       operations))
                             (setq swapped t)
                             (let ((directory (directory-file-name
                                               (file-name-directory (if swap-source source target)))))
                               (rename-file directory (concat directory "-original"))
                               (make-symbolic-link outside directory)))
                           (funcall run operations lock))))
                (should-error (mevedel-artifact-store-duplicate workspace "a" "b")))
              (should swapped)
              (should (equal "secret" (mevedel-artifact-store--read-bytes victim)))
              (should-not (file-exists-p (file-name-concat store ".state/b/meta.el"))))
          (delete-directory outside t)))))

  :doc "cleans up an incomplete duplicate without copying dedicated-session ownership"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-create-meta workspace "a" "missing.md")
    (mevedel-artifact-store-update-meta workspace "a" :dedicated-session "original")
    (should-error (mevedel-artifact-store-duplicate workspace "a" "b"))
    (should-not (file-exists-p (mevedel-artifact-store-artifact-directory workspace "b")))
    (should-not (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace "b")))
    (should (equal "original" (plist-get (mevedel-artifact-store-meta workspace "a") :dedicated-session))))

  :doc "failed version publication removes readonly duplicate directories and preserves its error"
  (mevedel-artifact-store-test--with-workspace
    (let ((source (file-name-concat store "a/assets"))
          (target (file-name-concat store "b/assets")))
      (unwind-protect
          (progn
            (mevedel-artifact-store-test--write store "a/assets/x.md" "read-only")
            (mevedel-artifact-store-create-meta workspace "a" "assets/x.md")
            (set-file-modes source #o555)
            (cl-letf (((symbol-function 'mevedel-artifact-store-record-version)
                       (lambda (&rest _) (error "Injected version publication failure"))))
              (should (equal '(error "Injected version publication failure")
                             (should-error (mevedel-artifact-store-duplicate workspace "a" "b")))))
            (should-not (file-exists-p (file-name-concat store "b")))
            (should-not (file-exists-p (file-name-concat store ".state/b")))
            (should (= #o555 (file-modes source))))
        (set-file-modes source #o755)
        (when (file-directory-p target) (set-file-modes target #o755)))))

  :doc "does not adopt or remove an existing orphan bookkeeping directory"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-test--write store "a/x.md" "original")
    (mevedel-artifact-store-create-meta workspace "a" "x.md")
    (let ((marker (mevedel-artifact-store-test--write store ".state/b/marker" "keep")))
      (should-error (mevedel-artifact-store-duplicate workspace "a" "b"))
      (should-not (file-exists-p (mevedel-artifact-store-artifact-directory workspace "b")))
      (should (equal "keep" (mevedel-artifact-store--read-bytes marker))))))

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
        (should (plist-get (cadr rows) :missing)))))

  :doc "batched listings preserve Unicode metadata and tolerate an absent version index"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-test--write store "a/x.md" "hello")
    (mevedel-artifact-store-create-meta workspace "a" "x.md" 'markdown "Caf\u00e9")
    (let ((row (car (mevedel-artifact-store-list workspace))))
      (should (equal "Caf\u00e9" (plist-get row :title)))
      (should (= 0 (plist-get row :versions)))
      (should (= 5 (plist-get row :size)))))

  :doc "batched metadata reads refuse symlink files and parent directories"
  (mevedel-artifact-store-test--with-workspace
    (let* ((session (mevedel-session--create :workspace workspace))
           (source (mevedel-artifact-store-test--write store "a/x.md" "hello")))
      (mevedel-artifact-store-test--note session source)
      (let* ((directory (mevedel-artifact-store-bookkeeping-directory workspace "a"))
             (metadata (file-name-concat directory "meta.el"))
             (saved (file-name-concat root "original-meta.el")))
        (rename-file metadata saved)
        (make-symbolic-link saved metadata)
        (should-error (mevedel-artifact-store-list workspace))
        (delete-file metadata)
        (rename-file saved metadata)
        (let ((versions (file-name-concat directory "versions"))
              (saved-versions (file-name-concat root "original-versions")))
          (rename-file versions saved-versions)
          (make-symbolic-link saved-versions versions)
          (should-error (mevedel-artifact-store-list workspace))
          (delete-file versions)
          (rename-file saved-versions versions))
        (rename-file source (file-name-concat root "original.md"))
        (make-symbolic-link (file-name-concat root "original.md") source)
        (should-error (mevedel-artifact-store-list workspace)))))

  :doc "remote listings prove all primary paths in one target program without component probes"
  (mevedel-artifact-store-test--with-workspace
    (let* ((source (mevedel-artifact-store-test--write store "a/nested/x.md" "hello"))
           (file-remote (symbol-function 'file-remote-p))
           (run-program (symbol-function 'mevedel-session-control-fs-run-program))
           (programs 0))
      (mevedel-artifact-store-create-meta workspace "a" "nested/x.md")
      (mevedel-artifact-store-create-meta workspace "b" "absent.md")
      ;; Select the remote listing branch while exercising real pinned target
      ;; operations locally, including absent files and refused symlinks.
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (path &rest args)
                   (if (equal path root) "remote" (apply file-remote path args))))
                ((symbol-function 'mevedel-resource-within-root-p)
                 (lambda (&rest _) (ert-fail "Unexpected component proof")))
                ((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (&rest args)
                   (cl-incf programs)
                   (apply run-program args))))
        (let ((rows (mevedel-artifact-store-list workspace)))
          (should (= 3 programs))
          (should (equal '("a" "b") (mapcar (lambda (row) (plist-get row :id)) rows)))
          (should (= 5 (plist-get (car rows) :size)))
          (should (plist-get (cadr rows) :missing)))
        (let ((saved (file-name-concat root "saved.md")))
          (rename-file source saved)
          (make-symbolic-link saved source)
          (should-error (mevedel-artifact-store-list workspace))
          (delete-file source)
          (rename-file saved source))
        (let ((parent (directory-file-name (file-name-directory source)))
              (saved (file-name-concat store "a/saved")))
          (rename-file parent saved)
          (make-symbolic-link saved parent)
          (should-error (mevedel-artifact-store-list workspace)))))))

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
  :doc "announces versions, new artifacts and deletions"
  (mevedel-artifact-store-test--with-workspace
    (let* (changed
           (mevedel-artifact-store-changed-functions
            (list (lambda (seen) (push seen changed)))))
      (mevedel-artifact-store-test--note
       (mevedel-session--create :workspace workspace)
       (mevedel-artifact-store-test--write store "a/x.md" "1"))
      (should (= (length changed) 1))
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
            (should (equal (list id) (mevedel-artifact-store-dedicated-ids workspace)))
            (should (file-exists-p (mevedel-session-artifacts-sidecar-path
                                    (mevedel-session-save-path session))))
            (should (eq buffer (mevedel-artifact-store-conversation workspace "flow")))
            ;; A running turn keeps its artifact.
            (cl-letf (((symbol-function 'mevedel-turn-busy-p) (lambda (&rest _) t)))
              (should-error (mevedel-artifact-store-delete workspace "flow")))
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
      (mevedel-workspace-clear-registry)))

  :doc "a losing candidate is closed and deleted before returning the winner"
  (pcase-let* ((`(,workspace . ,root)
                (test-mevedel-session-persistence--make-tempdir-workspace))
               (winner nil) (candidate nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--ensure-chat-preset) #'ignore))
          (mevedel-artifact-store-create-meta workspace "a" "index.html")
          (setq winner (mevedel-artifact-store-conversation workspace "a"))
          (let ((winner-id (mevedel-session-session-id
                            (buffer-local-value 'mevedel--session winner))))
            (mevedel-artifact-store-update-meta workspace "a" :dedicated-session nil)
            (cl-letf (((symbol-function 'mevedel-artifact-store--claim-conversation)
                       (lambda (_workspace _id _previous new)
                         (setq candidate new)
                         (mevedel-artifact-store-update-meta
                          workspace "a" :dedicated-session winner-id)
                         winner-id)))
              (should (eq winner (mevedel-artifact-store-conversation workspace "a"))))
            (should candidate)
            (should-not (mevedel-artifact-store--live-buffer workspace candidate))
            (should-not (file-exists-p
                         (file-name-concat
                          (mevedel-session-artifacts-sessions-dir workspace) candidate)))
            (should (buffer-live-p winner))))
      (when (buffer-live-p winner)
        (test-mevedel-session-persistence--release-and-kill
         winner (buffer-local-value 'mevedel--session winner)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-artifact-store--claim-conversation ()
  ,test
  (test)
  :doc "concurrent conversation creation keeps the winner and other metadata"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-create-meta workspace "a" "index.html")
    (let ((compare (symbol-function 'mevedel-artifact-store--compare-and-write))
          (raced nil))
      (cl-letf (((symbol-function 'mevedel-artifact-store--compare-and-write)
                 (lambda (&rest args)
                   (unless raced
                     (setq raced t)
                     (mevedel-artifact-store-update-meta
                      workspace "a" :dedicated-session "winner" :title "Renamed"))
                   (apply compare args))))
        (should (equal "winner" (mevedel-artifact-store--claim-conversation
                                  workspace "a" nil "loser"))))
      (should (equal "Renamed" (plist-get (mevedel-artifact-store-meta workspace "a") :title)))
      (should (equal "replacement" (mevedel-artifact-store--claim-conversation
                                     workspace "a" "winner" "replacement"))))))

(mevedel-deftest mevedel-artifact-store-bookkeeping-directory ()
  ,test
  (test)
  :doc "keeps metadata separate from the authored artifact directory"
  (mevedel-artifact-store-test--with-workspace
    (should (equal (file-name-concat store ".state/a")
                   (mevedel-artifact-store-bookkeeping-directory workspace "a")))
    (should-error (mevedel-artifact-store-bookkeeping-directory workspace ".."))))

(mevedel-deftest mevedel-artifact-store-primary-path ()
  ,test
  (test)
  :doc "routes shared item state to protected bookkeeping and files to authored paths"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-create-meta workspace "a" "index.html")
    (should (equal (file-name-concat store "a/index.html")
                   (mevedel-artifact-store-primary-path workspace "a")))
    (mevedel-artifact-store-create-meta workspace "b" "state.json" 'document "Board")
    (should (equal (file-name-concat store ".state/b/state.json")
                   (mevedel-artifact-store-primary-path workspace "b")))
    (should (equal '("a" "b") (mevedel-artifact-store-ids workspace)))))

(mevedel-deftest mevedel-artifact-store--relative-name-p ()
  ,test
  (test)
  :doc "accepts nested relative filenames and rejects traversal or empty components"
  (should (mevedel-artifact-store--relative-name-p "a/file.md"))
  (dolist (name '(nil "" "../x" "a/../x" "a//x" "/x"))
    (should-not (mevedel-artifact-store--relative-name-p name))))

(mevedel-deftest mevedel-artifact-store-meta ()
  ,test
  (test)
  :doc "invalid persisted paths cannot restore over files outside the artifact"
  (mevedel-artifact-store-test--with-workspace
    (let ((victim (mevedel-artifact-store-test--write root "victim.md" "original")))
      (mevedel-artifact-store-create-meta workspace "a" "x.md")
      (mevedel-artifact-store-test--write
       store ".state/a/meta.el"
       "(:kind markdown :title \"Bad\" :file \"../../../victim.md\")")
      (should-error (mevedel-artifact-store-restore-version workspace "a" 1))
      (should (equal "original" (mevedel-artifact-store--read-bytes victim))))))

(mevedel-deftest mevedel-artifact-store--path ()
  ,test
  (test)
  :doc "refuses traversal, reserved spellings and symlinks without touching targets"
  (mevedel-artifact-store-test--with-workspace
    (make-directory store t)
    (dolist (name '("../victim" "a/../victim" "/victim" "a//b" "a/./b" ""))
      (should-error (mevedel-artifact-store--path store name)))
    (make-symbolic-link root (file-name-concat store "linked"))
    (should-error (mevedel-artifact-store--path store "linked/victim"))
    (should (equal (file-name-concat store "nested/new.md")
                   (mevedel-artifact-store--path store "nested/new.md"))))

  :doc "refuses parent and leaf links inside and outside the artifact, including root ancestors"
  (mevedel-artifact-store-test--with-workspace
    (let* ((directory (file-name-concat store "a"))
           (source (mevedel-artifact-store-test--write store "a/nested/x.md" "hello"))
           (outside (make-temp-file "mevedel-store-path-outside-" t)))
      (unwind-protect
          (progn
            (mevedel-artifact-store-create-meta workspace "a" "nested/x.md")
            (dolist (destination (list (file-name-concat directory "saved") outside))
              (make-directory destination t)
              (let ((saved-file (file-name-concat destination "x.md"))
                    (saved-parent (file-name-concat destination "nested"))
                    (parent (directory-file-name (file-name-directory source))))
                (rename-file source saved-file)
                (make-symbolic-link saved-file source)
                (should-error (mevedel-artifact-store-list workspace))
                (delete-file source)
                (rename-file saved-file source)
                (rename-file parent saved-parent)
                (make-symbolic-link saved-parent (directory-file-name parent))
                (should-error (mevedel-artifact-store-list workspace))
                ;; DIRECTORY itself is below the linked ancestor in this call.
                (should-error (mevedel-artifact-store--path parent "x.md"))
                (should-error (mevedel-artifact-store--path
                               (file-name-concat parent "subdir") "missing.md"))
                (delete-file (directory-file-name parent))
                (rename-file saved-parent parent)))
            (should (equal source (mevedel-artifact-store--path directory "nested/x.md"))))
        (delete-directory outside t)))))

(mevedel-deftest mevedel-artifact-store--read-bytes ()
  ,test
  (test)
  :doc "reads exact binary bytes and distinguishes absence from unreadable directories"
  (mevedel-artifact-store-test--with-workspace
    (let ((path (mevedel-artifact-store-test--write store "bytes" (unibyte-string 0 255))))
      (should (equal (unibyte-string 0 255) (mevedel-artifact-store--read-bytes path)))
      (should-not (mevedel-artifact-store--read-bytes (file-name-concat store "missing")))
      (should-error (mevedel-artifact-store--read-bytes store)))))

(mevedel-deftest mevedel-artifact-store--validate-meta ()
  ,test
  (test)
  :doc "rejects malformed records and paths capable of overwriting unrelated files"
  (mevedel-artifact-store-test--with-workspace
    (dolist (meta '(garbage (:file 42) (:kind file :title "x" :file "../../victim")
                   (:kind file :title "x" :file "x.md" :dedicated-session "../session")))
      (should-error (mevedel-artifact-store--validate-meta meta store)))
    (should (mevedel-artifact-store--validate-meta
             '(:kind document :title "Document" :file "state.json") store))))

(mevedel-deftest mevedel-artifact-store--version-index ()
  ,test
  (test)
  :doc "rejects escaping and malformed index entries before restoration or pruning"
  (mevedel-artifact-store-test--with-workspace
    (dolist (row '((:n 1 :file "../../victim" :bytes 1 :time 0)
                  (:n 1 :file "index.el" :bytes 1 :time 0)
                  (:n 1 :file "000002.md" :bytes 1 :time 0)
                  (:n -1 :file "000001.md" :bytes 1 :time 0)
                  (:n 1 :file "000001.md" :bytes -1 :time 0)))
      (should-error (mevedel-artifact-store--version-index (prin1-to-string (list row)))))
    (should-not (mevedel-artifact-store--version-index nil))))

(mevedel-deftest mevedel-artifact-store--lock-directory ()
  ,test
  (test)
  :doc "uses a stable sibling of item lease directories and refuses symlink escape"
  (mevedel-artifact-store-test--with-workspace
    (let ((directory (mevedel-artifact-store--lock-directory workspace)))
      (should (file-directory-p directory))
      (should (equal directory (mevedel-artifact-store--lock-directory workspace)))
      (delete-directory directory)
      (make-symbolic-link root directory)
      (should-error (mevedel-artifact-store--lock-directory workspace)))))

(mevedel-deftest mevedel-artifact-store--compare-and-write ()
  ,test
  (test)
  :doc "stale expectations refuse writes and exact bytes publish under the target lock"
  (mevedel-artifact-store-test--with-workspace
    (let* ((path (mevedel-artifact-store-test--write store "index" "old"))
           (operation (list (list :op 'write :path path :content "new"))))
      (should-not (mevedel-artifact-store--compare-and-write workspace path nil operation))
      (should-not (mevedel-artifact-store--compare-and-write workspace path "stale" operation))
      (should (equal "old" (mevedel-artifact-store--read-bytes path)))
      (should (mevedel-artifact-store--compare-and-write workspace path "old" operation))
      (should (equal "new" (mevedel-artifact-store--read-bytes path))))))

(mevedel-deftest mevedel-artifact-store-update-meta ()
  ,test
  (test)
  :doc "preserves current fields and refuses invalid paths before publication"
  (mevedel-artifact-store-test--with-workspace
    (mevedel-artifact-store-create-meta workspace "a" "x.md")
    (mevedel-artifact-store-update-meta workspace "a" :title "Renamed")
    (should-error (mevedel-artifact-store-update-meta workspace "a" :file "../../victim"))
    (should (equal "Renamed" (plist-get (mevedel-artifact-store-meta workspace "a") :title)))
    (should (equal "x.md" (plist-get (mevedel-artifact-store-meta workspace "a") :file)))))

(ert-deftest mevedel-artifact-store-record-version/concurrent-processes ()
  "Two Emacs writers retain both versions when their first index reads race."
  (mevedel-artifact-store-test--with-workspace
   (mevedel-artifact-store-test--write "" (file-name-concat store "a/x.md") "original")
   (mevedel-artifact-store-create-meta workspace "a" "x.md")
   (let (processes)
     (unwind-protect
         (progn
           (dotimes (i 2)
             (let* ((script (file-name-concat root (format "child-%s.el" i)))
                    (ready (file-name-concat root (format "ready-%s" i)))
                    (gate (file-name-concat root "go")))
               (mevedel-artifact-store-test--write ""
                script
                (concat
                 ";;; -*- lexical-binding: t -*-\n"
                 (prin1-to-string
                  `(progn
                     (setq load-path ',load-path)
                     (require 'mevedel-artifact-store)
                     (let ((waited nil))
                       (advice-add 'mevedel-artifact-store--read-bytes :around
                                   (lambda (read file)
                                     (let ((bytes (funcall read file)))
                                       (when (and (not waited) (equal (file-name-nondirectory file) "index.el"))
                                         (setq waited t)
                                         (write-region "ready" nil ,ready nil 'silent)
                                         (while (not (file-exists-p ,gate)) (sleep-for 0.01)))
                                       bytes))))
                     (mevedel-artifact-store-record-version
                      (mevedel-workspace--create :type 'project :id "probe" :root ,root :name "probe")
                      "a" ,(format "client-%s" i) ,(format "content-%s" i))))))
               (push (make-process :name (format "store-probe-%s" i)
                                   :buffer (generate-new-buffer " *artifact-store-writer-test*")
                                   :command (list (expand-file-name invocation-name invocation-directory)
                                                  "-Q" "--batch" "-l" script)
                                   :sentinel #'ignore) processes)))
           (let ((deadline (+ (float-time) 20)))
             (while (and (< (float-time) deadline)
                         (cl-every #'process-live-p processes)
                         (not (and (file-exists-p (file-name-concat root "ready-0"))
                                   (file-exists-p (file-name-concat root "ready-1")))))
               (accept-process-output nil 0.05)))
           (ert-info ((mapconcat
                       (lambda (process)
                         (with-current-buffer (process-buffer process) (buffer-string)))
                       processes "\n"))
             (should (file-exists-p (file-name-concat root "ready-0")))
             (should (file-exists-p (file-name-concat root "ready-1"))))
           (mevedel-artifact-store-test--write "" (file-name-concat root "go") "go")
           (let ((deadline (+ (float-time) 20)))
             (while (and (< (float-time) deadline) (cl-some #'process-live-p processes))
               (accept-process-output nil 0.05)))
           (dolist (process processes)
             (should (eq 'exit (process-status process)))
             (should (zerop (process-exit-status process))))
           (let ((versions (mevedel-artifact-store-versions workspace "a")))
             (should (equal '(1 2) (mapcar (lambda (row) (plist-get row :n)) versions))))
           (should (equal '("content-0" "content-1")
                          (sort (mapcar (lambda (n)
                                          (mevedel-artifact-store--read-bytes
                                           (mevedel-artifact-store-version-path workspace "a" n)))
                                        '(1 2)) #'string<))))
       (dolist (process processes)
         (when (process-live-p process) (delete-process process))
         (when (buffer-live-p (process-buffer process))
           (kill-buffer (process-buffer process))))))))

(provide 'test-mevedel-artifact-store)
;;; test-mevedel-artifact-store.el ends here
