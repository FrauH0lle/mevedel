;;; test-mevedel-artifacts-list.el --- Artifacts cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the artifacts cockpit listing, open, attach, versions, duplicate,
;; and delete-as-unpublish.

;;; Code:

(require 'mevedel-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-report-test-support"))

(require 'mevedel-artifact-store)
(require 'mevedel-artifacts-list)
(require 'mevedel-cockpit)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-structs)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-artifacts-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view
        :workspace (and session (mevedel-session-workspace session))))

(defmacro mevedel-artifacts-list-test--with-store (&rest body)
  "Run BODY with `workspace', `store', and a pid-lock `session' in it."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "mevedel-artifacts-cockpit-" t)))
          (workspace (mevedel-workspace--create :type 'project :id "w"
                                                :root root :name "proj"))
          (store (mevedel-artifact-store-directory workspace))
          (session (mevedel-session--create :name "main" :workspace workspace
                                            :authority-mode 'pid-lock))
          (view (generate-new-buffer " *artifacts-list-view*"))
          (data (generate-new-buffer " *artifacts-list-data*")))
     (unwind-protect (progn ,@body)
       (when-let* ((buffer (get-buffer mevedel-artifacts-list-buffer-name)))
         (kill-buffer buffer))
       (when (buffer-live-p view) (kill-buffer view))
       (when (buffer-live-p data) (kill-buffer data))
       (delete-directory root t))))

(defun mevedel-artifacts-list-test--artifact (session store id file content)
  "Create store artifact ID with FILE holding CONTENT, written by SESSION."
  (let ((path (file-name-concat store id file)))
    (make-directory (file-name-directory path) t)
    (write-region content nil path nil 'silent)
    (mevedel-artifact-store-note-writes
     session (list (list :action 'write :path path)))
    path))

(mevedel-deftest mevedel-artifacts-list-open ()
  ,test
  (test)
  :doc "lists the store with attachment and versions, and shows details"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "mockup" "index.html" "<h1>hi</h1>")
    (mevedel-artifacts-list-test--artifact
     (mevedel-session--create :workspace workspace) store "notes" "notes.md" "notes")
    (let ((buffer (mevedel-artifacts-list-open
                   (mevedel-artifacts-list-test--context session view data))))
      (with-current-buffer buffer
        (should (eq major-mode 'mevedel-artifacts-list-mode))
        (should (= 2 (length tabulated-list-entries)))
        (should (string-match-p
                 "mevedel: artifacts.*main.*2 artifacts"
                 (substring-no-properties
                  (mevedel-cockpit-surface-header-line))))
        (let ((row (cadr (assoc "mockup" tabulated-list-entries))))
          (should (equal "html" (aref row 1)))
          (should (equal "✓" (aref row 2)))
          (should (equal "1" (aref row 3))))
        (should (equal "" (aref (cadr (assoc "notes" tabulated-list-entries)) 2)))
        (mevedel-artifacts-list-toggle-attached)
        (should (equal '("mockup") (mapcar #'car tabulated-list-entries)))
        (should (string-match-p "(attached)" (mevedel-cockpit-surface-header-line)))
        (let ((item (mevedel-cockpit-surface-selected)))
          (should (string-match-p
                   "Path +.*artifacts/mockup/index.html"
                   (mevedel-report-test-text
                    (mevedel-artifacts-list--details item nil))))))))

  :doc "opens from a plain project buffer without a session"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "mockup" "index.html" "x")
    (with-temp-buffer
      (cl-letf (((symbol-function 'mevedel-workspace) (lambda (&rest _) workspace)))
        (let ((origin (current-buffer)))
          (with-current-buffer (mevedel-artifacts)
            (should (equal '("mockup") (mapcar #'car tabulated-list-entries)))
            (should (string-match-p "artifacts.*proj.*1 artifact"
                                    (mevedel-cockpit-surface-header-line)))
            (should (equal "" (aref (cadar tabulated-list-entries) 2)))
            (should-error (mevedel-artifacts-list-attach) :type 'user-error)
            (mevedel-artifacts-list-quit)
            (should (buffer-live-p origin))
            (should-not (get-buffer mevedel-artifacts-list-buffer-name))))))))

(mevedel-deftest mevedel-artifacts-list-count ()
  ,test
  (test)
  :doc "counts attached and project artifacts from ids, ignoring stale ids"
  (mevedel-artifacts-list-test--with-store
    (should (equal '(0 . 0) (mevedel-artifacts-list-count session)))
    (should (equal '(0 . 0) (mevedel-artifacts-list-count nil)))
    (mevedel-artifacts-list-test--artifact session store "a" "x.md" "1")
    (mevedel-artifacts-list-test--artifact
     (mevedel-session--create :workspace workspace) store "b" "y.md" "1")
    (setf (mevedel-session-attached-artifacts session)
          (append (mevedel-session-attached-artifacts session) '("gone")))
    (cl-letf (((symbol-function 'mevedel-artifact-store-list)
               (lambda (&rest _) (ert-fail "Count read artifact state"))))
      (should (equal '(1 . 2) (mevedel-artifacts-list-count session))))))

(mevedel-deftest mevedel-artifacts-list-attach (:quiet t)
  ,test
  (test)
  :doc "attaches the selected artifact and writes the sidecar"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact
     (mevedel-session--create :workspace workspace) store "a" "x.md" "1")
    (let (written)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'mevedel-session-persistence-write-sidecar-now)
                   (lambda (&rest args) (setq written args))))
          (mevedel-artifacts-list-attach))
        (should (equal '("a") (mevedel-session-attached-artifacts session)))
        (should (equal (list session data) written))
        (should (equal "✓" (aref (cadar tabulated-list-entries) 2)))))))

(mevedel-deftest mevedel-artifacts-list-versions (:quiet t)
  ,test
  (test)
  :doc "restores a chosen version and tells the workspace's rooms"
  (mevedel-artifacts-list-test--with-store
    (let ((path (mevedel-artifacts-list-test--artifact session store "a" "x.md" "old"))
          notified)
      (mevedel-artifacts-list-test--artifact session store "a" "x.md" "new")
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (car (cl-find 1 choices :key #'cdr))))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?r "restore")))
                  ((symbol-function 'mevedel-collaboration-notify-artifacts-changed)
                   (lambda (seen) (setq notified seen))))
          (mevedel-artifacts-list-versions))
        (should (eq workspace notified))
        (should (equal "old" (with-temp-buffer (insert-file-contents path)
                                               (buffer-string))))
        (should (equal "3" (aref (cadar tabulated-list-entries) 3))))))

  :doc "a queued restore refreshes its originating cockpit only on completion"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "a" "x.md" "old")
    (let ((cockpit (mevedel-artifacts-list-open
                    (mevedel-artifacts-list-test--context session view data)))
          finish refreshed)
      (with-current-buffer cockpit
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?r "restore")))
                  ((symbol-function 'mevedel-artifact-store-restore-version)
                   (lambda (_workspace _id _n _session _actor callback)
                     (setq finish callback))))
          (mevedel-artifacts-list-versions)))
      (cl-letf (((symbol-function 'mevedel-cockpit-surface-refresh)
                 (lambda (&rest _) (setq refreshed (current-buffer)))))
        (should-not refreshed)
        (funcall finish '(:error "Refused"))
        (should-not refreshed)
        (funcall finish '(:n 2))
        (should (eq cockpit refreshed))))))

(mevedel-deftest mevedel-artifacts-list-duplicate (:quiet t)
  ,test
  (test)
  :doc "copies the artifact under a new id and attaches it"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "a" "x.md" "1")
    (with-current-buffer (mevedel-artifacts-list-open
                          (mevedel-artifacts-list-test--context session view data))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "a-copy"))
                ((symbol-function 'mevedel-session-persistence-write-sidecar-now)
                 #'ignore)
                ((symbol-function 'mevedel-collaboration-notify-artifacts-changed)
                 #'ignore))
        (mevedel-artifacts-list-duplicate))
      (should (equal '("a" "a-copy") (mevedel-session-attached-artifacts session)))
      (should (assoc "a-copy" tabulated-list-entries)))))

(mevedel-deftest mevedel-artifacts-list-open-browser ()
  ,test
  (test)
  :doc "opens local files in the browser and rejects a stale selection"
  (let ((path (make-temp-file "mevedel-artifact-open-" nil ".html"))
        opened item)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
                   (lambda (&optional _) item))
                  ((symbol-function 'mevedel-cockpit-surface-context)
                   (lambda (&optional _) nil))
                  ((symbol-function 'mevedel-cockpit-surface-refresh)
                   (lambda (&optional _) nil))
                  ((symbol-function 'browse-url-of-file)
                   (lambda (seen) (setq opened seen))))
          (setq item (list :name "open.html" :path path))
          (mevedel-artifacts-list-open-browser)
          (should (equal path opened))
          (setq item (list :name "gone.html"
                           :path "/missing/mevedel-artifact.html"))
          (should-error (mevedel-artifacts-list-open-browser)
                        :type 'user-error))
      (when (file-exists-p path) (delete-file path))))

  :doc "opens a shared item in the session's room"
  (let ((data (generate-new-buffer " *artifacts-open-item*"))
        opened)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-cockpit-surface-selected)
                   (lambda (&optional _) (list :name "Whiteboard · Plan" :item "ab12")))
                  ((symbol-function 'mevedel-cockpit-surface-context)
                   (lambda (&optional _)
                     (mevedel-artifacts-list-test--context nil nil data)))
                  ((symbol-function 'mevedel-collaboration-open-shared-item)
                   (lambda (buffer id) (setq opened (list buffer id)))))
          (mevedel-artifacts-list-open-browser)
          (should (equal (list data "ab12") opened)))
      (kill-buffer data))))

(mevedel-deftest mevedel-artifacts-list-conversation ()
  ,test
  (test)
  :doc "displays the selected artifact's dedicated session"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "a" "x.md" "1")
    (let (shown)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'mevedel-artifact-store-conversation)
                   (lambda (seen id) (should (eq seen workspace)) (concat "buffer-" id)))
                  ((symbol-function 'mevedel--display-chat-buffer)
                   (lambda (buffer) (setq shown buffer))))
          (mevedel-artifacts-list-conversation)))
      (should (equal "buffer-a" shown)))))

(mevedel-deftest mevedel-artifacts-list-save-version (:quiet t)
  ,test
  (test)
  :doc "saves a whiteboard's version and refuses file artifacts"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--artifact session store "page" "index.html" "x")
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (write-region "{}" nil (mevedel-artifact-store-primary-path workspace "board")
                  nil 'silent)
    (let (saved)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'mevedel-shared-editing-save-version)
                   (lambda (_workspace id session-id) (push (list id session-id) saved) 2)))
          (mevedel-cockpit-goto-id "board")
          (should (equal "board" (plist-get (mevedel-cockpit-surface-selected) :item)))
          (mevedel-artifacts-list-save-version)
          (should (equal (list (list "board" (mevedel-session-session-id session))) saved))
          (mevedel-cockpit-goto-id "page")
          (should-error (mevedel-artifacts-list-save-version) :type 'user-error)
          ;; A whiteboard has no file to visit.
          (mevedel-cockpit-goto-id "board")
          (should-error (mevedel-artifacts-list-visit) :type 'user-error))))))

(mevedel-deftest mevedel-artifacts-list-delete (:quiet t)
  ,test
  (test)
  :doc "deletes the artifact with its versions and tells the workspace's rooms"
  (mevedel-artifacts-list-test--with-store
    (let ((path (mevedel-artifacts-list-test--artifact
                 session store "mockup" "index.html" "<h1>hi</h1>"))
          notified)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        ;; A declined confirmation deletes nothing.
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
          (mevedel-artifacts-list-delete))
        (should (file-exists-p path))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function
                    'mevedel-collaboration-notify-artifacts-changed)
                   (lambda (seen) (setq notified seen))))
          (mevedel-artifacts-list-delete))
        (should-not (file-exists-p (file-name-concat store "mockup")))
        (should (eq workspace notified))
        (should-not tabulated-list-entries))))

  :doc "deletes a shared item's identity through its queue despite its protected path"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (write-region "{}" nil (mevedel-artifact-store-primary-path workspace "board")
                  nil 'silent)
    (let (queued)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'mevedel-shared-editing-call)
                   (lambda (seen args _callback)
                     (should (eq workspace seen))
                     (setq queued args))))
          (mevedel-artifacts-list-delete)))
      (should (equal queued '(:action "delete" :id "board" :actor "Host"))))))

(mevedel-deftest mevedel-artifacts-list-quit ()
  ,test
  (test)
  :doc "quits through the shared cockpit quit"
  (let (label)
    (cl-letf (((symbol-function 'mevedel-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (mevedel-artifacts-list-quit))
    (should (equal "artifacts cockpit" label))))

(provide 'test-mevedel-artifacts-list)
;;; test-mevedel-artifacts-list.el ends here
