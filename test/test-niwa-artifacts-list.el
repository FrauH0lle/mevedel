;;; test-niwa-artifacts-list.el --- Artifacts cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the artifacts cockpit listing, open, attach, versions, duplicate,
;; and delete-as-unpublish.

;;; Code:

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'niwa-artifact-backend)
(require 'niwa-artifact-store)
(require 'niwa-artifacts-list)
(require 'niwa-cockpit)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-artifact-room)
(require 'niwa-remote-artifact)
(require 'niwa-shared-editing)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-artifacts-list-test--context (session view data)
  "Return a cockpit context for SESSION, VIEW, and DATA."
  (list :session session
        :view-buffer view
        :data-buffer data
        :origin-buffer view
        :workspace (and session (niwa-session-workspace session))))

(defmacro niwa-artifacts-list-test--with-store (&rest body)
  "Run BODY with `workspace', `store', and a process-local `session' in it."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "niwa-artifacts-cockpit-" t)))
          (workspace (niwa-workspace--create :type 'project :id "w"
                                                :root root :name "proj"))
          (store (niwa-artifact-store-directory workspace))
          (session (niwa-session--create :name "main" :workspace workspace
                                            ))
          (view (generate-new-buffer " *artifacts-list-view*"))
          (data (generate-new-buffer " *artifacts-list-data*")))
     (unwind-protect (progn ,@body)
       (when-let* ((buffer (get-buffer niwa-artifacts-list-buffer-name)))
         (kill-buffer buffer))
       (when (buffer-live-p view) (kill-buffer view))
       (when (buffer-live-p data) (kill-buffer data))
       (delete-directory root t))))

(defun niwa-artifacts-list-test--artifact (session store id file content)
  "Create store artifact ID with FILE holding CONTENT, written by SESSION."
  (let ((path (file-name-concat store id file)))
    (make-directory (file-name-directory path) t)
    (write-region content nil path nil 'silent)
    (niwa-artifact-store-note-writes
     session (list (list :action 'write :path path)))
    path))

(niwa-deftest niwa-artifacts-list-open ()
  ,test
  (test)
  :doc "lists the store with attachment and versions, and shows details"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "mockup" "index.html" "<h1>hi</h1>")
    (niwa-artifacts-list-test--artifact
     (niwa-session--create :workspace workspace) store "notes" "notes.md" "notes")
    (let ((buffer (niwa-artifacts-list-open
                   (niwa-artifacts-list-test--context session view data))))
      (with-current-buffer buffer
        (should (eq major-mode 'niwa-artifacts-list-mode))
        (should (= 2 (length tabulated-list-entries)))
        (should (string-match-p
                 "niwa: artifacts.*main.*2 artifacts"
                 (substring-no-properties
                  (niwa-cockpit-surface-header-line))))
        (let ((row (cadr (assoc "mockup" tabulated-list-entries))))
          (should (equal "mockup" (aref row 0)))
          (should (equal "html" (aref row 1)))
          (should (equal "✓" (aref row 2)))
          (should (equal "1" (aref row 3))))
        (should (equal "" (aref (cadr (assoc "notes" tabulated-list-entries)) 2)))
        (niwa-artifacts-list-toggle-attached)
        (should (equal '("mockup") (mapcar #'car tabulated-list-entries)))
        (should (string-match-p "(attached)" (niwa-cockpit-surface-header-line)))
        (let ((item (niwa-cockpit-surface-selected)))
          (should (string-match-p
                   "Path +.*artifacts/mockup/index.html"
                   (niwa-report-test-text
                    (niwa-artifacts-list--details item nil))))))))

  :doc "shows titles for files and shared items while selecting by stable id"
  (niwa-artifacts-list-test--with-store
    (let ((board-id (make-string 64 ?a))
          (document-id (make-string 64 ?b))
          (file-id (make-string 64 ?c)))
      (niwa-artifact-store-create-meta
       workspace board-id "state.json" 'whiteboard "Project plan")
      (niwa-artifact-store-create-meta
       workspace document-id "state.json" 'document "Project plan")
      (niwa-artifact-store-create-meta
       workspace file-id "index.html" 'html "Preview")
      (niwa-artifacts-list-test--artifact
       session store file-id "index.html" "<h1>Preview</h1>")
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (dolist (id (list board-id document-id file-id))
          (should (equal (if (equal id file-id) "Preview" "Project plan")
                         (aref (cadr (assoc id tabulated-list-entries)) 0)))
          (should-not (string-match-p id (buffer-string)))
          (niwa-cockpit-goto-id id)
          (should (equal id (tabulated-list-get-id)))
          (should (equal id (plist-get (niwa-cockpit-surface-selected) :id)))
          (niwa-cockpit-surface-refresh id)
          (should (equal id (tabulated-list-get-id))))
        (should (string-match-p "Project plan" (buffer-string)))
        (should (string-match-p "Preview" (buffer-string)))
        (niwa-cockpit-goto-id board-id)
        (should (equal board-id
                       (plist-get (niwa-cockpit-surface-selected) :item)))
        (should (string-match-p
                 (concat "Id +" board-id)
                 (niwa-report-test-text
                  (niwa-artifacts-list--details
                   (niwa-cockpit-surface-selected) nil)))))))

  :doc "multiline titles stay in one cockpit row without changing stored titles"
  (niwa-artifacts-list-test--with-store
    (let ((title "Project\nplan\tphase\rone"))
      (niwa-artifact-store-create-meta
       workspace "board" "state.json" 'whiteboard title)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (ert-info ((buffer-substring-no-properties (point-min) (point-max)))
          (should (= 1 (count-lines (point-min) (point-max)))))
        (should (equal "Project plan phase one"
                       (aref (cadr (assoc "board" tabulated-list-entries)) 0)))
        (niwa-cockpit-goto-id "board")
        (let ((item (niwa-cockpit-surface-selected)))
          (should (equal "board" (plist-get item :id)))
          (should (equal title (plist-get item :title)))))))

  :doc "marks the artifacts shared through artifact links"
  (niwa-artifacts-list-test--with-store
    (niwa-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (niwa-artifact-store-create-meta workspace "notes" "state.json" 'document "Notes")
    (let ((link (niwa-artifact-store-link-path workspace "board")))
      (make-directory (file-name-directory link) t)
      (write-region "" nil link nil 'silent))
    (with-current-buffer (niwa-artifacts-list-open
                          (niwa-artifacts-list-test--context session view data))
      (should (equal "shared" (aref (cadr (assoc "board" tabulated-list-entries)) 6)))
      (should (equal "" (aref (cadr (assoc "notes" tabulated-list-entries)) 6)))
      (niwa-cockpit-goto-id "board")
      (should (string-match-p "Shared +yes"
                              (niwa-report-test-text
                               (niwa-artifacts-list--details
                                (niwa-cockpit-surface-selected) nil))))))

  :doc "opens from a plain project buffer without a session"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "mockup" "index.html" "x")
    (with-temp-buffer
      (cl-letf (((symbol-function 'niwa-workspace) (lambda (&rest _) workspace)))
        (let ((origin (current-buffer)))
          (with-current-buffer (niwa-artifacts)
            (should (equal '("mockup") (mapcar #'car tabulated-list-entries)))
            (should (string-match-p "artifacts.*proj.*1 artifact"
                                    (niwa-cockpit-surface-header-line)))
            (should (equal "" (aref (cadar tabulated-list-entries) 2)))
            (should-error (niwa-artifacts-list-attach) :type 'user-error)
            (niwa-artifacts-list-quit)
            (should (buffer-live-p origin))
            (should-not (get-buffer niwa-artifacts-list-buffer-name))))))))

(niwa-deftest niwa-artifacts-list--entry
  (:doc "sorts Ver and Size by their numbers, not their text")
  (let ((rows (mapcar (lambda (item) (niwa-artifacts-list--entry item nil))
                      (list (list :row "small" :name "small" :versions 10
                                  :size 980 :modified 0)
                            (list :row "large" :name "large" :versions 9
                                  :size 1572864 :modified 0)))))
    (pcase-dolist (`(,column . ,first) '((3 . "large") (5 . "small")))
      (should (equal first (car (car (sort (copy-sequence rows)
                                           (niwa-cockpit-sort-by-number
                                            column)))))))))

(niwa-deftest niwa-artifacts-list-count ()
  ,test
  (test)
  :doc "counts attached and project artifacts from ids, ignoring stale ids"
  (niwa-artifacts-list-test--with-store
    (should (equal '(0 . 0) (niwa-artifacts-list-count session)))
    (should (equal '(0 . 0) (niwa-artifacts-list-count nil)))
    (niwa-artifacts-list-test--artifact session store "a" "x.md" "1")
    (niwa-artifacts-list-test--artifact
     (niwa-session--create :workspace workspace) store "b" "y.md" "1")
    (setf (niwa-session-attached-artifacts session)
          (append (niwa-session-attached-artifacts session) '("gone")))
    (cl-letf (((symbol-function 'niwa-artifact-store-list)
               (lambda (&rest _) (ert-fail "Count read artifact state"))))
      (should (equal '(1 . 2) (niwa-artifacts-list-count session))))))

(niwa-deftest niwa-artifacts-list-attach (:quiet t)
  ,test
  (test)
  :doc "attaches the selected artifact and writes the sidecar"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact
     (niwa-session--create :workspace workspace) store "a" "x.md" "1")
    (let (written)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'niwa-session-persistence-write-sidecar-now)
                   (lambda (&rest args) (setq written args))))
          (niwa-artifacts-list-attach))
        (should (equal '("a") (niwa-session-attached-artifacts session)))
        (should (equal (list session data) written))
        (should (equal "✓" (aref (cadar tabulated-list-entries) 2)))))))

(niwa-deftest niwa-artifacts-list-versions (:quiet t)
  ,test
  (test)
  :doc "restores a chosen version and tells the workspace's rooms"
  (niwa-artifacts-list-test--with-store
    (let ((path (niwa-artifacts-list-test--artifact session store "a" "x.md" "old"))
          notified)
      (niwa-artifacts-list-test--artifact session store "a" "x.md" "new")
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (car (cl-find 1 choices :key #'cdr))))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?r "restore")))
                  ((symbol-function 'niwa-collaboration-notify-artifacts-changed)
                   (lambda (seen) (setq notified seen))))
          (niwa-artifacts-list-versions))
        (should (eq workspace notified))
        (should (equal "old" (with-temp-buffer (insert-file-contents path)
                                               (buffer-string))))
        (should (equal "3" (aref (cadar tabulated-list-entries) 3))))))

  :doc "a queued restore refreshes its originating cockpit only on completion"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "a" "x.md" "old")
    (let ((cockpit (niwa-artifacts-list-open
                    (niwa-artifacts-list-test--context session view data)))
          finish refreshed)
      (with-current-buffer cockpit
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (&rest _) '(?r "restore")))
                  ((symbol-function 'niwa-artifact-store-restore-version)
                   (lambda (_workspace _id _n _session _actor callback)
                     (setq finish callback))))
          (niwa-artifacts-list-versions)))
      (cl-letf (((symbol-function 'niwa-cockpit-surface-refresh)
                 (lambda (&rest _) (setq refreshed (current-buffer)))))
        (should-not refreshed)
        (funcall finish '(:error "Refused"))
        (should-not refreshed)
        (funcall finish '(:n 2))
        (should (eq cockpit refreshed))))))

(niwa-deftest niwa-artifacts-list-duplicate (:quiet t)
  ,test
  (test)
  :doc "copies the artifact under a new id and attaches it"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "a" "x.md" "1")
    (with-current-buffer (niwa-artifacts-list-open
                          (niwa-artifacts-list-test--context session view data))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "a-copy"))
                ((symbol-function 'niwa-session-persistence-write-sidecar-now)
                 #'ignore)
                ((symbol-function 'niwa-collaboration-notify-artifacts-changed)
                 #'ignore))
        (niwa-artifacts-list-duplicate))
      (should (equal '("a" "a-copy") (niwa-session-attached-artifacts session)))
      (should (assoc "a-copy" tabulated-list-entries)))))

(niwa-deftest niwa-artifacts-list-open-browser ()
  ,test
  (test)
  :doc "opens local files in the browser and rejects a stale selection"
  (let ((path (make-temp-file "niwa-artifact-open-" nil ".html"))
        opened item)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
                   (lambda (&optional _) item))
                  ((symbol-function 'niwa-cockpit-surface-context)
                   (lambda (&optional _) nil))
                  ((symbol-function 'niwa-cockpit-surface-refresh)
                   (lambda (&optional _) nil))
                  ((symbol-function 'browse-url-of-file)
                   (lambda (seen) (setq opened seen))))
          (setq item (list :name "open.html" :path path))
          (niwa-artifacts-list-open-browser)
          (should (equal path opened))
          (setq item (list :name "gone.html"
                           :path "/missing/niwa-artifact.html"))
          (should-error (niwa-artifacts-list-open-browser)
                        :type 'user-error))
      (when (file-exists-p path) (delete-file path))))

  :doc "opens a shared item in the session's room"
  (let ((data (generate-new-buffer " *artifacts-open-item*"))
        opened)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-cockpit-surface-selected)
                   (lambda (&optional _) (list :name "Whiteboard · Plan" :item "ab12")))
                  ((symbol-function 'niwa-cockpit-surface-context)
                   (lambda (&optional _)
                     (niwa-artifacts-list-test--context nil nil data)))
                  ((symbol-function 'niwa-collaboration-open-shared-item)
                   (lambda (buffer id) (setq opened (list buffer id)))))
          (niwa-artifacts-list-open-browser)
          (should (equal (list data "ab12") opened)))
      (kill-buffer data))))

(niwa-deftest niwa-artifacts-list-conversation ()
  ,test
  (test)
  :doc "displays the selected artifact's dedicated session"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "a" "x.md" "1")
    (let (shown)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'niwa-artifact-store-conversation)
                   (lambda (seen id) (should (eq seen workspace)) (concat "buffer-" id)))
                  ((symbol-function 'niwa--display-chat-buffer)
                   (lambda (buffer) (setq shown buffer))))
          (niwa-artifacts-list-conversation)))
      (should (equal "buffer-a" shown)))))

(niwa-deftest niwa-artifacts-list-save-version (:quiet t)
  ,test
  (test)
  :doc "saves a whiteboard's version and refuses file artifacts"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "page" "index.html" "x")
    (niwa-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (write-region "{}" nil (niwa-artifact-store-primary-path workspace "board")
                  nil 'silent)
    (let (saved)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'niwa-shared-editing-save-version)
                   (lambda (_workspace id session-id) (push (list id session-id) saved) 2)))
          (niwa-cockpit-goto-id "board")
          (should (equal "board" (plist-get (niwa-cockpit-surface-selected) :item)))
          (niwa-artifacts-list-save-version)
          (should (equal (list (list "board" (niwa-session-session-id session))) saved))
          (niwa-cockpit-goto-id "page")
          (should-error (niwa-artifacts-list-save-version) :type 'user-error)
          ;; A whiteboard has no file to visit.
          (niwa-cockpit-goto-id "board")
          (should-error (niwa-artifacts-list-visit) :type 'user-error))))))

(niwa-deftest niwa-artifacts-list-delete (:quiet t)
  ,test
  (test)
  :doc "deletes the artifact with its versions and tells the workspace's rooms"
  (niwa-artifacts-list-test--with-store
    (let ((path (niwa-artifacts-list-test--artifact
                 session store "mockup" "index.html" "<h1>hi</h1>"))
          notified)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        ;; A declined confirmation deletes nothing.
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
          (niwa-artifacts-list-delete))
        (should (file-exists-p path))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function
                    'niwa-collaboration-notify-artifacts-changed)
                   (lambda (seen) (setq notified seen))))
          (niwa-artifacts-list-delete))
        (should-not (file-exists-p (file-name-concat store "mockup")))
        (should (eq workspace notified))
        (should-not tabulated-list-entries))))

  :doc "deletes a shared item's identity through its queue despite its protected path"
  (niwa-artifacts-list-test--with-store
    (niwa-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
    (write-region "{}" nil (niwa-artifact-store-primary-path workspace "board")
                  nil 'silent)
    (let (queued)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'niwa-shared-editing-call)
                   (lambda (seen args _callback)
                     (should (eq workspace seen))
                     (setq queued args))))
          (niwa-artifacts-list-delete)))
      (should (equal queued '(:action "delete" :id "board" :actor "Host"))))))

(niwa-deftest niwa-artifacts-list-share ()
  ,test
  (test)
  :doc "shares, rotates and revokes the selected artifact in the cockpit's workspace"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "mockup" "index.html" "<h1>hi</h1>")
    (let ((link (niwa-artifact-store-link-path workspace "mockup")) calls)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'niwa-artifact-store-shared-p)
                   (lambda (&rest _) (ert-fail "Unexpected scalar sharing probe")))
                  ((symbol-function 'niwa-collaboration-share-artifact)
                   (lambda (id seen)
                     (push (list 'share id seen) calls)
                     (write-region "private" nil link nil 'silent)))
                  ((symbol-function 'niwa-collaboration-rotate-artifact)
                   (lambda (id seen) (push (list 'rotate id seen) calls)))
                  ((symbol-function 'niwa-collaboration-revoke-artifact)
                   (lambda (id seen)
                     (push (list 'revoke id seen) calls)
                     (delete-file link))))
          ;; An unshared artifact has no links to replace or revoke.
          (should-error (niwa-artifacts-list-rotate) :type 'user-error)
          (should-error (niwa-artifacts-list-revoke) :type 'user-error)
          (niwa-artifacts-list-share)
          (should (equal "shared" (aref (tabulated-list-get-entry) 6)))
          (niwa-artifacts-list-rotate)
          (niwa-artifacts-list-revoke)
          (should (equal "" (aref (tabulated-list-get-entry) 6)))))
      (should (equal (list (list 'share "mockup" workspace)
                           (list 'rotate "mockup" workspace)
                           (list 'revoke "mockup" workspace))
                     (nreverse calls)))))

  :doc "attaches a remote artifact from a link read without echo"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--artifact session store "mockup" "index.html" "<h1>hi</h1>")
    (let (attached)
      (with-current-buffer (niwa-artifacts-list-open
                            (niwa-artifacts-list-test--context session view data))
        (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "LINK"))
                  ((symbol-function 'niwa-remote-artifact-attach)
                   (lambda (seen link name) (setq attached (list seen link name)) "board")))
          (call-interactively #'niwa-artifacts-list-attach-remote)))
      (should (equal (list workspace "LINK" nil) attached)))))

(niwa-deftest niwa-artifacts-list-quit ()
  ,test
  (test)
  :doc "quits through the shared cockpit quit"
  (let (label)
    (cl-letf (((symbol-function 'niwa-cockpit-quit)
               (lambda (&optional seen) (setq label seen))))
      (niwa-artifacts-list-quit))
    (should (equal "artifacts cockpit" label))))

(defmacro niwa-artifacts-list-test--with-editing (&rest body)
  "Run BODY with private shared-editing runtimes and item leases."
  (declare (indent 0) (debug t))
  `(let ((niwa-shared-editing--runtimes (make-hash-table :test #'equal))
         (niwa-artifact-lease--held (make-hash-table :test #'equal))
         (niwa-session-durability--client-id (make-string 64 ?a)))
     (unwind-protect (progn ,@body)
       (niwa-shared-editing-stop)
       (niwa-artifact-lease-release-all))))

(niwa-deftest niwa-artifacts-new-shared-item ()
  ,test
  (test)
  :doc "creates, attaches, lists and opens a whiteboard from the session cockpit"
  (niwa-artifacts-list-test--with-store
    (niwa-artifacts-list-test--with-editing
      (let (prepared opened written)
        (with-current-buffer (niwa-artifacts-list-open
                              (niwa-artifacts-list-test--context session view data))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Whiteboard"))
                    ((symbol-function 'read-string) (lambda (&rest _) " Plan "))
                    ((symbol-function 'niwa-session-persistence-write-sidecar-now)
                     (lambda (&rest args) (setq written args)))
                    ((symbol-function 'niwa-collaboration-item-opener)
                     (lambda (&optional buffer)
                       (setq prepared (list buffer))
                       (lambda (buffer id) (setq opened (list buffer id))))))
            (niwa-test--with-captured-messages nil
              (call-interactively #'niwa-artifacts-new-shared-item)
              ;; Consent is settled before the item is queued.
              (should (equal (list data) prepared))
              (niwa-test--await 10 "the item opens" opened)))
          (let ((id (cadr opened)))
            (should (equal (list data id) opened))
            (should (equal '(:kind whiteboard :title "Plan")
                           (cl-subseq (niwa-artifact-store-meta workspace id) 0 4)))
            (should (equal (list id) (niwa-session-attached-artifacts session)))
            (should (equal (list session data) written))
            (should (assoc id tabulated-list-entries))
            (should (equal id (plist-get (niwa-cockpit-surface-selected) :id))))))))

  :doc "without a session, opens the new document in its dedicated conversation"
  (niwa-artifacts-list-test--with-store
    (let ((conversation (generate-new-buffer " *artifact-conversation*"))
          prepared opened messages)
      (unwind-protect
          (with-current-buffer (niwa-artifacts-list-open
                                (niwa-cockpit-workspace-context workspace))
            (cl-letf (((symbol-function 'niwa-collaboration-item-opener)
                       (lambda (&optional buffer)
                         (setq prepared (list buffer))
                         (lambda (buffer id) (setq opened (list buffer id)))))
                      ((symbol-function 'niwa-shared-editing-create)
                       (lambda (seen kind title actor callback &rest keys)
                         (should (eq workspace seen))
                         (should (equal '("document" "Notes" "Host")
                                        (list kind title actor)))
                         (should-not (plist-get keys :session))
                         (funcall callback '(:id "ab12"))))
                      ((symbol-function 'niwa-artifact-store-conversation)
                       (lambda (seen id)
                         (should (eq workspace seen))
                         (should (equal "ab12" id))
                         conversation)))
              (niwa-test--with-captured-messages messages
                (niwa-artifacts-new-shared-item "document" "Notes"))
              (should (equal '(nil) prepared))
              (should (equal (list conversation "ab12") opened))
              (should (string-match-p "shared://ab12" messages))))
        (kill-buffer conversation))))

  :doc "refuses a read-only session, an invalid title or declined sharing before queuing"
  (niwa-artifacts-list-test--with-store
    (with-current-buffer data
      (setq-local niwa-session--read-only-mode t))
    (with-current-buffer (niwa-artifacts-list-open
                          (niwa-artifacts-list-test--context session view data))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Prompted a read-only session")))
                ((symbol-function 'niwa-shared-editing-create)
                 (lambda (&rest _) (ert-fail "Queued a refused creation"))))
        (should-error (call-interactively #'niwa-artifacts-new-shared-item)
                      :type 'user-error)
        (with-current-buffer data
          (setq-local niwa-session--read-only-mode nil))
        (cl-letf (((symbol-function 'niwa-collaboration-item-opener)
                   (lambda (&rest _) (ert-fail "Asked to share an invalid item"))))
          (should-error (niwa-artifacts-new-shared-item "document" "  ")
                        :type 'user-error)
          (should-error (niwa-artifacts-new-shared-item "slides" "Deck")
                        :type 'user-error))
        (cl-letf (((symbol-function 'niwa-collaboration-item-opener)
                   (lambda (&rest _) (user-error "Collaboration not started"))))
          (should-error (niwa-artifacts-new-shared-item "document" "Notes")
                        :type 'user-error)))))

  :doc "reports a failed creation, and keeps an item that could not be opened"
  (niwa-artifacts-list-test--with-store
    (with-current-buffer (niwa-artifacts-list-open
                          (niwa-artifacts-list-test--context session view data))
      (let (reply messages)
        (cl-letf (((symbol-function 'niwa-collaboration-item-opener)
                   (lambda (&rest _)
                     (lambda (&rest _) (error "No browser"))))
                  ((symbol-function 'niwa-shared-editing-create)
                   (lambda (_workspace _kind _title _actor callback &rest _)
                     (funcall callback reply)))
                  ((symbol-function 'niwa-artifact-store-delete)
                   (lambda (&rest _) (ert-fail "Deleted a created item"))))
          (setq reply '(:error "Node 22.4 or newer is required"))
          (niwa-test--with-captured-messages messages
            (niwa-artifacts-new-shared-item "document" "Notes"))
          (should (string-match-p "Node 22.4.*was not created" messages))
          (setq reply '(:id "ab12" :attach-error "Disk full"))
          (niwa-test--with-captured-messages messages
            (niwa-artifacts-new-shared-item "document" "Notes"))
          (should (string-match-p "Disk full.*shared://ab12.*not attached" messages))
          (should (string-match-p "No browser.*shared://ab12.*could not open" messages))))))

  :doc "attributes a deferred failure from another buffer without changing reply data"
  (let* ((workspace (niwa-workspace--create :root "/tmp/artifact-owner"))
         (reply '(:error "Failed at 50%"))
         messages)
    (with-temp-buffer
      (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/other"))
      (niwa-test--with-captured-messages messages
        (niwa-artifacts-list--created reply "document" "Notes" workspace nil nil)))
    (should (equal messages
                   "niwa [/tmp/artifact-owner]: Failed at 50%; document \"Notes\" was not created\n"))
    (should (equal reply '(:error "Failed at 50%")))
    (niwa-test--with-captured-messages messages
      (niwa-artifacts-list--created
       (list :error "niwa [/tmp/other]: Failed") "document" "Notes" workspace nil nil))
    (should (equal messages
                   "niwa [/tmp/artifact-owner]: Failed; document \"Notes\" was not created\n")))

  :doc "leaves another workspace's cockpit and its selection alone"
  (niwa-artifacts-list-test--with-store
    (let* ((other-root (file-name-as-directory
                        (make-temp-file "niwa-artifacts-other-" t)))
           (other (niwa-workspace--create :type 'project :id "o"
                                             :root other-root :name "other"))
           finish refreshed)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-collaboration-item-opener)
                       (lambda (&rest _) #'ignore))
                      ((symbol-function 'niwa-shared-editing-create)
                       (lambda (_workspace _kind _title _actor callback &rest _)
                         (setq finish callback))))
              (with-current-buffer (niwa-artifacts-list-open
                                    (niwa-artifacts-list-test--context session view data))
                (niwa-artifacts-new-shared-item "document" "Notes")))
            ;; The user switched to another project's cockpit meanwhile.
            (niwa-artifacts-list-open (niwa-cockpit-workspace-context other))
            (cl-letf (((symbol-function 'niwa-cockpit-surface-refresh)
                       (lambda (&rest _) (setq refreshed t))))
              (niwa-test--with-captured-messages nil
                (funcall finish '(:id "ab12"))))
            (should-not refreshed))
        (delete-directory other-root t)))))

(provide 'test-niwa-artifacts-list)
;;; test-niwa-artifacts-list.el ends here
