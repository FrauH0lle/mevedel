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
(require 'mevedel-shared-editing)
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
          (should (equal "mockup" (aref row 0)))
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

  :doc "shows titles for files and shared items while selecting by stable id"
  (mevedel-artifacts-list-test--with-store
    (let ((board-id (make-string 64 ?a))
          (document-id (make-string 64 ?b))
          (file-id (make-string 64 ?c)))
      (mevedel-artifact-store-create-meta
       workspace board-id "state.json" 'whiteboard "Project plan")
      (mevedel-artifact-store-create-meta
       workspace document-id "state.json" 'document "Project plan")
      (mevedel-artifact-store-create-meta
       workspace file-id "index.html" 'html "Preview")
      (mevedel-artifacts-list-test--artifact
       session store file-id "index.html" "<h1>Preview</h1>")
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (dolist (id (list board-id document-id file-id))
          (should (equal (if (equal id file-id) "Preview" "Project plan")
                         (aref (cadr (assoc id tabulated-list-entries)) 0)))
          (should-not (string-match-p id (buffer-string)))
          (mevedel-cockpit-goto-id id)
          (should (equal id (tabulated-list-get-id)))
          (should (equal id (plist-get (mevedel-cockpit-surface-selected) :id)))
          (mevedel-cockpit-surface-refresh id)
          (should (equal id (tabulated-list-get-id))))
        (should (string-match-p "Project plan" (buffer-string)))
        (should (string-match-p "Preview" (buffer-string)))
        (mevedel-cockpit-goto-id board-id)
        (should (equal board-id
                       (plist-get (mevedel-cockpit-surface-selected) :item)))
        (should (string-match-p
                 (concat "Id +" board-id)
                 (mevedel-report-test-text
                  (mevedel-artifacts-list--details
                   (mevedel-cockpit-surface-selected) nil)))))))

  :doc "multiline titles stay in one cockpit row without changing stored titles"
  (mevedel-artifacts-list-test--with-store
    (let ((title "Project\nplan\tphase\rone"))
      (mevedel-artifact-store-create-meta
       workspace "board" "state.json" 'whiteboard title)
      (with-current-buffer (mevedel-artifacts-list-open
                            (mevedel-artifacts-list-test--context session view data))
        (ert-info ((buffer-substring-no-properties (point-min) (point-max)))
          (should (= 1 (count-lines (point-min) (point-max)))))
        (should (equal "Project plan phase one"
                       (aref (cadr (assoc "board" tabulated-list-entries)) 0)))
        (mevedel-cockpit-goto-id "board")
        (let ((item (mevedel-cockpit-surface-selected)))
          (should (equal "board" (plist-get item :id)))
          (should (equal title (plist-get item :title)))))))

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

(mevedel-deftest mevedel-artifacts-list--entry
  (:doc "sorts Ver and Size by their numbers, not their text")
  (let ((rows (mapcar (lambda (item) (mevedel-artifacts-list--entry item nil))
                      (list (list :row "small" :name "small" :versions 10
                                  :size 980 :modified 0)
                            (list :row "large" :name "large" :versions 9
                                  :size 1572864 :modified 0)))))
    (pcase-dolist (`(,column . ,first) '((3 . "large") (5 . "small")))
      (should (equal first (car (car (sort (copy-sequence rows)
                                           (mevedel-cockpit-sort-by-number
                                            column)))))))))

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

(defmacro mevedel-artifacts-list-test--with-editing (&rest body)
  "Run BODY with private shared-editing runtimes and item leases."
  (declare (indent 0) (debug t))
  `(let ((mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
         (mevedel-artifact-lease--held (make-hash-table :test #'equal))
         (mevedel-session-durability--client-id (make-string 64 ?a)))
     (unwind-protect (progn ,@body)
       (mevedel-shared-editing-stop)
       (mevedel-artifact-lease-release-all))))

(mevedel-deftest mevedel-artifacts-new-shared-item ()
  ,test
  (test)
  :doc "creates, attaches, lists and opens a whiteboard from the session cockpit"
  (mevedel-artifacts-list-test--with-store
    (mevedel-artifacts-list-test--with-editing
      (let (prepared opened written)
        (with-current-buffer (mevedel-artifacts-list-open
                              (mevedel-artifacts-list-test--context session view data))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Whiteboard"))
                    ((symbol-function 'read-string) (lambda (&rest _) " Plan "))
                    ((symbol-function 'mevedel-session-persistence-write-sidecar-now)
                     (lambda (&rest args) (setq written args)))
                    ((symbol-function 'mevedel-collaboration-item-opener)
                     (lambda (&optional buffer)
                       (setq prepared (list buffer))
                       (lambda (buffer id) (setq opened (list buffer id))))))
            (mevedel-test--with-captured-messages nil
              (call-interactively #'mevedel-artifacts-new-shared-item)
              ;; Consent is settled before the item is queued.
              (should (equal (list data) prepared))
              (mevedel-test--await 10 "the item opens" opened)))
          (let ((id (cadr opened)))
            (should (equal (list data id) opened))
            (should (equal '(:kind whiteboard :title "Plan")
                           (cl-subseq (mevedel-artifact-store-meta workspace id) 0 4)))
            (should (equal (list id) (mevedel-session-attached-artifacts session)))
            (should (equal (list session data) written))
            (should (assoc id tabulated-list-entries))
            (should (equal id (plist-get (mevedel-cockpit-surface-selected) :id))))))))

  :doc "without a session, opens the new document in its dedicated conversation"
  (mevedel-artifacts-list-test--with-store
    (let ((conversation (generate-new-buffer " *artifact-conversation*"))
          prepared opened messages)
      (unwind-protect
          (with-current-buffer (mevedel-artifacts-list-open
                                (mevedel-cockpit-workspace-context workspace))
            (cl-letf (((symbol-function 'mevedel-collaboration-item-opener)
                       (lambda (&optional buffer)
                         (setq prepared (list buffer))
                         (lambda (buffer id) (setq opened (list buffer id)))))
                      ((symbol-function 'mevedel-shared-editing-create)
                       (lambda (seen kind title actor callback &rest keys)
                         (should (eq workspace seen))
                         (should (equal '("document" "Notes" "Host")
                                        (list kind title actor)))
                         (should-not (plist-get keys :session))
                         (funcall callback '(:id "ab12"))))
                      ((symbol-function 'mevedel-artifact-store-conversation)
                       (lambda (seen id)
                         (should (eq workspace seen))
                         (should (equal "ab12" id))
                         conversation)))
              (mevedel-test--with-captured-messages messages
                (mevedel-artifacts-new-shared-item "document" "Notes"))
              (should (equal '(nil) prepared))
              (should (equal (list conversation "ab12") opened))
              (should (string-match-p "shared://ab12" messages))))
        (kill-buffer conversation))))

  :doc "refuses a read-only session, an invalid title or declined sharing before queuing"
  (mevedel-artifacts-list-test--with-store
    (with-current-buffer data
      (setq-local mevedel-session--read-only-mode t))
    (with-current-buffer (mevedel-artifacts-list-open
                          (mevedel-artifacts-list-test--context session view data))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Prompted a read-only session")))
                ((symbol-function 'mevedel-shared-editing-create)
                 (lambda (&rest _) (ert-fail "Queued a refused creation"))))
        (should-error (call-interactively #'mevedel-artifacts-new-shared-item)
                      :type 'user-error)
        (with-current-buffer data
          (setq-local mevedel-session--read-only-mode nil))
        (cl-letf (((symbol-function 'mevedel-collaboration-item-opener)
                   (lambda (&rest _) (ert-fail "Asked to share an invalid item"))))
          (should-error (mevedel-artifacts-new-shared-item "document" "  ")
                        :type 'user-error)
          (should-error (mevedel-artifacts-new-shared-item "slides" "Deck")
                        :type 'user-error))
        (cl-letf (((symbol-function 'mevedel-collaboration-item-opener)
                   (lambda (&rest _) (user-error "Collaboration not started"))))
          (should-error (mevedel-artifacts-new-shared-item "document" "Notes")
                        :type 'user-error)))))

  :doc "reports a failed creation, and keeps an item that could not be opened"
  (mevedel-artifacts-list-test--with-store
    (with-current-buffer (mevedel-artifacts-list-open
                          (mevedel-artifacts-list-test--context session view data))
      (let (reply messages)
        (cl-letf (((symbol-function 'mevedel-collaboration-item-opener)
                   (lambda (&rest _)
                     (lambda (&rest _) (error "No browser"))))
                  ((symbol-function 'mevedel-shared-editing-create)
                   (lambda (_workspace _kind _title _actor callback &rest _)
                     (funcall callback reply)))
                  ((symbol-function 'mevedel-artifact-store-delete)
                   (lambda (&rest _) (ert-fail "Deleted a created item"))))
          (setq reply '(:error "Node 22.4 or newer is required"))
          (mevedel-test--with-captured-messages messages
            (mevedel-artifacts-new-shared-item "document" "Notes"))
          (should (string-match-p "not created: Node 22.4" messages))
          (setq reply '(:id "ab12" :attach-error "Disk full"))
          (mevedel-test--with-captured-messages messages
            (mevedel-artifacts-new-shared-item "document" "Notes"))
          (should (string-match-p "shared://ab12.*not attached.*Disk full" messages))
          (should (string-match-p "shared://ab12.*could not open.*No browser" messages))))))

  :doc "leaves another workspace's cockpit and its selection alone"
  (mevedel-artifacts-list-test--with-store
    (let* ((other-root (file-name-as-directory
                        (make-temp-file "mevedel-artifacts-other-" t)))
           (other (mevedel-workspace--create :type 'project :id "o"
                                             :root other-root :name "other"))
           finish refreshed)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-collaboration-item-opener)
                       (lambda (&rest _) #'ignore))
                      ((symbol-function 'mevedel-shared-editing-create)
                       (lambda (_workspace _kind _title _actor callback &rest _)
                         (setq finish callback))))
              (with-current-buffer (mevedel-artifacts-list-open
                                    (mevedel-artifacts-list-test--context session view data))
                (mevedel-artifacts-new-shared-item "document" "Notes")))
            ;; The user switched to another project's cockpit meanwhile.
            (mevedel-artifacts-list-open (mevedel-cockpit-workspace-context other))
            (cl-letf (((symbol-function 'mevedel-cockpit-surface-refresh)
                       (lambda (&rest _) (setq refreshed t))))
              (mevedel-test--with-captured-messages nil
                (funcall finish '(:id "ab12"))))
            (should-not refreshed))
        (delete-directory other-root t)))))

(provide 'test-mevedel-artifacts-list)
;;; test-mevedel-artifacts-list.el ends here
