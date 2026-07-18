;;; test-mevedel-view-agent.el --- Agent view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests agent transcript inspection and live status presentation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-stream)
(require 'mevedel-transcript)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-exec)
(require 'mevedel-workspace)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-permission-queue)
(require 'mevedel-permission-prompt)
(require 'mevedel-agents)
(require 'gptel)
(require 'mevedel-agent-runtime)
(require 'mevedel-compact)
(require 'mevedel-view-zone)

(mevedel-deftest mevedel-view-agent-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-view-open-agent-transcript
                    mevedel-view--agent-status-collect
                    mevedel-view-refresh-agent-rendering))
    (should (equal "mevedel-view-agent"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))

(mevedel-deftest mevedel-view-agent-initialize
  (:doc "initializes ordinary and transcript inspection views")
  ,test
  (test)

  :doc "transcript initialization owns its state, keys, header, and data lifecycle"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (use-local-map mevedel-view-mode-map)
          (let ((info '(:agent-id "explorer--abc" :status running))
                (parent (current-buffer)))
            (mevedel-view-agent-initialize
             (list :agent-transcript-p t
                   :agent-id "explorer--abc"
                   :transcript-info info
                   :parent-view parent
                   :preserve-data-view-buffer t)
             data-buffer)
            (should mevedel-view--agent-transcript-p)
            (should (equal "explorer--abc" mevedel-view--agent-id))
            (should (equal info mevedel-view--agent-transcript-info))
            (should (eq parent mevedel-view--agent-transcript-parent-view))
            (should (hash-table-p mevedel-view--agent-refresh-timers))
            (should-not (eq (current-local-map) mevedel-view-mode-map))
            (should (eq #'mevedel-view-close-agent-transcript
                        (lookup-key (current-local-map) (kbd "q"))))
            (should (eq #'mevedel-view--transcript-gptel-send-blocked
                        (lookup-key (current-local-map) [remap gptel-send])))
            (should (equal
                     '(:eval (mevedel-view--agent-transcript-header-line))
                     header-line-format))
            (with-current-buffer data-buffer
              (should (memq #'mevedel-view--on-agent-transcript-data-killed
                            kill-buffer-hook)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "ordinary initialization leaves generic view chrome untouched"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((map (make-sparse-keymap)))
            (use-local-map map)
            (setq header-line-format 'keep)
            (mevedel-view-agent-initialize nil data-buffer)
            (should-not mevedel-view--agent-transcript-p)
            (should (eq map (current-local-map)))
            (should (eq 'keep header-line-format))
            (with-current-buffer data-buffer
              (should-not
               (memq #'mevedel-view--on-agent-transcript-data-killed
                     kill-buffer-hook)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-view--agent-transcript-current-info
  (:doc "keeps finalized elapsed time fixed while retaining live data")
  ,test
  (test)
  (let* ((data-buffer (generate-new-buffer " *test-agent-header-data*"))
         (inv (mevedel-agent-invocation--create
               :agent-id "explorer--header"
               :buffer data-buffer
               :started-at (seconds-to-time 10)
               :transcript-status 'completed
               :call-count 2)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--data-buffer data-buffer)
          (setq-local mevedel-view--agent-transcript-info
                      '(:live-buffer nil :status completed :elapsed 3.0))
          (with-current-buffer data-buffer
            (setq-local mevedel--agent-invocation inv))
          (cl-letf (((symbol-function 'current-time)
                     (lambda () (seconds-to-time 100))))
            (should (= 3.0
                       (plist-get
                        (mevedel-view--agent-transcript-current-info)
                        :elapsed)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-view-agent-handle-view-kill
  (:doc "handles transcript-only view cleanup")
  ,test
  (test)

  :doc "terminal transcript cleanup clears both parent and data references"
  (let ((parent (generate-new-buffer " *test-agent-parent*"))
        (data-buffer (generate-new-buffer " *test-agent-data*"))
        (view-buffer (generate-new-buffer " *test-agent-view*")))
    (unwind-protect
        (progn
          (with-current-buffer parent
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window)))
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (with-current-buffer view-buffer
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel-view--agent-transcript-parent-view parent)
            (setq-local mevedel-view--agent-transcript-info
                        '(:status completed))
            (setq-local mevedel--data-buffer data-buffer)
            (should (mevedel-view-agent-handle-view-kill)))
          (with-current-buffer parent
            (should-not mevedel-view--agent-transcript-window))
          (should-not (buffer-live-p data-buffer)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "ordinary views are not handled"
  (with-temp-buffer
    (should-not (mevedel-view-agent-handle-view-kill)))

  :doc "closing an inspection view preserves retained conversation data"
  (let* ((session
          (mevedel-session-create
           "main"
           (mevedel-workspace--create
            :type 'project :id "retained-view"
            :root temporary-file-directory :name "retained-view")))
         (data-buffer (generate-new-buffer " *test-retained-agent-data*"))
         (view-buffer (generate-new-buffer " *test-retained-agent-view*"))
         (record (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer data-buffer)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (with-current-buffer view-buffer
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel-view--agent-transcript-info
                        (list :status 'completed :session session))
            (setq-local mevedel--data-buffer data-buffer)
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p view-buffer))
          (should (buffer-live-p data-buffer)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-view-agent-cleanup-parent
  (:doc "removes transcript views owned by a parent")
  ,test
  (test)

  :doc "parent cleanup also kills retained data at session teardown"
  (let* ((parent (generate-new-buffer " *test-agent-parent*"))
         (data-buffer (generate-new-buffer " *test-agent-data*"))
         (view-buffer (generate-new-buffer " *test-agent-view*"))
         (session
          (mevedel-session-create
           "main"
           (mevedel-workspace--create
            :type 'project :id "retained-cleanup"
            :root temporary-file-directory :name "retained-cleanup")))
         (record (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer data-buffer)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (progn
          (with-current-buffer view-buffer
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel-view--agent-transcript-parent-view parent)
            (setq-local mevedel-view--agent-transcript-info
                        (list :status 'completed :session session))
            (setq-local mevedel--data-buffer data-buffer))
          (mevedel-view-agent-cleanup-parent parent)
          (should-not (buffer-live-p view-buffer))
          (should-not (buffer-live-p data-buffer)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(mevedel-deftest mevedel-view--agent-live-transcript-dispatch
  (:doc "rolls back synchronous observer updates that fail")
  ,test
  (test)
  (let ((agent-data (generate-new-buffer " *test-agent-dispatch-data*"))
        (agent-view (generate-new-buffer " *test-agent-dispatch-view*"))
        warned)
    (unwind-protect
        (progn
          (with-current-buffer agent-view
            (insert "last good projection")
            (setq-local mevedel-view--agent-transcript-p t)
            (setq-local mevedel--data-buffer agent-data))
          (with-current-buffer agent-data
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (&rest _) (setq warned t))))
              (mevedel-view--agent-live-transcript-dispatch
               (lambda ()
                 (with-current-buffer mevedel--view-buffer
                   (delete-region (point-min) (point-max)))
                 (error "Observer update failed")))))
          (should warned)
          (with-current-buffer agent-view
            (should (equal "last good projection" (buffer-string)))))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data)))))

(mevedel-deftest mevedel-view-open-agent-transcript-at-point
  (:doc "opens transcript at attribution targets")
  ,test
  (test)

  :doc "insert-attribution stamps clickable id with transcript property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr"
                       :root temporary-file-directory
                       :name "attr"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-session"))))
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id))
               (pos (string-match-p "explorer--abc123" s)))
          (should pos)
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))))))

  :doc "short display ids resolve to canonical transcript entries"
  (mevedel-view-test--with-buffers
    (let* ((canonical "explorer--abcdef0123456789abcdef0123456789")
           (short "explorer--abcdef01")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-short"
                       :root temporary-file-directory
                       :name "attr-short"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-short-session"))))
      (unwind-protect
          (progn
            (make-directory (file-name-concat save-path "agents") t)
            (write-region "" nil
                          (file-name-concat
                           save-path
                           "agents/explorer--abcdef01.chat.org")
                          nil 'silent)
            (setf (mevedel-session-save-path session) save-path)
            (setf (mevedel-session-agent-transcripts session)
                  (list (cons canonical
                              '(:path "agents/explorer--abcdef01.chat.org"
                                :status completed))))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (let ((entry (mevedel-view--lookup-transcript-entry short))
                    (info (mevedel-view--resolve-agent-transcript short)))
                (should (equal "agents/explorer--abcdef01.chat.org"
                               (plist-get entry :path)))
                (should (equal canonical (plist-get info :agent-id))))))
        (when (file-directory-p save-path)
          (delete-directory save-path t)))))

  :doc "running transcript attribution is clickable and reports unavailable live buffer"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-running"
                       :root temporary-file-directory
                       :name "attr-running"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-running-session")))
           message-text)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 7))
               (pos (string-match-p "explorer--abc123" s)))
          (should pos)
          (should (get-text-property pos 'keymap s))
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (string-match-p "Live buffer unavailable" message-text))
          (should (string-match-p "7 tool calls" message-text)))))))

  :doc "terminal live status overrides stale running sidecar for attribution"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--race123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-race"
                       :root temporary-file-directory
                       :name "attr-race"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-race-session")))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'completed))
           opened
           message-text)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--race123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv))
                  ((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-text (apply #'format fmt args)))))
          (mevedel-view--open-agent-transcript-or-message agent-id)
          (should (equal agent-id opened))
          (should-not message-text)))))

  :doc "read-only attach reports unavailable live buffer for running transcripts"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (root (file-name-as-directory
                  (make-temp-file "mevedel-attr-readonly" t)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-readonly"
                       :root root
                       :name "attr-readonly"))
           (session (mevedel-session-create "main" workspace))
           opened
           message-text)
      (setf (mevedel-session-save-path session) root)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-session--read-only-mode t))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 4))
               (pos (string-match-p "explorer--abc123" s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (&rest _) (setq opened t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
          (mevedel-view-open-agent-transcript-at-point))
        (should-not opened)
        (should (string-match-p "Live buffer unavailable" message-text))))))

  :doc "survives display-region keymap overlay by using agent-id property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (opened nil)
           (s (copy-sequence "from explorer--abc123")))
      (add-text-properties (length "from ") (length s)
                           `(mevedel-view-agent-id ,agent-id
                             keymap ,(make-sparse-keymap)
                             help-echo "Open transcript")
                           s)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert s)
                (add-text-properties
                 start (point)
                 `(read-only t keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char (point-min))
        (search-forward "explorer--abc123")
        (goto-char (match-beginning 0))
        (should (equal agent-id
                       (get-text-property (point) 'mevedel-view-agent-id)))
        (should (eq (get-text-property (point) 'keymap)
                    mevedel-view--display-map))
        (should (eq (lookup-key mevedel-view--display-map [mouse-2])
                    #'mevedel-view-activate-at-point))
        (cl-letf (((symbol-function
                    'mevedel-view--open-agent-transcript-or-message)
                   (lambda (id &rest _) (setq opened id))))
          (mevedel-view-activate-at-point)
          (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--agent-transcript-setup
  (:doc "sets up transcript inspection views without chat input zones")
  ,test
  (test)

  :doc "transcript view has co-located hidden markers and no prompt"
  (let ((data-buf (generate-new-buffer " *test-agent-data*"))
        (view-buf (generate-new-buffer " *test-agent-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           view-buf data-buf
           '(:agent-transcript-p t
             :agent-id "explorer--abc123"
             :transcript-info (:agent-id "explorer--abc123"
                               :status completed
                               :calls 2
                               :elapsed 1.5
                               :session-label "main")))
          (with-current-buffer view-buf
            (should mevedel-view--agent-transcript-p)
            (should (equal "explorer--abc123" mevedel-view--agent-id))
            (should (eq (lookup-key (current-local-map) (kbd "q"))
                        #'mevedel-view-close-agent-transcript))
            (should-not (eq (lookup-key mevedel-view-mode-map (kbd "q"))
                            #'mevedel-view-close-agent-transcript))
            (should (= (point-min) (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--status-marker)
                       (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--interaction-marker)
                       (marker-position mevedel-view--input-marker)))
            (should-not (string-match-p "> " (buffer-string)))
            (should (string-match-p "Agent explorer--abc123"
                                    (mevedel-view--agent-transcript-header-line)))
            (should-error (mevedel-view-send) :type 'user-error)
            (should-error (mevedel-view-abort) :type 'user-error)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view-agent-handle-activate
  (:doc "dispatches agent handles to transcript open when available")
  ,test
  (test)

  :doc "running handle opens a live transcript when an invocation exists"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle"
                       :root temporary-file-directory
                       :name "handle"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'running))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (should (eq 'running
                    (plist-get
                     (mevedel-view--lookup-transcript-entry agent-id)
                     :status)))
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv))
                  ((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-handle-activate)
          (should opened))))))

  :doc "terminal handle opens rendered transcript path"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-terminal"
                       :root temporary-file-directory
                       :name "handle-terminal"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened))))))

  :doc "terminal status wins when a running handle races completion"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-race"
                       :root temporary-file-directory
                       :name "handle-race"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t
                              'mevedel-view-agent-status 'running)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--agent-transcript-window
  (:doc "manages the singleton transcript side window")
  ,test
  (test)

  :doc "new transcript reuses the prior singleton and kills the previous view"
  (let ((parent (generate-new-buffer " *test-parent-view*"))
        (old-data (generate-new-buffer " *test-old-agent-data*"))
        (old-view (generate-new-buffer " *test-old-agent-view*"))
        (new-data (generate-new-buffer " *test-new-agent-data*"))
        (new-view (generate-new-buffer " *test-new-agent-view*"))
        reused-window)
    (unwind-protect
        (progn
          (with-current-buffer old-data
            (org-mode))
          (mevedel-view--setup
           old-view old-data
           (list :agent-transcript-p t
                 :agent-id "explorer--old"
                 :parent-view parent))
          (with-current-buffer new-data
            (org-mode))
          (mevedel-view--setup
           new-view new-data
           (list :agent-transcript-p t
                 :agent-id "explorer--new"
                 :parent-view parent))
          (with-current-buffer new-view
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\ntranscript tail")))
          (with-current-buffer parent
            (mevedel-view-mode)
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window))
            (set-window-buffer (selected-window) old-view)
            (setq reused-window (selected-window))
            (mevedel-view--display-agent-transcript-view new-view)
            (should (eq reused-window mevedel-view--agent-transcript-window))
            (should (eq (window-buffer reused-window) new-view))
            (should (= (window-point reused-window)
                       (with-current-buffer new-view (point-max))))
            (should-not (buffer-live-p old-view))
            (should-not (buffer-live-p old-data))
            (should (window-live-p mevedel-view--agent-transcript-window)))
          (kill-buffer new-view)
          (with-current-buffer parent
            (should-not mevedel-view--agent-transcript-window)))
      (when (buffer-live-p new-view) (kill-buffer new-view))
      (when (buffer-live-p new-data) (kill-buffer new-data))
      (when (buffer-live-p old-view) (kill-buffer old-view))
      (when (buffer-live-p old-data) (kill-buffer old-data))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "killing the parent view kills open terminal transcript buffers"
  (let ((parent-data (generate-new-buffer " *test-parent-data-close*"))
        (parent-view (generate-new-buffer " *test-parent-view-close*"))
        (agent-data (generate-new-buffer " *test-agent-data-close*"))
        (agent-view (generate-new-buffer " *test-agent-view-close*")))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           agent-view agent-data
           (list :agent-transcript-p t
                 :agent-id "explorer--close"
                 :parent-view parent-view
                 :transcript-info
                 (list :agent-id "explorer--close"
                       :status 'completed)))
          (with-current-buffer parent-view
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window)))
          (kill-buffer parent-view)
          (should-not (buffer-live-p parent-view))
          (should-not (buffer-live-p parent-data))
          (should-not (buffer-live-p agent-view))
          (should-not (buffer-live-p agent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "killing a live transcript data buffer kills only its inspection view"
  (let ((parent-data (generate-new-buffer " *test-parent-data-live-close*"))
        (parent-view (generate-new-buffer " *test-parent-view-live-close*"))
        (agent-data (generate-new-buffer " *test-agent-data-live-close*"))
        (agent-view (generate-new-buffer " *test-agent-view-live-close*")))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           agent-view agent-data
           (list :agent-transcript-p t
                 :agent-id "explorer--live-close"
                 :parent-view parent-view
                 :preserve-data-view-buffer t
                 :transcript-info
                 (list :agent-id "explorer--live-close"
                       :status 'running
                       :live-buffer t)))
          (kill-buffer agent-data)
          (should-not (buffer-live-p agent-data))
          (should-not (buffer-live-p agent-view))
          (should (buffer-live-p parent-view))
          (should (buffer-live-p parent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data)
        (with-current-buffer agent-data
          (setq kill-buffer-hook nil))
        (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "pop-to-buffer fallback stores the selected window, not its buffer"
  (let ((parent (generate-new-buffer " *test-parent-fallback-view*"))
        (data (generate-new-buffer " *test-agent-fallback-data*"))
        (view (generate-new-buffer " *test-agent-fallback-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (org-mode))
          (mevedel-view--setup
           view data
           (list :agent-transcript-p t
                 :agent-id "explorer--fallback"
                 :parent-view parent))
          (with-current-buffer parent
            (mevedel-view-mode)
	            (cl-letf (((symbol-function 'display-buffer)
	                       (lambda (&rest _) (error "Display failed")))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buf &rest _)
                         (set-window-buffer (selected-window) buf)
                         buf)))
              (mevedel-view--display-agent-transcript-view view))
            (should (window-live-p mevedel-view--agent-transcript-window))
            (should (eq (window-buffer mevedel-view--agent-transcript-window)
                        view))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(mevedel-deftest mevedel-view-open-agent-transcript
  (:doc "opens terminal transcripts as rendered inspection views")
  ,test
  (test)

  :doc "terminal transcript opens rendered view and q kills view plus data buffer"
  (let* ((agent-id "explorer--abc123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-view" t)))
         (agents-dir (file-name-concat root "agents"))
         (rel-path "agents/explorer--abc123.chat.org")
         (abs-path (file-name-concat root rel-path))
         (data-buf (generate-new-buffer " *test-parent-data*"))
         (view-buf (generate-new-buffer " *test-parent-view*"))
         agent-view
         agent-data
         restored-bounds)
    (make-directory agents-dir t)
    (with-temp-file abs-path
      (insert ":PROPERTIES:\n"
              ":GPTEL_BOUNDS: ((response (42 55)))\n"
              ":END:\n"
              "*** Agent prompt\n"))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-view"
                           :root root :name "transcript-view"))
               (session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-save-path session) root)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            (list :path rel-path
                                  :status 'completed
                                  :calls 3
                                  :elapsed 2.5))))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'gptel--restore-props)
                       (lambda (bounds)
                         (setq restored-bounds bounds))))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view (get-buffer "*mevedel-agent:explorer--abc123*"))
          (should (buffer-live-p agent-view))
          (should (equal '((response (42 55))) restored-bounds))
          (with-current-buffer agent-view
            (setq agent-data mevedel--data-buffer)
            (should mevedel-view--agent-transcript-p)
            (should (string-match-p "3 calls"
                                    (mevedel-view--agent-transcript-header-line)))
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p agent-view))
          (should-not (buffer-live-p agent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "running transcript opens live buffer and closing view leaves agent alive"
  (let* ((agent-id "explorer--live123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-live" t)))
         (parent-data (generate-new-buffer " *test-parent-data-live*"))
         (parent-view (generate-new-buffer " *test-parent-view-live*"))
         (agent-data (generate-new-buffer " *test-agent-data-live*"))
         agent-view
         parent-before)
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-live"
                           :root root :name "transcript-live"))
               (session (mevedel-session-create "main" workspace))
               (inv (mevedel-agent-invocation--create
                     :agent-id agent-id
                     :buffer agent-data
                     :transcript-status 'running
                     :call-count 4)))
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory root)
            (insert "*** Live agent prompt\n"))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (setq parent-before
                  (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer parent-view
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv)))
              (mevedel-view-open-agent-transcript agent-id)))
          (with-current-buffer agent-data
            (should (eq mevedel--view-buffer parent-view)))
          (setq agent-view (get-buffer "*mevedel-agent:explorer--live123*"))
          (should (buffer-live-p agent-view))
          (with-current-buffer agent-view
            (should mevedel-view--agent-transcript-p)
            (should (plist-get mevedel-view--agent-transcript-info
                               :live-buffer))
            (should (string-search
                     "Live agent prompt"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
            (should (string-match-p "4 calls"
                                    (mevedel-view--agent-transcript-header-line)))
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p agent-view))
          (with-current-buffer parent-view
            (should (equal parent-before
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
          (should (buffer-live-p agent-data))
          (with-current-buffer agent-data
            (should (eq mevedel--view-buffer parent-view))))
	      (when (buffer-live-p agent-view) (kill-buffer agent-view))
	      (when (buffer-live-p agent-data) (kill-buffer agent-data))
	      (when (buffer-live-p parent-view) (kill-buffer parent-view))
	      (when (buffer-live-p parent-data) (kill-buffer parent-data))
	      (when (file-directory-p root) (delete-directory root t))))

  :doc "open running transcript follows streamed text without redirecting parent view"
  (let* ((agent-id "explorer--live-stream")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-live-stream" t)))
         (parent-data (generate-new-buffer " *test-parent-data-live-stream*"))
         (parent-view (generate-new-buffer " *test-parent-view-live-stream*"))
         (agent-data (generate-new-buffer " *test-agent-data-live-stream*"))
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :buffer agent-data
               :transcript-status 'running
               :call-count 0))
         agent-view
         parent-before)
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-live-stream"
                           :root root :name "transcript-live-stream"))
               (session (mevedel-session-create "main" workspace)))
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local mevedel--session session)
            (setq-local default-directory root)
            (insert "Observe this agent.\n")
            (insert "\n")
            (add-hook 'gptel-post-stream-hook
                      #'mevedel-view-agent-live-transcript-stream nil t))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (setq parent-before (buffer-string))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv)))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view
                (get-buffer "*mevedel-agent:explorer--live-str*"))
          (should (buffer-live-p agent-view))
          (with-current-buffer agent-view
            (should (string-search "Observe this agent" (buffer-string)))
            (should-not (string-search "Partial response" (buffer-string)))
            (should buffer-read-only)
            (goto-char (point-max))
            (should-error (insert "not allowed"))
            (should (markerp mevedel-view--in-flight-turn-start))
            (should (markerp mevedel-view--data-turn-start)))
          (with-current-buffer agent-data
            (should (memq agent-view
                          (mevedel-view--agent-transcript-views-for-data
                           agent-data)))
            (goto-char (point-max))
            (let ((start (point)))
              (insert "Partial response complete\n")
              (add-text-properties
               start (point) '(gptel response front-sticky (gptel))))
            (let ((mevedel-view-stream-render-delay 0))
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (_delay repeat callback &rest args)
                           (unless repeat
                             (apply callback args))
                           'scheduled)))
                (run-hooks 'gptel-post-stream-hook))))
          (with-current-buffer agent-view
            (should (string-search "Partial response complete"
                                   (buffer-string))))
          (with-current-buffer parent-view
            (should (equal parent-before (buffer-string))))
          (with-current-buffer agent-data
            (should (eq mevedel--view-buffer parent-view))))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "running transcript view skips bounds repair for incomplete live blocks"
  (let* ((agent-id "verifier--partial-live")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-partial-live" t)))
         (parent-data (generate-new-buffer " *test-parent-data-partial-live*"))
         (parent-view (generate-new-buffer " *test-parent-view-partial-live*"))
         (agent-data (generate-new-buffer " *test-agent-data-partial-live*"))
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :buffer agent-data
               :transcript-status 'running
               :call-count 0))
         agent-view
         restored
         normalized)
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-partial-live"
                           :root root :name "transcript-partial-live"))
               (session (mevedel-session-create "main" workspace)))
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory root)
            (insert "*** Live verifier prompt\n\n#+begin_reasoning\npartial"))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv))
                      ((symbol-function 'mevedel-transcript-restore-properties)
                       (lambda () (setq restored t)))
                      ((symbol-function
                        'mevedel-transcript-normalize-properties)
                       (lambda () (setq normalized t))))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view
                (get-buffer
                 (format "*mevedel-agent:%s*"
                         (mevedel-view--display-label-for-agent agent-id))))
          (should (buffer-live-p agent-view))
          (should-not restored)
          (should-not normalized)
          (with-current-buffer agent-view
            (should (string-search
                     "partial"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "terminal live invocation status overrides stale running sidecar"
  (let* ((agent-id "explorer--race123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-race" t)))
         (agents-dir (file-name-concat root "agents"))
         (rel-path "agents/explorer--race123.chat.org")
         (abs-path (file-name-concat root rel-path))
         (data-buf (generate-new-buffer " *test-parent-data-race*"))
         (view-buf (generate-new-buffer " *test-parent-view-race*"))
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :transcript-status 'completed)))
    (make-directory agents-dir t)
    (with-temp-file abs-path
      (insert "*** Agent prompt\n"))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-race"
                           :root root :name "transcript-race"))
               (session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-save-path session) root)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            (list :path rel-path
                                  :status 'running))))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv)))
              (let ((info (mevedel-view--resolve-agent-transcript agent-id)))
                (should (eq 'completed (plist-get info :status)))
                (should (equal abs-path
                               (plist-get info :absolute-path)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--agent-status-collect
  (:doc "derives aggregate rows from current rendered handles and sidecar")
  ,test
  (test)

  :doc "aggregate status leaves a blank line before the input prompt"
  (let ((text (mevedel-view--agent-status-string
               (list (list :agent-id "explorer--abc"
                           :status 'completed
                           :description "done"
                           :calls 1))
               nil)))
    (should (string-suffix-p "\n\n" text)))

  :doc "aggregate status toggle is attached to the suffix button only"
  (let* ((text (mevedel-view--agent-status-string
                (list (list :agent-id "explorer--abc"
                            :status 'running
                            :description "count"
                            :calls 1))
                nil))
         (button-pos (string-match-p (regexp-quote "[+]") text)))
    (should-not (lookup-key mevedel-view-mode-map (kbd "C-c C-a")))
    (should button-pos)
    (should (eq (lookup-key (get-text-property button-pos 'keymap text)
                            (kbd "RET"))
                #'mevedel-view-activate-at-point))
    (should (get-text-property button-pos 'follow-link text))
    (should-not (get-text-property (max 0 (1- button-pos))
                                   'keymap text)))

  :doc "display navigation moves through status fragments before the composer"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-navigation"
                       :root temporary-file-directory
                       :name "status-fragment-navigation"))
           (session (mevedel-session-create "main" workspace))
           (agent-id "explorer--nav123"))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (should (eq (lookup-key mevedel-view--display-map (kbd "n"))
                    #'mevedel-view-next-display))
        (should (eq (lookup-key mevedel-view--display-map (kbd "p"))
                    #'mevedel-view-previous-display))
        (should-not (lookup-key mevedel-view-mode-map (kbd "n")))
        (should-not (lookup-key mevedel-view-mode-map (kbd "RET")))
        (goto-char (point-min))
        (mevedel-view-next-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-zone-id)))
        (let ((map (get-text-property (point) 'keymap)))
          (should (eq (lookup-key map (kbd "n"))
                      #'mevedel-view-next-display))
          (should (eq (lookup-key map (kbd "p"))
                      #'mevedel-view-previous-display))
          (should (eq (lookup-key map (kbd "RET"))
                      #'mevedel-view-activate-at-point))
          (should-not (cdr (get-char-property-and-overlay
                            (point) 'mevedel-tool-task))))
        (mevedel-view-next-display)
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-zone-id)))
        (let ((last-fragment (point)))
          (mevedel-view-next-display)
          (should (= (point) last-fragment)))
        (mevedel-view-previous-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-zone-id))))))

  :doc "display navigation chooses the next turn before later status fragments"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-nearest"
                       :root temporary-file-directory
                       :name "status-fragment-nearest"))
           (session (mevedel-session-create "main" workspace))
           first-start second-start)
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--status-marker)
          (setq first-start (point))
          (insert "First turn\n")
          (add-text-properties first-start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 mevedel-view-source (1 . 10)))
          (setq second-start (point))
          (insert "Second turn\n")
          (add-text-properties second-start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 mevedel-view-source (11 . 20)))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point)))
        (mevedel-view--render-status data-buf)
        (goto-char first-start)
        (mevedel-view-next-display)
        (should (= (point) second-start))
        (mevedel-view-next-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-zone-id))))))

  :doc "shared activation refuses the editable composer"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "draft text" 2)
      (should-error (mevedel-view-activate-at-point) :type 'user-error)
      (should (string= "draft text" (mevedel-view--input-text)))))

  :doc "agent status collapse is backed by fragment collapse state"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-collapse"
                       :root temporary-file-directory
                       :name "status-fragment-collapse"))
           (session (mevedel-session-create "main" workspace))
           (agent-id "explorer--collapse123"))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-zone-id)))
        (mevedel-view-toggle-section)
        (should (mevedel-view-zone-collapse-state
                 mevedel-view--status-agent-collapse-key))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "1 agent: 1 running" text))
          (should-not (string-match-p "Agent: explorer -- count" text)))
        (goto-char (point-min))
        (search-forward "[+]" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq (lookup-key (get-text-property (point) 'keymap)
                                (kbd "RET"))
                    #'mevedel-view-activate-at-point))
        (mevedel-view-activate-at-point)
        (should-not (mevedel-view-zone-collapse-state
                     mevedel-view--status-agent-collapse-key))
        (goto-char (point-min))
        (should (search-forward "Agent: explorer -- count"
                                mevedel-view--input-marker t)))))

  :doc "status fallback renders as a status Agent handle fragment"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((agent-id "explorer--fragment"))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id agent-id
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (should (eq (get-text-property (point) 'mevedel-view-type)
                      'agent-handle))
          (should (get-text-property (point) 'mevedel-view-agent-handle-p))
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-handle-map))
          (should-not (lookup-key (get-text-property (point) 'keymap)
                                  [mouse-1]))
          (should (eq 'status (get-text-property
                               (point) 'mevedel-view-zone-namespace)))
          (should (eq 'agents (get-text-property
                               (point) 'mevedel-view-zone-id)))))))

  :doc "status fallback leaves a blank line before request spinner"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                 (lambda ()
                   (list (list :agent-id "explorer--spinner"
                               :status 'running
                               :description "count"
                               :calls 1)))))
        (mevedel-view--render-agent-status)
        (mevedel-view--start-spinner "Working...")
        (let ((display (buffer-substring-no-properties
                        (point-min) (mevedel-view--input-start))))
          (should (string-match-p
                   "Agent: explorer -- count[^\n]*\n\n[^\n]*Working"
                   display))
          (should-not (string-match-p
                       "Agent: explorer -- count[^\n]*\n\n\n"
                       display)))
        (mevedel-view--stop-spinner))))

  :doc "status fallback preserves multiline composer text starting with >"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "explorer--draft"
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should (string-match-p "Agent: explorer -- count" display))
            (goto-char (point-min))
            (search-forward "Agent: explorer -- count"
                            mevedel-view--input-marker)
            (should (get-text-property (match-beginning 0) 'read-only)))))))

  :doc "status fallback preserves composer when all zone markers drift"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "explorer--drift"
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) (mevedel-view--input-start)))
                (input (buffer-substring-no-properties
                        (mevedel-view--input-start) (point-max))))
            (should (string-match-p "Agent: explorer -- count" display))
            (should-not (string-match-p "Agent: explorer -- count" input)))))))

  :doc "live status rows render below the task status fragment"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-below-tasks"
                       :root temporary-file-directory
                       :name "status-below-tasks"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (mevedel-view--render-status data-buf)
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "verifier--below123"
                                 :status 'running
                                 :agent-type "verifier"
                                 :description "verify changes"
                                 :calls 2)))))
          (mevedel-view--render-agent-status))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (task-pos (string-match-p "visible task" text))
               (agent-pos (string-match-p
                           "Agent: verifier -- verify changes"
                           text)))
          (should task-pos)
          (should agent-pos)
          (should (< task-pos agent-pos))))))

  :doc "status fallback handles survive repeated refreshes"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "count"
                 :transcript-status 'running
                 :call-count 1
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-refresh"
                       :root temporary-file-directory
                       :name "status-refresh"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-agent-runtime--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (mevedel-view--render-agent-status)
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count"
                          mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (should (get-text-property (point)
                                     'mevedel-view-agent-handle-p))))))

  :doc "status refresh does not delete history inserted at the boundary"
  (mevedel-view-test--with-buffers
    (let ((agent-id "explorer--boundary123"))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id agent-id
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (goto-char mevedel-view--status-marker)
        (mevedel-view--with-render-boundaries-advancing
          (let ((inhibit-read-only t))
            (insert "Assistant transcript\n")))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () nil)))
          (mevedel-view--render-agent-status))
        (should (string-match-p
                 "Assistant transcript"
                 (buffer-substring-no-properties
                  (point-min) (point-max))))
        (should-not (string-match-p
                     "Agent: explorer -- count"
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))))

  :doc "status fallback handles stay compact even with live activity"
  (mevedel-view-test--with-buffers
    (let* ((first-id "explorer--first123")
           (second-id "explorer--second456")
           (first-inv (mevedel-agent-invocation--create
                       :agent-id first-id
                       :description "count defvars"
                       :transcript-status 'running
                       :call-count 2
                       :activity '((:type waiting))
                       :buffer data-buf))
           (second-inv (mevedel-agent-invocation--create
                        :agent-id second-id
                        :description "count defcustoms"
                        :transcript-status 'running
                        :call-count 1
                        :activity '((:type waiting))
                        :buffer data-buf))
           (first-fsm (gptel-make-fsm
                       :info (list :mevedel-agent-invocation first-inv)
                       :handlers nil
                       :state 'WAIT))
           (second-fsm (gptel-make-fsm
                        :info (list :mevedel-agent-invocation second-inv)
                        :handlers nil
                        :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sibling"
                       :root temporary-file-directory
                       :name "status-sibling"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons first-id first-fsm)
                          (cons second-id second-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-agent-runtime--agent-invocation-at)
                   (lambda (fsm)
                     (cond
                      ((eq fsm first-fsm) first-inv)
                      ((eq fsm second-fsm) second-inv)))))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count defvars"
                          mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (when-let* ((bounds (mevedel-view-zone-bounds-at (point))))
            (let ((inhibit-read-only t))
              (put-text-property
               (plist-get bounds :start)
               (plist-get bounds :end)
               'mevedel-view-source
               (cons 1 1))))
          (should (equal first-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should (eq 'running
                      (get-text-property
                       (point) 'mevedel-view-agent-status)))
          (should (get-text-property (point) 'mevedel-view-source))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count defvars"
                          mevedel-view--input-marker)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p
                     "Agent: explorer -- count defvars"
                     text))
            (should-not
             (string-match-p
              "\n  … waiting"
              text)))))))

  :doc "status fallback handles truncate to one visual row"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "coordinator--wide123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-truncate"
                       :root temporary-file-directory
                       :name "status-truncate"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "coordinator"
                          :description "Run a bounded validation loop for current changes: tests, compile/checks, reviewer, verifier, fixes, then repeat until green"
                          :calls 14
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-status-line-width)
                   (lambda () 72)))
          (let* ((text (mevedel-view--agent-status-handles-string
                        (mevedel-view--agent-status-collect)))
                 (lines (split-string text "\n" t)))
            (should (= 1 (length lines)))
            (should (<= (string-width (car lines)) 72))
            (should (string-match-p
                     "Run a bounded .*\\.\\.\\.  \\[running · 14 calls\\]"
                     (car lines)))
            (should (string-match "coordinator" text))
            (should (eq (get-text-property
                         (match-beginning 0) 'keymap text)
                        mevedel-view--agent-label-map))
            (should (equal (get-text-property
                            (match-beginning 0)
                            'mevedel-view-agent-id text)
                           agent-id)))))))

  :doc "status row width uses the displayed view window"
  (mevedel-view-test--with-buffers
    (let ((other-buf (generate-new-buffer " *mevedel-test-other-window*")))
      (unwind-protect
          (progn
            (switch-to-buffer view-buf)
            (delete-other-windows)
            (let* ((view-window (selected-window))
                   (other-window (split-window-right)))
              (set-window-buffer other-window other-buf)
              (select-window other-window)
              (with-current-buffer view-buf
                (should (= (mevedel-view--agent-status-line-width)
                           (max 20
                                (1- (window-body-width
                                     view-window))))))))
        (delete-other-windows)
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf)))))

  :doc "live nested invocations are reachable from parent session view"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef123456")
           (display-id "explorer--abcdef12")
           (agent-buf (generate-new-buffer " *test-nested-agent-live*"))
           (fake-fsm (cons 'nested 'fsm))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :buffer agent-buf
                 :transcript-status 'running))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "nested-live-lookup"
                       :root temporary-file-directory
                       :name "nested-live-lookup"))
           (session (mevedel-session-create "main" workspace)))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer agent-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-agent-runtime--fsms
                          (list (cons agent-id fake-fsm))))
            (with-current-buffer view-buf
              (cl-letf (((symbol-function 'mevedel-agent-runtime--agent-invocation-at)
                         (lambda (fsm)
                           (and (eq fsm fake-fsm) inv))))
                (should (eq inv (mevedel-view--agent-invocation
                                 display-id))))))
        (when (buffer-live-p agent-buf)
          (kill-buffer agent-buf)))))

  :doc "live parent background-agent list orders child without sidecar metadata"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "reviewer--parent123")
           (child-id "explorer--child456")
           (parent-inv (mevedel-agent-invocation--create
                        :agent-id parent-id
                        :background-agents (list child-id)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-live-parent"
                       :root temporary-file-directory
                       :name "status-live-parent"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        '(:status running
                          :agent-type "explorer"
                          :description "inspect current changes"
                          :parent-turn 1))
                  (cons parent-id
                        '(:status running
                          :agent-type "reviewer"
                          :description "review patch"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (agent-id)
                     (and (equal agent-id parent-id) parent-inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 2 (length rows)))
            (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
            (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
            (should (= 1 (plist-get (nth 1 rows) :depth))))))))

  :doc "omits agents whose handles are already visible in the current view"
  (mevedel-view-test--with-buffers
    (let* ((running-id "explorer--run123")
           (done-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-collect"
                       :root temporary-file-directory
                       :name "status-collect"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons running-id '(:status running :calls 1))
                  (cons done-id '(:status completed :calls 2))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id running-id
                              'mevedel-view-agent-handle-p t))
          (insert (propertize "done\n"
                              'mevedel-view-agent-id done-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "stale queue origins do not promote terminal handles to blocked"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-stale"
                       :root temporary-file-directory
                       :name "status-stale"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "done\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "sidecar-running agent with queued interaction reports blocked"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--blocked123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sidecar-blocked"
                       :root temporary-file-directory
                       :name "status-sidecar-blocked"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "sidecar-running queued agent reports blocked without rendered handle"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--sidecaronly123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sidecar-only"
                       :root temporary-file-directory
                       :name "status-sidecar-only"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running :calls 1))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 1 (length rows)))
          (should (eq 'blocked (plist-get (car rows) :status)))))))

  :doc "sidecar-running nested agents appear even when parent handle is visible"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "coordinator--parent123")
           (child-id "reviewer--child456")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-nested-running"
                       :root temporary-file-directory
                       :name "status-nested-running"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons parent-id
                        '(:status running
                          :agent-type "coordinator"
                          :description "green loop"
                          :parent-turn 1))
                  (cons child-id
                        (list :status 'running
                              :agent-type "reviewer"
                              :description "review patch"
                              :parent-turn 1
                              :parent-agent-id parent-id))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "coordinator handle\n"
                              'mevedel-view-agent-id parent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 2 (length rows)))
          (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
          (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
          (should (eq 'running (plist-get (nth 1 rows) :status)))
          (should (= 1 (plist-get (nth 1 rows) :depth))))
        (let ((text (mevedel-view--agent-status-handles-string
                     (mevedel-view--agent-status-collect))))
          (should (string-match-p "^  ● Agent: coordinator -- green loop"
                                  text))
          (should (string-match-p "^    ● Agent: reviewer -- review patch"
                                  text))))))

  :doc "live invocation parent context orders nested agents by actual parent"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "reviewer--parent123")
           (child-id "explorer--child456")
           (parent-inv (mevedel-agent-invocation--create
                        :agent-id parent-id
                        :description "review patch"))
           (child-inv (mevedel-agent-invocation--create
                       :agent-id child-id
                       :description "validate current changes"
                       :parent-context parent-inv))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-infer-parent"
                       :root temporary-file-directory
                       :name "status-infer-parent"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        '(:status running
                          :agent-type "explorer"
                          :description "validate current changes"
                          :parent-turn 1))
                  (cons parent-id
                        '(:status running
                          :agent-type "reviewer"
                          :description "review patch"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (agent-id)
                     (cond
                      ((equal agent-id parent-id) parent-inv)
                      ((equal agent-id child-id) child-inv)))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 2 (length rows)))
            (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
            (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
            (should (= 1 (plist-get (nth 1 rows) :depth))))))))

  :doc "running aggregate rows show call count without activity body"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--activity123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-activity"
                       :root temporary-file-directory
                       :name "status-activity"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "validate current changes"
                 :call-count 2
                 :activity (list (list :type 'waiting :summary "waiting")
                                 (list :type 'tool-finish
                                       :tool-name "Read"
                                       :summary "Read done")))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "validate current changes"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv)))
          (let ((text (mevedel-view--agent-status-handles-string
                       (mevedel-view--agent-status-collect))))
            (should (string-match-p "\\[running · 2 calls\\]" text))
            (should-not (string-match-p "^  … waiting" text))
            (should-not (string-match-p "^  ✓ Read done" text)))))))

  :doc "current-turn completed nested agents do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "coordinator--parent123")
           (child-id "verifier--child456")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-nested-done"
                       :root temporary-file-directory
                       :name "status-nested-done"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        (list :status 'completed
                              :agent-type "verifier"
                              :description "verify patch"
                              :calls 2
                              :parent-turn 1
                              :parent-agent-id parent-id))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "current-turn terminal sidecar entries do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-terminal-sidecar"
                       :root temporary-file-directory
                       :name "status-terminal-sidecar"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-turn-count session) 2)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "verifier--done123"
                        (list :status 'completed
                              :agent-type "verifier"
                              :description "verify patch"
                              :calls 2
                              :parent-turn 3))
                  (cons "reviewer--aborted123"
                        (list :status 'aborted
                              :agent-type "reviewer"
                              :description "review task tooling diff"
                              :calls 3
                              :parent-turn 3
                              :reason "stopped by user"))
                  (cons "explorer--error123"
                        (list :status 'error
                              :agent-type "explorer"
                              :description "inspect changes"
                              :calls 4
                              :parent-turn 3
                              :reason "failed"))
                  (cons "coordinator--incomplete123"
                        (list :status 'incomplete
                              :agent-type "coordinator"
                              :description "coordinate"
                              :calls 5
                              :parent-turn 3))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "terminal live invocation suppresses stale running sidecar in aggregate"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--race123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'completed
                 :call-count 5
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-terminal-race"
                       :root temporary-file-directory
                       :name "status-terminal-race"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running :calls 2))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-agent-runtime--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (null rows)))))))

  :doc "old-turn errored registry entries are pruned from aggregate status"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--old-error123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-old-error"
                       :root temporary-file-directory
                       :name "status-old-error"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "verify current diff"
                 :transcript-status 'error
                 :call-count 16))
           (fsm (gptel-make-fsm
                 :info (list :mevedel-agent-invocation inv)
                 :handlers nil
                 :state 'WAIT)))
      (setf (mevedel-session-turn-count session) 3)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status error
                          :agent-type "verifier"
                          :description "verify current diff"
                          :calls 16
                          :parent-turn 1
                          :reason "HTTP/2 503"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons agent-id fsm))))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))
      (with-current-buffer data-buf
        (should-not (assoc agent-id mevedel-agent-runtime--fsms)))))

  :doc "current-turn errored sidecar entries do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--current-error123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-current-error"
                       :root temporary-file-directory
                       :name "status-current-error"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status error
                          :agent-type "verifier"
                          :description "verify current diff"
                          :calls 16
                          :parent-turn 1
                          :reason "HTTP/2 503"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "live agent without a visible handle still appears in aggregate status"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--hidden123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "hidden task"
                 :transcript-status 'running
                 :call-count 3
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-hidden-live"
                       :root temporary-file-directory
                       :name "status-hidden-live"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-agent-runtime--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 1 (length rows)))
            (should (eq 'running (plist-get (car rows) :status)))
            (should (= 3 (plist-get (car rows) :calls)))))))))

(mevedel-deftest mevedel-view-refresh-agent-rendering ()
  ,test
  (test)

  :doc "agent and compaction refreshes preserve an expanded handle and draft"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (draft "> quoted\nsecond line")
           (agent-tool (mevedel-tool--create
                        :name "Agent"
                        :renderer #'mevedel-tool-ui--render-agent))
           (invocation
            (mevedel-agent-invocation--create
             :agent (mevedel-agent--create :name "explorer")
             :agent-id agent-id
             :parent-data-buffer data-buf))
           bounds
           (render-data
            (list :kind 'agent-transcript
                  :agent-id agent-id
                  :status 'running
                  :calls 1
                  :background t)))
      (with-current-buffer data-buf
        (goto-char (point-max))
        (setq bounds (cons (point) nil))
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"count\"))\n")
        (insert "Agent is running.\n")
        (insert (mevedel-pipeline--format-render-data-block render-data))
        (setcdr bounds (point))
        (put-text-property (car bounds) (cdr bounds) 'gptel '(tool . "call-1")))
      (cl-letf (((symbol-function 'mevedel-tool-get)
                 (lambda (name &optional _category)
                   (and (equal name "Agent") agent-tool))))
        (with-current-buffer view-buf
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (mevedel-view--render-tool-group
             (list (list 'tool (car bounds) (cdr bounds))) data-buf))
          (goto-char (point-min))
          (should (search-forward "[running · 1 calls]"
                                  mevedel-view--input-marker t))
          (goto-char (match-beginning 0))
          (mevedel-view--expand-section
           (get-text-property (point) 'mevedel-view-source)
           'agent-handle)
          (should (search-forward "Agent is running."
                                  mevedel-view--input-marker t))
          (goto-char (mevedel-view--input-start))
          (insert draft)
          (goto-char (+ (mevedel-view--input-start) 4)))
        (let ((mevedel-view-agent-refresh-delay 0))
          (cl-letf (((symbol-function 'gptel--update-status) #'ignore))
            (mevedel--compact-agent-start
             (list :invocation invocation))
            (mevedel--compact-agent-complete
             (list :invocation invocation) t)))
        (with-current-buffer view-buf
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 4))))
        (with-current-buffer data-buf
          (pcase-let ((`(,start . ,end)
                       (mevedel-pipeline--find-render-data-block-by-agent-id
                        agent-id)))
            (mevedel-pipeline--patch-render-data-block
             start end (plist-put (copy-sequence render-data) :calls 2))))
        (with-current-buffer view-buf
          (let ((fallbacks 0)
                (mevedel-view-agent-refresh-delay 0))
            (cl-letf (((symbol-function 'mevedel-view-rerender)
                       (lambda (&optional _buffer)
                         (cl-incf fallbacks))))
              (mevedel-view-refresh-agent-rendering view-buf agent-id))
            (should (= 0 fallbacks)))
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 4)))
          (goto-char (point-min))
          (should (search-forward "[running · 2 calls]"
                                  mevedel-view--input-marker t))
          (goto-char (match-beginning 0))
          (should-not (get-text-property (point) 'mevedel-view-collapsed))
          (should (search-forward "Agent is running."
                                  mevedel-view--input-marker t))))))

  :doc "refreshes aggregate status rows without full rerendering"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--status-refresh")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-refresh"
                       :root temporary-file-directory
                       :name "status-refresh"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "status"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-agent-status)
        (goto-char (point-min))
        (should (search-forward "1 calls" nil t))
        (setf (cdr (assoc agent-id (mevedel-session-agent-transcripts session)))
              '(:status running
                :agent-type "explorer"
                :description "status"
                :calls 2))
        (let ((fallbacks 0)
              (mevedel-view-agent-refresh-delay 0))
          (cl-letf (((symbol-function 'mevedel-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf fallbacks))))
            (mevedel-view-refresh-agent-rendering view-buf agent-id))
          (should (= 0 fallbacks)))
        (goto-char (point-min))
        (should (search-forward "2 calls" nil t)))))

  :doc "agent status redraw preserves later queued permission prompt"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--refresh-permission")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "refresh-permission"
                       :root temporary-file-directory
                       :name "refresh-permission"))
           (session (mevedel-session-create "main" workspace))
           outcomes
           (entry (list :kind 'eval
                        :expression "(+ 20 22)"
                        :mode "live"
                        :origin agent-id
                        :session session
                        :callback (lambda (outcome)
                                    (push outcome outcomes)))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "verifier"
                          :description "permission"
                          :calls 1))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-agent-status)
        (goto-char (point-min))
        (should (search-forward "Agent: verifier -- permission"
                                mevedel-view--input-marker t))
        (let ((status-bounds (mevedel-view-zone-bounds-at
                              (match-beginning 0))))
          (should status-bounds)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (mevedel-permission-queue--render-head session))
          (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                  (buffer-string)))
          (should (= 1 (length (mevedel-session-permission-queue session))))
          (should-not outcomes)
          (should (overlayp (mevedel-view-zone-region 'interaction)))
          (should (<= (plist-get status-bounds :end)
                      (overlay-start
                       (mevedel-view-zone-region 'interaction))))
          (mevedel-view--render-agent-status)
          (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                  (buffer-string)))
          (should (= 1 (length (mevedel-session-permission-queue session))))
          (should-not outcomes)
          (goto-char (point-min))
          (search-forward "Agent: verifier -- permission"
                          mevedel-view--input-marker)
          (setq status-bounds
                (mevedel-view-zone-bounds-at (match-beginning 0)))
          (should (<= (plist-get status-bounds :end)
                      (overlay-start
                       (mevedel-view-zone-region 'interaction))))))))

  :doc "falls back when data has an Agent source but no visible handle"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--missing-handle")
           (render-data (list :kind 'agent-transcript
                              :agent-id agent-id
                              :status 'running
                              :calls 1))
           bounds)
      (with-current-buffer data-buf
        (goto-char (point-max))
        (setq bounds (cons (point) nil))
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"missing\"))\n")
        (insert "Agent is running.\n")
        (insert (mevedel-pipeline--format-render-data-block render-data))
        (setcdr bounds (point))
        (put-text-property (car bounds) (cdr bounds) 'gptel '(tool . "call-1")))
      (with-current-buffer view-buf
        (let ((fallbacks 0)
              (mevedel-view-agent-refresh-delay 0))
          (cl-letf (((symbol-function 'mevedel-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf fallbacks))))
            (mevedel-view-refresh-agent-rendering view-buf agent-id))
          (should (= 1 fallbacks)))))))

(mevedel-deftest mevedel-view--agent-status-counts
  (:doc "ignores malformed stale FSM registry entries")
  ,test
  (test)

  :doc "malformed stale FSM entries do not break counts or aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--badfsm123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-bad-fsm"
                       :root temporary-file-directory
                       :name "status-bad-fsm"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "stale sidecar"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-agent-runtime--fsms
                    (list (cons agent-id (cons 'bad 'fsm)))))
      (with-current-buffer view-buf
        (should (equal (mevedel-view--agent-status-counts)
                       '(:blocked 0 :running 1)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 1 (length rows)))
          (should (equal agent-id (plist-get (car rows) :agent-id))))))))

(mevedel-deftest mevedel-view-agent-status-fragment
  (:doc "uses fragment-backed Agent handles for expanded aggregate status")
  ,test
  (test)

  :doc "expanded aggregate status renders Agent handles inside the status fragment"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--run123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-agent-handle"
                       :root temporary-file-directory
                       :name "status-agent-handle"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq 'status (get-text-property
                             (point) 'mevedel-view-zone-namespace)))
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-zone-id)))
        (should (get-text-property (point) 'mevedel-view-agent-handle-p)))))

  :doc "shared activation on expanded status handles opens the Agent transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-agent-handle-activate"
                       :root temporary-file-directory
                       :name "status-agent-handle-activate"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "done"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- done" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript-at-point)
                   (lambda (&optional _event)
                     (setq opened (get-text-property
                                   (point) 'mevedel-view-agent-id)))))
          (mevedel-view-activate-at-point))
        (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--insert-attribution
  (:doc "builds transcript attribution fragments")
  ,test
  (test)

  :doc "uses short display label and installs a deferred click target"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((s (mevedel-view--insert-attribution
                 "explorer--abcdef1234567890"))
             (pos (string-match-p "explorer--abcdef12" s)))
        (should (string-match-p "from explorer--abcdef12" s))
        (should pos)
        (should (get-text-property pos 'keymap s))
        (should (equal (get-text-property pos 'mevedel-view-agent-id s)
                       "explorer--abcdef1234567890"))
        (should (get-text-property pos 'help-echo s)))))

  :doc "completed transcript dispatches through the shared open command"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-open"
                       :root temporary-file-directory
                       :name "attr-open"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-open-session")))
           opened)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view--open-agent-transcript-or-message agent-id)
          (should (equal agent-id opened)))))))



(provide 'test-mevedel-view-agent)
;;; test-mevedel-view-agent.el ends here
