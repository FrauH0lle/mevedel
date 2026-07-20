;;; test-mevedel-view-agent.el --- Agent view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests retained-agent transcript inspection and registry-backed status.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-agents)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tool-ui)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-zone)
(require 'mevedel-workspace)

(defun mevedel-view-agent-test--session ()
  "Return a fresh in-memory session for retained-agent view tests."
  (mevedel-session-create
   "main"
   (mevedel-workspace--create
    :type 'project
    :id "agent-view"
    :root temporary-file-directory
    :name "agent-view")))

(defun mevedel-view-agent-test--record
    (path activity &optional invocation role parent)
  "Return a retained record at PATH with ACTIVITY and optional metadata."
  (mevedel-agent-record--create
   :id (concat "storage-" (replace-regexp-in-string "/" "-" path))
   :path path
   :parent-path (or parent "/root")
   :role (or role "default")
   :activity activity
   :invocation invocation))

(mevedel-deftest mevedel-view-agent--handle-badge
  (:doc "maps :status + :calls/:elapsed/:reason to a state badge string")
  ,test
  (test)

  :doc "running with N calls renders [running · N calls]"
  (should (string-match-p
           "running.*3 calls"
           (mevedel-view-agent--handle-badge
            '(:status running :calls 3))))

  :doc "running with zero calls suppresses the count suffix"
  (let ((badge (mevedel-view-agent--handle-badge
                '(:status running :calls 0))))
    (should (string-match-p "running" badge))
    (should-not (string-match-p "calls" badge)))

  :doc "blocked reason overrides running badge"
  (let ((badge (mevedel-view-agent--handle-badge
                '(:status running :calls 2 :blocked-reason "permission"))))
    (should (string-match-p "blocked" badge))
    (should (string-match-p "permission" badge))
    (should-not (string-match-p "running" badge)))

  :doc "completed renders ✓ done with elapsed and calls"
  (let ((badge (mevedel-view-agent--handle-badge
                '(:status completed :calls 5 :elapsed 2.3))))
    (should (string-match-p "done" badge))
    (should (string-match-p "2\\.3s" badge))
    (should (string-match-p "5 calls" badge)))

  :doc "completed without elapsed/calls renders just ✓ done"
  (should (string-match-p
           "✓ done"
           (mevedel-view-agent--handle-badge '(:status completed))))

  :doc "error renders ✗ error · REASON"
  (should (string-match-p
           "error.*max-turns"
           (mevedel-view-agent--handle-badge
            '(:status error :reason "max-turns"))))

  :doc "aborted renders ✗ aborted"
  (should (string-match-p
           "✗ aborted"
           (mevedel-view-agent--handle-badge '(:status aborted))))

  :doc "incomplete renders ○ incomplete"
  (should (string-match-p
           "○ incomplete"
           (mevedel-view-agent--handle-badge '(:status incomplete))))

  :doc "unknown status returns empty string"
  (should (equal ""
                 (mevedel-view-agent--handle-badge '(:status banana)))))

(mevedel-deftest mevedel-view-agent--blocked-reason
  (:doc "reports the root interaction queue blocking a canonical path")
  ,test
  (test)

  :doc "prefers a pending permission over a pending plan"
  (let ((session (mevedel-view-agent-test--session)))
    (setf (mevedel-session-permission-queue session)
          '((:origin "/root/worker"))
          (mevedel-session-plan-queue session)
          '((:origin "/root/worker")))
    (should
     (equal "permission"
            (mevedel-view-agent--blocked-reason
             "/root/worker" session))))

  :doc "reports plan blocking and ignores unrelated paths"
  (let ((session (mevedel-view-agent-test--session)))
    (setf (mevedel-session-plan-queue session)
          '((:origin "/root/worker")))
    (should
     (equal "plan"
            (mevedel-view-agent--blocked-reason
             "/root/worker" session)))
    (should-not
     (mevedel-view-agent--blocked-reason "/root/other" session))))

(mevedel-deftest mevedel-view-agent-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-view-open-agent-transcript
                    mevedel-view--agent-status-collect
                    mevedel-view-refresh-agent-rendering))
    (should (equal "mevedel-view-agent"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))

(mevedel-deftest mevedel-view-agent-initialize
  (:doc "initializes canonical transcript inspection state")
  ,test
  (test)

  :doc "transcript views retain their canonical path and inspection-only keys"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (use-local-map mevedel-view-mode-map)
          (let ((info '(:agent-path "/root/spec_review" :status running))
                (parent (current-buffer)))
            (mevedel-view-agent-initialize
             (list :agent-transcript-p t
                   :agent-path "/root/spec_review"
                   :transcript-info info
                   :parent-view parent
                   :preserve-data-view-buffer t)
             data-buffer)
            (should mevedel-view--agent-transcript-p)
            (should (equal "/root/spec_review" mevedel-view--agent-path))
            (should (equal info mevedel-view--agent-transcript-info))
            (should (eq parent mevedel-view--agent-transcript-parent-view))
            (should (eq #'mevedel-view-close-agent-transcript
                        (lookup-key (current-local-map) (kbd "q"))))
            (should (eq #'mevedel-view--transcript-gptel-send-blocked
                        (lookup-key (current-local-map) [remap gptel-send])))
            (with-current-buffer data-buffer
              (should (memq #'mevedel-view--on-agent-transcript-data-killed
                            kill-buffer-hook)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "ordinary views leave their map and header untouched"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((map (make-sparse-keymap)))
            (use-local-map map)
            (setq header-line-format 'keep)
            (mevedel-view-agent-initialize nil data-buffer)
            (should-not mevedel-view--agent-transcript-p)
            (should (eq map (current-local-map)))
            (should (eq 'keep header-line-format))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-view-agent-cleanup-parent
  (:doc "cleans up saved transcript views without tearing down the session")
  ,test
  (test)
  (let* ((session (mevedel-view-agent-test--session))
         (parent-view (generate-new-buffer " *test-agent-parent-view*"))
         (data-buffer (generate-new-buffer " *test-agent-saved-data*"))
         (view-buffer (generate-new-buffer " *test-agent-saved-view*"))
         (teardown-count 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup
           view-buffer data-buffer
           (list :agent-transcript-p t
                 :parent-view parent-view))
          (cl-letf (((symbol-function
                      'mevedel-agent-control-teardown-session)
                     (lambda (_session)
                       (cl-incf teardown-count))))
            (mevedel-view-agent-cleanup-parent parent-view))
          (should (= 0 teardown-count))
          (should-not (buffer-live-p view-buffer))
          (should-not (buffer-live-p data-buffer)))
      (dolist (buffer (list view-buffer data-buffer parent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-view--agent-record
  (:doc "resolves retained identities by canonical path")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (record (mevedel-view-agent-test--record
                    "/root/spec_review" 'idle)))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/spec_review" record)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (eq record
                    (mevedel-view--agent-record "/root/spec_review")))
        (should-not (mevedel-view--agent-record "/root/missing"))))))

(mevedel-deftest mevedel-view--lookup-transcript-pair
  (:doc "joins canonical paths to opaque transcript storage identities")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (record (mevedel-view-agent-test--record
                    "/root/spec_review" 'idle))
           (storage-id (mevedel-agent-record-id record))
           (pair (cons storage-id '(:path "agents/spec.chat.org"
                                    :status completed))))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/spec_review" record)))
      (setf (mevedel-session-agent-transcripts session) (list pair))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (eq pair
                    (mevedel-view--lookup-transcript-pair
                     "/root/spec_review")))
        (should-not (mevedel-view--lookup-transcript-pair storage-id)))))

  :doc "historical fork artifacts resolve by recorded canonical path"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (pair (cons "storage-historical"
                       '(:agent-path "/root/historical"
                         :path "agents/historical.chat.org"
                         :status completed))))
      (setf (mevedel-session-agent-transcripts session) (list pair))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (eq pair
                    (mevedel-view--lookup-transcript-pair
                     "/root/historical")))))))

(mevedel-deftest mevedel-view--agent-status-counts
  (:doc "counts active retained records without transcript inference")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-view-agent-test--session)))
      (setf (mevedel-session-agent-registry session)
            (list
             (cons "/root/run"
                   (mevedel-view-agent-test--record "/root/run" 'running))
             (cons "/root/wait"
                   (mevedel-view-agent-test--record "/root/wait" 'waiting))
             (cons "/root/blocked"
                   (mevedel-view-agent-test--record
                    "/root/blocked" 'permission-blocked))
             (cons "/root/done"
                   (mevedel-view-agent-test--record "/root/done" 'idle))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (equal '(:blocked 2 :running 1)
                       (mevedel-view--agent-status-counts)))))))

(mevedel-deftest mevedel-view--agent-status-collect
  (:doc "derives live rows and hierarchy solely from the retained registry")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (parent-inv (mevedel-agent-invocation--create
                        :description "Review the spec"
                        :call-count 2
                        :started-at (current-time)))
           (child-inv (mevedel-agent-invocation--create
                       :description "Verify details"
                       :call-count 1
                       :started-at (current-time))))
      (setf (mevedel-session-agent-registry session)
            (list
             (cons "/root/spec_review"
                   (mevedel-view-agent-test--record
                    "/root/spec_review" 'running parent-inv "reviewer"))
             (cons "/root/spec_review/details"
                   (mevedel-view-agent-test--record
                    "/root/spec_review/details" 'waiting child-inv "explorer"
                    "/root/spec_review"))
             (cons "/root/done"
                   (mevedel-view-agent-test--record "/root/done" 'idle))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (equal '("/root/spec_review"
                           "/root/spec_review/details")
                         (mapcar (lambda (row) (plist-get row :path)) rows)))
          (should (eq 'running (plist-get (car rows) :status)))
          (should (eq 'waiting (plist-get (cadr rows) :status)))
          (should (= 1 (plist-get (cadr rows) :depth)))))))

  :doc "source-backed path handles suppress duplicate aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (path "/root/spec_review"))
      (setf (mevedel-session-agent-registry session)
            (list (cons path
                        (mevedel-view-agent-test--record path 'running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert (propertize "Started /root/spec_review"
                              'mevedel-view-agent-path path
                              'mevedel-view-agent-handle-p t)))
        (should-not (mevedel-view--agent-status-collect))))))

(mevedel-deftest mevedel-view-agent-status-fragment
  (:doc "renders canonical paths and preserves active composer drafts")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-view-agent-test--session))
           (path "/root/spec_review"))
      (setf (mevedel-session-agent-registry session)
            (list (cons path
                        (mevedel-view-agent-test--record
                         path 'running nil "reviewer"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (point-max))
        (insert "> first line\nsecond line")
        (mevedel-view--render-status data-buf)
        (should (string-suffix-p "> first line\nsecond line"
                                 (buffer-substring-no-properties
                                  (point-min) (point-max))))
        (goto-char (point-min))
        (search-forward "Running /root/spec_review")
        (should (equal path
                       (get-text-property
                        (1- (point)) 'mevedel-view-agent-path)))))))

(mevedel-deftest mevedel-view--insert-attribution
  (:doc "uses canonical paths as transcript click targets")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((path "/root/spec_review")
             (text (mevedel-view--insert-attribution path))
             (pos (string-match-p path text)))
        (should (string= "from /root/spec_review" text))
        (should pos)
        (should (equal path
                       (get-text-property pos 'mevedel-view-agent-path text)))
        (should (get-text-property pos 'keymap text))))))

(mevedel-deftest mevedel-view-refresh-agent-rendering
  (:doc "coalesces canonical-path refreshes without altering the composer")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "> draft\ncontinues")
      (let ((mevedel-view-agent-refresh-delay 0))
        (cl-letf (((symbol-function 'mevedel-view--agent-handle-refresh-points)
                   (lambda (_path) '(nil . nil)))
                  ((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil)))
          (mevedel-view-refresh-agent-rendering
           view-buf "/root/spec_review")))
      (should (string-suffix-p "> draft\ncontinues"
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))))

(provide 'test-mevedel-view-agent)
;;; test-mevedel-view-agent.el ends here
