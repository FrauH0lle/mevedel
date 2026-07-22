;;; test-mevedel-view-interaction.el --- Interaction-zone tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests managed interaction-zone registration and redraw behavior.

;;; Code:

(require 'gptel-agent-tools)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-interaction)
(require 'mevedel-view-stream)
(require 'mevedel-transcript)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-preview-mode)
(require 'mevedel-permission-queue)
(require 'mevedel-goal)
(require 'mevedel-agents)
(require 'mevedel-agent-control)
(require 'mevedel-agent-runtime)
(require 'mevedel-view-zone)

(defun test-mevedel-view-interaction--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(mevedel-deftest mevedel-view-interaction-ownership ()
  ,test
  (test)
  (dolist (symbol '(mevedel-view--interaction-register
                    mevedel-view--interaction-rebuild
                    mevedel-view--interaction-clear))
    (should (equal "mevedel-view-interaction"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))

(mevedel-deftest mevedel-view-interaction-initialize
  (:doc "creates fresh interaction state for each view")
  ,test
  (test)

  :doc "initialization replaces stale descriptor and overlay tables"
  (with-temp-buffer
    (setq-local mevedel-view--interaction-descriptors
                (let ((table (make-hash-table :test #'equal)))
                  (puthash 'stale t table)
                  table))
    (setq-local mevedel-view--interaction-overlays
                mevedel-view--interaction-descriptors)
    (mevedel-view-interaction-initialize)
    (should (hash-table-p mevedel-view--interaction-descriptors))
    (should (hash-table-p mevedel-view--interaction-overlays))
    (should-not (eq mevedel-view--interaction-descriptors
                    mevedel-view--interaction-overlays))
    (should (= 0 (hash-table-count mevedel-view--interaction-descriptors)))
    (should (= 0 (hash-table-count mevedel-view--interaction-overlays)))))

(mevedel-deftest mevedel-view-interaction-pending-p
  (:doc "reports pending view-owned user interactions")
  ,test
  (test)

  :doc "detects prompt-hook and registered descriptor state"
  (with-temp-buffer
    (mevedel-view-interaction-initialize)
    (should-not (mevedel-view-interaction-pending-p))
    (setq-local mevedel-view--prompt-hook-pending t)
    (should (mevedel-view-interaction-pending-p))
    (setq-local mevedel-view--prompt-hook-pending nil)
    (puthash 'ask '(:kind ask) mevedel-view--interaction-descriptors)
    (should (mevedel-view-interaction-pending-p (current-buffer))))

  :doc "rejects a dead view buffer"
  (let ((view (generate-new-buffer " *dead-interaction-view*")))
    (kill-buffer view)
    (should-not (mevedel-view-interaction-pending-p view))))

(mevedel-deftest mevedel-view--interaction-register ()
  ,test
  (test)
  :doc "acquires one blocker per non-permission interaction identity"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "interaction-state"))
           (invocation
            (mevedel-agent-invocation--create :path "/root/worker"))
           (record
            (mevedel-agent-record--create
             :path "/root/worker" :activity 'running
             :invocation invocation)))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "ask"))
        (mevedel-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "updated"))
        (should (eq 'interaction-blocked
                    (mevedel-agent-record-activity record)))
        (should (= 1 (length (mevedel-agent-record-blockers record))))
        (mevedel-view--interaction-register
         '(:kind permission :id permission :origin "/root/worker"
           :body "permission"))
        (should (= 1 (length (mevedel-agent-record-blockers record))))))))

(mevedel-deftest mevedel-view--interaction-unregister ()
  ,test
  (test)
  :doc "releases overlapping interactions independently and in any order"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "interaction-release"))
           (invocation
            (mevedel-agent-invocation--create :path "/root/worker"))
           (record
            (mevedel-agent-record--create
             :path "/root/worker" :activity 'running
             :invocation invocation)))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-register
         '(:kind ask :id ask :origin "/root/worker" :body "ask"))
        (mevedel-view--interaction-register
         '(:kind preview :id preview :origin "/root/worker" :body "diff"))
        (should (= 2 (length (mevedel-agent-record-blockers record))))
        (mevedel-view--interaction-unregister 'ask)
        (should (eq 'interaction-blocked
                    (mevedel-agent-record-activity record)))
        (mevedel-view--interaction-unregister 'preview)
        (should (eq 'running (mevedel-agent-record-activity record)))
        (should-not (mevedel-agent-record-blockers record))))))

(mevedel-deftest mevedel-view--interaction-telemetry-close
  (:doc "records base and effective permission modes at open and close")
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "interaction-telemetry"
                    :permission-mode 'full-auto))
          events)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'mevedel-telemetry-record)
                   (lambda (_session event &rest props)
                     (push (cons event props) events))))
          (mevedel-view--interaction-register
           '(:kind permission :id permission :origin "/root" :body "allow"))
          (mevedel-view--interaction-unregister 'permission))
        (dolist (event events)
          (should (eq 'full-auto
                      (plist-get (cdr event) :permission-mode-base)))
          (should (eq 'full-auto
                      (plist-get (cdr event) :permission-mode-effective))))
        (should (assq 'interaction-opened events))
        (should (assq 'interaction-closed events))))))

(mevedel-deftest mevedel-view--interaction-delete-overlay ()
  ,test
  (test)
  :doc "releases an overlay activity token at most once while deleting it"
  (with-temp-buffer
    (let ((overlay (make-overlay (point-min) (point-min)))
          (releases 0))
      (overlay-put overlay 'mevedel-agent-activity-release
                   (lambda () (cl-incf releases)))
      (mevedel-view--interaction-delete-overlay overlay)
      (mevedel-view--interaction-delete-overlay overlay)
      (should (= 1 releases))
      (should-not (overlay-buffer overlay))
      (should-not
       (overlay-get overlay 'mevedel-agent-activity-release)))))

(mevedel-deftest mevedel-view--interaction-target-buffer
  (:doc "resolves the live parent view for queued interactions")
  ,test
  (test)

  :doc "agent data buffers fall back through invocation parent data"
  (let ((parent-data (generate-new-buffer " *test-parent-data-prompt*"))
        (parent-view (generate-new-buffer " *test-parent-view-prompt*"))
        (agent-data (generate-new-buffer " *test-agent-data-prompt*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/prompt-view/"
                   :root "/tmp/prompt-view/" :name "prompt-view"))))
    (unwind-protect
        (let ((inv (mevedel-agent-invocation--create
                    :agent-id "verifier--prompt123"
                    :parent-data-buffer parent-data
                    :buffer agent-data
                    :transcript-status 'running)))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer nil))
          (should (eq parent-view
                      (mevedel-view--interaction-target-buffer
                       agent-data)))
          (with-current-buffer agent-data
            (should (eq parent-view
                        (mevedel-view--interaction-target-buffer)))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data)))))

(mevedel-deftest mevedel-view--interaction-zone-render
  (:doc "renders and rebuilds interaction-zone fragments")
  ,test
  (test)

  :doc "permission and plan descriptors materialize as real text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 2
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (mevedel-view--interaction-register
         (list :kind 'plan :id 'plan :count 1
               :body "\nplan\n" :keymap map
               :help-echo "Plan" :entry 'plan-entry
               :activate #'ignore)))
      (should (equal "1 plan · 2 permissions pending"
                     (mevedel-view--interaction-count-label)))
      (goto-char (point-min))
      (search-forward "1 plan · 2 permissions pending"
                      mevedel-view--input-marker)
      (should (eq 'interaction (get-text-property
                                (match-beginning 0)
                                'mevedel-view-zone-namespace)))
      (should (eq :separator (get-text-property
                              (match-beginning 0)
                              'mevedel-view-zone-id)))
      (should (overlayp (mevedel-view-zone-region 'interaction)))
      (should (string-match-p "plan" (buffer-string)))
      (should (string-match-p "permission" (buffer-string)))
      (should (string-match-p "plan\n\npermission" (buffer-string)))
      (maphash
       (lambda (_id overlay)
         (should (< (overlay-start overlay) (overlay-end overlay)))
         (should (overlay-get overlay 'mevedel-view-interaction-entry))
         (should (overlay-get overlay 'mevedel-view-interaction-activate))
         (should (overlay-get overlay 'keymap))
         (should-not (overlay-get overlay 'before-string))
         (should (get-text-property
                  (overlay-start overlay)
                  'mevedel-view-interaction-overlay))
         (should-not (get-text-property
                      (overlay-start overlay)
                      'mouse-face)))
       mevedel-view--interaction-overlays)))

  :doc "shared activation does not call interaction descriptor callbacks"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((called nil))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission needs an explicit outcome\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate (lambda () (setq called t))))
        (goto-char (point-min))
        (search-forward "permission needs" mevedel-view--input-marker)
        (should-error (mevedel-view-activate-at-point) :type 'user-error)
        (should-not called))))

  :doc "fragment migration reuses descriptor overlays and metadata"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((map (make-sparse-keymap))
             (overlay
              (mevedel-view--interaction-register
               (list :kind 'preview :id 'preview :count 1
                     :body "\nold body text\n" :keymap map
                     :help-echo "Preview" :entry 'preview-entry
                     :activate #'ignore))))
        (overlay-put overlay 'test-private-state 'kept)
        (goto-char (overlay-start overlay))
        (search-forward "body" (overlay-end overlay))
        (let ((updated
               (mevedel-view--interaction-register
                (list :kind 'preview :id 'preview :count 1
                      :body "\nnew body text\n" :keymap map
                      :help-echo "Preview" :entry 'preview-entry
                      :activate #'ignore))))
          (should (eq overlay updated))
          (should (eq 'kept (overlay-get overlay 'test-private-state)))
          (should (eq overlay
                      (get-text-property
                       (overlay-start overlay)
                       'mevedel-view-interaction-overlay)))
          (should (get-text-property (overlay-start overlay)
                                     'mevedel-view-zone-key))
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (= 1 (mevedel-view-test--count-substring
                          "new body text" text)))
            (should (= 0 (mevedel-view-test--count-substring
                          "old body text" text))))))))

  :doc "fragment rendering normalizes body without trailing newline"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((overlay
             (mevedel-view--interaction-register
              (list :kind 'preview :id 'preview :count 1
                    :body "preview body" :keymap (make-sparse-keymap)
                    :entry 'preview-entry :activate #'ignore))))
        (should (equal "preview body\n"
                       (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay)))))))

  :doc "normalizes raw UTF-8 bytes in interaction descriptor bodies"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((raw (test-mevedel-view-interaction--raw-bytes
                  #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body (concat "\npermission " raw "\n")
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore)))
      (should (string-match-p "permission “x”" (buffer-string)))))

  :doc "ignores interaction markers that drift before the status zone"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (set-marker mevedel-view--interaction-marker (point-min))
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "\npermission below status\n"
             :keymap (make-sparse-keymap)
             :entry 'permission-entry
             :activate #'ignore))
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (header (string-trim-right
                      (mevedel-view--header-string data-buf)))
             (header-pos (string-search header text))
             (permission-pos (string-search "permission below status"
                                            text)))
        (should header-pos)
        (should permission-pos)
        (should (< header-pos permission-pos))
        (should (>= (overlay-start
                     (mevedel-view-zone-region 'interaction))
                    (marker-position mevedel-view--status-marker))))))

  :doc "ignores jointly drifted status and interaction markers before the header"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-stale-header/"
                :root "/tmp/view-stale-header/"
                :name "view-stale-header"))
           (session (mevedel-session-create "renamed" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (set-marker mevedel-view--status-marker (point-min))
        (set-marker mevedel-view--interaction-marker (point-min))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission below header\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (header "mevedel")
               (header-pos (string-search header text))
               (permission-pos (string-search "permission below header"
                                              text)))
          (should header-pos)
          (should permission-pos)
          (should (< header-pos permission-pos))
          (should (>= (overlay-start
                       (mevedel-view-zone-region 'interaction))
                      (mevedel-view--header-end-position)))))))

  :doc "managed interaction region excludes request-progress spinner"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-test--with-buffers
      (with-current-buffer view-buf
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission\n" :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (mevedel-view--start-spinner "Working...")
        (should (overlayp (mevedel-view-zone-region 'interaction)))
        (should (overlayp (mevedel-view-zone-region 'progress)))
        (should (<= (overlay-end (mevedel-view-zone-region 'interaction))
                    (overlay-start
                     (mevedel-view-zone-region 'progress))))
        (should-not (string-match-p
                     "Working"
                     (buffer-substring-no-properties
                      (overlay-start (mevedel-view-zone-region 'interaction))
                      (overlay-end (mevedel-view-zone-region 'interaction))))))))

  :doc "clear-for-rebuild removes rebuild-owned fragment text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'permission-entry :activate #'ignore))
      (mevedel-view--interaction-clear-for-rebuild)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "permission prompt" text))
        (should (= 0 (hash-table-count
                      mevedel-view--interaction-descriptors)))
        (should (= 0 (hash-table-count
                      mevedel-view--interaction-overlays))))))

  :doc "status redraw stays above existing permission interaction"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project :id "/tmp/view-interaction-order/"
                :root "/tmp/view-interaction-order/"
                :name "view-interaction-order"))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission prompt\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/verifier"
                                 :status 'blocked
                                 :role "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19))))
                  ((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'ask)))
          (mevedel-view--render-agent-status))
        (let (agent-pos separator-pos prompt-pos)
          (save-excursion
            (goto-char (point-min))
            (search-forward "Blocked /root/verifier")
            (setq agent-pos (match-beginning 0))
            (search-forward "1 permission pending")
            (setq separator-pos (match-beginning 0))
            (search-forward "permission prompt")
            (setq prompt-pos (match-beginning 0)))
          (should (eq 'status (get-text-property
                               agent-pos
                               'mevedel-view-zone-namespace)))
          (should (eq 'agents (get-text-property
                               agent-pos 'mevedel-view-zone-id)))
          (should (eq 'interaction (get-text-property
                                    separator-pos
                                    'mevedel-view-zone-namespace)))
          (should (eq :separator (get-text-property
                                  separator-pos 'mevedel-view-zone-id)))
          (should (< agent-pos separator-pos))
          (let ((permission-overlay
                 (gethash 'permission mevedel-view--interaction-overlays)))
            (should (overlayp permission-overlay))
            (should (<= separator-pos (overlay-start permission-overlay))))
          (should (<= separator-pos prompt-pos))))))

  :doc "permission prompt appears below existing agent status"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project :id "/tmp/view-interaction-after-status/"
                :root "/tmp/view-interaction-after-status/"
                :name "view-interaction-after-status"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :path "/root/verifier"
                                 :status 'blocked
                                 :role "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19)))))
          (mevedel-view--render-agent-status))
        (should (string-match-p
                 "Blocked /root/verifier"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (with-current-buffer data-buf
        (mevedel-permission--enqueue
         (list :kind 'generic
               :tool-name "Read"
               :specifier-key :path
               :specifier-value "/tmp/after-status.txt"
               :include-always nil
               :origin "/root/verifier"
               :callback (lambda (outcome) (push outcome outcomes)))
         session))
      (with-current-buffer view-buf
        (should-not outcomes)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (agent-pos (string-search
                           "Blocked /root/verifier" text))
               (permission-pos (string-search "Permission Request" text)))
          (should agent-pos)
          (should permission-pos)
          (should (< agent-pos permission-pos))
          (should (string-search "1 permission pending" text))
          (goto-char (point-min))
          (search-forward "1 permission pending" mevedel-view--input-marker)
          (should (eq :separator (get-text-property
                                  (match-beginning 0)
                                  'mevedel-view-zone-id)))))))

  :doc "permission queue renders only the FIFO head while request progress is visible"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-test--with-buffers
      (let ((session (mevedel-session--create
                      :name "test"
                      :workspace nil
                      :permission-rules nil
                      :permission-mode 'ask
                      :permission-queue nil
                      :pending-plan-approval nil))
            outcomes)
        (with-current-buffer data-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--view-buffer view-buf))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (mevedel-view--start-spinner "Working..."))
        (cl-letf (((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'ask)))
          (with-current-buffer data-buf
            (dolist (path '("/tmp/one.el" "/tmp/two.el" "/tmp/three.el"))
              (let ((captured-path path))
                (mevedel-permission--enqueue
                 (list :kind 'generic
                       :tool-name "Read"
                       :specifier-key :path
                       :specifier-value captured-path
                       :include-always nil
                       :origin "/root"
                       :callback
                       (lambda (outcome)
                         (push (cons captured-path outcome) outcomes)))
                 session)))))
        (with-current-buffer view-buf
          (cl-labels
              ((display-text ()
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))
               (head-overlay ()
                 (let* ((entry (car (mevedel-session-permission-queue
                                     session)))
                        (id (mevedel-queue--entry-metadata-get
                             entry :interaction-id)))
                   (and id (gethash id mevedel-view--interaction-overlays))))
               (settle-head ()
                 (mevedel--prompt--settle (head-overlay) 'allow-once))
               (should-show (count visible-path hidden-paths)
                 (let ((text (display-text)))
                   (should (= 1 (mevedel-view-test--count-substring
                                 "Permission Request" text)))
                   (should (string-search
                            (format "%d permission%s pending"
                                    count (if (= count 1) "" "s"))
                            text))
                   (should (string-search visible-path text))
                   (dolist (path hidden-paths)
                     (should-not (string-search path text))))))
            (should-show 3 "/tmp/one.el" '("/tmp/two.el" "/tmp/three.el"))
            (settle-head)
            (should-show 2 "/tmp/two.el" '("/tmp/one.el" "/tmp/three.el"))
            (settle-head)
            (should-show 1 "/tmp/three.el" '("/tmp/one.el" "/tmp/two.el"))
            (settle-head)
            (let ((text (display-text)))
              (should (= 0 (mevedel-view-test--count-substring
                            "Permission Request" text)))
              (should-not (string-search "permission pending" text))
              (should-not (string-search "permissions pending" text))
              (should-not (string-search "/tmp/one.el" text))
              (should-not (string-search "/tmp/two.el" text))
              (should-not (string-search "/tmp/three.el" text)))
            (should (equal '(("/tmp/three.el" . allow-once)
                             ("/tmp/two.el" . allow-once)
                             ("/tmp/one.el" . allow-once))
                           outcomes)))))))

  :doc "render removes interaction fragment text from a dead region overlay"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'old-permission :count 1
             :body "old permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'old-entry :activate #'ignore))
      (delete-overlay (mevedel-view-zone-region 'interaction))
      (mevedel-view-zone-forget 'interaction)
      (mevedel-view--interaction-clear-for-rebuild)
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'new-permission :count 1
             :body "new permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'new-entry :activate #'ignore))
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 0 (mevedel-view-test--count-substring
                      "old permission prompt" text)))
        (should (= 1 (mevedel-view-test--count-substring
                      "new permission prompt" text))))))

  :doc "clears stale interaction prompt overlays without settling"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((outcomes nil)
            (ov (make-overlay (point-min) (point-min)
                              (current-buffer) nil t)))
        (overlay-put ov 'mevedel-view-interaction-id 'stale-permission)
        (overlay-put ov 'mevedel-user-request t)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (push outcome outcomes)))
        (setq mevedel--prompt-overlays (list ov))
        (mevedel-view--interaction-clear)
        (should-not (overlay-buffer ov))
        (should-not mevedel--prompt-overlays)
        (should-not outcomes))))

  :doc "rebuild preserves direct request and ask prompts without settling"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (overlays
            outcomes)
        (dolist (kind '(request ask))
          (let* ((captured-kind kind)
                 (id (list captured-kind))
                 (ov
                  (mevedel-view--interaction-register
                   (list :kind captured-kind
                         :id id
                         :count 0
                         :body (format "\n%s prompt\n" captured-kind)
                         :keymap (make-sparse-keymap)
                         :activate
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes))))))
            (overlay-put ov 'mevedel-user-request t)
            (overlay-put ov 'mevedel--callback
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes)))
            (push (cons captured-kind ov) overlays)
            (cl-pushnew ov mevedel--prompt-overlays :test #'eq)))
        (mevedel-view--interaction-rebuild)
        (should-not outcomes)
        (should (= 2 (length mevedel--prompt-overlays)))
        (dolist (pair overlays)
          (let* ((kind (car pair))
                 (ov (cdr pair))
                 (id (list kind)))
            (should (eq ov (gethash id
                                    mevedel-view--interaction-overlays)))
            (should (overlay-buffer ov))
            (should (gethash id mevedel-view--interaction-descriptors))))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "request prompt" text))
          (should (string-match-p "ask prompt" text)))
        (should (equal "1 request · 1 question pending"
                       (mevedel-view--interaction-count-label)))
        (mevedel--prompt--settle (cdr (assq 'request overlays)) 'deny)
        (mevedel--prompt--settle (cdr (assq 'ask overlays)) 'aborted)
        (should (member '(request . deny) outcomes))
        (should (member '(ask . aborted) outcomes)))))

  :doc "does not focus interaction prompt while a live window is drafting"
  (mevedel-view-test--with-buffers
    (switch-to-buffer view-buf)
    (delete-other-windows)
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> quoted\nsecond line")
      (goto-char (+ (mevedel-view--input-start) 4))
      ;; Simulate buffer point drifting away from the selected window point.
      ;; Prompt focus must respect the live cursor, not this stale value.
      (save-excursion
        (goto-char (point-min))))
    (with-current-buffer view-buf
      (let ((overlay
             (mevedel-view--interaction-register
              (list :kind 'permission :id 'permission :count 1
                    :body "\npermission\n" :keymap (make-sparse-keymap)
                    :help-echo "Permission" :entry 'permission-entry
                    :activate #'ignore))))
        (should (= (window-point (selected-window))
                   (+ (mevedel-view--input-start) 4)))
        (should (string= "> quoted\nsecond line" (mevedel-view--input-text)))
        (should (overlay-get overlay 'read-only))
        (should (get-text-property (overlay-start overlay) 'read-only))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))))
    (delete-other-windows))

  :doc "redraw regression: interaction update/rebuild preserves multiline > composer and removes stale body"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      ;; Status/task redraw coverage for this composer shape lives in
      ;; `mevedel-tool-task--refresh-display'; this case fills the
      ;; interaction-zone redraw gap before the fragment migration.
      (let* ((draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (old-body "old preview body")
             (current-body "current preview body")
             (map (make-sparse-keymap)))
        (cl-labels
            ((display-text ()
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker))
             (should-preserve-composer ()
               (should (string= draft (mevedel-view--input-text)))
               (should (= (point)
                          (+ (mevedel-view--input-start) point-offset)))
               (should (< (point) (point-max)))
               (should (equal " line"
                              (buffer-substring-no-properties
                               (point)
                               (min (point-max)
                                    (+ (point) (length " line"))))))
               (should-not (get-text-property (mevedel-view--input-start)
                                              'read-only)))
             (should-show-current-body ()
               (let ((display (display-text)))
                 (should (= 1 (mevedel-view-test--count-substring
                               current-body display)))
                 (should (= 0 (mevedel-view-test--count-substring
                               old-body display)))
                 (should (equal "1 preview pending"
                                (mevedel-view--interaction-count-label)))))
             (should-clear-bodies ()
               (let ((display (display-text)))
                 (should (= 0 (mevedel-view-test--count-substring
                               current-body display)))
                 (should (= 0 (mevedel-view-test--count-substring
                               old-body display))))))
          (mevedel-view-test--insert-composer-draft draft point-offset)
          (mevedel-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" old-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (mevedel-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" current-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-clear)
          (should-preserve-composer)
          (should-clear-bodies)))))

  :doc "incremental history render stays above fragment-backed interaction UI"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Read files\n" nil)
    (mevedel-view-test--insert-data data-buf "Working through it.\n" 'response)
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min) t)))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--status-marker t))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                   #'ignore))
          (mevedel-view--render-incremental data-buf))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-pos (string-match-p "Assistant" text))
               (permission-pos (string-match-p "permission" text)))
          (should assistant-pos)
          (should permission-pos)
          (should (< assistant-pos permission-pos)))
        (mevedel-view--interaction-clear)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Working through it" text))
          (should-not (string-match-p "permission" text))))))

  :doc "rerender rebuilds from live queues without settling"
  (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'ask
                       :permission-queue nil
                       :pending-plan-approval nil))
             (plan-outcomes nil)
             (permission-outcomes nil))
        (with-current-buffer data-buf
          (setq-local mevedel--session session))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (setf (mevedel-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :renderer
                            (lambda (entry)
                              (mevedel-view--interaction-register
                               (list :kind 'plan :id 'plan
                                     :entry entry :activate #'ignore
                                     :body "# Plan")))
                            :callback
                            (lambda (outcome)
                              (push outcome plan-outcomes))))
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :specifier-value "/tmp/file.txt"
                            :include-always t
                            :session session
                            :callback
                            (lambda (outcome)
                              (push outcome permission-outcomes)))))
          (mevedel-view--interaction-rebuild)
          (should-not plan-outcomes)
          (should-not permission-outcomes)
          (should (equal "1 plan · 1 permission pending"
                         (mevedel-view--interaction-count-label)))
          (let ((kinds nil))
            (maphash
             (lambda (_id descriptor)
               (push (plist-get descriptor :kind) kinds)
               (should (plist-member descriptor :entry))
               (should (plist-get descriptor :activate)))
             mevedel-view--interaction-descriptors)
            (should (memq 'plan kinds))
            (should (memq 'permission kinds)))))))

  :doc "nested permission stays root-owned and preserves the active composer"
  (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'ask
                       :permission-queue nil
                       :pending-plan-approval nil))
             (agent (mevedel-agent--create :name "verifier"))
             (inv (mevedel-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-perm*"))
             (origin "/root/worker/verifier")
             (draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (outcomes nil))
        (unwind-protect
            (progn
              (setf (mevedel-agent-invocation-agent-id inv)
                    "verifier--0123456789abcdef0123456789abcdef")
              (setf (mevedel-agent-invocation-path inv) origin)
              (setf (mevedel-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (setf (mevedel-agent-invocation-parent-session inv)
                    session)
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (mevedel-request-begin session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (mevedel-view-test--insert-composer-draft
                 draft point-offset))
              (setf (mevedel-session-agent-registry session)
                    (list
                     (cons "/root/worker"
                           (mevedel-agent-record--create
                            :path "/root/worker" :parent-path "/root"
                            :activity 'idle))
                     (cons origin
                           (mevedel-agent-record--create
                            :path origin :parent-path "/root/worker"
                            :activity 'running :invocation inv))))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel--agent-invocation inv)
                (setq-local mevedel--view-buffer view-buf)
                (mevedel-request-begin session))
              (cl-letf (((symbol-function 'gptel-agent--block-bg)
                         (lambda () 'ask)))
                (with-current-buffer agent-buf
                  (mevedel-permission--enqueue
                   (list :kind 'generic
                         :tool-name "Read"
                         :specifier-key :path
                         :specifier-value "/tmp/from-agent.txt"
                         :include-always nil
                         :origin (mevedel-current-origin)
                         :callback
                         (lambda (outcome)
                           (push outcome outcomes)))
                   session)))
              (let* ((entry (car (mevedel-session-permission-queue session)))
                     (interaction-id
                      (mevedel-queue--entry-metadata-get
                       entry :interaction-id)))
                (should interaction-id)
                (should (= 1 (mevedel-agent-control--active-count session)))
                (should (equal origin (plist-get entry :origin)))
                (with-current-buffer view-buf
                  (let ((overlay
                         (gethash interaction-id
                                  mevedel-view--interaction-overlays)))
                    (should overlay)
                    (should
                     (equal origin
                            (overlay-get
                             overlay 'mevedel-view-interaction-origin))))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string-match-p
                           (regexp-quote (format "from %s" origin))
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string= draft (mevedel-view--input-text)))
                  (should (= (point)
                             (+ (mevedel-view--input-start) point-offset)))
                  (should-not mevedel--prompt-overlays))
                (with-current-buffer agent-buf
                  (should-not
                   (string-match-p
                    "Permission Request"
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
                (with-current-buffer data-buf
                  (mevedel-request-end))
                (should-not outcomes)
                (should (= 1 (mevedel-agent-control--active-count session)))
                (should (= 1 (length
                              (mevedel-session-permission-queue session))))
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   mevedel-view--interaction-overlays))
                  (cl-letf (((symbol-function 'gptel-agent--block-bg)
                             (lambda () 'ask)))
                    (mevedel-view--full-rerender))
                  (should-not outcomes)
                  (should (= 1 (length
                                (mevedel-session-permission-queue
                                 session))))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should (string= draft (mevedel-view--input-text)))
                  (should (= (point)
                             (+ (mevedel-view--input-start) point-offset))))
                (with-current-buffer agent-buf
                  (mevedel-request-end))
                (should (equal '(aborted) outcomes))))
          (when (buffer-live-p agent-buf)
            (kill-buffer agent-buf)))))

  :doc "interaction target ignores transcript inspection view"
  (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create :name "test"))
             (agent (mevedel-agent--create :name "verifier"))
             (inv (mevedel-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-data*"))
             (agent-view (generate-new-buffer " *test-agent-view*")))
        (unwind-protect
            (progn
              (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
              (setf (mevedel-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (with-current-buffer data-buf
                (setq-local mevedel--session session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel--agent-invocation inv)
                (setq-local mevedel--view-buffer view-buf))
              (mevedel-view--setup
               agent-view agent-buf
               (list :agent-transcript-p t
                     :agent-id "verifier--abc"
                     :parent-view view-buf
                     :preserve-data-view-buffer t
                     :transcript-info
                     (list :agent-id "verifier--abc"
                           :status 'running
                           :buffer agent-buf
                           :live-buffer t
                           :session session)))
              (with-current-buffer agent-view
                (should (eq view-buf
                            (mevedel-view--interaction-target-buffer
                             agent-buf)))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))

  :doc "all interaction kinds keep priority, callbacks, and a leading-> draft across redraws"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (kinds '(queued-user-message permission ask request plan preview))
             (bodies '((queued-user-message . "queued message body")
                       (permission . "permission body")
                       (ask . "question body")
                       (request . "access body")
                       (plan . "plan body")
                       (preview . "preview body")))
             overlays
             callbacks)
        (mevedel-view-test--insert-composer-draft draft point-offset)
        (dolist (kind kinds)
          (let ((captured-kind kind))
            (push
             (cons kind
                   (mevedel-view--interaction-register
                    (list :kind kind
                          :id kind
                          :count 1
                          :body (concat (alist-get kind bodies) "\n")
                          :keymap (make-sparse-keymap)
                          :entry (intern (format "%s-entry" kind))
                          :activate
                          (lambda (outcome)
                            (push (cons captured-kind outcome) callbacks)))))
             overlays)))
        (dotimes (_ 2)
          (mevedel-view--interaction-render)
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) point-offset))))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (preview (string-search "preview body" text))
               (plan (string-search "plan body" text))
               (request (string-search "access body" text))
               (ask (string-search "question body" text))
               (permission (string-search "permission body" text))
               (queued (string-search "queued message body" text)))
          (should (< preview plan))
          (should (< plan request permission queued))
          (should (< plan ask permission)))
        (should (equal
                 "1 preview · 1 plan · 1 request · 1 question · 1 permission · 1 queued message pending"
                 (mevedel-view--interaction-count-label)))
        (dolist (pair overlays)
          (let* ((kind (car pair))
                 (overlay (cdr pair))
                 (current (gethash kind mevedel-view--interaction-overlays)))
            (should (eq overlay current))
            (should (eq (overlay-get current 'mevedel-view-interaction-entry)
                        (intern (format "%s-entry" kind))))
            (funcall (overlay-get current 'mevedel-view-interaction-activate)
                     'kept)))
        (should (equal kinds (mapcar #'car callbacks)))))))

(provide 'test-mevedel-view-interaction)
;;; test-mevedel-view-interaction.el ends here
