;;; test-mevedel-view.el -- Tests for mevedel-view -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-stream)
(require 'mevedel-menu)
(require 'mevedel-transcript)
(require 'mevedel-transcript-restore)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-media)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair)
(require 'mevedel-mentions)
(require 'mevedel-skills)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-preview-mode)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-plan)
(require 'mevedel-tool-task)
(require 'mevedel-agents)
(require 'mevedel-agent-runtime)
(require 'mevedel-hooks)
(require 'mevedel-review)
(require 'mevedel-view-zone)
(require 'mevedel-view-history)

(defvar mevedel-plugin-extra-roots)
(defvar org-mode-hook)
(declare-function gptel-menu "ext:gptel-transient" ())
(declare-function org-entry-put "org" (pom property value))


(defun test-mevedel-view--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))


;;
;;; Test helpers

(defun mevedel-view-test--insert-composer-draft (draft &optional point-offset)
  "Insert DRAFT into the editable composer and move point by POINT-OFFSET."
  (let ((start (mevedel-view--input-start))
        (inhibit-read-only t))
    (goto-char start)
    (insert draft)
    (remove-text-properties
     start (point)
     '(read-only nil
       mevedel-view-prompt nil
       font-lock-face nil
       face nil
       front-sticky nil
       rear-nonsticky nil))
    (goto-char (+ start (or point-offset (length draft))))))

(defun mevedel-view-test--insert-repair-audited-tool (data-buf)
  "Insert one completed repair-audited tool call into DATA-BUF."
  (with-current-buffer data-buf
    (let ((start (point)))
      (insert "(:name \"Collect\" :args (:names [\"alice\"]))\n\ncompleted\n")
      (put-text-property start (point) 'gptel '(tool . "repair-call")))
    (let ((start (point)))
      (insert
       (mevedel-tool-repair-format-audit-block
        'committed
        '((:rule wrap-array-singleton :source generic
                :paths ((names)) :before string :after array))))
      (put-text-property start (point) 'gptel 'ignore))))


;;
;;; Rendering

(mevedel-deftest mevedel-view--setup ()
  ,test
  (test)
  :doc "fontifies known dollar skill mentions"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session-create
                     "main"
                     (mevedel-workspace--create
                      :type 'test :id "skills" :root "/tmp/skills"
                      :name "skills")))
           (skill (mevedel-skill--create :name "review" :body "Review")))
      (setf (mevedel-session-skills session) (list skill))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (font-lock-mode 1)
        (goto-char (mevedel-view--input-start))
        (insert "Run $review but keep $PATH literal")
        (font-lock-flush (mevedel-view--input-start) (point-max))
        (font-lock-ensure (mevedel-view--input-start) (point-max))
        (goto-char (mevedel-view--input-start))
        (search-forward "$review")
        (should (memq 'font-lock-keyword-face
                      (ensure-list
                       (get-text-property (match-beginning 0) 'face))))
        (search-forward "$PATH")
        (should-not (get-text-property (match-beginning 0) 'face)))))

  :doc "wires buffers together correctly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq mevedel--data-buffer data-buf))
      (should mevedel-view--input-marker)
      (should (derived-mode-p 'mevedel-view-mode))
      (should-not buffer-read-only))
    (with-current-buffer data-buf
      (should (eq mevedel--view-buffer view-buf))
      (should (eq (local-key-binding (kbd "C-c C-o"))
                  #'mevedel-menu))
      (should-not (eq (local-key-binding (kbd "C-c C-m"))
                      #'mevedel-menu))
      (let ((data-map (current-local-map)))
        (with-temp-buffer
          (org-mode)
          (should-not (eq data-map (current-local-map)))
          (should-not (eq (lookup-key (current-local-map) (kbd "C-c C-o"))
                          #'mevedel-menu))))))

  :doc "view buffers are ephemeral and never offered for saving"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not buffer-auto-save-file-name)
      (goto-char (point-max))
      (insert "draft input")
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not (buffer-modified-p))
      (should-not (memq view-buf (files--buffers-needing-to-be-saved t))))
    (let ((prompted nil))
	      (cl-letf (((symbol-function 'read-file-name)
	                 (lambda (&rest _)
	                   (setq prompted t)
	                   (error "View buffer requested save filename")))
	                ((symbol-function 'y-or-n-p)
	                 (lambda (&rest _)
	                   (setq prompted t)
	                   (error "View buffer requested save confirmation"))))
        (save-some-buffers t (lambda () (eq (current-buffer) view-buf))))
      (should-not prompted))))

  :doc "view buffers stay out of save prompts even if a file name leaks in"
  (mevedel-view-test--with-buffers
    (let ((fake-file (make-temp-file "mevedel-view-leaked-file-")))
      (unwind-protect
          (with-current-buffer view-buf
            (setq buffer-file-name fake-file
                  buffer-file-truename (file-truename fake-file))
            (set-buffer-modified-p t)
            (goto-char (point-max))
            (insert "draft input")
            (should-not buffer-file-name)
            (should-not buffer-file-truename)
            (should-not (buffer-modified-p))
            (should-not (memq view-buf
                              (files--buffers-needing-to-be-saved t))))
        (when (file-exists-p fake-file)
          (delete-file fake-file)))))

(mevedel-deftest mevedel-view--status-strip-button ()
  ,test
  (test)
  :doc "status strip button routes clicks to the requested cockpit area"
  (let ((button (mevedel-view--status-strip-button
                 "Mode" 'mode "Open mode cockpit"))
        called)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (area) (setq called area))))
      (let* ((map (get-text-property 0 'local-map button))
             (command (lookup-key map [header-line mouse-1])))
        (should (eq (get-text-property 0 'mevedel-view-cockpit-area button)
                    'mode))
        (should (string= button "Mode"))
        (should command)
        (funcall command nil)
        (should (eq called 'mode))))))

(mevedel-deftest mevedel-view--status-strip ()
  ,test
  (test)
  :doc "status strip root label truncates to the workspace tail, then disappears"
  (let ((root "~/Projekte/mevedel/"))
    (should (equal root
                   (mevedel-view--status-strip-root-label root 24)))
    (should (equal "…/mevedel/"
                   (mevedel-view--status-strip-root-label root 10)))
    (should (equal ""
                   (mevedel-view--status-strip-root-label root 9))))

  :doc "status strip shows mevedel-owned session orientation instead of the data header"
  (let* ((root (make-temp-file "mevedel-status-root-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project (format "status-%s" root) root "mevedel"))
         (session (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-permission-mode session) 'plan)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local default-directory (file-name-as-directory root))
            (setq-local header-line-format "GPTEL HEADER")
            (setq-local mevedel--session session)
            (setq-local gptel-model 'gpt-5.5)
            (setq-local gptel-tools '(read edit)))
          (with-current-buffer view-buf
            (let ((line (mevedel-view--status-strip)))
              (should (string-prefix-p "main  " line))
              (should (string-match-p
                       (regexp-quote
                        (file-name-nondirectory
                         (directory-file-name root)))
                       line))
              (should (string-match-p
                       (regexp-quote "plan · idle · gpt-5.5 · 2 tools")
                       line))
              (should-not (string-match-p "mevedel:" line))
              (should-not (string-match-p "\\[gpt-5\\.5\\]" line))
              (should-not (string-match-p "\\[2 tools\\]" line))
              (should-not (string-match-p "GPTEL HEADER" line)))))
	  (when (file-directory-p root)
	    (delete-directory root t))))

  :doc "status strip preserves the model none label"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip)))
        (should (string-match-p
                 (regexp-quote "ask · idle · model none · 0 tools")
                 line)))))

  :doc "status strip routes click targets to cockpit surfaces"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip))
            called)
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (area '(top mode model tools))
            (let* ((pos (text-property-any
                         0 (length line)
                         'mevedel-view-cockpit-area area line))
                   (map (and pos (get-text-property pos 'local-map line)))
                   (command (and map
                                 (lookup-key map [header-line mouse-1]))))
              (should pos)
              (should command)
              (setq called nil)
              (funcall command nil)
              (should (eq called area))))))))

  :doc "status strip clicks do not call gptel transients directly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((line (mevedel-view--status-strip))
             (pos (text-property-any
                   0 (length line)
                   'mevedel-view-cockpit-area 'tools line))
             (map (get-text-property pos 'local-map line))
             (command (lookup-key map [header-line mouse-1]))
             (gptel-called nil))
        (cl-letf (((symbol-function 'gptel-menu)
                   (lambda ()
                     (interactive)
                     (setq gptel-called t)))
                  ((symbol-function 'mevedel-menu-open) #'ignore))
          (funcall command nil)
          (should-not gptel-called)))))

  :doc "status strip keeps the raw data buffer header line"
  (let ((data-buf (generate-new-buffer " *status-data*"))
        (view-buf (generate-new-buffer " *status-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local header-line-format "GPTEL HEADER"))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer data-buf
            (should (equal header-line-format "GPTEL HEADER")))
          (with-current-buffer view-buf
            (should (equal header-line-format
                           '(:eval (mevedel-view--status-strip))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--dnd-file-mentions
  (:doc "view drag/drop inserts @file mentions and records exact grants")
  ,test
  (test)
  (let* ((dir (make-temp-file "mevedel dnd-" t))
         (path (expand-file-name "image file.png" dir))
         (data-buf (generate-new-buffer " *test-data-dnd*"))
         (view-buf (generate-new-buffer " *test-view-dnd*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd"
                                        :root dir :name "dnd"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "fake image\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (mevedel-view--insert-dropped-file-mentions (list path))
            (should (equal (format "@file:{%s}" path)
                           (mevedel-view--input-text)))
            (should (equal (list path)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "DND handler accepts the single-URI protocol-handler shape"
  (let* ((dir (make-temp-file "mevedel-dnd-" t))
         (path (expand-file-name "single.txt" dir))
         (uri (concat "file://" path))
         (data-buf (generate-new-buffer " *test-data-dnd-single*"))
         (view-buf (generate-new-buffer " *test-view-dnd-single*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd-single"
                                        :root dir :name "dnd-single"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "single\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (should (eq 'copy
                        (mevedel-view--dnd-handle-files uri 'copy)))
            (should (equal (format "@file:%s" path)
                           (mevedel-view--input-text)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "yank-dwim saves clipboard images to workspace media and inserts @file"
  (let* ((dir (make-temp-file "mevedel-clipboard-" t))
         (data-buf (generate-new-buffer " *test-data-clipboard*"))
         (view-buf (generate-new-buffer " *test-view-clipboard*"))
         (ws (mevedel-workspace--create :type 'project :id "clipboard"
                                        :root dir :name "clipboard"))
         (session (mevedel-session-create "main" ws))
         (expected (file-name-concat
                    dir ".mevedel" "media"
                    "clipboard-20260620-121314.png"))
         (mevedel-view-clipboard-image-handlers
          `(((:command . "fake-clipboard")
             (:save . ,(lambda (file-path)
                         (with-temp-file file-path
                           (set-buffer-multibyte nil)
                           (insert "png"))))))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (cl-letf (((symbol-function 'window-system)
                       (lambda (&optional _frame) 'x))
                      ((symbol-function 'executable-find)
                       (lambda (command)
                         (and (equal command "fake-clipboard")
                              command)))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260620-121314")))
              (mevedel-view-yank-dwim))
            (should (file-exists-p expected))
            (should (equal (format "@file:%s" expected)
                           (mevedel-view--input-text)))
            (should (equal (list expected)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "yank-dwim falls back to text yank when image saving has no session"
  (with-temp-buffer
    (let ((kill-ring '("plain text"))
          (kill-ring-yank-pointer nil)
          (mevedel-view-clipboard-image-handlers
           '(((:command . "fake-clipboard")
              (:save . ignore)))))
      (cl-letf (((symbol-function 'window-system)
                 (lambda (&optional _frame) 'x))
                ((symbol-function 'executable-find)
                 (lambda (command)
                   (and (equal command "fake-clipboard")
                        command))))
        (mevedel-view-yank-dwim))
      (should (equal "plain text" (buffer-string))))))


;;
;;; View lifecycle

(mevedel-deftest mevedel-view--on-view-killed
  (:doc "view kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the view aborts both queues and kills the data buffer"
  (let ((data-buf (generate-new-buffer " *test-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-view-kill-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-view/"
                   :root "/tmp/kill-view/" :name "kill-view")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes)))))
          (kill-buffer view-buf)
          (should-not (buffer-live-p view-buf))
          (should-not (buffer-live-p data-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-plan-queue session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--on-data-killed
  (:doc "data kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the data buffer aborts both queues and kills the view"
  (let ((data-buf (generate-new-buffer " *test-data-kill-data*"))
        (view-buf (generate-new-buffer " *test-view-kill-data*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-data/"
                   :root "/tmp/kill-data/" :name "kill-data")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes)))))
          (kill-buffer data-buf)
          (should-not (buffer-live-p data-buf))
          (should-not (buffer-live-p view-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-plan-queue session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

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


;;
;;; View command wiring

(mevedel-deftest mevedel-view-cycle-permission-mode-keymap
  (:doc "binds backtab spellings to permission mode cycling")
  ,test
  (test)

  :doc "backtab is bound"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "<backtab>"))
              #'mevedel-view-cycle-permission-mode))

  :doc "S-TAB is bound"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "S-TAB"))
              #'mevedel-view-cycle-permission-mode)))

(mevedel-deftest mevedel-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-o"))
              #'mevedel-menu))
  (should-not (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-m"))
                  #'mevedel-menu))
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c RET"))
              #'mevedel-view-send)))


;;; Interaction zone

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
      (let ((raw (test-mevedel-view--raw-bytes
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
                     (list (list :agent-id "verifier--abcdef0123456789abcdef0123456789"
                                 :status 'blocked
                                 :agent-type "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19))))
                  ((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'default)))
          (mevedel-view--render-agent-status))
        (let (agent-pos separator-pos prompt-pos)
          (save-excursion
            (goto-char (point-min))
            (search-forward "Agent: verifier -- Verify tracked diff")
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
                     (list (list :agent-id "verifier--abcdef0123456789abcdef0123456789"
                                 :status 'blocked
                                 :agent-type "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19)))))
          (mevedel-view--render-agent-status))
        (should (string-match-p
                 "Agent: verifier -- Verify tracked diff"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (with-current-buffer data-buf
        (mevedel-permission--enqueue
         (list :kind 'generic
               :tool-name "Read"
               :specifier-key :path
               :specifier-value "/tmp/after-status.txt"
               :include-always nil
               :origin "verifier--abcdef0123456789abcdef0123456789"
               :callback (lambda (outcome) (push outcome outcomes)))
         session))
      (with-current-buffer view-buf
        (should-not outcomes)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (agent-pos (string-search
                           "Agent: verifier -- Verify tracked diff" text))
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
  (let ((mevedel-view-spinner-animate nil)
        (mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let ((session (mevedel-session--create
                      :name "test"
                      :workspace nil
                      :permission-rules nil
                      :permission-mode 'default
                      :permission-queue nil
                      :plan-queue nil))
            outcomes)
        (with-current-buffer data-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--view-buffer view-buf))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (mevedel-view--start-spinner "Working..."))
        (cl-letf (((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'default)))
          (with-current-buffer data-buf
            (dolist (path '("/tmp/one.el" "/tmp/two.el" "/tmp/three.el"))
              (let ((captured-path path))
                (mevedel-permission--enqueue
                 (list :kind 'generic
                       :tool-name "Read"
                       :specifier-key :path
                       :specifier-value captured-path
                       :include-always nil
                       :origin "main"
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
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'default
                       :permission-queue nil
                       :plan-queue nil))
             (plan-outcomes nil)
             (permission-outcomes nil))
        (with-current-buffer data-buf
          (setq-local mevedel--session session))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push outcome plan-outcomes)))))
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
            (should (memq 'permission kinds))))))))

  :doc "agent-owned permission prompt stays in parent view after parent request end"
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'default
                       :permission-queue nil
                       :plan-queue nil))
             (agent (mevedel-agent--create :name "verifier"))
             (inv (mevedel-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-perm*"))
             (outcomes nil))
        (unwind-protect
            (progn
              (setf (mevedel-agent-invocation-agent-id inv)
                    "verifier--0123456789abcdef0123456789abcdef")
              (setf (mevedel-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (setf (mevedel-agent-invocation-parent-session inv)
                    session)
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (mevedel-request-begin session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel--agent-invocation inv)
                (setq-local mevedel--view-buffer view-buf))
              (cl-letf (((symbol-function 'gptel-agent--block-bg)
                         (lambda () 'default)))
                (with-current-buffer agent-buf
                  (mevedel-permission--enqueue
                   (list :kind 'generic
                         :tool-name "Read"
                         :specifier-key :path
                         :specifier-value "/tmp/from-agent.txt"
                         :include-always nil
                         :origin "verifier--0123456789abcdef0123456789abcdef"
                         :callback
                         (lambda (outcome)
                           (push outcome outcomes)))
                   session)))
              (let* ((entry (car (mevedel-session-permission-queue session)))
                     (interaction-id
                      (mevedel-queue--entry-metadata-get
                       entry :interaction-id)))
                (should interaction-id)
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   mevedel-view--interaction-overlays))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
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
                (should (= 1 (length
                              (mevedel-session-permission-queue session))))
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   mevedel-view--interaction-overlays))
                  (cl-letf (((symbol-function 'gptel-agent--block-bg)
                             (lambda () 'default)))
                    (mevedel-view--full-rerender))
                  (should-not outcomes)
                  (should (= 1 (length
                                (mevedel-session-permission-queue
                                 session))))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))))
          (when (buffer-live-p agent-buf)
            (kill-buffer agent-buf))))))

  :doc "interaction target ignores transcript inspection view"
  (let ((mevedel-session-persistence nil))
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
          (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

;;
;;; Expand/collapse

(mevedel-deftest mevedel-tool-ui--render-agent-body
  (:doc "selects the correct Agent expanded body for foreground and background rows")
  ,test
  (test)

  :doc "agent description truncation never exceeds narrow widths"
  (progn
    (should (equal "" (mevedel-tool-ui--compact-agent-description
                       "long task" 0)))
    (should (equal "." (mevedel-tool-ui--compact-agent-description
                       "long task" 1)))
    (should (equal ".." (mevedel-tool-ui--compact-agent-description
                        "long task" 2)))
    (should (equal "..." (mevedel-tool-ui--compact-agent-description
                         "long task" 3)))
    (should (<= (string-width
                 (mevedel-tool-ui--compact-agent-description
                  "long task" 2))
                2)))

  :doc "foreground Agent rows still render the final response"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--fg"
                 :status completed
                 :activity ((:type tool-start :tool-name "Read"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "final response body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-expanded-body rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "final response body" text))
          (should-not (string-match-p "Read" text)))))))

  :doc "Agent rows render SubagentStart hook context as a compact audit note"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--hook"
                 :status running
                 :hook-audits
                 ((:type subagent-context
                   :event "SubagentStart"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "launch body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "hook added sub-agent context" text))
          (should-not (string-match-p "extra start context" text))))))

  :doc "Agent handle transcript click target is the visible type label"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "agent-target"
                       :root temporary-file-directory
                       :name "agent-target"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-agent-target-session")))
           (args '(:subagent_type "explorer" :description "Task"))
           (rd `(:kind agent-transcript
                 :agent-id ,agent-id
                 :status completed))
           rendering)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq rendering (mevedel-tool-ui--render-agent
                         "Agent" args "final response body" rd))
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (setq start (point))
          (mevedel-view--insert-rendered-tool rendering (cons 1 1))
          (mevedel-view--add-display-region-properties
           start (point) 'agent-handle)
          (should
           (string-search
            "Agent: explorer -- Task"
            (buffer-substring-no-properties start (point))))
          (goto-char start)
          (search-forward "Agent: explorer")
          (search-backward "explorer")
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-label-map))
          (should (equal agent-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should
           (lookup-key (get-text-property (point) 'keymap) [mouse-1])))))

  :doc "Agent handle header normalizes long task text to one line"
  (let* ((args '(:subagent_type "coordinator"
                 :description "Run validation.\nThen repeat until green."))
         (rd '(:kind agent-transcript
               :agent-id "coordinator--abcdef123456"
               :status running
               :calls 9))
         (mevedel-tool-ui-agent-description-width 30)
         (rendering (mevedel-tool-ui--render-agent
                     "Agent" args "launch status" rd))
         (header (plist-get rendering :header)))
    (should (string-match-p
             "Agent: coordinator -- Run validation\\. Then repeat\\.\\.\\."
             header))
    (should-not (string-match-p "\n" header))
    (should (string-match-p "\\[running · 9 calls\\]" header))))

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

  :doc "updates an expanded visible agent handle without full rerendering or changing draft"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (draft "> quoted\nsecond line")
           (agent-tool (mevedel-tool--create
                        :name "Agent"
                        :renderer #'mevedel-tool-ui--render-agent))
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
                     (lambda () 'default)))
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

(mevedel-deftest mevedel-view-agent-status-fragment-handles
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



(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
