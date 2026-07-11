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



(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
