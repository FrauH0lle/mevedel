;;; test-niwa-view-agent.el --- Agent view tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests retained-agent transcript inspection and registry-backed status.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-agents)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-tool-ui)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-zone)
(require 'niwa-workspace)

(defun niwa-view-agent-test--session ()
  "Return a fresh in-memory session for retained-agent view tests."
  (niwa-session-create
   "main"
   (niwa-workspace--create
    :type 'project
    :id "agent-view"
    :root temporary-file-directory
    :name "agent-view")))

(defun niwa-view-agent-test--record
    (path activity &optional invocation role parent)
  "Return a retained record at PATH with ACTIVITY and optional metadata."
  (niwa-agent-record--create
   :id (concat "storage-" (replace-regexp-in-string "/" "-" path))
   :path path
   :parent-path (or parent "/root")
   :role (or role "default")
   :activity activity
   :invocation invocation))

(defun niwa-view-agent-test--draft-offset ()
  "Return the caret offset from the draft start, read in the composer buffer."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(niwa-deftest niwa-view--render-agent-status
  (:doc "renders agent status and nudges the session's collaboration room")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "status-nudge"))
          rendered notified)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view--render-status)
                   (lambda (&optional _) (setq rendered t)))
                  ((symbol-function
                    'niwa-collaboration-notify-agents-changed)
                   (lambda (seen) (setq notified seen))))
          (niwa-view--render-agent-status)))
      (should rendered)
      (should (eq session notified))))

  :doc "agent inspection and stream refresh preserve parent history, roster and cursor"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((session (niwa-view-agent-test--session))
             (path "/root/inspected")
             (agent-data (generate-new-buffer " *inspected-agent-data*"))
             (record (niwa-view-agent-test--record path 'running))
             agent-view)
        (unwind-protect
            (progn
              (setf (niwa-agent-record-conversation-buffer record) agent-data
                    (niwa-session-agent-registry session) (list (cons path record)))
              (with-current-buffer data-buf (setq-local niwa--session session))
              (with-current-buffer agent-data
                (org-mode)
                (setq-local niwa--view-buffer view-buf)
                (setq-local niwa--session session)
                (insert (propertize "Agent response\n" 'gptel 'response)))
              (niwa-view-test--insert-data data-buf "Parent response\n" 'response)
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view--render-agent-status)
                (niwa-view-stream-begin-turn
                 niwa-view--status-marker
                 (with-current-buffer data-buf (copy-marker (point-min))))
                (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 3)
                (niwa-view-render-live-update data-buf)
                (let ((before (buffer-substring-no-properties (point-min) (point-max))))
                  (niwa-view-open-agent-transcript path)
                  (setq agent-view (window-buffer (selected-window)))
                  (with-current-buffer agent-view
                    (niwa-view-close-agent-transcript))
                  (should (equal before (buffer-substring-no-properties (point-min) (point-max)))))
                (dolist (refresh '(niwa-view--full-rerender
                                   niwa-view--render-agent-status
                                   niwa-view--spinner-tick))
                  (funcall refresh)
                  (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
                  (should (= 3 (niwa-view-agent-test--draft-offset)))
                  (should (string-match-p "Parent response" (buffer-string)))
                  (should (string-match-p "Running   agent    /root/inspected" (buffer-string))))))
          (when (and (buffer-live-p agent-view) (not (eq agent-view view-buf)))
            (kill-buffer agent-view))
          (when (buffer-live-p agent-data) (kill-buffer agent-data)))))))

(niwa-deftest niwa-view-open-agent-transcript ()
  ,test
  (test)
  :doc "a cold transcript renders read-only and closing restores the parent"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((root (make-temp-file "niwa-cold-agent-view-" t))
             (session (niwa-session-create
                       "cold" (niwa-workspace--create
                               :type 'file :id root :root root :name "cold")))
             (path "/root/cold")
             (relative "agents/cold.chat.org")
             (record (niwa-view-agent-test--record path 'idle))
             agent-view)
        (unwind-protect
            (progn
              (make-directory (file-name-concat root "agents") t)
              (write-region "* Agent Task: cold\nRetained response\n" nil
                            (file-name-concat root relative) nil 'silent)
              (setf (niwa-session-authority-mode session) 'portable
                    (niwa-session-save-path session) root
                    (niwa-session-root-buffer session) data-buf
                    (niwa-session-agent-registry session) (list (cons path record))
                    (niwa-agent-record-conversation-location record) relative
                    (niwa-agent-record-configuration record)
                    (niwa-agent-configuration--create :agent (niwa-agent-default)
                                                        :request-locals nil))
              (with-current-buffer data-buf
                (setq-local niwa--session session
                            niwa--workspace (niwa-session-workspace session)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view-test--insert-composer-draft "> draft\nsecond line")
                (let ((before (buffer-string)))
                  ;; The cold transcript is a raw file here, not a published generation.
                  (cl-letf (((symbol-function 'niwa-session-artifacts-artifact-present-p)
                             (lambda (&rest _) t))
                            ((symbol-function 'niwa-session-artifacts-read-artifact)
                             (lambda (_session logical &rest _)
                               (niwa-session-artifacts-read-file-raw
                                (file-name-concat root logical)))))
                    (niwa-view-open-agent-transcript path))
                  (setq agent-view (window-buffer (selected-window)))
                  (with-current-buffer agent-view
                    (should (eq 'prepare (plist-get niwa-view-render--batch :phase)))
                    (cl-loop repeat 100 while niwa-view-render--batch do
                      (niwa-view-render--batch-step
                       agent-view niwa-view-render--batch)))
                  (should (with-current-buffer agent-view
                            (and buffer-read-only
                                 (string-search "Retained response" (buffer-string)))))
                  (with-current-buffer agent-view (niwa-view-close-agent-transcript))
                  (should (equal before (buffer-string)))))
              (should (buffer-live-p (niwa-agent-record-conversation-buffer record)))
              (should-not (buffer-local-value 'buffer-read-only
                                              (niwa-agent-record-conversation-buffer record))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (niwa-agent-control-teardown-session session)
          (delete-directory root t)))))

  :doc "the agent header sits in the status row and the task prompt pins below it"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((session (niwa-view-agent-test--session))
             (path "/root/pinned")
             (agent-data (generate-new-buffer " *pinned-agent-data*"))
             (record (niwa-view-agent-test--record path 'running))
             agent-view)
        (unwind-protect
            (progn
              (setf (niwa-agent-record-conversation-buffer record) agent-data
                    (niwa-session-agent-registry session) (list (cons path record)))
              (with-current-buffer data-buf (setq-local niwa--session session))
              (with-current-buffer agent-data
                (org-mode)
                (setq-local niwa--view-buffer view-buf
                            niwa--session session)
                (insert "Agent Task: pinned review of the header rows\n")
                (insert (propertize (apply #'concat
                                           (make-list 120 "Agent answer line\n"))
                                    'gptel 'response)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view-open-agent-transcript path)
                (setq agent-view (window-buffer (selected-window)))
                (with-current-buffer agent-view
                  (cl-loop repeat 100 while niwa-view-render--batch do
                    (niwa-view-render--batch-step
                     agent-view niwa-view-render--batch))
                  (let ((window (selected-window)))
                    (should (equal '(:eval (niwa-view--sticky-prompt-line))
                                   header-line-format))
                    (should (equal '(:eval (niwa-view--agent-transcript-status-strip))
                                   tab-line-format))
                    ;; Batch `format-mode-line' returns "" without evaluating;
                    ;; the graphical check covers the rendered rows.
                    (should (string-search
                             path (niwa-view--agent-transcript-status-strip)))
                    ;; The task header is on screen: the reserved row stays empty.
                    (set-window-start window (point-min) t)
                    (should-not (niwa-view--sticky-prompt-line))
                    (goto-char (point-min))
                    (search-forward "Agent answer line")
                    (forward-line 40)
                    (set-window-start window (line-beginning-position) t)
                    (set-window-point window (line-beginning-position))
                    (let ((start (window-start window))
                          (point (window-point window))
                          (line (niwa-view--sticky-prompt-line)))
                      (should (string-search
                               "Agent Task: pin" line))
                      (should-not (string-search path line))
                      (redisplay t)
                      (should (= start (window-start window)))
                      (should (= point (window-point window))))))))
          (when (and (buffer-live-p agent-view) (not (eq agent-view view-buf)))
            (kill-buffer agent-view))
          (when (buffer-live-p agent-data) (kill-buffer agent-data)))))))

(niwa-deftest niwa-view--display-agent-transcript-view
  (:doc "same-window and side-window inspection return to the original parent")
  (dolist (action '((display-buffer-same-window)
                    (display-buffer-in-side-window (side . right)
                                                   (window-width . 0.4))))
    (save-window-excursion
      (niwa-view-test--with-buffers
        (let* ((session (niwa-view-agent-test--session))
               (niwa-agent-view-display-action action)
               (first (generate-new-buffer " *agent-first*"))
               (second (generate-new-buffer " *agent-second*"))
               first-view second-view)
          (unwind-protect
              (progn
                (with-current-buffer data-buf (setq-local niwa--session session))
                (dolist (entry `(("/root/first" . ,first)
                                 ("/root/first/second" . ,second)))
                  (let ((record (niwa-view-agent-test--record (car entry) 'running)))
                    (setf (niwa-agent-record-conversation-buffer record) (cdr entry))
                    (push (cons (car entry) record) (niwa-session-agent-registry session)))
                  (with-current-buffer (cdr entry)
                    (org-mode)
                    (setq-local niwa--session session niwa--view-buffer view-buf)
                    (insert (propertize "Agent response\n" 'gptel 'response))))
                (niwa-view-test--insert-data
                 data-buf (apply #'concat (make-list 100 "Parent response\n")) 'response)
                (switch-to-buffer view-buf)
                (niwa-view--full-rerender)
                (niwa-view-test--insert-composer-draft "> keep\nsecond line" 3)
                (set-window-start (selected-window)
                                  (save-excursion (forward-line -10) (point)))
                (should (> (window-start) (point-min)))
                (let ((parent-window (selected-window))
                      (parent-point (point))
                      (parent-start (window-start))
                      (before (buffer-string)))
                  (niwa-view-open-agent-transcript "/root/first")
                  (setq first-view (window-buffer (selected-window)))
                  (should (eq (eq (car action) 'display-buffer-same-window)
                              (eq parent-window (selected-window))))
                  (with-current-buffer first-view
                    (niwa-view-open-agent-transcript "/root/first/second"))
                  (setq second-view (window-buffer (selected-window)))
                  (should-not (buffer-live-p first-view))
                  (with-current-buffer second-view
                    (should (eq view-buf niwa-view--agent-transcript-parent-view))
                    (if (eq (car action) 'display-buffer-same-window)
                        (niwa-view-close-agent-transcript)
                      (let* ((header (niwa-view--agent-transcript-status-strip))
                             (map (get-text-property 0 'local-map header))
                             (window (selected-window)))
                        (should (string-prefix-p "[← Parent]" header))
                        ;; A click must use its event window, not the current
                        ;; buffer, which can still be the parent view.
                        (select-window parent-window)
                        (funcall (lookup-key map [tab-line mouse-1])
                                 (list 'mouse-1 (list window 'tab-line '(0 . 0) 0))))))
                  (should (eq parent-window (selected-window)))
                  (should (eq view-buf (window-buffer parent-window)))
                  (with-current-buffer view-buf
                    (should (= parent-point (point)))
                    (should (= parent-start (window-start parent-window)))
                    (should (equal before (buffer-string)))
                    (should (equal "> keep\nsecond line" (niwa-view-composer-draft-text))))))
            (dolist (buffer (list first-view second-view first second))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(niwa-deftest niwa-view--kill-agent-transcript-view
  (:doc "returning from a side window leaves the dock's row and caret intact")
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((session (niwa-view-agent-test--session))
             (niwa-agent-view-display-action
              '(display-buffer-in-side-window (side . right) (window-width . 0.4)))
             (agent (generate-new-buffer " *agent-return*"))
             agent-view)
        (unwind-protect
            (progn
              (with-current-buffer data-buf (setq-local niwa--session session))
              (let ((record (niwa-view-agent-test--record "/root/agent" 'running)))
                (setf (niwa-agent-record-conversation-buffer record) agent)
                (push (cons "/root/agent" record)
                      (niwa-session-agent-registry session)))
              (with-current-buffer agent
                (org-mode)
                (setq-local niwa--session session niwa--view-buffer view-buf)
                (insert (propertize "Agent response\n" 'gptel 'response)))
              ;; Paragraphs that fit the full width but wrap beside the side window.
              (niwa-view-test--insert-data
               data-buf
               (apply #'concat
                      (make-list 60 (concat (make-string 30 ?a) " "
                                            (make-string 30 ?b) "\n\n")))
               'response)
              (switch-to-buffer view-buf)
              (delete-other-windows)
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft "> keep\nsecond line" 3)
              (let* ((parent-window (selected-window))
                     (dock (niwa-view-composer-display parent-window))
                     (y (niwa-view-test--composer-screen-y dock)))
                (should dock)
                (should y)
                (niwa-view-open-agent-transcript "/root/agent")
                (setq agent-view (window-buffer (selected-window)))
                (should-not (eq parent-window (selected-window)))
                (with-current-buffer agent-view
                  (niwa-view-close-agent-transcript))
                (should (eq parent-window (selected-window)))
                (should (= (window-total-width parent-window) (frame-width)))
                (with-current-buffer view-buf
                  (should (eq dock (niwa-view-composer-window parent-window)))
                  (should (= y (niwa-view-test--composer-screen-y dock)))
                  (should (= 3 (niwa-view-agent-test--draft-offset)))
                  (should (equal "> keep\nsecond line"
                                 (niwa-view-composer-draft-text))))))
          (dolist (buffer (list agent-view agent))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(niwa-deftest niwa-view--kill-agent-transcript-view/history-navigation
  (:doc "closing keeps a parent window the user moved into history")
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((session (niwa-view-agent-test--session))
             (niwa-agent-view-display-action
              '(display-buffer-in-side-window (side . right) (window-width . 0.4)))
             (agent (generate-new-buffer " *agent-history*"))
             agent-view)
        (unwind-protect
            (progn
              (with-current-buffer data-buf (setq-local niwa--session session))
              (let ((record (niwa-view-agent-test--record "/root/agent" 'running)))
                (setf (niwa-agent-record-conversation-buffer record) agent)
                (push (cons "/root/agent" record)
                      (niwa-session-agent-registry session)))
              (with-current-buffer agent
                (org-mode)
                (setq-local niwa--session session niwa--view-buffer view-buf)
                (insert (propertize "Agent response\n" 'gptel 'response)))
              (niwa-view-test--insert-data
               data-buf
               (apply #'concat (make-list 100 "Parent response\n"))
               'response)
              (switch-to-buffer view-buf)
              (delete-other-windows)
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft "> keep\nsecond line" 3)
              (goto-char (point-max))
              (recenter -1)
              (let* ((parent-window (selected-window))
                     (dock (niwa-view-composer-display parent-window)))
                (should dock)
                (should (niwa-view-test--composer-screen-y dock))
                (niwa-view-open-agent-transcript "/root/agent")
                (setq agent-view (window-buffer (selected-window)))
                (should-not (eq parent-window (selected-window)))
                ;; Read earlier history in the parent with real commands.
                (with-selected-window parent-window
                  (call-interactively #'beginning-of-buffer)
                  (call-interactively #'forward-line)
                  (call-interactively #'scroll-up-command)
                  (call-interactively #'scroll-down-command)
                  (redisplay t))
                (let ((start (window-start parent-window))
                      (point (window-point parent-window)))
                  (should (< point (with-current-buffer view-buf
                                     (niwa-view-transcript-tail))))
                  (with-current-buffer agent-view
                    (niwa-view-close-agent-transcript))
                  (should (eq parent-window (selected-window)))
                  (with-current-buffer view-buf
                    (should (= start (window-start parent-window)))
                    (should (= point (window-point parent-window)))
                    (should (equal "> keep\nsecond line"
                                   (niwa-view-composer-draft-text)))))))
          (dolist (buffer (list agent-view agent))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(niwa-deftest niwa-view--kill-agent-transcript-view/offscreen-composer
  (:doc "closing preserves a scrolled dock and the independent history start")
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((session (niwa-view-agent-test--session))
             (niwa-agent-view-display-action
              '(display-buffer-in-side-window (side . right) (window-width . 0.4)))
             (agent (generate-new-buffer " *agent-offscreen*"))
             agent-view)
        (unwind-protect
            (progn
              (with-current-buffer data-buf (setq-local niwa--session session))
              (let ((record (niwa-view-agent-test--record "/root/agent" 'running)))
                (setf (niwa-agent-record-conversation-buffer record) agent)
                (push (cons "/root/agent" record)
                      (niwa-session-agent-registry session)))
              (with-current-buffer agent
                (org-mode)
                (setq-local niwa--session session niwa--view-buffer view-buf)
                (insert (propertize "Agent response\n" 'gptel 'response)))
              (niwa-view-test--insert-data
               data-buf
               (apply #'concat (make-list 100 "Parent response\n"))
               'response)
              (switch-to-buffer view-buf)
              (delete-other-windows)
              (niwa-view--full-rerender)
              (niwa-view-test--insert-composer-draft "> keep\nsecond line")
              (let* ((parent-window (selected-window))
                     (dock (niwa-view-composer-display parent-window)))
                (should dock)
                (niwa-view-open-agent-transcript "/root/agent")
                (setq agent-view (window-buffer (selected-window)))
                (with-selected-window parent-window
                  (goto-char (point-min))
                  (set-window-start parent-window (point-min)))
                (with-current-buffer (window-buffer dock)
                  (set-window-start dock (line-beginning-position)))
                (let ((start (window-start dock)))
                  (should-not (niwa-view-test--composer-screen-y dock))
                  (with-current-buffer agent-view
                    (niwa-view-close-agent-transcript))
                  (should (= start (window-start dock)))
                  (should-not (niwa-view-test--composer-screen-y dock)))
                (with-current-buffer view-buf
                  (should (= (point-min) (window-start parent-window)))
                  (should (equal "> keep\nsecond line"
                                 (niwa-view-composer-draft-text))))))
          (dolist (buffer (list agent-view agent))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(niwa-deftest niwa-view-agent-model-label
  (:doc "formats only known model metadata without inventing effort")
  (progn
    (should (equal "opus:high"
                   (niwa-view-agent-model-label
                    '(:backend "Claude Code" :model "opus" :effort "high"))))
    (should (equal "Claude Code / opus:high"
                   (niwa-view-agent-model-label
                    '(:backend "Claude Code" :model "opus" :effort "high") t)))
    (should (equal "mistral"
                   (niwa-view-agent-model-label '(:model "mistral" :effort nil))))
    (should-not (niwa-view-agent-model-label nil))
    (should-not (niwa-view-agent-model-label '(:backend "provider")))
    (should (equal "provider / model name:high"
                   (niwa-view-agent-model-label
                    '(:backend "provider" :model "model\nname" :effort "high") t)))))

(niwa-deftest niwa-view-agent-model-details
  (:doc "lists known metadata in disclosure without placeholder values")
  (progn
    (should (equal "Provider  Claude Code\nModel     opus\nEffort    high"
                   (niwa-view-agent-model-details
                    '(:backend "Claude Code" :model "opus" :effort "high"))))
    (should (equal "Model     mistral"
                   (niwa-view-agent-model-details '(:model "mistral"))))
    (should-not (niwa-view-agent-model-details nil))))

(niwa-deftest niwa-view--agent-transcript-status-strip
  ()
  ,test
  (test)

  :doc "shows current retained model and known effort without duplicate metrics"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (path "/root/partner")
           (record (niwa-view-agent-test--record path 'running))
           (metadata '(:backend "Claude Code" :model "opus" :effort "high")))
      (setf (niwa-agent-record-model-metadata record) metadata
            (niwa-session-agent-registry session) (list (cons path record)))
      (with-temp-buffer
        (setq-local niwa-view--agent-path path
                    niwa-view--agent-transcript-parent-view view-buf
                    niwa-view--agent-transcript-info
                    (list :session session :status 'running :calls 12 :elapsed 84))
        (let ((header (niwa-view--agent-transcript-status-strip)))
          (should (string-prefix-p "[← Parent] · /root/partner · [running]" header))
          (should (string-search "Claude Code / opus:high" header))
          (should (= 2 (length (split-string header "12 calls"))))
          (should (string-search "12 calls" header)))
        (setf (niwa-agent-record-model-metadata record)
              '(:backend "Claude Code" :model "sonnet" :effort nil))
        (let ((header (niwa-view--agent-transcript-status-strip)))
          (should (string-search "Claude Code / sonnet" header))
          (should-not (string-search "opus" header))
          (should-not (string-search "sonnet:" header)))
        (setf (niwa-agent-record-model-metadata record) nil)
        (should-not (string-search "Claude Code" (niwa-view--agent-transcript-status-strip))))))

  :doc "narrow windows keep Parent, leaf identity and state before model metadata"
  (save-window-excursion
    (delete-other-windows)
    (split-window-right 40)
    (let ((parent (current-buffer)))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (setq-local niwa-view--agent-transcript-parent-view parent
                    niwa-view--agent-path "/root/long_ancestor/another_ancestor/worker"
                    niwa-view--agent-transcript-info
                    '(:status running :calls 12 :elapsed 84
                      :model-metadata (:backend "Claude Code" :model "opus" :effort "high")))
        (let ((header (niwa-view--agent-transcript-status-strip)))
          (should (<= (string-width header) (window-body-width)))
          (should (string-prefix-p "[← Parent]" header))
          (should (string-search "worker" header))
          (should (string-search "[running]" header))
          (should-not (string-search "opus" header)))
        (window-resize (selected-window) 20 t)
        (setq niwa-view--agent-path "/root/worker")
        (let ((header (niwa-view--agent-transcript-status-strip)))
          (should (<= (string-width header) (window-body-width)))
          (should (string-search "opus:high" header))
          (should-not (string-search "Claude Code" header))))))

  :doc "truncated paths keep full identity and model details on hover"
  (save-window-excursion
    (delete-other-windows)
    (split-window-right 40)
    (let ((parent (current-buffer))
          (path "/root/long_ancestor/another_ancestor/worker"))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (setq-local niwa-view--agent-transcript-parent-view parent
                    niwa-view--agent-path path
                    niwa-view--agent-transcript-info
                    '(:status running :session-label "session-7"
                      :model-metadata (:backend "Claude Code" :model "opus" :effort "high")))
        (let* ((header (niwa-view--agent-transcript-status-strip))
               (pos (string-search "worker" header))
               (help (get-text-property pos 'help-echo header)))
          (should-not (string-search path header))
          (should (equal (concat path "\nSession session-7\n"
                                 "Provider  Claude Code\nModel     opus\nEffort    high")
                         (if (functionp help)
                             (funcall help (selected-window) header pos)
                           help))))))))

(niwa-deftest niwa-view--render-agent-status
  (:doc "roster metadata refresh preserves a multiline leading-> composer draft")
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (record (niwa-view-agent-test--record "/root/partner" 'running)))
      (setf (niwa-session-agent-registry session) (list (cons "/root/partner" record))
            (niwa-agent-record-model-metadata record)
            '(:backend "Claude Code" :model "opus" :effort "high"))
      (with-current-buffer data-buf (setq-local niwa--session session))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> keep this\nand this" 4)
        (niwa-view--render-agent-status)
        (should (string-search "Running   agent    /root/partner · opus:high" (buffer-string)))
        (setf (niwa-agent-record-model-metadata record)
              '(:backend "Claude Code" :model "sonnet"))
        (niwa-view--render-agent-status)
        (should (string-search "Running   agent    /root/partner · sonnet" (buffer-string)))
        (should-not (string-search "opus:high" (buffer-string)))
        (should (equal "> keep this\nand this" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-agent-test--draft-offset)))
        (setq niwa-view-execution--selected-agent "/root/partner")
        (niwa-view--render-agent-status)
        (should (string-search "    Provider  Claude Code" (buffer-string)))
        (niwa-view-zone-set-collapse-state niwa-view-execution--collapse-key t)
        (niwa-view--render-agent-status)
        (should (string-search "background work · 1 agent [+]" (buffer-string)))
        (should-not (string-search "sonnet" (buffer-string)))
        (should (equal "> keep this\nand this" (niwa-view-composer-draft-text)))))))

(niwa-deftest niwa-view-agent--handle-badge
  (:doc "maps :status + :calls/:elapsed/:reason to a state badge string")
  ,test
  (test)

  :doc "running with N calls renders [running · N calls]"
  (should (string-match-p
           "running.*3 calls"
           (niwa-view-agent--handle-badge
            '(:status running :calls 3))))

  :doc "running with zero calls suppresses the count suffix"
  (let ((badge (niwa-view-agent--handle-badge
                '(:status running :calls 0))))
    (should (string-match-p "running" badge))
    (should-not (string-match-p "calls" badge)))

  :doc "blocked reason overrides running badge"
  (let ((badge (niwa-view-agent--handle-badge
                '(:status running :calls 2 :blocked-reason "permission"))))
    (should (string-match-p "blocked" badge))
    (should (string-match-p "permission" badge))
    (should-not (string-match-p "running" badge)))

  :doc "completed renders ✓ done with elapsed and calls"
  (let ((badge (niwa-view-agent--handle-badge
                '(:status completed :calls 5 :elapsed 2.3))))
    (should (string-match-p "done" badge))
    (should (string-match-p "· 2s ·" badge))
    (should (string-match-p "5 calls" badge)))

  :doc "completed without elapsed/calls renders just ✓ done"
  (should (string-match-p
           "✓ done"
           (niwa-view-agent--handle-badge '(:status completed))))

  :doc "error renders ✗ error · REASON"
  (should (string-match-p
           "error.*max-turns"
           (niwa-view-agent--handle-badge
            '(:status error :reason "max-turns"))))

  :doc "aborted renders ✗ aborted"
  (should (string-match-p
           "✗ aborted"
           (niwa-view-agent--handle-badge '(:status aborted))))

  :doc "incomplete renders ○ incomplete"
  (should (string-match-p
           "○ incomplete"
           (niwa-view-agent--handle-badge '(:status incomplete))))

  :doc "unknown status returns empty string"
  (should (equal ""
                 (niwa-view-agent--handle-badge '(:status banana)))))

(niwa-deftest niwa-view-agent--blocked-reason
  (:doc "reports a permission queue blocking a canonical path")
  ,test
  (test)

  :doc "reports a pending permission"
  (let ((session (niwa-view-agent-test--session)))
    (setf (niwa-session-permission-queue session)
          '((:origin "/root/worker")))
    (should
     (equal "permission"
            (niwa-view-agent--blocked-reason
             "/root/worker" session))))

  :doc "ignores unrelated paths"
  (let ((session (niwa-view-agent-test--session)))
    (setf (niwa-session-permission-queue session)
          '((:origin "/root/worker")))
    (should-not
     (niwa-view-agent--blocked-reason "/root/other" session))))

(niwa-deftest niwa-view-agent-ownership ()
  ,test
  (test)
  (dolist (symbol '(niwa-view-open-agent-transcript
                    niwa-view--agent-status-collect
                    niwa-view-refresh-agent-rendering))
    (should (equal "niwa-view-agent"
                   (file-name-base (or (symbol-file symbol 'defun) ""))))))

(niwa-deftest niwa-view-agent-handle-activate ()
  ,test
  (test)
  :doc "agent handles with missing saved transcripts report unavailable"
  (niwa-view-test--with-buffers
    (let* ((agent-path "/root/missing")
           (workspace (niwa-workspace--create
                       :type 'project
                       :id "missing-transcript"
                       :root temporary-file-directory
                       :name "missing-transcript"))
           (session (niwa-session-create "main" workspace))
           opened
           message-text)
      (setf (niwa-session-agent-transcripts session)
            (list (cons "storage-missing"
                        `(:agent-path ,agent-path :status completed))))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "(:name \"Agent\" :args (:task_name \"explore\" :message \"Inspect.\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering
                (plist-put
                 (niwa-tool-ui--render-agent
                  "Agent"
                  '(:task_name "explore"
                    :message "Missing transcript task")
                  "rendered missing transcript body\n"
                  `(:kind collaboration-event
                    :event started
                    :path ,agent-path
                    :status completed))
                 :agent-path agent-path)))
          (let ((inhibit-read-only t))
            (goto-char (niwa-view-transcript-tail-marker))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
            (unwind-protect
                (niwa-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
          (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'niwa-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Transcript file missing")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Started /root/missing")
            (goto-char (match-beginning 0))
            (niwa-view-agent-handle-activate agent-path)
            (let ((text (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail))))
              (should opened)
              (should (equal (format "niwa [%s]: Transcript file missing"
                                     temporary-file-directory)
                             message-text))
              (should-not (string-match-p
                           "rendered missing transcript body" text))
              (should-not (string-match-p "raw launch payload" text))))))))

  :doc "agent handles with stale live invocations report unavailable"
  (niwa-view-test--with-buffers
    (let* ((agent-path "/root/stale")
           (workspace (niwa-workspace--create
                       :type 'project
                       :id "stale-live-transcript"
                       :root temporary-file-directory
                       :name "stale-live-transcript"))
           (session (niwa-session-create "main" workspace))
           (inv (niwa-agent-invocation--create
                 :path agent-path
                 :transcript-status 'running))
           opened
           message-text)
      (setf (niwa-session-agent-transcripts session)
            (list (cons "storage-stale"
                        `(:agent-path ,agent-path :status running))))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "(:name \"Agent\" :args (:task_name \"explore\" :message \"Inspect.\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering
                (plist-put
                 (niwa-tool-ui--render-agent
                  "Agent"
                  '(:task_name "explore"
                    :message "Stale live task")
                  "rendered stale live body\n"
                  `(:kind collaboration-event
                    :event started
                    :path ,agent-path
                    :status running))
                 :agent-path agent-path)))
          (let ((inhibit-read-only t))
            (goto-char (niwa-view-transcript-tail-marker))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
            (unwind-protect
                (niwa-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type (niwa-view-transcript-tail-marker) nil)))
          (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'niwa-view--agent-invocation)
                     (lambda (_id) inv))
                    ((symbol-function 'niwa-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Live buffer unavailable")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Started /root/stale")
            (goto-char (match-beginning 0))
            (niwa-view-agent-handle-activate agent-path)
            (let ((text (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail))))
              (should opened)
              (should (equal (format "niwa [%s]: Live buffer unavailable"
                                     temporary-file-directory)
                             message-text))
              (should-not (string-match-p "rendered stale live body" text))
              (should-not (string-match-p "raw launch payload" text)))))))))

(niwa-deftest niwa-view-agent-initialize
  (:doc "initializes canonical transcript inspection state")
  ,test
  (test)

  :doc "transcript views retain their canonical path and inspection-only keys"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (use-local-map niwa-view-mode-map)
          (let ((info '(:agent-path "/root/spec_review" :status running))
                (parent (current-buffer)))
            (niwa-view-agent-initialize
             (list :agent-transcript-p t
                   :agent-path "/root/spec_review"
                   :transcript-info info
                   :parent-view parent
                   :preserve-data-view-buffer t)
             data-buffer)
            (should niwa-view--agent-transcript-p)
            (should (equal "/root/spec_review" niwa-view--agent-path))
            (should (equal info niwa-view--agent-transcript-info))
            (should (eq parent niwa-view--agent-transcript-parent-view))
            (should (eq #'niwa-view-close-agent-transcript
                        (lookup-key (current-local-map) (kbd "q"))))
            (should (eq #'niwa-view--transcript-gptel-send-blocked
                        (lookup-key (current-local-map) [remap gptel-send])))
            (should (equal '(:eval (niwa-view--agent-transcript-status-strip))
                           tab-line-format))
            (with-current-buffer data-buffer
              (should (memq #'niwa-view--on-agent-transcript-data-killed
                            kill-buffer-hook)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "ordinary views leave their map and status row untouched"
  (let ((data-buffer (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((map (make-sparse-keymap)))
            (use-local-map map)
            (setq-local tab-line-format 'keep)
            (niwa-view-agent-initialize nil data-buffer)
            (should-not niwa-view--agent-transcript-p)
            (should (eq map (current-local-map)))
            (should (eq 'keep tab-line-format))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(niwa-deftest niwa-view--agent-transcript-start-streaming
  (:doc "anchors live rendering in agent data without duplicating history")
  ,test
  (test)
  (let ((data-buffer (generate-new-buffer " *test-agent-stream-data*"))
        (view-buffer (generate-new-buffer " *test-agent-stream-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create
                         :transcript-status 'running))
            (insert "Original prompt\n"
                    "<hook-context>\n"
                    (make-string 2000 ?x)
                    "\n</hook-context>\n")
            (let ((start (point)))
              (insert "Assistant response\n")
              (put-text-property start (point) 'gptel 'response)))
          (niwa-view--setup
           view-buffer data-buffer
           '(:agent-transcript-p t
             :agent-path "/root/streaming"
             :preserve-data-view-buffer t
             :transcript-info (:live-buffer t :status running)))
          (with-current-buffer view-buffer
            (niwa-view--full-rerender)
            (niwa-view--agent-transcript-start-streaming)
            (should (eq data-buffer
                        (marker-buffer niwa-view--data-turn-start)))
            (niwa-view-render-live-update data-buffer)
            (should (= 1
                       (niwa-view-test--count-substring
                        "Original prompt" (buffer-string))))))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(niwa-deftest niwa-view-agent-live-transcript-start
  (:doc "turns an open idle retained view into a live stream projection")
  ,test
  (test)
  (let ((parent-data (generate-new-buffer " *test-agent-start-parent-data*"))
        (parent-view (generate-new-buffer " *test-agent-start-parent-view*"))
        (data-buffer (generate-new-buffer " *test-agent-start-data*"))
        (view-buffer (generate-new-buffer " *test-agent-start-view*"))
        invocation
        scheduled)
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode))
          (niwa-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (niwa-view-test--insert-composer-draft "> draft\ncontinues"))
          (with-current-buffer data-buffer
            (org-mode)
            (setq invocation
                  (niwa-agent-invocation--create
                   :buffer data-buffer
                   :transcript-status 'running
                   :call-count 2))
            (setq-local niwa--agent-invocation invocation)
            (insert "Follow-up prompt\n"))
          (niwa-view--setup
           view-buffer data-buffer
           '(:agent-transcript-p t
             :agent-path "/root/streaming"
             :parent-view parent-view
             :preserve-data-view-buffer t
             :transcript-info (:live-buffer nil :status completed)))
          (with-current-buffer view-buffer
            (niwa-view--full-rerender))
          (niwa-view-agent-live-transcript-start invocation)
          (with-current-buffer view-buffer
            (should (plist-get niwa-view--agent-transcript-info
                               :live-buffer))
            (should (eq 'running
                        (plist-get
                         (niwa-view--agent-transcript-current-info)
                         :status)))
            (should (niwa-view-stream-in-flight-turn-start-position))
            (should (eq data-buffer
                        (marker-buffer niwa-view--data-turn-start))))
          (cl-letf (((symbol-function 'niwa-view-stream-schedule)
                     (lambda () (setq scheduled t))))
            (with-current-buffer data-buffer
              (niwa-view-agent-live-transcript-stream)))
          (should scheduled)
          (with-current-buffer parent-view
            (should (equal "> draft\ncontinues"
                           (niwa-view-composer-draft-text nil t)))))
      (dolist (buffer (list parent-data parent-view data-buffer view-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(niwa-deftest niwa-view-agent-live-transcript-finalize ()
  ,test
  (test)
  :doc "successive turns replace visible status without mutating earlier snapshots"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Agent answer\n" 'response)
    (let* ((original (list :agent-path "/root/worker" :live-buffer t
                           :status 'running :session-label "parent"))
           (before (copy-sequence original))
           (invocation (niwa-agent-invocation--create :buffer data-buf))
           previous)
      (with-current-buffer view-buf
        (setq niwa-view--agent-transcript-p t
              niwa-view--agent-transcript-info original))
      (dolist (status '(error completed aborted))
        (setf (niwa-agent-invocation-transcript-status invocation) 'running)
        (niwa-view-agent-live-transcript-start invocation)
        (setf (niwa-agent-invocation-transcript-status invocation) status
              (niwa-agent-invocation-call-count invocation) 3
              (niwa-agent-invocation-terminal-reason invocation)
              (and (eq status 'error) "failed"))
        (should (equal (list view-buf)
                       (niwa-view-agent-live-transcript-finalize invocation)))
        (with-current-buffer view-buf
          (let ((info (niwa-view--agent-transcript-current-info)))
            (should (eq status (plist-get info :status)))
            (should (= 3 (plist-get info :calls)))
            (should-not (plist-get info :elapsed))
            (should (equal (and (eq status 'error) "failed")
                           (plist-get info :reason)))
            (should (equal "parent" (plist-get info :session-label)))
            (should-not (plist-get info :live-buffer)))
          (should-not niwa-view--data-turn-start)
          (should-not niwa-view--in-flight-turn-start)
          (when previous
            (should (equal (car previous) (cdr previous)))
            (should (= (length (car previous))
                       (length niwa-view--agent-transcript-info))))
          (setq previous
                (cons niwa-view--agent-transcript-info
                      (copy-sequence niwa-view--agent-transcript-info)))))
      (should (equal before original)))))

(niwa-deftest niwa-view-agent-cleanup-parent
  (:doc "cleans up saved transcript views without tearing down the session")
  ,test
  (test)
  (let* ((session (niwa-view-agent-test--session))
         (parent-view (generate-new-buffer " *test-agent-parent-view*"))
         (data-buffer (generate-new-buffer " *test-agent-saved-data*"))
         (view-buffer (generate-new-buffer " *test-agent-saved-view*"))
         (teardown-count 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup
           view-buffer data-buffer
           (list :agent-transcript-p t
                 :parent-view parent-view))
          (cl-letf (((symbol-function
                      'niwa-agent-control-teardown-session)
                     (lambda (_session)
                       (cl-incf teardown-count))))
            (niwa-view-agent-cleanup-parent parent-view))
          (should (= 0 teardown-count))
          (should-not (buffer-live-p view-buffer))
          (should-not (buffer-live-p data-buffer)))
      (dolist (buffer (list view-buffer data-buffer parent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(niwa-deftest niwa-view--agent-record
  (:doc "resolves retained identities by canonical path")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (record (niwa-view-agent-test--record
                    "/root/spec_review" 'idle)))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/spec_review" record)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (should (eq record
                    (niwa-view--agent-record "/root/spec_review")))
        (should-not (niwa-view--agent-record "/root/missing"))))))

(niwa-deftest niwa-view--lookup-transcript-pair
  (:doc "joins canonical paths to opaque transcript storage identities")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (record (niwa-view-agent-test--record
                    "/root/spec_review" 'idle))
           (storage-id (niwa-agent-record-id record))
           (pair (cons storage-id '(:path "agents/spec.chat.org"
                                    :status completed))))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/spec_review" record)))
      (setf (niwa-session-agent-transcripts session) (list pair))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (should (eq pair
                    (niwa-view--lookup-transcript-pair
                     "/root/spec_review")))
        (should-not (niwa-view--lookup-transcript-pair storage-id)))))

  :doc "historical fork artifacts resolve by recorded canonical path"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (pair (cons "storage-historical"
                       '(:agent-path "/root/historical"
                         :path "agents/historical.chat.org"
                         :status completed))))
      (setf (niwa-session-agent-transcripts session) (list pair))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (should (eq pair
                    (niwa-view--lookup-transcript-pair
                     "/root/historical")))))))

(niwa-deftest niwa-view--resolve-agent-transcript
  (:doc "uses publication membership instead of a remote fixed cache")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((tempdir (make-temp-file "niwa-agent-view-resolve-" t))
           (session (niwa-view-agent-test--session))
           (agent-path "/root/historical")
           (relative "agents/historical.chat.org")
           (fixed (expand-file-name relative tempdir)))
      (unwind-protect
          (progn
            (setf (niwa-session-save-path session)
                  (file-name-as-directory tempdir)
                  (niwa-session-agent-transcripts session)
                  (list (cons "storage-historical"
                              `(:agent-path ,agent-path :path ,relative
                                :status completed))))
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (cl-letf (((symbol-function
                          'niwa-session-artifacts-artifact-present-p)
                         (lambda (seen-session logical)
                           (and (eq seen-session session)
                                (equal logical relative)))))
                (let ((info
                       (niwa-view--resolve-agent-transcript agent-path)))
                  (should (equal relative
                                 (plist-get info :relative-path)))
                  (should (equal fixed (plist-get info :logical-path)))
                  (should-not (plist-member info :absolute-path))))
              (make-directory (file-name-directory fixed) t)
              (write-region "poisoned cache" nil fixed nil 'silent)
              (cl-letf (((symbol-function
                          'niwa-session-artifacts-artifact-present-p)
                         (lambda (&rest _) nil)))
                (should-error
                 (niwa-view--resolve-agent-transcript agent-path)
                 :type 'user-error))))
        (delete-directory tempdir t)))))

(niwa-deftest niwa-view--ensure-agent-transcript-view
  (:doc "hydrates saved transcript inspection through verified artifacts")
  ,test
  (test)
  (let* ((session (niwa-session--create
                   :name "main" :session-id "session-historical"))
         (relative "agents/historical.chat.org")
         (data-buffer (generate-new-buffer " *agent-inspection-data*"))
         (view-buffer (generate-new-buffer " *agent-inspection-view*"))
         (parent-view (generate-new-buffer " *agent-inspection-parent*"))
         seen scheduled)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-session-artifacts-find-artifact-noselect)
                   (lambda (seen-session logical &optional inspection)
                     (setq seen (list seen-session logical inspection))
                     (with-current-buffer data-buffer
                       (setq buffer-file-name
                             "/tmp/session/agents/historical.chat.org")
                       (insert "published transcript"))
                     data-buffer))
                  ((symbol-function 'niwa-view--ensure)
                   (lambda (&rest _) view-buffer))
                  ((symbol-function 'niwa-view-render-batched-full)
                   (lambda () (setq scheduled t)))
                  ((symbol-function
                    'niwa-transcript-restore-properties)
                   #'ignore))
          (should
           (eq view-buffer
               (niwa-view--ensure-agent-transcript-view
                "/root/historical"
                (list :session session :relative-path relative)
                parent-view)))
          (should (equal (list session relative t) seen))
          (should scheduled)
          (with-current-buffer data-buffer
            (should (equal "published transcript" (buffer-string)))
            (should buffer-read-only)))
      (dolist (buffer (list data-buffer view-buffer parent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))
  :doc "leaves an idle retained conversation buffer writable"
  (let* ((session (niwa-session--create
                   :name "main" :session-id "session-retained"))
         (data-buffer (generate-new-buffer " *agent-retained-data*"))
         (view-buffer (generate-new-buffer " *agent-retained-view*"))
         (parent-view (generate-new-buffer " *agent-retained-parent*"))
         restored
         preserve-data-view-buffer)
    (unwind-protect
        (progn
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/verifier"
                            (niwa-agent-record--create
                             :conversation-buffer data-buffer))))
          (with-current-buffer data-buffer
            (org-mode)
            (setq-local niwa--view-buffer parent-view))
          (cl-letf (((symbol-function 'niwa-view--ensure)
                     (lambda (_data _name options)
                       (setq preserve-data-view-buffer
                             (plist-get options :preserve-data-view-buffer))
                       view-buffer))
                    ((symbol-function 'niwa-view-render-batched-full) #'ignore)
                    ((symbol-function 'niwa-transcript-restore-properties)
                     (lambda () (setq restored t))))
            (niwa-view--ensure-agent-transcript-view
             "/root/verifier"
             (list :session session :buffer data-buffer :live-buffer nil)
             parent-view))
          (with-current-buffer data-buffer
            (should-not buffer-read-only)
            (should (eq niwa--view-buffer parent-view)))
          (should preserve-data-view-buffer)
          (should-not restored))
      (dolist (buffer (list data-buffer view-buffer parent-view))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))

  :doc "keeps equal agent paths isolated across parent sessions"
  (let* ((session-a-id (make-temp-name "session-a-"))
         (session-a (niwa-session--create
                     :name "main" :session-id session-a-id))
         (session-b (niwa-session--create
                     :name "main" :session-id "session-b"))
         (data-a (generate-new-buffer " *agent-a-data*"))
         (data-b (generate-new-buffer " *agent-b-data*"))
         (parent-a (generate-new-buffer " *agent-a-parent*"))
         (parent-b (generate-new-buffer " *agent-b-parent*"))
         (collision
          (get-buffer-create
           (format "*niwa-agent:%s:/root/reviewer*" session-a-id)))
         views)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer collision
            (insert "unrelated"))
          (dolist (entry `((,data-a ,session-a "session A")
                           (,data-b ,session-b "session B")))
            (with-current-buffer (car entry)
              (setq-local niwa--session (cadr entry))
              (org-mode)
              (insert (caddr entry))))
          (with-current-buffer parent-a
            (setq-local niwa--data-buffer data-a))
          (with-current-buffer parent-b
            (setq-local niwa--data-buffer data-b))
          (cl-letf (((symbol-function 'niwa-view--full-rerender)
                     (lambda ()
                       (let ((inhibit-read-only t)
                             (source niwa--data-buffer))
                         (erase-buffer)
                         (insert (with-current-buffer source
                                   (buffer-string)))))))
            (let ((view-a
                   (niwa-view--ensure-agent-transcript-view
                    "/root/reviewer"
                    (list :session session-a :buffer data-a :live-buffer t)
                    parent-a)))
              (push view-a views)
              (should-not (eq collision view-a))
              (with-current-buffer collision
                (should (equal "unrelated" (buffer-string))))
              (set-window-buffer (selected-window) view-a)
              (let ((view-b
                     (niwa-view--ensure-agent-transcript-view
                      "/root/reviewer"
                      (list :session session-b :buffer data-b :live-buffer t)
                      parent-b)))
                (push view-b views)
                (should-not (eq view-a view-b))
                (should (eq view-a (window-buffer (selected-window))))
                (with-current-buffer view-a
                  (should (equal "session A" (buffer-string)))
                  (should (eq data-a niwa--data-buffer)))
                (with-current-buffer view-b
                  (should (equal "session B" (buffer-string)))
                  (should (eq data-b niwa--data-buffer)))))))
      (dolist (buffer (append views
                              (list collision data-a data-b parent-a parent-b)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))
  :doc "rejects missing session identity before opening saved data"
  (let ((session (niwa-session--create :name "main"))
        (parent (generate-new-buffer " *agent-unowned-parent*"))
        opened-buffer)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-session-artifacts-find-artifact-noselect)
                   (lambda (&rest _)
                     (setq opened-buffer
                           (generate-new-buffer " *agent-unowned-data*")))))
          (should-error
           (niwa-view--ensure-agent-transcript-view
            "/root/reviewer"
            (list :session session :relative-path "agents/reviewer.chat.org")
            parent)
           :type 'error)
          (should-not opened-buffer))
      (dolist (buffer (list opened-buffer parent))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(niwa-deftest niwa-view--agent-status-collect
  (:doc "derives live rows and hierarchy solely from the retained registry")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (parent-inv (niwa-agent-invocation--create
                        :description "Review the spec"
                        :call-count 2
                        :started-at (current-time)))
           (child-inv (niwa-agent-invocation--create
                       :description "Verify details"
                       :call-count 1
                       :started-at (current-time)
                       :parent-tool-use-id "agent-call")))
      (setf (niwa-session-agent-registry session)
            (list
             (cons "/root/spec_review"
                   (niwa-view-agent-test--record
                    "/root/spec_review" 'running parent-inv "reviewer"))
             (cons "/root/spec_review/details"
                   (niwa-view-agent-test--record
                    "/root/spec_review/details" 'waiting child-inv "explorer"
                    "/root/spec_review"))
             (cons "/root/done"
                   (niwa-view-agent-test--record "/root/done" 'idle))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (let ((rows (niwa-view--agent-status-collect)))
          (should (equal '("/root/spec_review"
                           "/root/spec_review/details")
                         (mapcar (lambda (row) (plist-get row :path)) rows)))
          (should (eq 'running (plist-get (car rows) :status)))
          (should (eq 'waiting (plist-get (cadr rows) :status)))
          (should (eq 'waiting (plist-get (cadr rows) :activity)))
          (should (= 1 (plist-get (cadr rows) :depth)))
          (should (equal "/root/spec_review" (plist-get (cadr rows) :parent-path)))
          (should (equal "agent-call" (plist-get (cadr rows) :tool-use-id)))))))

  :doc "active agents remain listed when their launch handle is in history"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (path "/root/spec_review"))
      (setf (niwa-session-agent-registry session)
            (list (cons path
                        (niwa-view-agent-test--record path 'running))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert (propertize "Started /root/spec_review"
                              'niwa-view-agent-path path
                              'niwa-view-agent-handle-p t)))
        (should (equal (list path)
                       (mapcar (lambda (row) (plist-get row :path))
                               (niwa-view--agent-status-collect))))))))

(niwa-deftest niwa-view-agent-status-panel
  (:doc "agents render in the background-work panel and preserve composer drafts")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (path "/root/spec_review"))
      (setf (niwa-session-agent-registry session)
            (list (cons path
                        (niwa-view-agent-test--record
                         path 'running nil "reviewer"))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "> first line\nsecond line")
        (niwa-view--render-status data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p
                   (regexp-quote "background work · 1 agent [-]") text))
          (should (string-match-p "─\n  ▸ Running   agent    /root/spec_review" text))
          (should (equal "> first line\nsecond line"
                         (niwa-view-composer-draft-text nil t))))
        (niwa-view-execution-toggle-panel)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p
                   (regexp-quote "background work · 1 agent [+]") text))
          (should-not (string-match-p "/root/spec_review" text))
          (should (equal "> first line\nsecond line"
                         (niwa-view-composer-draft-text nil t))))
        (niwa-view-execution-toggle-panel)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p
                   (regexp-quote "background work · 1 agent [-]") text))
          (should (string-match-p "Running   agent    /root/spec_review" text))
          (should (equal "> first line\nsecond line"
                         (niwa-view-composer-draft-text nil t))))))))

(niwa-deftest niwa-view--insert-attribution
  (:doc "uses canonical paths as transcript click targets")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((path "/root/spec_review")
             (text (niwa-view--insert-attribution path))
             (pos (string-match-p path text)))
        (should (string= "from /root/spec_review" text))
        (should pos)
        (should (equal path
                       (get-text-property pos 'niwa-view-agent-path text)))
        (should (get-text-property pos 'keymap text))))))

(niwa-deftest niwa-view--refresh-agent-handle-at
  (:doc "keeps sandbox warnings read-only after a targeted refresh")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let ((niwa-tool--registry
           (copy-hash-table niwa-tool--registry))
          (niwa-tool--resolve-cache (make-hash-table :test #'equal))
          (path "/root/spec_review"))
      (niwa-tool-register
       (niwa-tool--create
        :name "RefreshAgent"
        :category "niwa"
        :renderer
        (lambda (_name _args result _data)
          `(:header ,(concat "Started " path)
            :body ,result
            :vtype agent-handle
            :agent-path ,path
            :agent-status running
            :initially-collapsed-p t
            :sandbox-summary
            (:attempt-count 1 :started-count 1 :refused-count 0
             :sandbox bubblewrap :filesystem workspace-write
             :network isolated :proc fresh
             :additional-read-count 0 :additional-write-count 1)))))
      (niwa-view-test--insert-data
       data-buf
       "(:name \"RefreshAgent\" :args (:task_name \"review\"))\n\nRunning.\n"
       '(tool . "call_refresh_agent"))
      (let* ((source
              (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering
              (niwa-view--segment-rendering
               data-buf (car source) (cdr source) t)))
        (with-current-buffer view-buf
          (let ((inhibit-read-only t)
                (start (point-min)))
            (goto-char start)
            (niwa-view--insert-rendered-tool rendering source)
            (niwa-view-render-add-display-properties
             start (point) 'agent-handle))
          (goto-char (point-min))
          (search-forward "Sandbox:")
          (goto-char (+ (match-beginning 0) 2))
          (should (get-text-property (point) 'read-only))
          (should (eq niwa-view--agent-handle-map
                      (get-text-property (point) 'keymap)))
          (let ((inhibit-read-only t))
            (should (niwa-view--refresh-agent-handle-at
                     (point-min) path)))
          ;; Replacing retained metadata must retire the superseded cache key
          ;; even when every refresh succeeds without a full projection.
          (dotimes (i 10)
            (with-current-buffer data-buf
              (goto-char (point-min))
              (re-search-forward "\\(?:Running\\|Update[0-9]\\)\\.")
              (replace-match (format "Update%d." i) t t))
            (let ((inhibit-read-only t))
              (should (niwa-view--refresh-agent-handle-at (point-min) path)))
            (should (= 1 (hash-table-count niwa-view--tool-rendering-cache))))
          (goto-char (point-min))
          (search-forward "Sandbox:")
          (goto-char (+ (match-beginning 0) 2))
          (should (get-text-property (point) 'read-only))
          (should (eq niwa-view--agent-handle-map
                      (get-text-property (point) 'keymap)))
          (let ((before (buffer-string)))
            (should-error (insert "  ") :type 'text-read-only)
            (should (equal before (buffer-string)))))))))

(niwa-deftest niwa-view--agent-source-present-p
  (:doc "answers Agent-source presence from invocation-owned markers")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((session (niwa-view-agent-test--session))
           (invocation (niwa-agent-invocation--create))
           (record (niwa-view-agent-test--record
                    "/root/worker_1" 'running invocation)))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker_1" record)))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (insert "agent render data")
        (setf (niwa-agent-invocation-render-data-start-marker invocation)
              (copy-marker (point-min))
              (niwa-agent-invocation-render-data-end-marker invocation)
              (copy-marker (point-max) t)))
      (with-current-buffer view-buf
        (should (niwa-view--agent-source-present-p "/root/worker_1"))
        ;; A path without a registry record has no discoverable source.
        (should-not
         (niwa-view--agent-source-present-p "/root/other"))
        ;; Markers in another buffer are not this view's source.
        (setf (niwa-agent-invocation-render-data-start-marker invocation)
              (with-temp-buffer (copy-marker (point-min))))
        (should-not
         (niwa-view--agent-source-present-p "/root/worker_1"))))))

(niwa-deftest niwa-view-agent-cancel-refresh ()
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-agent-refresh-delay 60))
        (niwa-view-refresh-agent-rendering view-buf "/root/a")
        (let ((timer niwa-view--agent-refresh-timer))
          (should (memq timer timer-list))
          (niwa-view-agent-cancel-refresh)
          (should-not (memq timer timer-list))
          (should-not niwa-view--agent-refresh-pending)
          (should-not niwa-view--agent-refresh-timer))))))

(niwa-deftest niwa-view-agent--flush-refresh ()
  ,test
  (test)
  :doc "one callback consumes one distinct agent and preserves pending order"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-agent-refresh-delay 60) refreshed)
        (dolist (path '("/root/a" "/root/b" "/root/a"))
          (niwa-view-refresh-agent-rendering view-buf path))
        (cl-letf (((symbol-function 'niwa-view--refresh-agent-rendering-now)
                   (lambda (path) (push path refreshed))))
          (niwa-view-agent--flush-refresh view-buf)
          (should (equal refreshed '("/root/a")))
          (should (equal niwa-view--agent-refresh-pending '("/root/b")))
          (niwa-view-agent--flush-refresh view-buf)
          (should (equal refreshed '("/root/b" "/root/a")))
          (should-not niwa-view--agent-refresh-pending)
          (should-not niwa-view--agent-refresh-timer)))))
  :doc "a failed row does not strand other agents and view death cancels work"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((niwa-view-agent-refresh-delay 60))
        (niwa-view-refresh-agent-rendering view-buf "/root/a")
        (niwa-view-refresh-agent-rendering view-buf "/root/b")
        (cl-letf (((symbol-function 'niwa-view--refresh-agent-rendering-now)
                   (lambda (_) (error "Injected refresh failure"))))
          (should-error (niwa-view-agent--flush-refresh view-buf)))
        (should (equal niwa-view--agent-refresh-pending '("/root/b")))
        (let ((timer niwa-view--agent-refresh-timer))
          (kill-buffer view-buf)
          (should-not (memq timer timer-list)))))))

(niwa-deftest niwa-view-refresh-agent-rendering
  (:doc "coalesces canonical-path refreshes without altering the composer")
  ,test
  (test)

  :doc "distinct agent bursts share one timer rather than expiring together"
  (niwa-view-test--with-buffers
    (let ((niwa-view-agent-refresh-delay 60)
          (schedule (symbol-function 'run-at-time)) timers)
      (unwind-protect
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest args)
                       (let ((timer (apply schedule args)))
                         (push timer timers) timer))))
            (dolist (path '("/root/a" "/root/b" "/root/c" "/root/a"))
              (niwa-view-refresh-agent-rendering view-buf path))
            (should (= (length timers) 1)))
        (mapc #'cancel-timer timers))))

  :doc "no handle and no source leaves the draft untouched"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\ncontinues")
      (let ((niwa-view-agent-refresh-delay 0))
        (cl-letf (((symbol-function 'niwa-view--agent-handle-refresh-points)
                   (lambda (_path) '(nil . nil)))
                  ((symbol-function 'niwa-view--render-agent-status)
                   (lambda () nil)))
          (niwa-view-refresh-agent-rendering
           view-buf "/root/spec_review")))
      (should (equal "> draft\ncontinues"
                     (niwa-view-composer-draft-text nil t)))))

  :doc "the stale-source full rerender preserves a multiline draft"
  ;; An unrendered handle whose source exists falls back to a full
  ;; rerender; the composer draft, whose first editable character is
  ;; `>', must survive that path too.
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\ncontinues")
      (let ((niwa-view-agent-refresh-delay 0))
        (cl-letf (((symbol-function 'niwa-view--agent-handle-refresh-points)
                   (lambda (_path) '(nil . nil)))
                  ((symbol-function 'niwa-view--agent-source-present-p)
                   (lambda (_path) t)))
          (niwa-view-refresh-agent-rendering
           view-buf "/root/spec_review")))
      (should (equal "> draft\ncontinues"
                     (niwa-view-composer-draft-text nil t))))))

(niwa-deftest niwa-view--agent-live-transcript-dispatch ()
  ,test
  (test)
  :doc "child stream and tool callbacks preserve the open parent transcript and draft"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let ((parent-data data-buf) (parent-view view-buf))
        (niwa-view-test--insert-data parent-data "PARENT response must remain visible.\n" 'response)
        (with-current-buffer parent-view
          (niwa-view-stream-begin-turn niwa-view--status-marker
                                          (with-current-buffer parent-data (copy-marker 1)))
          (niwa-view-render-live-update parent-data)
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4))
        (niwa-view-test--with-buffers
          (let ((child-data data-buf) (child-view view-buf)
                (niwa-view-stream-render-delay 60)
                (niwa-view-tool-boundary-render-delay 60))
            (with-current-buffer child-data
              (setq-local niwa--view-buffer parent-view)
              (setq-local niwa--agent-invocation
                          (niwa-agent-invocation--create :buffer child-data :path "/root/child"
                                                            :transcript-status 'running)))
            (with-current-buffer child-view
              (niwa-view-agent-initialize
               (list :agent-transcript-p t :agent-path "/root/child"
                     :parent-view parent-view :preserve-data-view-buffer t
                     :transcript-info '(:live-buffer t :status running)) child-data)
              (niwa-view-stream-begin-turn niwa-view--status-marker
                                              (with-current-buffer child-data (copy-marker 1))))
            (switch-to-buffer parent-view)
            (select-window (split-window-right))
            (switch-to-buffer child-view)
            (niwa-view-test--insert-data child-data "CHILD response belongs only here.\n" 'response)
            (dolist (event '(stream pre-tool post-tool progress))
              (with-current-buffer child-data
                (pcase event
                  ('stream (niwa-view-agent-live-transcript-stream))
                  ('pre-tool
                   (niwa-view-agent-live-transcript-pre-tool
                    '(:name "Read" :id "child-read" :args (:file_path "child.txt"))))
                  ('post-tool
                   (niwa-view-agent-live-transcript-post-tool
                    '(:name "Read" :id "child-read" :args (:file_path "child.txt"))))
                  ('progress
                   (niwa-view-stream-handle-tool-progress
                    (list :type 'progress :data-buffer child-data
                          :tool-use-id "child-read"
                          :facts '(:kind ptc :active-tool "Read"))))))
              (dolist (view (list child-view parent-view))
                (with-current-buffer view
                  (when niwa-view--render-timer (cancel-timer niwa-view--render-timer))
                  (niwa-view--flush-scheduled-render view t)))
              (with-current-buffer parent-view
                (should (string-search "PARENT response must remain visible." (buffer-string)))
                (should-not (string-search "CHILD response" (buffer-string)))
                (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
                (should (= 4 (niwa-view-agent-test--draft-offset)))
                (should-not niwa-view--pending-tool-calls))
              (with-current-buffer child-data
                (should (eq niwa--view-buffer parent-view))))
            (with-current-buffer child-view
              (should (string-search "CHILD response belongs only here." (buffer-string)))))))))

  :doc "a child's running call is a running row in its transcript only"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let ((parent-view view-buf))
        (niwa-view-test--with-buffers
          (let ((child-data data-buf) (child-view view-buf)
                (niwa-view-tool-boundary-render-delay 60)
                (call '(:name "Read" :args (:file_path "child.txt"))))
            (with-current-buffer child-data
              (setq-local niwa--view-buffer parent-view)
              (setq-local niwa--agent-invocation
                          (niwa-agent-invocation--create
                           :buffer child-data :path "/root/child"
                           :transcript-status 'running)))
            (with-current-buffer child-view
              (niwa-view-agent-initialize
               (list :agent-transcript-p t :agent-path "/root/child"
                     :parent-view parent-view :preserve-data-view-buffer t
                     :transcript-info '(:live-buffer t :status running))
               child-data)
              (niwa-view-stream-begin-turn
               niwa-view--status-marker
               (with-current-buffer child-data (copy-marker (point-max)))))
            (switch-to-buffer parent-view)
            (select-window (split-window-right))
            (switch-to-buffer child-view)
            (cl-flet ((flush ()
                        (dolist (view (list child-view parent-view))
                          (with-current-buffer view
                            (when niwa-view--render-timer
                              (cancel-timer niwa-view--render-timer))
                            (niwa-view--flush-scheduled-render view t)))))
              (with-current-buffer child-data
                (niwa-view-agent-live-transcript-pre-tool call))
              (flush)
              (with-current-buffer child-view
                (should (string-search "● Read: child.txt" (buffer-string))))
              (with-current-buffer parent-view
                (should-not (string-search "Read: child.txt" (buffer-string))))
              (niwa-view-test--insert-data
               child-data
               "(:name \"Read\" :args (:file_path \"child.txt\"))\n\nresult\n"
               '(tool . "child-read"))
              (with-current-buffer child-data
                (niwa-view-agent-live-transcript-post-tool call))
              (flush)
              (with-current-buffer child-view
                (should (string-search "✓ Read: child.txt" (buffer-string)))
                (should-not (string-search "● Read" (buffer-string)))))))))))

(provide 'test-niwa-view-agent)
;;; test-niwa-view-agent.el ends here
