;;; test-niwa-side-conversation.el -- Side conversation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Acceptance tests for ephemeral `/btw' side conversations.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(defvar org-mode-hook)

(require 'gptel-context)
(require 'gptel-openai)
(require 'gptel-request)
(require 'niwa)
(require 'niwa-permission-rules)
(require 'niwa-side-conversation)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-view)

(declare-function gptel-make-openai "ext:gptel-openai" (name &rest args))

(defvar gptel--known-backends)

(niwa-deftest niwa-side-conversation--interrupted-boundary ()
  ,test
  (test)

  :doc "bookends model-visible reminder text with ignored delimiters"
  (let* ((body "The inherited parent turn is incomplete.")
         (boundary (niwa-side-conversation--interrupted-boundary body))
         (body-start (string-match (regexp-quote body) boundary)))
    (should (eq 'ignore (get-text-property 0 'gptel boundary)))
    (should-not (get-text-property body-start 'gptel boundary))
    (should (eq 'ignore
                (get-text-property (1- (length boundary)) 'gptel boundary)))))

(niwa-deftest niwa-side-conversation-close
  (:quiet t)
  ,test
  (test)

  :doc "refuses to close an ordinary chat view"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (should-error (niwa-side-conversation-close) :type 'user-error)))

  :doc "closes the paired side buffers and clears the parent link"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-command-close" :root (niwa-test--placeholder-root)
           :name "btw-command-close"))
         (session (niwa-session-create "main" workspace))
         side-view side-data)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq side-data (buffer-local-value 'niwa--data-buffer side-view))
      (with-current-buffer side-view
        (should (eq #'niwa-side-conversation-close
                    (lookup-key (current-local-map) (kbd "C-c C-z"))))
        (should (eq #'niwa-side-conversation-close
                    (lookup-key niwa-view--side-conversation-keymap
                                (kbd "C-c C-z"))))
        (niwa-side-conversation-close))
      (should-not (buffer-live-p side-view))
      (should-not (buffer-live-p side-data))
      (should (buffer-live-p data-buf))
      (with-current-buffer data-buf
        (should-not niwa-side-conversation--side-buffer))))

  :doc "stops an unsettled remote mutation before the close query runs"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-command-mutation" :root (niwa-test--placeholder-root)
           :name "btw-command-mutation"))
         (session (niwa-session-create "main" workspace))
         (unsettled t)
         (teardown-count 0))
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa-view--side-conversation-p t))
      (cl-letf (((symbol-function 'niwa-execution-unsettled-mutation-p)
                 (lambda (_session) unsettled))
                ((symbol-function 'niwa-execution-teardown-session)
                 (lambda (_session)
                   (cl-incf teardown-count)
                   (setq unsettled nil))))
        (with-current-buffer view-buf
          (niwa-side-conversation-close)))
      (should (> teardown-count 0))
      (should-not (buffer-live-p view-buf))
      (should-not (buffer-live-p data-buf)))))

(niwa-deftest niwa-view-send/btw (:quiet t)
  ,test
  (test)
  :doc "rejects /btw before the parent has accepted a prompt"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-empty" :root (niwa-test--placeholder-root) :name "btw-empty"))
         (session (niwa-session-create "main" workspace)))
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (should-error (with-current-buffer (niwa-view-composer-buffer)
                        (niwa-view-send)) :type 'user-error)
        (should (equal "/btw" (niwa-view-composer-draft-text nil t))))))
  :doc "waits for asynchronous inherited gptel context formatting"
  (let* ((temporary-file-directory
          (file-name-as-directory (expand-file-name "~")))
         (root (make-temp-file "niwa-btw-async-context-" t))
         (context-file (file-name-concat root "context.txt"))
         (buffer-file (file-name-concat root "buffer.txt"))
         context-buffer
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-async-context"))
         (session (niwa-session-create "main" workspace))
         formatter-callback
         formatter-contexts
         context-overlay
         bounds
         request
         side-view)
    (unwind-protect
        (progn
          (with-temp-file context-file
            (insert "BTWFROZENORIGINAL"))
          (with-temp-file buffer-file
            (insert "BTWOVERLAY\nBTWBUFFERFROZEN"))
          (setq context-buffer (find-file-noselect buffer-file))
          (with-current-buffer context-buffer
            (put-text-property 12 (point-max) 'btw-context-property 'kept)
            (setq context-overlay (make-overlay 1 11)
                  bounds (cons (copy-marker 12) (copy-marker (point-max))))
            (set-buffer-modified-p nil))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-context
                          (list context-file
                                (list context-buffer
                                      :bounds bounds
                                      :overlays (list context-overlay)))
                          gptel-use-context 'user
                          gptel-context-string-function
                          (lambda (callback contexts)
                            (setq formatter-callback callback
                                  formatter-contexts contexts)))
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw Explain the frozen context")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer)))
                        ((symbol-function 'gptel-send)
                         (lambda (&optional _arg)
                           (setq request
                                 (niwa-view-test--dry-run-request-data)))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))
                (should formatter-callback)
                (should formatter-contexts)
                (should-not side-view)
                (should (equal "/btw Explain the frozen context"
                               (niwa-view-composer-draft-text nil t)))
                (let* ((entry (cl-find-if
                               (lambda (item) (bufferp (car item)))
                               formatter-contexts))
                       (copy (car entry))
                       (spec (cdr entry)))
                  (should-not (eq copy context-buffer))
                  (should-not (buffer-local-value 'buffer-file-name copy))
                  (should (eq 'kept
                              (get-text-property
                               13 'btw-context-property copy)))
                  (should (cl-every #'integerp
                                    (list (car (plist-get spec :bounds))
                                          (cdr (plist-get spec :bounds)))))
                  (should (cl-every
                           (lambda (overlay)
                             (and (overlayp overlay)
                                  (eq copy (overlay-buffer overlay))))
                           (plist-get spec :overlays))))
                (with-temp-file context-file
                  (insert "BTWLIVECHANGED"))
                (with-current-buffer context-buffer
                  (erase-buffer)
                  (insert "BTWBUFFERLIVECHANGED")
                  (save-buffer))
                (should
                 (equal "BTWBUFFERLIVECHANGED"
                        (string-trim
                         (with-temp-buffer
                           (insert-file-contents buffer-file)
                           (buffer-string)))))
                (let* ((file-entry
                        (cl-find-if
                         (lambda (item) (stringp (car item)))
                         formatter-contexts))
                       (temporary-label
                        (abbreviate-file-name (car file-entry)))
                       (formatted (gptel-context--string formatter-contexts))
                       (label-start
                        (string-match (regexp-quote temporary-label)
                                      formatted)))
                  (should label-start)
                  (put-text-property 1 2 'btw-output-property 'kept formatted)
                  (put-text-property
                   label-start (+ label-start (length temporary-label))
                   'btw-label-property 'kept formatted)
                  (funcall formatter-callback formatted)))
              (should (buffer-live-p side-view))
              (should (string-empty-p (niwa-view-composer-draft-text nil t))))
            (should (string-match-p "BTWFROZENORIGINAL" request))
            (should-not (string-match-p "BTWLIVECHANGED" request))
            (should (string-match-p "BTWBUFFERFROZEN" request))
            (should-not (string-match-p "BTWBUFFERLIVECHANGED" request))
            (should (string-prefix-p "~/" (abbreviate-file-name context-file)))
            (should (string-match-p
                     (regexp-quote (abbreviate-file-name context-file))
                     request))
            (should (string-match-p
                     (regexp-quote (buffer-name context-buffer)) request))
            (should-not (string-match-p "niwa-btw-context-" request))
            (should (string-match-p "Explain the frozen context" request))
            (let* ((side-data
                    (buffer-local-value 'niwa--data-buffer side-view))
                   (frozen
                    (buffer-local-value
                     'niwa-side-conversation--frozen-context side-data)))
              (let* ((text (plist-get frozen :text))
                     (original-label (abbreviate-file-name context-file))
                     (label-start
                      (string-match (regexp-quote original-label) text)))
                (should (eq 'kept
                            (get-text-property
                             1 'btw-output-property text)))
                (should label-start)
                (should (eq 'kept
                            (get-text-property
                             label-start 'btw-label-property text)))))
            (with-current-buffer data-buf
              (should (buffer-live-p
                       niwa-side-conversation--side-buffer))))
      (when (buffer-live-p context-buffer)
        (kill-buffer context-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))
  :doc "preserves parent edits while asynchronous context is pending"
  (let* ((root (make-temp-file "niwa-btw-async-draft-" t))
         (context-file (file-name-concat root "context.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-async-draft"))
         (session (niwa-session-create "main" workspace))
         formatter-callback
         side-view)
    (unwind-protect
        (progn
          (with-temp-file context-file
            (insert "Context"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-context (list context-file)
                          gptel-use-context 'user
                          gptel-context-string-function
                          (lambda (callback _contexts)
                            (setq formatter-callback callback)))
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer)))
                      ((symbol-function 'gptel-send)
                       (lambda (&optional _arg))))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (niwa-view-test--insert-composer-draft "/btw Initial question")
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send)))
              (with-current-buffer data-buf
                (should-error
                 (niwa-side-conversation-open "Duplicate question")
                 :type 'user-error))
              (with-current-buffer view-buf
                (let ((inhibit-read-only t))
                  (with-current-buffer (niwa-view-composer-buffer)
                    (delete-region (niwa-view-composer-draft-start) (niwa-view-composer-draft-end))))
                (niwa-view-test--insert-composer-draft
                 "> quoted\nsecond line" 4))
              (funcall formatter-callback "FROZEN ASYNC CONTEXT"))
            (should (buffer-live-p side-view))
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (niwa-view-composer-draft-text nil t)))
              (should (= 4 (niwa-view-test--composer-point-offset))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "cleans async context after formatter quit or side mode failure"
  (dolist (failure '(formatter-quit mode-failure))
    (let* ((root (make-temp-file "niwa-btw-async-failure-" t))
           (media-file (file-name-concat root "image.png"))
           (workspace
            (niwa-workspace--create
             :type 'file :id root :root root :name "btw-async-failure"))
           (session (niwa-session-create "main" workspace))
           formatter-callback
           frozen-path
           pending-side)
      (unwind-protect
          (progn
            (with-temp-file media-file
              (insert "FROZEN MEDIA BYTES"))
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--workspace workspace
                            niwa--session session
                            gptel-context
                            (list (list media-file :mime "image/png"))
                            gptel-use-context 'user
                            gptel-context-string-function
                            (lambda (callback contexts)
                              (setq pending-side
                                    (buffer-local-value
                                     'niwa-side-conversation--side-buffer
                                     data-buf)
                                    frozen-path (caar contexts))
                              (if (eq failure 'formatter-quit)
                                  (signal 'quit nil)
                                (setq formatter-callback callback))))
                (insert "*** Parent prompt\n"
                        (propertize "Parent answer\n" 'gptel 'response)))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (niwa-view-test--insert-composer-draft "/btw")
                (if (eq failure 'formatter-quit)
                    (let (quit-seen)
                      (condition-case nil
                          (with-current-buffer (niwa-view-composer-buffer)
                            (niwa-view-send))
                        (quit (setq quit-seen t)))
                      (should quit-seen))
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send))
                  (should formatter-callback)
                  (should (file-exists-p frozen-path))
                  (let ((mode-function
                         (symbol-function 'niwa--transcript-org-mode)))
                    (cl-letf
                        (((symbol-function 'niwa--transcript-org-mode)
                          (lambda ()
                            (funcall mode-function)
                            (error "Injected side mode failure"))))
                      (should-error
                       (funcall formatter-callback "Frozen context"))))))
              (should frozen-path)
              (should-not (file-exists-p frozen-path))
              (should-not (buffer-live-p pending-side))
              (with-current-buffer data-buf
                (should-not niwa-side-conversation--side-buffer))))
        (when (file-exists-p frozen-path)
          (delete-file frozen-path))
        (when (file-directory-p root)
          (delete-directory root t)))))
  :doc "cleans frozen media when a pending asynchronous open loses its parent"
  (let* ((root (make-temp-file "niwa-btw-async-close-" t))
         (media-file (file-name-concat root "image.png"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-async-close"))
         (session (niwa-session-create "main" workspace))
         formatter-callback
         frozen-path
         pending-side)
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-context
                          (list (list media-file :mime "image/png"))
                          gptel-use-context 'user
                          gptel-context-string-function
                          (lambda (callback _contexts)
                            (setq formatter-callback callback)))
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send)))
            (with-current-buffer data-buf
              (setq pending-side niwa-side-conversation--side-buffer))
            (setq frozen-path
                  (car
                   (car
                    (plist-get
                     (buffer-local-value
                      'niwa-side-conversation--frozen-context
                      pending-side)
                     :media))))
            (should (file-exists-p frozen-path))
            (kill-buffer data-buf)
            (should-not (buffer-live-p pending-side))
            (should-not (file-exists-p frozen-path))
            (funcall formatter-callback "Late context")
            (should-not (file-exists-p frozen-path))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "does not leak frozen media when side construction fails"
  (let* ((root (make-temp-file "niwa-btw-create-failure-" t))
         ;; Inventory and remove only this fixture's files, not another ERT
         ;; worker's live context media in the shared temporary directory.
         (temporary-file-directory (file-name-as-directory root))
         (media-file (file-name-concat root "image.png"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-create-failure"))
         (session (niwa-session-create "main" workspace))
         (before
          (directory-files temporary-file-directory t
                           "\\`niwa-btw-context-")))
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-context
                          (list (list media-file :mime "image/png"))
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (cl-letf (((symbol-function 'niwa-session-create)
                         (lambda (&rest _)
                           (error "Injected side session creation failure"))))
                (should-error (with-current-buffer (niwa-view-composer-buffer)
                                (niwa-view-send)))))
            (should
             (equal before
                    (directory-files temporary-file-directory t
                                     "\\`niwa-btw-context-")))))
      (dolist (file
               (cl-set-difference
                (directory-files temporary-file-directory t
                                 "\\`niwa-btw-context-")
                before :test #'equal))
        (ignore-errors (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "opens a paired ephemeral side while hiding inherited context"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw" :root (niwa-test--placeholder-root) :name "btw"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (setf (niwa-session-plan-mode session) t)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (should (string-empty-p (niwa-view-composer-draft-text nil t))))
      (should (buffer-live-p side-view))
      (should-not (eq side-view view-buf))
      (let ((side-data
             (buffer-local-value 'niwa--data-buffer side-view)))
        (should (buffer-live-p side-data))
        (should (string-prefix-p " " (buffer-name side-data)))
        (should-not (string-prefix-p " " (buffer-name side-view)))
        (with-current-buffer side-data
          (should-not (niwa-session-plan-mode niwa--session))
          (should (string-match-p "Parent prompt" (buffer-string)))
          (should (string-match-p "Parent answer" (buffer-string))))
        (with-current-buffer side-view
          (should (string-match-p "btw" (buffer-string)))
          (should-not (string-match-p "Parent prompt" (buffer-string)))
          (should-not (string-match-p "Parent answer" (buffer-string))))
        (with-current-buffer (niwa-view-composer-buffer side-view)
          (should-not (memq #'niwa-agent-capf
                            completion-at-point-functions))
          (niwa-view-test--insert-composer-draft "@")
          (let ((candidates (nth 2 (niwa-mention-capf))))
            (should (member "@file:" candidates))
            (should-not (member "@agent:" candidates)))
          (delete-region (niwa-view-composer-draft-start)
                         (niwa-view-composer-draft-end))))
      (with-current-buffer data-buf
        (should (niwa-session-plan-mode session))
        (should (equal "*** Parent prompt\nParent answer\n"
                       (buffer-string))))))
  :doc "preserves a materialized list-valued gptel system prompt"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-system-list" :root (niwa-test--placeholder-root)
           :name "btw-system-list"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session
                    gptel-system-prompt
                    '("Base system message"
                      "Template user turn"
                      "Template assistant response"))
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (let ((side-system
             (buffer-local-value
              'gptel-system-prompt
              (buffer-local-value 'niwa--data-buffer side-view))))
        (should (listp side-system))
        (should (string-prefix-p "Base system message"
                                 (car side-system)))
        (should (string-match-p "ephemeral /btw"
                                (car side-system)))
        (should (equal '("Template user turn"
                         "Template assistant response")
                       (cdr side-system))))))
  :doc "keeps inherited reminders hidden during live and terminal side renders"
  (dolist (live-first '(t nil))
    (let* ((workspace
            (niwa-workspace--create
             :type 'file :id "btw-render-boundary"
             :root (niwa-test--placeholder-root) :name "btw-render-boundary"))
           (session (niwa-session-create "main" workspace))
           (draft "> keep this draft\nsecond line")
           side-view side-data response-info)
      (niwa-view-test--with-buffers
        (with-current-buffer data-buf
          (setq-local niwa--workspace workspace
                      niwa--session session)
          (insert "*** Parent prompt\n"
                  (propertize "Parent answer\n" 'gptel 'response)
                  "\n*** Active parent prompt\n")
          (setq-local
           niwa--current-request
           (niwa-request--create
            :id "btw-render-parent" :session session
            :fsm (gptel-make-fsm
                  :state 'TYPE
                  :info (list :buffer data-buf :position (copy-marker (point))
                              :niwa-request-locals
                              (niwa-side-conversation--request-locals)
                              :niwa-model-context (buffer-string))))))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view-test--insert-composer-draft "/btw Unique side question")
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (setq side-view buffer)))
                    ((symbol-function 'gptel-send) #'ignore))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send))))
        (setq side-data (buffer-local-value 'niwa--data-buffer side-view)
              response-info
              (with-current-buffer side-data
                (list :buffer side-data :position (copy-marker (point-max)))))
        (with-current-buffer side-view
          (niwa-view-test--insert-composer-draft draft (- (length draft) 3)))
        (cl-labels
            ((check-view
              ()
              (with-current-buffer side-view
                (let ((text (buffer-substring-no-properties
                             (point-min) (niwa-view-transcript-tail))))
                  (should (= 1 (cl-count "Unique side question"
                                         (split-string text "\n") :test #'equal)))
                  (should-not (string-match-p "System reminder\\|Parent prompt"
                                             text)))
                (should (equal draft (niwa-view-composer-draft-text nil t)))
                (should (= 3 (with-current-buffer (niwa-view-composer-buffer)
                               (- (niwa-view-composer-draft-end) (point))))))))
          (when live-first
            ;; A tool/permission refresh can run before the first response
            ;; establishes a new gptel property run after the inherited reminder.
            (dotimes (_ 2)
              (with-current-buffer side-view
                (niwa-view--render-stream-update side-data))
              (check-view)))
          (with-current-buffer side-data
            (gptel-curl--stream-insert-response "Side answer." response-info))
          (when live-first
            (with-current-buffer side-view
              (niwa-view--render-stream-update side-data))
            (check-view))
          (with-current-buffer side-data
            (niwa-view-stream-render-response
             (plist-get response-info :position)
             (plist-get response-info :tracking-marker)))
          (check-view)
          (with-current-buffer side-view
            (should (string-match-p "Side answer\\." (buffer-string)))
            (niwa-view--full-rerender))
          (check-view)))))
  :doc "submits an inline prompt and independent multi-turn follow-up"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-multi" :root (niwa-test--placeholder-root) :name "btw-multi"))
         (session (niwa-session-create "main" workspace))
         side-view
         requests)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq side-view buffer)))
                ((symbol-function 'gptel-send)
                 (lambda (&optional _arg)
                   (push (buffer-string) requests)
                   (let ((start (point-max)))
                     (goto-char start)
                     (insert (format "Side answer %d\n" (length requests)))
                     (put-text-property start (point) 'gptel 'response)))))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view-test--insert-composer-draft "/btw First side prompt")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (with-current-buffer side-view
          (niwa-view-test--insert-composer-draft "Second side prompt")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-side-conversation-send))))
      (should (= 2 (length requests)))
      (should (string-match-p "Parent prompt" (cadr requests)))
      (should (string-match-p "First side prompt" (cadr requests)))
      (should (string-match-p "Side answer 1" (car requests)))
      (should (string-match-p "Second side prompt" (car requests)))
      (with-current-buffer data-buf
        (should (equal "*** Parent prompt\nParent answer\n"
                       (buffer-string))))))
  :doc "freezes inherited gptel context across side turns"
  (let* ((root (make-temp-file "niwa-btw-context-" t))
         (context-file (file-name-concat root "context.txt"))
         (changed-file (file-name-concat root "changed.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-context"))
         (session (niwa-session-create "main" workspace))
         side-view
         requests)
    (unwind-protect
        (progn
          (with-temp-file context-file
            (insert "FROZEN INVOCATION CONTEXT"))
          (with-temp-file changed-file
            (insert "CHANGED PARENT CONTEXT"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-context (list context-file)
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (delete-file context-file)
            (with-current-buffer data-buf
              (setq-local gptel-context (list changed-file)
                          gptel-use-context 'system))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&optional _arg)
                         (push (niwa-view-test--dry-run-request-data)
                               requests)
                         (let ((start (point-max)))
                           (goto-char start)
                           (insert "Side answer\n")
                           (put-text-property start (point)
                                              'gptel 'response)))))
              (dolist (prompt '("First side turn" "Second side turn"))
                (with-current-buffer side-view
                  (niwa-view-test--insert-composer-draft prompt)
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-side-conversation-send)))))
            (should (= 2 (length requests)))
            (dolist (request requests)
              (should (string-match-p "FROZEN INVOCATION CONTEXT" request))
              (should-not (string-match-p "CHANGED PARENT CONTEXT" request)))
            (let ((side-data
                   (buffer-local-value 'niwa--data-buffer side-view)))
              (should (eq 'user
                          (buffer-local-value 'gptel-use-context side-data))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "freezes inherited media bytes and removes the side-owned copy"
  (let* ((gptel--known-backends nil)
         (model (make-symbol "btw-media-model"))
         (backend
          (progn
            (put model :capabilities '(media))
            (put model :mime-types '("image/png"))
            (gptel-make-openai
             "btw-media" :host "example.test" :key "test"
             :models (list model))))
         (root (make-temp-file "niwa-btw-media-" t))
         (media-file (file-name-concat root "image.png"))
         (second-media-file (file-name-concat root "second.png"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-media"))
         (session (niwa-session-create "main" workspace))
         side-view
         frozen-paths
         request)
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (with-temp-file second-media-file
            (insert "SECOND MEDIA BYTES"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          gptel-backend backend
                          gptel-model model
                          gptel-context
                          (list (list media-file :mime "image/png")
                                (list second-media-file :mime "image/png"))
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (let ((side-data
                   (buffer-local-value 'niwa--data-buffer side-view)))
              (setq frozen-paths
                    (mapcar #'car
                            (buffer-local-value 'gptel-context side-data)))
              (should
               (equal '("FROZEN MEDIA BYTES" "SECOND MEDIA BYTES")
                      (mapcar
                       (lambda (path)
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string)))
                       frozen-paths)))
              (with-temp-file media-file
                (insert "CHANGED MEDIA BYTES"))
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&optional _arg)
                           (setq request
                                 (niwa-view-test--dry-run-request-data)))))
                (with-current-buffer side-view
                  (niwa-view-test--insert-composer-draft "Inspect the inherited image")
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-side-conversation-send))))
              (should (string-match-p
                       "RlJPWkVOIE1FRElBIEJZVEVT" request))
              (should-not (string-match-p
                           "Q0hBTkdFRCBNRURJQSBCWVRFUw==" request))
              (should (cl-every #'file-exists-p frozen-paths))
              (kill-buffer side-view)
              (should-not (cl-some #'file-exists-p frozen-paths)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "owns an ephemeral one-shot request with only the side tool surface"
  (let* ((root (make-temp-file "niwa-btw-tools-" t))
         (path (file-name-concat root "context.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-tools"))
         (session (niwa-session-create "main" workspace))
         side-view result request)
    (unwind-protect
        (progn
          (with-temp-file path (insert "side pipeline context"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session)
              (insert "*** Parent prompt\n"
                      (propertize "Parent answer\n" 'gptel 'response)))
            (niwa-session-control-transfer-register-root-buffer session data-buf)
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (let ((side-data
                   (buffer-local-value 'niwa--data-buffer side-view)))
              (with-current-buffer side-data
                (should
                 (equal
                  '("Read" "Glob" "Grep" "XrefReferences"
                    "XrefDefinitions" "Imenu" "Treesitter" "WebSearch"
                    "WebFetch" "ApplyPatch" "Bash"
                    "WriteStdin" "ListExecutions" "StopExecution")
                  (mapcar #'gptel-tool-name gptel-tools)))
                (let ((fsm (gptel-make-fsm
                            :state 'WAIT
                            :info (list :buffer side-data))))
                  (niwa-side-conversation--handle-wait fsm)
                  (setq request niwa--current-request)
                  (should (niwa-request-one-shot-mutations-p request))
                  (should (niwa-request-ephemeral-p request))
                  (should (eq fsm (niwa-request-fsm request)))
                  (niwa-pipeline-run-tool
                   (niwa-tool-ensure "Read")
                   (lambda (value) (setq result value))
                   (list :file_path path))
                  (should (string-match-p "side pipeline context" result))
                  (should (eq session
                              (niwa-session-audit-session
                               niwa--session)))
                  (let ((events
                         (append
                          (niwa-session-telemetry-pending session)
                          (with-temp-buffer
                            (insert-file-contents (niwa-telemetry-path session))
                            (let (entries)
                              (condition-case nil
                                  (while t (push (read (current-buffer)) entries))
                                (end-of-file))
                              (nreverse entries))))))
                    (dolist (event '(tool-received tool-finished))
                      (should
                       (cl-find event events
                                :key (lambda (entry)
                                       (plist-get entry :event)))))
                    (should (cl-every
                             (lambda (entry)
                               (eq 'btw
                                   (plist-get entry :conversation-scope)))
                             (cl-remove-if-not
                              (lambda (entry)
                                (memq (plist-get entry :event)
                                      '(tool-received tool-finished)))
                              events)))
                    (should-not
                     (string-match-p
                      (regexp-opt (list path "side pipeline context"))
                      (prin1-to-string events))))
                  (should-not (niwa-session-save-path niwa--session))
                  (niwa-side-conversation--handle-terminal fsm)
                  (should-not niwa--current-request)
                  (should (= 1 (niwa-session-turn-count
                                niwa--session)))
                  (unwind-protect
                      (progn
                        (setf (niwa-session-pending-publication session) '(:batches nil))
                        (should-error (niwa-session-artifacts-assert-mutation-authority
                                       niwa--session) :type 'user-error)
                        (should-error (niwa-session-artifacts-assert-new-mutation-authority
                                       niwa--session) :type 'user-error))
                    (setf (niwa-session-pending-publication session) nil))
                  (unwind-protect
                      (progn
                        (setf (niwa-session-control-transfer session) '(:state quiescing))
                        (should-error (niwa-session-artifacts-assert-new-mutation-authority
                                       niwa--session) :type 'user-error))
                    (setf (niwa-session-control-transfer session) nil))
                  (niwa-session-persistence-lock-release
                   (niwa-session-save-path session) session)
                  (let ((niwa-session-durability--client-id (make-string 64 ?b)))
                    (should (niwa-session-durability-lease-acquire
                             (niwa-session-save-path session) "*foreign parent owner*")))
                  (should-error (niwa-session-artifacts-assert-mutation-authority
                                 niwa--session) :type 'user-error)
                  (should-error (niwa-session-artifacts-assert-new-mutation-authority
                                 niwa--session) :type 'user-error)
                  (should-not (niwa-session-save-path niwa--session)))))))
      (delete-directory root t)))
  :doc "applies the frozen active-request deny to fresh side file mentions"
  (let* ((root (make-temp-file "niwa-btw-file-deny-" t))
         (path (file-name-concat root "secret.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-file-deny"))
         (session (niwa-session-create "main" workspace root))
         side-view
         request)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "SIDE FILE SECRET"))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session)
              (insert "*** Parent prompt\nParent answer\n\n"
                      "*** Active question\n")
              (let* ((response-start (copy-marker (point) nil))
                     (fsm
                      (gptel-make-fsm
                       :state 'TYPE
                       :info
                       (list :buffer data-buf
                             :position response-start
                             :niwa-model-context
                             (buffer-substring (point-min) response-start)))))
                (setq-local
                 niwa--current-request
                 (niwa-request--create
                  :id "btw-file-deny" :session session :fsm fsm
                  :skill-permission-rules
                  '(("Read" :action deny))))))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (niwa-view-test--insert-composer-draft "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&optional _arg)
                         (setq request
                               (niwa-view-test--dry-run-request-data)))))
              (with-current-buffer side-view
                (niwa-view-test--insert-composer-draft "Inspect @file:secret.txt")
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-side-conversation-send))))
            (should request)
            (should-not (string-match-p "SIDE FILE SECRET" request))
            (should (string-match-p "permission denied" request))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "closing aborts active provider work and a pending side permission"
  (let* ((root (make-temp-file "niwa-btw-close-pending-" t))
         (target (file-name-concat root "must-not-exist.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-close-pending"))
         (session (niwa-session-create "main" workspace root))
         side-view side-data side-session fsm result
         (callback-count 0)
         (provider-aborts 0))
    (setf (niwa-session-permission-mode session) 'ask
          (niwa-session-sandbox-mode session) 'off)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--workspace workspace
                        niwa--session session
                        niwa-permission-rules nil
                        niwa-protected-paths nil)
            (insert "*** Parent prompt\n"
                    (propertize "Parent answer\n" 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local niwa--session session)
            (niwa-view-test--insert-composer-draft "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))))
          (setq side-data
                (buffer-local-value 'niwa--data-buffer side-view)
                side-session
                (buffer-local-value 'niwa--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (niwa-side-conversation--handle-wait fsm)
            (let ((niwa-permission-reviewer 'user))
              (niwa-pipeline-run-tool
               (niwa-tool-ensure "Bash")
               (lambda (value)
                 (cl-incf callback-count)
                 (setq result value))
               (list :description "Probe side conversation permission" :command
                     (format "printf pending > %s"
                             (shell-quote-argument target))))))
          (should (= 1 (length
                        (niwa-session-permission-queue side-session))))
          (should-not result)
          (should-not (file-exists-p target))
          (let ((gptel--request-alist (list (list 'provider fsm))))
            (cl-letf (((symbol-function 'gptel-abort)
                       (lambda (buffer)
                         (should (eq side-data buffer))
                         (cl-incf provider-aborts)
                         (setq gptel--request-alist nil))))
              (with-current-buffer side-view
                (niwa-side-conversation-close))))
          (should (= 1 provider-aborts))
          (should (= 1 callback-count))
          (should (string-prefix-p "Error:" result))
          (should-not (niwa-session-permission-queue side-session))
          (should-not (buffer-live-p side-view))
          (should-not (buffer-live-p side-data))
          (should-not (file-exists-p target))
          (with-current-buffer data-buf
            (should-not niwa-side-conversation--side-buffer)))
      (when side-session
        (niwa-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "closing a side stops its still-running Bash process group"
  (let* ((root (make-temp-file "niwa-btw-close-bash-" t))
         (pid-file (file-name-concat root "child.pid"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-close-bash"))
         (session (niwa-session-create "main" workspace root))
         (niwa-execution-process--child-kill-delay 0.05)
         side-view side-data side-session fsm result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setf (niwa-session-permission-mode session) 'ask
          (niwa-session-sandbox-mode session) 'off)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--workspace workspace
                        niwa--session session
                        niwa-permission-rules nil
                        niwa-protected-paths nil)
            (insert "*** Parent prompt\n"
                    (propertize "Parent answer\n" 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local niwa--session session)
            (niwa-view-test--insert-composer-draft "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))))
          (setq side-data
                (buffer-local-value 'niwa--data-buffer side-view)
                side-session
                (buffer-local-value 'niwa--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (niwa-side-conversation--handle-wait fsm)
            (let ((niwa-permission-reviewer 'user))
              (niwa-pipeline-run-tool
               (niwa-tool-ensure "Bash")
               (lambda (value) (setq result value))
               (list
                :description "Probe side conversation cancellation"
                :command
                (format
                 "sh -c %s first %s"
                 (shell-quote-argument
                  "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait")
                 (shell-quote-argument pid-file))
                :yield_time_ms 30000)))
            (let* ((entry
                    (car (niwa-session-permission-queue side-session)))
                   (interaction-id
                    (niwa-queue--entry-metadata-get
                     entry :interaction-id)))
              (should entry)
              (should (plist-get entry :once-only))
              (with-current-buffer side-view
                (let ((overlay
                       (gethash interaction-id
                                niwa-view--interaction-overlays)))
                  (should (overlayp overlay))
                  (niwa--prompt--settle overlay 'allow-once)))))
          (test-niwa-execution--wait
           (lambda ()
             (or result
                 (file-readable-p pid-file)
                 (niwa-execution-session-live-p side-session))))
          (should-not (niwa-session-permission-queue side-session))
          (test-niwa-execution--wait
           (lambda () (or result (file-readable-p pid-file))))
          (should-not result)
          (should (file-readable-p pid-file))
          (setq pid (test-niwa-execution--read-pid pid-file))
          (should (niwa-execution-session-live-p side-session))
          (should-not (test-niwa-execution--process-gone-p pid))
          (kill-buffer side-view)
          (should-not (buffer-live-p side-view))
          (should-not (buffer-live-p side-data))
          (should-not (niwa-execution-session-live-p side-session))
          (should (= 0
                     (hash-table-count
                      (niwa-execution--state-records
                       (niwa-session-execution-state side-session)))))
          (test-niwa-execution--wait
           (lambda () (test-niwa-execution--process-gone-p pid))))
      (when (buffer-live-p side-view)
        (kill-buffer side-view))
      (when side-session
        (niwa-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "Ask-mode Bash mutation still allows once without storing authority"
  (let* ((root (make-temp-file "niwa-btw-bash-once-" t))
         (target (file-name-concat root "approved.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-bash-once"))
         (session (niwa-session-create "main" workspace root))
         side-view side-data side-session fsm result)
    (setf (niwa-session-permission-mode session) 'ask
          (niwa-session-permission-rules session)
          '(("Bash" :action allow))
          (niwa-session-sandbox-mode session) 'off)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--workspace workspace
                        niwa--session session
                        niwa-permission-rules nil
                        niwa-protected-paths nil)
            (insert "*** Parent prompt\n"
                    (propertize "Parent answer\n" 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local niwa--session session)
            (niwa-view-test--insert-composer-draft "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (with-current-buffer (niwa-view-composer-buffer)
                (niwa-view-send))))
          (setq side-data
                (buffer-local-value 'niwa--data-buffer side-view)
                side-session
                (buffer-local-value 'niwa--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (niwa-side-conversation--handle-wait fsm)
            (let ((niwa-permission-reviewer 'user))
              (niwa-pipeline-run-tool
               (niwa-tool-ensure "Bash")
               (lambda (value) (setq result value))
               (list :description "Probe approved side effect" :command
                     (format "printf 'APPROVED SIDE EFFECT' > %s"
                             (shell-quote-argument target))))))
          (should-not result)
          (should-not (file-exists-p target))
          (let* ((entry
                  (car (niwa-session-permission-queue side-session)))
                 (interaction-id
                  (niwa-queue--entry-metadata-get
                   entry :interaction-id)))
            (should entry)
            (should (plist-get entry :once-only))
            (with-current-buffer side-view
              (let ((overlay
                     (gethash interaction-id
                              niwa-view--interaction-overlays))
                    (text
                     (buffer-substring-no-properties
                      (point-min) (niwa-view-transcript-tail))))
                (should (overlayp overlay))
                (should (overlay-get overlay 'niwa-permission-prompt))
                (should (string-search "allow once" text))
                (should-not
                 (string-search "remember selected profile" text))
                (niwa--prompt--settle overlay 'allow-once))))
          (test-niwa-execution--wait (lambda () result))
          (should-not (string-prefix-p "Error:" result))
          (should
           (equal "APPROVED SIDE EFFECT"
                  (with-temp-buffer
                    (insert-file-contents target)
                    (buffer-string))))
          (should-not (niwa-session-permission-queue side-session))
          (should
           (equal '(("Bash" :action allow))
                  (niwa-session-permission-rules side-session)))
          (should
           (equal '(("Bash" :action allow))
                  (niwa-session-permission-rules session)))
          (should-not (niwa-session-resource-grants side-session))
          (should-not
           (niwa-permission-persistence-load-rules workspace))
          (should-not
           (niwa-permission-persistence-load-resource-grants workspace))
          (with-current-buffer side-data
            (niwa-side-conversation--handle-terminal fsm)))
      (when (buffer-live-p side-view)
        (kill-buffer side-view))
      (when side-session
        (niwa-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "ApplyPatch reviews in Edits and applies automatically in Full Access"
  (dolist (mode '(edits full-auto))
    (let* ((root (make-temp-file "niwa-btw-patch-review-" t))
           (path (file-name-concat root "target.txt"))
           (workspace
            (niwa-workspace--create
             :type 'file :id root :root root :name "btw-patch-review"
             :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "main" workspace root))
           (patch
            (string-join
             '("*** Begin Patch"
               "*** Update File: target.txt"
               "@@"
               "-before"
               "+after"
               "*** End Patch")
             "\n"))
           side-view side-data side-session fsm result)
      (setf (niwa-session-permission-mode session) mode
            (niwa-session-permission-rules session)
            '(("ApplyPatch" :action allow)))
      (unwind-protect
          (progn
            (with-temp-file path (insert "before\n"))
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--workspace workspace
                            niwa--session session
                            niwa-permission-rules nil
                            niwa-protected-paths nil)
                (insert "*** Parent prompt\n"
                        (propertize "Parent answer\n" 'gptel 'response)))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (niwa-view-test--insert-composer-draft "/btw")
                (cl-letf (((symbol-function 'pop-to-buffer)
                           (lambda (buffer &rest _)
                             (setq side-view buffer))))
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send))))
              (setq side-data
                    (buffer-local-value 'niwa--data-buffer side-view)
                    side-session
                    (buffer-local-value 'niwa--session side-data)
                    fsm (gptel-make-fsm
                         :state 'TOOL :info (list :buffer side-data)))
              (with-current-buffer side-data
                (niwa-side-conversation--handle-wait fsm)
                (niwa-pipeline-run-tool
                 (niwa-tool-ensure "ApplyPatch")
                 (lambda (value) (setq result value))
                 (list :patch patch)))
              (should-not (niwa-session-permission-queue side-session))
              (if (eq mode 'full-auto)
                  (with-current-buffer side-view
                    (should result)
                    (should-not niwa--prompt-overlays)
                    (should-not
                     (string-search
                      "Permission Request"
                      (buffer-substring-no-properties
                       (point-min) (niwa-view-transcript-tail)))))
                (should-not result)
                (should
                 (equal "before\n"
                        (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
                (with-current-buffer side-view
                  (let ((text
                         (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))
                    (should-not (string-search "Permission Request" text))
                    (should (string-search "ApplyPatch ·" text))
                    (should (string-search "M target.txt" text))
                    (should (= 1 (length niwa--prompt-overlays))))
                  (goto-char (point-min))
                  (search-forward "[ Apply 1 change in 1 file ]")
                  (backward-char 2)
                  (should (eq #'niwa-patch-review-submit
                              (key-binding (kbd "RET"))))
                  (niwa-patch-review-submit)))
              (should (string-search "Applied patch" result))
              (should
               (equal "after\n"
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))))
              (with-current-buffer side-data
                (niwa-side-conversation--handle-terminal fsm))))
        (when (buffer-live-p side-view)
          (kill-buffer side-view))
        (when side-session
          (niwa-execution-teardown-session side-session))
        (when (file-directory-p root)
          (delete-directory root t)))))
  :doc "settles a side request when an earlier terminal handler fails"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-terminal" :root (niwa-test--placeholder-root)
           :name "btw-terminal"))
         (session (niwa-session-create "main" workspace))
         side-view
         warning)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (let ((gptel-send--handlers
             `((WAIT)
               (DONE ,(lambda (_fsm)
                        (error "Injected terminal handler failure")))
               (ERRS)
               (ABRT))))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view-test--insert-composer-draft "/btw")
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (setq side-view buffer))))
            (with-current-buffer (niwa-view-composer-buffer)
              (niwa-view-send)))))
      (let ((side-data
             (buffer-local-value 'niwa--data-buffer side-view)))
        (with-current-buffer side-data
          (let ((fsm (gptel-make-fsm
                      :state 'DONE :info (list :buffer side-data))))
            (niwa-side-conversation--handle-wait fsm)
            (should niwa--current-request)
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (_type message &rest _)
                         (setq warning message))))
              (mapc (lambda (handler) (funcall handler fsm))
                    (cdr (assq 'DONE gptel-send--handlers))))
            (should-not niwa--current-request)
            (should (string-match-p
                     "Injected terminal handler failure" warning)))))))
  :doc "cuts an active parent at complete context and freezes request locals"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-active" :root (niwa-test--placeholder-root) :name "btw-active"))
         (session (niwa-session-create "main" workspace))
         (frozen-memory "/tmp/niwa-btw-frozen-memory/")
         (frozen-temp "/tmp/niwa-btw-frozen-temp/")
         (changed-memory "/tmp/niwa-btw-changed-memory/")
         (changed-temp "/tmp/niwa-btw-changed-temp/")
         side-view
         fsm)
    (setf (niwa-session-permission-rules session)
          '(("Read" :action allow)))
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session
                    gptel-backend 'base-backend
                    gptel-model 'frozen-model
                    gptel-reasoning-effort 'low
                    gptel-system-prompt "Base system"
                    niwa-permission-rules
                    '(("Bash" :pattern "danger*" :action deny))
                    niwa-protected-paths
                    '(("/frozen/**" . inaccessible))
                    niwa-memory-dirs (list frozen-memory)
                    temporary-file-directory frozen-temp)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response))
        (insert "\n*** Stored parent input\n")
        (let ((response-start (copy-marker (point) nil)))
          (let ((response (point)))
            (insert "I will inspect the file.\n")
            (put-text-property response (point) 'gptel 'response))
          (insert "#+begin_tool (Read :file_path \"context.txt\")\n")
          (let ((tool-start (point)))
            (insert "(:name \"Read\" :args (:file_path \"context.txt\"))\n\n"
                    "completed tool result\n")
            (let ((tool-end (point)))
              (insert "#+end_tool\n")
              (put-text-property tool-start tool-end
                                 'gptel '(tool . "btw-read"))))
          (let ((partial-start (point)))
          (insert "Streaming partial answer")
            (put-text-property partial-start (point) 'gptel 'response))
          (setq fsm
                (gptel-make-fsm
                 :state 'TYPE
                  :info (list :buffer data-buf
                             :position response-start
                             :backend 'active-backend
                             :model 'active-model
                             :reasoning-effort 'high
                             :niwa-request-locals
                             '((gptel-backend . active-backend)
                               (gptel-model . active-model)
                               (gptel-reasoning-effort . high)
                               (gptel-system-prompt . "Effective system"))
                             :niwa-model-context
                             (concat
                              "*** Parent prompt\n"
                              (propertize "Parent answer\n"
                                          'gptel 'response)
                              "\n*** Expanded active question\n"
                              "<system-reminder>\n"
                              "Frozen mention content\n"
                              "</system-reminder>\n"))))
          (setq-local niwa--current-request
                      (niwa-request--create
                       :id "btw-active" :session session :fsm fsm
                       :skill-permission-rules
                       '(("Read" :action deny)
                         ("Bash" :action ask)
                         ("WebFetch" :action allow))))))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (let ((side-data
             (buffer-local-value 'niwa--data-buffer side-view)))
        (with-current-buffer side-data
          (should (niwa-side-conversation-parent-active-p))
          (should (string-match-p "Expanded active question"
                                  (buffer-string)))
          (should (string-match-p "Frozen mention content"
                                  (buffer-string)))
          (should-not (string-match-p "Stored parent input"
                                      (buffer-string)))
          (should-not
           (string-match-p "Streaming partial answer" (buffer-string)))
          (should (string-match-p "completed tool result" (buffer-string)))
          (let ((tool-start (string-match "(:name \"Read\"" (buffer-string))))
            (should (equal '(tool . "btw-read")
                           (get-text-property (1+ tool-start) 'gptel))))
          (let ((answer-start (string-match "Parent answer" (buffer-string))))
            (should (eq 'response
                        (get-text-property (1+ answer-start) 'gptel))))
          (should (eq 'active-backend gptel-backend))
          (should (eq 'active-model gptel-model))
          (should (eq 'high gptel-reasoning-effort))
          (should (memq #'niwa-side-conversation--transform-frozen-effort
                        gptel-prompt-transform-functions))
          (let* ((source (current-buffer))
                 (effort-fsm
                  (gptel-make-fsm
                   :state 'WAIT :info (list :buffer source))))
            (with-temp-buffer
              (niwa-side-conversation--transform-frozen-effort effort-fsm)
              (should (eq 'high gptel-reasoning-effort))
              (should (eq 'high
                          (plist-get (gptel-fsm-info effort-fsm)
                                     :reasoning-effort)))))
          (should (string-match-p "Effective system" gptel-system-prompt))
          (should-not (string-match-p "Base system" gptel-system-prompt))
          (should niwa-permission--context-frozen-p)
          (should (equal '(("Bash" :pattern "danger*" :action deny))
                         niwa-permission-rules))
          (should (equal '(("/frozen/**" . inaccessible))
                         niwa-protected-paths)))
        (with-current-buffer side-data
          (let ((side-fsm
                 (gptel-make-fsm
                  :state 'WAIT :info (list :buffer side-data))))
            (niwa-side-conversation--handle-wait side-fsm)
            (should
             (equal '(("Read" :action deny))
                    (niwa-request-skill-permission-rules
                     niwa--current-request)))
            (should
             (eq 'deny
                 (niwa-check-permission
                  "Read"
                  :request-rules
                  (niwa-request-skill-permission-rules
                   niwa--current-request)
                  :session-rules
                  (niwa-session-permission-rules niwa--session)
                  :session niwa--session
                  :path "/tmp/inherited-deny.txt")))
            (niwa-side-conversation--handle-terminal side-fsm))
          (should (string-match-p
                   "inherited parent turn is incomplete"
                   (buffer-string)))
          (should (eq 'reminder
                      (car (car (last (niwa-transcript-segments
                                      (point-min) (point-max)))))))))
      (with-current-buffer data-buf
        (setq-local gptel-model 'changed-model
                    niwa-permission-rules
                    '(("Bash" :action allow))
                    niwa-memory-dirs (list changed-memory)
                    temporary-file-directory changed-temp)
        (setf (niwa-request-skill-permission-rules
               niwa--current-request)
              '(("Read" :action allow))))
      (should
       (eq 'active-model
           (buffer-local-value
            'gptel-model
            (buffer-local-value 'niwa--data-buffer side-view))))
      (should
       (equal '(("Bash" :pattern "danger*" :action deny))
              (buffer-local-value
               'niwa-permission-rules
               (buffer-local-value 'niwa--data-buffer side-view))))
      (with-current-buffer
          (buffer-local-value 'niwa--data-buffer side-view)
        (let ((roots (niwa--all-allowed-roots (current-buffer)))
              (second-fsm
               (gptel-make-fsm
                :state 'WAIT :info (list :buffer (current-buffer)))))
          (should (member frozen-memory roots))
          (should (member frozen-temp roots))
          (should-not (member changed-memory roots))
          (should-not (member changed-temp roots))
          (niwa-side-conversation--handle-wait second-fsm)
          (should
           (equal '(("Read" :action deny))
                  (niwa-request-skill-permission-rules
                   niwa--current-request)))
          (niwa-side-conversation--handle-terminal second-fsm)))))
  :doc "freezes parent tool-safety hooks without inheriting workflow hooks"
  (let* ((root (make-temp-file "niwa-btw-hooks-" t))
         (path (file-name-concat root "context.txt"))
         (workspace
          (niwa-workspace--create
           :type 'file :id root :root root :name "btw-hooks"))
         (session (niwa-session-create "main" workspace))
         (niwa-hook-rules
          '((PreToolUse
             (:matcher "Read"
              :hooks
              ((:type elisp
                :function
                niwa-side-conversation-test--global-allow))))
            (SessionStart
             (:hooks ((:type elisp :function ignore))))))
         parent-request
         side-view
         trace
         result)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "private side hook fixture"))
          (cl-letf
              (((symbol-function
                 'niwa-side-conversation-test--global-allow)
                (lambda (_event)
                  (push 'global trace)
                  '(:permission-decision allow)))
               ((symbol-function
                 'niwa-side-conversation-test--session-allow)
                (lambda (_event)
                  (push 'session trace)
                  '(:permission-decision allow)))
               ((symbol-function
                 'niwa-side-conversation-test--request-deny)
                (lambda (_event)
                  (push 'request trace)
                  '(:permission-decision deny
                    :permission-reason "frozen request safety"))))
            (setf
             (niwa-session-hook-rules session)
             '(("PreToolUse"
                (:matcher "Read"
                 :hooks
                 ((:type "elisp"
                   :function
                   "niwa-side-conversation-test--session-allow"))))
               (UserPromptSubmit
                (:hooks ((:type elisp :function ignore))))))
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--workspace workspace
                            niwa--session session)
                (insert "*** Parent prompt\n"
                        (propertize "Parent answer\n" 'gptel 'response))
                (insert "\n*** Active question\n")
                (let ((response-start (copy-marker (point) nil)))
                  (setq parent-request
                        (niwa-request--create
                         :id "btw-hooks-parent"
                         :session session
                         :fsm
                         (gptel-make-fsm
                          :state 'TYPE
                          :info
                          (list
                           :buffer data-buf
                           :position response-start
                           :niwa-model-context
                           (buffer-substring (point-min) response-start)))
                         :hook-rules
                         '(("PreToolUse"
                            (:matcher "Read"
                             :hooks
                             ((:type "elisp"
                               :function
                               "niwa-side-conversation-test--request-deny"))))
                           ("Stop"
                            (:hooks ((:type elisp :function ignore)))))))
                  (setq-local niwa--current-request parent-request)))
              (with-current-buffer view-buf
                (setq-local niwa--session session)
                (niwa-view-test--insert-composer-draft "/btw")
                (cl-letf (((symbol-function 'pop-to-buffer)
                           (lambda (buffer &rest _)
                             (setq side-view buffer))))
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send))))
              (setq niwa-hook-rules nil)
              (setf (niwa-session-hook-rules session) nil
                    (niwa-request-hook-rules parent-request) nil)
              (let ((side-data
                     (buffer-local-value 'niwa--data-buffer side-view)))
                (with-current-buffer side-data
                  (let ((side-fsm
                         (gptel-make-fsm
                          :state 'WAIT :info (list :buffer side-data))))
                    (niwa-side-conversation--handle-wait side-fsm)
                    (let ((rules
                           (niwa-session-hook-rules niwa--session)))
                      (should (= 3 (cl-count 'PreToolUse rules :key #'car)))
                      (should-not (assq 'SessionStart rules))
                      (should-not (assq 'UserPromptSubmit rules))
                      (should-not (assq 'Stop rules)))
                    (niwa-pipeline-run-tool
                     (niwa-tool-ensure "Read")
                     (lambda (value) (setq result value))
                     (list :file_path path))
                    (should (equal '(global session request)
                                   (nreverse trace)))
                    (should (string-match-p "frozen request safety" result))
                    (should-not
                     (string-match-p "private side hook fixture" result))
                    (should
                     (eq parent-request
                         (buffer-local-value
                          'niwa--current-request data-buf)))
                    (niwa-side-conversation--handle-terminal side-fsm)))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "excludes a pending TOOL confirmation while retaining completed prose"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-pending-tool" :root (niwa-test--placeholder-root)
           :name "btw-pending-tool"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response))
        (insert "\n*** Active question\n")
        (let ((response-start (copy-marker (point) nil)))
          (let ((start (point)))
            (insert "Completed assistant prose before the tool call.\n")
            (put-text-property start (point) 'gptel 'response))
          (let ((pending-start (point)))
            (insert "(Read \"secret.txt\")\n")
            (let* ((tracking (copy-marker (point) t))
                   (prompt-overlay
                    (make-overlay pending-start (point) data-buf t))
                   (tool-overlay
                    (make-overlay response-start (point) data-buf)))
              (overlay-put tool-overlay 'gptel-tool 'pending)
              (overlay-put tool-overlay 'prompt (list prompt-overlay))
              (setq-local
               niwa--current-request
               (niwa-request--create
                :id "btw-pending-tool"
                :session session
                :fsm (gptel-make-fsm
                      :state 'TOOL
                      :info (list :buffer data-buf
                                  :position response-start
                                  :niwa-model-context
                                  (buffer-substring (point-min)
                                                    response-start)
                                  :tracking-marker tracking))))))))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (with-current-buffer
          (buffer-local-value 'niwa--data-buffer side-view)
        (should (= 1
                   (how-many
                    (regexp-quote
                     "Completed assistant prose before the tool call")
                    (point-min) (point-max))))
        (should-not (string-match-p "secret.txt" (buffer-string)))
        (should (string-match-p "inherited parent turn is incomplete"
                                (buffer-string))))))
  :doc "reuses one side and preserves both drafts when inline delivery is refused"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-drafts" :root (niwa-test--placeholder-root) :name "btw-drafts"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq side-view buffer))))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view-test--insert-composer-draft "/btw")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send)))
        (let ((first-side side-view)
              (side-draft "> quoted\nsecond line"))
          (with-current-buffer side-view
            (niwa-view-test--insert-composer-draft side-draft 3))
          (with-current-buffer view-buf
            (niwa-view-test--insert-composer-draft
             "/btw another question" 5)
            (let ((point-before (niwa-view-test--composer-point-offset)))
              (should-error (with-current-buffer (niwa-view-composer-buffer)
                              (niwa-view-send)) :type 'user-error)
              (should (= point-before (niwa-view-test--composer-point-offset)))
              (should (equal "/btw another question"
                             (niwa-view-composer-draft-text nil t)))))
          (should (eq first-side side-view))
          (with-current-buffer side-view
            (should (equal side-draft (niwa-view-composer-draft-text nil t)))
            (should (= 3 (niwa-view-test--composer-point-offset)))
            (let ((error
                   (should-error
                    (with-current-buffer (niwa-view-composer-buffer)
                      (niwa-view-send-follow-up)) :type 'user-error)))
              (should (string-match-p
                       "/btw" (error-message-string error))))
            (should (equal side-draft (niwa-view-composer-draft-text nil t))))
          (with-current-buffer view-buf
            (niwa-view--full-rerender)
            (should (equal "/btw another question"
                           (niwa-view-composer-draft-text nil t))))
          (with-current-buffer side-view
            (should (equal side-draft (niwa-view-composer-draft-text nil t))))))))
  :doc "aborts a response in place and preserves the side draft"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-abort" :root (niwa-test--placeholder-root) :name "btw-abort"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq side-view buffer))))
        (with-current-buffer view-buf
          (setq-local niwa--session session)
          (niwa-view-test--insert-composer-draft "/btw")
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (let* ((side-data
              (buffer-local-value 'niwa--data-buffer side-view))
             (side-session
              (buffer-local-value 'niwa--session side-data))
             (fsm (gptel-make-fsm
                   :state 'TOOL :info (list :buffer side-data)))
             (request
              (niwa-request--create
               :id "btw-tool-wait" :session side-session :turn 1 :fsm fsm))
             (gptel--request-alist nil)
             (cancelled nil)
             (aborted-provider nil)
             (draft "> quoted\nsecond line"))
        (with-current-buffer side-data
          (setq-local niwa--current-request request)
          (niwa-request-push-canceller
           request
           (lambda ()
             (setq cancelled t
                   gptel--request-alist (list (list 'process fsm)))))
          (let ((start (point-max)))
            (goto-char start)
            (insert "Partial side answer")
            (put-text-property start (point) 'gptel 'response)))
        (with-current-buffer side-view
          (niwa-view-test--insert-composer-draft draft 3)
          (should-error (with-current-buffer (niwa-view-composer-buffer)
                          (niwa-side-conversation-send))
                        :type 'user-error)
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (_buffer)
                       (should cancelled)
                       (setq aborted-provider t)
                       (setq gptel--request-alist nil))))
            (niwa-view-abort))
          (should (buffer-live-p side-view))
          (should (equal draft (niwa-view-composer-draft-text nil t)))
          (should (= 3 (niwa-view-test--composer-point-offset))))
        (should (buffer-live-p side-data))
        (with-current-buffer side-data
          (should cancelled)
          (should aborted-provider)
          (should-not niwa--current-request)
          (should-not gptel--request-alist)
          (should (string-match-p
                   "preceding side response was interrupted"
                   (buffer-string)))
          (should (eq 'reminder
                      (car (car (last (niwa-transcript-segments
                                      (point-min) (point-max)))))))))))
  :doc "accepts compacted context without projecting its inherited indicator"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-compact" :root (niwa-test--placeholder-root) :name "btw-compact"))
         (session (niwa-session-create "main" workspace))
         side-view)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "#+begin_summary niwa-role=compaction-summary\n"
                "Only effective compacted context remains.\n"
                "#+end_summary\n"))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (with-current-buffer side-view
        (niwa-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should-not (string-match-p "conversation compacted" text))
          (should-not (string-match-p "Only effective" text))))))
  :doc "discards the side with its parent without listing or persisting it"
  (let* ((workspace
          (niwa-workspace--create
           :type 'file :id "btw-close" :root (niwa-test--placeholder-root) :name "btw-close"))
         (session (niwa-session-create "main" workspace))
         side-view side-data side-session)
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--workspace workspace
                    niwa--session session)
        (insert "*** Parent prompt\n"
                (propertize "Parent answer\n" 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (with-current-buffer (niwa-view-composer-buffer)
            (niwa-view-send))))
      (setq side-data (buffer-local-value 'niwa--data-buffer side-view)
            side-session (buffer-local-value 'niwa--session side-data))
      (should (= 1 (length (niwa--workspace-sessions workspace))))
      (should-not (niwa-session-save-path side-session))
      (kill-buffer data-buf)
      (should-not (buffer-live-p side-view))
      (should-not (buffer-live-p side-data))
      (should-not (niwa-session-save-path side-session)))))

(niwa-deftest niwa-side-conversation/cold-order ()
  ,test
  (test)
  :doc "preserves explicit sandbox policy in the formerly failing case order"
  (let ((source (file-name-concat
                 (file-name-directory (locate-library "niwa"))
                 "test/test-niwa-side-conversation.el")))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory)
              nil t nil "--batch" "-Q" "--eval"
              (prin1-to-string
               `(progn
                  (setq load-path ',load-path)
                  (load ,source nil t)
                  (dolist (name '(niwa-side-conversation--interrupted-boundary/test
                                  niwa-view-send/btw/test@1
                                  niwa-view-send/btw/test@13
                                  niwa-view-send/btw/test@17))
                    (let ((result (ert-run-test (ert-get-test name))))
                      (unless (ert-test-passed-p result)
                        (signal
                         (car (ert-test-result-with-condition-condition result))
                         (cdr (ert-test-result-with-condition-condition result)))))))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))
          (should (equal "" (buffer-string))))))))

(provide 'test-niwa-side-conversation)
;;; test-niwa-side-conversation.el ends here
