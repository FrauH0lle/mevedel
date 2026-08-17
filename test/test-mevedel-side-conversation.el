;;; test-mevedel-side-conversation.el -- Side conversation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Acceptance tests for ephemeral `/btw' side conversations.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(defvar mevedel-plugin-extra-roots)
(defvar org-mode-hook)

(require 'gptel-context)
(require 'gptel-openai)
(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
(require 'mevedel-view-composer)

(declare-function gptel-make-openai "ext:gptel-openai" (name &rest args))

(defvar gptel--known-backends)

(mevedel-deftest mevedel-view-send/btw (:quiet t)
  ,test
  (test)
  :doc "rejects /btw before the parent has accepted a prompt"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-empty" :root "/tmp" :name "btw-empty"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (should-error (mevedel-view-send) :type 'user-error)
        (should (equal "/btw" (mevedel-view--input-text))))))
  :doc "rejects /btw during a directive-owned active request"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-directive" :root "/tmp"
           :name "btw-directive"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response))
        (insert "\n*** Active directive question\n")
        (let ((response-start (copy-marker (point) nil)))
          (setq-local
           mevedel--current-request
           (mevedel-request--create
            :id "btw-directive"
            :session session
            :directive-uuid "directive-1"
            :fsm (gptel-make-fsm
                  :state 'TYPE
                  :info (list :buffer data-buf
                              :position response-start))))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw Explain without changing the directive")
        (let ((point-before (point))
              (error (should-error (mevedel-view-send)
                                   :type 'user-error)))
          (should (string-match-p "directive"
                                  (error-message-string error)))
          (should (= point-before (point)))
          (should (equal "/btw Explain without changing the directive"
                         (mevedel-view--input-text)))))
      (with-current-buffer data-buf
        (should-not mevedel-side-conversation--side-buffer))))
  :doc "waits for asynchronous inherited gptel context formatting"
  (let* ((temporary-file-directory
          (file-name-as-directory (expand-file-name "~")))
         ;; Eask changes HOME after Emacs initializes its abbreviation cache.
         (abbreviated-home-dir nil)
         (root (make-temp-file "mevedel-btw-async-context-" t))
         (context-file (file-name-concat root "context.txt"))
         (buffer-file (file-name-concat root "buffer.txt"))
         context-buffer
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-async-context"))
         (session (mevedel-session-create "main" workspace))
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
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
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
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw Explain the frozen context")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer)))
                        ((symbol-function 'gptel-send)
                         (lambda (&optional _arg)
                           (setq request
                                 (mevedel-view-test--dry-run-request-data)))))
                (mevedel-view-send)
                (should formatter-callback)
                (should formatter-contexts)
                (should-not side-view)
                (should (equal "/btw Explain the frozen context"
                               (mevedel-view--input-text)))
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
              (should (string-empty-p (mevedel-view--input-text))))
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
            (should-not (string-match-p "mevedel-btw-context-" request))
            (should (string-match-p "Explain the frozen context" request))
            (let* ((side-data
                    (buffer-local-value 'mevedel--data-buffer side-view))
                   (frozen
                    (buffer-local-value
                     'mevedel-side-conversation--frozen-context side-data)))
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
                       mevedel-side-conversation--side-buffer))))
      (when (buffer-live-p context-buffer)
        (kill-buffer context-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))
  :doc "preserves parent edits while asynchronous context is pending"
  (let* ((root (make-temp-file "mevedel-btw-async-draft-" t))
         (context-file (file-name-concat root "context.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-async-draft"))
         (session (mevedel-session-create "main" workspace))
         formatter-callback
         side-view)
    (unwind-protect
        (progn
          (with-temp-file context-file
            (insert "Context"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          gptel-context (list context-file)
                          gptel-use-context 'user
                          gptel-context-string-function
                          (lambda (callback _contexts)
                            (setq formatter-callback callback)))
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer)))
                      ((symbol-function 'gptel-send)
                       (lambda (&optional _arg))))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert "/btw Initial question")
                (mevedel-view-send))
              (with-current-buffer data-buf
                (should-error
                 (mevedel-side-conversation-open "Duplicate question")
                 :type 'user-error))
              (with-current-buffer view-buf
                (let ((inhibit-read-only t))
                  (delete-region (mevedel-view--input-start) (point-max)))
                (mevedel-view-test--insert-composer-draft
                 "> quoted\nsecond line" 4))
              (funcall formatter-callback "FROZEN ASYNC CONTEXT"))
            (should (buffer-live-p side-view))
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line"
                             (buffer-substring-no-properties
                              (mevedel-view--input-start) (point-max))))
              (should (= 4 (- (point) (mevedel-view--input-start)))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "cleans async context after formatter quit or side mode failure"
  (dolist (failure '(formatter-quit mode-failure))
    (let* ((root (make-temp-file "mevedel-btw-async-failure-" t))
           (media-file (file-name-concat root "image.png"))
           (workspace
            (mevedel-workspace--create
             :type 'file :id root :root root :name "btw-async-failure"))
           (session (mevedel-session-create "main" workspace))
           formatter-callback
           frozen-path
           pending-side)
      (unwind-protect
          (progn
            (with-temp-file media-file
              (insert "FROZEN MEDIA BYTES"))
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--workspace workspace
                            mevedel--session session
                            gptel-context
                            (list (list media-file :mime "image/png"))
                            gptel-use-context 'user
                            gptel-context-string-function
                            (lambda (callback contexts)
                              (setq pending-side
                                    (buffer-local-value
                                     'mevedel-side-conversation--side-buffer
                                     data-buf)
                                    frozen-path (caar contexts))
                              (if (eq failure 'formatter-quit)
                                  (signal 'quit nil)
                                (setq formatter-callback callback))))
                (insert "*** Parent prompt\n")
                (let ((start (point)))
                  (insert "Parent answer\n")
                  (put-text-property start (point) 'gptel 'response)))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert "/btw")
                (if (eq failure 'formatter-quit)
                    (let (quit-seen)
                      (condition-case nil
                          (mevedel-view-send)
                        (quit (setq quit-seen t)))
                      (should quit-seen))
                  (mevedel-view-send)
                  (should formatter-callback)
                  (should (file-exists-p frozen-path))
                  (require 'mevedel-utilities)
                  (let ((mode-function
                         (symbol-function 'mevedel--transcript-org-mode)))
                    (cl-letf
                        (((symbol-function 'mevedel--transcript-org-mode)
                          (lambda ()
                            (funcall mode-function)
                            (error "Injected side mode failure"))))
                      (should-error
                       (funcall formatter-callback "Frozen context"))))))
              (should frozen-path)
              (should-not (file-exists-p frozen-path))
              (should-not (buffer-live-p pending-side))
              (with-current-buffer data-buf
                (should-not mevedel-side-conversation--side-buffer))))
        (when (file-exists-p frozen-path)
          (delete-file frozen-path))
        (when (file-directory-p root)
          (delete-directory root t)))))
  :doc "cleans frozen media when a pending asynchronous open loses its parent"
  (let* ((root (make-temp-file "mevedel-btw-async-close-" t))
         (media-file (file-name-concat root "image.png"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-async-close"))
         (session (mevedel-session-create "main" workspace))
         formatter-callback
         frozen-path
         pending-side)
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          gptel-context
                          (list (list media-file :mime "image/png"))
                          gptel-use-context 'user
                          gptel-context-string-function
                          (lambda (callback _contexts)
                            (setq formatter-callback callback)))
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (mevedel-view-send))
            (with-current-buffer data-buf
              (setq pending-side mevedel-side-conversation--side-buffer))
            (setq frozen-path
                  (car
                   (car
                    (plist-get
                     (buffer-local-value
                      'mevedel-side-conversation--frozen-context
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
  (let* ((root (make-temp-file "mevedel-btw-create-failure-" t))
         (media-file (file-name-concat root "image.png"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-create-failure"))
         (session (mevedel-session-create "main" workspace))
         (before
          (directory-files temporary-file-directory t
                           "\\`mevedel-btw-context-")))
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          gptel-context
                          (list (list media-file :mime "image/png"))
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'mevedel-session-create)
                         (lambda (&rest _)
                           (error "Injected side session creation failure"))))
                (should-error (mevedel-view-send))))
            (should
             (equal before
                    (directory-files temporary-file-directory t
                                     "\\`mevedel-btw-context-")))))
      (dolist (file
               (cl-set-difference
                (directory-files temporary-file-directory t
                                 "\\`mevedel-btw-context-")
                before :test #'equal))
        (ignore-errors (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "opens a paired ephemeral side while hiding inherited context"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw" :root "/tmp" :name "btw"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (setf (mevedel-session-plan-mode session) t)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send))
        (should (string-empty-p (mevedel-view--input-text))))
      (should (buffer-live-p side-view))
      (should-not (eq side-view view-buf))
      (let ((side-data
             (buffer-local-value 'mevedel--data-buffer side-view)))
        (should (buffer-live-p side-data))
        (with-current-buffer side-data
          (should-not (mevedel-session-plan-mode mevedel--session))
          (should (string-match-p "Parent prompt" (buffer-string)))
          (should (string-match-p "Parent answer" (buffer-string))))
        (with-current-buffer side-view
          (should (string-match-p "btw" (buffer-string)))
          (should-not (string-match-p "Parent prompt" (buffer-string)))
          (should-not (string-match-p "Parent answer" (buffer-string)))
          (should-not (memq #'mevedel-agent-capf
                            completion-at-point-functions))
          (goto-char (mevedel-view--input-start))
          (insert "@")
          (let ((completion (mevedel-mention-capf)))
            (should-not (member "@agent:" (nth 2 completion))))
          (delete-region (mevedel-view--input-start) (point-max))))
      (with-current-buffer data-buf
        (should (mevedel-session-plan-mode session))
        (should (equal "*** Parent prompt\nParent answer\n"
                       (buffer-string))))))
  :doc "preserves a materialized list-valued gptel system prompt"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-system-list" :root "/tmp"
           :name "btw-system-list"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session
                    gptel-system-prompt
                    '("Base system message"
                      "Template user turn"
                      "Template assistant response"))
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send)))
      (let ((side-system
             (buffer-local-value
              'gptel-system-prompt
              (buffer-local-value 'mevedel--data-buffer side-view))))
        (should (listp side-system))
        (should (string-prefix-p "Base system message"
                                 (car side-system)))
        (should (string-match-p "ephemeral /btw"
                                (car side-system)))
        (should (equal '("Template user turn"
                         "Template assistant response")
                       (cdr side-system))))))
  :doc "submits an inline prompt and independent multi-turn follow-up"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-multi" :root "/tmp" :name "btw-multi"))
         (session (mevedel-session-create "main" workspace))
         side-view
         requests)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
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
          (setq-local mevedel--session session)
          (goto-char (mevedel-view--input-start))
          (insert "/btw First side prompt")
          (mevedel-view-send))
        (with-current-buffer side-view
          (goto-char (mevedel-view--input-start))
          (insert "Second side prompt")
          (mevedel-side-conversation-send)))
      (should (= 2 (length requests)))
      (should (string-match-p "Parent prompt" (cadr requests)))
      (should (string-match-p "First side prompt" (cadr requests)))
      (should (string-match-p "Side answer 1" (car requests)))
      (should (string-match-p "Second side prompt" (car requests)))
      (with-current-buffer data-buf
        (should (equal "*** Parent prompt\nParent answer\n"
                       (buffer-string))))))
  :doc "freezes inherited gptel context across side turns"
  (let* ((root (make-temp-file "mevedel-btw-context-" t))
         (context-file (file-name-concat root "context.txt"))
         (changed-file (file-name-concat root "changed.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-context"))
         (session (mevedel-session-create "main" workspace))
         side-view
         requests)
    (unwind-protect
        (progn
          (with-temp-file context-file
            (insert "FROZEN INVOCATION CONTEXT"))
          (with-temp-file changed-file
            (insert "CHANGED PARENT CONTEXT"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          gptel-context (list context-file)
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (mevedel-view-send)))
            (delete-file context-file)
            (with-current-buffer data-buf
              (setq-local gptel-context (list changed-file)
                          gptel-use-context 'system))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&optional _arg)
                         (push (mevedel-view-test--dry-run-request-data)
                               requests)
                         (let ((start (point-max)))
                           (goto-char start)
                           (insert "Side answer\n")
                           (put-text-property start (point)
                                              'gptel 'response)))))
              (dolist (prompt '("First side turn" "Second side turn"))
                (with-current-buffer side-view
                  (goto-char (mevedel-view--input-start))
                  (insert prompt)
                  (mevedel-side-conversation-send))))
            (should (= 2 (length requests)))
            (dolist (request requests)
              (should (string-match-p "FROZEN INVOCATION CONTEXT" request))
              (should-not (string-match-p "CHANGED PARENT CONTEXT" request)))
            (let ((side-data
                   (buffer-local-value 'mevedel--data-buffer side-view)))
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
         (root (make-temp-file "mevedel-btw-media-" t))
         (media-file (file-name-concat root "image.png"))
         (second-media-file (file-name-concat root "second.png"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-media"))
         (session (mevedel-session-create "main" workspace))
         side-view
         frozen-paths
         request)
    (unwind-protect
        (progn
          (with-temp-file media-file
            (insert "FROZEN MEDIA BYTES"))
          (with-temp-file second-media-file
            (insert "SECOND MEDIA BYTES"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          gptel-backend backend
                          gptel-model model
                          gptel-context
                          (list (list media-file :mime "image/png")
                                (list second-media-file :mime "image/png"))
                          gptel-use-context 'user)
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (mevedel-view-send)))
            (let ((side-data
                   (buffer-local-value 'mevedel--data-buffer side-view)))
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
                                 (mevedel-view-test--dry-run-request-data)))))
                (with-current-buffer side-view
                  (goto-char (mevedel-view--input-start))
                  (insert "Inspect the inherited image")
                  (mevedel-side-conversation-send)))
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
  (let* ((root (make-temp-file "mevedel-btw-tools-" t))
         (path (file-name-concat root "context.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-tools"))
         (session (mevedel-session-create "main" workspace))
         side-view result request)
    (unwind-protect
        (progn
          (with-temp-file path (insert "side pipeline context"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session)
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (mevedel-view-send)))
            (let ((side-data
                   (buffer-local-value 'mevedel--data-buffer side-view)))
              (with-current-buffer side-data
                (should
                 (equal
                  '("Read" "Glob" "Grep" "XrefReferences"
                    "XrefDefinitions" "Imenu" "Treesitter" "WebSearch"
                    "WebFetch" "YouTube" "ApplyPatch" "Bash"
                    "WriteStdin" "ListExecutions" "StopExecution")
                  (mapcar #'gptel-tool-name gptel-tools)))
                (let ((fsm (gptel-make-fsm
                            :state 'WAIT
                            :info (list :buffer side-data))))
                  (mevedel-side-conversation--handle-wait fsm)
                  (setq request mevedel--current-request)
                  (should (mevedel-request-one-shot-mutations-p request))
                  (should (mevedel-request-ephemeral-p request))
                  (should (eq fsm (mevedel-request-fsm request)))
                  (mevedel-pipeline-run-tool
                   (mevedel-tool-ensure "Read")
                   (lambda (value) (setq result value))
                   (list :file_path path))
                  (should (string-match-p "side pipeline context" result))
                  (should (eq session
                              (mevedel-session-audit-session
                               mevedel--session)))
                  (let ((events
                         (mevedel-session-telemetry-pending session)))
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
                  (should-not (mevedel-session-save-path mevedel--session))
                  (mevedel-side-conversation--handle-terminal fsm)
                  (should-not mevedel--current-request)
                  (should (= 1 (mevedel-session-turn-count
                                mevedel--session))))))))
      (delete-directory root t)))
  :doc "applies the frozen active-request deny to fresh side file mentions"
  (let* ((root (make-temp-file "mevedel-btw-file-deny-" t))
         (path (file-name-concat root "secret.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-file-deny"))
         (session (mevedel-session-create "main" workspace root))
         side-view
         request)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "SIDE FILE SECRET"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session)
              (insert "*** Parent prompt\nParent answer\n\n"
                      "*** Active question\n")
              (let* ((response-start (copy-marker (point) nil))
                     (fsm
                      (gptel-make-fsm
                       :state 'TYPE
                       :info
                       (list :buffer data-buf
                             :position response-start
                             :mevedel-model-context
                             (buffer-substring (point-min) response-start)))))
                (setq-local
                 mevedel--current-request
                 (mevedel-request--create
                  :id "btw-file-deny" :session session :fsm fsm
                  :skill-permission-rules
                  '(("Read" :action deny))))))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (mevedel-view-send)))
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda (&optional _arg)
                         (setq request
                               (mevedel-view-test--dry-run-request-data)))))
              (with-current-buffer side-view
                (goto-char (mevedel-view--input-start))
                (insert "Inspect @file:secret.txt")
                (mevedel-side-conversation-send)))
            (should request)
            (should-not (string-match-p "SIDE FILE SECRET" request))
            (should (string-match-p "permission denied" request))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "closing aborts active provider work and a pending side permission"
  (let* ((root (make-temp-file "mevedel-btw-close-pending-" t))
         (target (file-name-concat root "must-not-exist.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-close-pending"))
         (session (mevedel-session-create "main" workspace root))
         side-view side-data side-session fsm result
         (callback-count 0)
         (provider-aborts 0))
    (setf (mevedel-session-permission-mode session) 'full-auto
          (mevedel-session-sandbox-mode session) 'off)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--workspace workspace
                        mevedel--session session
                        mevedel-permission-rules nil
                        mevedel-protected-paths nil)
            (insert "*** Parent prompt\n")
            (let ((start (point)))
              (insert "Parent answer\n")
              (put-text-property start (point) 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (goto-char (mevedel-view--input-start))
            (insert "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (mevedel-view-send)))
          (setq side-data
                (buffer-local-value 'mevedel--data-buffer side-view)
                side-session
                (buffer-local-value 'mevedel--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (mevedel-side-conversation--handle-wait fsm)
            (let ((mevedel-permission-guardian nil))
              (mevedel-pipeline-run-tool
               (mevedel-tool-ensure "Bash")
               (lambda (value)
                 (cl-incf callback-count)
                 (setq result value))
               (list :command
                     (format "printf pending > %s"
                             (shell-quote-argument target))))))
          (should (= 1 (length
                        (mevedel-session-permission-queue side-session))))
          (should-not result)
          (should-not (file-exists-p target))
          (let ((gptel--request-alist (list (list 'provider fsm))))
            (cl-letf (((symbol-function 'gptel-abort)
                       (lambda (buffer)
                         (should (eq side-data buffer))
                         (cl-incf provider-aborts)
                         (setq gptel--request-alist nil))))
              (kill-buffer side-view)))
          (should (= 1 provider-aborts))
          (should (= 1 callback-count))
          (should (string-prefix-p "Error:" result))
          (should-not (mevedel-session-permission-queue side-session))
          (should-not (buffer-live-p side-view))
          (should-not (buffer-live-p side-data))
          (should-not (file-exists-p target))
          (with-current-buffer data-buf
            (should-not mevedel-side-conversation--side-buffer)))
      (when side-session
        (mevedel-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "closing a side stops its still-running Bash process group"
  (let* ((root (make-temp-file "mevedel-btw-close-bash-" t))
         (pid-file (file-name-concat root "child.pid"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-close-bash"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-execution--child-kill-delay 0.05)
         side-view side-data side-session fsm result pid)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setf (mevedel-session-permission-mode session) 'full-auto
          (mevedel-session-sandbox-mode session) 'off)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--workspace workspace
                        mevedel--session session
                        mevedel-permission-rules nil
                        mevedel-protected-paths nil)
            (insert "*** Parent prompt\n")
            (let ((start (point)))
              (insert "Parent answer\n")
              (put-text-property start (point) 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (goto-char (mevedel-view--input-start))
            (insert "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (mevedel-view-send)))
          (setq side-data
                (buffer-local-value 'mevedel--data-buffer side-view)
                side-session
                (buffer-local-value 'mevedel--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (mevedel-side-conversation--handle-wait fsm)
            (let ((mevedel-permission-guardian nil))
              (mevedel-pipeline-run-tool
               (mevedel-tool-ensure "Bash")
               (lambda (value) (setq result value))
               (list
                :command
                (format
                 "sh -c %s first %s"
                 (shell-quote-argument
                  "sleep 30 & child=$!; printf '%s' \"$child\" > \"$1\"; wait")
                 (shell-quote-argument pid-file))
                :yield-time_ms 30000)))
            (let* ((entry
                    (car (mevedel-session-permission-queue side-session)))
                   (interaction-id
                    (mevedel-queue--entry-metadata-get
                     entry :interaction-id)))
              (should entry)
              (should (plist-get entry :once-only))
              (with-current-buffer side-view
                (let ((overlay
                       (gethash interaction-id
                                mevedel-view--interaction-overlays)))
                  (should (overlayp overlay))
                  (mevedel--prompt--settle overlay 'allow-once)))))
          (test-mevedel-execution--wait
           (lambda ()
             (or result
                 (file-readable-p pid-file)
                 (mevedel-execution-session-live-p side-session))))
          (should-not (mevedel-session-permission-queue side-session))
          (test-mevedel-execution--wait
           (lambda () (or result (file-readable-p pid-file))))
          (should-not result)
          (should (file-readable-p pid-file))
          (setq pid (test-mevedel-execution--read-pid pid-file))
          (should (mevedel-execution-session-live-p side-session))
          (should-not (test-mevedel-execution--process-gone-p pid))
          (kill-buffer side-view)
          (should-not (buffer-live-p side-view))
          (should-not (buffer-live-p side-data))
          (should-not (mevedel-execution-session-live-p side-session))
          (should (= 0
                     (hash-table-count
                      (mevedel-execution--state-records
                       (mevedel-session-execution-state side-session)))))
          (test-mevedel-execution--wait
           (lambda () (test-mevedel-execution--process-gone-p pid))))
      (when (buffer-live-p side-view)
        (kill-buffer side-view))
      (when side-session
        (mevedel-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "Full Auto Bash mutation still allows once without storing authority"
  (let* ((root (make-temp-file "mevedel-btw-bash-once-" t))
         (target (file-name-concat root "approved.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-bash-once"))
         (session (mevedel-session-create "main" workspace root))
         side-view side-data side-session fsm result)
    (setf (mevedel-session-permission-mode session) 'full-auto
          (mevedel-session-permission-rules session)
          '(("Bash" :action allow))
          (mevedel-session-sandbox-mode session) 'off)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--workspace workspace
                        mevedel--session session
                        mevedel-permission-rules nil
                        mevedel-protected-paths nil)
            (insert "*** Parent prompt\n")
            (let ((start (point)))
              (insert "Parent answer\n")
              (put-text-property start (point) 'gptel 'response)))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (goto-char (mevedel-view--input-start))
            (insert "/btw")
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq side-view buffer))))
              (mevedel-view-send)))
          (setq side-data
                (buffer-local-value 'mevedel--data-buffer side-view)
                side-session
                (buffer-local-value 'mevedel--session side-data)
                fsm (gptel-make-fsm
                     :state 'TOOL :info (list :buffer side-data)))
          (with-current-buffer side-data
            (mevedel-side-conversation--handle-wait fsm)
            (let ((mevedel-permission-guardian nil))
              (mevedel-pipeline-run-tool
               (mevedel-tool-ensure "Bash")
               (lambda (value) (setq result value))
               (list :command
                     (format "printf 'APPROVED SIDE EFFECT' > %s"
                             (shell-quote-argument target))))))
          (should-not result)
          (should-not (file-exists-p target))
          (let* ((entry
                  (car (mevedel-session-permission-queue side-session)))
                 (interaction-id
                  (mevedel-queue--entry-metadata-get
                   entry :interaction-id)))
            (should entry)
            (should (plist-get entry :once-only))
            (with-current-buffer side-view
              (let ((overlay
                     (gethash interaction-id
                              mevedel-view--interaction-overlays))
                    (text
                     (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
                (should (overlayp overlay))
                (should (overlay-get overlay 'mevedel-permission-prompt))
                (should (string-search "allow once" text))
                (should-not
                 (string-search "remember selected profile" text))
                (mevedel--prompt--settle overlay 'allow-once))))
          (test-mevedel-execution--wait (lambda () result))
          (should-not (string-prefix-p "Error:" result))
          (should
           (equal "APPROVED SIDE EFFECT"
                  (with-temp-buffer
                    (insert-file-contents target)
                    (buffer-string))))
          (should-not (mevedel-session-permission-queue side-session))
          (should
           (equal '(("Bash" :action allow))
                  (mevedel-session-permission-rules side-session)))
          (should
           (equal '(("Bash" :action allow))
                  (mevedel-session-permission-rules session)))
          (should-not (mevedel-session-resource-grants side-session))
          (should-not
           (mevedel-permission--load-persistent-rules workspace))
          (should-not
           (mevedel-permission--load-persistent-resource-grants workspace))
          (with-current-buffer side-data
            (mevedel-side-conversation--handle-terminal fsm)))
      (when (buffer-live-p side-view)
        (kill-buffer side-view))
      (when side-session
        (mevedel-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "ApplyPatch reaches mandatory review despite inherited Full Auto"
  (let* ((root (make-temp-file "mevedel-btw-patch-review-" t))
         (path (file-name-concat root "target.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-patch-review"
           :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session-create "main" workspace root))
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
    (setf (mevedel-session-permission-mode session) 'full-auto
          (mevedel-session-permission-rules session)
          '(("ApplyPatch" :action allow)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "before\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          mevedel-permission-rules nil
                          mevedel-protected-paths nil)
              (insert "*** Parent prompt\n")
              (let ((start (point)))
                (insert "Parent answer\n")
                (put-text-property start (point) 'gptel 'response)))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert "/btw")
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _)
                           (setq side-view buffer))))
                (mevedel-view-send)))
            (setq side-data
                  (buffer-local-value 'mevedel--data-buffer side-view)
                  side-session
                  (buffer-local-value 'mevedel--session side-data)
                  fsm (gptel-make-fsm
                       :state 'TOOL :info (list :buffer side-data)))
            (with-current-buffer side-data
              (mevedel-side-conversation--handle-wait fsm)
              (mevedel-pipeline-run-tool
               (mevedel-tool-ensure "ApplyPatch")
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should-not result)
            (should-not (mevedel-session-permission-queue side-session))
            (should
             (equal "before\n"
                    (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
            (with-current-buffer side-view
              (let ((text
                     (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker)))
                (should-not (string-search "Permission Request" text))
                (should (string-search "ApplyPatch ·" text))
                (should (string-search "M target.txt" text))
                (should (= 1 (length mevedel--prompt-overlays))))
              (goto-char (point-min))
              (search-forward "[ Apply 1 change in 1 file ]")
              (backward-char 2)
              (should (eq #'mevedel-patch-review-submit
                          (key-binding (kbd "RET"))))
              (mevedel-patch-review-submit))
            (should (string-search "Applied patch" result))
            (should
             (equal "after\n"
                    (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
            (with-current-buffer side-data
              (mevedel-side-conversation--handle-terminal fsm))))
      (when (buffer-live-p side-view)
        (kill-buffer side-view))
      (when side-session
        (mevedel-execution-teardown-session side-session))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "settles a side request when an earlier terminal handler fails"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-terminal" :root "/tmp"
           :name "btw-terminal"))
         (session (mevedel-session-create "main" workspace))
         side-view
         warning)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (let ((gptel-send--handlers
             `((WAIT)
               (DONE ,(lambda (_fsm)
                        (error "Injected terminal handler failure")))
               (ERRS)
               (ABRT))))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (goto-char (mevedel-view--input-start))
          (insert "/btw")
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (setq side-view buffer))))
            (mevedel-view-send))))
      (let ((side-data
             (buffer-local-value 'mevedel--data-buffer side-view)))
        (with-current-buffer side-data
          (let ((fsm (gptel-make-fsm
                      :state 'DONE :info (list :buffer side-data))))
            (mevedel-side-conversation--handle-wait fsm)
            (should mevedel--current-request)
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (_type message &rest _)
                         (setq warning message))))
              (mapc (lambda (handler) (funcall handler fsm))
                    (cdr (assq 'DONE gptel-send--handlers))))
            (should-not mevedel--current-request)
            (should (string-match-p
                     "Injected terminal handler failure" warning)))))))
  :doc "cuts an active parent at complete context and freezes request locals"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-active" :root "/tmp" :name "btw-active"))
         (session (mevedel-session-create "main" workspace))
         (frozen-memory "/tmp/mevedel-btw-frozen-memory/")
         (frozen-temp "/tmp/mevedel-btw-frozen-temp/")
         (changed-memory "/tmp/mevedel-btw-changed-memory/")
         (changed-temp "/tmp/mevedel-btw-changed-temp/")
         side-view
         fsm)
    (setf (mevedel-session-permission-rules session)
          '(("Read" :action allow)))
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session
                    gptel-backend 'base-backend
                    gptel-model 'frozen-model
                    gptel-reasoning-effort 'low
                    gptel-system-prompt "Base system"
                    mevedel-permission-rules
                    '(("Bash" :pattern "danger*" :action deny))
                    mevedel-protected-paths
                    '(("/frozen/**" . inaccessible))
                    mevedel-memory-dirs (list frozen-memory)
                    temporary-file-directory frozen-temp)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response))
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
                             :mevedel-request-locals
                             '((gptel-backend . active-backend)
                               (gptel-model . active-model)
                               (gptel-reasoning-effort . high)
                               (gptel-system-prompt . "Effective system"))
                             :mevedel-model-context
                             (concat
                              "*** Parent prompt\n"
                              (propertize "Parent answer\n"
                                          'gptel 'response)
                              "\n*** Expanded active question\n"
                              "<system-reminder>\n"
                              "Frozen mention content\n"
                              "</system-reminder>\n"))))
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :id "btw-active" :session session :fsm fsm
                       :skill-permission-rules
                       '(("Read" :action deny)
                         ("Bash" :action ask)
                         ("WebFetch" :action allow))))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send)))
      (let ((side-data
             (buffer-local-value 'mevedel--data-buffer side-view)))
        (with-current-buffer side-data
          (should (mevedel-side-conversation-parent-active-p))
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
          (should (memq #'mevedel-side-conversation--transform-frozen-effort
                        gptel-prompt-transform-functions))
          (let* ((source (current-buffer))
                 (effort-fsm
                  (gptel-make-fsm
                   :state 'WAIT :info (list :buffer source))))
            (with-temp-buffer
              (mevedel-side-conversation--transform-frozen-effort effort-fsm)
              (should (eq 'high gptel-reasoning-effort))
              (should (eq 'high
                          (plist-get (gptel-fsm-info effort-fsm)
                                     :reasoning-effort)))))
          (should (string-match-p "Effective system" gptel-system-prompt))
          (should-not (string-match-p "Base system" gptel-system-prompt))
          (should mevedel-permission--context-frozen-p)
          (should (equal '(("Bash" :pattern "danger*" :action deny))
                         mevedel-permission-rules))
          (should (equal '(("/frozen/**" . inaccessible))
                         mevedel-protected-paths)))
        (with-current-buffer side-data
          (let ((side-fsm
                 (gptel-make-fsm
                  :state 'WAIT :info (list :buffer side-data))))
            (mevedel-side-conversation--handle-wait side-fsm)
            (should
             (equal '(("Read" :action deny))
                    (mevedel-request-skill-permission-rules
                     mevedel--current-request)))
            (should
             (eq 'deny
                 (mevedel-check-permission
                  "Read"
                  :request-rules
                  (mevedel-request-skill-permission-rules
                   mevedel--current-request)
                  :session-rules
                  (mevedel-session-permission-rules mevedel--session)
                  :session mevedel--session
                  :path "/tmp/inherited-deny.txt")))
            (mevedel-side-conversation--handle-terminal side-fsm))
          (should (string-match-p
                   "inherited parent turn is incomplete"
                   (buffer-string)))
          (should (eq 'reminder
                      (car (car (last (mevedel-transcript-segments
                                      (point-min) (point-max)))))))))
      (with-current-buffer data-buf
        (setq-local gptel-model 'changed-model
                    mevedel-permission-rules
                    '(("Bash" :action allow))
                    mevedel-memory-dirs (list changed-memory)
                    temporary-file-directory changed-temp)
        (setf (mevedel-request-skill-permission-rules
               mevedel--current-request)
              '(("Read" :action allow))))
      (should
       (eq 'active-model
           (buffer-local-value
            'gptel-model
            (buffer-local-value 'mevedel--data-buffer side-view))))
      (should
       (equal '(("Bash" :pattern "danger*" :action deny))
              (buffer-local-value
               'mevedel-permission-rules
               (buffer-local-value 'mevedel--data-buffer side-view))))
      (with-current-buffer
          (buffer-local-value 'mevedel--data-buffer side-view)
        (let ((roots (mevedel--all-allowed-roots (current-buffer)))
              (second-fsm
               (gptel-make-fsm
                :state 'WAIT :info (list :buffer (current-buffer)))))
          (should (member frozen-memory roots))
          (should (member frozen-temp roots))
          (should-not (member changed-memory roots))
          (should-not (member changed-temp roots))
          (mevedel-side-conversation--handle-wait second-fsm)
          (should
           (equal '(("Read" :action deny))
                  (mevedel-request-skill-permission-rules
                   mevedel--current-request)))
          (mevedel-side-conversation--handle-terminal second-fsm)))))
  :doc "freezes parent tool-safety hooks without inheriting workflow hooks"
  (let* ((root (make-temp-file "mevedel-btw-hooks-" t))
         (path (file-name-concat root "context.txt"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id root :root root :name "btw-hooks"))
         (session (mevedel-session-create "main" workspace))
         (mevedel-hook-rules
          '((PreToolUse
             (:matcher "Read"
              :hooks
              ((:type elisp
                :function
                mevedel-side-conversation-test--global-allow))))
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
                 'mevedel-side-conversation-test--global-allow)
                (lambda (_event)
                  (push 'global trace)
                  '(:permission-decision allow)))
               ((symbol-function
                 'mevedel-side-conversation-test--session-allow)
                (lambda (_event)
                  (push 'session trace)
                  '(:permission-decision allow)))
               ((symbol-function
                 'mevedel-side-conversation-test--request-deny)
                (lambda (_event)
                  (push 'request trace)
                  '(:permission-decision deny
                    :permission-reason "frozen request safety"))))
            (setf
             (mevedel-session-hook-rules session)
             '(("PreToolUse"
                (:matcher "Read"
                 :hooks
                 ((:type "elisp"
                   :function
                   "mevedel-side-conversation-test--session-allow"))))
               (UserPromptSubmit
                (:hooks ((:type elisp :function ignore))))))
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--workspace workspace
                            mevedel--session session)
                (insert "*** Parent prompt\n")
                (let ((start (point)))
                  (insert "Parent answer\n")
                  (put-text-property start (point) 'gptel 'response))
                (insert "\n*** Active question\n")
                (let ((response-start (copy-marker (point) nil)))
                  (setq parent-request
                        (mevedel-request--create
                         :id "btw-hooks-parent"
                         :session session
                         :fsm
                         (gptel-make-fsm
                          :state 'TYPE
                          :info
                          (list
                           :buffer data-buf
                           :position response-start
                           :mevedel-model-context
                           (buffer-substring (point-min) response-start)))
                         :hook-rules
                         '(("PreToolUse"
                            (:matcher "Read"
                             :hooks
                             ((:type "elisp"
                               :function
                               "mevedel-side-conversation-test--request-deny"))))
                           ("Stop"
                            (:hooks ((:type elisp :function ignore)))))))
                  (setq-local mevedel--current-request parent-request)))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert "/btw")
                (cl-letf (((symbol-function 'pop-to-buffer)
                           (lambda (buffer &rest _)
                             (setq side-view buffer))))
                  (mevedel-view-send)))
              (setq mevedel-hook-rules nil)
              (setf (mevedel-session-hook-rules session) nil
                    (mevedel-request-hook-rules parent-request) nil)
              (let ((side-data
                     (buffer-local-value 'mevedel--data-buffer side-view)))
                (with-current-buffer side-data
                  (let ((side-fsm
                         (gptel-make-fsm
                          :state 'WAIT :info (list :buffer side-data))))
                    (mevedel-side-conversation--handle-wait side-fsm)
                    (let ((rules
                           (mevedel-session-hook-rules mevedel--session)))
                      (should (= 3 (cl-count 'PreToolUse rules :key #'car)))
                      (should-not (assq 'SessionStart rules))
                      (should-not (assq 'UserPromptSubmit rules))
                      (should-not (assq 'Stop rules)))
                    (mevedel-pipeline-run-tool
                     (mevedel-tool-ensure "Read")
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
                          'mevedel--current-request data-buf)))
                    (mevedel-side-conversation--handle-terminal side-fsm)))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "excludes a pending TOOL confirmation while retaining completed prose"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-pending-tool" :root "/tmp"
           :name "btw-pending-tool"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response))
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
               mevedel--current-request
               (mevedel-request--create
                :id "btw-pending-tool"
                :session session
                :fsm (gptel-make-fsm
                      :state 'TOOL
                      :info (list :buffer data-buf
                                  :position response-start
                                  :mevedel-model-context
                                  (buffer-substring (point-min)
                                                    response-start)
                                  :tracking-marker tracking))))))))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send)))
      (with-current-buffer
          (buffer-local-value 'mevedel--data-buffer side-view)
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
          (mevedel-workspace--create
           :type 'file :id "btw-drafts" :root "/tmp" :name "btw-drafts"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq side-view buffer))))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (goto-char (mevedel-view--input-start))
          (insert "/btw")
          (mevedel-view-send))
        (let ((first-side side-view)
              (side-draft "> quoted\nsecond line"))
          (with-current-buffer side-view
            (mevedel-view-test--insert-composer-draft side-draft 3))
          (with-current-buffer view-buf
            (mevedel-view-test--insert-composer-draft
             "/btw another question" 5)
            (let ((point-before (point)))
              (should-error (mevedel-view-send) :type 'user-error)
              (should (= point-before (point)))
              (should (equal "/btw another question"
                             (mevedel-view--input-text)))))
          (should (eq first-side side-view))
          (with-current-buffer side-view
            (should (equal side-draft (mevedel-view--input-text)))
            (should (= 3 (- (point) (mevedel-view--input-start))))
            (let ((error
                   (should-error
                    (mevedel-view-send-follow-up) :type 'user-error)))
              (should (string-match-p
                       "/btw" (error-message-string error))))
            (should (equal side-draft (mevedel-view--input-text))))
          (with-current-buffer view-buf
            (mevedel-view--full-rerender)
            (should (equal "/btw another question"
                           (mevedel-view--input-text))))
          (with-current-buffer side-view
            (should (equal side-draft (mevedel-view--input-text))))))))
  :doc "aborts a response in place and preserves the side draft"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-abort" :root "/tmp" :name "btw-abort"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq side-view buffer))))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (goto-char (mevedel-view--input-start))
          (insert "/btw")
          (mevedel-view-send)))
      (let* ((side-data
              (buffer-local-value 'mevedel--data-buffer side-view))
             (side-session
              (buffer-local-value 'mevedel--session side-data))
             (fsm (gptel-make-fsm
                   :state 'TOOL :info (list :buffer side-data)))
             (request
              (mevedel-request--create
               :id "btw-tool-wait" :session side-session :turn 1 :fsm fsm))
             (gptel--request-alist nil)
             (cancelled nil)
             (aborted-provider nil)
             (draft "> quoted\nsecond line"))
        (with-current-buffer side-data
          (setq-local mevedel--current-request request)
          (mevedel-request-push-canceller
           request
           (lambda ()
             (setq cancelled t
                   gptel--request-alist (list (list 'process fsm)))))
          (let ((start (point-max)))
            (goto-char start)
            (insert "Partial side answer")
            (put-text-property start (point) 'gptel 'response)))
        (with-current-buffer side-view
          (mevedel-view-test--insert-composer-draft draft 3)
          (should-error (mevedel-side-conversation-send)
                        :type 'user-error)
          (should (equal draft (mevedel-view--input-text)))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (_buffer)
                       (should cancelled)
                       (setq aborted-provider t)
                       (setq gptel--request-alist nil))))
            (mevedel-view-abort))
          (should (buffer-live-p side-view))
          (should (equal draft (mevedel-view--input-text)))
          (should (= 3 (- (point) (mevedel-view--input-start)))))
        (should (buffer-live-p side-data))
        (with-current-buffer side-data
          (should cancelled)
          (should aborted-provider)
          (should-not mevedel--current-request)
          (should-not gptel--request-alist)
          (should (string-match-p
                   "preceding side response was interrupted"
                   (buffer-string)))
          (should (eq 'reminder
                      (car (car (last (mevedel-transcript-segments
                                      (point-min) (point-max)))))))))))
  :doc "accepts compacted context without projecting its inherited indicator"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-compact" :root "/tmp" :name "btw-compact"))
         (session (mevedel-session-create "main" workspace))
         side-view)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (let ((start (point)))
          (insert "#+begin_summary mevedel-role=compaction-summary\n"
                  "Only effective compacted context remains.\n"
                  "#+end_summary\n")
          (put-text-property start (point) 'face 'shadow)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send)))
      (with-current-buffer side-view
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "conversation compacted" text))
          (should-not (string-match-p "Only effective" text))))))
  :doc "discards the side with its parent without listing or persisting it"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "btw-close" :root "/tmp" :name "btw-close"))
         (session (mevedel-session-create "main" workspace))
         side-view side-data side-session)
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local mevedel--workspace workspace
                    mevedel--session session)
        (insert "*** Parent prompt\n")
        (let ((start (point)))
          (insert "Parent answer\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "/btw")
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     (setq side-view buffer))))
          (mevedel-view-send)))
      (setq side-data (buffer-local-value 'mevedel--data-buffer side-view)
            side-session (buffer-local-value 'mevedel--session side-data))
      (should (= 1 (length (mevedel--workspace-sessions workspace))))
      (should-not (mevedel-session-save-path side-session))
      (kill-buffer data-buf)
      (should-not (buffer-live-p side-view))
      (should-not (buffer-live-p side-data))
      (should-not (mevedel-session-save-path side-session)))))

(provide 'test-mevedel-side-conversation)
;;; test-mevedel-side-conversation.el ends here
