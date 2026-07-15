;;; test-mevedel-permission-queue.el -- Tests for mevedel-permission-queue -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the session permission FIFO queue.  Covers:
;; - enqueue order + head-only render
;; - coalesce on rule-creating outcomes (allow-session, always-allow,
;;   deny-session) using captured session context
;; - coalesce skip on once outcomes
;; - per-agent sweep on terminal state
;; - abort flush
;; - kind dispatch (generic / bash / eval)
;; - outcome vocabulary translation for bash adapters

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-structs)
(require 'mevedel-permissions)
(require 'mevedel-permission-log)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-skills-ui)

(defun test-pq--make-session (&optional rules)
  "Create a fresh queue-test session, optionally with RULES."
  (mevedel-session--create
   :name "test"
   :workspace nil
   :permission-rules rules
   :permission-mode 'ask
   :permission-queue nil
   :plan-queue nil))

(defun test-pq--read-permission-log (session)
  "Read SESSION's permission log entries."
  (let ((file (mevedel-permission-log-path session))
        entries)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (push (read (current-buffer)) entries))
          (end-of-file nil))))
    (nreverse entries)))


;;
;;; Enqueue order + head render

(mevedel-deftest mevedel-permission--enqueue
  (:doc "FIFO permission queue contract")
  ,test
  (test)

  :doc "enqueue keeps the visible head rendered while siblings wait"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (rendered nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Edit"
             :origin "main" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Write"
             :origin "main" :callback #'ignore)))
    ;; The visible head is re-rendered as siblings arrive so pending
    ;; counts stay current, but FIFO order is unchanged.
    (should (= 3 (length rendered)))
    (should (cl-every (lambda (entry)
                        (equal "Read" (plist-get entry :tool-name)))
                      rendered))
    ;; All three entries are on the queue in FIFO order.
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 3 (length q)))
      (should (equal "Read" (plist-get (nth 0 q) :tool-name)))
      (should (equal "Edit" (plist-get (nth 1 q) :tool-name)))
      (should (equal "Write" (plist-get (nth 2 q) :tool-name)))))

  :doc "entry captures :session at enqueue so settlement is buffer-independent"
  (let* ((session (test-pq--make-session))
         (mevedel--session session))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main" :callback #'ignore)))
    (let* ((q (mevedel-session-permission-queue session))
           (entry (car q)))
      (should (eq session (plist-get entry :session)))))

  :doc "no-session enqueue aborts without direct rendering"
  (let ((mevedel--session nil)
        (rendered nil)
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main"
             :callback (lambda (o) (setq outcome o)))))
    (should (null rendered))
    (should (eq 'aborted outcome)))

  :doc "enqueue rejects a missing origin"
  (let ((session (test-pq--make-session)))
    (should-error
     (mevedel-permission--enqueue
      (list :kind 'generic :tool-name "Read" :callback #'ignore)
      session)
     :type 'error))

  :doc "enqueue rejects malformed agent origins"
  (let ((session (test-pq--make-session)))
    (dolist (origin '(nil "" "explorer" "explorer--abc"
                          "explorer--0123456789abcdef0123456789abcdeg"))
      (should-error
       (mevedel-permission--enqueue
        (list :kind 'generic :tool-name "Read"
              :origin origin :callback #'ignore)
        session)
       :type 'error))))


;;
;;; Permission diagnostics

(mevedel-deftest mevedel-permission-log
  ()
  ,test
  (test)
  :doc "buffers diagnostics until the session is materialized"
  (let ((session (test-pq--make-session))
        (mevedel-permission-log-enabled t))
    (mevedel-permission-log session 'permission-decision :tool-name "Read")
    (should (= 1 (length (mevedel-session-permission-log-pending session))))
    (should-not (mevedel-permission-log-path session)))

  :doc "permission queue writes enqueue and resolve diagnostics"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t)
         outcome)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'generic
                   :tool-name "Read"
                   :specifier-key :path
                   :specifier-value "/tmp/a.el"
                   :origin "verifier--0123456789abcdef0123456789abcdef"
                   :callback (lambda (o) (setq outcome o)))
             session))
          (let ((entry (car (mevedel-session-permission-queue session))))
            (mevedel-permission-queue--on-head-outcome entry 'deny-once))
          (should (eq 'deny-once outcome))
          (let ((entries (test-pq--read-permission-log session)))
            (should (= 2 (length entries)))
            (should (eq 'permission-enqueued
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'permission-resolved
                        (plist-get (nth 1 entries) :event)))
            (should (equal "Read" (plist-get (nth 0 entries) :tool-name)))
            (should (equal "verifier--0123456789abcdef0123456789abcdef"
                           (plist-get (nth 0 entries) :origin)))
            (should (eq 'deny-once
                        (plist-get (nth 1 entries) :outcome)))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "Bash lifecycle diagnostics omit raw command payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'bash
                   :command "printf SECRET_TOKEN"
                   :commands-summary "printf"
                   :command-class 'unknown
                   :origin "main"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (equal "printf" (plist-get entry :commands-summary)))
            (should-not (plist-member entry :command))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "Eval lifecycle diagnostics omit raw expression payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'eval
                   :expression "(message \"SECRET_TOKEN\")"
                   :mode "live"
                   :origin "main"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (equal "live" (plist-get entry :mode)))
            (should-not (plist-member entry :expression))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))


;;
;;; Coalesce: rule-creating outcomes

(mevedel-deftest mevedel-permission-queue--coalesce
  (:doc "coalesce re-evaluates queued siblings against the just-created rule")
  ,test
  (test)

  :doc "allow-session coalesces queued path siblings via a resource grant"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (resolved-outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      ;; Enqueue two identical Read requests.
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "main"
             :callback (lambda (o) (push (cons "Read1" o) resolved-outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "main"
             :callback (lambda (o) (push (cons "Read2" o) resolved-outcomes))))
      ;; User answers allow-session for the head.  Simulate the exact
      ;; resource grant written by the normal permission prompt.
      (mevedel-permission-add-session-resource-grant
       session "/foo/bar.el" 'read)
      ;; Coalesce should resolve the queued sibling as 'allow.
      (mevedel-permission-queue--coalesce 'allow-session session))
    ;; Read2 was coalesced; Read1 (the head) wasn't touched by
    ;; --coalesce (the head's callback already fired before
    ;; --coalesce was called).
    (should (assoc "Read2" resolved-outcomes))
    (should (eq 'allow (cdr (assoc "Read2" resolved-outcomes))))
    ;; Queue is now empty (Read1 was already dropped before
    ;; --coalesce; Read2 was just resolved).
    (should (null (mevedel-session-permission-queue session))))

  :doc "deny-session coalesces queued siblings to deny"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (push '("Read" :path "/x.el" :action deny)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (memq 'deny outcomes)))

  :doc "queued sibling whose rule does not cover it stays queued"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      ;; Two queued entries with different paths.
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo.el"
             :resource-access 'read
             :origin "main"
             :callback (lambda (o) (push (cons "foo" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/bar.el"
             :resource-access 'read
             :origin "main"
             :callback (lambda (o) (push (cons "bar" o) outcomes))))
      ;; Resource authority covers /foo.el only.
      (mevedel-permission-add-session-resource-grant
       session "/foo.el" 'read)
      (mevedel-permission-queue--coalesce 'allow-session session))
    ;; /foo.el's queued sibling resolved; /bar.el stayed.
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "/bar.el" (plist-get (car q) :specifier-value)))))

  :doc "coalesce honors non-path generic specifier keys"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "WebFetch"
             :specifier-key :domain
             :specifier-value "example.com"
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (push '("WebFetch" :domain "example.com" :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (memq 'allow outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "protected paths do not coalesce allow rules but do coalesce deny rules"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-protected-paths '(("**/.git/**" . read-only)))
         (path "/repo/.git/config")
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (push `("Read" :path ,path :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session)
      (should-not outcomes)
      (should (= 2 (length (mevedel-session-permission-queue session))))
      (setf (mevedel-session-permission-rules session)
            (list `("Read" :path ,path :action deny)))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny deny) outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "rule write failure leaves queued siblings pending"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "main"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should-not outcomes)
    (should (= 2 (length (mevedel-session-permission-queue session))))))


;;
;;; Render dispatch

(mevedel-deftest mevedel-permission-queue--render-generic
  (:doc "renders generic permission queue entries")
  ,test
  (test)

  :doc "no-workspace entries still render through the generic prompt adapter"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'generic
                      :tool-name "Read"
                      :specifier-value nil
                      :include-always nil
                      :workspace nil
                      :session session
                      :callback #'ignore))
         captured)
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
               (lambda (tool path include-always _origin cont count rendered-entry)
                 (setq captured
                       (list tool path include-always cont count
                             rendered-entry)))))
      (mevedel-permission-queue--render-generic entry))
    (should (equal "Read" (nth 0 captured)))
    (should (null (nth 1 captured)))
    (should (= 1 (nth 4 captured)))
    (should (eq entry (nth 5 captured))))

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (mevedel--session session)
           (outcome nil)
           (entry (list :kind 'generic
                        :tool-name "Read"
                        :specifier-value "/tmp/file.txt"
                        :include-always nil
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (mevedel-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (mevedel-session-permission-queue session)))))

  :doc "Grep prompt with session back-reference renders without mutating the entry plist"
  (let ((data-buf (generate-new-buffer " *test-pq-grep-data*"))
        (view-buf (generate-new-buffer " *test-pq-grep-view*"))
        (session (test-pq--make-session))
        (render-error nil)
        entry
        interaction-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer-name)
                       (when (string-match-p "permission-queue: render error"
                                             message)
                         (setq render-error message)))))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Grep"
                     :args (list :pattern "mevedel-check-permission"
                                 :path default-directory
                                 :glob "*.el"
                                 :output_mode "content"
                                 :head_limit nil
                                 :offset nil
                                 :context nil
                                 :-A nil
                                 :-B nil
                                 :-C nil
                                 :-i nil
                                 :-n t
                                 :type nil
                                 :multiline nil)
                     :specifier-key :path
                     :specifier-value
                     (file-name-concat
                      (file-name-directory
                       (directory-file-name default-directory))
                      "**")
                     :protected-path nil
                     :include-always t
                     :workspace nil
                     :origin "main"
                     :callback #'ignore)
               session)
              (setq entry (car (mevedel-session-permission-queue session)))
              (setq interaction-id
                    (mevedel-queue--entry-metadata-get
                     entry :interaction-id))
              (should-not render-error)
              (should interaction-id)
              (should-not (plist-member entry :interaction-id))
              (should-not (plist-member entry :view-buffer))
              (with-current-buffer view-buf
                (should (gethash interaction-id
                                 mevedel-view--interaction-overlays))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-permission-queue--allow-once-advance
  (:doc "allow-once settles only the visible head and renders the next prompt")
  ,test
  (test)

  (let ((data-buf (generate-new-buffer " *test-pq-once-data*"))
        (view-buf (generate-new-buffer " *test-pq-once-view*"))
        (session (test-pq--make-session))
        outcomes)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/one.el"
                     :include-always t
                     :origin "main"
                     :callback
                     (lambda (outcome)
                       (push (cons "one" outcome) outcomes)))
               session)
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/two.el"
                     :include-always t
                     :origin "main"
                     :callback
                     (lambda (outcome)
                       (push (cons "two" outcome) outcomes)))
               session))
            (with-current-buffer view-buf
              (let ((first-id (mevedel-queue--entry-metadata-get
                               (car (mevedel-session-permission-queue session))
                               :interaction-id)))
                (should first-id)
                (should (gethash first-id mevedel-view--interaction-overlays))
                (mevedel--prompt--settle
                 (gethash first-id mevedel-view--interaction-overlays)
                 'allow-once)
                (should (equal '(("one" . allow-once)) outcomes))
                (should (= 1 (length (mevedel-session-permission-queue session))))
                (let ((next-id (mevedel-queue--entry-metadata-get
                                (car (mevedel-session-permission-queue session))
                                :interaction-id)))
                  (should next-id)
                  (should (gethash next-id mevedel-view--interaction-overlays)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-permission-queue--gptel-batch-dispatch
  (:doc "queues every concurrently dispatched outside-workspace Read")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-pq-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "mevedel-pq-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-batch-data*"))
         (view-buf (generate-new-buffer " *test-pq-batch-view*"))
         (workspace (mevedel-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (mevedel-session-create "main" workspace))
         (_register (mevedel-tool-fs--register))
         (read-tool (mevedel-tool-get "Read"))
         (gptel-tool (mevedel-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local temporary-file-directory workspace-root))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'ask))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let* ((first-id
                        (mevedel-queue--entry-metadata-get
                         (car (mevedel-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         mevedel-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (should (overlay-get ov 'mevedel--callback))
                  (mevedel--prompt--settle ov 'allow-once))
                (should (= 3 (length (mevedel-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (mevedel-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (mevedel-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-permission-queue--gptel-batch-deny-once
  (:doc "deny-once settles one queued gptel-dispatched permission")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-pq-deny-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "mevedel-pq-deny-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-deny-data*"))
         (view-buf (generate-new-buffer " *test-pq-deny-view*"))
         (workspace (mevedel-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (mevedel-session-create "main" workspace))
         (_register (mevedel-tool-fs--register))
         (read-tool (mevedel-tool-get "Read"))
         (gptel-tool (mevedel-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local temporary-file-directory workspace-root))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'ask))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let* ((first-id
                        (mevedel-queue--entry-metadata-get
                         (car (mevedel-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         mevedel-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (mevedel--prompt--settle ov 'deny-once))
                (should (= 3 (length (mevedel-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (mevedel-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (should (string-match-p
                       "Error: Permission denied"
                       (car processed-results)))
              (mevedel-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-permission-queue--render-bash
  (:doc "renders queued Bash permission prompts")
  ,test
  (test)

  :doc "missing Bash UI helper produces the pinned denial outcome"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (entry (list :kind 'bash
                      :command "sudo ls"
                      :command-class 'dangerous
                      :include-always nil
                      :session session))
         (outcome nil)
         (saved (and (fboundp 'mevedel-permission--prompt-async-bash)
                     (symbol-function
                      'mevedel-permission--prompt-async-bash))))
    (setq entry (plist-put entry :callback (lambda (o) (setq outcome o))))
    (unwind-protect
        (progn
          (when saved
            (fmakunbound 'mevedel-permission--prompt-async-bash))
          (setf (mevedel-session-permission-queue session) (list entry))
          (mevedel-permission-queue--render-head session)
          (should (equal '(deny . "Bash permission UI unavailable")
                         outcome))
          (should (null (mevedel-session-permission-queue session))))
	  (when saved
	    (fset 'mevedel-permission--prompt-async-bash saved))))

  :doc "Bash approval resumes once while background agents remain tracked"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'bash
                      :command "git status"
                      :command-class 'read-only
                      :include-always nil
                      :session session))
         (outcomes nil)
         rendered)
    (setf (mevedel-session-background-agents session)
          '("explorer--still-running"))
    (setq entry (plist-put entry :callback
                           (lambda (o) (push o outcomes))))
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (next-entry) (push next-entry rendered))))
      (mevedel-permission-queue--on-head-outcome entry 'allow-once)
      (mevedel-permission-queue--on-head-outcome entry 'allow-once))
    (should (equal '(allow-once) outcomes))
    (should (null rendered))
    (should (null (mevedel-session-permission-queue session)))
    (should (equal '("explorer--still-running")
                   (mevedel-session-background-agents session)))))

(mevedel-deftest mevedel-permission-queue--render-eval
  (:doc "renders queued Eval permission prompts")
  ,test
  (test)

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (mevedel--session session)
           (outcome nil)
           (entry (list :kind 'eval
                        :expression "(message \"hi\")"
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (mevedel-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (mevedel-session-permission-queue session)))))

  :doc "agent Eval permission renders in the parent interaction view"
  (let ((parent-data (generate-new-buffer " *test-pq-parent-data*"))
        (parent-view (generate-new-buffer " *test-pq-parent-view*"))
        (agent-data (generate-new-buffer " *test-pq-agent-data*"))
        (session (test-pq--make-session)))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create
                         :agent-id
                         "verifier--abcdef123456abcdef123456abcdef12")))
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer agent-data
              (mevedel-permission--enqueue
               (list :kind 'eval
                     :expression "(message \"hi\")"
                     :mode "batch"
                     :origin "verifier--abcdef123456abcdef123456abcdef12"
                     :callback #'ignore)
               session)))
          (with-current-buffer parent-view
            (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                    (buffer-string)))
            (should (string-match-p "from verifier--abcdef"
                                    (buffer-string)))
            (should (string-match-p "Mode: batch"
                                    (buffer-string)))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "agent Eval permissions survive blocked status redraw and rebuild"
  (let ((parent-data (generate-new-buffer " *test-pq-parent-status-data*"))
        (parent-view (generate-new-buffer " *test-pq-parent-status-view*"))
        (agent-data (generate-new-buffer " *test-pq-agent-status-data*"))
        (session (test-pq--make-session))
        outcomes)
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create
                         :agent-id
                         "verifier--abcdef123456abcdef123456abcdef12")))
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask))
                    ((symbol-function 'mevedel-view--agent-status-collect)
                     (lambda ()
                       (list (list :agent-id
                                   "verifier--abcdef123456abcdef123456abcdef12"
                                   :status 'blocked
                                   :agent-type "verifier"
                                   :description "Verify tracked diff"
                                   :calls 18)))))
            (with-current-buffer parent-view
              (mevedel-view--render-agent-status))
            (with-current-buffer agent-data
              (dotimes (i 3)
                (mevedel-permission--enqueue
                 (list :kind 'eval
                       :expression (format "(+ %d 1)" i)
                       :mode "batch"
                       :origin
                       "verifier--abcdef123456abcdef123456abcdef12"
                       :callback (lambda (outcome)
                                   (push outcome outcomes)))
                 session)))
            (with-current-buffer parent-view
              (mevedel-view--render-agent-status)
              (mevedel-view--interaction-rebuild)
              (should-not outcomes)
              (should (= 3 (length (mevedel-session-permission-queue session))))
              (let* ((text (buffer-substring-no-properties
                            (point-min) mevedel-view--input-marker))
                     (agent-pos (string-search
                                 "Agent: verifier -- Verify tracked diff" text))
                     (prompt-pos (string-search
                                  "The LLM is requesting permission to evaluate elisp"
                                  text)))
                (should agent-pos)
                (should prompt-pos)
                (should (< agent-pos prompt-pos))
                (should (equal "3 permissions pending"
                               (mevedel-view--interaction-count-label)))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "3 permissions pending"
                                  mevedel-view--input-marker)
                  (should (eq 'interaction
                              (get-text-property
                               (match-beginning 0)
                               'mevedel-view-zone-namespace)))
                  (should (eq :separator
                              (get-text-property
                               (match-beginning 0)
                               'mevedel-view-zone-id))))
                (should (string-search "from verifier--abcdef" text))
                (should (string-search "Mode: batch" text))))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data)))))


;;
;;; Coalesce vocabulary translation

(mevedel-deftest mevedel-permission-queue--translate-coalesce-outcome
  (:doc "translates 'allow / 'deny to the kind's expected callback vocabulary")
  ,test
  (test)

  :doc "generic kind passes 'allow / 'deny through unchanged"
  (should (eq 'allow
              (mevedel-permission-queue--translate-coalesce-outcome
               'generic 'allow)))
  (should (eq 'deny
              (mevedel-permission-queue--translate-coalesce-outcome
               'generic 'deny)))

  :doc "bash kind passes 'allow through"
  (should (eq 'allow
              (mevedel-permission-queue--translate-coalesce-outcome
               'bash 'allow)))
  (should (eq 'deny
              (mevedel-permission-queue--translate-coalesce-outcome
               'bash 'deny)))

  :doc "eval kind translates 'allow to authoritative allow-once"
  (should (eq 'allow-once
              (mevedel-permission-queue--translate-coalesce-outcome
               'eval 'allow))))


;;
;;; Abort flush

(mevedel-deftest mevedel-permission-queue-abort-all
  (:doc "abort flushes queue and fires 'aborted on every callback")
  ,test
  (test)

  :doc "every queued callback fires 'aborted; queue is empty after"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main"
             :callback (lambda (o) (push (cons "Read" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'bash :command "rm /tmp/x"
             :origin "main"
             :callback (lambda (o) (push (cons "Bash" o) outcomes))))
      (mevedel-permission-queue-abort-all session))
    (should (= 2 (length outcomes)))
    (should (cl-every (lambda (o) (eq 'aborted (cdr o))) outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "abort on empty queue is a no-op"
  (let ((session (test-pq--make-session)))
    (should-not (mevedel-permission-queue-abort-all session))
    (should (null (mevedel-session-permission-queue session)))))


;;
;;; Per-agent sweep

(mevedel-deftest mevedel-permission-queue-sweep-origin
  (:doc "sweep fires 'aborted on entries owned by ORIGIN; others stay")
  ,test
  (test)

  :doc "entries with matching :origin fire 'aborted; non-matching stay"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "explorer--0123456789abcdef0123456789abcdef"
             :callback (lambda (o) (push (cons "explorer" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main"
             :callback (lambda (o) (push (cons "main" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "explorer--0123456789abcdef0123456789abcdef"
             :callback (lambda (o) (push (cons "explore2" o) outcomes))))
      (mevedel-permission-queue-sweep-origin
       "explorer--0123456789abcdef0123456789abcdef" session))
    ;; explorer-owned entries fired 'aborted.
    (should (eq 'aborted (cdr (assoc "explorer" outcomes))))
    (should (eq 'aborted (cdr (assoc "explore2" outcomes))))
    ;; main-owned entry did NOT fire — still queued.
    (should-not (assoc "main" outcomes))
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "main" (plist-get (car q) :origin)))))

  :doc "sweeping the visible head removes its interaction overlay"
  (let ((data-buf (generate-new-buffer " *test-pq-sweep-data*"))
        (view-buf (generate-new-buffer " *test-pq-sweep-view*"))
        (session (test-pq--make-session))
        outcomes
        swept-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/agent.txt"
                     :include-always nil
                     :origin "explorer--0123456789abcdef0123456789abcdef"
                     :callback (lambda (o) (push (cons "agent" o) outcomes))))
              (setq swept-id
                    (mevedel-queue--entry-metadata-get
                     (car (mevedel-session-permission-queue session))
                     :interaction-id))
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/main.txt"
                     :include-always nil
                     :origin "main"
                     :callback (lambda (o) (push (cons "main" o) outcomes))))
              (should swept-id)
              (with-current-buffer view-buf
                (should (gethash swept-id mevedel-view--interaction-overlays)))
              (mevedel-permission-queue-sweep-origin
               "explorer--0123456789abcdef0123456789abcdef" session)))
          (should (eq 'aborted (cdr (assoc "agent" outcomes))))
          (should-not (assoc "main" outcomes))
          (with-current-buffer view-buf
            (should-not (gethash swept-id mevedel-view--interaction-overlays))
            (should (= 1 (hash-table-count
                          mevedel-view--interaction-overlays))))
          (let ((q (mevedel-session-permission-queue session)))
            (should (= 1 (length q)))
            (should (equal "main" (plist-get (car q) :origin)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))


(provide 'test-mevedel-permission-queue)
;;; test-mevedel-permission-queue.el ends here
