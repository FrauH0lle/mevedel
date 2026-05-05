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
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-mentions)

(defun test-pq--make-session (&optional rules)
  "Create a fresh session for queue tests, optionally with RULES."
  (mevedel-session--create
   :name "test"
   :workspace nil
   :permission-rules rules
   :permission-mode 'default
   :permission-queue nil
   :plan-queue nil))


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
       (list :kind 'generic :tool-name "Read" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Edit" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Write" :callback #'ignore)))
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
       (list :kind 'generic :tool-name "Read" :callback #'ignore)))
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
             :callback (lambda (o) (setq outcome o)))))
    (should (null rendered))
    (should (eq 'aborted outcome))))


;;
;;; Coalesce: rule-creating outcomes

(mevedel-deftest mevedel-permission-queue--coalesce
  (:doc "coalesce re-evaluates queued siblings against the just-created rule")
  ,test
  (test)

  :doc "allow-session coalesces queued generic siblings via session-rules"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (resolved-outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      ;; Enqueue two identical Read requests.
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :callback (lambda (o) (push (cons "Read1" o) resolved-outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :callback (lambda (o) (push (cons "Read2" o) resolved-outcomes))))
      ;; User answers allow-session for the head.  Simulate the rule
      ;; write by adding it to the session struct directly.
      (push '("Read" :path "/foo/bar.el" :action allow)
            (mevedel-session-permission-rules session))
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
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
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
             :callback (lambda (o) (push (cons "foo" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/bar.el"
             :callback (lambda (o) (push (cons "bar" o) outcomes))))
      ;; Rule covers /foo.el only.
      (push '("Read" :path "/foo.el" :action allow)
            (mevedel-session-permission-rules session))
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
             :callback (lambda (o) (push o outcomes))))
      (push '("WebFetch" :domain "example.com" :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (memq 'allow outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "protected paths do not coalesce allow rules but do coalesce deny rules"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-protected-paths '("**/.git/**"))
         (path "/repo/.git/config")
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
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
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
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
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async)
               (lambda (tool path include-always cont count rendered-entry)
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
                     (lambda () 'default))
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
                     (lambda () 'default)))
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
         processed-results
         (tool-use (cl-loop for path in paths
                            for idx from 1
                            collect
                            (list :name "Read"
                                  :id (format "read-%d" idx)
                                  :args (list :file_path path)))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'default))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'default))
                      ((symbol-function 'gptel--process-tool-call)
                       (lambda (&rest args)
                         (push args processed-results))))
              (let ((fsm (gptel-make-fsm
                          :info (list :buffer data-buf
                                      :backend 'test-backend
                                      :tools (list gptel-tool)
                                      :tool-use tool-use
                                      :callback #'ignore))))
                (gptel--handle-tool-use fsm))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let ((ov (mevedel--prompt--overlay-at-point
                           'mevedel-permission-prompt)))
                  (should ov)
                  (should (eq ov
                              (gethash
                               (overlay-get ov 'mevedel-view-interaction-id)
                               mevedel-view--interaction-overlays)))
                  (should (overlay-get ov 'mevedel--callback)))
                (let ((last-command-event ?a))
                  (call-interactively
                   #'mevedel-permission--prompt-approve-once))
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
         processed-results
         (tool-use (cl-loop for path in paths
                            for idx from 1
                            collect
                            (list :name "Read"
                                  :id (format "read-%d" idx)
                                  :args (list :file_path path)))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'default))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'default))
                      ((symbol-function 'gptel--process-tool-call)
                       (lambda (&rest args)
                         (push args processed-results))))
              (let ((fsm (gptel-make-fsm
                          :info (list :buffer data-buf
                                      :backend 'test-backend
                                      :tools (list gptel-tool)
                                      :tool-use tool-use
                                      :callback #'ignore))))
                (gptel--handle-tool-use fsm))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let ((last-command-event ?d))
                  (call-interactively
                   #'mevedel-permission--prompt-deny-once))
                (should (= 3 (length (mevedel-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (mevedel-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (should (string-match-p
                       "Error: Permission denied"
                       (nth 3 (car processed-results))))
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
                      :dangerous t
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
        (fset 'mevedel-permission--prompt-async-bash saved)))))

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
      (should (null (mevedel-session-permission-queue session))))))


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
             :callback (lambda (o) (push (cons "Read" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'bash :command "rm /tmp/x"
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

(mevedel-deftest mevedel-permission-queue-sweep-agent
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
             :origin "explorer--abc"
             :callback (lambda (o) (push (cons "explorer" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "main"
             :callback (lambda (o) (push (cons "main" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "explorer--abc"
             :callback (lambda (o) (push (cons "explore2" o) outcomes))))
      (mevedel-permission-queue-sweep-agent "explorer--abc" session))
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
                     (lambda () 'default)))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/agent.txt"
                     :include-always nil
                     :origin "explorer--abc"
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
              (mevedel-permission-queue-sweep-agent "explorer--abc" session)))
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
